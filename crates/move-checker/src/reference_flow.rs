use std::collections::{HashMap, HashSet};

use hir::{
    HirFile,
    body::{
        BinaryOp, Body, BodyId, Expr, ExprId, PatId, Pattern, PatternBindingId, ResolvedName, Stmt,
        StmtId, UnaryOp,
    },
    item_tree::{EnumId, FunctionId, StructId},
    place::Projection,
};
use rowan::TextRange;
use ty::{CapturePlace, CaptureSource, PatternBindingMode, Type, TypeCheckResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FlowKind {
    Inherit,
    Shared,
    Mutable,
}

/// One step of a field/index path recorded on a summary origin: how the
/// borrowed place was reached from the parameter root (`&self.values[i]`
/// records `[Field(0), Index(None)]` against param 0).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum FlowProjection {
    Field(usize),
    Index(Option<usize>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SummaryOrigin {
    pub(crate) param: usize,
    pub(crate) path: Vec<FlowProjection>,
    pub(crate) kind: FlowKind,
    /// The path crosses through a reference-typed prefix (a reference stored
    /// in a field, or the pointee of the receiver's own reference): the
    /// aliased region lives outside the receiver's own storage, so a loan
    /// mapped from this origin can never conflict with borrows of the
    /// receiver itself. Computed after the fixpoint by walking the path
    /// against the impl's self type.
    pub(crate) behind_reference: bool,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FunctionSummary {
    pub(crate) origins: HashSet<SummaryOrigin>,
    pub(crate) opaque: bool,
    pub(crate) fields: Vec<Self>,
    pub(crate) stores: Vec<StoredReference>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StoredReference {
    pub(crate) target_param: usize,
    pub(crate) target_path: Vec<FlowProjection>,
    pub(crate) origins: HashSet<SummaryOrigin>,
    pub(crate) opaque: bool,
}

#[derive(Debug, Default)]
pub struct ReferenceFlow {
    summaries: HashMap<FunctionId, FunctionSummary>,
    /// Per trait method, the join of every impl's (and default method's)
    /// summary. Generic-bound dispatch has no concrete callee to summarize;
    /// the join is what those calls may assume — sound because the
    /// compilation sees every impl.
    trait_method_joins: HashMap<(hir::item_tree::TraitId, String), FunctionSummary>,
}

impl ReferenceFlow {
    pub(crate) fn build(hir: &HirFile, type_result: &TypeCheckResult) -> Self {
        // Contracted trait methods license adapters' summaries to carry the
        // behind-reference marker for generic inner calls. The contract is
        // verified against every impl after the fixpoint; a failure disables
        // the contract and the analysis reruns without it (at most one extra
        // round — disabling is monotone).
        let mut disabled_contracts = HashSet::new();
        let mut summaries;
        loop {
            let fresh = hir
                .function_bodies
                .keys()
                .copied()
                .filter(|fid| !is_std_builtin(hir, *fid))
                .map(|fid| (fid, FunctionSummary::default()))
                .collect::<HashMap<_, _>>();
            summaries = fresh;
            loop {
                let previous = summaries.clone();
                for (fid, body_id) in &hir.function_bodies {
                    if is_std_builtin(hir, *fid) {
                        continue;
                    }
                    let summary = SummaryAnalyzer::new(
                        hir,
                        type_result,
                        &previous,
                        &disabled_contracts,
                        *body_id,
                    )
                    .analyze_function(*fid);
                    summaries.insert(*fid, summary);
                }
                if summaries == previous {
                    break;
                }
            }
            annotate_behind_reference(hir, &mut summaries);
            let failed = verify_contracts(hir, &summaries);
            let newly_failed = failed
                .iter()
                .filter(|contract| !disabled_contracts.contains(*contract))
                .cloned()
                .collect::<HashSet<_>>();
            if newly_failed.is_empty() {
                break;
            }
            disabled_contracts.extend(newly_failed);
        }

        let trait_method_joins = build_trait_method_joins(hir, &summaries, &disabled_contracts);

        Self {
            summaries,
            trait_method_joins,
        }
    }

    pub(crate) fn summary(&self, fid: FunctionId) -> Option<&FunctionSummary> {
        self.summaries.get(&fid)
    }

    pub(crate) fn trait_method_summary(
        &self,
        trait_id: hir::item_tree::TraitId,
        method: &str,
    ) -> Option<&FunctionSummary> {
        self.trait_method_joins.get(&(trait_id, method.to_string()))
    }
}

/// A trait method declared `#[flow = "behind_reference"]` promises that every
/// impl's returned references alias regions behind references stored in the
/// receiver, never the receiver's own storage. The promise is verified
/// against each impl's summary (all origins empty or walk-verified as
/// behind-reference, nothing opaque); if any impl fails the check the
/// contract is demoted and generic dispatch stays conservative.
pub(crate) fn trait_method_contracted(
    hir: &HirFile,
    trait_id: hir::item_tree::TraitId,
    method: &str,
) -> bool {
    let tr = &hir.item_tree.traits[trait_id];
    if !tr
        .attrs
        .iter()
        .any(|attr| attr.name.0 == "flow" && attr.value.as_deref() == Some("behind_reference"))
        && !tr.methods.iter().any(|m| {
            m.name.0 == method
                && m.attrs.iter().any(|attr| {
                    attr.name.0 == "flow" && attr.value.as_deref() == Some("behind_reference")
                })
        })
    {
        return false;
    }
    true
}

fn summary_conforms_to_contract(summary: &FunctionSummary) -> bool {
    if summary.opaque {
        return false;
    }
    summary.origins.iter().all(|origin| origin.behind_reference)
        && summary.fields.iter().all(summary_conforms_to_contract)
}

/// Checks every contracted trait method against its impls' summaries and
/// returns the contracts that failed (a non-conforming or opaque member).
fn verify_contracts(
    hir: &HirFile,
    summaries: &HashMap<FunctionId, FunctionSummary>,
) -> HashSet<(hir::item_tree::TraitId, String)> {
    let mut failed = HashSet::new();
    for (trait_id, tr) in hir.item_tree.traits.iter() {
        for method in &tr.methods {
            if !trait_method_contracted(hir, trait_id, &method.name.0) {
                continue;
            }
            let conforms =
                |fid: &FunctionId| summaries.get(fid).is_none_or(summary_conforms_to_contract);
            let all_impls_conform = hir.item_tree.impls.values().all(|imp| {
                let Some(trait_range) = imp.trait_ty_range else {
                    return true;
                };
                let Some(ResolvedName::Trait(id)) = hir.type_resolutions.get(&trait_range) else {
                    return true;
                };
                if *id != trait_id {
                    return true;
                }
                imp.methods.iter().all(|impl_method| {
                    hir.item_tree.functions[*impl_method].name.0 != method.name.0
                        || conforms(impl_method)
                })
            });
            if !all_impls_conform {
                failed.insert((trait_id, method.name.0.clone()));
            }
        }
    }
    failed
}

fn build_trait_method_joins(
    hir: &HirFile,
    summaries: &HashMap<FunctionId, FunctionSummary>,
    disabled_contracts: &HashSet<(hir::item_tree::TraitId, String)>,
) -> HashMap<(hir::item_tree::TraitId, String), FunctionSummary> {
    let mut joins = HashMap::new();
    for imp in hir.item_tree.impls.values() {
        let Some(trait_range) = imp.trait_ty_range else {
            continue;
        };
        let Some(ResolvedName::Trait(trait_id)) = hir.type_resolutions.get(&trait_range) else {
            continue;
        };
        for &method in &imp.methods {
            let name = hir.item_tree.functions[method].name.0.clone();
            if disabled_contracts.contains(&(*trait_id, name.clone())) {
                continue;
            }
            join_trait_method(summaries, &mut joins, *trait_id, &name, method);
        }
    }
    for (trait_id, tr) in hir.item_tree.traits.iter() {
        for &method in &tr.default_methods {
            join_trait_method(
                summaries,
                &mut joins,
                trait_id,
                &hir.item_tree.functions[method].name.0,
                method,
            );
        }
    }
    joins
}

fn join_trait_method(
    summaries: &HashMap<FunctionId, FunctionSummary>,
    joins: &mut HashMap<(hir::item_tree::TraitId, String), FunctionSummary>,
    trait_id: hir::item_tree::TraitId,
    method_name: &str,
    method: FunctionId,
) {
    let Some(summary) = summaries.get(&method) else {
        return;
    };
    joins
        .entry((trait_id, method_name.to_string()))
        .or_default()
        .merge(summary.clone());
}

/// Marks every impl-method summary origin whose path crosses through a
/// reference-typed prefix of the impl's self type. `HashSet` keys hash on
/// every field, so origins are taken out and reinserted.
fn annotate_behind_reference(hir: &HirFile, summaries: &mut HashMap<FunctionId, FunctionSummary>) {
    let impl_self_types = hir
        .item_tree
        .impls
        .values()
        .flat_map(|imp| {
            imp.methods
                .iter()
                .map(|method| (*method, imp.self_ty.clone()))
        })
        .collect::<HashMap<_, _>>();
    for (fid, summary) in summaries.iter_mut() {
        let Some(self_ty) = impl_self_types.get(fid) else {
            continue;
        };
        annotate_summary_origins(hir, self_ty, summary);
    }
}

fn annotate_summary_origins(
    hir: &HirFile,
    self_ty: &hir::item_tree::HirTypeRef,
    summary: &mut FunctionSummary,
) {
    let origins = std::mem::take(&mut summary.origins);
    summary.origins = origins
        .into_iter()
        .map(|mut origin| {
            // Only strengthen: the walk verifies structurally-crossed paths,
            // while contracted-trait fallbacks license their own marker —
            // neither source may clear the other's claim.
            origin.behind_reference |= path_crosses_reference(hir, self_ty, &origin.path);
            origin
        })
        .collect();
    for field in &mut summary.fields {
        annotate_summary_origins(hir, self_ty, field);
    }
}

/// Walks a summary origin's path against the impl's self type. Any step that
/// applies after a reference-typed prefix crosses into the reference's
/// pointee, which lives outside the receiver's own storage. Steps that
/// cannot be walked stop the walk conservatively.
fn path_crosses_reference(
    hir: &HirFile,
    self_ty: &hir::item_tree::HirTypeRef,
    path: &[FlowProjection],
) -> bool {
    let mut ty = self_ty;
    let mut crossed = false;
    for step in path {
        // Reference and raw-pointer prefixes both reach outside the
        // receiver's own storage (a stored borrow's pointee, or the heap
        // behind a `*mut T` field like Vector's buffer).
        while let hir::item_tree::HirTypeRef::Ref(inner, _)
        | hir::item_tree::HirTypeRef::Ptr { inner, .. } = ty
        {
            crossed = true;
            ty = inner;
        }
        ty = match step {
            FlowProjection::Field(index) => match field_type_at(hir, ty, *index) {
                Some(field) => field,
                None => return crossed,
            },
            FlowProjection::Index(_) => match element_type_at(ty) {
                Some(element) => element,
                None => return crossed,
            },
        };
    }
    crossed
}

fn field_type_at<'a>(
    hir: &'a HirFile,
    ty: &'a hir::item_tree::HirTypeRef,
    index: usize,
) -> Option<&'a hir::item_tree::HirTypeRef> {
    match ty {
        hir::item_tree::HirTypeRef::Named(path) => match hir.type_resolutions.get(&path.range) {
            Some(ResolvedName::Struct(id)) => hir.item_tree.structs[*id]
                .fields
                .get(index)
                .map(|field| &field.ty),
            _ => None,
        },
        hir::item_tree::HirTypeRef::Tuple(elements) => elements.get(index),
        _ => None,
    }
}

fn element_type_at(ty: &hir::item_tree::HirTypeRef) -> Option<&hir::item_tree::HirTypeRef> {
    match ty {
        hir::item_tree::HirTypeRef::Slice(inner) | hir::item_tree::HirTypeRef::Array(inner, _) => {
            Some(inner)
        }
        _ => None,
    }
}

fn is_std_builtin(hir: &HirFile, fid: FunctionId) -> bool {
    hir.std_loaded
        && hir
            .package_for_range(hir.item_tree.functions[fid].name_range)
            .is_none()
        && hir.item_tree.functions[fid]
            .attrs
            .iter()
            .any(|attr| attr.name.0 == "builtin")
}

type FlowValue = FunctionSummary;

impl FunctionSummary {
    fn from_param(param: usize) -> Self {
        Self {
            origins: std::iter::once(SummaryOrigin {
                param,
                path: Vec::new(),
                kind: FlowKind::Inherit,
                behind_reference: false,
            })
            .collect(),
            opaque: false,
            fields: Vec::new(),
            stores: Vec::new(),
        }
    }

    fn merge(&mut self, other: Self) {
        if self.is_empty() {
            *self = other;
            return;
        }
        if other.is_empty() {
            return;
        }

        if self.fields.len() == other.fields.len() && !self.fields.is_empty() {
            for (field, other_field) in self.fields.iter_mut().zip(other.fields.iter().cloned()) {
                field.merge(other_field);
            }
        } else {
            self.fields.clear();
        }
        self.origins.extend(other.origins);
        self.opaque |= other.opaque;
        for store in other.stores {
            if !self.stores.contains(&store) {
                self.stores.push(store);
            }
        }
    }

    fn with_kind(mut self, kind: FlowKind) -> Self {
        self.origins = self
            .origins
            .into_iter()
            .map(|origin| SummaryOrigin {
                param: origin.param,
                path: origin.path,
                kind,
                behind_reference: origin.behind_reference,
            })
            .collect();
        self.fields = self
            .fields
            .into_iter()
            .map(|field| field.with_kind(kind))
            .collect();
        self
    }

    /// Marks every origin (recursively through fields) as living behind a
    /// reference — used by the contracted-trait-call fallback, where the
    /// contract guarantees the callee's returns never alias the receiver's
    /// own storage.
    fn with_behind_reference(mut self) -> Self {
        self.origins = self
            .origins
            .iter()
            .map(|origin| SummaryOrigin {
                behind_reference: true,
                ..origin.clone()
            })
            .collect();
        self.fields = self
            .fields
            .iter()
            .map(|field| field.clone().with_behind_reference())
            .collect();
        self
    }

    fn from_fields(fields: Vec<Self>) -> Self {
        let mut value = Self::default();
        for field in &fields {
            value.origins.extend(field.origins.iter().cloned());
            value.opaque |= field.opaque;
        }
        value.fields = fields;
        value
    }

    fn project(&self, index: usize) -> Self {
        self.fields
            .get(index)
            .cloned()
            .unwrap_or_else(|| self.flattened())
    }

    /// Narrows a flow value by one place step. Structured values (tuple,
    /// array, struct literals) narrow to the element's own flow; param-rooted
    /// values instead record the step on the origin's path, so a summary can
    /// say "the return aliases param 0's field 0" instead of flattening to
    /// the whole parameter. Paths are capped: join feedback composes adapter
    /// chains onto themselves, and without a cap the composition would grow
    /// every fixpoint round; truncating loses precision (a shorter path
    /// claims less), never soundness, and guarantees convergence.
    fn project_path(&self, projection: FlowProjection) -> Self {
        const MAX_PATH_LEN: usize = 8;
        match &projection {
            FlowProjection::Field(index) | FlowProjection::Index(Some(index)) => {
                if let Some(field) = self.fields.get(*index) {
                    return field.clone();
                }
            }
            FlowProjection::Index(None) => {
                if !self.fields.is_empty() {
                    return self.iterated();
                }
            }
        }
        Self {
            origins: self
                .origins
                .iter()
                .map(|origin| SummaryOrigin {
                    param: origin.param,
                    path: origin
                        .path
                        .iter()
                        .cloned()
                        .chain((origin.path.len() < MAX_PATH_LEN).then_some(projection.clone()))
                        .collect(),
                    kind: origin.kind,
                    behind_reference: origin.behind_reference,
                })
                .collect(),
            opaque: self.opaque,
            fields: Vec::new(),
            stores: Vec::new(),
        }
    }

    fn iterated(&self) -> Self {
        if self.fields.is_empty() {
            return self.flattened();
        }
        merge_values(self.fields.iter().cloned())
    }

    fn flattened(&self) -> Self {
        Self {
            origins: self.origins.clone(),
            opaque: self.opaque,
            fields: Vec::new(),
            stores: Vec::new(),
        }
    }

    fn retain_params(&mut self, param_count: usize) {
        self.origins.retain(|origin| origin.param < param_count);
        for field in &mut self.fields {
            field.retain_params(param_count);
        }
    }

    fn is_empty(&self) -> bool {
        self.origins.is_empty() && !self.opaque && self.fields.is_empty() && self.stores.is_empty()
    }
}

struct SummaryAnalyzer<'a> {
    hir: &'a HirFile,
    type_result: &'a TypeCheckResult,
    summaries: &'a HashMap<FunctionId, FunctionSummary>,
    /// Contracts that failed verification in an earlier build round: calls
    /// to them fall back to the conservative opaque merge.
    disabled_contracts: &'a HashSet<(hir::item_tree::TraitId, String)>,
    body_id: BodyId,
    body: &'a Body,
    /// Provenance per binding. `let`, `match` arms and `for` all land here —
    /// `PatternBindingId` is unique per pattern site, so one flat map suffices.
    locals: HashMap<PatternBindingId, FlowValue>,
    returned: FlowValue,
    stores: Vec<StoredReference>,
    /// 每层循环收集带值 break 的 provenance；while/for 压入空帧后丢弃。
    loop_break_values: Vec<Vec<FlowValue>>,
}

impl<'a> SummaryAnalyzer<'a> {
    fn new(
        hir: &'a HirFile,
        type_result: &'a TypeCheckResult,
        summaries: &'a HashMap<FunctionId, FunctionSummary>,
        disabled_contracts: &'a HashSet<(hir::item_tree::TraitId, String)>,
        body_id: BodyId,
    ) -> Self {
        Self {
            hir,
            type_result,
            summaries,
            disabled_contracts,
            body_id,
            body: &hir.bodies[body_id],
            locals: HashMap::new(),
            returned: FlowValue::default(),
            stores: Vec::new(),
            loop_break_values: Vec::new(),
        }
    }

    fn analyze_function(mut self, fid: FunctionId) -> FunctionSummary {
        let tail = self.analyze_expr(self.body.root_block);
        self.returned.merge(tail);
        let function = &self.hir.item_tree.functions[fid];
        let param_count = function.params.len();
        self.returned.retain_params(param_count);
        self.stores.retain(|store| {
            store.target_param < param_count
                && store
                    .origins
                    .iter()
                    .all(|origin| origin.param < param_count)
        });
        FunctionSummary {
            origins: self.returned.origins,
            opaque: self.returned.opaque,
            fields: self.returned.fields,
            stores: self.stores,
        }
    }

    fn analyze_expr(&mut self, expr_id: ExprId) -> FlowValue {
        let expr = &self.body.exprs[expr_id];
        let value = match expr {
            Expr::Missing
            | Expr::IntLiteral { .. }
            | Expr::FloatLiteral { .. }
            | Expr::StringLiteral { .. }
            | Expr::CharLiteral { .. }
            | Expr::BoolLiteral { .. } => FlowValue::default(),

            Expr::Path { resolved, .. } => match resolved {
                Some(ResolvedName::Param(index)) => FlowValue::from_param(*index),
                Some(ResolvedName::PatternBinding(id)) => {
                    self.locals.get(id).cloned().unwrap_or_default()
                }
                _ => FlowValue::default(),
            },

            Expr::Unary { operand, op } => {
                let operand_value = self.analyze_expr(*operand);
                match op {
                    UnaryOp::Ref | UnaryOp::MutRef => {
                        // Borrowing an empty array literal (`&[]` in
                        // `Vector::as_slice`'s empty branch) aliases no data:
                        // an empty flow, not an opaque one, so the branch
                        // does not poison the merged summary.
                        if matches!(
                            &self.body.exprs[*operand],
                            Expr::Array { elements } if elements.is_empty()
                        ) {
                            FlowValue::default()
                        } else {
                            let kind = if matches!(op, UnaryOp::Ref) {
                                FlowKind::Shared
                            } else {
                                FlowKind::Mutable
                            };
                            self.place_value(*operand)
                                .with_kind(kind)
                                .or_opaque_reference()
                        }
                    }
                    UnaryOp::Deref => operand_value,
                    _ => FlowValue::default(),
                }
            }

            Expr::Struct { fields, .. } => {
                let mut analyzed = Vec::with_capacity(fields.len());
                for field in fields {
                    let value = self.analyze_expr(field.value);
                    analyzed.push((field.name.clone(), value));
                }
                // Struct literals keep per-field flow (mapped to the declared
                // field order), so a returned struct's reference-typed fields
                // stay projectable at the call site — an iterator's `values`
                // field keeps the borrow it was constructed from.
                let struct_id = match self.type_result.expr_types.get(&(self.body_id, expr_id)) {
                    Some(Type::Struct(id, _)) => Some(*id),
                    _ => None,
                };
                match struct_id {
                    None => merge_values(analyzed.into_iter().map(|(_, value)| value)),
                    Some(id) => {
                        let declared = &self.hir.item_tree.structs[id].fields;
                        let mut slots = vec![FlowValue::default(); declared.len()];
                        for (name, value) in analyzed {
                            if let Some(slot) = declared.iter().position(|field| field.name == name)
                            {
                                slots[slot] = value;
                            }
                        }
                        FlowValue::from_fields(slots)
                    }
                }
            }

            Expr::Array { elements } | Expr::Tuple { elements } => FlowValue::from_fields(
                elements
                    .iter()
                    .map(|element| self.analyze_expr(*element))
                    .collect(),
            ),

            Expr::ArrayRepeat { value, len } => {
                let result = self.analyze_expr(*value);
                self.analyze_expr(*len);
                result
            }

            Expr::Binary { lhs, rhs, op } => self.analyze_binary(*lhs, *rhs, *op),

            Expr::Block { stmts, tail } => {
                for stmt in stmts {
                    self.analyze_stmt(*stmt);
                }
                tail.map(|tail| self.analyze_expr(tail)).unwrap_or_default()
            }

            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => self.analyze_if(*cond, *then_branch, *else_branch),

            Expr::While { condition, body } => self.analyze_while(*condition, *body),

            Expr::Loop { body } => self.analyze_loop(*body),

            Expr::For {
                pat,
                iterable,
                body,
            } => self.analyze_for(*pat, *iterable, *body),

            Expr::Match { scrutinee, arms } => self.analyze_match(*scrutinee, arms),

            Expr::Call { callee, args, .. } => self.analyze_call(*callee, args, expr_id),

            Expr::Lambda { body, .. } => {
                self.analyze_expr(*body);
                let mut result = FlowValue::default();
                if let Some(info) = self.type_result.lambda_infos.get(&(self.body_id, expr_id)) {
                    for capture in &info.captures {
                        result.merge(self.capture_value(&capture.place));
                    }
                }
                result
            }

            Expr::FieldAccess { base, field } => {
                let value = self.analyze_expr(*base);
                self.field_index(*base, field).map_or_else(
                    || value.flattened(),
                    |index| value.project_path(FlowProjection::Field(index)),
                )
            }

            Expr::IndexAccess { base, index } => {
                let value = self.analyze_expr(*base);
                self.analyze_expr(*index);
                let projection = match &self.body.exprs[*index] {
                    Expr::IntLiteral { value, .. } => usize::try_from(*value)
                        .map(|index| FlowProjection::Index(Some(index)))
                        .unwrap_or(FlowProjection::Index(None)),
                    _ => FlowProjection::Index(None),
                };
                value.project_path(projection)
            }

            Expr::Unsafe { body } => self.analyze_expr(*body),
            // A cast produces a fresh value: the operand's origins flow
            // through, but its per-field structure does not project — the
            // `(ptr, len) as &[T]` idiom must not leave the pair's two
            // slots behind for a later index step to "iterate" over.
            Expr::Cast { base, .. } => self.analyze_expr(*base).flattened(),
            Expr::Try { operand } => self.analyze_expr(*operand),
        };

        if self.expr_may_carry_provenance(expr_id) {
            value
        } else {
            FlowValue::default()
        }
    }

    fn analyze_binary(&mut self, lhs: ExprId, rhs: ExprId, op: BinaryOp) -> FlowValue {
        self.analyze_expr(lhs);
        let rhs_value = self.analyze_expr(rhs);
        let stored_origins = rhs_value.origins.clone();
        let stored_opaque = rhs_value.opaque;
        if op == BinaryOp::Assign
            && let Some((binding, direct)) = self.local_assignment(lhs)
        {
            if direct {
                self.locals.insert(binding, rhs_value);
            } else {
                self.locals.entry(binding).or_default().merge(rhs_value);
            }
        }
        if op == BinaryOp::Assign
            && let Some((target_param, target_path)) = self.store_target(lhs)
            && (!stored_origins.is_empty() || stored_opaque)
        {
            let store = StoredReference {
                target_param,
                target_path,
                origins: stored_origins,
                opaque: stored_opaque,
            };
            if !self.stores.contains(&store) {
                self.stores.push(store);
            }
        }
        FlowValue::default()
    }

    fn store_target(&self, expr_id: ExprId) -> Option<(usize, Vec<FlowProjection>)> {
        let value = self.place_value(expr_id);
        let origin = value.origins.iter().next()?;
        if value
            .origins
            .iter()
            .any(|candidate| candidate.param != origin.param || candidate.path != origin.path)
        {
            return None;
        }
        Some((origin.param, origin.path.clone()))
    }

    fn analyze_if(
        &mut self,
        cond: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
    ) -> FlowValue {
        self.analyze_expr(cond);
        let entry = self.locals.clone();
        self.locals.clone_from(&entry);
        let then_value = self.analyze_expr(then_branch);
        let then_locals = self.locals.clone();
        self.locals.clone_from(&entry);
        let else_value = else_branch
            .map(|branch| self.analyze_expr(branch))
            .unwrap_or_default();
        let else_locals = self.locals.clone();
        self.locals = merge_locals(entry, then_locals, else_locals);
        let mut result = then_value;
        result.merge(else_value);
        result
    }

    fn analyze_while(&mut self, condition: ExprId, body: ExprId) -> FlowValue {
        self.analyze_expr(condition);
        let entry = self.locals.clone();
        self.loop_break_values.push(Vec::new());
        self.analyze_expr(body);
        self.loop_break_values.pop();
        self.locals = merge_two_locals(entry, self.locals.clone());
        FlowValue::default()
    }

    fn analyze_loop(&mut self, body: ExprId) -> FlowValue {
        let entry = self.locals.clone();
        self.loop_break_values.push(Vec::new());
        self.analyze_expr(body);
        let break_values = self
            .loop_break_values
            .pop()
            .expect("loop break stack must be present");
        self.locals = merge_two_locals(entry, self.locals.clone());
        // loop 的结果值 = 所有带值 break 的 provenance 合并
        let mut result = FlowValue::default();
        for value in break_values {
            result.merge(value);
        }
        result
    }

    fn analyze_for(&mut self, pat: PatId, iterable: ExprId, body: ExprId) -> FlowValue {
        let iterable_value = self.analyze_expr(iterable).iterated();
        let entry = self.locals.clone();
        self.bind_pattern_sources(pat, &iterable_value);
        self.loop_break_values.push(Vec::new());
        self.analyze_expr(body);
        self.loop_break_values.pop();
        self.locals = merge_two_locals(entry, self.locals.clone());
        FlowValue::default()
    }

    fn analyze_match(&mut self, scrutinee: ExprId, arms: &[hir::body::MatchArm]) -> FlowValue {
        let scrutinee_value = self.analyze_expr(scrutinee);
        let entry = self.locals.clone();
        let mut result = FlowValue::default();
        let mut merged_locals = entry.clone();
        for arm in arms {
            self.locals.clone_from(&entry);
            self.bind_pattern_sources(arm.pat, &scrutinee_value);
            if let Some(guard) = arm.guard {
                self.analyze_expr(guard);
            }
            result.merge(self.analyze_expr(arm.body));
            merged_locals = merge_two_locals(merged_locals, self.locals.clone());
        }
        self.locals = merged_locals;
        result
    }

    fn capture_value(&self, place: &CapturePlace) -> FlowValue {
        let mut value = match &place.source {
            CaptureSource::Param(index) => FlowValue::from_param(*index),
            CaptureSource::Pattern(id) => self.locals.get(id).cloned().unwrap_or_default(),
            CaptureSource::LambdaParam { .. } => FlowValue {
                opaque: true,
                ..FlowValue::default()
            },
        };
        for projection in &place.projections {
            value = match projection {
                Projection::Field(index) | Projection::Index(Some(index)) => value.project(*index),
                Projection::Index(None) => value.iterated(),
            };
        }
        value
    }

    fn analyze_stmt(&mut self, stmt_id: StmtId) {
        match &self.body.stmts[stmt_id] {
            Stmt::Let {
                pat, init, else_, ..
            } => {
                let (pat, init) = (*pat, *init);
                let value = init.map(|init| self.analyze_expr(init)).unwrap_or_default();
                if let Some(else_) = else_ {
                    self.analyze_expr(*else_);
                }
                self.bind_pattern_sources(pat, &value);
            }
            Stmt::Expr { expr } => {
                self.analyze_expr(*expr);
            }
            Stmt::Return { value } => {
                if let Some(value) = value {
                    let returned = self.analyze_expr(*value);
                    self.returned.merge(returned);
                }
            }
            Stmt::Break { value } => {
                if let Some(value) = value {
                    let value = self.analyze_expr(*value);
                    if let Some(values) = self.loop_break_values.last_mut() {
                        values.push(value);
                    }
                }
            }
            Stmt::Continue | Stmt::Item { .. } => {}
        }
    }

    fn analyze_call(&mut self, callee: ExprId, args: &[ExprId], call: ExprId) -> FlowValue {
        let callee_value = self.analyze_expr(callee);
        // A value-shaped callee (`self.f(value)` — a closure or callable
        // parameter stored in a field) is invoked as a value, not as a method
        // of the receiver: the receiver base is not an input, and the result
        // can only derive from the callee's captures (its own flow) and the
        // arguments — a dyn-trait call is the one value-shaped callee whose
        // target may borrow the object itself, so it keeps the opaque merge.
        let resolves_to_function = self.resolve_callee(callee).is_some();
        let trait_call_info = self
            .type_result
            .trait_method_calls
            .get(&(self.body_id, callee))
            .map(|call| (call.dynamic, call.trait_id, call.method.clone()));
        let value_shaped_callee = !resolves_to_function
            && !matches!(
                self.body.exprs[callee],
                Expr::Path {
                    resolved: Some(ResolvedName::EnumVariant(..)),
                    ..
                }
            );
        let dyn_call = matches!(trait_call_info, Some((true, ..)));
        let mut inputs = Vec::new();
        if let Expr::FieldAccess { base, .. } = &self.body.exprs[callee]
            && (!value_shaped_callee || dyn_call)
        {
            inputs.push(self.analyze_expr(*base));
        }
        inputs.extend(args.iter().map(|arg| self.analyze_expr(*arg)));

        if matches!(
            self.body.exprs[callee],
            Expr::Path {
                resolved: Some(ResolvedName::EnumVariant(..)),
                ..
            }
        ) {
            return FlowValue::from_fields(inputs);
        }

        if let Some(fid) = self.resolve_callee(callee)
            && let Some(summary) = self.summaries.get(&fid).cloned()
        {
            self.propagate_stores(&summary, &inputs);
            return instantiate_summary(&summary, &inputs);
        }

        // Generic-bound trait dispatch on a `#[flow = "behind_reference"]`
        // contracted method: the returned references provably live behind
        // the receiver's stored references (verified against every impl
        // after the fixpoint), so the input flows carry the marker instead
        // of going opaque — adapters composing such calls keep their
        // provenance.
        if let Some(trait_call) = self
            .type_result
            .trait_method_calls
            .get(&(self.body_id, callee))
            && !trait_call.dynamic
            && !self
                .disabled_contracts
                .contains(&(trait_call.trait_id, trait_call.method.clone()))
            && trait_method_contracted(self.hir, trait_call.trait_id, &trait_call.method)
            && self.expr_may_carry_provenance(call)
        {
            if let Some(summary) =
                self.joined_trait_summary(trait_call.trait_id, &trait_call.method)
            {
                self.propagate_stores(&summary, &inputs);
            }
            let mut result = callee_value;
            result.merge(merge_values(inputs));
            return result.with_behind_reference();
        }

        if !self.expr_may_carry_provenance(call) {
            return FlowValue::default();
        }
        // The callee value's own reachable references (a closure's captures,
        // a callable parameter's innards) were constructed outside the
        // current receiver, so they alias regions behind it.
        let mut result = if value_shaped_callee {
            callee_value.with_behind_reference()
        } else {
            callee_value
        };
        if let Some((_, trait_id, method)) = trait_call_info
            && let Some(summary) = self.joined_trait_summary(trait_id, &method)
        {
            self.propagate_stores(&summary, &inputs);
        }
        result.merge(merge_values(inputs));
        if dyn_call {
            result.opaque = true;
        }
        result
    }

    fn joined_trait_summary(
        &self,
        trait_id: hir::item_tree::TraitId,
        method_name: &str,
    ) -> Option<FunctionSummary> {
        let mut joined = None;
        for imp in self.hir.item_tree.impls.values() {
            let Some(trait_range) = imp.trait_ty_range else {
                continue;
            };
            let Some(ResolvedName::Trait(id)) = self.hir.type_resolutions.get(&trait_range) else {
                continue;
            };
            if *id != trait_id {
                continue;
            }
            for method in &imp.methods {
                if self.hir.item_tree.functions[*method].name.0 != method_name {
                    continue;
                }
                if let Some(summary) = self.summaries.get(method) {
                    joined
                        .get_or_insert_with(FunctionSummary::default)
                        .merge(summary.clone());
                }
            }
        }
        joined
    }

    fn propagate_stores(&mut self, summary: &FunctionSummary, inputs: &[FlowValue]) {
        for store in &summary.stores {
            let Some(target) = inputs.get(store.target_param) else {
                continue;
            };
            let source_summary = FunctionSummary {
                origins: store.origins.clone(),
                opaque: store.opaque,
                fields: Vec::new(),
                stores: Vec::new(),
            };
            let source = instantiate_summary(&source_summary, inputs);
            if source.origins.is_empty() && !source.opaque {
                continue;
            }
            for target_origin in &target.origins {
                let mut target_path = target_origin.path.clone();
                target_path.extend(store.target_path.iter().cloned());
                let propagated = StoredReference {
                    target_param: target_origin.param,
                    target_path,
                    origins: source.origins.clone(),
                    opaque: source.opaque,
                };
                if !self.stores.contains(&propagated) {
                    self.stores.push(propagated);
                }
            }
        }
    }

    fn resolve_callee(&self, callee: ExprId) -> Option<FunctionId> {
        match self.type_result.expr_types.get(&(self.body_id, callee)) {
            Some(Type::FunctionItem { function: fid, .. })
                if self.hir.function_bodies.contains_key(fid) =>
            {
                Some(*fid)
            }
            _ => None,
        }
    }

    fn place_value(&self, expr_id: ExprId) -> FlowValue {
        // Raw pointers stay outside borrow tracking (the documented `unsafe`
        // escape hatch): borrows taken through them are opaque.
        if matches!(
            self.type_result.expr_types.get(&(self.body_id, expr_id)),
            Some(Type::Ptr { .. })
        ) {
            return FlowValue::default();
        }
        match &self.body.exprs[expr_id] {
            Expr::Path {
                resolved: Some(ResolvedName::Param(index)),
                ..
            } => FlowValue::from_param(*index),
            Expr::Path {
                resolved: Some(ResolvedName::PatternBinding(id)),
                ..
            } => self.locals.get(id).cloned().unwrap_or_default(),
            Expr::FieldAccess { base, field } => {
                let value = self.place_value(*base);
                self.field_index(*base, field).map_or_else(
                    || value.flattened(),
                    |index| value.project_path(FlowProjection::Field(index)),
                )
            }
            Expr::IndexAccess { base, index } => {
                // Indexing through a raw pointer (`self.data[index]` in
                // Vector internals) reads the pointee, which is untracked.
                if matches!(
                    self.type_result.expr_types.get(&(self.body_id, *base)),
                    Some(Type::Ptr { .. })
                ) {
                    return FlowValue::default();
                }
                let value = self.place_value(*base);
                let projection = match &self.body.exprs[*index] {
                    Expr::IntLiteral { value, .. } => usize::try_from(*value)
                        .map(|index| FlowProjection::Index(Some(index)))
                        .unwrap_or(FlowProjection::Index(None)),
                    _ => FlowProjection::Index(None),
                };
                value.project_path(projection)
            }
            Expr::Unary {
                operand,
                op: UnaryOp::Deref,
            } => {
                if matches!(
                    self.type_result.expr_types.get(&(self.body_id, *operand)),
                    Some(Type::Ptr { .. })
                ) {
                    return FlowValue::default();
                }
                self.place_value(*operand)
            }
            _ => FlowValue::default(),
        }
    }

    fn expr_may_carry_provenance(&self, expr_id: ExprId) -> bool {
        self.type_result
            .expr_types
            .get(&(self.body_id, expr_id))
            .is_none_or(|ty| type_may_carry_provenance(self.hir, ty))
    }

    fn local_assignment(&self, expr_id: ExprId) -> Option<(PatternBindingId, bool)> {
        match &self.body.exprs[expr_id] {
            Expr::Path {
                resolved: Some(ResolvedName::PatternBinding(id)),
                ..
            } => Some((*id, true)),
            Expr::FieldAccess { base, .. } | Expr::IndexAccess { base, .. } => {
                self.local_assignment(*base).map(|(id, _)| (id, false))
            }
            _ => None,
        }
    }

    fn bind_pattern_sources(&mut self, pat: PatId, value: &FlowValue) {
        match &self.body.pats[pat] {
            Pattern::Binding { .. } => {
                let binding = PatternBindingId {
                    pattern: pat,
                    field: None,
                };
                self.locals
                    .insert(binding, self.pattern_binding_value(binding, value));
            }
            Pattern::Reference { pattern, .. } => {
                self.bind_pattern_sources(*pattern, value);
            }
            Pattern::Tuple { elements } | Pattern::TupleStruct { elements, .. } => {
                for (index, element) in elements.iter().enumerate() {
                    self.bind_pattern_sources(*element, &value.project(index));
                }
            }
            Pattern::Struct { fields, .. } => {
                for (binding_index, field) in fields.iter().enumerate() {
                    let Some(index) = self.pattern_field_index(pat, &field.name) else {
                        continue;
                    };
                    let field_value = value.project(index);
                    if let Some(field_pat) = field.pat {
                        self.bind_pattern_sources(field_pat, &field_value);
                    } else {
                        let binding = PatternBindingId {
                            pattern: pat,
                            field: Some(binding_index),
                        };
                        self.locals
                            .insert(binding, self.pattern_binding_value(binding, &field_value));
                    }
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Path { .. } | Pattern::Or { .. } => {
            }
        }
    }

    fn pattern_binding_value(&self, binding: PatternBindingId, value: &FlowValue) -> FlowValue {
        match self
            .type_result
            .pattern_binding_modes
            .get(&(self.body_id, binding))
            .copied()
            .unwrap_or(PatternBindingMode::Move)
        {
            PatternBindingMode::Ref => value
                .clone()
                .with_kind(FlowKind::Shared)
                .or_opaque_reference(),
            PatternBindingMode::RefMut => value
                .clone()
                .with_kind(FlowKind::Mutable)
                .or_opaque_reference(),
            PatternBindingMode::Move
                if self
                    .type_result
                    .pattern_binding_types
                    .get(&(self.body_id, binding))
                    .is_none_or(|ty| type_may_carry_provenance(self.hir, ty)) =>
            {
                value.clone()
            }
            PatternBindingMode::Move => FlowValue::default(),
        }
    }

    fn field_index(&self, base: ExprId, field: &hir::Name) -> Option<usize> {
        match self.type_result.expr_types.get(&(self.body_id, base))? {
            Type::Ref(inner, _) => self.field_index_for_type(inner, field),
            ty => self.field_index_for_type(ty, field),
        }
    }

    fn field_index_for_type(&self, ty: &Type, field: &hir::Name) -> Option<usize> {
        match ty {
            Type::Struct(id, _) => self.hir.item_tree.structs[*id]
                .fields
                .iter()
                .position(|item| item.name == *field),
            Type::Tuple(elements) => field
                .0
                .parse::<usize>()
                .ok()
                .filter(|index| *index < elements.len()),
            _ => None,
        }
    }

    fn pattern_field_index(&self, pat: PatId, field: &hir::Name) -> Option<usize> {
        match self.type_result.pattern_types.get(&(self.body_id, pat))? {
            Type::Struct(id, _) => self.hir.item_tree.structs[*id]
                .fields
                .iter()
                .position(|item| item.name == *field),
            Type::Enum(id, _) => {
                let Pattern::Struct { path, .. } = &self.body.pats[pat] else {
                    return None;
                };
                let name = path.segments.last()?;
                let variant = self.hir.item_tree.enums[*id]
                    .variants
                    .iter()
                    .find(|variant| variant.name == *name)?;
                let hir::item_tree::HirVariantKind::Struct(fields) = &variant.kind else {
                    return None;
                };
                fields.iter().position(|item| item.name == *field)
            }
            _ => None,
        }
    }
}

trait OpaqueReference {
    fn or_opaque_reference(self) -> Self;
}

impl OpaqueReference for FlowValue {
    fn or_opaque_reference(mut self) -> Self {
        if self.origins.is_empty() {
            self.opaque = true;
        }
        self
    }
}

fn merge_values(values: impl IntoIterator<Item = FlowValue>) -> FlowValue {
    let mut result = FlowValue::default();
    for value in values {
        result.merge(value);
    }
    result
}

fn instantiate_summary(summary: &FunctionSummary, inputs: &[FlowValue]) -> FlowValue {
    let mut result = FlowValue::default();
    for origin in &summary.origins {
        let Some(input) = inputs.get(origin.param) else {
            continue;
        };
        // Compose the callee's field path onto the caller's input: a
        // structured input narrows per field; a param-rooted input records
        // the steps on its own origin path.
        let mut projected = input.clone();
        for projection in &origin.path {
            projected = projected.project_path(projection.clone());
        }
        result.merge(match origin.kind {
            FlowKind::Inherit => projected,
            kind => projected.with_kind(kind),
        });
    }
    if !summary.fields.is_empty() {
        result.fields = summary
            .fields
            .iter()
            .map(|field| instantiate_summary(field, inputs))
            .collect();
    }
    if summary.opaque {
        result.merge(merge_values(inputs.iter().cloned()));
        result.opaque = true;
    }
    result
}

fn merge_two_locals(
    mut left: HashMap<PatternBindingId, FlowValue>,
    right: HashMap<PatternBindingId, FlowValue>,
) -> HashMap<PatternBindingId, FlowValue> {
    for (binding, value) in right {
        left.entry(binding).or_default().merge(value);
    }
    left
}

fn merge_locals(
    entry: HashMap<PatternBindingId, FlowValue>,
    left: HashMap<PatternBindingId, FlowValue>,
    right: HashMap<PatternBindingId, FlowValue>,
) -> HashMap<PatternBindingId, FlowValue> {
    merge_two_locals(merge_two_locals(entry, left), right)
}

pub fn type_may_carry_reference(hir: &HirFile, ty: &Type) -> bool {
    type_may_carry_flow(hir, ty, false)
}

fn type_may_carry_provenance(hir: &HirFile, ty: &Type) -> bool {
    type_may_carry_flow(hir, ty, true)
}

fn type_may_carry_flow(hir: &HirFile, ty: &Type, through_raw_pointer: bool) -> bool {
    match ty {
        Type::Ref(..)
        | Type::DynTrait { .. }
        | Type::OwnedDynTrait { .. }
        | Type::Closure { .. }
        | Type::OpaqueCallable { .. }
        | Type::OpaqueTrait { .. }
        | Type::Param(..)
        | Type::InferVar(..)
        | Type::Unknown
        | Type::Error => true,
        Type::Ptr { .. } => through_raw_pointer,
        Type::Tuple(elements) => elements
            .iter()
            .any(|element| type_may_carry_flow(hir, element, through_raw_pointer)),
        Type::Slice(inner) | Type::Array(inner, _) => {
            type_may_carry_flow(hir, inner, through_raw_pointer)
        }
        Type::Struct(id, args) => {
            args.iter()
                .any(|arg| type_may_carry_flow(hir, arg, through_raw_pointer))
                || hir_struct_may_carry_flow(hir, *id, through_raw_pointer, &mut HashSet::new())
        }
        Type::Enum(id, args) => {
            args.iter()
                .any(|arg| type_may_carry_flow(hir, arg, through_raw_pointer))
                || hir_enum_may_carry_flow(hir, *id, through_raw_pointer, &mut HashSet::new())
        }
        Type::CallableConstraint(signature) => {
            signature
                .params
                .iter()
                .any(|param| type_may_carry_flow(hir, param, through_raw_pointer))
                || type_may_carry_flow(hir, &signature.ret, through_raw_pointer)
        }
        Type::FunctionItem { .. }
        | Type::Int(..)
        | Type::Float(..)
        | Type::InferInt
        | Type::InferFloat
        | Type::Bool
        | Type::Str
        | Type::Char
        | Type::Unit
        | Type::Never
        | Type::Const(..) => false,
    }
}

fn hir_struct_may_carry_flow(
    hir: &HirFile,
    id: StructId,
    through_raw_pointer: bool,
    visiting: &mut HashSet<TextRange>,
) -> bool {
    hir.item_tree.structs[id]
        .fields
        .iter()
        .any(|field| hir_type_may_carry_flow(hir, &field.ty, through_raw_pointer, visiting))
}

fn hir_enum_may_carry_flow(
    hir: &HirFile,
    id: EnumId,
    through_raw_pointer: bool,
    visiting: &mut HashSet<TextRange>,
) -> bool {
    hir.item_tree.enums[id]
        .variants
        .iter()
        .any(|variant| match &variant.kind {
            hir::item_tree::HirVariantKind::Unit => false,
            hir::item_tree::HirVariantKind::Tuple(fields) => fields
                .iter()
                .any(|field| hir_type_may_carry_flow(hir, field, through_raw_pointer, visiting)),
            hir::item_tree::HirVariantKind::Struct(fields) => fields.iter().any(|field| {
                hir_type_may_carry_flow(hir, &field.ty, through_raw_pointer, visiting)
            }),
        })
}

fn hir_type_may_carry_flow(
    hir: &HirFile,
    ty: &hir::item_tree::HirTypeRef,
    through_raw_pointer: bool,
    visiting: &mut HashSet<TextRange>,
) -> bool {
    match ty {
        hir::item_tree::HirTypeRef::Ref(..) | hir::item_tree::HirTypeRef::DynTrait { .. } => true,
        hir::item_tree::HirTypeRef::Ptr { .. } => through_raw_pointer,
        hir::item_tree::HirTypeRef::Tuple(elements) => elements
            .iter()
            .any(|element| hir_type_may_carry_flow(hir, element, through_raw_pointer, visiting)),
        hir::item_tree::HirTypeRef::Slice(inner) | hir::item_tree::HirTypeRef::Array(inner, _) => {
            hir_type_may_carry_flow(hir, inner, through_raw_pointer, visiting)
        }
        hir::item_tree::HirTypeRef::ImplTrait {
            trait_ty, callable, ..
        } => {
            hir_type_may_carry_flow(hir, trait_ty, through_raw_pointer, visiting)
                || callable.as_ref().is_some_and(|signature| {
                    signature.params.iter().any(|param| {
                        hir_type_may_carry_flow(hir, param, through_raw_pointer, visiting)
                    }) || hir_type_may_carry_flow(
                        hir,
                        &signature.ret,
                        through_raw_pointer,
                        visiting,
                    )
                })
        }
        hir::item_tree::HirTypeRef::Named(path) => {
            if path
                .type_args
                .iter()
                .any(|arg| hir_type_may_carry_flow(hir, arg, through_raw_pointer, visiting))
            {
                return true;
            }
            if !visiting.insert(path.range) {
                return false;
            }
            let carries_flow = match hir.type_resolutions.get(&path.range) {
                Some(ResolvedName::Struct(id)) => {
                    hir_struct_may_carry_flow(hir, *id, through_raw_pointer, visiting)
                }
                Some(ResolvedName::Enum(id)) => {
                    hir_enum_may_carry_flow(hir, *id, through_raw_pointer, visiting)
                }
                Some(ResolvedName::TypeAlias(id)) => hir.item_tree.type_aliases[*id]
                    .ty
                    .as_ref()
                    .is_some_and(|ty| {
                        hir_type_may_carry_flow(hir, ty, through_raw_pointer, visiting)
                    }),
                _ => false,
            };
            visiting.remove(&path.range);
            carries_flow
        }
        hir::item_tree::HirTypeRef::Never
        | hir::item_tree::HirTypeRef::Const(_)
        | hir::item_tree::HirTypeRef::Unknown
        | hir::item_tree::HirTypeRef::Error => false,
    }
}
