use std::collections::{HashMap, HashSet};

use rowan::TextRange;

use hir::{
    HirFile,
    body::{
        Body, BodyId, Expr, ExprId, PatId, Pattern, PatternBindingId, ResolvedName, SourceMap,
        Stmt, StmtId, UnaryOp,
    },
    item_tree::{FunctionId, HirTypeRef},
    place::Place,
};
use ty::{
    CaptureMode, CapturePlace, CaptureSource, ClosureKind, Diagnostic, LabelStyle, LambdaInfo,
    PatternBindingMode, Severity, SourceLabel, TraitBound, TraitEnv, Type, TypeCheckResult,
    ValueUse,
};

mod initialization;
mod reference_flow;

use reference_flow::{
    FlowKind, FlowProjection, FunctionSummary, ReferenceFlow, SummaryOrigin,
    type_may_carry_reference,
};

type LoanId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BorrowKind {
    Shared,
    Mutable,
}

impl BorrowKind {
    const fn from_flow(kind: FlowKind, inherited: Self) -> Self {
        match kind {
            FlowKind::Inherit => inherited,
            FlowKind::Shared => Self::Shared,
            FlowKind::Mutable => Self::Mutable,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum AccessRoot {
    /// Any binding introduced by a pattern — a `let`, a `match` arm, or a `for`
    /// loop. `let` has no separate root because every `let` carries a pattern.
    Pattern(PatternBindingId),
    Param(usize),
    LambdaParam {
        lambda: ExprId,
        index: usize,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum AccessProjection {
    Field(usize),
    Index(Option<usize>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct AccessPlace {
    root: AccessRoot,
    projections: Vec<AccessProjection>,
}

/// Longest projection chain an [`AccessPlace`] keeps.
///
/// A loop that reassigns a value derived from its own place — `current = next`
/// where `next` was read out of `current.link`, with `Link::Next(&Node)` making
/// the type recursive — composes one more projection per fixpoint round. The
/// loop head then never stops changing, because each round's borrow sits on a
/// strictly deeper place and so gets a fresh loan. Truncating keeps a prefix of
/// the real place, and a prefix covers at least as much storage as the place it
/// came from: an overlap can only be reported more often, never missed.
const MAX_PLACE_PROJECTIONS: usize = 8;

impl AccessPlace {
    const fn new(root: AccessRoot) -> Self {
        Self {
            root,
            projections: Vec::new(),
        }
    }

    fn field(mut self, index: usize) -> Self {
        if self.projections.len() < MAX_PLACE_PROJECTIONS {
            self.projections.push(AccessProjection::Field(index));
        }
        self
    }

    fn index(mut self, index: Option<usize>) -> Self {
        if self.projections.len() < MAX_PLACE_PROJECTIONS {
            self.projections.push(AccessProjection::Index(index));
        }
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Origin {
    place: AccessPlace,
    kind: BorrowKind,
    loan: LoanId,
}

type Origins = HashSet<Origin>;

#[derive(Debug, Clone, Default)]
struct OriginValue {
    origins: Origins,
    fields: Vec<Self>,
}

impl OriginValue {
    const fn from_origins(origins: Origins) -> Self {
        Self {
            origins,
            fields: Vec::new(),
        }
    }

    fn from_fields(fields: Vec<Self>) -> Self {
        let origins = fields
            .iter()
            .flat_map(|field| field.origins.iter().cloned())
            .collect();
        Self { origins, fields }
    }

    /// Project the value onto field `index`.
    ///
    /// When the per-field structure is unknown, the projected value can only
    /// reach that field's data, so each origin's place gains a `Field(index)`
    /// projection instead of staying at the (whole-root) base place — keeping
    /// the loan precise for borrows taken through a reference-typed field
    /// read (`self.source.as_bytes()` borrows `self.source`, not `self`).
    fn project(&self, index: usize) -> Self {
        if let Some(field) = self.fields.get(index) {
            return field.clone();
        }
        Self::from_origins(
            self.origins
                .iter()
                .map(|origin| Origin {
                    place: origin.place.clone().field(index),
                    kind: origin.kind,
                    loan: origin.loan,
                })
                .collect(),
        )
    }

    fn iterated(&self) -> Self {
        if self.fields.is_empty() {
            return self.flattened();
        }
        let mut value = Self::default();
        for field in self.fields.iter().cloned() {
            value.merge(field);
        }
        value
    }

    fn flattened(&self) -> Self {
        Self::from_origins(self.origins.clone())
    }

    fn merge(&mut self, other: Self) {
        if self.origins.is_empty() && self.fields.is_empty() {
            *self = other;
            return;
        }
        if other.origins.is_empty() && other.fields.is_empty() {
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
    }
}

fn projected_origin_value(
    value: &OriginValue,
    projection: &[AccessProjection],
    index: usize,
) -> (OriginValue, Vec<AccessProjection>) {
    if let Some(field) = value.fields.get(index) {
        return (field.clone(), Vec::new());
    }
    let mut projection = projection.to_vec();
    projection.push(AccessProjection::Field(index));
    (value.flattened(), projection)
}

#[derive(Debug, Clone)]
struct AccessTarget {
    place: AccessPlace,
    parents: HashSet<LoanId>,
}

#[derive(Debug, Default)]
pub struct AnalysisResult {
    pub diagnostics: Vec<Diagnostic>,
    pub moved_exprs: HashSet<(BodyId, ExprId)>,
}

/// Run move/borrow checking. Escape analysis identifies storage duration only;
/// heap allocation does not relax move or borrow rules.
#[must_use]
pub fn analyze(hir: &HirFile, type_result: &TypeCheckResult) -> AnalysisResult {
    let reference_flow = ReferenceFlow::build(hir, type_result);
    let mut result = AnalysisResult::default();
    initialization::check(hir, type_result, &mut result);
    let mut a = Analyzer {
        hir,
        type_result,
        trait_env: &type_result.trait_env,
        reference_flow: &reference_flow,
        result,
        loop_break_values: Vec::new(),
        loop_break_states: Vec::new(),
        diagnostic_suppression: 0,
        interior_writes: HashMap::new(),
    };
    a.compute_interior_writes();
    a.analyze_all_bodies();
    a.result
}

/// break 跳出点的 move 状态快照，用于合并 `loop` 出口状态。
struct MoveStateSnapshot {
    bindings: MoveBindings,
    moved_places: HashSet<Place>,
    moved_sites: HashMap<Place, (Option<TextRange>, String)>,
}

struct Analyzer<'a> {
    hir: &'a HirFile,
    type_result: &'a TypeCheckResult,
    trait_env: &'a TraitEnv,
    reference_flow: &'a ReferenceFlow,
    result: AnalysisResult,
    /// 每层 `loop` 收集带值 break 的操作数与跳出点的 move 状态。
    /// while/for 也压入空帧，保证内层 break 不会泄漏到外层 loop。
    loop_break_values: Vec<Vec<ExprId>>,
    loop_break_states: Vec<Vec<MoveStateSnapshot>>,
    /// 循环不动点迭代（含首遍）期间抑制诊断：迭代中产生的诊断基于
    /// 尚未收敛的 move 状态，直接上报会既重复又可能基于中间态。
    /// 收敛后每个循环用收敛态完整重放一遍（此计数归零）再发诊断，
    /// 保证依赖回边状态的借用/移动错误也恰好上报一次。
    diagnostic_suppression: usize,
    /// Which `mut` fields each callable may write, per parameter. A call site
    /// reads it to learn which of the receiver's or an argument's `mut` fields
    /// the callee can move, so a live borrow into one of them is rejected.
    interior_writes: HashMap<FunctionId, ParamWrites>,
}

/// The `mut` field paths one callable may write, relative to its parameters.
///
/// A path is a chain of field indices from the parameter root, truncated at the
/// first `mut` field on it — writing below that field is covered by it. `None`
/// records "unknown", which a caller reads as every `mut` field of that
/// parameter.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct ParamWrites {
    params: HashMap<usize, Option<HashSet<Vec<usize>>>>,
}

impl ParamWrites {
    fn record(&mut self, param: usize, path: Vec<usize>) -> bool {
        match self.params.entry(param) {
            std::collections::hash_map::Entry::Occupied(mut entry) => match entry.get_mut() {
                None => false,
                Some(paths) => paths.insert(path),
            },
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(Some(std::iter::once(path).collect()));
                true
            }
        }
    }

    fn record_unknown(&mut self, param: usize) -> bool {
        self.params.insert(param, None) != Some(None)
    }
}

/// How the place an expression reaches relates to the enclosing function's
/// parameters.
enum PlaceShape {
    /// Rooted at parameter `param` through `steps` field accesses. `indexed`
    /// records that an index projection appeared before the first field step,
    /// which makes a field-index chain from the root meaningless.
    Known {
        param: usize,
        steps: Vec<(ExprId, usize)>,
        indexed: bool,
    },
    /// Rooted at parameter `param`, but the path cannot be expressed.
    Opaque { param: usize },
    /// Not rooted at a parameter.
    Other,
}

/// What a callable is known to write on one of its parameters.
enum WrittenFields<'a> {
    /// No concrete callee, or the callee's own answer is unknown.
    Unknown,
    /// Nothing: the callee's body was analysed and writes no `mut` field here.
    Nothing,
    Paths(&'a HashSet<Vec<usize>>),
}

impl Analyzer<'_> {
    /// Computes, for every callable with a body, which `mut` fields of its
    /// parameters it may write — including writes performed by the callables it
    /// passes those places to.
    ///
    /// A call site uses this to decide whether invoking a callable can move
    /// storage a live borrow points into. Without it the only sound answer is
    /// "every `mut` field of every type it might touch", which rejects calls
    /// that write a different field, or nothing at all.
    fn compute_interior_writes(&mut self) {
        let order = self
            .hir
            .item_tree
            .functions
            .iter()
            .filter_map(|(fid, _)| self.hir.function_bodies.get(&fid).map(|body| (fid, *body)))
            .collect::<Vec<_>>();
        let mut summaries: HashMap<FunctionId, ParamWrites> = HashMap::new();
        // The lattice only grows, so this converges; the cap is a guard against
        // an unexpectedly long chain, and falling back to "unknown" keeps the
        // result sound if it is ever hit.
        for _ in 0..32 {
            let mut changed = false;
            let mut next = HashMap::with_capacity(order.len());
            for (fid, body_id) in &order {
                let writes = self.body_interior_writes(*fid, *body_id, &summaries);
                if summaries.get(fid) != Some(&writes) {
                    changed = true;
                }
                next.insert(*fid, writes);
            }
            summaries = next;
            if !changed {
                self.interior_writes = summaries;
                return;
            }
        }
        for (fid, writes) in &mut summaries {
            let _ = fid;
            *writes = ParamWrites::default();
            writes.params.insert(0, None);
        }
        self.interior_writes = summaries;
    }

    /// The `mut` fields one body may write, relative to that function's
    /// parameters.
    fn body_interior_writes(
        &self,
        function_id: FunctionId,
        body_id: BodyId,
        summaries: &HashMap<FunctionId, ParamWrites>,
    ) -> ParamWrites {
        let body = &self.hir.bodies[body_id];
        let bounds = self
            .type_result
            .body_bounds
            .get(&body_id)
            .map_or(&[][..], Vec::as_slice);
        let ctx = BodyCtx::new(function_id, body_id, body, bounds);
        let mut writes = ParamWrites::default();
        for (_, expr) in body.exprs.iter() {
            match expr {
                Expr::Binary { lhs, op, .. } if op.is_assignment() => {
                    self.record_place_write(&ctx, *lhs, &mut writes);
                }
                Expr::Unary {
                    operand,
                    op: UnaryOp::MutRef,
                } => self.record_place_write(&ctx, *operand, &mut writes),
                Expr::Call { callee, args, .. } => {
                    self.record_call_writes(&ctx, *callee, args, summaries, &mut writes);
                }
                _ => {}
            }
        }
        writes
    }

    /// Records the `mut` field `place_expr` writes, if it is parameter-rooted.
    fn record_place_write(&self, ctx: &BodyCtx<'_>, place_expr: ExprId, writes: &mut ParamWrites) {
        match self.parameter_place(ctx, place_expr) {
            PlaceShape::Known {
                param,
                steps,
                indexed: false,
            } => {
                if let Some(path) = self.mut_field_prefix(ctx, &steps) {
                    writes.record(param, path);
                }
            }
            // An index projection before the first field step, or an
            // unresolvable field, makes the written field path inexpressible.
            // The place itself is still known to be inside `param`.
            PlaceShape::Known { param, .. } | PlaceShape::Opaque { param } => {
                writes.record_unknown(param);
            }
            PlaceShape::Other => {}
        }
    }

    /// Propagates what the callee writes onto the places this body hands it.
    fn record_call_writes(
        &self,
        ctx: &BodyCtx<'_>,
        callee: ExprId,
        args: &[ExprId],
        summaries: &HashMap<FunctionId, ParamWrites>,
        writes: &mut ParamWrites,
    ) {
        let (inputs, modes, fid, _trait_call) = self.call_signature(ctx, callee, args);
        // A `&mut` argument hands the callee full write access to that place,
        // whatever the callee's own body does with it — `holder.items.push(..)`
        // writes `items` because `push` takes `&mut self`, not because `items`
        // is a `mut` field of the callee's own type.
        for (index, input) in inputs.iter().enumerate() {
            if modes.get(index).copied().flatten() == Some(BorrowKind::Mutable) {
                self.record_place_write(ctx, peel_reference(ctx, *input), writes);
            }
        }
        let Some(summary) = fid.and_then(|fid| summaries.get(&fid)) else {
            // No body to summarise (an extern) or no concrete callee (dynamic
            // dispatch, a function pointer, a generic bound): assume every
            // `mut` field of the places handed over.
            for (index, input) in inputs.iter().enumerate() {
                if modes.get(index).copied().flatten() != Some(BorrowKind::Mutable) {
                    self.record_opaque_argument(ctx, *input, writes);
                }
            }
            return;
        };
        for (index, input) in inputs.iter().enumerate() {
            if modes.get(index).copied().flatten() == Some(BorrowKind::Mutable) {
                continue;
            }
            match summary.params.get(&index) {
                None => {}
                Some(None) => self.record_opaque_argument(ctx, *input, writes),
                Some(Some(paths)) => {
                    for path in paths {
                        self.record_mapped_write(ctx, *input, path, writes);
                    }
                }
            }
        }
    }

    /// Marks the caller's parameter holding `input` as an unknown write, but
    /// only when that place can carry `mut` fields at all — a call through a
    /// type parameter cannot write fields of `T`.
    fn record_opaque_argument(&self, ctx: &BodyCtx<'_>, input: ExprId, writes: &mut ParamWrites) {
        let place_expr = peel_reference(ctx, input);
        if self
            .mut_field_indices(ctx, place_expr)
            .is_none_or(|fields| fields.is_empty())
        {
            return;
        }
        match self.parameter_place(ctx, place_expr) {
            PlaceShape::Known { param, .. } | PlaceShape::Opaque { param } => {
                writes.record_unknown(param);
            }
            PlaceShape::Other => {}
        }
    }

    /// Maps a path the callee writes on this argument back onto the caller.
    fn record_mapped_write(
        &self,
        ctx: &BodyCtx<'_>,
        input: ExprId,
        callee_path: &[usize],
        writes: &mut ParamWrites,
    ) {
        let place_expr = peel_reference(ctx, input);
        match self.parameter_place(ctx, place_expr) {
            PlaceShape::Known {
                param,
                steps,
                indexed: false,
            } => {
                if let Some(prefix) = self.mut_field_prefix(ctx, &steps) {
                    // The caller's own path already lands on a `mut` field, so
                    // that field covers whatever the callee does below it.
                    writes.record(param, prefix);
                } else {
                    let mut path = steps.iter().map(|(_, index)| *index).collect::<Vec<_>>();
                    path.extend_from_slice(callee_path);
                    writes.record(param, path);
                }
            }
            PlaceShape::Known { param, .. } | PlaceShape::Opaque { param } => {
                writes.record_unknown(param);
            }
            PlaceShape::Other => {}
        }
    }

    /// Walks an expression down to the parameter it is rooted at, collecting
    /// the field accesses on the way.
    fn parameter_place(&self, ctx: &BodyCtx<'_>, expr: ExprId) -> PlaceShape {
        match &ctx.body.exprs[expr] {
            Expr::Path {
                resolved: Some(ResolvedName::Param(index)),
                ..
            } => PlaceShape::Known {
                param: *index,
                steps: Vec::new(),
                indexed: false,
            },
            Expr::FieldAccess { base, field } => {
                let index = self.resolve_field_index(ctx.body_id, *base, field);
                match self.parameter_place(ctx, *base) {
                    PlaceShape::Known {
                        param,
                        mut steps,
                        indexed,
                    } => match index {
                        Some(index) => {
                            steps.push((expr, index));
                            PlaceShape::Known {
                                param,
                                steps,
                                indexed,
                            }
                        }
                        None => PlaceShape::Opaque { param },
                    },
                    PlaceShape::Opaque { param } => PlaceShape::Opaque { param },
                    PlaceShape::Other => PlaceShape::Other,
                }
            }
            Expr::IndexAccess { base, .. } => match self.parameter_place(ctx, *base) {
                PlaceShape::Known {
                    param,
                    steps,
                    indexed: _,
                } => PlaceShape::Known {
                    param,
                    steps,
                    indexed: true,
                },
                PlaceShape::Opaque { param } => PlaceShape::Opaque { param },
                PlaceShape::Other => PlaceShape::Other,
            },
            _ => PlaceShape::Other,
        }
    }

    /// The shortest prefix of `steps` that ends on a `mut` field, as a chain of
    /// field indices.
    fn mut_field_prefix(&self, ctx: &BodyCtx<'_>, steps: &[(ExprId, usize)]) -> Option<Vec<usize>> {
        let mut path = Vec::new();
        for (expr, index) in steps {
            path.push(*index);
            if self.field_is_mut(ctx, *expr) == Some(true) {
                return Some(path);
            }
        }
        None
    }

    /// Field indices of the `mut` fields of `expr`'s type, or `None` when that
    /// type is not a struct.
    fn mut_field_indices(&self, ctx: &BodyCtx<'_>, expr: ExprId) -> Option<Vec<usize>> {
        let ty = self.type_result.expr_types.get(&(ctx.body_id, expr))?;
        let ty = match ty {
            Type::Ref(inner, _) => inner.as_ref(),
            ty => ty,
        };
        let Type::Struct(struct_id, _) = ty else {
            return None;
        };
        Some(
            self.hir.item_tree.structs[*struct_id]
                .fields
                .iter()
                .enumerate()
                .filter(|(_, field)| field.is_mut)
                .map(|(index, _)| index)
                .collect(),
        )
    }

    fn analyze_all_bodies(&mut self) {
        for (fid, _) in self.hir.item_tree.functions.iter() {
            if let Some(body_id) = self.hir.function_bodies.get(&fid).copied() {
                self.analyze_body(fid, body_id);
            }
        }
    }

    fn analyze_body(&mut self, function_id: FunctionId, body_id: BodyId) {
        let body = &self.hir.bodies[body_id];
        let bounds = self
            .type_result
            .body_bounds
            .get(&body_id)
            .map_or(&[][..], Vec::as_slice);
        let mut ctx = BodyCtx::new(function_id, body_id, body, bounds);
        ctx.seed_params(
            self.hir.item_tree.functions[function_id]
                .params
                .iter()
                .map(|param| param.name.0.as_str()),
        );
        ctx.seed_reference_params(
            self.hir.item_tree.functions[function_id]
                .params
                .iter()
                .enumerate(),
        );
        self.move_check_body(&mut ctx);
    }

    // ═══════════════════════════════════════════════════════════
    // Move checking
    // ═══════════════════════════════════════════════════════════

    fn move_check_body(&mut self, ctx: &mut BodyCtx<'_>) {
        self.move_check_expr(ctx, ctx.body.root_block);
        if let Expr::Block {
            tail: Some(tail), ..
        } = &ctx.body.exprs[ctx.body.root_block]
        {
            self.check_returned_drop_borrow(ctx, *tail);
            self.apply_recorded_value_use(ctx, *tail);
        }
    }

    fn move_check_expr(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) {
        let span = ctx.expr_range(expr_id);
        match &ctx.body.exprs[expr_id] {
            Expr::Missing
            | Expr::IntLiteral { .. }
            | Expr::FloatLiteral { .. }
            | Expr::StringLiteral { .. }
            | Expr::CharLiteral { .. }
            | Expr::BoolLiteral { .. } => {}

            Expr::Path { .. } => self.move_check_path(ctx, expr_id, span),

            Expr::Struct { .. } => self.move_check_struct(ctx, expr_id),

            Expr::Binary { .. } => self.move_check_binary(ctx, expr_id, span),

            Expr::Unary { .. } => self.move_check_unary(ctx, expr_id, span),

            Expr::Block { .. } => self.move_check_block(ctx, expr_id),

            Expr::If { .. } => self.move_check_if(ctx, expr_id),

            Expr::While { .. } => self.move_check_while(ctx, expr_id),

            Expr::Loop { .. } => self.move_check_loop(ctx, expr_id),

            Expr::For { .. } => self.move_check_for(ctx, expr_id),

            Expr::Match { .. } => self.move_check_match(ctx, expr_id),

            Expr::Array { .. } | Expr::Tuple { .. } | Expr::ArrayRepeat { .. } => {
                self.move_check_aggregate(ctx, expr_id);
            }

            Expr::Call { .. } => self.move_check_call(ctx, expr_id, span),

            Expr::Lambda { .. } => self.move_check_lambda(ctx, expr_id),

            Expr::Unsafe { .. } | Expr::Cast { .. } | Expr::Try { .. } => {
                self.move_check_passthrough(ctx, expr_id);
            }

            Expr::FieldAccess { .. } | Expr::IndexAccess { .. } => {
                self.move_check_projection(ctx, expr_id, span);
            }
        }
        ctx.release_expired_locals(expr_id);
    }

    fn move_check_path(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId, span: Option<TextRange>) {
        let Expr::Path { path, resolved } = ctx.body.exprs[expr_id].clone() else {
            unreachable!("expected path expression");
        };
        let value = match &resolved {
            Some(ResolvedName::PatternBinding(id)) => ctx.local_origin_value(*id),
            Some(ResolvedName::Param(index)) => {
                OriginValue::from_origins(ctx.param_origins.get(index).cloned().unwrap_or_default())
            }
            _ => OriginValue::default(),
        };
        ctx.set_expr_origin_value(expr_id, value);
        if let Some(name) = path.as_single_name()
            && let Some(moved) = ctx.bindings.get(&name.0)
        {
            if *moved {
                let extra = resolved
                    .as_ref()
                    .and_then(|resolved| match resolved {
                        ResolvedName::PatternBinding(id) => {
                            Some(Self::move_site_labels(ctx, &Place::root(*id)))
                        }
                        _ => None,
                    })
                    .unwrap_or_default();
                self.diag_with_labels(
                    format!("use of moved value: `{}`", name.0),
                    span,
                    "E0100",
                    &extra,
                );
            }
            // A not-yet-moved parameter may still be partially moved: field
            // moves on parameters record places without marking the name, so
            // a whole-parameter use must check place overlap here, where the
            // early return would otherwise skip the pattern-binding check.
            if !*moved
                && !matches!(resolved, Some(ResolvedName::PatternBinding(_)))
                && let Some(place) = self.place_from_expr(ctx, expr_id)
                && place.projections.is_empty()
                && ctx
                    .moved_places
                    .iter()
                    .any(|moved_place| place_overlaps(moved_place, &place))
            {
                let extra = Self::move_site_labels(ctx, &place);
                self.diag_with_labels(
                    format!("use of moved value: `{}`", name.0),
                    span,
                    "E0100",
                    &extra,
                );
            }
            if let Some(ResolvedName::PatternBinding(id)) = resolved {
                ctx.release_local_if_dead(id, expr_id);
            }
            return;
        }
        if let Some(place) = self.place_from_expr(ctx, expr_id)
            && place.projections.is_empty()
        {
            // Whole-place use: reject when any part of it was already moved
            // (a partially moved parameter or binding). Parameters take the
            // same path as pattern bindings — their whole-move used to skip
            // this check, silently re-copying a torn value whose moved field
            // was then dropped a second time.
            if ctx
                .moved_places
                .iter()
                .any(|moved| place_overlaps(moved, &place))
            {
                let label = path.as_single_name().map_or("_", |name| name.0.as_str());
                let extra = Self::move_site_labels(ctx, &place);
                self.diag_with_labels(
                    format!("use of moved value: `{label}`"),
                    span,
                    "E0100",
                    &extra,
                );
            }
            if let Some(ResolvedName::PatternBinding(id)) = resolved {
                ctx.release_local_if_dead(id, expr_id);
            }
        }
    }

    fn move_check_struct(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) {
        let Expr::Struct { fields, .. } = ctx.body.exprs[expr_id].clone() else {
            unreachable!("expected struct expression");
        };
        let mut origins = Origins::new();
        for field in &fields {
            self.move_check_expr(ctx, field.value);
            self.apply_recorded_value_use(ctx, field.value);
            origins.extend(ctx.expr_origin_value(field.value).origins);
        }
        let value = if self.expr_may_carry_reference(ctx, expr_id) {
            OriginValue::from_origins(origins)
        } else {
            for field in &fields {
                Self::deactivate_unretained(ctx, field.value, &HashSet::new());
            }
            OriginValue::default()
        };
        ctx.set_expr_origin_value(expr_id, value);
    }

    fn move_check_binary(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        span: Option<TextRange>,
    ) {
        let Expr::Binary { lhs, rhs, op } = ctx.body.exprs[expr_id] else {
            unreachable!("expected binary expression");
        };
        let direct_binding = (op == hir::body::BinaryOp::Assign)
            .then(|| Self::local_assignment(ctx, lhs))
            .flatten()
            .filter(|(_, direct)| *direct)
            .map(|(binding, _)| binding);
        let lhs_place = if let Some(binding) = direct_binding {
            // Whole-local assignment (`x = v`): the old value is not read, and
            // the binding is re-initialized below.
            ctx.release_local_if_dead(binding, lhs);
            self.place_from_expr(ctx, lhs)
        } else if op == hir::body::BinaryOp::Assign {
            // A plain assignment writes its left-hand side without reading
            // it: only a move of a place *containing* the target is an error
            // (assigning into a wholly moved value), while moved places the
            // target covers are re-initialized below.
            if let Some(place) = self.place_from_expr(ctx, lhs) {
                if let Some(moved) = ctx
                    .moved_places
                    .iter()
                    .find(|moved| moved.is_prefix_of(&place))
                {
                    let name = Self::expr_name(ctx, lhs);
                    let extra = Self::move_site_labels(ctx, moved);
                    self.diag_with_labels(
                        format!("use of moved value: `{name}`"),
                        span,
                        "E0100",
                        &extra,
                    );
                }
                Some(place)
            } else {
                self.move_check_expr(ctx, lhs);
                None
            }
        } else {
            self.move_check_expr(ctx, lhs);
            self.place_from_expr(ctx, lhs)
        };
        self.move_check_expr(ctx, rhs);
        if op.is_assignment() {
            // A shared reference does not permit mutating what it points to.
            // The write target is reached through the borrow whenever the
            // left-hand side implicitly dereferences a `&T` base (`r.field`,
            // `r[0]`) or spells the dereference out (`*r`).
            if self.writes_through_shared_reference(ctx, lhs) {
                let name = Self::expr_name(ctx, lhs);
                self.diag(
                    format!("cannot assign to `{name}` through a shared reference"),
                    span,
                    "E0309",
                );
            }
            if let Some(lhs_place) = lhs_place.as_ref()
                && Self::has_conflicting_place_move_borrow(ctx, lhs_place)
            {
                let name = Self::expr_name(ctx, lhs);
                self.diag(
                    format!("cannot assign to `{name}` while borrowed"),
                    span,
                    "E0303",
                );
            }
            // Writing through a reference (`*m = v`, `m.field = v` with
            // `m: &mut _`) writes the referent: loans derived from the same
            // underlying place — a shared element reference still held by a
            // closure, say — conflict even though the assignment's own place
            // is rooted at the reference binding. The reference's own loan
            // family (the incoming parameter seed included) stays excluded,
            // matching the method-call path.
            if self.place_has_explicit_reference_deref(ctx, lhs) {
                for target in self.reference_write_targets(ctx, lhs) {
                    self.borrow_conflicts(
                        ctx,
                        &target.place,
                        BorrowKind::Mutable,
                        &target.parents,
                        span,
                        lhs,
                    );
                }
            }
            if let Some((binding, direct)) = Self::local_assignment(ctx, lhs) {
                let mut value = ctx.expr_origin_value(rhs);
                if !direct {
                    value.origins.extend(
                        ctx.local_origins
                            .get(&binding)
                            .into_iter()
                            .flatten()
                            .cloned(),
                    );
                    value.fields.clear();
                }
                ctx.bind_origin_value(binding, value);
            }
            self.apply_recorded_value_use(ctx, rhs);
            if let Some(binding) = direct_binding {
                let place = Place::root(binding);
                ctx.bindings.mark_available(&Self::expr_name(ctx, lhs));
                ctx.moved_places
                    .retain(|moved| !place_overlaps(moved, &place));
                ctx.moved_sites
                    .retain(|moved, _| !place_overlaps(moved, &place));
            } else if let Some(lhs_place) = self.place_from_expr(ctx, lhs) {
                // Assigning to a place re-initializes everything at or inside
                // it, so prior moves of those places no longer apply. This
                // covers parameter fields, whose moves were previously
                // untracked, and partially moved locals.
                ctx.moved_places
                    .retain(|moved| !lhs_place.is_prefix_of(moved));
                ctx.moved_sites
                    .retain(|moved, _| !lhs_place.is_prefix_of(moved));
            }
        }
        let origins = if op.is_assignment() {
            ctx.expr_origins.get(&rhs).cloned().unwrap_or_default()
        } else {
            self.apply_recorded_value_use(ctx, lhs);
            self.apply_recorded_value_use(ctx, rhs);
            Self::deactivate_unretained(ctx, lhs, &HashSet::new());
            Self::deactivate_unretained(ctx, rhs, &HashSet::new());
            Origins::new()
        };
        ctx.expr_origins.insert(expr_id, origins);
    }

    fn move_check_unary(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        span: Option<TextRange>,
    ) {
        let Expr::Unary { operand, op } = ctx.body.exprs[expr_id] else {
            unreachable!("expected unary expression");
        };
        self.move_check_expr(ctx, operand);
        let origins = match op {
            UnaryOp::Ref => self.create_borrow(ctx, expr_id, operand, BorrowKind::Shared, span),
            UnaryOp::MutRef => self.create_borrow(ctx, expr_id, operand, BorrowKind::Mutable, span),
            UnaryOp::Deref => ctx.expr_origins.get(&operand).cloned().unwrap_or_default(),
            _ => Origins::new(),
        };
        let origins = if self.expr_may_carry_reference(ctx, expr_id) {
            origins
        } else {
            Self::deactivate_unretained(ctx, operand, &HashSet::new());
            Origins::new()
        };
        ctx.expr_origins.insert(expr_id, origins);
        self.apply_recorded_value_use(ctx, operand);
    }

    fn move_check_block(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) {
        let Expr::Block { stmts, tail } = ctx.body.exprs[expr_id].clone() else {
            unreachable!("expected block expression");
        };
        ctx.push_scope();
        for stmt in stmts {
            self.move_check_stmt(ctx, stmt);
        }
        if let Some(tail) = tail {
            self.move_check_expr(ctx, tail);
            ctx.set_expr_origin_value(expr_id, ctx.expr_origin_value(tail));
            self.apply_recorded_value_use(ctx, tail);
        }
        ctx.pop_scope();
    }

    fn move_check_if(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) {
        let Expr::If {
            cond,
            then_branch,
            else_branch,
        } = ctx.body.exprs[expr_id]
        else {
            unreachable!("expected if expression");
        };
        self.move_check_expr(ctx, cond);
        self.apply_recorded_value_use(ctx, cond);
        let branch_entry = ctx.move_state_snapshot();
        let entry_origins = ctx.local_origins.clone();
        self.move_check_expr(ctx, then_branch);
        self.apply_recorded_value_use(ctx, then_branch);
        let then_diverges = self.always_diverges(ctx, then_branch);
        if let Some(else_branch) = else_branch {
            let then_exit = ctx.move_state_snapshot();
            let then_origins = ctx.local_origins.clone();
            ctx.copy_move_state_snapshot(&branch_entry);
            ctx.local_origins = entry_origins.clone();
            self.move_check_expr(ctx, else_branch);
            self.apply_recorded_value_use(ctx, else_branch);
            if self.always_diverges(ctx, else_branch) {
                // The `else` path never reaches the join, so the `then` exit
                // stands on its own.
                ctx.copy_move_state_snapshot(&then_exit);
                ctx.local_origins = then_origins;
            } else if !then_diverges {
                ctx.merge_move_state_snapshot(&then_exit);
                // A delayed binding assigned in both branches may hold either
                // branch's borrow afterwards: union the branch origins so the
                // conservative loan set survives the merge.
                BodyCtx::merge_local_origins(ctx, &entry_origins, &then_origins);
            }
            // `then` diverges while `else` does not: the `else` exit is already
            // the only state that reaches the join.
        } else if then_diverges {
            // Without an `else`, the code after the `if` is reached only when
            // the condition is false. A `then` branch that always returns (or
            // breaks, or panics) must not contribute its moves to that path —
            // otherwise `if c { return f(1); } f(2)` reports the second call as
            // a use of a moved value.
            ctx.copy_move_state_snapshot(&branch_entry);
            ctx.local_origins = entry_origins;
        }
        let mut value = ctx.expr_origin_value(then_branch);
        if let Some(else_branch) = else_branch {
            value.merge(ctx.expr_origin_value(else_branch));
        }
        ctx.set_expr_origin_value(expr_id, value);
    }

    /// Whether control never falls out of `expr_id`: every path through it
    /// ends in a `return`, `break`, `continue`, or a `!`-typed call.
    ///
    /// A move performed on a diverging path never reaches the join after an
    /// `if` or `match`, so it must not be merged into the state that follows.
    fn always_diverges(&self, ctx: &BodyCtx<'_>, expr_id: ExprId) -> bool {
        if matches!(
            self.type_result.expr_types.get(&(ctx.body_id, expr_id)),
            Some(Type::Never)
        ) {
            return true;
        }
        match &ctx.body.exprs[expr_id] {
            // Statements after a diverging one are unreachable, so a single
            // diverging statement is enough for the whole block.
            Expr::Block { stmts, tail } => {
                stmts
                    .iter()
                    .any(|stmt| self.stmt_always_diverges(ctx, *stmt))
                    || tail.is_some_and(|tail| self.always_diverges(ctx, tail))
            }
            Expr::If {
                then_branch,
                else_branch,
                ..
            } => {
                self.always_diverges(ctx, *then_branch)
                    && else_branch.is_some_and(|branch| self.always_diverges(ctx, branch))
            }
            Expr::Match { arms, .. } => {
                !arms.is_empty() && arms.iter().all(|arm| self.always_diverges(ctx, arm.body))
            }
            Expr::Unsafe { body } => self.always_diverges(ctx, *body),
            _ => false,
        }
    }

    fn stmt_always_diverges(&self, ctx: &BodyCtx<'_>, stmt: StmtId) -> bool {
        match &ctx.body.stmts[stmt] {
            Stmt::Return { .. } | Stmt::Break { .. } | Stmt::Continue => true,
            Stmt::Expr { expr } => self.always_diverges(ctx, *expr),
            // A `let` continues past its own statement once the pattern
            // matches, and `let ... else` diverges only on the else block.
            // Items lowered into the body likewise fall through.
            Stmt::Let { .. } | Stmt::Item { .. } => false,
        }
    }

    fn move_check_while(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) {
        let Expr::While { condition, body } = ctx.body.exprs[expr_id] else {
            unreachable!("expected while expression");
        };
        let loop_entry = ctx.clone();
        self.diagnostic_suppression += 1;
        self.move_check_expr(ctx, condition);
        self.apply_recorded_value_use(ctx, condition);
        self.push_loop_frames();
        self.move_check_expr(ctx, body);
        self.pop_loop_frames();
        self.apply_recorded_value_use(ctx, body);
        let mut loop_head = loop_entry.clone();
        let mut loop_exit = ctx.clone();
        loop {
            let mut next_head = loop_head.clone();
            next_head.merge_loop_head_move_state_from(&loop_exit);
            if next_head.same_move_state(&loop_head) {
                break;
            }
            loop_head = next_head;
            let mut iteration = loop_head.clone();
            self.move_check_expr(&mut iteration, condition);
            self.apply_recorded_value_use(&mut iteration, condition);
            self.push_loop_frames();
            self.move_check_expr(&mut iteration, body);
            self.pop_loop_frames();
            self.apply_recorded_value_use(&mut iteration, body);
            loop_exit = iteration;
        }
        // 不动点已收敛：用收敛后的回边状态完整重放一遍并发诊断。
        self.diagnostic_suppression -= 1;
        let mut final_iteration = loop_head.clone();
        self.move_check_expr(&mut final_iteration, condition);
        self.apply_recorded_value_use(&mut final_iteration, condition);
        self.push_loop_frames();
        self.move_check_expr(&mut final_iteration, body);
        self.pop_loop_frames();
        self.apply_recorded_value_use(&mut final_iteration, body);
        *ctx = final_iteration;
    }

    fn move_check_loop(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) {
        let Expr::Loop { body } = ctx.body.exprs[expr_id] else {
            unreachable!("expected loop expression");
        };
        // 与 while 相同的不动点迭代；循环体至少执行一次，出口只经由 break
        let loop_entry = ctx.clone();
        self.diagnostic_suppression += 1;
        self.push_loop_frames();
        self.move_check_expr(ctx, body);
        self.apply_recorded_value_use(ctx, body);
        let (mut break_values, mut break_states) = self.pop_loop_frames();
        let mut loop_head = loop_entry.clone();
        let mut loop_exit = ctx.clone();
        loop {
            let mut next_head = loop_head.clone();
            next_head.merge_loop_head_move_state_from(&loop_exit);
            if next_head.same_move_state(&loop_head) {
                break;
            }
            loop_head = next_head;
            let mut iteration = loop_head.clone();
            self.push_loop_frames();
            self.move_check_expr(&mut iteration, body);
            self.apply_recorded_value_use(&mut iteration, body);
            let (values, states) = self.pop_loop_frames();
            break_values.extend(values);
            break_states.extend(states);
            loop_exit = iteration;
        }
        // 不动点已收敛：用收敛后的回边状态完整重放一遍并发诊断。
        self.diagnostic_suppression -= 1;
        let mut final_iteration = loop_head.clone();
        self.push_loop_frames();
        self.move_check_expr(&mut final_iteration, body);
        self.apply_recorded_value_use(&mut final_iteration, body);
        let (values, states) = self.pop_loop_frames();
        break_values.extend(values);
        break_states.extend(states);
        *ctx = final_iteration;
        if let Some(first) = break_states.first() {
            ctx.copy_move_state_snapshot(first);
            for state in &break_states[1..] {
                ctx.merge_move_state_snapshot(state);
            }
        }
        let mut value = OriginValue::default();
        for break_value in break_values {
            value.merge(ctx.expr_origin_value(break_value));
        }
        ctx.set_expr_origin_value(expr_id, value);
    }

    fn push_loop_frames(&mut self) {
        self.loop_break_values.push(Vec::new());
        self.loop_break_states.push(Vec::new());
    }

    fn pop_loop_frames(&mut self) -> (Vec<ExprId>, Vec<MoveStateSnapshot>) {
        let values = self
            .loop_break_values
            .pop()
            .expect("loop break value stack must be present");
        let states = self
            .loop_break_states
            .pop()
            .expect("loop break state stack must be present");
        (values, states)
    }

    fn move_check_for(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) {
        let Expr::For {
            pat,
            iterable,
            body,
        } = ctx.body.exprs[expr_id]
        else {
            unreachable!("expected for expression");
        };
        ctx.push_scope();
        self.move_check_expr(ctx, iterable);
        let item_value = ctx.expr_origin_value(iterable).iterated();
        self.apply_recorded_value_use(ctx, iterable);
        let item_ty = self
            .type_result
            .for_loops
            .get(&(ctx.body_id, expr_id))
            .map(|info| info.item_ty.clone())
            .or_else(|| {
                self.type_result
                    .expr_types
                    .get(&(ctx.body_id, iterable))
                    .and_then(|ty| match ty {
                        Type::Array(item, _) => Some((**item).clone()),
                        _ => None,
                    })
            });
        if let Some(item_ty) = item_ty {
            self.check_pattern_move_from_drop(ctx, pat, &item_ty);
        }
        self.check_explicit_reference_pattern_move(ctx, pat);
        let loop_entry = ctx.clone();
        ctx.push_scope();
        Self::bind_pattern_names(ctx, pat);
        self.bind_pattern_origins(ctx, pat, &item_value);
        self.diagnostic_suppression += 1;
        self.push_loop_frames();
        self.move_check_expr(ctx, body);
        self.pop_loop_frames();
        ctx.pop_scope();
        let mut loop_head = loop_entry.clone();
        let mut loop_exit = ctx.clone();
        loop {
            let mut next_head = loop_head.clone();
            next_head.merge_loop_head_move_state_from(&loop_exit);
            if next_head.same_move_state(&loop_head) {
                break;
            }
            loop_head = next_head;
            let mut iteration = loop_head.clone();
            iteration.push_scope();
            Self::bind_pattern_names(&mut iteration, pat);
            self.bind_pattern_origins(&mut iteration, pat, &item_value);
            self.push_loop_frames();
            self.move_check_expr(&mut iteration, body);
            self.pop_loop_frames();
            iteration.pop_scope();
            loop_exit = iteration;
        }
        // 不动点已收敛：用收敛后的回边状态完整重放一遍并发诊断。
        self.diagnostic_suppression -= 1;
        let mut final_iteration = loop_head.clone();
        final_iteration.push_scope();
        Self::bind_pattern_names(&mut final_iteration, pat);
        self.bind_pattern_origins(&mut final_iteration, pat, &item_value);
        self.push_loop_frames();
        self.move_check_expr(&mut final_iteration, body);
        self.pop_loop_frames();
        final_iteration.pop_scope();
        *ctx = final_iteration;
        ctx.copy_move_state_from(&loop_head);
        ctx.pop_scope();
    }

    fn move_check_match(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) {
        let Expr::Match { scrutinee, arms } = ctx.body.exprs[expr_id].clone() else {
            unreachable!("expected match expression");
        };
        self.move_check_expr(ctx, scrutinee);
        let scrutinee_value = ctx.expr_origin_value(scrutinee);
        let scrutinee_ty = self
            .type_result
            .expr_types
            .get(&(ctx.body_id, scrutinee))
            .cloned()
            .unwrap_or(Type::Unknown);
        let scrutinee_place = self.place_from_expr(ctx, scrutinee);
        if scrutinee_place.is_none() {
            // The scrutinee is not a trackable place (e.g. `match *r` behind a
            // reference, or a temporary). Route the recorded value use through
            // the normal consume path so moving out of a dereference of a
            // non-Copy value is still rejected.
            self.apply_recorded_value_use(ctx, scrutinee);
        }
        let base_bindings = ctx.bindings.clone();
        let base_moved_places = ctx.moved_places.clone();
        let base_moved_sites = ctx.moved_sites.clone();
        let mut merged_bindings = base_bindings.clone();
        let mut merged_moved_places = base_moved_places.clone();
        let mut merged_moved_sites = base_moved_sites.clone();
        for arm in &arms {
            ctx.bindings.clone_from(&base_bindings);
            ctx.moved_places.clone_from(&base_moved_places);
            ctx.moved_sites.clone_from(&base_moved_sites);
            self.check_pattern_move_from_drop(ctx, arm.pat, &scrutinee_ty);
            self.check_explicit_reference_pattern_move(ctx, arm.pat);
            ctx.push_scope();
            Self::bind_pattern_names(ctx, arm.pat);
            self.bind_pattern_origins(ctx, arm.pat, &scrutinee_value);
            if let Some(guard) = arm.guard {
                let old_guard = std::mem::replace(&mut ctx.in_match_guard, true);
                let old_scrutinee = std::mem::take(&mut ctx.guard_scrutinee);
                ctx.guard_scrutinee = scrutinee_place.iter().cloned().collect();
                let mut arm_bindings = Vec::new();
                initialization::collect_pattern_bindings(ctx.body, arm.pat, &mut arm_bindings);
                for (binding, _) in arm_bindings {
                    // A whole-value binding aliases the scrutinee place; a
                    // field binding aliases that field, and its root still
                    // covers the moved-out part.
                    ctx.guard_scrutinee.push(Place::root(binding));
                }
                self.move_check_expr(ctx, guard);
                ctx.guard_scrutinee = old_scrutinee;
                ctx.in_match_guard = old_guard;
            }
            if let Some(root) = &scrutinee_place {
                for place in self.pattern_move_places(ctx, arm.pat, root) {
                    if Self::has_any_borrow(ctx, &place) {
                        self.diag(
                            "cannot move a pattern field while borrowed".into(),
                            ctx.source_map.pat_ranges.get(&arm.pat).copied(),
                            "E0304",
                        );
                        continue;
                    }
                    ctx.moved_places.insert(place.clone());
                    ctx.moved_sites.insert(
                        place,
                        (
                            ctx.source_map.pat_ranges.get(&arm.pat).copied(),
                            "field moved by pattern here".into(),
                        ),
                    );
                }
            }
            self.move_check_expr(ctx, arm.body);
            self.apply_recorded_value_use(ctx, arm.body);
            ctx.pop_scope();
            // An arm that always diverges never reaches the code after the
            // `match`, so its moves must not join here — otherwise a `return`
            // arm that spends a value reports the next arm's use of it. A
            // guarded arm is safe to skip for the same reason: when the guard
            // fails the body never ran.
            if !self.always_diverges(ctx, arm.body) {
                merged_bindings.merge_moved_from(&ctx.bindings);
                merged_moved_places.extend(ctx.moved_places.iter().cloned());
                merged_moved_sites.extend(ctx.moved_sites.clone());
            }
        }
        ctx.bindings = merged_bindings;
        ctx.moved_places = merged_moved_places;
        ctx.moved_sites = merged_moved_sites;
        let mut value = OriginValue::default();
        for arm in &arms {
            value.merge(ctx.expr_origin_value(arm.body));
        }
        ctx.set_expr_origin_value(expr_id, value);
    }

    fn move_check_aggregate(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) {
        match ctx.body.exprs[expr_id].clone() {
            Expr::Array { elements } | Expr::Tuple { elements } => {
                let mut fields = Vec::with_capacity(elements.len());
                for element in elements {
                    self.move_check_expr(ctx, element);
                    self.apply_recorded_value_use(ctx, element);
                    fields.push(ctx.expr_origin_value(element));
                }
                ctx.set_expr_origin_value(expr_id, OriginValue::from_fields(fields));
            }
            Expr::ArrayRepeat { value, len } => {
                self.move_check_expr(ctx, value);
                self.apply_recorded_value_use(ctx, value);
                self.move_check_expr(ctx, len);
                self.apply_recorded_value_use(ctx, len);
                ctx.set_expr_origin_value(
                    expr_id,
                    OriginValue::from_fields(vec![ctx.expr_origin_value(value)]),
                );
            }
            _ => unreachable!("expected aggregate expression"),
        }
    }

    fn move_check_call(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId, span: Option<TextRange>) {
        let Expr::Call { callee, args, .. } = ctx.body.exprs[expr_id].clone() else {
            unreachable!("expected call expression");
        };
        if let Expr::FieldAccess { base, .. } = &ctx.body.exprs[callee]
            && let Some(place) = self.place_from_expr(ctx, *base)
            && ctx
                .moved_places
                .iter()
                .any(|moved| place_overlaps(moved, &place))
        {
            let extra = Self::move_site_labels(ctx, &place);
            let label = Self::expr_name(ctx, *base);
            self.diag_with_labels(
                format!("use of moved value: `{label}`"),
                span,
                "E0100",
                &extra,
            );
        }
        self.move_check_expr(ctx, callee);
        // A value-passed argument that itself holds borrows (e.g. a `&mut`
        // binding moved into the call) keeps them live for the duration of
        // the call: later argument expressions and reference parameters of
        // the same call must still conflict with them, exactly as if the
        // borrows had been written inline.
        let mut transferred_loans = Vec::new();
        for arg in &args {
            self.move_check_expr(ctx, *arg);
            if let Expr::Path {
                resolved: Some(ResolvedName::PatternBinding(binding)),
                ..
            } = &ctx.body.exprs[*arg]
            {
                for origin in ctx.local_origins.get(binding).into_iter().flatten() {
                    if let Some(record) = ctx.loans.get_mut(&origin.loan)
                        && !record.active
                        && !record.permanent
                    {
                        record.active = true;
                        transferred_loans.push(origin.loan);
                    }
                }
            }
        }
        let (inputs, modes, fid, trait_call) = self.call_signature(ctx, callee, &args);
        let value = self.check_call_borrows(ctx, expr_id, &inputs, &modes, fid, trait_call, span);
        if fid.is_none()
            && self
                .type_result
                .trait_method_calls
                .contains_key(&(ctx.body_id, callee))
            && self
                .type_result
                .expr_types
                .get(&(ctx.body_id, expr_id))
                .is_some_and(contains_opaque_owned_result)
        {
            // ponytail: an opaque associated item cannot yet distinguish an
            // iterator's stored references from borrowing the iterator itself.
            // Reset only these implicit receiver loans on backedges until
            // associated-return provenance is available; explicit borrows stay.
            for origin in &value.origins {
                if ctx
                    .loans
                    .get(&origin.loan)
                    .is_some_and(|loan| loan.issued_at == span)
                {
                    ctx.backedge_reset_loans.insert(origin.loan);
                }
            }
        }
        for loan in transferred_loans {
            if let Some(record) = ctx.loans.get_mut(&loan) {
                record.active = false;
            }
        }
        ctx.set_expr_origin_value(expr_id, value);
        for input in &inputs {
            self.apply_recorded_value_use(ctx, *input);
        }
        self.apply_recorded_value_use(ctx, callee);
    }

    fn move_check_lambda(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) {
        let Expr::Lambda { params, body, .. } = ctx.body.exprs[expr_id].clone() else {
            unreachable!("expected lambda expression");
        };
        if let Some(info) = self
            .type_result
            .lambda_infos
            .get(&(ctx.body_id, expr_id))
            .cloned()
        {
            self.apply_capture_effects(ctx, expr_id, &info);
            self.move_check_lambda_body(ctx, &params, body, &info);
        }
    }

    fn move_check_passthrough(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) {
        let operand = match ctx.body.exprs[expr_id] {
            Expr::Unsafe { body } => body,
            Expr::Cast { base, .. } => base,
            Expr::Try { operand } => operand,
            _ => unreachable!("expected passthrough expression"),
        };
        self.move_check_expr(ctx, operand);
        ctx.set_expr_origin_value(expr_id, ctx.expr_origin_value(operand));
        self.apply_recorded_value_use(ctx, operand);
    }

    fn move_check_projection(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        expr_id: ExprId,
        span: Option<TextRange>,
    ) {
        match ctx.body.exprs[expr_id].clone() {
            Expr::FieldAccess { base, field } => {
                let base_moved = self.place_from_expr(ctx, base).is_some_and(|place| {
                    ctx.moved_places
                        .iter()
                        .any(|moved| place_overlaps(moved, &place))
                });
                if !base_moved {
                    self.move_check_expr(ctx, base);
                }
                if let Some(place) = self.place_from_expr(ctx, expr_id)
                    && ctx
                        .moved_places
                        .iter()
                        .any(|moved| place_overlaps(moved, &place))
                {
                    let extra = Self::move_site_labels(ctx, &place);
                    self.diag_with_labels(
                        format!("use of moved field: `{}`", field.0),
                        span,
                        "E0100",
                        &extra,
                    );
                }
                let value = if self.expr_may_carry_reference(ctx, expr_id) {
                    let base_value = ctx.expr_origin_value(base);
                    self.resolve_field_index(ctx.body_id, base, &field)
                        .map_or_else(|| base_value.flattened(), |index| base_value.project(index))
                } else {
                    OriginValue::default()
                };
                ctx.set_expr_origin_value(expr_id, value);
            }
            Expr::IndexAccess { base, index } => {
                self.move_check_expr(ctx, base);
                self.move_check_expr(ctx, index);
                self.check_trait_index_receiver_borrow(ctx, expr_id, base, span);
                self.apply_recorded_value_use(ctx, index);
                if let Some(place) = self.place_from_expr(ctx, expr_id)
                    && ctx
                        .moved_places
                        .iter()
                        .any(|moved| place_overlaps(moved, &place))
                {
                    let extra = Self::move_site_labels(ctx, &place);
                    self.diag_with_labels(
                        "use of moved value from array".into(),
                        span,
                        "E0100",
                        &extra,
                    );
                }
                let value = if self.expr_may_carry_reference(ctx, expr_id) {
                    let base_value = ctx.expr_origin_value(base);
                    match &ctx.body.exprs[index] {
                        Expr::IntLiteral { value: index, .. } => {
                            usize::try_from(*index).ok().map_or_else(
                                || base_value.iterated(),
                                |index| base_value.project(index),
                            )
                        }
                        _ => base_value.iterated(),
                    }
                } else {
                    OriginValue::default()
                };
                ctx.set_expr_origin_value(expr_id, value);
            }
            _ => unreachable!("expected projection expression"),
        }
    }

    fn move_check_stmt(&mut self, ctx: &mut BodyCtx<'_>, stmt_id: StmtId) {
        let s = &ctx.body.stmts[stmt_id];
        match s {
            Stmt::Let {
                pat, init, else_, ..
            } => {
                let pat = *pat;
                if let Some(init) = *init {
                    self.move_check_expr(ctx, init);
                    let value = ctx.expr_origin_value(init);
                    self.bind_pattern_origins(ctx, pat, &value);
                    Self::deactivate_unretained(ctx, init, &HashSet::new());
                    self.check_explicit_reference_pattern_move(ctx, pat);
                    if let Some(init_ty) = self
                        .type_result
                        .expr_types
                        .get(&(ctx.body_id, init))
                        .cloned()
                    {
                        // `let` destructuring must obey the same
                        // move-out-of-Drop-owner rule as `match` arms.
                        self.check_pattern_move_from_drop(ctx, pat, &init_ty);
                    }
                    self.apply_recorded_value_use(ctx, init);
                }
                if let Some(else_) = else_ {
                    // The else block diverges, so its moves never flow past
                    // the statement; check it against a snapshot and restore.
                    let bindings = ctx.bindings.clone();
                    let moved_places = ctx.moved_places.clone();
                    let moved_sites = ctx.moved_sites.clone();
                    self.move_check_expr(ctx, *else_);
                    ctx.bindings = bindings;
                    ctx.moved_places = moved_places;
                    ctx.moved_sites = moved_sites;
                }
                // Record the declaration scope of every binding the pattern
                // introduces, so deferred assignments inside nested blocks
                // clamp their loans to this depth (see `bind_origins`).
                let mut declared = Vec::new();
                initialization::collect_pattern_bindings(ctx.body, pat, &mut declared);
                for (id, _) in declared {
                    ctx.binding_scopes.entry(id).or_insert(ctx.scope_depth);
                }
                Self::reset_pattern_moves(ctx, pat);
            }
            Stmt::Expr { expr } => {
                self.move_check_expr(ctx, *expr);
                self.apply_recorded_value_use(ctx, *expr);
            }
            Stmt::Return { value } => {
                if let Some(v) = value {
                    self.move_check_expr(ctx, *v);
                    self.check_returned_drop_borrow(ctx, *v);
                    self.apply_recorded_value_use(ctx, *v);
                }
            }
            Stmt::Break { value } => {
                if let Some(v) = value {
                    self.move_check_expr(ctx, *v);
                    self.apply_recorded_value_use(ctx, *v);
                    if let Some(values) = self.loop_break_values.last_mut() {
                        values.push(*v);
                    }
                }
                if let Some(states) = self.loop_break_states.last_mut() {
                    states.push(ctx.move_state_snapshot());
                }
            }
            Stmt::Continue | Stmt::Item { .. } => {}
        }
    }

    /// `Copy` for a body: a generic parameter bound by `T: Copy` is
    /// copyable even though the global environment holds no impl for the bare
    /// parameter, so bound assumptions are checked alongside the impls.
    fn type_is_copy(&self, ctx: &BodyCtx<'_>, ty: &Type) -> bool {
        match self.trait_env.copy_trait_id {
            Some(copy_trait_id) => self.trait_env.type_implements_with_args_assuming(
                ty,
                copy_trait_id,
                &[],
                ctx.bounds,
            ),
            None => self.trait_env.type_is_copy(ty),
        }
    }

    fn consume_if_local(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) {
        if let Expr::Path { path, resolved } = &ctx.body.exprs[expr_id]
            && let Some(name) = path.as_single_name()
            && ctx.bindings.contains(&name.0)
        {
            let (ty, closure_kind) = self.expr_move_properties(ctx, expr_id);
            if !self.type_is_copy(ctx, &ty)
                || matches!(closure_kind, Some(ClosureKind::FnMut | ClosureKind::FnOnce))
            {
                if ctx.in_match_guard
                    && self
                        .place_from_expr(ctx, expr_id)
                        .or_else(|| {
                            resolved.as_ref().and_then(|resolved| match resolved {
                                ResolvedName::PatternBinding(id) => Some(Place::root(*id)),
                                ResolvedName::Param(index) => Some(Place::param(*index)),
                                ResolvedName::LambdaParam { lambda, index } => {
                                    Some(Place::lambda_param(*lambda, *index))
                                }
                                _ => None,
                            })
                        })
                        .is_some_and(|place| {
                            ctx.guard_scrutinee
                                .iter()
                                .any(|scrutinee| place_overlaps(&place, scrutinee))
                        })
                {
                    self.diag(
                        format!("cannot move `{}` in a match guard", name.0),
                        ctx.expr_range(expr_id),
                        "E0307",
                    );
                    return;
                }
                let access_place = resolved.as_ref().and_then(access_place_from_resolved_name);
                if access_place.as_ref().is_some_and(|place| {
                    Self::has_access_borrow_except_origins(ctx, place, expr_id)
                }) {
                    self.diag(
                        format!("cannot move `{}` while borrowed", name.0),
                        ctx.expr_range(expr_id),
                        "E0304",
                    );
                    return;
                }
                ctx.bindings.mark_moved(&name.0);
                self.result.moved_exprs.insert((ctx.body_id, expr_id));
                // Record move site for secondary label. Parameters and lambda
                // parameters record their whole place too, so later field
                // uses see the move (pattern bindings already do).
                let span = ctx.expr_range(expr_id);
                match resolved {
                    Some(ResolvedName::PatternBinding(id)) => {
                        let p = Place::root(*id);
                        ctx.moved_places.insert(p.clone());
                        ctx.moved_sites.insert(p, (span, "value moved here".into()));
                    }
                    Some(ResolvedName::Param(index)) => {
                        let p = Place::param(*index);
                        ctx.moved_places.insert(p.clone());
                        ctx.moved_sites.insert(p, (span, "value moved here".into()));
                    }
                    Some(ResolvedName::LambdaParam { lambda, index }) => {
                        let p = Place::lambda_param(*lambda, *index);
                        ctx.moved_places.insert(p.clone());
                        ctx.moved_sites.insert(p, (span, "value moved here".into()));
                    }
                    _ => {}
                }
            }
            return;
        }

        let (ty, closure_kind) = self.expr_move_properties(ctx, expr_id);
        if self.type_is_copy(ctx, &ty)
            && !matches!(closure_kind, Some(ClosureKind::FnMut | ClosureKind::FnOnce))
        {
            return;
        }
        if self.place_has_explicit_reference_deref(ctx, expr_id) {
            if std::env::var_os("RIDDLE_MC_DEBUG").is_some() {
                eprintln!(
                    "mc-debug: E0308 expr {expr_id:?} = {:#?}",
                    ctx.body.exprs[expr_id]
                );
            }
            self.diag(
                "cannot move out of dereference of a non-Copy value".into(),
                ctx.expr_range(expr_id),
                "E0308",
            );
            return;
        }
        let Some(place) = self.place_from_expr(ctx, expr_id) else {
            return;
        };
        if ctx.in_match_guard
            && ctx
                .guard_scrutinee
                .iter()
                .any(|scrutinee| place_overlaps(&place, scrutinee))
        {
            let name = Self::expr_name(ctx, expr_id);
            self.diag(
                format!("cannot move `{name}` in a match guard",),
                ctx.expr_range(expr_id),
                "E0307",
            );
            return;
        }
        if !place.projections.is_empty()
            && self
                .root_type_from_expr(ctx, expr_id)
                .is_some_and(|ty| self.trait_env.type_has_explicit_drop(ty))
        {
            self.diag(
                "cannot move out of a field of a type that implements `Drop`".into(),
                ctx.expr_range(expr_id),
                "E0305",
            );
            return;
        }
        if Self::has_conflicting_place_move_borrow(ctx, &place) {
            let name = Self::expr_name(ctx, expr_id);
            self.diag(
                format!("cannot move `{name}` while borrowed"),
                ctx.expr_range(expr_id),
                "E0304",
            );
            return;
        }
        // Record the expression move unconditionally so MIR clears the
        // element's drop flag, but only track the place statically for
        // pattern-rooted locals: a runtime index on a parameter cannot be
        // told apart from a different index, and standard-library shift
        // loops move and re-read neighbouring elements in one pass.
        if matches!(place.root, hir::place::PlaceRoot::Pattern(_))
            || !place_has_wildcard_index(&place)
        {
            ctx.moved_places.insert(place.clone());
            let span = ctx.expr_range(expr_id);
            let desc = "value moved here".to_string();
            ctx.moved_sites.insert(place, (span, desc));
        }
        self.result.moved_exprs.insert((ctx.body_id, expr_id));
    }

    fn expr_move_properties(
        &self,
        ctx: &BodyCtx<'_>,
        expr_id: ExprId,
    ) -> (Type, Option<ClosureKind>) {
        let ty = self
            .type_result
            .expr_types
            .get(&(ctx.body_id, expr_id))
            .cloned()
            .unwrap_or(Type::Unknown);
        let closure_kind = ty.closure_kind();
        (ty, closure_kind)
    }

    fn apply_recorded_value_use(&mut self, ctx: &mut BodyCtx<'_>, expr_id: ExprId) {
        if self
            .type_result
            .value_uses
            .get(&(ctx.body_id, expr_id))
            .copied()
            == Some(ValueUse::Move)
        {
            self.consume_if_local(ctx, expr_id);
        }
    }

    fn root_type_from_expr<'b>(&'b self, ctx: &BodyCtx<'_>, expr_id: ExprId) -> Option<&'b Type> {
        let mut root = expr_id;
        loop {
            root = match &ctx.body.exprs[root] {
                Expr::FieldAccess { base, .. } | Expr::IndexAccess { base, .. } => *base,
                _ => break,
            };
        }
        self.type_result.expr_types.get(&(ctx.body_id, root))
    }

    fn check_returned_drop_borrow(&mut self, ctx: &BodyCtx<'_>, expr_id: ExprId) {
        let captured_owner = self
            .type_result
            .lambda_infos
            .get(&(ctx.body_id, expr_id))
            .into_iter()
            .flat_map(|info| &info.captures)
            .find_map(|capture| {
                let root = match &capture.place.source {
                    CaptureSource::Pattern(id) => AccessRoot::Pattern(*id),
                    CaptureSource::Param(index) => AccessRoot::Param(*index),
                    CaptureSource::LambdaParam { lambda, index } => AccessRoot::LambdaParam {
                        lambda: *lambda,
                        index: *index,
                    },
                };
                (capture.mode != CaptureMode::Value && self.root_has_explicit_drop(ctx, root))
                    .then_some(root)
            });
        let owner = captured_owner.or_else(|| {
            ctx.expr_origins
                .get(&expr_id)
                .into_iter()
                .flatten()
                .find_map(|origin| {
                    self.root_has_explicit_drop(ctx, origin.place.root)
                        .then_some(origin.place.root)
                })
        });
        let Some(owner) = owner else { return };
        let owner_range = match owner {
            AccessRoot::Pattern(id) => ctx.source_map.pat_ranges.get(&id.pattern).copied(),
            AccessRoot::Param(index) => self.hir.item_tree.functions[ctx.function_id]
                .params
                .get(index)
                .map(|param| param.name_range),
            AccessRoot::LambdaParam { .. } => None,
        };
        let labels = owner_range
            .map(|range| {
                vec![(
                    range,
                    "value that owns the destructor is declared here".into(),
                    LabelStyle::Secondary,
                )]
            })
            .unwrap_or_default();
        self.diag_with_labels(
            "borrow of a value that implements `Drop` cannot outlive its owner".into(),
            ctx.expr_range(expr_id),
            "E0306",
            &labels,
        );
    }

    fn root_has_explicit_drop(&self, ctx: &BodyCtx<'_>, root: AccessRoot) -> bool {
        let ty = match root {
            AccessRoot::Pattern(binding) => self
                .type_result
                .pattern_binding_types
                .get(&(ctx.body_id, binding)),
            AccessRoot::Param(index) => {
                ctx.body
                    .exprs
                    .iter()
                    .find_map(|(expr_id, expr)| match expr {
                        Expr::Path {
                            resolved: Some(ResolvedName::Param(param)),
                            ..
                        } if *param == index => {
                            self.type_result.expr_types.get(&(ctx.body_id, expr_id))
                        }
                        _ => None,
                    })
            }
            AccessRoot::LambdaParam { .. } => None,
        };
        ty.is_some_and(|ty| self.trait_env.type_has_explicit_drop(ty))
    }

    fn apply_capture_effects(&mut self, ctx: &mut BodyCtx<'_>, lambda: ExprId, info: &LambdaInfo) {
        let span = ctx.expr_range(lambda);
        let mut capture_origins = Origins::new();
        for capture in &info.captures {
            if ctx.bindings.get(&capture.name).copied() == Some(true) {
                self.diag(
                    format!("use of moved value: `{}`", capture.name),
                    span,
                    "E0100",
                );
                continue;
            }
            let move_place = move_place_from_capture(&capture.place);
            let access_place = access_place_from_capture(&capture.place);
            if let Some(place) = &move_place
                && ctx
                    .moved_places
                    .iter()
                    .any(|moved| place_overlaps(moved, place))
            {
                let extra = Self::move_site_labels(ctx, place);
                self.diag_with_labels(
                    format!("use of moved value: `{}`", capture.name),
                    span,
                    "E0100",
                    &extra,
                );
                continue;
            }

            if let Some(origin) =
                self.apply_capture_mode(ctx, capture, move_place, &access_place, span)
            {
                capture_origins.insert(origin);
            }
        }
        // A captured value keeps its outstanding loans alive for as long as
        // the lambda value itself lives. The captured binding's last textual
        // use sits inside the lambda body, so without this the loan expired
        // right after the lambda expression — leaving `v.push(..)` between
        // the closure's creation and its call unchecked while the captured
        // reference still pointed into the container.
        for capture in &info.captures {
            let CaptureSource::Pattern(id) = capture.place.source else {
                // Reference parameters carry permanent seed loans; lambda
                // parameters have no trackable place here.
                continue;
            };
            let mut value = ctx.local_origin_value(id);
            for projection in &capture.place.projections {
                value = match projection {
                    hir::place::Projection::Field(index)
                    | hir::place::Projection::Index(Some(index)) => value.project(*index),
                    hir::place::Projection::Index(None) => value.iterated(),
                };
            }
            capture_origins.extend(value.flattened().origins);
        }
        if !capture_origins.is_empty() {
            // The lambda's value carries its captured references: attach the
            // capture loans as the expression's origins so they flow into the
            // binding that owns the lambda and expire at its last use, not at
            // the enclosing scope's exit.
            let mut value = ctx.expr_origin_value(lambda);
            value.origins.extend(capture_origins);
            ctx.set_expr_origin_value(lambda, value);
        }
    }

    fn apply_capture_mode(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        capture: &ty::LambdaCapture,
        move_place: Option<Place>,
        access_place: &AccessPlace,
        span: Option<TextRange>,
    ) -> Option<Origin> {
        match capture.mode {
            CaptureMode::Shared => {
                if Self::has_mut_access_borrow(ctx, access_place) {
                    self.diag(
                        format!(
                            "cannot capture `{}` by shared reference while mutably borrowed",
                            capture.name
                        ),
                        span,
                        "E0301",
                    );
                    None
                } else {
                    let loan = ctx.new_loan(access_place.clone(), BorrowKind::Shared, span, false);
                    Some(Origin {
                        place: access_place.clone(),
                        kind: BorrowKind::Shared,
                        loan,
                    })
                }
            }
            CaptureMode::Mutable => {
                if Self::has_shared_access_borrow(ctx, access_place) {
                    self.diag(
                        format!(
                            "cannot capture `{}` mutably while shared-borrowed",
                            capture.name
                        ),
                        span,
                        "E0300",
                    );
                    None
                } else if Self::has_mut_access_borrow(ctx, access_place) {
                    self.diag(
                        format!("cannot capture `{}` mutably more than once", capture.name),
                        span,
                        "E0302",
                    );
                    None
                } else {
                    let loan = ctx.new_loan(access_place.clone(), BorrowKind::Mutable, span, false);
                    Some(Origin {
                        place: access_place.clone(),
                        kind: BorrowKind::Mutable,
                        loan,
                    })
                }
            }
            CaptureMode::Value => {
                if self.type_is_copy(ctx, &capture.ty) {
                    return None;
                }
                if ctx.in_match_guard
                    && move_place.as_ref().is_some_and(|place| {
                        ctx.guard_scrutinee
                            .iter()
                            .any(|scrutinee| place_overlaps(place, scrutinee))
                    })
                {
                    self.diag(
                        format!(
                            "cannot move `{}` into a closure in a match guard",
                            capture.name
                        ),
                        span,
                        "E0307",
                    );
                    return None;
                }
                if !capture.place.projections.is_empty()
                    && self.root_has_explicit_drop(ctx, access_place.root)
                {
                    self.diag(
                        "cannot move out of a field of a type that implements `Drop`".into(),
                        span,
                        "E0305",
                    );
                    return None;
                }
                if Self::has_any_access_borrow(ctx, access_place) {
                    self.diag(
                        format!("cannot move `{}` into closure while borrowed", capture.name),
                        span,
                        "E0304",
                    );
                    return None;
                }
                if let Some(place) = move_place {
                    ctx.moved_places.insert(place.clone());
                    ctx.moved_sites
                        .insert(place, (span, "value moved into closure here".into()));
                }
                if capture.place.projections.is_empty() {
                    ctx.bindings.mark_moved(&capture.name);
                }
                None
            }
        }
    }

    fn move_check_lambda_body(
        &mut self,
        outer: &BodyCtx<'_>,
        params: &[hir::body::LambdaParam],
        body: ExprId,
        info: &LambdaInfo,
    ) {
        let mut ctx = BodyCtx::new(outer.function_id, outer.body_id, outer.body, outer.bounds);
        ctx.seed_params(
            params
                .iter()
                .map(|param| param.name.0.as_str())
                .chain(info.captures.iter().map(|capture| capture.name.as_str())),
        );
        self.move_check_expr(&mut ctx, body);
        if let Expr::Block {
            tail: Some(tail), ..
        } = &ctx.body.exprs[body]
        {
            self.check_returned_drop_borrow(&ctx, *tail);
        }
    }

    /// Write targets for an assignment whose left-hand side goes through a
    /// reference: the reference root's current origins (the local binding's
    /// or the parameter's), projected by the field/index chain written
    /// through. Unlike `access_targets`, this does not rely on the
    /// left-hand side having been visited as an expression — assignments
    /// never read their LHS, so its path expression carries no recorded
    /// origins.
    fn reference_write_targets(&self, ctx: &BodyCtx<'_>, lhs: ExprId) -> Vec<AccessTarget> {
        let mut projections: Vec<AccessProjection> = Vec::new();
        let mut cursor = lhs;
        loop {
            match &ctx.body.exprs[cursor] {
                Expr::Unary {
                    operand,
                    op: UnaryOp::Deref,
                } => {
                    if !self.expr_is_reference(ctx, *operand) {
                        return Vec::new();
                    }
                    cursor = *operand;
                    break;
                }
                Expr::FieldAccess { base, field } => {
                    if let Some(index) = self.resolve_field_index(ctx.body_id, *base, field) {
                        projections.push(AccessProjection::Field(index));
                    }
                    cursor = *base;
                    if self.expr_is_reference(ctx, cursor) {
                        break;
                    }
                }
                Expr::IndexAccess { base, index } => {
                    let index = match &ctx.body.exprs[*index] {
                        Expr::IntLiteral { value, .. } => usize::try_from(*value).ok(),
                        _ => None,
                    };
                    projections.push(AccessProjection::Index(index));
                    cursor = *base;
                    if self.expr_is_reference(ctx, cursor) {
                        break;
                    }
                }
                _ => return Vec::new(),
            }
        }
        let origins = match &ctx.body.exprs[cursor] {
            Expr::Path {
                resolved: Some(ResolvedName::PatternBinding(id)),
                ..
            } => ctx.local_origins.get(id).cloned().unwrap_or_default(),
            Expr::Path {
                resolved: Some(ResolvedName::Param(index)),
                ..
            } => ctx.param_origins.get(index).cloned().unwrap_or_default(),
            _ => ctx.expr_origins.get(&cursor).cloned().unwrap_or_default(),
        };
        let mut targets: HashMap<AccessPlace, HashSet<LoanId>> = HashMap::new();
        for origin in origins {
            let mut place = origin.place;
            for projection in projections.iter().rev() {
                place = match projection {
                    AccessProjection::Field(index) => place.field(*index),
                    AccessProjection::Index(index) => place.index(*index),
                };
            }
            targets
                .entry(place)
                .or_default()
                .extend(ctx.loan_family(origin.loan));
        }
        targets
            .into_iter()
            .map(|(place, parents)| AccessTarget { place, parents })
            .collect()
    }

    fn place_from_expr(&self, ctx: &BodyCtx<'_>, expr_id: ExprId) -> Option<Place> {
        match &ctx.body.exprs[expr_id] {
            Expr::Path { resolved, .. } => match resolved.as_ref()? {
                ResolvedName::PatternBinding(id) => Some(Place::root(*id)),
                ResolvedName::Param(index) => Some(Place::param(*index)),
                ResolvedName::LambdaParam { lambda, index } => {
                    Some(Place::lambda_param(*lambda, *index))
                }
                _ => None,
            },
            Expr::FieldAccess { base, field } => {
                let base_place = self.place_from_expr(ctx, *base)?;
                let idx = self.resolve_field_index(ctx.body_id, *base, field)?;
                Some(base_place.field(idx))
            }
            Expr::IndexAccess { base, index } => {
                let base_place = self.place_from_expr(ctx, *base)?;
                let idx = match &ctx.body.exprs[*index] {
                    Expr::IntLiteral { value, .. } => usize::try_from(*value).ok(),
                    _ => None,
                };
                Some(base_place.index(idx))
            }
            _ => None,
        }
    }

    fn place_has_explicit_reference_deref(&self, ctx: &BodyCtx<'_>, expr_id: ExprId) -> bool {
        match &ctx.body.exprs[expr_id] {
            Expr::Unary {
                operand,
                op: UnaryOp::Deref,
            } => matches!(
                self.type_result.expr_types.get(&(ctx.body_id, *operand)),
                Some(Type::Ref(..))
            ),
            Expr::FieldAccess { base, .. } => {
                let base_ty = self.type_result.expr_types.get(&(ctx.body_id, *base));
                let value_ty = self.type_result.expr_types.get(&(ctx.body_id, expr_id));
                matches!(base_ty, Some(Type::Ref(..)))
                    && !matches!(value_ty, Some(Type::Ptr { .. }))
                    || self.place_has_explicit_reference_deref(ctx, *base)
            }
            Expr::IndexAccess { base, .. } => {
                let base_ty = self.type_result.expr_types.get(&(ctx.body_id, *base));
                self.type_result
                    .trait_method_calls
                    .get(&(ctx.body_id, expr_id))
                    .is_some_and(|call| call.method == "index" || call.method == "index_mut")
                    || !matches!(base_ty, Some(Type::Ptr { .. }))
                        && (matches!(base_ty, Some(Type::Ref(..)))
                            || self.place_has_explicit_reference_deref(ctx, *base))
            }
            _ => false,
        }
    }

    /// Whether writing to `expr_id` — or taking a `&mut` of it — would reach
    /// the place through a shared reference.
    ///
    /// Field and index accesses implicitly dereference a reference base, so
    /// `r.field = v` with `r: &T` writes through the borrow exactly like an
    /// explicit `*r = v` does. Raw pointer bases are excluded: reaching
    /// through them is governed by the `unsafe` rules instead. A path that
    /// lands on — or passes through — a `mut` field is writable anyway, which
    /// is what that declaration is for.
    fn writes_through_shared_reference(&self, ctx: &BodyCtx<'_>, expr_id: ExprId) -> bool {
        self.reached_through_shared_reference(ctx, expr_id)
            && !self.path_has_mut_field(ctx, expr_id)
    }

    /// Whether the place `expr_id` names is reached by dereferencing a `&T` at
    /// some step, ignoring `mut` fields. This is the shape that the call-scoped
    /// `&mut` rule applies to.
    fn reached_through_shared_reference(&self, ctx: &BodyCtx<'_>, expr_id: ExprId) -> bool {
        let expr_type = |id: ExprId| self.type_result.expr_types.get(&(ctx.body_id, id));
        match &ctx.body.exprs[expr_id] {
            Expr::Unary {
                operand,
                op: UnaryOp::Deref | UnaryOp::MutRef,
            } => match expr_type(*operand) {
                Some(Type::Ref(_, false)) => true,
                Some(Type::Ref(_, true) | Type::Ptr { .. }) => false,
                _ => self.reached_through_shared_reference(ctx, *operand),
            },
            Expr::FieldAccess { base, .. } | Expr::IndexAccess { base, .. } => {
                match expr_type(*base) {
                    Some(Type::Ref(_, false)) => true,
                    Some(Type::Ref(_, true) | Type::Ptr { .. }) => false,
                    _ => self.reached_through_shared_reference(ctx, *base),
                }
            }
            _ => false,
        }
    }

    /// Whether the field `expr_id` projects out of its base is declared `mut`.
    fn field_is_mut(&self, ctx: &BodyCtx<'_>, expr_id: ExprId) -> Option<bool> {
        let Expr::FieldAccess { base, field } = &ctx.body.exprs[expr_id] else {
            return None;
        };
        let index = self.resolve_field_index(ctx.body_id, *base, field)?;
        let base_ty = self.type_result.expr_types.get(&(ctx.body_id, *base))?;
        let base_ty = match base_ty {
            Type::Ref(inner, _) => inner.as_ref(),
            ty => ty,
        };
        let Type::Struct(struct_id, _) = base_ty else {
            return None;
        };
        self.hir.item_tree.structs[*struct_id]
            .fields
            .get(index)
            .map(|field| field.is_mut)
    }

    /// Whether any field on the path from the root to `expr_id` is declared
    /// `mut`. A `mut` field hands out mutable access to everything below it,
    /// and a `mut` field below a plain one is writable on its own account, so
    /// either position makes the whole path writable.
    fn path_has_mut_field(&self, ctx: &BodyCtx<'_>, expr_id: ExprId) -> bool {
        let mut current = expr_id;
        loop {
            match &ctx.body.exprs[current] {
                Expr::FieldAccess { base, .. } => {
                    if self.field_is_mut(ctx, current) == Some(true) {
                        return true;
                    }
                    current = *base;
                }
                Expr::IndexAccess { base, .. } => current = *base,
                Expr::Unary {
                    operand,
                    op: UnaryOp::Deref,
                } => current = *operand,
                _ => return false,
            }
        }
    }

    fn resolve_field_index(
        &self,
        body_id: BodyId,
        base: ExprId,
        field: &hir::Name,
    ) -> Option<usize> {
        let ty = self.type_result.expr_types.get(&(body_id, base))?;
        let ty = match ty {
            Type::Ref(inner, _) => inner.as_ref(),
            ty => ty,
        };
        match ty {
            Type::Struct(struct_id, _) => self.hir.item_tree.structs[*struct_id]
                .fields
                .iter()
                .position(|candidate| candidate.name == *field),
            Type::Tuple(elements) => field
                .0
                .parse::<usize>()
                .ok()
                .filter(|index| *index < elements.len()),
            _ => None,
        }
    }

    fn has_any_borrow(ctx: &BodyCtx<'_>, place: &Place) -> bool {
        let place = access_place_from_move_place(place);
        Self::has_any_access_borrow(ctx, &place)
    }

    /// True when moving `place` conflicts with an active loan, ignoring the
    /// seed loan a reference parameter carries into the function.
    ///
    /// A `&T`/`&mut T` parameter's seed loan represents the incoming borrow;
    /// moving the reference itself (forwarding it) or moving through its raw
    /// pointers inside `unsafe` blocks is how the standard library uses its
    /// parameters. Loans created inside the body still conflict as usual.
    fn has_conflicting_place_move_borrow(ctx: &BodyCtx<'_>, place: &Place) -> bool {
        let access = access_place_from_move_place(place);
        let seed_loans = match place.root {
            hir::place::PlaceRoot::Param(index) => ctx.param_origins.get(&index),
            hir::place::PlaceRoot::LambdaParam { .. } | hir::place::PlaceRoot::Pattern(_) => None,
        };
        ctx.loans.iter().any(|(id, loan)| {
            loan.active
                && access_places_overlap(&loan.place, &access)
                && !seed_loans
                    .is_some_and(|origins| origins.iter().any(|origin| origin.loan == *id))
        })
    }

    fn check_trait_index_receiver_borrow(
        &mut self,
        ctx: &BodyCtx<'_>,
        expr_id: ExprId,
        base: ExprId,
        span: Option<TextRange>,
    ) {
        let Some(call) = self
            .type_result
            .trait_method_calls
            .get(&(ctx.body_id, expr_id))
        else {
            return;
        };
        let kind = if call.method == "index_mut" {
            BorrowKind::Mutable
        } else if call.method == "index" {
            BorrowKind::Shared
        } else {
            return;
        };
        let targets = if self.expr_is_reference(ctx, base) {
            Self::origin_targets(ctx, base)
        } else {
            self.access_targets(ctx, base)
        };
        for target in targets {
            self.borrow_conflicts(ctx, &target.place, kind, &target.parents, span, base);
        }
    }

    fn has_any_access_borrow(ctx: &BodyCtx<'_>, place: &AccessPlace) -> bool {
        ctx.loans
            .values()
            .any(|loan| loan.active && access_places_overlap(&loan.place, place))
    }

    fn has_access_borrow_except_origins(
        ctx: &BodyCtx<'_>,
        place: &AccessPlace,
        expr_id: ExprId,
    ) -> bool {
        let origins = ctx.expr_origins.get(&expr_id);
        ctx.loans.iter().any(|(id, loan)| {
            loan.active
                && access_places_overlap(&loan.place, place)
                && !origins.is_some_and(|origins| origins.iter().any(|origin| origin.loan == *id))
        })
    }

    /// Places a call may write through the shared references it is handed.
    ///
    /// Every `&T` argument is considered, not just a method's receiver: a free
    /// function or another type's method can write `T`'s `mut` fields through a
    /// `&T` parameter just as a `&self` method can. The callee's interior-write
    /// summary says which fields those are; when there is no concrete callee to
    /// summarise, every `mut` field of the argument's type is assumed.
    fn interior_write_places(
        &self,
        ctx: &BodyCtx<'_>,
        inputs: &[ExprId],
        modes: &[Option<BorrowKind>],
        fid: Option<FunctionId>,
    ) -> Vec<AccessPlace> {
        let summary = fid.and_then(|fid| self.interior_writes.get(&fid));
        let mut places = Vec::new();
        for (index, input) in inputs.iter().enumerate() {
            if modes.get(index).copied().flatten() != Some(BorrowKind::Shared) {
                continue;
            }
            let place_expr = peel_reference(ctx, *input);
            let Some(mut_fields) = self.mut_field_indices(ctx, place_expr) else {
                continue;
            };
            if mut_fields.is_empty() {
                continue;
            }
            let written = match summary.map(|summary| summary.params.get(&index)) {
                Some(Some(None)) | None => WrittenFields::Unknown,
                Some(Some(Some(paths))) => WrittenFields::Paths(paths),
                Some(None) => WrittenFields::Nothing,
            };
            for target in self.access_targets(ctx, place_expr) {
                match &written {
                    WrittenFields::Nothing => {}
                    WrittenFields::Unknown => {
                        for field in &mut_fields {
                            places.push(target.place.clone().field(*field));
                        }
                    }
                    WrittenFields::Paths(paths) => {
                        for path in *paths {
                            let mut place = target.place.clone();
                            for step in path {
                                place = place.field(*step);
                            }
                            places.push(place);
                        }
                    }
                }
            }
        }
        places
    }

    fn access_targets(&self, ctx: &BodyCtx<'_>, expr_id: ExprId) -> Vec<AccessTarget> {
        match &ctx.body.exprs[expr_id] {
            Expr::Path {
                resolved: Some(ResolvedName::PatternBinding(id)),
                ..
            } => vec![AccessTarget {
                place: AccessPlace::new(AccessRoot::Pattern(*id)),
                parents: HashSet::new(),
            }],
            Expr::Path {
                resolved: Some(ResolvedName::Param(index)),
                ..
            } => vec![AccessTarget {
                place: AccessPlace::new(AccessRoot::Param(*index)),
                parents: HashSet::new(),
            }],
            Expr::Path {
                resolved: Some(ResolvedName::LambdaParam { lambda, index }),
                ..
            } => vec![AccessTarget {
                place: AccessPlace::new(AccessRoot::LambdaParam {
                    lambda: *lambda,
                    index: *index,
                }),
                parents: HashSet::new(),
            }],
            Expr::FieldAccess { base, field } => {
                let index = self.resolve_field_index(ctx.body_id, *base, field);
                let mut targets = if self.expr_is_reference(ctx, *base) {
                    Self::origin_targets(ctx, *base)
                } else {
                    self.access_targets(ctx, *base)
                };
                let Some(index) = index else {
                    return targets;
                };
                for target in &mut targets {
                    target.place = target.place.clone().field(index);
                }
                targets
            }
            Expr::IndexAccess { base, index } => {
                let mut targets = if self.expr_is_reference(ctx, *base) {
                    Self::origin_targets(ctx, *base)
                } else {
                    self.access_targets(ctx, *base)
                };
                let index = match &ctx.body.exprs[*index] {
                    Expr::IntLiteral { value, .. } => usize::try_from(*value).ok(),
                    _ => None,
                };
                for target in &mut targets {
                    target.place = target.place.clone().index(index);
                }
                targets
            }
            Expr::Unary {
                operand,
                op: UnaryOp::Deref,
            } => Self::origin_targets(ctx, *operand),
            _ => Vec::new(),
        }
    }

    fn local_assignment(ctx: &BodyCtx<'_>, expr_id: ExprId) -> Option<(PatternBindingId, bool)> {
        match &ctx.body.exprs[expr_id] {
            Expr::Path {
                resolved: Some(ResolvedName::PatternBinding(id)),
                ..
            } => Some((*id, true)),
            Expr::FieldAccess { base, .. } | Expr::IndexAccess { base, .. } => {
                Self::local_assignment(ctx, *base).map(|(id, _)| (id, false))
            }
            _ => None,
        }
    }

    fn origin_targets(ctx: &BodyCtx<'_>, expr_id: ExprId) -> Vec<AccessTarget> {
        let mut targets: HashMap<AccessPlace, HashSet<LoanId>> = HashMap::new();
        for origin in ctx.expr_origins.get(&expr_id).into_iter().flatten() {
            targets
                .entry(origin.place.clone())
                .or_default()
                .extend(ctx.loan_family(origin.loan));
        }
        targets
            .into_iter()
            .map(|(place, parents)| AccessTarget { place, parents })
            .collect()
    }

    fn expr_is_reference(&self, ctx: &BodyCtx<'_>, expr_id: ExprId) -> bool {
        matches!(
            self.type_result.expr_types.get(&(ctx.body_id, expr_id)),
            Some(Type::Ref(..))
        )
    }

    #[allow(clippy::type_complexity)]
    fn call_signature(
        &self,
        ctx: &BodyCtx<'_>,
        callee: ExprId,
        args: &[ExprId],
    ) -> (
        Vec<ExprId>,
        Vec<Option<BorrowKind>>,
        Option<FunctionId>,
        Option<(hir::item_tree::TraitId, String)>,
    ) {
        // A resolved concrete callee (an impl method or a trait default
        // method) is authoritative: it supplies the actual parameter modes
        // and — via the reference-flow summary — where the return value's
        // borrows really come from, instead of the trait-branch fallback
        // that ties an elided return reference to every reference input.
        let fid = match self.type_result.expr_types.get(&(ctx.body_id, callee)) {
            Some(Type::FunctionItem { function: fid, .. }) => Some(*fid),
            _ => None,
        };
        if let Some(fid) = fid {
            let function = &self.hir.item_tree.functions[fid];
            let is_method = matches!(ctx.body.exprs[callee], Expr::FieldAccess { .. })
                && !function.params.is_empty();
            let mut inputs = Vec::new();
            if is_method && let Expr::FieldAccess { base, .. } = &ctx.body.exprs[callee] {
                inputs.push(*base);
            }
            inputs.extend(args.iter().copied());
            let modes = function
                .params
                .iter()
                .take(inputs.len())
                .map(|param| hir_ref_kind(&param.ty))
                .collect();
            return (inputs, modes, Some(fid), None);
        }

        if let Some(call) = self
            .type_result
            .trait_method_calls
            .get(&(ctx.body_id, callee))
            && let Expr::FieldAccess { base, .. } = &ctx.body.exprs[callee]
            && let Some(function) = self.hir.item_tree.traits[call.trait_id]
                .methods
                .iter()
                .find(|method| method.name.0 == call.method)
        {
            let inputs = std::iter::once(*base)
                .chain(args.iter().copied())
                .collect::<Vec<_>>();
            let modes = function
                .params
                .iter()
                .take(inputs.len())
                .map(|param| hir_ref_kind(&param.ty))
                .collect();
            // No concrete callee (generic-bound or dynamic dispatch): the
            // joined summaries of every impl of this trait method stand in.
            let trait_call = if call.dynamic {
                None
            } else {
                Some((call.trait_id, call.method.clone()))
            };
            return (inputs, modes, None, trait_call);
        }

        let inputs = args.to_vec();
        let modes = self
            .type_result
            .expr_types
            .get(&(ctx.body_id, callee))
            .and_then(callable_parameter_modes)
            .unwrap_or_else(|| vec![None; inputs.len()]);
        (inputs, modes, None, None)
    }

    #[allow(clippy::too_many_arguments)]
    fn check_call_borrows(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        call: ExprId,
        inputs: &[ExprId],
        modes: &[Option<BorrowKind>],
        fid: Option<FunctionId>,
        trait_call: Option<(hir::item_tree::TraitId, String)>,
        span: Option<TextRange>,
    ) -> OriginValue {
        let mut prepared = Vec::with_capacity(inputs.len());
        for (index, input) in inputs.iter().enumerate() {
            let Some(kind) = modes.get(index).copied().flatten() else {
                prepared.push(ctx.expr_origin_value(*input));
                continue;
            };

            // Taking `&mut` of a place that is itself reached through a shared
            // reference (`self.field.push(..)` inside a `&self` method) mutates
            // through the shared borrow. An explicit `&mut r.field` argument is
            // checked where the borrow is created, so it is skipped here to
            // keep the diagnostic single.
            if kind == BorrowKind::Mutable
                && !matches!(
                    ctx.body.exprs[*input],
                    Expr::Unary {
                        op: UnaryOp::MutRef,
                        ..
                    }
                )
                && self.writes_through_shared_reference(ctx, *input)
            {
                let name = Self::expr_name(ctx, *input);
                self.diag(
                    format!("cannot borrow `{name}` as mutable through a shared reference"),
                    span,
                    "E0309",
                );
                prepared.push(OriginValue::default());
                continue;
            }

            let targets = if self.expr_is_reference(ctx, *input) {
                Self::origin_targets(ctx, *input)
            } else {
                self.access_targets(ctx, *input)
            };
            let mut origins = Origins::new();
            for target in targets {
                if self.borrow_conflicts(ctx, &target.place, kind, &target.parents, span, *input) {
                    continue;
                }
                let loan = ctx.new_loan_with_parents(
                    target.place.clone(),
                    kind,
                    span,
                    false,
                    target.parents,
                );
                origins.insert(Origin {
                    place: target.place,
                    kind,
                    loan,
                });
            }
            prepared.push(OriginValue::from_origins(origins));
        }

        // A callee can still write the `mut` fields of what it is handed, and
        // its own checks cannot see the loans this caller holds. Rejecting a
        // call whose write would invalidate a live borrow *into* one of those
        // fields is all that is needed — the write lasts only for the call, so
        // no loan outlives this check.
        for path in self.interior_write_places(ctx, inputs, modes, fid) {
            self.borrow_conflicts_named_ext(
                ctx,
                &path,
                BorrowKind::Mutable,
                &HashSet::new(),
                span,
                &Self::expr_name(ctx, call),
                false,
                true,
            );
        }

        let may_carry_reference = self.expr_may_carry_reference(ctx, call);
        let summary = may_carry_reference
            .then(|| {
                fid.and_then(|fid| self.reference_flow.summary(fid))
                    .or_else(|| {
                        trait_call.as_ref().and_then(|(trait_id, method)| {
                            self.reference_flow.trait_method_summary(*trait_id, method)
                        })
                    })
            })
            .flatten();
        let result = summary.map_or_else(
            || {
                if may_carry_reference {
                    let mut result = OriginValue::default();
                    for value in &prepared {
                        result.merge(value.flattened());
                    }
                    result
                } else {
                    OriginValue::default()
                }
            },
            |summary| self.instantiate_call_summary(ctx, summary, &prepared, inputs, span),
        );

        let retained = result
            .origins
            .iter()
            .map(|origin| origin.loan)
            .collect::<HashSet<_>>();
        for (index, value) in prepared.iter().enumerate() {
            if modes.get(index).copied().flatten().is_none() {
                continue;
            }
            for origin in &value.origins {
                if !retained.contains(&origin.loan)
                    && let Some(loan) = ctx.loans.get_mut(&origin.loan)
                {
                    loan.active = false;
                }
            }
        }

        for input in inputs {
            Self::deactivate_unretained(ctx, *input, &retained);
        }

        result
    }

    fn instantiate_call_summary(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        summary: &FunctionSummary,
        inputs: &[OriginValue],
        input_exprs: &[ExprId],
        span: Option<TextRange>,
    ) -> OriginValue {
        let mut mapped = HashMap::new();
        self.instantiate_call_summary_inner(ctx, summary, inputs, input_exprs, span, &mut mapped)
    }

    fn instantiate_call_summary_inner(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        summary: &FunctionSummary,
        inputs: &[OriginValue],
        input_exprs: &[ExprId],
        span: Option<TextRange>,
        mapped: &mut HashMap<(SummaryOrigin, LoanId), Option<Origin>>,
    ) -> OriginValue {
        let mut result = OriginValue::default();
        for source in &summary.origins {
            let Some(input) = inputs.get(source.param) else {
                continue;
            };
            let refined = self.refine_summary_input(ctx, source, input_exprs);
            let (effective, excluded) = match refined {
                Some(refined) => {
                    // A refined mapping routes through the receiver's stored
                    // loans while the call's own receiver loan exists
                    // alongside it: exclude that loan family too, or the two
                    // would flag each other within the same call.
                    let excluded = input
                        .origins
                        .iter()
                        .flat_map(|origin| ctx.loan_family(origin.loan))
                        .collect::<HashSet<_>>();
                    (refined, excluded)
                }
                None => (input.clone(), HashSet::new()),
            };
            result.merge(self.map_summary_input(
                ctx,
                &effective,
                source.clone(),
                input_exprs,
                span,
                mapped,
                &excluded,
            ));
        }
        if !summary.fields.is_empty() {
            result.fields = summary
                .fields
                .iter()
                .map(|field| {
                    self.instantiate_call_summary_inner(
                        ctx,
                        field,
                        inputs,
                        input_exprs,
                        span,
                        mapped,
                    )
                })
                .collect();
        }
        if summary.opaque {
            for input in inputs {
                result.merge(input.flattened());
            }
        }
        result
    }

    /// Refines a path-carrying summary origin ("the return aliases the
    /// receiver's `values` field") through the input expression's stored
    /// per-field origins — an iterator's `next` borrows the reference stored
    /// inside the iterator, not the iterator itself, so the returned element
    /// reference carries that stored loan and the call's own `&mut self`
    /// receiver loan can expire. Returns `None` when the input's stored value
    /// has no structure for the path (an owned receiver, say): the return
    /// then flows through the call's own receiver borrow, the historical
    /// whole-input mapping.
    fn refine_summary_input(
        &self,
        ctx: &BodyCtx<'_>,
        source: &SummaryOrigin,
        input_exprs: &[ExprId],
    ) -> Option<OriginValue> {
        if source.path.is_empty() {
            return None;
        }
        let expr = *input_exprs.get(source.param)?;
        let stored = match &ctx.body.exprs[expr] {
            Expr::Path {
                resolved: Some(ResolvedName::PatternBinding(id)),
                ..
            } => ctx.local_origin_value(*id),
            _ => ctx.expr_origin_value(expr),
        };
        if stored.origins.is_empty() && stored.fields.is_empty() {
            return None;
        }
        let mut projected = stored;
        for projection in &source.path {
            projected = match projection {
                FlowProjection::Field(index) | FlowProjection::Index(Some(index)) => {
                    projected.project(*index)
                }
                FlowProjection::Index(None) => projected.iterated(),
            };
        }
        if projected.origins.is_empty() && projected.fields.is_empty() {
            return None;
        }
        Some(projected)
    }

    #[allow(clippy::too_many_arguments)]
    fn map_summary_input(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        input: &OriginValue,
        source: SummaryOrigin,
        input_exprs: &[ExprId],
        span: Option<TextRange>,
        mapped: &mut HashMap<(SummaryOrigin, LoanId), Option<Origin>>,
        excluded: &HashSet<LoanId>,
    ) -> OriginValue {
        let origins = input
            .origins
            .iter()
            .filter_map(|origin| {
                if source.kind == FlowKind::Inherit {
                    return Some(origin.clone());
                }
                let key = (source.clone(), origin.loan);
                if let Some(mapped) = mapped.get(&key) {
                    return mapped.clone();
                }
                let kind = BorrowKind::from_flow(source.kind, origin.kind);
                if kind == origin.kind {
                    mapped.insert(key, Some(origin.clone()));
                    return Some(origin.clone());
                }
                let mut parents = ctx.loan_family(origin.loan);
                parents.extend(excluded.iter().copied());
                let name = Self::expr_name(ctx, input_exprs[source.param]);
                let mapped_origin = if self.borrow_conflicts_named_ext(
                    ctx,
                    &origin.place,
                    kind,
                    &parents,
                    span,
                    &name,
                    source.behind_reference,
                    false,
                ) {
                    None
                } else {
                    let loan = ctx.new_loan_ext(
                        origin.place.clone(),
                        kind,
                        span,
                        false,
                        parents,
                        source.behind_reference,
                    );
                    Some(Origin {
                        place: origin.place.clone(),
                        kind,
                        loan,
                    })
                };
                mapped.insert(key, mapped_origin.clone());
                mapped_origin
            })
            .collect();
        let fields = input
            .fields
            .iter()
            .map(|field| {
                self.map_summary_input(
                    ctx,
                    field,
                    source.clone(),
                    input_exprs,
                    span,
                    mapped,
                    excluded,
                )
            })
            .collect();
        OriginValue { origins, fields }
    }

    fn expr_may_carry_reference(&self, ctx: &BodyCtx<'_>, expr_id: ExprId) -> bool {
        self.type_result
            .expr_types
            .get(&(ctx.body_id, expr_id))
            .is_none_or(|ty| type_may_carry_reference(self.hir, ty))
    }

    fn deactivate_unretained(ctx: &mut BodyCtx<'_>, expr_id: ExprId, retained: &HashSet<LoanId>) {
        let origins = ctx.expr_origins.get(&expr_id).cloned().unwrap_or_default();
        for origin in origins {
            if retained.contains(&origin.loan) {
                continue;
            }
            ctx.deactivate_loan_if_unheld(origin.loan);
        }
    }

    fn create_borrow(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        borrow_expr: ExprId,
        operand: ExprId,
        kind: BorrowKind,
        span: Option<TextRange>,
    ) -> Origins {
        // A `&mut` of a place reached through a shared reference would hand out
        // write access the borrow does not have. Checking here covers every
        // position a `&mut` can appear in — a binding, a return value, a struct
        // field, an argument — while implicit receiver borrows have no `&mut`
        // expression to check and are handled at the call site.
        if kind == BorrowKind::Mutable && self.reached_through_shared_reference(ctx, operand) {
            let name = Self::expr_name(ctx, operand);
            // A `mut` field is writable through the borrow, but only for the
            // call it is written in: anywhere else the `&mut` could be bound,
            // returned or stored, and two of them could then alias.
            let call_scoped = self.path_has_mut_field(ctx, operand)
                && ctx.call_borrow_exprs.contains(&borrow_expr);
            if !call_scoped {
                self.diag(
                    format!("cannot borrow `{name}` as mutable through a shared reference"),
                    span,
                    "E0309",
                );
                return Origins::new();
            }
        }
        let mut origins = Origins::new();
        for target in self.access_targets(ctx, operand) {
            if self.borrow_conflicts(ctx, &target.place, kind, &target.parents, span, operand) {
                continue;
            }
            let loan = ctx.new_loan_with_parents(
                target.place.clone(),
                kind,
                span,
                false,
                target.parents.clone(),
            );
            origins.insert(Origin {
                place: target.place,
                kind,
                loan,
            });
        }
        origins
    }

    fn borrow_conflicts(
        &mut self,
        ctx: &BodyCtx<'_>,
        place: &AccessPlace,
        kind: BorrowKind,
        parents: &HashSet<LoanId>,
        span: Option<TextRange>,
        expr_id: ExprId,
    ) -> bool {
        let name = Self::expr_name(ctx, expr_id);
        self.borrow_conflicts_named_ext(ctx, place, kind, parents, span, &name, false, false)
    }

    fn borrow_conflicts_named(
        &mut self,
        ctx: &BodyCtx<'_>,
        place: &AccessPlace,
        kind: BorrowKind,
        parents: &HashSet<LoanId>,
        span: Option<TextRange>,
        name: &str,
    ) -> bool {
        self.borrow_conflicts_named_ext(ctx, place, kind, parents, span, name, false, false)
    }

    /// `attempted_behind_reference` marks borrows whose region lives behind
    /// a reference (a summary origin that crossed a reference-typed field).
    /// Such a region is disjoint from the receiver's own storage, and vice
    /// versa, so a conflict where exactly one side is behind a reference can
    /// never materialize and is skipped.
    #[allow(clippy::too_many_arguments)]
    fn borrow_conflicts_named_ext(
        &mut self,
        ctx: &BodyCtx<'_>,
        place: &AccessPlace,
        kind: BorrowKind,
        parents: &HashSet<LoanId>,
        span: Option<TextRange>,
        name: &str,
        attempted_behind_reference: bool,
        attempted_interior_write: bool,
    ) -> bool {
        let conflict = ctx.loans.iter().find_map(|(id, loan)| {
            (loan.active
                && !parents.contains(id)
                && loan.behind_reference == attempted_behind_reference
                && access_places_overlap(&loan.place, place)
                && !(loan.kind == BorrowKind::Shared && kind == BorrowKind::Shared)
                // A shared borrow of an enclosing place does not freeze a `mut`
                // field inside it — that is what the declaration is for. A
                // borrow *into* the field is a different matter and still
                // conflicts, because the write can move what it points at.
                && !(attempted_interior_write
                    && loan.kind == BorrowKind::Shared
                    && access_place_encloses(&loan.place, place)))
            .then(|| loan.clone())
        });
        let Some(conflict) = conflict else {
            return false;
        };

        let (code, message) = match (kind, conflict.kind) {
            (BorrowKind::Mutable, BorrowKind::Shared) => (
                "E0300",
                format!(
                    "cannot borrow `{name}` as mutable because it is also borrowed as immutable"
                ),
            ),
            (BorrowKind::Shared, BorrowKind::Mutable) => (
                "E0301",
                format!(
                    "cannot borrow `{name}` as immutable because it is also borrowed as mutable"
                ),
            ),
            (BorrowKind::Mutable, BorrowKind::Mutable) => (
                "E0302",
                format!("cannot borrow `{name}` as mutable more than once at a time"),
            ),
            (BorrowKind::Shared, BorrowKind::Shared) => unreachable!(),
        };
        let labels = conflict
            .issued_at
            .map(|range| {
                vec![(
                    range,
                    "first borrow occurs here".into(),
                    LabelStyle::Secondary,
                )]
            })
            .unwrap_or_default();
        self.diag_with_labels(message, span, code, &labels);
        true
    }

    fn has_shared_access_borrow(ctx: &BodyCtx<'_>, place: &AccessPlace) -> bool {
        ctx.loans.values().any(|loan| {
            loan.active
                && loan.kind == BorrowKind::Shared
                && access_places_overlap(&loan.place, place)
        })
    }

    fn has_mut_access_borrow(ctx: &BodyCtx<'_>, place: &AccessPlace) -> bool {
        ctx.loans.values().any(|loan| {
            loan.active
                && loan.kind == BorrowKind::Mutable
                && access_places_overlap(&loan.place, place)
        })
    }

    fn expr_name(ctx: &BodyCtx<'_>, expr_id: ExprId) -> String {
        match &ctx.body.exprs[expr_id] {
            Expr::Path { path, .. } => path
                .as_single_name()
                .map_or_else(|| "_".into(), |n| n.0.as_str().to_string()),
            Expr::FieldAccess { field, .. } => field.0.clone(),
            _ => String::from("_"),
        }
    }

    fn bind_pattern_names(ctx: &mut BodyCtx<'_>, pat: hir::body::PatId) {
        match &ctx.body.pats[pat] {
            hir::body::Pattern::Binding { name, .. } => {
                Self::bind_pattern_name(
                    ctx,
                    PatternBindingId {
                        pattern: pat,
                        field: None,
                    },
                    &name.0,
                );
            }
            hir::body::Pattern::Reference { pattern, .. } => {
                Self::bind_pattern_names(ctx, *pattern);
            }
            hir::body::Pattern::Tuple { elements }
            | hir::body::Pattern::TupleStruct { elements, .. } => {
                for el in elements {
                    Self::bind_pattern_names(ctx, *el);
                }
            }
            hir::body::Pattern::Struct { fields, .. } => {
                for (index, f) in fields.iter().enumerate() {
                    if let Some(p) = f.pat {
                        Self::bind_pattern_names(ctx, p);
                    } else {
                        Self::bind_pattern_name(
                            ctx,
                            PatternBindingId {
                                pattern: pat,
                                field: Some(index),
                            },
                            &f.name.0,
                        );
                    }
                }
            }
            _ => {}
        }
    }

    fn bind_pattern_name(ctx: &mut BodyCtx<'_>, id: PatternBindingId, name: &str) {
        ctx.bindings.insert_available(name.to_string());
        ctx.binding_scopes.insert(id, ctx.scope_depth);
        Self::reset_binding_move(ctx, id);
    }

    fn reset_pattern_moves(ctx: &mut BodyCtx<'_>, pat: PatId) {
        let mut bindings = Vec::new();
        initialization::collect_pattern_bindings(ctx.body, pat, &mut bindings);
        for (id, _) in bindings {
            Self::reset_binding_move(ctx, id);
        }
    }

    fn reset_binding_move(ctx: &mut BodyCtx<'_>, id: PatternBindingId) {
        let place = Place::root(id);
        ctx.moved_places
            .retain(|moved| !place_overlaps(moved, &place));
        ctx.moved_sites
            .retain(|moved, _| !place_overlaps(moved, &place));
    }

    fn bind_pattern_origins(&mut self, ctx: &mut BodyCtx<'_>, pat: PatId, value: &OriginValue) {
        let mut reborrows = HashMap::new();
        self.bind_pattern_origins_inner(ctx, pat, value, &[], &mut reborrows);
    }

    fn bind_pattern_origins_inner(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        pat: PatId,
        value: &OriginValue,
        projection: &[AccessProjection],
        reborrows: &mut HashMap<(LoanId, BorrowKind, Vec<AccessProjection>), Origin>,
    ) {
        match &ctx.body.pats[pat] {
            Pattern::Binding { .. } => {
                let id = PatternBindingId {
                    pattern: pat,
                    field: None,
                };
                let value =
                    self.pattern_binding_origin_value(ctx, id, value, projection, reborrows);
                ctx.bind_origin_value(id, value);
            }
            Pattern::Reference { pattern, .. } => {
                self.bind_pattern_origins_inner(ctx, *pattern, value, projection, reborrows);
            }
            Pattern::Tuple { elements } | Pattern::TupleStruct { elements, .. } => {
                let elements = elements.clone();
                for (index, element) in elements.into_iter().enumerate() {
                    let (field_value, field_projection) =
                        projected_origin_value(value, projection, index);
                    self.bind_pattern_origins_inner(
                        ctx,
                        element,
                        &field_value,
                        &field_projection,
                        reborrows,
                    );
                }
            }
            Pattern::Struct { fields, .. } => {
                let fields = fields.clone();
                for (binding_index, field) in fields.into_iter().enumerate() {
                    let Some(index) = self.pattern_field_index(ctx, pat, &field.name) else {
                        continue;
                    };
                    let (field_value, field_projection) =
                        projected_origin_value(value, projection, index);
                    if let Some(field_pat) = field.pat {
                        self.bind_pattern_origins_inner(
                            ctx,
                            field_pat,
                            &field_value,
                            &field_projection,
                            reborrows,
                        );
                    } else {
                        let id = PatternBindingId {
                            pattern: pat,
                            field: Some(binding_index),
                        };
                        let field_value = self.pattern_binding_origin_value(
                            ctx,
                            id,
                            &field_value,
                            &field_projection,
                            reborrows,
                        );
                        ctx.bind_origin_value(id, field_value);
                    }
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Path { .. } | Pattern::Or { .. } => {
            }
        }
    }

    fn pattern_binding_origin_value(
        &mut self,
        ctx: &mut BodyCtx<'_>,
        id: PatternBindingId,
        value: &OriginValue,
        projection: &[AccessProjection],
        reborrows: &mut HashMap<(LoanId, BorrowKind, Vec<AccessProjection>), Origin>,
    ) -> OriginValue {
        let kind = match self
            .type_result
            .pattern_binding_modes
            .get(&(ctx.body_id, id))
        {
            Some(PatternBindingMode::Ref) => BorrowKind::Shared,
            Some(PatternBindingMode::RefMut) => BorrowKind::Mutable,
            _ => {
                return self
                    .type_result
                    .pattern_binding_types
                    .get(&(ctx.body_id, id))
                    .filter(|ty| type_may_carry_reference(self.hir, ty))
                    .map(|_| value.clone())
                    .unwrap_or_default();
            }
        };
        let mut origins = Origins::new();
        for origin in &value.origins {
            let key = (origin.loan, kind, projection.to_vec());
            if let Some(reborrow) = reborrows.get(&key) {
                origins.insert(reborrow.clone());
                continue;
            }
            let mut place = origin.place.clone();
            for projection in projection {
                place = match projection {
                    AccessProjection::Field(index) => place.field(*index),
                    AccessProjection::Index(index) => place.index(*index),
                };
            }
            let parents = ctx.loan_family(origin.loan);
            let span = ctx.source_map.pat_ranges.get(&id.pattern).copied();
            if self.borrow_conflicts_named(ctx, &place, kind, &parents, span, "pattern binding") {
                continue;
            }
            let loan = ctx.new_loan_with_parents(place.clone(), kind, span, false, parents);
            let reborrow = Origin { place, kind, loan };
            reborrows.insert(key, reborrow.clone());
            origins.insert(reborrow);
        }
        OriginValue::from_origins(origins)
    }

    fn pattern_move_places(&self, ctx: &BodyCtx<'_>, pat: PatId, root: &Place) -> Vec<Place> {
        let mut places = Vec::new();
        self.collect_pattern_move_places(ctx, pat, root, &mut places);
        places
    }

    fn collect_pattern_move_places(
        &self,
        ctx: &BodyCtx<'_>,
        pat: PatId,
        root: &Place,
        places: &mut Vec<Place>,
    ) {
        let binding_moves = |id| {
            self.type_result
                .pattern_binding_modes
                .get(&(ctx.body_id, id))
                == Some(&PatternBindingMode::Move)
                && self
                    .type_result
                    .pattern_binding_types
                    .get(&(ctx.body_id, id))
                    .is_some_and(|ty| !self.type_is_copy(ctx, ty))
        };
        match &ctx.body.pats[pat] {
            Pattern::Binding { .. } => {
                if binding_moves(PatternBindingId {
                    pattern: pat,
                    field: None,
                }) {
                    places.push(root.clone());
                }
            }
            Pattern::Tuple { elements } | Pattern::TupleStruct { elements, .. } => {
                for (index, element) in elements.iter().enumerate() {
                    self.collect_pattern_move_places(
                        ctx,
                        *element,
                        &root.clone().field(index),
                        places,
                    );
                }
            }
            Pattern::Struct { fields, .. } => {
                for (binding_index, field) in fields.iter().enumerate() {
                    let Some(index) = self.pattern_field_index(ctx, pat, &field.name) else {
                        continue;
                    };
                    let field_place = root.clone().field(index);
                    if let Some(field_pat) = field.pat {
                        self.collect_pattern_move_places(ctx, field_pat, &field_place, places);
                    } else if binding_moves(PatternBindingId {
                        pattern: pat,
                        field: Some(binding_index),
                    }) {
                        places.push(field_place);
                    }
                }
            }
            Pattern::Reference { .. }
            | Pattern::Wildcard
            | Pattern::Literal(_)
            | Pattern::Path { .. }
            | Pattern::Or { .. } => {}
        }
    }

    fn pattern_field_index(
        &self,
        ctx: &BodyCtx<'_>,
        pat: PatId,
        field: &hir::Name,
    ) -> Option<usize> {
        let ty = self.type_result.pattern_types.get(&(ctx.body_id, pat))?;
        match ty {
            Type::Struct(id, _) => self.hir.item_tree.structs[*id]
                .fields
                .iter()
                .position(|item| item.name == *field),
            Type::Enum(id, _) => {
                let Pattern::Struct { path, .. } = &ctx.body.pats[pat] else {
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

    fn check_pattern_move_from_drop(&mut self, ctx: &BodyCtx<'_>, pat: PatId, ty: &Type) {
        let Some(pat) = self.pattern_move_from_drop(ctx, pat, ty) else {
            return;
        };
        self.diag(
            "cannot move out of a field of a type that implements `Drop`".into(),
            ctx.source_map.pat_ranges.get(&pat).copied(),
            "E0305",
        );
    }

    fn pattern_move_from_drop(&self, ctx: &BodyCtx<'_>, pat: PatId, ty: &Type) -> Option<PatId> {
        if self.trait_env.type_has_explicit_drop(ty)
            && matches!(
                ctx.body.pats[pat],
                Pattern::Tuple { .. } | Pattern::TupleStruct { .. } | Pattern::Struct { .. }
            )
            && self.pattern_moves_non_copy(ctx, pat)
        {
            return Some(pat);
        }
        let children = match &ctx.body.pats[pat] {
            Pattern::Tuple { elements } | Pattern::TupleStruct { elements, .. } => elements.clone(),
            Pattern::Struct { fields, .. } => fields.iter().filter_map(|field| field.pat).collect(),
            _ => Vec::new(),
        };
        children.into_iter().find_map(|child| {
            let child_ty = self.type_result.pattern_types.get(&(ctx.body_id, child))?;
            self.pattern_move_from_drop(ctx, child, child_ty)
        })
    }

    fn pattern_moves_non_copy(&self, ctx: &BodyCtx<'_>, pat: PatId) -> bool {
        let binding_moves = |id| {
            self.type_result
                .pattern_binding_modes
                .get(&(ctx.body_id, id))
                == Some(&PatternBindingMode::Move)
                && self
                    .type_result
                    .pattern_binding_types
                    .get(&(ctx.body_id, id))
                    .is_some_and(|ty| !self.type_is_copy(ctx, ty))
        };
        match &ctx.body.pats[pat] {
            Pattern::Binding { .. } => binding_moves(PatternBindingId {
                pattern: pat,
                field: None,
            }),
            Pattern::Tuple { elements } | Pattern::TupleStruct { elements, .. } => elements
                .iter()
                .any(|element| self.pattern_moves_non_copy(ctx, *element)),
            Pattern::Struct { fields, .. } => fields.iter().enumerate().any(|(index, field)| {
                field
                    .pat
                    .is_some_and(|pat| self.pattern_moves_non_copy(ctx, pat))
                    || (field.pat.is_none()
                        && binding_moves(PatternBindingId {
                            pattern: pat,
                            field: Some(index),
                        }))
            }),
            Pattern::Reference { .. }
            | Pattern::Wildcard
            | Pattern::Literal(_)
            | Pattern::Path { .. }
            | Pattern::Or { .. } => false,
        }
    }

    fn check_explicit_reference_pattern_move(&mut self, ctx: &BodyCtx<'_>, pat: PatId) {
        let Some(binding) = self.explicit_reference_pattern_move(ctx, pat, false) else {
            return;
        };
        self.diag(
            "cannot move out of dereference of a non-Copy value".into(),
            ctx.source_map.pat_ranges.get(&binding).copied(),
            "E0308",
        );
    }

    fn explicit_reference_pattern_move(
        &self,
        ctx: &BodyCtx<'_>,
        pat: PatId,
        behind_reference: bool,
    ) -> Option<PatId> {
        let binding_moves = |id| {
            self.type_result
                .pattern_binding_modes
                .get(&(ctx.body_id, id))
                == Some(&PatternBindingMode::Move)
                && self
                    .type_result
                    .pattern_binding_types
                    .get(&(ctx.body_id, id))
                    .is_some_and(|ty| !self.type_is_copy(ctx, ty))
        };
        match &ctx.body.pats[pat] {
            Pattern::Binding { .. }
                if behind_reference
                    && binding_moves(PatternBindingId {
                        pattern: pat,
                        field: None,
                    }) =>
            {
                Some(pat)
            }
            Pattern::Reference { pattern, .. } => {
                self.explicit_reference_pattern_move(ctx, *pattern, true)
            }
            Pattern::Tuple { elements } | Pattern::TupleStruct { elements, .. } => {
                elements.iter().find_map(|element| {
                    self.explicit_reference_pattern_move(ctx, *element, behind_reference)
                })
            }
            Pattern::Struct { fields, .. } => {
                fields.iter().enumerate().find_map(|(index, field)| {
                    field.pat.map_or_else(
                        || {
                            (behind_reference
                                && binding_moves(PatternBindingId {
                                    pattern: pat,
                                    field: Some(index),
                                }))
                            .then_some(pat)
                        },
                        |field_pattern| {
                            self.explicit_reference_pattern_move(
                                ctx,
                                field_pattern,
                                behind_reference,
                            )
                        },
                    )
                })
            }
            Pattern::Binding { .. }
            | Pattern::Wildcard
            | Pattern::Literal(_)
            | Pattern::Path { .. }
            | Pattern::Or { .. } => None,
        }
    }

    fn diag(&mut self, message: String, span: Option<TextRange>, code: &'static str) {
        self.diag_with_labels(message, span, code, &[]);
    }

    /// Build secondary labels for the move site that caused this E0100 error.
    fn move_site_labels(ctx: &BodyCtx<'_>, place: &Place) -> Vec<(TextRange, String, LabelStyle)> {
        // Find the most specific moved site — scan for a prefix match.
        let mut best: Option<(&Place, &(Option<TextRange>, String))> = None;
        for (moved_place, site) in &ctx.moved_sites {
            if place_overlaps(moved_place, place) {
                match best {
                    None => best = Some((moved_place, site)),
                    Some((existing, _))
                        if moved_place.projections.len() > existing.projections.len() =>
                    {
                        best = Some((moved_place, site));
                    }
                    _ => {}
                }
            }
        }
        match best {
            Some((_, (Some(range), desc))) => {
                vec![(*range, desc.clone(), LabelStyle::Secondary)]
            }
            _ => vec![],
        }
    }

    fn diag_with_labels(
        &mut self,
        message: String,
        span: Option<TextRange>,
        code: &'static str,
        extra_labels: &[(TextRange, String, LabelStyle)],
    ) {
        if self.diagnostic_suppression > 0 {
            // 循环不动点尚未收敛（或处于外层未收敛循环的重放中），
            // 诊断由收敛后的最终重放统一上报。
            return;
        }
        let span = span.expect("move-checker diagnostics require a source range");
        let notes = match code {
            "E0059" => vec!["assign the binding on every path before reading it".into()],
            "E0100" => vec!["borrow with `&` if the original value must remain usable".into()],
            "E0300" => vec!["a mutable borrow cannot overlap an existing shared borrow".into()],
            "E0301" => vec!["a shared borrow cannot overlap an existing mutable borrow".into()],
            "E0302" => vec!["only one mutable borrow of a place may be active at a time".into()],
            "E0303" => vec!["the borrow must end before assigning to the value".into()],
            "E0304" => vec!["the borrow must end before moving the value".into()],
            "E0307" => vec![
                "borrow the pattern binding in the guard or move it from the selected arm body"
                    .into(),
            ],
            "E0308" => vec![
                "borrow through the reference, or implement `Copy` when duplication is intended"
                    .into(),
            ],
            "E0309" => vec![
                "a shared reference does not permit mutation; take `&mut` or return an owned value"
                    .into(),
            ],
            _ => Vec::new(),
        };
        let mut labels = vec![SourceLabel {
            range: span,
            message: String::new(),
            style: LabelStyle::Primary,
        }];
        for (range, msg, style) in extra_labels {
            labels.push(SourceLabel {
                range: *range,
                message: msg.clone(),
                style: *style,
            });
        }
        let diagnostic = Diagnostic {
            code,
            severity: Severity::Error,
            message,
            labels,
            help: None,
            notes,
        };
        // The root block's tail is visited by both the body walk and the block
        // walk, and recovery paths re-enter the same place: an identical
        // diagnostic adds nothing, so drop the repeat.
        if !self.result.diagnostics.contains(&diagnostic) {
            self.result.diagnostics.push(diagnostic);
        }
    }
}

// ═══════════════════════════════════════════════════════════
// Context types
// ═══════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
struct BorrowRecord {
    place: AccessPlace,
    kind: BorrowKind,
    scope_depth: usize,
    issued_at: Option<TextRange>,
    active: bool,
    permanent: bool,
    /// The loan's real region lives behind a reference (a summary origin
    /// whose path crossed a reference-typed field): it can never conflict
    /// with borrows of the receiver's own storage.
    behind_reference: bool,
    holders: HashSet<PatternBindingId>,
    parents: HashSet<LoanId>,
}

#[derive(Clone)]
struct BodyCtx<'a> {
    function_id: FunctionId,
    body_id: BodyId,
    body: &'a Body,
    source_map: &'a SourceMap,

    // Move tracking
    bindings: MoveBindings,
    moved_places: HashSet<Place>,
    /// Where each place was moved — (span, description) for secondary labels.
    moved_sites: HashMap<Place, (Option<TextRange>, String)>,

    // Borrow and reference provenance tracking
    loans: HashMap<LoanId, BorrowRecord>,
    backedge_reset_loans: HashSet<LoanId>,
    next_loan: LoanId,
    expr_origins: HashMap<ExprId, Origins>,
    local_origins: HashMap<PatternBindingId, Origins>,
    expr_origin_fields: HashMap<ExprId, Vec<OriginValue>>,
    local_origin_fields: HashMap<PatternBindingId, Vec<OriginValue>>,
    param_origins: HashMap<usize, Origins>,
    last_uses: HashMap<PatternBindingId, usize>,
    /// Scope depth at each enclosing loop's entry.
    /// Declaration scope depth of each pattern binding, so loans assigned to
    /// a deferred binding keep living until the binding's own scope ends.
    binding_scopes: HashMap<PatternBindingId, usize>,
    scope_depth: usize,
    in_match_guard: bool,
    /// Places that a match guard must leave intact: the scrutinee place plus
    /// the roots of the arm's whole-value pattern bindings (they alias the
    /// scrutinee). Moves out of them must be rejected in guards: the guard
    /// may fail and the next arm still needs the value.
    guard_scrutinee: Vec<Place>,
    /// Generic bounds in scope for this body (`T: Copy`, plus the enclosing
    /// impl's bounds), so a parameter type is copyable when its bound says so.
    bounds: &'a [TraitBound],
    /// `&mut` expressions that appear directly as a call's receiver or
    /// argument. A `&mut` of a `mut` field reached through a shared reference
    /// is confined to the call it is written in, so only these are allowed;
    /// anywhere else the borrow could be bound or returned.
    call_borrow_exprs: HashSet<ExprId>,
}

impl<'a> BodyCtx<'a> {
    fn new(
        function_id: FunctionId,
        body_id: BodyId,
        body: &'a Body,
        bounds: &'a [TraitBound],
    ) -> Self {
        Self {
            function_id,
            body_id,
            body,
            source_map: &body.source_map,
            bindings: MoveBindings::default(),
            moved_places: HashSet::new(),
            moved_sites: HashMap::new(),
            loans: HashMap::new(),
            backedge_reset_loans: HashSet::new(),
            next_loan: 0,
            expr_origins: HashMap::new(),
            local_origins: HashMap::new(),
            expr_origin_fields: HashMap::new(),
            local_origin_fields: HashMap::new(),
            param_origins: HashMap::new(),
            last_uses: collect_local_uses(body),
            binding_scopes: HashMap::new(),
            scope_depth: 0,
            in_match_guard: false,
            guard_scrutinee: Vec::new(),
            bounds,
            call_borrow_exprs: collect_call_borrow_exprs(body),
        }
    }

    fn seed_params<'b>(&mut self, params: impl IntoIterator<Item = &'b str>) {
        for name in params {
            self.bindings.insert_available(name.to_string());
        }
    }

    fn push_scope(&mut self) {
        self.bindings.push_scope();
        self.scope_depth += 1;
    }
    fn pop_scope(&mut self) {
        self.bindings.pop_scope();
        if self.scope_depth > 0 {
            self.scope_depth -= 1;
        }
        let current = self.scope_depth;
        for loan in self.loans.values_mut() {
            if loan.scope_depth > current && !loan.permanent {
                loan.active = false;
            }
        }
    }

    fn copy_move_state_from(&mut self, other: &Self) {
        self.bindings.clone_from(&other.bindings);
        self.moved_places.clone_from(&other.moved_places);
        self.moved_sites.clone_from(&other.moved_sites);
    }

    fn move_state_snapshot(&self) -> MoveStateSnapshot {
        MoveStateSnapshot {
            bindings: self.bindings.clone(),
            moved_places: self.moved_places.clone(),
            moved_sites: self.moved_sites.clone(),
        }
    }

    fn copy_move_state_snapshot(&mut self, snapshot: &MoveStateSnapshot) {
        self.bindings.clone_from(&snapshot.bindings);
        self.moved_places.clone_from(&snapshot.moved_places);
        self.moved_sites.clone_from(&snapshot.moved_sites);
    }

    fn merge_move_state_snapshot(&mut self, snapshot: &MoveStateSnapshot) {
        self.bindings.merge_moved_from(&snapshot.bindings);
        self.moved_places
            .extend(snapshot.moved_places.iter().cloned());
        self.moved_sites.extend(snapshot.moved_sites.clone());
    }

    /// Merges two branch exits' `local_origins`: for every binding whose
    /// origins differ from the branch entry, keep the union of both branches
    /// and re-activate the unioned loans. A binding assigned a borrow in
    /// either branch may hold that borrow after the `if`, so loans from both
    /// branches must stay live and conflicting.
    fn merge_local_origins(
        ctx: &mut BodyCtx<'_>,
        entry: &HashMap<PatternBindingId, Origins>,
        other: &HashMap<PatternBindingId, Origins>,
    ) {
        let current = ctx.local_origins.clone();
        let mut merged: HashMap<PatternBindingId, Origins> = HashMap::new();
        let mut bindings = current
            .keys()
            .chain(other.keys())
            .copied()
            .collect::<Vec<_>>();
        bindings.sort_unstable_by_key(|id| id.field.map_or(0, |index| index + 1));
        bindings.dedup_by(|a, b| a.pattern == b.pattern && a.field == b.field);
        bindings.dedup();
        for binding in bindings {
            let entry_origins = entry.get(&binding);
            let mut origins = current.get(&binding).cloned().unwrap_or_default();
            let other_origins = other.get(&binding).cloned().unwrap_or_default();
            if entry_origins == Some(&origins) && entry_origins == Some(&other_origins) {
                continue;
            }
            origins.extend(other_origins);
            if origins.is_empty() {
                continue;
            }
            let declared = ctx
                .binding_scopes
                .get(&binding)
                .copied()
                .unwrap_or(ctx.scope_depth);
            for origin in &origins {
                if let Some(record) = ctx.loans.get_mut(&origin.loan) {
                    record.holders.insert(binding);
                    record.scope_depth = record.scope_depth.min(declared);
                    // Re-activate only loans whose scope still covers the
                    // current depth; a loan from a deeper, already-popped
                    // scope (e.g. a match arm binding) stays deactivated.
                    if record.scope_depth <= ctx.scope_depth {
                        record.active = true;
                    }
                }
            }
            merged.insert(binding, origins);
        }
        for (binding, origins) in merged {
            ctx.local_origins.insert(binding, origins);
        }
    }

    // ponytail: wildcard moves are checked within an iteration and reset on
    // backedges for array IntoIterator; per-slot initialization would allow
    // rejecting repeated dynamic-index moves across iterations as well.
    fn merge_loop_head_move_state_from(&mut self, other: &Self) {
        self.bindings.merge_moved_from(&other.bindings);
        self.moved_places.extend(
            other
                .moved_places
                .iter()
                .filter(|place| !place_has_wildcard_index(place))
                .cloned(),
        );
        let filtered = other
            .moved_sites
            .iter()
            .filter(|(place, _)| !place_has_wildcard_index(place));
        self.moved_sites
            .extend(filtered.map(|(place, site)| (place.clone(), site.clone())));
        self.next_loan = other.next_loan;
        self.backedge_reset_loans
            .extend(&other.backedge_reset_loans);
        for (id, record) in &other.loans {
            let entry = self.loans.entry(*id).or_insert_with(|| record.clone());
            entry.active |= record.active;
            entry.holders.extend(&record.holders);
            entry.parents.extend(&record.parents);
            entry.scope_depth = entry.scope_depth.min(record.scope_depth);
            if self.backedge_reset_loans.contains(id) {
                entry.active = false;
                entry.holders.clear();
            }
        }
        for binding in self.binding_scopes.keys().copied().collect::<Vec<_>>() {
            let mut value = self.local_origin_value(binding);
            value.merge(other.local_origin_value(binding));
            discard_reset_origins(&mut value, &self.backedge_reset_loans);
            self.bind_origin_value(binding, value);
        }
    }

    fn same_move_state(&self, other: &Self) -> bool {
        self.bindings == other.bindings
            && self.moved_places == other.moved_places
            && self.local_origins == other.local_origins
            && self
                .loans
                .iter()
                .filter(|(_, loan)| loan.active)
                .all(|(id, _)| other.loans.get(id).is_some_and(|loan| loan.active))
            && other
                .loans
                .iter()
                .filter(|(_, loan)| loan.active)
                .all(|(id, _)| self.loans.get(id).is_some_and(|loan| loan.active))
    }

    fn seed_reference_params<'b>(
        &mut self,
        params: impl IntoIterator<Item = (usize, &'b hir::item_tree::HirParam)>,
    ) {
        for (index, param) in params {
            let kind = match &param.ty {
                HirTypeRef::Ref(_, true) => BorrowKind::Mutable,
                HirTypeRef::Ref(_, false) => BorrowKind::Shared,
                _ => continue,
            };
            let place = AccessPlace::new(AccessRoot::Param(index));
            let loan = self.new_loan(place.clone(), kind, Some(param.name_range), true);
            self.param_origins.insert(
                index,
                std::iter::once(Origin { place, kind, loan }).collect(),
            );
        }
    }
    fn expr_range(&self, id: ExprId) -> Option<TextRange> {
        self.source_map.expr_ranges.get(&id).copied()
    }

    fn new_loan(
        &mut self,
        place: AccessPlace,
        kind: BorrowKind,
        issued_at: Option<TextRange>,
        permanent: bool,
    ) -> LoanId {
        self.new_loan_with_parents(place, kind, issued_at, permanent, HashSet::new())
    }

    fn new_loan_with_parents(
        &mut self,
        place: AccessPlace,
        kind: BorrowKind,
        issued_at: Option<TextRange>,
        permanent: bool,
        parents: HashSet<LoanId>,
    ) -> LoanId {
        self.new_loan_ext(place, kind, issued_at, permanent, parents, false)
    }

    fn new_loan_ext(
        &mut self,
        place: AccessPlace,
        kind: BorrowKind,
        issued_at: Option<TextRange>,
        permanent: bool,
        parents: HashSet<LoanId>,
        behind_reference: bool,
    ) -> LoanId {
        if let Some((id, record)) = self.loans.iter_mut().find(|(_, record)| {
            record.place == place
                && record.kind == kind
                && record.issued_at == issued_at
                && record.permanent == permanent
                && record.behind_reference == behind_reference
        }) {
            record.active = true;
            record.scope_depth = self.scope_depth;
            record.parents.extend(parents);
            return *id;
        }
        let id = self.next_loan;
        self.next_loan += 1;
        self.loans.insert(
            id,
            BorrowRecord {
                place,
                kind,
                scope_depth: self.scope_depth,
                issued_at,
                active: true,
                permanent,
                behind_reference,
                holders: HashSet::new(),
                parents,
            },
        );
        id
    }

    fn loan_family(&self, id: LoanId) -> HashSet<LoanId> {
        let mut family = HashSet::from([id]);
        let mut pending = vec![id];
        while let Some(current) = pending.pop() {
            let Some(loan) = self.loans.get(&current) else {
                continue;
            };
            for parent in &loan.parents {
                if family.insert(*parent) {
                    pending.push(*parent);
                }
            }
        }
        family
    }

    fn bind_origins(&mut self, binding: PatternBindingId, origins: Origins) {
        if let Some(previous) = self.local_origins.insert(binding, origins.clone()) {
            let mut released = Vec::new();
            for origin in previous {
                if let Some(loan) = self.loans.get_mut(&origin.loan) {
                    loan.holders.remove(&binding);
                    released.push(origin.loan);
                }
            }
            for loan in released {
                self.deactivate_loan_if_unheld(loan);
            }
        }
        for origin in origins {
            if let Some(loan) = self.loans.get_mut(&origin.loan) {
                loan.holders.insert(binding);
                // A loan assigned inside a nested block to a binding declared
                // in an outer scope must outlive that block: clamp to the
                // binding's declaration depth, not the assignment site.
                let declared = self
                    .binding_scopes
                    .get(&binding)
                    .copied()
                    .unwrap_or(self.scope_depth);
                loan.scope_depth = loan.scope_depth.min(declared);
                loan.active = true;
            }
        }
    }

    fn expr_origin_value(&self, expr: ExprId) -> OriginValue {
        OriginValue {
            origins: self.expr_origins.get(&expr).cloned().unwrap_or_default(),
            fields: self
                .expr_origin_fields
                .get(&expr)
                .cloned()
                .unwrap_or_default(),
        }
    }

    fn local_origin_value(&self, binding: PatternBindingId) -> OriginValue {
        OriginValue {
            origins: self
                .local_origins
                .get(&binding)
                .cloned()
                .unwrap_or_default(),
            fields: self
                .local_origin_fields
                .get(&binding)
                .cloned()
                .unwrap_or_default(),
        }
    }

    fn set_expr_origin_value(&mut self, expr: ExprId, value: OriginValue) {
        self.expr_origins.insert(expr, value.origins);
        if value.fields.is_empty() {
            self.expr_origin_fields.remove(&expr);
        } else {
            self.expr_origin_fields.insert(expr, value.fields);
        }
    }

    fn bind_origin_value(&mut self, binding: PatternBindingId, value: OriginValue) {
        self.bind_origins(binding, value.origins);
        if value.fields.is_empty() {
            self.local_origin_fields.remove(&binding);
        } else {
            self.local_origin_fields.insert(binding, value.fields);
        }
    }

    fn release_local_if_dead(&mut self, binding: PatternBindingId, expr: ExprId) {
        if let Some(range) = self.expr_range(expr)
            && self
                .last_uses
                .get(&binding)
                .is_some_and(|last| *last <= usize::from(range.end()))
        {
            self.release_local_origins(binding);
        }
    }

    fn release_expired_locals(&mut self, expr: ExprId) {
        let Some(range) = self.expr_range(expr) else {
            return;
        };
        let expired = self
            .local_origins
            .keys()
            .copied()
            .filter(|binding| {
                self.last_uses
                    .get(binding)
                    .is_some_and(|last| *last <= usize::from(range.end()))
            })
            .collect::<Vec<_>>();
        for binding in expired {
            self.release_local_origins(binding);
        }
    }

    fn release_local_origins(&mut self, binding: PatternBindingId) {
        let Some(origins) = self.local_origins.get(&binding) else {
            return;
        };
        let origins = origins.iter().map(|origin| origin.loan).collect::<Vec<_>>();
        for loan in &origins {
            if let Some(record) = self.loans.get_mut(loan) {
                record.holders.remove(&binding);
            }
        }
        for loan in origins {
            self.deactivate_loan_if_unheld(loan);
        }
    }

    fn deactivate_loan_if_unheld(&mut self, loan: LoanId) {
        let can_deactivate = self.loans.get(&loan).is_some_and(|record| {
            record.active
                && !record.permanent
                && record.holders.is_empty()
                && !self.loans.iter().any(|(child, child_record)| {
                    *child != loan && child_record.active && child_record.parents.contains(&loan)
                })
        });
        if !can_deactivate {
            return;
        }
        let parents = self
            .loans
            .get(&loan)
            .map(|record| record.parents.iter().copied().collect::<Vec<_>>())
            .unwrap_or_default();
        if let Some(record) = self.loans.get_mut(&loan) {
            record.active = false;
        }
        for parent in parents {
            self.deactivate_loan_if_unheld(parent);
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct MoveBindings {
    scopes: Vec<HashMap<String, bool>>,
}

impl MoveBindings {
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }
    fn insert_available(&mut self, name: String) {
        if self.scopes.is_empty() {
            self.push_scope();
        }
        self.scopes.last_mut().unwrap().insert(name, false);
    }
    fn get(&self, name: &str) -> Option<&bool> {
        self.scopes.iter().rev().find_map(|s| s.get(name))
    }
    fn contains(&self, name: &str) -> bool {
        self.scopes.iter().rev().any(|s| s.contains_key(name))
    }
    fn mark_moved(&mut self, name: &str) {
        for s in self.scopes.iter_mut().rev() {
            if let Some(m) = s.get_mut(name) {
                *m = true;
                return;
            }
        }
    }

    fn mark_available(&mut self, name: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(moved) = scope.get_mut(name) {
                *moved = false;
                return;
            }
        }
    }

    fn merge_moved_from(&mut self, other: &Self) {
        for (scope, other_scope) in self.scopes.iter_mut().zip(&other.scopes) {
            for (name, moved) in other_scope {
                if *moved && let Some(current) = scope.get_mut(name) {
                    *current = true;
                }
            }
        }
    }
}

fn place_overlaps(a: &Place, b: &Place) -> bool {
    a.is_prefix_of(b) || b.is_prefix_of(a)
}

fn contains_opaque_owned_result(ty: &Type) -> bool {
    match ty {
        Type::Param(_) => true,
        Type::Enum(_, args) | Type::Struct(_, args) | Type::Tuple(args) => {
            args.iter().any(contains_opaque_owned_result)
        }
        Type::Array(inner, _) => contains_opaque_owned_result(inner),
        _ => false,
    }
}

fn discard_reset_origins(value: &mut OriginValue, reset: &HashSet<LoanId>) {
    value.origins.retain(|origin| !reset.contains(&origin.loan));
    for field in &mut value.fields {
        discard_reset_origins(field, reset);
    }
}

/// True when the place contains a runtime (wildcard) array index projection.
fn place_has_wildcard_index(place: &Place) -> bool {
    place
        .projections
        .iter()
        .any(|projection| matches!(projection, hir::place::Projection::Index(None)))
}

/// The place expression behind an explicit `&`/`&mut`, when there is one.
fn peel_reference(ctx: &BodyCtx<'_>, expr: ExprId) -> ExprId {
    match &ctx.body.exprs[expr] {
        Expr::Unary {
            operand,
            op: UnaryOp::Ref | UnaryOp::MutRef,
        } => *operand,
        _ => expr,
    }
}

fn access_places_overlap(a: &AccessPlace, b: &AccessPlace) -> bool {
    if a.root != b.root {
        return false;
    }
    for (left, right) in a.projections.iter().zip(&b.projections) {
        match (left, right) {
            (AccessProjection::Field(left), AccessProjection::Field(right)) if left != right => {
                return false;
            }
            (AccessProjection::Index(Some(left)), AccessProjection::Index(Some(right)))
                if left != right =>
            {
                return false;
            }
            (AccessProjection::Field(_), AccessProjection::Index(_))
            | (AccessProjection::Index(_), AccessProjection::Field(_)) => return false,
            _ => {}
        }
    }
    true
}

/// Whether `outer` names the same place as `inner` or a prefix of it, so
/// `inner` is storage inside `outer`.
fn access_place_encloses(outer: &AccessPlace, inner: &AccessPlace) -> bool {
    outer.root == inner.root
        && outer.projections.len() <= inner.projections.len()
        && outer
            .projections
            .iter()
            .zip(&inner.projections)
            .all(|(left, right)| left == right)
}

fn access_place_from_move_place(place: &Place) -> AccessPlace {
    let root = match place.root {
        hir::place::PlaceRoot::Pattern(id) => AccessRoot::Pattern(id),
        hir::place::PlaceRoot::Param(index) => AccessRoot::Param(index),
        hir::place::PlaceRoot::LambdaParam { lambda, index } => {
            AccessRoot::LambdaParam { lambda, index }
        }
    };
    let mut result = AccessPlace::new(root);
    for projection in &place.projections {
        result = match projection {
            hir::place::Projection::Field(index) => result.field(*index),
            hir::place::Projection::Index(index) => result.index(*index),
        };
    }
    result
}

const fn access_place_from_resolved_name(name: &ResolvedName) -> Option<AccessPlace> {
    let root = match name {
        ResolvedName::PatternBinding(id) => AccessRoot::Pattern(*id),
        ResolvedName::Param(index) => AccessRoot::Param(*index),
        ResolvedName::LambdaParam { lambda, index } => AccessRoot::LambdaParam {
            lambda: *lambda,
            index: *index,
        },
        _ => return None,
    };
    Some(AccessPlace::new(root))
}

fn access_place_from_capture(place: &CapturePlace) -> AccessPlace {
    let root = match &place.source {
        CaptureSource::Pattern(id) => AccessRoot::Pattern(*id),
        CaptureSource::Param(index) => AccessRoot::Param(*index),
        CaptureSource::LambdaParam { lambda, index } => AccessRoot::LambdaParam {
            lambda: *lambda,
            index: *index,
        },
    };
    let mut result = AccessPlace::new(root);
    for projection in &place.projections {
        result = match projection {
            hir::place::Projection::Field(index) => result.field(*index),
            hir::place::Projection::Index(index) => result.index(*index),
        };
    }
    result
}

fn move_place_from_capture(capture: &CapturePlace) -> Option<Place> {
    let mut place = match &capture.source {
        CaptureSource::Pattern(id) => Place::root(*id),
        CaptureSource::Param(index) => Place::param(*index),
        CaptureSource::LambdaParam { lambda, index } => Place::lambda_param(*lambda, *index),
    };
    for projection in &capture.projections {
        place = match projection {
            hir::place::Projection::Field(index) => place.field(*index),
            hir::place::Projection::Index(index) => place.index(*index),
        };
    }
    Some(place)
}

const fn hir_ref_kind(ty: &HirTypeRef) -> Option<BorrowKind> {
    match ty {
        HirTypeRef::Ref(_, true) => Some(BorrowKind::Mutable),
        HirTypeRef::Ref(_, false) => Some(BorrowKind::Shared),
        _ => None,
    }
}

const fn type_ref_kind(ty: &Type) -> Option<BorrowKind> {
    match ty {
        Type::Ref(_, true) => Some(BorrowKind::Mutable),
        Type::Ref(_, false) => Some(BorrowKind::Shared),
        _ => None,
    }
}

fn callable_parameter_modes(ty: &Type) -> Option<Vec<Option<BorrowKind>>> {
    match ty {
        Type::Ref(inner, _) => callable_parameter_modes(inner),
        Type::CallableConstraint(signature)
        | Type::Closure { signature, .. }
        | Type::OpaqueCallable { signature, .. } => {
            Some(signature.params.iter().map(type_ref_kind).collect())
        }
        _ => None,
    }
}

/// `&mut` expressions written directly as a call's receiver or argument.
///
/// A `&mut` of a `mut` field reached through a shared reference is only sound
/// for the call it is written in — the callee borrows the field for the
/// duration of the call and cannot keep it. Every other position (a binding, a
/// return value, a struct field) would let the borrow outlive the statement.
fn collect_call_borrow_exprs(body: &Body) -> HashSet<ExprId> {
    let mut exprs = HashSet::new();
    for (_, expr) in body.exprs.iter() {
        let Expr::Call { callee, args, .. } = expr else {
            continue;
        };
        for candidate in args.iter().copied().chain(std::iter::once(*callee)) {
            // A method call's receiver is the base of the field access.
            let receiver = match &body.exprs[candidate] {
                Expr::FieldAccess { base, .. } => *base,
                _ => candidate,
            };
            if matches!(
                body.exprs[receiver],
                Expr::Unary {
                    op: UnaryOp::MutRef,
                    ..
                }
            ) {
                exprs.insert(receiver);
            }
        }
    }
    exprs
}

fn collect_local_uses(body: &Body) -> HashMap<PatternBindingId, usize> {
    let mut uses = HashMap::new();
    collect_expr_local_uses(body, body.root_block, &mut uses);
    // A binding with no recorded use never expires, so loans it holds would
    // outlive the scope forever. Seed those with their own binding site:
    // they expire as soon as checking moves past the pattern, like a
    // binding whose last use is at its initializer.
    for (pat, pattern) in body.pats.iter() {
        let bindings: Vec<PatternBindingId> = match pattern {
            Pattern::Binding { .. } => vec![PatternBindingId {
                pattern: pat,
                field: None,
            }],
            Pattern::Struct { fields, .. } => fields
                .iter()
                .enumerate()
                .filter(|(_, field)| field.pat.is_none())
                .map(|(index, _)| PatternBindingId {
                    pattern: pat,
                    field: Some(index),
                })
                .collect(),
            _ => continue,
        };
        let Some(end) = body
            .source_map
            .pat_ranges
            .get(&pat)
            .map(|range| usize::from(range.end()))
        else {
            continue;
        };
        for binding in bindings {
            uses.entry(binding).or_insert(end);
        }
    }
    uses
}

fn collect_expr_local_uses(body: &Body, id: ExprId, uses: &mut HashMap<PatternBindingId, usize>) {
    match &body.exprs[id] {
        Expr::Path {
            resolved: Some(ResolvedName::PatternBinding(binding)),
            ..
        } => {
            if let Some(range) = body.source_map.expr_ranges.get(&id) {
                let last = uses.entry(*binding).or_default();
                *last = (*last).max(usize::from(range.end()));
            }
        }
        Expr::Binary { lhs, rhs, .. } => {
            collect_expr_local_uses(body, *lhs, uses);
            collect_expr_local_uses(body, *rhs, uses);
        }
        Expr::Unary { operand, .. }
        | Expr::FieldAccess { base: operand, .. }
        | Expr::Unsafe { body: operand }
        | Expr::Cast { base: operand, .. }
        | Expr::Try { operand } => collect_expr_local_uses(body, *operand, uses),
        Expr::Block { stmts, tail } => collect_block_local_uses(body, stmts, *tail, uses),
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_expr_local_uses(body, *cond, uses);
            collect_expr_local_uses(body, *then_branch, uses);
            if let Some(branch) = else_branch {
                collect_expr_local_uses(body, *branch, uses);
            }
        }
        Expr::While {
            condition,
            body: loop_body,
        } => {
            collect_expr_local_uses(body, *condition, uses);
            collect_expr_local_uses(body, *loop_body, uses);
        }
        Expr::Loop { body: loop_body } => {
            collect_expr_local_uses(body, *loop_body, uses);
        }
        Expr::For {
            iterable,
            body: loop_body,
            ..
        } => {
            collect_expr_local_uses(body, *iterable, uses);
            collect_expr_local_uses(body, *loop_body, uses);
        }
        Expr::Match { scrutinee, arms } => {
            collect_expr_local_uses(body, *scrutinee, uses);
            for arm in arms {
                if let Some(guard) = arm.guard {
                    collect_expr_local_uses(body, guard, uses);
                }
                collect_expr_local_uses(body, arm.body, uses);
            }
        }
        Expr::Array { elements } | Expr::Tuple { elements } => {
            for element in elements {
                collect_expr_local_uses(body, *element, uses);
            }
        }
        Expr::ArrayRepeat { value, len } => {
            collect_expr_local_uses(body, *value, uses);
            collect_expr_local_uses(body, *len, uses);
        }
        Expr::Struct { fields, .. } => {
            for field in fields {
                collect_expr_local_uses(body, field.value, uses);
            }
        }
        Expr::Call { callee, args, .. } => {
            collect_expr_local_uses(body, *callee, uses);
            for arg in args {
                collect_expr_local_uses(body, *arg, uses);
            }
        }
        Expr::Lambda {
            body: lambda_body, ..
        } => collect_expr_local_uses(body, *lambda_body, uses),
        Expr::IndexAccess { base, index } => {
            collect_expr_local_uses(body, *base, uses);
            collect_expr_local_uses(body, *index, uses);
        }
        Expr::Missing
        | Expr::IntLiteral { .. }
        | Expr::FloatLiteral { .. }
        | Expr::StringLiteral { .. }
        | Expr::CharLiteral { .. }
        | Expr::BoolLiteral { .. }
        | Expr::Path { .. } => {}
    }
    if matches!(
        body.exprs[id],
        Expr::While { .. } | Expr::Loop { .. } | Expr::For { .. } | Expr::Lambda { .. }
    ) && let Some(range) = body.source_map.expr_ranges.get(&id)
    {
        for (binding, last) in uses.iter_mut() {
            if *last >= usize::from(range.start())
                && *last <= usize::from(range.end())
                && body
                    .source_map
                    .pat_ranges
                    .get(&binding.pattern)
                    .is_some_and(|declaration| declaration.start() < range.start())
            {
                *last = usize::from(range.end());
            }
        }
    }
}

fn collect_block_local_uses(
    body: &Body,
    stmts: &[StmtId],
    tail: Option<ExprId>,
    uses: &mut HashMap<PatternBindingId, usize>,
) {
    for stmt_id in stmts {
        match &body.stmts[*stmt_id] {
            Stmt::Let { init, else_, .. } => {
                if let Some(init) = init {
                    collect_expr_local_uses(body, *init, uses);
                }
                if let Some(else_) = else_ {
                    collect_expr_local_uses(body, *else_, uses);
                }
            }
            Stmt::Expr { expr } => collect_expr_local_uses(body, *expr, uses),
            Stmt::Return { value } => {
                if let Some(value) = value {
                    collect_expr_local_uses(body, *value, uses);
                }
            }
            Stmt::Break { value } => {
                if let Some(value) = value {
                    collect_expr_local_uses(body, *value, uses);
                }
            }
            Stmt::Continue | Stmt::Item { .. } => {}
        }
    }
    if let Some(tail) = tail {
        collect_expr_local_uses(body, tail, uses);
    }
}
