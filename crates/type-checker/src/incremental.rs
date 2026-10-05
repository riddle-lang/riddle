use std::{
    collections::{HashMap, HashSet, hash_map::DefaultHasher},
    hash::{Hash, Hasher},
};

use hir::{
    HirFile,
    body::{Body, BodyId, ExprId, PatId, PatternBindingId},
    item_tree::{
        FunctionId, HirAttr, HirConst, HirEnum, HirEnumVariant, HirFunction, HirGenericBound,
        HirImpl, HirModule, HirStruct, HirStructField, HirTrait, HirTypeAlias, HirUse, HirUseTree,
        HirUseTreeKind, HirVariantKind, ItemTree,
    },
};
use rowan::{TextRange, TextSize};
use syntax::SyntaxNode;

use crate::{
    TypeCheckResult,
    checker::{GenericEdge, TypeChecker},
    result::{
        Diagnostic, ForLoopInfo, GenericCall, LambdaInfo, OperatorCall, PatternBindingMode,
        TraitMethodCall, ValueUse,
    },
    types::{CallableSignature, OpaqueCallableId, Type},
};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct IncrementalStats {
    pub checked_bodies: usize,
    pub reused_bodies: usize,
    /// Bodies whose cached entry was not even considered: no entry at all.
    pub missed_absent: usize,
    /// Bodies whose cached entry carried a different type-context fingerprint.
    pub missed_context: usize,
    /// Bodies whose type context matched but whose own fingerprint did not.
    pub missed_body: usize,
    /// Bodies that matched on both fingerprints but could not be replayed.
    pub missed_replay: usize,
    /// Whether the type context itself was recomputed rather than reused.
    pub rebuilt_globals: bool,
}

#[derive(Debug, Clone)]
pub struct IncrementalTypeCheckResult {
    pub result: TypeCheckResult,
    pub stats: IncrementalStats,
}

#[derive(Debug, Default)]
pub struct IncrementalTypeChecker {
    bodies: HashMap<FunctionId, CachedBody>,
    globals: Option<CachedGlobals>,
    last_source: Option<String>,
    /// The type-context fingerprint of the last check, so a trace can show
    /// whether it moved and by how much.
    last_context_hash: Option<u64>,
    /// Reuse counters from the most recent check, so a caller can assert that
    /// an edit re-checked only what it had to.
    last_stats: IncrementalStats,
}

impl IncrementalTypeChecker {
    /// How many bodies this checker currently holds.
    ///
    /// A checker that keeps its entries serves the next request from them; one
    /// that lost them re-type-checks the whole standard library. The difference
    /// is ~700 ms and nothing else in the system reports which happened.
    #[must_use]
    pub fn known_bodies(&self) -> usize {
        self.bodies.len()
    }

    /// Reuse counters from the most recent check.
    ///
    /// The interesting figure is `checked_bodies` on a warm checker: an edit to
    /// one function may re-check that one body and nothing else, or all ~827
    /// standard-library bodies, and only these counters say which.
    #[must_use]
    pub const fn last_stats(&self) -> IncrementalStats {
        self.last_stats
    }
}

#[derive(Debug, Clone)]
struct CachedGlobals {
    type_context_hash: u64,
    value_diagnostics: Vec<Diagnostic>,
    impl_diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, Copy)]
struct TextEdit {
    old_range: TextRange,
    insert_len: TextSize,
}

#[derive(Debug, Clone)]
struct CachedBody {
    type_context_hash: u64,
    body_hash: u64,
    diagnostics: Vec<Diagnostic>,
    expr_types: Vec<(ExprId, Type)>,
    expr_coercions: Vec<(ExprId, Type)>,
    generic_calls: Vec<(ExprId, GenericCall)>,
    trait_method_calls: Vec<(ExprId, TraitMethodCall)>,
    callable_impl_calls: Vec<(ExprId, FunctionId)>,
    operator_calls: Vec<(ExprId, OperatorCall)>,
    for_loops: Vec<(ExprId, ForLoopInfo)>,
    lambda_infos: Vec<(ExprId, LambdaInfo)>,
    pattern_types: Vec<(PatId, Type)>,
    pattern_binding_types: Vec<(PatternBindingId, Type)>,
    pattern_binding_modes: Vec<(PatternBindingId, PatternBindingMode)>,
    value_uses: Vec<(ExprId, ValueUse)>,
    opaque_hidden_types: Vec<(OpaqueCallableId, Type)>,
    method_signatures: Vec<(ExprId, CallableSignature)>,
    generic_edges: Vec<GenericEdge>,
}

pub fn check_hir_incremental(
    hir: &HirFile,
    checker: &mut IncrementalTypeChecker,
) -> IncrementalTypeCheckResult {
    checker.check(hir)
}

impl IncrementalTypeChecker {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn check(&mut self, hir: &HirFile) -> IncrementalTypeCheckResult {
        self.globals = None;
        self.last_source = None;
        self.check_inner(
            hir,
            body_fingerprint,
            None,
            type_context_fingerprint_with_ranges(&hir.item_tree, hir.pointer_width_bits),
        )
    }

    pub fn check_with_syntax(
        &mut self,
        hir: &HirFile,
        syntax: &SyntaxNode,
    ) -> IncrementalTypeCheckResult {
        let source = syntax.text().to_string();
        let edit = self
            .last_source
            .as_deref()
            .map(|old_source| text_edit(old_source, &source));
        if std::env::var_os("RIDDLEC_PHASE_TIMING").is_some() {
            eprintln!(
                "[phase]    check_with_syntax: known bodies {} previous context {:#x} edit {:?}",
                self.bodies.len(),
                self.last_context_hash.unwrap_or(0),
                edit,
            );
        }
        let context_hash = type_context_fingerprint_with_packages(hir);
        let result = self.check_inner(
            hir,
            |body| syntax_body_fingerprint(body, syntax),
            edit,
            context_hash,
        );
        self.last_source = Some(source);
        self.last_context_hash = Some(context_hash);
        result
    }

    fn check_inner(
        &mut self,
        hir: &HirFile,
        body_fingerprint: impl Fn(&Body) -> u64,
        edit: Option<TextEdit>,
        type_context_hash: u64,
    ) -> IncrementalTypeCheckResult {
        let mut checker = TypeChecker::new(hir);
        let mut stats = IncrementalStats::default();
        let mut live_functions = HashSet::new();

        let cached_globals = self
            .globals
            .as_ref()
            .filter(|cached| cached.type_context_hash == type_context_hash)
            .and_then(|cached| shift_cached_globals(cached, edit));
        let value_diagnostics = if let Some(cached) = &cached_globals {
            checker
                .result
                .diagnostics
                .extend(cached.value_diagnostics.iter().cloned());
            cached.value_diagnostics.clone()
        } else {
            let start = checker.result.diagnostics.len();
            checker.check_value_type_declarations();
            checker.result.diagnostics[start..].to_vec()
        };
        checker.check_type_layouts();
        checker.check_traits();
        let impl_diagnostics = if let Some(cached) = &cached_globals {
            checker
                .result
                .diagnostics
                .extend(cached.impl_diagnostics.iter().cloned());
            cached.impl_diagnostics.clone()
        } else {
            let start = checker.result.diagnostics.len();
            checker.check_impls();
            checker.result.diagnostics[start..].to_vec()
        };
        checker.build_trait_env();
        checker.validate_copy_impls();
        stats.rebuilt_globals = cached_globals.is_none();
        self.globals = Some(CachedGlobals {
            type_context_hash,
            value_diagnostics,
            impl_diagnostics,
        });

        // Constants are module-level inputs to every body and are not cached
        // as function bodies. Recheck them on each incremental pass so an
        // initializer diagnostic cannot disappear behind a reused function.
        checker.check_const_bodies();

        if std::env::var_os("RIDDLEC_PHASE_TIMING").is_some() {
            let cached_context = self
                .bodies
                .values()
                .next()
                .map(|cached| cached.type_context_hash);
            eprintln!(
                "[phase]    before body loop: checker {:#x}, cache {} entries, cached context {:?}, current {type_context_hash:#x}",
                std::ptr::from_ref(self) as usize,
                self.bodies.len(),
                cached_context.map(|hash| format!("{hash:#x}")),
            );
        }

        for (fid, function) in hir.item_tree.functions.iter() {
            let Some(body_id) = hir.function_bodies.get(&fid).copied() else {
                continue;
            };

            let body = &hir.bodies[body_id];
            let body_hash = body_fingerprint(body);
            live_functions.insert(fid);

            if let Some(cached) = self.bodies.get(&fid).filter(|cached| {
                cached.type_context_hash == type_context_hash && cached.body_hash == body_hash
            }) && replay_cached_body(&mut checker, body_id, cached, edit)
            {
                stats.reused_bodies += 1;
                continue;
            }

            // Record why the entry above did not apply, so a caller can tell a
            // genuinely changed body from a cache that stopped matching.
            match self.bodies.get(&fid) {
                None => {
                    if std::env::var_os("RIDDLEC_PHASE_TIMING").is_some() && stats.missed_absent < 3
                    {
                        let mut keys: Vec<String> = self
                            .bodies
                            .keys()
                            .take(3)
                            .map(|key| format!("{key:?}"))
                            .collect();
                        keys.sort();
                        eprintln!(
                            "[phase]    absent fid {fid:?} (name {:?}); cache holds {} entries, \
                             first keys {keys:?}",
                            function.name.0,
                            self.bodies.len(),
                        );
                    }
                    stats.missed_absent += 1;
                }
                Some(cached) if cached.type_context_hash != type_context_hash => {
                    stats.missed_context += 1;
                }
                Some(cached) if cached.body_hash != body_hash => stats.missed_body += 1,
                Some(_) => stats.missed_replay += 1,
            }
            stats.checked_bodies += 1;
            let diagnostic_start = checker.result.diagnostics.len();
            let generic_edge_start = checker.generic_edges.len();
            let opaque_hidden_before = checker
                .result
                .opaque_hidden_types
                .keys()
                .copied()
                .collect::<HashSet<_>>();
            checker.check_function(
                fid,
                function,
                body_id,
                &checker.impl_generic_names(fid),
                &checker.impl_const_generic_names(fid),
            );
            self.cache_checked_body(
                &checker,
                fid,
                body_id,
                (
                    type_context_hash,
                    body_hash,
                    diagnostic_start,
                    generic_edge_start,
                    &opaque_hidden_before,
                ),
            );
        }

        self.bodies
            .retain(|function, _| live_functions.contains(function));
        checker.check_generic_recursion();
        self.last_stats = stats;

        IncrementalTypeCheckResult {
            result: checker.result,
            stats,
        }
    }

    fn cache_checked_body(
        &mut self,
        checker: &TypeChecker<'_>,
        fid: FunctionId,
        body_id: BodyId,
        metadata: (u64, u64, usize, usize, &HashSet<OpaqueCallableId>),
    ) {
        let (
            type_context_hash,
            body_hash,
            diagnostic_start,
            generic_edge_start,
            opaque_hidden_before,
        ) = metadata;
        let diagnostics = checker.result.diagnostics[diagnostic_start..].to_vec();
        let generic_edges = checker.generic_edges[generic_edge_start..].to_vec();
        let expr_types = entries_for_body(&checker.result.expr_types, body_id);
        let expr_coercions = entries_for_body(&checker.result.expr_coercions, body_id);
        let generic_calls = entries_for_body(&checker.result.generic_calls, body_id);
        let trait_method_calls = entries_for_body(&checker.result.trait_method_calls, body_id);
        let callable_impl_calls = entries_for_body(&checker.result.callable_impl_calls, body_id);
        let operator_calls = entries_for_body(&checker.result.operator_calls, body_id);
        let for_loops = entries_for_body(&checker.result.for_loops, body_id);
        let lambda_infos = entries_for_body(&checker.result.lambda_infos, body_id);
        let pattern_types = entries_for_body(&checker.result.pattern_types, body_id);
        let pattern_binding_types =
            entries_for_body(&checker.result.pattern_binding_types, body_id);
        let pattern_binding_modes =
            entries_for_body(&checker.result.pattern_binding_modes, body_id);
        let value_uses = entries_for_body(&checker.result.value_uses, body_id);
        let method_signatures = entries_for_body(&checker.result.method_signatures, body_id);
        let opaque_hidden_types = checker
            .result
            .opaque_hidden_types
            .iter()
            .filter(|(id, _)| !opaque_hidden_before.contains(id))
            .map(|(id, ty)| (*id, ty.clone()))
            .collect();
        self.bodies.insert(
            fid,
            CachedBody {
                type_context_hash,
                body_hash,
                diagnostics,
                expr_types,
                expr_coercions,
                generic_calls,
                trait_method_calls,
                callable_impl_calls,
                operator_calls,
                for_loops,
                lambda_infos,
                pattern_types,
                pattern_binding_types,
                pattern_binding_modes,
                value_uses,
                opaque_hidden_types,
                method_signatures,
                generic_edges,
            },
        );
    }
}

fn entries_for_body<K, V>(entries: &HashMap<(BodyId, K), V>, body_id: BodyId) -> Vec<(K, V)>
where
    K: Copy,
    V: Clone,
{
    entries
        .iter()
        .filter(|((checked_body, _), _)| *checked_body == body_id)
        .map(|((_, key), value)| (*key, value.clone()))
        .collect()
}

fn replay_cached_body(
    checker: &mut TypeChecker<'_>,
    body_id: BodyId,
    cached: &CachedBody,
    edit: Option<TextEdit>,
) -> bool {
    let diagnostics = match edit {
        Some(edit) => match shift_diagnostics(&cached.diagnostics, edit) {
            Some(diagnostics) => diagnostics,
            None => return false,
        },
        None => cached.diagnostics.clone(),
    };
    let generic_edges = match edit {
        Some(edit) => match shift_generic_edges(&cached.generic_edges, edit) {
            Some(edges) => edges,
            None => return false,
        },
        None => cached.generic_edges.clone(),
    };
    checker.result.diagnostics.extend(diagnostics);
    for (expr, ty) in &cached.expr_types {
        checker
            .result
            .expr_types
            .insert((body_id, *expr), ty.clone());
    }
    for (expr, ty) in &cached.expr_coercions {
        checker
            .result
            .expr_coercions
            .insert((body_id, *expr), ty.clone());
    }
    for (expr, call) in &cached.generic_calls {
        checker
            .result
            .generic_calls
            .insert((body_id, *expr), call.clone());
    }
    for (expr, call) in &cached.trait_method_calls {
        checker
            .result
            .trait_method_calls
            .insert((body_id, *expr), call.clone());
    }
    for (expr, function) in &cached.callable_impl_calls {
        checker
            .result
            .callable_impl_calls
            .insert((body_id, *expr), *function);
    }
    for (expr, call) in &cached.operator_calls {
        checker
            .result
            .operator_calls
            .insert((body_id, *expr), call.clone());
    }
    for (expr, info) in &cached.for_loops {
        checker
            .result
            .for_loops
            .insert((body_id, *expr), info.clone());
    }
    for (expr, info) in &cached.lambda_infos {
        checker
            .result
            .lambda_infos
            .insert((body_id, *expr), info.clone());
    }
    for (pattern, ty) in &cached.pattern_types {
        checker
            .result
            .pattern_types
            .insert((body_id, *pattern), ty.clone());
    }
    for (binding, ty) in &cached.pattern_binding_types {
        checker
            .result
            .pattern_binding_types
            .insert((body_id, *binding), ty.clone());
    }
    for (binding, mode) in &cached.pattern_binding_modes {
        checker
            .result
            .pattern_binding_modes
            .insert((body_id, *binding), *mode);
    }
    for (expr, use_kind) in &cached.value_uses {
        checker
            .result
            .value_uses
            .insert((body_id, *expr), *use_kind);
    }
    for (id, ty) in &cached.opaque_hidden_types {
        checker.result.opaque_hidden_types.insert(*id, ty.clone());
    }
    for (expr, signature) in &cached.method_signatures {
        checker
            .result
            .method_signatures
            .insert((body_id, *expr), signature.clone());
    }
    checker.generic_edges.extend(generic_edges);
    true
}

fn shift_range(range: TextRange, offset: i64) -> TextRange {
    let shift = |position: TextSize| {
        let shifted = i64::from(u32::from(position)) + offset;
        TextSize::from(u32::try_from(shifted).expect("shifted text position should fit in u32"))
    };
    TextRange::new(shift(range.start()), shift(range.end()))
}

fn shift_cached_globals(cached: &CachedGlobals, edit: Option<TextEdit>) -> Option<CachedGlobals> {
    let Some(edit) = edit else {
        return Some(cached.clone());
    };
    Some(CachedGlobals {
        type_context_hash: cached.type_context_hash,
        value_diagnostics: shift_diagnostics(&cached.value_diagnostics, edit)?,
        impl_diagnostics: shift_diagnostics(&cached.impl_diagnostics, edit)?,
    })
}

fn shift_diagnostics(diagnostics: &[Diagnostic], edit: TextEdit) -> Option<Vec<Diagnostic>> {
    diagnostics
        .iter()
        .cloned()
        .map(|mut diagnostic| {
            for label in &mut diagnostic.labels {
                label.range = shift_range_for_edit(label.range, edit)?;
            }
            Some(diagnostic)
        })
        .collect()
}

fn shift_generic_edges(edges: &[GenericEdge], edit: TextEdit) -> Option<Vec<GenericEdge>> {
    edges
        .iter()
        .cloned()
        .map(|mut edge| {
            edge.span = match edge.span {
                Some(range) => Some(shift_range_for_edit(range, edit)?),
                None => None,
            };
            Some(edge)
        })
        .collect()
}

fn shift_range_for_edit(range: TextRange, edit: TextEdit) -> Option<TextRange> {
    if range.end() <= edit.old_range.start() {
        return Some(range);
    }
    if range.start() < edit.old_range.end() {
        return None;
    }
    let offset = i64::from(u32::from(edit.insert_len)) - i64::from(u32::from(edit.old_range.len()));
    Some(shift_range(range, offset))
}

fn text_edit(old: &str, new: &str) -> TextEdit {
    let mut prefix = 0;
    for (old_char, new_char) in old.chars().zip(new.chars()) {
        if old_char != new_char {
            break;
        }
        prefix += old_char.len_utf8();
    }

    let mut suffix = 0;
    for (old_char, new_char) in old[prefix..].chars().rev().zip(new[prefix..].chars().rev()) {
        if old_char != new_char {
            break;
        }
        suffix += old_char.len_utf8();
    }

    TextEdit {
        old_range: TextRange::new(
            TextSize::from(u32::try_from(prefix).expect("edit offset should fit in u32")),
            TextSize::from(
                u32::try_from(old.len() - suffix).expect("edit offset should fit in u32"),
            ),
        ),
        insert_len: TextSize::from(
            u32::try_from(new.len() - prefix - suffix)
                .expect("inserted text length should fit in u32"),
        ),
    }
}

fn body_fingerprint(body: &Body) -> u64 {
    let mut hasher = DefaultHasher::new();
    format!("{:?}", body.exprs).hash(&mut hasher);
    format!("{:?}", body.stmts).hash(&mut hasher);
    format!("{:?}", body.pats).hash(&mut hasher);
    body.root_ptr.text_range().hash(&mut hasher);
    for (expr, _) in body.exprs.iter() {
        body.source_map.expr_ranges.get(&expr).hash(&mut hasher);
    }
    for (stmt, _) in body.stmts.iter() {
        body.source_map.stmt_ranges.get(&stmt).hash(&mut hasher);
    }
    for (pat, _) in body.pats.iter() {
        body.source_map.pat_ranges.get(&pat).hash(&mut hasher);
    }
    hasher.finish()
}

fn syntax_body_fingerprint(body: &Body, syntax: &SyntaxNode) -> u64 {
    let mut hasher = DefaultHasher::new();
    body.root_ptr
        .to_node(syntax)
        .green()
        .into_owned()
        .hash(&mut hasher);
    hasher.finish()
}

fn type_context_fingerprint(tree: &ItemTree) -> u64 {
    let mut hasher = DefaultHasher::new();
    tree.functions.len().hash(&mut hasher);
    for (_, function) in tree.functions.iter() {
        hash_function(function, &mut hasher);
    }
    tree.structs.len().hash(&mut hasher);
    for (_, strukt) in tree.structs.iter() {
        hash_struct(strukt, &mut hasher);
    }
    tree.modules.len().hash(&mut hasher);
    for (_, module) in tree.modules.iter() {
        hash_module(module, &mut hasher);
    }
    tree.uses.len().hash(&mut hasher);
    for (_, use_item) in tree.uses.iter() {
        hash_use(use_item, &mut hasher);
    }
    tree.enums.len().hash(&mut hasher);
    for (_, enumeration) in tree.enums.iter() {
        hash_enum(enumeration, &mut hasher);
    }
    tree.traits.len().hash(&mut hasher);
    for (_, trait_item) in tree.traits.iter() {
        hash_trait(trait_item, &mut hasher);
    }
    tree.impls.len().hash(&mut hasher);
    for (_, impl_item) in tree.impls.iter() {
        hash_impl(impl_item, &mut hasher);
    }
    tree.consts.len().hash(&mut hasher);
    for (_, const_item) in tree.consts.iter() {
        hash_const(const_item, &mut hasher);
    }
    tree.type_aliases.len().hash(&mut hasher);
    for (_, alias) in tree.type_aliases.iter() {
        hash_type_alias(alias, &mut hasher);
    }
    format!("{:?}", tree.top_level).hash(&mut hasher);
    format!("{:?}", tree.extern_function_ids).hash(&mut hasher);
    hasher.finish()
}

/// Fingerprint of everything that can change how a body type-checks.
///
/// Deliberately blind to absolute source positions. It used to be this hash
/// combined with `format!("{tree:?}")`, and since `Debug` carries every item's
/// `TextRange`, editing one character moved the items after it and changed the
/// fingerprint. Every cached body in the bundle was then discarded — including
/// the standard library's, which is most of them. `RIDDLEC_PHASE_TIMING` showed
/// a whole-project check spending ~700 ms re-type-checking 827 unchanged bodies
/// on each keystroke for that reason.
///
/// Declarations still participate through `hash_function` and friends, which
/// cover names, signatures, generic bounds, variants and fields. Bodies do not:
/// they are tracked individually by `body_fingerprint` and
/// `syntax_body_fingerprint`, which decide whether one body needs re-checking.
///
/// The pointer width participates because an integer range check was made
/// against it: a `usize` literal that fits a 64-bit target does not fit a
/// 32-bit one, and switching the target changes no declaration at all.
fn type_context_fingerprint_with_ranges(tree: &ItemTree, pointer_bits: u32) -> u64 {
    let mut hasher = DefaultHasher::new();
    type_context_fingerprint(tree).hash(&mut hasher);
    pointer_bits.hash(&mut hasher);
    hasher.finish()
}

/// The three components of the type-context fingerprint, for tracing.
///
/// Exists so a caller can tell *which* part of the context moved — declarations,
/// the item structure, or function signatures — without guessing. Two of the
/// three are already inside [`type_context_fingerprint`]; the middle one is not,
/// and is what reveals a shape change.
#[must_use]
pub fn context_fingerprint_parts(tree: &ItemTree) -> (u64, u64, u64) {
    let declarations = type_context_fingerprint(tree);
    let mut structure = DefaultHasher::new();
    format!("{:?}", tree.top_level).hash(&mut structure);
    format!("{:?}", tree.extern_function_ids).hash(&mut structure);
    let mut signatures = DefaultHasher::new();
    for (_, function) in tree.functions.iter() {
        hash_function(function, &mut signatures);
    }
    let parts = (declarations, structure.finish(), signatures.finish());
    if std::env::var_os("RIDDLEC_PHASE_TIMING").is_some() {
        eprintln!(
            "[phase]    context fingerprint: {} functions, decl {:#x} structure {:#x} \
             signatures {:#x}",
            tree.functions.len(),
            parts.0,
            parts.1,
            parts.2,
        );
    }
    parts
}

fn type_context_fingerprint_with_packages(hir: &HirFile) -> u64 {
    let mut hasher = DefaultHasher::new();
    type_context_fingerprint(&hir.item_tree).hash(&mut hasher);
    hir.pointer_width_bits.hash(&mut hasher);
    for (_, item) in hir.item_tree.structs.iter() {
        hir.package_for_range(item.name_range).hash(&mut hasher);
    }
    for (_, item) in hir.item_tree.enums.iter() {
        hir.package_for_range(item.name_range).hash(&mut hasher);
    }
    for (_, item) in hir.item_tree.traits.iter() {
        hir.package_for_range(item.name_range).hash(&mut hasher);
    }
    for (_, item) in hir.item_tree.impls.iter() {
        hir.package_for_range(item.self_ty_range).hash(&mut hasher);
    }
    for internal in &hir.internal_attrs {
        internal.attr.name.hash(&mut hasher);
        internal.attr.value.hash(&mut hasher);
        internal.target.hash(&mut hasher);
        hir.package_for_range(internal.attr.range).hash(&mut hasher);
    }
    hasher.finish()
}

fn hash_function(function: &HirFunction, hasher: &mut impl Hasher) {
    function.name.hash(hasher);
    function.visibility.is_public().hash(hasher);
    function.is_unsafe.hash(hasher);
    function.generics.hash(hasher);
    function.const_generics.hash(hasher);
    hash_bounds(&function.generic_bounds, hasher);
    function.params.len().hash(hasher);
    for param in &function.params {
        param.name.hash(hasher);
        param.ty.hash(hasher);
        hash_attrs(&param.attrs, hasher);
    }
    function.ret_type.hash(hasher);
    function.has_body.hash(hasher);
    hash_attrs(&function.attrs, hasher);
}

fn hash_struct(strukt: &HirStruct, hasher: &mut impl Hasher) {
    strukt.name.hash(hasher);
    strukt.visibility.is_public().hash(hasher);
    strukt.generics.hash(hasher);
    strukt.const_generics.hash(hasher);
    hash_bounds(&strukt.generic_bounds, hasher);
    hash_fields(&strukt.fields, hasher);
    hash_attrs(&strukt.attrs, hasher);
}

fn hash_fields(fields: &[HirStructField], hasher: &mut impl Hasher) {
    fields.len().hash(hasher);
    for field in fields {
        field.name.hash(hasher);
        field.visibility.is_public().hash(hasher);
        field.ty.hash(hasher);
        hash_attrs(&field.attrs, hasher);
    }
}

fn hash_module(module: &HirModule, hasher: &mut impl Hasher) {
    module.name.hash(hasher);
    module.visibility.is_public().hash(hasher);
    format!("{:?}", module.items).hash(hasher);
    hash_attrs(&module.attrs, hasher);
}

fn hash_use(use_item: &HirUse, hasher: &mut impl Hasher) {
    hash_use_tree(&use_item.tree, hasher);
    use_item.visibility.is_public().hash(hasher);
    hash_attrs(&use_item.attrs, hasher);
}

fn hash_use_tree(tree: &HirUseTree, hasher: &mut impl Hasher) {
    tree.prefix.hash(hasher);
    match &tree.kind {
        HirUseTreeKind::Simple { alias } => {
            0u8.hash(hasher);
            alias.hash(hasher);
        }
        HirUseTreeKind::Glob => 1u8.hash(hasher),
        HirUseTreeKind::List(children) => {
            2u8.hash(hasher);
            children.len().hash(hasher);
            for child in children {
                hash_use_tree(child, hasher);
            }
        }
    }
}

fn hash_enum(enumeration: &HirEnum, hasher: &mut impl Hasher) {
    enumeration.name.hash(hasher);
    enumeration.visibility.is_public().hash(hasher);
    enumeration.generics.hash(hasher);
    enumeration.const_generics.hash(hasher);
    hash_bounds(&enumeration.generic_bounds, hasher);
    enumeration.variants.len().hash(hasher);
    for variant in &enumeration.variants {
        hash_variant(variant, hasher);
    }
    hash_attrs(&enumeration.attrs, hasher);
}

fn hash_variant(variant: &HirEnumVariant, hasher: &mut impl Hasher) {
    variant.name.hash(hasher);
    match &variant.kind {
        HirVariantKind::Unit => 0u8.hash(hasher),
        HirVariantKind::Tuple(types) => {
            1u8.hash(hasher);
            types.hash(hasher);
        }
        HirVariantKind::Struct(fields) => {
            2u8.hash(hasher);
            hash_fields(fields, hasher);
        }
    }
    hash_attrs(&variant.attrs, hasher);
}

fn hash_trait(trait_item: &HirTrait, hasher: &mut impl Hasher) {
    trait_item.name.hash(hasher);
    trait_item.visibility.is_public().hash(hasher);
    trait_item.generics.hash(hasher);
    trait_item.generic_defaults.hash(hasher);
    hash_bounds(&trait_item.generic_bounds, hasher);
    hash_bounds(&trait_item.supertraits, hasher);
    trait_item.methods.len().hash(hasher);
    for method in &trait_item.methods {
        hash_function(method, hasher);
    }
    format!("{:?}", trait_item.default_methods).hash(hasher);
    trait_item.type_aliases.len().hash(hasher);
    for alias in &trait_item.type_aliases {
        hash_type_alias(alias, hasher);
    }
    hash_attrs(&trait_item.attrs, hasher);
}

fn hash_impl(impl_item: &HirImpl, hasher: &mut impl Hasher) {
    impl_item.self_ty.hash(hasher);
    impl_item.trait_ty.hash(hasher);
    impl_item.generics.hash(hasher);
    impl_item.const_generics.hash(hasher);
    hash_bounds(&impl_item.generic_bounds, hasher);
    format!("{:?}", impl_item.methods).hash(hasher);
    format!("{:?}", impl_item.consts).hash(hasher);
    format!("{:?}", impl_item.type_aliases).hash(hasher);
    hash_attrs(&impl_item.attrs, hasher);
}

fn hash_const(const_item: &HirConst, hasher: &mut impl Hasher) {
    const_item.name.hash(hasher);
    const_item.visibility.is_public().hash(hasher);
    const_item.ty.hash(hasher);
    const_item.has_value.hash(hasher);
    hash_attrs(&const_item.attrs, hasher);
}

fn hash_type_alias(alias: &HirTypeAlias, hasher: &mut impl Hasher) {
    alias.name.hash(hasher);
    alias.visibility.is_public().hash(hasher);
    alias.ty.hash(hasher);
    hash_attrs(&alias.attrs, hasher);
}

fn hash_bounds(bounds: &[HirGenericBound], hasher: &mut impl Hasher) {
    bounds.len().hash(hasher);
    for bound in bounds {
        bound.param.hash(hasher);
        bound.target_ty.hash(hasher);
        bound.trait_ty.hash(hasher);
        bound.assoc_constraints.len().hash(hasher);
        for constraint in &bound.assoc_constraints {
            constraint.name.hash(hasher);
            constraint.ty.hash(hasher);
        }
    }
}

fn hash_attrs(attrs: &[HirAttr], hasher: &mut impl Hasher) {
    attrs.len().hash(hasher);
    for attr in attrs {
        attr.name.hash(hasher);
        attr.value.hash(hasher);
        attr.raw.hash(hasher);
    }
}
