use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::hash::{BuildHasher, DefaultHasher, Hash, Hasher};
use std::sync::Arc;

use escape_analysis::EscapeResult;
use hir::{
    HirFile,
    body::{
        BinaryOp as HirBinOp, Body, BodyId, Expr, ExprId, LiteralPattern, MatchArm, PatId, Pattern,
        PatternBindingId, ResolvedName, Stmt, StmtId, UnaryOp as HirUnOp,
    },
    item_tree::{DynMethodSafety, HirTypeRef, PathAnchor, dyn_method_safety},
    place::Projection,
};
use type_checker::{
    CaptureMode, CapturePlace, CaptureSource, LambdaCapture, LambdaInfo, OperatorCall,
    PatternBindingMode, TypeCheckResult,
};

use crate::builder::Builder;
use crate::func::Function;
use crate::instr::{BinOp, CastOp, CmpOp, Inst, InstKind, PanicSite, UnOp};
use crate::module::Module;
use crate::types::{EnumVariantKind, FloatTy, FnPtrType, IntTy, StructDef, StructType, Type};
use crate::value::{BlockId, FuncRef, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BuiltinOperator {
    Binary(BinOp),
    Unary(UnOp),
    Assign(BinOp),
}

/// Lower a type-checked HIR module into MIR.
///
/// `analysis` determines whether each local variable escapes its scope;
/// non-escaping locals can be stack-allocated, while escaping ones require
/// GC-managed heap allocation in GC'd backends.
#[must_use]
pub fn lower_hir<S: BuildHasher>(
    hir: &HirFile,
    source: &str,
    type_result: &TypeCheckResult,
    escape_result: &EscapeResult,
    moved_exprs: &HashSet<(BodyId, ExprId), S>,
    gc_enabled: bool,
    package_names: &[String],
) -> Module {
    let method_impls = hir
        .item_tree
        .impls
        .iter()
        .flat_map(|(impl_id, imp)| {
            imp.methods
                .iter()
                .copied()
                .map(move |function_id| (function_id, impl_id))
        })
        .collect();
    let default_methods = hir
        .item_tree
        .traits
        .iter()
        .flat_map(|(trait_id, tr)| {
            tr.default_methods
                .iter()
                .copied()
                .map(move |function_id| (function_id, trait_id))
        })
        .collect();
    let mut ctx = LowerCtx {
        hir,
        source,
        type_result,
        analysis: escape_result,
        moved_exprs: moved_exprs.iter().copied().collect(),
        gc_enabled,
        module: Module::new("main"),
        method_impls,
        default_methods,
        expr_cache: HashMap::new(),
        coerced_values: HashSet::new(),
        current_body: None,
        current_function: None,
        scope_map: HashMap::new(),
        drop_scopes: Vec::new(),
        drop_slots: HashMap::new(),
        temporary_drop_scopes: Vec::new(),
        temporary_drop_slots: HashMap::new(),
        storage_bindings: HashSet::new(),
        parameter_storage: HashMap::new(),
        pattern_bindings: Vec::new(),
        generic_subst: HashMap::new(),
        generic_tc_subst: HashMap::new(),
        generic_const_subst: HashMap::new(),
        mono_functions: HashMap::new(),
        mono_methods: HashMap::new(),
        mono_generated_symbols: std::collections::HashSet::new(),
        loop_targets: Vec::new(),
        lambda_functions: HashMap::new(),
        generic_lambda_functions: HashMap::new(),
        function_adapters: HashMap::new(),
        capture_access: HashMap::new(),
        struct_types: RefCell::new(HashMap::new()),
        struct_defs: RefCell::new(Vec::new()),
        current_lambda: None,
        current_lambda_env: None,
        lambda_counter: 0,
        active_consts: HashSet::new(),
        package_names,
        free_symbol_counts: HashMap::new(),
    };
    let free_symbol_counts = hir
        .item_tree
        .functions
        .iter()
        .filter(|(fid, function)| {
            !ctx.method_impls.contains_key(fid)
                && !ctx.default_methods.contains_key(fid)
                // Extern and `c_export` names are ABI, not internal symbols.
                && !hir.item_tree.extern_function_ids.contains(fid)
                && !function.attrs.iter().any(|attr| attr.name.0 == "c_export")
        })
        .map(|(fid, function)| ctx.package_qualified_symbol(fid, function.name.0.clone()))
        .fold(HashMap::new(), |mut counts, symbol| {
            *counts.entry(symbol).or_insert(0) += 1;
            counts
        });
    ctx.free_symbol_counts = free_symbol_counts;

    // 遍历所有有函数体的函数
    for (fid, func) in hir.item_tree.functions.iter() {
        if ctx.builtin_operator_for_method(fid).is_some()
            || ctx.default_methods.contains_key(&fid)
            || (hir.std_loaded
                && hir.package_for_range(func.name_range).is_none()
                && func.attrs.iter().any(|attr| attr.name.0 == "builtin"))
            || !func.generics.is_empty()
            || !func.implicit_generics.is_empty()
            || !func.const_generics.is_empty()
            || ctx
                .impl_for_method(fid)
                .is_some_and(|imp| !imp.generics.is_empty() || !imp.const_generics.is_empty())
        {
            continue;
        }
        if let Some(body_id) = hir.function_bodies.get(&fid).copied() {
            let mir_func = ctx.lower_function(fid, ctx.function_name(fid), body_id);
            ctx.module.add_function(mir_func);
        }
    }

    // 注册 extern 函数声明
    let extern_funcs: Vec<_> = hir
        .item_tree
        .extern_function_ids
        .iter()
        .filter(|&&fid| !hir.function_bodies.contains_key(&fid))
        .map(|&fid| {
            let func = &hir.item_tree.functions[fid];
            (func.name.0.clone(), func)
        })
        .collect();
    for (name, func) in &extern_funcs {
        let params: Vec<Type> = func
            .params
            .iter()
            .map(|p| ctx.convert_hir_type(&p.ty))
            .collect();
        let ret_type = func
            .ret_type
            .as_ref()
            .map_or(Type::Unit, |rt| ctx.convert_hir_type(rt));
        ctx.module.add_extern(name.clone(), params, ret_type);
    }

    ctx.module.struct_defs = ctx.struct_defs.take();
    ctx.module
}

struct LowerCtx<'a> {
    hir: &'a HirFile,
    source: &'a str,
    type_result: &'a TypeCheckResult,
    analysis: &'a EscapeResult,
    moved_exprs: HashSet<(BodyId, ExprId)>,
    gc_enabled: bool,
    module: Module,
    method_impls: HashMap<hir::item_tree::FunctionId, hir::item_tree::ImplId>,
    default_methods: HashMap<hir::item_tree::FunctionId, hir::item_tree::TraitId>,
    expr_cache: HashMap<ExprId, Value>,
    /// Values already converted to their expression's target representation.
    coerced_values: HashSet<(Value, Type)>,
    /// The `BodyId` currently being lowered, used to look up `expr_types`.
    current_body: Option<BodyId>,
    current_function: Option<hir::item_tree::FunctionId>,
    /// Maps a `let` binding → its Value (or storage pointer, see below).
    scope_map: HashMap<PatternBindingId, Value>,
    drop_scopes: Vec<Vec<DropSlot>>,
    drop_slots: HashMap<CaptureSource, Vec<DropSlot>>,
    temporary_drop_scopes: Vec<Vec<DropSlot>>,
    temporary_drop_slots: HashMap<ExprId, Vec<DropSlot>>,
    /// Bindings backed by storage rather than a direct SSA value.
    storage_bindings: HashSet<PatternBindingId>,
    parameter_storage: HashMap<CaptureSource, Value>,
    pattern_bindings: Vec<HashMap<PatternBindingId, PatternBindingValue>>,
    generic_subst: HashMap<String, Type>,
    generic_tc_subst: HashMap<String, type_checker::Type>,
    generic_const_subst: HashMap<String, usize>,
    mono_functions: HashMap<(hir::item_tree::FunctionId, String), String>,
    mono_methods: HashMap<(hir::item_tree::FunctionId, String), String>,
    /// Final symbols of already-generated monomorphizations. The two keyed
    /// caches above can independently derive the same symbol (an impl
    /// method reached both as a method call and as a generic call); without
    /// this registry both would emit a body and the C build would fail on
    /// the redefinition.
    mono_generated_symbols: std::collections::HashSet<String>,
    loop_targets: Vec<LoopTargets>,
    lambda_functions: HashMap<(BodyId, ExprId), String>,
    generic_lambda_functions: HashMap<(BodyId, ExprId, String), String>,
    function_adapters: HashMap<(hir::item_tree::FunctionId, Vec<type_checker::Type>), String>,
    capture_access: HashMap<CapturePlace, CaptureAccess>,
    /// Converted nominal structs, keyed by symbol. A definition is registered as
    /// a recursive handle before its own fields are converted, which is what
    /// lets a self-referential struct terminate.
    struct_types: RefCell<HashMap<String, StructType>>,
    /// Strong handles for every definition created above. They move into the
    /// module, which keeps the `Weak` occurrences inside recursive types
    /// resolvable for as long as the module's types are used.
    struct_defs: RefCell<Vec<Arc<StructDef>>>,
    current_lambda: Option<ExprId>,
    current_lambda_env: Option<Value>,
    lambda_counter: u32,
    active_consts: HashSet<hir::item_tree::ConstId>,
    package_names: &'a [String],
    /// Final (pre-disambiguation) symbols of free functions, counted so
    /// same-named privates can be renamed before they emit duplicate C
    /// symbols or shadow each other in the interpreter's name table.
    free_symbol_counts: HashMap<String, usize>,
}

#[derive(Clone)]
struct CaptureAccess {
    place: Value,
    ty: Type,
}

#[derive(Clone)]
struct DropSlot {
    place: Value,
    flag: Value,
    ty: type_checker::Type,
    projection: Vec<DropProjection>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum DropProjection {
    Field(usize),
    Index(usize),
}

enum RuntimeDropProjection {
    Exact(DropProjection),
    Index(Value, IntTy),
}

/// Where a `let` binding's data lives: a stable slot it can take the address
/// of, or a plain SSA value.
#[derive(Clone, Copy)]
enum LetSource {
    Place(Value),
    Value(Value),
}

struct LetStorage {
    root: PatternBindingId,
    value: Value,
    value_ty: type_checker::Type,
    needs_drop: bool,
    delayed: bool,
}

struct LetPatternInput<'a> {
    pat: PatId,
    source: LetSource,
    value_ty: &'a type_checker::Type,
    projection: Vec<DropProjection>,
}

#[derive(Clone)]
struct PatternBindingValue {
    value: Value,
    ty: Type,
    tc_ty: type_checker::Type,
    place: Option<Value>,
    projection: Vec<DropProjection>,
}

struct MatchBindingInput<'a> {
    pat: PatId,
    value: Value,
    place: Option<Value>,
    value_ty: &'a type_checker::Type,
    projection: Vec<DropProjection>,
}

impl PatternBindingValue {
    const fn direct(
        value: Value,
        ty: Type,
        tc_ty: type_checker::Type,
        place: Option<Value>,
        projection: Vec<DropProjection>,
    ) -> Self {
        Self {
            value,
            ty,
            tc_ty,
            place,
            projection,
        }
    }
}

#[derive(Clone, Copy)]
struct LoopTargets {
    break_block: BlockId,
    continue_block: BlockId,
    /// `loop { }` 表达式的 break 值存放位（alloca），仅当结果类型非 Unit/Never 时存在。
    break_slot: Option<Value>,
    drop_depth: usize,
    temporary_drop_depth: usize,
}

#[derive(Clone, Copy)]
struct ForExprInput<'a> {
    param_values: &'a [Value],
    body: &'a Body,
    expr_id: ExprId,
    pat: PatId,
    iterable: ExprId,
    loop_body: ExprId,
}

struct ExprLoweringInput<'a> {
    param_values: &'a [Value],
    body: &'a Body,
    expr_id: ExprId,
    tc_type: Option<&'a type_checker::Type>,
    mir_type: &'a Type,
    diverges: bool,
}

struct LambdaExprInput<'a> {
    body_id: BodyId,
    expr_id: ExprId,
    params: &'a [hir::body::LambdaParam],
    body: ExprId,
    ty: &'a Type,
}

struct LambdaFunctionInput<'a> {
    body_id: BodyId,
    expr_id: ExprId,
    params: &'a [hir::body::LambdaParam],
    body: ExprId,
    name: &'a str,
    call_signature: &'a FnPtrType,
    info: &'a LambdaInfo,
    capture_types: &'a [Type],
    env_struct: &'a StructType,
}

#[derive(Default)]
struct MirSubst {
    types: HashMap<String, Type>,
    tc_types: HashMap<String, type_checker::Type>,
    consts: HashMap<String, usize>,
}

struct LoweringState {
    expr_cache: HashMap<ExprId, Value>,
    scope_map: HashMap<PatternBindingId, Value>,
    drop_scopes: Vec<Vec<DropSlot>>,
    drop_slots: HashMap<CaptureSource, Vec<DropSlot>>,
    temporary_drop_scopes: Vec<Vec<DropSlot>>,
    temporary_drop_slots: HashMap<ExprId, Vec<DropSlot>>,
    storage_bindings: HashSet<PatternBindingId>,
    parameter_storage: HashMap<CaptureSource, Value>,
    pattern_bindings: Vec<HashMap<PatternBindingId, PatternBindingValue>>,
    capture_access: HashMap<CapturePlace, CaptureAccess>,
    current_lambda: Option<ExprId>,
    current_lambda_env: Option<Value>,
    current_body: Option<BodyId>,
    coerced_values: HashSet<(Value, Type)>,
}

impl LowerCtx<'_> {
    fn take_lowering_state(&mut self) -> LoweringState {
        LoweringState {
            expr_cache: std::mem::take(&mut self.expr_cache),
            scope_map: std::mem::take(&mut self.scope_map),
            drop_scopes: std::mem::take(&mut self.drop_scopes),
            drop_slots: std::mem::take(&mut self.drop_slots),
            temporary_drop_scopes: std::mem::take(&mut self.temporary_drop_scopes),
            temporary_drop_slots: std::mem::take(&mut self.temporary_drop_slots),
            storage_bindings: std::mem::take(&mut self.storage_bindings),
            parameter_storage: std::mem::take(&mut self.parameter_storage),
            pattern_bindings: std::mem::take(&mut self.pattern_bindings),
            capture_access: std::mem::take(&mut self.capture_access),
            current_lambda: self.current_lambda,
            current_lambda_env: self.current_lambda_env.take(),
            current_body: self.current_body,
            coerced_values: std::mem::take(&mut self.coerced_values),
        }
    }

    fn restore_lowering_state(&mut self, state: LoweringState) {
        self.expr_cache = state.expr_cache;
        self.scope_map = state.scope_map;
        self.drop_scopes = state.drop_scopes;
        self.drop_slots = state.drop_slots;
        self.temporary_drop_scopes = state.temporary_drop_scopes;
        self.temporary_drop_slots = state.temporary_drop_slots;
        self.storage_bindings = state.storage_bindings;
        self.parameter_storage = state.parameter_storage;
        self.pattern_bindings = state.pattern_bindings;
        self.capture_access = state.capture_access;
        self.current_lambda = state.current_lambda;
        self.current_lambda_env = state.current_lambda_env;
        self.current_body = state.current_body;
        self.coerced_values = state.coerced_values;
    }
}

enum TypePattern {
    Other,
    EnumVariant {
        enum_id: hir::item_tree::EnumId,
        variant_index: usize,
        args: Vec<type_checker::Type>,
    },
}

mod calls;
mod drops;
mod expressions;
mod functions;
mod monomorphize;
mod patterns;
mod places;
mod types;

fn closure_env_type() -> Type {
    Type::Ptr(Box::new(Type::Unit))
}

fn closure_drop_function_type() -> Type {
    Type::FnPtr(FnPtrType {
        params: vec![closure_env_type()],
        ret: Box::new(Type::Unit),
    })
}

fn closure_value_type(signature: FnPtrType) -> Type {
    let mut hasher = DefaultHasher::new();
    signature.hash(&mut hasher);
    let mut call_params = Vec::with_capacity(signature.params.len() + 1);
    call_params.push(closure_env_type());
    call_params.extend(signature.params.clone());
    let call = Type::FnPtr(FnPtrType {
        params: call_params,
        ret: signature.ret,
    });
    let name = format!("riddle_closure_{:016x}", hasher.finish());
    Type::Struct(StructType::defined(
        name.clone(),
        name,
        vec![
            ("call".into(), call),
            ("env".into(), closure_env_type()),
            ("drop".into(), closure_drop_function_type()),
        ],
    ))
}

fn closure_call_signature(ty: &Type) -> Option<FnPtrType> {
    let Type::Struct(strukt) = ty else {
        return None;
    };
    let fields = strukt.def();
    match fields.fields.first().map(|(_, ty)| ty) {
        Some(Type::FnPtr(signature))
            if fields.fields.get(1).map(|field| &field.1) == Some(&closure_env_type()) =>
        {
            Some(signature.clone())
        }
        _ => None,
    }
}

fn is_self_associated_path(path: &hir::item_tree::HirPath) -> bool {
    matches!(path.anchor, hir::item_tree::PathAnchor::Plain)
        && path.segments.len() == 2
        && path.segments[0].0 == "Self"
}

fn mono_name_from_parts(base: &str, args: &[String]) -> String {
    if args.is_empty() {
        return base.to_string();
    }
    format!("{}_{}", base, args.join("_"))
}

const fn tc_const_arg_to_usize(ty: &type_checker::Type) -> Option<usize> {
    match ty {
        type_checker::Type::Const(value) => value.as_usize(),
        _ => None,
    }
}

/// Encodes a name with its byte length so `_`-joined suffixes stay unique.
fn encode_mono_name(name: &str) -> String {
    format!("{}:{}", name.len(), name)
}

fn mono_type_name(ty: &Type) -> String {
    match ty {
        Type::Int(i) => format!("{i:?}").to_ascii_lowercase(),
        Type::Float(f) => format!("{f:?}").to_ascii_lowercase(),
        Type::Bool => "bool".into(),
        Type::Str => "str".into(),
        Type::Char => "char".into(),
        Type::Unit => "unit".into(),
        Type::Never => "never".into(),
        Type::Ref(inner, mutable) => format!(
            "ref{}_{}",
            if *mutable { "mut" } else { "" },
            mono_type_name(inner)
        ),
        Type::Ptr(inner) => format!("ptr_{}", mono_type_name(inner)),
        Type::Tuple(elems) => format!(
            "tuple_{}",
            elems
                .iter()
                .map(mono_type_name)
                .collect::<Vec<_>>()
                .join("_")
        ),
        Type::Slice(inner) => format!("slice_{}", mono_type_name(inner)),
        Type::Array(inner, len) => format!("arr{len}_{}", mono_type_name(inner)),
        // Length-prefix user type names so differently-named type
        // arguments cannot collide when suffixes join on `_`:
        // `<A_B, C>` and `<A, B_C>` encode as `4:A_B_1:C` vs `1:A_3:B_C`.
        Type::Struct(st) => encode_mono_name(st.name()),
        Type::Enum(e) => encode_mono_name(&e.name),
        Type::FnPtr(_) => "fn".into(),
        Type::Void => "void".into(),
    }
}

fn mono_type_symbol(ty: &Type) -> String {
    match ty {
        Type::Struct(st) => st.symbol().to_string(),
        Type::Enum(enumeration) => format!("enum-layout::{enumeration:?}"),
        Type::FnPtr(signature) => format!(
            "fn({})->{}",
            signature
                .params
                .iter()
                .map(mono_type_symbol)
                .collect::<Vec<_>>()
                .join(","),
            mono_type_symbol(&signature.ret)
        ),
        Type::Ref(inner, mutable) => format!(
            "ref{}<{}>",
            if *mutable { "mut" } else { "" },
            mono_type_symbol(inner)
        ),
        Type::Ptr(inner) => format!("ptr<{}>", mono_type_symbol(inner)),
        Type::Tuple(elements) => format!(
            "tuple<{}>",
            elements
                .iter()
                .map(mono_type_symbol)
                .collect::<Vec<_>>()
                .join(",")
        ),
        Type::Slice(inner) => format!("slice<{}>", mono_type_symbol(inner)),
        Type::Array(inner, len) => format!("array<{len},{}>", mono_type_symbol(inner)),
        _ => mono_type_name(ty),
    }
}

// 辅助函数

fn primitive_scalar_name(ty: &hir::item_tree::HirTypeRef) -> Option<&str> {
    let hir::item_tree::HirTypeRef::Named(path) = ty else {
        return None;
    };
    path.as_single_name()
        .map(|name| name.0.as_str())
        .filter(|name| {
            matches!(
                *name,
                "bool"
                    | "i8"
                    | "i16"
                    | "i32"
                    | "i64"
                    | "isize"
                    | "u8"
                    | "u16"
                    | "u32"
                    | "u64"
                    | "usize"
                    | "f32"
                    | "f64"
            )
        })
}

fn builtin_operator(lang: &str, method: &str) -> Option<BuiltinOperator> {
    let operator = match (lang, method) {
        ("add", "add") => BuiltinOperator::Binary(BinOp::Add),
        ("sub", "sub") => BuiltinOperator::Binary(BinOp::Sub),
        ("mul", "mul") => BuiltinOperator::Binary(BinOp::Mul),
        ("div", "div") => BuiltinOperator::Binary(BinOp::Div),
        ("rem", "rem") => BuiltinOperator::Binary(BinOp::Mod),
        ("neg", "neg") => BuiltinOperator::Unary(UnOp::Neg),
        ("not", "not") => BuiltinOperator::Unary(UnOp::Not),
        ("bitand", "bitand") => BuiltinOperator::Binary(BinOp::BitAnd),
        ("bitor", "bitor") => BuiltinOperator::Binary(BinOp::BitOr),
        ("bitxor", "bitxor") => BuiltinOperator::Binary(BinOp::BitXor),
        ("shl", "shl") => BuiltinOperator::Binary(BinOp::Shl),
        ("shr", "shr") => BuiltinOperator::Binary(BinOp::Shr),
        ("add_assign", "add_assign") => BuiltinOperator::Assign(BinOp::Add),
        ("sub_assign", "sub_assign") => BuiltinOperator::Assign(BinOp::Sub),
        ("mul_assign", "mul_assign") => BuiltinOperator::Assign(BinOp::Mul),
        ("div_assign", "div_assign") => BuiltinOperator::Assign(BinOp::Div),
        ("rem_assign", "rem_assign") => BuiltinOperator::Assign(BinOp::Mod),
        ("bitand_assign", "bitand_assign") => BuiltinOperator::Assign(BinOp::BitAnd),
        ("bitor_assign", "bitor_assign") => BuiltinOperator::Assign(BinOp::BitOr),
        ("bitxor_assign", "bitxor_assign") => BuiltinOperator::Assign(BinOp::BitXor),
        ("shl_assign", "shl_assign") => BuiltinOperator::Assign(BinOp::Shl),
        ("shr_assign", "shr_assign") => BuiltinOperator::Assign(BinOp::Shr),
        _ => return None,
    };
    Some(operator)
}

fn builtin_operator_supports(op: BuiltinOperator, scalar: &str) -> bool {
    let integer = matches!(
        scalar,
        "i8" | "i16" | "i32" | "i64" | "isize" | "u8" | "u16" | "u32" | "u64" | "usize"
    );
    let float = matches!(scalar, "f32" | "f64");
    let signed = matches!(scalar, "i8" | "i16" | "i32" | "i64" | "isize");
    match op {
        BuiltinOperator::Binary(BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod)
        | BuiltinOperator::Assign(BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod) => {
            integer || float
        }
        BuiltinOperator::Unary(UnOp::Neg) => signed || float,
        BuiltinOperator::Unary(UnOp::Not) => scalar == "bool" || integer,
        BuiltinOperator::Binary(BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor)
        | BuiltinOperator::Assign(BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor) => {
            scalar == "bool" || integer
        }
        BuiltinOperator::Binary(BinOp::Shl | BinOp::Shr)
        | BuiltinOperator::Assign(BinOp::Shl | BinOp::Shr) => integer,
        BuiltinOperator::Unary(UnOp::Ref | UnOp::MutRef | UnOp::Deref) => false,
    }
}

fn operator_params_match(
    function: &hir::item_tree::HirFunction,
    self_ty: &hir::item_tree::HirTypeRef,
    op: BuiltinOperator,
) -> bool {
    match op {
        BuiltinOperator::Binary(_) => {
            function.params.len() == 2
                && type_matches_self(&function.params[0].ty, self_ty)
                && type_matches_self(&function.params[1].ty, self_ty)
        }
        BuiltinOperator::Unary(_) => {
            function.params.len() == 1 && type_matches_self(&function.params[0].ty, self_ty)
        }
        BuiltinOperator::Assign(_) => {
            function.params.len() == 2
                && matches!(
                    &function.params[0].ty,
                    hir::item_tree::HirTypeRef::Ref(inner, true)
                        if type_matches_self(inner, self_ty)
                )
                && type_matches_self(&function.params[1].ty, self_ty)
        }
    }
}

fn type_matches_self(
    ty: &hir::item_tree::HirTypeRef,
    self_ty: &hir::item_tree::HirTypeRef,
) -> bool {
    ty == self_ty
        || matches!(
            ty,
            hir::item_tree::HirTypeRef::Named(path)
                if path.as_single_name().is_some_and(|name| name.0 == "Self")
        )
}

fn trait_operator_contract(
    trait_item: &hir::item_tree::HirTrait,
    method: &str,
    op: BuiltinOperator,
) -> bool {
    let Some(function) = trait_item
        .methods
        .iter()
        .find(|function| function.name.0 == method)
    else {
        return false;
    };
    if !function.generics.is_empty() || !function.const_generics.is_empty() {
        return false;
    }
    let self_ty = hir::item_tree::HirTypeRef::Named(hir::item_tree::HirPath {
        anchor: hir::item_tree::PathAnchor::Plain,
        segments: vec![hir::Name("Self".into())],
        segment_type_args: Vec::new(),
        type_args: Vec::new(),
        range: function.name_range,
    });
    if !trait_operator_params_match(trait_item, function, &self_ty, op) {
        return false;
    }
    match op {
        BuiltinOperator::Assign(_) => returns_unit(function),
        BuiltinOperator::Binary(_) | BuiltinOperator::Unary(_) => {
            trait_item
                .type_aliases
                .iter()
                .any(|alias| alias.name.0 == "Output" && alias.ty.is_none())
                && function.ret_type.as_ref().is_some_and(is_self_output)
        }
    }
}

fn trait_operator_params_match(
    trait_item: &hir::item_tree::HirTrait,
    function: &hir::item_tree::HirFunction,
    self_ty: &hir::item_tree::HirTypeRef,
    op: BuiltinOperator,
) -> bool {
    if matches!(op, BuiltinOperator::Unary(_)) {
        return operator_params_match(function, self_ty, op);
    }
    let Some(rhs_name) = trait_item.generics.first() else {
        return operator_params_match(function, self_ty, op);
    };
    if trait_item.generics.len() != 1
        || !trait_item
            .generic_defaults
            .first()
            .and_then(Option::as_ref)
            .is_some_and(|default| type_matches_self(default, self_ty))
    {
        return false;
    }
    let rhs_matches = function.params.get(1).is_some_and(|param| {
        matches!(
            &param.ty,
            hir::item_tree::HirTypeRef::Named(path)
                if path.as_single_name().is_some_and(|name| name == rhs_name)
        )
    });
    match op {
        BuiltinOperator::Binary(_) => {
            function.params.len() == 2
                && type_matches_self(&function.params[0].ty, self_ty)
                && rhs_matches
        }
        BuiltinOperator::Assign(_) => {
            function.params.len() == 2
                && matches!(
                    &function.params[0].ty,
                    hir::item_tree::HirTypeRef::Ref(inner, true)
                        if type_matches_self(inner, self_ty)
                )
                && rhs_matches
        }
        BuiltinOperator::Unary(_) => unreachable!(),
    }
}

fn is_self_output(ty: &hir::item_tree::HirTypeRef) -> bool {
    let hir::item_tree::HirTypeRef::Named(path) = ty else {
        return false;
    };
    is_self_associated_path(path) && path.segments[1].0 == "Output"
}

fn returns_unit(function: &hir::item_tree::HirFunction) -> bool {
    function.ret_type.as_ref().is_none_or(
        |ty| matches!(ty, hir::item_tree::HirTypeRef::Tuple(elements) if elements.is_empty()),
    )
}

fn convert_binop(op: HirBinOp) -> BinOp {
    match op {
        HirBinOp::Add => BinOp::Add,
        HirBinOp::Sub => BinOp::Sub,
        HirBinOp::Mul => BinOp::Mul,
        HirBinOp::Div => BinOp::Div,
        HirBinOp::Mod => BinOp::Mod,
        HirBinOp::BitAnd | HirBinOp::And => BinOp::BitAnd,
        HirBinOp::BitOr | HirBinOp::Or => BinOp::BitOr,
        HirBinOp::BitXor => BinOp::BitXor,
        HirBinOp::Shl => BinOp::Shl,
        HirBinOp::Shr => BinOp::Shr,
        // comparison/assign should be handled before reaching here
        HirBinOp::Eq
        | HirBinOp::Neq
        | HirBinOp::Lt
        | HirBinOp::Gt
        | HirBinOp::LtEq
        | HirBinOp::GtEq
        | HirBinOp::Assign
        | HirBinOp::AddAssign
        | HirBinOp::SubAssign
        | HirBinOp::MulAssign
        | HirBinOp::DivAssign
        | HirBinOp::ModAssign
        | HirBinOp::BitAndAssign
        | HirBinOp::BitOrAssign
        | HirBinOp::BitXorAssign
        | HirBinOp::ShlAssign
        | HirBinOp::ShrAssign => unreachable!("cmp/assign handled before convert_binop"),
    }
}

fn convert_cmp_op(op: HirBinOp) -> CmpOp {
    match op {
        HirBinOp::Eq => CmpOp::Eq,
        HirBinOp::Neq => CmpOp::Neq,
        HirBinOp::Lt => CmpOp::Lt,
        HirBinOp::Gt => CmpOp::Gt,
        HirBinOp::LtEq => CmpOp::LtEq,
        HirBinOp::GtEq => CmpOp::GtEq,
        // Guarded by the caller — these never reach here
        HirBinOp::Assign
        | HirBinOp::Add
        | HirBinOp::Sub
        | HirBinOp::Mul
        | HirBinOp::Div
        | HirBinOp::Mod
        | HirBinOp::BitAnd
        | HirBinOp::BitOr
        | HirBinOp::BitXor
        | HirBinOp::Shl
        | HirBinOp::Shr
        | HirBinOp::And
        | HirBinOp::Or
        | HirBinOp::AddAssign
        | HirBinOp::SubAssign
        | HirBinOp::MulAssign
        | HirBinOp::DivAssign
        | HirBinOp::ModAssign
        | HirBinOp::BitAndAssign
        | HirBinOp::BitOrAssign
        | HirBinOp::BitXorAssign
        | HirBinOp::ShlAssign
        | HirBinOp::ShrAssign => {
            unreachable!("convert_cmp_op called with non-comparison op: {op:?}")
        }
    }
}

const fn comparison_trait(op: HirBinOp) -> Option<(&'static str, &'static str)> {
    Some(match op {
        HirBinOp::Eq => ("partial_eq", "eq"),
        HirBinOp::Neq => ("partial_eq", "ne"),
        HirBinOp::Lt => ("partial_ord", "lt"),
        HirBinOp::Gt => ("partial_ord", "gt"),
        HirBinOp::LtEq => ("partial_ord", "le"),
        HirBinOp::GtEq => ("partial_ord", "ge"),
        _ => return None,
    })
}

fn builtin_comparison_types(
    op: HirBinOp,
    lhs: &type_checker::Type,
    rhs: &type_checker::Type,
) -> bool {
    let numeric = |ty: &type_checker::Type| {
        matches!(
            ty,
            type_checker::Type::Int(_)
                | type_checker::Type::Float(_)
                | type_checker::Type::InferInt
                | type_checker::Type::InferFloat
        )
    };
    let same_numeric_kind = |lhs: &type_checker::Type, rhs: &type_checker::Type| {
        matches!(
            (lhs, rhs),
            (type_checker::Type::Int(_), type_checker::Type::Int(_))
                | (type_checker::Type::Float(_), type_checker::Type::Float(_))
                | (type_checker::Type::InferInt, type_checker::Type::InferInt)
                | (
                    type_checker::Type::InferFloat,
                    type_checker::Type::InferFloat
                )
        )
    };
    match op {
        HirBinOp::Eq | HirBinOp::Neq => {
            if numeric(lhs) && numeric(rhs) && same_numeric_kind(lhs, rhs) {
                return true;
            }
            matches!(
                (lhs, rhs),
                (type_checker::Type::Bool, type_checker::Type::Bool)
                    | (type_checker::Type::Char, type_checker::Type::Char)
                    | (type_checker::Type::Str, type_checker::Type::Str)
                    | (type_checker::Type::Unit, type_checker::Type::Unit)
            ) || matches!(
                (lhs, rhs),
                (
                    type_checker::Type::Ref(inner_lhs, false),
                    type_checker::Type::Ref(inner_rhs, false)
                ) if matches!(inner_lhs.as_ref(), type_checker::Type::Str)
                    && matches!(inner_rhs.as_ref(), type_checker::Type::Str)
            ) || matches!(
                (lhs, rhs),
                (
                    type_checker::Type::Ptr {
                        inner: inner_lhs,
                        mutable: mut_lhs,
                    },
                    type_checker::Type::Ptr {
                        inner: inner_rhs,
                        mutable: mut_rhs,
                    },
                ) if inner_lhs == inner_rhs && mut_lhs == mut_rhs
            )
        }
        HirBinOp::Lt | HirBinOp::Gt | HirBinOp::LtEq | HirBinOp::GtEq => {
            (numeric(lhs) && numeric(rhs) && same_numeric_kind(lhs, rhs))
                || matches!(
                    (lhs, rhs),
                    (type_checker::Type::Char, type_checker::Type::Char)
                )
        }
        _ => false,
    }
}

fn convert_unop(op: HirUnOp) -> UnOp {
    match op {
        HirUnOp::Neg => UnOp::Neg,
        HirUnOp::Not => UnOp::Not,
        HirUnOp::Ref => UnOp::Ref,
        HirUnOp::MutRef => UnOp::MutRef,
        HirUnOp::Deref => UnOp::Deref,
        // Pos is handled as a passthrough before reaching here
        HirUnOp::Pos => unreachable!("Pos should be handled as passthrough"),
    }
}

fn parse_int_suffix(suffix: Option<&str>) -> IntTy {
    match suffix {
        Some("i8") => IntTy::I8,
        Some("i16") => IntTy::I16,
        Some("i64") => IntTy::I64,
        Some("isize") => IntTy::Isize,
        Some("u8") => IntTy::U8,
        Some("u16") => IntTy::U16,
        Some("u32") => IntTy::U32,
        Some("u64") => IntTy::U64,
        Some("usize") => IntTy::Usize,
        _ => IntTy::I32, // 默认 i32
    }
}

fn parse_float_suffix(suffix: Option<&str>) -> FloatTy {
    match suffix {
        Some("f32") => FloatTy::F32,
        _ => FloatTy::F64, // 默认 f64
    }
}

/// Every binding a `let` pattern introduces, paired with its `mut` flag.
fn let_pattern_bindings(body: &Body, pat: PatId) -> Vec<(PatternBindingId, bool)> {
    let mut bindings = Vec::new();
    collect_let_pattern_bindings(body, pat, &mut bindings);
    bindings
}

fn collect_let_pattern_bindings(
    body: &Body,
    pat: PatId,
    bindings: &mut Vec<(PatternBindingId, bool)>,
) {
    match &body.pats[pat] {
        Pattern::Binding { is_mut, .. } => bindings.push((
            PatternBindingId {
                pattern: pat,
                field: None,
            },
            *is_mut,
        )),
        Pattern::Reference { pattern, .. } => {
            collect_let_pattern_bindings(body, *pattern, bindings);
        }
        Pattern::Tuple { elements } | Pattern::TupleStruct { elements, .. } => {
            for element in elements {
                collect_let_pattern_bindings(body, *element, bindings);
            }
        }
        Pattern::Struct { fields, .. } => {
            for (index, field) in fields.iter().enumerate() {
                match field.pat {
                    Some(nested) => collect_let_pattern_bindings(body, nested, bindings),
                    // Shorthand `Point { x, y }` binds immutably.
                    None => bindings.push((
                        PatternBindingId {
                            pattern: pat,
                            field: Some(index),
                        },
                        false,
                    )),
                }
            }
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Path { .. } | Pattern::Or { .. } => {}
    }
}

/// Resolve an aggregate field name to its index using type information.
fn resolve_field_index(
    hir: &HirFile,
    type_result: &TypeCheckResult,
    body_id: BodyId,
    base: ExprId,
    field_name: &hir::Name,
) -> usize {
    let Some(ty) = type_result.expr_types.get(&(body_id, base)) else {
        return 0;
    };
    let ty = match ty {
        type_checker::Type::Ref(inner, _) | type_checker::Type::Ptr { inner, .. } => inner.as_ref(),
        ty => ty,
    };
    match ty {
        type_checker::Type::Struct(struct_id, _) => hir.item_tree.structs[*struct_id]
            .fields
            .iter()
            .position(|field| field.name == *field_name)
            .unwrap_or(0),
        type_checker::Type::Tuple(elements) => field_name
            .0
            .parse::<usize>()
            .ok()
            .filter(|index| *index < elements.len())
            .unwrap_or(0),
        _ => 0,
    }
}

/// The cast matrix the type checker's E0012 rule admits.
fn try_cast_op(source: &Type, target: &Type) -> Option<CastOp> {
    match (source, target) {
        (Type::Int(IntTy::U8), Type::Char) => Some(CastOp::IntToChar),
        (Type::Int(_) | Type::Char, Type::Int(_)) => Some(CastOp::IntToInt),
        (Type::Int(_), Type::Float(_)) => Some(CastOp::IntToFloat),
        (Type::Float(_), Type::Int(_)) => Some(CastOp::FloatToInt),
        (Type::Float(_), Type::Float(_)) => Some(CastOp::FloatToFloat),
        (Type::Bool, Type::Int(_)) => Some(CastOp::BoolToInt),
        (Type::Int(_), Type::Bool) => Some(CastOp::IntToBool),
        (Type::Int(_), Type::Ptr(_)) => Some(CastOp::IntToPtr),
        (Type::Ref(source, _), Type::Ptr(target)) if source == target => Some(CastOp::PtrToPtr),
        (Type::Ptr(_), Type::Ptr(_)) => Some(CastOp::PtrToPtr),
        _ => None,
    }
}

fn determine_cast_op(source: &Type, target: &Type) -> Option<CastOp> {
    try_cast_op(source, target)
}

fn is_raw_parts_to_slice_cast(source: &Type, target: &Type) -> bool {
    match (source, target) {
        (Type::Tuple(parts), Type::Ref(target, _)) => {
            let Type::Slice(target) = target.as_ref() else {
                return false;
            };
            matches!(
                parts.as_slice(),
                [Type::Ptr(source), Type::Int(IntTy::Usize)] if source.as_ref() == target.as_ref()
            )
        }
        _ => false,
    }
}

/// `&[T] as (*const T, usize)`: decomposing a fat pointer into its parts.
fn is_slice_to_raw_parts_cast(source: &Type, target: &Type) -> bool {
    match (source, target) {
        (Type::Ref(source, _), Type::Tuple(parts)) => {
            let Type::Slice(element) = source.as_ref() else {
                return false;
            };
            matches!(
                parts.as_slice(),
                [Type::Ptr(inner), Type::Int(IntTy::Usize)] if inner.as_ref() == element.as_ref()
            )
        }
        _ => false,
    }
}

fn is_byte_str_layout_cast(source: &Type, target: &Type) -> bool {
    match (source, target) {
        (Type::Ref(source, false), Type::Ref(target, false)) => matches!(
            (source.as_ref(), target.as_ref()),
            (Type::Slice(element), Type::Str)
                | (Type::Str, Type::Slice(element))
                if matches!(element.as_ref(), Type::Int(IntTy::U8))
        ),
        _ => false,
    }
}

/// Returns whether a trait method can be represented in a borrowed dyn-trait table.
pub(super) fn is_dyn_object_safe_method(method: &hir::item_tree::HirFunction) -> bool {
    matches!(dyn_method_safety(method), DynMethodSafety::Dispatchable)
}

#[cfg(test)]
mod cast_matrix_tests {
    use super::{CastOp, try_cast_op};
    use crate::types::{IntTy, Type};

    fn int(ty: IntTy) -> Type {
        Type::Int(ty)
    }

    #[test]
    fn every_documented_cast_pair_has_a_mir_lowering() {
        let int_widths = [
            IntTy::I8,
            IntTy::I16,
            IntTy::I32,
            IntTy::I64,
            IntTy::U8,
            IntTy::U16,
            IntTy::U32,
            IntTy::U64,
        ];
        for source in int_widths {
            let source = int(source);
            assert!(
                try_cast_op(&source, &int(IntTy::I64)).is_some(),
                "{source:?} as int"
            );
            assert!(
                try_cast_op(&source, &Type::Float(crate::types::FloatTy::F64)).is_some(),
                "{source:?} as float"
            );
            assert!(
                try_cast_op(&source, &Type::Bool).is_some(),
                "{source:?} as bool"
            );
            assert!(
                try_cast_op(&source, &Type::Ptr(Box::new(int(IntTy::U8)))).is_some(),
                "{source:?} as *mut u8"
            );
        }
        assert_eq!(
            try_cast_op(&int(IntTy::U8), &Type::Char),
            Some(CastOp::IntToChar)
        );
        assert_eq!(
            try_cast_op(&Type::Char, &int(IntTy::I32)),
            Some(CastOp::IntToInt)
        );
        assert_eq!(
            try_cast_op(&Type::Bool, &int(IntTy::I32)),
            Some(CastOp::BoolToInt)
        );
        assert_eq!(
            try_cast_op(&Type::Float(crate::types::FloatTy::F32), &int(IntTy::U32)),
            Some(CastOp::FloatToInt)
        );
        assert!(try_cast_op(&int(IntTy::I64), &int(IntTy::U8)).is_some());
        let pointer = Type::Ptr(Box::new(int(IntTy::U8)));
        let other_pointer = Type::Ptr(Box::new(int(IntTy::I32)));
        assert!(
            try_cast_op(&pointer, &other_pointer).is_some(),
            "*u8 as *i32"
        );
        // Unsized references (slices, str) cannot be cast to pointers at the
        // type-checker level, so no lowering exists for them either.
        let byte_slice = Type::Ref(Box::new(Type::Slice(Box::new(int(IntTy::U8)))), false);
        assert!(
            try_cast_op(&byte_slice, &pointer).is_none(),
            "&[u8] as *mut u8"
        );
    }

    #[test]
    fn pairs_the_checker_rejects_have_no_lowering() {
        let float = Type::Float(crate::types::FloatTy::F64);
        assert!(try_cast_op(&Type::Bool, &float).is_none(), "bool as f64");
        assert!(try_cast_op(&float, &Type::Char).is_none(), "f64 as char");
        assert!(try_cast_op(&Type::Str, &Type::Ptr(Box::new(int(IntTy::U8)))).is_none());
        let array = Type::Array(Box::new(int(IntTy::I32)), 2);
        assert!(
            try_cast_op(&array, &int(IntTy::I32)).is_none(),
            "[i32; 2] as i32"
        );
    }
}
