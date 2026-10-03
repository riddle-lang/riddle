use std::{
    fmt::Write as _,
    hash::{Hash, Hasher},
};

use la_arena::{Arena, Idx};
use rowan::TextRange;

use super::Name;

pub type FunctionId = Idx<HirFunction>;
pub type StructId = Idx<HirStruct>;
pub type ModuleId = Idx<HirModule>;
pub type UseId = Idx<HirUse>;
pub type EnumId = Idx<HirEnum>;
pub type TraitId = Idx<HirTrait>;
pub type ImplId = Idx<HirImpl>;
pub type ConstId = Idx<HirConst>;
pub type TypeAliasId = Idx<HirTypeAlias>;

/// Implements `Debug` while leaving `extent` out.
///
/// `extent` is the declaration's whole source range, which editor outlines need
/// and nothing else does. Keeping it out of `Debug` keeps position out of
/// everything derived from it: an item that moves has not changed, and ranges
/// are read directly wherever they are actually wanted.
macro_rules! debug_without_extent {
    ($item:ident { $($field:ident),+ $(,)? }) => {
        impl std::fmt::Debug for $item {
            fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter
                    .debug_struct(stringify!($item))
                    $(.field(stringify!($field), &self.$field))+
                    .finish_non_exhaustive()
            }
        }
    };
}

debug_without_extent!(HirFunction {
    name,
    name_range,
    visibility,
    is_unsafe,
    generics,
    implicit_generics,
    const_generics,
    generic_bounds,
    params,
    ret_type,
    ret_type_range,
    has_body,
    attrs,
});
debug_without_extent!(HirStruct {
    name,
    visibility,
    name_range,
    generics,
    const_generics,
    generic_bounds,
    fields,
    attrs,
});
debug_without_extent!(HirEnum {
    name,
    name_range,
    visibility,
    generics,
    const_generics,
    generic_bounds,
    variants,
    attrs,
});
debug_without_extent!(HirTrait {
    name,
    name_range,
    visibility,
    generics,
    generic_defaults,
    generic_bounds,
    supertraits,
    methods,
    default_methods,
    type_aliases,
    attrs,
});
debug_without_extent!(HirImpl {
    self_ty,
    self_ty_range,
    trait_ty,
    trait_ty_range,
    callable,
    generics,
    const_generics,
    generic_bounds,
    methods,
    consts,
    type_aliases,
    attrs,
});
debug_without_extent!(HirConst {
    name,
    name_range,
    visibility,
    ty,
    ty_range,
    has_value,
    attrs,
});
debug_without_extent!(HirTypeAlias {
    name,
    name_range,
    visibility,
    ty,
    ty_range,
    attrs,
});
debug_without_extent!(HirModule {
    name,
    name_range,
    visibility,
    items,
    attrs,
});

#[derive(Debug)]
pub struct ItemTree {
    pub functions: Arena<HirFunction>,
    pub structs: Arena<HirStruct>,
    pub modules: Arena<HirModule>,
    pub uses: Arena<HirUse>,
    pub enums: Arena<HirEnum>,
    pub traits: Arena<HirTrait>,
    pub impls: Arena<HirImpl>,
    pub consts: Arena<HirConst>,
    pub type_aliases: Arena<HirTypeAlias>,
    pub top_level: Vec<TopLevelItem>,
    /// Functions declared or defined with the `extern "C"` ABI.
    pub extern_function_ids: Vec<FunctionId>,
}

#[derive(Debug, Clone, Copy)]
pub enum TopLevelItem {
    Function(FunctionId),
    Struct(StructId),
    Module(ModuleId),
    Use(UseId),
    Enum(EnumId),
    Trait(TraitId),
    Impl(ImplId),
    Const(ConstId),
    TypeAlias(TypeAliasId),
}

#[derive(Debug, Clone)]
pub enum Visibility {
    Private,
    Public,
}

impl Visibility {
    #[must_use]
    pub const fn is_public(&self) -> bool {
        matches!(self, Self::Public)
    }
}

#[derive(Debug, Clone)]
pub struct HirAttr {
    pub name: Name,
    pub value: Option<String>,
    pub raw: String,
    pub range: TextRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InternalAttrTarget {
    Trait,
    Struct,
    Enum,
    Other,
}

#[derive(Debug, Clone)]
pub struct HirInternalAttr {
    pub attr: HirAttr,
    pub target: InternalAttrTarget,
}

#[derive(Clone)]
pub struct HirFunction {
    pub name: Name,
    pub name_range: TextRange,
    /// The whole declaration, for editor outlines that need more than the name.
    pub extent: TextRange,
    pub visibility: Visibility,
    pub is_unsafe: bool,
    pub generics: Vec<Name>,
    pub implicit_generics: Vec<Name>,
    pub const_generics: Vec<Name>,
    pub generic_bounds: Vec<HirGenericBound>,
    pub params: Vec<HirParam>,
    pub ret_type: Option<HirTypeRef>,
    pub ret_type_range: Option<TextRange>,
    pub has_body: bool,
    pub attrs: Vec<HirAttr>,
}

#[derive(Debug, Clone)]
pub struct HirGenericBound {
    pub param: Name,
    pub target_ty: HirTypeRef,
    pub target_range: TextRange,
    pub trait_ty: HirTypeRef,
    pub trait_range: TextRange,
    pub callable: Option<HirCallableSignature>,
    pub assoc_constraints: Vec<HirAssocTypeConstraint>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HirCallableSignature {
    pub params: Vec<HirTypeRef>,
    pub ret: Box<HirTypeRef>,
}

#[derive(Debug, Clone)]
pub struct HirAssocTypeConstraint {
    pub name: Name,
    pub ty: HirTypeRef,
    pub range: TextRange,
}

/// Identity is the name and the type; `range` is only where it was written.
///
/// A `dyn Trait<Assoc = Ty>` is the same type wherever it appears, and hashing
/// the range into it moved the type-context fingerprint whenever the source
/// around it changed length.
impl PartialEq for HirAssocTypeConstraint {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.ty == other.ty
    }
}

impl Eq for HirAssocTypeConstraint {}

impl Hash for HirAssocTypeConstraint {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.ty.hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct HirParam {
    pub name: Name,
    pub name_range: TextRange,
    /// True only for a source-level `self` receiver.
    pub is_receiver: bool,
    pub is_mut: bool,
    pub ty: HirTypeRef,
    pub ty_range: TextRange,
    pub attrs: Vec<HirAttr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DynMethodSafety {
    Dispatchable,
    MissingReceiver,
    ByValueReceiver,
    Generic,
    ParameterUsesSelf,
    ReturnUsesSelf,
}

impl DynMethodSafety {
    #[must_use]
    pub const fn reason(self) -> &'static str {
        match self {
            Self::Dispatchable => "",
            Self::MissingReceiver => "the first parameter is not a `self` receiver",
            Self::ByValueReceiver => {
                "by-value `self` cannot be called through a borrowed dyn object"
            }
            Self::Generic => "generic methods require static monomorphization",
            Self::ParameterUsesSelf => "a non-receiver parameter mentions `Self`",
            Self::ReturnUsesSelf => "the return type mentions `Self`",
        }
    }
}

#[must_use]
pub fn dyn_method_safety(method: &HirFunction) -> DynMethodSafety {
    let Some(receiver) = method.params.first() else {
        return DynMethodSafety::MissingReceiver;
    };
    if !receiver.is_receiver {
        return DynMethodSafety::MissingReceiver;
    }
    if !matches!(receiver.ty, HirTypeRef::Ref(_, _)) {
        return DynMethodSafety::ByValueReceiver;
    }
    if !method.generics.is_empty()
        || !method.implicit_generics.is_empty()
        || !method.const_generics.is_empty()
    {
        return DynMethodSafety::Generic;
    }
    if method
        .params
        .iter()
        .skip(1)
        .any(|param| hir_type_mentions_bare_self(&param.ty))
    {
        return DynMethodSafety::ParameterUsesSelf;
    }
    if method
        .ret_type
        .as_ref()
        .is_some_and(hir_type_mentions_bare_self)
    {
        return DynMethodSafety::ReturnUsesSelf;
    }
    DynMethodSafety::Dispatchable
}

fn hir_type_mentions_bare_self(ty: &HirTypeRef) -> bool {
    match ty {
        HirTypeRef::Named(path) => {
            (path.segments.len() == 1 && path.segments.first().is_some_and(|name| name.0 == "Self"))
                || path
                    .segment_type_args
                    .iter()
                    .flat_map(|(_, args)| args)
                    .any(hir_type_mentions_bare_self)
                || path.type_args.iter().any(hir_type_mentions_bare_self)
        }
        HirTypeRef::Ref(inner, _)
        | HirTypeRef::Ptr { inner, .. }
        | HirTypeRef::Slice(inner)
        | HirTypeRef::Array(inner, _) => hir_type_mentions_bare_self(inner),
        HirTypeRef::Tuple(elements) => elements.iter().any(hir_type_mentions_bare_self),
        HirTypeRef::ImplTrait {
            trait_ty, callable, ..
        }
        | HirTypeRef::DynTrait {
            trait_ty, callable, ..
        } => {
            hir_type_mentions_bare_self(trait_ty)
                || callable
                    .as_ref()
                    .is_some_and(hir_callable_mentions_bare_self)
                || matches!(
                    ty,
                    HirTypeRef::DynTrait { assoc_constraints, .. }
                        if assoc_constraints
                            .iter()
                            .any(|constraint| hir_type_mentions_bare_self(&constraint.ty))
                )
        }
        HirTypeRef::Never | HirTypeRef::Const(_) | HirTypeRef::Unknown | HirTypeRef::Error => false,
    }
}

fn hir_callable_mentions_bare_self(signature: &HirCallableSignature) -> bool {
    signature.params.iter().any(hir_type_mentions_bare_self)
        || hir_type_mentions_bare_self(&signature.ret)
}

#[derive(Clone)]
pub struct HirStruct {
    pub name: Name,
    pub visibility: Visibility,
    pub name_range: TextRange,
    pub extent: TextRange,
    pub generics: Vec<Name>,
    pub const_generics: Vec<Name>,
    pub generic_bounds: Vec<HirGenericBound>,
    pub fields: Vec<HirStructField>,
    pub attrs: Vec<HirAttr>,
}

#[derive(Debug, Clone)]
pub struct HirStructField {
    pub name: Name,
    pub name_range: TextRange,
    pub visibility: Visibility,
    /// `mut name: T` — writable through a shared reference to the containing
    /// value, so a `&self` method can update it.
    pub is_mut: bool,
    pub ty: HirTypeRef,
    pub ty_range: TextRange,
    pub attrs: Vec<HirAttr>,
}

#[derive(Clone)]
pub struct HirEnum {
    pub name: Name,
    pub name_range: TextRange,
    pub extent: TextRange,
    pub visibility: Visibility,
    pub generics: Vec<Name>,
    pub const_generics: Vec<Name>,
    pub generic_bounds: Vec<HirGenericBound>,
    pub variants: Vec<HirEnumVariant>,
    pub attrs: Vec<HirAttr>,
}

#[derive(Debug, Clone)]
pub struct HirEnumVariant {
    pub name: Name,
    pub name_range: TextRange,
    pub kind: HirVariantKind,
    pub field_ranges: Vec<TextRange>,
    pub attrs: Vec<HirAttr>,
}

#[derive(Debug, Clone)]
pub enum HirVariantKind {
    /// `Foo`
    Unit,
    /// `Foo(A, B)`
    Tuple(Vec<HirTypeRef>),
    /// `Foo { x: T }`
    Struct(Vec<HirStructField>),
}

#[derive(Clone)]
pub struct HirTrait {
    pub name: Name,
    pub name_range: TextRange,
    pub extent: TextRange,
    pub visibility: Visibility,
    pub generics: Vec<Name>,
    pub generic_defaults: Vec<Option<HirTypeRef>>,
    pub generic_bounds: Vec<HirGenericBound>,
    pub supertraits: Vec<HirGenericBound>,
    pub methods: Vec<HirFunction>,
    pub default_methods: Vec<FunctionId>,
    pub type_aliases: Vec<HirTypeAlias>,
    pub attrs: Vec<HirAttr>,
}

#[derive(Clone)]
pub struct HirImpl {
    /// The whole `impl` block.
    pub extent: TextRange,
    /// The implementing type's path (`T` in `impl T` / `impl Trait for T`).
    pub self_ty: HirTypeRef,
    pub self_ty_range: TextRange,
    /// The trait being implemented, if any (`Trait` in `impl Trait for T`).
    pub trait_ty: Option<HirTypeRef>,
    pub trait_ty_range: Option<TextRange>,
    pub callable: Option<HirCallableSignature>,
    pub generics: Vec<Name>,
    pub const_generics: Vec<Name>,
    pub generic_bounds: Vec<HirGenericBound>,
    pub methods: Vec<FunctionId>,
    pub consts: Vec<ConstId>,
    pub type_aliases: Vec<TypeAliasId>,
    pub attrs: Vec<HirAttr>,
}

#[derive(Clone)]
pub struct HirConst {
    pub name: Name,
    pub name_range: TextRange,
    pub extent: TextRange,
    pub visibility: Visibility,
    pub ty: HirTypeRef,
    pub ty_range: TextRange,
    pub has_value: bool,
    pub attrs: Vec<HirAttr>,
}

#[derive(Clone)]
pub struct HirTypeAlias {
    pub name: Name,
    pub name_range: TextRange,
    pub extent: TextRange,
    pub visibility: Visibility,
    pub ty: Option<HirTypeRef>,
    pub ty_range: Option<TextRange>,
    pub attrs: Vec<HirAttr>,
}

#[derive(Clone)]
pub struct HirModule {
    pub name: Name,
    pub name_range: TextRange,
    pub extent: TextRange,
    pub visibility: Visibility,
    /// `mod foo;` → None; `mod foo { ... }` → Some(items)
    pub items: Option<Vec<TopLevelItem>>,
    pub attrs: Vec<HirAttr>,
}

#[derive(Debug, Clone)]
pub struct HirUse {
    pub tree: HirUseTree,
    pub visibility: Visibility,
    pub attrs: Vec<HirAttr>,
}

#[derive(Debug, Clone)]
pub struct HirUseTree {
    /// Prefix path, which may be an empty segment (top-level `{a, b}` form).
    pub prefix: HirPath,
    pub kind: HirUseTreeKind,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub enum HirUseTreeKind {
    /// `use foo::bar;` / `use foo::bar as baz;`
    Simple { alias: Option<Name> },
    /// `use foo::*;`
    Glob,
    /// `use foo::{a, b as c};`
    List(Vec<HirUseTree>),
}

#[derive(Debug, Clone)]
pub struct HirPath {
    pub anchor: PathAnchor,
    pub segments: Vec<Name>,
    pub segment_type_args: Vec<(usize, Vec<HirTypeRef>)>,
    pub type_args: Vec<HirTypeRef>,
    pub range: TextRange,
}

impl PartialEq for HirPath {
    fn eq(&self, other: &Self) -> bool {
        self.anchor == other.anchor
            && self.segments == other.segments
            && self.segment_type_args == other.segment_type_args
            && self.type_args == other.type_args
    }
}

impl Eq for HirPath {}

impl Hash for HirPath {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.anchor.hash(state);
        self.segments.hash(state);
        self.segment_type_args.hash(state);
        self.type_args.hash(state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PathAnchor {
    Plain,    // foo::bar
    Crate,    // crate::foo
    Super,    // super::foo
    SelfMod,  // self::foo
    Absolute, // ::foo
}

#[derive(Debug, Clone)]
pub enum HirTypeRef {
    Named(HirPath),
    Never,
    Ref(Box<Self>, bool), // (inner, mutable)
    /// Raw pointer type: `*const T` or `*mut T`.
    Ptr {
        mutable: bool,
        inner: Box<Self>,
    },
    Tuple(Vec<Self>),
    Slice(Box<Self>),
    Array(Box<Self>, HirConstArg),
    Const(HirConstArg),
    ImplTrait {
        trait_ty: Box<Self>,
        trait_range: TextRange,
        callable: Option<HirCallableSignature>,
        hidden: Option<Name>,
    },
    DynTrait {
        trait_ty: Box<Self>,
        trait_range: TextRange,
        callable: Option<HirCallableSignature>,
        assoc_constraints: Vec<HirAssocTypeConstraint>,
    },
    Unknown,
    Error,
}

/// Compares two type references by what they *mean*, not where they were
/// written.
///
/// `trait_range` records where an `impl Trait` or `dyn Trait` was spelled and
/// has no bearing on the type. Including it made the type identity move with
/// the surrounding source: the incremental checker hashes signatures into its
/// type-context fingerprint, and the pipeline bundles the user's code *before*
/// the standard library, so editing one user function byte-shifted every
/// standard-library `impl Trait` and invalidated all ~827 cached bodies.
impl PartialEq for HirTypeRef {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Named(left), Self::Named(right)) => left == right,
            (Self::Never, Self::Never) | (Self::Unknown, Self::Unknown) => true,
            (Self::Error, Self::Error) => true,
            (Self::Ref(left, left_mut), Self::Ref(right, right_mut)) => {
                left == right && left_mut == right_mut
            }
            (
                Self::Ptr {
                    mutable: left_mut,
                    inner: left,
                },
                Self::Ptr {
                    mutable: right_mut,
                    inner: right,
                },
            ) => left == right && left_mut == right_mut,
            (Self::Tuple(left), Self::Tuple(right)) => left == right,
            (Self::Slice(left), Self::Slice(right)) => left == right,
            (Self::Array(left, left_arg), Self::Array(right, right_arg)) => {
                left == right && left_arg == right_arg
            }
            (Self::Const(left), Self::Const(right)) => left == right,
            (
                Self::ImplTrait {
                    trait_ty: left_ty,
                    callable: left_callable,
                    hidden: left_hidden,
                    ..
                },
                Self::ImplTrait {
                    trait_ty: right_ty,
                    callable: right_callable,
                    hidden: right_hidden,
                    ..
                },
            ) => {
                left_ty == right_ty
                    && left_callable == right_callable
                    && left_hidden == right_hidden
            }
            (
                Self::DynTrait {
                    trait_ty: left_ty,
                    callable: left_callable,
                    assoc_constraints: left_constraints,
                    ..
                },
                Self::DynTrait {
                    trait_ty: right_ty,
                    callable: right_callable,
                    assoc_constraints: right_constraints,
                    ..
                },
            ) => {
                left_ty == right_ty
                    && left_callable == right_callable
                    && left_constraints == right_constraints
            }
            _ => false,
        }
    }
}

impl Eq for HirTypeRef {}

impl Hash for HirTypeRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Self::Named(path) => path.hash(state),
            Self::Never | Self::Unknown | Self::Error => {}
            Self::Ref(inner, mutable) => {
                inner.hash(state);
                mutable.hash(state);
            }
            Self::Ptr { mutable, inner } => {
                mutable.hash(state);
                inner.hash(state);
            }
            Self::Tuple(types) => types.hash(state),
            Self::Slice(inner) => inner.hash(state),
            Self::Array(inner, arg) => {
                inner.hash(state);
                arg.hash(state);
            }
            Self::Const(arg) => arg.hash(state),
            Self::ImplTrait {
                trait_ty,
                callable,
                hidden,
                ..
            } => {
                trait_ty.hash(state);
                callable.hash(state);
                hidden.hash(state);
            }
            Self::DynTrait {
                trait_ty,
                callable,
                assoc_constraints,
                ..
            } => {
                trait_ty.hash(state);
                callable.hash(state);
                assoc_constraints.hash(state);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HirConstArg {
    Value(usize),
    Param(Name),
    Unknown,
    Error,
}

impl HirPath {
    pub fn display(&self) -> String {
        let mut s = String::new();
        match self.anchor {
            PathAnchor::Absolute => s.push_str("::"),
            PathAnchor::Crate => s.push_str("crate"),
            PathAnchor::Super => s.push_str("super"),
            PathAnchor::SelfMod => s.push_str("self"),
            PathAnchor::Plain => {}
        }
        for (i, seg) in self.segments.iter().enumerate() {
            let need_sep = i > 0
                || matches!(
                    self.anchor,
                    PathAnchor::Crate | PathAnchor::Super | PathAnchor::SelfMod
                );
            if need_sep {
                s.push_str("::");
            }
            s.push_str(&seg.0);
            if let Some((_, args)) = self
                .segment_type_args
                .iter()
                .find(|(segment, _)| *segment == i)
            {
                s.push_str("::<");
                s.push_str(
                    &args
                        .iter()
                        .map(HirTypeRef::display)
                        .collect::<Vec<_>>()
                        .join(", "),
                );
                s.push('>');
            }
        }
        if !self.type_args.is_empty() {
            let args = self
                .type_args
                .iter()
                .map(HirTypeRef::display)
                .collect::<Vec<_>>()
                .join(", ");
            s.push('<');
            s.push_str(&args);
            s.push('>');
        }
        s
    }

    #[must_use]
    pub fn type_args_for_segment(&self, index: usize) -> &[HirTypeRef] {
        self.segment_type_args
            .iter()
            .find_map(|(segment, args)| (*segment == index).then_some(args.as_slice()))
            .unwrap_or_default()
    }

    /// `crate`, `super`, `self`, and `::xxx` are all considered non-pure simple names.
    #[must_use]
    pub fn as_single_name(&self) -> Option<&Name> {
        if matches!(self.anchor, PathAnchor::Plain) && self.segments.len() == 1 {
            Some(&self.segments[0])
        } else {
            None
        }
    }
}

impl HirTypeRef {
    pub fn display(&self) -> String {
        match self {
            Self::Named(path) => path.display(),
            Self::Never => "!".to_string(),
            Self::Ref(inner, mutable) => {
                let kw = if *mutable { "&mut " } else { "&" };
                format!("{}{}", kw, inner.display())
            }
            Self::Ptr { mutable, inner } => {
                let kind = if *mutable { "*mut" } else { "*const" };
                format!("{kind} {}", inner.display())
            }
            Self::Tuple(elements) => {
                let inner = elements
                    .iter()
                    .map(Self::display)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({inner})")
            }
            Self::Slice(inner) => format!("[{}]", inner.display()),
            Self::Array(inner, len) => format!("[{}; {}]", inner.display(), len.display()),
            Self::Const(value) => value.display(),
            Self::ImplTrait {
                trait_ty, callable, ..
            } => {
                let mut display = format!("impl {}", trait_ty.display());
                if let Some(signature) = callable {
                    let params = signature
                        .params
                        .iter()
                        .map(Self::display)
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(display, "({params}) -> {}", signature.ret.display())
                        .expect("writing to a String cannot fail");
                }
                display
            }
            Self::DynTrait {
                trait_ty,
                callable,
                assoc_constraints,
                ..
            } => {
                let mut display = format!("dyn {}", trait_ty.display());
                if !assoc_constraints.is_empty() {
                    let constraints = assoc_constraints
                        .iter()
                        .map(|constraint| {
                            format!("{} = {}", constraint.name.0, constraint.ty.display())
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(display, "<{constraints}>").expect("writing to a String cannot fail");
                }
                if let Some(signature) = callable {
                    let params = signature
                        .params
                        .iter()
                        .map(Self::display)
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(display, "({params}) -> {}", signature.ret.display())
                        .expect("writing to a String cannot fail");
                }
                display
            }
            Self::Unknown => "_".to_string(),
            Self::Error => "<error>".to_string(),
        }
    }
}

impl HirConstArg {
    #[must_use]
    pub fn display(&self) -> String {
        match self {
            Self::Value(value) => value.to_string(),
            Self::Param(name) => name.0.clone(),
            Self::Unknown => "_".to_string(),
            Self::Error => "<error>".to_string(),
        }
    }
}
