use hir::{
    HirFile,
    body::{BodyId, ExprId},
    item_tree::{EnumId, FunctionId, HirStruct, StructId, TraitId},
};
use rowan::TextRange;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClosureId {
    pub body: BodyId,
    pub expr: ExprId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OpaqueCallableId(pub TextRange);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallableSignature {
    pub is_unsafe: bool,
    pub kind: ClosureKind,
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int(IntTy),
    Float(FloatTy),
    InferInt,
    InferFloat,
    Bool,
    Str,
    Char,
    Unit,
    Never,
    Ref(Box<Self>, bool), // (inner, mutable)
    /// A dynamically dispatched trait object. It is unsized and only valid
    /// behind a reference or raw pointer.
    DynTrait {
        trait_id: TraitId,
        args: Vec<Self>,
        assoc_bindings: Vec<(String, Self)>,
    },
    /// An owned, sized trait object. The MIR representation stores a heap
    /// data pointer, method table, and type-specific drop function.
    OwnedDynTrait {
        trait_id: TraitId,
        args: Vec<Self>,
        assoc_bindings: Vec<(String, Self)>,
    },
    /// Raw pointer type: `*const T` or `*mut T`.
    Ptr {
        mutable: bool,
        inner: Box<Self>,
    },
    Tuple(Vec<Self>),
    Slice(Box<Self>),
    Array(Box<Self>, ConstArg),
    Struct(StructId, Vec<Self>),
    Enum(EnumId, Vec<Self>),
    Param(String),
    Const(ConstArg),
    FunctionItem {
        function: FunctionId,
        args: Vec<Self>,
    },
    Closure {
        id: ClosureId,
        generics: Vec<String>,
        signature: CallableSignature,
    },
    OpaqueCallable {
        id: OpaqueCallableId,
        signature: CallableSignature,
    },
    OpaqueTrait {
        id: OpaqueCallableId,
        trait_id: TraitId,
        args: Vec<Self>,
    },
    CallableConstraint(CallableSignature),
    InferVar(u32),
    Unknown,
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstArg {
    Value(usize),
    Param(String),
    Unknown,
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClosureKind {
    Fn,
    FnMut,
    FnOnce,
}

impl ClosureKind {
    #[must_use]
    pub const fn accepts(self, actual: Self) -> bool {
        matches!(
            (self, actual),
            (Self::FnOnce, _) | (Self::FnMut, Self::Fn | Self::FnMut) | (Self::Fn, Self::Fn)
        )
    }

    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Fn => "Fn",
            Self::FnMut => "FnMut",
            Self::FnOnce => "FnOnce",
        }
    }
}

impl Type {
    #[must_use]
    pub fn display(&self, hir: &HirFile) -> String {
        match self {
            Self::Int(ty) => ty.as_str().to_string(),
            Self::Float(ty) => ty.as_str().to_string(),
            Self::InferInt => "i32".to_string(),
            Self::InferFloat => "f64".to_string(),
            Self::Bool => "bool".to_string(),
            Self::Str => "str".to_string(),
            Self::Char => "char".to_string(),
            Self::Unit => "()".to_string(),
            Self::Never => "!".to_string(),
            Self::Ref(inner, mutable) => {
                let kw = if *mutable { "&mut " } else { "&" };
                format!("{}{}", kw, inner.display(hir))
            }
            Self::DynTrait {
                trait_id,
                args,
                assoc_bindings,
            } => {
                let name = &hir.item_tree.traits[*trait_id].name.0;
                let mut parts = Vec::new();
                if !args.is_empty() {
                    parts.extend(args.iter().map(|arg| arg.display(hir)).collect::<Vec<_>>());
                }
                parts.extend(
                    assoc_bindings
                        .iter()
                        .map(|(name, ty)| format!("{name} = {}", ty.display(hir))),
                );
                if parts.is_empty() {
                    format!("dyn {name}")
                } else {
                    format!("dyn {name}<{}>", parts.join(", "))
                }
            }
            Self::OwnedDynTrait {
                trait_id,
                args,
                assoc_bindings,
            } => {
                let name = &hir.item_tree.traits[*trait_id].name.0;
                let mut parts = Vec::new();
                if !args.is_empty() {
                    parts.extend(args.iter().map(|arg| arg.display(hir)).collect::<Vec<_>>());
                }
                parts.extend(
                    assoc_bindings
                        .iter()
                        .map(|(name, ty)| format!("{name} = {}", ty.display(hir))),
                );
                if parts.is_empty() {
                    format!("dyn {name}")
                } else {
                    format!("dyn {name}<{}>", parts.join(", "))
                }
            }
            Self::Ptr { mutable, inner } => {
                let kind = if *mutable { "*mut" } else { "*const" };
                format!("{kind} {}", inner.display(hir))
            }
            Self::Tuple(elements) => {
                let inner = elements
                    .iter()
                    .map(|ty| ty.display(hir))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({inner})")
            }
            Self::Slice(inner) => format!("[{}]", inner.display(hir)),
            Self::Array(inner, len) => format!("[{}; {}]", inner.display(hir), len.display()),
            Self::Struct(id, args) => {
                let HirStruct { name, .. } = &hir.item_tree.structs[*id];
                if args.is_empty() {
                    name.0.clone()
                } else {
                    let args = args
                        .iter()
                        .map(|arg| arg.display(hir))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}<{}>", name.0, args)
                }
            }
            Self::Enum(id, args) => {
                let enum_data = &hir.item_tree.enums[*id];
                if args.is_empty() {
                    enum_data.name.0.clone()
                } else {
                    let args = args
                        .iter()
                        .map(|arg| arg.display(hir))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}<{}>", enum_data.name.0, args)
                }
            }
            Self::FunctionItem { function: id, .. } => {
                let function = &hir.item_tree.functions[*id];
                let prefix = if function.is_unsafe { "unsafe " } else { "" };
                format!("{prefix}fun {}", function.name.0)
            }
            Self::Closure { signature, .. } => {
                format_callable_signature(signature, "anonymous ", hir)
            }
            Self::OpaqueCallable { signature, .. } => {
                format_callable_signature(signature, "impl ", hir)
            }
            Self::OpaqueTrait { trait_id, args, .. } => {
                let name = &hir.item_tree.traits[*trait_id].name.0;
                if args.is_empty() {
                    format!("impl {name}")
                } else {
                    let args = args
                        .iter()
                        .map(|arg| arg.display(hir))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("impl {name}<{args}>")
                }
            }
            Self::CallableConstraint(signature) => format_callable_signature(signature, "", hir),
            Self::InferVar(_) | Self::Unknown => "_".to_string(),
            Self::Param(name) => name.clone(),
            Self::Const(value) => value.display(),
            Self::Error => "<error>".to_string(),
        }
    }

    #[must_use]
    pub const fn is_unknown_like(&self) -> bool {
        matches!(self, Self::Unknown | Self::Error | Self::InferVar(_))
    }

    #[must_use]
    pub const fn is_numeric(&self) -> bool {
        matches!(
            self,
            Self::Int(_) | Self::Float(_) | Self::InferInt | Self::InferFloat
        )
    }

    #[must_use]
    pub const fn is_integer(&self) -> bool {
        matches!(self, Self::Int(_) | Self::InferInt)
    }

    #[must_use]
    pub const fn is_bitwise_scalar(&self) -> bool {
        self.is_integer() || matches!(self, Self::Bool)
    }

    #[must_use]
    pub const fn is_ordered_scalar(&self) -> bool {
        self.is_numeric() || matches!(self, Self::Char)
    }

    #[must_use]
    pub const fn is_never(&self) -> bool {
        matches!(self, Self::Never)
    }

    /// Returns `true` if this type has a known size at compile time.
    /// Unsized types (`str` and `[T]`) can only exist behind a pointer/reference.
    pub fn is_sized(&self) -> bool {
        match self {
            Self::Str | Self::Slice(_) | Self::DynTrait { .. } => false,
            Self::Tuple(elements) => elements.iter().all(Self::is_sized),
            Self::Array(inner, _) => inner.is_sized(),
            Self::Struct(_, args) | Self::Enum(_, args) | Self::FunctionItem { args, .. } => {
                args.iter().all(Self::is_sized)
            }
            Self::OpaqueTrait { args, .. } => args.iter().all(Self::is_sized),
            Self::CallableConstraint(signature)
            | Self::Closure { signature, .. }
            | Self::OpaqueCallable { signature, .. } => {
                signature.params.iter().all(Self::is_sized) && signature.ret.is_sized()
            }
            _ => true,
        }
    }

    pub fn is_valid_value_type(&self) -> bool {
        match self {
            Self::Str | Self::Slice(_) | Self::DynTrait { .. } => false,
            Self::Ref(inner, _) | Self::Ptr { inner, .. } => {
                matches!(
                    inner.as_ref(),
                    Self::Str | Self::Slice(_) | Self::DynTrait { .. }
                ) || inner.is_valid_value_type()
            }
            Self::Tuple(elements) => elements.iter().all(Self::is_valid_value_type),
            Self::Array(inner, _) => inner.is_valid_value_type(),
            Self::Struct(_, args) | Self::Enum(_, args) => {
                args.iter().all(Self::is_valid_value_type)
            }
            Self::OpaqueTrait { args, .. } => args.iter().all(Self::is_valid_value_type),
            Self::CallableConstraint(signature)
            | Self::Closure { signature, .. }
            | Self::OpaqueCallable { signature, .. } => {
                signature.params.iter().all(Self::is_valid_value_type)
                    && signature.ret.is_valid_value_type()
            }
            Self::FunctionItem { args, .. } => args.iter().all(Self::is_valid_value_type),
            _ => true,
        }
    }

    /// Compiler-intrinsic `Copy` candidates – types that are `Copy`
    /// regardless of whether a `Copy` trait is defined.
    #[must_use]
    pub const fn is_fundamentally_copy(&self) -> bool {
        matches!(
            self,
            Self::Int(_)
                | Self::Float(_)
                | Self::InferInt
                | Self::InferFloat
                | Self::Bool
                | Self::Char
                | Self::Unit
                | Self::Never
                | Self::Ref(_, false)
                | Self::Ptr { .. }
                | Self::FunctionItem { .. }
                | Self::InferVar(_)
                | Self::Unknown
                | Self::Error
        )
    }

    #[must_use]
    pub const fn closure_kind(&self) -> Option<ClosureKind> {
        match self {
            Self::CallableConstraint(signature) => Some(signature.kind),
            Self::Closure { signature, .. } | Self::OpaqueCallable { signature, .. } => {
                Some(signature.kind)
            }
            Self::FunctionItem { .. } => Some(ClosureKind::Fn),
            _ => None,
        }
    }

    #[must_use]
    pub const fn callable_signature(&self) -> Option<&CallableSignature> {
        match self {
            Self::CallableConstraint(signature)
            | Self::Closure { signature, .. }
            | Self::OpaqueCallable { signature, .. } => Some(signature),
            _ => None,
        }
    }

    #[must_use]
    pub fn or(self, fallback: Self) -> Self {
        if self.is_unknown_like() {
            fallback
        } else {
            self
        }
    }
}

fn format_callable_signature(signature: &CallableSignature, prefix: &str, hir: &HirFile) -> String {
    let params = signature
        .params
        .iter()
        .map(|param| param.display(hir))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "{prefix}{}({params}) -> {}",
        signature.kind.as_str(),
        signature.ret.display(hir)
    )
}

impl ConstArg {
    #[must_use]
    pub fn display(&self) -> String {
        match self {
            Self::Value(value) => value.to_string(),
            Self::Param(name) => name.clone(),
            Self::Unknown => "_".to_string(),
            Self::Error => "<error>".to_string(),
        }
    }

    #[must_use]
    pub const fn as_usize(&self) -> Option<usize> {
        match self {
            Self::Value(value) => Some(*value),
            _ => None,
        }
    }

    #[must_use]
    pub const fn is_unknown_like(&self) -> bool {
        matches!(self, Self::Unknown | Self::Error)
    }
}

impl IntTy {
    #[must_use]
    pub fn parse(text: &str) -> Option<Self> {
        match text {
            "i8" => Some(Self::I8),
            "i16" => Some(Self::I16),
            "i32" => Some(Self::I32),
            "i64" => Some(Self::I64),
            "isize" => Some(Self::Isize),
            "u8" => Some(Self::U8),
            "u16" => Some(Self::U16),
            "u32" => Some(Self::U32),
            "u64" => Some(Self::U64),
            "usize" => Some(Self::Usize),
            _ => None,
        }
    }

    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::I8 => "i8",
            Self::I16 => "i16",
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::Isize => "isize",
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
            Self::Usize => "usize",
        }
    }

    /// True for the signed integer types.
    #[must_use]
    pub const fn is_signed(self) -> bool {
        matches!(
            self,
            Self::I8 | Self::I16 | Self::I32 | Self::I64 | Self::Isize
        )
    }

    /// Width in bits. Only `usize` and `isize` vary with it, and the width that
    /// answers them is the *target's* pointer width: the machine running the
    /// compiler is not necessarily the machine being compiled for. `pointer_bits`
    /// is one of the supported target widths, 32 or 64.
    #[must_use]
    pub const fn width_bits(self, pointer_bits: u32) -> u32 {
        match self {
            Self::Isize | Self::Usize => pointer_bits,
            Self::I8 | Self::U8 => 8,
            Self::I16 | Self::U16 => 16,
            Self::I32 | Self::U32 => 32,
            Self::I64 | Self::U64 => 64,
        }
    }

    /// Largest value the type can hold.
    #[must_use]
    const fn max_value(self, pointer_bits: u32) -> u64 {
        let bits = self.width_bits(pointer_bits);
        if self.is_signed() {
            // The widest signed type is 64 bits, so `bits - 1` never overflows.
            (1u64 << (bits - 1)) - 1
        } else if bits >= 64 {
            u64::MAX
        } else {
            (1u64 << bits) - 1
        }
    }

    /// The inclusive `(min, max)` value range of the type.
    #[must_use]
    pub const fn value_range(self, pointer_bits: u32) -> (i128, i128) {
        let max = self.max_value(pointer_bits) as i128;
        if self.is_signed() {
            (-(max + 1), max)
        } else {
            (0, max)
        }
    }

    #[must_use]
    pub const fn contains_u64(self, value: u64, pointer_bits: u32) -> bool {
        value <= self.max_value(pointer_bits)
    }

    /// True when `-value` is representable, the check a negatively written
    /// literal needs. An unsigned type holds no negative value at all, however
    /// small the magnitude.
    #[must_use]
    pub const fn contains_negative_magnitude(self, value: u64, pointer_bits: u32) -> bool {
        if !self.is_signed() {
            return false;
        }
        value <= self.max_value(pointer_bits) + 1
    }
}

impl FloatTy {
    #[must_use]
    pub fn parse(text: &str) -> Option<Self> {
        match text {
            "f32" => Some(Self::F32),
            "f64" => Some(Self::F64),
            _ => None,
        }
    }

    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::F32 => "f32",
            Self::F64 => "f64",
        }
    }
}
