use std::fmt::{self, Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Weak};

/// MIR type system — a flattened representation of Riddle types,
/// oriented toward code generation rather than type checking.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    // 基本标量类型
    Int(IntTy),
    Float(FloatTy),
    Bool,
    Str,
    Char,
    Unit,
    Never,

    // 复合类型
    Ref(Box<Self>, bool), // (inner, mutable)
    Ptr(Box<Self>),
    Tuple(Vec<Self>),
    Slice(Box<Self>),
    Array(Box<Self>, usize),
    Struct(StructType),
    Enum(EnumType),

    // 函数指针
    FnPtr(FnPtrType),

    /// No type (used for instructions that don't produce a value).
    Void,
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

impl IntTy {
    #[must_use]
    pub const fn is_signed(self) -> bool {
        matches!(
            self,
            Self::I8 | Self::I16 | Self::I32 | Self::I64 | Self::Isize
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FloatTy {
    F32,
    F64,
}

/// A nominal struct type.
///
/// Field types are expanded eagerly, but a struct may reach itself through a
/// reference — `struct List { next: Option<&List> }` — which an eager tree
/// cannot represent, because the expansion has no end. The definition is
/// therefore shared behind an [`Arc`], and the occurrence inside its own fields
/// is a [`Weak`] back-reference to that same definition. Reading the fields of a
/// recursive occurrence reaches them through the definition that owns it, so a
/// type stays finite without giving up its layout.
#[derive(Clone)]
pub enum StructType {
    /// A complete definition.
    Defined(Arc<StructDef>),
    /// The definition currently being built, reached from inside its own
    /// fields. `name` and `symbol` are kept here so identity and diagnostics
    /// never depend on resolving the definition.
    Recursive {
        name: String,
        symbol: String,
        def: Weak<StructDef>,
    },
}

/// The field list of a struct. Enums are lowered to a tagged struct, so they
/// use this representation too.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    pub name: String,
    pub symbol: String,
    pub fields: Vec<(String, Type)>,
}

impl StructType {
    /// A definition that nothing refers back to.
    #[must_use]
    pub fn defined(name: String, symbol: String, fields: Vec<(String, Type)>) -> Self {
        Self::Defined(Arc::new(StructDef {
            name,
            symbol,
            fields,
        }))
    }

    /// A definition its own fields can refer back to: `build` receives the
    /// handle for the definition being created and returns its fields.
    #[must_use]
    pub fn new_cyclic<F>(build: F) -> Self
    where
        F: FnOnce(Weak<StructDef>) -> StructDef,
    {
        Self::Defined(Arc::new_cyclic(|def| build(def.clone())))
    }

    /// The recursive occurrence of a definition that is still being built.
    #[must_use]
    pub fn recursive(def: Weak<StructDef>, name: String, symbol: String) -> Self {
        Self::Recursive { name, symbol, def }
    }

    /// The shared definition, which carries the field list.
    ///
    /// A recursive occurrence resolves through the definition that owns it — and
    /// keeps it alive for as long as the returned handle is — so callers read
    /// fields from an owned handle rather than borrowing through the `Weak`.
    #[must_use]
    pub fn def(&self) -> Arc<StructDef> {
        match self {
            Self::Defined(def) => Arc::clone(def),
            Self::Recursive { def, symbol, .. } => def.upgrade().unwrap_or_else(|| {
                panic!("internal error: recursive struct `{symbol}` outlived its definition")
            }),
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Defined(def) => &def.name,
            Self::Recursive { name, .. } => name,
        }
    }

    #[must_use]
    pub fn symbol(&self) -> &str {
        match self {
            Self::Defined(def) => &def.symbol,
            Self::Recursive { symbol, .. } => symbol,
        }
    }

    /// Whether this handle is the back-reference inside a definition's own
    /// fields.
    #[must_use]
    pub const fn is_recursive_occurrence(&self) -> bool {
        matches!(self, Self::Recursive { .. })
    }
}

impl PartialEq for StructType {
    /// Nominal identity: two handles denote the same struct exactly when their
    /// symbols match. Symbols encode the item and its type arguments, and
    /// comparing fields instead would not terminate on a recursive struct.
    fn eq(&self, other: &Self) -> bool {
        self.symbol() == other.symbol()
    }
}

impl Eq for StructType {}

impl Hash for StructType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.symbol().hash(state);
    }
}

impl Debug for StructType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Defined(def) => f
                .debug_struct("StructType")
                .field("name", &def.name)
                .field("symbol", &def.symbol)
                .field("fields", &def.fields)
                .finish(),
            // Printing the target would recurse forever.
            Self::Recursive { name, .. } => write!(f, "StructType({name}, recursive)"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumType {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EnumVariantKind {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<(String, Type)>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub name: String,
    pub discriminant: u32,
    pub kind: EnumVariantKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnPtrType {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

impl Type {
    /// Returns true if the type fits in a machine register.
    #[must_use]
    pub const fn is_scalar(&self) -> bool {
        matches!(
            self,
            Self::Int(_)
                | Self::Float(_)
                | Self::Bool
                | Self::Char
                | Self::Ptr(_)
                | Self::Ref(_, _)
                | Self::FnPtr(_)
        )
    }

    /// Returns `true` if this type has a known size at compile time.
    /// Unsized types (`str` and `[T]`) can only exist behind a pointer/reference.
    #[must_use]
    pub const fn is_sized(&self) -> bool {
        !matches!(self, Self::Str | Self::Slice(_))
    }
}
