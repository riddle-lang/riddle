use std::collections::HashMap;

use mir::types::{EnumVariantKind, Type};
use mir::value::FuncRef;

use super::value::Val;

/// Pointer bits layout: high 32 bits are the allocation id, low 32 bits the
/// byte offset inside the allocation.
pub const fn ptr_parts(bits: u64) -> (u32, u32) {
    ((bits >> 32) as u32, bits as u32)
}

/// Packs an allocation id and byte offset back into pointer bits.
pub const fn make_ptr(alloc: u32, offset: u32) -> u64 {
    ((alloc as u64) << 32) | offset as u64
}

/// Size of a type under the interpreter's own byte layout.
///
/// The layout only needs to be self-consistent: every read and write in the
/// interpreter goes through the same offsets, and no native C code observes
/// the bytes. Aggregates are packed without padding, pointers are one word,
/// and unsized pointees (`str`, `[T]`) are represented by the two-word fat
/// pointer struct like the C backend's `riddle_str` / `riddle_slice`.
#[must_use]
pub fn size_of(ty: &Type) -> usize {
    match ty {
        Type::Int(int_ty) => int_size(*int_ty),
        Type::Float(float_ty) => match float_ty {
            mir::types::FloatTy::F32 => 4,
            mir::types::FloatTy::F64 => 8,
        },
        Type::Bool => 1,
        Type::Char => 4,
        Type::Str | Type::Slice(_) => 16,
        Type::Unit | Type::Never | Type::Void => 0,
        Type::Ref(inner, _) | Type::Ptr(inner) => {
            if inner.is_sized() {
                8
            } else {
                16
            }
        }
        Type::FnPtr(_) => 8,
        Type::Tuple(elements) => elements.iter().map(size_of).sum(),
        Type::Array(inner, count) => size_of(inner) * count,
        Type::Struct(strukt) => strukt
            .def()
            .fields
            .iter()
            .map(|(_, field)| size_of(field))
            .sum(),
        Type::Enum(enum_ty) => {
            let payload = enum_ty
                .variants
                .iter()
                .flat_map(|variant| variant_payload_types(variant.kind.clone()))
                .map(|payload_ty| size_of(&payload_ty))
                .sum::<usize>();
            payload + 4
        }
    }
}

/// Byte offset of field `index` inside a struct/tuple/enum pointee.
///
/// Enums flatten to `[tag: u32, payload fields of every variant in order]`,
/// mirroring the lowering's tagged-struct representation.
pub fn field_offset(pointee: &Type, index: usize) -> Option<usize> {
    let fields = field_types(pointee)?;
    if index >= fields.len() {
        return None;
    }
    Some(fields[..index].iter().map(size_of).sum())
}

/// Flattened field types of an aggregate pointee.
#[must_use]
pub fn field_types(pointee: &Type) -> Option<Vec<Type>> {
    match pointee {
        Type::Struct(strukt) => Some(
            strukt
                .def()
                .fields
                .iter()
                .map(|(_, ty)| ty.clone())
                .collect(),
        ),
        Type::Tuple(elements) => Some(elements.clone()),
        Type::Enum(enum_ty) => {
            let mut fields = vec![Type::Int(mir::types::IntTy::U32)];
            for variant in &enum_ty.variants {
                fields.extend(variant_payload_types(variant.kind.clone()));
            }
            Some(fields)
        }
        // Fat pointees are addressed through their pointer field, never
        // through FieldPtr, so they expose no flattened fields here.
        _ => None,
    }
}

fn variant_payload_types(kind: EnumVariantKind) -> Vec<Type> {
    match kind {
        EnumVariantKind::Unit => Vec::new(),
        EnumVariantKind::Tuple(types) => types,
        EnumVariantKind::Struct(fields) => fields.into_iter().map(|(_, ty)| ty).collect(),
    }
}

const fn int_size(ty: mir::types::IntTy) -> usize {
    use mir::types::IntTy;
    match ty {
        IntTy::I8 | IntTy::U8 => 1,
        IntTy::I16 | IntTy::U16 => 2,
        IntTy::I32 | IntTy::U32 => 4,
        IntTy::I64 | IntTy::U64 | IntTy::Isize | IntTy::Usize => 8,
    }
}

/// Byte-backed memory arena plus the interning tables that let pointer,
/// string, and function-pointer identities survive a round trip through
/// memory.
pub struct Memory {
    allocs: Vec<Box<[u8]>>,
    strings: HashMap<String, u64>,
    fnptrs: Vec<FuncRef>,
    fnptr_ids: HashMap<FuncRef, u64>,
}

impl Default for Memory {
    fn default() -> Self {
        Self::new()
    }
}

impl Memory {
    pub fn new() -> Self {
        Self {
            // Allocation id 0 stays unused so no real pointer ever collides
            // with the null pointer bit pattern.
            allocs: vec![Box::<[u8]>::default()],
            strings: HashMap::new(),
            // Id 0 is reserved as the null function pointer.
            fnptrs: vec![FuncRef::Extern(String::new())],
            fnptr_ids: HashMap::new(),
        }
    }

    /// Allocates a zeroed block and returns its base pointer bits.
    pub fn alloc(&mut self, size: usize) -> u64 {
        let id = u32::try_from(self.allocs.len()).expect("allocation count overflow");
        self.allocs.push(vec![0; size].into_boxed_slice());
        make_ptr(id, 0)
    }

    /// Allocates a block filled with `bytes`.
    pub fn alloc_bytes(&mut self, bytes: &[u8]) -> u64 {
        let id = u32::try_from(self.allocs.len()).expect("allocation count overflow");
        self.allocs.push(bytes.to_vec().into_boxed_slice());
        make_ptr(id, 0)
    }

    /// Returns a stable pointer to a copy of `text`'s bytes, interning
    /// identical content like the C backend's static string literals.
    pub fn intern_str(&mut self, text: &str) -> u64 {
        if let Some(ptr) = self.strings.get(text) {
            return *ptr;
        }
        let ptr = self.alloc_bytes(text.as_bytes());
        self.strings.insert(text.to_string(), ptr);
        ptr
    }

    /// Interns a function reference so it can be stored as 8 bytes.
    pub fn intern_fnptr(&mut self, func: &FuncRef) -> u64 {
        if let Some(id) = self.fnptr_ids.get(func) {
            return *id;
        }
        let id = self.fnptrs.len() as u64;
        self.fnptrs.push(func.clone());
        self.fnptr_ids.insert(func.clone(), id);
        id
    }

    /// Looks up an interned function reference by id.
    #[must_use]
    pub fn fnptr_by_id(&self, id: u64) -> Option<&FuncRef> {
        self.fnptrs.get(id as usize)
    }

    /// Size in bytes of an existing allocation.
    #[must_use]
    pub fn allocation_len(&self, alloc: usize) -> usize {
        self.allocs.get(alloc).map_or(0, |bytes| bytes.len())
    }

    fn bounds(&self, bits: u64, len: usize) -> Result<(usize, usize), String> {
        if bits == 0 {
            return Err("null pointer dereference".into());
        }
        let (alloc, offset) = ptr_parts(bits);
        let offset = offset as usize;
        let bytes = self
            .allocs
            .get(alloc as usize)
            .ok_or_else(|| format!("dangling pointer to allocation {alloc}"))?;
        if offset + len > bytes.len() {
            return Err(format!(
                "memory access of {len} bytes at offset {offset} escapes allocation {alloc} of {} bytes",
                bytes.len()
            ));
        }
        Ok((alloc as usize, offset))
    }

    /// Reads `len` bytes at `ptr`.
    pub fn read_bytes(&self, ptr: u64, len: usize) -> Result<&[u8], String> {
        let (alloc, offset) = self.bounds(ptr, len)?;
        Ok(&self.allocs[alloc][offset..offset + len])
    }

    /// Writes `bytes` at `ptr`.
    pub fn write_bytes(&mut self, ptr: u64, bytes: &[u8]) -> Result<(), String> {
        let (alloc, offset) = self.bounds(ptr, bytes.len())?;
        self.allocs[alloc][offset..offset + bytes.len()].copy_from_slice(bytes);
        Ok(())
    }

    fn read_scalar(&self, ptr: u64, len: usize) -> Result<u64, String> {
        let bytes = self.read_bytes(ptr, len)?;
        Ok(u64::from_le_bytes(
            bytes
                .iter()
                .chain(std::iter::repeat(&0))
                .take(8)
                .copied()
                .collect::<Vec<u8>>()
                .try_into()
                .unwrap_or([0; 8]),
        ))
    }

    fn write_scalar(&mut self, ptr: u64, len: usize, bits: u64) -> Result<(), String> {
        self.write_bytes(ptr, &bits.to_le_bytes()[..len])
    }

    /// Loads a value of static type `ty` located at `ptr`.
    pub fn read_val(&self, ptr: u64, ty: &Type) -> Result<Val, String> {
        match ty {
            Type::Int(_) => Ok(Val::Int(self.read_scalar(ptr, size_of(ty))?)),
            Type::Float(float_ty) => {
                let bits = self.read_scalar(ptr, size_of(ty))?;
                Ok(Val::Float(match float_ty {
                    mir::types::FloatTy::F32 => f32::from_bits(bits as u32) as f64,
                    mir::types::FloatTy::F64 => f64::from_bits(bits),
                }))
            }
            Type::Bool => Ok(Val::Bool(self.read_scalar(ptr, 1)? != 0)),
            Type::Char => {
                let bits = u32::try_from(self.read_scalar(ptr, 4)?).unwrap_or(0);
                Ok(Val::Char(char::from_u32(bits).unwrap_or('\0')))
            }
            Type::Ptr(_) => Ok(Val::Ptr(self.read_scalar(ptr, 8)?)),
            Type::Ref(inner, _) if inner.is_sized() => Ok(Val::Ptr(self.read_scalar(ptr, 8)?)),
            Type::Ref(_, _) => Ok(Val::Fat(
                self.read_scalar(ptr, 8)?,
                self.read_scalar(ptr + 8, 8)?,
            )),
            Type::Str | Type::Slice(_) => Ok(Val::Fat(
                self.read_scalar(ptr, 8)?,
                self.read_scalar(ptr + 8, 8)?,
            )),
            Type::FnPtr(_) => {
                let id = self.read_scalar(ptr, 8)?;
                let func = self
                    .fnptr_by_id(id)
                    .cloned()
                    .unwrap_or_else(|| FuncRef::Extern(String::new()));
                Ok(Val::FnPtr(func))
            }
            Type::Struct(_) | Type::Enum(_) | Type::Tuple(_) => {
                let fields = field_types(ty).unwrap_or_default();
                let mut offset = 0u64;
                let mut values = Vec::with_capacity(fields.len());
                for field_ty in fields {
                    values.push(self.read_val(ptr + offset, &field_ty)?);
                    offset += size_of(&field_ty) as u64;
                }
                Ok(Val::Struct(values))
            }
            Type::Array(inner, count) => {
                let stride = size_of(inner);
                let mut values = Vec::with_capacity(*count);
                for index in 0..*count {
                    values.push(self.read_val(ptr + (index * stride) as u64, inner)?);
                }
                Ok(Val::Array(values.into()))
            }
            Type::Unit | Type::Never | Type::Void => Ok(Val::Unit),
        }
    }

    /// Stores `val` with static type `ty` at `ptr`.
    pub fn write_val(&mut self, ptr: u64, ty: &Type, val: &Val) -> Result<(), String> {
        match (ty, val) {
            (Type::Int(_), Val::Int(bits)) => self.write_scalar(ptr, size_of(ty), *bits),
            (Type::Float(float_ty), Val::Float(value)) => {
                let bits = match float_ty {
                    mir::types::FloatTy::F32 => (*value as f32).to_bits() as u64,
                    mir::types::FloatTy::F64 => value.to_bits(),
                };
                self.write_scalar(ptr, size_of(ty), bits)
            }
            (Type::Bool, Val::Bool(value)) => self.write_scalar(ptr, 1, *value as u64),
            (Type::Char, Val::Char(value)) => self.write_scalar(ptr, 4, u32::from(*value) as u64),
            (Type::Ptr(_), Val::Ptr(bits)) => self.write_scalar(ptr, 8, *bits),
            (Type::Ref(inner, _), Val::Ptr(bits)) if inner.is_sized() => {
                self.write_scalar(ptr, 8, *bits)
            }
            (Type::Ref(_, _), Val::Fat(ptr_bits, len))
            | (Type::Str | Type::Slice(_), Val::Fat(ptr_bits, len)) => {
                self.write_scalar(ptr, 8, *ptr_bits)?;
                self.write_scalar(ptr + 8, 8, *len)
            }
            // Fat values may also be produced from owned str registers when
            // a `&str` is stored; materialize the bytes first.
            (Type::Ref(_, _), Val::Str(text)) | (Type::Str, Val::Str(text)) => {
                let bytes = self.intern_str(text);
                self.write_scalar(ptr, 8, bytes)?;
                self.write_scalar(ptr + 8, 8, text.len() as u64)
            }
            (Type::FnPtr(_), Val::FnPtr(func)) => {
                let id = self.intern_fnptr(func);
                self.write_scalar(ptr, 8, id)
            }
            (Type::FnPtr(_), Val::Int(bits)) => self.write_scalar(ptr, 8, *bits),
            (Type::Struct(_) | Type::Enum(_) | Type::Tuple(_), Val::Struct(fields)) => {
                let layout = field_types(ty).unwrap_or_default();
                if fields.len() > layout.len() {
                    return Err(format!(
                        "aggregate of type {ty:?} stored with {} fields but layout has {}",
                        fields.len(),
                        layout.len()
                    ));
                }
                let mut offset = 0u64;
                for (index, field_ty) in layout.iter().enumerate() {
                    let placeholder = zero_val(field_ty);
                    let field = fields.get(index).unwrap_or(&placeholder);
                    self.write_val(ptr + offset, field_ty, field)?;
                    offset += size_of(field_ty) as u64;
                }
                Ok(())
            }
            (Type::Array(inner, count), Val::Array(elements)) => {
                let stride = size_of(inner);
                if elements.len() != *count {
                    return Err(format!(
                        "array of {count} elements stored with {}",
                        elements.len()
                    ));
                }
                for (index, element) in elements.iter().enumerate() {
                    self.write_val(ptr + (index * stride) as u64, inner, element)?;
                }
                Ok(())
            }
            (Type::Unit | Type::Never | Type::Void, Val::Unit) => Ok(()),
            (ty, val) => Err(format!("cannot store {val:?} as {ty:?}")),
        }
    }
}

/// The value an uninitialized (zeroed) slot of type `ty` reads back as.
#[must_use]
pub fn zero_val(ty: &Type) -> Val {
    match ty {
        Type::Int(_) => Val::Int(0),
        Type::Float(_) => Val::Float(0.0),
        Type::Bool => Val::Bool(false),
        Type::Char => Val::Char('\0'),
        Type::Ptr(_) => Val::Ptr(0),
        Type::Ref(inner, _) if inner.is_sized() => Val::Ptr(0),
        Type::Ref(_, _) => Val::Fat(0, 0),
        Type::Str | Type::Slice(_) => Val::Fat(0, 0),
        Type::FnPtr(_) => Val::FnPtr(FuncRef::Extern(String::new())),
        Type::Struct(_) | Type::Enum(_) | Type::Tuple(_) => Val::Struct(
            field_types(ty)
                .unwrap_or_default()
                .iter()
                .map(zero_val)
                .collect(),
        ),
        Type::Array(inner, count) => Val::Array(std::rc::Rc::new(
            (0..*count).map(|_| zero_val(inner)).collect(),
        )),
        Type::Unit | Type::Never | Type::Void => Val::Unit,
    }
}
