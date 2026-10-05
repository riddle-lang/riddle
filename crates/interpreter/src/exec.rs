use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use la_arena::{Idx, RawIdx};
use mir::func::Function;
use mir::instr::{BinOp, CastOp, CmpOp, ConstValue, Inst, InstKind, Terminator, UnOp};
use mir::module::Module;
use mir::source_map::SourceFile;
use mir::types::{FloatTy, IntTy, Type};
use mir::value::{BlockId, FuncRef, Value};

use super::externs::Stream;
use super::mem::{Memory, field_offset, field_types, size_of};
use super::value::Val;

/// Why interpretation stopped abnormally.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Trap {
    /// A user `panic!` — rendered like the C runtime's `riddle_panic`.
    Panic {
        message: String,
        file: String,
        line: u32,
        column: u32,
    },
    /// A runtime abort with the C backend's `riddle: <message>` output
    /// (division by zero, out-of-bounds index, plain `abort()`).
    Abort { message: String },
    /// `std::process::exit(code)`.
    ProcessExit(i32),
    /// An `extern "C"` function the interpreter does not provide.
    UnsupportedExtern { name: String },
    /// A `unreachable` terminator executed.
    Unreachable { function: String },
    /// Recursion exceeded the configured depth limit.
    StackOverflow { function: String },
    /// An interpreter-level failure (invalid memory access, malformed MIR).
    /// Indicates an interpreter bug or unsafe-pointer misuse.
    Internal(String),
}

impl Trap {
    /// Writes what the compiled C program would print before abort, plus
    /// diagnostic lines for interpreter-level traps (stack overflow,
    /// internal errors) that have no compiled counterpart.
    pub fn render(&self, stderr: &mut Vec<u8>) {
        match self {
            Self::Panic {
                message,
                file,
                line,
                column,
            } => {
                stderr.extend_from_slice(
                    format!("thread 'main' panicked at {file}:{line}:{column}:\n").as_bytes(),
                );
                if !message.is_empty() {
                    stderr.extend_from_slice(message.as_bytes());
                }
                stderr.push(b'\n');
            }
            Self::Abort { message } if !message.is_empty() => {
                stderr.extend_from_slice(format!("riddle: {message}\n").as_bytes());
            }
            Self::Abort { .. } => {}
            Self::UnsupportedExtern { name } => {
                stderr.extend_from_slice(
                    format!("riddle: interpreter does not support extern `{name}`\n").as_bytes(),
                );
            }
            // Interpreter-level traps have no compiled counterpart, so there
            // is no C-runtime output to mirror; say something instead of
            // exiting silently.
            Self::StackOverflow { function } => {
                stderr.extend_from_slice(
                    format!("riddle: stack overflow in `{function}` (recursion limit exceeded)\n")
                        .as_bytes(),
                );
            }
            Self::Internal(message) => {
                stderr.extend_from_slice(
                    format!("riddle: interpreter internal error: {message}\n").as_bytes(),
                );
            }
            Self::Unreachable { .. } | Self::ProcessExit(_) => {}
        }
    }

    /// Whether this trap leaves the process via `abort()` in compiled code.
    #[must_use]
    pub const fn is_abort(&self) -> bool {
        matches!(
            self,
            Self::Panic { .. } | Self::Abort { .. } | Self::Unreachable { .. }
        )
    }
}

pub(crate) struct Interpreter {
    /// Functions keyed by `Idx<Function>` (assigned in load order). `Rc` slots
    /// let each activation hold an owned handle and borrow its instruction
    /// stream instead of cloning blocks on every basic-block step.
    functions: Vec<Rc<Function>>,
    /// Function name → arena index.
    index: HashMap<String, Idx<Function>>,
    /// Static type of every `Value` per function.
    types: HashMap<Idx<Function>, Rc<Vec<Type>>>,
    pub(crate) mem: Memory,
    source_files: Vec<SourceFile>,
    module_name: String,
    pub(crate) args: Vec<String>,
    pub(crate) stdout: Vec<u8>,
    pub(crate) stderr: Vec<u8>,
    pub(crate) rng: u64,
    pub(crate) handles: HashMap<u64, Stream>,
    /// Handles whose last read failed, backing the C runtime's `ferror`
    /// indicator so `std::fs` can separate a failed read from end of stream.
    pub(crate) read_failed: HashSet<u64>,
    next_handle: u64,
    depth: usize,
    max_depth: usize,
}

/// One activation frame: register file indexed by `Value` number, plus
/// lazily materialized "home" allocations for register aggregates whose
/// address gets taken (each MIR value maps to one stable address, like a C
/// local).
struct Frame {
    regs: Vec<Val>,
    homes: HashMap<u32, u64>,
    // Arena blocks owned by this activation: allocas and homes, released when
    // the frame returns.
    slots: Vec<u32>,
}

impl Interpreter {
    pub fn new(
        module: &Module,
        source_files: Vec<SourceFile>,
        args: Vec<String>,
        rng_seed: u64,
        max_depth: usize,
    ) -> Self {
        let mut functions = Vec::with_capacity(module.function_order.len());
        let mut index = HashMap::new();
        let mut types = HashMap::new();
        for fid in &module.function_order {
            let function = Rc::new(module.functions[*fid].clone());
            let function_types = Rc::new(value_types(&function));
            let new_id = Idx::from_raw(RawIdx::from_u32(functions.len() as u32));
            functions.push(function);
            index.insert(module.functions[*fid].name.clone(), new_id);
            types.insert(new_id, function_types);
        }
        Self {
            functions,
            index,
            types,
            mem: Memory::new(),
            source_files,
            module_name: module.name.clone(),
            args,
            stdout: Vec::new(),
            stderr: Vec::new(),
            rng: next_seed(rng_seed),
            handles: HashMap::new(),
            read_failed: HashSet::new(),
            next_handle: 1,
            depth: 0,
            max_depth,
        }
    }

    /// Allocates a fresh host stream handle.
    pub(crate) fn new_handle(&mut self, stream: Stream) -> u64 {
        let handle = self.next_handle;
        self.next_handle += 1;
        self.handles.insert(handle, stream);
        handle
    }

    /// Runs `main`, returning its exit code.
    pub fn run_main(&mut self) -> Result<i32, Trap> {
        let result = self.call("main", Vec::new())?;
        match result {
            Val::Int(bits) => Ok(bits as u32 as i32),
            _ => Ok(0),
        }
    }

    /// Calls a function by name with argument values.
    pub fn call(&mut self, name: &str, args: Vec<Val>) -> Result<Val, Trap> {
        let fid = self
            .index
            .get(name)
            .copied()
            .ok_or_else(|| Trap::Internal(format!("call to unknown function `{name}`")))?;
        self.call_idx(fid, args)
    }

    /// Shared handle to a function by arena-style index.
    fn function(&self, fid: Idx<Function>) -> &Rc<Function> {
        &self.functions[u32::from(fid.into_raw()) as usize]
    }

    /// Return type of a function, if present.
    #[must_use]
    pub fn return_type(&self, name: &str) -> Option<Type> {
        self.index
            .get(name)
            .map(|fid| self.function(*fid).ret_type.clone())
    }

    fn call_idx(&mut self, fid: Idx<Function>, args: Vec<Val>) -> Result<Val, Trap> {
        if self.depth >= self.max_depth {
            let name = self.function(fid).name.clone();
            return Err(Trap::StackOverflow { function: name });
        }
        self.depth += 1;
        let result = self.execute(fid, args);
        self.depth -= 1;
        result
    }

    fn execute(&mut self, fid: Idx<Function>, args: Vec<Val>) -> Result<Val, Trap> {
        let types = self.types[&fid].clone();
        // An owned handle per activation: blocks and instructions are then
        // borrowed from `function` (not `self`), so evaluating an instruction
        // — which needs `&mut self` for memory — never clones the instruction
        // stream the way a direct `self.functions[fid]` borrow would force.
        let function = self.function(fid).clone();
        let next_value = function.next_value as usize;
        let mut frame = Frame {
            regs: vec![Val::Unit; next_value],
            homes: HashMap::new(),
            slots: Vec::new(),
        };

        let result = self.run_frame(fid, &types, &function, &mut frame, args);
        // Storage for this activation's locals goes back to the arena on every
        // exit, including traps. Promoted heap blocks and interned text are not
        // listed in `slots`, so they keep living past the return.
        self.mem.release_slots(&frame.slots);
        result
    }

    fn run_frame(
        &mut self,
        fid: Idx<Function>,
        types: &[Type],
        function: &Function,
        frame: &mut Frame,
        args: Vec<Val>,
    ) -> Result<Val, Trap> {
        for (param, arg) in function.params.iter().zip(args) {
            frame.regs[param.value.0 as usize] = arg;
        }

        let mut block = function.entry;
        loop {
            let (insts, terminator, start_value) = {
                let b = &function.blocks[block];
                (&b.insts, &b.terminator, b.start_value)
            };
            for (offset, inst) in insts.iter().enumerate() {
                if matches!(inst.kind, InstKind::Phi(_)) {
                    continue;
                }
                let value = Value(start_value + offset as u32);
                let val = self
                    .eval_inst(types, frame, inst)
                    .map_err(|trap| match trap {
                        Trap::Internal(message) => Trap::Internal(format!(
                            "`{}` executing {} ({message})",
                            function.name,
                            display_inst(inst)
                        )),
                        other => other,
                    })?;
                frame.regs[value.0 as usize] = val;
            }
            match terminator {
                Terminator::Pending => {
                    return Err(Trap::Internal(format!(
                        "block in `{}` left a pending terminator",
                        function.name
                    )));
                }
                Terminator::Branch(target) => {
                    let target = *target;
                    self.transfer_phis(frame, fid, block, target);
                    block = target;
                }
                Terminator::CondBranch(cond, then_block, else_block) => {
                    let decided = match &frame.regs[cond.0 as usize] {
                        Val::Bool(value) => *value,
                        Val::Int(bits) => *bits != 0,
                        other => {
                            return Err(Trap::Internal(format!("condition branched on {other:?}")));
                        }
                    };
                    let target = if decided { *then_block } else { *else_block };
                    self.transfer_phis(frame, fid, block, target);
                    block = target;
                }
                Terminator::Return(value) => {
                    return Ok(value
                        .as_ref()
                        .map_or(Val::Unit, |v| frame.regs[v.0 as usize].clone()));
                }
                Terminator::Unreachable => {
                    return Err(Trap::Unreachable {
                        function: function.name.clone(),
                    });
                }
            }
        }
    }

    /// Copies phi inputs across the edge `from → target`, matching the C
    /// backend's per-edge phi assignments.
    fn transfer_phis(&self, frame: &mut Frame, fid: Idx<Function>, from: BlockId, target: BlockId) {
        let block = &self.function(fid).blocks[target];
        let start = block.start_value;
        for (offset, inst) in block.insts.iter().enumerate() {
            let InstKind::Phi(entries) = &inst.kind else {
                // Phis only lead a block; stop at the first real inst.
                break;
            };
            let Some((input, _)) = entries.iter().find(|(_, source)| *source == from) else {
                continue;
            };
            let value = Value(start + offset as u32);
            frame.regs[value.0 as usize] = frame.regs[input.0 as usize].clone();
        }
    }

    fn ty_of(&self, types: &[Type], value: Value) -> Type {
        types.get(value.0 as usize).cloned().unwrap_or(Type::Void)
    }

    /// Materializes a register value into memory once, returning its stable
    /// address (the interpreter analogue of `&` on a C local).
    fn home_of(&mut self, frame: &mut Frame, value: Value, ty: &Type) -> Result<u64, Trap> {
        if let Some(ptr) = frame.homes.get(&value.0) {
            return Ok(*ptr);
        }
        let (slot, ptr) = self.mem.alloc_slot(size_of(ty));
        frame.slots.push(slot);
        let val = frame.regs[value.0 as usize].clone();
        self.mem.write_val(ptr, ty, &val).map_err(Trap::Internal)?;
        frame.homes.insert(value.0, ptr);
        Ok(ptr)
    }

    fn eval_inst(&mut self, types: &[Type], frame: &mut Frame, inst: &Inst) -> Result<Val, Trap> {
        match &inst.kind {
            InstKind::Const(constant) => Ok(self.const_val(&inst.ty, constant)),
            InstKind::BinOp(op, lhs, rhs) => {
                let a = frame.regs[lhs.0 as usize].clone();
                let b = frame.regs[rhs.0 as usize].clone();
                let operand_ty = self.ty_of(types, *lhs);
                self.binop(*op, &operand_ty, &a, &b)
            }
            InstKind::UnOp(op, operand) => {
                let a = frame.regs[operand.0 as usize].clone();
                // `Ref` on a register aggregate materializes the OPERAND's
                // home — passing the result number would read the not-yet-
                // written destination register (Unit) instead.
                self.unop(
                    *op,
                    &inst.ty,
                    &self.ty_of(types, *operand),
                    a,
                    frame,
                    *operand,
                )
            }
            InstKind::Cmp(op, lhs, rhs) => {
                let a = frame.regs[lhs.0 as usize].clone();
                let b = frame.regs[rhs.0 as usize].clone();
                let operand_ty = self.ty_of(types, *lhs);
                self.cmp(*op, &operand_ty, &a, &b)
            }
            InstKind::Cast(op, operand, target) => {
                let a = frame.regs[operand.0 as usize].clone();
                self.cast(*op, &self.ty_of(types, *operand), target, a)
            }
            InstKind::SizeOf(ty) => Ok(Val::Int(size_of(ty) as u64)),
            InstKind::Alloca(ty) => {
                // The instruction type is the pointer; storage sized by the
                // pointee, like the C backend's `sizeof(pointee)`.
                let pointee_size = pointee(ty).map_or_else(|| size_of(ty), size_of);
                let (slot, ptr) = self.mem.alloc_slot(pointee_size);
                frame.slots.push(slot);
                Ok(Val::Ptr(ptr))
            }
            InstKind::HeapAlloc(ty) => {
                let pointee_size = pointee(ty).map_or_else(|| size_of(ty), size_of);
                Ok(Val::Ptr(self.mem.heap_alloc(pointee_size)))
            }
            InstKind::HeapFree(ptr) => {
                let bits = self.expect_ptr(&frame.regs[ptr.0 as usize])?;
                self.mem.free(bits);
                Ok(Val::Unit)
            }
            InstKind::Load(ptr) => {
                let bits = self.expect_ptr(&frame.regs[ptr.0 as usize])?;
                self.mem.read_val(bits, &inst.ty).map_err(Trap::Internal)
            }
            InstKind::Store(val, ptr) => {
                let bits = self.expect_ptr(&frame.regs[ptr.0 as usize])?;
                let stored = frame.regs[val.0 as usize].clone();
                let ty = self.ty_of(types, *val);
                self.mem
                    .write_val(bits, &ty, &stored)
                    .map_err(Trap::Internal)?;
                Ok(Val::Unit)
            }
            InstKind::FieldPtr(base, index) => {
                let base_ty = self.ty_of(types, *base);
                let base_val = frame.regs[base.0 as usize].clone();
                let bits = self.address_of(frame, *base, &base_ty, base_val)?;
                let pointee = pointee(&base_ty).ok_or_else(|| {
                    Trap::Internal(format!("field_ptr on non-pointer type {base_ty:?}"))
                })?;
                let offset = field_offset(pointee, *index).ok_or_else(|| {
                    Trap::Internal(format!(
                        "field_ptr index {index} out of bounds for {pointee:?}"
                    ))
                })?;
                Ok(Val::Ptr(bits + offset as u64))
            }
            InstKind::IndexPtr(base, index) => {
                let base_ty = self.ty_of(types, *base);
                let index_bits = self.expect_int(&frame.regs[index.0 as usize])?;
                self.index_ptr(frame, *base, &base_ty, index_bits)
            }
            InstKind::CheckedIndexPtr(base, index, len) => {
                let index_bits = self.expect_int(&frame.regs[index.0 as usize])?;
                let len_bits = self.expect_int(&frame.regs[len.0 as usize])?;
                if index_bits >= len_bits {
                    return Err(Trap::Abort {
                        message: "index out of bounds".into(),
                    });
                }
                let base_ty = self.ty_of(types, *base);
                self.index_ptr(frame, *base, &base_ty, index_bits)
            }
            InstKind::ExtractValue(aggregate, index) => {
                let val = frame.regs[aggregate.0 as usize].clone();
                let agg_ty = self.ty_of(types, *aggregate);
                self.extract(val, &agg_ty, *index)
            }
            InstKind::Call(callee, args) => {
                let vals = args
                    .iter()
                    .map(|arg| frame.regs[arg.0 as usize].clone())
                    .collect();
                match callee {
                    FuncRef::Local(name) => self.call(name, vals),
                    FuncRef::Extern(name) => {
                        super::externs::call_extern(self, name, &vals, &inst.ty)
                    }
                    FuncRef::Intrinsic(name) => Err(Trap::Internal(format!(
                        "call to intrinsic `{name}` (never produced by lowering)"
                    ))),
                }
            }
            InstKind::Panic(message, site) => {
                let message = frame.regs[message.0 as usize]
                    .str_content(&self.mem)
                    .unwrap_or_default();
                let fallback = (self.module_name.clone(), site.line, site.column);
                let (file, line, column) =
                    SourceFile::resolve(&self.source_files, site.offset as usize)
                        .unwrap_or(fallback);
                Err(Trap::Panic {
                    message,
                    file,
                    line,
                    column,
                })
            }
            InstKind::FunctionRef(function) => Ok(Val::FnPtr(function.clone())),
            InstKind::CallIndirect(callee, args) => {
                let target = frame.regs[callee.0 as usize].clone();
                let vals = args
                    .iter()
                    .map(|arg| frame.regs[arg.0 as usize].clone())
                    .collect();
                match target {
                    Val::FnPtr(FuncRef::Local(name)) => self.call(&name, vals),
                    Val::FnPtr(FuncRef::Extern(name)) => {
                        super::externs::call_extern(self, &name, &vals, &inst.ty)
                    }
                    Val::FnPtr(FuncRef::Intrinsic(name)) => Err(Trap::Internal(format!(
                        "call to intrinsic `{name}` (never produced by lowering)"
                    ))),
                    other => Err(Trap::Internal(format!(
                        "indirect call on non-function value {other:?}"
                    ))),
                }
            }
            InstKind::StructValue(fields) => {
                if is_fat(&inst.ty) {
                    let [ptr, len] = fields.as_slice() else {
                        return Err(Trap::Internal(
                            "fat pointer construction needs a pointer and a length".into(),
                        ));
                    };
                    let ptr_bits = match &frame.regs[ptr.0 as usize] {
                        Val::Ptr(bits) => *bits,
                        Val::Str(text) => self.mem.intern_str(text),
                        Val::Fat(bits, _) => *bits,
                        other => {
                            return Err(Trap::Internal(format!(
                                "fat pointer built from {other:?}"
                            )));
                        }
                    };
                    let len_bits = self.expect_int(&frame.regs[len.0 as usize])?;
                    return Ok(Val::Fat(ptr_bits, len_bits));
                }
                Ok(Val::Struct(
                    fields
                        .iter()
                        .map(|field| frame.regs[field.0 as usize].clone())
                        .collect(),
                ))
            }
            InstKind::SparseStructValue(entries) => {
                let fields = field_types(&inst.ty).unwrap_or_default();
                let mut values: Vec<Val> = fields.iter().map(super::mem::zero_val).collect();
                for (index, source) in entries {
                    let Some(slot) = values.get_mut(*index) else {
                        return Err(Trap::Internal(format!(
                            "sparse struct field {index} out of bounds"
                        )));
                    };
                    *slot = frame.regs[source.0 as usize].clone();
                }
                Ok(Val::Struct(values))
            }
            InstKind::ArrayValue(elements) => Ok(Val::Array(
                elements
                    .iter()
                    .map(|element| frame.regs[element.0 as usize].clone())
                    .collect::<Vec<_>>()
                    .into(),
            )),
            InstKind::TupleValue(elements) => Ok(Val::Struct(
                elements
                    .iter()
                    .map(|element| frame.regs[element.0 as usize].clone())
                    .collect(),
            )),
            InstKind::Phi(_) => Ok(Val::Unit),
        }
    }

    fn index_ptr(
        &mut self,
        frame: &mut Frame,
        base: Value,
        base_ty: &Type,
        index: u64,
    ) -> Result<Val, Trap> {
        let base_val = frame.regs[base.0 as usize].clone();
        match base_val {
            Val::Ptr(bits) => {
                let pointee = pointee(base_ty).ok_or_else(|| {
                    Trap::Internal(format!("index_ptr on non-pointer type {base_ty:?}"))
                })?;
                let stride = match pointee {
                    Type::Slice(element) => size_of(element),
                    // Indexing through a pointer to an array (`&[T; N]`) steps
                    // by the element, not by the whole array's size.
                    Type::Array(element, _) => size_of(element),
                    Type::Str => 1,
                    other => size_of(other),
                };
                Ok(Val::Ptr(bits + index * stride as u64))
            }
            Val::Fat(bits, _) => {
                let element = match pointee(base_ty) {
                    Some(Type::Slice(element)) => (**element).clone(),
                    _ => Type::Int(IntTy::U8),
                };
                Ok(Val::Ptr(bits + index * size_of(&element) as u64))
            }
            Val::Struct(_) | Val::Array(_) => {
                let Type::Array(element, _) = base_ty else {
                    return Err(Trap::Internal(format!(
                        "index_ptr on register aggregate {base_ty:?}"
                    )));
                };
                let bits = self.home_of(frame, base, base_ty)?;
                Ok(Val::Ptr(bits + index * size_of(element) as u64))
            }
            other => Err(Trap::Internal(format!("index_ptr on {other:?}"))),
        }
    }

    /// Takes the address of an operand: pointers pass their bits through,
    /// register aggregates materialize into their home allocation.
    fn address_of(
        &mut self,
        frame: &mut Frame,
        value: Value,
        ty: &Type,
        val: Val,
    ) -> Result<u64, Trap> {
        match val {
            Val::Ptr(bits) => Ok(bits),
            Val::Str(text) => Ok(self.mem.intern_str(&text)),
            Val::Fat(bits, _) => Ok(bits),
            _ => self.home_of(frame, value, ty),
        }
    }

    fn extract(&mut self, val: Val, agg_ty: &Type, index: usize) -> Result<Val, Trap> {
        match val {
            Val::Struct(fields) => fields.into_iter().nth(index).ok_or_else(|| {
                Trap::Internal(format!("extract_value index {index} out of bounds"))
            }),
            Val::Array(elements) => (*elements).clone().into_iter().nth(index).ok_or_else(|| {
                Trap::Internal(format!("extract_value index {index} out of bounds"))
            }),
            Val::Fat(bits, len) => match index {
                0 => Ok(Val::Ptr(bits)),
                1 => Ok(Val::Int(len)),
                _ => Err(Trap::Internal(format!(
                    "fat pointer field index {index} out of bounds"
                ))),
            },
            Val::Str(text) => {
                let bits = self.mem.intern_str(&text);
                match index {
                    0 => Ok(Val::Ptr(bits)),
                    1 => Ok(Val::Int(text.len() as u64)),
                    _ => Err(Trap::Internal(format!(
                        "str field index {index} out of bounds"
                    ))),
                }
            }
            // Field access through a reference/pointer operand — the
            // lowering reads `self.field` as `extract_value` directly on a
            // `&Struct` receiver.
            Val::Ptr(bits) => {
                let pointee = pointee(agg_ty).ok_or_else(|| {
                    Trap::Internal(format!("extract_value {index} through {agg_ty:?}"))
                })?;
                let fields = field_types(pointee).ok_or_else(|| {
                    Trap::Internal(format!("extract_value {index} on {pointee:?}"))
                })?;
                let field_ty = fields.get(index).cloned().ok_or_else(|| {
                    Trap::Internal(format!("extract_value index {index} out of bounds"))
                })?;
                let offset = field_offset(pointee, index).unwrap_or(0);
                self.mem
                    .read_val(bits + offset as u64, &field_ty)
                    .map_err(Trap::Internal)
            }
            other => Err(Trap::Internal(format!(
                "extract_value on non-aggregate {other:?}"
            ))),
        }
    }

    fn const_val(&mut self, ty: &Type, constant: &ConstValue) -> Val {
        match constant {
            ConstValue::Int(bits, _) => Val::Int(*bits),
            // `NegativeInt` stores the magnitude; negate for the bits (which
            // also maps iN::MIN onto itself like the C literal).
            ConstValue::NegativeInt(bits, _) => Val::Int(bits.wrapping_neg()),
            ConstValue::Float(value, _) => Val::Float(*value),
            ConstValue::Bool(value) => Val::Bool(*value),
            // String constants keep their source quotes; the C backend
            // strips and escape-decodes them at emission, so the interpreter
            // decodes them at materialization.
            ConstValue::String(text) => {
                let decoded = decode_string_literal(text);
                if is_fat(ty) {
                    let ptr = self.mem.intern_str(&decoded);
                    Val::Fat(ptr, decoded.len() as u64)
                } else {
                    Val::Str(decoded.into())
                }
            }
            ConstValue::Char(value) => Val::Char(*value),
            ConstValue::Unit => Val::Unit,
        }
    }

    fn expect_ptr(&self, val: &Val) -> Result<u64, Trap> {
        val.as_ptr()
            .ok_or_else(|| Trap::Internal(format!("expected pointer, found {val:?}")))
    }

    fn expect_int(&self, val: &Val) -> Result<u64, Trap> {
        val.as_int()
            .ok_or_else(|| Trap::Internal(format!("expected integer, found {val:?}")))
    }

    fn binop(&self, op: BinOp, ty: &Type, a: &Val, b: &Val) -> Result<Val, Trap> {
        match ty {
            Type::Int(int_ty) => {
                let width = int_width(*int_ty);
                let lhs = self.expect_int(a)?;
                let rhs = self.expect_int(b)?;
                integer_binop(op, *int_ty, width, lhs, rhs).map(Val::Int)
            }
            // Eager bitwise operators on `bool`, which `match` lowering uses to
            // fold pattern tests and arm guards together; `&&`/`||` reach the
            // executor as short-circuit control flow instead.
            Type::Bool => {
                let Val::Bool(lhs) = a else {
                    return Err(Trap::Internal(format!("bool op on {a:?}")));
                };
                let Val::Bool(rhs) = b else {
                    return Err(Trap::Internal(format!("bool op on {b:?}")));
                };
                match op {
                    BinOp::BitAnd => Ok(Val::Bool(lhs & rhs)),
                    BinOp::BitOr => Ok(Val::Bool(lhs | rhs)),
                    BinOp::BitXor => Ok(Val::Bool(lhs ^ rhs)),
                    _ => Err(Trap::Internal(format!("binary op `{op:?}` on bool"))),
                }
            }
            Type::Float(float_ty) => {
                let Val::Float(lhs) = a else {
                    return Err(Trap::Internal(format!("float op on {a:?}")));
                };
                let Val::Float(rhs) = b else {
                    return Err(Trap::Internal(format!("float op on {b:?}")));
                };
                let raw = match op {
                    BinOp::Add => lhs + rhs,
                    BinOp::Sub => lhs - rhs,
                    BinOp::Mul => lhs * rhs,
                    BinOp::Div => lhs / rhs,
                    BinOp::Mod => lhs % rhs,
                    _ => return Err(Trap::Internal(format!("bitwise float op `{op:?}`"))),
                };
                Ok(Val::Float(round_float(*float_ty, raw)))
            }
            _ => Err(Trap::Internal(format!(
                "binary op `{op:?}` on unsupported type {ty:?}"
            ))),
        }
    }

    fn unop(
        &mut self,
        op: UnOp,
        result_ty: &Type,
        operand_ty: &Type,
        val: Val,
        frame: &mut Frame,
        value: Value,
    ) -> Result<Val, Trap> {
        match op {
            UnOp::Neg => match (operand_ty, &val) {
                (Type::Int(int_ty), Val::Int(bits)) => {
                    Ok(Val::Int(truncate(bits.wrapping_neg(), int_width(*int_ty))))
                }
                (Type::Float(_), Val::Float(number)) => Ok(Val::Float(-number)),
                _ => Err(Trap::Internal(format!("negation on {val:?}"))),
            },
            UnOp::Not => match (operand_ty, &val) {
                (Type::Int(int_ty), Val::Int(bits)) => {
                    Ok(Val::Int(truncate(!*bits, int_width(*int_ty))))
                }
                (Type::Bool, Val::Bool(flag)) => Ok(Val::Bool(!*flag)),
                _ => Err(Trap::Internal(format!("not on {val:?}"))),
            },
            UnOp::Ref | UnOp::MutRef => match val {
                // `Ref` on a reference/pointer operand copies the reference
                // (params are by-value; `SliceIter { bytes: self, .. }`
                // relies on this fat-copy semantics).
                Val::Ptr(bits) => Ok(Val::Ptr(bits)),
                fat @ Val::Fat(_, _) => Ok(fat),
                Val::Str(text) => Ok(Val::Fat(self.mem.intern_str(&text), text.len() as u64)),
                _ => {
                    let bits = self.address_of(frame, value, operand_ty, val)?;
                    Ok(Val::Ptr(bits))
                }
            },
            UnOp::Deref => match (&val, result_ty) {
                // An unsized pointee travels as its own fat pointer, so
                // `*self` on a `&str` / `&[T]` keeps the `{pointer, length}`
                // pair rather than reading a fixed-size object through it.
                (Val::Fat(bits, len), Type::Str | Type::Slice(_)) => Ok(Val::Fat(*bits, *len)),
                _ => {
                    let bits = self.expect_ptr(&val)?;
                    self.mem.read_val(bits, result_ty).map_err(Trap::Internal)
                }
            },
        }
    }

    fn cmp(&self, op: CmpOp, ty: &Type, a: &Val, b: &Val) -> Result<Val, Trap> {
        let decided = match ty {
            Type::Int(int_ty) => {
                let width = int_width(*int_ty);
                let lhs = self.expect_int(a)?;
                let rhs = self.expect_int(b)?;
                if int_ty.is_signed() {
                    integer_cmp(
                        op,
                        i128::from(sign_extend(lhs, width)),
                        i128::from(sign_extend(rhs, width)),
                    )
                } else {
                    integer_cmp(op, lhs as i128, rhs as i128)
                }
            }
            Type::Float(_) => {
                let Val::Float(lhs) = a else {
                    return Err(Trap::Internal(format!("float compare on {a:?}")));
                };
                let Val::Float(rhs) = b else {
                    return Err(Trap::Internal(format!("float compare on {b:?}")));
                };
                float_cmp(op, *lhs, *rhs)
            }
            Type::Bool => {
                let Some(lhs) = a.as_bool() else {
                    return Err(Trap::Internal(format!("bool compare on {a:?}")));
                };
                let Some(rhs) = b.as_bool() else {
                    return Err(Trap::Internal(format!("bool compare on {b:?}")));
                };
                match op {
                    CmpOp::Eq => lhs == rhs,
                    CmpOp::Neq => lhs != rhs,
                    _ => return Err(Trap::Internal(format!("ordered bool compare `{op:?}`"))),
                }
            }
            Type::Char => {
                let Val::Char(lhs) = a else {
                    return Err(Trap::Internal(format!("char compare on {a:?}")));
                };
                let Val::Char(rhs) = b else {
                    return Err(Trap::Internal(format!("char compare on {b:?}")));
                };
                integer_cmp(op, i128::from(u32::from(*lhs)), i128::from(u32::from(*rhs)))
            }
            // `&str` Eq/Neq compare content (the C backend emits a memcmp);
            // every other reference/pointer compares bit identity.
            Type::Ref(inner, _)
                if matches!(&**inner, Type::Str) && matches!(op, CmpOp::Eq | CmpOp::Neq) =>
            {
                let lhs = self.str_bytes(a)?;
                let rhs = self.str_bytes(b)?;
                match op {
                    CmpOp::Eq => lhs == rhs,
                    _ => lhs != rhs,
                }
            }
            Type::Str => {
                let lhs = self.str_bytes(a)?;
                let rhs = self.str_bytes(b)?;
                match op {
                    CmpOp::Eq => lhs == rhs,
                    CmpOp::Neq => lhs != rhs,
                    _ => {
                        return Err(Trap::Internal(format!("ordered str compare `{op:?}`")));
                    }
                }
            }
            Type::Ptr(_) | Type::Ref(_, _) => {
                let lhs = self.pointer_bits(a)?;
                let rhs = self.pointer_bits(b)?;
                match op {
                    CmpOp::Eq => lhs == rhs,
                    CmpOp::Neq => lhs != rhs,
                    CmpOp::Lt => lhs < rhs,
                    CmpOp::Gt => lhs > rhs,
                    CmpOp::LtEq => lhs <= rhs,
                    CmpOp::GtEq => lhs >= rhs,
                }
            }
            _ => {
                return Err(Trap::Internal(format!(
                    "comparison `{op:?}` on unsupported type {ty:?}"
                )));
            }
        };
        Ok(Val::Bool(decided))
    }

    fn pointer_bits(&self, val: &Val) -> Result<u64, Trap> {
        match val {
            Val::Ptr(bits) => Ok(*bits),
            Val::Fat(bits, _) => Ok(*bits),
            other => Err(Trap::Internal(format!("pointer comparison on {other:?}"))),
        }
    }

    /// Byte content of a `&str`-typed operand for content comparisons.
    fn str_bytes(&self, val: &Val) -> Result<Vec<u8>, Trap> {
        match val {
            Val::Fat(bits, len) => self
                .mem
                .read_bytes(*bits, *len as usize)
                .map(<[u8]>::to_vec)
                .map_err(Trap::Internal),
            Val::Str(text) => Ok(text.as_bytes().to_vec()),
            other => Err(Trap::Internal(format!("str comparison on {other:?}"))),
        }
    }

    fn cast(&mut self, op: CastOp, from: &Type, to: &Type, val: Val) -> Result<Val, Trap> {
        match op {
            CastOp::IntToInt => {
                // Integer casts also carry char operands (`char as u32`).
                let bits = match &val {
                    Val::Int(bits) => *bits,
                    Val::Char(ch) => u64::from(u32::from(*ch)),
                    other => {
                        return Err(Trap::Internal(format!("int-to-int cast on {other:?}")));
                    }
                };
                let width = match to {
                    Type::Int(int_ty) => int_width(*int_ty),
                    _ => return Err(Trap::Internal(format!("int-to-int cast to {to:?}"))),
                };
                // Widen signed sources with sign extension; narrowing and
                // unsigned widening re-truncate the bits, like C.
                let converted = match from {
                    Type::Int(from_ty) if from_ty.is_signed() => {
                        sign_extend(bits, int_width(*from_ty)) as u64
                    }
                    _ => bits,
                };
                Ok(Val::Int(truncate(converted, width)))
            }
            CastOp::IntToChar => {
                let bits = self.expect_int(&val)?;
                Ok(Val::Char(char::from_u32(bits as u32).unwrap_or('\0')))
            }
            CastOp::IntToFloat => {
                let bits = self.expect_int(&val)?;
                let number = match from {
                    Type::Int(int_ty) if int_ty.is_signed() => {
                        sign_extend(bits, int_width(*int_ty)) as f64
                    }
                    _ => bits as f64,
                };
                Ok(Val::Float(round_float(float_ty(to), number)))
            }
            CastOp::FloatToInt => {
                let Val::Float(number) = val else {
                    return Err(Trap::Internal(format!("float-to-int on {val:?}")));
                };
                let (int_ty, width) = match to {
                    Type::Int(int_ty) => (*int_ty, int_width(*int_ty)),
                    Type::Char => (IntTy::U32, 32),
                    _ => return Err(Trap::Internal(format!("float-to-int cast to {to:?}"))),
                };
                let converted = float_to_int(number, int_ty, width);
                Ok(Val::Int(converted))
            }
            CastOp::FloatToFloat => {
                let Val::Float(number) = val else {
                    return Err(Trap::Internal(format!("float-to-float on {val:?}")));
                };
                Ok(Val::Float(round_float(float_ty(to), number)))
            }
            CastOp::BoolToInt => {
                let flag = val
                    .as_bool()
                    .ok_or_else(|| Trap::Internal(format!("bool-to-int on {val:?}")))?;
                let width = match to {
                    Type::Int(int_ty) => int_width(*int_ty),
                    _ => 64,
                };
                Ok(Val::Int(truncate(u64::from(flag), width)))
            }
            CastOp::IntToBool => {
                let bits = self.expect_int(&val)?;
                Ok(Val::Bool(bits != 0))
            }
            CastOp::IntToPtr => {
                let bits = self.expect_int(&val)?;
                Ok(Val::Ptr(bits))
            }
            CastOp::PtrToPtr => Ok(match val {
                Val::Ptr(bits) => {
                    if is_fat(to) {
                        // Thin-to-fat casts have no length source; lowering
                        // always builds fat values from (ptr, len) literals,
                        // so this only guards against malformed MIR.
                        Val::Fat(bits, 0)
                    } else {
                        Val::Ptr(bits)
                    }
                }
                fat @ Val::Fat(_, _) => {
                    if is_fat(to) || matches!(to, Type::Tuple(_)) {
                        fat
                    } else {
                        Val::Ptr(self.pointer_bits(&fat)?)
                    }
                }
                Val::Str(text) => {
                    let bits = self.mem.intern_str(&text);
                    if is_fat(to) {
                        Val::Fat(bits, text.len() as u64)
                    } else {
                        Val::Ptr(bits)
                    }
                }
                other => return Err(Trap::Internal(format!("pointer cast on {other:?}"))),
            }),
        }
    }
}

/// Decodes a MIR string constant into its runtime bytes, mirroring the C
/// backend's `c_string_parts`: raw strings (`r#"…"#`) pass through as-is,
/// regular literals drop their quotes and resolve backslash escapes.
fn decode_string_literal(text: &str) -> String {
    if let Some(body) = raw_string_body(text) {
        return body.to_string();
    }
    let inner = if text.len() >= 2 {
        &text[1..text.len() - 1]
    } else {
        text
    };
    decode_escapes(inner)
}

fn decode_escapes(text: &str) -> String {
    let mut out = String::new();
    let mut chars = text.chars();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            out.push(ch);
            continue;
        }
        out.push(match chars.next() {
            Some('n') => '\n',
            Some('r') => '\r',
            Some('t') => '\t',
            Some('0') => '\0',
            Some(ch) => ch,
            None => '\\',
        });
    }
    out
}

fn raw_string_body(text: &str) -> Option<&str> {
    let rest = text.strip_prefix('r')?;
    let hashes = rest.bytes().take_while(|&b| b == b'#').count();
    let open_quote = 1 + hashes;
    if text.as_bytes().get(open_quote) != Some(&b'"') {
        return None;
    }
    let suffix_len = 1 + hashes;
    let suffix_start = text.len().checked_sub(suffix_len)?;
    if suffix_start <= open_quote || text.as_bytes().get(suffix_start) != Some(&b'"') {
        return None;
    }
    if !text.as_bytes()[suffix_start + 1..]
        .iter()
        .all(|&b| b == b'#')
    {
        return None;
    }
    Some(&text[open_quote + 1..suffix_start])
}

fn pointee(ty: &Type) -> Option<&Type> {
    match ty {
        Type::Ref(inner, _) | Type::Ptr(inner) => Some(inner),
        _ => None,
    }
}

/// Compact one-line rendering of an instruction for trap context.
fn display_inst(inst: &Inst) -> String {
    match &inst.kind {
        InstKind::Const(value) => format!("const {value:?}"),
        InstKind::BinOp(op, a, b) => format!("binop {op:?} %{a:?} %{b:?}"),
        InstKind::UnOp(op, a) => format!("unop {op:?} %{a:?}"),
        InstKind::Cmp(op, a, b) => format!("cmp {op:?} %{a:?} %{b:?}"),
        InstKind::Cast(op, a, _) => format!("cast {op:?} %{a:?}"),
        InstKind::SizeOf(ty) => format!("size_of {ty:?}"),
        InstKind::Alloca(ty) => format!("alloca {ty:?}"),
        InstKind::HeapAlloc(ty) => format!("heap_alloc {ty:?}"),
        InstKind::HeapFree(a) => format!("heap_free %{a:?}"),
        InstKind::Load(a) => format!("load %{a:?}"),
        InstKind::Store(v, p) => format!("store %{v:?} -> %{p:?}"),
        InstKind::FieldPtr(a, index) => format!("field_ptr %{a:?} #{index}"),
        InstKind::IndexPtr(a, i) => format!("index_ptr %{a:?} %{i:?}"),
        InstKind::CheckedIndexPtr(a, i, len) => {
            format!("checked_index_ptr %{a:?} %{i:?} %{len:?}")
        }
        InstKind::ExtractValue(a, index) => format!("extract_value %{a:?} #{index}"),
        InstKind::Call(callee, _) => format!("call {callee:?}"),
        InstKind::Panic(..) => "panic".to_string(),
        InstKind::FunctionRef(f) => format!("function_ref {f:?}"),
        InstKind::CallIndirect(a, _) => format!("call_indirect %{a:?}"),
        InstKind::StructValue(_) => "struct_value".to_string(),
        InstKind::SparseStructValue(_) => "sparse_struct_value".to_string(),
        InstKind::ArrayValue(_) => "array_value".to_string(),
        InstKind::TupleValue(_) => "tuple_value".to_string(),
        InstKind::Phi(_) => "phi".to_string(),
    }
}

/// Whether a type is represented as a fat `(ptr, len)` value.
fn is_fat(ty: &Type) -> bool {
    matches!(pointee(ty), Some(inner) if !inner.is_sized())
}

/// Static type of every value in a function: parameters reserve the low
/// numbers, then each block's instructions take consecutive values from
/// `start_value`.
fn value_types(function: &Function) -> Vec<Type> {
    let mut types = vec![Type::Void; function.next_value as usize];
    for param in &function.params {
        if (param.value.0 as usize) < types.len() {
            types[param.value.0 as usize] = param.ty.clone();
        }
    }
    for (_, block) in function.blocks.iter() {
        for (offset, inst) in block.insts.iter().enumerate() {
            let value = block.value_at(offset).0 as usize;
            if value < types.len() {
                types[value] = inst.ty.clone();
            }
        }
    }
    types
}

const fn int_width(ty: IntTy) -> u32 {
    match ty {
        IntTy::I8 | IntTy::U8 => 8,
        IntTy::I16 | IntTy::U16 => 16,
        IntTy::I32 | IntTy::U32 => 32,
        IntTy::I64 | IntTy::U64 | IntTy::Isize | IntTy::Usize => 64,
    }
}

fn truncate(bits: u64, width: u32) -> u64 {
    if width == 64 {
        bits
    } else {
        bits & ((1u64 << width) - 1)
    }
}

fn sign_extend(bits: u64, width: u32) -> i64 {
    if width == 64 {
        bits as i64
    } else {
        ((bits << (64 - width)) as i64) >> (64 - width)
    }
}

fn float_ty(ty: &Type) -> FloatTy {
    match ty {
        Type::Float(float_ty) => *float_ty,
        _ => FloatTy::F64,
    }
}

fn round_float(ty: FloatTy, value: f64) -> f64 {
    match ty {
        FloatTy::F32 => value as f32 as f64,
        FloatTy::F64 => value,
    }
}

fn float_cmp(op: CmpOp, lhs: f64, rhs: f64) -> bool {
    match op {
        CmpOp::Eq => lhs == rhs,
        CmpOp::Neq => lhs != rhs,
        CmpOp::Lt => lhs < rhs,
        CmpOp::Gt => lhs > rhs,
        CmpOp::LtEq => lhs <= rhs,
        CmpOp::GtEq => lhs >= rhs,
    }
}

fn integer_cmp(op: CmpOp, lhs: i128, rhs: i128) -> bool {
    match op {
        CmpOp::Eq => lhs == rhs,
        CmpOp::Neq => lhs != rhs,
        CmpOp::Lt => lhs < rhs,
        CmpOp::Gt => lhs > rhs,
        CmpOp::LtEq => lhs <= rhs,
        CmpOp::GtEq => lhs >= rhs,
    }
}

/// Saturating float→int conversion: NaN maps to 0, out-of-range values
/// clamp to the type's min/max, matching the C backend's cast expression.
fn float_to_int(number: f64, int_ty: IntTy, width: u32) -> u64 {
    if number.is_nan() {
        return 0;
    }
    let (min, max) = if int_ty.is_signed() {
        let max = (1i128 << (width - 1)) - 1;
        (-max - 1, max)
    } else {
        (0, (1i128 << width) - 1)
    };
    // Clamping through f64 is exact: every in-range integer is exactly
    // representable, and out-of-range values clamp to the endpoints.
    let clamped = if number < min as f64 {
        min
    } else if number > max as f64 {
        max
    } else {
        number as i128
    };
    truncate(clamped as u64, width)
}

fn integer_binop(op: BinOp, int_ty: IntTy, width: u32, lhs: u64, rhs: u64) -> Result<u64, Trap> {
    match op {
        BinOp::Add => Ok(truncate(lhs.wrapping_add(rhs), width)),
        BinOp::Sub => Ok(truncate(lhs.wrapping_sub(rhs), width)),
        BinOp::Mul => Ok(truncate(lhs.wrapping_mul(rhs), width)),
        BinOp::BitAnd => Ok(truncate(lhs & rhs, width)),
        BinOp::BitOr => Ok(truncate(lhs | rhs, width)),
        BinOp::BitXor => Ok(truncate(lhs ^ rhs, width)),
        BinOp::Shl => {
            let count = (rhs % u64::from(width)) as u32;
            Ok(truncate(lhs.wrapping_shl(count), width))
        }
        BinOp::Shr => {
            let count = (rhs % u64::from(width)) as u32;
            if int_ty.is_signed() {
                let signed = sign_extend(lhs, width);
                Ok(truncate(signed.wrapping_shr(count) as u64, width))
            } else {
                Ok(truncate(lhs.wrapping_shr(count), width))
            }
        }
        BinOp::Div | BinOp::Mod => {
            let is_div = op == BinOp::Div;
            if rhs == 0 {
                return Err(Trap::Abort {
                    message: if is_div {
                        "division by zero".into()
                    } else {
                        "remainder by zero".into()
                    },
                });
            }
            if int_ty.is_signed() {
                let lhs_signed = sign_extend(lhs, width);
                let rhs_signed = sign_extend(rhs, width);
                // Signed min is exactly `1 << (width-1)` in width-truncated
                // bits; comparing bits avoids negating it.
                if lhs == 1u64 << (width - 1) && rhs == truncate(u64::MAX, width) {
                    return Err(Trap::Abort {
                        message: if is_div {
                            "integer division overflow".into()
                        } else {
                            "integer remainder overflow".into()
                        },
                    });
                }
                let result = if is_div {
                    lhs_signed.wrapping_div(rhs_signed)
                } else {
                    lhs_signed.wrapping_rem(rhs_signed)
                };
                Ok(truncate(result as u64, width))
            } else {
                let result = if is_div { lhs / rhs } else { lhs % rhs };
                Ok(truncate(result, width))
            }
        }
    }
}

/// The RNG seed to run with: an explicit nonzero seed is used verbatim so
/// runs stay reproducible; zero seeds from the clock so separate runs of a
/// random program differ.
fn next_seed(seed: u64) -> u64 {
    if seed != 0 {
        return seed;
    }
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(1, |since| since.as_nanos() as u64);
    if nanos == 0 {
        0x9E37_79B9_7F4A_7C15
    } else {
        nanos
    }
}

pub(crate) fn advance_rng(rng: &mut u64) -> u64 {
    let mut x = *rng;
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    *rng = x;
    x.wrapping_mul(0x2545_F491_4F6C_DD1D)
}
