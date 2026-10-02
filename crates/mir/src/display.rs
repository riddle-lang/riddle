use std::fmt;

use crate::func::Function;
use crate::instr::{ConstValue, InstKind, Terminator};
use crate::module::Module;
use crate::types::Type;

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "module {} {{", self.name)?;

        // 外部函数声明
        for ext in &self.externs {
            let params = ext
                .params
                .iter()
                .map(|t| format!("{}", TypeFmt(t)))
                .collect::<Vec<_>>()
                .join(", ");
            writeln!(
                f,
                "  extern {} ({}) -> {};",
                ext.name,
                params,
                TypeFmt(&ext.ret_type)
            )?;
        }

        // 函数定义
        for &fid in &self.function_order {
            let func = &self.functions[fid];
            fmt_func(f, func, 1)?;
        }

        writeln!(f, "}}")?;
        Ok(())
    }
}

fn fmt_func(f: &mut fmt::Formatter<'_>, func: &Function, indent: usize) -> fmt::Result {
    let pad = "  ".repeat(indent);
    let params = func
        .params
        .iter()
        .map(|p| format!("%{}: {}", p.value.0, TypeFmt(&p.ty)))
        .collect::<Vec<_>>()
        .join(", ");

    writeln!(
        f,
        "{}fn {}({}) -> {} {{",
        pad,
        func.name,
        params,
        TypeFmt(&func.ret_type)
    )?;

    // 遍历所有基本块
    for (bid, block) in func.blocks.iter() {
        let label = block.label.as_deref().unwrap_or("?");
        writeln!(f, "  {}block_{}({:?}):", pad, label, bid.into_raw())?;

        // 收集 phi 节点
        let _phi_params = String::new();
        for (i, inst) in block.insts.iter().enumerate() {
            if let InstKind::Phi(pairs) = &inst.kind {
                let v = block.value_at(i);
                let pair_strs: Vec<String> = pairs
                    .iter()
                    .map(|(val, bid)| format!("%{} from block{}", val.0, bid.into_raw()))
                    .collect();
                writeln!(
                    f,
                    "  {}  {}  v{} = phi [{}]",
                    pad,
                    TypeFmt(&inst.ty),
                    v.0,
                    pair_strs.join(", ")
                )?;
            }
        }

        // 非 phi 指令
        for (i, inst) in block.insts.iter().enumerate() {
            if matches!(&inst.kind, InstKind::Phi(_)) {
                continue; // phi 已在上面处理
            }
            let v = block.value_at(i);
            if inst.ty == Type::Void {
                writeln!(f, "  {}  {}", pad, InstFmt(&inst.kind))?;
            } else {
                writeln!(
                    f,
                    "  {}  v{} = {} : {}",
                    pad,
                    v.0,
                    InstFmt(&inst.kind),
                    TypeFmt(&inst.ty)
                )?;
            }
        }

        // 终止指令
        write!(f, "  {pad}  ")?;
        match &block.terminator {
            Terminator::Pending => writeln!(f, "<pending>")?,
            Terminator::Branch(target) => {
                let tl = func.blocks[*target].label.as_deref().unwrap_or("?");
                writeln!(f, "br block_{tl}")?;
            }
            Terminator::CondBranch(cond, then_block, else_block) => {
                let tl = func.blocks[*then_block].label.as_deref().unwrap_or("?");
                let el = func.blocks[*else_block].label.as_deref().unwrap_or("?");
                writeln!(f, "br v{}, block_{}, block_{}", cond.0, tl, el)?;
            }
            Terminator::Return(val) => match val {
                Some(v) => writeln!(f, "return v{}", v.0)?,
                None => writeln!(f, "return")?,
            },
            Terminator::Unreachable => writeln!(f, "unreachable")?,
        }
    }

    writeln!(f, "{pad}}}")?;
    Ok(())
}

// 格式化辅助

struct TypeFmt<'a>(&'a Type);
impl fmt::Display for TypeFmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Type::Int(ty) => write!(f, "{ty:?}"),
            Type::Float(ty) => write!(f, "{ty:?}"),
            Type::Bool => write!(f, "bool"),
            Type::Str => write!(f, "str"),
            Type::Char => write!(f, "char"),
            Type::Unit => write!(f, "()"),
            Type::Never => write!(f, "!"),
            Type::Ref(inner, mutable) => {
                let kw = if *mutable { "&mut " } else { "&" };
                write!(f, "{}{}", kw, TypeFmt(inner))
            }
            Type::Ptr(inner) => write!(f, "*{}", TypeFmt(inner)),
            Type::Tuple(elems) => {
                let inner: Vec<String> = elems.iter().map(|t| format!("{}", TypeFmt(t))).collect();
                write!(f, "({})", inner.join(", "))
            }
            Type::Slice(inner) => write!(f, "[{}]", TypeFmt(inner)),
            Type::Array(inner, len) => write!(f, "[{}; {}]", TypeFmt(inner), len),
            Type::Struct(s) => write!(f, "{}", s.name()),
            Type::Enum(e) => write!(f, "{}", e.name),
            Type::FnPtr(fp) => {
                let params: Vec<String> = fp
                    .params
                    .iter()
                    .map(|t| format!("{}", TypeFmt(t)))
                    .collect();
                write!(f, "fn({}) -> {}", params.join(", "), TypeFmt(&fp.ret))
            }
            Type::Void => write!(f, "void"),
        }
    }
}

struct InstFmt<'a>(&'a InstKind);
impl fmt::Display for InstFmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            InstKind::Const(c) => fmt_const(f, c),
            InstKind::BinOp(op, a, b) => write!(f, "{:?} v{}, v{}", op, a.0, b.0),
            InstKind::UnOp(op, a) => write!(f, "{:?} v{}", op, a.0),
            InstKind::Cmp(op, a, b) => write!(f, "cmp_{:?} v{}, v{}", op, a.0, b.0),
            InstKind::Cast(op, a, _) => write!(f, "cast_{:?} v{}", op, a.0),
            InstKind::SizeOf(ty) => write!(f, "size_of {}", TypeFmt(ty)),
            InstKind::Alloca(_) => write!(f, "alloca"),
            InstKind::HeapAlloc(_) => write!(f, "heap_alloc"),
            InstKind::HeapFree(ptr) => write!(f, "heap_free v{}", ptr.0),
            InstKind::Load(p) => write!(f, "load v{}", p.0),
            InstKind::Store(v, p) => write!(f, "store v{} -> v{}", v.0, p.0),
            InstKind::FieldPtr(b, idx) => write!(f, "field_ptr v{}, #{}", b.0, idx),
            InstKind::IndexPtr(b, i) => write!(f, "index_ptr v{}, v{}", b.0, i.0),
            InstKind::CheckedIndexPtr(b, i, len) => {
                write!(f, "checked_index_ptr v{}, v{}, v{}", b.0, i.0, len.0)
            }
            InstKind::ExtractValue(v, i) => write!(f, "extract_value v{}, #{}", v.0, i),
            InstKind::Call(callee, args) => {
                let args_str: Vec<String> = args.iter().map(|a| format!("v{}", a.0)).collect();
                write!(f, "call {:?}({})", callee, args_str.join(", "))
            }
            InstKind::Panic(message, site) => {
                write!(f, "panic v{} at {}:{}", message.0, site.line, site.column)
            }
            InstKind::FunctionRef(function) => write!(f, "function_ref {function:?}"),
            InstKind::CallIndirect(callee, args) => {
                let args = args
                    .iter()
                    .map(|arg| format!("v{}", arg.0))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "call_indirect v{}({})", callee.0, args)
            }
            InstKind::StructValue(fields) => {
                let flds: Vec<String> = fields.iter().map(|a| format!("v{}", a.0)).collect();
                write!(f, "struct [{}]", flds.join(", "))
            }
            InstKind::SparseStructValue(fields) => {
                let fields = fields
                    .iter()
                    .map(|(index, value)| format!("#{index}: v{}", value.0))
                    .collect::<Vec<_>>();
                write!(f, "struct {{ {} }}", fields.join(", "))
            }
            InstKind::ArrayValue(elems) => {
                let el: Vec<String> = elems.iter().map(|a| format!("v{}", a.0)).collect();
                write!(f, "array [{}]", el.join(", "))
            }
            InstKind::TupleValue(elems) => {
                let el: Vec<String> = elems.iter().map(|a| format!("v{}", a.0)).collect();
                write!(f, "tuple ({})", el.join(", "))
            }
            InstKind::Phi(_) => write!(f, "phi"), // 实际在块头打印
        }
    }
}

fn fmt_const(f: &mut fmt::Formatter<'_>, c: &ConstValue) -> fmt::Result {
    match c {
        ConstValue::Int(v, w) => write!(f, "iconst({w:?}) {v:?}"),
        ConstValue::NegativeInt(v, w) => write!(f, "iconst({w:?}) -{v:?}"),
        ConstValue::Float(v, w) => write!(f, "fconst({w:?}) {v:?}"),
        ConstValue::Bool(v) => write!(f, "bconst {v}"),
        ConstValue::String(v) => write!(f, "sconst {v:?}"),
        ConstValue::Char(v) => write!(f, "cconst {v:?}"),
        ConstValue::Unit => write!(f, "unit"),
    }
}
