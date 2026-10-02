//! Tree-walking interpreter for Riddle MIR modules.
//!
//! The interpreter executes a fully lowered [`mir::Module`] — monomorphized,
//! closures and trait objects already lowered to structs of function
//! pointers — with C-backend-compatible semantics: wrapping integer
//! arithmetic, trapping division, saturating float→int casts, bounds-checked
//! indexing, and rustc-style panic rendering. It powers `riddle run` (no C
//! toolchain needed) and the `riddle repl` session loop, and doubles as a
//! fast, compiler-independent test oracle for MIR behavior.
//!
//! Extern functions declared by the bundled standard library are served by
//! native Rust shims (`std::fs`, time, random, process I/O, the `rgc_*`
//! allocation facade). Any other extern reports
//! [`Trap::UnsupportedExtern`].

mod exec;
mod externs;
mod mem;
mod value;

use mir::module::Module;
use mir::source_map::SourceFile;

use exec::Interpreter;
pub use exec::Trap;
pub use value::Val;

/// Knobs for a single interpreter run.
#[derive(Clone, Debug)]
pub struct Config {
    /// `std::env::args` served to the program (argv[0] included).
    pub args: Vec<String>,
    /// Seed for `std::random`; zero seeds from the clock so separate runs
    /// differ, any other value is used verbatim to keep runs reproducible.
    pub rng_seed: u64,
    /// Maximum Riddle call depth before [`Trap::StackOverflow`].
    pub max_depth: usize,
    /// Stack size for the interpreter thread. Interpreted recursion
    /// consumes host stack, so this is generous by default.
    pub stack_size: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            args: vec![
                std::env::current_exe()
                    .map(|path| path.display().to_string())
                    .unwrap_or_else(|_| "riddle".into()),
            ],
            rng_seed: 0,
            max_depth: 200_000,
            stack_size: 256 * 1024 * 1024,
        }
    }
}

/// Result of running a program to completion.
#[derive(Debug, Clone)]
pub struct Outcome {
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
    /// Exit code from `main`, or the trap that terminated the process.
    pub result: Result<i32, Trap>,
}

/// Compiles nothing, executes everything: runs `main` on a dedicated
/// large-stack thread (like the C backend's native stack budget).
#[must_use]
pub fn run(module: &Module, source_files: Vec<SourceFile>) -> Outcome {
    run_with(module, source_files, Config::default())
}

/// [`run`] with explicit configuration.
pub fn run_with(module: &Module, source_files: Vec<SourceFile>, config: Config) -> Outcome {
    std::thread::scope(|scope| {
        let spawned = std::thread::Builder::new()
            .stack_size(config.stack_size)
            .spawn_scoped(scope, move || {
                let mut interpreter = Interpreter::new(
                    module,
                    source_files,
                    config.args,
                    config.rng_seed,
                    config.max_depth,
                );
                let result = interpreter.run_main();
                Outcome {
                    stdout: interpreter.stdout,
                    stderr: interpreter.stderr,
                    result,
                }
            });
        let handle = match spawned {
            Ok(handle) => handle,
            Err(error) => return crash(format!("failed to start interpreter thread: {error}")),
        };
        match handle.join() {
            Ok(outcome) => outcome,
            Err(payload) => crash(payload.downcast_ref::<&str>().map_or_else(
                || "unknown interpreter crash".to_string(),
                ToString::to_string,
            )),
        }
    })
}

fn crash(message: String) -> Outcome {
    Outcome {
        stdout: Vec::new(),
        stderr: Vec::new(),
        result: Err(Trap::Internal(message)),
    }
}

/// Renders a value the way `{:?}` would print it in Riddle, using the
/// static type to recover signedness, field names, and pointee types.
/// Powers REPL expression echo; pointers render as `ptr@<bits>`.
#[must_use]
pub fn render_value(val: &Val, ty: &mir::types::Type, mem: &mem::Memory) -> String {
    match (ty, val) {
        (_, Val::Int(bits)) => render_int(*bits, ty),
        (_, Val::Float(number)) => {
            if *number == number.trunc() && number.is_finite() && number.abs() < 1e15 {
                format!("{number:.1}")
            } else {
                format!("{number}")
            }
        }
        (_, Val::Bool(flag)) => flag.to_string(),
        (_, Val::Char(ch)) => format!("{ch:?}"),
        (mir::types::Type::Str, Val::Str(text)) => format!("{text:?}"),
        (mir::types::Type::Ref(inner, _) | mir::types::Type::Ptr(inner), Val::Fat(ptr, len))
            if !inner.is_sized() =>
        {
            match &**inner {
                mir::types::Type::Str => {
                    let text = mem
                        .read_bytes(*ptr, *len as usize)
                        .map(|bytes| String::from_utf8_lossy(bytes).into_owned())
                        .unwrap_or_default();
                    format!("{text:?}")
                }
                element => {
                    let count = (*len as usize).min(8);
                    let stride = mem::size_of(element);
                    let mut items = Vec::with_capacity(count);
                    for index in 0..count {
                        let Ok(element_val) = mem.read_val(ptr + (index * stride) as u64, element)
                        else {
                            break;
                        };
                        items.push(render_value(&element_val, element, mem));
                    }
                    let ellipsis = if *len as usize > count { ", .." } else { "" };
                    format!("&[{}{}]", items.join(", "), ellipsis)
                }
            }
        }
        (_, Val::Ptr(bits)) => format!("ptr@{bits:#x}"),
        (_, Val::Fat(bits, len)) => format!("fat@{bits:#x}+{len}"),
        (_, Val::FnPtr(func)) => format!("fn({func:?})"),
        (mir::types::Type::Tuple(elements), Val::Struct(fields)) => {
            render_tuple(fields, elements, mem)
        }
        (mir::types::Type::Struct(strukt), Val::Struct(fields)) => {
            let def = strukt.def();
            let rendered = def
                .fields
                .iter()
                .zip(fields)
                .map(|((name, field_ty), field_val)| {
                    format!("{name}: {}", render_value(field_val, field_ty, mem))
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("{} {{ {rendered} }}", def.name)
        }
        (mir::types::Type::Enum(enum_ty), Val::Struct(fields)) => {
            let Some(Val::Int(tag)) = fields.first() else {
                return format!("{:?}", enum_ty.name);
            };
            let Some(variant) = enum_ty
                .variants
                .iter()
                .find(|v| v.discriminant == *tag as u32)
            else {
                return enum_ty.name.to_string();
            };
            match &variant.kind {
                mir::types::EnumVariantKind::Unit => format!("{}::{}", enum_ty.name, variant.name),
                mir::types::EnumVariantKind::Tuple(types) => {
                    let payload_start = 1;
                    let rendered = types
                        .iter()
                        .zip(fields.iter().skip(payload_start))
                        .map(|(payload_ty, payload_val)| render_value(payload_val, payload_ty, mem))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}::{}({rendered})", enum_ty.name, variant.name)
                }
                mir::types::EnumVariantKind::Struct(fields_layout) => {
                    let mut rendered = Vec::new();
                    for (offset, (name, payload_ty)) in (1usize..).zip(fields_layout.iter()) {
                        if let Some(payload_val) = fields.get(offset) {
                            rendered.push(format!(
                                "{name}: {}",
                                render_value(payload_val, payload_ty, mem)
                            ));
                        }
                    }
                    format!(
                        "{}::{} {{ {} }}",
                        enum_ty.name,
                        variant.name,
                        rendered.join(", ")
                    )
                }
            }
        }
        (mir::types::Type::Array(element, count), Val::Array(elements)) => {
            let rendered = elements
                .iter()
                .take(count.saturating_add(4))
                .map(|element_val| render_value(element_val, element, mem))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{rendered}]")
        }
        (_, Val::Struct(fields)) => format!("struct({})", fields.len()),
        (_, Val::Array(elements)) => format!("array({})", elements.len()),
        (_, Val::Unit) => "()".to_string(),
        (_, other) => format!("{other:?}"),
    }
}

fn render_tuple(fields: &[Val], elements: &[mir::types::Type], mem: &mem::Memory) -> String {
    let rendered = fields
        .iter()
        .zip(elements)
        .map(|(field_val, field_ty)| render_value(field_val, field_ty, mem))
        .collect::<Vec<_>>()
        .join(", ");
    format!("({rendered})")
}

fn render_int(bits: u64, ty: &mir::types::Type) -> String {
    use mir::types::{IntTy, Type};
    let Type::Int(int_ty) = ty else {
        return format!("{bits}");
    };
    let width = match int_ty {
        IntTy::I8 | IntTy::U8 => 8,
        IntTy::I16 | IntTy::U16 => 16,
        IntTy::I32 | IntTy::U32 => 32,
        _ => 64,
    };
    if int_ty.is_signed() {
        let extended = if width == 64 {
            bits as i64
        } else {
            ((bits << (64 - width)) as i64) >> (64 - width)
        };
        extended.to_string()
    } else if width == 64 {
        bits.to_string()
    } else {
        (bits & ((1u64 << width) - 1)).to_string()
    }
}

/// Shared handle so callers (REPL) can run multiple functions against one
/// persistent memory image.
pub struct Session {
    interpreter: Interpreter,
}

impl Session {
    #[must_use]
    pub fn new(module: &Module, source_files: Vec<SourceFile>, config: &Config) -> Self {
        Self {
            interpreter: Interpreter::new(
                module,
                source_files,
                config.args.clone(),
                config.rng_seed,
                config.max_depth,
            ),
        }
    }

    /// Calls a function by name with raw values.
    pub fn call(&mut self, name: &str, args: Vec<Val>) -> Result<Val, Trap> {
        self.interpreter.call(name, args)
    }

    /// Renders a value with the given static type.
    #[must_use]
    pub fn render(&self, val: &Val, ty: &mir::types::Type) -> String {
        render_value(val, ty, &self.interpreter.mem)
    }

    /// Drains captured stdout, letting REPL hosts interleave output.
    pub fn take_stdout(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.interpreter.stdout)
    }

    /// Drains captured stderr.
    pub fn take_stderr(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.interpreter.stderr)
    }

    /// Exposes the interpreter memory for value rendering.
    #[must_use]
    pub fn memory(&self) -> &mem::Memory {
        &self.interpreter.mem
    }

    /// Return type of a function, used to render REPL results.
    #[must_use]
    pub fn return_type_of(&self, function: &str) -> Option<mir::types::Type> {
        self.interpreter.return_type(function)
    }
}

// Re-exported for Session users constructing argument values.
pub use mem::Memory;
