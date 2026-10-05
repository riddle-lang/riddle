use std::cell::RefCell;
use std::fs::File;
use std::io::{Read as _, Write as _};
use std::path::Path;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use mir::types::Type;

use super::exec::{Interpreter, Trap, advance_rng};
use super::value::Val;

/// A host stream behind a `usize` handle.
pub(crate) enum Stream {
    Stdin,
    File(Rc<RefCell<File>>),
}

fn arg_int(args: &[Val], index: usize, name: &str) -> Result<u64, Trap> {
    args.get(index)
        .and_then(Val::as_int)
        .ok_or_else(|| Trap::Internal(format!("extern `{name}`: missing integer argument")))
}

fn arg_ptr(args: &[Val], index: usize, name: &str) -> Result<u64, Trap> {
    args.get(index)
        .and_then(Val::as_ptr)
        .ok_or_else(|| Trap::Internal(format!("extern `{name}`: missing pointer argument")))
}

fn arg_str(interp: &Interpreter, args: &[Val], index: usize, name: &str) -> Result<String, Trap> {
    args.get(index)
        .and_then(|value| value.str_content(&interp.mem))
        .ok_or_else(|| Trap::Internal(format!("extern `{name}`: missing string argument")))
}

/// Dispatches an `extern "C"` call to a native shim. Covers every extern the
/// runtime standard library declares except the `std::proc_macro` host entry
/// points (`riddle_alloc_bytes` and the `riddle_proc_*` set), which only exist
/// in the C runtime that clue builds and drives as a separate process; calling
/// one here fails with `UnsupportedExtern`.
pub(crate) fn call_extern(
    interp: &mut Interpreter,
    name: &str,
    args: &[Val],
    ret_ty: &Type,
) -> Result<Val, Trap> {
    match name {
        // ---- fmt: process stdout / stderr sinks ----
        "putchar" => {
            let byte = arg_int(args, 0, name)? as u8;
            interp.stdout.push(byte);
            Ok(Val::Int(u64::from(byte)))
        }
        "riddle_fmt_fputc_stderr" => {
            let byte = arg_int(args, 0, name)? as u8;
            interp.stderr.push(byte);
            Ok(Val::Int(u64::from(byte)))
        }

        // ---- panic fallback ----
        "abort" => Err(Trap::Abort {
            message: String::new(),
        }),

        // ---- GC heap facade (Vector buffers) ----
        "rgc_realloc" => {
            let old = arg_ptr(args, 0, name)?;
            let size = arg_int(args, 1, name)? as usize;
            let new = interp.mem.heap_alloc(size);
            if old != 0 {
                let (old_alloc, _) = super::mem::ptr_parts(old);
                let old_bytes = interp.mem.allocation_len(old_alloc as usize);
                let copy = old_bytes.min(size);
                if copy > 0 {
                    let bytes = interp
                        .mem
                        .read_bytes(old, copy)
                        .map_err(Trap::Internal)?
                        .to_vec();
                    interp
                        .mem
                        .write_bytes(new, &bytes)
                        .map_err(Trap::Internal)?;
                }
                interp.mem.free(old);
            }
            Ok(Val::Ptr(new))
        }
        "rgc_free" => {
            let ptr = arg_ptr(args, 0, name)?;
            interp.mem.free(ptr);
            Ok(Val::Unit)
        }
        "riddle_str_slice_ptr" => {
            let base = arg_ptr(args, 0, name)?;
            let start = arg_int(args, 1, name)?;
            Ok(Val::Ptr(base.wrapping_add(start)))
        }
        "riddle_mem_swap" => {
            let a = arg_ptr(args, 0, name)?;
            let b = arg_ptr(args, 1, name)?;
            let size = arg_int(args, 2, name)? as usize;
            let mut left = interp
                .mem
                .read_bytes(a, size)
                .map_err(Trap::Internal)?
                .to_vec();
            let right = interp
                .mem
                .read_bytes(b, size)
                .map_err(Trap::Internal)?
                .to_vec();
            interp.mem.write_bytes(a, &right).map_err(Trap::Internal)?;
            interp.mem.write_bytes(b, &left).map_err(Trap::Internal)?;
            left.clear();
            Ok(Val::Unit)
        }

        // ---- args ----
        "riddle_argc" => Ok(Val::Int(interp.args.len() as u64)),
        "riddle_argv_at" => {
            let index = arg_int(args, 0, name)? as usize;
            let Some(arg) = interp.args.get(index) else {
                return Err(Trap::Internal(format!(
                    "extern `{name}`: argument index {index} out of range"
                )));
            };
            Ok(Val::Ptr(interp.mem.intern_str(arg)))
        }
        "riddle_argv_len" => {
            let index = arg_int(args, 0, name)? as usize;
            let Some(arg) = interp.args.get(index) else {
                return Err(Trap::Internal(format!(
                    "extern `{name}`: argument index {index} out of range"
                )));
            };
            Ok(Val::Int(arg.len() as u64))
        }

        // ---- io / process ----
        "riddle_io_stdin" => Ok(Val::Int(interp.new_handle(Stream::Stdin))),
        "riddle_process_exit" => {
            let code = arg_int(args, 0, name)? as u32 as i32;
            Err(Trap::ProcessExit(code))
        }

        // ---- fs: stdio facade ----
        "riddle_fs_fopen" => {
            let path = arg_str(interp, args, 0, name)?;
            let mode = arg_str(interp, args, 1, name)?;
            let file = std::fs::OpenOptions::new()
                .read(mode.contains('r'))
                .write(mode.contains('w') || mode.contains('a'))
                .create(mode.contains('w') || mode.contains('a'))
                .append(mode.contains('a'))
                .truncate(mode.contains('w'))
                .open(&path);
            match file {
                Ok(file) => Ok(Val::Int(
                    interp.new_handle(Stream::File(Rc::new(RefCell::new(file)))),
                )),
                Err(_) => Ok(Val::Int(0)),
            }
        }
        "riddle_fs_fclose" => {
            let handle = arg_int(args, 0, name)?;
            match interp.handles.remove(&handle) {
                Some(Stream::File(_)) => Ok(Val::Int(0)),
                _ => Ok(Val::Int(u64::from(u32::MAX))),
            }
        }
        "riddle_fs_fread" => {
            let buffer = arg_ptr(args, 0, name)?;
            let size = arg_int(args, 1, name)? as usize;
            let count = arg_int(args, 2, name)? as usize;
            let handle = arg_int(args, 3, name)?;
            let total = size.saturating_mul(count);
            let mut bytes = vec![0u8; total];
            let outcome = match interp.handles.get(&handle) {
                Some(Stream::File(file)) => file.borrow_mut().read(&mut bytes),
                Some(Stream::Stdin) => {
                    flush_stdout(interp);
                    std::io::stdin().read(&mut bytes)
                }
                None => {
                    return Err(Trap::Internal(format!(
                        "extern `{name}`: unknown stream handle {handle}"
                    )));
                }
            };
            // A host read error raises the stream's error indicator and reports
            // a zero count, like `fread`, instead of trapping the program.
            let read = match outcome {
                Ok(read) => {
                    interp.read_failed.remove(&handle);
                    read
                }
                Err(_) => {
                    interp.read_failed.insert(handle);
                    return Ok(Val::Int(0));
                }
            };
            if read > 0 {
                interp
                    .mem
                    .write_bytes(buffer, &bytes[..read])
                    .map_err(Trap::Internal)?;
            }
            let items = read.checked_div(size).unwrap_or(0);
            Ok(Val::Int(items as u64))
        }
        "riddle_fs_ferror" => {
            let handle = arg_int(args, 0, name)?;
            Ok(Val::Int(u64::from(interp.read_failed.contains(&handle))))
        }
        "riddle_fs_fwrite" => {
            let buffer = arg_ptr(args, 0, name)?;
            let size = arg_int(args, 1, name)? as usize;
            let count = arg_int(args, 2, name)? as usize;
            let handle = arg_int(args, 3, name)?;
            let total = size.saturating_mul(count);
            let bytes = interp
                .mem
                .read_bytes(buffer, total)
                .map_err(Trap::Internal)?
                .to_vec();
            let written = (|| {
                match interp.handles.get(&handle) {
                    Some(Stream::File(file)) => file.borrow_mut().write(&bytes),
                    Some(Stream::Stdin) => {
                        Err(std::io::Error::other("cannot write to stdin handle"))
                    }
                    None => {
                        return Err(Trap::Internal(format!(
                            "extern `{name}`: unknown stream handle {handle}"
                        )));
                    }
                }
                .map_err(|_| Trap::Abort {
                    message: "fs write failed".into(),
                })
            })()?;
            let items = written.checked_div(size).unwrap_or(0);
            Ok(Val::Int(items as u64))
        }
        "riddle_fs_fflush" => {
            let handle = arg_int(args, 0, name)?;
            match interp.handles.get(&handle) {
                Some(Stream::File(file)) => {
                    let _ = file.borrow_mut().flush();
                }
                Some(Stream::Stdin) => {}
                None => {
                    return Err(Trap::Internal(format!(
                        "extern `{name}`: unknown stream handle {handle}"
                    )));
                }
            }
            Ok(Val::Int(0))
        }
        "riddle_fs_fgetc" => {
            let handle = arg_int(args, 0, name)?;
            let mut byte = [0u8; 1];
            let read = match interp.handles.get(&handle) {
                Some(Stream::File(file)) => {
                    file.borrow_mut().read(&mut byte).map_err(|_| Trap::Abort {
                        message: "fs read failed".into(),
                    })?
                }
                Some(Stream::Stdin) => {
                    flush_stdout(interp);
                    std::io::stdin().read(&mut byte).map_err(|_| Trap::Abort {
                        message: "fs read failed".into(),
                    })?
                }
                None => {
                    return Err(Trap::Internal(format!(
                        "extern `{name}`: unknown stream handle {handle}"
                    )));
                }
            };
            Ok(Val::Int(if read == 0 {
                u64::from(u32::MAX)
            } else {
                u64::from(byte[0])
            }))
        }

        // ---- fs: metadata + directory ----
        "riddle_fs_exists" => {
            let path = arg_str(interp, args, 0, name)?;
            Ok(Val::Int(u64::from(Path::new(&path).exists())))
        }
        "riddle_fs_remove" => {
            let path = arg_str(interp, args, 0, name)?;
            Ok(Val::Int(u64::from(std::fs::remove_file(&path).is_ok())))
        }
        "riddle_fs_rename" => {
            let from = arg_str(interp, args, 0, name)?;
            let to = arg_str(interp, args, 1, name)?;
            Ok(Val::Int(u64::from(std::fs::rename(&from, &to).is_ok())))
        }
        "riddle_fs_size" => {
            let path = arg_str(interp, args, 0, name)?;
            let size = std::fs::metadata(&path).map_or(0, |meta| meta.len());
            Ok(Val::Int(size))
        }
        "riddle_fs_is_file" => {
            let path = arg_str(interp, args, 0, name)?;
            Ok(Val::Int(u64::from(Path::new(&path).is_file())))
        }
        "riddle_fs_is_dir" => {
            let path = arg_str(interp, args, 0, name)?;
            Ok(Val::Int(u64::from(Path::new(&path).is_dir())))
        }
        "riddle_fs_read_dir" => {
            let path = arg_str(interp, args, 0, name)?;
            let names_out = arg_ptr(args, 1, name)?;
            let lens_out = arg_ptr(args, 2, name)?;
            let capacity = arg_int(args, 3, name)? as usize;
            let entries = std::fs::read_dir(&path)
                .map_err(|error| Trap::Internal(error.to_string()))?
                .filter_map(Result::ok)
                .map(|entry| entry.file_name().to_string_lossy().into_owned())
                .filter(|entry| entry != "." && entry != "..")
                .collect::<Vec<_>>();
            let seen = entries.len();
            for (index, entry) in entries.iter().take(capacity).enumerate() {
                let ptr = interp.mem.intern_str(entry);
                interp
                    .mem
                    .write_bytes(names_out + ((index * 8) as u64), &ptr.to_le_bytes())
                    .map_err(Trap::Internal)?;
                let len = entry.len() as u64;
                interp
                    .mem
                    .write_bytes(lens_out + ((index * 8) as u64), &len.to_le_bytes())
                    .map_err(Trap::Internal)?;
            }
            // The C facade reports the total entry count even when it
            // exceeded capacity, so the caller retries with more room.
            Ok(Val::Int(seen as u64))
        }

        // ---- time / random / sleep ----
        "riddle_time" => {
            let out = arg_ptr(args, 0, name)?;
            let now = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map_or(0, |since| since.as_secs() as i64);
            if out != 0 {
                interp
                    .mem
                    .write_bytes(out, &now.to_le_bytes())
                    .map_err(Trap::Internal)?;
            }
            Ok(Val::Int(now as u64))
        }
        "riddle_sleep_ms" => {
            let millis = arg_int(args, 0, name)?;
            std::thread::sleep(std::time::Duration::from_millis(millis));
            Ok(Val::Unit)
        }
        "riddle_random_u32" => Ok(Val::Int(advance_rng(&mut interp.rng) as u32 as u64)),
        "riddle_random_u64" => Ok(Val::Int(advance_rng(&mut interp.rng))),

        _ => {
            let _ = ret_ty;
            Err(Trap::UnsupportedExtern {
                name: name.to_string(),
            })
        }
    }
}

/// Interactive programs expect buffered output to appear before they read.
/// The buffer is drained so each byte is written exactly once: later output
/// keeps accumulating after the flush, and the host's final `stdout` dump
/// only contains what was produced since the last read.
fn flush_stdout(interp: &mut Interpreter) {
    if !interp.stdout.is_empty() {
        let drained = std::mem::take(&mut interp.stdout);
        let mut out = std::io::stdout();
        let _ = out.write_all(&drained);
        let _ = out.flush();
    }
}
