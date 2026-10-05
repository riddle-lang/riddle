use gc::{ARGS_RUNTIME_C, NO_GC_RUNTIME_C, RUNTIME_C};
use std::{fs, process::Command};

fn run_c(body: &str) -> std::process::Output {
    run_c_program("", body)
}

/// Compile a scenario against the bundled runtime. The prelude is emitted at
/// file scope before the scenario, which is where helper functions with
/// compiler-specific attributes belong.
fn run_c_program(prelude: &str, body: &str) -> std::process::Output {
    run_c_program_with_env(prelude, body, None)
}

fn run_c_program_with_env(
    prelude: &str,
    body: &str,
    env: Option<(&str, &str)>,
) -> std::process::Output {
    use std::sync::atomic::{AtomicU32, Ordering};
    static SEQUENCE: AtomicU32 = AtomicU32::new(0);

    // Unique file names: the tests run in parallel threads and Windows
    // locks an executable while it runs, so a shared test.c/test.exe pair
    // makes one thread's compile race the other's run.
    let dir = std::env::temp_dir().join(format!(
        "riddle-gc-{}-{}",
        std::process::id(),
        SEQUENCE.fetch_add(1, Ordering::Relaxed)
    ));
    let _ = fs::create_dir_all(&dir);
    let src = dir.join("test.c");
    let exe = dir.join(if cfg!(windows) { "test.exe" } else { "test" });
    // The scenario runs in a callee of main so every live pointer sits in
    // a frame below the stack-bottom anchor: the scan covers
    // [collect frame .. anchor], and same-frame locals allocated above the
    // anchor's slot would otherwise escape the conservative scan.
    //
    // `_POSIX_C_SOURCE` has to be set before the first system header: glibc
    // latches the feature selection there, and the runtime's own guard would
    // arrive too late to expose `clock_gettime` / `nanosleep` under `-std=c11`.
    // The C backend emits the same guard at the top of its prologue.
    let program = format!(
        "#if !defined(_WIN32) && !defined(_POSIX_C_SOURCE)\n#define _POSIX_C_SOURCE 199309L\n#endif\n#include <stddef.h>\n#include <stdint.h>\n#include <stdio.h>\n{RUNTIME_C}\n{prelude}\nstatic int scenario(void){{ {body} }}\nint main(void){{ void *bottom = &bottom; rgc_init(bottom); return scenario(); }}"
    );
    fs::write(&src, program).unwrap();
    let compiler = std::env::var("CC").unwrap_or_else(|_| "cc".into());
    let compile = Command::new(&compiler)
        .args(["-std=c11"])
        .arg(&src)
        .arg("-o")
        .arg(&exe)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "C compile failed: {}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let mut command = Command::new(&exe);
    if let Some((name, value)) = env {
        command.env(name, value);
    }
    let output = command.output().unwrap();
    let _ = fs::remove_dir_all(&dir);
    output
}

#[test]
fn exports_process_argument_runtime() {
    for symbol in [
        "GetCommandLineW",
        "riddle_parse_windows_args",
        "riddle_args_init",
        "riddle_argc",
        "riddle_argv_at",
        "riddle_argv_len",
    ] {
        assert!(ARGS_RUNTIME_C.contains(symbol), "missing {symbol}");
    }
}

#[test]
fn exports_runtime_api() {
    assert!(RUNTIME_C.contains("void rgc_init(void *stack_bottom)"));
    assert!(RUNTIME_C.contains("void *rgc_alloc(size_t size, const uint32_t *descriptor)"));
    assert!(RUNTIME_C.contains("void *rgc_realloc(void *ptr, size_t size)"));
    assert!(RUNTIME_C.contains("void rgc_free(void *ptr)"));
    assert!(RUNTIME_C.contains("void rgc_collect(void)"));
    for symbol in [
        "size_t rgc_stat_live_bytes(void)",
        "size_t rgc_stat_live_objects(void)",
        "size_t rgc_stat_heap_bytes(void)",
        "size_t rgc_stat_collections(void)",
        "int rgc_is_allocated(const void *ptr)",
    ] {
        assert!(RUNTIME_C.contains(symbol), "missing {symbol}");
    }
    assert!(!RUNTIME_C.contains("GC_MALLOC"));
    assert!(!RUNTIME_C.contains("<gc.h>"));
    assert!(!RUNTIME_C.contains("abort()"));
}

#[test]
fn exports_an_allocator_only_runtime() {
    for symbol in [
        "riddle_alloc",
        "riddle_alloc_bytes",
        "riddle_realloc",
        "riddle_free",
    ] {
        assert!(NO_GC_RUNTIME_C.contains(symbol), "missing {symbol}");
    }
    for forbidden in ["rgc_", "RgcHeader", "collect", "stack_bottom"] {
        assert!(
            !NO_GC_RUNTIME_C.contains(forbidden),
            "no-GC runtime contains {forbidden}"
        );
    }
}

#[test]
fn collection_preserves_live_objects_and_interior_roots() {
    // The interior pointer lives in a local that is read after the
    // collection, so it is guaranteed to sit in the scanned root set
    // (stack range or callee-saved register snapshot) at rgc_collect time;
    // the collector must resolve it back to the object header and keep the
    // object alive.
    let output = run_c(
        "unsigned char *p = rgc_alloc(32, NULL); p[3] = 77; unsigned char *interior = p + 3; for (int i=0;i<2000;i++) (void)rgc_alloc(4096, NULL); rgc_collect(); if (*interior != 77) return 1; return 0;",
    );
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn realloc_and_exact_free_keep_address_semantics() {
    // Realloc preserves content across collections, and rgc_free only accepts
    // the exact allocation address: q + 1 must be a no-op, after which q still
    // survives a collection until the exact free.
    let output = run_c(
        "unsigned char *p = rgc_alloc(8, NULL); p[0] = 9; p = rgc_realloc(p, 4096); if (p[0] != 9) return 1; unsigned char *q = rgc_alloc(8, NULL); rgc_free(q + 1); q[0] = 5; rgc_collect(); if (q[0] != 5) return 2; rgc_free(q); return 0;",
    );
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// Helper functions for the layout-precision scenarios. The stash helpers run
/// in their own frames so the target pointer never lives in the probe frame,
/// and scrub overwrites the dead helper frame before the collection.
const PRECISION_PRELUDE: &str = r#"
#if defined(_MSC_VER)
#define NOINLINE __declspec(noinline)
#else
#define NOINLINE __attribute__((noinline))
#endif

struct Holder { void *p; uintptr_t fake; };

NOINLINE static void stash_as_integer(struct Holder *holder) {
    void *target = rgc_alloc(64, NULL);
    holder->fake = (uintptr_t)target;
}

NOINLINE static void stash_as_pointer(struct Holder *holder) {
    holder->p = rgc_alloc(64, NULL);
}

NOINLINE static void scrub(void) {
    volatile unsigned char buffer[8192];
    size_t i;
    for (i = 0; i < sizeof(buffer); ++i) {
        buffer[i] = 0;
    }
}

static int probe(const uint32_t *descriptor, int as_pointer) {
    struct Holder *holder = (struct Holder *)rgc_alloc(sizeof(struct Holder), descriptor);
    uintptr_t address;
    int alive;
    holder->p = NULL;
    holder->fake = 0;
    if (as_pointer) {
        stash_as_pointer(holder);
    } else {
        stash_as_integer(holder);
    }
    scrub();
    rgc_collect();
    address = as_pointer ? (uintptr_t)holder->p : holder->fake;
    alive = rgc_is_allocated((const void *)address);
    rgc_free(holder);
    return alive;
}
"#;

#[test]
fn typed_descriptors_ignore_integer_lookalikes() {
    // The typed holder declares only p as a GC pointer slot, so the address
    // parked in the integer field must not keep its target alive. The same
    // layout without a descriptor is scanned word by word and does retain it,
    // which is exactly the imprecision descriptors remove.
    let body = r#"
        static const uint32_t typed[] = { 1u, (uint32_t)offsetof(struct Holder, p) };
        int typed_alive = probe(typed, 0);
        int conservative_alive = probe(NULL, 0);
        printf("typed=%d conservative=%d\n", typed_alive, conservative_alive);
        return typed_alive == 0 && conservative_alive == 1 ? 0 : 1;
    "#;
    let output = run_c_program(PRECISION_PRELUDE, body);
    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn typed_descriptors_keep_declared_pointer_slots_alive() {
    let body = r#"
        static const uint32_t typed[] = { 1u, (uint32_t)offsetof(struct Holder, p) };
        int typed_alive = probe(typed, 1);
        int conservative_alive = probe(NULL, 1);
        printf("typed=%d conservative=%d\n", typed_alive, conservative_alive);
        return typed_alive == 1 && conservative_alive == 1 ? 0 : 1;
    "#;
    let output = run_c_program(PRECISION_PRELUDE, body);
    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn malformed_descriptors_degrade_to_conservative_scanning() {
    // A descriptor that points outside its payload, or claims more slots than
    // could ever fit, must be rejected at registration: the collector then
    // falls back to the conservative scan instead of reading out of bounds.
    let body = r#"
        static const uint32_t outside[] = { 1u, 4096u };
        static const uint32_t too_many[] = { 100000u };
        int outside_alive = probe(outside, 0);
        int too_many_alive = probe(too_many, 0);
        printf("outside=%d too_many=%d\n", outside_alive, too_many_alive);
        return outside_alive == 1 && too_many_alive == 1 ? 0 : 1;
    "#;
    let output = run_c_program(PRECISION_PRELUDE, body);
    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn small_blocks_are_recycled_within_their_size_class() {
    // Freeing a small block returns it to its size class rather than to
    // malloc: re-allocating the same shape then reuses the chunk instead of
    // asking the OS for another one.
    let body = r#"
        void *blocks[256];
        size_t chunks_after_first_round;
        size_t i;
        for (i = 0; i < 256; ++i) {
            blocks[i] = rgc_alloc(48, NULL);
        }
        for (i = 0; i < 256; ++i) {
            rgc_free(blocks[i]);
        }
        chunks_after_first_round = rgc_stat_chunks();
        for (i = 0; i < 256; ++i) {
            blocks[i] = rgc_alloc(48, NULL);
        }
        if (rgc_stat_live_objects() != 256) return 1;
        if (rgc_stat_chunks() != chunks_after_first_round) return 2;
        for (i = 0; i < 256; ++i) {
            rgc_free(blocks[i]);
        }
        if (rgc_stat_live_objects() != 0) return 3;
        return 0;
    "#;
    let output = run_c(body);
    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn large_objects_bypass_the_size_classes() {
    let body = r#"
        size_t chunks_before = rgc_stat_chunks();
        size_t heap_before = rgc_stat_heap_bytes();
        void *large = rgc_alloc(64u * 1024u, NULL);
        if (rgc_stat_chunks() != chunks_before) return 1;
        if (rgc_stat_heap_bytes() < heap_before + 64u * 1024u) return 2;
        rgc_free(large);
        if (rgc_stat_heap_bytes() != heap_before) return 3;
        return 0;
    "#;
    let output = run_c(body);
    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn statistics_track_object_kinds_and_collections() {
    let body = r#"
        static const uint32_t desc[] = { 0u };
        void *typed = rgc_alloc(32, desc);
        void *conservative = rgc_alloc(32, NULL);
        if (rgc_stat_live_objects() != 2) return 1;
        if (rgc_stat_typed_objects() != 1) return 2;
        if (rgc_stat_conservative_objects() != 1) return 3;
        if (rgc_stat_live_bytes() < 64) return 4;
        if (rgc_stat_total_objects() < 2) return 5;
        rgc_collect();
        if (rgc_stat_collections() == 0) return 6;
        if (rgc_stat_marked_objects() != 2) return 7;
        if (rgc_stat_typed_payload_scans() == 0) return 8;
        rgc_free(typed);
        rgc_free(conservative);
        if (rgc_stat_typed_objects() != 0 || rgc_stat_conservative_objects() != 0) return 9;
        return 0;
    "#;
    let output = run_c(body);
    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn debug_stats_report_one_line_per_collection() {
    let body = r#"
        static const uint32_t desc[] = { 0u };
        (void)rgc_alloc(48, desc);
        (void)rgc_alloc(48, NULL);
        rgc_collect();
        return 0;
    "#;
    let output = run_c_program_with_env("", body, Some(("RGC_DEBUG_STATS", "1")));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "stderr: {stderr}\nstdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );
    assert!(
        stderr.contains("rgc: collection 1:"),
        "missing collection line: {stderr}"
    );
    for field in [
        "live bytes in",
        "typed",
        "conservative",
        "payload scans",
        "mark",
        "sweep",
    ] {
        assert!(stderr.contains(field), "missing {field} in: {stderr}");
    }
}

#[test]
#[ignore = "manual GC benchmark; run with --ignored --nocapture"]
fn benchmark_allocation_throughput() {
    // Phase 0 baseline: prints allocation and collection counters for a mixed
    // small-object, large-object, and survivor workload. Use it to compare
    // collector revisions; it asserts nothing about timing because CI hosts
    // vary too much.
    let body = r#"
        void *live[128];
        size_t i;
        for (i = 0; i < 128; ++i) {
            live[i] = rgc_alloc(64, NULL);
        }
        for (i = 0; i < 200000; ++i) {
            void *small = rgc_alloc(48, NULL);
            rgc_free(small);
            if ((i % 1024u) == 0u) {
                (void)rgc_alloc(4096, NULL);
            }
        }
        printf(
            "allocations=%zu live_objects=%zu live_bytes=%zu chunks=%zu heap_bytes=%zu collections=%zu mark_ns=%zu sweep_ns=%zu\n",
            rgc_stat_total_objects(),
            rgc_stat_live_objects(),
            rgc_stat_live_bytes(),
            rgc_stat_chunks(),
            rgc_stat_heap_bytes(),
            rgc_stat_collections(),
            rgc_stat_last_mark_ns(),
            rgc_stat_last_sweep_ns()
        );
        return 0;
    "#;
    let output = run_c(body);
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    println!("{}", String::from_utf8_lossy(&output.stdout));
}
