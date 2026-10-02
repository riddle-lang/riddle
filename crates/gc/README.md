# Riddle default GC runtime

This crate owns Riddle's default non-moving mark-sweep runtime. The compiler
does not embed this implementation; `clue` selects it when a binary
package does not provide a custom runtime source.

Every runtime provider implements this C ABI:

```c
void rgc_init(void *stack_bottom);
void *rgc_alloc(size_t size, const uint32_t *descriptor);
void *rgc_realloc(void *ptr, size_t size);
void rgc_free(void *ptr);
void rgc_collect(void);
```

Clue links the platform process-argument runtime separately, so custom memory
runtime providers do not implement `std::env` argument functions.

`rgc_alloc` must return a non-null, suitably aligned address that does not
move while references may still exist. An allocator without collection may
ignore `stack_bottom` and implement `rgc_collect` as a no-op.
`rgc_realloc` must preserve the existing prefix, may return a different
address, and keeps the previous block's descriptor; `rgc_free` must accept
null and release an allocation owned by the provider. The current ABI does not
support moving collection, finalizers, or thread stack registration.

Shared standard-library code that needs untyped byte storage calls
`riddle_alloc_bytes(size_t)`, which both runtimes provide: the default
runtime forwards to `rgc_alloc(size, NULL)` (so that payload is scanned
conservatively) and the allocator-only runtime forwards to `riddle_alloc`.
The name is neutral because one std declaration has to compile against either
runtime, and the allocator-only runtime must not reference any `rgc_` symbol.
## Layout descriptors

The second argument of `rgc_alloc` is a flat `uint32_t` array published
by the compiler:

```c
descriptor[0]      number of GC pointer slots
descriptor[1 + i]  byte offset of slot i inside the payload
```

The collector marks exactly those words and skips the rest, so an integer that
happens to look like an address no longer keeps an object alive. Two special
cases matter for providers:

- `NULL` means "unknown layout". The payload is scanned conservatively,
  word by word, which is always sound but may retain objects longer. The C
  backend passes `NULL` for copied C strings and for any type it cannot
  describe faithfully.
- A descriptor with zero slots (`{ 0u }`) is precise and cheap: the payload
  provably holds no GC pointer at all.

Providers are free to ignore the descriptor and behave conservatively; the
parameter exists so a provider can be precise when it wants to be. The runtime
validates every descriptor at registration (slot count and offset bounds) and
silently degrades a malformed one to `NULL`, so a codegen bug cannot make
the collector read out of bounds.

## Allocation shape

Small payloads (up to 1 KiB) are carved out of 64 KiB chunks into 16-byte size
classes, so recycling a freed object is a free-list push/pop instead of a
`malloc`/`free` round trip. Larger payloads go straight to
`malloc`, and `rgc_free` returns each block to the path it came from.

## Collection statistics

The runtime always maintains cheap counters, readable through
`rgc_stat_*` accessors: live bytes and objects, total allocations, collections,
typed versus conservative object counts and payload scans, objects marked by
the last cycle, mark and sweep duration in nanoseconds, bytes obtained from the
OS, and the number of chunks. `rgc_is_allocated` answers whether an address is
currently registered, which is what the tests use to observe reclamation. The
counters that have no accessor — mark edges by origin, and what the last cycle
collected — are part of the debug line below.

Run `cargo test -p gc -- --ignored --nocapture` for the manual
allocation-throughput baseline, which reports the accessor set above for a
mixed small-object, large-object, and survivor workload.

Setting `RGC_DEBUG_STATS=1` prints one stderr line per collection with
those numbers; the typed-versus-conservative split is the measurement that says
how much of the heap is still scanned conservatively.
