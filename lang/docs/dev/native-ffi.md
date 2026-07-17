# Vo Native FFI Guide

This guide describes the current Rust extension API. The current extension
macro is `#[vo_fn]` from `vo-ext` / `vo-ffi-macro`; stdlib shims use
`#[vostd_fn]` inside stdlib crates. Older macro names are historical and should
not be used in current extension examples.

## Minimal Extension

Vo declares an extern function by writing a function declaration without a
body:

```vo
package math

func Add(a int, b int) int;
```

Rust implements and exports the function table with `vo_ext`:

```rust
use vo_ext::prelude::*;

#[vo_fn("mylib", "Add")]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

vo_ext::export_extensions!();
```

The Rust crate must point at the authoritative module file so the macro can
derive the full package owner and track source changes:

```toml
[package.metadata.vo]
vomod = "../vo.mod"
```

For module `github.com/example/mylib`, `#[vo_fn("mylib", "Add")]` registers
the typed identity `(github.com/example/mylib, Add)`. A subpackage uses the
module leaf plus its relative path, such as `mylib/codec`. The wire key is the
canonical length-prefixed `vo1:<package-byte-length>:<package>:<function-byte-length>:<function>`
encoding. Package and function boundaries therefore remain unambiguous for
Unicode identifiers and punctuation in module owners.

On native targets, `export_extensions!()` generates exactly three Volang ABI
exports: `vo_ext_get_abi_version`, `vo_ext_get_abi_fingerprint`, and
`vo_ext_get_entries`. The entry table is collected from `#[vo_fn]`
registrations. An extension owner may publish an explicitly namespaced product
C API from the same library; those symbols remain outside the Volang ABI and
the runtime never resolves or guarantees them. Host services travel through
the per-call ABI-v9 callback table; there are no process-wide bridge setter or
clearer exports.

The loader validates every raw entry as a canonical length-prefixed extern key.
Null callbacks, invalid effect bits, duplicate names, legacy flattened names,
and packages outside the exact canonical module-owner boundary reject the
complete table. `[extension].name` remains a display/provider identity and is
never rewritten into an extern symbol.
Loader APIs that address a library use its canonical module owner. Human names
may repeat across unrelated modules and remain diagnostics-only. One owner may
not be split across multiple native libraries.

For process-local linkme registration, `export_extensions!()` also emits one
module-level declaration into `EXTERN_MODULE_OWNER_TABLE`. This owner catalog
is independent of function entries, so an extension with zero `#[vo_fn]`
functions still reserves its namespace and prevents fallback to a parent
provider. Repeated, invalid, or undeclared-owner records reject the catalog as
one transaction.

Macro dependency markers cover `Cargo.toml`, the configured `vo.mod`, a
selected `vo.work`, each selected workspace source's `vo.mod`, and every
`.vo` source file consulted during expansion. The macro also observes `VOWORK`
and `VO_FFI_SOURCE_FINGERPRINT` as compile-time inputs.

For normal local development, run this command from the module root:

```bash
vo build .
```

Commit the `Cargo.lock` at the actual Cargo workspace root for every native
extension (`rust/Cargo.lock` for a standalone extension crate). The engine uses
Cargo workspace discovery and locked metadata, invokes the build with
`--locked`, and rejects a missing or concurrently changed workspace lock. It
does not create or restore a member-local substitute lock.

Volang-owned producers such as vo-engine and the Quickplay builders derive a
deterministic content fingerprint from their complete declared inputs. Those
inputs cover the Rust tree, every reachable local Cargo package, Vo source
membership and bytes, `vo.mod`, `vo.lock`, the active `vo.work`, active
workspace sources, the actual Cargo workspace lock, and the relevant
Cargo, toolchain, configuration, and build context. The producer injects a
content-and-generation token into the complete reachable Rust graph and also
sets it through `VO_FFI_SOURCE_FINGERPRINT`, so adding, removing, modifying, or
performing an A-B-A transition on an input forces recompilation and invalidates
the producer's artifact cache. vo-engine compares the input generation before
and after the Cargo build and retries with a fresh token when compilation raced
an edit.

Every cache probe still executes Cargo, allowing `build.rs`
`rerun-if-changed` declarations to remain authoritative for external inputs.
The engine accepts only the exact package/`cdylib` artifact reported by Cargo's
JSON output and requires its canonical path to match the path declared in
`vo.mod`.

Running `cargo build` directly is a low-level integration path whose caller
owns source-set invalidation. Whenever relevant source membership changes, set
`VO_FFI_SOURCE_FINGERPRINT` to a new deterministic content fingerprint or run
`cargo clean` before rebuilding. A content hash preserves useful Cargo cache
hits while still closing stable Rust's proc-macro directory-membership gap.

## Function Modes

`#[vo_fn]` chooses a mode from the Rust function signature:

| Mode | Signature shape | Use |
|---|---|---|
| Simple | `fn(args...) -> T` | Direct argument and return slot mapping |
| Result | `fn(args...) -> Result<T, String>` | Return values plus an error result |
| Manual | `fn(ctx: &mut ExternCallContext) -> ExternResult` | Full stack, GC, host-event, and closure control |

Manual mode injects a local `slots` module when the macro can resolve the Vo
package and extern declaration:

```rust
use vo_ext::prelude::*;

#[vo_fn("math", "Describe")]
fn describe(ctx: &mut ExternCallContext) -> ExternResult {
    let value = ctx.arg_i64(slots::ARG_VALUE);
    ctx.ret_str(slots::RET_0, &format!("value={value}"));
    ExternResult::Ok
}
```

Every exported implementation must be a plain, non-generic Rust function.
`const`, `async`, `unsafe`, explicit Rust ABI declarations, C variadics,
receivers, generic parameters, and `where` clauses are rejected. Manual mode
requires exactly one `&mut ExternCallContext` parameter with no explicit
lifetime and an exact `ExternResult` return.

Result mode requires the Vo declaration to contain exactly one `error`, in the
final result position. `T` maps to every preceding result in order. Additional
or misplaced `error` results fail expansion.

## Type Mapping

Automatic modes validate the Vo declaration and Rust signature exactly:

| Vo type | Rust argument | Rust return |
|---|---|---|
| `int`, `int64` | `i64` | `i64` |
| `int8` | `i8` | `i8` |
| `int16` | `i16` | `i16` |
| `int32`, `rune` | `i32` | `i32` |
| `uint`, `uint64` | `u64` | `u64` |
| `uint8`, `byte` | `u8` | `u8` |
| `uint16` | `u16` | `u16` |
| `uint32` | `u32` | `u32` |
| `float32` | `f32` | `f32` |
| `float64` | `f64` | `f64` |
| `bool` | `bool` | `bool` |
| `string` | `&str` | `String` |
| `[]byte` | `&[u8]` | `Vec<u8>` |
| `[]string` | Manual mode | `Vec<String>` |
| `any`, `error` | `InterfaceSlot` | `InterfaceSlot` |
| pointer, slice, map, channel, port, island, function | `GcRef` | `GcRef` |

Platform-sized Rust integers, `&mut Gc`, variadics, named types, and by-value
arrays or structs have no automatic mapping. Use Manual mode for these cases.
All argument and return slot totals are checked against the `u16` address
space during expansion.

Manual mode derives layout from the parsed Vo sources. It always generates
positional `ARG_0`, `ARG_1`, ... and `RET_0`, `RET_1`, ... constants. A readable
ASCII argument alias is added only when its normalized name is unique. Local
and imported named layouts are resolved with package scope, recursively;
cycles, unknown names, and overflow are compile errors. Array lengths support
package integer constants and exact integer expressions using the language's
constant arithmetic.

`ExternCallContext` also exposes lower-level readers and writers such as
`arg_i64`, `arg_str`, `arg_bytes`, `arg_any`, `ret_i64`, `ret_str`,
`ret_bytes`, `ret_any`, `ret_error`, `ret_nil_error`, `alloc_str`,
`alloc_bytes`, `alloc_slice`, and `gc_alloc_raw`. The sections below define
which helpers are valid across a dynamic-library boundary.

## Dynamic-library Helper Contract

ABI-v9 dynamic extensions may use these `ExternCallContext` groups:

| Group | Supported helpers |
|---|---|
| Frame and output | `available_slots`, `arg_count`/`arg_slots`, `arg_start`, `ret_start`, `ret_slots`, `extern_id`, `slot`, `set_slot`, `write_output*`, `writeln_output*` |
| Typed arguments | scalar `arg_*`, `arg_ref`, `arg_str`/`try_arg_str`, arbitrary-byte string and byte-slice readers, `arg_any`, `arg_error`, and `arg_any_as_*` |
| Typed returns | scalar `ret_*`, `ret_ref`, `ret_nil`, string/byte helpers, `ret_any`, `ret_error`, `ret_interface_pair`, `ret_nil_error`, and `ret_error_msg` |
| Replay and result payloads | host-event token/data helpers, `resume_closure_result`, `set_host_output`, and the `set_ext_*` helpers used by generated trampolines |
| Allocator-neutral GC | `gc`, string/byte/slice allocation, string-slice allocation, `gc_alloc_raw`, `alloc_and_copy_slots`, and the typed container accessors described below |

Low-level GC allocation must use canonical `ValueMeta`, the exact host module
metadata id, and the exact physical slot width. The host validates allocation
intent (`object`, canonical array, or bare value slots), value kind, metadata,
and width before allocating. Forged kinds, missing metadata, zero-width
canonical arrays, and allocator-specific payloads fail the call.

These APIs remain same-image runtime facilities and are rejected for a loaded
dynamic extension:

- module/runtime-type and itab inspection, including `module`, `struct_meta`,
  `runtime_types`, type-resolution helpers, module-driven boxing, and
  `gc_alloc_struct`
- VM/fiber pointers, direct I/O runtime access, program arguments, sentinel
  caches, and direct function-definition inspection
- map allocation or mutation (`alloc_map`, map helpers, and `VoMap`)

Generated trampolines contain provider panics and translate ABI-facade misuse
to a failed native call. Extension code should treat the table above as the
public contract instead of relying on same-image runtime methods that happen to
be visible on the Rust type.

### Argument and Return Windows

Typed argument and return indices are relative to their declared windows. A
helper may touch the window only when its complete width fits: scalar/reference
helpers use one slot and interface helpers use two adjacent slots. Frame
initialization also requires every non-empty window's final offset to remain
representable as `u16`; a one-slot window beginning at `u16::MAX` is valid, and
any wider window at that offset is rejected.

All offset and width arithmetic is checked. An invalid read returns an inert
value (`0`, null, empty bytes/text, or a nil interface), an invalid write leaves
the stack unchanged, and either operation records a contract error. The host
then rejects the complete call and restores the original return window. Raw
`slot`/`set_slot` offsets are frame-relative and still receive checked stack
bounds; they do not bypass the typed helper contract.

The raw ABI structs represent callbacks as nullable C function pointers. The
frame constructor validates every required main-table and host-service callback
before building the safe facade, so `None` produces `RESULT_ABI_ERROR` without
an indirect call through address zero.

## Manual Results

Manual functions return `ExternResult`:

```rust
use vo_ext::prelude::*;

#[vo_fn("host", "ReadLater", effects(MAY_HOST_REPLAY))]
fn read_later(ctx: &mut ExternCallContext) -> ExternResult {
    if let Some(token) = ctx.take_resume_host_event_token() {
        ctx.ret_i64(slots::RET_0, token as i64);
        return ExternResult::Ok;
    }

    ExternResult::HostEventWaitAndReplay { token: 1 }
}
```

The native extension trampoline maps `ExternResult` variants to
`vo_runtime::ffi::ext_abi::RESULT_*` codes. Panic messages, host-event tokens,
and closure callback payloads are stored on `ExternCallContext`.

Effectful manual functions must declare their provider effects in the macro:

| Result variant | Required effect |
|---|---|
| `Yield` | `MAY_YIELD` |
| `Block` | `MAY_QUEUE_BLOCK` |
| `WaitIo { .. }` | `MAY_WAIT_IO_REPLAY` |
| `HostEventWait { .. }` | `MAY_HOST_WAIT` |
| `HostEventWaitAndReplay { .. }` | `MAY_HOST_REPLAY` |
| `CallClosure { .. }` | `MAY_CALL_CLOSURE_REPLAY` |

`effects(UNKNOWN_CONTROL)` is allowed only as the sole effect. Combining
`UNKNOWN_CONTROL` with precise `MAY_*` bits is rejected.

## Container Helpers

`vo_ext::prelude` re-exports typed helpers backed by `vo-runtime`, including
`VoString`, `VoBytes`, `VoSlice`, `VoArray`, `VoPtr`, and `VoClosure`.
`VoMap` is intentionally absent because map payloads own same-image Rust
collections.

`VoElem` describes readable layout. `VoWritableElem` is the additional bound
required by `VoSlice::set` and `VoArray::set`, which keeps read-only element
representations out of the mutable API. `VoStringElem` returns exact owned
bytes and is read-only; allocate a VM string explicitly and store its `GcRef`
when mutation is required. `bool` occupies one logical VM slot and one physical
byte in canonical packed arrays/slices. Flat-slot aliases still use an
eight-byte storage stride, and the accessors handle that distinction. Slice and
array cursors implement Rust `Iterator` and `ExactSizeIterator`; `reset`
rewinds them.

These helpers are low-level views over live runtime objects. Their `unsafe
from_ref` constructors require the pointer, element kind, element width, and
lifetime to match the declared accessor type.

```rust
use vo_ext::prelude::*;

#[vo_fn("strings", "Len")]
fn len(ctx: &mut ExternCallContext) -> ExternResult {
    let s = VoString::from_ref(ctx.arg_ref(slots::ARG_S));
    ctx.ret_i64(slots::RET_0, s.len() as i64);
    ExternResult::Ok
}
```

## Current Boundaries

- `#[vo_fn]` is for extension crates. `#[vostd_fn]` is for stdlib registration.
- `vo-stdlib-source` is the publishable standard-library asset crate rooted at
  `lang/stdlib`. Its packaged `stdlib.toml` and `.vo` files are the same
  canonical source tree consumed by proc macros and embedded compiler/runtime
  users; no generated source mirror participates in that boundary.
- Native extensions export a dynamic-library entry table. WASM extension builds
  use explicit entry lists with stable source identities:

  ```rust
  vo_ext::export_extensions!(vo_ext::vo_extension_entry!(
      "mylib/math",
      "FastAdd",
  ));
  ```

  `vo_extension_entry!` accepts only string literals, resolves the same
  authoritative `vo.mod` and bodyless declaration as `#[vo_fn]`, and expands
  to the exact injectively mangled entry constant. A leading Rust module path
  may be supplied before the two literals for an implementation declared in a
  nested module.
- The extension ABI version and fingerprint live in `vo-runtime` and are
  exported by `vo-ext`.
- ABI-v9 owns all Rust allocations in the image that created them. Cross-image
  data uses scalars, borrowed pointer/length pairs, opaque handles, or host
  callbacks that copy into owner storage.
- If an example needs a helper outside the dynamic-library contract above,
  design an allocator-neutral host capability for it.
