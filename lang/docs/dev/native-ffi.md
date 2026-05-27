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

#[vo_fn("math", "Add")]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

vo_ext::export_extensions!();
```

On native targets, `export_extensions!()` generates `vo_ext_get_entries`,
`vo_ext_get_abi_fingerprint`, and host-bridge entry points. The entry table is
collected from `#[vo_fn]` registrations.

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

## Type Mapping

The macro codegen currently recognizes these Rust argument/return types:

| Rust type | Direction | Slot behavior |
|---|---|---|
| `i64`, `i32`, `i16`, `i8`, `isize` | arg/return | integer slot |
| `u64`, `u32`, `u16`, `u8`, `usize` | arg/return | unsigned slot |
| `f64`, `f32` | arg/return | float slot |
| `bool` | arg/return | boolean slot |
| `&str` | argument | borrowed Vo string |
| `String` | return | allocated Vo string |
| `&[u8]` | argument | borrowed Vo byte slice |
| `Vec<u8>` | return | allocated Vo byte slice |
| `Vec<String>` | return | allocated Vo string slice |
| `GcRef` | arg/return | one reference slot |
| `InterfaceSlot` | arg/return | two-slot interface value |

`ExternCallContext` also exposes lower-level readers and writers such as
`arg_i64`, `arg_str`, `arg_bytes`, `arg_any`, `ret_i64`, `ret_str`,
`ret_bytes`, `ret_any`, `ret_error`, `ret_nil_error`, `alloc_str`,
`alloc_bytes`, `alloc_slice`, `alloc_map`, `gc_alloc_struct`, and
`gc_alloc_raw`.

## Manual Results

Manual functions return `ExternResult`:

```rust
use vo_ext::prelude::*;

#[vo_fn("host", "ReadLater")]
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

## Container Helpers

`vo_ext::prelude` re-exports typed helpers backed by `vo-runtime`, including
`VoString`, `VoBytes`, `VoSlice`, `VoMap`, `VoArray`, `VoPtr`, and
`VoClosure`. These are low-level views over runtime objects; check their
current method set in `lang/crates/vo-runtime/src/ffi/containers.rs` before
documenting a helper as public API.

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
- Native extensions export a dynamic-library entry table. WASM extension builds
  use explicit `export_extensions!(...)` entry lists.
- The extension ABI version and fingerprint live in `vo-runtime` and are
  exported by `vo-ext`.
- If an example needs a helper not exported by `vo-ext::prelude`, verify the
  current source before treating that helper as supported.
