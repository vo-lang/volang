---
description: FFI unified architecture redesign (v2)
---

# FFI Unified Architecture Redesign (v2)

**Date**: 2026-02-11  
**Status**: Implemented  
**Scope**: `vo-runtime/ffi`, `vo-vm` (VM + JIT call sites), `vo-ffi-macro`, `vo-ext`, `vo-runtime/ext_loader`, stdlib migration.

This document is a *full redesign + migration plan* for the Vo native FFI layer.

The design is written **after reading the current implementation**, and therefore explicitly preserves the following existing runtime semantics:

- **Extern replay is “extern re-execution”**: `ExternResult::CallClosure` causes the VM to run a closure on the current fiber, cache its return values on the fiber, and then **re-execute the same `CallExtern` instruction** (`pc -= 1`).
- **Panic interception at replay boundary**: panics during the closure call are intercepted during unwinding and converted into a replay flag (`replay_panicked=true`) so the extern can return an error.
- **JIT extern ABI uses an overlapping args/ret buffer**: JIT passes the same buffer for arguments and returns, and `ret_slots` is required to size the buffer.
- **I/O resume token is fiber-owned**: `resume_io_token` is taken from the fiber before calling the extern and must be visible to the extern for replay-at-PC semantics.

The goal was to remove the fundamental split (`ExternCall` vs `ExternCallContext`, `ExternFn` vs `ExternFnWithContext`, two registries/tables/macros) and replace it with a single, explicit, stable set of APIs. **This has been completed.** The migration stages below are preserved as historical record.

---

## Motivation

The pre-redesign system had the following costs:

- Two function pointer types and an enum dispatch in `ExternRegistry`.
- Two registration tables (`EXTERN_TABLE` / `EXTERN_TABLE_WITH_CONTEXT`) and two lookup paths.
- Macro complexity and an API cliff for extension authors.
- Call-site parameter explosion (VM + JIT must pass the full "world state" for every extern call).

At the same time, the existing replay/IO/JIT semantics are valuable and must remain correct.

---

## Design Goals

- **Unified authoring model**: extern authors always implement `fn(&mut ExternCallContext) -> ExternResult`.
- **Explicit call interface**: VM/JIT pass a small, explicit call description rather than 18+ parameters.
- **Fiber-owned scheduling semantics**: replay state and resume I/O token remain owned by the fiber.
- **No hidden allocations**: avoid per-call `Vec` cloning in the hot path; replay buffers are moved out of fiber once per extern execution.
- **Extension ABI direction**: enable a safe C-ABI boundary for dynamically loaded extensions.

Non-goals (for this redesign):

- Changing Vo’s slot ABI.
- Changing the replay mechanism’s user-visible semantics.

---

# Architecture

## 1) `ExternInvoke`: a compact call descriptor

`ExternInvoke` describes how to interpret a `stack: &mut [u64]` as an extern call frame.

**Key property**: args and returns may overlap (required by the current JIT path).

```rust
#[repr(C)]
pub struct ExternInvoke {
    pub extern_id: u32,

    pub bp: u32,
    pub arg_start: u16,
    pub arg_slots: u16,

    pub ret_start: u16,
    pub ret_slots: u16,
}
```

### Why `ret_slots` exists

`ret_slots` is not “dead data”. It is consumed by:

- JIT call sites: sizing the overlapping args/ret buffer (`buffer_size = max(arg_slots, ret_slots)`).
- Wrappers and helpers that implement **Result-mode** semantics: writing/zeroing only the declared return slot range.
- Debug/runtime assertions: `ret_*` accessors can `debug_assert!(n < ret_slots)` without changing release behavior.

### Invariants

- `(bp as usize) + (arg_start as usize) + (arg_slots as usize) <= stack.len()` must hold (VM/JIT guarantee).
- `(bp as usize) + (ret_start as usize) + (ret_slots as usize) <= stack.len()` must hold.
- The wrapper must read all arguments before overwriting return slots when overlap is possible.
- The macro-generated wrapper does not need to compute `ret_slots` itself; it is filled by VM/JIT when constructing `ExternInvoke`.

Additionally, the meaning of these fields is fixed:

- `arg_slots` / `ret_slots` are **u64 slot counts**, not “parameter count”.
- `arg_start` / `ret_start` are **u64 slot indices** relative to `bp`.

### Width/units constraints (non-negotiable)

- All offsets/counts in `ExternInvoke` are **u64 slot indices/counts**, not byte offsets.
- VM/JIT must guarantee the following at the call site, otherwise it is a VM/JIT bug and must panic:
  - `stack.len() <= u32::MAX as usize` (stack fits in u32 addressing)
  - `bp` fits in `u32` (already represented as `u32`)
  - `arg_start + arg_slots` does not overflow `u16` (both fields are `u16`)
  - `ret_start + ret_slots` does not overflow `u16` (both fields are `u16`)

---

## 2) `ExternWorld`: borrowed runtime state (no fiber-owned fields)

`ExternWorld` groups all borrowed “world state” needed by externs.

```rust
pub struct ExternWorld<'env> {
    pub gc: &'env mut Gc,
    pub module: &'env Module,
    pub itab_cache: &'env mut ItabCache,

    pub vm_opaque: *mut core::ffi::c_void,

    pub program_args: &'env [String],
    pub sentinel_errors: &'env mut SentinelErrorCache,

    #[cfg(feature = "std")]
    pub io: &'env mut IoRuntime,
}
```

### Opaque pointer contract

- `vm_opaque` is an **opaque handle**. Extensions must treat it as an uninterpreted token.
- Extensions must never dereference it nor assume its layout.
- It may only be passed back into runtime-provided APIs that explicitly accept it.

### Rationale

- These are naturally owned by the VM runtime state.
- Putting fiber-owned replay/IO token inside `ExternWorld` would blur responsibilities and break the “single source of truth” property.

---

## 3) `ExternFiberInputs`: one-shot fiber-derived inputs for this extern execution

This struct is produced by VM/JIT from the fiber immediately before calling an extern.

```rust
pub struct ExternFiberInputs {
    pub fiber_opaque: *mut core::ffi::c_void,

    #[cfg(feature = "std")]
    pub resume_io_token: Option<IoToken>,

    pub replay_results: Vec<Vec<u64>>,
    pub replay_panicked: bool,
}
```

### Source of truth

- `fiber_opaque` is derived from the active fiber at the call site.
- `resume_io_token` is taken from `fiber.resume_io_token`.
- `replay_results` and `replay_panicked` are taken from `fiber.closure_replay.take_for_extern()`.

### Invariants

- `replay_results` is moved out once per `CallExtern` execution.
- `replay_results` consumption is ordered.

### Replay payload layout / meaning (must be unambiguous)

Each element of `replay_results` is a single **replay payload** produced by one `ExternResult::CallClosure` round-trip.
The payload is defined as:

- **Raw u64 return slots** of the closure call, in the same slot ABI that a normal call would write to the stack.
- **No frame metadata**: it is not “the whole frame” and does not include args.
- The payload length is determined by the VM/JIT at the moment the closure returns (using the closure call’s declared return slot count).
- Consumers (wrappers/stdlib/ext code) must interpret this raw slot slice using the same slot ABI rules as normal returns.

The concrete in-memory representation is not part of the extern-facing contract.
It may be implemented as `Vec<Vec<u64>>` or `Vec<Box<[u64]>>` (see optional optimizations).

### `resume_io_token` semantics (resume vs request tokens)

`resume_io_token` is a fiber-owned **completion/resume token**:

- It exists only on the **PC re-execution** path (the second execution of the same `CallExtern` after the runtime resumes the fiber).
- On the first execution of a `CallExtern`, VM/JIT must pass `resume_io_token = None`.

Separately, `ExternResult::WaitIo { token }` carries a **request token** that the runtime stores back onto the fiber to resume later.
These are different roles and must not be conflated.

### Post-call contract (must be defined for correctness)

At the end of **every** extern execution (regardless of `ExternResult`), the runtime must validate:

- `replay_index == replay_results.len()`.

If this is not true, it means the extern violated the replay protocol (ignored cached results or attempted to
perform a non-linear replay), and the runtime must panic (fail-fast).

`resume_io_token` is a one-shot **read-only input** taken from the fiber. The call site does not write it back.
Externs must consume it via `take_resume_io_token()`.

At the end of every extern execution, if `resume_io_token.is_some()` still holds, the runtime must panic.
This includes the `ExternResult::WaitIo { .. }` path: an extern that returns `WaitIo` must still have consumed
the resume token if one was provided.

---

## 4) `ExternCallContext`: the only context extern authors see

```rust
pub struct ExternCallContext<'stack, 'env> {
    stack: &'stack mut [u64],
    invoke: ExternInvoke,

    world: ExternWorld<'env>,

    fiber_inputs: ExternFiberInputs,
    replay_index: usize,
    last_replay: Option<Vec<u64>>,  // holds the last consumed replay payload for slice view
}
```

### Rationale: why `replay_index` lives on `ExternCallContext`

`replay_index` is per-*extern-execution* consumption state, not a fiber-owned input. Keeping it on the context
ensures the fiber only owns stable replay buffers and flags.

### Required API surface

- Slot access (read args):
  - `arg_slots_total() -> u16`
  - `arg_u64(n) -> u64`, `arg_i64(n) -> i64`, `arg_f64(n) -> f64`, `arg_bool(n) -> bool`
  - `arg_ref(n) -> GcRef`
  - `arg_str(n) -> &str`, `arg_bytes(n) -> &[u8]`
  - `arg_any(n) -> InterfaceSlot` (reads 2 slots)
  - `arg_error(n) -> ErrorSlot` (reads 2 slots)

- Slot access (write returns):
  - `ret_u64(n, v)`, `ret_i64(n, v)`, `ret_f64(n, v)`, `ret_bool(n, v)`
  - `ret_ref(n, v)`
  - `ret_str(n, &str)`, `ret_bytes(n, &[u8])`, `ret_string_slice(n, &[String])`
  - `ret_any(n, InterfaceSlot)` (writes 2 slots)
  - `ret_error(n, ErrorSlot)` (writes 2 slots)
  - `ret_nil_error(n)` (writes 2 zero slots for nil error)
  - `ret_error_msg(n, &str)` (allocates a Vo error object via GC, writes 2 slots)

- World access:
  - `gc() -> &mut Gc`, `module() -> &Module`, `itab_cache_mut() -> &mut ItabCache`
  - `program_args() -> &[String]`
  - `sentinel_errors() -> &SentinelErrorCache`, `sentinel_errors_mut() -> &mut SentinelErrorCache`
  - `io_mut() -> &mut IoRuntime` (std)
  - `take_resume_io_token() -> Option<IoToken>` (std)

- VM/Fiber opaque access:
  - `vm_ptr() -> *mut core::ffi::c_void`
  - `fiber_ptr() -> *mut core::ffi::c_void`

- Replay access:
  - `is_replay_panicked() -> bool`
  - `resume_closure_result() -> Option<&[u64]>`

- GC helpers:
  - `alloc_str(&str) -> GcRef`
  - `alloc_bytes(&[u8]) -> GcRef`
  - `alloc_struct(slot_count: usize) -> GcRef`

- Post-call verification (called by `ExternRegistry::call`, not by extern authors):
  - `verify_post_call()` — panics if `replay_index != replay_results.len()` or if `resume_io_token` was not consumed.
    See Section 3 post-call contract.

All `n` parameters in `arg_*` / `ret_*` APIs are **slot indices**, not parameter indices.
For example, an `any` argument occupies 2 slots; `arg_any(0)` reads slots `[0, 1]`.
The `n` is relative to `arg_start` (for arg APIs) or `ret_start` (for ret APIs), not absolute stack offsets.

### Replay API semantics

- `resume_closure_result()` consumes `fiber_inputs.replay_results` in order via `replay_index`.
- Consumption must be **move-based** (e.g. `mem::take`) rather than `clone`, to satisfy the “no hidden allocations” goal.
- `resume_closure_result()` semantics per call:
  - If `replay_index < replay_results.len()`: consume `replay_results[replay_index]` via `mem::take`, increment `replay_index`, return `Some(&[u64])`.
  - If `replay_index == replay_results.len()`: return `None` (no more cached results; extern should issue `CallClosure`).
- `resume_closure_result()` must panic if:
  - `replay_panicked == true` (extern must check `is_replay_panicked()` first and return an error without consuming results).
  - `replay_index > replay_results.len()` (implementation bug; should never happen with correct increment logic).

To avoid exposing an owned `Vec<u64>` to extern code, the runtime should keep the last replay payload inside the
context (e.g. `last_replay: Option<Vec<u64>>`) and return a slice view (`&[u64]`).

This is a representation detail. The extern-facing contract is the slice view and the ordered-consumption rule.

---

## Why v1-style `env = world + fiber inputs` was wrong

The v1 direction that lumps replay state and resume tokens into a single borrowed `env` is a correctness hazard:

- `ExternWorld` is naturally **borrowed** from VM runtime state.
- Replay buffers / resume tokens are **fiber-owned one-shot inputs** that are moved out of the fiber for this single extern execution.

These have different lifetimes and responsibilities. Treating them as one borrowed blob:

- obscures the single source of truth (the fiber),
- makes post-call protocol verification ambiguous, and
- encourages storing per-execution consumption state (like `replay_index`) in the wrong place.

---

## 5) Unified `ExternFn` and `ExternRegistry`

```rust
pub type ExternFn = fn(&mut ExternCallContext) -> ExternResult;

pub struct ExternRegistry {
    funcs: Vec<Option<ExternFn>>,
}

impl ExternRegistry {
    pub fn call(
        &self,
        stack: &mut [u64],
        invoke: ExternInvoke,
        world: ExternWorld,
        fiber_inputs: ExternFiberInputs,
    ) -> ExternResult {
        let f = match self.funcs.get(invoke.extern_id as usize) {
            Some(Some(f)) => f,
            _ => return ExternResult::NotRegistered(invoke.extern_id),
        };
        let mut ctx = ExternCallContext::new(stack, invoke, world, fiber_inputs);
        let result = f(&mut ctx);
        // Post-call protocol verification (see Section 3)
        ctx.verify_post_call();
        result
    }
}
```

### Borrowing constraints

Implementation must ensure the borrows backing `stack` and the borrows inside `world` do not overlap.
In the current VM/JIT design this is satisfied because `stack` comes from the fiber, while `world` comes from the VM state.

### Implementation strategy (to avoid borrowck dead-ends)

At VM/JIT call sites, expect Rust's borrow checker to reject naive code that borrows `&mut fiber.stack` and
`&mut vm.state.*` in the same scope. The intended strategy is:

- take the world borrows and stack access in separate phases, and
- use raw pointers for one side of the borrow, restoring `&mut [u64]` only at the final call boundary.

This is not defensive coding; it is an explicit implementation technique for a known aliasing-free pattern.

To keep this auditable in implementation review, the intended unsafe boundary is:

- the call site may temporarily hold either:
  - `*mut [u64]` (or `*mut u64` + len) for the stack slice, or
  - a raw pointer to the VM state used to build `ExternWorld`.

and then reconstruct the final `&mut [u64]` and `ExternWorld` only at the direct `ExternRegistry::call(...)` boundary.

The required aliasing invariants are:

- the stack slice points to fiber-owned stack storage,
- `ExternWorld` references VM-owned state,
- these regions do not alias, and
- no references derived from either side outlive the call.

### Result type

`ExternResult` stays as the current enum (`Ok/Yield/Block/WaitIo/Panic/NotRegistered/CallClosure`).

---

# Macro System (v2)

The old family of macros (`vo_extern`, `vo_extern_ctx`, `vostd_extern*`) has been replaced with a unified macro set.

## Names

- Extension developers: `#[vo_fn("pkg", "Name")]`
- Stdlib developers: `#[vostd_fn("pkg", "Name")]`
- Std-only stdlib: `#[vostd_fn("pkg", "Name", std)]`

The third argument `std` marks the function as std-only. In `no_std` builds, the macro generates a panic stub
(`ExternResult::Panic("pkg::Name requires std")`).

## Mode detection

The macro inspects the Rust function signature to select one of three modes.
Detection order is **Manual → Result → Simple**. The rules must be unambiguous.

| # | Condition | Mode |
|---|-----------|------|
| 1 | First param is `&mut ExternCallContext` **AND** return type is `ExternResult` | **Manual** |
| 2 | Return type matches `Result<T, String>` (and rule 1 did not match) | **Result** |
| 3 | Everything else | **Simple** |

### Conflict handling (compile errors)

The macro must reject ambiguous or invalid signatures with clear compile errors:

- First param is `&mut ExternCallContext` but return type is **not** `ExternResult`:
  ```
  error: Manual-mode function must return `ExternResult`.
  ```
- Return type is `ExternResult` but first param is **not** `&mut ExternCallContext`:
  ```
  error: return type `ExternResult` is only valid in Manual mode.
         Add `ctx: &mut ExternCallContext` as first parameter.
  ```
- Return type is `Result<T, E>` where `E` is not `String`:
  ```
  error: Result-mode error type must be `String`. Found `MyError`.
         Use Manual mode for custom error handling.
  ```
- Unsupported parameter or return type in Simple/Result mode:
  ```
  error: unsupported parameter type: `MyStruct`.
  ```

## 1) Manual mode

```rust
#[vostd_fn("io", "Read")]
fn read(ctx: &mut ExternCallContext) -> ExternResult {
    let fd = ctx.arg_i64(slots::ARG_FD);
    // ...
}
```

The macro generates:
- A thin wrapper that delegates directly to the user function (no arg-read / ret-write codegen).
- A `mod slots { ... }` block injected into the function scope, containing `ARG_X` and `RET_Y` constants
  parsed from the corresponding `.vo` signature. This preserves the current `vo_extern_ctx` behavior.
- Registration entry (`StdlibEntry` / `linkme`).

Manual mode is for functions that need direct `ExternCallContext` access: replay, closure calls,
I/O scheduling, or multi-phase protocols.

## 2) Result mode

```rust
#[vostd_fn("strconv", "ParseFloat")]
fn parse_float(s: &str) -> Result<f64, String> {
    s.parse::<f64>().map_err(|e| e.to_string())
}
```

Result mode targets the common Vo pattern `func F(args...) (values..., error)`.

### Error type restriction

The error type `E` in `Result<T, E>` is fixed to `String`. This is intentional:
- Keeps macro parsing simple (no trait resolution at proc-macro level).
- `String` is available in `no_std + alloc` environments.
- The wrapper calls `ret_error_msg(slot, &err)` which allocates a Vo error object via GC.

Optional future extension: `Result<T, &'static str>` may be added as a separate variant.
This is deferred; it requires the macro to distinguish two `Result` forms.

### Type mapping: `Result<T, String>` → Vo return layout

The Vo function's last return value is `error` (2 slots: interface type). The `T` in `Result<T, String>`
maps to all Vo return values *before* the final `error`.

| Rust `Result<T, String>` | Vo returns | Slot layout |
|---|---|---|
| `Result<(), String>` | `error` | `[err.0, err.1]` |
| `Result<i64, String>` | `(int, error)` | `[int, err.0, err.1]` |
| `Result<f64, String>` | `(float64, error)` | `[f64, err.0, err.1]` |
| `Result<bool, String>` | `(bool, error)` | `[bool, err.0, err.1]` |
| `Result<String, String>` | `(string, error)` | `[str_ref, err.0, err.1]` |
| `Result<GcRef, String>` | `(*T, error)` | `[ref, err.0, err.1]` |
| `Result<AnySlot, String>` | `(any, error)` | `[any.0, any.1, err.0, err.1]` |
| `Result<(i64, String), String>` | `(int, string, error)` | `[int, str_ref, err.0, err.1]` |
| `Result<(i64, bool, f64), String>` | `(int, bool, float64, error)` | `[int, bool, f64, err.0, err.1]` |

General rule: **value_slots(T) + 2 (error) = total return slots**.

### Wrapper generation

For `fn parse_float(s: &str) -> Result<f64, String>`, the macro generates:

```rust
#[doc(hidden)]
pub fn __vo_strconv_ParseFloat(call: &mut ExternCallContext) -> ExternResult {
    // Phase 1: Read all args into locals
    let s = call.arg_str(0);

    // Phase 2: Call user function (last use of all borrowed args)
    let __result = parse_float(s);

    // Phase 3: Write returns (Ok path or Err path)
    match __result {
        Ok(__val) => {
            call.ret_f64(0, __val);        // slot 0: value
            call.ret_nil_error(1);          // slot 1-2: nil error
        }
        Err(__err) => {
            call.ret_f64(0, 0.0);          // slot 0: zero value
            call.ret_error_msg(1, &__err);  // slot 1-2: error from String
        }
    }
    ExternResult::Ok
}
```

For `Result<(), String>` (Vo `error`):

```rust
match __result {
    Ok(()) => {
        call.ret_nil_error(0);
    }
    Err(__err) => {
        call.ret_error_msg(0, &__err);
    }
}
```

For `Result<(i64, bool), String>` (Vo `(int, bool, error)`):

```rust
match __result {
    Ok(__val) => {
        call.ret_i64(0, __val.0);
        call.ret_bool(1, __val.1);
        call.ret_nil_error(2);
    }
    Err(__err) => {
        call.ret_i64(0, 0);
        call.ret_bool(1, false);
        call.ret_error_msg(2, &__err);
    }
}
```

### Zero-value table (Err branch)

The Err branch must write the correct zero value for each value slot before writing the error:

| Rust type | Zero-value write |
|---|---|
| `i64`, `i32`, `i16`, `i8`, `isize` | `ret_i64(n, 0)` |
| `u64`, `u32`, `u16`, `u8`, `usize` | `ret_u64(n, 0)` |
| `f64`, `f32` | `ret_f64(n, 0.0)` |
| `bool` | `ret_bool(n, false)` |
| `String` | `ret_str(n, "")` |
| `GcRef` | `ret_u64(n, 0)` |
| `AnySlot`, `InterfaceSlot` | `ret_any(n, InterfaceSlot::nil())` |
| `ErrorSlot` | `ret_error(n, ErrorSlot::nil())` |

This table should be shared with `generate_ret_write` as a single source of truth in the macro crate.

### Signature validation for Result mode

The macro validates the Rust signature against the `.vo` declaration:

1. Parameter count and types: same rules as Simple mode.
2. The Vo function's **last** return value must be `error`.
3. The Rust `T` (unwrapped from `Result<T, String>`) must match the Vo return values *before* the final `error`:
   - `T = ()` → Vo has only `error` as returns.
   - `T = single_type` → Vo has exactly `(single_type, error)`.
   - `T = (T1, T2, ...)` → Vo has exactly `(T1, T2, ..., error)`.

If the Vo function does not end with `error`, or the value types do not match, the macro emits a compile error.

### Result mode always uses `ExternCallContext`

The Err branch calls `ret_error_msg(slot, &err)`, which allocates a Vo error object (requires GC + module + itab).
Therefore, Result mode wrappers always take `&mut ExternCallContext`. After the v2 unification this is a non-issue
since all wrappers use `ExternCallContext`.

## 3) Simple mode

```rust
#[vostd_fn("math", "Floor")]
fn floor(x: f64) -> f64 {
    x.floor()
}
```

The macro generates a wrapper that reads args, calls the function, writes returns, and returns `ExternResult::Ok`.

### Supported types

**Parameters** (read from arg slots):

| Rust type | Slots | Read expression |
|---|---|---|
| `i64`, `i32`, `i16`, `i8`, `isize` | 1 | `call.arg_i64(n) as T` |
| `u64`, `u32`, `u16`, `u8`, `usize` | 1 | `call.arg_u64(n) as T` |
| `f64` | 1 | `call.arg_f64(n)` |
| `f32` | 1 | `call.arg_f64(n) as f32` |
| `bool` | 1 | `call.arg_bool(n)` |
| `GcRef` | 1 | `call.arg_ref(n)` |
| `&str` | 1 | `call.arg_str(n)` |
| `&[u8]` | 1 | `call.arg_bytes(n)` |
| `AnySlot`, `InterfaceSlot` | 2 | `call.arg_any(n)` |
| `ErrorSlot` | 2 | `call.arg_error(n)` |

**Returns** (written to ret slots):

| Rust type | Slots | Write expression |
|---|---|---|
| `i64`, `i32`, `i16`, `i8`, `isize` | 1 | `call.ret_i64(n, v as i64)` |
| `u64`, `u32`, `u16`, `u8`, `usize` | 1 | `call.ret_u64(n, v as u64)` |
| `f64` | 1 | `call.ret_f64(n, v)` |
| `f32` | 1 | `call.ret_f64(n, v as f64)` |
| `bool` | 1 | `call.ret_bool(n, v)` |
| `String` | 1 | `call.ret_str(n, &v)` |
| `GcRef` | 1 | `call.ret_ref(n, v)` |
| `Vec<u8>` | 1 | `call.ret_bytes(n, &v)` |
| `Vec<String>` | 1 | `call.ret_string_slice(n, &v)` |
| `AnySlot`, `InterfaceSlot` | 2 | `call.ret_any(n, v)` |
| `ErrorSlot` | 2 | `call.ret_error(n, v)` |

Multiple returns use Rust tuples: `fn f(a: i64) -> (bool, String)`.

Any type not in these tables causes a compile error.

## Critical wrapper rule (applies to Simple and Result modes)

The wrapper must read all arguments into locals **before** writing any return slots (JIT overlap).

This must be interpreted strictly:

- Wrappers must first materialize arguments into values that do **not** borrow from the call stack region that may
  overlap with returns.
- Wrappers must not keep references into the args region alive across any return-slot writes.

This is enforced **structurally by code generation order**, not by runtime checks:

```
Phase 1: read all args into locals           (immutable borrows of call/stack)
Phase 2: call user function                  (last use of all borrowed args → NLL ends borrows)
Phase 3: write all returns                   (mutable borrows of call/stack)
```

Rust's Non-Lexical Lifetimes (NLL, stable since edition 2021) guarantee that Phase 2 is the **last use point**
of all `&str` / `&[u8]` borrows from Phase 1. After Phase 2, no immutable borrows survive, so the mutable
borrows in Phase 3 are safe.

### Current ABI invariant: `&str` / `&[u8]` are GC-heap-backed

In the current Vo slot ABI, `arg_str(n)` reads a `GcRef` from the stack slot and dereferences it to a `&str`
pointing into **GC heap memory**, not the stack buffer itself. The same applies to `arg_bytes(n)`.

Therefore, even under JIT args/ret overlap, writing to return slots does not corrupt `&str` or `&[u8]` data,
because the data lives on the GC heap while the overlap only affects raw `u64` slot values in the stack buffer.

**If the ABI ever changes to inline small strings into stack slots**, the wrapper generator must switch to the
**Owned-then-write** strategy (clone `&str` → `String`, `&[u8]` → `Vec<u8>` before any `ret_*` writes).
This would be a macro-level change only; extern function signatures would not change.

### Strategy definitions (for reference)

- **Use-then-write**: all uses of borrowed views are completed before the first `ret_*` write.
  This is the current strategy, enforced by the Phase 1→2→3 code generation order.
- **Owned-then-write**: convert all stack-backed borrowed views into owned values before any `ret_*` writes.
  Reserved for future ABI changes where string data may live in stack slots.

Wrappers MUST NOT let any stack-backed borrowed view escape across the point where return slots are written.
Violating this rule is correctness-UB under JIT overlap.

---

# Extension ABI (v2) — Implemented

## Current implementation: ctx-pointer scheme

The extension ABI uses an `extern "C"` trampoline that receives an opaque `*mut ExternCallContext` pointer
and returns a `u32` result code:

```rust
pub type ExternFnPtr = unsafe extern "C" fn(ctx: *mut ExternCallContext) -> u32;
```

Each extension exports an `ExtensionTable` via a well-known symbol (`vo_ext_get_entries`):

```rust
#[repr(C)]
pub struct ExternEntry {
    pub name_ptr: *const u8,
    pub name_len: u32,
    pub func: ExternFnPtr,
}

#[repr(C)]
pub struct ExtensionTable {
    pub version: u32,
    pub entry_count: u32,
    pub entries: *const ExternEntry,
}
```

The `#[vo_fn]` macro generates a trampoline (`generate_ext_trampoline`) that:

1. Casts `*mut ExternCallContext` to `&mut ExternCallContext`.
2. Calls the inner Rust function inside `catch_unwind`.
3. Maps `ExternResult` variants to `ext_abi::RESULT_*` u32 codes.
4. Stores complex payloads (panic message, IO token, closure ref) on the context.

### Panic boundary

Every trampoline wraps the inner call in `std::panic::catch_unwind`. Rust panics never cross the
`extern "C"` boundary (which would be UB). A caught panic is mapped to `ext_abi::RESULT_PANIC`
and the panic message is stored on the `ExternCallContext` via `set_ext_panic_msg()`.

The host's `decode_ext_result()` reconstructs `ExternResult::Panic(msg)` from the code + stored message.
The VM then enters the standard panic unwinding pipeline (including defer/recover).

### Design rationale

The ctx-pointer scheme was chosen over the scalarized parameter scheme (passing stack_ptr, invoke fields,
world_ptr as 11+ separate arguments) for the following reasons:

- **Simplicity**: 1 parameter vs 11. Trampoline code is minimal.
- **Type safety**: Rust extensions access GC/module/itab through typed `&mut ExternCallContext` methods,
  not through opaque `*mut c_void` pointers that require manual casting.
- **Extensibility**: adding new accessor methods to `ExternCallContext` does not change the ABI signature.
- **Correctness**: all current extensions (vogui, vox) are compiled in the same cargo workspace,
  so Rust struct layout is guaranteed to match. There is no cross-compiler ABI risk.

### Limitation

The ctx-pointer scheme passes a Rust struct pointer across the dylib boundary. This is safe only when
host and extension are compiled with the **same Rust toolchain** (same struct layout). If a third-party
extension is compiled with a different Rust version, the memory layout could mismatch.

This is acceptable because all current extensions are built in the same workspace.

### Future upgrade path: C accessor API for non-Rust extensions

When non-Rust extensions (C, C++, Zig) are needed, the upgrade is **additive** — no existing API changes:

1. The trampoline signature stays `extern "C" fn(ctx: *mut ExternCallContext) -> u32`.
2. The host exports a set of `extern "C"` accessor functions that operate on an opaque `*mut void` ctx:

```c
// Host exports (vo_ext.h)
uint64_t vo_arg_u64(void* ctx, uint16_t n);
int64_t  vo_arg_i64(void* ctx, uint16_t n);
double   vo_arg_f64(void* ctx, uint16_t n);
void     vo_ret_u64(void* ctx, uint16_t n, uint64_t val);
void     vo_ret_i64(void* ctx, uint16_t n, int64_t val);
void     vo_ret_f64(void* ctx, uint16_t n, double val);
void     vo_ret_str(void* ctx, uint16_t n, const char* s, uint32_t len);
// ... etc
```

3. Non-Rust extensions call these accessors instead of dereferencing the ctx pointer directly.
4. Rust extensions continue using `&mut ExternCallContext` methods — zero changes required.

This approach avoids the scalarized-parameter complexity while providing a stable C API boundary
for non-Rust consumers. The upgrade only requires adding `ffi/c_api.rs` and a `vo_ext.h` header.

---

# Migration Plan

This migration is intentionally staged so that every stage is testable.

## Stage 0: Preparatory refactors ✅

- Introduced `ExternInvoke`, `ExternWorld`, `ExternFiberInputs` types in `ffi/call.rs`.

## Stage 1: Unify `ExternRegistry::call` signature ✅

- `ExternRegistry::call(stack, invoke, world, fiber_inputs)` implemented.
- VM and JIT call sites build invoke/world/fiber_inputs and call unified path.

## Stage 2: Unify function pointer type ✅

- Registry stores `Vec<Option<RegisteredFn>>` where `RegisteredFn` wraps both internal (`ExternFn`)
  and extension (`ExternFnPtr` via trampoline) functions.

## Stage 3: Migrate to unified `ExternCallContext` ✅

- All wrapper functions use `ExternCallContext` directly.
- All slot-access methods live on `ExternCallContext`.
- Old `ExternCall`, `ExternFnWithContext`, `ExternFnEntry` types removed.
- Dual table lookups removed.

## Stage 4: Macro replacement ✅

- `#[vo_fn]` / `#[vostd_fn]` macros implemented with three-mode detection (Manual / Result / Simple).
- All stdlib functions migrated. All extensions migrated. `vo_builtin` preserved as internal-only macro.
- Old macros (`vo_extern`, `vo_extern_ctx`, `vostd_extern`, `vostd_extern_ctx`) removed.
- Macro crate refactored: shared `emit_fn_registration` and `parse_fn_args` helpers eliminate duplication.

## Stage 5: Extension ABI v2 ✅

- `ext_loader` and `vo-ext` export `ExtensionTable` via C ABI (`extern "C"` trampoline + `catch_unwind`).
- Ctx-pointer scheme implemented (see Extension ABI section above for rationale and future upgrade path).
- `#[vo_fn]` generates `generate_ext_trampoline` + linkme `ExternEntry` registration.
- WASM path uses `StdlibEntry` fallback (no dynamic loading on WASM).

## Stage 6: Cleanup ✅

- Old macros and old FFI types removed.
- Dual `linkme` tables (`EXTERN_TABLE` / `EXTERN_TABLE_WITH_CONTEXT`) collapsed to single `EXTERN_TABLE`.
- Legacy "C ABI" terminology in comments replaced with "Extension ABI".
- Symbol names standardized: `__vo_ext_trampoline_*`, `__VO_EXT_ENTRY_*`.

### Semantic regression checklist (verified at each stage)

- replay ordering: single + nested `CallClosure` ✅
- replay panicked boundary interception ✅
- JIT args/ret overlap: multi-return + error returns ✅
- `resume_io_token` semantics across PC re-exec ✅
- `NotRegistered` behavior ✅
- Full test suite: 1930/1930 tests pass (970 VM + 960 JIT) ✅

---

# Test Plan

At minimum, each stage must pass:

- `./d.py test vm`
- `./d.py test jit` (std builds)
- `./d.py test nostd` (if affected)
- `./d.py test wasm` (if affected)

Additionally, add focused regression tests for:

- Extern replay: nested `CallClosure`, replay ordering, replay panic interception.
- JIT extern overlap: args/ret buffer correctness for multi-return + error.
- I/O resume token: extern called twice at same PC uses completion token correctly.

---

# Open Questions (resolved)

- ~~Final shape of the C ABI boundary~~ → **Resolved**: ctx-pointer scheme. See Extension ABI section.
- ~~Whether to keep any compatibility for old extension ABI~~ → **Resolved**: no compatibility needed, direct replacement.
- ~~Whether `vo_builtin` should be folded into `vostd_fn`~~ → **Resolved**: kept as separate internal-only macro (`vo_builtin` skips `.vo` signature validation, used only for compiler builtins).
- Whether to support `Result<T, &'static str>` as a second Result-mode variant (avoids allocation for static error messages). **Still open, low priority.**

---

# Deferred / Out of Scope (explicit)

The following items are valuable but not required for the unified FFI core. They are intentionally deferred:

- `StringKeyMap` zero-allocation lookup.
- HashMap-based extern registration / alternative registry backends.
- Replay payload storage representation: use `Vec<Box<[u64]>>` instead of `Vec<Vec<u64>>` to make ownership explicit.
- Island-aware extern convenience APIs (e.g. `send_island_command`). If needed later, they should be introduced
  as an additive extension to `ExternWorld` rather than baked into the minimal core.
- C accessor API for non-Rust extensions (`ffi/c_api.rs` + `vo_ext.h`). See Extension ABI future upgrade path.
