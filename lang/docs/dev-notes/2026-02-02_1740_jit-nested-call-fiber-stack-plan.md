---
description: JIT nested Call/WaitIo redesign (fiber.stack ABI)
---

# Background

We hit a correctness bug in JIT mode when running `toml.Decode()` followed by `toml.Marshal()`: after decode, marshal returns an empty `[]byte`. The same program works in VM mode. Each function (`Decode` or `Marshal`) also works when executed alone in JIT mode.

The failing call chain (simplified):

- `toml.Decode()` -> parser code (JIT)
- `toml.Marshal()` -> `writeTable()` (JIT, recursive)
- `writeTable()` calls `writeValue()`
- `writeValue()` contains type assertions (`switch v := x.(type)`), so it is not JIT-compatible and must execute in the VM.

This creates a critical control-flow shape:

- JIT function `writeTable()` (A)
  - self-recursive JIT call to `writeTable()` (B)
    - needs VM fallback (`JitResult::Call`) to execute `writeValue()`

This is a *nested* `JitResult::Call` emerging from inside a JIT-to-JIT call chain.

## Root cause (current architecture limitation)

The current JIT architecture uses a "shadow-frame" ABI:

- VM dispatch (`dispatch_jit_call`) builds an `args: Vec<u64>` buffer and passes it to the JIT function as `args_ptr`.
- JIT code may also allocate Cranelift stack slots (`locals_slot`, `arg_slot`, `ret_slot`) to store locals/arguments.
- When the JIT needs to hand off execution to the VM (non-jittable callee, blocking I/O), it returns `JitResult::Call`/`WaitIo` and spills state.

However, the VM can only restore state from memory it owns and understands (`fiber.stack`, `fiber.frames`).

When a nested JIT-to-JIT call uses a Cranelift stack slot as the callee `args_ptr`, and the callee triggers `JitResult::Call`, the spill state ends up in memory that is *not* part of `fiber.stack`. The VM then reads parameters from `fiber.stack[jit_bp + call_arg_start]`, which still contains old values/zeros, leading to `nil` arguments inside `writeValue()`.

The broader issue is that the current design does not model intermediate JIT frames in the VM, so nested `Call/WaitIo` cannot be restored correctly.

## Goal

Redesign the JIT calling convention so that *all JIT frames live in `fiber.stack`*, making the VM and JIT share a single source of truth for locals and arguments. This enables correct handling of nested `JitResult::Call/WaitIo` across JIT-to-JIT recursion.

# Proposed design (Plan B)

## High-level idea

- Treat `fiber.stack` as the canonical storage for **all** JIT locals (including parameters).
- JIT code may keep hot values in SSA registers, but the authoritative memory is `fiber.stack`.
- Any time we need to return `JitResult::Call/WaitIo`, the VM must be able to read the current state from `fiber.stack` without relying on private Cranelift stack slots.
- JIT-to-JIT calls must allocate a proper VM-visible frame region in `fiber.stack` (similar to how the interpreter pushes frames).

# Development plan

## Milestone 1: Freeze ABI + invariants

### JIT Function Signature (unchanged)
```c
JitResult jit_func(JitContext* ctx, u64* args_ptr, u64* ret_ptr);
```

### ABI Rules

1. **`args_ptr` MUST point to `fiber.stack[frame.bp]`**
   - For top-level JIT call: VM sets `args_ptr = fiber.stack_ptr + jit_bp`
   - For nested JIT-to-JIT call: caller allocates frame via runtime helper, gets new `args_ptr`

2. **`ret_ptr` points to a temporary buffer**
   - Caller allocates (Cranelift stack slot or heap)
   - Return values copied to `fiber.stack[caller_bp + ret_reg]` after call completes

3. **`fiber.stack` is the single source of truth for locals**
   - No Cranelift `locals_slot` as authoritative storage
   - `read_var(slot)` = `load(args_ptr + slot * 8)`
   - `write_var(slot, val)` = `store(val, args_ptr + slot * 8)`

### Invariants

- If JIT returns `Call/WaitIo`, VM can restore state from `fiber.stack` + `fiber.frames` alone
- JIT-to-JIT calls MUST push a frame via runtime helper (not use private Cranelift slots)
- `emit_variable_spill()` becomes a no-op (already in `fiber.stack`)

Deliverable: this note + code implementation.

## Milestone 2: Extend `JitContext` (vo-runtime)

Add fields for JIT to access fiber stack:

```rust
// In JitContext:
pub stack_ptr: *mut u64,      // fiber.stack.as_mut_ptr()
pub stack_cap: u32,           // fiber.stack.len() (capacity)
pub jit_bp: u32,              // current frame base pointer
```

Add runtime helper for JIT-to-JIT frame management:

```rust
/// Push a new frame for JIT-to-JIT call.
/// Returns: args_ptr for the new frame (fiber.stack_ptr + new_bp)
/// 
/// This function:
/// 1. Ensures fiber.stack has capacity for local_slots
/// 2. Zeros the new frame region
/// 3. Updates fiber.sp
/// 4. Pushes CallFrame to fiber.frames
/// 5. Updates ctx.jit_bp
#[no_mangle]
pub extern "C" fn vo_jit_push_frame(
    ctx: *mut JitContext,
    func_id: u32,
    local_slots: u32,
    ret_reg: u32,
    ret_slots: u32,
) -> *mut u64;

/// Pop the current JIT frame after callee returns.
/// Restores ctx.jit_bp to caller's bp.
#[no_mangle]
pub extern "C" fn vo_jit_pop_frame(ctx: *mut JitContext);
```

Note: `stack_cap` allows JIT to do inline capacity check before pushing frame.

## Milestone 3: Refactor VM JIT dispatch (vo-vm)

### `dispatch_jit_call` changes:

```rust
// Before (wrong):
let mut args: Vec<u64> = vec![0u64; local_slots];
// copy caller args to args buffer
jit_func(ctx, args.as_mut_ptr(), ret.as_mut_ptr());

// After (correct):
let jit_bp = fiber.sp;
fiber.ensure_capacity(jit_bp + local_slots);
fiber.sp = jit_bp + local_slots;
// copy caller args directly to fiber.stack[jit_bp..]
let args_ptr = unsafe { fiber.stack_ptr().add(jit_bp) };
// fill ctx fields
ctx.stack_ptr = fiber.stack_ptr();
ctx.stack_cap = fiber.stack.len() as u32;
ctx.jit_bp = jit_bp as u32;
jit_func(ctx, args_ptr, ret.as_mut_ptr());
```

### `handle_jit_result(Call)` changes:

```rust
// Before: copy args back to fiber.stack (no longer needed)
// for (i, &val) in args.iter().enumerate() {
//     fiber.stack[jit_bp + i] = val;
// }

// After: state is already in fiber.stack, just push callee frame
// (existing code for pushing CallFrame is mostly unchanged)
```

### `handle_jit_result(WaitIo)` changes:

Same as Call - no copy needed, state already in fiber.stack.

## Milestone 4: Refactor JIT codegen (vo-jit)

### Remove `locals_slot` as source of truth

```rust
// Before: locals_slot is a Cranelift stack slot
fn emit_prologue(&mut self) {
    let slot = self.builder.create_sized_stack_slot(...);
    self.locals_slot = Some(slot);
    // Load params from args into locals_slot
}

// After: args_ptr IS the frame in fiber.stack, no separate locals_slot needed
fn emit_prologue(&mut self) {
    // args_ptr (param[1]) already points to fiber.stack[bp]
    // Params are already there, nothing to copy
    // Just initialize SSA vars from args_ptr for register allocation
}
```

### Variable access changes

```rust
// read_var: load from fiber.stack via args_ptr
fn read_var(&mut self, slot: u16) -> Value {
    let args_ptr = self.args_ptr_param();  // block_params[1]
    self.builder.ins().load(types::I64, MemFlags::trusted(), args_ptr, (slot as i32) * 8)
}

// write_var: store to fiber.stack via args_ptr
fn write_var(&mut self, slot: u16, val: Value) {
    let args_ptr = self.args_ptr_param();
    self.builder.ins().store(MemFlags::trusted(), val, args_ptr, (slot as i32) * 8);
}

// var_addr: compute address in fiber.stack
fn var_addr(&mut self, slot: u16) -> Value {
    let args_ptr = self.args_ptr_param();
    self.builder.ins().iadd_imm(args_ptr, (slot as i64) * 8)
}
```

### JIT-to-JIT call changes

```rust
// Before: use Cranelift stack slots for callee args
let arg_slot = builder.create_sized_stack_slot(...);
let args_ptr = builder.ins().stack_addr(arg_slot, 0);
call_indirect(jit_func_ptr, [ctx, args_ptr, ret_ptr]);

// After: use runtime helper to allocate frame in fiber.stack
let push_frame_func = self.helpers.push_frame.unwrap();
let callee_args_ptr = builder.ins().call(push_frame_func, [ctx, func_id, local_slots, ret_reg, ret_slots]);
// Copy args to callee_args_ptr
call_indirect(jit_func_ptr, [ctx, callee_args_ptr, ret_ptr]);
// Pop frame after call
let pop_frame_func = self.helpers.pop_frame.unwrap();
builder.ins().call(pop_frame_func, [ctx]);
```

### `emit_variable_spill` becomes no-op

```rust
fn emit_variable_spill(&mut self) {
    // No-op: variables are already in fiber.stack
    // (Previously: copy locals_slot to args buffer)
}
```

## Milestone 5: Tests + validation

- Add/keep regression tests for:
  - TOML: decode then marshal (JIT)
  - Nested recursion with VM fallback inside (JIT)
- Run:
  - `./d.py test jit` full suite
  - targeted TOML tests
- Do a basic performance smoke test:
  - one serialization benchmark (toml/json)
  - one pure compute benchmark

# Notes

- This is a non-trivial cross-crate refactor (`vo-runtime`, `vo-vm`, `vo-jit`).
- Correctness comes first: the system must fail fast (panic) if an old path still relies on Cranelift-only storage for VM-visible state.
