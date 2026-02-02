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

- Define a strict ABI for JIT functions:
  - `args_ptr` points to the beginning of the current JIT frame region in `fiber.stack`.
  - Decide whether `ret_ptr` is also in `fiber.stack` (recommended).
- Define invariants:
  - If a JIT function returns `Call/WaitIo`, the VM can reconstruct everything from `fiber.stack` + `fiber.frames` + fields in `JitContext`.
  - No Cranelift-only stack slots may be used as the source of truth for state that must be visible to the VM.

Deliverable: this note (and code asserts/unreachable points enforcing invariants).

## Milestone 2: Extend `JitContext` (vo-runtime)

- Add the minimal fields needed for the JIT to reason about fiber stack layout:
  - `stack_ptr: *mut u64` (base pointer to `fiber.stack`)
  - `jit_bp: u32` (current JIT frame base)
  - `jit_sp: u32` (current fiber.sp)
- Add corresponding `OFFSET_*` constants.
- Consider adding runtime helpers to manage frames from JIT (if JIT-to-JIT calls need to push/pop frames safely):
  - `vo_jit_push_frame(...)`
  - `vo_jit_pop_frame(...)`

## Milestone 3: Refactor VM JIT dispatch (vo-vm)

- In `dispatch_jit_call`:
  - Stop building `args: Vec<u64>` as the execution state.
  - Allocate a JIT frame region directly in `fiber.stack` (`jit_bp = fiber.sp`, `sp += local_slots`).
  - Copy caller args into the correct positions inside `fiber.stack[jit_bp + ...]`.
  - Pass `args_ptr = fiber.stack_ptr().add(jit_bp)` to the JIT function.
  - Fill `JitContext.stack_ptr/jit_bp/jit_sp` before calling.
- In `handle_jit_result(Call/WaitIo)`:
  - Remove the "copy args buffer back" step (state is already in `fiber.stack`).
  - Use `ctx.call_*` fields to push a real VM call frame (like `exec_call`).

## Milestone 4: Refactor JIT codegen (vo-jit)

- Remove the reliance on Cranelift `locals_slot` as the authoritative state.
- Update variable accessors:
  - `read_var(slot)` loads from `args_ptr + slot*8`.
  - `write_var(slot,val)` stores to `args_ptr + slot*8` (and optionally keeps SSA var for performance).
  - `var_addr(slot)` returns `args_ptr + slot*8`.
- Update call paths:
  - JIT-to-JIT calls must use VM-visible stack regions for callee args/locals.
  - Self-recursive optimization must also allocate a callee frame region rather than using Cranelift stack slots.

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
