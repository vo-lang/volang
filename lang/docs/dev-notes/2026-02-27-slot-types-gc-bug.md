# Bug: Codegen Call Buffer Produces Wrong SlotTypes → GC Segfault

**Date**: 2026-02-27  
**Severity**: P0 — Blocks GC activation entirely  
**File**: `lang/crates/vo-codegen/src/expr/call.rs`  
**Symptom**: Segfault (SIGSEGV / exit code 139) in release build when `gc_step()` is enabled  
**Status**: Root cause identified, fix not yet implemented

---

## Background: How SlotTypes Work

Every compiled function has a `slot_types: Vec<SlotType>` array stored in `FunctionDef`. At GC time, the root scanner walks every live fiber's stack frames and uses this array to find GcRef pointers:

```rust
// gc_roots.rs
for frame in &fiber.frames {
    let func = &functions[frame.func_id as usize];
    let stack_slice = &fiber.stack[frame.bp..];
    scan_slots_by_types(gc, stack_slice, &func.slot_types);  // uses slot_types!
}
```

`scan_slots_by_types` calls `gc.mark_gray(slot_value)` for every slot typed `GcRef`. **If a slot is typed `GcRef` but actually contains an integer, `mark_gray` dereferences the integer as a pointer → segfault.**

### FuncBuilder slot allocation model

`FuncBuilder` (in `func.rs`) tracks a `next_slot: u16` cursor and a `slot_types: Vec<SlotType>` high-water-mark array:

- `alloc_slots(types)` at position `next_slot`:
  - If `slot_types[next_slot..next_slot+n] == types` → **reuse in place** (no reallocation)
  - Otherwise → **allocate fresh slots at the END** of `slot_types`
- `begin_temp_region()` / `end_temp_region()` save/restore `next_slot`, allowing temp slots to be reused across statements
- **`slot_types` never shrinks** (high-water mark). A slot's type, once set, is permanent.

This design assumes: *at any GC safepoint, slot N holds a value consistent with `slot_types[N]`.*

The call buffer optimization violates this assumption.

---

## The Bug: Call Buffer Type Mismatches

### Context: GC Safepoints

GC runs at scheduling boundaries — after each `TimesliceExpired` return from `run_fiber`. The fiber is interrupted between any two VM instructions. This means GC can scan the stack **between any two instructions** in the function body.

### Failure Mode 1 — Integer argument in GcRef-typed slot (CRASH)

In `compile_call` (`call.rs:117–132`):

```rust
let need_slots = total_arg_slots.max(ret_slots);
let args_start = if ret_slots > 0 && ret_slots >= total_arg_slots {
    dst                 // ← REUSE dst DIRECTLY as the argument buffer
} else {
    func.alloc_slots(&vec![SlotType::Value; need_slots as usize])
};

compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;
//                  ^^^^ writes argument values into args_start..args_start+arg_slots

func.emit_with_flags(Opcode::Call, ...);

if ret_slots > 0 && dst != args_start {
    func.emit_copy(dst, args_start, ret_slots);
}
```

When `ret_slots >= total_arg_slots`, `args_start = dst`.

**The problem**: `dst` was allocated by the outer expression context with `ret_slot_types`. For example, if the call returns a `string`, `slot_types[dst] = GcRef`. But before the `Call` instruction executes, `compile_method_args` writes the function's arguments into `dst`. If the first argument is an integer, the emitted bytecode stores an integer at `dst[0]` while `slot_types[dst] = GcRef`.

**The safepoint window**: The `TimesliceExpired` interrupt can fire after the Copy instruction that stores the integer argument at `dst`, but before the `Call` instruction. At that point:

```
stack[dst]        = 2           (integer argument)
slot_types[dst]   = GcRef       ← codegen set this for the return type
```

GC calls `mark_gray(2)`. `2 & 7 = 2` — misaligned, would fail the alignment check added as a workaround. But in release mode with different memory layout, the same slot may hold a different integer (e.g., `8`, `16`) that IS 8-byte aligned. `mark_gray(8)` tries to read `GcHeader` at address `8` → SIGSEGV.

**Concrete example**:
```vo
// s is string (GcRef), n is int
func foo(n int) string { ... }
s := foo(2)   // ret_slots=1 (GcRef), arg_slots=1 (Value), ret>=args → args_start=dst
              // codegen emits:  Copy(dst, src_of_2, 1)   ← integer 2 lands at GcRef slot
              //                 Call(foo, dst, ...)
              //                 (no emit_copy needed since args_start==dst)
```

### Failure Mode 2 — GcRef return in all-`Value` buffer (silent use-after-free)

In the `else` branch (`call.rs:121`):

```rust
func.alloc_slots(&vec![SlotType::Value; need_slots as usize])
```

When `total_arg_slots > ret_slots`, the buffer is typed entirely as `Value`. But if the function returns a GcRef (string, `*T`, slice, map, chan, func), that GcRef temporarily lives in a `Value`-typed slot between the `Call` instruction and the `Copy(dst, args_start, ret_slots)` instruction.

If `TimesliceExpired` fires in this window, the GC does NOT see the returned GcRef (it's in a `Value`-typed slot). If this is the only live reference to the object, GC sweeps it → the subsequent `Copy` instruction reads a dangling pointer → undefined behavior / use-after-free.

This is the same in **closure calls** (`call.rs:97`, `call.rs:177`):
```rust
let args_start = func.alloc_slots(&vec![SlotType::Value; total_arg_slots.max(ret_slots) as usize]);
```
All-`Value` types, same problem.

### Failure Mode 3 — GcRef arguments in `Value`-typed slots (silent use-after-free)

In ALL call paths that use all-`Value` buffers (failure mode 2's else branch), if any argument is a GcRef (string, pointer, interface, slice, etc.), the emitted `Copy` instructions put those GcRef values into `Value`-typed slots. If GC fires during argument setup (after the arguments are placed, before the `Call` instruction), those GcRefs are invisible to GC → premature collection of argument objects.

**The only safe path currently**: `alloc_call_buffer()` (in `call.rs:20–31`) does the right thing — it uses `ret_slot_types` for the return slots and `Value` for the remaining slots:

```rust
fn alloc_call_buffer(func: &mut FuncBuilder, buffer_size: u16, ret_slots: u16, ret_slot_types: &[SlotType]) -> u16 {
    let mut types = ret_slot_types.to_vec();
    for _ in ret_slots..buffer_size {
        types.push(SlotType::Value);
    }
    func.alloc_slots(&types)
}
```

This correctly handles the AFTER-call window (return value has correct type). But it still doesn't handle failure mode 3 (argument GcRefs in non-GcRef-typed slots). And it's not consistently used across all call compilation paths.

---

## Affected Code Locations

| File | Lines | Description |
|------|-------|-------------|
| `expr/call.rs` | 97 | Closure call: all-`Value` buffer, doesn't account for GcRef args or returns |
| `expr/call.rs` | 118–122 | Vo-to-Vo call: `args_start=dst` (failure mode 1) or all-`Value` (failure mode 2) |
| `expr/call.rs` | 177 | Non-ident closure call: all-`Value` buffer |
| `expr/call.rs` | 381, 447 | Uses `alloc_call_buffer` (partial fix for returns, still broken for arg GcRefs) |
| `expr/call.rs` | ~420+ | Method calls — need to audit |

---

## How to Reproduce

Run any test that GC scans during a function call. The simplest trigger:

```
# Enable gc_step in vm/mod.rs (uncomment self.gc_step()), then:
target/release/vo-test vm lang/test_data/gc_struct_nested.vo
# → Segmentation fault (core dumped)

# Debug build shows the panic:
# thread panicked: misaligned pointer dereference: address must be a multiple of 0x4 but is 0xfffffffffffffffa
# at: vo_runtime::gc::Gc::header_mut → mark_gray → scan_slots_by_types → scan_fibers → gc_step
```

The value `0xfffffffffffffffa` is `-6` as i64 — an integer stored in a slot typed as `GcRef`.

---

## Root Cause Summary

> **`slot_types[N]` reflects the TYPE for which slot N was originally allocated (determined at codegen compile time), but at runtime the call buffer optimization causes slot N to transiently hold a value of a DIFFERENT type (during argument passing). If GC fires during this transient window, `mark_gray` dereferences the integer as a pointer.**

The invariant violated:
```
INVARIANT: At every GC safepoint (between any two VM instructions),
           for all N: typeof(stack[N]) == slot_types[N]
```

---

## Fix Plan

### Option A — Correct slot_types per GC window (complete fix)

The call buffer is used for two phases with potentially different types:
1. **Argument phase** (before `Call`): slots 0..arg_slots hold argument values
2. **Return phase** (after `Call`): slots 0..ret_slots hold return values

A correct `slot_types` must be the **union** (widest type) across both phases. For each buffer slot `i`:

```
buffer_types[i] = union(
    arg_types[i] if i < arg_slots else Value,
    ret_slot_types[i] if i < ret_slots else Value
)

where union(GcRef, Value) = GcRef
      union(GcRef, GcRef) = GcRef
      union(Value, Value) = Value
      union(Interface0, X) = Interface0 (be conservative)
      etc.
```

Replace all ad-hoc `alloc_slots(&vec![SlotType::Value; n])` in call compilation with a helper that computes this union.

### Option B — Separate arg buffer from dst (simpler, some overhead)

Never use `dst` directly as `args_start`. Always allocate a fresh buffer. This eliminates failure mode 1 entirely.

For failure modes 2/3 (GcRef values in all-`Value` buffer during arg-passing or return), replace the all-`Value` allocation with an arg-types-aware allocation helper.

### Minimum viable fix for failure mode 1 (crash fix only)

Remove the `args_start = dst` optimization:

```rust
// call.rs, before:
let args_start = if ret_slots > 0 && ret_slots >= total_arg_slots {
    dst
} else {
    func.alloc_slots(&vec![SlotType::Value; need_slots as usize])
};

// After (always allocate fresh with correct return types):
let args_start = alloc_call_buffer(func, need_slots, ret_slots, &ret_slot_types);
```

This fixes failure mode 1 (crash) at the cost of an extra `Copy` instruction for every non-returning call. Failures 2 and 3 (silent use-after-free) remain but won't crash immediately with gc_step disabled.

---

## What Is NOT the Bug

- `gc_roots.rs` scanning logic is correct
- `typed_write_barrier` helpers are correct
- `Gc::drop` implementation is correct (no drop currently)
- `mark_gray` alignment guard is a band-aid, not a root cause
- The crash-on-original-code tests (`print_format.vo`, `gc_map_stress.vo`) — those are pre-existing unrelated bugs, confirmed by `git stash` test

---

## Current Workaround

`gc_step()` is disabled in `vm/mod.rs`. Uncomment `self.gc_step()` after this bug is fixed to activate the GC.

```rust
// lang/crates/vo-vm/src/vm/mod.rs  ~line 358
// self.gc_step();   ← uncomment this after fix
```
