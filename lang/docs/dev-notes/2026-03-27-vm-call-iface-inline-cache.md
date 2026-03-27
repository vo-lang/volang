# VM CallIface Inline Cache (2026-03-27)

## Status: STARTED

## Scope

This note defines the first runtime-only specialization slice after the VM call ABI redesign work.

In scope:
- Interpreter `CallIface` execution in `lang/crates/vo-vm/src/exec/call.rs`
- Per-fiber callsite cache state needed by interpreter `CallIface`
- Minimal shared structure choices that do not couple the VM fast path to the current JIT implementation details
- Correctness rules around borrowed frames, stack zeroing, and GC-visible slots

Out of scope:
- `CallClosure` implementation in this slice
- JIT-side inline cache redesign
- Step 5 prepared call metadata
- Allocator / GC redesign

## Why this is the first VM slice

The compiler-side Step 4 de-thunking already removes dynamic calls when the target is statically provable.
The remaining call-heavy VM cost is now concentrated in runtime-dynamic calls, especially `CallIface`.

The current interpreter `CallIface` path still does all of the following on every execution:
- decode interface slot0 / slot1
- extract `itab_id`
- fetch `Itab`
- resolve `methods[method_idx]`
- fetch callee `FunctionDef`
- read `local_slots` and `recv_slots`
- build the borrowed frame
- zero the tail and write receiver slot0

After the borrowed-parameter-window ABI work, that repeated target resolution is now a first-order cost.
It is the right next place to make the common monomorphic case cheaper.

## Current Interpreter Path

Current `exec_call_iface` does:

1. Read interface `slot0` / `slot1` from caller stack
2. Decode `itab_id = slot0 >> 32`
3. Resolve callee `func_id` through `ItabCache`
4. Fetch `FunctionDef`
5. Assert ABI assumptions (`recv_slots == 1`, hidden receiver prefix exists)
6. Push a borrowed call frame at `arg_start - 1`
7. Zero the non-parameter tail of the callee frame
8. Write receiver into callee slot 0

Semantically this is correct.
Structurally it is too expensive for monomorphic hot callsites.

## Design Goals

- Make repeated monomorphic `CallIface` cheap in VM mode
- Preserve the current borrowed-frame ABI exactly
- Preserve fail-fast behavior when bytecode / runtime metadata is inconsistent
- Avoid hidden fallback ladders or speculative multi-shape machinery in the first slice
- Keep the design extensible to `CallClosure` later without forcing the VM to reuse JIT-specific key packing

## Cache Model

### Per-fiber, per-callsite monomorphic cache

The cache is owned by `Fiber`.
This keeps it race-free across goroutines and matches the execution-local nature of interpreter call hot paths.

Each cache entry is keyed by:
- caller `func_id`
- callsite `pc`
- observed `itab_id`
- `method_idx`

Each cache entry stores:
- resolved callee `func_id`
- callee `local_slots`

This is intentionally VM-specific.
The current JIT `DynCallIC` packs interface keys into a 32-bit value using `(itab_id << 16) | method_idx`.
That is acceptable for the current JIT implementation, but the interpreter slice should not depend on that packing assumption.
The VM cache should store the full decoded fields explicitly.

### Why callsite matters

The same `(itab_id, method_idx)` pair may be observed from many dynamic callsites.
The interpreter optimization target is not global interface dispatch memoization.
It is making a repeated hot call instruction cheap.

Using `(caller_func_id, pc)` in the key gives the interpreter a stable callsite identity without needing bytecode changes.
The dispatch loop pre-increments `frame.pc`, so the dynamic call helper must use `frame.pc - 1` as the current callsite.

## Fast Path

On each `CallIface`:

1. Read caller frame, derive `caller_func_id` and `callsite_pc`
2. Read interface `slot0` / `slot1`
3. Decode `itab_id`
4. Probe the per-fiber cache bucket for `(caller_func_id, callsite_pc, itab_id, method_idx)`
5. On hit:
   - use cached `func_id`
   - use cached `local_slots`
   - push borrowed frame
   - zero callee tail
   - write receiver into callee slot0
6. On miss:
   - use the existing `ItabCache` resolution path
   - fetch `FunctionDef`
   - validate ABI assumptions
   - populate cache entry
   - continue with the same frame setup path

The fast path must skip only repeated target-resolution work.
It must not skip frame creation, tail zeroing, or receiver placement.

## Correctness Rules

This slice must preserve all existing call-ABI invariants.

### 1. Borrowed-frame invariants stay authoritative

The cache only memoizes target resolution metadata.
It must not memoize caller-dependent stack pointers, `bp`, `sp_restore`, `scan_slots`, or return-window placement.
Those remain execution-specific and must continue to flow through `push_borrowed_call_frame`.

### 2. GC-visible slot cleanup stays mandatory

A `CallIface` cache hit still executes `zero_slots_tail_at`.
No cache fast path may expose stale borrowed caller-tail data to GC-visible scan ranges.
The earlier borrowed-frame unwind bug showed that stack cleanliness rules are part of correctness, not an optional perf detail.

### 3. ABI assumptions remain fail-fast

The first miss that populates the cache must still validate:
- `recv_slots == 1`
- `inst.b > 0`

A cache hit may reuse the validated shape because these are callee properties.
If bytecode or metadata is inconsistent, the interpreter should still fail immediately instead of silently de-optimizing.

### 4. Receiver handling is unchanged

`slot1` continues to be written directly into callee slot0.
The cache does not change interface calling convention semantics.

## Data Structure Sketch

A VM-specific cache entry can be as small as:

- `caller_func_id: u32`
- `callsite_pc: u32`
- `itab_id: u32`
- `method_idx: u16`
- `func_id: u32`
- `local_slots: u16`
- `valid: bool` or equivalent zero-invalid sentinel

The cache should use a fixed-size power-of-two table and direct-mapped indexing for the first slice.
That is enough to prove the runtime specialization concept without introducing probe chains.

Suggested indexing inputs:
- `caller_func_id`
- `callsite_pc`

This makes the table fundamentally callsite-oriented, which is what we want.
Observed receiver shape (`itab_id`) is checked inside the entry match, not used as the primary index.

## Why not reuse the JIT table directly

The JIT table exists for native fast-path dispatch and currently stores JIT-specific fields such as:
- native function pointer
- arg offset
- slot0 kind
- leafness

The interpreter does not need those fields for the first slice.
Reusing that structure directly would either waste space or force the VM path to inherit JIT-specific key/layout assumptions.
It is better to keep the first interpreter cache explicit and minimal.

A later unification is possible once both sides converge on the right abstraction.
It should not be forced prematurely.

## Implementation Plan

### Step 1. Add a VM-specific `CallIface` cache table to `Fiber`

- Add a fixed-size per-fiber table
- Lazily allocate it on first `CallIface` use
- Zero it in `Fiber::reset`

### Step 2. Add `CallIface` probe/fill helpers

- Helper to compute callsite identity from current frame
- Helper to probe cache entry
- Helper to populate cache on miss after ABI validation

### Step 3. Rewrite `exec_call_iface`

Structure the function as:
- decode current callsite + interface slots
- cache probe
- resolve/fill on miss
- single shared frame-setup tail used by both hit and miss

The implementation must not duplicate the borrowed-frame setup between hit and miss branches.
Only target resolution should differ.

## Validation

### Correctness

Run targeted both-mode tests that exercise interface calls and shared return/unwind paths.
At minimum include:
- existing interface call regressions
- defer / recover coverage that shares borrowed-frame cleanup rules
- any monomorphic interface method-expression regression already added during Step 4

### Performance

Primary metric:
- `benchmarks/call-iface-mono`

Secondary checks:
- `benchmarks/call-iface-poly4`
- broader VM benchmark sweep after correctness is stable

Expected result:
- strong improvement on monomorphic interface-call workloads
- limited or mixed change on poly4 workloads, which is acceptable for a monomorphic first slice

## Follow-up

If this lands cleanly, the next adjacent steps are:
1. apply the same callsite-cache idea to `CallClosure`
2. add per-function zeroing plans so dynamic-call hits also pay less frame-init cost
3. layer Step 5 prepared call metadata on top of the stabilized runtime-specialized call path
