# JIT Dispatch Redesign: Removing can_jit_to_jit_call Limitations

## Status: DESIGN PROPOSAL

---

## 1. Problem Statement

### Current Limitation: `can_jit_to_jit_call`

When JIT-compiled function A calls function B, the compiler checks `can_jit_to_jit_call(B)`. If it returns false, the compiler emits `emit_call_via_vm` which:
1. Spills all vars to fiber.stack
2. Sets call request fields in JitContext
3. Returns `JitResult::Call` — **terminates the JIT block**

After the VM executes callee B, **caller A resumes in the VM interpreter** at `resume_pc`. All remaining instructions in A are interpreted. If A is in a hot loop, **every iteration** exits JIT.

### What `can_jit_to_jit_call` Rejects

| Condition | Root Cause |
|-----------|------------|
| `has_defer` | `DeferEntry.frame_depth` needs `fiber.frames.len()`, but JIT-to-JIT fast path has no real frame |
| Blocking `CallExtern` | May return `WaitIo`, which propagates up; intermediate shadow frames have no real `CallFrame` |
| `Call` to rejected target | Recursive: if callee contains any rejected opcode, caller is also rejected |
| `CallClosure` / `CallIface` | Target unknown at compile time; always returns `JitResult::Call` |
| `SelectExec` | Callback reads `fiber.current_frame().bp` — wrong bp when no real frame |
| `PortSend` / `PortRecv` | Cross-island wakes can't be processed by `execute_func_sync` mini-scheduler |

### Impact

Any function that directly or transitively calls a function with defer, select, closures, interfaces, ports, or blocking externs **loses all JIT benefit for the caller's remaining code**.

This is devastating for real-world code. Example: a hot loop calling `sort.Slice()` (closure call) — every iteration exits JIT and resumes in the interpreter.

---

## 2. Architecture Analysis

### Two Call Mechanisms Already Exist

The codebase already has two complete mechanisms for JIT calling other functions:

**Mechanism A: JIT-to-JIT Direct Call** (`emit_jit_call_with_fallback`, JIT path)
- Caller JIT calls callee JIT natively on the same stack
- Fast: ~5ns overhead on ok path (just ctx.jit_bp/fiber_sp update)
- If callee returns non-OK: spill vars, push_frame, push_resume_point, return result → propagates up through all JIT callers, **all intermediate JIT state is lost**
- Used for: `Call` opcode when `can_jit_to_jit_call(callee)` is true

**Mechanism B: Synchronous VM Call** (`emit_jit_call_with_fallback`, VM fallback path)
- Caller JIT calls `vo_call_vm` → `execute_func_sync` → callee runs on callback fiber
- Slower: ~100-200ns (callback fiber setup, mini-scheduler)
- If callee blocks: mini-scheduler runs other goroutines; caller waits on native stack
- When callee finishes: caller JIT **continues** — native stack preserved
- Used for: `Call` opcode when callee is not yet JIT-compiled (jit_func_table entry is null)

**Key insight**: Mechanism B already exists in `emit_jit_call_with_fallback`! When `jit_func_table[func_id]` is null, the function calls `push_frame`, `vo_call_vm`, checks result, `pop_frame`, and continues in JIT. This is exactly lines 521-571 of `call_helpers.rs`.

### What's Missing

1. **CallClosure/CallIface don't use Mechanism B** — they use `emit_call_via_vm` which terminates the JIT block instead
2. **`can_jit_to_jit_call` prevents using Mechanism A for functions with defer/select/port** — but Mechanism B (with push_frame) would work
3. **`execute_func_sync` mini-scheduler can't process island commands** — causes port deadlock

---

## 3. Root Cause Analysis Per Limitation

### 3.1 `has_defer`

**Problem**: `jit_defer_push` (callbacks/defer.rs:29) does:
```rust
let frame_depth = fiber.frames.len();
```
In JIT-to-JIT fast path (Mechanism A), callee has no entry in `fiber.frames`. So `frame_depth` refers to the wrong frame, causing defers to fire at the wrong time.

**Required fix**: Callee must have a real `CallFrame` in `fiber.frames` when defer_push runs.

### 3.2 `CallClosure` / `CallIface`

**Problem**: `emit_call_closure` / `emit_call_iface` resolve `func_id` at runtime, then return `JitResult::Call`. This terminates the JIT block because there's no continuation code after the return.

**Required fix**: Use Mechanism B (vo_call_vm + continue) instead of returning `JitResult::Call`.

### 3.3 `SelectExec`

**Problem**: `jit_select_exec` (callbacks/select.rs:122) does:
```rust
let bp = fiber.current_frame().map(|f| f.bp).unwrap_or(0);
```
Without a real frame, this returns the wrong bp. Select writes results to `fiber.stack[bp + result_reg]`, corrupting memory.

Additionally, the callback writes to `fiber.stack`, but JIT-to-JIT callee's vars live on native stack (`args_ptr`). So even with correct bp, the value wouldn't be visible to JIT SSA variables.

**Required fix**: Either use Mechanism B (real frame, callee on callback fiber with its own stack), or pass bp explicitly and sync values between fiber.stack and native stack.

### 3.4 `PortSend` / `PortRecv`

**Problem**: Port callbacks may return `WaitQueue`. In Mechanism A, this propagates up through all JIT callers (acceptable loss of JIT state). In Mechanism B (`vo_call_vm` → `execute_func_sync`), the callback fiber blocks, and the mini-scheduler runs other goroutines. But the mini-scheduler doesn't process `IslandCommand::WakeFiber` from other islands → cross-island deadlock.

**Required fix**: Fix `execute_func_sync` mini-scheduler to process island commands.

### 3.5 Blocking Externs (WaitIo)

**Problem**: Blocking extern returns `WaitIo`. In Mechanism A, this propagates up. The intermediate JIT frames are only shadow frames (`resume_stack`), which `materialize_jit_frames` converts correctly. In Mechanism B, `execute_func_sync` runs the extern on a callback fiber where blocking is handled by the mini-scheduler.

**Analysis**: This actually works correctly with BOTH mechanisms. The restriction in `can_jit_to_jit_call` is overly conservative — if a function calls a blocking extern, Mechanism A handles WaitIo propagation correctly via resume_stack + materialize. The reason it was restricted: any function in the JIT-to-JIT chain that has blocking externs needs correct `resume_pc` set before the call, which `emit_call_extern` already does.

**Required fix**: Just remove the check. Both mechanisms handle this correctly.

### 3.6 Recursive `Call` Rejection

**Problem**: `can_jit_to_jit_call_impl` recursively checks callee's callees. If any transitive callee is rejected, the entire chain is rejected.

**Required fix**: This check becomes unnecessary once the individual limitations are fixed. Remove it.

---

## 4. Design

### 4.1 Strategy: Use the Right Mechanism for Each Call Type

Instead of one binary `can_jit_to_jit_call` gate, each call site selects the appropriate mechanism:

| Call Type | Mechanism | Rationale |
|-----------|-----------|-----------|
| `Call` (static target, pure compute) | A (direct JIT call) | Maximum performance |
| `Call` (static target, has blocking ops) | A with propagation | Blocking is rare; JIT loss on block is acceptable |
| `CallClosure` | B (vo_call_vm) | Target unknown; caller preserves JIT state |
| `CallIface` | B (vo_call_vm) | Target unknown; caller preserves JIT state |
| Channel/Port/Select ops | Callbacks + propagation | Direct ops in current function; JIT loss on block is acceptable |
| Blocking externs | Callbacks + propagation | Already handled by emit_call_extern |

### 4.2 Change 1: CallClosure/CallIface → New Trampolines

**Current** (`emit_call_closure`):
```
resolve func_id → set call request → return JitResult::Call  [TERMINATES JIT]
```

**New** (`emit_call_closure`):
```
prepare user args on native stack slot
call jit_closure_call_trampoline(vm, fiber, closure_ref, args, arg_count, ret, ret_count)
if result == Panic → return JitResult::Panic
copy ret values to SSA vars
continue  [JIT NOT TERMINATED]
```

#### Why New Trampolines Instead of `vo_call_vm`

`vm_call_trampoline` does flat arg copy: `args[0..param_slots] → callback_fiber.stack`. But closures and interface calls have special arg layouts:
- **Closure**: slot0 = closure_ref (for closures with captures), user args at `arg_offset`
- **Interface**: slot0 = receiver, user args at `recv_slots` offset

The existing `closure_call_trampoline` handles this via `build_closure_args`, but it returns `ClosureCallResult` (for extern callbacks), not `JitResult` (for JIT), and doesn't transfer `panic_state` to the main fiber.

#### New Trampolines

```rust
// For JIT closure calls
pub extern "C" fn jit_closure_call_trampoline(
    vm: *mut c_void,
    fiber: *mut c_void,      // main fiber, for panic_state transfer
    closure_ref: u64,
    args: *const u64,         // user args (NOT including closure_ref)
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult {
    // 1. resolve func_id from closure_ref
    // 2. build_closure_args (handles slot0 + arg_offset layout)
    // 3. trigger JIT compilation for target (like vm_call_trampoline does)
    // 4. execute_func_sync on callback fiber
    // 5. transfer panic_state to main fiber if failed
    // 6. return JitResult::Ok or JitResult::Panic
}

// For JIT interface method calls
pub extern "C" fn jit_iface_call_trampoline(
    vm: *mut c_void,
    fiber: *mut c_void,
    func_id: u32,             // already resolved by JIT via vo_iface_get_func_id
    receiver: u64,            // iface slot1
    recv_slots: u32,          // from func_def
    args: *const u64,         // user args (NOT including receiver)
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult {
    // 1. prepend receiver to args
    // 2. execute_func_sync on callback fiber
    // 3. transfer panic_state to main fiber
    // 4. return JitResult
}
```

#### No push_frame/pop_frame Needed

Unlike the existing VM fallback in `emit_jit_call_with_fallback`, the new closure/iface trampolines do NOT need `push_frame`/`pop_frame` on the main fiber:

- The callee runs entirely on the **callback fiber** which has its own frame stack
- If callee panics, `execute_func_sync` captures `panic_state` on the callback fiber
- The trampoline transfers `panic_state` to the main fiber and returns `JitResult::Panic`
- `handle_jit_result` handles Panic using the **caller's** frame (pushed by `dispatch_jit_call`), not a callee ghost frame
- No "ghost frame" in main fiber.frames means no stale frame to clean up

This is simpler and faster than the existing VM fallback path.

#### Callback Fiber: No Changes Needed

| Component | Change? | Notes |
|-----------|---------|-------|
| Callback fiber mechanism | **No change** | Pooling, lifecycle, stack management unchanged |
| `execute_func_sync` | **Enhance** | Mini-scheduler: add island command + I/O polling |
| `vm_call_trampoline` | **No change** | Regular `Call` VM fallback continues to use it |
| `closure_call_trampoline` | **No change** | Extern callbacks continue to use it |
| New `jit_closure_call_trampoline` | **Add** | JIT closure call, returns JitResult |
| New `jit_iface_call_trampoline` | **Add** | JIT iface call, returns JitResult |

### 4.3 Change 2: Remove `can_jit_to_jit_call` for `Call` Opcode

**Current** (`func_compiler.rs:call()`):
```rust
let callee_jittable = crate::can_jit_to_jit_call(target_func, self.vo_module);
if callee_jittable {
    emit_jit_call_with_fallback(...)  // A with B fallback
} else {
    emit_call_via_vm(...)  // TERMINATES JIT
}
```

**New**:
```rust
// Always use emit_jit_call_with_fallback — it already handles both JIT and VM paths
emit_jit_call_with_fallback(...)
```

This works because `emit_jit_call_with_fallback` already has both paths:
- If `jit_func_table[func_id]` is non-null → Mechanism A (direct JIT call)
- If null → Mechanism B (push_frame + vo_call_vm + pop_frame)

The callee may have defer/select/port ops. That's fine:
- If callee is JIT-compiled (table entry non-null): Mechanism A. If callee returns non-OK (e.g., WaitQueue from channel), the result propagates up correctly via resume_stack.
- If callee is not JIT-compiled (table entry null): Mechanism B. Callee runs on callback fiber, caller stays in JIT.

**But wait — defer!** If callee has defer and is called via Mechanism A (direct JIT call), the callee's `jit_defer_push` callback will read wrong `fiber.frames.len()` because no real frame was pushed for the callee.

**Solution**: For Mechanism A, we need callee to have a correct frame when it runs defer. Two options:

**Option A**: Always push a real frame on the Mechanism A fast path (for ALL JIT-to-JIT calls):
```
// Instead of just updating ctx.jit_bp/fiber_sp:
push_frame(func_id, local_slots, ...)  // push real CallFrame
call callee JIT
pop_frame(caller_bp)                   // pop CallFrame
```
Cost: +~30ns per call (two extern "C" callbacks). Affects ALL JIT-to-JIT calls including pure compute.

**Option B**: Only push a real frame for callees that need it (defer, select):
```
if callee.has_defer || callee.has_select {
    // Mechanism B: push_frame + vo_call_vm + pop_frame
} else {
    // Mechanism A: fast path (no frame push)
}
```
This is a compile-time decision using callee metadata, NOT a conservative transitive scan like the old `can_jit_to_jit_call`.

**Option B is optimal**: It preserves the fast path for pure compute while allowing all functions to be called from JIT. The check is simple and local (no recursion).

### 4.4 Change 3: Fix `execute_func_sync` Mini-Scheduler

Add island command processing to the mini-scheduler loop in `execute_func_sync`:

```rust
// In the while loop of execute_func_sync:
// Process island commands (enables cross-island port wakes)
#[cfg(feature = "std")]
if let Some(ref rx) = self.state.main_cmd_rx {
    while let Ok(cmd) = rx.try_recv() {
        if let vo_runtime::island::IslandCommand::WakeFiber { fiber_id } = cmd {
            self.scheduler.wake_fiber(crate::scheduler::FiberId::from_raw(fiber_id));
        }
    }
}

// Also handle "no work" case: wait for island commands or I/O
if !self.scheduler.has_work() && !is_callback_runnable(&self.scheduler) {
    #[cfg(feature = "std")]
    {
        if let Some(ref rx) = self.state.main_cmd_rx {
            if let Ok(cmd) = rx.recv_timeout(std::time::Duration::from_millis(10)) {
                // process cmd
            }
        }
        self.scheduler.poll_io(&mut self.state.io);
    }
}
```

This fixes the port deadlock for ALL callers of `execute_func_sync` (JIT, extern callbacks, etc.).

### 4.5 Change 4: Fix SelectExec in JIT

`jit_select_exec` currently reads bp from `fiber.current_frame()` and writes results to `fiber.stack`. Two sub-problems:

1. **bp correctness**: With Option B (defer/select callees always use Mechanism B), callee runs on a callback fiber where `fiber.current_frame()` is correct. Fixed.

2. **Value visibility**: Callee runs on callback fiber (Mechanism B). Select writes to callback fiber's stack. Return values are copied back via `execute_func_sync`'s return path. The result_reg value ends up in the ret buffer, which JIT copies to SSA vars. Fixed.

No special JIT select emission changes needed — just ensure select-containing functions use Mechanism B.

### 4.6 Decision Matrix for `Call` Opcode

| Callee Property | Path | Reason |
|----------------|------|--------|
| Pure compute, JIT-compiled | Mechanism A (direct call) | Maximum performance |
| Pure compute, not compiled | Mechanism B (vo_call_vm) | Already in emit_jit_call_with_fallback |
| Has defer | Mechanism B (vo_call_vm) | Needs real frame for frame_depth |
| Has select | Mechanism B (vo_call_vm) | Needs real frame for bp access |
| Has blocking externs | Mechanism A (direct call) | WaitIo propagation works correctly |
| Has port ops | Mechanism A (direct call) | WaitQueue propagation works correctly; mini-scheduler fix handles Mechanism B |
| Has channel ops | Mechanism A (direct call) | WaitQueue propagation works correctly |
| Calls non-jittable targets | Mechanism A with B fallback | emit_jit_call_with_fallback handles both |

**Implementation**: Replace `can_jit_to_jit_call` with `needs_vm_call`:
```rust
fn needs_vm_call(func: &FunctionDef) -> bool {
    func.has_defer || func.has_select  // Simple, local check. No recursion.
}
```

In `func_compiler.rs:call()`:
```rust
if needs_vm_call(target_func) {
    // Use Mechanism B path from emit_jit_call_with_fallback (push_frame + vo_call_vm + pop_frame)
    emit_call_via_vm_sync(...)  // NEW: synchronous VM call, JIT continues
} else {
    emit_jit_call_with_fallback(...)  // Mechanism A with B fallback (unchanged)
}
```

Where `emit_call_via_vm_sync` is the VM fallback block extracted from `emit_jit_call_with_fallback` (lines 521-571).

---

## 5. Detailed Implementation Plan

### Step 1: Fix `execute_func_sync` Mini-Scheduler
- **File**: `vo-vm/src/vm/mod.rs` (`execute_func_sync`)
- **Change**: Add island command processing and I/O polling to the mini-scheduler loop
- **Risk**: Low. Additive change, doesn't affect existing logic
- **Test**: Existing island tests should continue passing

### Step 2: Refactor `emit_call_via_vm` → `emit_call_via_vm_sync`
- **File**: `vo-jit/src/call_helpers.rs`
- **Change**: Create `emit_call_via_vm_sync` that does push_frame + vo_call_vm + pop_frame + continue (extract from emit_jit_call_with_fallback VM block)
- **Old `emit_call_via_vm`**: Keep temporarily for CallClosure/CallIface migration, remove after Step 3
- **Risk**: Medium. Must correctly handle args layout, ret values, panic propagation

### Step 3: Migrate CallClosure to `emit_call_via_vm_sync`
- **File**: `vo-jit/src/call_helpers.rs` (`emit_call_closure`)
- **Change**: Replace "set call request + return Call" with "resolve func_id + push_frame + vo_call_vm + pop_frame + continue"
- **Args**: Need closure-specific layout in fiber.stack (closure_ref at slot 0 if needed, user args at arg_offset)
- **Test**: All closure-calling tests in both VM and JIT mode

### Step 4: Migrate CallIface to `emit_call_via_vm_sync`
- **File**: `vo-jit/src/call_helpers.rs` (`emit_call_iface`)
- **Change**: Same pattern as Step 3, with interface-specific args (receiver at slot 0)
- **Test**: All interface method call tests

### Step 5: Replace `can_jit_to_jit_call` with `needs_vm_call`
- **File**: `vo-jit/src/lib.rs`, `vo-jit/src/func_compiler.rs`, `vo-jit/src/loop_compiler.rs`
- **Change**:
  - Remove `can_jit_to_jit_call` and `can_jit_to_jit_call_impl`
  - Add `needs_vm_call(func) -> bool` that only checks `has_defer || has_select`
  - In `call()`: if `needs_vm_call` → `emit_call_via_vm_sync`, else → `emit_jit_call_with_fallback`
- **Risk**: Medium. Must verify that blocking externs and port ops propagate correctly via Mechanism A
- **Test**: Full test suite in both VM and JIT mode. Especially: defer tests, select tests, port tests, channel tests, blocking extern tests

### Step 6: Delete Old Code
- Remove `emit_call_via_vm` (replaced by `emit_call_via_vm_sync`)
- Remove `CallViaVmConfig`
- Remove `CALL_KIND_CLOSURE`, `CALL_KIND_IFACE` from JitContext (no longer needed — closure/iface calls are handled entirely within JIT)
- Clean up `handle_jit_result` Call branch (simplify call_kind handling)

### Step 7 (Optional): Optimize with `has_select` Flag
- Add `has_select: bool` to `FunctionDef` in codegen (set when function contains SelectExec)
- Currently may need to scan bytecode; better to compute during codegen

### Step 8 (Phase 2): JIT-to-JIT Direct Call for CallClosure/CallIface

After Phase 1 is working, add a fast path that bypasses the trampoline when the callee is JIT-compiled and doesn't need VM special handling.

#### The Problem with Trampoline-Only

Phase 1 trampoline cost: ~200ns per closure/iface call (callback fiber setup + mini-scheduler). For hot loops calling closures (sort comparators, map/filter callbacks), this is significant.

#### Why Normal `emit_jit_call_with_fallback` Doesn't Work

For a regular `Call`, `emit_jit_call_with_fallback` creates a native stack slot of size `callee_local_slots * 8` for passing args. This works because `callee_local_slots` is a **compile-time constant**.

For CallClosure/CallIface, `func_id` is dynamic → `callee_local_slots` is unknown at compile time → **Cranelift cannot create a dynamically-sized stack slot**.

Additionally, closure arg layout depends on runtime state (capture_count, recv_slots, is_closure) with 3 possible configurations.

#### Solution: Combined Prepare Callback

Encapsulate all complex logic in a single Rust callback. The callback does: resolve func_id, check metadata, allocate in fiber.stack via push_frame, copy args with correct layout, return JIT function pointer.

```rust
#[repr(C)]
pub struct PreparedCall {
    pub jit_func_ptr: *const u8,     // null = use trampoline fallback
    pub callee_args_ptr: *mut u64,   // points into fiber.stack
}

/// Prepare a closure call for JIT-to-JIT dispatch.
/// Returns jit_func_ptr=null if callee should use trampoline instead.
extern "C" fn vo_jit_prepare_closure_call(
    ctx: *mut JitContext,
    closure_ref: u64,
    ret_reg: u32,
    ret_slots: u32,
    caller_resume_pc: u32,
    user_args: *const u64,
    user_arg_count: u32,
) -> PreparedCall {
    // 1. resolve func_id from closure_ref
    // 2. look up func_def → local_slots, recv_slots, is_closure, has_defer
    // 3. if has_defer || has_select → return { jit_func_ptr: null }
    // 4. jit_func_table[func_id] → if null → return { jit_func_ptr: null }
    // 5. push_frame(func_id, local_slots, ...) → allocate fiber.stack, push CallFrame
    // 6. call_layout() → copy slot0 + user_args with correct offsets
    // 7. return { jit_func_ptr, callee_args_ptr }
}

/// Same for interface method calls.
extern "C" fn vo_jit_prepare_iface_call(
    ctx: *mut JitContext,
    iface_slot0: u64,
    iface_slot1: u64,  // receiver
    method_idx: u32,
    ret_reg: u32,
    ret_slots: u32,
    caller_resume_pc: u32,
    user_args: *const u64,
    user_arg_count: u32,
) -> PreparedCall {
    // 1. resolve func_id from itab_cache
    // 2. check has_defer, jit_func_table
    // 3. push_frame + copy [receiver, user_args at recv_slots]
    // 4. return { jit_func_ptr, callee_args_ptr }
}
```

#### JIT IR (per call site, ~40 instructions)

```
// Store user args on native stack
for i in 0..arg_slots:
    stack_store(user_args_slot, read_var(arg_start + i))
user_args_ptr = stack_addr(user_args_slot)

// One callback does all complex work
prepared = vo_jit_prepare_closure_call(ctx, closure_ref, ret_reg, ret_slots,
                                       resume_pc, user_args_ptr, arg_count)
if prepared.jit_func_ptr == null:
    goto trampoline_path

// === JIT-to-JIT fast path ===
result = call_indirect(prepared.jit_func_ptr, ctx, prepared.callee_args_ptr, ret_ptr)
if result == Ok:
    pop_frame(ctx, caller_bp)
    copy ret values to SSA vars
    continue
else:
    spill, push_resume_point, return result

// === Trampoline fallback ===
trampoline_path:
    result = jit_closure_call_trampoline(vm, fiber, closure_ref, user_args_ptr, ...)
    if result == Panic → return Panic
    copy ret values, continue
```

#### Why push_frame Is Required Even Without defer

The callee's `local_slots` is unknown at compile time, so we cannot create a native stack slot for it. The callee JIT function expects `args_ptr` to point to a buffer of `local_slots` size. `push_frame` allocates this space in `fiber.stack` and pushes a real `CallFrame`.

Cost: push_frame (~15ns) + pop_frame (~10ns) = ~25ns. Unavoidable for dynamic targets, but much less than the ~200ns trampoline.

#### Performance Comparison

| Path | Cost | When |
|------|------|------|
| Phase 1: Trampoline only | ~200ns | Always (Phase 1) |
| Phase 2: JIT-to-JIT fast path | **~38ns** | Callee JIT-compiled + no defer/select |
| Phase 2: Trampoline fallback | ~200ns | Callee not compiled or has defer/select |
| Current (terminates JIT) | Varies (very high) | Always (current code) |

Fast path breakdown: prepare callback ~20ns + JIT call ~5ns + pop_frame ~10ns + overhead ~3ns.

**5.3x faster than trampoline for pure-compute closures** (sort comparators, map/filter callbacks, etc.) which are the vast majority of real-world closure/iface calls.

---

## 6. Performance Analysis

### What Gets Faster

**Phase 1 — CallClosure/CallIface callers**:
- Before: Every closure/interface call exits JIT. Caller interprets remaining instructions.
- After: Closure/interface call goes through trampoline (~200ns), caller continues in JIT.
- For a hot loop with N iterations calling a closure: saves N × (loop_body_interpreted_cost - loop_body_jit_cost)
- Typical speedup: 2-10x for loops with closure calls

**Phase 1 — Functions calling defer/select targets**:
- Before: Caller exits JIT at call site
- After: Call goes through Mechanism B, caller continues in JIT

**Phase 2 — Pure-compute closure/iface calls in hot loops**:
- Before (Phase 1): ~200ns trampoline per call
- After: ~38ns JIT-to-JIT direct call
- Additional 5x speedup for sort/map/filter patterns

### What Gets Slower

**Nothing measurably**:
- Functions that were previously called via Mechanism A fast path are unchanged
- Functions with defer/select that were previously rejected now go through Mechanism B instead of `emit_call_via_vm`. Both are ~similar cost, but Mechanism B preserves caller's JIT state.

### Compilation Speed

**Minimal impact**:
- Phase 1: `emit_call_via_vm_sync` generates slightly more IR than `emit_call_via_vm`, roughly neutral
- Phase 2: ~40 IR instructions per closure/iface call site (vs ~25 current), acceptable

---

## 7. Alternatives Considered

### First-Principles Architecture Comparison

The fundamental problem: JIT runs on the native call stack, which cannot be "paused" when a blocking op (channel/port/select/blocking extern) requires fiber yield. Five possible architectures:

| Metric | A: Callback Fiber | B: Propagate to VM | C: Stackful Coroutines | D: CPS / Split Functions | **Proposed (A+B hybrid)** |
|--------|-------------------|-------------------|----------------------|------------------------|--------------------------|
| Non-blocking call overhead | ~200ns (trampoline) | ~5ns (direct) | ~5ns (direct) | ~5ns (direct) | **~5ns (Mech A) / ~200ns (Mech B)** |
| Blocking call overhead | ~200ns (same) | ~100ns + JIT loss | ~50ns (stack switch) | ~20ns (state save) | **~200ns (Mech B), caller keeps JIT** |
| Caller continues JIT after call? | ✅ Yes | ❌ No | ✅ Yes | ✅ Yes | **✅ Yes** |
| Callee continues JIT after block? | ✅ Yes (callback fiber) | ❌ No (interpreter) | ✅ Yes | ✅ Yes | **✅ Yes (callback fiber)** |
| Memory overhead | Low (shared pool) | Lowest | High (per-fiber native stack) | Medium | **Low (shared pool)** |
| Implementation complexity | Low | Lowest | Medium | Extreme | **Low-Medium** |
| Cranelift compatibility | ✅ | ✅ | ⚠️ Stack alignment | ❌ No support | **✅** |
| wasm / no_std support | ✅ | ✅ | ❌ No native stack switch | ⚠️ Partial | **✅** |
| True yield (free OS thread)? | ❌ Blocks OS thread | ✅ | ✅ | ✅ | **❌ Blocks OS thread** |
| Eliminates `can_jit_to_jit_call`? | Partial | ❌ | ✅ Fully | ✅ Fully | **✅ Fully** |

### Analysis

**Architecture C (Stackful Coroutines)** is theoretically optimal: zero-overhead non-blocking calls, true yield on block, unified call model, all limitations eliminated. Go's goroutines use this model. However:
- Requires per-fiber native stack (64KB-1MB × N goroutines)
- Platform-specific stack switching (setjmp/ucontext/asm per arch)
- **Incompatible with wasm** (no native stack switching)
- **Incompatible with no_std** (needs mmap or equivalent)
- GC must scan native stacks for roots

**Architecture D (CPS/Split Functions)** compiles each function into a state machine with explicit suspend/resume points (like Rust async). Theoretically elegant but:
- Cranelift has no native support for this transformation
- Massive code size inflation
- Extreme implementation complexity
- Would essentially require writing a custom async transform pass

**Proposed design (A+B hybrid)** is optimal given Vo's constraints (wasm/no_std support, Cranelift backend):
- Pure compute calls: Mechanism A (~5ns), identical to current fast path
- Closure/iface/defer/select calls: Mechanism B (~200ns), caller stays in JIT
- No new per-fiber memory overhead
- Incremental migration from current code
- The ~200ns Mechanism B cost is dominated by the callee's actual work in practice

### Verdict

Potential for a fundamentally better design exists only if we drop wasm/no_std support (Architecture C). Within the wasm/no_std constraint, the proposed A+B hybrid is at the Pareto frontier — no other design improves one metric without worsening another.

---

## 8. Migration Safety

### Invariants That Must Hold

1. **frame_depth**: `DeferEntry.frame_depth == fiber.frames.len()` at point of defer_push
   - Guaranteed by: Mechanism B always creates real frame via push_frame before vo_call_vm
   
2. **select bp**: `fiber.current_frame().bp` must point to the select-executing function's base
   - Guaranteed by: Mechanism B runs callee on callback fiber with correct frame

3. **resume_stack consistency**: When non-OK result propagates through Mechanism A chain, resume_stack correctly tracks all intermediate frames
   - Unchanged: This mechanism is not modified

4. **Cross-island wakes**: `execute_func_sync` must be able to receive and process island commands
   - Guaranteed by: Step 1 (mini-scheduler fix)

5. **Panic propagation**: `vo_call_vm` returns Panic when callee panics; caller's JIT correctly propagates
   - Already handled in emit_jit_call_with_fallback VM block

### Regression Risk Assessment

| Area | Risk | Mitigation |
|------|------|------------|
| Defer execution order | Medium | Existing defer tests cover this extensively |
| Closure call args layout | High | Must match handle_jit_result CALL_KIND_CLOSURE logic exactly |
| Interface call receiver | High | Must match handle_jit_result CALL_KIND_IFACE logic exactly |
| Port cross-island | Low | Mini-scheduler fix is additive |
| Channel blocking | Low | No changes to channel mechanism |
| GC roots scanning | Low | resume_stack scanning unchanged; real frames are already scanned |
| OSR interaction | Low | Loop compiler unchanged; uses emit_call_via_vm for Call ops anyway |
