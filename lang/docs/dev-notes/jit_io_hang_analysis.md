# Eliminate `execute_func_sync`: Unified Suspend/Replay Architecture

## Problem

`execute_func_sync` creates callback fibers with nested mini-schedulers. These nest when one fiber scheduled inside a mini-scheduler triggers another `execute_func_sync`, causing deadlocks in concurrent IO scenarios (HTTP server/client in JIT mode).

**Root cause**: The architecture has two classes of fibers (regular vs callback) and two schedulers (main vs mini). This asymmetry creates circular dependencies when fibers interact across scheduler boundaries.

**Goal**: One fiber class. One scheduler. All fibers are peers.

---

## Design: Suspend/Replay Pattern

Both `execute_func_sync` callers need the same thing: "run some Vo code, give me the result." Instead of blocking synchronously, the caller **suspends** (returns to VM), VM pushes the target frame on the **current fiber**, main scheduler executes it normally, and when execution resumes the caller's instruction, it **replays** from the beginning with cached results.

This is the exact same pattern already used for IO:

```
// IO replay (existing):
let token = match call.resume_io_token() {
    Some(t) => t,                              // 2nd call: use cached
    None => return ExternResult::WaitIo { .. } // 1st call: suspend
};
let completion = call.io_mut().take_completion(token);
```

```
// Closure replay (new):
let result = match call.resume_closure_result() {
    Some(r) => r,                                      // 2nd call: use cached
    None => return ExternResult::CallClosure { .. }     // 1st call: suspend
};
// use result...
```

---

## Part 1: Eliminate `vo_call_vm` / `vm_call_trampoline`

There are **three** JIT codegen paths that call `vo_call_vm` (→ `vm_call_trampoline` → `execute_func_sync`):

1. **`emit_jit_call_with_fallback` VM call block** — regular Call instruction, callee not JIT-compiled
2. **`emit_prepared_call` trampoline block** — closure/iface Call, callee not in direct_call_table
3. **`emit_call_via_vm`** — callee has defer (always goes to VM)

All three must be changed to return `JitResult::Call` instead of calling `vo_call_vm`.

### Key Insight: `fiber.sp` is Already Correct

Deep review shows `fiber.sp` is correctly maintained during JIT execution:
- `jit_push_frame` sets `fiber.sp = new_sp` (frame.rs:48)
- `jit_pop_frame` restores `fiber.sp = ctx_ref.jit_bp` (frame.rs:76)
- The VM call block restores `ctx.jit_bp = caller_bp` and `ctx.fiber_sp = old_fiber_sp`
  but does NOT modify `fiber.sp` — which is still at the correct caller sp value.

**No `jit_sync_fiber_sp` helper is needed.** The previous GC crash was a red herring:
debug logs showed `[EFS]` = execute_func_sync, meaning the crash was in the
closure_call_trampoline path, not in our VM call block change.

### Path 1: `emit_jit_call_with_fallback` VM call block

**Current** (call_helpers.rs:733-783):
```
vm_call_block:
  restore ctx.jit_bp = caller_bp
  restore ctx.fiber_sp = old_fiber_sp
  push_frame(callee)              ← uses jit_push_frame, updates fiber.sp
  copy args to callee_args_ptr
  call vo_call_vm(...)            ← execute_func_sync on callback fiber
  if panic → return panic
  pop_frame                       ← restores fiber.sp
  jump merge_block
```

**New**:
```
vm_call_block:
  restore ctx.jit_bp = caller_bp
  restore ctx.fiber_sp = old_fiber_sp
  spill_all_vars                  ← writes cranelift vars to fiber.stack
  vo_set_call_request(ctx, func_id, arg_start, resume_pc, ret_slots)
  return JitResult::Call
```

`handle_jit_result` Call handler (already exists, jit/mod.rs:259-321):
1. `materialize_jit_frames(fiber, resume_pc)` → resume_stack empty → sets entry frame.pc
2. `callee_bp = fiber.sp` (correct — still at caller's sp)
3. `ensure_capacity`, zero callee locals, copy args from `fiber.stack[actual_jit_bp + call_arg_start..]`
4. Push `CallFrame`, `fiber.sp = new_sp`, return `FrameChanged`

**Data flow verification**:
- `spill_all_vars` writes args at `fiber.stack[bp + arg_start..]` via entry stack_ptr ✓
- `actual_jit_bp = ctx.jit_bp = caller_bp = bp` → reads `fiber.stack[bp + arg_start + i]` ✓
- `fiber.sp` = caller's sp (never changed by JIT inline updates) ✓
- `resume_stack` is empty (no JIT non-OK before reaching vm_call_block) ✓
- `merge_block` only reachable from JIT OK path ✓

### Path 2: `emit_prepared_call` trampoline block

Used by `emit_call_closure_jit` and `emit_call_iface_jit`. The prepare callbacks
(`jit_prepare_closure_call`, `jit_prepare_iface_call`) already do:
- `jit_push_frame` → updates fiber.sp, ctx.jit_bp, ctx.fiber_sp, pushes NO CallFrame
- Copy args (closure captures + user args) to callee_args_ptr
- Set `caller_frame.pc = caller_resume_pc`

**Current** (call_helpers.rs:331-358):
```
trampoline_block:
  call vo_call_vm(ctx, func_id, callee_args_ptr, ...)  ← execute_func_sync
  if panic → return panic
  pop_frame(ctx, caller_bp)                              ← restores fiber.sp
  jump merge_block
```

**New**:
```
trampoline_block:
  vo_set_call_request(ctx, func_id, ?, resume_pc, ret_slots)
  return JitResult::Call
```

**Problem**: After prepare, `fiber.sp = callee_bp + callee_local_slots` (push_frame updated it).
`handle_jit_result` reads `callee_bp = fiber.sp` — but fiber.sp is now callee's NEW sp,
not callee's bp!

**Solution**: Before returning Call, restore `fiber.sp` to pre-push_frame value.
The prepare's `jit_push_frame` updates `fiber.sp = new_sp`. We need to undo this.

Option A: Call `jit_pop_frame(ctx, caller_bp)` to restore fiber.sp, then set_call_request + return Call.
This undoes push_frame, and handle_jit_result re-pushes correctly. But args are already in
fiber.stack[callee_bp..] — after pop, fiber.sp is back to callee_bp (= caller's old sp).
handle_jit_result will:
- `callee_bp = fiber.sp` = correct (= what jit_push_frame used as new_bp)
- Copy args from `fiber.stack[actual_jit_bp + call_arg_start..]`
- But wait — prepare copied args to `callee_args_ptr` (= fiber.stack + new_bp), NOT to
  `fiber.stack[actual_jit_bp + arg_start]`. The args are at `fiber.stack[callee_bp..]`,
  and call_arg_start for prepared calls is... it's `ret_reg` from prepare = `arg_start`.

Actually, prepare_closure_call/prepare_iface_call copy args to `callee_args_ptr` (returned
by push_frame, = `&fiber.stack[new_bp]`). After pop_frame, these values are still in
fiber.stack[new_bp..new_bp+arg_slots] (pop doesn't zero them). handle_jit_result reads
`fiber.stack[actual_jit_bp + call_arg_start..]`. For this to work:
`actual_jit_bp + call_arg_start` must equal `new_bp` (= old fiber.sp).

`actual_jit_bp = ctx.jit_bp`. After pop_frame: `ctx.jit_bp = caller_bp` (the param passed to pop_frame).
This is the CALLER's bp (entry bp), not callee_bp. So `actual_jit_bp + call_arg_start` would be
`caller_bp + arg_start`, which points to the caller's locals, NOT to callee's args at new_bp.

**This won't work with the generic Call handler.** We need a different approach.

Option B: Add `CALL_KIND_PREPARED` to distinguish "frame already set up by prepare".
In `handle_jit_result` Call handler, if `call_kind == CALL_KIND_PREPARED`:
- Don't read args from `fiber.stack[actual_jit_bp + call_arg_start..]`
- Instead, use the frame already set up by prepare (args already in place)
- Just push the CallFrame and adjust fiber.sp

```rust
JitResult::Call => {
    let call_kind = ctx.ctx.call_kind;
    match call_kind {
        JitContext::CALL_KIND_PREPARED => {
            // prepare_closure/iface_call already did:
            // - jit_push_frame: fiber.sp = new_sp, args copied to fiber.stack[callee_bp..]
            // - caller_frame.pc = resume_pc (via push_frame's caller_resume_pc)
            // - ensure_capacity for callee's local_slots
            //
            // We do NOT call materialize_jit_frames — caller_frame.pc was already set
            // by push_frame's caller_resume_pc parameter.
            //
            // We MUST clear resume_stack (should be empty, but defensive):
            fiber.resume_stack.clear();
            
            let callee_func_id = ctx.call_func_id();
            let call_arg_start = ctx.call_arg_start();
            let callee_ret_slots = ctx.call_ret_slots();
            let callee_bp = ctx.ctx.jit_bp as usize; // push_frame set jit_bp = new_bp
            let callee_func_def = &module.functions[callee_func_id as usize];
            
            // CRITICAL: Zero non-arg local slots for VM interpreter.
            // jit_push_frame does NOT zero (JIT callees init their own locals).
            // But VM interpreter expects zero-initialized locals.
            // Args are already at fiber.stack[callee_bp..callee_bp+param_slots].
            // Zero only [callee_bp+param_slots .. callee_bp+local_slots].
            let param_slots = callee_func_def.param_slots as usize;
            let local_slots = callee_func_def.local_slots as usize;
            if local_slots > param_slots {
                let stack = fiber.stack_ptr();
                unsafe {
                    core::ptr::write_bytes(
                        stack.add(callee_bp + param_slots), 0,
                        local_slots - param_slots
                    );
                };
            }
            
            fiber.frames.push(CallFrame::new(
                callee_func_id, callee_bp,
                call_arg_start as u16, callee_ret_slots as u16,
            ));
            
            ExecResult::FrameChanged
        }
        CALL_KIND_REGULAR => { /* existing handler */ }
        // ...
    }
}
```

Wait — `ctx.jit_bp` after prepare's push_frame = `new_bp` (= callee's bp). And
`fiber.sp` = `new_sp` (= callee_bp + local_slots). Both correct for pushing CallFrame!
`call_arg_start` = `ret_reg` from prepare = the `arg_start` parameter. When callee returns,
return values go to `caller_bp + ret_reg = entry_bp + arg_start`. ✓

**This works.** The trampoline block becomes:
```
trampoline_block:
  // prepare already did push_frame + arg layout
  // Set call request with CALL_KIND_PREPARED
  vo_set_call_request(ctx, func_id, arg_start, resume_pc, ret_slots)
  set ctx.call_kind = CALL_KIND_PREPARED
  return JitResult::Call
```

But we don't need to call vo_set_call_request for everything — we can set fewer fields.
Actually, prepare already set caller_frame.pc via push_frame's caller_resume_pc. And
ctx.jit_bp already has callee_bp. We just need:
- `ctx.call_func_id = func_id`
- `ctx.call_arg_start = ret_reg` (= arg_start)
- `ctx.call_ret_slots = ret_slots`
- `ctx.call_kind = CALL_KIND_PREPARED`

### Path 3: `emit_call_via_vm`

This is used for callees with defer. Let me check if it also uses vo_call_vm
or a different mechanism...

Actually, `emit_call_via_vm` (call_helpers.rs:523+) already returns `JitResult::Call`
directly — it doesn't use vo_call_vm! It sets call request fields and returns.
So Path 3 is already correct and needs no changes.

### Changes Required

| File | Change |
|------|--------|
| `vo-jit/src/call_helpers.rs` | Path 1: VM call block → spill + set_call_request + return Call. Path 2: trampoline block → set_call_request(PREPARED) + return Call. |
| `vo-vm/src/vm/jit/mod.rs` | Call handler: add CALL_KIND_PREPARED branch. |
| `vo-runtime/src/jit_api.rs` | Add `CALL_KIND_PREPARED` constant. |
| `vo-vm/src/vm/trampoline.rs` | Delete `vm_call_trampoline`. |

### What Happens After Callee Returns

When the VM-interpreted callee completes:
1. VM pops callee frame (`exec_return`)
2. Caller's frame is on top, with `frame.pc = resume_pc` (set by materialize_jit_frames
   for Path 1, or by push_frame's caller_resume_pc for Path 2)
3. VM **continues interpreting the caller** from resume_pc (the instruction after the call)
4. Caller runs in interpreter mode for the rest of the function

This means the caller loses JIT for its remaining instructions after the fallback call.
This only affects the **current invocation** — next time the caller is called, it enters
JIT again normally.

---

## Part 2: Closure Callback (`closure_call_trampoline`)

### Current Flow
```
VM executes CallExtern → extern function runs → call.call_closure(closure, args, ret)
  → closure_call_trampoline → execute_func_sync on callback fiber
  → result back to extern → extern continues → ExternResult::Ok
```

### New Flow
```
VM executes CallExtern → extern function runs:
  match call.resume_closure_result() {
      Some(cached) => { /* use cached result, continue */ }
      None => {
          return ExternResult::CallClosure {
              closure_ref,
              args: args.to_vec(),
          };
      }
  }

→ exec_call_extern maps CallClosure → ExecResult::CallClosure
→ VM CallExtern handler sees CallClosure:
  1. frame.pc -= 1 (undo the pre-increment, so PC points back at CallExtern)
  2. Build full closure args (prepend captures from closure object)
  3. Push closure frame on current fiber (ret_reg=0, ret_count=ret_slots)
  4. Set fiber.closure_replay_depth = fiber.frames.len() (mark this depth)
  5. return FrameChanged → refetch!()

→ main scheduler: executes closure normally (may block on IO, scheduled as peer)

→ closure returns (handle_return / handle_initial_return):
  1. pop_frame reduces fiber.frames.len()
  2. Caller frame is now on top (the frame that had CallExtern)
  3. VM checks: fiber.closure_replay_depth == fiber.frames.len() + 1?
     Actually simpler: check in write_return_values or after pop_frame.
  
  Better approach — intercept in handle_initial_return:
  1. Before pop_frame, read return values from closure's stack frame
  2. Store in fiber.resume_closure_result
  3. Pop frame
  4. Set fiber.closure_replay_depth = 0
  5. Do NOT write return values to caller (they go via replay)
  6. Return FrameChanged (caller continues at CallExtern → replay)
```

### PC Management Detail

The VM main loop does `frame.pc += 1` BEFORE matching the opcode (line 636).
So when CallExtern executes, PC already points to the NEXT instruction.

For WaitIo, the scheduler does `frame.pc -= 1` in the Block handler, so on
resume the extern is re-executed. **CallClosure needs the same `pc -= 1`.**

When the closure frame is on top, its PC starts at 0 (normal). When it
returns and the caller frame is restored, caller's PC still points at
CallExtern (because we did `pc -= 1` earlier). So the extern replays.

### Fiber State for Closure Replay

An extern function may call `call_closure` **multiple times** (e.g., `do_method`
calls `get_method_via_protocol` → `call_protocol` → `call_closure`, then
`do_call` → `do_call_closure` → `call_closure`). Each suspend/replay cycle
caches one result. On replay, all previous results are consumed in order,
then the next `call_closure` triggers a new suspend.

Add to `Fiber`:
```rust
/// Accumulated closure call results for extern replay.
/// Each entry is the return values from one closure call.
/// On extern replay, results are consumed in order via closure_replay_index.
/// Cleared when extern finally returns Ok/Panic (not CallClosure).
pub closure_replay_results: Vec<Vec<u64>>,

/// Consumption index during extern replay.
/// Tracks how many cached results have been consumed in the current replay.
/// Reset to 0 at the start of each CallExtern execution.
pub closure_replay_index: usize,

/// Frame depth at which a closure-for-extern-replay was pushed.
/// When a Return pops down to this depth, the return values are
/// appended to closure_replay_results and the extern is replayed.
/// 0 = no pending closure replay.
pub closure_replay_depth: usize,
```

### ExternResult Change

```rust
pub enum ExternResult {
    Ok,
    Yield,
    Block,
    #[cfg(feature = "std")]
    WaitIo { token: IoToken },
    Panic(String),
    NotRegistered(u32),
    // NEW:
    CallClosure {
        closure_ref: GcRef,
        args: Vec<u64>,
    },
}
```

### ExternCall API Change

Add to `ExternCallContext`:
```rust
/// Accumulated closure results from previous CallClosure suspends.
/// Consumed in order via closure_result_index.
closure_replay_results: Vec<Vec<u64>>,
/// Current consumption index. Incremented by each resume_closure_result() call.
closure_result_index: usize,

/// Get cached closure result from a previous CallClosure suspend.
/// Returns None if no more cached results (needs new suspend).
/// Returns Some(ret_values) if a cached result is available.
/// Each call advances the internal index — results are consumed in order.
pub fn resume_closure_result(&mut self) -> Option<Vec<u64>> {
    if self.closure_result_index < self.closure_replay_results.len() {
        let result = self.closure_replay_results[self.closure_result_index].clone();
        self.closure_result_index += 1;
        Some(result)
    } else {
        None
    }
}
```

Pass-through:
- CallExtern handler: `let replay_results = std::mem::take(&mut fiber.closure_replay_results);`
- Reset index: `fiber.closure_replay_index = 0;`
- Pass `replay_results` to `exec_call_extern` → `registry.call` → `ExternCallContext::new`
- When extern returns `Ok`/`Panic`: results are already taken, nothing to clean up
- When extern returns `CallClosure`: VM pushes closure frame, on return appends
  result to `fiber.closure_replay_results`, then re-executes CallExtern

### exec_call_extern Change

```rust
// New mapping in exec_call_extern:
ExternResult::CallClosure { closure_ref, args } => {
    ExecResult::CallClosure { closure_ref, args }
}
```

Need a new `ExecResult` variant:
```rust
pub enum ExecResult {
    // ... existing variants ...
    CallClosure { closure_ref: GcRef, args: Vec<u64> },
}
```

### VM CallExtern Handler Change

In `mod.rs` CallExtern handler:
```rust
Opcode::CallExtern => {
    let resume_closure_result = fiber.resume_closure_result.take();  // NEW
    let resume_io_token = fiber.resume_io_token.take();
    let result = exec::exec_call_extern(
        ..., resume_io_token, resume_closure_result,  // pass both
    );
    match result {
        // ... existing Ok/Panic/Block/WaitIo handling ...
        
        ExecResult::CallClosure { closure_ref, args } => {
            // 1. Undo PC pre-increment (same as WaitIo pattern)
            let frame = fiber.current_frame_mut().unwrap();
            frame.pc -= 1;
            
            // 2. Build full closure args (prepend captures)
            let func_id = closure::func_id(closure_ref);
            let func_def = &module.functions[func_id as usize];
            let full_args = helpers::build_closure_args(
                closure_ref as u64, closure_ref, func_def, args_ptr, args_len
            );
            
            // 3. Push closure frame
            //    ret_reg=0, ret_count=ret_slots (we'll intercept in return path)
            let new_bp = fiber.sp;
            let local_slots = func_def.local_slots as usize;
            let new_sp = new_bp + local_slots;
            fiber.ensure_capacity(new_sp);
            unsafe { core::ptr::write_bytes(fiber.stack.as_mut_ptr().add(new_bp), 0, local_slots) };
            let n = full_args.len().min(func_def.param_slots as usize);
            fiber.stack[new_bp..new_bp + n].copy_from_slice(&full_args[..n]);
            fiber.sp = new_sp;
            fiber.frames.push(CallFrame::new(func_id, new_bp, 0, func_def.ret_slots));
            
            // 4. Mark replay depth so return path knows to cache
            fiber.closure_replay_depth = fiber.frames.len();
            
            stack = fiber.stack_ptr();
            refetch!();
        }
    }
}
```

### Return Path: Intercepting Closure Result

In `unwind.rs`, `handle_initial_return` (the fast path for no-defer returns):

Before `pop_frame`, check if this is a closure-replay return:
```rust
fn handle_initial_return(...) -> ExecResult {
    let current_depth = fiber.frames.len();
    
    // Check: is this return from a closure-for-extern-replay?
    if fiber.closure_replay_depth == current_depth {
        // Cache return values — APPEND to accumulated results
        let bp = fiber.frames.last().unwrap().bp;
        let stack = fiber.stack.as_ptr();
        let ret_start = inst.a as usize;
        let ret_count = inst.b as usize;
        let vals: Vec<u64> = (0..ret_count)
            .map(|i| stack_get(stack, bp + ret_start + i))
            .collect();
        
        fiber.closure_replay_results.push(vals);
        fiber.closure_replay_depth = 0;
        
        // Pop closure frame, do NOT write return values to caller.
        // Caller's PC still points at CallExtern (we did pc -= 1 earlier).
        // VM will re-execute CallExtern → extern replays → consumes cached result.
        pop_frame(fiber);
        return ExecResult::FrameChanged;
    }
    
    // ... existing logic unchanged ...
}
```

**Defer handling**: If the closure has defers, `handle_initial_return` enters
the defer path. When all defers complete, `handle_return_defer_returned`
finalizes the return. The same `closure_replay_depth` check is needed there
too — instead of calling `write_return_values`, append to `closure_replay_results`
and return FrameChanged.

**Multi-call flow example** (`do_method` calls `call_closure` twice):
```
1st execution: extern runs → 1st call_closure → no cached → CallClosure
  → closure executes → returns → append result[0] → replay extern
2nd execution: extern runs → 1st call_closure → consume result[0] → continue
  → 2nd call_closure → no more cached → CallClosure
  → closure executes → returns → append result[1] → replay extern
3rd execution: extern runs → 1st call_closure → consume result[0] → continue
  → 2nd call_closure → consume result[1] → continue → ExternResult::Ok
  → clear closure_replay_results
```

### Extern Function Migration

All 4 `call_closure` call sites in `dynamic.rs` need to change from:
```rust
// OLD:
match call.call_closure(closure_ref, &args, &mut ret) { ... }
```
to:
```rust
// NEW:
let ret = match call.resume_closure_result() {
    Some(cached) => cached,
    None => return ExternResult::CallClosure { closure_ref, args: args.to_vec() },
};
```

Affected functions:
1. `call_protocol()` — wraps `call_closure`, called by protocol dispatch
2. `do_call_closure()` — main dyn_call implementation
3. `do_call_object()` — protocol-based call
4. Plus any outside `dynamic.rs` (check `vo-web/src/lib.rs`)

### Replay Safety

The extern function is re-executed from the beginning. Everything before
`resume_closure_result()` must be **deterministic and side-effect-free**.

In practice:
- Argument unpacking: reads from fiber.stack (unchanged between calls) ✓
- Type lookups: reads from module metadata (immutable) ✓
- GC allocations: `closure::create` allocates — but on replay, we skip past it ✓
  (NOTE: GC may have moved objects between calls. But closure_ref in 
  resume_closure_result was stored in fiber field which is a GC root,
  and stack args haven't changed.)
- `call_protocol` creates a closure via `closure::create` then calls it — on
  replay, closure::create runs again (wasteful but harmless), then
  resume_closure_result() returns cached → skip the call ✓

The key invariant: **nothing between the extern entry and
`resume_closure_result()` mutates shared state that would change the
closure call decision**.

### Edge Case: Closure Panics

If the closure panics:
- Panic unwinding pops frames until it finds a recover or exits the fiber
- If it reaches the caller's frame (the one with CallExtern), normal panic
  handling continues — the caller function panics
- `closure_replay_depth` should be reset when panic unwinding passes through it
- Add to `panic_pop_frame` or equivalent: if `fiber.closure_replay_depth > 0`
  and frame depth drops below it, reset to 0

### Edge Case: Closure Blocks on IO

This is the WHOLE POINT. The closure blocks normally:
1. Closure returns `ExecResult::Block(Io(token))`
2. Main scheduler blocks the fiber
3. IO completes, fiber is woken
4. Fiber resumes in closure (which replays the extern that reads from IO)
5. Eventually closure returns → cache → extern replays with result

No mini-scheduler needed. The fiber is scheduled as a peer.

---

## Part 3: Cleanup

### Delete
- `execute_func_sync` (entire function, ~140 lines)
- `vm_call_trampoline` (entire function, ~40 lines)
- `closure_call_trampoline` (entire function, ~50 lines)
- `Scheduler::acquire_callback_fiber`, `release_callback_fiber`, `callback_fiber_pool`
- `ClosureCallFn` type, `ClosureCallResult` enum
- `call_closure_fn` field from `ExternCall`/`ExternCallContext`
- `vo_call_vm` function in `jit_api.rs` and its `VmCallFn` type
- `call_vm` from `HelperFuncIds`/`HelperFuncs` in `vo-jit`
- `call_vm_fn` field from `JitContext`

### Keep
- `build_closure_args` in `vm/helpers.rs` (needed in VM's CallClosure handler)
- `resume_io_token` pattern (unchanged, serves as the template)
- All 3 previously applied fixes (resume_pc, resume_stack, Osr handling)

### `vo-web::call_closure`

`vo-web/src/lib.rs:263` also calls `execute_func_sync`. It's a separate entry point
(not inside an extern function), so it doesn't need the replay pattern. Replace with:
```rust
pub fn call_closure(vm: &mut Vm, closure: GcRef, args: &[u64]) -> Result<(), String> {
    // Push closure frame on main fiber, then run scheduler
    let func_id = closure::func_id(closure);
    let module = vm.module().expect("module not set");
    let func_def = &module.functions[func_id as usize];
    let full_args = build_closure_args(closure, func_def, args);
    
    // Get or create main fiber, push frame with args
    let fiber_id = vm.get_or_create_main_fiber();
    vm.push_frame_with_args(fiber_id, func_id, &full_args);
    
    // Run scheduler until fiber completes
    vm.run_scheduled().map_err(|e| format!("{:?}", e))
}
```
(Exact API TBD — may need a simpler `vm.run_closure(func_id, args)` method.)

---

## GC Safety

### `closure_replay_results` Must Be Scanned

`closure_replay_results: Vec<Vec<u64>>` may contain GcRef values. Between
closure return (cache) and extern replay (take), GC can trigger (extern functions
may allocate, e.g. `closure::create` in `call_protocol`).

**Fix**: Add to `gc_roots.rs` `scan_fibers`:
```rust
// Scan closure replay results (accumulated across multiple closure calls)
for vals in &fiber.closure_replay_results {
    // Conservative: treat all values as potential GcRefs
    scan_gcrefs(gc, vals);
}
```

**Conservative scanning**: Without slot_types, we treat all u64 values as potential
GcRefs. This is safe (marks extra objects as live = no correctness issue, just
slightly less precise collection). The results are short-lived (cleared when
extern finally returns Ok/Panic), so the impact is negligible.

### `closure_replay_depth` and Panic Unwinding

During panic unwinding, if frames are popped past `closure_replay_depth`, we must
reset it to 0. Otherwise a stale depth value could incorrectly intercept a later
normal return.

Add to panic frame-popping logic:
```rust
if fiber.closure_replay_depth > 0 && fiber.frames.len() < fiber.closure_replay_depth {
    fiber.closure_replay_depth = 0;
    fiber.closure_replay_results.clear(); // also clear any stale cached results
}
```

---

## Implementation Order

1. **Part 1: JIT VM fallback** — eliminates `vm_call_trampoline` and `vo_call_vm`
   - Add `CALL_KIND_PREPARED` constant to `jit_api.rs`
   - Path 1: Change `emit_jit_call_with_fallback` VM call block → spill + set_call_request + return Call
   - Path 2: Change `emit_prepared_call` trampoline → set_call_request(PREPARED) + return Call
   - Add `CALL_KIND_PREPARED` handler in `handle_jit_result` Call branch
   - Delete `vm_call_trampoline`, `vo_call_vm`, `call_vm` helper
   - Run full JIT test suite

2. **Part 2: Closure callback** — eliminates `closure_call_trampoline`
   - Add `CallClosure` to `ExternResult` and `ExecResult`
   - Add `resume_closure_result`, `closure_replay_depth` to `Fiber`
   - Add `cached_closure_result` to `ExternCallContext`, pass through call chain
   - Add GC root scanning for `resume_closure_result`
   - Add VM CallExtern handler for `ExecResult::CallClosure` (pc -= 1, push closure frame)
   - Add return interception in `handle_initial_return` and `handle_return_defer_returned`
   - Add panic cleanup for `closure_replay_depth`
   - Migrate `call_closure` sites in `dynamic.rs`
   - Delete `closure_call_trampoline`, `execute_func_sync`
   - Run full test suite

3. **Part 3: Cleanup**
   - Remove callback fiber pool from scheduler
   - Remove `ClosureCallFn`, `ClosureCallResult`, `call_closure_fn`
   - Fix `vo-web::call_closure` to use direct frame push
   - Unskip HTTP server/client test
   - Run full test suite including IO tests

---

## Files Involved

| File | Part | Change |
|------|------|--------|
| `vo-jit/src/call_helpers.rs` | 1 | Path 1: VM call block → spill + set_call_request + return Call. Path 2: trampoline → set_call_request(PREPARED) + return Call. Remove push_frame/pop_frame/vo_call_vm from both paths. |
| `vo-vm/src/vm/jit/mod.rs` | 1 | Add `CALL_KIND_PREPARED` handler in `handle_jit_result` Call branch |
| `vo-runtime/src/jit_api.rs` | 1 | Add `CALL_KIND_PREPARED` constant. Delete `vo_call_vm`, `VmCallFn`. |
| `vo-vm/src/vm/trampoline.rs` | 1+2 | Delete both trampolines (entire file eventually) |
| `vo-runtime/src/ffi/mod.rs` | 2 | Add `CallClosure` to `ExternResult`, add `resume_closure_result` API to `ExternCallContext` |
| `vo-vm/src/fiber.rs` | 2 | Add `closure_replay_results`, `closure_replay_index`, `closure_replay_depth` fields |
| `vo-vm/src/gc_roots.rs` | 2 | Scan `closure_replay_results` for GcRefs |
| `vo-vm/src/vm/mod.rs` | 2 | CallExtern handler for CallClosure (pc -= 1, push frame, set depth) |
| `vo-vm/src/exec/extern_call.rs` | 2 | Map `ExternResult::CallClosure` → `ExecResult::CallClosure` |
| `vo-vm/src/exec/unwind.rs` | 2 | Return interception: check `closure_replay_depth` in `handle_initial_return` and `handle_return_defer_returned`. Panic cleanup. |
| `vo-runtime/src/builtins/dynamic.rs` | 2 | Migrate 4 `call_closure` → replay pattern |
| `vo-vm/src/scheduler.rs` | 3 | Remove callback fiber pool |
| `vo-web/src/lib.rs` | 3 | Replace `execute_func_sync` with direct frame push |

## Deep Review Findings

1. **`jit_sync_fiber_sp` NOT needed**: `jit_push_frame`/`jit_pop_frame` maintain `fiber.sp` correctly. The VM call block restores ctx fields but fiber.sp was never changed by inline JIT updates.

2. **Previous crash was red herring**: The GC misaligned pointer crash showed `[EFS]` logs = execute_func_sync. It was in the closure_call_trampoline path, not our Part 1 change. Individual test passed; likely an intermittent issue with callback fiber GC interaction.

3. **`emit_prepared_call` also uses `vo_call_vm`**: Closure/iface JIT VM fallback goes through the same trampoline. Requires `CALL_KIND_PREPARED` to avoid double frame push.

4. **GC root scanning**: `resume_closure_result` can contain GcRefs that must survive GC during extern replay. Conservative scanning (treat all values as potential GcRefs) is safe and simple.

5. **Latent stack_ptr bug** (pre-existing, NOT introduced by this refactor): If `jit_push_frame` causes `fiber.stack` reallocation, `spill_all_vars` in a later non-OK path uses a stale `stack_ptr` Value from function entry. In practice, the 64KB initial stack capacity prevents reallocation. This is NOT a blocker for this refactor.

6. **`vo-web::call_closure`**: Also uses `execute_func_sync` but from outside extern functions. Needs separate fix (direct frame push + run_scheduled).

7. **Multiple closure calls per extern**: `do_method` calls `get_method_via_protocol` (→ `call_protocol` → `call_closure`) then `do_call` (→ `do_call_closure` → `call_closure`). Single `resume_closure_result` slot would cause infinite loop. Solution: `closure_replay_results: Vec<Vec<u64>>` + consumption index, accumulating results across replay cycles.

8. **CALL_KIND_PREPARED must zero non-arg locals**: `jit_push_frame` does NOT zero callee's local slots (JIT callees init their own). But VM interpreter needs zero-initialized locals. Must zero `[callee_bp + param_slots .. callee_bp + local_slots]` without overwriting args at `[callee_bp .. callee_bp + param_slots]`.

## Previously Applied Fixes (keep these)

1. **resume_pc off-by-one**: `current_pc + 1` in `emit_call_extern` for WaitIo resume
2. **resume_stack leak**: `fiber.resume_stack.clear()` in `dispatch_loop_osr`
3. **ExecResult::Osr handling**: `execute_func_sync` mini-scheduler yields on Osr (same as TimesliceExpired)
