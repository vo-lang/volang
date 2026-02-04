# JIT Opcode Support Plan

## Goal

Make **all functions JIT-compilable** with **full native JIT execution** where possible. No simple fallbacks - implement proper helper functions that execute the operation and return control to JIT.

## Design Principle

**不要简单方案** - Each opcode should be properly implemented with:
1. A runtime helper function that performs the actual operation
2. Proper handling of all result cases (Ok, Panic, WaitIo, Wake)
3. JIT continues execution after the helper returns (when possible)

## Current Status

Currently `is_func_jittable()` rejects entire functions containing these opcodes:
- DeferPush, ErrDeferPush, Recover
- GoStart, GoIsland
- ChanSend, ChanRecv, ChanClose
- SelectBegin, SelectSend, SelectRecv, SelectExec
- IslandNew
- PortSend, PortRecv, PortClose

## Implementation Batches

### Batch 1: Non-blocking Operations

**Opcodes**: `IslandNew`, `ChanClose`, `PortClose`

#### IslandNew

**Implementation**:
- Add `vo_island_new(ctx) -> (handle, result)` helper
- Helper creates island, registers with scheduler via callback
- Returns island handle to JIT
- JIT stores handle and continues

```rust
// jit_api.rs
pub extern "C" fn vo_island_new(ctx: *mut JitContext) -> u64 {
    // Call into VM via callback to create island with proper scheduler registration
    let create_fn = ctx.create_island_fn.expect("create_island_fn not set");
    create_fn(ctx)
}

// translate.rs
IslandNew => {
    let result = call vo_island_new(ctx);
    write_var(inst.a, result);
    // continue JIT execution
}
```

#### ChanClose

**Implementation**:
- Add `vo_chan_close(ctx, chan) -> JitResult` helper
- Helper handles: nil check, already-closed check, wake waiting fibers
- Returns: Ok (success), Panic (nil/closed)
- If needs to wake fibers, sets info in ctx for VM to process after JIT returns

```rust
// jit_api.rs
pub extern "C" fn vo_chan_close(ctx: *mut JitContext, chan: u64) -> JitResult {
    if chan == 0 { return set_panic(ctx, "close of nil channel"); }
    let state = channel::get_state(chan);
    if state.is_closed() { return set_panic(ctx, "close of closed channel"); }
    state.close();
    // Store wake list in ctx for VM to process
    let waiters = collect_waiters(state);
    ctx.pending_wakes = waiters;
    JitResult::Ok
}

// translate.rs  
ChanClose => {
    let chan = read_var(inst.a);
    let result = call vo_chan_close(ctx, chan);
    if result == Panic { return Panic; }
    // continue JIT execution
}
```

#### PortClose

**Implementation**: Same pattern as ChanClose.

**Estimated effort**: 3-4 hours

---

### Batch 2: Channel Send/Recv

**Opcodes**: `ChanSend`, `ChanRecv`

#### ChanSend

**Implementation**:
- Add `vo_chan_send(ctx, chan, val_ptr, val_slots, fiber_id) -> JitResult` helper
- Helper performs send operation:
  - Nil check → Panic
  - Closed check → Panic
  - Try send: DirectSend (wake receiver), Buffered (success), WouldBlock (yield)
- Returns: Ok, Panic, or WaitIo
- On DirectSend: store wake target in ctx.pending_wake

```rust
// jit_api.rs
pub extern "C" fn vo_chan_send(
    ctx: *mut JitContext,
    chan: u64,
    val_ptr: *const u64,
    val_slots: u32,
) -> JitResult {
    if chan == 0 { return set_panic(ctx, "send on nil channel"); }
    let state = channel::get_state(chan);
    let value: Box<[u64]> = read_slots(val_ptr, val_slots);
    
    match state.try_send(value, capacity) {
        SendResult::DirectSend(receiver_id) => {
            ctx.pending_wake = Some(receiver_id);
            JitResult::Ok
        }
        SendResult::Buffered => JitResult::Ok,
        SendResult::WouldBlock(value) => {
            state.register_sender(ctx.fiber_id, value);
            JitResult::WaitIo
        }
        SendResult::Closed => set_panic(ctx, "send on closed channel"),
    }
}

// func_compiler.rs
ChanSend => {
    let chan = read_var(inst.a);
    // Copy values to native stack buffer
    let val_ptr = copy_to_buffer(inst.b, inst.flags);
    let result = call vo_chan_send(ctx, chan, val_ptr, inst.flags);
    
    check_result(result);  // Panic → return Panic, WaitIo → spill + return WaitIo
    // Ok → continue JIT execution
}
```

#### ChanRecv

**Implementation**: Similar to ChanSend, but writes received value to destination slots.

```rust
pub extern "C" fn vo_chan_recv(
    ctx: *mut JitContext,
    chan: u64,
    dst_ptr: *mut u64,
    elem_slots: u32,
    has_ok: bool,
) -> JitResult {
    if chan == 0 { return set_panic(ctx, "receive on nil channel"); }
    let state = channel::get_state(chan);
    
    match state.try_recv() {
        (RecvResult::Success(woke_sender), Some(val)) => {
            write_slots(dst_ptr, &val, elem_slots);
            if has_ok { write_ok(dst_ptr, elem_slots, true); }
            if let Some(id) = woke_sender { ctx.pending_wake = Some(id); }
            JitResult::Ok
        }
        (RecvResult::WouldBlock, _) => {
            state.register_receiver(ctx.fiber_id);
            JitResult::WaitIo
        }
        (RecvResult::Closed, _) => {
            zero_slots(dst_ptr, elem_slots);
            if has_ok { write_ok(dst_ptr, elem_slots, false); }
            JitResult::Ok
        }
    }
}
```

**Estimated effort**: 4-5 hours

---

### Batch 3: Port Send/Recv

**Opcodes**: `PortSend`, `PortRecv`

**Implementation**: Same pattern as Channel, but with cross-island serialization.

- `vo_port_send` needs access to struct_metas and runtime_types for packing
- `vo_port_recv` needs same for unpacking
- Wake targets are WaiterInfo (island_id, fiber_id) instead of just fiber_id

```rust
pub extern "C" fn vo_port_send(
    ctx: *mut JitContext,
    port: u64,
    val_ptr: *const u64,
    val_slots: u32,
) -> JitResult {
    // Pack value for cross-island transfer
    let packed = pack_slots(ctx.gc, val_ptr, val_slots, ctx.struct_metas, ctx.runtime_types);
    
    match port::try_send(port, packed) {
        SendResult::DirectSend(receiver) => {
            ctx.pending_remote_wake = Some(receiver);
            JitResult::Ok
        }
        SendResult::Buffered => JitResult::Ok,
        SendResult::WouldBlock(value) => {
            let waiter = WaiterInfo { island_id: ctx.island_id, fiber_id: ctx.fiber_id };
            port::register_sender(port, waiter, value);
            JitResult::WaitIo
        }
        SendResult::Closed => set_panic(ctx, "send on closed port"),
    }
}
```

**Estimated effort**: 4-5 hours

---

### Batch 4: Goroutine Start

**Opcodes**: `GoStart`, `GoIsland`

#### GoStart

**Implementation**:
- Add `vo_go_start(ctx, func_id, is_closure, closure_ref, args_ptr, arg_slots)` helper
- Helper creates new fiber via scheduler callback
- Copies args to new fiber's stack
- Returns immediately (fire-and-forget)

```rust
pub extern "C" fn vo_go_start(
    ctx: *mut JitContext,
    func_id: u32,
    is_closure: bool,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_slots: u32,
) {
    let spawn_fn = ctx.spawn_fiber_fn.expect("spawn_fiber_fn not set");
    spawn_fn(ctx, func_id, is_closure, closure_ref, args_ptr, arg_slots);
}

// func_compiler.rs
GoStart => {
    let func_id = extract_func_id(inst);
    let is_closure = (inst.flags & 1) != 0;
    let closure_ref = if is_closure { read_var(inst.a) } else { 0 };
    let args_ptr = var_addr(inst.b);
    call vo_go_start(ctx, func_id, is_closure, closure_ref, args_ptr, inst.c);
    // continue JIT execution immediately
}
```

#### GoIsland

**Implementation**: Similar but spawns on remote island via command channel.

**Estimated effort**: 3-4 hours

---

### Batch 5: Defer/ErrDefer

**Opcodes**: `DeferPush`, `ErrDeferPush`

**Implementation**:
- Add `vo_defer_push(ctx, closure_ref, is_errdefer)` helper
- Helper pushes to fiber's defer_state via callback
- At Return: call `vo_run_defers(ctx)` to execute all pending defers
- If defer panics, `vo_run_defers` returns Panic

```rust
pub extern "C" fn vo_defer_push(
    ctx: *mut JitContext,
    closure_ref: u64,
    is_errdefer: bool,
) {
    let push_fn = ctx.defer_push_fn.expect("defer_push_fn not set");
    push_fn(ctx, closure_ref, is_errdefer);
}

pub extern "C" fn vo_run_defers(ctx: *mut JitContext) -> JitResult {
    let run_fn = ctx.run_defers_fn.expect("run_defers_fn not set");
    run_fn(ctx)  // Returns Ok or Panic
}

// func_compiler.rs
DeferPush => {
    let closure = read_var(inst.a);
    call vo_defer_push(ctx, closure, false);
    self.has_defer = true;
    // continue JIT execution
}

Return => {
    if self.has_defer {
        spill_all_vars();
        let result = call vo_run_defers(ctx);
        if result == Panic { return Panic; }
    }
    // normal return
}
```

**Estimated effort**: 4-5 hours

---

### Batch 6: Recover

**Opcode**: `Recover`

**Implementation**:
- Add `vo_recover(ctx) -> (slot0, slot1)` helper
- Helper queries fiber's panic state, returns recovered value or nil
- This CAN be properly implemented in JIT

```rust
pub extern "C" fn vo_recover(ctx: *mut JitContext, dst_ptr: *mut u64) {
    let recover_fn = ctx.recover_fn.expect("recover_fn not set");
    recover_fn(ctx, dst_ptr);  // Writes 2 slots (interface value) to dst_ptr
}

// func_compiler.rs
Recover => {
    let dst_ptr = var_addr(inst.a);
    call vo_recover(ctx, dst_ptr);
    // Load result from dst_ptr to variables
    let slot0 = load(dst_ptr, 0);
    let slot1 = load(dst_ptr, 8);
    write_var(inst.a, slot0);
    write_var(inst.a + 1, slot1);
    // continue JIT execution
}
```

**Estimated effort**: 2-3 hours

---

### Batch 7: Select

**Opcodes**: `SelectBegin`, `SelectSend`, `SelectRecv`, `SelectExec`

**Implementation**:
- Add select state to JitContext (case list)
- `vo_select_begin`: Initialize select state
- `vo_select_add_send/recv`: Register cases
- `vo_select_exec`: Execute select, may return Ok (with selected case) or WaitIo

```rust
pub extern "C" fn vo_select_begin(ctx: *mut JitContext) {
    ctx.select_cases.clear();
}

pub extern "C" fn vo_select_add_send(
    ctx: *mut JitContext,
    chan: u64,
    val_ptr: *const u64,
    val_slots: u32,
) {
    ctx.select_cases.push(SelectCase::Send { chan, val_ptr, val_slots });
}

pub extern "C" fn vo_select_add_recv(
    ctx: *mut JitContext,
    chan: u64,
    dst_ptr: *mut u64,
    elem_slots: u32,
    has_ok: bool,
) {
    ctx.select_cases.push(SelectCase::Recv { chan, dst_ptr, elem_slots, has_ok });
}

pub extern "C" fn vo_select_exec(ctx: *mut JitContext, has_default: bool) -> JitResult {
    // Try each case, if any succeeds return Ok with selected index
    // If all would block and has_default, select default
    // If all would block and no default, register on all channels and return WaitIo
    let exec_fn = ctx.select_exec_fn.expect("select_exec_fn not set");
    exec_fn(ctx, has_default)  // Sets ctx.select_result_case on success
}

// func_compiler.rs
SelectExec => {
    let has_default = (inst.flags & 1) != 0;
    spill_all_vars();  // Always spill before select (may WaitIo)
    let result = call vo_select_exec(ctx, has_default);
    if result == WaitIo {
        set_resume_pc(current_pc);
        return WaitIo;
    }
    if result == Panic { return Panic; }
    // Read selected case index and jump to handler
    let case_idx = load ctx.select_result_case;
    emit_switch(case_idx, case_targets);
}
```

**Estimated effort**: 6-8 hours

---

## Summary

| Batch | Opcodes | Effort | Cumulative |
|-------|---------|--------|------------|
| 1 | IslandNew, ChanClose, PortClose | 3-4h | 3-4h |
| 2 | ChanSend, ChanRecv | 4-5h | 7-9h |
| 3 | PortSend, PortRecv | 4-5h | 11-14h |
| 4 | GoStart, GoIsland | 3-4h | 14-18h |
| 5 | DeferPush, ErrDeferPush | 4-5h | 18-23h |
| 6 | Recover | 2-3h | 20-26h |
| 7 | Select* (4个) | 6-8h | 26-34h |

**Total**: ~4-5 days of work

## After Completion

1. **Delete** `is_func_jittable()` function entirely
2. **All functions** will be JIT-compiled
3. **Unsupported paths** fallback to VM at runtime
4. **Result**: Maximum JIT coverage, graceful degradation

## Architecture: Callback-based VM Integration

JIT helpers need access to VM state (scheduler, fiber, gc). Instead of passing all state through JitContext, use **callback functions** set by the VM before JIT execution.

```rust
// JitContext additions
pub struct JitContext {
    // ... existing fields ...
    
    // VM callbacks (set by VM before JIT execution)
    pub vm_callbacks: *mut VmCallbacks,
}

// Defined in vo-vm, passed to JIT
pub struct VmCallbacks {
    // Batch 1
    pub create_island: extern "C" fn(cb: *mut VmCallbacks) -> u64,
    pub chan_close: extern "C" fn(cb: *mut VmCallbacks, chan: u64) -> i32,  // returns JitResult
    pub port_close: extern "C" fn(cb: *mut VmCallbacks, port: u64) -> i32,
    
    // Batch 2-3
    pub chan_send: extern "C" fn(cb: *mut VmCallbacks, chan: u64, val_ptr: *const u64, val_slots: u32) -> i32,
    pub chan_recv: extern "C" fn(cb: *mut VmCallbacks, chan: u64, dst_ptr: *mut u64, elem_slots: u32, has_ok: bool) -> i32,
    pub port_send: extern "C" fn(cb: *mut VmCallbacks, port: u64, val_ptr: *const u64, val_slots: u32) -> i32,
    pub port_recv: extern "C" fn(cb: *mut VmCallbacks, port: u64, dst_ptr: *mut u64, elem_slots: u32, has_ok: bool) -> i32,
    
    // Batch 4
    pub go_start: extern "C" fn(cb: *mut VmCallbacks, func_id: u32, is_closure: bool, closure: u64, args_ptr: *const u64, arg_slots: u32),
    pub go_island: extern "C" fn(cb: *mut VmCallbacks, island: u64, closure: u64),
    
    // Batch 5-6
    pub defer_push: extern "C" fn(cb: *mut VmCallbacks, closure: u64, is_errdefer: bool),
    pub run_defers: extern "C" fn(cb: *mut VmCallbacks) -> i32,
    pub recover: extern "C" fn(cb: *mut VmCallbacks, dst_ptr: *mut u64),
    
    // Batch 7
    pub select_begin: extern "C" fn(cb: *mut VmCallbacks),
    pub select_add_send: extern "C" fn(cb: *mut VmCallbacks, chan: u64, val_ptr: *const u64, val_slots: u32),
    pub select_add_recv: extern "C" fn(cb: *mut VmCallbacks, chan: u64, dst_ptr: *mut u64, elem_slots: u32, has_ok: bool),
    pub select_exec: extern "C" fn(cb: *mut VmCallbacks, has_default: bool) -> i32,
    pub select_result_case: i32,  // Set by select_exec
    
    // Internal state (owned by VM)
    scheduler: *mut Scheduler,
    fiber: *mut Fiber,
    gc: *mut Gc,
    module: *const Module,
}
```

## JIT Helper Functions (jit_api.rs)

Thin wrappers that call into VM callbacks:

```rust
// Batch 1
pub extern "C" fn vo_island_new(ctx: *mut JitContext) -> u64 {
    let cb = unsafe { &mut *(*ctx).vm_callbacks };
    (cb.create_island)(cb as *mut _)
}

pub extern "C" fn vo_chan_close(ctx: *mut JitContext, chan: u64) -> i32 {
    let cb = unsafe { &mut *(*ctx).vm_callbacks };
    (cb.chan_close)(cb as *mut _, chan)
}

pub extern "C" fn vo_port_close(ctx: *mut JitContext, port: u64) -> i32 {
    let cb = unsafe { &mut *(*ctx).vm_callbacks };
    (cb.port_close)(cb as *mut _, port)
}

// Batch 2
pub extern "C" fn vo_chan_send(ctx: *mut JitContext, chan: u64, val_ptr: *const u64, val_slots: u32) -> i32 {
    let cb = unsafe { &mut *(*ctx).vm_callbacks };
    (cb.chan_send)(cb as *mut _, chan, val_ptr, val_slots)
}

pub extern "C" fn vo_chan_recv(ctx: *mut JitContext, chan: u64, dst_ptr: *mut u64, elem_slots: u32, has_ok: bool) -> i32 {
    let cb = unsafe { &mut *(*ctx).vm_callbacks };
    (cb.chan_recv)(cb as *mut _, chan, dst_ptr, elem_slots, has_ok)
}

// ... similar for other batches
```
