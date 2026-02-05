# JIT Defer/ErrDefer/Recover Implementation Design

## Executive Summary

This document provides a comprehensive design for implementing `DeferPush`, `ErrDeferPush`, and `Recover` opcodes in the JIT compiler. Currently these opcodes are blocked by `is_func_jittable()` check, causing any function containing them to fall back to VM interpretation.

---

## Part 1: VM Implementation Analysis

### 1.1 Core Data Structures

#### DeferEntry (`vo-vm/src/fiber.rs`)

```rust
pub struct DeferEntry {
    pub func_id: u32,           // Function to call (0 if closure)
    pub closure: Option<GcRef>, // Closure if func_id == 0
    pub args: Vec<Slot>,        // Captured arguments
    pub frame_depth: usize,     // frames.len() when defer was registered
    pub is_errdefer: bool,      // true for errdefer (only run on error/panic)
    pub registered_at_generation: u64, // panic_generation when registered
}
```

**Key fields:**
- `frame_depth`: Determines which defers belong to which function. When unwinding, defers are collected for all frames from current to target.
- `is_errdefer`: Distinguishes between `defer` and `errdefer`. Only matters during unwinding mode selection.
- `registered_at_generation`: Critical for `recover()` semantics - a defer can only recover a panic that started *after* the defer was registered.

#### UnwindingState (`vo-vm/src/fiber.rs`)

```rust
pub struct UnwindingState {
    pub pending: Vec<DeferEntry>,           // Defers waiting to execute (LIFO)
    pub target_depth: usize,                // Target frame depth to unwind to
    pub mode: UnwindingMode,                // Return or Panic mode
    pub current_defer_generation: u64,      // Generation of currently executing defer
    pub return_values: Option<ReturnValues>, // Return values (None for void/fresh panic)
    pub caller_ret_reg: u16,                // Where to write return values
    pub caller_ret_count: usize,            // How many slots caller expects
}

pub enum UnwindingMode {
    Return,  // Normal return with pending defers
    Panic,   // Panic unwinding - execute defers, check for recover()
}

pub enum ReturnValues {
    Stack { vals: Vec<u64>, slot_types: Vec<SlotType> },
    Heap { gcrefs: Vec<u64>, slots_per_ref: Vec<usize> },
}
```

**Key semantics:**
- `mode`: Determines whether we're in normal return unwinding (errdefers skipped) or panic unwinding (all defers run).
- `target_depth`: The final frame depth we're unwinding to (usually caller's depth).
- `current_defer_generation`: Used by nested defers - if a panic defer registers a new defer, that nested defer inherits this generation, allowing it to potentially recover the same panic.
- `return_values`: `None` for void functions or fresh panic, `Some(Stack{..})` for stack returns, `Some(Heap{..})` for heap-escaped named returns.

#### PanicState (`vo-vm/src/fiber.rs`)

```rust
pub enum PanicState {
    Recoverable(InterfaceSlot),  // Can be caught by recover()
    RecoverableTrap { kind: RuntimeTrapKind, value: InterfaceSlot }, // Runtime trap
    Fatal,  // Cannot be recovered (e.g., stack overflow)
}
```

### 1.2 DeferPush/ErrDeferPush Execution

**VM main loop** (`vo-vm/src/vm/mod.rs:1452-1458`):
```rust
Opcode::DeferPush => {
    let generation = fiber.effective_defer_generation();
    exec::exec_defer_push(stack, bp, &fiber.frames, &mut fiber.defer_stack, &inst, &mut self.state.gc, generation);
}
Opcode::ErrDeferPush => {
    let generation = fiber.effective_defer_generation();
    exec::exec_err_defer_push(stack, bp, &fiber.frames, &mut fiber.defer_stack, &inst, &mut self.state.gc, generation);
}
```

**Implementation** (`vo-vm/src/exec/defer.rs`):

```rust
fn push_defer_entry(
    stack: *const Slot,
    bp: usize,
    frames: &[CallFrame],
    defer_stack: &mut Vec<DeferEntry>,
    inst: &Instruction,
    gc: &mut Gc,
    is_errdefer: bool,
    panic_generation: u64,
) {
    // inst.a = func_id or closure_reg
    // inst.b = arg_start
    // inst.c = arg_count
    // inst.flags: bit 0 = is_closure
    
    let is_closure = (inst.flags & 1) != 0;
    let (func_id, closure) = if is_closure {
        let closure_ref = stack_get(stack, bp + inst.a as usize) as GcRef;
        // Clone closure so it's not affected by stack changes
        let cloned = gc.ptr_clone(closure_ref);
        (0, Some(cloned))
    } else {
        (inst.a as u32 | ((inst.flags as u32 >> 1) << 16), None)
    };
    
    // Capture arguments
    let arg_count = inst.c as usize;
    let arg_start = bp + inst.b as usize;
    let args: Vec<Slot> = (0..arg_count)
        .map(|i| stack_get(stack, arg_start + i))
        .collect();
    
    defer_stack.push(DeferEntry {
        func_id,
        closure,
        args,
        frame_depth: frames.len(),
        is_errdefer,
        registered_at_generation: panic_generation,
    });
}
```

**Instruction encoding:**
- `inst.a`: Function ID (low 16 bits) or closure register
- `inst.b`: Argument start register
- `inst.c`: Argument count
- `inst.flags`: Bit 0 = is_closure, Bits 1-7 = func_id high bits

### 1.3 Recover Execution

**VM main loop** (`vo-vm/src/vm/mod.rs:1464-1466`):
```rust
Opcode::Recover => {
    exec::exec_recover(stack, bp, fiber, &inst);
}
```

**Implementation** (`vo-vm/src/exec/defer.rs:103-123`):

```rust
pub fn exec_recover(stack: *mut Slot, bp: usize, fiber: &mut Fiber, inst: &Instruction) {
    // Check if we're in valid recover context
    if !fiber.is_direct_defer_context() {
        // Not in direct defer context - return nil without consuming panic
        stack_set(stack, bp + inst.a as usize, 0);
        stack_set(stack, bp + inst.a as usize + 1, 0);
        return;
    }

    // Try to take the panic value
    let recovered = fiber.take_recoverable_panic();
    let val = recovered.unwrap_or(InterfaceSlot::nil());
    
    // Store result (interface{} = 2 slots)
    stack_set(stack, bp + inst.a as usize, val.slot0);
    stack_set(stack, bp + inst.a as usize + 1, val.slot1);
    
    // If recover succeeded, switch from Panic to Return mode
    if recovered.is_some() {
        fiber.switch_panic_to_return_mode();
    }
}
```

**Critical helper: `is_direct_defer_context()`** (`vo-vm/src/fiber.rs`):

```rust
pub fn is_direct_defer_context(&self) -> bool {
    match &self.unwinding {
        Some(state) if state.mode == UnwindingMode::Panic => {
            // Condition 1: Must be exactly at defer execution depth
            // (one frame deeper than target = the defer function itself)
            if !state.at_defer_boundary(self.frames.len()) {
                return false;
            }
            // Condition 2: Defer must have been registered BEFORE the panic
            // (registered_at < panic_generation)
            state.current_defer_generation < self.panic_generation
        }
        _ => false,
    }
}
```

**Key insight**: `recover()` only works when:
1. We're in Panic unwinding mode
2. The current frame is the defer function itself (exactly target_depth + 1)
3. The defer was registered before the current panic started

### 1.4 Panic Generation Mechanism

The `panic_generation` counter prevents defers registered *during* panic handling from recovering the panic that triggered them.

**`effective_defer_generation()`** (`vo-vm/src/fiber.rs`):

```rust
pub fn effective_defer_generation(&self) -> u64 {
    match &self.unwinding {
        Some(state) if state.mode == UnwindingMode::Panic => {
            // During panic unwinding, nested defers inherit the current defer's generation
            // This allows them to potentially recover the same panic
            state.current_defer_generation
        }
        _ => self.panic_generation,
    }
}
```

**Scenario:**
```go
func f() {
    defer func() {  // registered at generation 0
        defer func() {  // registered during panic unwinding, but inherits generation 0
            recover()   // CAN recover because 0 < 1
        }()
        panic("nested")
    }()
    panic("outer")  // panic_generation becomes 1
}
```

---

## Part 2: Unwinding System

### 2.1 Entry Points

The unwinding system has two main entry points in the VM loop:

**Return instruction** (`vo-vm/src/vm/mod.rs:1077-1088`):
```rust
Opcode::Return => {
    let result = if fiber.is_direct_defer_context() {
        // Defer returned during Panic unwinding
        exec::handle_panic_unwind(fiber, module)
    } else {
        // Normal return or Return-mode defer return
        let func = &module.functions[func_id as usize];
        let is_error_return = (inst.flags & 1) != 0;
        exec::handle_return(fiber, &inst, func, module, is_error_return)
    };
    // ...
}
```

### 2.2 Normal Return Flow (`handle_return`)

**Path 1: Initial return (no unwinding in progress)**

```rust
fn handle_initial_return(fiber: &mut Fiber, inst: &Instruction, func: &FunctionDef, module: &Module, is_error_return: bool) -> ExecResult {
    let defers = collect_defers(&mut fiber.defer_stack, fiber.frames.len());
    
    if defers.is_empty() {
        // Fast path: no defers, just return
        pop_frame_and_store_result(fiber, inst, func);
        ExecResult::FrameChanged
    } else {
        // Slow path: need to execute defers
        let include_errdefers = compute_include_errdefers(fiber, inst, func, is_error_return);
        
        fiber.unwinding = Some(UnwindingState {
            pending: filter_errdefers(defers, include_errdefers),
            target_depth: fiber.frames.len() - 1,
            mode: UnwindingMode::Return,
            current_defer_generation: fiber.panic_generation,
            return_values: Some(ReturnValues::Stack { ... }),
            caller_ret_reg: frame.ret_reg,
            caller_ret_count: frame.ret_count as usize,
        });
        
        execute_next_defer(fiber, module)
    }
}
```

**Path 2: Defer returned in Return mode**

```rust
fn handle_return_defer_returned(fiber: &mut Fiber, inst: &Instruction, func: &FunctionDef, module: &Module) -> ExecResult {
    // Collect any new defers registered by the just-returned defer
    let new_defers = collect_defers(&mut fiber.defer_stack, fiber.frames.len());
    
    // Prepend to pending (LIFO order)
    if let Some(ref mut state) = fiber.unwinding {
        prepend_defers(&mut state.pending, new_defers, state.kind.includes_errdefers());
    }
    
    // Pop the defer frame
    fiber.frames.pop();
    fiber.sp = fiber.frames.last().map_or(0, |f| f.bp + ...);
    
    // Execute next pending defer or complete unwinding
    if has_more_defers(&fiber.unwinding) {
        execute_next_defer(fiber, module)
    } else {
        complete_return_unwinding(fiber)
    }
}
```

### 2.3 Panic Unwinding Flow (`handle_panic_unwind`)

**Path 1: New panic (no unwinding in progress)**

```rust
fn start_panic_unwind(fiber: &mut Fiber, module: &Module) -> ExecResult {
    // Increment panic generation (already done by set_recoverable_panic)
    
    // Collect ALL defers from current frame
    let defers = collect_defers(&mut fiber.defer_stack, fiber.frames.len());
    
    fiber.unwinding = Some(UnwindingState {
        pending: defers,  // All defers (including errdefers)
        target_depth: fiber.frames.len() - 1,
        mode: UnwindingMode::Panic,
        current_defer_generation: 0,  // Will be set per-defer in execute_next_defer
        return_values: extract_frame_return_values(...),
        caller_ret_reg: frame.ret_reg,
        caller_ret_count: frame.ret_count as usize,
    });
    
    execute_next_defer(fiber, module)
}
```

**Path 2: Defer returned during panic unwinding**

```rust
fn handle_panic_defer_returned(fiber: &mut Fiber, module: &Module) -> ExecResult {
    // Check if panic was recovered
    if fiber.panic_state.is_none() {
        // Recovered! Switch to Return mode (filters errdefers)
        let state = fiber.unwinding.as_mut().unwrap();
        state.switch_to_return_mode();
    }
    
    // Collect nested defers
    let new_defers = collect_defers(&mut fiber.defer_stack, fiber.frames.len());
    prepend_defers(&mut state.pending, new_defers, true);
    
    // Pop defer frame
    fiber.frames.pop();
    
    // Continue unwinding
    execute_next_defer_or_propagate(fiber, module)
}
```

**Path 3: Panic during unwinding (panic inside defer)**

```rust
fn handle_panic_during_unwinding(fiber: &mut Fiber, module: &Module) -> ExecResult {
    // The new panic replaces the old one
    // Continue unwinding with new panic value
    // This is handled by the normal panic flow
}
```

### 2.4 Defer Execution

```rust
fn call_defer_entry(fiber: &mut Fiber, entry: DeferEntry, module: &Module) -> ExecResult {
    // Set current_defer_generation for nested defer registration
    if let Some(ref mut state) = fiber.unwinding {
        state.current_defer_generation = entry.registered_at_generation;
    }
    
    // Get function to call
    let func_id = if entry.func_id != 0 {
        entry.func_id
    } else {
        closure::func_id(entry.closure.unwrap())
    };
    
    let func = &module.functions[func_id as usize];
    
    // Allocate new frame
    let callee_bp = fiber.sp;
    fiber.ensure_capacity(callee_bp + func.local_slots as usize);
    
    // Copy captured args
    for (i, &arg) in entry.args.iter().enumerate() {
        fiber.stack[callee_bp + i] = arg;
    }
    
    // If closure, set slot0
    if let Some(closure) = entry.closure {
        fiber.stack[callee_bp] = closure as u64;
    }
    
    // Push frame
    fiber.sp = callee_bp + func.local_slots as usize;
    fiber.frames.push(CallFrame::new(func_id, callee_bp, 0, 0));
    
    ExecResult::FrameChanged
}
```

### 2.5 Error Return Detection

`compute_include_errdefers()` determines if errdefers should run:

```rust
fn compute_include_errdefers(fiber: &Fiber, inst: &Instruction, func: &FunctionDef, is_error_return: bool) -> bool {
    // Case 1: Explicit `fail err` statement
    if is_error_return {
        return true;
    }
    
    // Case 2: Function has error return and last return slot is non-nil
    if func.has_error_return {
        let ret_reg = inst.a as usize;
        let ret_slots = func.ret_slots as usize;
        let error_slot = fiber.stack[fiber.frames.last().unwrap().bp + ret_reg + ret_slots - 2];
        // Check if error interface is non-nil (slot0 != 0 means typed value)
        // Actually check value_kind field
        let vk = error_slot & 0xFF;
        return vk != 0;  // ValueKind::Void = 0 means nil
    }
    
    false
}
```

---

## Part 3: Current JIT Architecture

### 3.1 Function Jittability Check

**`is_func_jittable()`** (`vo-jit/src/lib.rs:37-54`):

```rust
pub fn is_func_jittable(func: &FunctionDef) -> bool {
    for inst in &func.code {
        match inst.opcode() {
            // Defer/recover - CURRENTLY BLOCKED
            Opcode::DeferPush | Opcode::ErrDeferPush | Opcode::Recover => return false,
            // Select (complex control flow)
            | Opcode::SelectBegin | Opcode::SelectSend | Opcode::SelectRecv | Opcode::SelectExec => return false,
            _ => {}
        }
    }
    true
}
```

### 3.2 JIT Execution Flow

When VM encounters a Call to a jittable function:

1. **dispatch_jit_call**: Allocates frame in `fiber.stack`, builds `JitContext`
2. **JIT function executes**: Native code runs, locals in `fiber.stack[jit_bp..]`
3. **Result handling**: Based on `JitResult`:
   - `Ok`: Pop frame, copy return values, continue
   - `Panic`: Set panic state, call `panic_unwind()`
   - `Call`: Materialize frames, push callee frame, VM continues
   - `WaitIo`: Materialize frames, block fiber

### 3.3 JIT-to-JIT Calls

For jittable callees, JIT uses direct native calls:
- Fast path: Native call via function pointer table
- Slow path: On `Call`/`WaitIo` result, spill to `fiber.stack`, push `ResumePoint`

**Shadow Frame Design** (`resume_stack`):
- JIT-to-JIT calls don't push to `fiber.frames` immediately
- Instead, `ResumePoint` entries track call chain
- On side-exit (`Call`/`WaitIo`), `materialize_jit_frames()` converts to real `CallFrame`s

### 3.4 Panic Handling in JIT

Current JIT can *trigger* panics but doesn't handle defer/recover:

```rust
fn emit_panic_if<'a>(e: &mut impl IrEmitter<'a>, condition: Value, call_vo_panic: bool) {
    let panic_block = e.builder().create_block();
    let ok_block = e.builder().create_block();
    e.builder().ins().brif(condition, panic_block, &[], ok_block, &[]);
    
    e.builder().switch_to_block(panic_block);
    if call_vo_panic {
        // Call vo_panic helper to set panic message
        e.builder().ins().call(panic_func, &[ctx, msg_slot0, msg_slot1]);
    }
    // Return JitResult::Panic
    let panic_ret = e.builder().ins().iconst(types::I32, 1);
    e.builder().ins().return_(&[panic_ret]);
    
    e.builder().switch_to_block(ok_block);
}
```

After JIT returns `Panic`, `handle_jit_result` calls `panic_unwind()` which uses VM's unwinding system.

---

## Part 4: Design Options

### Option A: Full JIT Implementation (Complex)

Implement defer/recover entirely in JIT-generated code:
- Maintain defer stack in JIT variables
- Generate unwinding code inline
- Handle all edge cases in native code

**Pros**: Maximum performance
**Cons**: Extremely complex, error-prone, huge code size increase

### Option B: Hybrid Approach (Recommended)

JIT handles `DeferPush`/`ErrDeferPush`/`Recover` by calling into VM helpers, while actual defer *execution* remains VM-interpreted.

**Key insight**: Defer functions are called *after* the containing function returns. So:
1. JIT function containing `defer` can be compiled
2. When JIT returns, VM handles defer execution
3. `recover()` in JIT can call VM helper to check/consume panic

### Option C: Transparent VM Fallback (Simplest)

When JIT function encounters `DeferPush`, return `Call` to VM and let VM re-execute from current PC.

**Pros**: Minimal changes
**Cons**: Performance penalty, JIT function "restarts" from defer point

---

## Part 5: Recommended Design (Option B - Revised)

### 5.0 Critical Design Corrections

**Original design had these flaws:**

1. **Flaw 1**: JIT-to-JIT direct calls bypass defer execution
   - If B (JIT, has defer) is called directly by A (JIT), B's return goes directly back to A
   - A's Ok path continues execution without checking defer_stack
   - **B's defers never execute!**

2. **Flaw 2**: `handle_jit_result` pops frame before defer check
   - Original: `fiber.frames.pop()` then check defer_stack
   - But defer_stack entries have `frame_depth` that matches the now-popped frame
   - **Defer check always fails!**

3. **Flaw 3**: JIT Return doesn't convey `is_error_return`
   - errdefer needs to know if function returned with error
   - JIT Return only returns `JitResult::Ok`, no error info

**Corrected design:**

1. **Functions with defer CANNOT be JIT-to-JIT direct called**
   - Add `has_defer` flag to `FunctionDef`
   - `can_jit_to_jit_call()` returns false if callee has defer
   - Such calls go through VM trampoline (`JitResult::Call`)

2. **`handle_jit_result` checks defer BEFORE popping frame**
   - Check if `defer_stack.last().frame_depth == frames.len()`
   - If true, trigger defer execution via VM unwinding
   - Only pop frame after all defers complete

3. **JIT Return stores `is_error_return` in JitContext**
   - New field `JitContext.is_error_return`
   - VM reads this to determine if errdefers should run

### 5.1 Overview

1. **Add `has_defer` field to `FunctionDef`** (codegen change)
2. **Modify `can_jit_to_jit_call()`** to reject functions with defer
3. **Remove defer/recover from `is_func_jittable()` blocklist**
4. **Implement JIT opcode handlers** that call VM helpers
5. **Modify `handle_jit_result`** to check defer_stack before frame pop

### 5.2 New JIT Helpers

Add to `JitContext`:

```rust
pub struct JitContext {
    // ... existing fields ...
    
    /// Callback to push a defer entry
    pub defer_push_fn: Option<extern "C" fn(
        ctx: *mut JitContext,
        func_id: u32,
        is_closure: u32,
        closure_ref: u64,
        args_ptr: *const u64,
        arg_count: u32,
        is_errdefer: u32,
    )>,
    
    /// Callback for recover() - returns (slot0, slot1) or (0, 0) if no panic
    pub recover_fn: Option<extern "C" fn(
        ctx: *mut JitContext,
        result_ptr: *mut u64,  // 2 slots output
    ) -> u32>,  // 1 if recovered, 0 if not
}
```

### 5.3 JIT Opcode Translation

**DeferPush/ErrDeferPush:**

```rust
fn emit_defer_push(e: &mut impl IrEmitter, inst: &Instruction, is_errdefer: bool) {
    let ctx = e.ctx_param();
    
    // Load defer_push_fn from ctx
    let fn_ptr = e.builder().ins().load(types::I64, MemFlags::trusted(), ctx, OFFSET_DEFER_PUSH_FN);
    
    // Prepare arguments
    let is_closure = (inst.flags & 1) != 0;
    let (func_id_val, closure_ref_val) = if is_closure {
        let closure = e.read_var(inst.a);
        (e.builder().ins().iconst(types::I32, 0), closure)
    } else {
        let fid = inst.a as u32 | ((inst.flags as u32 >> 1) << 16);
        (e.builder().ins().iconst(types::I32, fid as i64), e.builder().ins().iconst(types::I64, 0))
    };
    
    // Get args pointer
    let args_ptr = e.var_addr(inst.b);
    let arg_count = e.builder().ins().iconst(types::I32, inst.c as i64);
    let is_errdefer_val = e.builder().ins().iconst(types::I32, is_errdefer as i64);
    
    // Call helper
    let sig = import_defer_push_sig(e);
    e.builder().ins().call_indirect(sig, fn_ptr, &[
        ctx, func_id_val, is_closure as i32, closure_ref_val, args_ptr, arg_count, is_errdefer_val
    ]);
}
```

**Recover:**

```rust
fn emit_recover(e: &mut impl IrEmitter, inst: &Instruction) {
    let ctx = e.ctx_param();
    
    // Allocate stack space for result (2 slots)
    let result_slot = e.builder().create_stack_slot(...);
    let result_ptr = e.builder().ins().stack_addr(types::I64, result_slot, 0);
    
    // Load recover_fn from ctx
    let fn_ptr = e.builder().ins().load(types::I64, MemFlags::trusted(), ctx, OFFSET_RECOVER_FN);
    
    // Call helper
    let sig = import_recover_sig(e);
    let call = e.builder().ins().call_indirect(sig, fn_ptr, &[ctx, result_ptr]);
    let recovered = e.builder().inst_results(call)[0];
    
    // Load result slots
    let slot0 = e.builder().ins().stack_load(types::I64, result_slot, 0);
    let slot1 = e.builder().ins().stack_load(types::I64, result_slot, 8);
    
    // Store to destination
    e.write_var(inst.a, slot0);
    e.write_var(inst.a + 1, slot1);
}
```

### 5.4 VM Helper Implementations

**jit_defer_push** (`vo-vm/src/vm/jit/callbacks/`):

```rust
pub extern "C" fn jit_defer_push(
    ctx: *mut JitContext,
    func_id: u32,
    is_closure: u32,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_count: u32,
    is_errdefer: u32,
) {
    let ctx_ref = unsafe { &*ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    let gc = unsafe { &mut *ctx_ref.gc };
    
    // Determine func_id and closure
    let (fid, closure) = if is_closure != 0 {
        let cloned = gc.ptr_clone(closure_ref as GcRef);
        (0, Some(cloned))
    } else {
        (func_id, None)
    };
    
    // Copy arguments
    let args: Vec<u64> = (0..arg_count as usize)
        .map(|i| unsafe { *args_ptr.add(i) })
        .collect();
    
    // Get generation
    let generation = fiber.effective_defer_generation();
    
    // Push entry
    fiber.defer_stack.push(DeferEntry {
        func_id: fid,
        closure,
        args,
        frame_depth: fiber.frames.len(),
        is_errdefer: is_errdefer != 0,
        registered_at_generation: generation,
    });
}
```

**jit_recover** (`vo-vm/src/vm/jit/callbacks/`):

```rust
pub extern "C" fn jit_recover(
    ctx: *mut JitContext,
    result_ptr: *mut u64,
) -> u32 {
    let ctx_ref = unsafe { &*ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    
    // Check if in valid recover context
    if !fiber.is_direct_defer_context() {
        unsafe {
            *result_ptr = 0;
            *result_ptr.add(1) = 0;
        }
        return 0;
    }
    
    // Try to recover
    let recovered = fiber.take_recoverable_panic();
    let val = recovered.unwrap_or(InterfaceSlot::nil());
    
    unsafe {
        *result_ptr = val.slot0;
        *result_ptr.add(1) = val.slot1;
    }
    
    if recovered.is_some() {
        fiber.switch_panic_to_return_mode();
        1
    } else {
        0
    }
}
```

### 5.5 Return Handling (REVISED)

**Key insight**: Because `can_jit_to_jit_call()` rejects functions with defer, any JIT function that has defers will ALWAYS be called via `dispatch_jit_call` → `handle_jit_result`. This simplifies defer handling significantly.

**JIT Return instruction:**

```rust
fn emit_return(e: &mut impl IrEmitter, inst: &Instruction) {
    let ret_ptr = e.ret_ptr_param();
    let is_error_return = (inst.flags & 1) != 0;
    
    // Copy return values to ret buffer
    // ... existing code ...
    
    // Store is_error_return in JitContext for VM to read
    let ctx = e.ctx_param();
    let error_flag = e.builder().ins().iconst(types::I8, is_error_return as i64);
    e.builder().ins().store(MemFlags::trusted(), error_flag, ctx, JitContext::OFFSET_IS_ERROR_RETURN);
    
    // Always return Ok - VM will check defer_stack
    let ok = e.builder().ins().iconst(types::I32, 0);
    e.builder().ins().return_(&[ok]);
}
```

**handle_jit_result (REVISED):**

```rust
JitResult::Ok => {
    // CRITICAL: Check for defers BEFORE popping frame
    let current_depth = fiber.frames.len();
    let has_defers = fiber.defer_stack.last()
        .map_or(false, |d| d.frame_depth == current_depth);
    
    if has_defers {
        // Get is_error_return from JitContext
        let is_error_return = ctx.ctx.is_error_return != 0;
        
        // Get return info from current frame
        let frame = fiber.frames.last().unwrap();
        let func = &module.functions[frame.func_id as usize];
        
        // Also check if last return slot is non-nil error (for implicit error returns)
        let include_errdefers = is_error_return || check_implicit_error_return(fiber, func);
        
        // Collect defers for this frame
        let defers = collect_defers(&mut fiber.defer_stack, current_depth);
        
        // Build return_values based on ret_is_heap
        let return_values = if ctx.ctx.ret_is_heap != 0 {
            let gcref_start = jit_bp + ctx.ctx.ret_gcref_start as usize;
            let gcrefs = (0..func.heap_ret_gcref_count as usize)
                .map(|i| fiber.stack[gcref_start + i])
                .collect();
            Some(ReturnValues::Heap { gcrefs, slots_per_ref: func.heap_ret_slots.clone() })
        } else {
            let vals = ret[0..ret_slots].to_vec();
            Some(ReturnValues::Stack { vals, slot_types: func.ret_slot_types.clone() })
        };
        
        // Set up Return unwinding
        fiber.unwinding = Some(UnwindingState {
            pending: filter_errdefers(defers, include_errdefers),
            target_depth: current_depth - 1,
            mode: UnwindingMode::Return,
            current_defer_generation: fiber.panic_generation,
            return_values,
            caller_ret_reg: frame.ret_reg,
            caller_ret_count: frame.ret_slots as usize,
        });
        
        // Store return values for later (after defers complete)
        // They're already in the ret buffer, which we'll use in complete_return_unwinding
        
        // Execute first defer
        return execute_next_defer(fiber, module);
    }
    
    // No defers - normal return path
    fiber.frames.pop();
    fiber.sp = jit_bp;
    
    // Copy return values to caller's stack
    for i in 0..ret_slots {
        fiber.stack[caller_bp + arg_start + i] = ret[i];
    }
    
    #[cfg(feature = "jit")]
    fiber.resume_stack.clear();
    
    ExecResult::FrameChanged
}
```

### 5.6 Frame Depth Consistency (REVISED)

**Problem**: JIT uses shadow frames (`resume_stack`), but `DeferEntry.frame_depth` uses `fiber.frames.len()`.

**Key insight from corrected design**: Because `can_jit_to_jit_call()` now rejects functions with defer, **any function that registers a defer will always have a real CallFrame in `fiber.frames`**.

**Proof**:
1. If function F has defer instructions, `F.has_defer = true`
2. `can_jit_to_jit_call(F)` returns false
3. Any call to F goes through `dispatch_jit_call` or VM interpreter
4. `dispatch_jit_call` pushes real CallFrame before JIT execution
5. Therefore, when F's `DeferPush` executes, `fiber.frames.len()` is correct

**Consequence**: We do NOT need `jit_frame_depth` field! The existing `fiber.frames.len()` works correctly because:
- Functions with defer are never JIT-to-JIT direct called
- They always go through `dispatch_jit_call` which pushes real frames

**Simplified `jit_defer_push`:**

```rust
pub extern "C" fn jit_defer_push(
    ctx: *mut JitContext,
    func_id: u32,
    is_closure: u32,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_count: u32,
    is_errdefer: u32,
) {
    let ctx_ref = unsafe { &*ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    let gc = unsafe { &mut *ctx_ref.gc };
    
    // ... capture args and closure ...
    
    // fiber.frames.len() is correct because:
    // - This function has defer (otherwise DeferPush wouldn't be called)
    // - Functions with defer always go through dispatch_jit_call
    // - dispatch_jit_call pushes real CallFrame
    let frame_depth = fiber.frames.len();
    let generation = fiber.effective_defer_generation();
    
    fiber.defer_stack.push(DeferEntry {
        func_id: fid,
        closure,
        args,
        frame_depth,
        is_errdefer: is_errdefer != 0,
        registered_at_generation: generation,
    });
}
```

### 5.7 Error Return Detection in JIT

For `ErrDeferPush` to work correctly, we need `is_error_return` information.

The Return instruction already has `flags & 1` for explicit `fail`:

```rust
fn emit_return(e: &mut impl IrEmitter, inst: &Instruction) {
    let is_error_return = (inst.flags & 1) != 0;
    
    // Store is_error_return in context for VM to read
    if is_error_return {
        let flag_ptr = e.ctx_field_ptr(OFFSET_IS_ERROR_RETURN);
        e.builder().ins().store(MemFlags::trusted(), true_val, flag_ptr, 0);
    }
    
    // ... rest of return handling ...
}
```

Add to `JitContext`:

```rust
pub is_error_return: u8,  // Set by JIT on `fail` return
```

---

## Part 6: Implementation Plan (Revised - Step by Step)

### Phase 1: Add `has_defer` to FunctionDef (Codegen)

**Files**: `vo-common-core/src/bytecode.rs`, `vo-codegen/src/func.rs`

```rust
// vo-common-core/src/bytecode.rs
pub struct FunctionDef {
    // ... existing fields ...
    pub has_defer: bool,  // NEW: true if function contains DeferPush or ErrDeferPush
}
```

**Changes in codegen:**
- Scan function body for defer/errdefer statements during compilation
- Set `has_defer = true` if any found

**Test**: Compile a function with defer, verify `has_defer` is true in bytecode dump.

---

### Phase 2: Modify `can_jit_to_jit_call()`

**File**: `vo-jit/src/lib.rs`

```rust
pub fn can_jit_to_jit_call(func: &FunctionDef, module: &VoModule) -> bool {
    if !is_func_jittable(func) {
        return false;
    }
    // NEW: Functions with defer cannot be JIT-to-JIT direct called
    if func.has_defer {
        return false;
    }
    // ... existing depth limit and recursive checks ...
}
```

**Test**: Function with defer should go through `emit_call_via_vm` instead of `emit_jit_call_with_fallback`.

---

### Phase 3: Add JitContext Fields

**File**: `vo-runtime/src/jit_api.rs`

```rust
pub struct JitContext {
    // ... existing fields ...
    
    /// Set by JIT Return to indicate explicit `fail` return
    pub is_error_return: u8,
    
    /// Callback to push a defer entry
    pub defer_push_fn: Option<extern "C" fn(
        ctx: *mut JitContext,
        func_id: u32,
        is_closure: u32,
        closure_ref: u64,
        args_ptr: *const u64,
        arg_count: u32,
        is_errdefer: u32,
    )>,
    
    /// Callback for recover() - writes result to output, returns 1 if recovered
    pub recover_fn: Option<extern "C" fn(
        ctx: *mut JitContext,
        result_ptr: *mut u64,
    ) -> u32>,
}

impl JitContext {
    pub const OFFSET_IS_ERROR_RETURN: i32 = std::mem::offset_of!(JitContext, is_error_return) as i32;
    pub const OFFSET_DEFER_PUSH_FN: i32 = std::mem::offset_of!(JitContext, defer_push_fn) as i32;
    pub const OFFSET_RECOVER_FN: i32 = std::mem::offset_of!(JitContext, recover_fn) as i32;
}
```

**Test**: Compile check passes.

---

### Phase 4: Implement VM Callbacks

**File**: `vo-vm/src/vm/jit/callbacks/defer.rs` (NEW FILE)

```rust
//! JIT callbacks for defer/recover operations.

use vo_runtime::jit_api::JitContext;
use crate::fiber::{Fiber, DeferEntry};

pub extern "C" fn jit_defer_push(
    ctx: *mut JitContext,
    func_id: u32,
    is_closure: u32,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_count: u32,
    is_errdefer: u32,
) {
    let ctx_ref = unsafe { &*ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    let gc = unsafe { &mut *ctx_ref.gc };
    
    let (fid, closure) = if is_closure != 0 {
        let cloned = gc.ptr_clone(closure_ref as crate::gc::GcRef);
        (0, Some(cloned))
    } else {
        (func_id, None)
    };
    
    let args: Vec<u64> = (0..arg_count as usize)
        .map(|i| unsafe { *args_ptr.add(i) })
        .collect();
    
    let generation = fiber.effective_defer_generation();
    
    fiber.defer_stack.push(DeferEntry {
        func_id: fid,
        closure,
        args,
        frame_depth: fiber.frames.len(),
        is_errdefer: is_errdefer != 0,
        registered_at_generation: generation,
    });
}

pub extern "C" fn jit_recover(
    ctx: *mut JitContext,
    result_ptr: *mut u64,
) -> u32 {
    use vo_runtime::InterfaceSlot;
    
    let ctx_ref = unsafe { &*ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    
    if !fiber.is_direct_defer_context() {
        unsafe {
            *result_ptr = 0;
            *result_ptr.add(1) = 0;
        }
        return 0;
    }
    
    let recovered = fiber.take_recoverable_panic();
    let val = recovered.unwrap_or(InterfaceSlot::nil());
    
    unsafe {
        *result_ptr = val.slot0;
        *result_ptr.add(1) = val.slot1;
    }
    
    if recovered.is_some() {
        fiber.switch_panic_to_return_mode();
        1
    } else {
        0
    }
}
```

Update `vo-vm/src/vm/jit/callbacks/mod.rs`:
```rust
pub mod defer;
pub use defer::{jit_defer_push, jit_recover};
```

**Test**: Unit test the callbacks with mock fiber state.

---

### Phase 5: Register Helpers in JIT

**File**: `vo-jit/src/helpers.rs`

Add to `register_symbols`:
```rust
builder.symbol("vo_defer_push", vo_runtime::jit_api::vo_defer_push as *const u8);
builder.symbol("vo_recover", vo_runtime::jit_api::vo_recover as *const u8);
```

Add to `HelperFuncIds` and `declare_helpers`:
```rust
pub defer_push: cranelift_module::FuncId,
pub recover: cranelift_module::FuncId,
```

**File**: `vo-runtime/src/jit_api.rs`

Add wrapper functions:
```rust
#[no_mangle]
pub extern "C" fn vo_defer_push(
    ctx: *mut JitContext,
    func_id: u32,
    is_closure: u32,
    closure_ref: u64,
    args_ptr: *const u64,
    arg_count: u32,
    is_errdefer: u32,
) {
    let ctx_ref = unsafe { &*ctx };
    if let Some(f) = ctx_ref.defer_push_fn {
        f(ctx, func_id, is_closure, closure_ref, args_ptr, arg_count, is_errdefer);
    }
}

#[no_mangle]
pub extern "C" fn vo_recover(ctx: *mut JitContext, result_ptr: *mut u64) -> u32 {
    let ctx_ref = unsafe { &*ctx };
    match ctx_ref.recover_fn {
        Some(f) => f(ctx, result_ptr),
        None => {
            unsafe { *result_ptr = 0; *result_ptr.add(1) = 0; }
            0
        }
    }
}
```

**Test**: Compile check passes, symbols resolve.

---

### Phase 6: JIT Opcode Translation

**File**: `vo-jit/src/translate.rs`

Add to `translate_inst`:
```rust
Opcode::DeferPush => {
    emit_defer_push(e, inst, false);
    Ok(Completed)
}
Opcode::ErrDeferPush => {
    emit_defer_push(e, inst, true);
    Ok(Completed)
}
Opcode::Recover => {
    emit_recover(e, inst);
    Ok(Completed)
}
```

Implement helper functions (see detailed code in section 5.3).

**File**: `vo-jit/src/func_compiler.rs`

Modify `ret()` to store `is_error_return`:
```rust
fn ret(&mut self, inst: &Instruction) {
    // ... existing return value handling ...
    
    // Store is_error_return in JitContext
    let is_error_return = (inst.flags & 1) != 0;
    if is_error_return {
        let ctx = self.builder.block_params(self.entry_block)[0];
        let one = self.builder.ins().iconst(types::I8, 1);
        self.builder.ins().store(MemFlags::trusted(), one, ctx, JitContext::OFFSET_IS_ERROR_RETURN);
    }
    
    let ok = self.builder.ins().iconst(types::I32, 0);
    self.builder.ins().return_(&[ok]);
}
```

**Test**: Compile function with defer, verify JIT code calls helpers.

---

### Phase 7: Remove Blocklist & Update handle_jit_result

**File**: `vo-jit/src/lib.rs`

```rust
pub fn is_func_jittable(func: &FunctionDef) -> bool {
    for inst in &func.code {
        match inst.opcode() {
            // REMOVED: DeferPush, ErrDeferPush, Recover
            // Select still blocked
            Opcode::SelectBegin | Opcode::SelectSend | Opcode::SelectRecv | Opcode::SelectExec => return false,
            _ => {}
        }
    }
    true
}
```

**File**: `vo-vm/src/vm/jit/mod.rs`

Update `handle_jit_result` Ok branch (see section 5.5 for full code).

**File**: `vo-vm/src/vm/jit/context.rs`

Update `build_jit_context` to set callbacks:
```rust
ctx.defer_push_fn = Some(callbacks::jit_defer_push);
ctx.recover_fn = Some(callbacks::jit_recover);
ctx.is_error_return = 0;
```

**Test**: Run existing defer tests with `./d.py test jit`.

---

### Phase 8: Integration Testing

**Test files to verify**:
1. `test_data/defer_*.vo` - all existing defer tests
2. `test_data/errdefer_*.vo` - all existing errdefer tests  
3. `test_data/panic_recover_*.vo` - all panic/recover tests

**New test cases** (create if not exist):
```go
// test_data/jit_defer_basic.vo
func main() {
    x := 0
    defer func() { x = 1 }()
    assert(x == 0)
}
// After function returns, defer runs, but we can't observe it in main...
// Need a different test structure

// test_data/jit_defer_with_return.vo
func f() int {
    x := 0
    defer func() { x = 42 }()
    return x  // Should return 0, defer runs after
}
func main() {
    assert(f() == 0)
}

// test_data/jit_errdefer_error.vo  
func f() (int, error) {
    errdefer fmt.Println("errdefer ran")
    fail errors.New("oops")
}

// test_data/jit_recover_basic.vo
func f() (r int) {
    defer func() {
        if x := recover(); x != nil {
            r = 42
        }
    }()
    panic("test")
    return 0
}
func main() {
    assert(f() == 42)
}
```

---

## Part 7: Edge Cases and Considerations (REVISED)

### 7.1 Nested Calls with Defers

**Scenario**: A (JIT, has defer) calls B (JIT, has defer)

```go
func A() {
    defer fa()
    B()
}

func B() {
    defer fb()
    panic("oops")
}
```

**Flow with corrected design:**

1. A is called via `dispatch_jit_call` (because `A.has_defer = true`)
2. A's frame pushed to `fiber.frames`
3. A pushes defer `fa` via `jit_defer_push` (frame_depth = 1)
4. A calls B → `can_jit_to_jit_call(B)` returns **false** (B has defer)
5. A returns `JitResult::Call` to request B execution
6. VM's `handle_jit_result` sets up B's frame via `dispatch_jit_call`
7. B's frame pushed to `fiber.frames`
8. B pushes defer `fb` via `jit_defer_push` (frame_depth = 2)
9. B panics → returns `JitResult::Panic`
10. VM's `handle_jit_result` handles panic
11. `panic_unwind` executes `fb` (frame_depth 2), then `fa` (frame_depth 1)

**Key insight**: Because both A and B have defers:
- Neither can be JIT-to-JIT direct called
- Both go through `dispatch_jit_call` which pushes real frames
- `frame_depth` is always correct

### 7.2 Recover in JIT-compiled Defer

```go
func f() {
    defer func() {
        if r := recover(); r != nil {
            fmt.Println("recovered:", r)
        }
    }()
    panic("test")
}
```

**Flow:**
1. f (JIT) panics → returns `JitResult::Panic`
2. `handle_jit_result` calls `panic_unwind`
3. `panic_unwind` calls defer closure via VM's `call_defer_entry`
4. If defer closure is JIT-compiled, it runs via `dispatch_jit_call`
5. Defer closure calls `jit_recover` helper
6. Helper checks `is_direct_defer_context()`:
   - `frames.len() == target_depth + 1`? (defer function is exactly one deeper than target)
   - `current_defer_generation < panic_generation`? (defer registered before panic)
7. If both true, returns panic value and calls `switch_panic_to_return_mode()`
8. Defer closure returns `JitResult::Ok`
9. `handle_jit_result` sees no more defers, completes return

**Important**: The defer closure itself has `has_defer = false` (no nested defer), so it CAN be JIT-to-JIT called. But since it's called from `call_defer_entry` (VM code), it goes through `dispatch_jit_call`.

### 7.3 Panic Inside JIT Defer

```go
func f() {
    defer func() {
        panic("second")  // Panic inside defer
    }()
    panic("first")
}
```

**Flow:**
1. f panics "first" → `panic_unwind` starts
2. Defer closure (JIT) executes via `dispatch_jit_call`
3. Defer closure panics "second" → returns `JitResult::Panic`
4. `handle_jit_result` handles new panic
5. `panic_unwind` is called again, replaces "first" with "second"
6. Since defer already executed, unwinding continues with "second"

### 7.4 Mixed JIT/VM Call Chain

```go
func A() {  // JIT, has defer
    defer da()
    B()
}

func B() {  // VM only (has select or other blocking)
    C()
}

func C() {  // JIT, has defer
    defer dc()
    panic("oops")
}
```

**Flow:**
1. A (JIT) via `dispatch_jit_call`, pushes defer `da`
2. A calls B → B not jittable, returns `JitResult::Call`
3. VM executes B (interpreter)
4. B calls C → VM uses `dispatch_jit_call` for C
5. C pushes defer `dc`
6. C panics → `JitResult::Panic`
7. `handle_jit_result` starts `panic_unwind`
8. Executes `dc`, then VM unwinds through B, then executes `da`

**Key**: The design handles mixed JIT/VM seamlessly because:
- All defer registrations go to `fiber.defer_stack`
- All panic handling goes through `panic_unwind`
- Frame depth is always correct (real frames for defer functions)

### 7.5 GC Considerations

`DeferEntry` holds:
- `closure: Option<GcRef>` - needs to be GC roots
- `args: Vec<Slot>` - may contain GcRefs

Current VM already handles this in `gc_roots.rs`. Since we're using the same `fiber.defer_stack`, **no additional GC work needed**.

### 7.6 Return Value Preservation After Defer (CRITICAL BUG FIX)

```go
func f() (r int) {
    r = 42
    defer func() {
        r = 100  // Modify named return (heap-escaped)
    }()
    return r
}
// Should return 100, not 42!
```

**Bug identified!** 

JIT's current `ret()` implementation:
```rust
if heap_returns {
    // Dereference GcRef and copy to ret buffer IMMEDIATELY
    let val = load(gcref, offset);
    store(val, ret_ptr, offset);  // ret buffer has 42
}
return Ok;
// VM checks defer_stack, executes defer (modifies heap to 100)
// But ret buffer still has 42!
```

**VM's correct behavior** (from `unwind.rs`):
```rust
// For heap returns, only save GcRefs (NOT dereferenced values)
Some(ReturnValues::Heap { gcrefs, slots_per_ref })

// After defers complete, THEN dereference:
fn return_values_to_vec(rv: Option<ReturnValues>, ...) {
    Some(ReturnValues::Heap { gcrefs, .. }) => read_heap_gcrefs(&gcrefs, ...)  // Reads 100!
}
```

**Root cause**: VM saves GcRef pointers, dereferences after defers. JIT dereferences before defers.

**Fix requires new JitContext fields:**

```rust
pub struct JitContext {
    // ... existing fields ...
    
    /// For heap returns: starting register of GcRefs (inst.a from Return)
    pub ret_gcref_start: u16,
    
    /// True if function uses heap returns
    pub ret_is_heap: bool,
}
```

**JIT ret() modification:**

```rust
fn ret(&mut self, inst: &Instruction) {
    let ctx = self.ctx_param();
    
    if heap_returns {
        // Store info for VM, but DON'T dereference to ret buffer
        let gcref_start = inst.a as u16;
        self.builder.ins().store(MemFlags::trusted(), gcref_start, ctx, OFFSET_RET_GCREF_START);
        let one = self.builder.ins().iconst(types::I8, 1);
        self.builder.ins().store(MemFlags::trusted(), one, ctx, OFFSET_RET_IS_HEAP);
        
        // GcRefs stay in fiber.stack[jit_bp + gcref_start], VM will read them
    } else {
        // Stack returns: copy to ret buffer as before
        // ...
        let zero = self.builder.ins().iconst(types::I8, 0);
        self.builder.ins().store(MemFlags::trusted(), zero, ctx, OFFSET_RET_IS_HEAP);
    }
    
    // Store is_error_return
    // ...
    
    let ok = self.builder.ins().iconst(types::I32, 0);
    self.builder.ins().return_(&[ok]);
}
```

**handle_jit_result modification:**

```rust
JitResult::Ok => {
    let has_defers = fiber.defer_stack.last()
        .map_or(false, |d| d.frame_depth == fiber.frames.len());
    
    if has_defers {
        let func = &module.functions[frame.func_id as usize];
        
        let return_values = if ctx.ctx.ret_is_heap != 0 {
            // Read GcRefs from fiber.stack (NOT from ret buffer!)
            let gcref_start = jit_bp + ctx.ctx.ret_gcref_start as usize;
            let gcref_count = func.heap_ret_gcref_count as usize;
            let gcrefs: Vec<u64> = (0..gcref_count)
                .map(|i| fiber.stack[gcref_start + i])
                .collect();
            let slots_per_ref = func.heap_ret_slots.iter().map(|&s| s as usize).collect();
            Some(ReturnValues::Heap { gcrefs, slots_per_ref })
        } else {
            // Stack returns: use ret buffer values
            let vals = ret[0..ret_slots].to_vec();
            let slot_types = func.ret_slot_types.clone().unwrap_or_default();
            Some(ReturnValues::Stack { vals, slot_types })
        };
        
        // Set up unwinding state...
        fiber.unwinding = Some(UnwindingState {
            pending: ...,
            target_depth: current_depth - 1,
            mode: UnwindingMode::Return,
            current_defer_generation: fiber.panic_generation,
            return_values,
            caller_ret_reg: frame.ret_reg,
            caller_ret_count: frame.ret_slots as usize,
        });
        
        return execute_next_defer(fiber, module);
    }
    
    // No defers - use ret buffer values directly
    // ...
}
```

**Why this works:**
1. JIT saves GcRef locations in JitContext, doesn't dereference
2. VM constructs `Some(ReturnValues::Heap { ... })` with GcRefs from fiber.stack
3. Defers execute, may modify heap values
4. `return_values_to_vec` dereferences GcRefs → gets modified values

### 7.7 Conditional Defer with Heap Returns (BUG FIX #5)

```go
func f() (r int) {
    r = 42
    if someCondition {
        defer func() { r = 100 }()
    }
    return r  // If someCondition=false, no defer registered
}
```

**Bug**: 
- `has_defer = true` (compile time) → JIT doesn't write to ret buffer
- Runtime: someCondition=false → defer_stack empty
- `handle_jit_result` sees `has_defers = false`, reads from empty ret buffer!

**Fix**: The NO-DEFER path must also check `ret_is_heap`:

```rust
JitResult::Ok => {
    let has_defers = fiber.defer_stack.last()
        .map_or(false, |d| d.frame_depth == fiber.frames.len());
    
    if has_defers {
        // ... defer handling with ReturnValues ...
    } else {
        // No defers - but STILL need to handle heap returns!
        let ret_vals = if ctx.ctx.ret_is_heap != 0 {
            // Read from GcRefs in fiber.stack
            let func = &module.functions[frame.func_id as usize];
            let gcref_start = jit_bp + ctx.ctx.ret_gcref_start as usize;
            let gcrefs: Vec<u64> = (0..func.heap_ret_gcref_count as usize)
                .map(|i| fiber.stack[gcref_start + i])
                .collect();
            read_heap_gcrefs(&gcrefs, &func.heap_ret_slots)
        } else {
            // Normal stack returns from ret buffer
            ret[0..ret_slots].to_vec()
        };
        
        fiber.frames.pop();
        fiber.sp = jit_bp;
        
        for (i, val) in ret_vals.into_iter().enumerate() {
            fiber.stack[caller_bp + arg_start + i] = val;
        }
    }
}
```

**Key insight**: `ret_is_heap` affects return value reading regardless of whether defers exist at runtime.

---

## Part 8: Performance Considerations

### 8.1 Overhead Analysis

**DeferPush**: One helper call per defer statement
- Allocates `DeferEntry` with captured args
- Small allocation, O(arg_count) copy

**Recover**: One helper call per recover() call
- O(1) check for valid context
- Conditional panic state manipulation

**Return with defers**: 
- One check of `defer_stack` (O(1) - just check last entry's frame_depth)
- If defers exist, fall back to VM unwinding

### 8.2 Optimization Opportunities

**Inline fast path for recover()**:

```rust
fn emit_recover_optimized(e: &mut impl IrEmitter, inst: &Instruction) {
    // Fast path: check if NOT in panic unwinding at all
    // unwinding == None → return nil immediately
    
    let unwinding_ptr = load_fiber_unwinding_ptr(e);
    let is_none = e.builder().ins().icmp_imm(IntCC::Equal, unwinding_ptr, 0);
    
    let fast_nil_block = e.builder().create_block();
    let slow_path_block = e.builder().create_block();
    let merge_block = e.builder().create_block();
    
    e.builder().ins().brif(is_none, fast_nil_block, &[], slow_path_block, &[]);
    
    // Fast path: return nil
    e.builder().switch_to_block(fast_nil_block);
    let zero = e.builder().ins().iconst(types::I64, 0);
    e.builder().ins().jump(merge_block, &[zero, zero]);
    
    // Slow path: call helper
    e.builder().switch_to_block(slow_path_block);
    // ... call jit_recover ...
    e.builder().ins().jump(merge_block, &[slot0, slot1]);
    
    // Merge
    e.builder().switch_to_block(merge_block);
    // ...
}
```

**Defer-free function fast path**:

At compile time, mark functions that have no defer/errdefer statements. These don't need any defer checking at return.

---

## Conclusion (REVISED)

The corrected Hybrid design (Option B) provides:

### Correctness Guarantees

1. **Frame depth always correct**: Functions with defer cannot be JIT-to-JIT direct called, ensuring they always have real `CallFrame`s
2. **Reuses VM unwinding**: All defer execution, panic handling, recover logic goes through battle-tested VM code
3. **No new GC roots needed**: Uses existing `fiber.defer_stack`
4. **Named return semantics preserved**: Values read from heap-escaped slots after defers complete

### Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| `has_defer` in FunctionDef | Enables compile-time decision for call routing |
| `can_jit_to_jit_call` rejects has_defer | Ensures real frames exist for defer registration |
| Defer execution via VM | Reuses unwinding state machine, avoids code duplication |
| `is_error_return` in JitContext | Supports errdefer semantics |

### Implementation Phases

| Phase | Description | Test |
|-------|-------------|------|
| 1 | Add `has_defer` to FunctionDef | Bytecode dump |
| 2 | Modify `can_jit_to_jit_call` | Call routing |
| 3 | Add JitContext fields | Compile check |
| 4 | Implement VM callbacks | Unit test |
| 5 | Register JIT helpers | Symbol resolution |
| 6 | JIT opcode translation | JIT codegen |
| 7 | Update handle_jit_result | Integration |
| 8 | Full test suite | `./d.py test jit` |

### Estimated Effort

- **vo-common-core**: ~10 lines (FunctionDef.has_defer)
- **vo-codegen**: ~20 lines (set has_defer during compilation)
- **vo-runtime**: ~100 lines (JitContext fields including ret_gcref_start/ret_is_heap, wrapper functions)
- **vo-vm**: ~200 lines (callbacks, handle_jit_result with ReturnValues handling)
- **vo-jit**: ~150 lines (opcode translation, can_jit_to_jit_call, ret() heap handling)

**Total: ~480 lines of new/modified code**

### Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Named return value corruption | JIT stores ret_gcref_start, VM reads GcRefs after defer (7.6) |
| Frame depth mismatch | Proof: has_defer functions always have real frames (5.6) |
| Performance regression | Only affects functions with defer; most hot loops won't have defer |
| Heap returns dereference timing | JIT doesn't dereference heap returns when has_defer (7.6) |
| Conditional defer + heap returns | NO-DEFER path also checks ret_is_heap (7.7) |

### Open Questions

1. **Loop OSR with defer**: If a loop contains defer, the loop itself is marked not jittable. OK?
   - Answer: Yes, `is_func_jittable` already blocks, loop analysis respects this.

2. **Recursive functions with defer**: Each recursive call goes through `dispatch_jit_call`. Performance impact?
   - Answer: Acceptable. Recursion with defer is uncommon in hot paths.

3. **Defer in closures captured by goroutines**: Any special handling?
   - Answer: No. Defer registration happens in the executing fiber, goroutines have separate fibers.
