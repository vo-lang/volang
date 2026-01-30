# JIT Execution Architecture Redesign (Option B: SSA + Deopt via NeedVm)

## Problem Statement

Today, when JIT code needs to execute VM-only bytecode, we create a temporary Fiber (e.g. ~64KB malloc + memset per transition). This makes recursion-heavy code extremely slow and introduces unnecessary allocation/GC pressure.

## Goals

- **Zero allocation** when switching between JIT and VM.
- **Single fiber state machine**: scheduling/blocking/unwinding remains VM-authoritative.
- Keep the **fast path fast**: JIT locals remain in SSA/registers most of the time.

## Non-goals

- Supporting blocking operations inside synchronous JIT execution (channel block, `select`, etc.). If encountered, the program must fail with a clear error.
- Making every function JIT-able.

---

## Architecture Overview

### Execution Model

```
┌─────────────────────────────────────────────────────────┐
│                      Fiber                               │
│  ┌─────────────────────────────────────────────────┐    │
│  │  stack:  [slot0, slot1, ..., slotN]              │    │
│  │  frames: [Frame0, Frame1, ...]                   │    │
│  │  exec_mode: Vm | Jit                             │    │
│  │  jit_call_stack: [JitCallContext, ...]           │    │
│  └─────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────┘
                           │
           ┌───────────────┴───────────────┐
           ▼                               ▼
    ┌─────────────┐                 ┌─────────────┐
    │   VM Loop    │                │   JIT Code    │
    │ (interpreter)│ ◄── NeedVm ─── │ (native code) │
    └─────────────┘                 └─────────────┘
```

### Core Invariants

- **Single truth for control-flow**: the VM owns `Fiber.frames` and the scheduler state. JIT code never maintains an alternative fiber/scheduler state.
- **JIT locals are SSA-first**: locals live in SSA/registers by default. `Fiber.stack` is used as the ABI boundary with the VM.
- **At deopt/safepoints, VM-visible state must be materialized**: before returning `NeedVm` (or entering VM for any reason), JIT must spill all values required by the VM into `Fiber.stack`.
- **GC root visibility**: if GC only scans `Fiber.stack`, then at any JIT safepoint that may trigger GC, all live GC references must be spilled to `Fiber.stack`. If this is not satisfied, the program must fail (no silent corruption).

---

## NeedVm (Deopt) Mechanism

### When JIT Needs VM

JIT code must return to the VM loop whenever the next operation requires VM semantics or an unknown callee:

| Call site kind | Handling |
|---|---|
| Call known JIT-able function | JIT-to-JIT direct call |
| Call non-JIT-able function | `NeedVm { entry_pc, resume_pc }` |
| `CallInterface` | `NeedVm { entry_pc, resume_pc }` (callee unknown) |
| `CallClosure` | `NeedVm { entry_pc, resume_pc }` (callee unknown) |

### Result Type

`execute_jit()` returns a structured result. The VM loop must never “peek into a transient JitContext” to recover the reason.

```rust
pub enum JitResult {
    Ok,
    NeedVm { entry_pc: u32, resume_pc: u32 },
    Panic,
    Trap(JitTrap),
}

pub enum JitTrap {
    BlockingOperation(&'static str),
    UnsupportedInJit(&'static str),
}
```

### NeedVm Flow

```
1. JIT executes function A
2. A reaches a call site that requires VM execution (e.g. CallInterface)
3. JIT performs:
   a. Spill VM-required state into Fiber.stack[bp + slot]
      - and spill all live GC references if this is a GC safepoint
   b. Return JitResult::NeedVm { entry_pc = pc(Call), resume_pc = pc(next) }
4. VM loop receives NeedVm:
   a. Push JitCallContext { caller_func_id, resume_pc, caller_frame_count, vm_stack_depth }
   b. Set top frame PC to entry_pc (the Call instruction)
   c. Continue interpreting (pushes callee frames as usual)
5. VM returns to A and reaches resume_pc
6. VM checks re-entry condition and jumps to the corresponding JIT continuation
```

---

## JIT Continuation Table

### Problem

After returning `NeedVm`, the VM eventually reaches `resume_pc` and wants to continue execution in native code. However, the JIT compiler cannot safely “start at an arbitrary bytecode PC” unless we explicitly generate entry points.

### Solution: Continuations per Deopt Point

For each `NeedVm` point, codegen creates a dedicated continuation entry.

```rust
pub struct JitContinuation {
    pub resume_pc: u32,
    pub native_addr: *const u8,
}

pub struct CompiledFunction {
    pub entry: *const u8,
    pub continuations: Vec<JitContinuation>,
}
```

**Invariant**: `continuations` must be sorted by `resume_pc` so the VM can use binary search. Missing continuation is a hard bug and must fail loudly.

### Codegen Sketch

At a deopt boundary:

```
1. Spill the VM-required state (and GC roots if needed)
2. Return JitResult::NeedVm { entry_pc, resume_pc }
3. Create a continuation block for resume_pc
4. In the continuation block, reload the spilled SSA values needed by the following basic blocks
```

### VM Re-entry Sketch

```
1. VM detects it is back in the caller frame at resume_pc
2. VM finds compiled.continuations[resume_pc]
3. VM builds JitContext and jumps to native continuation
```

---

## JitContext

`JitContext` provides the native code with access to the current VM/Fiber and the VM stack memory for spilling/reloading at boundaries.

```rust
#[repr(C)]
pub struct JitContext {
    pub gc: *mut Gc,
    pub globals: *mut u64,
    pub vm: *mut c_void,
    pub fiber: *mut c_void,

    pub fiber_stack: *mut u64,   // Fiber.stack.as_mut_ptr()
    pub fiber_bp: usize,         // current frame bp
    pub fiber_local_slots: usize,
}
```

### SSA Locals vs Fiber.stack

- Normal JIT execution keeps locals in SSA/registers.
- At deopt/safepoints, JIT spills the required subset of SSA values to `fiber_stack[bp + slot]`.
- On continuation entry, JIT reloads the spilled values back into SSA.

---

## Fiber Extensions

```rust
pub struct Fiber {
    // existing fields...
    pub stack: Vec<u64>,
    pub frames: Vec<Frame>,
    
    // JIT support
    pub exec_mode: ExecMode,
    pub jit_call_stack: Vec<JitCallContext>,
}

pub enum ExecMode {
    Vm,
    Jit,
}

pub struct JitCallContext {
    pub caller_func_id: u32,
    pub resume_pc: u32,
    pub caller_frame_count: usize,
    pub vm_stack_depth: usize,
}
```

---

## VM Loop Changes

```rust
fn run_fiber(&mut self, fiber_id: FiberId) -> ExecResult {
    loop {
        let fiber = self.scheduler.get_fiber_mut(fiber_id);
        
        match fiber.exec_mode {
            ExecMode::Vm => {
                let result = self.step_vm(fiber);
                
                match result {
                    StepResult::Continue => {}
                    StepResult::Return => {
                        // Check JIT re-entry
                        if let Some(ctx) = fiber.jit_call_stack.last() {
                            if self.should_resume_jit(fiber, ctx) {
                                fiber.jit_call_stack.pop();
                                fiber.exec_mode = ExecMode::Jit;
                                continue;
                            }
                        }
                        if fiber.frames.is_empty() {
                            return ExecResult::Done;
                        }
                    }
                    StepResult::Block(reason) => return ExecResult::Block(reason),
                    StepResult::Panic => return self.handle_panic(fiber),
                }
            }
            ExecMode::Jit => {
                let result = self.execute_jit(fiber);
                
                match result {
                    JitResult::Ok => {
                        if fiber.frames.is_empty() {
                            return ExecResult::Done;
                        }
                        // If the caller is not JIT-able, continue in VM mode.
                        if !self.can_jit(fiber.current_func_id()) {
                            fiber.exec_mode = ExecMode::Vm;
                        }
                    }
                    JitResult::NeedVm { entry_pc, resume_pc } => {
                        fiber.jit_call_stack.push(JitCallContext {
                            caller_func_id: fiber.current_func_id(),
                            resume_pc,
                            caller_frame_count: fiber.frames.len(),
                            vm_stack_depth: fiber.stack.len(),
                        });
                        fiber.exec_mode = ExecMode::Vm;
                        fiber.frames.last_mut().unwrap().pc = entry_pc as usize;
                    }
                    JitResult::Panic => return self.handle_panic(fiber),
                    JitResult::Trap(reason) => {
                        // Must fail loudly: synchronous JIT cannot proceed.
                        panic!("JIT trap: {reason:?}");
                    }
                }
            }
        }
    }
}

fn should_resume_jit(&self, fiber: &Fiber, ctx: &JitCallContext) -> bool {
    fiber.frames.len() == ctx.caller_frame_count
        && fiber.frames.last().map(|f| f.func_id) == Some(ctx.caller_func_id)
        && fiber.frames.last().map(|f| f.pc as u32) == Some(ctx.resume_pc)
}

### Re-entry Invariants

- `vm_stack_depth` is recorded for debugging and must match `fiber.stack.len()` at re-entry. If it does not match, the VM must fail loudly.
- If a panic unwinds past `caller_frame_count`, the corresponding `JitCallContext` is invalid and must be discarded (see panic/unwind section below).
```

---

## Panic/Unwind Rules

During panic unwinding, the VM may pop multiple frames without ever reaching `resume_pc`. In that case, pending JIT re-entry contexts must not survive.

Rules:

- Whenever the VM pops frames due to panic/unwind, it must also pop `jit_call_stack` entries whose `caller_frame_count > fiber.frames.len()`.
- If a panic is recovered and execution continues normally, `jit_call_stack` must reflect the new control-flow and must not retain stale contexts.
- Any attempt to re-enter JIT with a missing continuation for `resume_pc` is a hard bug and must fail loudly.

---

## GC Safepoints and Root Visibility

This design keeps the VM as the single authority for GC. Therefore, native code must obey explicit safepoint rules.

Definitions:

- **Safepoint**: a point where the runtime may allocate / trigger GC / run finalizers.
- **Live GC reference**: any SSA value that contains a `GcRef` (or an `any/interface` whose payload is a `GcRef`) and is live across the safepoint.

Rules:

- At any safepoint inside JIT code, all live GC references must be materialized in VM-visible memory (typically `Fiber.stack` at the current frame bp).
- The safepoint set includes (at minimum) any runtime helper call that can allocate.
- If the JIT cannot precisely compute the live reference set for a safepoint, it must not claim the function is JIT-able.

---

## Implementation Phases

### Phase 1: Infrastructure (partially done)

- [x] Extend JitContext with fiber pointers and stack access
- [x] Add `exec_mode` and `jit_call_stack` to Fiber
- [x] Define JitCallContext
- [ ] VM loop: handle `JitResult::NeedVm { .. }` and re-entry

### Phase 2: JIT Codegen (Option B)

- [ ] Keep SSA locals; do not rewrite normal reads/writes to fiber_stack
- [ ] Generate `NeedVm { entry_pc, resume_pc }` at deopt points
- [ ] Generate continuation blocks and a sorted continuation table
- [ ] Implement spill/reload for deopt boundaries (based on liveness)
- [ ] Implement safepoint spill of live GC references

### Phase 3: VM Re-entry

- [ ] Binary search continuation table
- [ ] JIT re-entry detection and jump
- [ ] Correct `jit_call_stack` maintenance across panic/unwind/recover

### Phase 4: Optimization

- [ ] Improve liveness precision to minimize spill sets
- [ ] Continuation inlining and block layout improvements
- [ ] Reduce redundant memory traffic at boundaries

---

## Benefits

1. **Zero allocation**: no temporary Fiber is created for JIT↔VM transitions
2. **Single state machine**: scheduling and unwinding remain VM-authoritative
3. **Fast path preserved**: SSA/register execution remains intact in JIT code
4. **Debuggable**: VM-visible state is explicit at boundaries

## Risks

1. **Complexity**: continuation + deopt requires careful invariants
2. **GC correctness**: safepoint/root handling must be correct and explicit
3. **Debugging**: mixed-mode execution requires good tooling (bytecode/IR dumps)

## Alternatives Considered

### A. Keep vm_call_sync and optimize temporary Fiber creation

- Reuse a Fiber pool
- Pre-allocate stack
- Drawback: still allocates or manages extra state; not a fundamental fix

### B. Fully independent JIT stack with copy-back

- Use a native stack for JIT locals
- Copy results back to Fiber.stack at return
- Drawback: incorrect semantics for closures/pointers/GC roots

### C. Disallow JIT calling non-JIT-able functions

- Require all callees to be JIT-able
- Drawback: too restrictive; large parts of real programs would never run under JIT

---

## Work Estimation

### Phase 1: Infrastructure (~1 day)

| Task | Files | Effort |
|------|-------|--------|
| Extend `JitContext` | `vo-runtime/src/jit_api.rs` | ✅ Done |
| Update `build_jit_ctx` | `vo-vm/src/vm/jit_glue.rs` | ✅ Done |
| Add `exec_mode` and `jit_call_stack` | `vo-vm/src/fiber.rs` | ✅ Done |
| VM loop NeedVm handling | `vo-vm/src/vm/mod.rs` | ~2h |

### Phase 2: JIT Codegen (~3-5 days)

| Task | Files | Effort |
|------|-------|--------|
| **SSA locals + boundary spill** | `vo-jit/src/func_compiler.rs` | ~1-2d |
| - liveness for deopt boundaries | | |
| - spill/reload implementation | | |
| **NeedVm emission** | `vo-jit/src/func_compiler.rs` | ~0.5d |
| - spill boundary state | | |
| - return `JitResult::NeedVm` | | |
| **Continuation table** | `vo-jit/src/func_compiler.rs` | ~1-2d |
| - create continuation blocks | | |
| - record (resume_pc, native_addr) | | |
| - extend `CompiledFunction` | `vo-jit/src/lib.rs` | |
| **GC safepoints** | `vo-jit/src/func_compiler.rs` | ~0.5-1d |
| - spill live GC refs at safepoints | | |

### Phase 3: VM Re-entry (~2 days)

| Task | Files | Effort |
|------|-------|--------|
| Continuation lookup | `vo-vm/src/vm/jit_glue.rs` | ~0.5d |
| JIT re-entry jump | `vo-vm/src/vm/mod.rs` | ~0.5d |
| Panic/unwind cleanup | `vo-vm/src/vm/mod.rs` | ~0.5d |
| Tests + debugging | | ~0.5d |

### Phase 4: Optimization (~2-3 days, optional)

| Task | Effort |
|------|--------|
| Liveness analysis to minimize spill sets | ~1d |
| Continuation inlining | ~1d |
| Reduce boundary stack traffic | ~0.5d |

### Total: ~8-11 days

---

## Chosen Design (Option B)

- Locals remain SSA/register values.
- Deopt boundary is explicit: `NeedVm { entry_pc, resume_pc }`.
- Continuations are generated per deopt point (per `resume_pc`).
- Nested transitions are supported by `jit_call_stack`.

Continuation granularity: one continuation per `NeedVm` point (precise and debuggable).

---

## Testing Strategy

### Unit Tests

1. Basic JIT function deopts to VM on CallInterface/CallClosure
2. Recursion-heavy scenario (JIT → JIT → VM)
3. Panic/unwind across a pending `jit_call_stack` entry
4. Nested deopts (JIT A → VM B → JIT C → VM D)
5. GC safepoint correctness: live GC refs survive allocation/GC across safepoints

### Integration Tests

1. Quicksort benchmark (original regression)
2. Existing JIT test suite
3. Mixed feature programs: ensure unsupported JIT features reliably deopt or fail with clear errors (no hang)

### Performance Tests

1. Quicksort: must not regress vs VM and should improve
2. Fibonacci: JIT-to-JIT must not regress
3. Frequent deopt scenario: quantify overhead and validate no allocations
