# Unified Fiber State Machine Design

## JIT Big Picture

### Two-Level JIT Strategy

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         Function Entry                                   │
│                              │                                           │
│                              ▼                                           │
│                    ┌──────────────────┐                                  │
│                    │ can_jit(func)?   │                                  │
│                    └────────┬─────────┘                                  │
│                             │                                            │
│              ┌──────────────┴──────────────┐                             │
│              │                             │                             │
│              ▼ YES                         ▼ NO                          │
│     ┌────────────────┐            ┌────────────────┐                     │
│     │ JIT compile &  │            │ VM interpret   │                     │
│     │ execute        │            │ entire func    │                     │
│     └───────┬────────┘            └────────────────┘                     │
│             │                                                            │
│             ▼                                                            │
│    ┌─────────────────┐                                                   │
│    │ Call non-JIT-   │                                                   │
│    │ able function?  │                                                   │
│    └────────┬────────┘                                                   │
│             │                                                            │
│      ┌──────┴──────┐                                                     │
│      │             │                                                     │
│      ▼ YES         ▼ NO                                                  │
│  ┌──────────┐  ┌──────────┐                                              │
│  │ NeedVm   │  │ Continue │                                              │
│  │ handoff  │  │ JIT      │                                              │
│  └──────────┘  └──────────┘                                              │
└─────────────────────────────────────────────────────────────────────────┘
```

### Level 1: Function-Level JIT Decision (`can_jit`)

A function is **entirely excluded from JIT** if it contains any of these instructions:

| Instruction | Category |
|-------------|----------|
| `DeferPush`, `ErrDeferPush`, `Recover` | Defer/panic handling |
| `GoStart` | Goroutine creation |
| `ChanSend`, `ChanRecv`, `ChanClose` | Channel operations |
| `SelectBegin`, `SelectSend`, `SelectRecv`, `SelectExec` | Select statement |

**Rationale**: These instructions require complex VM state machine interactions (defer stack, panic unwinding, scheduler blocking). JIT-compiling them would require duplicating significant VM logic.

**Result**: If `can_jit(func) == false`, the entire function runs in VM interpreter mode.

### Level 2: Call-Site NeedVm Handoff

When a **JIT-compiled function** calls a **non-JIT-able function**, the JIT cannot simply inline the call because:
1. The callee may block (channel/select inside)
2. Blocking requires the VM loop to be on top of the native stack (no JIT frames)

**Solution**: JIT returns `NeedVm` at the call-site, VM executes the call (including any blocking), then control returns to JIT.

| Call Type | NeedVm Required? |
|-----------|------------------|
| `Call` to JIT-able func | No - direct JIT-to-JIT call |
| `Call` to non-JIT-able func | **Yes** - VM must execute |
| `CallInterface` | **Yes** - callee unknown at compile time |
| `CallClosure` | **Yes** - callee unknown at compile time |

### Key Insight

The bug this document addresses occurs at **Level 2**: JIT `main()` calls a function containing `select`. The current "trampoline fiber" approach creates a separate fiber with its own block/wake state, causing the "two sources of truth" problem.

---

## Problem Statement

The current JIT trampoline implementation has a **"two sources of truth"** problem:

```
┌─────────────────────────────────────────────────────────┐
│  Regular Fibers                │  Trampoline Fibers     │
│  scheduler.fibers[]            │  scheduler.trampoline_ │
│                                │  fibers[]              │
├────────────────────────────────┼────────────────────────┤
│  block_for_queue()             │  block_trampoline_     │
│  wake_fiber()                  │  for_queue()           │
│  has_blocked()                 │  wake_trampoline()     │
│  ready_queue scheduling        │  sync wait (busy loop)  │
└─────────────────────────────────────────────────────────┘
```

### Bug Example

`2026_01_29_jit_trampoline_select.vo` exposes the issue:
1. JIT `main` calls a function that contains `select`, so execution goes through a trampoline path
2. When `select` blocks, the trampoline fiber becomes `Blocked(Queue)`
3. Other goroutines send into channels and then exit
4. `wait_for_trampoline_wake` checks `has_blocked()` — **but it only checks regular fibers**
5. It returns false and exits early; the trampoline is never woken
6. **Fatal error**

### Root Cause

- Two independent fiber state machines
- Separate block/wake paths
- Unclear ownership boundaries (Scheduler vs JIT glue)

---

## Design Principles

### 0) **FIRST-ORDER INVARIANT: No Native JIT Frames Across Scheduler Boundaries**

> **The Scheduler may only block/yield/switch fibers when the VM loop is on top of the call stack.**

This is the foundational constraint that drives the entire design:
- Any operation that may cause `StepResult::Block` or `StepResult::Yield` **must** be executed by the VM interpreter.
- JIT code **must not** block or park a fiber directly; it can only return `StopReason::NeedVm` to hand control back to the VM loop.
- When a fiber transitions out of `Running`, there must be no native JIT frames still active on the system stack.

**Executable constraint points**:

*VM loop side (assertions)*:
```rust
// In run_fiber(), before returning Block/Yield to Scheduler:
debug_assert!(fiber.exec_mode == ExecMode::Vm, 
    "Block/Yield must only occur in VM mode");
```
- Runtime invariant: `FiberResult::Block` / `FiberResult::Yield` can **only** be returned when `fiber.exec_mode == ExecMode::Vm`.
- This is enforced by architecture (JIT returns `NeedVm`, VM produces `Block`/`Yield`), but debug assertions catch implementation bugs.

*JIT side (NeedVm generation)*:

Per the **Two-Level JIT Strategy** (see above):
- **Level 1**: Functions containing blocking instructions (`ChanSend`, `Select`, etc.) are excluded from JIT entirely by `can_jit()`. JIT never sees these instructions.
- **Level 2**: JIT functions that **call** non-JIT-able functions must return `NeedVm` at the call-site.

JIT **must** generate `NeedVm` for these call patterns:
| Call Type | Reason |
|-----------|--------|
| `Call` to non-JIT-able func | Callee may contain blocking ops |
| `CallInterface` | Callee unknown at compile time, may block |
| `CallClosure` | Callee unknown at compile time, may block |

Note: `Call` to a JIT-able function does **not** require `NeedVm` — JIT can call JIT directly.

### 1) Single State Machine

There is **exactly one** fiber state machine, owned by the Scheduler.

```rust
pub enum FiberState {
    Runnable,  // in ready_queue
    Running,   // currently executing (VM or JIT — exec_mode distinguishes)
    Blocked(BlockReason),  // waiting for wake-up
    Dead,      // finished
}
```

**Note**: `FiberState::Running` means "the Scheduler has selected this fiber to run". It does **not** imply VM-only execution. The `exec_mode` field (see below) distinguishes whether the fiber is currently in VM or JIT mode.

**Implementation sync required**: The current `fiber.rs` comment for `Running` says "Currently being executed by VM." This comment must be updated to reflect the new semantics: "Currently being executed (VM or JIT, distinguished by `exec_mode`)."

### 2) JIT Is an Execution Engine, Not a State

JIT must not own its own block/yield/wake logic.

```rust
pub enum ExecMode {
    Vm,
    Jit,
}

// JIT returns a unified StopReason; the Scheduler drives the state machine.
pub enum StopReason {
    /// Returned from the currently running JIT entrypoint back to the VM loop.
    /// This does NOT necessarily mean the fiber has finished.
    ///
    /// **Contract**:
    /// - On Return, the top frame's `pc` **must** be at a valid instruction boundary
    ///   (JIT has already advanced pc past the last executed instruction).
    /// - This ensures VM can safely continue interpreting without decode errors or
    ///   re-executing the last JIT instruction.
    /// - VM loop determines fiber completion by checking `frames.is_empty()`.
    /// - If `frames` is non-empty, VM loop continues execution (may switch mode or re-enter JIT).
    /// - JIT must NOT pop the last frame and return; frame management is VM's responsibility.
    Return,
    /// JIT detected a panic (or VM marked panic state). JIT exits native code and
    /// hands control to the VM loop. **The actual unwind is performed by VM**, not JIT.
    /// JIT's only responsibility is to exit cleanly and return this variant.
    PanicUnwind,
    /// Time slice ended. Budget is maintained by VM loop / Scheduler; JIT checks
    /// a counter (e.g., every N instructions or at loop back-edges) and returns Yield
    /// when budget is exhausted.
    Yield,
    /// Hand off execution to the VM interpreter.
    ///
    /// Semantics: VM continues interpreting from `vm_entry_pc` in the CURRENT top frame,
    /// potentially pushing additional VM frames (e.g. non-JIT function calls) and potentially
    /// blocking/yielding. When the VM returns back to the caller frame and reaches
    /// `resume_pc` (the continuation PC after the instruction that triggered this handoff),
    /// it switches back to ExecMode::Jit.
    ///
    /// Contract: the VM must execute the triggering instruction to completion and then
    /// explicitly set the current frame's PC to `resume_pc`.
    NeedVm {
        /// The bytecode PC where the VM should resume interpreting.
        /// This is typically the PC of the instruction that could not be JIT-executed.
        vm_entry_pc: u32,
        /// The continuation PC where JIT should resume AFTER the VM has finished executing
        /// the non-JIT-able instruction (e.g. after Call/select/channel op completes).
        ///
        /// IMPORTANT: This MUST be the post-instruction continuation PC, not the call-site PC.
        resume_pc: u32,
    },
}
```

Blocking is produced only by VM execution (`StepResult::Block`). JIT must not directly block or
park a fiber; any bytecode-level operation that may block must return `StopReason::NeedVm`.

### 3) Trampoline != Fiber

A trampoline is a **call boundary / execution context**, not a schedulable unit.

```rust
// Must not exist
pub trampoline_fibers: Vec<Fiber>  // ❌ remove

// Instead
pub struct JitCallContext {
    /// Function ID of the JIT caller (the frame that will resume in JIT).
    /// Used for re-entry validation: top frame's func_id MUST match this.
    pub caller_func_id: u32,
    /// Continuation PC where JIT should resume.
    /// This is the *post-instruction continuation*.
    pub resume_pc: u32,
    /// The expected `frames.len()` when the JIT caller is back at top of the call stack.
    /// This is captured at the moment JIT returns `NeedVm`, BEFORE VM executes the
    /// triggering instruction (which may push frames for Call).
    pub caller_frame_count: usize,
    /// VM stack depth snapshot (for cleanup on panic / ensuring consistent stack).
    pub vm_stack_depth: usize,
}

// Stored on the Fiber
pub struct Fiber {
    jit_call_stack: Vec<JitCallContext>,
    // ...
}
```

**Re-entry mechanism**: When VM finishes executing the non-JIT-able instruction,
the VM loop detects the continuation point and switches back to JIT:
1. VM runs in `ExecMode::Vm` until it is back at the caller frame (`frames.len() == caller_frame_count`).
2. VM executes the triggering instruction to completion; `pc` advances to the post-instruction position.
3. VM loop checks the **three-tuple re-entry condition** (see below).
4. VM pops `JitCallContext`.
5. VM sets `fiber.exec_mode = ExecMode::Jit`.
6. The VM loop calls into the JIT engine.
7. JIT resumes execution at `(caller_func_id, resume_pc)` via the continuation table.

**Hard contract: Three-tuple re-entry condition**:
```rust
// FIVE conditions for safe JIT re-entry:
fiber.exec_mode == ExecMode::Vm                          // (1) must be in VM mode
    && !fiber.jit_call_stack.is_empty()                  // (2) have pending continuation
    && fiber.frames.len() == ctx.caller_frame_count      // (3) frame depth matches
    && fiber.frames.last().func_id == ctx.caller_func_id // (4) function matches
    && fiber.frames.last().pc == ctx.resume_pc           // (5) PC matches
    && fiber.stack.len() == ctx.vm_stack_depth           // (6) stack depth matches (debug)
```
- Conditions (1)-(5) **must** all match for JIT resume.
- Condition (6) (stack depth) should be checked in debug builds; mismatch indicates VM left stack garbage in caller frame, which would corrupt JIT slot access.
- If `func_id` does not match (e.g., due to panic unwind, tailcall, frame replacement), this is a **fatal invariant violation** — the VM must panic, not silently fallback.
- This prevents subtle bugs where frame depth happens to match but the actual function is different.

**Why exec_mode and jit_call_stack checks?**
- Prevents accidental JIT switch when VM is executing a JIT-able function entry (not a continuation).
- Without these guards, a VM loop iteration could confuse "initial JIT entry" with "continuation re-entry".

**Hard contract: `resume_pc` calculation**:
- `resume_pc` **must** equal the bytecode PC immediately after the triggering instruction completes.
- JIT **must** call a shared `next_pc()` or `instr_size()` helper (in the bytecode crate) — the same one used by VM interpreter. **No separate decode logic in JIT.**
- If JIT and VM disagree on instruction boundaries, the re-entry check will fail silently (infinite VM loop) or resume at wrong PC (corruption).

**Hard contract: NeedVm caller-frame instruction boundary**:
- `NeedVm` corresponds to **one triggering instruction in the caller frame** (the frame that will resume in JIT).
- `vm_entry_pc` is the start PC of that instruction; `resume_pc` is the PC immediately after it (in the same caller frame).
- VM is **allowed to execute arbitrarily many instructions** to complete the triggering instruction's semantics (e.g., a `Call` will execute the entire callee function).
- **Critical constraint**: Before returning to the caller frame, VM must **not** advance the caller frame's PC beyond the triggering instruction. The caller frame's PC must remain at `vm_entry_pc` until the instruction completes, then jump directly to `resume_pc`.
- When VM returns to the caller frame (after callee returns / channel op completes / etc.), the caller frame's `pc` **must** equal `resume_pc`.

**Hard contract: VM execution to completion**:
- If the triggering instruction is `Call`, VM may push/pop frames, but **must** eventually return to caller frame and advance `pc` to `resume_pc`.
- If VM execution diverges (panic unwind, early return, yield) **before** reaching `resume_pc`, JIT resume is **forbidden**. The `jit_call_stack` must be unwound (see PanicUnwind handling).
- Re-entry check runs after **every** VM step (not just `StepResult::Return`), because `pc == resume_pc` may be reached via `Continue` for non-Call instructions.

**Concrete example (Call instruction)**:
```
frames.len() = 2   // JIT main (0) -> JIT funcA (1)
JIT funcA at pc=10 encounters Call to non-JIT-able funcB
  -> JIT explicitly computes resume_pc (e.g., 11 for fixed-size instructions)
  -> returns NeedVm { vm_entry_pc: 10, resume_pc: 11 }
  -> push JitCallContext { caller_frame_count: 2, resume_pc: 11, ... }
  -> VM executes Call at pc=10, pushes frame for funcB
  -> frames.len() = 3
  -> VM runs funcB to completion, pops frame
  -> frames.len() = 2, pc = 11 (Call advances pc after return)
  -> re-entry check passes: frames.len() == 2 && pc == 11
  -> switch back to JIT
```

**How `resume_pc` maps to native code**: The JIT compiler generates code that can resume from each continuation point. Two options:
- **Option A (Interpreter-style dispatch)**: JIT generates a switch/dispatch at function entry that jumps to the correct bytecode offset. Simple but adds overhead.
- **Option B (Continuation table)**: JIT maintains a `HashMap<(func_id, resume_pc), native_addr>` for each call-site. VM loop calls `jit_resume(func_id, resume_pc)` which looks up and jumps to the native continuation.

We choose **Option B** for performance. The JIT compiler emits a continuation entry point after each `NeedVm` call-site.

This avoids storing native code addresses in `JitCallContext`, which would be invalidated by recompilation.

**Continuation table storage**:
- Stored inside `JitManager`, indexed by `func_id` first: `Vec<HashMap<u32, *const u8>>` where outer index is `func_id`, inner key is `resume_pc`.
- Populated at JIT compile time, read-only at runtime.
- **Thread model**: Current VM/JIT is single-threaded synchronous execution. No locking needed; table is written during compilation (before execution) and only read during execution.

**Recompilation policy**: The current design does **not** support recompilation of already-JIT-compiled functions. Once a function is compiled, its continuation table entries remain valid for the lifetime of the VM. If tiered compilation or deoptimization is added in the future, the continuation table must be invalidated atomically with the old code, and any in-flight `JitCallContext` entries pointing to the old code must be handled (e.g., by falling back to VM interpretation).

**Critical requirement: No SSA state across the boundary (NeedVm = forced spill point)**
- Any value that must survive a `NeedVm` boundary MUST be materialized in the fiber VM stack slots (or other VM-owned storage).
- JIT continuations are allowed to assume the fiber stack represents the authoritative state for locals/temps.
- JIT must not rely on register-only/SSA-only live values across `NeedVm`.
- **Implementation requirement**: Before returning `NeedVm`, JIT codegen **must** emit spills for all VM-visible slots. This is a "forced spill point" in the JIT's register allocator / SSA strategy. Failure to do so will cause the continuation to read stale values — an extremely subtle bug.

**VM-visible slots scope** (what must be spilled):
- **Definition**: `CallFrame.bp..bp+frame_size` where `frame_size` comes from `FunctionDef.frame_size` (the bytecode metadata for the function).
- **StackValue slots** (locals, params, scalar temps): Must be written back to `fiber.stack[bp+slot_index]` before returning `NeedVm`.
- **StackArray/memory slots** (arrays, slices allocated on stack): Already have memory semantics; no explicit spill needed, but JIT must ensure any cached pointers or GC roots are consistent.
- SSA-only temporaries that are **not** addressable by bytecode (no `ReadLocal`/`WriteLocal` for that slot) do **not** need to be spilled, but JIT must re-compute or reload them from spilled sources after continuation.
- Rule of thumb: if bytecode can reference it via slot index, it must be spilled.

**Nested JIT entry policy (Strategy A: Conservative)**:
- While `jit_call_stack` is non-empty, VM **must not** switch to JIT for new function entries.
- JIT entry is only allowed when `jit_call_stack.is_empty()` (no pending JIT continuations).
- This simplifies the state machine: once JIT hands off to VM, VM runs to completion of the triggering instruction before JIT resumes.
- **Implication**: The "nested handoffs" example `JIT main → VM funcA → JIT funcB → VM funcC` is **not supported** in Strategy A. Instead, funcB would run in VM mode until the original JIT continuation is popped.

**Why Strategy A is required (specific risks of nested JIT entry)**:
1. **Multi-source continuations**: Nested entry would create multiple independent `JitCallContext` entries from different JIT "sessions". Distinguishing which continuation belongs to which JIT instance is error-prone.
2. **Interleaved forced spill points**: If VM calls JIT funcB which then returns `NeedVm`, we now have two forced spill points with different `vm_stack_depth` snapshots. SSA/stack synchronization complexity grows exponentially.
3. **Re-entry ordering**: With multiple pending continuations, the VM must pop them in correct LIFO order. Any misordering corrupts the JIT call stack.

**Rationale**: We choose correctness and simplicity over potential performance gain. Nested JIT entry can be added later if profiling shows significant impact.

**`jit_call_stack` semantics**:
- It is a **stack of pending JIT continuations**, each waiting for VM to complete a triggering instruction.
- Push on `NeedVm`; pop on successful re-entry (three-tuple match).
- If panic/yield/block occurs before re-entry, the entire stack must be unwound (no partial pop).

---

## Status Quo Mapping

This section maps current code constructs to the new design, clarifying what gets removed/replaced.

| Current Code | New Design | Notes |
|--------------|------------|-------|
| `FiberState::Suspended` | **Removed** | Was used to suspend caller fiber during trampoline. No longer needed: same fiber stays `Running`, only `exec_mode` changes. |
| `JitResult::{Ok,Panic,Block}` | `StopReason::{Return,PanicUnwind,Yield,NeedVm}` | `JitResult::Block` becomes `NeedVm` + VM executes to `StepResult::Block`. |
| `vm_call_trampoline()` | **Deleted** | JIT returns `NeedVm`; VM continues in same fiber. |
| `wait_for_trampoline_wake()` | **Deleted** | No busy waiting; fiber blocks normally via Scheduler. |
| `scheduler.trampoline_fibers[]` | **Deleted** | Only `scheduler.fibers[]` exists. |
| `call_extern_trampoline()` WaitIo loop | **Out of scope** | This design focuses on channel/select blocking. Extern I/O busy-wait is a separate refactor. **Future goal**: extern I/O should also use `NeedVm` → VM → Scheduler `io_waiters` path for mechanism consistency. |

---

## Current Architecture

### JIT -> VM Call Flow (Before)

```
1. JIT code calls `vm_call_trampoline()`
2. A trampoline fiber is created to run VM code
3. If VM code blocks:
   - trampoline fiber state = Blocked
   - `wait_for_trampoline_wake()` spins synchronously
   - scheduler runs until the trampoline is woken
4. The trampoline completes and returns results back to JIT
```

**Problems**:
- `wait_for_trampoline_wake` is synchronous busy waiting
- Trampoline blocked state is separated from regular fibers
- Wake-up logic is duplicated and inconsistent

---

## New Architecture

### JIT -> VM Call Flow (After)

```
1. JIT reaches a bytecode instruction that must be executed by the VM (e.g. `select`, channel ops, or any call into a non-JIT-able function)
2. JIT returns to the VM loop with `StopReason::NeedVm { vm_entry_pc, resume_pc }`
3. The VM loop pushes a `JitCallContext` onto `fiber.jit_call_stack` (capturing caller func_id / caller_frame_count / resume_pc)
4. The VM loop sets `fiber.exec_mode = ExecMode::Vm` and continues interpreting from `vm_entry_pc` in the CURRENT frame
    (VM may push additional frames naturally, e.g. for a Call)
5. If VM blocks:
   - `fiber.state = FiberState::Blocked`
   - control returns to the Scheduler (no synchronous waiting)
6. The Scheduler runs other fibers
7. A channel send/recv wakes the blocked fiber via the single `wake_fiber` path
8. The fiber continues VM execution; when the VM returns to the caller frame and reaches `resume_pc`, the VM loop pops `JitCallContext`
9. The VM loop sets `fiber.exec_mode = ExecMode::Jit`
10. The VM loop re-enters compiled code via `jit_resume(caller_func_id, resume_pc)`
```

This flow enforces the invariant: when a fiber becomes `Blocked`, the VM loop is on top (no native JIT frames remain on the system stack).

### Sequence Diagram: JIT → VM → Block → Wake → Resume

```
┌─────────┐      ┌──────────┐      ┌───────────┐      ┌─────────┐
│   JIT   │      │ VM Loop  │      │ Scheduler │      │ Channel │
└────┬────┘      └────┬─────┘      └─────┬─────┘      └────┬────┘
     │                │                  │                 │
     │ NeedVm(select) │                  │                 │
     │───────────────>│                  │                 │
     │                │                  │                 │
     │                │ push JitCallCtx  │                 │
     │                │ exec_mode = Vm   │                 │
     │                │                  │                 │
     │                │ execute select   │                 │
     │                │ (blocks)         │                 │
     │                │                  │                 │
     │                │ Blocked(Queue)   │                 │
     │                │─────────────────>│                 │
     │                │                  │                 │
     │                │                  │ run other fibers│
     │                │                  │<────────────────│
     │                │                  │                 │
     │                │                  │ chan.send()     │
     │                │                  │<────────────────│
     │                │                  │                 │
     │                │                  │ wake_fiber(id)  │
     │                │                  │────────────────>│
     │                │                  │                 │
     │                │ fiber scheduled  │                 │
     │                │<─────────────────│                 │
     │                │                  │                 │
     │                │ VM call returns  │                 │
     │                │ pop JitCallCtx   │                 │
     │                │ exec_mode = Jit  │                 │
     │                │                  │                 │
     │ jit_resume()   │                  │                 │
     │<───────────────│                  │                 │
     │                │                  │                 │
```

### Key Changes

| Before | After |
|--------|-------|
| `trampoline_fibers[]` parallel world | removed; only `fibers[]` |
| `wait_for_trampoline_wake()` busy waiting | removed; Scheduler runs other fibers, blocked fiber wakes normally |
| JIT glue owns its own blocking logic | JIT returns `StopReason`; Scheduler/VM owns blocking |
| `block_trampoline_for_queue()` | use the single `block_for_queue()` |
| `wake_trampoline()` | use the single `wake_fiber()` |
| Two blocking paths | exactly one blocking path |

---

## Data Structures

### Fiber (Modified)

```rust
pub struct Fiber {
    // Unified state (BlockReason is inside FiberState::Blocked)
    pub state: FiberState,
    
    // Execution engine
    pub exec_mode: ExecMode,
    
    // JIT call context stack (replaces trampoline fibers)
    pub jit_call_stack: Vec<JitCallContext>,
    
    // Existing fields
    pub stack: Vec<u64>,
    pub frames: Vec<CallFrame>,
    pub panic_state: Option<PanicState>,
    // ...
}

pub enum ExecMode {
    Vm,
    Jit,
}

impl Default for ExecMode {
    /// New fibers start in `Vm` mode. The VM loop will switch to `Jit` if the
    /// entry function is JIT-compiled (checked at first step).
    fn default() -> Self { ExecMode::Vm }
}

pub struct JitCallContext {
    /// Function ID of the JIT caller (the frame that will resume in JIT).
    /// Used by `jit_resume(func_id, resume_pc)` to look up the continuation entry point.
    pub caller_func_id: u32,
    /// Continuation PC where JIT should resume (explicitly computed by JIT, typically
    /// the post-instruction PC). This is NOT necessarily `vm_entry_pc + 1` if instructions
    /// have variable encoding sizes.
    pub resume_pc: u32,
    /// The expected `frames.len()` when the JIT caller is back at top of the call stack.
    /// This is captured at the moment JIT returns `NeedVm`, BEFORE VM executes the
    /// triggering instruction (which may push frames for Call).
    /// VM switches back to JIT when `frames.len() == caller_frame_count` AND
    /// `frames.last().pc == resume_pc`.
    pub caller_frame_count: usize,
    /// VM stack depth snapshot (for cleanup on panic / ensuring consistent stack).
    pub vm_stack_depth: usize,
}
```

### Scheduler (Simplified)

```rust
pub struct Scheduler {
    pub fibers: Vec<Box<Fiber>>,
    pub ready_queue: VecDeque<FiberId>,
    pub current: Option<FiberId>,
    pub free_slots: Vec<FiberId>,
    
    // removed: trampoline_fibers / trampoline_free_slots
    
    #[cfg(feature = "std")]
    pub io_waiters: HashMap<IoToken, FiberId>,
}
```

### FiberId (Type-Safe, Trampoline Removed)

```rust
/// Opaque fiber identifier.
///
/// We keep `enum` (not newtype) to preserve compile-time type safety:
/// - Prevents accidental `id.0 as usize` indexing bugs
/// - All fiber access MUST go through `scheduler.get_fiber(id)`
///
/// The `Trampoline` variant is removed; only `Regular` remains.
/// (Could simplify to `struct FiberId(u32)` if ALL indexing APIs are sealed,
/// but enum provides stronger "can't write the wrong code" guarantee.)
pub enum FiberId {
    Regular(u32),
}

impl FiberId {
    pub fn to_raw(self) -> u32;      // for storage (e.g., in channels)
    pub fn from_raw(raw: u32) -> Self;
}
```

---

## Implementation Plan

### Step 1: Add New Fields to Fiber

```rust
// fiber.rs
pub struct Fiber {
    pub exec_mode: ExecMode,
    pub jit_call_stack: Vec<JitCallContext>,
    // ... existing fields
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

### Step 2: Remove Trampoline from FiberId

```rust
// Remove FiberId::Trampoline variant, keep enum for type safety.
// All fiber access MUST go through scheduler.get_fiber(id).
pub enum FiberId {
    Regular(u32),
}
```

### Step 3: Remove Trampoline Infrastructure from Scheduler

Delete from `Scheduler`:
- `trampoline_fibers: Vec<Box<Fiber>>`
- `trampoline_free_slots: Vec<u32>`
- `create_trampoline_fiber()`
- `block_trampoline_for_queue()`
- `wake_trampoline()`
- All `is_trampoline` branches in `get_fiber`, `wake_fiber`, etc.

### Step 4: Modify VM Main Loop

**Key design decisions addressed in this pseudocode**:

1. **Initial JIT entry**: Only at function entry (pc=0), not arbitrary mid-function points. Continuation table only has entries for NeedVm resume points, not arbitrary PCs.

2. **VM panic handling**: `StepResult::Panic` triggers cleanup of `jit_call_stack` before returning to scheduler.

3. **JIT Return with non-JIT-able caller**: After JIT returns, check if current function is JIT-able; if not, switch to VM mode.

```rust
// vm/mod.rs
pub fn run_fiber(&mut self, fiber: &mut Fiber) -> FiberResult {
    loop {
        match fiber.exec_mode {
            ExecMode::Vm => {
                // Initial JIT entry: ONLY at function entry (pc=0), not mid-function.
                // Continuation table only supports NeedVm resume points, not arbitrary PCs.
                if fiber.jit_call_stack.is_empty() 
                    && fiber.frames.last().map_or(false, |f| f.pc == 0)
                    && self.can_jit(fiber.current_func_id()) 
                {
                    fiber.exec_mode = ExecMode::Jit;
                    continue;
                }

                match self.execute_vm_step(fiber) {
                    StepResult::Continue => {}
                    StepResult::Return => {
                        if fiber.frames.is_empty() {
                            return FiberResult::Return;
                        }
                    }
                    StepResult::Block(reason) => {
                        return FiberResult::Block(reason);
                    }
                    StepResult::Yield => {
                        return FiberResult::Yield;
                    }
                    // NEW: Handle panic during VM execution
                    StepResult::Panic => {
                        // Clean up jit_call_stack before returning
                        fiber.jit_call_stack.clear();
                        fiber.exec_mode = ExecMode::Vm;
                        return FiberResult::Panic;
                    }
                }

                // Re-entry check: switch back to JIT when continuation conditions are met
                if let Some(ctx) = fiber.jit_call_stack.last() {
                    if fiber.frames.len() == ctx.caller_frame_count
                        && !fiber.frames.is_empty()
                    {
                        let top = fiber.frames.last().unwrap();
                        if top.func_id == ctx.caller_func_id && top.pc as u32 == ctx.resume_pc {
                            debug_assert_eq!(fiber.stack.len(), ctx.vm_stack_depth,
                                "JIT re-entry: stack depth mismatch");
                            fiber.jit_call_stack.pop();
                            fiber.exec_mode = ExecMode::Jit;
                            continue;
                        }
                        if top.pc as u32 == ctx.resume_pc && top.func_id != ctx.caller_func_id {
                            panic!("JIT re-entry invariant violated: func_id mismatch");
                        }
                    }
                }
            }
            ExecMode::Jit => {
                match self.execute_jit(fiber) {
                    StopReason::Return => {
                        if fiber.frames.is_empty() {
                            return FiberResult::Return;
                        }
                        // NEW: Check if caller is JIT-able; if not, switch to VM
                        if !self.can_jit(fiber.current_func_id()) {
                            fiber.exec_mode = ExecMode::Vm;
                        }
                        // If caller is JIT-able, stay in JIT mode and continue
                    }
                    StopReason::Yield => {
                        return FiberResult::Yield;
                    }
                    StopReason::NeedVm { vm_entry_pc, resume_pc } => {
                        fiber.jit_call_stack.push(JitCallContext {
                            caller_func_id: fiber.current_func_id(),
                            resume_pc,
                            caller_frame_count: fiber.frames.len(),
                            vm_stack_depth: fiber.stack.len(),
                        });
                        fiber.exec_mode = ExecMode::Vm;
                        fiber.frames.last_mut().unwrap().pc = vm_entry_pc as usize;
                    }
                    StopReason::PanicUnwind => {
                        // JIT detected panic; clean up and let VM handle unwinding
                        fiber.jit_call_stack.clear();
                        fiber.exec_mode = ExecMode::Vm;
                        return FiberResult::Panic;
                    }
                }
            }
        }
    }
}
```

**Note on `StepResult::Panic`**: The current VM may use a different mechanism for panic (e.g., checking `fiber.panic_state`). The key point is that panic during VM execution must clean up `jit_call_stack` before returning to the scheduler. Adapt to actual VM API as needed.

### Step 5: Modify JIT Glue

Delete from `jit_glue.rs`:
- `vm_call_trampoline()`
- `wait_for_trampoline_wake()`
- All trampoline fiber management

Replace with:
- JIT code returns `StopReason::NeedVm` for **calls to non-JIT-able functions** (not for blocking instructions — those are excluded at Level 1)
- No synchronous waiting — the VM loop handles blocking naturally

### Step 6: Update JIT Codegen for Call Instructions

Per the **Two-Level JIT Strategy**:
- Level 1 instructions (defer/channel/select) **never appear** in JIT code — `can_jit()` already excluded them.
- Level 2: JIT must handle **calls to non-JIT-able functions** via `NeedVm`.

**For `Call` instruction**:
1. At JIT compile time, check if callee is JIT-able (query `can_jit(callee_func_id)`)
2. If JIT-able: emit direct JIT-to-JIT call (or `call_vm` helper for simplicity)
3. If **not** JIT-able: emit code that returns `StopReason::NeedVm { vm_entry_pc: current_pc, resume_pc: next_pc }`

**For `CallInterface` / `CallClosure`**:
- Callee unknown at compile time → always emit `NeedVm` (conservative)
- Future optimization: runtime check + fast path for JIT-able callee

**Continuation table**:
- JIT compiler registers `(func_id, resume_pc) -> native_addr` for each `NeedVm` return site
- VM loop uses this table to resume JIT at the correct native code address

### Done Criteria

**Structural removal**:
- `trampoline_fibers[]` is fully removed
- `FiberId::Trampoline` no longer exists
- No synchronous waiting loop in JIT/VM glue
- All tests pass: `./d.py test jit`

**Verifiable invariants** (hard metrics):

1. **No busy loop invariant**:
   - Grep check: no `while` or `loop` containing `scheduler.run_until` or `has_blocked()` patterns in JIT glue code.
   - Specifically: `wait_for_trampoline_wake()` and similar busy-wait patterns must not exist.

2. **State machine regression test** (`2026_01_29_jit_trampoline_select.vo`):
   - Verifies: JIT → NeedVm(select) → Block → Wake → Resume JIT
   - Asserts:
     - After wake, execution continues in JIT (not VM interpreting the rest)
     - Fiber has no residual "trampoline state" or extra `jit_call_stack` entries
     - Return value is correct (proves continuation resumed at right PC)

3. **Panic path invariant test** (new test required):
   - Scenario: JIT calls non-JIT-able func via NeedVm, callee panics before returning
   - Asserts:
     - `jit_call_stack` is fully cleared (not partially popped)
     - `fiber.stack` and `fiber.frames` are in consistent VM-recoverable state
     - Panic propagates correctly to caller (or is recovered if defer present)
   - This is the highest-risk path for subtle state corruption.

---

## Risks and Mitigations

### Risk 1: Engine Switching Correctness

**Problem**: Suspending a fiber while native JIT frames are still on the system stack makes blocking semantics and resumption unsafe.

**Mitigation**: Enforce the invariant strictly: cross-engine boundaries must be `StopReason`-driven.
JIT must return to the VM loop before any operation that may block or yield.

### Risk 2: Migration Surface Area

**Problem**: Many call-sites rely on the current split APIs.

**Mitigation**:
- Phase 1 keeps storage unchanged and focuses on API unification
- Migrate in phases and run the full test suite after each phase

### Risk 3: Performance

**Problem**: Engine switching adds overhead at boundaries.

**Mitigation**:
- `JitCallContext` stores only what is needed to re-enter compiled code
- Removing busy waiting is a net win

### Risk 4: Panic During VM Call

**Problem**: If VM code panics while `jit_call_stack` is non-empty, how do we unwind?

 **Mitigation**:
- `JitCallContext` stores enough information to safely recover VM-owned state when unwinding crosses a VM<->JIT boundary
  (at minimum: `vm_stack_depth`, plus the caller frame depth / continuation PC).
- Panic unwinding checks `jit_call_stack`; if non-empty, restore `vm_stack_depth` and discard pending JIT contexts until the fiber is in a consistent VM state
- In JIT mode, functions/instructions that are not supported by JIT are executed by the VM interpreter (fallback).
  This must not crash JIT mode.
- Key: panic unwind always happens in VM mode (invariant), so existing unwind logic applies

---

## Benefits

1. **Correctness**: remove state divergence bugs caused by two sources of truth
2. **Maintainability**: a single wake-up path; shorter debugging chains
3. **Simplicity**: delete duplicated code and `is_trampoline` branching
4. **Performance**: no busy waiting; better scheduling

---

## References

- `lang/crates/vo-vm/src/scheduler.rs` - Scheduler implementation
- `lang/crates/vo-vm/src/fiber.rs` - Fiber definition
- `lang/crates/vo-vm/src/vm/jit_glue.rs` - JIT/VM glue code
- `lang/test_data/2026_01_29_jit_trampoline_select.vo` - regression test
