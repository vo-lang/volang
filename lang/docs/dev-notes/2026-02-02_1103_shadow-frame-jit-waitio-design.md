---
description: Shadow-Frame JIT Scheduling + WaitIo (Correct Design)
---

# Shadow-Frame JIT Scheduling + WaitIo — Correct, Non-Compromising Design

This note specifies the **correct and complete** design for supporting `WaitIo` (and other suspension reasons) in a VM that integrates a synchronous JIT.

The key requirement is:

- The VM scheduler is the **only authority** that can block/park/resume fibers.
- JIT code **never blocks** inside native execution.
- When suspension is required, the JIT performs a **side-exit** back to the VM and provides enough information to resume correctly.

The design is centered around **shadow frames** (a.k.a. a *resume stack*): a minimal logical call stack tracked while executing JIT-only call chains, so that the VM can resume at the correct function and program counter after a `WaitIo`.


## Terminology

- **VM frame**: an interpreter-managed frame (function id, pc, bp, ret_reg, etc).
- **JIT frame**: an executing unit of native code corresponding to a function (or loop OSR region).
- **Shadow frame / ResumePoint**: a minimal record sufficient to resume a suspended JIT call chain.
- **Side-exit**: returning from JIT to VM with a reason (`Ok`, `Panic`, `Suspend`).
- **Resume PC discipline**: the contract that defines how resumption re-enters execution relative to suspension-capable instructions.


## Core Principle

If `WaitIo` may happen inside a nested JIT call chain (e.g. `A(jit) -> B(jit) -> extern(blocking)`), then the VM **must** preserve a logical continuation for the chain.

Because JIT-to-JIT calls do not (and should not) push VM frames for performance, we need **shadow frames**.

Without shadow frames, correct resumption after `WaitIo` is impossible unless we:

- force VM frames for every call (high overhead), or
- transform code into an async/CPS state machine (high complexity).

Shadow frames are the minimal correct solution.


## Non-Goals (Explicit)

These are not part of this design:

- Saving native CPU registers / native stack as resumable state.
- General deoptimization (HotSpot-style) from arbitrary machine PCs.
- Debugger-quality value reconstruction.
- Allowing truly blocking operations inside JIT.

The design should **fail fast** if unsupported behaviors are attempted.


## JIT Return Protocol

Every JIT entry returns a `JitResult`:

- `Ok`: completed normally; callee has written returns to the agreed return buffer/slots.
- `Panic`: panic occurred; VM must enter unwinding/panic handling.
- `Call`: callee requests the VM to execute a non-JIT-available call (VM fallback / VM-managed call boundary).
- `WaitIo`: callee requires I/O waiting; VM must park the current fiber and resume later.

**Important:** `WaitIo` is not an internal JIT pause. It is a request for the VM scheduler to park the fiber.


## Shadow Frames (Resume Stack)

### ResumePoint: minimal sufficient state

A shadow frame must contain only what is required to resume:

- `func_id`: which function to resume.
- `resume_pc`: the bytecode PC (or a logical PC) to resume from.
- `bp_or_offset`: how to locate this frame’s locals/args window inside `fiber.stack`.

```text
ResumePoint = { func_id, resume_pc, bp_or_offset }
```

This record is intentionally smaller than a VM frame.

### Resume Stack semantics

- On a nested call from a JIT function to another function, if that call is executed without pushing a VM frame, the caller pushes a `ResumePoint`.
- When the callee returns `Ok`, we pop one `ResumePoint` and resume the caller.
- When the callee returns `WaitIo`, we keep the resume stack intact and return to the VM.
- After I/O completion, the VM resumes the top resume point and continues unwinding the stack as `Ok` results come back.


## Stack Window / Offset Discipline

To avoid complicated copy protocols, we impose a single rule:

- JIT execution always reads/writes values from a well-defined window in `fiber.stack`.
- Nested calls advance a `current_offset` (or update `fiber.sp`) to point at the callee window.
- `ResumePoint.bp_or_offset` records the caller window so we can restore it.

This eliminates the need for:

- copying return values between disjoint buffers,
- storing extra “callee_offset/ret_var_offset” metadata,
- “return value patch-up” logic in the VM.


## Resume PC Discipline

This design uses exactly one discipline for suspension-capable operations.

### Model A: replay-at-PC

For suspension-capable opcodes (e.g. blocking extern call), resumption re-executes the same opcode.

- On first execution, the extern helper starts I/O and returns `WaitIo(token)`.
- On resume, the extern helper receives a `resume_token` and completes, returning `Ok` and writing results.

Contract:

- `resume_pc = current_pc`.

This model is the simplest because it avoids out-of-band result injection by the VM.


## I/O Waiting Contract

### Token lifecycle

- A blocking extern returns `WaitIo(token)`.
- The VM stores `token` in the fiber state (`BlockReason::Io(token)`), parks the fiber.
- When the I/O runtime reports readiness, the scheduler wakes the fiber and provides the `token` as a resume token.

### Resume token semantics

The resume token must ensure **at-most-once** side effects:

- The first call with no token is allowed to submit a new I/O request.
- A resumed call with a token must *not* submit again.
- If a token is unexpected or invalid, the runtime must fail (panic), not silently fallback.


## Correctness Invariants (Must Hold)

### 1) At-most-once side effects

No operation may be performed twice due to resumption logic.

This is the most important invariant for `WaitIo`, channel ops, and any other side-effecting primitives.

### 2) Precise resumption target

After `WaitIo`, the VM must resume:

- the correct `func_id` (the one that actually blocked),
- at the correct `resume_pc`,
- with the correct stack window (`bp_or_offset`).

### 3) GC visibility on side-exit

Before returning `WaitIo`/`Call` to the VM, any live GC references must be made visible to the VM/GC root set.

Fail-fast rule:

- If the JIT cannot provide correct GC roots at a suspension boundary, it must not attempt to suspend.

### 4) No synchronous blocking VM calls inside JIT

A “sync VM call in a separate fiber” is incompatible with `WaitIo` because the scheduler must park/resume the *current* fiber.

Rule:

- Any call path that may reach a blocking operation must go through the unified `Call`/`WaitIo` protocol, not a sync helper.


## VM Integration (Authoritative Scheduler)

### VM is the only one that blocks

When JIT returns `WaitIo(token)`:

1. VM records the current continuation by pushing a `ResumePoint`.
2. VM updates the top execution frame to reflect the resume pc.
3. VM transitions fiber state to `Blocked(Io(token))`.
4. Scheduler parks the fiber.

### Resume entry

On I/O readiness:

1. scheduler moves fiber to runnable,
2. VM re-enters JIT execution with the resume token set in the context,
3. VM uses the resume stack to continue from the top resume point.


## Unified Call Handling

### Why a unified call path is mandatory

There must be exactly one call protocol for any call that can appear in JIT code:

- direct JIT-to-JIT call (compiled callee)
- VM fallback call (not compiled / unsupported)
- closure and interface dispatch

If closure/interface calls bypass the unified protocol (e.g. by doing synchronous VM calls), the system becomes incorrect under `WaitIo`.

### Correct contract for closure/interface calls

- JIT must compute `func_id` and argument layout.
- If callee is compiled: execute via JIT (still within the unified call chain).
- Otherwise: return `JitResult::Call` with call request information.

No other behavior is acceptable.


## Minimal API Surface

This design intentionally keeps the public surface small.

### JitContext (minimum fields)

- pointers to VM/fiber/module runtime state
- `wait_io_token: u64` (resume token)
- `call_request: { func_id, arg_start, entry_pc, resume_pc, ret_slots }`
- optional pointer to per-fiber call dispatcher/resume stack

### JitResult

- `Ok | Panic | Call | WaitIo`

### ResumePoint

- `func_id, resume_pc, bp_or_offset`


## Failure Modes (Expected)

The system should fail loudly when invariants are violated.

Examples:

- `WaitIo` occurs but no resume point exists (or it is inconsistent): panic.
- Blocking operation attempted in a sync VM call path: panic.
- Resume token mismatch / missing runtime pending state: panic.


## Implementation Plan (Phased, Always Correct)

### Phase 0 — Lock down contracts

- Use Model A (replay-at-PC) for all suspension-capable operations.
- Define exact meaning of `resume_pc`.
- Define the stack window discipline and where args/rets live.

### Phase 1 — Shadow frames in VM (single source of truth)

- Implement a per-fiber `resume_stack` of `ResumePoint`.
- Implement VM-side loop that:
  - executes JIT function,
  - handles `Ok/Panic/Call/WaitIo`,
  - maintains push/pop symmetry.

### Phase 2 — WaitIo via blocking extern (end-to-end)

- Extern returns `WaitIo(token)`.
- VM parks fiber.
- On resume, token is provided.
- Re-execution completes and returns `Ok`.

Add regression tests:

- nested call chain `A->B->extern WaitIo`.

### Phase 3 — VM fallback via `Call`

- Any unsupported callee returns `Call`.
- VM executes callee (VM frames) then resumes JIT.

Add tests:

- `jit caller -> vm callee -> blocking extern WaitIo`.

### Phase 4 — closure/interface dispatch

- Implement closure and iface calls through the same unified protocol.
- Ensure `WaitIo` works through these paths.

### Phase 5 — OSR (optional)

- OSR must obey the same protocol.
- OSR resume must interact correctly with resume stack.


## Notes on Deletion/Refactor Strategy

If the codebase already contains multiple competing dispatchers/trampolines:

- Keep exactly one implementation as the source of truth.
- Delete or fully isolate the others.
- Never keep two partially working systems.


## Summary

Supporting `WaitIo` in a synchronous JIT is **not** about suspending native execution.

It is about implementing a correct, minimal continuation mechanism:

- side-exit + `JitResult`
- VM-authoritative scheduling
- per-fiber shadow frames (resume stack)
- a single unified call protocol

Anything less will be incorrect under nested calls.
