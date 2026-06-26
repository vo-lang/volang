# VM Runtime Hardening Plan

Status, 2026-06-11: this document records a source-backed review of the
performance-sensitive VM runtime surface. It is a development plan for
correctness fixes and low-risk refactors, not a user-facing spec.

Update, 2026-06-12: the VM runtime-boundary implementation closed the
endpoint close, generation wake-key, host-event payload, and typed JIT extern
suspend items called out below. Sections marked as implemented describe the
current source-backed behavior rather than open work.

Current source wins over this document. Re-check the referenced files before
starting work because VM, JIT, GC, and FFI code changes frequently.

Related context:

- [`vm-production-readiness.md`](vm-production-readiness.md)
- [`lang/crates/vo-vm/src/vm/mod.rs`](../../crates/vo-vm/src/vm/mod.rs)
- [`lang/crates/vo-vm/src/fiber.rs`](../../crates/vo-vm/src/fiber.rs)
- [`lang/crates/vo-vm/src/gc_roots.rs`](../../crates/vo-vm/src/gc_roots.rs)
- [`lang/crates/vo-vm/src/vm/jit/`](../../crates/vo-vm/src/vm/jit/)
- [`lang/crates/vo-runtime/src/ffi/mod.rs`](../../crates/vo-runtime/src/ffi/mod.rs)
- [`gc-correctness-testing-framework.md`](gc-correctness-testing-framework.md)
- [`jit-fact-source.md`](jit-fact-source.md)
- [`vm-module-verifier-hardening-plan.md`](vm-module-verifier-hardening-plan.md)

## Scope

The review covered:

- interpreter call dispatch and borrowed call frames
- fiber stack/frame capacity behavior
- JIT frame materialization and JIT-to-VM call transitions
- extern call replay, host-event replay, and closure callback replay
- GC root scanning for closure replay, fibers, scheduler state, and endpoint
  state
- scheduler wake/block state for queues, host events, I/O, and island endpoint
  responses

The review intentionally did not propose broad opcode dispatch rewrites. The
main interpreter loop is a hot path; correctness work should first target cold
runtime boundaries and shared helpers.

## Performance-Sensitive Change Rules

Use these rules before touching `vm/mod.rs` or call dispatch:

1. Prefer correctness fixes in shared helpers over reshaping the dispatch loop.
2. Keep hot opcode arms flat unless measurement shows a win from extraction.
3. Split cold code first: module load, extern bridge setup, replay handling,
   scheduler result handling, and tests.
4. Avoid adding heap allocation or trait dispatch to per-instruction hot paths.
5. For refactors, keep behavior-preserving moves separate from semantic fixes.
6. Validate VM/JIT/GC boundaries together when a change crosses stack frames,
   root scanning, extern replay, or scheduler blocking.

## Confirmed Findings

### F0. Multi-step extern closure replay loses earlier results

Severity: P0

`ClosureReplayState::take_for_extern` removes all cached closure replay results
from the fiber before each extern re-execution. If an extern consumes replay
result A and then returns another `CallClosure` for result B, the next full
replay only has B in the fiber-owned cache. The extern expects to replay from
the beginning and read A then B, so the replay sequence can silently reorder or
lose results.

This also explains why the typed-root issue below is correctness-critical: the
current ownership transfer loses both old replay entries and their slot types.

Current references:

- [`fiber.rs`](../../crates/vo-vm/src/fiber.rs) `take_for_extern`
- [`vm/mod.rs`](../../crates/vo-vm/src/vm/mod.rs) interpreter `CallExtern`
- [`vm/jit/extern_call.rs`](../../crates/vo-vm/src/vm/jit/extern_call.rs) JIT
  extern bridge
- [`vo-runtime/src/ffi/mod.rs`](../../crates/vo-runtime/src/ffi/mod.rs)
  `resume_closure_result` and terminal-call verification

Fix direction:

- keep replay results in a persistent VM-owned replay log until the extern
  finishes with a terminal result
- pass a cursor/view into `ExternCallContext` instead of moving entries out of
  the fiber on every replay
- preserve slot types with each entry
- clear the replay log only on terminal `Ok` or `Panic`, not on intermediate
  `CallClosure`

Required tests:

- extern first requests closure A, then on replay consumes A and requests
  closure B, then on final replay consumes A followed by B
- run the same scenario through interpreter `CallExtern` and JIT extern bridge
- include one result with a `GcRef` slot so root retention is tested too

### F1. Closing a queue wakes blocked senders as if send succeeded

Severity: P0

`queue_close_core` collects waiting receivers and waiting senders into one
`waiters` vector. The VM then calls `wake_waiter` for every waiter. A simple
blocked `QueueSend` does not rewind its PC before blocking, so waking the sender
continues after the send as if it succeeded. A blocked select-send case also
completes the selected send case without checking that the channel was closed.

Closing a channel with blocked senders must wake those senders into a
send-on-closed panic, not a successful send.

Current references:

- [`exec/queue.rs`](../../crates/vo-vm/src/exec/queue.rs) `queue_close_core`
- [`vm/mod.rs`](../../crates/vo-vm/src/vm/mod.rs) `QueueAction::Close`
- [`exec/select.rs`](../../crates/vo-vm/src/exec/select.rs)
  `complete_woken_case`

Fix direction:

- separate close wake results into receiver close wake, sender close panic, and
  endpoint close response
- for blocked senders, arrange replay/panic state so the resumed fiber sees
  `RuntimeTrapKind::SendOnClosedChannel`
- make select-send wake check the queue close state before completing the case

Required tests:

- unbuffered send blocks, another fiber closes the channel, sender recovers
  `send on closed channel`
- blocked `select { case ch <- v: }` is woken by close and panics
- run native VM and JIT targets

### F2. Closure replay caches return values before defers finish

Severity: P1

Closure replay return handling pushes the closure return values into
`closure_replay.results` before running that closure's defers. For named
returns, a defer can modify the return value after it has already been cached,
so the extern sees the pre-defer value. The interpreter path also caches stack
slots directly before checking `ReturnFlags::HEAP_RETURNS`, so heap/named
returns can be cached as heap return references rather than final values.

Current references:

- [`exec/unwind.rs`](../../crates/vo-vm/src/exec/unwind.rs) JIT replay return
  boundary
- [`exec/unwind.rs`](../../crates/vo-vm/src/exec/unwind.rs) interpreter
  closure replay return boundary
- [`exec/unwind.rs`](../../crates/vo-vm/src/exec/unwind.rs) defer completion
  path for closure replay

Fix direction:

- for closure replay with defers, store return values in `UnwindingState`
  first, run defers, then append final replay results after defer completion
- handle heap/named returns using the same finalization path as normal returns
- only call `closure_replay.pop_depth()` once the replay result is finalized

Required tests:

- replayed closure with `func() (x int) { defer func(){ x = 2 }(); x = 1;
  return }` must replay `2`, not `1`
- repeat with heap/named return shape and JIT return path

### F3. JIT frame materialization can collapse recursive frames with same func/bp

Severity: P1

`materialize_jit_frames` treats a frame as already materialized when any
existing frame has the same `(func_id, bp)`. A zero-arg, zero-local recursive
JIT call can reuse the same `bp` for multiple logical frames. During side exit,
materialization can then update one existing frame instead of pushing the
missing recursive frames, losing call depth, defer depth, and unwind state.

Current references:

- [`vm/jit/materialize.rs`](../../crates/vo-vm/src/vm/jit/materialize.rs)
  existing-frame lookup
- [`vm/jit/frame.rs`](../../crates/vo-vm/src/vm/jit/frame.rs) resume point
  recording

Fix direction:

- materialize resume-stack entries positionally instead of deduplicating by
  `(func_id, bp)`; or
- track an explicit materialized-frame identity in resume points
- keep any special "entry frame already exists" handling limited to the known
  entry frame, not every matching frame

Required tests:

- construct entry frame plus two resume points with identical `func_id` and
  `bp`; materialization must produce all logical frames
- add a recursive zero-slot language/JIT regression if the source language can
  generate that shape

### F4. Defer call frame allocation can Rust-panic instead of VM stack overflow

Severity: P1

`call_defer_entry` uses infallible `reserve_slots_at` and `push_call_frame`.
Capacity failure therefore panics in Rust instead of returning the same
recoverable stack-overflow path as normal calls.

Current references:

- [`exec/unwind.rs`](../../crates/vo-vm/src/exec/unwind.rs) `call_defer_entry`
- [`fiber.rs`](../../crates/vo-vm/src/fiber.rs) infallible reserve/push helpers

Fix direction:

- switch defer frame setup to fallible helpers
- preflight stack and call-frame capacity before copying defer args
- route failure through `RuntimeTrapKind::StackOverflow`

Required tests:

- defer target with a very large frame returns recoverable stack overflow
- near `MAX_CALL_FRAMES`, executing a defer returns a VM trap instead of a host
  panic

### F5. WaitIo resume token contract can be bypassed by returning WaitIo again

Severity: P1

`verify_post_call` enforces that a provided resume I/O token must be consumed.
However, `ExternRegistry` skips post-call verification for `ExternResult::WaitIo`
because it treats WaitIo as an intermediate result. If an extern is re-executed
with `resume_io_token=Some(old)` and ignores it while returning `WaitIo { token:
new }`, the old completion is silently dropped.

Current references:

- [`vo-runtime/src/ffi/mod.rs`](../../crates/vo-runtime/src/ffi/mod.rs)
  `take_resume_io_token`
- [`vo-runtime/src/ffi/mod.rs`](../../crates/vo-runtime/src/ffi/mod.rs)
  `verify_post_call`
- [`vo-runtime/src/ffi/mod.rs`](../../crates/vo-runtime/src/ffi/mod.rs)
  WaitIo verification skip
- [`vm/mod.rs`](../../crates/vo-vm/src/vm/mod.rs) interpreter `CallExtern`
  resume-token input assembly

Fix direction:

- when a resume token is present, require it to be consumed even if the extern
  returns another `WaitIo`
- keep first-call `WaitIo` intermediate behavior unchanged
- mirror the rule in JIT extern bridge tests

Required tests:

- FFI unit: context has `resume_io_token=Some(x)`, extern returns `WaitIo(y)`
  without consuming x, and registry returns contract error
- VM/JIT replay-at-PC scenario for the same contract

### F6. Borrowed call frame setup is not atomic

Severity: P1

`Fiber::try_push_borrowed_call_frame` mutates the fiber before the final
fallible frame push:

- reserves stack slots and updates `sp`
- shrinks the caller frame's `scan_slots`
- then calls `try_push_call_frame_extended`

If the stack reservation succeeds but call-frame capacity fails, the caller
receives a stack-overflow path with a modified `sp` and modified caller scan
extent, but no callee frame was pushed.

Current references:

- [`fiber.rs`](../../crates/vo-vm/src/fiber.rs) around
  `try_push_borrowed_call_frame`
- [`exec/call.rs`](../../crates/vo-vm/src/exec/call.rs) for interpreter
  `Call`, `CallClosure`, and `CallIface`
- [`vm/jit/invoke.rs`](../../crates/vo-vm/src/vm/jit/invoke.rs) for JIT call
  dispatch
- [`vm/jit/materialize.rs`](../../crates/vo-vm/src/vm/jit/materialize.rs) for
  regular call materialization

Fix direction:

- preflight `try_reserve_call_frame()` before changing `sp` or caller
  `scan_slots`; or
- introduce an atomic frame setup helper that records old `sp`, old caller
  `scan_slots`, and zeroed ranges, then commits only after all capacity checks
  pass.

Required tests:

- construct a fiber at `MAX_CALL_FRAMES` and verify failed borrowed-call setup
  leaves `sp` and caller `scan_slots` unchanged
- cover interpreter static `Call`, dynamic `CallClosure`, dynamic `CallIface`,
  JIT dispatch, and JIT materialization callers
- cover stack overflow panic location after failed setup

### F7. Extern closure replay call setup is not atomic and does not validate payload

Severity: P1

`prepare_extern_closure_replay_call` reserves a new frame window, zeros/copies
slots, and only then tries to push the call frame. If the call-frame push fails,
the fiber has already been modified.

The same helper dereferences `closure_ref` and copies `args` from
`ExternResult::CallClosure` without applying the normal `CallClosure` guards.
It does not reject nil closure refs, unsupported receiver layouts, or argument
slot mismatches. Too many args can panic in `copy_slots_from_slice`; too few
args leave zero-filled parameters and silently change the closure call.

Current references:

- [`vm/mod.rs`](../../crates/vo-vm/src/vm/mod.rs) around
  `prepare_extern_closure_replay_call`
- [`vm/jit/transition/wait.rs`](../../crates/vo-vm/src/vm/jit/transition/wait.rs)
  for the JIT extern-suspend replay path
- [`vo-runtime/src/ffi/mod.rs`](../../crates/vo-runtime/src/ffi/mod.rs) for
  `ExternResult::CallClosure`

Fix direction:

- compute the closure call layout before any stack mutation
- reject null or non-closure refs before reading closure headers
- validate `layout.arg_offset + args.len() <= local_slots`
- validate `args.len()` against the callee's required argument slots after
  receiver/closure slot handling
- share the normal `CallClosure` layout guard for unsupported `arg_offset > 1`
- preflight call-frame capacity before reserving or zeroing stack slots
- return `ExecResult::JitError` or a contract error instead of panicking

Required tests:

- extern returns `CallClosure` with null closure ref
- extern returns `CallClosure` with too many args
- extern returns `CallClosure` with too few args
- method closure / receiver layout with unsupported arg offset is rejected
- call-frame capacity failure leaves `sp`, frame count, and replay state
  unchanged
- run both interpreter and JIT extern replay paths

### F8. Closure replay results lose typed root metadata while extern runs

Severity: P1 design risk, P2 current bug risk

`ClosureReplayState` stores replay results as `(values, slot_types)` so GC root
scanning can distinguish `GcRef`, interface, and raw value slots. Before an
extern is re-executed, `take_for_extern` strips the slot types and moves only
`Vec<Vec<u64>>` into `ExternCallContext`.

That creates a root ownership gap: replay results are no longer in
`fiber.closure_replay.results`, and the runtime context does not retain the
slot metadata needed for precise scanning. Today many extern allocations do not
force immediate VM root scanning, but `ExternCallContext::gc()` exposes the GC
to host/extension code, and future GC scheduling changes could turn this into a
soundness bug.

Current references:

- [`fiber.rs`](../../crates/vo-vm/src/fiber.rs) `ClosureReplayState`
- [`gc_roots.rs`](../../crates/vo-vm/src/gc_roots.rs) replay-result root scan
- [`vm/mod.rs`](../../crates/vo-vm/src/vm/mod.rs) interpreter `CallExtern`
- [`vm/jit/extern_call.rs`](../../crates/vo-vm/src/vm/jit/extern_call.rs) JIT
  extern bridge
- [`vo-runtime/src/ffi/call.rs`](../../crates/vo-runtime/src/ffi/call.rs)
  `ExternFiberInputs`
- [`vo-runtime/src/ffi/mod.rs`](../../crates/vo-runtime/src/ffi/mod.rs)
  `resume_closure_result` and `gc`

Fix direction:

- preserve typed replay results in `ExternFiberInputs` and `ExternCallContext`;
  or
- move replay results into a VM-visible temporary root list for the duration of
  the extern call; and
- make `resume_closure_result` consume typed entries without discarding root
  metadata before the result is fully consumed.

Required tests:

- closure replay returns a `GcRef`, extern allocates before consuming or while
  consuming the replay result, and the object survives GC stress
- interface return replay with raw value slot and GC-backed data slot
- JIT extern bridge version of the same scenario

### F9. Home endpoint close can wake remote waiters through the wrong channel

Severity: P1

Status: implemented by the VM runtime-boundary work.

Endpoint send/recv waiters use a generation-keyed endpoint response key so
stale responses cannot wake a reused fiber slot and so
`pending_island_responses` is decremented in one place. Home endpoint close now
keeps remote send/recv completions on the endpoint-response path, preserves the
full generation-bearing `fiber_key`, and routes local wake effects through
`RuntimeTransition` wake commands. Endpoint tombstone broadcasts use
`EndpointResponseKind::Closed` as endpoint state notification, not as raw fiber
wake authority.

Current references:

- [`vm/mod.rs`](../../crates/vo-vm/src/vm/mod.rs) `QueueAction::Close`
- [`vm/types.rs`](../../crates/vo-vm/src/vm/types.rs) `wake_waiter`
- [`vm/island_shared.rs`](../../crates/vo-vm/src/vm/island_shared.rs)
  endpoint response resume path

Implemented behavior:

- `QueueWaiter` stores `fiber_key`, not a raw `fiber_id`;
- remote close responses use `send_endpoint_response(..., fiber_key)` and
  resume through `RuntimeCommand::endpoint_response`;
- local queue close wakes use `WakeCommand::queue_closed_receiver` or
  `WakeCommand::queue_closed_sender` and are applied by the transition applier;
- endpoint response application validates the generation-bearing key, endpoint
  id, registered send/recv operation kind, and current queue wait before
  waking a fiber.

Implemented tests:

- `vm::types::tests::closed_remote_sender_uses_endpoint_response_fiber_key`
- `vm::types::tests::closed_remote_receiver_uses_endpoint_response_fiber_key`
- `vm::island_shared::tests::endpoint_response_ignores_stale_fiber_generation_key`
- `vm::island_shared::tests::endpoint_response_rejects_wrong_wait_source`
- `vm::island_shared::tests::endpoint_response_rejects_wrong_endpoint_id`
- `vm::island_shared::tests::endpoint_response_rejects_wrong_response_kind`
- `vm::tests::runtime_command_island_wake_rejects_stale_generation_key`

### F10. Host-event wake data can leak into an unrelated extern call

Severity: P2

Status: implemented by the VM runtime-boundary work.

`wake_host_event_with_data` now accepts data only for
`HostEventWaitAndReplay`. Non-replay `HostEventWait` wake attempts with data
return `false`, leave `resume_host_event_data` empty, and do not poison the next
extern call.

Current references:

- [`scheduler.rs`](../../crates/vo-vm/src/scheduler.rs)
  `wake_host_event_with_data`
- [`vm/mod.rs`](../../crates/vo-vm/src/vm/mod.rs) `wake_host_event_with_data`
  and `CallExtern` input assembly
- [`vo-runtime/src/ffi/mod.rs`](../../crates/vo-runtime/src/ffi/mod.rs)
  `take_resume_host_event_data` and `verify_post_call`

Implemented behavior:

- `RuntimeCommand::host_event_wake_with_data` enters through the command
  bridge;
- the scheduler matches both token and replay source before attaching payload
  data;
- app-runtime and Studio continue to call VM host-event ingress, which now
  routes through the same command bridge.

Implemented tests:

- `scheduler::tests::wake_host_event_with_data_ignores_data_for_non_replay_waiter`
- `vm::tests::runtime_command_rejects_host_event_source_mismatch`
- `vm::tests::runtime_command_host_event_wake_uses_boundary_applier`

### F11. Dynamic call return offsets use unchecked `u16` addition

Severity: P2

Dynamic call setup computes return offsets with unchecked `u16` arithmetic or
casts from `usize` to `u16`. Malformed bytecode with a borrowed-start near
`u16::MAX` and non-zero args can panic in debug builds or wrap in release,
placing return slots at the wrong offset.

Current references:

- [`exec/call.rs`](../../crates/vo-vm/src/exec/call.rs) `CallClosure`
- [`exec/call.rs`](../../crates/vo-vm/src/exec/call.rs) `CallIface`
- [`vm/jit/invoke.rs`](../../crates/vo-vm/src/vm/jit/invoke.rs) JIT call
  dispatch

Fix direction:

- use checked addition for `borrowed_start + arg_slots`
- return `ExecResult::JitError` on overflow
- move the invariant into the shared verifier if the bytecode shape can be
  rejected before execution

Required tests:

- malformed `CallClosure` and `CallIface` with near-`u16::MAX` borrowed start
  return structured VM errors
- JIT dispatch rejects equivalent malformed request

### F12. Replay PC rewind should use checked invariants

Severity: P3

Status: implemented for the `CallExtern` replay boundary.

`CallExtern` now derives the fetched PC with `checked_sub(1)` and uses
`ResumePolicy` helpers when replay-style extern results park the fiber. A
malformed frame with `pc == 0` returns a structured VM error instead of
silently replaying PC 0.

Current references:

- [`vm/mod.rs`](../../crates/vo-vm/src/vm/mod.rs) `HostEventWaitAndReplay`,
  `WaitIo`, and `CallClosure`
- [`vm/mod.rs`](../../crates/vo-vm/src/vm/mod.rs) queue replay checked-sub
  pattern

Implemented behavior:

- `HostEventWaitAndReplay`, `WaitIo`, and `CallClosure` replay use
  `set_current_frame_pc_for_resume`;
- queue replay branches use `checked_sub(1)`;
- JIT and OSR transport values decode through `RuntimeTransition` adapters
  before mapping to legacy result enums.

Implemented tests:

- `vm::tests::host_event_replay_block_at_pc_zero_does_not_rewind`
- `vm::tests::host_event_replay_block_does_not_require_current_fiber_rewind`
- `vm::tests::host_event_replay_block_does_not_require_current_frame_rewind`
- `vm::jit::bridge_result::tests::jit_host_replay_transport_decodes_to_runtime_transition`

### F13. Host-event suspension can hide pending island work

Severity: P2

Status: implemented by the VM runtime-boundary work.

`wait_for_work` now checks outbound island commands and
`pending_island_responses` before returning `SuspendedForHostEvents`. Mixed
host-event/island workloads therefore keep the host loop driving island work
instead of parking only on host events.

Current references:

- [`vm/mod.rs`](../../crates/vo-vm/src/vm/mod.rs) `wait_for_work`
- [`scheduler.rs`](../../crates/vo-vm/src/scheduler.rs) host-event waiter
  tracking

Implemented behavior:

- pending island responses return `SchedulingOutcome::Suspended`;
- host-event-only suspension is reported only after island outbound work and
  pending responses are clear.

Implemented tests:

- `vm::tests::wait_for_work_prioritizes_pending_island_response_over_host_events`
- `vm::tests::run_scheduled_returns_suspended_when_waiting_for_island_response`

## Hardening Opportunities

### H1. Queue waiters use generation-bearing fiber keys

Status: implemented by the VM runtime-boundary work.

Queue, select, endpoint response, I/O, host-event, and island wake identities
now use generation-bearing `FiberWakeKey`/`WaitRegistration` data. The island
wire format carries `fiber_key: u64` for `WakeFiber`, `EndpointRequest`, and
`EndpointResponse`, so the high-generation bits are not truncated in transport.

Implemented behavior:

- `Fiber::wake_key_packed()` is used when registering queue/select waiters;
- scheduler wake APIs decode and validate `FiberWakeKey` before touching a
  fiber;
- select wake additionally validates active `select_id` and case identity;
- host-event and I/O waits record `WaitRegistration` with `WaitSource`;
- island wake ingress uses `RuntimeCommand::island_wake`.

Implemented tests cover raw slot rejection, stale generation rejection,
source-mismatch rejection, stale select identity, and island command codec
round-tripping of high generation bits.

### H2. `jit_extern_suspend` can temporarily hold unscanned GC refs

Status: implemented by the VM runtime-boundary work.

`JitExternSuspend::CallClosure` now stores a `closure_ref` plus typed argument
payloads, and the GC root scanner scans that suspend state. JIT extern replay
derives payload metadata through `FrameCallBuilder`, rejects metadata drift,
and keeps GC-backed pending payloads visible until they are consumed.

Implemented tests include typed extern replay argument derivation, payload
metadata drift rejection, and
`vm::tests::gc_root_matrix_scans_jit_extern_suspend_typed_payload`.

### H3. JIT heap errdefer checks should use one metadata source

`invoke.rs` comments say function metadata is the ground truth for JIT return
handling, but heap-return errdefer detection reads `ctx.ret_gcref_start` while
`handle_jit_ok_return` later uses `FunctionDef.heap_ret_gcref_start`. If those
ever drift, errdefer execution can be skipped or triggered incorrectly.

Fix direction:

- use `FunctionDef.heap_ret_gcref_start` consistently; or
- validate that the JIT context value equals the function metadata before using
  either value

Tests:

- focused unit with heap error returns where context and function metadata
  intentionally disagree, expecting a structured JIT error

### H4. `finish_load` can be misused without resetting resolved externs

The public `load` and `load_with_extensions` paths set
`state.resolved_externs` before calling `finish_load`, so the normal path is
safe. Tests and future internal callers can call `finish_load` directly and
leave stale resolved extern entries from a previous module.

Fix direction:

- pass a `ResolvedExternTable` into `finish_load`; or
- reset it inside `finish_load` and require test helpers to set it after load.

### H5. Debug-only extern return validation should become a contract option

`debug_validate_extern_returns` catches invalid `GcRef` or interface returns in
debug builds. Some safe FFI helpers already validate references when writing
returns, but raw slot writes and extension boundaries are easier to misuse. The
interpreter has this debug check; JIT direct extern bridge should expose an
equivalent development-time validation path.

Fix direction:

- add a cheap opt-in validation mode for development and GC stress runs
- route failures through `ExternContractError` or `ExecResult::JitError`
- avoid enabling full validation on the default hot path without measurement

## Refactor Workstreams

### Workstream A: Atomic frame setup

Owner paths:

- `lang/crates/vo-vm/src/fiber.rs`
- `lang/crates/vo-vm/src/exec/call.rs`
- `lang/crates/vo-vm/src/exec/unwind.rs`
- `lang/crates/vo-vm/src/vm/jit/materialize.rs`
- `lang/crates/vo-vm/src/vm/jit/invoke.rs`

Deliverables:

- one shared frame setup helper for borrowed calls
- one shared helper for non-borrowed stack-window call setup
- explicit rollback or preflight semantics documented in tests
- no extra allocation on successful hot call paths

### Workstream B: Extern replay ownership

Owner paths:

- `lang/crates/vo-vm/src/fiber.rs`
- `lang/crates/vo-vm/src/gc_roots.rs`
- `lang/crates/vo-vm/src/exec/unwind.rs`
- `lang/crates/vo-vm/src/vm/mod.rs`
- `lang/crates/vo-vm/src/vm/jit/extern_call.rs`
- `lang/crates/vo-runtime/src/ffi/`

Deliverables:

- replay logs survive across multi-step extern closure replay
- typed replay results survive while extern code is running
- closure replay arg layout is validated before stack mutation
- closure replay caches final post-defer return values
- replay depth/result lifecycle has explicit tests for nested replay, defers,
  heap returns, and panic

### Workstream C: Scheduler wait handles

Owner paths:

- `lang/crates/vo-vm/src/scheduler.rs`
- `lang/crates/vo-vm/src/exec/queue.rs`
- `lang/crates/vo-vm/src/exec/select.rs`
- `lang/crates/vo-vm/src/vm/mod.rs`
- `lang/crates/vo-vm/src/vm/types.rs`
- `lang/crates/vo-vm/src/vm/island_shared.rs`

Deliverables:

- close wakes blocked receivers and blocked senders through distinct semantics
- remote endpoint close uses endpoint-response accounting and generation keys
- island work is not hidden by host-event suspension
- host-event data is replay-only
- wake APIs return observable success/failure
- generation-keyed wake handles are used where waiters can outlive one
  scheduler turn

### Workstream D: Cold-path `vm/mod.rs` split

This should happen after the above correctness fixes, not before them.

Safe first splits:

- `vm/load.rs`: validation, extern resolution, `finish_load`
- `vm/extern_call.rs`: interpreter extern bridge and replay result handling
- `vm/scheduling.rs`: `handle_exec_result`, wait-for-work, host wake wrappers
- `vm/tests/`: move large unit-test helpers out of the hot implementation file

Avoid first:

- splitting every opcode arm into tiny functions
- changing hot dispatch control flow while fixing replay or frame bugs
- adding generic trait-based opcode handlers

## Validation Matrix

For documentation-only updates:

```sh
./d.py ci task docs-lint
```

For Workstream A:

```sh
cargo test -p vo-vm --features jit
./d.py test jit
./d.py test gc
```

For Workstream B:

Add focused tests for:

- multi-step closure replay preserving all prior results
- closure replay with defers and heap/named returns
- replay result roots under GC stress
- WaitIo resume-token contract enforcement

```sh
cargo test -p vo-runtime gc
cargo test -p vo-vm --features jit gc
cargo test -p vo-vm --features jit
./d.py test gc
./d.py test jit
```

For Workstream C:

Add focused tests for:

- close wakes blocked senders into send-on-closed panic
- select send blocked on a closing channel panics
- remote endpoint close decrements pending response accounting
- host-event waiters do not hide pending island work
- non-replay host-event wake data does not poison a later extern

```sh
cargo test -p vo-vm --features jit scheduler
cargo test -p vo-vm --features jit queue
./d.py test jit
```

For Workstream D:

```sh
cargo test -p vo-vm --features jit
cargo test -p vo-jit
./d.py test jit
```

Run wider gates when a change crosses codegen, bytecode verification, or
runtime metadata:

```sh
cargo test -p vo-common-core
cargo test -p vo-codegen
cargo test -p vo-jit
cargo run -q -p vo-dev -- test lint --suite lang --strict
```
