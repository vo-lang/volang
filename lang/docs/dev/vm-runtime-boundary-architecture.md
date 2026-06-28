# VM Runtime Boundary Architecture

Status, 2026-06-12: implemented VM runtime-boundary architecture after a
source-backed review and landing pass across VM, JIT, extern, scheduler, and GC
boundary code. This is a development architecture document, not a user-facing
spec.

Current Rust source remains the implementation truth. Re-check the referenced
files before changing this boundary because VM, JIT, GC, FFI, WASM, Studio, and
app-runtime code are closely coupled here.

Related context:

- [`vm-production-readiness.md`](vm-production-readiness.md)
- [`vm-runtime-hardening-plan.md`](vm-runtime-hardening-plan.md)
- [`vm-runtime-boundary-repair-plan.md`](vm-runtime-boundary-repair-plan.md)
- [`extern-effect-jit-routing-design.md`](extern-effect-jit-routing-design.md)
- [`vm-module-verifier-hardening-plan.md`](vm-module-verifier-hardening-plan.md)
- [`jit-fact-source.md`](jit-fact-source.md)
- [`gc-correctness-testing-framework.md`](gc-correctness-testing-framework.md)
- [`lang/crates/vo-vm/src/vm/mod.rs`](../../crates/vo-vm/src/vm/mod.rs)
- [`lang/crates/vo-vm/src/fiber.rs`](../../crates/vo-vm/src/fiber.rs)
- [`lang/crates/vo-vm/src/scheduler.rs`](../../crates/vo-vm/src/scheduler.rs)
- [`lang/crates/vo-vm/src/vm/jit/`](../../crates/vo-vm/src/vm/jit/)
- [`lang/crates/vo-runtime/src/ffi/mod.rs`](../../crates/vo-runtime/src/ffi/mod.rs)
- [`lang/crates/vo-common-core/src/bytecode.rs`](../../crates/vo-common-core/src/bytecode.rs)

## Document Role

This file is the current runtime-boundary architecture and acceptance contract.
It also preserves historical problem and migration context so maintainers can
understand why the invariants exist.

Use the current-state sections when reviewing present source:

- `Implemented Target State`
- `Implementation Status`
- `Current Source Audit`
- `Definition Of Done`
- `Acceptance Matrix`

Use `Historical Problem Statement` and `Historical Migration Record` as
provenance. They should not be treated as open defects unless current source or
tests no longer satisfy the current-state sections above.

## Implemented Target State

The final VM has one runtime-boundary kernel. Every path that can park, wake,
replay, resume, call back into a closure, publish host work, or mutate
GC-visible boundary state must publish either a `RuntimeTransition` or a
`RuntimeCommand` that is converted by the applier-owned command bridge. The
final state is not "new names exist"; it is "the old side path for the same
semantic event is deleted or isolated behind the same validator".

The implemented shape is:

- execution producers emit `RuntimeTransition`;
- host, I/O, island, WASM, app-runtime, and Studio ingress emit
  `RuntimeCommand`;
- the command bridge validates external identity and converts commands into
  transition application work;
- the transition applier is the only code that mutates scheduler state,
  wait/runqueue state, current-fiber parked state, replay PC state, and
  cross-fiber wake state;
- `ResumePolicy` is the only representation of next-instruction, replay, or
  materialized-PC continuation;
- `ResolvedExternAbi` is the only execution-time extern ABI authority;
- `FrameCallBuilder` is the only closure/call frame construction authority;
- `FiberWakeKey` plus `WaitSource`, registration identity, and source-specific
  token data is the only cross-turn wake identity;
- `GcRootEffect` plus typed pending-root containers is the only way a boundary
  can make root-set dirtiness visible to GC;
- `vo-jit/src/semantics` remains the only opcode fact source.

The interpreter, full JIT, OSR exits, extern bridge, queue/select helpers, host
event ingress, I/O poll ingress, island commands, WASM bridge, app runtime, and
Studio host-event paths now consume these authorities through VM-facing
adapters. Provider lifecycle remains outside this boundary; native, WASM,
Studio, app-runtime, and host layers continue to own loading, registration, and
session setup while publishing resolved facts or runtime commands to the VM.

## Review Conclusion

The architecture is internally consistent after the adjustments below:

1. `RuntimeBoundary` must be carried inside a broader `RuntimeTransition`,
   because execution can wake other fibers, dirty GC roots, or enqueue host
   work while the current fiber continues.
2. `NotRegistered` does not survive as an ordinary resolved extern result.
   Missing or drifted providers on a resolved call belong to the resolved-call
   contract-error path and are fatal infrastructure errors. A resolved provider
   that still returns raw `ExternResult::NotRegistered` is classified as fatal
   provider contract drift, not a recoverable language panic. User-visible
   extern failures use explicit recoverable panic results.
3. Closure replay must preserve ordinary closure semantics. A replayed closure
   runs its own defer/recover machinery; only an unrecovered panic is reported
   back to the replayed extern.
4. The scheduler/fiber borrow model needs a real split between fiber storage
   and wait/runqueue state. A JIT or interpreter execution context may hold a
   `FiberLease`, but wait registration and wake application happen through a
   transition applier after that lease is released.
5. JIT opcode facts remain owned by `vo-jit/src/semantics`. This document owns
   VM runtime boundary protocol, not opcode capability or lowering facts.
6. The hot interpreter loop should not gain heap allocation, trait dispatch, or
   broad virtualized opcode handling. The unification point is the cold boundary
   between opcode execution and scheduler/runtime state.
7. Wait ownership must account for current queue/select reality: not every
   waiter physically lives in `Scheduler`. The transition applier owns the
   semantic registration, cancellation, wake, and replay policy, even when the
   storage adapter is a queue object or select state.
8. Host, I/O, and island command ingress paths are runtime boundaries too. They
   must either enter through the same transition applier or through a documented
   applier-owned command bridge before mutating wait/runqueue state.
9. `MaterializeAt` is an interpreter/scheduler resume contract. It does not
   imply that JIT may re-enter at arbitrary bytecode PCs; JIT entry policy
   remains owned by the current JIT dispatch and semantic-row contracts.
10. GC effects in transitions must map to the existing precise root and dirty
    epoch protocol. They are not a replacement for typed root exposure, and
    `GcRootEffect::None` is valid only when no root-owning boundary state has
    changed.
11. Resolved extern ABI shape is an implementation prerequisite, not a late
    cleanup. `ParamShape`, provider trust, exact return shape, slot kinds, and
    intrinsic eligibility must be frozen before shared extern classification is
    considered complete.
12. Pending transition payloads that can survive until a GC boundary must carry
    typed root metadata, including interface and composite-slot metadata when
    those payloads can hold GC references. Untyped `Vec<u64>` payloads are only
    allowed when they are consumed synchronously before any GC step can observe
    them.
13. Generation-bearing wake identity must become an explicit type, but it is
    not sufficient by itself. Wake application must also validate `WaitSource`,
    registration identity, source-specific token data, and select case identity
    where applicable. Packed endpoint response keys may be used during
    migration, but new APIs must not blur raw fiber slots and generation keys
    under a `fiber_id` field name.
14. Intrinsic eligibility is explicit provider trust, not provider source.
    `Stdlib`, `Builtin`, `RuntimeInternal`, native extension, or WASM host
    source labels must not imply intrinsic lowering authority.
15. Endpoint generation keys are a useful migration adapter, not a complete
    wait identity. Endpoint response application must still validate endpoint
    identity, response kind, current wait source, and registration identity.
    Select wake application must validate the active `select_id` and selected
    case identity before mutating `woken_index`.
16. The initial `GcRootEffect` vocabulary is intentionally coarse. Mutations to
    non-fiber typed pending-root owners must conservatively use
    `AllRootsDirty` until a more specific pending-root dirty domain exists.
17. Runtime-boundary protocol must not absorb provider lifecycle policy. Native
    extension loading, linkme collection, WASM host shims, WASM extension JS
    bridges, Studio dependency preparation, and app-runtime session setup remain
    owned by their current host layers. The VM boundary consumes resolved
    provider facts and runtime commands; it does not fetch, install, instantiate,
    or register providers.
18. The JIT transition decoder is not an interpreter fallback policy. Missing
    metadata, bad callback ABI state, invalid helper return policy, unsupported
    lowering ownership, bad resolved extern ABI, and provider drift remain fatal
    JIT infrastructure errors. Legal side exits and VM call materialization must
    be explicit transitions only.

With those constraints, no blocking design contradiction was found. The
implementation is treated as current only while the acceptance tests and source
ownership constraints in this document continue to pass.

## Implementation Status

The architecture is implemented through source-backed adapters rather than a
large hot-loop rewrite. Cold boundary code owns the new authorities:

- `ResolvedExternAbi` is resolved before execution and consumed by interpreter
  and JIT extern paths.
- `vm/extern_call.rs` classifies extern results for interpreter and JIT.
- `frame_call.rs` owns closure replay frame construction and validation.
- `scheduler.rs` owns `FiberWakeKey`, `WaitRegistration`, `WaitSource`, and
  `FiberLease`.
- `runtime_boundary.rs` owns `RuntimeTransition`, `RuntimeCommand`, wake
  commands, pending spawns, command bridging, and transition application.
- `vm/jit/bridge_result.rs` decodes JIT and OSR transport results into
  `RuntimeTransition` before mapping through compatibility result enums.
- `gc_roots.rs` applies `GcRootEffect` to the existing precise dirty-epoch
  protocol and scans typed pending payloads.

Where legacy result enums remain, they are compatibility transports behind the
same classifier, decoder, or applier. They are not separate active authorities
for extern lifecycle, wake identity, scheduler mutation, or GC dirty policy.

## Closed Implementation Gates

The source-backed review found no architectural contradiction. The
implementation closed the preconditions below in dependency order before
promoting the broad boundary authorities.

The gates remain here as maintenance constraints. Future changes must not
reintroduce a second active decision table beside these owners.

The broad `FiberLease` and transition-applier ownership refactor was introduced
after the ABI, extern-classification, closure-replay, wake-identity, and
GC-root gates were represented in tests and adapters.

The P0 blockers are now explicit in tests and isolated adapter code:

- a raw `ExternResult::NotRegistered` escaping from a resolved provider must be
  classified as fatal infrastructure drift in interpreter, full JIT, and OSR
  adapters, not as a recoverable language panic or transport-specific error;
- closure replay panic runs ordinary defer/recover collection before reaching
  the replay boundary;
- recovered replay panics must finalize through the replay return path, not by
  writing directly to the caller frame as an ordinary panic recovery;
- terminal extern suspend outcomes share replay-scope cleanup policy. `Yield`,
  `Block`, and non-replay host waits close the active extern replay scope in
  interpreter, full JIT, and OSR adapters before scheduler state is mutated;
- `JitExternSuspend::CallClosure` and extern replay arguments carry typed root
  ownership when they can survive until a GC boundary;
- `GcRootEffect` must preserve the current dirty epoch and bounded root-scan
  restart semantics. It is not enough to mark "roots changed" after the fact;
  root-bearing mutations must advance the existing dirty protocol before any
  bounded root scan can incorrectly report stability.
- stale wake and source-mismatch behavior is locked for queue/select, host,
  I/O, endpoint, and island wait records before those records reach the shared
  applier. A wake from a reused fiber slot, wrong wait source, wrong
  registration token, stale endpoint response identity, or stale select
  identity does not resume an unrelated blocked fiber.

Non-negotiable sequencing rules:

- do not wire a transition applier beside a legacy direct mutation path for
  the same wake or suspend source. The legacy path must either become an
  adapter that emits the same transition or be deleted in the same slice;
- do not introduce `FiberLease` as a cosmetic wrapper around the current raw
  `&mut Vm` plus `&mut Fiber` pattern. JIT callbacks remain adapter-only until
  they publish pending transitions instead of mutating scheduler state
  directly;
- do not promote endpoint generation into the general wake identity. It is
  only one field in a source-specific wait registration that also validates
  source and operation kind;
- do not let untyped extern or closure payload vectors survive across any
  boundary operation that may allocate or collect. Payloads must either remain
  synchronous locals or be represented as typed roots;
- do not change JIT transport numbers, OSR patching, or interpreted/JIT parity
  behavior before a cold decoder maps suspend and extern outcomes into
  explicit transitions.

1. ABI/schema gate: replace the `param_slots == 0` sentinel and expand the
   resolved extern table before relying on JIT route, intrinsic route, or
   extern result classification as a single source of truth.
2. Closure semantic gate: add tests for replayed closure defer/recover,
   canonical object validation, and multi-slot receiver layouts before moving
   call-frame construction.
3. Wake identity gate: introduce `FiberWakeKey` plus `WaitSource`,
   registration-token, and source-specific identity checks at host, I/O,
   queue/select, endpoint, and island ingress before claiming the transition
   applier owns wake semantics.
4. Borrowing gate: keep JIT callbacks as adapter code until they publish
   pending transitions instead of mutating scheduler state while holding a
   fiber borrow.
5. GC gate: every transition or pending command that can contain GC values must
   either expose typed roots, including interface and composite slot metadata,
   or be consumed before a boundary GC step.
6. Source-validation gate: each phase that replaces an old path must either
   delete that path or isolate it behind the same validator, then add a
   regression that would have failed on the old path.

## Current Source Audit

After implementation, the current codebase has these boundary facts:

- `Vm` keeps the top-level split between `Scheduler` and `VmState`, while
  runtime-boundary effects are published through `RuntimeTransition` or
  `RuntimeCommand`.
- Queue/select execution may still own queue-local object decisions, but
  cross-fiber wakes are emitted as wake commands and applied after the producer
  boundary.
- `ResolvedExternAbi` carries parameter shape, return shape, slot kinds,
  provider identity/source/trust, effects, ABI fingerprint, and optional JIT
  route. Interpreter and JIT extern execution consume that resolved view.
- `FunctionDef` and `ExternDef` now both publish typed call/return shape facts
  needed by verifier, GC, runtime dispatch, and JIT routing.
- Host event, I/O, queue/select, endpoint response, and island wake paths use
  generation-bearing `FiberWakeKey`/`WaitRegistration` data instead of raw
  cross-turn fiber slot identity.
- GC root scanning uses precise slot types, closure replay logs expose typed
  roots, JIT extern suspend payloads carry typed roots, and dirty effects flow
  through `GcRootEffect` into the existing dirty epoch protocol.
- JIT opcode semantics rows remain centralized in `vo-jit/src/semantics`; VM
  bridge transport values decode through cold transition adapters.
- Interpreter, JIT, and OSR extern suspend paths share resolved extern
  classification. Raw `ExternResult::NotRegistered` from a resolved provider is
  fatal infrastructure drift.
- Native, WASM, Studio, and app-runtime paths keep their provider lifecycle
  owners. They enter the VM boundary through resolved provider facts or runtime
  commands rather than moving loading policy into `vo-vm`.
- `FiberLease` isolates active fiber execution from scheduler wake/runqueue
  mutation; JIT callbacks publish pending transitions instead of directly
  mutating scheduler state.

## Historical Problem Statement

Before this implementation, the VM had several separate representations of the
same runtime facts:

- `ExternResult` for extern providers.
- `ExecResult` for interpreter scheduling boundaries.
- `BlockReason` for scheduler parking.
- `JitResult`, `JitExternSuspend`, `JitBridgeTransition`, and `OsrResult` for
  JIT and OSR exits.
- Ad hoc PC rewinds for replay-style queue, select, extern, and host-event
  paths.
- Ad hoc closure call frame construction in interpreter, extern replay, defer,
  and JIT callbacks.
- An extern ABI contract split across `ExternDef`, provider registration,
  resolved extern metadata, verifier checks, and JIT intrinsic routing.

That split let semantically identical events drift. Examples fixed by this
implementation include:

- extern closure replay accepted canonicalizable non-closure references and
  then read the original pointer as a closure header;
- terminal extern outcomes such as `Yield`, `Block`, and non-replay host waits
  did not consistently close closure replay scopes;
- `NotRegistered` behaved as a recoverable language panic in the interpreter
  but a fatal JIT infrastructure error through the JIT bridge;
- `closure::call_layout` claimed to be the closure ABI fact source, while
  interpreter extern replay rejected valid `arg_offset > 1` layouts;
- `ExternDef.param_slots == 0` doubled as both exact zero arguments and
  dynamic or unchecked argument shape;
- JIT intrinsic routing trusted a name whitelist without freezing exact ABI
  shape;
- PC replay/resume policy was encoded by local mutations in several unrelated
  modules.
- closure replay panic interception could run before the replayed closure's own
  defer/recover machinery;
- endpoint responses had generation keys, while host events, I/O waiters,
  queue/select waiters, and generic island wake commands still carried raw
  fiber slot identities;
- GC root dirtiness was inferred from lossy execution results or marked by
  local helper calls instead of being part of the runtime boundary.

The final architecture must remove that class of bug, not patch each match arm
individually.

## Target Invariants

1. Every execution boundary is represented as one `RuntimeTransition`.
2. Only the transition applier may mutate scheduler state, wait tables, ready
   queues, current-fiber blocked state, or scheduling outcomes.
3. Only `ResumePolicy` may decide whether execution continues at the next
   instruction, replays the current instruction, or materializes at an explicit
   PC.
4. Only the boundary finalizer may open or close closure replay scopes.
5. Extern shape, effects, provider identity, trust, and JIT route are frozen in
   one resolved ABI table before execution or JIT compilation.
6. Resolved extern calls cannot return `NotRegistered`. Missing, unregistered,
   drifted, or wrong-identity providers are fatal infrastructure errors.
7. Runtime provider `Panic` is the only extern-controlled recoverable panic
   path. Ordinary language panics and runtime traps keep their existing
   recoverable semantics when `recover` can observe them.
8. Closure calls use one frame builder for canonicalization, kind checking,
   slot0, argument offset, zeroing, return slots, and replay depth.
9. Cross-turn wake protocols use generation-bearing keys, never raw fiber slot
   ids. This includes host events, I/O waiters, queue/select waiters, endpoint
   responses, and island wake commands.
10. JIT bridge and OSR paths produce the same runtime transitions as the
    interpreter for equivalent bytecode.
11. VM runtime boundary protocol does not replace `vo-jit/src/semantics` as
    the opcode fact source.
12. Any boundary payload that can contain GC values and survive to a GC
    boundary exposes typed roots or is committed into an existing typed root
    container before the boundary returns.
13. No new per-instruction hot path depends on heap allocation or trait
    dispatch introduced by this architecture. Existing hot-path allocations are
    separate performance debt and must not be widened by the boundary adapter.

## Core Types

The names below are target names. They can move during implementation, but the
responsibilities should stay singular.

```rust
pub struct RuntimeTransition {
    pub boundary: RuntimeBoundary,
    pub resume: ResumePolicy,
    pub wakes: Vec<WakeCommand>,
    pub gc_roots: GcRootEffect,
    pub spawns: Vec<Fiber>,
}

pub enum RuntimeBoundary {
    Continue,
    Done,
    Yield,
    Block(BlockReason),
    Replay(ReplayRequest),
    CallClosure(ClosureReplayRequest),
    Panic(RuntimePanic),
    FatalInfra(String),
}

pub enum ResumePolicy {
    NextInstruction,
    ReplayCurrentInstruction,
    MaterializeAt { pc: u32 },
}

pub struct RuntimeCommand {
    pub source: WaitSource,
    pub target: RuntimeCommandTarget,
    pub wake_key: Option<FiberWakeKey>,
    pub registration: Option<WaitRegistrationKey>,
    pub source_token: SourceWakeToken,
    pub resume: ResumePolicy,
    pub payload: RuntimePayload,
    pub gc_roots: GcRootEffect,
}
```

`RuntimeBoundary` describes the current fiber. `RuntimeTransition` describes the
whole runtime step, including side effects that may target other fibers or host
state.

`RuntimeBoundary::Continue` inside a `RuntimeTransition` can still contain wake
commands and GC dirty effects. This is important for queue sends, queue
closes, select operations, and island endpoint work that wakes other fibers
while the current fiber keeps running or yields voluntarily.

`Continue` is not a scheduler quiescence marker. After the transition is
applied, the scheduling loop must keep the current fiber as the next runnable
fiber when it is still `Running`; otherwise interpreter-only side-effect
boundaries can drop the current turn while JIT continues, breaking parity.

JIT callbacks may publish local runtime side effects as pending transitions
while generated code continues to the next VM boundary. Each pending transition
declares a terminal policy in source:

- `CommitOnLanguagePanic` is for language-visible effects that JIT bytecode has
  already executed, such as `go` and `go island`; those effects commit before a
  later language panic is reported, but discard on JIT infrastructure terminal
  results.
- `CommitOnAnyTerminal` is for compensation after an object mutation has
  already committed, such as queue/select wakes after queue-local state changed;
  those effects commit even if the later result is panic, JIT error, or
  interruption.
- `DiscardOnTerminal` is reserved for effects that must not escape any terminal
  result.

Pending effects still commit with the next side-effect-carrying VM boundary.
Terminal handling is policy-based, not "merge everything" and not "discard
everything." This is a transactional boundary contract, not a promise that JIT
scheduling observes every interpreter opcode boundary.

Here "pending effects" means scheduler-visible VM boundary work that has not
yet been applied: wakes, spawned fibers, GC dirty effects, and host/runtime
commands. Queue-local readiness mutations are ordinary queue object state
changes; a bug exists only if such a committed queue-local mutation can make a
waiter depend on a pending wake that is later discarded without rollback or
another commit path.

`RuntimeCommand` is the external-ingress envelope. It is not a second scheduler
API. The command bridge validates it with the same identity, resume, and GC-root
rules as an in-fiber transition before any scheduler state is mutated.

## Runtime Boundary

### Boundary Production

Interpreter opcode handlers, JIT callbacks, OSR exits, extern bridges, queue
helpers, select helpers, and island helpers produce `RuntimeTransition`. They
do not directly:

- park the current fiber;
- push the current fiber back into the ready queue;
- mutate ready queues;
- decrement or increment scheduler blocked counts;
- set host-event waiters;
- set I/O waiters;
- wake arbitrary fibers;
- manually rewind the current PC except through a transition builder.

Host event wake APIs, I/O poll completion, island command processing, endpoint
responses, and remote wake commands are also boundary producers. They may run
outside a currently executing fiber, but they still produce transition-like
commands that are applied by the applier or by an applier-owned external
command bridge.

Initial ingress inventory to migrate or wrap includes:

- `Vm::wake_host_event` and token-based host-event wake helpers;
- scheduler I/O poll completion and resume-token delivery;
- `Vm::process_island_commands` and island thread command handlers;
- endpoint response registration and response delivery;
- queue/select wake helpers and close wake paths;
- generic `IslandCommand::WakeFiber` and any equivalent raw slot wake command;
- `vo-app-runtime` frame, mailbox, and event dispatch;
- bare `vo-web` async runner wake paths;
- WASM `runtime-wasm` extern bridge tags, including host event wait and replay;
- Studio wasm/TypeScript host-event scheduling and wake APIs.

### Applier-Owned Command Bridge

External ingress must not wake fibers, enqueue ready work, or write replay state
directly. It publishes a `RuntimeCommand` to the command bridge, and the bridge
either applies it through `apply_transition` or rejects it before scheduler
state is touched.

The bridge owns these command classes:

- host wake and host wake-with-data;
- I/O readiness and resume-token delivery;
- island wake, endpoint request, and endpoint response delivery;
- queue/select wake delivery when the physical waiter is stored outside
  `Scheduler`;
- app-runtime frame, mailbox, and event dispatch that can resume VM work;
- WASM and Studio host-event wake/replay ingress.

Outbound host or island work emitted by executing VM code must have a complete
producer, applier, and proof chain before it becomes a public transition field.
The current implementation does not keep inert `HostCommand` or generic
`WaitRegistration` transition surfaces; host waits are represented by
`RuntimeBoundary::Block(BlockReason::HostEvent*)`, and external wake ingress
comes back as a validated `RuntimeCommand` before touching scheduling state.
Outbound island spawn, endpoint request/response, endpoint close finalization,
and queue-handle transfer work is represented as `IslandCommandEffect` on
`RuntimeTransition`; low-level `VmState` send helpers are applier-owned
transport primitives, not VM opcode/JIT callback publication paths.

Every command that can resume or wake VM work carries:

- a `WaitSource`;
- a generation-bearing `FiberWakeKey` unless the command provably targets no
  fiber;
- a `WaitRegistrationKey` or source-specific equivalent;
- source-specific token data, such as I/O resume token, endpoint id, response
  kind, select id, and selected case id;
- a `ResumePolicy`;
- typed payload roots, or a proof that untyped payload words are consumed
  synchronously before any GC boundary;
- a `GcRootEffect` that matches any pending-root or resume-payload mutation.

Bridge validation is source-specific, not just generation-specific. A command
with the right fiber generation but the wrong wait source, endpoint id, response
kind, registration token, select id, selected case, or I/O token is stale and
must not resume the fiber. Generic commands named like `wake_fiber` are allowed
only as migration adapters after they carry the same identity envelope.

Opcode arms may still mutate the current frame, stack, globals, objects, and GC
through normal execution. The boundary rule applies to control transfer,
waiting, waking, replay, and scheduling.

Queue and select helpers may still mutate the queue object or select-local data
needed to decide readiness. They must not directly mutate run queues, blocked
counts, parked-fiber state, or PC replay once the transition layer exists.

### Boundary Application

The scheduler loop applies a transition in one place:

```rust
fn apply_transition(
    vm: &mut Vm,
    fiber_key: FiberWakeKey,
    transition: RuntimeTransition,
) -> Result<SchedulingControl, VmError>;
```

The applier is responsible for:

- applying `WakeCommand` entries;
- applying GC root dirtiness and deciding whether a boundary GC step is legal;
- registering waits using `WaitRegistration`;
- bridging queue/select waiter storage when the physical waiter lives inside a
  queue object rather than inside scheduler storage;
- validating wake key generation, source identity, and registration identity
  before waking a fiber;
- closing or preserving closure replay scopes;
- applying the current fiber state transition;
- translating recoverable panics into unwind machinery;
- translating fatal infrastructure errors into `VmError`;
- producing the next scheduler action.

The interpreter loop may remain flat and performance-sensitive. It should call
helpers that produce transitions rather than grow another local state machine.
The JIT transition adapter is also cold boundary code; generated code and
per-opcode interpreter arms should not pay for dynamic dispatch introduced by
this architecture.

## Resume Policy

The current source uses several local conventions: pre-increment `frame.pc`,
manual checked rewinds, direct subtraction, saturating subtraction, and JIT
materialization PCs. The target design replaces those conventions with
`ResumePolicy`.

Rules:

- `NextInstruction` means the current instruction completed. This policy closes
  any active closure replay scope for the current extern.
- `ReplayCurrentInstruction` means the instruction did not complete and must be
  re-executed after wake. This policy preserves the current replay scope.
- `MaterializeAt { pc }` means JIT or OSR must materialize VM frames at the
  exact bytecode PC before handing control to the interpreter or scheduler.
- Underflow is impossible by construction: the builder receives the fetched PC,
  not only the post-incremented PC, and can validate the requested policy
  before the transition is emitted.
- `HostEventWait` and queue waits that continue after wake use
  `NextInstruction`.
- `WaitIo`, `HostEventWaitAndReplay`, remote queue receive replay, and closure
  replay use `ReplayCurrentInstruction` or `MaterializeAt` as appropriate.

Target resume matrix:

| Boundary source | Resume policy | Closure replay scope | Notes |
| --- | --- | --- | --- |
| extern `Ok` | `NextInstruction` | close current extern scope | instruction completed |
| extern recoverable `Panic` | `NextInstruction` | close current extern scope | starts ordinary panic unwind |
| extern `Yield` | `NextInstruction` | close current extern scope | terminal suspend for this extern invocation |
| extern `Block` | `NextInstruction` | close current extern scope | queue-style terminal park |
| extern `HostEventWait` | `NextInstruction` | close current extern scope | host wakes after completed invocation |
| extern `WaitIo` | `ReplayCurrentInstruction` or `MaterializeAt` | preserve current extern scope | same extern consumes resume token |
| extern `HostEventWaitAndReplay` | `ReplayCurrentInstruction` or `MaterializeAt` | preserve current extern scope | same extern consumes host token/data |
| extern `CallClosure` | `ReplayCurrentInstruction` or `MaterializeAt` | preserve current extern scope | closure result is replay input |
| local queue send/recv that completes | `NextInstruction` | unchanged | may include wake side effects |
| local queue send/recv that parks | source-specific `NextInstruction` or `ReplayCurrentInstruction` | unchanged | encoded by the operation, not inferred from `Queue` |
| select wait | `ReplayCurrentInstruction` | unchanged | selected case state is wake payload |
| remote endpoint send ack success | `NextInstruction` | unchanged | remote send completed; response carries generation wake key |
| remote endpoint send closed response | `ReplayCurrentInstruction` | unchanged | instruction replays to observe closed-channel semantics |
| remote endpoint recv data/closed response | `ReplayCurrentInstruction` | unchanged | response carries generation wake key and decoded payload |
| JIT/OSR side exit | `MaterializeAt { pc }` | classification-dependent | materializes interpreter state at exact PC |

The table is normative for transition classification. Existing local PC
mutations such as checked rewind, direct subtraction, or saturating subtraction
must disappear behind transition builders.

## Wait Registration

```rust
pub struct WaitRegistration {
    pub source: WaitSource,
    pub resume: ResumePolicy,
    pub wake_key: FiberWakeKey,
    pub registration_key: WaitRegistrationKey,
    pub payload: WaitPayload,
}

pub enum WaitSource {
    Queue,
    Select,
    Io,
    HostEvent,
    HostEventReplay,
    IslandEndpoint,
    IslandWake,
}

pub struct FiberWakeKey {
    pub slot: u32,
    pub generation: u32,
}

pub struct WaitRegistrationKey {
    pub token: u64,
}
```

All waits carry a generation-bearing `FiberWakeKey`. Raw `FiberId` values may
still be used inside a single scheduler turn, but they must not be stored in
host, I/O, queue/select, island, or endpoint protocols that can outlive the
current turn.

Any field that stores a generation-bearing key is named as a key, not as
`fiber_id`. A raw slot id and a packed or structured wake key must not share
the same API shape.

`FiberWakeKey` proves only that the wake targets the same live fiber slot
generation. It is necessary but not sufficient. The applier must also verify
the current wait source, the registration token issued when the wait was
installed, and source-specific identity such as I/O resume tokens, host event
tokens, island endpoint response ids, `select_id`, and selected case identity.
A generation match alone must never be enough to apply a wake payload.

Endpoint response keys are generation-bearing, and endpoint response
application validates the endpoint id, registered send/recv operation kind,
current blocked operation, wait source, and response kind before it writes
`remote_send_closed`, `remote_recv_response`, or any other resume payload.
Likewise, a select wake compares the waiter `select_id` and case identity with
the active `SelectState` before writing `woken_index`; stale select waiters are
cancelled or ignored, not applied.

The transition applier owns wait semantics. It does not infer replay behavior
from the wait source. Replay behavior comes from `ResumePolicy`.

Physical storage may live in different places:

- scheduler-owned `WaitRegistry` for scheduler-native waits;
- host event and I/O maps for host-owned waiters;
- queue object waiter lists for queue send/receive waits;
- select state for multi-case selection;
- endpoint response registries for island calls;
- island command mailboxes for remote wake commands.

Each storage adapter must expose the same semantic contract:

- generation-key registration, registration-token pairing, and stale-key
  rejection;
- cancellation and cleanup when the fiber exits or the wait is superseded;
- duplicate wake behavior;
- source and registration identity validation, including `select_id` or
  selected-case identity when the wait came from `Select`;
- explicit source-mismatch behavior so a queue wake cannot resume a fiber that
  is currently blocked on host, I/O, endpoint, or island state;
- typed GC roots for any payload values;
- source-specific wake payload validation;
- the `ResumePolicy` that will be applied after wake;
- dirty-root marking for any wake payload or registration mutation that changes
  blocked-fiber resume state, endpoint state, or another GC-visible root owner.

The implemented wake scope includes host event waiters, I/O waiters, queue
waiter lists, select waiter state, endpoint responses, and
`IslandCommand::WakeFiber`. These protocols now use generation-bearing wake
identity with source-specific validation.

## Extern ABI

The serialized bytecode declaration and the runtime resolved ABI are separate
layers.

```rust
pub struct ExternDecl {
    pub name: String,
    pub params: ParamShape,
    pub returns: ReturnShape,
    pub allowed_effects: ExternEffects,
    pub param_kinds: Vec<ExtSlotKind>,
}

pub enum ParamShape {
    Exact { slots: u16 },
    CallSiteVariadic,
}

pub enum ProviderTrust {
    RuntimeInternal,
    IntrinsicEligible,
    Untrusted,
}

pub struct ReturnShape {
    pub slots: u16,
    pub kinds: Vec<ExtSlotKind>,
    pub slot_types: Vec<SlotType>,
}

pub struct ResolvedExternAbi {
    pub id: u32,
    pub name: String,
    pub params: ParamShape,
    pub returns: ReturnShape,
    pub param_kinds: Vec<ExtSlotKind>,
    pub allowed_effects: ExternEffects,
    pub provider_effects: ExternEffects,
    pub effective_effects: ExternEffects,
    pub source: RegisteredExternSource,
    pub provider_identity: u64,
    pub abi_fingerprint: u64,
    pub trust: ProviderTrust,
    pub jit_route: ExternJitRoute,
}
```

Rules:

- `param_slots == 0` must no longer mean "skip validation".
- Exact zero-argument externs are represented by `ParamShape::Exact { slots: 0
  }`.
- Dynamic or call-site-shaped externs use `ParamShape::CallSiteVariadic`.
- `CallExternLayout` remains per-instruction metadata for slot layout. It must
  agree with the extern declaration and call-site encoding.
- `ReturnShape` carries the exact return count, scalar slot layout, and GC
  slot types. A plain `ret_slots` count is only a compatibility adapter.
- `ExtSlotKind` is bridge/ABI encoding metadata. `SlotType` remains the GC
  scanning authority for return values, replay values, and any VM-owned pending
  payload.
- The resolved ABI table freezes provider identity and effect authority at load
  time.
- JIT compilation receives the resolved ABI table and cannot choose an extern
  route from raw module extern names alone.
- New bytecode, generated bytecode, native extension metadata, stdlib
  registration, linkme registration, manual registration, and WASM host
  registration must all express the same shape/trust/effect vocabulary.
- Compatibility for old `param_slots == 0` belongs only at deserialize/load
  boundaries. The execution engine must not preserve the ambiguous sentinel.
- The shared extern result classifier is only complete once it receives a
  fully resolved ABI. An id/name/effects-only resolved entry is an adapter
  state, not the target contract.

Resolved field ownership:

| Field | Source of truth | Required checks |
| --- | --- | --- |
| `id`, `name` | module extern declaration | table id matches declaration order and call instruction id |
| `params`, `returns`, `param_kinds` | serialized declaration plus call-site layout and codegen return metadata | verifier and loader reject contradictory call layouts and return layouts |
| `allowed_effects` | module declaration | provider result effects must be a subset |
| `provider_effects`, `source`, `provider_identity`, `abi_fingerprint`, `trust` | runtime registry/native extension/WASM host registration | provider identity and ABI fingerprint cannot drift after resolution |
| `effective_effects` | intersection of declaration and provider metadata | never grants effects absent from either side |
| `jit_route` | resolved ABI eligibility | JIT never recomputes route from raw name only |

Any `ExternContractError` produced by `call_resolved` or equivalent resolved
call path is `RuntimeBoundary::FatalInfra`. Legacy id-only or name-only extern
call helpers should be removed from production paths or kept behind explicit
test-only APIs that cannot be reached by normal bytecode execution.

Provider trust is intentionally narrower than provider source. A source such
as `Stdlib`, `Builtin`, `NativeExtension`, or `WasmHost` describes where the
provider came from; `ProviderTrust` describes what the VM/JIT may assume about
the provider. Intrinsic trust is opt-in metadata with an ABI fingerprint. It
must not be inferred from source alone.

`RuntimeInternal` does not imply intrinsic eligibility; `IntrinsicEligible` is
a separate capability for exact ABI-checked lowering.

### Intrinsic Eligibility

Intrinsic routing requires all of the following:

- name is in the supported intrinsic set;
- provider has `ProviderTrust::IntrinsicEligible`;
- provider effects are `ExternEffects::NONE`;
- parameter shape exactly matches the intrinsic signature and is never
  `CallSiteVariadic`;
- return slot shape exactly matches the intrinsic signature;
- call-site `CallExternLayout` slot kinds match the expected scalar layout;
- module declaration, resolved provider metadata, and per-call layout all agree
  on arity, return count, and scalar slot kinds.

Name alone is never sufficient.

Initial exact intrinsic table:

| Extern name | Args | Returns | Required layout |
| --- | --- | --- | --- |
| `math_Sqrt` | 1 | 1 | one scalar `f64` input, one scalar `f64` output |
| `math_Floor` | 1 | 1 | one scalar `f64` input, one scalar `f64` output |
| `math_Ceil` | 1 | 1 | one scalar `f64` input, one scalar `f64` output |
| `math_Trunc` | 1 | 1 | one scalar `f64` input, one scalar `f64` output |
| `math_FMA` | 3 | 1 | three scalar `f64` inputs, one scalar `f64` output |

These routes are for providers whose resolved ABI grants
`ProviderTrust::IntrinsicEligible` only. Public stdlib entry registration,
manual registration, native extension registration, runtime-internal
registration, and WASM host registration must not become intrinsic-eligible
unless their provider metadata explicitly grants `IntrinsicEligible` authority
and the ABI shape above is exact.

### Not Registered

`NotRegistered` should be removed from the resolved extern success path. A
resolved extern invocation can produce:

- normal return;
- recoverable extern panic;
- an allowed suspend or replay result;
- fatal infrastructure error.

If a provider is missing or has drifted after resolution, the call returns
`RuntimeBoundary::FatalInfra`. This is not recoverable user code behavior. If
an extern wants user code to recover, it must return an explicit recoverable
panic.

If a resolved provider still returns raw `ExternResult::NotRegistered`, that
is provider contract drift. The shared classifier must close the active extern
replay scope through the fatal-unwind path and return `FatalInfra`. The
interpreter, full JIT, and OSR adapters must not diverge on this case.

## Extern Result Classification

There must be one conversion from provider result to VM transition:

```rust
fn extern_result_to_transition(
    abi: &ResolvedExternAbi,
    result: ExternResult,
    fetched_pc: u32,
    replay_scope: &mut ClosureReplayState,
) -> RuntimeTransition;
```

Classification rules:

- `Ok` closes the current extern replay scope and continues.
- recoverable `Panic` closes the current extern replay scope and starts normal
  panic unwind.
- `Yield`, `Block`, and non-replay `HostEventWait` close the current extern
  replay scope because the current extern invocation has completed.
- `WaitIo`, `HostEventWaitAndReplay`, and closure replay preserve the scope
  because the same extern is re-entered later.
- closure replay validates the closure object before frame construction.
- result effects must be a subset of the resolved ABI effect authority.
- resume input tokens and closure replay cursors are verified according to the
  classification, not by a second independent match table.
- a raw `NotRegistered` escaping from any resolved provider is provider
  contract drift and becomes `FatalInfra` after closing the active extern scope
  for fatal unwinding.
- terminal suspend outcomes (`Yield`, `Block`, and non-replay host wait) must
  close replay scopes in interpreter, full JIT, and OSR paths before scheduler
  state is mutated.

Target classification table:

| Provider outcome | Boundary | Resume policy | Replay scope |
| --- | --- | --- | --- |
| `Ok` | `Continue` | `NextInstruction` | close |
| `Panic` | `Panic(RuntimePanic::Extern)` | `NextInstruction` | close |
| `Yield` | `Yield` | `NextInstruction` | close |
| `Block` | `Wait` or scheduler park boundary | `NextInstruction` | close |
| `WaitIo` | `Wait` | `ReplayCurrentInstruction` or `MaterializeAt` | preserve |
| `HostEventWait` | `Wait` | `NextInstruction` | close |
| `HostEventWaitAndReplay` | `Wait` | `ReplayCurrentInstruction` or `MaterializeAt` | preserve |
| `CallClosure` | `CallClosure` | `ReplayCurrentInstruction` or `MaterializeAt` | preserve |
| provider missing/drift/contract error | `FatalInfra` | none | close during fatal unwinding |

`NotRegistered` remains in the raw FFI enum for low-level transport, but it
must not be produced by a resolved provider invocation that normal bytecode can
reach.
If it escapes a provider anyway, the classifier treats it as provider contract
drift and returns `FatalInfra`, not a recoverable language panic.

Interpreter and JIT extern bridges both call this conversion or a shared helper
that carries exactly the same decision table.

## Closure Call Frame Builder

All closure-like frame construction goes through one VM-owned builder:

```rust
pub struct FrameCallBuilder<'a> {
    pub fiber: &'a mut Fiber,
    pub module: &'a Module,
    pub gc: &'a mut Gc,
}

impl FrameCallBuilder<'_> {
    pub fn call_static(&mut self, request: StaticCallRequest) -> RuntimeTransition;
    pub fn call_closure(&mut self, request: ClosureCallRequest) -> RuntimeTransition;
    pub fn call_extern_replay_closure(
        &mut self,
        request: ClosureReplayRequest,
    ) -> RuntimeTransition;
    pub fn call_defer(&mut self, request: DeferCallRequest) -> RuntimeTransition;
    pub fn materialize_jit_chain(
        &mut self,
        request: JitMaterializeRequest,
    ) -> RuntimeTransition;
}
```

Responsibilities:

- canonicalize all GC refs before object-header reads;
- use the canonical GC ref for every later header read, layout query, and root
  entry;
- if a request needs the original raw closure value for language-visible slot0
  identity, store it only after validation; header reads and function metadata
  lookup must still use the canonical object reference;
- reject non-closure objects before reading closure headers;
- use `closure::call_layout` as the single layout source;
- support `arg_offset == 0`, `arg_offset == 1`, and `arg_offset == recv_slots`;
- write slot0 exactly once;
- zero intermediate slots between slot0 and user args when needed;
- zero non-argument GC scan tail slots;
- copy user args;
- set replay depth only after successful frame construction;
- use fallible stack and frame capacity helpers;
- return recoverable stack overflow traps through the normal panic path;
- if a request carries argument values outside the stack, it must also carry
  slot types for any period in which those values can outlive the dynamic call
  that produced them.

The builder has two phases. Validation canonicalizes refs, checks object kind,
loads the closure function id from the canonical object, validates function
metadata, validates layout against `param_slots`, and validates argument and
return slot counts. Commit reserves the call window, writes slot0, copies user
args, zeros GC scan tails, pushes the frame, and records replay depth. No
partial frame may be visible if validation fails.

The target request model distinguishes synchronous call construction from
pending replay payloads:

- stack-backed ordinary closure/defer calls borrow typed slots from an existing
  frame and must be committed before the current execution step returns;
- extern replay calls that pass detached values use a typed replay argument
  buffer, or are consumed before any GC boundary can run;
- JIT prepared callback state is trusted only after the VM validates callback
  shape, object kind, canonical refs, and return-slot shape against the module
  metadata.

Error classification is request-specific but centralized:

- ordinary language nil closure calls are recoverable runtime traps;
- stack overflow and capacity failures are recoverable runtime traps when the
  language can unwind them;
- invalid, stale, or non-canonicalizable refs supplied by extern replay or JIT
  prepared callback state are fatal infrastructure errors;
- non-closure objects, missing function metadata, and ABI-inconsistent closure
  layouts are rejected before header-dependent reads and classified by the
  request source.

`exec/call.rs`, extern closure replay, defer calls, and JIT closure callbacks
may have different call request types, but they must not duplicate closure ABI
placement logic.

## Closure Replay And Panic Semantics

Final semantic decision:

> A closure invoked for extern replay behaves like an ordinary language closure
> call. Its own defers run, its own recover calls can observe and recover its
> panic, and its named returns are finalized before the extern receives replay
> values. Only an unrecovered panic is converted into a replay panic for the
> extern.

Consequences:

- replay return values are appended after defer completion, not before;
- heap/named returns use the same finalization path as ordinary returns;
- closure replay panic interception happens only after the closure's own
  unwind machinery has finished or failed to recover;
- replay panic message is typed as an extern replay input, not as a scheduler
  wait reason;
- nested extern replay scopes restore parent scopes after terminal results.

Implementation rule: the replay boundary must not catch a panic before the
closure frame has run ordinary unwind collection and defer execution. If a
defer recovers, the replay request completes with the closure's finalized
return values. Only after unwind reaches the replay closure boundary with no
recovery may the VM convert that panic into an extern replay panic input.

The implementation should split panic handling into two explicit moments:

1. unwind the replayed closure like an ordinary call, collecting and executing
   its own defers and allowing its own `recover` calls to observe the panic;
2. only if that unwind reaches the replay boundary unrecovered, convert the
   remaining panic state into a typed extern replay panic message and restore
   the parent extern replay scope.

Recovered replay panics must use the same replay return finalization as normal
closure returns. They must not write directly to the caller as if the replay
closure had been an ordinary caller-owned call.

Tests must lock recovered panic, unrecovered panic after defers, nested replay,
and defer-modified named return behavior before changing unwind behavior.

## Fiber Lease And Borrowing

The long-term scheduler shape should split fiber storage from scheduling state:

```rust
pub struct Scheduler {
    fibers: FiberStore,
    run_queue: RunQueueState,
    waits: WaitRegistry,
}

pub struct FiberLease<'a> {
    pub key: FiberWakeKey,
    fiber: &'a mut Fiber,
}

pub struct VmRuntimeParts<'a> {
    pub gc: &'a mut Gc,
    pub module: &'a Module,
    pub externs: &'a ExternRuntime,
    pub queues: &'a mut QueueRuntime,
    pub islands: &'a mut IslandRuntime,
    pub jit: &'a mut JitRuntime,
    pub host: &'a mut HostRuntime,
}
```

Rules:

- interpreter and JIT execution hold `VmRuntimeParts` plus `FiberLease`, not
  `&mut Vm` plus an independently borrowed `&mut Fiber`;
- `VmRuntimeParts` must not include mutable access to the fiber store while a
  `FiberLease` exists;
- wake requests are accumulated in `RuntimeTransition::wakes` and applied after
  the lease is released;
- host event and endpoint APIs expose only generation-bearing wake keys;
- stale wake keys are ignored or reported as infrastructure diagnostics,
  depending on source;
- no callback may keep a raw pointer to a fiber beyond the dynamic extent of a
  JIT or interpreter execution call;
- cross-turn APIs use `FiberWakeKey` or explicitly named generation-bearing
  keys, not ambiguous raw `FiberId` values.

JIT callbacks, extern callbacks, goroutine spawn helpers, queue/select helpers,
and island helpers follow the same rule: while a `FiberLease` exists they may
prepare VM-owned pending transition data, but they may not mutate scheduler
wait tables, run queues, blocked counts, or another fiber's parked state. The
transition applier consumes that pending data only after the lease is released.

Old callback signatures may remain as thin adapters only. They must write a
pending transition or boundary command and return immediately. They must not
perform direct queue insertion, wait registration, fiber state mutation, or
wake delivery while a `FiberLease` is active.

This is the architectural answer to unsafe aliasing risk in the JIT bridge.

## JIT And OSR Bridge

The C ABI still needs a compact `JitResult`, but it should not encode semantic
policy by itself. The JIT context should include a VM-owned boundary slot:

```rust
pub enum JitResult {
    Ok,
    Panic,
    Boundary,
    FatalInfra,
}

pub struct JitBoundarySlot {
    pub transition: PendingJitTransition,
}
```

If changing `JitResult` is too invasive, the implementation may keep existing
numeric variants temporarily, but they must decode into `RuntimeTransition`
through one adapter and must not be directly mapped to `ExecResult` or
`OsrResult`.

That adapter is the only place that may interpret JIT transport numbers. Other
VM code consumes `RuntimeTransition` and does not re-create the transport table
in local match arms.

Rules:

- `JitBridgeTransition` and `OsrResult` become adapters or disappear.
- full-function JIT and OSR use the same transition decoder.
- JIT extern suspend payloads are written into VM-owned state before returning
  from generated code.
- callback ABI declarations in `vo-runtime::jit_api`, context construction,
  and `vo-jit/src/semantics` runtime-dependency rows must be covered by one
  manifest-style consistency check. Adding a callback or changing its return
  policy, GC behavior, scheduling behavior, or frame-observation behavior must
  update that contract together with lowering.
- JIT callbacks may remain raw ABI shims only if they publish pending
  transitions into VM-owned state. Scheduler mutation, wait registration, and
  wake delivery happen after the active `FiberLease` is released.
- `JitExternSuspend::CallClosure` carries a typed replay argument buffer when
  it can survive beyond the immediate callback return. The GC scanner treats
  that payload as a root owner instead of relying on synchronous consumption by
  convention.
- a `PendingJitTransition` or boundary slot must not store detached
  `Vec<u64>` closure replay arguments across a scheduler-visible boundary
  unless the payload has been converted into a typed root owner first.
- any JIT boundary slot or suspend payload that stores GC-bearing values across
  a possible boundary must expose typed roots through VM-owned state.
- JIT materialization request carries exact `ResumePolicy`.
- `MaterializeAt { pc }` materializes VM frames at an exact bytecode PC for the
  interpreter or scheduler. It is not a policy to re-enter generated code at
  arbitrary bytecode PCs.
- side-exit statistics are derived from the transition, not from duplicated
  local match arms.
- strict JIT invalid metadata, wrong helper state, and ABI drift are fatal
  infrastructure errors.
- user panics remain recoverable runtime panics when the language semantics
  allow recover.
- the transition adapter must remain cold bridge code. It must not introduce a
  heap allocation, trait object dispatch, or broad semantic lookup into every
  generated opcode or every interpreter opcode arm.

## GC Integration

`RuntimeTransition` carries GC root effects so the scheduling loop does not
infer root dirtiness from a lossy `ExecResult`.

```rust
pub enum GcRootEffect {
    None,
    CurrentFiberDirty,
    AllRootsDirty,
}
```

This initial enum is deliberately conservative. `CurrentFiberDirty` is only for
mutations whose root ownership is confined to the current fiber. Any mutation
to globals, endpoint registries, host or I/O wait payloads, command queues, JIT
materialization records, or other non-fiber typed pending-root owners must use
`AllRootsDirty` until a dedicated pending-root dirty domain exists.

Rules:

- `GcRootEffect` is a command into the existing VM dirty-root protocol
  (`mark_gc_fiber_roots_dirty`, `mark_gc_all_roots_dirty`, `gc_dirty_epoch`,
  and bounded root-scan restart behavior), not a replacement root model;
- dirty effects are applied at the mutation boundary. If an applier mutates a
  blocked fiber resume payload, endpoint registry entry, replay log, wait
  payload, JIT materialization record, or any other root owner while a bounded
  root snapshot may exist, the existing dirty epoch must be advanced before
  another GC step can observe the old snapshot as stable;
- `GcRootEffect::None` is valid only when the transition does not change any
  root slot, wait payload, endpoint registry entry, replay log, panic/defer
  state, JIT materialization record, typed pending-root owner, or wake payload
  that can contain GC refs. When a bounded root snapshot may exist, `None`
  also means the dirty epoch remains unchanged by the transition;
- closure replay typed result logs remain VM-owned roots until the replay scope
  closes;
- wait registrations that hold GC values must expose typed roots through the
  GC root scanner;
- wait payloads, replay logs, defer arguments, endpoint response payloads, and
  JIT materialization records must expose typed roots. Keeping an otherwise
  stale replay scope alive only to preserve roots is not allowed;
- pending transitions must not be the only owner of untyped GC-bearing values.
  Either the values are moved into a typed VM root container before the
  transition is returned, or the transition is consumed before the scheduler can
  run a GC step. The current VM enforces the synchronous-consumption contract at
  GC entry: JIT pending runtime transitions must be attached or discarded before
  any VM GC step begins;
- untyped VM-owned pending payloads are forbidden across a boundary GC. If a
  payload can outlive the current stack turn and may contain `SlotType::GcRef`,
  `SlotType::Interface0`, `SlotType::Interface1`, composite typed slots, or
  another GC-visible value, it must have a typed root owner before the boundary
  is returned;
- wake payloads that contain deserialized GC refs mark relevant roots dirty;
- JIT materialization always validates frame scan extents before allowing a GC
  boundary;
- if roots change during a bounded root scan, the existing dirty epoch protocol
  must cause restart or rescan according to the current GC root scanner rules;
- `StableSinceLastScan` may be reported only under the same conditions as the
  current root scanner: no dirty fibers, no dirty global or pending-root state,
  and no boundary payload mutation since the last scan snapshot;
- no transition may leave a stale replay scope solely to keep roots alive.

## Module And ABI Compatibility

This design changed serialized and native-extension-visible contracts. The
implementation handled migration explicitly.

Implemented compatibility work:

- bumped bytecode serialization when replacing `param_slots` sentinel semantics;
- updated serialization and deserialization for `ParamShape`;
- kept bytecode compatibility policy explicit. The binary format uses lockstep
  version support, so accepting old bytecode requires deliberate reader
  support, not just a new field. New bytecode encodes exact zero as
  `ParamShape::Exact { slots: 0 }`;
- updated bytecode dump formatting for `ExternDecl`;
- updated `vo-codegen` extern registration APIs to declare exact or variadic
  shape explicitly;
- moved builtin, dynamic, wrapper, and declared-extern return metadata out of
  scattered tables and call sites into the same ABI source that resolution and
  verifier checks consume;
- updated `vo-common-core::verifier` to reject ambiguous or contradictory
  extern shapes;
- updated `vo-jit` strict verifier for intrinsic exact ABI requirements;
- bumped native extension ABI when provider shape, trust, identity, or effect
  metadata changes;
- updated `vo-ffi-macro`, `vo-ext`, stdlib registration, and WASM host
  registration together so every provider declares shape, trust, effects, and
  ABI fingerprint data in the same format;
- kept native dylib loading, linkme collection, WASM host shims, WASM extension
  JS bridge instantiation, Studio dependency preparation, and app-runtime
  session setup in their current host layers. They publish provider metadata
  into resolution; they do not become responsibilities of the VM transition
  applier;
- rejected old native extension libraries through the existing ABI version or
  fingerprint mismatch path;
- avoided hand-editing generated Playground docs; run declared docs generators
  only when source specs are intentionally changed.

Compatibility shims may exist at bytecode loading boundaries, but execution
uses only the resolved target representation.

## Implementation Shape

The target module layout is:

```text
vo-vm/src/runtime_boundary.rs
vo-vm/src/extern_abi.rs
vo-vm/src/frame_call.rs
vo-vm/src/wait.rs
vo-vm/src/fiber_lease.rs
vo-vm/src/vm/mod.rs
vo-vm/src/vm/jit/
```

Suggested ownership:

- `runtime_boundary.rs`: `RuntimeTransition`, `RuntimeBoundary`,
  `ResumePolicy`, transition builders, transition applier.
- `extern_abi.rs`: resolved ABI views and VM-facing extern invocation helpers.
- `frame_call.rs`: static, closure, defer, extern replay, and JIT
  materialization frame builders.
- `wait.rs`: wait registration, wake keys, stale wake policy, wait roots.
- `fiber_lease.rs`: scheduler/fiber borrowing boundary.
- `vm/mod.rs`: scheduling loop and opcode dispatch, with cold boundary logic
  delegated out.
- `vm/jit/`: JIT context construction and ABI adapters only; semantic policy
  funnels into runtime transitions.

This layout is a target. The first implementation may keep modules under
`vo-vm/src/vm/` while preserving these ownership boundaries.

## Historical Migration Record

This architecture landed in coherent vertical slices. The order remains useful
for maintenance because later authorities depend on earlier contract tests and
validators.

Implemented first slices before the larger phases:

1. Add contract tests that capture current source/design mismatches without
   changing ownership: extern classification parity, closure replay
   `defer`/`recover`, stale wake and source mismatch rejection, and dirty
   epoch or typed-root behavior.
2. Add ABI/schema fields and loader or registration checks while preserving
   existing runtime behavior through adapters.
3. Introduce a shared extern-result classifier and make interpreter/JIT consume
   it from their current call sites.
4. Land `FrameCallBuilder` and closure replay unwind fixes before moving
   call-frame construction behind the runtime boundary.
5. Add `WakeKey`/`WaitRegistration` validators at ingress points, then convert
   storage sites one source at a time.
6. Add `RuntimeTransition` and the applier after producers are ready to route
   through it; convert queue/select, host event, I/O, and island wake paths
   independently.
7. Introduce `FiberLease` only after direct scheduler mutation and raw JIT
   callback ownership have been narrowed to transition publication.

Each slice deleted or isolated the replaced path behind the same validator. A
future slice that leaves both an old direct mutation path and a new transition
path active for the same source is not complete.

### Phase 1: ABI Schema And Resolved Extern Foundation

Status: implemented.

- Replace the `param_slots == 0` sentinel with `ParamShape`.
- Add deserialize/load compatibility for old bytecode before changing the
  execution representation, or deliberately keep lockstep bytecode support and
  reject old ambiguous bytecode at load. Do not silently preserve the old
  sentinel inside execution state.
- Freeze complete `ResolvedExternAbi`, including parameter shape, return
  shape, slot kinds, provider source, provider identity, provider trust,
  effect authority, ABI fingerprint, and JIT route.
- Update verifier, serialization, bytecode dump formatting, codegen, runtime
  registration, stdlib registration, `vo-ffi-macro`, native extension ABI,
  linkme registration, manual registration, WASM host registration, and JIT
  routing together.
- Implement the exact intrinsic table and reject intrinsic routes whose ABI is
  not exact.

Implemented exit criteria:

- exact zero-arg and call-site-shaped externs are distinguishable after load;
- new execution paths no longer read ambiguous `param_slots == 0` semantics;
- codegen, wrappers, dynamic externs, and builtins all produce extern ABI facts
  through one ABI source instead of a separate hard-coded return-slot table plus
  per-call metadata agreement by convention;
- intrinsic eligibility requires exact ABI shape, `IntrinsicEligible` metadata,
  empty provider effects, and matching call-site slot layout;
- intrinsic rejection tests cover name-spoofed providers, wrong arity, wrong
  return shape, wrong scalar slot layout, wrong ABI fingerprint, and untrusted
  providers before any JIT route is trusted;
- provider identity, source, trust, effects, shape, and ABI fingerprint drift
  are fatal infrastructure errors;
- unresolved extern call APIs are test-only or removed from production paths.

### Phase 2: Boundary Types And Extern Classification

Status: implemented.

- Add `RuntimeTransition`, `RuntimeBoundary`, and `ResumePolicy`.
- Add one extern result classifier that receives `ResolvedExternAbi`.
- Route interpreter `CallExtern` and JIT extern bridge through it.
- Keep existing scheduler result types as adapters while the scheduler loop is
  migrated.
- Cover every `ExternResult` in interpreter and JIT tests.

Implemented exit criteria:

- no duplicated extern-result lifecycle table;
- `Yield`, `Block`, and non-replay host wait close replay scopes;
- replay-style extern results preserve scopes;
- `NotRegistered` resolved path is fatal infra or removed;
- a provider that leaks raw `ExternResult::NotRegistered` through a resolved
  call is fatal infra in interpreter, full JIT, and OSR parity tests;
- provider drift and `ExternContractError` are fatal infra in interpreter, full
  JIT, and OSR bridges;
- `ResumePolicy` builders receive the fetched PC and cannot underflow.

### Phase 3: Closure Frame Builder And Replay Unwind

Status: implemented.

- Add `FrameCallBuilder`.
- Move extern closure replay to it first.
- Move ordinary `CallClosure` and defer closure calls next.
- Move JIT prepared closure callback last.
- Rework closure replay panic interception so closure defers/recover run before
  the replay boundary converts unrecovered panic into extern replay input.

Implemented exit criteria:

- non-closure GC refs are rejected before header reads;
- canonical base refs are used for closure headers;
- `arg_offset > 1` works everywhere `closure::call_layout` permits it;
- detached replay arguments are typed roots or consumed before any GC boundary;
- stack overflow and capacity failures route through VM traps;
- recovered replay panic returns finalized values to the extern replay caller;
- unrecovered replay panic is reported only after closure defers run;
- recover from a replayed closure panic never writes directly to the ordinary
  caller return destination before replay finalization.

### Phase 4: Wake Keys And Wait Registration

Status: implemented.

- Introduce explicit `FiberWakeKey { slot, generation }`.
- Wrap or replace packed endpoint response keys with a named generation-bearing
  type.
- Convert host event waiters, I/O waiters, endpoint responses, queue/select
  waiters, and island wake commands to generation keys.
- Add storage adapters for scheduler wait registry, queue waiter lists, select
  state, host/I/O maps, endpoint responses, and island command mailboxes.
- Move PC replay policy out of scheduler wait code.

Implemented exit criteria:

- no cross-turn wait table stores a raw fiber slot id;
- stale wake tests exist for host events, I/O where applicable, queue/select
  waits, endpoints, and island wake commands;
- source-mismatch tests prove a wake from one wait source cannot resume a fiber
  currently parked on another source;
- select wake tests cover stale or mismatched selected-case identity;
- endpoint response tests cover stale generation, wrong `endpoint_id`, wrong
  response kind for the registered operation, and source mismatch;
- duplicate wake and cancelled wait behavior is explicit for every storage
  adapter;
- scheduler does not hand-edit replay PC;
- queue/select helpers may own local object state but cannot mutate run queues,
  blocked counts, or replay PCs directly.

### Phase 5: Fiber Lease And Transition Applier Ownership

Status: implemented.

- Split fiber storage from runqueue/wait state enough to issue `FiberLease`.
- Introduce the cold transition applier as the only owner of scheduler state,
  wait registration, wake application, blocked counts, and scheduling
  outcomes.
- Convert host event, I/O, queue/select, endpoint, island, goroutine, extern,
  and JIT callback paths to publish transition commands.
- Apply wake commands after `FiberLease` release.

Implemented exit criteria:

- interpreter and JIT execution no longer receive `&mut Vm` plus an
  independently borrowed `&mut Fiber`;
- JIT callbacks cannot mutate scheduler wait/runqueue state while holding a
  fiber borrow;
- legacy callbacks that remain are adapter-only and publish pending
  transitions rather than applying scheduler effects;
- host, I/O, and island ingress route through the applier or an applier-owned
  command bridge;
- pending command payloads that can contain GC refs are typed roots.

### Phase 6: JIT And OSR Transition Unification

Status: implemented.

- Decode all JIT and OSR exits into `RuntimeTransition`.
- Remove or reduce `JitBridgeTransition` and `OsrResult` to adapters.
- Derive side-exit statistics from transitions.
- Ensure `MaterializeAt` remains an interpreter/scheduler materialization
  contract, not arbitrary generated-code re-entry.
- Keep the decoder cold and closed over known transport values. It must not
  introduce a generic "fall back to interpreter" branch for strict JIT
  infrastructure failures.

Implemented exit criteria:

- interpreter, full JIT, and OSR parity tests pass for every boundary kind;
- full-function JIT and OSR use the same transition decoder;
- existing JIT transport numbers are interpreted only by the cold decoder;
- `MaterializeAt` tests prove exact interpreter materialization without
  implying arbitrary JIT re-entry;
- strict JIT failure policy remains fail-fast for invalid metadata, wrong
  helper state, invalid callback ABI state, unsupported lowering ownership,
  helper return-policy drift, and ABI drift;
- side-exit accounting distinguishes legal materialization/wait/replay
  transitions from fatal infrastructure errors.

### Phase 7: GC Root Effects And Typed Payloads

Status: implemented.

- Replace lossy `ExecResult`-based dirty-root inference with
  `RuntimeTransition::gc_roots`.
- Map `GcRootEffect` to the existing `mark_gc_fiber_roots_dirty`,
  `mark_gc_all_roots_dirty`, and `gc_dirty_epoch` protocol.
- Expose typed roots for replay logs, wait payloads, defer arguments, endpoint
  payloads, JIT materialization records, and pending transition payloads,
  including interface and composite-slot pending payloads.
- Audit host/I/O/island wake paths that can wake fibers outside the normal
  scheduling boundary.

Implemented exit criteria:

- GC tests prove wake payloads and replay logs stay rooted without stale replay
  scopes;
- bounded root scans restart or rescan when the dirty epoch changes;
- `GcRootEffect::None` is covered by tests for root-stable transitions;
- active `StartCycle`, `Atomic`, and `Sweep` boundary steps do not accept
  `GcRootEffect::None` for transitions that mutate typed pending roots;
- non-fiber typed pending-root mutations are covered as `AllRootsDirty` unless
  a narrower pending-root dirty category has been implemented and tested;
- JIT materialized frames validate scan extents before any GC boundary;
- host/I/O/island wake paths mark dirty roots whenever they mutate blocked
  fiber resume payloads or endpoint state.

## Definition Of Done

This implementation is complete while these conditions remain true in source
and locked by tests:

- there is one runtime-boundary classifier for interpreter, full JIT, OSR, and
  extern bridge outcomes;
- resolved extern execution has no ambiguous `param_slots == 0` shape and no
  raw `NotRegistered` success path;
- closure replay runs ordinary defer/recover semantics and returns through the
  replay finalizer instead of direct caller-frame writes;
- every wait or wake path validates `FiberWakeKey`, `WaitSource`, registration
  identity, and source-specific token identity before resuming work;
- no cross-turn command uses a raw fiber slot id as wake authority;
- no producer mutates scheduler ready queues, blocked counts, parked-fiber
  state, wait registration state, wake state, or replay PC state outside the
  transition applier or command bridge;
- no replaced source keeps both a legacy direct mutation path and a transition
  application path for the same semantic event;
- all boundary payloads that can survive to a GC boundary expose typed roots,
  including interface/composite-slot metadata where applicable;
- every root-bearing transition or command marks the existing dirty epoch
  protocol precisely enough for bounded root scans to restart or rescan;
- JIT transport numbers and OSR exits decode through cold adapters into the same
  `RuntimeTransition` shape as the interpreter;
- host, I/O, island, WASM, app-runtime, and Studio ingress paths publish
  `RuntimeCommand` and pass the same bridge validators before touching VM
  scheduling state;
- provider lifecycle remains host-owned: native extension loading, linkme
  collection, WASM host shims, WASM extension JS bridge instantiation, Studio
  dependency preparation, and app-runtime session setup are not routed through
  the transition applier;
- JIT callback ABI facts are checked against semantic runtime-dependency rows,
  including helper return policy, GC/scheduling behavior, and frame-observation
  requirements;
- opcode capability and lowering facts remain owned by `vo-jit/src/semantics`.

## Acceptance Matrix

The acceptance matrix for the completed implementation is:

| Area | Required coverage |
| --- | --- |
| Extern result parity | `Ok`, recoverable panic, provider drift, `ExternContractError`, leaked raw `NotRegistered`, `Yield`, `Block`, `WaitIo`, `HostEventWait`, `HostEventWaitAndReplay`, `CallClosure` across interpreter, full JIT, and OSR where applicable |
| PC policy | `NextInstruction`, `ReplayCurrentInstruction`, and `MaterializeAt` cannot underflow, do not rely on direct decrement or `saturating_sub`, and resume at the expected PC |
| Closure replay | multi-step closure replay, GC-ref replay values, nested extern replay, terminal cleanup for `Ok`/`Panic`/`Yield`/`Block`/non-replay host wait, unrecovered panic after defers, recovered panic, defer-modified named returns, recovered replay panic return finalization, no direct caller write on replay recover |
| Closure object validation | nil closure, non-closure GC ref, interior pointer to closure, interior pointer to non-closure, stale/freed ref |
| Extern ABI | exact zero args, variadic/call-site args, wrong arg count, wrong return shape, wrong `param_kinds`, wrong `CallExternLayout`, dynamic `dyn_call` variable returns, large struct/interface returns through dynamic externs, provider identity drift, provider trust drift, stdlib/native/WASM provider shape drift, old bytecode `param_slots == 0` compatibility mapping or explicit rejection, old native extension ABI rejection |
| Intrinsics | accepted exact `math_*` ABI, rejected wrong arity, rejected wrong return shape, rejected wrong scalar layout, rejected `CallSiteVariadic`, rejected untrusted provider, rejected name-spoofed provider, rejected `IntrinsicEligible` provider with wrong ABI fingerprint |
| Waits and wakes | host event stale key, I/O stale key, queue/select stale key, endpoint stale key, endpoint wrong `endpoint_id`, endpoint wrong response kind, island wake stale key, duplicate wake, cancelled wait, close wake, source mismatch, wrong registration token, stale select identity, stale selected case identity, reused raw fiber slot, generic wake without generation, I/O resume token consumption, endpoint send ack success versus send closed replay policy |
| GC | closure replay log roots, typed pending transition roots, interface/composite typed pending roots, wait payload roots, endpoint payload roots, defer argument roots, JIT materialized frame roots, JIT extern suspend args consumed before GC or rooted, forced-GC rejection for untyped `Vec<u64>` across scheduler-visible boundaries, root dirtiness after wake payloads, non-fiber pending-root mutation uses `AllRootsDirty` unless a narrower dirty domain exists, `GcRootEffect::None` root-stable transitions, no `GcRootEffect::None` dirty-epoch mutation during bounded root scan, no `GcRootEffect::None` for typed pending-root mutation during active `StartCycle`/`Atomic`/`Sweep` steps, dirty epoch restart during bounded root scan, `StableSinceLastScan` withheld when any boundary payload mutates roots |
| Borrowing | JIT callback tests that would previously need `&mut Vm + &mut Fiber` aliasing now pass through `FiberLease` and transition application; legacy callbacks are adapter-only; queue/select/goroutine/island callbacks publish pending transitions |
| JIT callback ABI | every semantic runtime dependency has a matching runtime callback/helper declaration, expected return policy, GC behavior, scheduling behavior, and frame-observation flag; callback drift is fatal infra, not a side exit |
| Source path replacement | each replaced wait, wake, extern suspend, or callback source is deleted or isolated behind the same validator; no source has both legacy direct mutation and transition application active |
| JIT/OSR bridge | `MaterializeAt` materializes exact interpreter state, side-exit statistics derive from transitions, fatal JIT metadata and ABI drift do not silently fall back, fatal infra is not counted as a legal side exit, and JIT transport numbers decode only through the cold adapter |
| Web, WASM, Studio, app runtime | WASM fetch replay, timer wake, extension bridge suspend/replay, host output, display pulse or render wake, Studio `prepareEntry` dependency preparation, extension state save/restore during nested VM runs, app-runtime event/mailbox dispatch, and `wakeHostEvent` ingress all pass through the same wake/source/registration validators before scheduler mutation |
| Provider lifecycle boundaries | native dylib loading, linkme registration, no_std stdlib split, WASM host shims, WASM extension JS bridge instantiation, Studio dependency preparation, and app-runtime session setup keep their current owners while publishing shape/trust/effect/fingerprint metadata into resolution |

Focused commands should follow the Volang verification guide. Expected gates
include:

```sh
cargo test -p vo-vm --features jit
cargo test -p vo-jit
cargo test -p vo-runtime ffi
./d.py test both tests/lang/cases/<new-case>.vo
./d.py test jit tests/lang/cases/<new-case>.vo
./d.py test osr tests/lang/cases/<new-case>.vo
./d.py test gc
```

Use broader `vo-dev` task plans when the implementation touches bytecode
serialization, native extension ABI, WASM, generated docs, or CI policy.

## Non-Goals

- Do not move opcode capability, lowering owner, runtime dependency, or strict
  JIT metadata policy out of `vo-jit/src/semantics`.
- Do not turn strict JIT infrastructure errors into silent interpreter paths.
- Do not add heap allocation or trait dispatch to every opcode execution.
- Do not treat `MaterializeAt` as permission for arbitrary generated-code
  re-entry at any bytecode PC.
- Do not require all physical wait storage to move into `Scheduler`; centralize
  wait semantics and adapters, not necessarily every container.
- Do not treat this document as a replacement for bytecode or language specs.
- Do not hand-edit generated Playground documentation as part of this runtime
  architecture work.
- Do not preserve ambiguous extern ABI semantics for compatibility inside the
  execution engine. Compatibility belongs at load/deserialize boundaries.
- Do not move host-specific provider lifecycle into `vo-vm` or the transition
  applier. The boundary consumes resolved facts and commands; host layers still
  own loading, preparation, and instantiation.

## Final Shape

The final VM should have one answer to each boundary question:

- What happened? `RuntimeTransition`.
- Where should execution resume? `ResumePolicy`.
- How does external work enter the same boundary kernel? `RuntimeCommand`
  through the applier-owned command bridge.
- Who can wake this fiber? `FiberWakeKey` plus wait-source and registration
  identity.
- What extern ABI is valid? `ResolvedExternAbi`.
- How is a call frame built? `FrameCallBuilder`.
- Which root set changed? `GcRootEffect`.
- Where are GC-bearing boundary values rooted? typed VM root containers.
- Who may mutate scheduler state? the transition applier.
- Who owns opcode JIT facts? `vo-jit/src/semantics`.

When these answers are singular, the current class of bugs becomes much harder
to reintroduce. New runtime behavior must add one transition variant or one ABI
fact, then update the acceptance matrix, instead of adding another local match
table.
