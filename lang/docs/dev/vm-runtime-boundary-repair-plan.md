# VM Runtime Boundary Repair Record

Status, 2026-06-12: completed follow-up repair record for the
runtime-boundary implementation reviewed against
`vm-runtime-boundary-architecture.md`. The original filename is retained for
links, but this file is now a migration record and maintenance checklist, not
an open defect list or user-facing spec.

Current Rust source remains the implementation truth. Re-check the referenced
files before starting a slice because VM, JIT, GC, WASM, Studio, and
app-runtime code are tightly coupled here.

## Document Role

Read this file as layered maintenance context:

1. `Implementation status` records what landed in the current repair slice.
2. `Original Repair Context` records the bug classes that motivated the
   migration. It is historical provenance unless current source or tests have
   regressed.
3. The phase records preserve the landing recipe and the regression shape that
   locked each invariant. After a phase is marked landed, its required changes
   are maintenance constraints, not open work.
4. `Completion Definition` is the current invariant checklist. A new finding
   should be opened only when current source or tests violate that checklist,
   the architecture document, or the implementation status above.

## Review Guardrail

When reviewing current source, first classify an observation as one of:

- current invariant violation;
- completed phase regression;
- intentional completed-phase behavior;
- historical context only.

Do not open a finding from `Original Repair Context` or a phase
`Required Changes` section unless current source violates `Implementation
status`, `Completion Definition`, or the architecture acceptance matrix.

Intentional completed-phase behaviors include:

| Behavior | Current contract |
| --- | --- |
| rejected or stale endpoint response | must not decrement `pending_island_responses` |
| terminal handling of pending JIT transitions | follows the source-declared pending terminal policy: language-visible `go`/island effects commit before language panic, queue/select compensation commits on any terminal result, and pure uncommitted effects discard on JIT infrastructure terminal results |
| remaining compatibility result enums | allowed when isolated behind classifier, decoder, or applier adapters |

Implementation status, 2026-06-12:

- Phase 1 landed by moving pending spawned fibers into `RuntimeTransition`.
  JIT-local pending side effects now commit with the next VM boundary that can
  carry side effects. Terminal handling is policy-declared in source:
  language-visible `go`/island effects commit before language panic,
  queue/select compensation commits on any terminal result, and pure
  uncommitted effects discard on JIT infrastructure terminal results.
- Phase 2 landed with typed `HostWaitKey`/`HostWaitSource` wake identity across
  VM, web, app-runtime, Studio WASM, and legacy playground ingress.
- Phase 3 landed by aligning dynamic method lookup with the interface wrapper
  ABI for one-slot wrapper receivers, while retaining explicit rejection for
  unsupported direct method closure layouts.
- Phase 4 landed by validating endpoint responses before decrementing
  `pending_island_responses`.
- Phase 5 landed for the concrete remaining ABI split: interpreter and JIT
  interface-call preparation now share the same target resolver before any JIT
  callee frame is written. Closure-call layout validation is centralized
  through `closure::call_layout`.

Focused validation run for this landing slice included:

- `cargo test -p vo-vm pending_spawn`
- `cargo test -p vo-vm --features jit pending_spawn`
- `cargo test -p vo-vm endpoint_response`
- `cargo test -p vo-vm host_event`
- `cargo test -p vo-app-runtime mailbox`
- `cargo check -p vo-web --target wasm32-unknown-unknown`
- `cargo check --manifest-path apps/studio/wasm/Cargo.toml --target wasm32-unknown-unknown`
- `node ./node_modules/typescript/bin/tsc -p tsconfig.json` from `apps/studio`
  using the bundled Codex runtime Node
- `cargo test -p vo-runtime dynamic`
- `cargo test -p vo-vm closure`
- `cargo test -p vo-vm --features jit closure_call`
- `cargo test -p vo-vm --features jit
  prepare_iface_call_rejects_recv_slot_drift_before_push_frame`
- `./d.py test vm tests/lang/cases/dyn/dyn_method_value_receiver.vo`
- `./d.py test jit tests/lang/cases/dyn/dyn_method_value_receiver.vo`
- `./d.py test both
  tests/lang/cases/dyn/dyn_method_value_receiver_multi_slot.vo`

Related context:

- [`vm-production-readiness.md`](vm-production-readiness.md)
- [`vm-runtime-boundary-architecture.md`](vm-runtime-boundary-architecture.md)
- [`vm-runtime-hardening-plan.md`](vm-runtime-hardening-plan.md)
- [`extern-effect-jit-routing-design.md`](extern-effect-jit-routing-design.md)
- [`gc-correctness-testing-framework.md`](gc-correctness-testing-framework.md)
- [`jit-fact-source.md`](jit-fact-source.md)
- [`lang/crates/vo-vm/src/runtime_boundary.rs`](../../crates/vo-vm/src/runtime_boundary.rs)
- [`lang/crates/vo-vm/src/scheduler.rs`](../../crates/vo-vm/src/scheduler.rs)
- [`lang/crates/vo-vm/src/gc_roots.rs`](../../crates/vo-vm/src/gc_roots.rs)
- [`lang/crates/vo-vm/src/frame_call.rs`](../../crates/vo-vm/src/frame_call.rs)
- [`lang/crates/vo-runtime/src/builtins/dynamic.rs`](../../crates/vo-runtime/src/builtins/dynamic.rs)
- [`lang/crates/vo-web/src/async_runner.rs`](../../crates/vo-web/src/async_runner.rs)
- [`apps/studio/wasm/src/lib.rs`](../../../apps/studio/wasm/src/lib.rs)
- [`apps/studio/src/lib/backend/web_backend.ts`](../../../apps/studio/src/lib/backend/web_backend.ts)

## Original Repair Context

At the start of this repair, the runtime-boundary architecture had introduced
the right owner names, but several legacy protocols still carried incomplete
identity, incomplete side effects, or implicit ABI facts. These were not
separate design contradictions. They were places where old side paths had been
wrapped by the new boundary layer without being fully converted to the new
invariants.

The source reviewed for this repair had these classes of work:

1. Host-event wake identity is token-only at the final scheduler lookup, while
   token producers live in separate namespaces.
2. JIT pending runtime effects are split between a transition queue and a
   separate spawned-fiber buffer, so discard, merge, apply, and GC scanning are
   not transactional.
3. Runtime-created dynamic method closures encode receiver layout differently
   from codegen-created method closures and interface wrappers.
4. Endpoint response accounting decrements in-flight response count before the
   response is proven to apply to the waiting fiber.
5. JIT and interpreter call preparation still contain separate ABI hardening
   checks for some dynamic call shapes.

The repair goal was not a sequence of local patches. The goal was to delete the
remaining incomplete boundary representations.

## Boundary Maintenance Principles

These principles remain current for future VM boundary work. They explain how
to keep the landed architecture coherent; they do not imply that the original
problem classes above remain active defects.

1. A wake command must carry complete identity.

   Token-only wake is not a boundary identity. Host, I/O, queue, select,
   endpoint, and island wake commands must include source, generation-bearing
   fiber key when applicable, registration identity, and source-specific token
   data.

2. A pending runtime side effect must have one owner.

   If a JIT callback creates a transition and a spawned fiber, both must live in
   the same pending object. Merge, apply, discard, rollback, and root scanning
   must operate on that object as a unit.

3. Runtime-created values must have explicit ABI metadata.

   Dynamic/reflection paths cannot depend on codegen-only layout assumptions.
   If a runtime-created closure captures receiver data, the closure must either
   use the same wrapper ABI as codegen or carry enough typed layout metadata for
   frame construction and GC.

4. Accounting must follow accepted state changes.

   Counters such as `pending_island_responses` describe live VM obligations.
   They must change only after a command passes identity and operation-kind
   validation.

5. Shared boundary validators beat mirrored checks.

   If interpreter and JIT both prepare the same call shape, both should call
   the same cold validator. Do not maintain two defensive match tables.

## Phase 0 Record: Lock Regression Tests First

Before changing the implementation, add focused tests that fail on the current
source. These tests are the guard rails for the refactor slices below.

### Host Event Identity Tests

Add scheduler and WASM/web-facing tests for token collisions:

- a `HostEvent` timer waiter with token `1` and a `HostEventReplay` fetch waiter
  with token `1` must not wake each other;
- `wake_host_event` for a timer source must reject a replay waiter even if the
  raw token matches;
- `wake_host_event_with_data` must reject a timer waiter and must not remove it
  from the waiter table;
- `PendingHostEvent` dedup in app runtime and Studio must distinguish at least
  `(source, token)`, not only `token`.

Original risk checkpoints:

- `runtime-wasm/src/time.rs` has a local sleep token counter.
- `runtime-wasm/src/net_http.rs` has a local fetch token counter.
- `vo-runtime/src/ffi/mod.rs` has a generic host-event token counter.
- `scheduler.rs::wake_host_event` currently finds by token only.
- `vo-web/src/async_runner.rs` wakes fetches and timers through token-only VM
  APIs.

### Pending Spawn Transaction Tests

Add JIT-focused unit or integration tests that force these paths:

- a JIT helper enqueues a local `go` spawn and later returns `Panic` in the same
  JIT invocation;
- a JIT helper enqueues a local `go` spawn and later returns `CallClosure` or
  another boundary that discards pending transitions;
- after discard, no spawned fiber remains in any pending buffer;
- if a pending spawn is retained until a GC boundary, the spawned fiber's stack
  and closure references are scanned as roots.

Original risk checkpoints:

- `vm/jit/callbacks/goroutine.rs::push_pending_spawn` writes both
  `pending_spawned_fibers` and `pending_runtime_transitions`.
- `runtime_boundary.rs::attach_pending_runtime_transitions` clears only
  pending transitions for terminal discard cases.
- `gc_roots.rs` scans scheduler fibers, not `pending_spawned_fibers`.

### Dynamic Method Closure Tests

Add language-level and VM-level tests for dynamic method values:

- a dynamic lookup of a value-receiver method on a multi-slot struct must call
  the method with all receiver slots initialized;
- a dynamic method closure whose receiver contains GC references must keep those
  references alive across a forced GC;
- the same test should pass through direct `CallClosure`, `go`, island/local
  spawn, and web callback entry points if they all use the same closure frame
  builder.

Original risk checkpoints:

- `vo-runtime/src/builtins/dynamic.rs::get_method` captures only
  `receiver_slot1`.
- `vo-runtime/src/objects/closure.rs::call_layout` writes only capture slot 0
  for method closures and skips to `recv_slots` for user args.
- `vo-vm/src/frame_call.rs` and `vo-vm/src/vm/helpers.rs` follow that layout.
- `gc_roots.rs` only infers runtime method capture layout when
  `recv_slots == 1`.

### Endpoint Accounting Tests

Add tests that distinguish rejected and accepted endpoint responses:

- wrong fiber generation does not decrement `pending_island_responses`;
- wrong wait source does not decrement;
- wrong endpoint id or wrong response kind does not decrement;
- successful `SendAck` or `RecvData` decrements exactly once;
- duplicate accepted response does not underflow or decrement twice.

Original risk checkpoint:

- `runtime_boundary.rs::apply_runtime_command` decrements before most endpoint
  response validation.

### JIT/Interpreter ABI Parity Tests

Add a shared resolver test or malformed-metadata test:

- interface call target with `recv_slots != 1` must be rejected identically by
  interpreter and JIT;
- JIT callback must not partially construct a callee frame before this
  validation passes.

Original risk checkpoints:

- `exec/call.rs` rejects `recv_slots != 1`.
- `vm/jit/callbacks/closure_call.rs::jit_prepare_iface_call` only checks
  `recv_slots <= param_slots`.

## Phase 1 Record: Make Pending Runtime Effects Transactional

This phase should land before broad host-event work because it removes the
highest-risk split owner in the JIT boundary.

### Target Shape

Prefer moving spawned fibers into `RuntimeTransition` directly:

```rust
pub struct RuntimeTransition {
    pub boundary: RuntimeBoundary,
    pub resume: ResumePolicy,
    pub wakes: Vec<WakeCommand>,
    pub gc_roots: GcRootEffect,
    pub host: Vec<HostCommand>,
    pub spawns: Vec<Fiber>,
}
```

If direct ownership makes transition cloning or debug formatting awkward, use a
single pending owner:

```rust
pub struct PendingRuntimeEffects {
    transitions: Vec<RuntimeTransition>,
    spawns: Vec<Fiber>,
}
```

The direct `spawns: Vec<Fiber>` design is simpler and should be preferred
unless a concrete implementation constraint appears.

### Required Changes

1. Delete `VmState::pending_spawned_fibers`.
2. Replace `RuntimeTransition::spawn_count` with `RuntimeTransition::spawns`.
3. Change JIT `go` callbacks and interpreter `GoStart`/local `GoIsland` to push
   spawned fibers into the emitted transition.
4. Change `merge_side_effects_from` to append `spawns`.
5. Change terminal discard paths to drop the whole pending transition object.
6. Change `apply_runtime_transition` to spawn each fiber from
   `transition.spawns`.
7. Add a root-scanning hook for pending transitions if they can survive across
   a GC boundary. If the implementation proves pending transitions are always
   consumed before GC, encode that proof with tests and assertions.

### Non-Goals

- Do not change goroutine scheduling semantics.
- Do not refactor the hot interpreter dispatch loop beyond replacing the spawn
  side-effect container.
- Do not widen JIT support or fallback policy in this phase.

### Done Criteria

- There is no side buffer that requires a separate count to match a transition.
- Discarding pending transitions also discards spawned fibers.
- Applying a transition cannot drain an unrelated older spawn.
- GC can either scan pending spawned fibers or prove they are unreachable from a
  GC boundary.

## Phase 2 Record: Replace Host Token-Only Wake Identity

This phase fixes the broadest cross-layer boundary leak. It touches VM,
runtime-wasm, vo-web, app-runtime, and Studio.

### Target Shape

Introduce a VM-level identity that can represent all host waits:

```rust
pub enum HostWaitSource {
    Timer,
    FetchReplay,
    ExtReplay,
    DisplayPulse,
    AppEventReplay,
}

pub struct HostWaitKey {
    pub source: HostWaitSource,
    pub token: u64,
    pub wake_key: FiberWakeKey,
    pub registration: WaitRegistrationKey,
}
```

The exact enum names can differ, but the identity must include source and
registration. `FiberWakeKey` should be present once the waiter has been
registered by the scheduler. For outbound host scheduling APIs that are called
before the host receives the key, expose an opaque VM-owned key rather than a
raw token.

### Token Allocation Options

There are two acceptable designs:

1. One VM-owned token allocator for every host wait source.

   This keeps raw tokens unique within a VM instance, but still requires source
   checks so stale commands cannot cross source boundaries.

2. Per-source token allocators plus mandatory `(source, token)` matching.

   This preserves local source ownership in time/fetch/ext bridges, but every
   public wake API must require source.

The first design is cleaner for VM invariants. The second can be used as an
incremental migration if WASM or extension lifecycle code needs local counters
temporarily.

### Required Changes

1. Replace `Scheduler::wake_host_event(token)` with a source-aware API.
2. Change `RuntimeCommand::host_event_wake` and
   `RuntimeCommand::host_event_wake_with_data` to carry source and, when
   available, registration identity.
3. Change `Scheduler::host_event_waiters` lookup from token-only to complete
   identity matching.
4. Change `PendingHostEvent` to carry the opaque host wait key. Do not expose
   token-only scheduling to `vo-web`, app-runtime, or Studio.
5. Change `vo-web/src/async_runner.rs` so fetch promise completion wakes the
   fetch replay source, not any waiter with the same token.
6. Change `runtime-wasm/src/time.rs`, `runtime-wasm/src/net_http.rs`, and
   `runtime-wasm/src/ext_bridge.rs` to either request tokens from the same VM
   allocator or tag their local tokens with `HostWaitSource`.
7. Change Studio's `guiHostTimers` map key from token string to the opaque host
   wait key. This prevents a new pending event with the same raw token from
   canceling an unrelated timer.
8. Change app-runtime mailbox dedup from `BTreeSet<u64>` to a typed key set.

### Compatibility Adapter

During migration, a token-only public method may remain only as a narrow
adapter for old tests or external callers:

```rust
pub fn wake_host_event_legacy_timer_token(&mut self, token: u64)
```

It must be named as legacy/source-specific and route to `HostWaitSource::Timer`.
It must not be the default or generic API.

### Done Criteria

- No generic API can wake host events by raw `u64` alone.
- Timer, fetch replay, extension replay, and display pulse waits can reuse the
  same raw token without cross-waking.
- App runtime and Studio host-event queues dedup by typed key.
- Tests cover same-token different-source waiters.

## Phase 3 Record: Unify Runtime-Created Method Closure ABI

This phase fixes the dynamic/reflection path and prevents future runtime-created
closures from inventing private call layouts.

### Preferred Design: Use Wrapper Functions

Dynamic method lookup should return a closure targeting the same wrapper ABI
that interface method dispatch uses for value receivers.

For value receivers:

- dynamic lookup resolves the method wrapper function id;
- the wrapper receives one receiver-data slot;
- the wrapper reconstructs or unboxes the full receiver value before calling
  the original method;
- closure capture count remains one and GC layout is the receiver data slot
  type.

For pointer receivers:

- dynamic lookup may target the original method if `recv_slots == 1`;
- or it may use a wrapper consistently for all method values.

The wrapper approach is preferred because it keeps runtime-created method
closures in the same ABI family as codegen-created interface wrappers.

### Alternative Design: Typed Receiver Payload Captures

If wrapper reuse is not available for dynamic lookup, extend closure objects to
store typed capture payload layout for runtime-created method closures:

- capture all receiver slots, not only `receiver_slot1`;
- store or resolve capture slot types from function metadata;
- update `closure::call_layout`, `FrameCallBuilder`, JIT closure-call callback,
  `build_closure_fiber_from_args_ptr`, web callback helpers, and GC closure
  scanning to consume that explicit layout.

This is more invasive and should only be chosen if wrapper lookup cannot be
made reliable.

### Required Changes

1. Audit method metadata exposed through `ExternCallContext::lookup_method`.
   It must return enough information to select the wrapper function id when a
   wrapper exists.
2. Change `dynamic.rs::get_method` to stop creating direct original-method
   closures for multi-slot value receivers.
3. Add validation in `FrameCallBuilder` and JIT closure-call preparation:
   runtime-created direct method closures are legal only when their captured
   receiver layout exactly matches `recv_slots`.
4. Change GC closure scan metadata so runtime-created method closures expose
   their receiver data roots for every supported receiver shape.
5. Update web/app/island closure-entry helpers because they reuse the same
   closure argument builder.

### Done Criteria

- Dynamic method lookup of a multi-slot value receiver calls with all receiver
  slots initialized.
- Receiver fields that are GC references survive forced GC before and during
  dynamic method calls.
- Direct call, closure call, goroutine spawn, island spawn, and web callback
  entry points share the same method closure ABI.
- The VM rejects unsupported runtime-created method closure layouts with a
  clear infrastructure error instead of constructing a partial frame.

## Phase 4 Record: Move Endpoint Response Accounting After Validation

This phase is narrow but important because response accounting affects
deadlock/suspension and endpoint tombstone cleanup.

### Required Changes

1. In `apply_runtime_command`, perform endpoint response validation before
   decrementing `pending_island_responses`.
2. Validation must include:
   - command source is `IslandEndpoint`;
   - source token endpoint id and fiber key match command target;
   - fiber key resolves to the same live fiber generation;
   - fiber is blocked on queue;
   - fiber has a matching remote endpoint wait;
   - response kind matches the pending operation.
3. Decrement `pending_island_responses` only after the response is accepted.
4. Wake the fiber only after decrement succeeds.
5. Keep underflow impossible. Use explicit conditional decrement or checked
   decrement instead of `saturating_sub` for accepted responses.
6. Update tests that currently expect rejected responses to decrement.

### Done Criteria

- Stale endpoint responses do not change in-flight response count.
- Accepted responses decrement exactly once.
- Tombstone cleanup cannot run early because of a rejected stale response.
- `wait_for_work` continues to report suspended while a legitimate response is
  still outstanding.

## Phase 5 Record: Share Interpreter/JIT Boundary Validators

This phase prevents parity drift after the larger protocol repairs.

### Required Shared Validators

At minimum, extract these cold validators:

```rust
resolve_iface_call_target(module, itab_cache, itab_id, method_idx)
    -> Result<IfaceCallTarget, VmErrorLike>

resolve_closure_call_target(module, gc, closure_ref, user_arg_count, ret_slots)
    -> Result<ClosureCallTarget, VmErrorLike>

validate_host_wake_command(command, current_waiter)
    -> Result<ValidatedHostWake, StaleWake>
```

The exact error type can stay VM-local. JIT callbacks can map validation
failure to JIT infrastructure errors where that is current policy.

### Required Changes

1. Move the interpreter `CallIface` receiver-slot validation into a helper used
   by both interpreter and JIT.
2. Make JIT `jit_prepare_iface_call` call the shared resolver before pushing a
   callee frame.
3. Move closure target layout validation into `FrameCallBuilder` or a helper
   called by both `FrameCallBuilder` and JIT closure-call callback.
4. Keep `vo-jit/src/semantics` as the opcode fact source. These validators are
   runtime ABI guards, not new lowering policy tables.

### Done Criteria

- Malformed interface method metadata fails before JIT writes callee slots.
- Interpreter and JIT produce equivalent behavior for malformed dynamic call
  targets.
- There is one source-backed resolver for each runtime ABI shape.

## Historical Landing Order

1. Phase 0 tests for pending spawn and endpoint accounting.
2. Phase 1 transactional pending effects.
3. Phase 0 tests for host identity, then Phase 2 host key migration.
4. Phase 0 tests for dynamic method closures, then Phase 3 closure ABI repair.
5. Phase 4 endpoint accounting if not already fixed with earlier tests.
6. Phase 5 shared validators.
7. Update `vm-runtime-boundary-architecture.md` only after source and tests
   match the claimed target state.

This order avoids building new host identity code on top of a broken pending
effects model, and it keeps the most invasive closure ABI work separate from
the scheduler/host protocol migration.

## Focused Validation

Run the narrowest checks for each slice first, then widen when a slice crosses
VM/JIT/WASM boundaries.

Suggested gates:

```sh
cargo test -p vo-vm runtime_boundary
cargo test -p vo-vm scheduler
cargo test -p vo-vm gc_root
cargo test -p vo-vm jit
cargo test -p vo-runtime dynamic
cargo check -p vo-web --target wasm32-unknown-unknown
./d.py test both tests/lang/cases/<new-dynamic-method-case>.vo
./d.py test jit tests/lang/cases/<new-jit-boundary-case>.vo
./d.py test wasm tests/lang/cases/<new-host-event-case>.vo
```

Before calling the repair complete, run the repo-level changed-plan gate:

```sh
cargo run -q -p vo-dev -- task plan pr --changed
cargo run -q -p vo-dev -- verify plan pr
```

If the final patch touches Studio scheduling or generated quickplay artifacts,
also run the relevant Studio and artifact provenance tasks from `eng/tasks.toml`
through `vo-dev`.

## Maintenance Review Checklist

Use this checklist before merging a new boundary change or diagnosing a
regression:

- Does every new wake path carry source identity and, when cross-turn, a
  generation-bearing fiber key?
- Can a stale or wrong-source command mutate scheduler state before rejection?
- Are pending side effects committed, discarded, and scanned as one unit?
- Can GC run while an unscanned root-owning payload exists?
- Does JIT call the same runtime ABI validator as the interpreter?
- Does a rejected external response leave accounting unchanged?
- Are tests checking the bug shape, not only the happy path?

## Completion Definition

The repair is complete, and remains complete while:

1. token-only host wake is gone from generic VM, scheduler, web, app-runtime,
   and Studio paths;
2. no pending spawned fiber can exist outside the transition/effects object
   that owns it;
3. dynamic method closures use the same ABI and GC metadata as codegen-created
   wrappers, or unsupported layouts are rejected before frame construction;
4. endpoint response accounting changes only for accepted responses;
5. interpreter and JIT share validators for interface and closure call target
   preparation;
6. the architecture document can truthfully claim that the old side paths were
   deleted or isolated behind the same validator.
