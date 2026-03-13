# Local Channel + Remote Port Plan

**Date**: 2026-03-12  
**Status**: Plan  
**Spec**: `docs/spec/channel.md`  
**Scope**: vo-syntax, vo-analysis, vo-codegen, vo-common-core, vo-runtime, vo-vm, JIT, docs

This document **supersedes** the earlier "merge port into channel" direction.
The new design keeps two primitives with a hard semantic boundary:

- `chan T` = island-local synchronization
- `port T` = cross-island mailbox with transferable send capability

---

## 1. Why the Earlier Direction Is Wrong

Treating remote communication as "just a channel with a remote proxy" creates the wrong trade-off:

- it looks simple to the user
- it becomes complex in the runtime
- it leaves sharp edges around `select`, `close`, fairness, and distributed ownership

In particular, transparent remote channels force one of two bad outcomes:

- a distributed `select` protocol with register/ready/commit/cancel races, or
- a misleading abstraction where some channel operations look local but are not semantically local

That is the wrong center of gravity for Vo.

The correct boundary is:

- **local synchronization stays local**
- **cross-island communication is explicit messaging**

---

## 2. Final Design Decisions

| Topic | Decision | Rationale |
|------|----------|-----------|
| Local synchronization | Keep `chan T` | Best model for same-island rendezvous and buffered local coordination |
| Cross-island messaging | Keep `port T` | Makes island boundary explicit |
| Channel transfer | Forbidden | Prevents transparent remote synchronization |
| Port transfer | Only `port<- T` is sendable | Receive side must stay local |
| Port buffering | Always buffered, positive capacity | Mailbox semantics are simpler and avoid remote rendezvous |
| Select | Local-only primitive | No distributed arbitration |
| Port in select | Receive cases only | Keeps select tied to local wait queues |
| Delivery failure | Distinct from `close` | Cross-island failure must not masquerade as local closure |
| Runtime architecture | Reuse remote routing for ports, not channels | Keeps correctness while leveraging existing island transport work |

---

## 3. User-Facing Model

### 3.1 Channels

`chan T` is for goroutines on the **same island**.

- unbuffered or buffered
- full local send / recv / close / select support
- not sendable across islands

### 3.2 Ports

`port T` is for **cross-island delivery**.

- receive side remains on one home island
- only send capability may be transferred
- `select` may wait on local port receives
- no transparent remote select exists because local receive ports already cover the intended use case

### 3.3 What the User Learns

The model is intentionally small:

- use `chan` for local coordination
- use `port` for crossing an island boundary
- if you want to wait on remote activity, wait on a **local receive port**

This is simpler than a single primitive with mode-dependent behavior.

---

## 4. Semantic Rules

### 4.1 Sendability

- `chan T` is **not sendable**
- `port T` is **not sendable**
- `<-port T` is **not sendable**
- `port<- T` is sendable if `T` is sendable

### 4.2 Ownership

- channel queue state and waiters are local to the creating island
- port receive state and waiters are local to the home island
- remote code can hold send capability only

### 4.3 Select

`select` is a local waiting construct.

Allowed:

- receive from `chan`
- send to `chan`
- receive from `port`

Rejected:

- send to `port` inside `select`
- any form of distributed remote channel select

### 4.4 Failure Semantics

- `close(port)` means the owner endpoint intentionally closed the mailbox
- transport loss / island death / routing failure is a distinct runtime failure path
- failure must not be silently converted into `ok == false`

---

## 5. Runtime Architecture

### 5.1 Channels Become Strictly Local

The runtime must stop treating channels as potentially remote language objects.

Implications:

- no language-level remote proxy channel semantics
- no channel-handle transfer across islands
- no "unsupported remote select" path for channels because channels are never remote

The existing local channel queue implementation remains valuable and should stay.

### 5.2 Ports Become the Only Cross-Island Queue Primitive

Ports carry the remote routing responsibility.

Conceptual structure:

- **home endpoint**: owns the mailbox buffer, closed state, receive waiters
- **sender capability**: lightweight reference to the home endpoint, transferable across islands

The no_std island routing work already done for remote queue operations should be reused here:

- host-injected command queue
- outbound command queue
- `SchedulingOutcome::Suspended`
- endpoint registry and response replay machinery

### 5.3 Port Waiting Is Local

Receive waiters are registered only on the home island.

This is the key simplification:

- local scheduler handles wake-up
- local `select` works normally
- no distributed select coordination is required

---

## 6. Implementation Plan

### Phase 0: Decision Freeze and Spec Alignment

**Goal**: eliminate contradictory docs before code churn starts.

Tasks:

- update `docs/spec/channel.md` to describe `chan` local-only and `port` explicit remote messaging
- mark this document as the authoritative plan
- stop describing transparent remote channels as intended language behavior

Validation:

- docs reviewed for internal consistency

### Phase 1: Type System and Syntax

**Goal**: make the language enforce the new boundary.

Files:

- `vo-syntax/src/**`
- `vo-analysis/src/**`

Tasks:

- keep both `chan` and `port` in syntax
- add or formalize port direction types:
  - `port T`
  - `port<- T`
  - `<-port T`
- make `chan` non-sendable
- make `port<- T` conditionally sendable on `T`
- make `port T` and `<-port T` non-sendable
- reject channel capture in `go(island)`
- reject receive-port capture in `go(island)`
- reject port send cases in `select`
- allow port receive cases in `select`
- improve diagnostics so the error explains the design rule, not just "type mismatch"

Validation:

- checker unit tests for sendability
- parser/type round-trip tests
- targeted diagnostics tests

### Phase 2: Runtime Split

**Goal**: remove channel-from-remote semantics and center remote routing on ports.

Files:

- `vo-runtime/src/objects/channel.rs`
- `vo-runtime/src/objects/port/**`
- `vo-runtime/src/objects/queue_state.rs`
- `vo-runtime/src/island.rs`
- `vo-runtime/src/island_msg.rs`
- `vo-vm/src/exec/channel.rs`
- `vo-vm/src/exec/port.rs`
- `vo-vm/src/exec/island.rs`
- `vo-vm/src/exec/transport.rs`
- `vo-vm/src/vm/**`
- `vo-vm/src/gc_roots.rs`

Tasks:

- stop preparing channel handles for cross-island transfer
- remove language-path creation of remote channel proxies
- keep local channel runtime fast and unchanged where possible
- model port home endpoint + sender capability explicitly
- route remote sends only through port machinery
- keep port receive state on the home island
- preserve `close` vs delivery failure distinction
- keep current host-routed no_std plumbing as the transport substrate for ports

Important rule:

- reuse queue infrastructure internally where it helps
- do **not** reuse a single language semantic model for channel and port

Validation:

- `cargo check -p vo-runtime`
- `cargo check -p vo-vm`
- `cargo check -p vo-vm --no-default-features`
- targeted VM tests for remote port send / local port recv / close / failure

### Phase 3: Codegen, Bytecode, and JIT

**Goal**: make generated code reflect the split directly.

Files:

- `vo-codegen/src/**`
- `vo-common-core/src/**`
- `vo-vm/src/vm/jit/callbacks/**`

Tasks:

- ensure channel opcodes are emitted only for local channels
- ensure port opcodes are emitted for port operations
- teach codegen that select legality differs for channels and ports
- remove JIT paths that assume remote channel operations are language-visible
- preserve or refine low-level helper sharing only where semantics still match

Validation:

- `cargo check -p vo-vm --features jit`
- targeted JIT tests for port send / recv / select-recv

### Phase 4: Migration of Tests and User Code

**Goal**: update all examples and tests to the explicit model.

Tasks:

- replace cross-island channel examples with port-based request/reply or publish/subscribe patterns
- update docs and tests that currently rely on channel transfer
- add examples that demonstrate:
  - local worker pool with channels
  - cross-island request/reply with ports
  - local select over port receives

Validation:

- `./d.py test both --release`

### Phase 5: Cleanup

**Goal**: remove stale compatibility paths and misleading wording.

Tasks:

- remove dead remote-channel code paths
- rename command / helper internals if they still use channel terminology for remote port traffic
- simplify docs that still describe "remote channel"
- review GC scanning and endpoint lifetime after the split

Validation:

- `./d.py test both --release`
- targeted no_std / JIT / vo-web checks as needed

---

## 7. File Impact Summary

| Area | Main Impact |
|------|-------------|
| `docs/spec/channel.md` | Normative semantics flip: channels local-only, ports explicit |
| `vo-syntax` | Port direction syntax and select legality |
| `vo-analysis` | Sendability, `go(island)` capture checks, port select checks |
| `vo-codegen` | Distinct codegen paths for channel vs port semantics |
| `vo-common-core` | Opcode semantics reviewed; keep split explicit |
| `vo-runtime` | Channel/port object split hardened; remote queue path becomes port-focused |
| `vo-vm` | Remote queue dispatch becomes port-only; channel exec becomes local-only |
| `vo-vm` JIT | Remove remote-channel-visible assumptions |
| Tests and docs | Replace transparent remote channel examples with port patterns |

---

## 8. Risks

| Risk | Mitigation |
|------|------------|
| Existing code depends on channel transfer | Provide strong diagnostics and mechanical migration examples |
| Current runtime internals use channel names for remote queue operations | Allow temporary internal naming mismatch during migration; fix in cleanup |
| Port direction syntax touches parser and checker broadly | Land syntax/checker before deep runtime churn |
| no_std routing regressions while repurposing remote machinery | Reuse the already-validated command queue / outbound queue substrate |
| Interface / `any` sendability remains partially dynamic | Keep runtime checks explicit and tested |

---

## 9. Estimated Effort

| Phase | Days | Risk |
|------|------|------|
| Phase 0 | 1 | Low |
| Phase 1 | 2–3 | Medium |
| Phase 2 | 4–6 | Medium |
| Phase 3 | 2–3 | Medium |
| Phase 4 | 1–2 | Low |
| Phase 5 | 1–2 | Low |
| **Total** | **11–17** | |

The cost is meaningful, but it is still lower-risk than implementing and maintaining a correct
distributed remote-channel semantic model.

---

## 10. Success Criteria

The migration is successful when all of the following are true:

- the spec no longer promises transparent remote channels
- the compiler rejects cross-island channel transfer
- remote communication is expressed through ports only
- local `select` covers local channels and local receive ports
- no runtime path still depends on "remote channel in select"
- std, no_std, and JIT builds pass with the new split
