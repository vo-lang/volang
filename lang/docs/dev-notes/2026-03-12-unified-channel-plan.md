# Unified Channel: Merge Port into Channel

**Date**: 2026-03-12  
**Status**: Plan  
**Spec**: `docs/spec/channel.md`  
**Scope**: vo-syntax, vo-analysis, vo-codegen, vo-common-core, vo-runtime, vo-vm

---

## 1. Motivation

The current codebase has two nearly identical communication primitives:

- **`chan T`** — island-local, zero-copy, supports direction and select
- **`port T`** — cross-island capable, serialized deep-copy, no direction, no select

They share the same `QueueData` GC layout and the same generic `QueueState<W, M>` state machine,
but diverge on waiter type (`ChannelWaiter` vs `WaiterInfo`), message type (`Box<[u64]>` vs
`PackedValue`), opcode set, codegen paths, type checker paths, and VM dispatch.

This duplication is unnecessary. The distinction between "local" and "remote" is a **runtime
instance property** (determined by whether the channel has been transferred cross-island), not a
**type property**. Unifying them:

- Reduces language surface area (one concept instead of two)
- Enables select on cross-island channels
- Enables direction constraints on cross-island channels
- Eliminates redundant pack/unpack on same-island port operations
- Halves the opcode count, exec code, codegen branches, and type checker branches

---

## 2. Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Language type | `chan T` only, `port` keyword removed | One concept to learn |
| Direction | `chan<- T`, `<-chan T` apply to all channels | Useful for cross-island too |
| Element constraint | Unconstrained at creation; sendable required at `go(island)` capture | `chan func()` valid island-locally |
| Close semantics | End-state: panic on double-close. Phase 1 preserves current `port` behavior; semantic switch happens with language unification / compatibility removal | Keeps Phase 1 non-breaking while still converging on one rule |
| Local message type | `Box<[u64]>` (zero-copy slots) | Eliminates unnecessary pack/unpack |
| Remote message type | Pack at transport boundary only | Serialize on cross-island send, deserialize on cross-island recv response |
| Waiter type | Unified `QueueWaiter` with island routing + select support | One state machine for all |
| Wake routing | `QueueWaiter` is woken through VM state, not `Scheduler` alone | Remote waiters need island routing; select wake metadata still needs local scheduler integration |
| GC ValueKind | Keep both `ValueKind::Channel` and `ValueKind::Port` internally through Phase 1-2; defer GC-kind merge to cleanup | Decouples language/runtime unification from a large GC/serialization churn |
| Select | All channels support select; remote select deferred to Phase 3 | Incremental delivery |

---

## 3. Architecture: Before and After

### Before

```
Language:  chan T ──────────────── port T
              │                       │
Types:    ChanDetail              PortDetail
              │                       │
Opcodes:  ChanNew/Send/Recv/Close PortNew/Send/Recv/Close
              │                       │
Exec:     exec/channel.rs         exec/port.rs
              │                       │
Runtime:  ChannelState             PortState
          ChannelWaiter            WaiterInfo
          Box<[u64]>               PackedValue
              │                       │
VM loop:  ChanResult handler      PortResult + PortAction handler
              │                       │
Select:   select.rs (chan only)    —
```

### After

```
Language:  chan T  (with direction: chan<- T, <-chan T)
              │
Types:     ChanDetail  (direction + elem)
              │
Opcodes:   ChanNew / ChanSend / ChanRecv / ChanClose
              │
Exec:      exec/channel.rs  (unified, LOCAL + REMOTE paths)
              │
Runtime:   QueueState<QueueWaiter, Box<[u64]>>  (one state machine)
           QueueWaiter  (island_id + fiber_id + optional select info)
           BACKING_LOCAL / BACKING_REMOTE on QueueData
              │
VM loop:   ChanResult handler  (LOCAL: direct, REMOTE: island command)
              │
Select:    select.rs  (works for all channels; remote deferred)
```

---

## 4. Phases

### Phase 1: Runtime Unification (no language change, no user impact)

All existing tests continue to pass. Port and channel opcodes both exist but share the same
internal path.

#### Step 1.1: Unified waiter type

**Files**: `vo-runtime/src/objects/queue_state.rs`

Replace `ChannelWaiter` and `WaiterInfo` with a single `QueueWaiter`:

```rust
pub struct QueueWaiter {
    pub island_id: u32,
    pub fiber_id: u64,
    pub select: Option<SelectInfo>,
}

pub struct SelectInfo {
    pub case_index: u16,
    pub select_id: u64,
}
```

- `ChannelWaiter::Simple(id)` → `QueueWaiter { island_id: current, fiber_id: id, select: None }`
- `ChannelWaiter::Select(sw)` → `QueueWaiter { island_id: current, fiber_id: sw.fiber_id, select: Some(...) }`
- `WaiterInfo { island_id, fiber_id }` → `QueueWaiter { island_id, fiber_id, select: None }`

Move `cancel_select_waiters` from `impl ChannelState` to `impl<M> QueueState<QueueWaiter, M>`.

Update all consumers:
- `vo-vm/src/exec/channel.rs` — use `QueueWaiter` instead of `ChannelWaiter`
- `vo-vm/src/exec/port.rs` — use `QueueWaiter` instead of `WaiterInfo`
- `vo-vm/src/exec/select.rs` — use `QueueWaiter`
- `vo-vm/src/scheduler.rs` — local wake helper handles select bookkeeping only
- `vo-vm/src/vm/types.rs` — `wake_waiter` takes `QueueWaiter` and owns local-vs-remote routing
- `vo-vm/src/vm/island_thread.rs` — `WaiterInfo` → `QueueWaiter`
- `vo-vm/src/vm/jit/callbacks/port.rs` — `WaiterInfo` → `QueueWaiter`
- `vo-runtime/src/objects/port/std_impl.rs` — types change
- `vo-runtime/src/objects/port/nostd_stub.rs` — types change

Important constraint: **do not** make `Scheduler` responsible for remote wake routing. `Scheduler`
only knows local fibers. The unified waiter must be consumed by a VM-level wake path that can either:

- wake a local fiber directly and set select metadata, or
- send `IslandCommand::WakeFiber` to another island.

**Validation**: `./d.py test both --release` — all tests pass.

#### Step 1.2: Unified message type — LOCAL port uses `Box<[u64]>`

**Files**: `vo-runtime/src/objects/queue_state.rs`, `vo-runtime/src/objects/port/std_impl.rs`,
`vo-vm/src/exec/port.rs`, `vo-vm/src/vm/island_thread.rs`, `vo-vm/src/vm/jit/callbacks/port.rs`

Change `PortState` from `QueueState<QueueWaiter, PackedValue>` to
`QueueState<QueueWaiter, Box<[u64]>>` — same as `ChannelState`.

This means:
- `port_send_core` LOCAL path: no longer calls `pack_slots`. Collects `Box<[u64]>` directly.
  Add typed write barrier (same as `exec_chan_send`).
- `port_send_core` REMOTE path: still packs at the transport boundary (pack from src slots, not
  from buffer).
- `port_recv_core` LOCAL path: no longer calls `unpack_slots`. Reads `Box<[u64]>` directly.
- `port_recv_core` REMOTE path: unchanged (unpack from response bytes).
- `handle_port_request_inner` on home island: incoming remote send → unpack bytes → `Box<[u64]>`
  → enqueue. Outgoing remote recv response → dequeue `Box<[u64]>` → pack → send bytes.

Implementation note: the current `port_send_core` takes `&Gc`, which is insufficient for the
typed write barrier. Step 1.2 must either:

- change `port_send_core` / `exec_port_send` / JIT callback signatures to accept `&mut Gc`, or
- move the barrier into the caller before invoking the shared send path.

After this step, `ChannelState` and `PortState` are the **same type**:
`QueueState<QueueWaiter, Box<[u64]>>`.

**Validation**: `./d.py test both --release` — all tests pass.

#### Step 1.3: Introduce a shared queue action/result substrate

**Files**: `vo-vm/src/exec/channel.rs`, `vo-vm/src/exec/port.rs`, `vo-vm/src/vm/mod.rs`

The end-state is one queue operation result model, but the implementation should not force an
all-at-once enum collapse if that obscures control-flow details like PC rollback or pending remote
response state. The safe incremental target is:

- introduce a shared wake/action representation over `QueueWaiter`
- make VM/JIT dispatch share the same wake/block/remote-send/remote-recv/remote-close handling
- keep thin per-op wrappers temporarily if that makes the transition mechanically safer

An eventual unified shape can look like:

```rust
pub enum ChanResult {
    Continue,
    Yield,
    Wake(QueueWaiter),
    WakeMultiple(Vec<QueueWaiter>),
    Trap(RuntimeTrapKind),
    // Remote-only variants (for channels with BACKING_REMOTE)
    RemoteSend { endpoint_id: u64, home_island: u32, data: Vec<u8> },
    RemoteRecv { endpoint_id: u64, home_island: u32 },
    RemoteClose { endpoint_id: u64, home_island: u32 },
    Closed { waiters: Vec<QueueWaiter>, endpoint_id: Option<u64> },
}
```

Merge the VM main loop's queue dispatch handling so channel and port operations go through the same
wake/block/remote command path. Keep PC rollback behavior explicit:

- local/remote recv that blocks must replay the opcode
- remote send remains a "resume at next instruction" operation
- remote recv response staging (`fiber.port_recv_response`, `fiber.port_send_closed`) must stay
  coherent until opcode-level replay is removed or generalized

Remove `PortResult`, `PortRecvCoreResult`, and `PortAction`.

**Validation**: `./d.py test both --release` — all tests pass.

#### Step 1.4: Unify exec functions behind shared queue core

**Files**: `vo-vm/src/exec/channel.rs`, `vo-vm/src/exec/port.rs`

Port opcodes now call into shared queue helpers with a `backing` check:

- `exec_port_send` / `exec_chan_send` share the same queue send core
- `exec_port_recv` / `exec_chan_recv` share the same queue recv core
- `exec_port_close` / `exec_chan_close` share the same close/wake collection core where possible

After this, `exec/port.rs` becomes a thin delegation layer (or is removed entirely, with port
opcodes directly calling channel functions in the VM loop).

Phase 1 keeps current `port`-visible close semantics for compatibility. The semantic switch to
"double close panics" should happen together with the language-level removal of `port` (or with an
explicit compatibility break), not inside the "no user impact" phase.

**Validation**: `./d.py test both --release` — all tests pass.

---

### Phase 2: Language Unification (breaking change for `port` users)

#### Step 2.1: Remove `port` from syntax

**Files**: `vo-syntax/src/token.rs`, `vo-syntax/src/lexer.rs`, `vo-syntax/src/ast.rs`,
`vo-syntax/src/parser/mod.rs`, `vo-syntax/src/parser/types.rs`,
`vo-syntax/src/parser/decl.rs`, `vo-syntax/src/display.rs`

- Remove `TokenKind::Port`
- Remove `TypeExprKind::Port`
- Remove keyword mapping `"port" => Port`
- `port T` now parses as `chan T` — or becomes a parse error with a helpful migration message

#### Step 2.2: Remove `port` from type system

**Files**: `vo-analysis/src/typ.rs`, `vo-analysis/src/check/checker.rs`,
`vo-analysis/src/check/typexpr.rs`, `vo-analysis/src/check/expr.rs`,
`vo-analysis/src/check/stmt.rs`, `vo-analysis/src/check/builtin.rs`,
`vo-analysis/src/check/util.rs`, `vo-analysis/src/check/interface.rs`,
`vo-analysis/src/check/sendable.rs`, `vo-analysis/src/check/conversion.rs`

- Remove `Type::Port` and `PortDetail`
- Merge all `is_port()` branches into `is_chan()` branches
- `port_elem_type` / `port_elem_slots` → `chan_elem_type` / `chan_elem_slots`
- **DONE**: `Type::Chan(_)` → `Sendability::Static` in `sendable.rs`. Channel handle
  pack/unpack implemented in `pack.rs`. `prepare_value_chans_for_transfer` in `exec/island.rs`
  recursively prepares nested channels. `go_island.rs` post-pass checks capture/arg sendability.
  `stmt.rs` checks `go @(island)` target is `island` type.

#### Step 2.3: Remove port opcodes from codegen

**Files**: `vo-codegen/src/stmt/mod.rs`, `vo-codegen/src/expr/mod.rs`,
`vo-codegen/src/expr/builtin.rs`, `vo-codegen/src/type_info.rs`

- Remove all `is_port` branches — everything emits `ChanSend` / `ChanRecv` / `ChanClose` /
  `ChanNew`
- The runtime distinguishes LOCAL/REMOTE by `QueueData::backing`, not by opcode

#### Step 2.4: Remove port opcodes from bytecode

**Files**: `vo-common-core/src/bytecode.rs` (or wherever opcodes are defined)

Remove `PortNew`, `PortSend`, `PortRecv`, `PortClose`, `PortLen`, `PortCap`.

The VM interpreter's port opcode handlers become dead code. Remove them.

Channel opcodes now handle REMOTE backing internally — the VM checks `backing` after the
channel GcRef is loaded and routes to the appropriate path.

#### Step 2.5: Update island_msg.rs

**Files**: `vo-runtime/src/island_msg.rs`

Keep the current wire protocol shape for the first migration step. Renaming `PortWire` is optional;
the important part is semantic:

- syntax/type checker may expose only `chan`
- cross-island transfer still serializes transferred queue handles via the existing wire descriptor
- the runtime may continue to instantiate a `ValueKind::Port` object internally for transferred
  handles until a later cleanup phase

In other words, **language unification does not require immediate GC-kind unification**.

#### Step 2.6: Update select codegen

**Files**: `vo-codegen/src/stmt/select.rs`

Select already only supports `chan`. After Phase 2, this is the only channel type, so no change
is needed beyond removing any port-exclusion checks if they existed.

#### Step 2.7: Update all Vo test files and user code

All `port T` → `chan T`, `make(port T, cap)` → `make(chan T, cap)`.

**Validation**: `./d.py test both --release` — all tests pass.

---

### Phase 3: Select on Cross-Island Channels (future)

#### Step 3.1: Select readiness for LOCAL channels

Already works after Phase 1 (unified waiter + unified state). No additional work.

#### Step 3.2: Select readiness for REMOTE channels

Requires new `IslandCommand` variants:

```rust
IslandCommand::SelectRegister {
    endpoint_id: u64,
    kind: SelectRegisterKind, // Send or Recv
    from_island: u32,
    fiber_id: u64,
    select_id: u64,
    case_index: u16,
    data: Option<Vec<u8>>,    // for send cases
}

IslandCommand::SelectCancel {
    endpoint_id: u64,
    from_island: u32,
    select_id: u64,
}
```

The home island registers a `QueueWaiter` with `select: Some(...)` on the `PortState`. When
ready, it sends a `PortResponse` back. The remote island's `handle_port_response_command` sets
`fiber.select_state.woken_index` and wakes the fiber, same as local select.

Cancel: when one case wins, the remote island sends `SelectCancel` to all other home islands.
Race condition: a cancel may arrive after a wake is already in flight. The remote island must
handle duplicate wakes gracefully (ignore if select already completed).

This is significant complexity. Deferring to Phase 3 is appropriate.

#### Step 3.3: Compile-time restriction (interim)

Until Phase 3 is implemented, the compiler emits an error if a select case references a channel
that may be remote. In practice this is hard to determine statically (any `chan T` could
theoretically become remote). Options:

- **(A)** Runtime panic if select encounters a REMOTE channel
- **(B)** No restriction — REMOTE select just blocks forever (unsound)

Option A is correct. The `exec_select_exec` function checks `QueueData::backing` and panics with
"select on remote channel is not yet supported".

---

## 5. File Impact Summary

| File | Phase | Change |
|------|-------|--------|
| `vo-runtime/src/objects/queue_state.rs` | 1.1, 1.2 | Unified waiter, unified state type |
| `vo-runtime/src/objects/port/std_impl.rs` | 1.1, 1.2 | `Box<[u64]>` message type, new waiter |
| `vo-runtime/src/objects/port/nostd_stub.rs` | 1.1, 1.2 | Same changes for no_std |
| `vo-runtime/src/objects/channel.rs` | 1.1 | New waiter type |
| `vo-runtime/src/island_msg.rs` | 2.5 | Rename PortWire (optional) |
| `vo-runtime/src/island.rs` | — | No change in Phase 1-2 (`IslandCommand` changes only for remote select) |
| `vo-runtime/src/island_transport.rs` | — | No change |
| `vo-vm/src/exec/channel.rs` | 1.1, 1.3, 1.4 | Unified result type, REMOTE support |
| `vo-vm/src/exec/port.rs` | 1.1, 1.2, 1.3, 1.4 | Thin wrapper → removed |
| `vo-vm/src/exec/select.rs` | 1.1, 3.3 | New waiter type, runtime REMOTE check |
| `vo-vm/src/vm/mod.rs` | 1.3, 1.4 | Merged dispatch, remove PortAction macro |
| `vo-vm/src/vm/island_thread.rs` | 1.1, 1.2 | New waiter/message types |
| `vo-vm/src/vm/jit/callbacks/port.rs` | 1.1, 1.2, 1.4 | New waiter, delegates to unified exec |
| `vo-vm/src/vm/types.rs` | 1.1 | `wake_waiter` becomes `QueueWaiter`-aware and owns remote routing |
| `vo-vm/src/scheduler.rs` | 1.1 | Local queue waiter wake helper replaces channel-specific wake helper |
| `vo-vm/src/fiber.rs` | 1.1 | SelectState uses `QueueWaiter` |
| `vo-syntax/src/**` | 2.1 | Remove `port` keyword and AST node |
| `vo-analysis/src/**` | 2.2 | Remove `Type::Port`, merge branches |
| `vo-codegen/src/**` | 2.3 | Remove `is_port` branches |
| `vo-common-core/src/**` | 2.4 | Remove port opcodes |
| Vo test files | 2.7 | `port T` → `chan T` |

---

## 6. Risk Assessment

| Risk | Mitigation |
|------|------------|
| LOCAL port pack/unpack removal breaks GC | Add typed write barrier to port send (same as channel). Step 1.2 explicitly handles this. |
| GC root scanning misses port buffer values | After Step 1.2, port buffer contains `Box<[u64]>` with GC refs → same scanning as channel buffer. Verify in `gc_roots.rs`. |
| Phase 1 accidentally becomes user-visible | Do not change `port` close semantics in Phase 1. Keep compatibility until language removal / explicit break. |
| Remote select race conditions | Deferred to Phase 3. Phase 1-2 uses runtime panic for REMOTE select. |
| Breaking change for `port` users | Phase 2 is opt-in. Phase 1 is invisible. Migration is mechanical (`port` → `chan`). |
| Sendability flip for `chan` lands before transfer support | **DONE** — `Type::Chan` → `Sendability::Static` in `sendable.rs`, `pack.rs` Channel pack/unpack implemented, `prepare_value_chans_for_transfer` recursive preparation wired into VM loop ChanSend, JIT callback, and `handle_chan_request_command`. |
| Sendability check for `go(island)` captures | **DONE** — `go_island.rs` post-pass checks capture/arg sendability after escape analysis. `stmt.rs` checks target is `island` type. Error codes: `GoIslandTargetNotIsland` (2524), `GoIslandNotSendable` (2525). |
| Interface (`any`) cross-island transfer | `any` is `RuntimeCheck` sendable at compile time. Runtime `pack.rs` currently panics on `ValueKind::Interface`. Future work: pack interface inner value recursively based on dynamic type. |
| `ValueKind::Port` removal cascades | Keep `ValueKind::Port` internally through the migration; treat GC-kind merge as later cleanup. |

---

## 7. Estimated Effort

| Phase | Days | Risk |
|-------|------|------|
| Phase 1 (runtime unification) | 4–5 | Medium — VM/JIT wake routing and remote replay semantics must stay coherent |
| Phase 2 (language removal) | 2–3 | Low — mechanical, compiler-guided |
| Phase 3 (remote select) | 3–5 | Medium — distributed cancel protocol |
| **Total** | **8–12** | |

Phase 1 and Phase 2 can be shipped independently. Phase 3 is future work.
