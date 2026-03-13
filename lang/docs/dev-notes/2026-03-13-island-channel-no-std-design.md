# Island/Channel no_std Support Design

**Date**: 2026-03-13
**Status**: Implemented
**Scope**: vo-runtime, vo-vm (island, channel, transport, fiber, exec, JIT layers), vo-web host outcome mapping
**Prerequisite for**: Phase 1 of island-gpu-transport-design (transport-backed ports)

---

## 1. Problem Statement

Before this refactor, the island/channel subsystem was gated behind `#[cfg(feature = "std")]` at
every layer: transport traits, island thread management, remote channel proxies,
endpoint registries, fiber remote-recv state, and all cross-island dispatch logic.

This meant the **wasm/no_std VM could not participate in cross-island communication at
all**. It could not:
- Create stable non-main island handles (`IslandNew` returned a dummy main handle)
- Send/recv on remote channels (hit `unreachable!` / std-gated paths)
- Drain host-injected island commands or emit outbound routing commands
- Surface "waiting for host island routing" distinctly from completion/deadlock

The island-gpu-transport design requires a **wasm VM running in a WebView or Web
Worker** to be a full island participant — receiving spawned fibers, executing port
send/recv, and processing channel commands delivered via external transport.

**Goal**: Make the island/channel protocol work on no_std targets, so a wasm VM can
be a transport-backed island without requiring `std::thread`, `std::sync::mpsc`,
`std::sync::Arc<Mutex<_>>`, or `std::collections::HashSet`.

This document now records the implemented architecture that removed those limits.

---

## 2. Current std Dependencies — Audit

### 2.1 vo-runtime

| File | std dependency | What it provides |
|------|---------------|-----------------|
| `island_transport.rs` | `mpsc::{Sender, Receiver}`, `Duration`, `Arc` | `IslandTransport` trait, `IslandSender` trait, `InThreadTransport` |
| `island.rs` | `Vec<u8>` in `IslandCommand` variants | `IslandCommand`, `ChanRequestKind`, `ChanResponseKind` |
| `island_msg.rs` | `alloc::vec` (already no_std compatible) | Spawn payload encode/decode |
| `objects/queue_state.rs` | `HashSet<u32>` in `HomeInfo` | `HomeInfo`, `RemoteProxy`, `QueueData` |
| `objects/channel.rs` | `HashSet`, `Box` for `HomeInfo`/`RemoteProxy` | Remote proxy create, home info install, `is_remote`, `drop_inner` |

### 2.2 vo-vm

| File | std dependency | What it provides |
|------|---------------|-----------------|
| `vm/types.rs` | `JoinHandle`, `Arc<Mutex<HashMap>>`, `HashMap` | `IslandRegistry`, `IslandThread`, `EndpointRegistry`, `VmState` fields |
| `vm/mod.rs` | `thread::spawn`, `Arc`, `Mutex`, `HashMap` | `create_island`, `process_island_commands`, `dispatch_island_command`, `wait_for_work` |
| `vm/island_thread.rs` | `Arc`, `IslandTransport`, `Duration` | `run_island_thread`, `run_island_loop`, command handlers |
| `fiber.rs` | None beyond `Vec<u8>` | `RemoteRecvResponse`, `remote_recv_response`, `remote_send_closed` fields |
| `exec/queue.rs` | None | `QueueAction::Remote*` variants |
| `exec/channel.rs` | None beyond existing alloc | `decode_remote_recv_response`, `replay_remote_recv_response` |
| `exec/transport.rs` | `GcRef` for `resolve_unpacked_chan_handle` | Pack/unpack transport messages, chan handle resolution |
| `exec/island.rs` | `HashSet`, `IslandCommand`, `ChanRequestKind` | `prepare_chans_for_transfer`, `prepare_single_chan`, `pack_closure_for_island` |
| `exec/select.rs` | `is_remote` check | Remote channel rejection in select |
| `jit/callbacks/channel.rs` | `cfg(std)` blocks | Remote send/recv/close in JIT callbacks |

### 2.3 Categories of std usage

| Category | Examples | Can be replaced? |
|----------|---------|-----------------|
| **Threading** | `thread::spawn`, `JoinHandle` | std-only — island thread spawning is inherently std |
| **Synchronization** | `Arc<Mutex<HashMap>>` (IslandRegistry) | std-only — shared registry needs locking |
| **mpsc channels** | `Sender<IslandCommand>`, `Receiver<IslandCommand>` | std-only — InThreadTransport is inherently std |
| **Collections** | `HashSet<u32>` in HomeInfo.peers | Replace with `hashbrown::HashSet` |
| **Duration** | `recv_timeout(Duration)` | std-only — timeout semantics need OS clock |
| **Data structures** | `RemoteProxy`, `HomeInfo`, `EndpointRegistry` | **Can be made no_std** — pure data, no OS deps |
| **Protocol logic** | `IslandCommand`, `ChanRequestKind`, `ChanResponseKind` | **Can be made no_std** — pure enums with `Vec<u8>` |
| **Fiber state** | `RemoteRecvResponse`, `remote_send_closed` | **Can be made no_std** — just `Vec<u8>` + `bool` |
| **Channel objects** | `RemoteProxy`, `create_remote_proxy`, `is_remote` | **Can be made no_std** — GC object manipulation |

---

## 3. Design Principles

1. **no_std means "can be an island participant"** — a no_std VM can receive
   commands, process channel requests/responses, host remote channel proxies,
   run fibers spawned from other islands, and allocate island identities for host
   routing. It does NOT need to spawn threads.

2. **Transport injection, not transport ownership** — the no_std VM receives
   commands through a host-injected interface (function pointers, callbacks,
   or a queue), not through `IslandTransport` trait objects (which require
   `Send + 'static`).

3. **Split: protocol vs. plumbing** — The protocol (IslandCommand, ChanRequest,
   ChanResponse, RemoteProxy, HomeInfo, EndpointRegistry) is pure data and can
   be no_std. The plumbing (threads, mpsc, Arc<Mutex>) stays std-only.

4. **No new abstractions without necessity** — Don't invent a no_std transport
   trait. The host (JS bootstrap, Tauri, etc.) pushes commands into the VM via
   a simple `push_command(IslandCommand)` method. The VM pulls from an internal
   queue.

---

## 4. Architecture

### 4.1 Layer split

```
┌─────────────────────────────────────────────────┐
│              Protocol Layer (no_std)              │
│                                                   │
│  IslandCommand, ChanRequestKind, ChanResponseKind│
│  RemoteProxy, HomeInfo, EndpointRegistry          │
│  RemoteRecvResponse, QueueAction::Remote*         │
│  island_msg encode/decode                         │
│  pack/unpack transport messages                   │
│  prepare_chans_for_transfer                       │
│  channel_send_core / channel_recv_core remote path│
│  Fiber: remote_recv_response, remote_send_closed  │
│  Fiber: apply_chan_response                        │
│  vm/island_shared.rs shared command handlers       │
│  command_queue / outbound_commands / pending responses │
│  dispatch_island_command / SchedulingOutcome::Suspended │
└─────────────────────────────┬───────────────────┘
                              │ uses
┌─────────────────────────────▼───────────────────┐
│             Plumbing Layer (std only)             │
│                                                   │
│  IslandTransport trait, IslandSender trait         │
│  InThreadTransport (mpsc)                         │
│  IslandRegistry (Arc<Mutex<HashMap>>)             │
│  IslandThread (JoinHandle)                        │
│  Vm::create_island (thread::spawn)                │
│  island_thread::run_island_thread                 │
│  VmState: island_senders, island_registry         │
│  VmState: main_transport                          │
│  Duration-based recv_timeout                      │
└─────────────────────────────────────────────────┘
```

### 4.2 no_std VM command ingestion

Instead of `IslandTransport` (which is a trait with blocking recv and timeout),
the no_std VM uses a simple command queue that the host pushes into:

```rust
// In VmState — always available (not cfg-gated)
pub command_queue: VecDeque<IslandCommand>,
```

The host (JS via wasm extern, Tauri via FFI) calls:

```rust
impl Vm {
    /// Push a command from the host. Called by external transport bridge.
    pub fn push_island_command(&mut self, cmd: IslandCommand) {
        self.state.command_queue.push_back(cmd);
    }
}
```

`process_island_commands` drains this queue identically to how it drains
`main_transport.try_recv()` today.

On std, `main_transport` continues to work as before for in-thread islands.
The command_queue is an additional path for host-injected commands and is
useful for both std (Tauri native → VM) and no_std (JS → wasm VM).

### 4.3 no_std VM command emission

The no_std VM cannot `send_to_island` (no senders, no registry). Instead,
outbound commands are queued:

```rust
// In VmState — always available
pub outbound_commands: VecDeque<(u32, IslandCommand)>, // (target_island_id, cmd)
```

The host polls this after each scheduling step:

```rust
impl Vm {
    /// Take all outbound commands. Host routes them to target islands.
    pub fn take_outbound_commands(&mut self) -> VecDeque<(u32, IslandCommand)> {
        core::mem::take(&mut self.state.outbound_commands)
    }
}
```

On std, `send_to_island` continues to use `island_senders` / `island_registry`
as today. On no_std, `send_to_island` pushes to `outbound_commands` instead.

### 4.4 Unified send_to_island

```rust
impl VmState {
    /// Route a command to a target island.
    /// std: direct send via senders/registry
    /// no_std: queue for host pickup
    pub fn send_to_island(&mut self, island_id: u32, cmd: IslandCommand) {
        #[cfg(feature = "std")]
        {
            self.try_send_to_island(island_id, cmd)
                .unwrap_or_else(|e| panic!("send_to_island: {:?}", e));
            return;
        }
        #[cfg(not(feature = "std"))]
        {
            self.outbound_commands.push_back((island_id, cmd));
        }
    }
}
```

This refactor changes `send_to_island` and the `send_chan_*` helpers to `&mut self`
uniformly. In practice this worked cleanly because `Vm` already borrows `state` and
`scheduler` as disjoint fields.

### 4.5 Scheduler contract for host-routed islands

Remote send/recv requests increment `pending_island_responses`. When the scheduler
has no runnable fibers:

- `SuspendedForHostEvents` still means "host event/timer/fetch required"
- `Suspended` now means "host island routing required" (pending outbound commands
  and/or pending remote island responses)
- `Blocked` remains true deadlock / no-progress state

This is what lets a no_std host distinguish "VM is waiting for island routing"
from "VM is finished" and "VM is deadlocked".

---

## 5. Detailed Changes

### 5.1 vo-runtime — Make protocol types no_std

#### `island.rs`
- **Remove `#[cfg(feature = "std")]` from `IslandCommand`, `ChanRequestKind`, `ChanResponseKind`**
- These are pure enums containing only `Vec<u8>`, `u32`, `u64`, `bool`, `PackedValue`
- All inner types (`Vec<u8>`, `PackedValue`) already work on no_std with `alloc`
- This is the single most impactful change — it unblocks everything downstream

#### `island_transport.rs`
- **Keep `IslandTransport` and `IslandSender` traits as `#[cfg(feature = "std")]`**
- **Keep `InThreadTransport` and `InThreadSender` as `#[cfg(feature = "std")]`**
- **Move `TransportError` to always-available** (it's a simple enum, used in error paths)
- No new no_std transport trait needed

#### `objects/queue_state.rs`
- **Make `HomeInfo` always available**, replace `std::collections::HashSet<u32>` with `hashbrown::HashSet<u32>`
  - `hashbrown` is already a dependency of vo-runtime
- **Make `RemoteProxy` always available** — it's 3 fields: `u64`, `u32`, `bool`
- Remove `#[cfg(feature = "std")]` from both

#### `objects/channel.rs`
- **Make `create_remote_proxy`, `create_remote_proxy_with_closed` always available**
- **Make `is_remote` always available** (remove the no_std stub that returns `false`)
- **Make `remote_proxy`, `remote_proxy_mut` always available**
- **Make `home_info`, `home_info_mut`, `install_home_info` always available**
- **Update `drop_inner`** to handle BACKING_REMOTE on no_std
- These functions manipulate GC objects via raw pointers — no OS dependencies

### 5.2 vo-vm — Make channel remote paths no_std

#### `fiber.rs`
- **Remove `#[cfg(feature = "std")]` from `RemoteRecvResponse`**
- **Remove `#[cfg(feature = "std")]` from `remote_recv_response`, `remote_send_closed` fields**
- **Remove `#[cfg(feature = "std")]` from `take_remote_recv_response`, `consume_remote_send_closed`, `apply_chan_response`**
- These are pure data operations on `Vec<u8>` + `bool`

#### `exec/queue.rs`
- **Remove `#[cfg(feature = "std")]` from `QueueAction::RemoteSend`, `RemoteRecv`, `RemoteRecvData`, `RemoteClose`**
- These are pure enum variants with `Vec<u8>`, `u64`, `u32`

#### `exec/channel.rs`
- **Remove `#[cfg(feature = "std")]` from `decode_remote_recv_response`, `replay_remote_recv_response`**
- **Remove all `#[cfg(not(feature = "std"))] unreachable!("REMOTE channel in no_std")` stubs**
- Remote channel send/recv/close now work on no_std — the exec layer returns
  `QueueAction::Remote*` variants, and the VM dispatch layer routes them
  appropriately (via `send_to_island` → `outbound_commands` on no_std)

#### `exec/transport.rs`
- **Remove `#[cfg(feature = "std")]` from `resolve_unpacked_chan_handle`**
- **Remove conditional compilation from `unpack_transport_message`**
- endpoint_registry parameter becomes always-available

#### `exec/island.rs`
- **Remove `#[cfg(feature = "std")]` from `prepare_chans_for_transfer`, `prepare_value_chans_for_transfer`, `prepare_single_chan`, `prepare_remote_send_value_if_needed`, `pack_closure_for_island`**
- **Replace `std::collections::HashSet` with `hashbrown::HashSet`** in `prepare_chans_for_transfer` and `prepare_single_chan`
- **Remove `#[cfg(feature = "std")]` from helper functions**: `may_contain_channel`, `lookup_struct_meta_id_safe`, `read_element_raw`

#### `exec/select.rs`
- **Remote channel in select**: currently rejects with `UnsupportedRemote`. This
  stays as-is for now — select over remote channels is a separate feature.

#### `gc_roots.rs`
- **Scan `EndpointRegistry` live channels on all targets**
- no_std remote proxies / home endpoints are now part of the always-available
  protocol surface, so GC must mark them regardless of `std`

#### `vm/types.rs`
- **Make `EndpointRegistry` always available**, replace `HashMap` with `hashbrown::HashMap`
- **Make `EndpointEntry` always available**
- **Add `command_queue: VecDeque<IslandCommand>` to `VmState`** (always available)
- **Add `outbound_commands: VecDeque<(u32, IslandCommand)>` to `VmState`** (always available)
- **Keep `IslandThread`, `IslandRegistry` as `#[cfg(feature = "std")]`**
- **`send_to_island` / `send_chan_*` helpers**: change from `&self` to `&mut self`,
  route through `outbound_commands` on no_std (see §4.4)

#### `vm/mod.rs`
- **`process_island_commands`**: drain both `main_transport` (std) AND
  `command_queue` (always). This unifies the ingestion path.
- **`dispatch_island_command`**: remove `#[cfg(feature = "std")]` — the dispatch
  logic (match on IslandCommand variants) is pure protocol, no OS deps. The
  concrete command handlers live in `vm/island_shared.rs`.
- **`create_island`**: stays `#[cfg(feature = "std")]` — thread spawning is std-only
- **`exec_island_new` in no_std**: allocates a real island handle from
  `next_island_id`. The host owns transport routing; the VM only needs a stable
  identity.
- **`GoIsland` local execution check**: compare against `current_island_id`, not a
  hard-coded `0`, so nested / host-created islands behave correctly on both std
  and no_std.
- **`wait_for_work`**: return `SchedulingOutcome::Suspended` when progress depends
  on host-routed island commands/responses rather than host events or runnable fibers

#### `vm/island_thread.rs`
- **Stays `#[cfg(feature = "std")]`** as the thread-based island runner
- Command processing itself is thin and routes through `vm/island_shared.rs`
- The no_std equivalent is the host's event loop calling `push_island_command`
  + `run_scheduled`

#### `jit/callbacks/channel.rs`
- **Remove `#[cfg(feature = "std")]` from remote channel handling** in
  `jit_chan_send`, `jit_chan_recv`, `jit_chan_close`
- Remote paths now work on no_std (they produce QueueAction::Remote* which
  routes through `send_to_island` → `outbound_commands`)

#### `jit/callbacks/goroutine.rs` and `jit/callbacks/island.rs`
- **Unify JIT island behavior with the interpreter**
- `jit_go_island` now uses `current_island_id` for local-vs-remote dispatch and
  routes remote spawn through `send_spawn_fiber_to_island`
- `jit_create_island` now allocates a real island handle on no_std instead of
  fabricating a dummy main-island handle

### 5.3 `send_to_island` signature change

The current `send_to_island(&self, ...)` uses `&self` because mpsc `Sender::send`
only needs `&self`. On no_std, we need `&mut self` to push to `outbound_commands`.

**Strategy**: Change all `send_*` helpers on `VmState` from `&self` to `&mut self`.

Call sites that currently borrow `&self.state` while also holding `&mut scheduler`
may need restructuring. Audit:

1. **`vm/mod.rs` dispatch_island_command** — has `&mut self` (Vm), can borrow
   `&mut self.state` easily
2. **`vm/island_thread.rs` handle_chan_*_command** — takes `&mut Vm`, same
3. **`jit/callbacks/channel.rs`** — has mutable access via `extract_context`
4. **`exec/channel.rs` channel_send_core** — already takes `&mut state`
5. **`VmState::wake_waiter`** — currently `&self`, calls `send_wake_to_island(&self)`
   → needs `&mut self`. But it also takes `&mut scheduler`. This is the main
   friction point.

**Resolution for `wake_waiter`**: Split into two phases:
```rust
fn wake_waiter(&mut self, waiter: &QueueWaiter, scheduler: &mut Scheduler) {
    if waiter.island_id == self.current_island_id {
        scheduler.wake_queue_waiter(waiter);
    } else {
        self.send_wake_to_island(waiter.island_id, waiter.fiber_id as u32);
    }
}
```
This already works because `self` here is `&mut VmState` and `scheduler` is a
separate field on `Vm`. The borrow checker allows `&mut vm.state` and
`&mut vm.scheduler` simultaneously because they're disjoint fields.

---

## 6. Migration Order

### Step 1: vo-runtime protocol types (no code logic change)

1. Remove `#[cfg(feature = "std")]` from `IslandCommand`, `ChanRequestKind`, `ChanResponseKind`
2. Add `#[cfg(not(feature = "std"))] use alloc::vec::Vec;` where needed
3. Make `HomeInfo` use `hashbrown::HashSet<u32>` instead of `std::collections::HashSet<u32>`
4. Remove `#[cfg(feature = "std")]` from `HomeInfo`, `RemoteProxy`
5. Remove `#[cfg(feature = "std")]` from channel object functions: `is_remote`, `remote_proxy`, `home_info`, `create_remote_proxy`, `install_home_info`, `drop_inner` REMOTE branch
6. Make `TransportError` always available
7. `cargo check -p vo-runtime` (std) + `cargo check -p vo-runtime --no-default-features` (no_std)

### Step 2: vo-vm fiber + exec layer

1. Remove `#[cfg(feature = "std")]` from `RemoteRecvResponse` and fiber fields
2. Remove `#[cfg(feature = "std")]` from `QueueAction::Remote*` variants
3. Remove `#[cfg(feature = "std")]` from exec/channel.rs remote helpers
4. Remove `#[cfg(feature = "std")]` from exec/transport.rs
5. Remove `#[cfg(feature = "std")]` from exec/island.rs (replace HashSet with hashbrown)
6. `cargo check -p vo-vm` (std) + `cargo check -p vo-vm --no-default-features` (no_std)

### Step 3: VmState command queue + send routing

1. Add `command_queue: VecDeque<IslandCommand>` to VmState
2. Add `outbound_commands: VecDeque<(u32, IslandCommand)>` to VmState
3. Make `EndpointRegistry` always available (hashbrown::HashMap)
4. Change `send_to_island` et al to `&mut self`, add no_std path
5. Update `process_island_commands` to drain `command_queue`
6. Make `dispatch_island_command` always available
7. Add `push_island_command` and `take_outbound_commands` public API
8. `cargo check -p vo-vm` (std) + `cargo check -p vo-vm --no-default-features`

### Step 4: JIT callbacks + VM tests

1. Remove `#[cfg(feature = "std")]` from JIT callback remote paths
2. Write VM tests for:
   - blocked fiber + pending island response ⇒ `SchedulingOutcome::Suspended`
   - `push_island_command(IslandCommand::ChanResponse { ... })` wakes the blocked fiber
3. `./d.py test both --release` — full regression

### Step 5: Public API for host integration

1. Expose `Vm::push_island_command(&mut self, cmd: IslandCommand)`
2. Expose `Vm::take_outbound_commands(&mut self) -> VecDeque<(u32, IslandCommand)>`
3. Expose `Vm::current_island_id(&self) -> u32`
4. Expose `Vm::set_island_id(&mut self, id: u32)` (host configures island identity)
5. Document the host integration contract

---

## 7. What Does NOT Change

- **`IslandTransport` / `IslandSender` traits** — remain std-only
- **`InThreadTransport`** — remains std-only
- **`IslandRegistry` (`Arc<Mutex<HashMap>>`)** — remains std-only
- **`IslandThread` / `JoinHandle`** — remains std-only
- **`Vm::create_island` (thread spawn)** — remains std-only
- **`island_thread.rs` thread loop** — remains std-only
- **`wait_for_work` transport timeout** — remains std-only
- **`VmState::island_senders` / `island_registry` / `main_transport`** — remain std-only
- **Select over remote channels** — remains UnsupportedRemote
- **IoRuntime** — remains std-only (orthogonal)

---

## 8. Risk Assessment

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| `send_to_island` `&self` → `&mut self` breaks borrow patterns | Medium | Vm has disjoint fields (state, scheduler); split borrows work |
| `hashbrown::HashSet` behavior difference from `std::HashSet` | None | hashbrown IS the backing impl for std HashMap/HashSet |
| No_std binary size increase from always-compiled remote paths | Low | Remote paths are small; dead-code elimination removes unused |
| `command_queue` + `outbound_commands` overhead on std path | None | Empty VecDeque is 0 alloc; std path ignores them |
| Channel operations produce `QueueAction::Remote*` on no_std but host doesn't handle | Medium | VM returns `SchedulingOutcome::Suspended`; host must drain `outbound_commands` and inject responses |

Validated with the current implementation:

- `cargo check -p vo-runtime --no-default-features`
- `cargo check -p vo-vm`
- `cargo check -p vo-vm --no-default-features`
- `cargo check -p vo-vm --features jit`
- `cargo check -p vo-web`
- `cargo test -p vo-vm run_scheduled_returns_suspended_when_waiting_for_island_response --lib`
- `cargo test -p vo-vm command_queue_chan_response_wakes_blocked_fiber --lib`

---

## 9. Relation to Island GPU Transport Design

This design is **Phase 1 prerequisite** from the island-gpu-transport design doc.
After this lands:

- A wasm VM (no_std) can be configured as island N with `set_island_id(N)`
- The host (JS bootstrap) pushes `IslandCommand::SpawnFiber` to start the render loop
- Channel commands (port data, close) flow through `push_island_command` / `take_outbound_commands`
- The same mechanism extends to `PortData` / `PortClosed` variants (added in Phase 1 of transport design)

The transport-backed ports (design doc §4.2 `PortBacking::Remote`) build on top of
this command queue infrastructure. Port send → `IslandCommand::PortData` → host
routes → target VM `push_island_command` → port recv.

---

## 10. Summary

| Metric | Before | After |
|--------|--------|-------|
| Protocol types available on no_std | None | All (IslandCommand, ChanRequest/Response, HomeInfo, RemoteProxy, EndpointRegistry) |
| Remote channel operations on no_std | `unreachable!` | Fully functional (routed via outbound_commands) |
| Fiber remote state on no_std | Not compiled | Always available |
| Host → VM command injection | None | `push_island_command` |
| VM → Host command emission | None | `take_outbound_commands` |
| std regression | N/A | Zero in validated builds — std plumbing remains intact while protocol handlers are shared |
| New abstractions | N/A | Zero — reuses IslandCommand, adds two VecDeque fields |
