# Islands & Ports Implementation Plan

> Historical note: this document records the initial implementation plan and
> preserves API sketches from that phase. For the current VM/island contracts,
> see `vm-runtime-boundary-architecture.md`.

## Overview

| Phase | Description | Est. Days |
|-------|-------------|-----------|
| 1 | Type System (sendability, types, parser) | 4 |
| 2 | Runtime Infrastructure (opcodes, pack/unpack, port) | 3.5 |
| 3 | Multi-Island Core (island struct, codegen, VM) | 4.5 |
| 4 | Multi-threading Integration | 2.5 |
| 5 | Testing | 2 |
| **Total** | | **16.5** |

## Dependency Graph

```
1.1 Sendability ──┐
                  ├─→ 1.4 Type Check Integration
1.2 Types ────────┤
                  │
1.3 Parser ───────┘
        │
        ▼
2.1 Opcodes ──────┐
                  ├─→ 2.3 Port Struct ──→ 3.3 VM Opcodes
2.2 Pack/Unpack ──┘           │
                              ▼
                    3.1 Island Struct ──→ 4.1 Multi-thread
                              │
                    3.2 Codegen ─────→ 4.2 Cross-Island Wake
                                              │
                                              ▼
                                         4.3 Lifecycle
                                              │
                                              ▼
                                         5. Testing
```

---

## Phase 1: Type System ✓

### 1.1 Sendability Check ✓

**File**: `vo-analysis/src/check/sendable.rs`

**Implemented**:
- `check_sendable(type_key, objs) -> Sendability` - recursive check with cycle detection
- `Sendability` enum: `Static`, `RuntimeCheck`, `NotSendable(String)`
- Full coverage of sendable/non-sendable types

### 1.2 island/port Types ✓

**Files**:
- `vo-analysis/src/typ.rs` - `Type::Island`, `Type::Port(PortDetail)`
- Helper functions: `is_port()`, `is_island()`, `has_nil()`, `comparable()`

### 1.3 Parser Extension ✓

**Files**:
- `vo-syntax/src/ast.rs` - `GoStmt.target_island: Option<Expr>`
- `vo-syntax/src/parser/stmt.rs` - parse `go @(expr) call`

### 1.4 Type Check Integration ✓

**Files**:
- `vo-analysis/src/check/sendable.rs` - sendability checking integrated

---

## Phase 2: Runtime Infrastructure

### 2.1 New Opcodes (0.5 days) ✓

**File**: `vo-common-core/src/bytecode.rs`

**Implemented**:
```rust
IslandNew,    // a=dst
QueueNew,     // a=dst, b=elem_meta, c=cap; QueueLayout owns element layout, bit7=port
QueueSend,    // a=queue/port, b=src; QueueLayout owns element layout
QueueRecv,    // a=dst, b=queue/port; QueueLayout owns layout, bit0=has_ok
QueueClose,   // a=queue/port
GoIsland,     // a=island, b=closure, c=args_start; CallLayout owns argument layout
```

**Also updated**:
- `vo-vm/src/vm/mod.rs` - placeholder handlers (panic with "requires Phase 3")
- `vo-vox/src/format.rs` - opcode formatting for dump
- `vo-runtime/src/gc_types.rs` - Port finalization

### 2.2 Pack/Unpack Core (2 days) ⭐ Critical Path ✓

**File**: `vo-runtime/src/pack.rs` (new)

**Implemented**:
```rust
pub struct PackedValue {
    data: Vec<u8>,
}

pub fn pack_slots(gc: &Gc, src: &[u64], value_meta: ValueMeta, struct_metas: &[StructMeta]) -> PackedValue
pub fn unpack_slots(gc: &mut Gc, packed: &PackedValue, dst: &mut [u64], struct_metas: &[StructMeta])
```

**Pack logic by type**:
- Scalars: type tag + 8 bytes
- String: length + bytes
- Slice/Array: length + elem_meta + elem_bytes + recursive elements
- Struct: meta_id + slot_count + recursive fields
- `*T`: null marker + (if non-null: obj_meta + slots + recursive data)
- Map: length + key_meta + val_meta + key_slots + val_slots + key_rttid + recursive entries

**Verification**: Unit tests pass ✓

### 2.3 Port Data Structure (1 day) ✓

**File**: `vo-runtime/src/objects/port.rs` (new)

**Implemented**:
```rust
// GC object layout
struct PortData {
    state: Slot,      // Arc<Mutex<PortState>>
    cap: Slot,
    elem_meta: ValueMeta,
    elem_slots: u16,
}

// Thread-safe inner state
struct PortState {
    buffer: VecDeque<PackedValue>,
    closed: bool,
    waiting_senders: VecDeque<(WaiterInfo, PackedValue)>,
    waiting_receivers: VecDeque<WaiterInfo>,
}

pub fn create(gc, elem_meta, elem_slots, cap) -> GcRef
pub fn try_send(port, value: PackedValue) -> SendResult
pub fn try_recv(port) -> (RecvResult, Option<PackedValue>)
pub fn close(port)
pub fn register_sender/register_receiver(port, waiter, ...)
```

**Verification**: Compiles, GC finalization handled ✓

---

## Phase 3: Multi-Island Core

### 3.1 Island Data Structure ✓

**File**: `vo-runtime/src/island.rs`

**Implemented**:
```rust
pub struct IslandData {      // GC object layout
    pub id: u32,
    pub command_tx: Slot,     // Sender<IslandCommand>
}

pub enum IslandCommand {
    SpawnFiber { closure_data: PackedValue, capture_slots: u16 },
    WakeFiber { fiber_id: u32 },
    Shutdown,
}

pub fn create(gc, island_id) -> IslandSpawnResult
pub fn create_main(gc) -> GcRef
pub fn send_command(island, cmd) -> Result
pub fn spawn_fiber_on(island, closure_data, capture_slots)
pub fn wake_fiber_on(island, fiber_id)
```

### 3.2 Codegen ✓

**Files**:
- `vo-codegen/src/type_info.rs` - `is_port()`, `is_island()`, `port_elem_type()`
- `vo-codegen/src/expr/builtin.rs` - `make(island)` → IslandNew, `make(port T)` → PortNew, `close(port)` → PortClose
- `vo-codegen/src/stmt/defer_go.rs` - `go @(i)` → GoIsland

**Implemented**:
```rust
// make(island)
func.emit_op(Opcode::IslandNew, dst, 0, 0);

// make(port T, cap)
func.emit_with_flags(Opcode::PortNew, elem_slots, dst, elem_meta_reg, cap_reg);

// close(port)
func.emit_op(Opcode::PortClose, arg_reg, 0, 0);

// go @(island) func() {}()
func.emit_with_flags(Opcode::GoIsland, capture_slots, island_reg, closure_reg, 0);
```

**Note**: Port send/recv codegen pending (requires expr analysis changes)

### 3.3 VM Opcodes ✓

**Files**:
- `vo-vm/src/exec/island.rs` - exec_island_new, exec_go_island
- `vo-vm/src/exec/port.rs` - exec_port_new, exec_port_send, exec_port_recv, exec_port_close

**Implemented**:
- `PortResult` / `IslandResult` enums for opcode results
- Pack/unpack integration for cross-island value transfer
- Waiter registration for blocking operations

**Note**: VM main loop integration pending (placeholder handlers still in place)

---

## Phase 4: Multi-threading Integration (2.5 days)

### 4.1 Island Thread Execution

**File**: `vo-vm/src/vm/mod.rs`

**Changes**:
- Each Island runs on dedicated thread
- Main thread = Island 0
- IslandCoordinator manages all Islands
- Module shared via Arc

### 4.2 Cross-Island Wake

**File**: `vo-runtime/src/island.rs`

```rust
fn wake_on_island(island_id: u32, fiber_id: u32) {
    // Send WakeFiber via command_tx
}
```

### 4.3 Lifecycle Management

- Island panic handling
- Normal island exit
- Main island waits for all islands

---

## Phase 5: Testing (2 days)

**Test files**:
```
test_data/island_basic.vo          # Create island
test_data/port_send_recv.vo        # Basic communication
test_data/port_deep_copy.vo        # Verify deep copy
test_data/island_capture.vo        # Variable capture
test_data/island_sendable_err.vo   # Compile error tests
test_data/island_stress.vo         # Stress test
```

---

## Design Decisions

### Port = Cross-thread Channel + Pack/Unpack

Port internally reuses channel logic with:
1. `Arc<Mutex<>>` for thread safety
2. Pack before send, unpack after recv
3. Cross-island fiber wake mechanism

### Separate Opcodes for Channel vs Port

Compile-time known types → no runtime dispatch overhead:
- `ChanSend/ChanRecv` for `chan T`
- `PortSend/PortRecv` for `port T`

### Deep Copy Semantics

All sendable types are deep-copied:
- `*T` → pointed object is copied, receiver gets new pointer
- `map[K]V` → entire map structure is copied
- No shared references across islands
