# Islands & Ports Implementation Plan

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

## Phase 1: Type System

### 1.1 Sendability Check (1.5 days)

**File**: `vo-analysis/src/check/sendable.rs` (new)

**Tasks**:
- `is_sendable(type_key, objs) -> bool` - recursive check
- Error message generation for non-sendable types

**Sendable Types**:
- Scalars: `bool`, integers, floats, `rune`
- `string`
- `[]T`, `[N]T` where T is sendable
- `struct` where all fields are sendable
- `*T` where T is sendable (deep-copied)
- `map[K]V` where K and V are sendable

**Not Sendable**:
- `chan[T]` - bound to island scheduler
- `port[T]` - bound to island scheduler
- `island` - represents VM instance
- `func` / closures - may capture island-local state
- `any` - cannot statically verify contents

**Verification**: Unit tests + compile error tests with .vo files

### 1.2 island/port Types (1 day)

**Files**:
- `vo-analysis/src/typ.rs` - `IslandDetail`, `PortDetail`
- `vo-analysis/src/objects.rs` - TypeKey extension
- `vo-analysis/src/check/typexpr.rs` - parse `port[T]`

**Verification**: Can parse `var p port[int]`

### 1.3 Parser Extension (0.5 days)

**Files**:
- `vo-syntax/src/ast.rs` - GoStmt add `target_island` field ✓
- `vo-syntax/src/parser/stmt.rs` - parse `go @(expr) call` ✓

**Verification**: Can parse `go @(i) func() {}()` ✓

**Note**: Changed syntax from `go(island)` to `go @(island)` to avoid ambiguity with `go (expr)()` (parenthesized call).

### 1.4 Type Check Integration (1 day)

**Files**:
- `vo-analysis/src/check/stmt.rs` - go @(i) type check + sendability
- `vo-analysis/src/check/builtin.rs` - make(island), make(port[T], cap)
- `vo-analysis/src/check/expr.rs` - port send/recv expressions

**Verification**: Compile error tests
- `go @(i)` capturing non-sendable variable → error
- `make(port[chan[int]])` → error

---

## Phase 2: Runtime Infrastructure

### 2.1 New Opcodes (0.5 days)

**File**: `vo-common-core/src/instruction.rs`

**New opcodes**:
```rust
IslandNew,    // a=dst
PortNew,      // a=dst, b=elem_meta, c=cap, flags=elem_slots
PortSend,     // a=port, b=src, flags=elem_slots
PortRecv,     // a=dst, b=port, flags=(elem_slots<<1)|has_ok
PortClose,    // a=port
GoIsland,     // a=island, b=closure, flags=capture_slots
```

### 2.2 Pack/Unpack Core (2 days) ⭐ Critical Path

**File**: `vo-runtime/src/pack.rs` (new)

**Tasks**:
```rust
struct PackedValue {
    data: Vec<u8>,
    // Type info embedded in data
}

fn pack_slots(gc: &Gc, src: &[u64], type_meta: TypeMeta) -> PackedValue
fn unpack_slots(gc: &mut Gc, packed: &PackedValue, dst: &mut [u64])
```

**Pack logic by type**:
- Basic types: direct copy
- String: copy bytes
- Slice/Array: recursively pack elements
- Struct: recursively pack fields
- `*T`: pack pointed object
- Map: iterate and pack entries

**Verification**: Unit tests - pack → unpack yields same value, different GcRefs

### 2.3 Port Data Structure (1 day)

**File**: `vo-runtime/src/objects/port.rs` (new)

**Reuse channel.rs structure**:
```rust
struct PortState {
    inner: Arc<Mutex<ChannelState>>,  // Thread-safe wrapper
    elem_meta: ValueMeta,
    elem_slots: u16,
}

fn create(gc, elem_meta, cap) -> GcRef
fn send(port, packed, sender_info) -> SendResult
fn recv(port) -> (RecvResult, Option<PackedValue>)
fn close(port)
```

**Verification**: Single-thread port test (same island)

---

## Phase 3: Multi-Island Core

### 3.1 Island Data Structure (1.5 days)

**File**: `vo-runtime/src/island.rs` (new)

```rust
struct Island {
    id: u32,
    gc: Gc,
    module: Arc<Module>,  // Shared read-only
}

struct IslandHandle {
    id: u32,
    command_tx: Sender<IslandCommand>,
}

enum IslandCommand {
    SpawnFiber(PackedClosure),
    WakeFiber(u32),
    Shutdown,
}
```

### 3.2 Codegen (1.5 days)

**Files**:
- `vo-codegen/src/expr/make.rs` - make(island), make(port[T], cap)
- `vo-codegen/src/stmt/go.rs` - go @(i) generates GoIsland
- `vo-codegen/src/stmt/send.rs` - port send/recv generates PortSend/PortRecv

**Verification**: Correct bytecode generation (dump check)

### 3.3 VM Opcodes (1.5 days)

**Files**:
- `vo-vm/src/exec/island.rs` (new) - exec_island_new, exec_go_island
- `vo-vm/src/exec/port.rs` (new) - exec_port_send, exec_port_recv, exec_port_close
- `vo-vm/src/vm/mod.rs` - main loop cases

**Verification**: Single island .vo file execution

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
- `ChanSend/ChanRecv` for `chan[T]`
- `PortSend/PortRecv` for `port[T]`

### Deep Copy Semantics

All sendable types are deep-copied:
- `*T` → pointed object is copied, receiver gets new pointer
- `map[K]V` → entire map structure is copied
- No shared references across islands
