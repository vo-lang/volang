# GC Object Scanning Design

## Overview

GC traverses all reachable objects and marks internal references. This document describes a unified `scan_object` function shared by VM and JIT.

## Core Principle

- **Only user-defined structs require slot_types lookup**
- **All built-in types have fixed internal layouts**
- **Use ValueKind to determine scan method, RuntimeTypeId to lookup struct metadata**

## Type Classification

### needs_gc

```rust
/// Check if a ValueKind needs GC scanning.
pub fn needs_gc(value_kind: ValueKind) -> bool {
    matches!(value_kind,
        ValueKind::String | ValueKind::Slice | ValueKind::Map |
        ValueKind::Array | ValueKind::Channel | ValueKind::Closure |
        ValueKind::Struct | ValueKind::Pointer)
}
```

### Complete Type List

| ValueKind | Category | GcObject? | Scan Method |
|-----------|----------|-----------|-------------|
| Nil | value | ❌ | skip |
| Bool | value | ❌ | skip |
| Int* | value | ❌ | skip |
| Uint* | value | ❌ | skip |
| Float* | value | ❌ | skip |
| **String** | inline | ✅ | mark slot 0 |
| **Slice** | inline | ✅ | mark slot 0 |
| **Map** | inline | ✅ | iterate values |
| **Pointer** | pointer | ❌ value is ptr | mark directly |
| Interface | conditional | ❌ N/A | 2 slots, see below |
| **Array** | inline | ✅ | iterate elements |
| **Channel** | inline | ✅ | iterate buffer |
| **Closure** | inline | ✅ | mark upvalues |
| **Struct** | inline | ✅ | use slot_types from struct_metas[type_id] |

### Types NOT appearing as GcObject.header.value_kind

| ValueKind | Reason |
|-----------|--------|
| Pointer | Value itself is GcRef, pointed object has its own value_kind |
| Interface | Stored as 2 slots `[packed_types, data]`, not allocated separately |

## Built-in Type Layouts

### String
```
Layout: [array_ref, start, len]
Scan:   mark slot 0 (array_ref)
```

### Slice
```
Layout: [array_ref, start, len, cap]
Scan:   mark slot 0 (array_ref)
```

### Map
```
Layout:  [map_ptr, key_kind, val_kind, key_type_id, val_type_id]
Storage: IndexMap<u64, u64>
Scan:    if needs_gc(val_kind):
           for (_, val) in entries:
             mark val
Note:    keys must be comparable types, no GC refs
```

### Array
```
Layout: [elem_kind, elem_type_id, elem_bytes, len, data...]
Scan:
  if elem_kind == Struct:
    # elements stored inline, scan each with slot_types
    let slot_types = struct_metas[elem_type_id].slot_types
    for i in 0..len:
      scan_with_slot_types(obj, slot_types, base_offset)
  elif needs_gc(elem_kind):
    # builtin ref type, each element is a pointer
    for i in 0..len:
      mark data[i]
```

### Channel
```
Layout:  [chan_ptr, elem_kind, elem_type_id, cap]
Storage: ChannelState { buffer: VecDeque<u64>, waiting_senders: VecDeque<(GoId, u64)> }
Scan:    if needs_gc(elem_kind):
           for val in buffer: mark val
           for (_, val) in waiting_senders: mark val
```

### Closure
```
Layout: [func_id, count, upval0, upval1, ...]
Scan:   for i in 0..count: mark upval[i]
Note:   upvalues may contain GC refs
```

## User-Defined Struct

Structs use slot_types from `struct_metas[type_id]`.

```go
type Person struct {
    name   string      // GC ref
    age    int         // value
    friend *Person     // GC ref
}
// slot_types = [GcRef, Value, GcRef]
```

### Interface (value type, not GcObject)

```
Layout: [packed_types, data]  ← 2 slots
  slot 0: packed type info (64 bits)
    ┌─────────────────────────────────────────────────────────────┐
    │ iface_type_id (16) │ value_type_id (16) │ value_kind (8) │...│
    └─────────────────────────────────────────────────────────────┘
  slot 1: data (value or GcRef, depends on value_kind)

Read:
  let value_kind = (slot0 >> 32) as u8;  // extract value_kind
  let value_type_id = (slot0 >> 16) as u16;

GC Scan (dynamic check):
  let value_kind = extract_value_kind(slot0);
  if needs_gc(value_kind): mark slot1
```

**Key point**: Whether interface slot 1 needs scanning depends on runtime value_kind, cannot be determined statically.

### Interface Field in Struct

```go
type Container struct {
    data interface{}   // 2 slots: [packed_types, data]
}
// slot_types needs special markers for interface slots
```

For structs containing interface fields, dynamic check is required during scanning:
```rust
fn scan_struct_with_interface(gc: &mut Gc, obj: GcRef, slot_types: &[SlotType]) {
    for (i, slot_type) in slot_types.iter().enumerate() {
        match slot_type {
            SlotType::Value => { /* skip */ }
            SlotType::GcRef => {
                let val = Gc::read_slot(obj, i);
                if val != 0 { gc.mark_gray(val as GcRef); }
            }
            SlotType::Interface0 => { /* skip, this is packed type info */ }
            SlotType::Interface1 => {
                // Dynamic check: extract value_kind from previous slot
                let packed = Gc::read_slot(obj, i - 1);
                let value_kind = extract_value_kind(packed);
                if needs_gc(value_kind) {
                    let val = Gc::read_slot(obj, i);
                    if val != 0 { gc.mark_gray(val as GcRef); }
                }
            }
        }
    }
}
```

## SlotType (defined in gox-common-core)

Since interface requires dynamic checking, struct slot_types use `SlotType` instead of simple `bool`.
`SlotType` is defined in `gox-common-core/src/types.rs`, used for both stack and heap scanning:

```rust
// gox-common-core/src/types.rs
#[repr(u8)]
pub enum SlotType {
    Value = 0,       // non-pointer, skip
    GcRef = 1,       // GC pointer, must scan
    Interface0 = 2,  // interface slot 0 (type_id), skip
    Interface1 = 3,  // interface slot 1, dynamic check required
}
```

**Unified design**: Stack and heap scanning use the same `SlotType` enum.

### Generation Rules

| Field Type | Slots | SlotType |
|------------|-------|----------|
| `int`, `float`, `bool` | 1 | `[Value]` |
| `string`, `*T`, `[]T`, `map`, `chan`, `func` | 1 | `[GcRef]` |
| `interface{}` | 2 | `[Interface0, Interface1]` |
| `MyStruct` (embedded) | N | recursive |

## Static Type Tables

```rust
use once_cell::sync::OnceCell;

/// Struct slot types, indexed directly by type_id (starts from 0)
static STRUCT_SLOT_TYPES: OnceCell<Box<[Box<[SlotType]>]>> = OnceCell::new();

pub fn init_struct_slot_types(types: Vec<Vec<SlotType>>) { ... }

/// Get slot_types for a struct. type_id is direct index into struct_metas.
pub fn get_struct_slot_types(type_id: u16) -> Option<&'static [SlotType]> {
    STRUCT_SLOT_TYPES.get()?.get(type_id as usize).map(|b| b.as_ref())
}
```

## Unified scan_object

```rust
pub fn scan_object(gc: &mut Gc, obj: GcRef) {
    let header = unsafe { &(*obj).header };
    let value_kind = ValueKind::from_u8(header.value_kind);
    let type_id = header.type_id;
    
    match value_kind {
        // User-defined struct: use slot_types from struct_metas
        ValueKind::Struct => {
            if let Some(slot_types) = get_struct_slot_types(type_id) {
                scan_with_slot_types(gc, obj, slot_types, 0);
            }
        }
        // Built-in types: fixed layouts
        ValueKind::String | ValueKind::Slice => {
            let val = Gc::read_slot(obj, 0);
            if val != 0 { gc.mark_gray(val as GcRef); }
        }
        ValueKind::Array => scan_array(gc, obj),
        ValueKind::Map => scan_map(gc, obj),
        ValueKind::Channel => scan_channel(gc, obj),
        ValueKind::Closure => scan_closure(gc, obj),
        _ => {}
    }
}

/// Scan slots using SlotType array (handles interface dynamically)
fn scan_with_slot_types(gc: &mut Gc, obj: GcRef, slot_types: &[SlotType], base: usize) {
    for (i, &slot_type) in slot_types.iter().enumerate() {
        match slot_type {
            SlotType::Value | SlotType::Interface0 => { /* skip */ }
            SlotType::GcRef => {
                let val = Gc::read_slot(obj, base + i);
                if val != 0 { gc.mark_gray(val as GcRef); }
            }
            SlotType::Interface1 => {
                // Dynamic check: extract value_kind from previous slot
                let packed = Gc::read_slot(obj, base + i - 1);
                let value_kind = extract_value_kind(packed);
                if needs_gc(value_kind) {
                    let val = Gc::read_slot(obj, base + i);
                    if val != 0 { gc.mark_gray(val as GcRef); }
                }
            }
        }
    }
}
```

## Data Flow

```
Compile Time (codegen)
    │
    ▼
For each struct definition:
  - Analyze field types
  - Generate slot_types (interface → [Interface0, Interface1])
  - Store in bytecode.struct_metas[type_id].slot_types
    │
    ▼
Load Time (VM/JIT)
    │
    ▼
init_struct_slot_types(bytecode.struct_metas.map(|m| m.slot_types))
    │
    ▼
GC Time
    │
    ▼
gc.collect(|gc, obj| gc_types::scan_object(gc, obj))
```

## Implementation Location

- `gox-common-core/src/types.rs`: `ValueKind`, `SlotType` enum definitions
- `gox-runtime-core/src/gc_types.rs`: `scan_object`, `STRUCT_SLOT_TYPES`
- `gox-common-core/src/types.rs`: `needs_gc(ValueKind)`
- `gox-codegen-vm/src/types.rs`: slot_types generation

## Properties

- **Unified**: Stack and heap scanning use the same `SlotType` enum
- **Precise**: Interface scanned precisely via dynamic value_kind check
- **Lock-free**: Static data, read-only after initialization
- **No false positives**: Never incorrectly marks non-pointer values
- **Direct indexing**: type_id directly indexes struct_metas (no offset subtraction)
