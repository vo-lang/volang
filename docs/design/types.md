# Vo Type System

This document defines the memory layout of all runtime data structures in Vo.

> **Memory Model**: See `docs/design/memory-model.md` for escape analysis and allocation decisions.
> **GC Scanning**: See `docs/design/gc-object-scanning.md` for detailed GC scanning rules.

## 1. Memory Model

Vo uses escape analysis to determine stack vs heap allocation for value types.

```
┌─────────────────────────────────────────────────────────────────┐
│                         Stack                                    │
│  ┌───────┬───────┬───────┬───────┬───────┬───────┬───────┐      │
│  │ slot0 │ slot1 │ slot2 │ slot3 │ slot4 │ slot5 │ ...   │      │
│  │ i64   │GcRef  │struct.│struct.│ bool  │GcRef  │       │      │
│  │       │  ↓    │field0 │field1 │       │  ↓    │       │      │
│  └───────┴───┬───┴───────┴───────┴───────┴───┬───┴───────┘      │
└──────────────│───────────────────────────────│──────────────────┘
               │  (non-escaping struct         │
               │   stored inline on stack)     ↓
               ↓                              Heap
              Heap (escaping values)           
┌─────────────────────────────────────────────────────────────────┐
│                          Heap                                    │
│  ┌─────────────────┐    ┌─────────────────┐                     │
│  │   GcObject      │    │   GcObject      │                     │
│  │  ┌───────────┐  │    │  ┌───────────┐  │                     │
│  │  │ GcHeader  │  │    │  │ GcHeader  │  │                     │
│  │  ├───────────┤  │    │  ├───────────┤  │                     │
│  │  │   data    │  │    │  │   data    │  │                     │
│  │  └───────────┘  │    │  └───────────┘  │                     │
│  └─────────────────┘    └─────────────────┘                     │
└─────────────────────────────────────────────────────────────────┘
```

### Allocation Rules

| Type | Default | Escapes → |
|------|---------|----------|
| Primitives | Stack | Heap (BoxedInt/Float/Bool) |
| struct | Stack (inline slots) | Heap (GcRef) |
| array | Stack (inline slots) | Heap (GcRef) |
| interface | Stack (2 slots) | — |
| slice, map, chan, closure | Heap | — |

**Size threshold**: struct/array > 256 slots → always heap.

## 2. Fundamental Units

### Slot
Basic storage unit: **8 bytes = 64 bits**

### GcRef
Pointer to heap-allocated GcObject: `type GcRef = *mut GcObject`

### GcHeader
```rust
#[repr(C)]
struct GcHeader {
    mark: u8,        // GcColor: White/Gray/Black
    gen: u8,         // GcGen: Young/Old/Touched
    value_kind: u8,  // ValueKind
    flags: u8,       // Reserved
    type_id: u16,    // RuntimeTypeId (for Struct)
    slots: u16,      // Data slot count
}
// Total: 8 bytes
```

## 3. ValueKind and TypeId

### ValueKind (u8)

Distinguishes object types for GC scanning:

```rust
#[repr(u8)]
pub enum ValueKind {
    // Stack-only (never in GcObject)
    Nil = 0, Bool = 1, Int = 2, Float = 3,

    // Heap objects
    String = 10,    Slice = 11,     Array = 12,
    Map = 13,       Channel = 14,   Closure = 15,
    Struct = 16,

    // Boxed primitives (escaped)
    BoxedInt = 20,  BoxedFloat = 21, BoxedBool = 22,
}
```

### RuntimeTypeId (u16)

Index into metadata tables:
- `struct_metas[type_id]` for Struct
- `interface_metas[type_id]` for Interface

Two separate tables, both indexed from 0.

## 4. Primitive Types

Stored directly in slots, **not heap-allocated**:

| Type | Slots | Storage |
|------|-------|---------|
| `bool` | 1 | 0 or 1 |
| `int8/16/32/64` | 1 | Sign-extended to 64-bit |
| `uint8/16/32/64` | 1 | Zero-extended to 64-bit |
| `float32` | 1 | Lower 32 bits |
| `float64` | 1 | Full 64 bits |

## 5. String (type_id = 14)

```
Layout: [array_ref, start, len]
Slots:  3

slot 0: array_ref (GcRef → underlying byte array)
slot 1: start     (byte offset)
slot 2: len       (string length)
```

Substrings share the underlying byte array.

## 6. Array (type_id = 20)

```
Layout: [elem_type, elem_bytes, len, data...]
Slots:  3 + ceil(len * elem_bytes / 8)

slot 0: elem_type  (element TypeId)
slot 1: elem_bytes (bytes per element: 1/2/4/8 or 8*n for structs)
slot 2: len        (number of elements)
slot 3+: data      (packed or multi-slot storage)
```

**Packed storage** (elem_bytes ≤ 8):
```
int8 array [a, b, c, d, e, f, g, h]:
┌───┬───┬───┬───┬───┬───┬───┬───┐
│ a │ b │ c │ d │ e │ f │ g │ h │  ← 8 elements in 1 slot
└───┴───┴───┴───┴───┴───┴───┴───┘
```

**Multi-slot storage** (elem_bytes > 8, for structs):
```
Point{x, y int} array:
┌─────────┬─────────┬─────────┬─────────┬─────────┬─────────┐
│  p0.x   │  p0.y   │  p1.x   │  p1.y   │  p2.x   │  p2.y   │
└─────────┴─────────┴─────────┴─────────┴─────────┴─────────┘
  slot 3    slot 4    slot 5    slot 6    slot 7    slot 8
```

## 7. Slice (type_id = 15)

```
Layout: [array_ref, start, len, cap]
Slots:  4

slot 0: array_ref (GcRef → underlying Array)
slot 1: start     (start index in array)
slot 2: len       (slice length)
slot 3: cap       (capacity)
```

## 8. Map (type_id = 16)

```
Layout: [map_ptr, key_type, val_type]
Slots:  3

slot 0: map_ptr   (Box<IndexMap<u64, u64>> pointer)
slot 1: key_type  (key TypeId)
slot 2: val_type  (value TypeId)
```

**Note**: Keys must be comparable types (no slices, maps, funcs).

## 9. Channel (type_id = 21)

```
Layout: [chan_ptr, elem_type, cap]
Slots:  3

slot 0: chan_ptr  (Box<ChannelState> pointer)
slot 1: elem_type (element TypeId)
slot 2: cap       (buffer capacity, 0 = unbuffered)
```

```rust
struct ChannelState {
    buffer: VecDeque<u64>,
    closed: bool,
    waiting_senders: VecDeque<(GoId, u64)>,
    waiting_receivers: VecDeque<GoId>,
}
```

## 10. Closure (type_id = 22)

```
Layout: [func_id, upval_count, upval0, upval1, ...]
Slots:  2 + upval_count

slot 0: func_id      (function ID)
slot 1: upval_count  (number of captured variables)
slot 2+: upvalues    (captured variable values)
```

## 11. Struct

User-defined value types. Allocation determined by escape analysis.

### Stack Allocation (non-escaping)

Struct fields stored as consecutive stack slots:
```
var s Point  // Point{x, y int} - 2 slots

Stack:
┌─────────┬─────────┐
│   s.x   │   s.y   │
│  slot N │ slot N+1│
└─────────┴─────────┘
```

Field access: direct register offset (no GcRef indirection).

### Heap Allocation (escaping)

Struct stored as GcObject, stack holds GcRef:
```
var s Point  // escapes

Stack:          Heap:
┌─────────┐     ┌──────────────────┐
│  GcRef ─┼────→│ GcHeader         │
│  slot N │     ├──────────────────┤
└─────────┘     │ s.x   │   s.y    │
                └──────────────────┘
```

### Layout (heap)
```
Layout: [field0, field1, ...]
Slots:  sum of field slots
```

### Compact Field Layout

Small fields are packed within slots:

```go
type Packed struct {
    a int8   // 1 byte
    b int16  // 2 bytes  
    c int32  // 4 bytes
    d int8   // 1 byte
}
// Total: 8 bytes = 1 slot
```

```
slot 0:
┌───┬─────┬─────────┬───┐
│ a │  b  │    c    │ d │
│1B │ 2B  │   4B    │1B │
└───┴─────┴─────────┴───┘
```

### Alignment Rules

| Type | Alignment |
|------|-----------|
| int8/uint8/bool | 1 byte |
| int16/uint16 | 2 bytes |
| int32/uint32/float32 | 4 bytes |
| int64/uint64/float64/*T/string/[]T/map/chan/interface | 8 bytes |

### ptr_bitmap

Indicates which slots contain GcRef for GC scanning:

```go
type Person struct {
    name   string   // GcRef → true
    age    int      // value  → false
    friend *Person  // GcRef → true
}
// ptr_bitmap = [true, false, true]
```

## 12. Interface (inline, 2 slots)

**Not a heap object** - stored inline in stack/struct.

```
Current Layout: [value_type, data]
Future Layout:  [packed_types, data]
                packed_types = (iface_type << 32) | value_type
```

| Actual Type | data slot contains |
|-------------|--------------------|
| int, float, bool | Value directly |
| string, *T, []T, map, chan | GcRef |
| struct (value type) | GcRef → heap copy |

### Interface in Struct

```go
type Container struct {
    data interface{}
}
// ptr_bitmap = [false, true]  ← slot 1 conservatively marked
```

## 13. Boxed Primitives

When primitives escape (e.g., captured by closure), they are boxed:

```
Layout: [value]
Slots:  1

ValueKind: BoxedInt (20), BoxedFloat (21), BoxedBool (22)
```

GC scanning: `needs_gc(BoxedInt) = false` (no internal references).

## 14. Summary

| Type | Stack (non-escaping) | Stack (escaping) | Heap Layout | Slots |
|------|---------------------|------------------|-------------|-------|
| Primitives | value | GcRef (boxed) | [value] | 1 |
| String | GcRef | GcRef | [array_ref, start, len] | 3 |
| Slice | GcRef | GcRef | [array_ref, start, len, cap] | 4 |
| Array | inline slots | GcRef | [elem_kind, elem_type_id, elem_bytes, len, data...] | 4+ |
| Map | GcRef | GcRef | [map_ptr, key_type, val_type] | 3 |
| Channel | GcRef | GcRef | [chan_ptr, elem_type, cap] | 3 |
| Closure | GcRef | GcRef | [func_id, count, upvals...] | 2+ |
| Struct | inline slots | GcRef | [fields...] | N |
| Interface | inline (2 slots) | inline (2 slots) | — | 2 |
