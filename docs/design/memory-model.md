# Vo Memory Model Design

## 1. Overview

Vo adopts a Go-like memory model: value types are stack-allocated by default, with static escape analysis determining heap allocation.

## 2. Type Classification

| Type | Semantics | Default Allocation | Escape Analysis |
|------|-----------|-------------------|----------------|
| Primitives (int, float, bool, string) | Value | Stack | Participates |
| struct | Value | Stack | Participates (heap if > threshold) |
| array | Value | Stack | Participates (heap if > threshold) |
| interface | Value (2 slots) | Stack | — |
| closure | Reference | Heap | No |
| *StructType | Reference | — | — |
| slice, map, chan | Reference | Heap | — |

### Size Threshold

**Threshold**: 256 slots (2KB)

struct/array exceeding the threshold are heap-allocated directly without escape analysis. This prevents unexpected stack overflow.

## 3. Escape Analysis

### Input
- Type-checked AST
- Type information for each variable

### Output
Each local variable marked with `escapes: bool`

### Rules

**Primitive escape conditions**:
1. Captured by closure

**struct escape conditions**:
1. Address taken `&s`
2. Captured by closure
3. Assigned to interface variable

**array escape conditions**:
1. Captured by closure
2. Assigned to interface variable
3. Sliced via `arr[:]` or `arr[i:j]`

### Transitivity

Nested struct/array fields of an escaping variable also escape (allocated on heap as part of parent):

```vo
type Inner struct { x int }
type Outer struct { 
    inner Inner 
    arr [3]int
}

var o Outer
&o  // o escapes → o.inner and o.arr are on heap
```

### Scope

- Function-local variables
- No cross-function analysis (simplified: address-taken = escape, no pointer flow tracking)

## 4. GcObject Format

### GcHeader (8 bytes)

```rust
#[repr(C)]
pub struct GcHeader {
    pub mark: u8,           // GcColor: White/Gray/Black
    pub gen: u8,            // GcGen: Young/Old/Touched
    pub value_kind: u8,     // ValueKind
    pub flags: u8,          // Reserved
    pub type_id: u16,       // RuntimeTypeId
    pub slots: u16,         // Data slot count
}
```

### ValueKind

```rust
#[repr(u8)]
pub enum ValueKind {
    // Stack values (never in GcObject)
    Nil = 0, Bool = 1, Int = 2, Float = 3,

    // Heap objects
    String = 10,    Slice = 11,     Array = 12,
    Map = 13,       Channel = 14,   Closure = 15,
    Struct = 16,

    // Boxed primitives (when escaped)
    BoxedInt = 20,  BoxedFloat = 21, BoxedBool = 22,
}
```

### Object Layouts

| Type | Layout | slots |
|------|--------|-------|
| BoxedInt/Float/Bool | `[value]` | 1 |
| Struct | `[field0, field1, ...]` | N |
| Array | `[elem_kind, elem_type_id, elem_bytes, len, data...]` | 1 + data |
| String | `[array_ref, start, len]` | 3 |
| Slice | `[array_ref, start, len, cap]` | 4 |
| Closure | `[func_id, count, upvals...]` | 2 + count |

## 5. Instruction Design

### Stack Operations (non-escaping)

struct/array expanded to consecutive registers:

```asm
# Direct register operations
MOV           r1, 42               # s.field1 = 42 / arr[1] = 42
MOV_N         d, s, n              # Copy n slots (assignment)
```

### Heap Operations (escaping)

```asm
# Allocation
ALLOC         d, type_id, slots    # Allocate heap object

# Struct fields
GET_FIELD     d, obj, idx          # d = obj.fields[idx]
SET_FIELD     obj, idx, s          # obj.fields[idx] = s

# Array elements
ARRAY_GET     d, arr, idx          # d = arr[idx]
ARRAY_SET     arr, idx, s          # arr[idx] = s
ARRAY_GET_N   d, arr, idx, n       # Read n slots (struct elements)
ARRAY_SET_N   arr, idx, s, n       # Write n slots
```

### Unified Logic

Stack/heap fully distinguished at compile time. Codegen emits different instructions based on escape analysis. No runtime checks.

## 6. Interface

Interface is a value type (2 slots: `type_info + data`):
- interface variable lives on stack
- `data` slot may hold:
  - Small value inline (primitives)
  - Pointer to heap object (struct/array)

**This is why struct/array assigned to interface must escape** — the held value needs to be on heap.

## 7. Simplification Decisions

| Decision | Rationale |
|----------|-----------|
| Address-taken = escape | Only struct supports `&`, simplifies analysis |
| Closure always heap | Complex to analyze, low benefit |
| Large struct/array → heap | Prevents unexpected stack overflow |
| Only `*StructType` | Simplified pointer syntax |
| Unified ALLOC | Boxed primitives also use ALLOC |
| Different instructions for stack/heap | Compile-time decision, no runtime checks |

## 8. Comparison with Go

| | Go | Vo |
|---|---|---|
| Pointer syntax | `*T` for any type | Only `*StructType` |
| Escape analysis | Static, precise | Static, simplified |
| Closure | On-demand allocation | Always heap |
| Large objects | Full escape analysis | Direct heap allocation |
| Stack | Growable | Fixed size |
