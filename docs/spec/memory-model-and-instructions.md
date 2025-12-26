# Vo Memory Model and Instruction Set Specification

**Version**: 2.0 (Memory Model Refactoring)  
**Status**: Draft  
**Date**: 2025-12-23

## Table of Contents

1. [Overview](#1-overview)
2. [Type System](#2-type-system)
3. [Memory Model](#3-memory-model)
4. [Escape Analysis](#4-escape-analysis)
5. [Assignment Semantics](#5-assignment-semantics)
6. [Instruction Set](#6-instruction-set)
7. [Runtime Structures](#7-runtime-structures)

---

## 1. Overview

This document specifies the Vo memory model and instruction set architecture. The key design principle is **escape-analysis-driven allocation**: values are allocated on the stack by default, and only escape to the heap when necessary.

### 1.1 Design Goals

1. **Value semantics by default**: `struct`, `array`, and `interface` are value types
2. **Static escape analysis**: Determine allocation location at compile time
3. **Unified pointer representation**: All escaped values are represented as `Pointer`
4. **Simplified closure capture**: No delayed "upvalue promotion" - escaped variables are heap-allocated from the start

---

## 2. Type System

### 2.1 ValueKind

`ValueKind` is a runtime type tag (u8) that classifies all values:

```rust
pub enum ValueKind {
    // === Primitive Types (1 slot, no GC) ===
    Void = 0,  // No type (distinct from semantic nil like nil pointer/slice/map)
    Bool = 1,
    Int = 2,
    Int8 = 3,
    Int16 = 4,
    Int32 = 5,
    Int64 = 6,
    Uint = 7,
    Uint8 = 8,
    Uint16 = 9,
    Uint32 = 10,
    Uint64 = 11,
    Float32 = 12,
    Float64 = 13,
    FuncPtr = 14,       // Bare function pointer (no captures)

    // === Compound Value Types (multi-slot, may contain GC refs) ===
    Array = 16,         // [N]T - elements inline
    Struct = 21,        // Fields inline
    Interface = 23,     // 2 slots: header + data

    // === Reference Types (1 slot GcRef, heap allocated) ===
    String = 15,
    Slice = 17,
    Map = 18,
    Channel = 19,
    Closure = 20,
    Pointer = 22,       // *T - explicit pointer type (reference semantics)
}
```

### 2.2 Type Classification

#### 2.2.1 Value Types

Value types can be allocated on the stack. They are copied by value.

| Type | Slots | Description |
|------|-------|-------------|
| Primitives | 1 | `int`, `float`, `bool`, etc. |
| `[N]T` | N × sizeof(T) | Fixed-size array, elements inline |
| `struct` | Sum of fields | Fields inline, nested structs flattened |
| `interface` | 2 | Header slot + data slot |

#### 2.2.2 Reference Types

Reference types are always represented as a single GcRef slot pointing to a heap object.

| Type | Description |
|------|-------------|
| `string` | Immutable byte sequence |
| `[]T` | Slice (ptr, len, cap) |
| `map[K]V` | Hash map |
| `chan T` | Channel |
| `func(...)` | Closure with captures |
| `*T` | Explicit pointer (reference semantics) |

#### 2.2.3 Boxed Types

**Boxed** refers to the physical state: heap-allocated with a GcHeader. Both escaped struct/array and pointer (*T) are boxed.

| Physical State | Semantic | Assignment |
|----------------|----------|------------|
| Stack struct/array | Value | `CopyN` (copy slots) |
| Escaped struct/array (boxed) | Value | `PtrClone` (deep copy) |
| Pointer *T (boxed) | Reference | `Copy` (copy pointer only) |

**Key distinction**: Escaped struct and Pointer are both boxed (GcRef physically), but differ in semantics:
- **Escaped struct/array**: value semantics → assignment deep copies
- **Pointer (*T)**: reference semantics → assignment copies pointer only

### 2.3 Helper Methods

```rust
impl ValueKind {
    /// Returns true if this is a value type (can be stack-allocated)
    #[inline]
    pub fn is_value_type(self) -> bool {
        !self.is_ref_type()
    }
    
    /// Returns true if this is a reference type (always heap-allocated)
    #[inline]
    pub fn is_ref_type(self) -> bool {
        matches!(self, 
            Self::String | Self::Slice | Self::Map | 
            Self::Channel | Self::Closure | Self::Pointer)
    }
    
    /// Returns true if values of this type may contain GC references
    #[inline]
    pub fn may_contain_gc_refs(self) -> bool {
        matches!(self,
            Self::Array | Self::Struct | Self::Interface |
            Self::String | Self::Slice | Self::Map |
            Self::Channel | Self::Closure | Self::Pointer)
    }
}
```

---

## 3. Memory Model

### 3.1 Stack Layout

Each function has a set of **slots** (registers). Each slot is 8 bytes.

```
Function Frame:
┌─────────────────────────────┐
│ slot 0: param 0             │
│ slot 1: param 1             │
│ ...                         │
│ slot N: local var           │
│ slot N+1: local var         │
│ ...                         │
│ slot M: temp                │
└─────────────────────────────┘
```

#### 3.1.1 Value Type Layout on Stack

**Primitives**: 1 slot
```
var x int    // slot 0: value
```

**Struct** (non-escaping, flattened):
```vo
type Point struct { x, y int }
var p Point  // slot 0: p.x, slot 1: p.y
```

**Nested Struct** (flattened):
```vo
type Inner struct { a int; b string }
type Outer struct { inner Inner; c int }
var o Outer
// slot 0: o.inner.a (Value)
// slot 1: o.inner.b (GcRef - string is reference type)
// slot 2: o.c (Value)
```

**Array** (non-escaping):
```vo
var arr [3]int  // slot 0: arr[0], slot 1: arr[1], slot 2: arr[2]
```

**Interface**: 2 slots
```vo
var i interface{}
// slot 0: [itab_id:32 | named_type_id:24 | value_kind:8]
// slot 1: data (immediate value or GcRef)
//
// itab_id: index into VM's itab table (for method dispatch)
// named_type_id: index into named_type_metas[] (for type assertion)
// value_kind: concrete type's ValueKind (for nil check)
//
// nil check (same as Go):
//   i == nil  ⟺  value_kind == Void
//   Note: value_kind != Void but data == 0 means typed nil (e.g. (*T)(nil)), NOT nil interface
```

### 3.2 Heap Layout

Escaped values are allocated on the heap with a GC header.

#### 3.2.1 ValueMeta

```rust
/// Value metadata - 4 bytes
/// Layout: [meta_id:24 | value_kind:8]
pub struct ValueMeta(u32);

impl ValueMeta {
    pub fn new(meta_id: u32, value_kind: ValueKind) -> Self {
        Self((meta_id << 8) | (value_kind as u32))
    }

    pub fn from_raw(raw: u32) -> Self {
        Self(raw)
    }

    pub fn to_raw(self) -> u32 {
        self.0
    }

    pub fn value_kind(self) -> ValueKind {
        ValueKind::from(self.0 as u8)
    }

    pub fn meta_id(self) -> u32 {
        self.0 >> 8
    }
}

// meta_id meaning varies by value_kind:
// - Struct, Pointer: struct_metas[] index
// - Interface: interface_metas[] index
// - Array, Slice, Channel, Map: 0 (type info stored in object header/data)
// - Others: 0
```

#### 3.2.2 GcHeader

```rust
/// GC Object Header - 8 bytes
/// Layout: [mark:8 | gen:8 | slots:16 | ValueMeta:32]
pub struct GcHeader {
    pub mark: u8,           // GC mark bit (White/Gray/Black)
    pub gen: u8,            // Generation (Young/Old)
    pub slots: u16,         // Number of data slots
    pub value_meta: ValueMeta,
}
```

#### 3.2.3 Heap Object Layout

**Escaped Primitive** (BoxedInt, BoxedFloat, etc.):
```
┌─────────────────────────────┐
│ GcHeader (value_kind=Int)   │
├─────────────────────────────┤
│ value (8 bytes)             │
└─────────────────────────────┘
```

**Escaped Struct** (fields flattened, same as stack layout):
```
┌─────────────────────────────┐
│ GcHeader (value_kind=Struct)│
├─────────────────────────────┤
│ field 0 (8 bytes)           │
│ field 1 (8 bytes)           │
│ ...                         │
└─────────────────────────────┘

// Nested structs are flattened inline, not separate heap objects.
// Only reference-type fields (string/slice/map/chan/closure/pointer) are GcRef.
// Therefore, PtrClone (memcpy all slots) achieves correct value semantics:
// - Primitive/nested-struct fields: copied by value
// - Reference-type fields: shared (reference semantics, as expected)
```

**Escaped Array**:
```
┌─────────────────────────────────────────────────┐
│ GcHeader (value_kind=Array, meta_id=0)    │
├─────────────────────────────────────────────────┤
│ Array header: len (usize), elem_meta (u32)│  2 slots
├─────────────────────────────────────────────────┤
│ element 0                                 │
│ element 1                                 │
│ ...                                       │
└─────────────────────────────────────────────────┘

// GcHeader.value_kind = ValueKind::Array
// GcHeader.meta_id = 0 (Array doesn't need meta_id, elem info in ArrayHeader)
// ArrayHeader.elem_meta = element's ValueMeta (for GC scanning)
// elem_slots provided by instruction flags, not stored in object
```

**Slice** (reference type, 4 slots):
```
┌─────────────────────────────────────────────────┐
│ GcHeader (value_kind=Slice)               │
├─────────────────────────────────────────────────┤
│ array: GcRef (underlying array)           │
│ start: usize (offset into array)          │
│ len: usize                                │
│ cap: usize                                │
└─────────────────────────────────────────────────┘
```

### 3.3 Global Variables

Global variables are stored in a dedicated `globals` array, accessed via `GlobalGet`/`GlobalSet` instructions. They are **not** heap-allocated escaped values.

```vo
var globalCounter int   // globals[0] = value
var globalPoint Point   // globals[1..3] = x, y (inline, multi-slot)
```

---

## 4. Escape Analysis

### 4.1 Escape Rules

A local variable **escapes** (requires heap allocation) if any of the following conditions is true:

#### 4.1.1 Primitives (`int`, `float`, `bool`)

- Captured by a closure

#### 4.1.2 Struct

- Address taken: `&s`
- Captured by a closure
- Assigned to an interface
- Pointer-receiver method called: `s.Method()` where `Method` has `*T` receiver
- Larger than **256 slots**

#### 4.1.3 Array

- Captured by a closure
- Assigned to an interface
- Sliced: `arr[:]` or `arr[i:j]`
- Larger than **256 slots**

#### 4.1.5 Interface Assignment

When assigning a value to an interface:
- **Basic types** (int, float, bool): stored directly in interface's data slot, **no escape**
- **Reference types** (slice, map, chan, closure, pointer): already GcRef, copy directly
- **Value types** (struct, array): already escaped (escape analysis ensures this), **deep copy** to interface's data slot

### 4.2 Nested Escape Propagation

If any nested field triggers escape, the **entire root variable** escapes:

```vo
type Outer struct { inner Inner }
var o Outer
p := &o.inner    // o escapes (not just inner)
```

### 4.3 Non-Escape Cases

- Package-level variables are **not** considered "captured" by closures (they're already global)
- Variables only used within the declaring function without triggering any escape rule

---

## 5. Assignment Semantics

### 5.1 Regular Assignment

| Type | Operation | Description |
|------|-----------|-------------|
| Primitive (int/float/bool) | `Copy` | Copy single slot |
| Struct/Array (stack) | `CopyN` | Copy all slots |
| Struct/Array (boxed) | `PtrClone` | Deep copy (value semantics) |
| Pointer (*T) | `Copy` | Copy pointer (reference semantics) |
| Reference types (string/slice/map/chan/closure) | `Copy` | Copy GcRef (shared underlying) |

### 5.2 Interface-to-Interface Assignment

```rust
// dst.slot0 unchanged (target interface type is fixed at compile time)
// dst.slot1 depends on src.slot0.value_kind:
let vk = src.slot0 & 0xFF;
dst.slot1 = if vk == Struct || vk == Array {
    ptr_clone(src.slot1)  // deep copy (value semantics)
} else {
    src.slot1  // direct copy
};
```

| src.slot0.value_kind | dst.slot1 Operation |
|----------------------|---------------------|
| Primitive | `Copy` |
| Reference types | `Copy` |
| Pointer | `Copy` |
| **Struct/Array** | `PtrClone` (deep copy) |

---

## 6. Instruction Set

### 6.1 Instruction Format

Each instruction is 8 bytes:

```rust
pub struct Instruction {
    pub op: u8,      // Opcode
    pub flags: u8,   // Auxiliary flags
    pub a: u16,      // Operand A
    pub b: u16,      // Operand B
    pub c: u16,      // Operand C
}
```

### 6.2 Design Philosophy

#### 6.2.1 Fixed-Length Instructions
- **64-bit fixed size**: Simple decoding, cache-friendly
- Trade-off: Limited parameter space → use other mechanisms to extend

#### 6.2.2 Compile-Time Over Runtime
- **elem_slots encoded in instruction**, not inferred at runtime from objects
- Cranelift can unroll directly, no loops/branches
- Performance priority: Single-slot is the hot path

#### 6.2.3 Unification Over Fragmentation
- **Remove single-slot variants**: `ArrayGet` uses `flags=elem_slots` uniformly
- Fewer opcodes, simpler VM and JIT implementation
- When flags=1, compiler constant-folds, zero overhead

#### 6.2.4 Meta Slot for Unlimited Size
- **When 8-bit flags insufficient**, use adjacent slot to store metadata
- Map/Iter: `slots[x]` = meta, `slots[x+1..]` = data
- Consistent rule: meta first, data after

#### 6.2.5 Static vs Dynamic Indexing

| Index Type | Approach | Examples |
|------------|----------|----------|
| Static | Keep N-version | `GlobalGetN`, `PtrGetN` |
| Dynamic | Unified version | `ArrayGet`, `SliceGet` |
| Unlimited | Meta slot | `MapGet`, `IterBegin` |

#### 6.2.6 Simplicity Over Extreme Performance
- Fewer instructions → fewer VM dispatch branches
- Simpler maintenance → fewer bugs
- Minor performance differences are acceptable

### 6.3 Opcode Categories

#### 6.3.1 LOAD: Load Immediate/Constant

| Opcode | Operands | Description |
|--------|----------|-------------|
| `Nop` | - | No operation |
| `LoadNil` | a | `slots[a] = nil` |
| `LoadTrue` | a | `slots[a] = true` |
| `LoadFalse` | a | `slots[a] = false` |
| `LoadInt` | a, b, c | `slots[a] = sign_extend(b \| (c << 16))` |
| `LoadConst` | a, b | `slots[a] = constants[b]` |

#### 6.3.2 COPY: Stack Slot Copy

| Opcode | Operands | Description |
|--------|----------|-------------|
| `Copy` | a, b | `slots[a] = slots[b]` (single slot) |
| `CopyN` | a, b, c | `slots[a..a+c] = slots[b..b+c]` (multi-slot) |

#### 6.3.3 SLOT: Stack Dynamic Indexing

For stack-allocated arrays with dynamic indices.

| Opcode | Operands | Description |
|--------|----------|-------------|
| `SlotGet` | a, b, c | `slots[a] = slots[b + slots[c]]` |
| `SlotSet` | a, b, c | `slots[a + slots[b]] = slots[c]` |
| `SlotGetN` | a, b, c, flags | `slots[a..a+flags] = slots[b + slots[c]*flags..]` |
| `SlotSetN` | a, b, c, flags | `slots[a + slots[b]*flags..] = slots[c..c+flags]` |

#### 6.3.4 GLOBAL: Global Variable Access

| Opcode | Operands | Description |
|--------|----------|-------------|
| `GlobalGet` | a, b | `slots[a] = globals[b]` |
| `GlobalGetN` | a, b, flags | `slots[a..a+flags] = globals[b..]`, flags=n |
| `GlobalSet` | a, b | `globals[a] = slots[b]` |
| `GlobalSetN` | a, b, flags | `globals[a..] = slots[b..b+flags]`, flags=n |

#### 6.3.5 PTR: Heap Pointer Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `PtrNew` | a, b, flags | `slots[a] = alloc(slots[b])`, b=meta_reg containing ValueMeta, flags=slots |
| `PtrClone` | a, b | `slots[a] = clone(slots[b])` - allocate + memcpy |
| `PtrGet` | a, b, c | `slots[a] = heap[slots[b]].offset[c]` (single slot) |
| `PtrSet` | a, b, c | `heap[slots[a]].offset[b] = slots[c]` (single slot) |
| `PtrGetN` | a, b, c, flags | `slots[a..a+flags] = heap[slots[b]].offset[c..]` |
| `PtrSetN` | a, b, c, flags | `heap[slots[a]].offset[b..] = slots[c..c+flags]` |

#### 6.3.6 ARITH: Integer Arithmetic

| Opcode | Operands | Description |
|--------|----------|-------------|
| `AddI` | a, b, c | `slots[a] = slots[b] + slots[c]` |
| `SubI` | a, b, c | `slots[a] = slots[b] - slots[c]` |
| `MulI` | a, b, c | `slots[a] = slots[b] * slots[c]` |
| `DivI` | a, b, c | `slots[a] = slots[b] / slots[c]` |
| `ModI` | a, b, c | `slots[a] = slots[b] % slots[c]` |
| `NegI` | a, b | `slots[a] = -slots[b]` |

#### 6.3.7 ARITH: Float Arithmetic

| Opcode | Operands | Description |
|--------|----------|-------------|
| `AddF` | a, b, c | `slots[a] = slots[b] + slots[c]` |
| `SubF` | a, b, c | `slots[a] = slots[b] - slots[c]` |
| `MulF` | a, b, c | `slots[a] = slots[b] * slots[c]` |
| `DivF` | a, b, c | `slots[a] = slots[b] / slots[c]` |
| `NegF` | a, b | `slots[a] = -slots[b]` |

#### 6.3.8 CMP: Integer Comparison

| Opcode | Operands | Description |
|--------|----------|-------------|
| `EqI` | a, b, c | `slots[a] = slots[b] == slots[c]` |
| `NeI` | a, b, c | `slots[a] = slots[b] != slots[c]` |
| `LtI` | a, b, c | `slots[a] = slots[b] < slots[c]` |
| `LeI` | a, b, c | `slots[a] = slots[b] <= slots[c]` |
| `GtI` | a, b, c | `slots[a] = slots[b] > slots[c]` |
| `GeI` | a, b, c | `slots[a] = slots[b] >= slots[c]` |

#### 6.3.9 CMP: Float Comparison

| Opcode | Operands | Description |
|--------|----------|-------------|
| `EqF` | a, b, c | `slots[a] = slots[b] == slots[c]` |
| `NeF` | a, b, c | `slots[a] = slots[b] != slots[c]` |
| `LtF` | a, b, c | `slots[a] = slots[b] < slots[c]` |
| `LeF` | a, b, c | `slots[a] = slots[b] <= slots[c]` |
| `GtF` | a, b, c | `slots[a] = slots[b] > slots[c]` |
| `GeF` | a, b, c | `slots[a] = slots[b] >= slots[c]` |

#### 6.3.10 CMP: Reference Comparison

| Opcode | Operands | Description |
|--------|----------|-------------|
| `EqRef` | a, b, c | `slots[a] = slots[b] == slots[c]` (pointer equality) |
| `NeRef` | a, b, c | `slots[a] = slots[b] != slots[c]` |
| `IsNil` | a, b | `slots[a] = slots[b] == nil` |

#### 6.3.11 BIT: Bitwise Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `And` | a, b, c | `slots[a] = slots[b] & slots[c]` |
| `Or` | a, b, c | `slots[a] = slots[b] \| slots[c]` |
| `Xor` | a, b, c | `slots[a] = slots[b] ^ slots[c]` |
| `Not` | a, b | `slots[a] = ^slots[b]` (bitwise NOT) |
| `Shl` | a, b, c | `slots[a] = slots[b] << slots[c]` |
| `ShrS` | a, b, c | `slots[a] = slots[b] >> slots[c]` (arithmetic) |
| `ShrU` | a, b, c | `slots[a] = slots[b] >>> slots[c]` (logical) |

#### 6.3.12 LOGIC: Logical Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `BoolNot` | a, b | `slots[a] = !slots[b]` |

#### 6.3.13 JUMP: Control Flow

| Opcode | Operands | Description |
|--------|----------|-------------|
| `Jump` | b, c | `pc += sign_extend(b \| (c << 16))` |
| `JumpIf` | a, b, c | `if slots[a]: pc += sign_extend(b \| (c << 16))` |
| `JumpIfNot` | a, b, c | `if !slots[a]: pc += sign_extend(b \| (c << 16))` |

#### 6.3.14 CALL: Function Calls

| Opcode | Operands | Description |
|--------|----------|-------------|
| `Call` | a, b, c, flags | Call `functions[a\|(flags<<16)]`, args at `b`, `c`=(arg_slots<<8\|ret_slots) |
| `CallExtern` | a, b, c, flags | Call `externs[a\|(flags<<16)]`, args at `b`, `c`=(arg_slots<<8\|ret_slots) |
| `CallClosure` | a, b, c | Call closure at `slots[a]`, args at `b`, `c`=(arg_slots<<8\|ret_slots) |
| `CallIface` | a, b, c, flags | Call interface method: iface at `a`, args at `b`, `c`=(arg_slots<<8\|ret_slots), `flags`=method_idx |
| `Return` | a, b | Return values starting at `a`, ret_slots=`b` |

`Call`/`CallExtern` encoding: func_id = `a | (flags << 16)` → 24 bits (max 16M functions)

#### 6.3.15 STR: String Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `StrNew` | a, b | `slots[a] = constants[b]` (string constant) |
| `StrLen` | a, b | `slots[a] = len(slots[b])` |
| `StrIndex` | a, b, c | `slots[a] = slots[b][slots[c]]` |
| `StrConcat` | a, b, c | `slots[a] = slots[b] + slots[c]` |
| `StrSlice` | a, b, c | `slots[a] = str[lo:hi]`, b=str, c=params_start, lo=slots[c], hi=slots[c+1] |
| `StrEq` | a, b, c | `slots[a] = slots[b] == slots[c]` |
| `StrNe` | a, b, c | `slots[a] = slots[b] != slots[c]` |
| `StrLt` | a, b, c | `slots[a] = slots[b] < slots[c]` |
| `StrLe` | a, b, c | `slots[a] = slots[b] <= slots[c]` |
| `StrGt` | a, b, c | `slots[a] = slots[b] > slots[c]` |
| `StrGe` | a, b, c | `slots[a] = slots[b] >= slots[c]` |

#### 6.3.16 ARRAY: Heap Array Operations

For escaped arrays allocated on the heap.

| Opcode | Operands | Description |
|--------|----------|-------------|
| `ArrayNew` | a, b, c, flags | `slots[a] = new array`, b=meta_reg, c=len_reg, flags=elem_slots |
| `ArrayGet` | a, b, c, flags | `slots[a..a+flags] = arr[idx]`, b=arr, c=idx, flags=elem_slots |
| `ArraySet` | a, b, c, flags | `arr[idx] = slots[c..c+flags]`, a=arr, b=idx, flags=elem_slots |
| `ArrayLen` | a, b | `slots[a] = len(slots[b])` |

`ArrayNew` encoding: `slots[b]` contains elem's `ValueMeta` (loaded via `LoadConst`), `slots[c]` contains length, `flags` contains elem_slots.

#### 6.3.17 SLICE: Slice Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `SliceNew` | a, b, c, flags | `slots[a] = make([]T, len, cap)`, b=meta_reg, c=params_start, flags=elem_slots |
| `SliceGet` | a, b, c, flags | `slots[a..a+flags] = slice[idx]`, b=slice, c=idx, flags=elem_slots |
| `SliceSet` | a, b, c, flags | `slice[idx] = slots[c..c+flags]`, a=slice, b=idx, flags=elem_slots |
| `SliceLen` | a, b | `slots[a] = len(slots[b])` |
| `SliceCap` | a, b | `slots[a] = cap(slots[b])` |
| `SliceSlice` | a, b, c, flags | `slots[a] = slice[lo:hi:max]`, b=slice, c=params_start, flags: bit0=has_max. lo=slots[c], hi=slots[c+1], max=slots[c+2] if has_max else cap |
| `SliceAppend` | a, b, c, flags | `slots[a] = append(slice, slots[c..c+flags])`, b=slice, flags=elem_slots |

`SliceNew` encoding: `slots[b]` contains elem's `ValueMeta`, `slots[c]` contains length, `slots[c+1]` contains capacity, `flags` contains elem_slots.

#### 6.3.18 MAP: Map Operations

Uses meta slot for key/val slots encoding (no size limit).

| Opcode | Operands | Description |
|--------|----------|-------------|
| `MapNew` | a, b, c | `slots[a] = make(map[K]V)`, b=type_info_reg, c=(key_slots<<8)\|val_slots |
| `MapGet` | a, b, c | `slots[a..] = map[key]`, b=map, c=meta_and_key |
| `MapSet` | a, b, c | `map[key] = val`, a=map, b=meta_and_key, c=val_start |
| `MapDelete` | a, b | `delete(map, key)`, a=map, b=meta_and_key |
| `MapLen` | a, b | `slots[a] = len(slots[b])` |

**Meta slot encoding**:
- `MapGet`: `slots[c] = (key_slots << 16) | (val_slots << 1) | has_ok`, key=`slots[c+1..]`
- `MapSet`: `slots[b] = (key_slots << 8) | val_slots`, key=`slots[b+1..]`
- `MapDelete`: `slots[b] = key_slots`, key=`slots[b+1..]`

`MapNew` encoding: `LoadConst r, <packed_u64>` + `MapNew dst, r, slots_packed`.
- `slots[b]` (packed_u64): `[key_meta:32 | val_meta:32]`, each meta = `[meta_id:24 | kind:8]` (same as `ValueMeta`)
- `c`: `(key_slots << 8) | val_slots`

#### 6.3.19 CHAN: Channel Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `ChanNew` | a, b, c, flags | `slots[a] = make(chan T, cap)`, b=meta_reg, c=cap_reg, flags=elem_slots |
| `ChanSend` | a, b, flags | `chan <- slots[b..b+flags]`, a=chan, flags=elem_slots |
| `ChanRecv` | a, b, flags | `slots[a..] = <-chan`, b=chan, flags=(elem_slots<<1)\|has_ok |
| `ChanClose` | a | `close(slots[a])` |

`ChanNew` encoding: `slots[b]` contains elem's `ValueMeta`, `slots[c]` contains capacity, `flags` contains elem_slots.

#### 6.3.20 SELECT: Select Statement

| Opcode | Operands | Description |
|--------|----------|-------------|
| `SelectBegin` | a, flags | Begin select, a=case_count, flags: bit0=has_default |
| `SelectSend` | a, b, flags | Add send case: a=chan_reg, b=val_reg, flags=elem_slots |
| `SelectRecv` | a, b, flags | Add recv case: a=dst_reg, b=chan_reg, flags=(elem_slots<<1)\|has_ok |
| `SelectExec` | a | Execute select, chosen index → `slots[a]` (-1=default) |

#### 6.3.21 ITER: Iterator (for-range)

Uses meta slot for key/val slots encoding (no size limit).

| Opcode | Operands | Description |
|--------|----------|-------------|
| `IterBegin` | a, b | Begin iteration, a=meta_and_container, b=type |
| `IterNext` | a, b, c | `slots[a..], slots[b..] = next`, done_offset=`c`; slots count from IterState |
| `IterEnd` | - | End iteration |

**Meta slot encoding**:
- `slots[a] = (key_slots << 8) | val_slots`, container=`slots[a+1]`

#### 6.3.22 CLOSURE: Closure Operations

| Opcode | Operands | Description |
|--------|----------|-----------|
| `ClosureNew` | a, b, c, flags | `slots[a] = new_closure(func_id=b\|(flags<<16), capture_count=c)` |
| `ClosureGet` | a, b | `slots[a] = slots[0].captures[b]` (closure implicit in r0) |
| `ClosureSet` | a, b | `slots[0].captures[a] = slots[b]` (closure implicit in r0) |

Note: Escaped variables are heap-allocated directly, and closures store GcRefs to them (no indirection).

#### 6.3.23 GO: Goroutine

| Opcode | Operands | Description |
|--------|----------|-------------|
| `GoCall` | a | `go slots[a]()` (0-arg closure, same as defer) |
| `Yield` | - | Yield current goroutine |

#### 6.3.24 DEFER: Defer and Error Handling

| Opcode | Operands | Description |
|--------|----------|-------------|
| `DeferPush` | a | Push defer: closure=`slots[a]` (0-arg closure) |
| `ErrDeferPush` | a | Push errdefer: closure=`slots[a]` (executed only on error return) |
| `Panic` | a | `panic(slots[a])` |
| `Recover` | a | `slots[a] = recover()` |

Note: `DeferPop` removed. Defers are executed automatically by `Return`.

#### 6.3.25 IFACE: Interface Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `IfaceAssign` | a, b, c, flags | Assign to interface: dst=`slots[a..a+2]`, src=`slots[b]`, c=`const_idx`, flags=`value_kind` |
| `IfaceAssert` | a, b, c, flags | Type assert: dst=`a`, src_iface=`b`, target_id=`c`, flags=`assert_kind \| (has_ok << 2)` |

**IfaceAssign Semantics**:

```rust
// a = dst slot (interface occupies 2 slots: a, a+1)
// b = src value slot (interface source: b, b+1)
// c = constant pool index (Int64)
//     - 具体类型: (named_type_id << 32) | itab_id，编译时已建 itab
//     - Interface: iface_meta_id（高32位为0），运行时建 itab
// flags = value_kind of source

let vk = flags;
let packed = constants[c].as_int64();
let named_type_id = (packed >> 32) as u32;
let low = (packed & 0xFFFFFFFF) as u32;

let (actual_named_type_id, actual_vk, itab_id) = if vk == Interface {
    // Interface → Interface: 运行时查/建 itab
    let src_slot0 = slots[b];
    let src_named_type_id = (src_slot0 >> 8) & 0xFFFFFF;
    let src_vk = src_slot0 & 0xFF;
    let iface_meta_id = low;
    let itab_id = vm.get_or_create_itab(src_named_type_id, iface_meta_id);
    (src_named_type_id, src_vk, itab_id)
} else {
    // 具体类型 → Interface: 编译时已建 itab
    (named_type_id, vk, low)
};

// Write slot0: [itab_id:32 | named_type_id:24 | value_kind:8]
slots[a] = ((itab_id as u64) << 32) | ((actual_named_type_id as u64) << 8) | (actual_vk as u64);

// Write slot1: deep copy if Struct/Array
match actual_vk {
    Struct | Array => slots[a+1] = gc.ptr_clone(slots[b]),
    Interface => {
        let src_vk = slots[b] & 0xFF;
        slots[a+1] = if src_vk == Struct || src_vk == Array {
            gc.ptr_clone(slots[b+1])
        } else {
            slots[b+1]
        };
    }
    _ => slots[a+1] = slots[b],
}
```

**Source value types** (三种右值):
| 右值类型 | named_type_id | itab | 说明 |
|----------|---------------|------|------|
| Primitive | 0 (from const) | 编译时构建 | int, float, bool, string, etc. |
| Named type | from const | 编译时构建 | struct, type alias, etc. |
| Interface | from src.slot0 | 运行时构建 | 需要 itab_cache |

**常量格式** (Int64):
- 具体类型右值: `(named_type_id << 32) | itab_id`，itab 编译时已构建
- Interface 右值: `iface_meta_id`（高 32 位为 0），itab 运行时构建

**Itab Structure**:
```rust
// 字节码中
struct Module {
    itabs: Vec<Itab>,  // 编译时构建的 itabs
    // ...
}

// VM 运行时
struct VM {
    itabs: Vec<Itab>,           // 初始化时从 module.itabs 拷贝，运行时追加
    itab_cache: HashMap<(u32, u32), u32>,  // (named_type_id, iface_meta_id) -> itab_id
}

struct Itab {
    methods: Vec<u32>,  // method_idx -> func_id
}
```

**运行时逻辑**:
```rust
let packed = constants[c].as_int64();
let named_type_id = (packed >> 32) as u32;
let low = (packed & 0xFFFFFFFF) as u32;

let (actual_named_type_id, actual_vk, itab_id) = if vk == ValueKind::Interface {
    // Interface → Interface: 运行时查/建 itab
    let src_slot0 = slots[b];
    let src_named_type_id = (src_slot0 >> 8) & 0xFFFFFF;
    let src_vk = src_slot0 & 0xFF;
    let iface_meta_id = low;
    let itab_id = vm.get_or_create_itab(src_named_type_id, iface_meta_id);
    (src_named_type_id, src_vk, itab_id)
} else {
    // 具体类型 → Interface: 编译时已建 itab
    (named_type_id, vk, low)  // low = itab_id
};
```

**IfaceAssert Semantics**:

```rust
// a = dst, b = src_iface (2 slots), c = target_id
// flags = assert_kind | (has_ok << 2)
//   assert_kind: 0=Primitive, 1=Named, 2=Interface
//   has_ok: whether to return ok bool instead of panic

let assert_kind = flags & 0x3;
let has_ok = (flags >> 2) != 0;

let src_slot0 = slots[b];
let src_named_type_id = (src_slot0 >> 8) & 0xFFFFFF;
let src_vk = (src_slot0 & 0xFF) as ValueKind;

let matches = match assert_kind {
    0 => src_named_type_id == 0 && src_vk as u32 == c,  // Primitive: check value_kind
    1 => src_named_type_id == c,                         // Named: check named_type_id
    2 => {                                               // Interface: check method set
        if src_named_type_id == 0 { false }
        else {
            let is_pointer = src_vk == ValueKind::Pointer;
            itab_cache.try_build(src_named_type_id, c, is_pointer).is_ok()
        }
    }
    _ => unreachable!(),
};

if matches {
    // Copy value to dst (slots depend on target type)
    if has_ok { slots[a + dst_slots] = 1; }
} else if has_ok {
    slots[a + dst_slots] = 0;
} else {
    panic!("type assertion failed");
}
```

**Type Assertion Restrictions**:
- Only supports: primitive types, named types, interfaces
- Anonymous composite types (`[]int`, `map[K]V`, etc.) NOT supported
- Use named type wrapper: `type IntSlice []int`

#### 6.3.26 CONV: Type Conversion

| Opcode | Operands | Description |
|--------|----------|-------------|
| `ConvI2F` | a, b | `slots[a] = float64(slots[b])` |
| `ConvF2I` | a, b | `slots[a] = int64(slots[b])` |
| `ConvI32I64` | a, b | `slots[a] = int64(int32(slots[b]))` |
| `ConvI64I32` | a, b | `slots[a] = int32(slots[b])` |

#### 6.3.27 DEBUG: Debug Operations

Note: `Print` uses CallExtern (`vo_print`). Assert is implemented using `JumpIf` + `CallExtern(print)` + `Panic`.

---

## 7. Runtime Structures

### 7.1 SlotType

Used for GC stack scanning:

```rust
pub enum SlotType {
    Value,       // Non-GC value (int, float, bool)
    GcRef,       // GC reference (pointer to heap object)
    Interface0,  // Interface header slot
    Interface1,  // Interface data slot
}
```

### 7.2 StructMeta

Runtime type metadata for struct types (physical layout only):

```rust
pub struct StructMeta {
    pub size_slots: u16,
    pub slot_types: Vec<SlotType>,  // For GC scanning
    pub field_names: Vec<String>,
    pub field_offsets: Vec<u16>,
}
```

### 7.3 NamedTypeMeta

Runtime metadata for named types (used for itab building and type assertion):

```rust
pub struct MethodInfo {
    pub func_id: u32,
    pub is_pointer_receiver: bool,
}

pub struct NamedTypeMeta {
    pub name: String,
    pub underlying_meta: ValueMeta,             // [struct_meta_id:24 | value_kind:8]
    pub methods: HashMap<String, MethodInfo>,   // method_name -> MethodInfo
}
```

**Examples**:
| Named Type | underlying_meta |
|------------|-----------------|
| `type Point struct {...}` | `ValueMeta(struct_meta_id, Struct)` |
| `type MyInt int` | `ValueMeta(0, Int)` |
| `type MySlice []int` | `ValueMeta(0, Slice)` |

**Key points**:
- Methods belong to the named type, not the receiver form (`T` and `*T` share one namespace)
- `is_pointer_receiver` determines method set membership:
  - `T` method set: methods where `is_pointer_receiver = false`
  - `*T` method set: all methods
- `named_type_id` (index into `named_type_metas[]`) is stored in interface slot0
- `underlying_meta` provides access to physical layout (struct_meta_id) and value_kind

### 7.4 Closure Structure

```rust
/// Closure layout: GcHeader + ClosureHeader + [captures...]
pub struct ClosureHeader {
    pub func_id: u32,
    pub capture_count: u32,
}
// Followed by capture_count GcRef slots (direct pointers to escaped variables on heap)
```

