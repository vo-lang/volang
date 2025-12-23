# Vo Memory Model and Instruction Set Specification

**Version**: 2.0 (Memory Model Refactoring)  
**Status**: Draft  
**Date**: 2025-12-23

## Table of Contents

1. [Overview](#1-overview)
2. [Type System](#2-type-system)
3. [Memory Model](#3-memory-model)
4. [Escape Analysis](#4-escape-analysis)
5. [Instruction Set](#5-instruction-set)
6. [Runtime Structures](#6-runtime-structures)

---

## 1. Overview

This document specifies the Vo memory model and instruction set architecture. The key design principle is **escape-analysis-driven allocation**: values are allocated on the stack by default, and only escape to the heap when necessary.

### 1.1 Design Goals

1. **Value semantics by default**: `struct`, `array`, and `interface` are value types
2. **Static escape analysis**: Determine allocation location at compile time
3. **Unified pointer representation**: All escaped values are represented as `Pointer`
4. **Simplified closure capture**: No delayed "upvalue promotion" - escaped variables are heap-allocated from the start

### 1.2 Key Changes from Previous Model

| Aspect | Old Model | New Model |
|--------|-----------|-----------|
| struct allocation | Always heap | Stack by default, heap if escaped |
| array allocation | Always heap | Stack by default, heap if escaped |
| closure capture | Upval box (delayed promotion) | Direct heap allocation via escape analysis |
| nested struct | Separate heap objects | Flattened inline |

---

## 2. Type System

### 2.1 ValueKind

`ValueKind` is a runtime type tag (u8) that classifies all values:

```rust
pub enum ValueKind {
    // === Primitive Types (1 slot, no GC) ===
    Nil = 0,
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

#### 2.2.3 Boxed Value Types

When a value type (struct/array) escapes, it is **boxed** on the heap but retains **value semantics**:

| Physical State | Semantic | Assignment |
|----------------|----------|------------|
| Stack struct/array | Value | `CopyN` (copy slots) |
| Boxed struct/array | Value | `PtrClone` (deep copy) |
| Pointer (*T) | Reference | `Copy` (copy pointer only) |

**Key distinction**: Boxed struct and Pointer are both GcRef physically, but:
- **Boxed struct**: value semantics → assignment deep copies
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
// slot 0: header (iface_meta_id:24 | reserved:8 | value_meta_id:24 | value_kind:8)
// slot 1: data (immediate value or GcRef)
```

### 3.2 Heap Layout

Escaped values are allocated on the heap with a GC header.

#### 3.2.1 GcHeader

```rust
/// GC 对象头 - 8 字节
/// 布局: [mark:8 | gen:8 | slots:16 | meta_id:24 | value_kind:8]
pub struct GcHeader {
    pub mark: u8,          // GC mark bit (White/Gray/Black)
    pub gen: u8,           // Generation (Young/Old)
    pub slots: u16,        // Number of data slots
    pub meta_id: [u8; 3],  // 24-bit metadata index
    pub value_kind: u8,    // [is_array:1 | kind:7]
}

// value_kind 字段:
// - bit 7 (0x80): is_array 标记
//   - 1 = Array 对象, kind 是元素类型
//   - 0 = 普通对象, kind 是对象类型
// - bit 0-6: ValueKind 值
//
// meta_id 含义:
// - Struct: struct_metas[] 索引
// - Interface: interface_metas[] 索引  
// - Array of Struct/Interface: 元素的 meta_id
// - 其他: 0
```

#### 3.2.2 Heap Object Layout

**Escaped Primitive** (BoxedInt, BoxedFloat, etc.):
```
┌─────────────────────────────┐
│ GcHeader (value_kind=Int)   │
├─────────────────────────────┤
│ value (8 bytes)             │
└─────────────────────────────┘
```

**Escaped Struct**:
```
┌─────────────────────────────┐
│ GcHeader (value_kind=Struct)│
├─────────────────────────────┤
│ field 0 (8 bytes)           │
│ field 1 (8 bytes)           │
│ ...                         │
└─────────────────────────────┘
```

**Escaped Array**:
```
┌─────────────────────────────────────────────────┐
│ GcHeader (is_array=1, kind=elem_kind)     │
├─────────────────────────────────────────────────┤
│ Array header: [len:48 | elem_slots:16]    │
├─────────────────────────────────────────────────┤
│ element 0                                 │
│ element 1                                 │
│ ...                                       │
└─────────────────────────────────────────────────┘

// GcHeader.value_kind = 0x80 | elem_kind (is_array=1)
// GcHeader.meta_id = elem_meta_id (元素是 Struct/Interface 时)
// Array header slot 0: [len:48 | elem_slots:16]
```

### 3.3 Global Variables

Global variables are treated as **always escaped** and allocated on the heap. The global table stores GcRefs to heap objects.

```vo
var globalPoint Point  // Heap-allocated, globals[i] = GcRef
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

#### 4.1.3 Array

- Captured by a closure
- Assigned to an interface
- Sliced: `arr[:]` or `arr[i:j]`
- Dynamically indexed: `arr[i]` where `i` is not a compile-time constant

#### 4.1.5 Interface Assignment

When assigning a value to an interface:
- **Basic types** (int, float, bool): stored directly in interface's data slot, **no escape**
- **Reference types** (slice, map, chan, closure, pointer): already GcRef, copy directly
- **Value types** (struct, array): already boxed (escape analysis ensures this), **deep copy** to interface's data slot

---

## 4.2 Assignment Semantics

### 4.2.1 Regular Assignment

| Type | Operation | Description |
|------|-----------|-------------|
| Primitive (int/float/bool) | `Copy` | Copy single slot |
| Struct/Array (stack) | `CopyN` | Copy all slots |
| Struct/Array (boxed) | `PtrClone` | Deep copy (value semantics) |
| Pointer (*T) | `Copy` | Copy pointer (reference semantics) |
| Reference types (string/slice/map/chan/closure) | `Copy` | Copy GcRef (shared underlying) |

### 4.2.2 Interface-to-Interface Assignment

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

#### 4.2.3 Size Threshold

- `struct` or `array` larger than **256 slots** → always escapes

### 4.3 Nested Escape Propagation

If any nested field triggers escape, the **entire root variable** escapes:

```vo
type Outer struct { inner Inner }
var o Outer
p := &o.inner    // o escapes (not just inner)
```

### 4.4 Non-Escape Cases

- Package-level variables are **not** considered "captured" by closures (they're already global)
- Variables only used within the declaring function without triggering any escape rule

---

## 5. Instruction Set

### 5.1 Instruction Format

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

### 5.2 Opcode Categories

#### 5.2.1 LOAD: Load Immediate/Constant

| Opcode | Operands | Description |
|--------|----------|-------------|
| `Nop` | - | No operation |
| `LoadNil` | a | `slots[a] = nil` |
| `LoadTrue` | a | `slots[a] = true` |
| `LoadFalse` | a | `slots[a] = false` |
| `LoadInt` | a, b, c | `slots[a] = sign_extend(b \| (c << 16))` |
| `LoadConst` | a, b | `slots[a] = constants[b]` |

#### 5.2.2 COPY: Stack Slot Copy

| Opcode | Operands | Description |
|--------|----------|-------------|
| `Copy` | a, b | `slots[a] = slots[b]` (single slot) |
| `CopyN` | a, b, c | `slots[a..a+c] = slots[b..b+c]` (multi-slot) |

#### 5.2.3 SLOT: Stack Dynamic Indexing

For stack-allocated arrays with dynamic indices.

| Opcode | Operands | Description |
|--------|----------|-------------|
| `SlotGet` | a, b, c | `slots[a] = slots[b + slots[c]]` |
| `SlotSet` | a, b, c | `slots[a + slots[b]] = slots[c]` |
| `SlotGetN` | a, b, c, flags | `slots[a..a+flags] = slots[b + slots[c]*flags..]` |
| `SlotSetN` | a, b, c, flags | `slots[a + slots[b]*flags..] = slots[c..c+flags]` |

#### 5.2.4 GLOBAL: Global Variable Access

| Opcode | Operands | Description |
|--------|----------|-------------|
| `GlobalGet` | a, b | `slots[a] = globals[b]` |
| `GlobalSet` | a, b | `globals[a] = slots[b]` |

#### 5.2.5 PTR: Heap Pointer Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `PtrNew` | a, b, c, flags | `slots[a] = alloc(value_kind=flags, type_id=b, size=c)` with zero-init |
| `PtrClone` | a, b | `slots[a] = clone(slots[b])` - allocate + memcpy |
| `PtrGet` | a, b, c | `slots[a] = heap[slots[b]].offset[c]` (single slot) |
| `PtrSet` | a, b, c | `heap[slots[a]].offset[b] = slots[c]` (single slot) |
| `PtrGetN` | a, b, c, flags | `slots[a..a+flags] = heap[slots[b]].offset[c..]` |
| `PtrSetN` | a, b, c, flags | `heap[slots[a]].offset[b..] = slots[c..c+flags]` |

#### 5.2.6 ARITH: Integer Arithmetic

| Opcode | Operands | Description |
|--------|----------|-------------|
| `AddI` | a, b, c | `slots[a] = slots[b] + slots[c]` |
| `SubI` | a, b, c | `slots[a] = slots[b] - slots[c]` |
| `MulI` | a, b, c | `slots[a] = slots[b] * slots[c]` |
| `DivI` | a, b, c | `slots[a] = slots[b] / slots[c]` |
| `ModI` | a, b, c | `slots[a] = slots[b] % slots[c]` |
| `NegI` | a, b | `slots[a] = -slots[b]` |

#### 5.2.7 ARITH: Float Arithmetic

| Opcode | Operands | Description |
|--------|----------|-------------|
| `AddF` | a, b, c | `slots[a] = slots[b] + slots[c]` |
| `SubF` | a, b, c | `slots[a] = slots[b] - slots[c]` |
| `MulF` | a, b, c | `slots[a] = slots[b] * slots[c]` |
| `DivF` | a, b, c | `slots[a] = slots[b] / slots[c]` |
| `NegF` | a, b | `slots[a] = -slots[b]` |

#### 5.2.8 CMP: Integer Comparison

| Opcode | Operands | Description |
|--------|----------|-------------|
| `EqI` | a, b, c | `slots[a] = slots[b] == slots[c]` |
| `NeI` | a, b, c | `slots[a] = slots[b] != slots[c]` |
| `LtI` | a, b, c | `slots[a] = slots[b] < slots[c]` |
| `LeI` | a, b, c | `slots[a] = slots[b] <= slots[c]` |
| `GtI` | a, b, c | `slots[a] = slots[b] > slots[c]` |
| `GeI` | a, b, c | `slots[a] = slots[b] >= slots[c]` |

#### 5.2.9 CMP: Float Comparison

| Opcode | Operands | Description |
|--------|----------|-------------|
| `EqF` | a, b, c | `slots[a] = slots[b] == slots[c]` |
| `NeF` | a, b, c | `slots[a] = slots[b] != slots[c]` |
| `LtF` | a, b, c | `slots[a] = slots[b] < slots[c]` |
| `LeF` | a, b, c | `slots[a] = slots[b] <= slots[c]` |
| `GtF` | a, b, c | `slots[a] = slots[b] > slots[c]` |
| `GeF` | a, b, c | `slots[a] = slots[b] >= slots[c]` |

#### 5.2.10 CMP: Reference Comparison

| Opcode | Operands | Description |
|--------|----------|-------------|
| `EqRef` | a, b, c | `slots[a] = slots[b] == slots[c]` (pointer equality) |
| `NeRef` | a, b, c | `slots[a] = slots[b] != slots[c]` |
| `IsNil` | a, b | `slots[a] = slots[b] == nil` |

#### 5.2.11 BIT: Bitwise Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `And` | a, b, c | `slots[a] = slots[b] & slots[c]` |
| `Or` | a, b, c | `slots[a] = slots[b] \| slots[c]` |
| `Xor` | a, b, c | `slots[a] = slots[b] ^ slots[c]` |
| `Not` | a, b | `slots[a] = ^slots[b]` (bitwise NOT) |
| `Shl` | a, b, c | `slots[a] = slots[b] << slots[c]` |
| `ShrS` | a, b, c | `slots[a] = slots[b] >> slots[c]` (arithmetic) |
| `ShrU` | a, b, c | `slots[a] = slots[b] >>> slots[c]` (logical) |

#### 5.2.12 LOGIC: Logical Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `BoolNot` | a, b | `slots[a] = !slots[b]` |

#### 5.2.13 JUMP: Control Flow

| Opcode | Operands | Description |
|--------|----------|-------------|
| `Jump` | b, c | `pc += sign_extend(b \| (c << 16))` |
| `JumpIf` | a, b, c | `if slots[a]: pc += sign_extend(b \| (c << 16))` |
| `JumpIfNot` | a, b, c | `if !slots[a]: pc += sign_extend(b \| (c << 16))` |

#### 5.2.14 CALL: Function Calls

| Opcode | Operands | Description |
|--------|----------|-------------|
| `Call` | a, b, c, flags | Call `functions[a]`, args at `b`, arg_slots=`c`, ret_slots=`flags` |
| `CallExtern` | a, b, c, flags | Call extern function, arg_slots=`c`, ret_slots=`flags` |
| `CallClosure` | a, b, c, flags | Call closure at `slots[a]`, args at `b`, arg_slots=`c`, ret_slots=`flags` |
| `CallIface` | a, b, c, flags | Call interface method: iface at `a`, args at `b`, `c`=(arg_slots<<8\|ret_slots), `flags`=method_idx |
| `Return` | a, b | Return values starting at `a`, ret_slots=`b` |

#### 5.2.15 STR: String Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `StrLen` | a, b | `slots[a] = len(slots[b])` |
| `StrIndex` | a, b, c | `slots[a] = slots[b][slots[c]]` |
| `StrConcat` | a, b, c | `slots[a] = slots[b] + slots[c]` |
| `StrSlice` | a, b, c, flags | `slots[a] = slots[b][slots[c]:slots[flags]]` |
| `StrEq` | a, b, c | `slots[a] = slots[b] == slots[c]` |
| `StrNe` | a, b, c | `slots[a] = slots[b] != slots[c]` |
| `StrLt` | a, b, c | `slots[a] = slots[b] < slots[c]` |
| `StrLe` | a, b, c | `slots[a] = slots[b] <= slots[c]` |
| `StrGt` | a, b, c | `slots[a] = slots[b] > slots[c]` |
| `StrGe` | a, b, c | `slots[a] = slots[b] >= slots[c]` |

Note: `StrNew` uses CallExtern (`vo_string_new`).

#### 5.2.16 ARRAY: Heap Array Operations

For escaped arrays allocated on the heap.

| Opcode | Operands | Description |
|--------|----------|-------------|
| `ArrayGet` | a, b, c | `slots[a] = slots[b][slots[c]]` |
| `ArraySet` | a, b, c | `slots[a][slots[b]] = slots[c]` |
| `ArrayLen` | a, b | `slots[a] = len(slots[b])` |

Note: `ArrayNew` uses CallExtern (`vo_array_create`).

#### 5.2.17 SLICE: Slice Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `SliceGet` | a, b, c | `slots[a] = slots[b][slots[c]]` |
| `SliceSet` | a, b, c | `slots[a][slots[b]] = slots[c]` |
| `SliceLen` | a, b | `slots[a] = len(slots[b])` |
| `SliceCap` | a, b | `slots[a] = cap(slots[b])` |
| `SliceSlice` | a, b, c, flags | `slots[a] = slots[b][slots[c]:slots[flags]]` |
| `SliceAppend` | a, b, c | `slots[a] = append(slots[b], slots[c])` |

Note: `SliceNew` uses CallExtern (`vo_slice_create`).

#### 5.2.18 MAP: Map Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `MapGet` | a, b, c, flags | `slots[a] = slots[b][slots[c]]`, flags=1 时 ok 写到 `slots[a+1]` |
| `MapSet` | a, b, c | `slots[a][slots[b]] = slots[c]` |
| `MapDelete` | a, b | `delete(slots[a], slots[b])` |
| `MapLen` | a, b | `slots[a] = len(slots[b])` |

Note: `MapNew` uses CallExtern (`vo_map_create`).

#### 5.2.19 CHAN: Channel Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `ChanSend` | a, b | `slots[a] <- slots[b]` |
| `ChanRecv` | a, b, c, flags | `slots[a] = <-slots[b]`, flags=1 时 ok 写到 `slots[c]` |
| `ChanClose` | a | `close(slots[a])` |

Note: `ChanNew` uses CallExtern (`vo_channel_create`).

#### 5.2.20 SELECT: Select Statement

| Opcode | Operands | Description |
|--------|----------|-------------|
| `SelectBegin` | a, b | Begin select, case_count=`a`, has_default=`b` |
| `SelectSend` | a, b | Add send case: chan=`a`, val=`b` |
| `SelectRecv` | a, b, c | Add recv case: dst=`a`, chan=`b`, ok=`c` |
| `SelectEnd` | a | Execute select, chosen index → `a` |

#### 5.2.21 ITER: Iterator (for-range)

| Opcode | Operands | Description |
|--------|----------|-------------|
| `IterBegin` | a, b | Begin iteration over `slots[a]`, type=`b` |
| `IterNext` | a, b, c | `slots[a], slots[b] = next`, done_offset=`c` |
| `IterEnd` | - | End iteration |

#### 5.2.22 CLOSURE: Closure Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `ClosureGet` | a, b | `slots[a] = slots[0].captures[b]` (closure 隐式在 r0) |
| `ClosureSet` | a, b | `slots[0].captures[a] = slots[b]` (closure 隐式在 r0) |

Note: `ClosureNew` uses CallExtern (`vo_closure_create`). Escaped variables are heap-allocated directly, and closures capture GcRefs.

#### 5.2.23 GO: Goroutine

| Opcode | Operands | Description |
|--------|----------|-------------|
| `GoCall` | a | `go slots[a]()` (0 参数 closure，与 defer 一致) |
| `Yield` | - | Yield current goroutine |

#### 5.2.24 DEFER: Defer and Error Handling

| Opcode | Operands | Description |
|--------|----------|-------------|
| `DeferPush` | a | Push defer: closure=`slots[a]` (0 参数 closure) |
| `ErrDeferPush` | a | Push errdefer: closure=`slots[a]` (只在 error return 时执行) |
| `Panic` | a | `panic(slots[a])` |
| `Recover` | a | `slots[a] = recover()` |

Note: `DeferPop` removed. Defers are executed automatically by `Return`.

#### 5.2.25 IFACE: Interface Operations

| Opcode | Operands | Description |
|--------|----------|-------------|
| `IfaceInit` | a, b, c | Init nil interface at `slots[a..a+2]`, iface_meta_id=`b \| (c << 16)` |
| `IfaceAssign` | a, b, flags | Assign value to interface: dst=`slots[a..a+2]`, src=`slots[b]`, vk=`flags`. See below. |
| `IfaceAssert` | a, b, c, flags | Type assert: `slots[a..] = slots[b..b+2].(type c)`, ok at `slots[flags]` |

**IfaceAssign Semantics**:

The `value_meta_id` is read from the GcHeader of the source value (for Pointer/Struct/Array).

```rust
match vk {
    Struct | Array => {
        // Boxed value type → deep copy (value semantics)
        let src_ref = slots[b] as GcRef;
        let new_ref = gc.ptr_clone(src_ref);
        let meta_id = gc.get_header(src_ref).meta_id;
        slots[a] = (iface_meta_id << 32) | (meta_id << 8) | vk;
        slots[a+1] = new_ref;
    }
    Interface => {
        // Interface → Interface: copy slot0's value info, deep copy slot1 if needed
        let src_vk = slots[b] & 0xFF;
        slots[a] = (iface_meta_id << 32) | (slots[b] & 0xFFFFFFFF);
        slots[a+1] = if src_vk == Struct || src_vk == Array {
            gc.ptr_clone(slots[b+1])
        } else {
            slots[b+1]
        };
    }
    _ => {
        // Primitive, reference types, Pointer → direct copy
        slots[a] = (iface_meta_id << 32) | vk;
        slots[a+1] = slots[b];
    }
}
```

#### 5.2.26 CONV: Type Conversion

| Opcode | Operands | Description |
|--------|----------|-------------|
| `ConvI2F` | a, b | `slots[a] = float64(slots[b])` |
| `ConvF2I` | a, b | `slots[a] = int64(slots[b])` |
| `ConvI32I64` | a, b | `slots[a] = int64(int32(slots[b]))` |
| `ConvI64I32` | a, b | `slots[a] = int32(slots[b])` |

#### 5.2.27 DEBUG: Debug Operations

Note: `Print` uses CallExtern (`vo_print`). Assert is implemented using `JumpIf` + `CallExtern(print)` + `Panic`.

### 5.3 Built-in CallExtern Functions

The following operations are implemented via `CallExtern` calling runtime-core functions:

| Category | Function | Description |
|----------|----------|-------------|
| **String** | `vo_string_create` | Create string from constant |
| **Array** | `vo_array_create` | Create array |
| **Slice** | `vo_slice_create` | Create slice |
| **Map** | `vo_map_create` | Create map |
| **Channel** | `vo_channel_create` | Create channel |
| **Closure** | `vo_closure_create` | Create closure |
| **Debug** | `vo_print` | Print value |

**Design Principles**:
- **Allocation operations** → CallExtern (memory allocation overhead dominates, function call overhead negligible)
- **Read operations** → Keep as instructions (high frequency, lightweight)
- **VM state operations** → Keep as instructions (require internal state)

---

## 6. Runtime Structures

### 6.1 SlotType

Used for GC stack scanning:

```rust
pub enum SlotType {
    Value,       // Non-GC value (int, float, bool)
    GcRef,       // GC reference (pointer to heap object)
    Interface0,  // Interface header slot
    Interface1,  // Interface data slot
}
```

### 6.2 TypeMeta

Runtime type metadata for struct types:

```rust
pub struct TypeMeta {
    pub name: String,
    pub size_slots: u16,
    pub slot_types: Vec<SlotType>,  // For GC scanning
}
```

### 6.3 Closure Structure

```rust
pub struct Closure {
    pub func_id: u32,
    pub captures: Vec<GcRef>,  // Captured variables (already on heap)
}
```

---

## Appendix A: Instruction Opcode Values

```rust
pub enum Opcode {
    // LOAD
    Nop = 0, LoadNil, LoadTrue, LoadFalse, LoadInt, LoadConst,
    
    // COPY
    Copy = 10, CopyN,
    
    // SLOT
    SlotGet = 15, SlotSet, SlotGetN, SlotSetN,
    
    // GLOBAL
    GlobalGet = 20, GlobalSet,
    
    // PTR
    PtrNew = 25, PtrClone, PtrGet, PtrSet, PtrGetN, PtrSetN,
    
    // ARITH (int)
    AddI = 35, SubI, MulI, DivI, ModI, NegI,
    
    // ARITH (float)
    AddF = 45, SubF, MulF, DivF, NegF,
    
    // CMP (int)
    EqI = 55, NeI, LtI, LeI, GtI, GeI,
    
    // CMP (float)
    EqF = 65, NeF, LtF, LeF, GtF, GeF,
    
    // CMP (ref)
    EqRef = 75, NeRef, IsNil,
    
    // BIT
    And = 80, Or, Xor, Not, Shl, ShrS, ShrU,
    
    // LOGIC
    BoolNot = 90,
    
    // JUMP
    Jump = 95, JumpIf, JumpIfNot,
    
    // CALL
    Call = 100, CallExtern, CallClosure, CallIface, Return,
    
    // STR
    StrNew = 110, StrConcat, StrLen, StrIndex, StrSlice,
    StrEq, StrNe, StrLt, StrLe, StrGt, StrGe,
    
    // ARRAY
    ArrayNew = 125, ArrayGet, ArraySet, ArrayLen,
    
    // SLICE
    SliceNew = 135, SliceGet, SliceSet, SliceLen, SliceCap, SliceSlice, SliceAppend,
    
    // MAP
    MapNew = 145, MapGet, MapSet, MapDelete, MapLen,
    
    // CHAN
    ChanNew = 155, ChanSend, ChanRecv, ChanClose,
    
    // SELECT
    SelectBegin = 165, SelectSend, SelectRecv, SelectEnd,
    
    // ITER
    IterBegin = 175, IterNext, IterEnd,
    
    // CLOSURE
    ClosureNew = 185, ClosureGet, ClosureSet,
    
    // GO
    GoCall = 195, Yield,
    
    // DEFER
    DeferPush = 200, DeferPop, ErrDeferPush, Panic, Recover,
    
    // IFACE
    IfaceInit = 210, IfaceBox, IfaceUnbox, IfaceAssert,
    
    // CONV
    ConvI2F = 220, ConvF2I, ConvI32I64, ConvI64I32,
    
    // DEBUG
    Print = 230, AssertBegin, AssertArg, AssertEnd,
    
    Invalid = 255,
}
```

---

## Appendix B: Migration Notes

### B.1 Removed Instructions

| Instruction | Replacement |
|-------------|-------------|
| `Mov` | `Copy` |
| `MovN` | `CopyN` |
| `Alloc` | `PtrNew` |
| `StructClone` | `PtrClone` |
| `GetField` / `SetField` | `PtrGet` / `PtrSet` |
| `GetFieldN` / `SetFieldN` | `PtrGetN` / `PtrSetN` |
| `UpvalNew` | Removed (escape analysis) |
| `UpvalGet` | `PtrGet` on captured GcRef |
| `UpvalSet` | `PtrSet` on captured GcRef |
| `CallInterface` | `CallIface` |
| `InitInterface` | `IfaceInit` |
| `BoxInterface` | `IfaceBox` |
| `UnboxInterface` | `IfaceUnbox` |
| `TypeAssert` | `IfaceAssert` |

### B.2 New Instructions

| Instruction | Purpose |
|-------------|---------|
| `SlotGet` / `SlotSet` | Stack array dynamic indexing |
| `SlotGetN` / `SlotSetN` | Multi-slot stack dynamic indexing |
