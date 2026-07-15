# Vo VM and Bytecode Specification

**Version**: 1.0  
**Status**: Draft  
**Date**: 2025-12-27

## Table of Contents

1. [Overview](#1-overview)
2. [Instruction Format](#2-instruction-format)
3. [Opcode Definitions](#3-opcode-definitions)
4. [Bytecode Module](#4-bytecode-module)
5. [VM Runtime Structures](#5-vm-runtime-structures)
6. [Instruction Execution Semantics](#6-instruction-execution-semantics)

---

## 1. Overview

This document specifies the Vo bytecode format and VM interpreter. The VM is Vo's core execution engine.

The current Rust source is authoritative for exact layouts:

- `lang/crates/vo-common-core/src/instruction.rs` defines `Instruction` and `Opcode`.
- `lang/crates/vo-common-core/src/bytecode.rs` defines `Module`, `FunctionDef`, metadata tables, and serialization data.
- `lang/crates/vo-common-core/src/serialize.rs` defines the canonical VOB codec and resource limits.
- `lang/crates/vo-common-core/src/verifier.rs` defines the shared VM/module verifier that must accept bytecode before VM, GC, WASM, embed, or JIT execution.

### 1.1 Core Principles

- **8-byte fixed instruction format**: Simple decoding, cache-friendly
- **Register-based VM**: Each slot is 8 bytes on the stack
- **Cooperative scheduling**: Fiber-based concurrency (goroutines)
- **Bounded module encoding**: VOB input and output are each limited to 134,217,728 bytes (128 MiB); oversized input is rejected before decoding, and oversized or unreservable output returns a serialization error

---

## 2. Instruction Format

### 2.1 Instruction Layout

```rust
/// 8-byte fixed format
#[repr(C)]
pub struct Instruction {
    pub op: u8,      // Opcode
    pub flags: u8,   // flags/variant
    pub a: u16,      // dst register or operand
    pub b: u16,      // src operand 0
    pub c: u16,      // src operand 1
}

impl Instruction {
    pub const fn new(op: Opcode, a: u16, b: u16, c: u16) -> Self;
    pub const fn with_flags(op: Opcode, flags: u8, a: u16, b: u16, c: u16) -> Self;
    
    /// Combine b, c into 32-bit immediate
    pub fn imm32(&self) -> i32 {
        ((self.b as u32) | ((self.c as u32) << 16)) as i32
    }
}
```

---

## 3. Opcode Definitions

```rust
#[repr(u8)]
pub enum Opcode {
    // === HINT: NOP / Loop metadata for JIT ===
    Hint = 0,

    // === LOAD: Load immediate/constant ===
    LoadInt,      // slots[a] = sign_extend(b | (c << 16))
    LoadConst,    // slots[a] = constants[b]

    // === COPY: Stack slot copy ===
    Copy,         // slots[a] = slots[b]
    CopyN,        // slots[a..a+c] = slots[b..b+c]

    // === SLOT: Stack dynamic indexing (for stack arrays) ===
    SlotGet,      // slots[a] = slots[b + slots[c]]
    SlotSet,      // slots[a + slots[b]] = slots[c]
    SlotGetN,     // multi-slot element load; SlotLayout owns width, flags is mirror/sentinel
    SlotSetN,     // multi-slot element store; SlotLayout owns width, flags is mirror/sentinel

    // === GLOBAL: Global variables ===
    GlobalGet,    // slots[a] = globals[b]
    GlobalGetN,   // slots[a..a+flags] = globals[b..], flags=n
    GlobalSet,    // globals[a] = slots[b]
    GlobalSetN,   // globals[a..] = slots[b..b+flags], flags=n

    // === PTR: Heap pointer operations ===
    PtrNew,       // slots[a] = alloc(slots[b]), b=meta_reg, c=slots, flags=reserved
    PtrGet,       // slots[a] = heap[slots[b]].offset[c]
    PtrSet,       // heap[slots[a]].offset[b] = slots[c]
    PtrGetN,      // slots[a..a+flags] = heap[slots[b]].offset[c..]
    PtrSetN,      // heap[slots[a]].offset[b..] = slots[c..c+flags]
    PtrAdd,       // a=dst, b=ptr, c=offset_slots (ptr arithmetic: dst = ptr + offset * 8)

    // === ARITH: Integer arithmetic ===
    AddI, SubI, MulI, DivI, DivU, ModI, ModU, NegI,

    // === ARITH: Float arithmetic ===
    AddF, SubF, MulF, DivF, NegF,

    // === CMP: Integer comparison ===
    EqI, NeI, LtI, LtU, LeI, LeU, GtI, GtU, GeI, GeU,

    // === CMP: Float comparison ===
    EqF, NeF, LtF, LeF, GtF, GeF,

    // === CMP: Reference comparison ===
    // Note: IsNil checks if value is 0 (for pointers/interfaces)
    // Note: EqRef/NeRef compare raw bits
    // Note: IfaceEq compares two interface values (2 slots each)
    EqRef, NeRef, IsNil,

    // === BIT: Bitwise operations ===
    And, Or, Xor, AndNot, Not,
    Shl,          // flags bit0 = RHS shift count is unsigned
    ShrS,         // arithmetic right shift; flags bit0 = RHS count is unsigned
    ShrU,         // logical right shift; flags bit0 = RHS count is unsigned

    // === LOGIC: Logical operations ===
    BoolNot,

    // === JUMP: Control flow ===
    Jump,         // pc += sign_extend(b | (c << 16))
    JumpIf,       // if slots[a]: pc += sign_extend(b | (c << 16))
    JumpIfNot,    // if !slots[a]: pc += sign_extend(b | (c << 16))

    // === CALL: Function calls ===
    Call,         // static function id=a|(flags<<16), args at b; c is a compact shape mirror
    CallExtern,   // a=dst, b=extern_id, c=args_start; flags is a compact arg-count mirror
    CallClosure,  // closure at a, args at b; CallLayout owns the complete shape
    CallIface,    // interface at a, args at b; CallIfaceLayout owns shape and method index
    Return,       // return values at a, ret_slots=b

    // === STR: String operations ===
    StrNew,       // slots[a] = constants[b] (string constant)
    StrLen,       // slots[a] = len(slots[b])
    StrIndex,     // slots[a] = slots[b][slots[c]]
    StrConcat,    // slots[a] = slots[b] + slots[c]
    StrSlice,     // slots[a] = str[lo:hi], b=str, c=params_start
    StrEq, StrNe, StrLt, StrLe, StrGt, StrGe,
    StrDecodeRune,// Decode UTF-8 rune at position: (rune, width) = decode(str, pos)

    // === ARRAY: Heap array operations ===
    ArrayNew,     // slots[a] = new array, b=meta_reg, c=len_reg, flags=elem_slots
    ArrayGet,     // slots[a..a+flags] = arr[idx], b=arr, c=idx, flags=elem_slots
    ArraySet,     // arr[idx] = slots[c..c+flags], a=arr, b=idx, flags=elem_slots
    ArrayAddr,    // a=dst, b=array_gcref, c=index, flags=elem_bytes

    // === SLICE: Slice operations ===
    SliceNew,     // slots[a] = make([]T, len, cap), b=meta_reg, c=params_start, flags=elem_slots
    SliceGet,     // slots[a..a+flags] = slice[idx], b=slice, c=idx, flags=elem_slots
    SliceSet,     // slice[idx] = slots[c..c+flags], a=slice, b=idx, flags=elem_slots
    SliceLen,     // slots[a] = len(slots[b])
    SliceCap,     // slots[a] = cap(slots[b])
    SliceSlice,   // slots[a] = slice[lo:hi:max], b=slice, c=params_start
    SliceAppend,  // slots[a] = append(slice, slots[c..c+flags]), b=slice, flags=elem_slots
    SliceAddr,    // a=dst, b=slice_reg, c=index, flags=elem_bytes

    // === MAP: Map operations ===
    MapNew,       // slots[a] = make(map), MapNew metadata owns key/value widths
    MapGet,       // slots[a..] = map[key], b=map, c=meta_and_key
    MapSet,       // map[key] = val, a=map, b=meta_and_key, c=val_start
    MapDelete,    // delete(map, key), a=map, b=meta_and_key
    MapLen,       // slots[a] = len(slots[b])
    MapIterInit,  // a=iter_slot (7 slots), b=map_reg
    MapIterNext,  // a=key_slot, b=iter_slot, flags=key_slots|(val_slots<<4)

    // === QUEUE: Channel/port operations ===
    QueueNew,     // make queue; QueueLayout owns elem width, flags bit7=port
    QueueSend,    // queue <- slots[b..], a=queue; QueueLayout owns elem width
    QueueRecv,    // slots[a..] = <-queue, b=queue; QueueLayout owns width, flags bit0=has_ok
    QueueClose,   // close(slots[a])
    QueueLen,     // slots[a] = len(slots[b])
    QueueCap,     // slots[a] = cap(slots[b])

    // === SELECT: Select statement ===
    SelectBegin,  // a=case_count, flags: bit0=has_default
    SelectSend,   // a=chan_reg, b=val_reg; QueueLayout owns elem width
    SelectRecv,   // a=dst_reg, b=chan_reg; QueueLayout owns width, flags bit0=has_ok
    SelectExec,   // slots[a] = chosen_index (-1=default)

    // === CLOSURE: Closure operations ===
    ClosureNew,   // slots[a] = new_closure(func_id=b|(flags<<16), capture_count=c)
    ClosureGet,   // slots[a] = slots[0].captures[b] (closure implicit in r0)

    // === GO / ISLAND / LOOP ===
    GoStart,      // start goroutine, a=func_id_low/closure_reg, b=args_start, c=arg_slots, flags=is_closure|func_id_high
    IslandNew,    // create island handle, a=dst
    GoIsland,     // a=island, b=closure, c=args_start; CallLayout owns the argument layout
    ForLoop,      // idx step/check/jump, a=idx, b=limit, c=offset, flags=signed/decrement

    // === DEFER: Defer and error handling ===
    DeferPush,    // push defer: closure=slots[a]
    ErrDeferPush, // push errdefer: closure=slots[a]
    Panic,        // panic(slots[a])
    Recover,      // slots[a] = recover()

    // === IFACE: Interface operations ===
    IfaceAssign,  // dst=slots[a..a+2], src=slots[b], c=const_idx, flags=value_kind
    IfaceAssert,  // a=dst, b=src_iface; IfaceAssertLayout owns kind/target/layout
    IfaceEq,      // a = (b == c), b,c are 2-slot interfaces

    // === CONV: Type conversion ===
    ConvI2F,      // integer slots[b] -> f64/f32 slots[a], flags select signedness/result
    ConvF2I,      // f64 slots[b] -> saturated integer slots[a], flags select target
    ConvF64F32,   // slots[a] = float32(slots[b])
    ConvF32F64,   // slots[a] = float64(slots[b])
    Trunc,        // a = truncate(b), flags = target width (signed/unsigned + width)

    // === DEBUG: Debug operations ===
    IndexCheck,   // panic if a >= b (unsigned comparison)
}
```

### 3.1 Container Creation Instructions

Container creation instructions use registers to pass `ValueMeta` (via `LoadConst`). Fixed-width operands carry layouts where they fit; metadata-owned instructions use their per-instruction layout:

| Instruction | Encoding | Description |
|-------------|----------|-------------|
| `PtrNew` | `a=dst, b=meta_reg, c=slots, flags=reserved` | `slots[b]` = ValueMeta, `c` = object size in slots |
| `ArrayNew` | `a=dst, b=meta_reg, c=len_reg, flags=elem_slots` | `slots[b]` = elem ValueMeta, `slots[c]` = length |
| `SliceNew` | `a=dst, b=meta_reg, c=params, flags=elem_slots` | `slots[b]` = elem ValueMeta, `slots[c]` = len, `slots[c+1]` = cap |
| `QueueNew` | `a=dst, b=meta_reg, c=cap_reg, flags=optional port bit` | `slots[b]` = packed element ValueMeta/RTTID, `slots[c]` = capacity, `QueueLayout.elem_layout` = exact logical slot layout |
| `MapNew` | `a=dst, b=type_info_reg, c=compact width mirror or zero` | `slots[b]` = packed type info; MapNew metadata carries exact `u16`-bounded layouts |
| `StrNew` | `a=dst, b=const_idx` | Load string from constants pool |
| `ClosureNew` | `a=dst, b=func_id, c=capture_count` | Create closure |

**Codegen examples**:
```
// make([]int, 10, 20)
LoadConst r1, <int_value_meta>   // ValueMeta for int
LoadInt   r2, 10                 // len
LoadInt   r3, 20                 // cap (must be r2+1)
SliceNew  r0, r1, r2, flags=1    // elem_slots=1

// make(map[string]Point)  where Point is 2 slots
LoadConst r1, <packed_u64>       // [key_meta:32 | val_meta:32]
MapNew    r0, r1, c=0x0102       // key_slots=1, val_slots=2
```

MapNew, MapGet, and MapSet consume exact per-instruction layouts. Their compact
width fields are validated when nonzero; zero width bits are the metadata
sentinel. MapGet retains its comma-ok bit in the sentinel form. SlotGetN and
SlotSetN follow the same rule with `SlotLayout`, so widths above 255 are never
truncated.

### 3.2 Numeric Conversion Instructions

`ConvI2F` uses these flag bits:

| Bits | Meaning |
|------|---------|
| `0` | Interpret the source as unsigned when set; signed when clear |
| `3` | Produce an `f32` bit pattern directly when set; produce `f64` when clear |

All other `ConvI2F` flag bits are reserved and the verifier rejects them. The
direct-`f32` form rounds the integer to `f32` once using IEEE-754
round-to-nearest, ties-to-even; it must not pass through `f64`, which could
double-round.

`ConvF2I` uses these flag bits:

| Bits | Meaning |
|------|---------|
| `0` | Unsigned target when set; signed target when clear |
| `1..2` | Target width: `00=64`, `01=8`, `10=16`, `11=32` |

Bits `3..7` are reserved and the verifier rejects them. `ConvF2I` truncates a
finite source toward zero and saturates directly to the encoded target width.
NaN becomes zero; an unsigned target also clamps negative values to zero.
Overflow and infinities clamp to the applicable target endpoint. The slot holds
the canonical result: signed values are sign-extended and unsigned values are
zero-extended to 64 bits.

`ConvF64F32` and `ConvF32F64` require zero flags. `ConvF64F32` performs one
IEEE-754 narrowing conversion; `ConvF32F64` is exact. `Trunc` uses bit `7` for
signed output and a byte width of `1`, `2`, or `4` in bits `0..6`; any other
width is rejected.

### 3.3 Shift-count Flags

`Shl`, `ShrS`, and `ShrU` use flag bit `0` to describe the declared signedness
of the RHS shift count. When it is set, the count is unsigned and can never
trigger a negative-shift panic. When it is clear, a negative signed count raises
a recoverable runtime panic. Raw counts of at least 64 produce zero for `Shl`
and `ShrU`; `ShrS` produces zero or `-1` according to the sign of its LHS. Flag
bits `1..7` are reserved and rejected by the verifier.

### 3.4 Built-in CallExtern Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `vo_print` | `(value, value_kind) -> ()` | debug output |

Other operations (container creation, string, closure, etc.) use dedicated Opcodes.

### 3.5 Call-site Layout Authority

Per-PC metadata carries the complete ABI for calls whose target or arity is
dynamic:

- `CallLayout` owns `CallClosure` argument/return layouts and `GoIsland`
  arguments.
- `CallIfaceLayout` owns the interface id, full-width `method_idx`, and
  argument/return layouts.
- `CallExternLayout` owns extern argument/return layouts; `ExternDef` must
  agree with them.
- `IfaceAssertLayout` owns assertion kind, full-width target id, and result
  layout. `has_ok` remains a semantic flag bit.

The packed `CallClosure`/`CallIface` shape, `CallIface.flags`,
`CallExtern.flags`, and `GoIsland.flags` are integrity mirrors when the value
fits. A mirror is zero when its authoritative value does not fit the compact
field. Zero also represents a genuine zero value. The verifier checks this
canonical mirror rule, while the VM and JIT consume metadata.

For `IfaceAssert`, the kind and result-width flag fields and operand `c` are
integrity mirrors. A result width above 31 uses zero; a target id above
`u16::MAX` uses `0xffff`. Exact zero/`0xffff` values intentionally collide
with those sentinels, and `IfaceAssertLayout` supplies the unique meaning.

---

## 4. Bytecode Module

### 4.1 Data Structures

```rust
/// Constant
pub enum Constant {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

/// Function definition
pub struct FunctionDef {
    pub name: String,
    pub param_count: u16,    // parameter count
    pub param_slots: u16,    // slots used by parameters
    pub local_slots: u16,    // local variable slots
    pub gc_scan_slots: u16,  // derived scan prefix for local GC roots
    pub ret_slots: u16,      // return value slots
    pub ret_slot_types: Vec<SlotType>,
    /// Receiver slots for methods (0 for functions, >0 for methods)
    pub recv_slots: u16,
    /// Number of GcRefs for heap-allocated named returns
    pub heap_ret_gcref_count: u16,
    /// Starting slot for heap-allocated named return GcRefs
    pub heap_ret_gcref_start: u16,
    /// Slot count for each heap-allocated named return
    pub heap_ret_slots: Vec<u16>,
    /// True if this is a closure (anonymous function) that expects closure ref in slot 0
    pub is_closure: bool,
    /// -1 when the function has no error result; otherwise the first slot of
    /// the final two-slot error interface, in the range 0..=65533.
    pub error_ret_slot: i32,
    pub has_defer: bool,
    pub has_calls: bool,
    pub has_call_extern: bool,
    pub code: Vec<Instruction>,
    pub jit_metadata: Vec<JitInstructionMetadata>,
    pub slot_types: Vec<SlotType>,  // for GC scanning
    pub borrowed_scan_slots_prefix: Vec<u16>,
    pub capture_types: Vec<TransferType>,
    pub capture_slot_types: Vec<SlotType>,
    pub param_types: Vec<TransferType>,
}

pub enum ParamShape {
    Exact { slots: u16 },
    CallSiteVariadic,
}

pub struct ReturnShape {
    pub slots: u16,
    pub kinds: Vec<ExtSlotKind>,
    pub slot_types: Vec<SlotType>,
}

// ReturnShape invariants:
// - non-empty kinds and slot_types must each have exactly slots entries.
// - Interface0 and Interface1 slot_types must appear as adjacent pairs.
// - Empty kinds or slot_types mean the caller/provider has no precise
//   per-slot metadata for that axis.
// - A CallExternLayout ret_layout containing GcRef or interface slots requires
//   a non-empty ReturnShape.slot_types vector that exactly matches the callsite
//   layout; slots-only ReturnShape may be used only for all-Value returns.
//
// param_kinds is the precise parameter kind list for externs that have it.
// When present it must match ParamShape::Exact slot count.
pub struct ExternDef {
    pub name: String,
    pub params: ParamShape,
    pub returns: ReturnShape,
    pub allowed_effects: ExternEffects,
    pub param_kinds: Vec<ExtSlotKind>,
}

/// Global variable
pub struct GlobalDef {
    pub name: String,
    pub slots: u16,
    pub value_kind: u8,
    pub meta_id: u32,  // 24-bit meta_id
    pub slot_types: Vec<SlotType>,
}

/// Struct metadata (physical layout only)
pub struct StructMeta {
    pub slot_types: Vec<SlotType>,
    pub fields: Vec<FieldMeta>,
    pub field_index: HashMap<String, usize>,
}

pub struct FieldMeta {
    pub name: String,
    pub offset: u16,
    pub slot_count: u16,
    pub type_info: ValueRttid,
    pub embedded: bool,
    pub tag: Option<String>,
}

/// Interface metadata
pub struct InterfaceMeta {
    pub name: String,
    pub method_names: Vec<String>,
    pub methods: Vec<InterfaceMethodMeta>,
}

pub struct InterfaceMethodMeta {
    pub name: String,
    pub signature_rttid: u32,
}

/// Method info for a named type
pub struct MethodInfo {
    pub func_id: u32,
    pub is_pointer_receiver: bool,
    pub receiver_is_iface_boxed: bool,
    pub signature_rttid: u32,
}

/// Named type metadata (for itab building and type assertion)
pub struct NamedTypeMeta {
    pub name: String,
    pub underlying_meta: ValueMeta,             // [struct_meta_id:24 | value_kind:8]
    pub underlying_rttid: ValueRttid,
    pub methods: BTreeMap<String, MethodInfo>,
}

/// Itab: interface method table (method_idx -> func_id)
pub struct Itab {
    pub methods: Vec<u32>,
}

/// Bytecode module
pub struct Module {
    pub name: String,
    pub struct_metas: Vec<StructMeta>,
    pub interface_metas: Vec<InterfaceMeta>,
    pub named_type_metas: Vec<NamedTypeMeta>,  // index = named_type_id
    pub runtime_types: Vec<RuntimeType>,       // rttid -> RuntimeType
    pub itabs: Vec<Itab>,                      // compile-time built itabs
    pub well_known: WellKnownTypes,            // pre-computed type IDs
    pub constants: Vec<Constant>,
    pub globals: Vec<GlobalDef>,
    pub functions: Vec<FunctionDef>,
    pub externs: Vec<ExternDef>,
    pub entry_func: u32,
    pub island_init_func: u32,
    pub debug_info: DebugInfo,
}
```

**Key Points**:
- `slot_types` must match `local_slots` length
- `jit_metadata` must have one entry per instruction. The common verifier checks shared layout shape; strict opcode/metadata pairing remains JIT-only policy.
- `NamedTypeMeta` used by VM to build itab at runtime and for type assertion
- Source package-level named types use the identity `<canonical-package>.<declaration>`.
  Function-local named types use
  `<canonical-package>@local:<lowercase-hex-UTF-8-source-file>:<canonical-decimal-u32-file-local-declaration-start>.<declaration>`.
  The source file component is one canonical portable `.vo` file name. `@` is
  excluded from source package paths and identifiers, reserving this namespace
  for the compiler. The verifier rejects malformed identities and duplicate
  complete identities. Source file names are at most 255 UTF-8 bytes and the
  complete named-type identity is at most 8192 bytes.
- Methods belong to named type, not receiver form (T and *T share one namespace)
- `is_pointer_receiver` determines method set: T only has value receiver methods, *T has all
- `underlying_meta` provides access to physical layout (struct_meta_id) and value_kind

### 4.2 Serialization

```rust
impl Module {
    /// Serialize to bytes
    pub fn serialize(&self) -> Result<Vec<u8>, SerializeError>;
    
    /// Deserialize from bytes
    pub fn deserialize(bytes: &[u8]) -> Result<Self, BytecodeError>;
}
```

**File format**:
```
Magic: "VOB" (3 bytes)
Version: u32 (currently 14)
struct_metas: [StructMeta]
interface_metas: [InterfaceMeta]
named_type_metas: [NamedTypeMeta]
itabs: [Itab]
runtime_types: [RuntimeType]
well_known: WellKnownTypes
constants: [Constant]
globals: [GlobalDef]
functions: [FunctionDef]
externs: [ExternDef]
entry_func: u32
island_init_func: u32
debug_info: DebugInfo
```

Version 14 includes the canonical function-local named-type identity contract.
Only the current VOB version is accepted, so artifacts carrying the older
package-only local-type encoding are rejected before module verification.

Note: iface_dispatch removed, itab built lazily at runtime.

---

## 5. VM Runtime Structures

### 5.1 Call Frame

```rust
#[derive(Debug, Clone, Copy)]
pub struct CallFrame {
    pub func_id: u32,
    pub pc: usize,
    pub bp: usize,       // base pointer (stack bottom)
    pub ret_reg: u16,    // return value destination
    pub ret_count: u16,
}
```

### 5.2 Defer Entry

```rust
#[derive(Debug, Clone)]
pub struct DeferEntry {
    pub frame_depth: usize,
    pub func_id: u32,
    pub closure: GcRef,
    pub args: GcRef,     // Arguments stored in a heap array
    pub arg_slots: u16,
    pub is_closure: bool,
    pub is_errdefer: bool,
}
```

### 5.3 Unwinding State (Defer/Panic)

Replaces `DeferState`. Unified state for defer execution during return or panic unwinding.

```rust
#[derive(Debug, Clone)]
pub struct UnwindingState {
    /// Defers remaining to execute (LIFO order)
    pub pending: Vec<DeferEntry>,
    /// Frame depth after the unwinding function was popped
    pub target_depth: usize,
    /// What kind of unwinding is in progress
    pub kind: UnwindingKind,
}

#[derive(Debug, Clone)]
pub enum UnwindingKind {
    Return {
        return_kind: PendingReturnKind,
        caller_ret_reg: u16,
        caller_ret_count: usize,
    },
    Panic {
        heap_gcrefs: Option<Vec<u64>>,
        slots_per_ref: Vec<usize>,
        caller_ret_reg: u16,
        caller_ret_count: usize,
    },
}

#[derive(Debug, Clone)]
pub enum PendingReturnKind {
    None,
    Stack { vals: Vec<u64>, slot_types: Vec<SlotType> },
    Heap { gcrefs: Vec<u64>, slots_per_ref: Vec<usize> },
}
```

### 5.4 Fiber

```rust
#[derive(Debug)]
pub struct Fiber {
    pub id: u32,
    pub status: FiberStatus,
    pub stack: Vec<u64>,
    pub frames: Vec<CallFrame>,
    pub defer_stack: Vec<DeferEntry>,
    pub unwinding: Option<UnwindingState>,
    pub select_state: Option<SelectState>,
    pub panic_state: Option<PanicState>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FiberStatus {
    Running,
    Suspended,
    Dead,
}
```

### 5.5 Panic State

```rust
#[derive(Debug, Clone)]
pub enum PanicState {
    /// Recoverable panic (can be caught by recover)
    /// Stores full interface{} value: (slot0, slot1)
    Recoverable(u64, u64),
    /// Fatal panic (internal VM/JIT errors)
    Fatal(String),
}
```

### 5.6 Scheduler

```rust
pub struct Scheduler {
    pub fibers: Vec<Fiber>,
    pub ready_queue: VecDeque<u32>,
    pub current: Option<u32>,
    
    /// Trampoline fibers for JIT->VM calls
    pub trampoline_fibers: Vec<Fiber>,
    pub trampoline_free_slots: Vec<u32>,
}
```

### 5.7 VM

```rust
pub struct Vm {
    /// JIT manager (optional)
    pub jit_mgr: Option<JitManager>,
    pub module: Option<Module>,
    pub scheduler: Scheduler,
    pub state: VmState,
}

/// VM mutable state (borrowed independently)
pub struct VmState {
    pub gc: Gc,
    pub globals: Vec<u64>,
    pub itab_cache: ItabCache,
    pub extern_registry: ExternRegistry,
    pub program_args: Vec<String>,
}
```

---

## 6. Instruction Execution Semantics

### 6.1 Function Call (Call)

```rust
Opcode::Call => {
    // a/flags encode func_id, b=arg_start; FunctionDef owns the shape.
    let func_id = u32::from(a) | (u32::from(flags) << 16);
    let func = &module.functions[func_id as usize];
    let arg_slots = func.param_slots;
    let ret_slots = func.ret_slots;
    
    // Copy args (avoid overwrite after frame switch)
    let args: Vec<u64> = (0..arg_slots).map(|i| self.read_reg(fiber_id, b + i)).collect();
    
    // Push new frame
    fiber.push_frame(func_id, func.local_slots, b, ret_slots);
    
    // Write args to new frame
    for (i, arg) in args.into_iter().enumerate() {
        fiber.write_reg(i as u16, arg);
    }
}
```

### 6.2 Closure Call (CallClosure)

```rust
Opcode::CallClosure => {
    // a=closure_reg, b=arg_start; CallLayout is authoritative.
    let CallLayout { arg_layout, ret_layout } = metadata_at(pc);
    let arg_slots = arg_layout.len();
    let ret_slots = ret_layout.len();
    let closure = self.read_reg(fiber_id, a) as GcRef;
    let func_id = closure::func_id(closure);
    let func = &module.functions[func_id as usize];
    
    // Copy args
    let args: Vec<u64> = (0..arg_slots).map(|i| self.read_reg(fiber_id, b + i)).collect();
    
    // Push new frame
    fiber.push_frame(func_id, func.local_slots, b, ret_slots);
    
    // Key: closure as r0
    fiber.write_reg(0, closure as u64);
    
    // Parameters start from r1
    for (i, arg) in args.into_iter().enumerate() {
        fiber.write_reg((i + 1) as u16, arg);
    }
}
```

### 6.3 Interface Call (CallIface)

```rust
Opcode::CallIface => {
    // a=iface_reg, b=arg_start; CallIfaceLayout is authoritative.
    let CallIfaceLayout { method_idx, arg_layout, ret_layout, .. } = metadata_at(pc);
    let arg_slots = arg_layout.len();
    let ret_slots = ret_layout.len();
    
    let slot0 = self.read_reg(fiber_id, inst.a);
    let slot1 = self.read_reg(fiber_id, inst.a + 1);
    
    // Extract itab_id from slot0 high 32 bits
    let itab_id = (slot0 >> 32) as u32;
    
    // Direct lookup, O(1)
    let func_id = self.itabs[itab_id as usize].methods[method_idx as usize];
    
    // Call like regular function, receiver is slot1
    // ...
}
```

### 6.4 Extern and Island Calls

```rust
Opcode::CallExtern => {
    // a=return_start, b=extern_id, c=arg_start.
    let CallExternLayout { arg_layout, ret_layout } = metadata_at(pc);
    verify_u8_mirror(flags, arg_layout.len());
    verify_extern_shape(module.externs[b as usize], &arg_layout, &ret_layout);
    invoke_extern(b, &slots[c..c + arg_layout.len()], &mut slots[a..a + ret_layout.len()]);
}

Opcode::GoIsland => {
    // a=island, b=closure, c=arg_start.
    let CallLayout { arg_layout, ret_layout } = metadata_at(pc);
    assert(ret_layout.is_empty());
    verify_u8_mirror(flags, arg_layout.len());
    transfer_and_spawn(slots[a], slots[b], &slots[c..c + arg_layout.len()]);
}
```

Both paths use metadata for zero, 255, 256, and all other `u16`-bounded slot
counts. Compact flags only detect malformed bytecode.

### 6.5 Interface Assertion (IfaceAssert)

```rust
Opcode::IfaceAssert => {
    let IfaceAssertLayout { assert_kind, target_id, result_layout } = metadata_at(pc);
    let has_ok = ((flags >> 2) & 1) != 0;
    verify_iface_assert_mirrors(flags, c, assert_kind, target_id, result_layout.len());

    let matches = match assert_kind {
        0 => unpack_rttid(slots[b]) == target_id,
        1 => satisfies_interface(slots[b], slots[b + 1], target_id),
        _ => unreachable!(),
    };
    materialize_assertion(a, matches, has_ok, &result_layout);
}
```

The VM and JIT pass the metadata `target_id` to runtime helpers. In particular,
targets 65535 and 65536 share the encoded `c=0xffff` mirror and remain distinct.

### 6.6 Itab Cache Management

```rust
impl Vm {
    /// Get or create itab for (named_type_id, iface_meta_id) tuple
    fn get_or_create_itab(&mut self, named_type_id: u32, iface_meta_id: u32) -> u32 {
        let key = (named_type_id, iface_meta_id);
        
        // Check cache
        if let Some(&itab_id) = self.itab_cache.get(&key) {
            return itab_id;
        }
        
        // Build new itab
        let itab = self.build_itab(named_type_id, iface_meta_id);
        let itab_id = self.itabs.len() as u32;
        self.itabs.push(itab);
        self.itab_cache.insert(key, itab_id);
        
        itab_id
    }
    
    fn build_itab(&self, named_type_id: u32, iface_meta_id: u32) -> Itab {
        let module = self.module.as_ref().unwrap();
        let named_type = &module.named_type_metas[named_type_id as usize];
        let iface_meta = &module.interface_metas[iface_meta_id as usize];
        
        // Method set check done at compile time
        let methods: Vec<u32> = iface_meta.method_names.iter()
            .map(|name| {
                let info = named_type.methods.get(name).expect("method not found");
                info.func_id
            })
            .collect();
        
        Itab { methods }
    }
}
```

### 6.7 Return Implementation (Unified Unwinding)

Return and Panic are now handled by a unified unwinding mechanism (`UnwindingState`).

```rust
Opcode::Return => {
    // a=ret_start, b=ret_count
    if fiber.is_direct_defer_context() {
        // Return from a defer function: pop frame and continue unwinding
        exec::unwind::handle_return_defer_returned(fiber, &mut vm.state, module);
    } else {
        // Normal return: start unwinding (execute defers if any)
        exec::unwind::handle_return(fiber, &mut vm.state, module, inst);
    }
}
```

**Unwinding Logic**:
1.  **Initial Return**:
    *   Store return values (Stack or Heap kind).
    *   Collect current frame's defers (LIFO).
    *   Filter `errdefer`: Only keep if function returns error (checked via `ret_vals`).
    *   Create `UnwindingState` with `kind=Return`.
    *   Pop current frame.
    *   If defers exist, execute the first one.
    *   If no defers, write return values to caller and continue.

2.  **Defer Execution**:
    *   Defer functions run in a new frame (depth = target_depth + 1).
    *   When defer returns, `handle_return_defer_returned` is called.
    *   It checks if more defers are pending.
    *   If yes, execute next.
    *   If no, finish unwinding (write return values for Return kind, or continue panic for Panic kind).

### 6.8 Defer Implementation

```rust
Opcode::DeferPush | Opcode::ErrDeferPush => {
    // a=closure_reg
    // Logic: Create DeferEntry, push to fiber.defer_stack
    // Arguments are captured/copied to heap if necessary
}
```

### 6.9 GC Root Scanning

GC scanning must cover:
1.  **Global Variables**: `vm.state.globals`.
2.  **Fiber Stacks**: Scan slots based on `SlotType`.
3.  **Defer Stack**: `fiber.defer_stack` (closures and args).
4.  **Unwinding State**: `fiber.unwinding` (pending defers, stored return values, panic state).
5.  **Panic State**: `fiber.panic_state` (panic value).
6.  **Trampoline Fibers**: Scan their stacks and states.


### 6.10 Queue Operations

```rust
Opcode::QueueSend => {
    // chan <- slots[b..b+elem_slots], a=chan
    let chan = slots[a] as GcRef;
    let elem_slots = queue_layout(pc).elem_layout.len();
    let value: Box<[u64]> = slots[b..b+elem_slots].into();
    let cap = channel::capacity(chan);
    
    match channel::get_state(chan).try_send(value.clone(), cap) {
        SendResult::DirectSend(receiver_id) => {
            scheduler.wake_fiber(FiberId::from_raw(receiver_id));
        }
        SendResult::Buffered => {}
        SendResult::WouldBlock => {
            channel::get_state(chan).register_sender(fiber_id, value);
            return ExecResult::Yield;
        }
        SendResult::Closed => return ExecResult::Panic("send on closed channel"),
    }
}

Opcode::QueueRecv => {
    // slots[a..] = <-chan, b=chan; flags bit 0 is has_ok
    let chan = slots[b] as GcRef;
    let elem_slots = queue_layout(pc).elem_layout.len();
    let has_ok = (flags & 1) != 0;
    
    let (result, value) = channel::get_state(chan).try_recv();
    match result {
        RecvResult::Success(woke_sender) => {
            let val = value.unwrap();
            for (i, &v) in val.iter().enumerate() { slots[a + i] = v; }
            if has_ok { slots[a + elem_slots] = 1; }
            if let Some(id) = woke_sender { scheduler.wake_fiber(FiberId::from_raw(id)); }
        }
        RecvResult::WouldBlock => {
            channel::get_state(chan).register_receiver(fiber_id);
            return ExecResult::Yield;
        }
        RecvResult::Closed => {
            for i in 0..elem_slots { slots[a + i] = 0; }
            if has_ok { slots[a + elem_slots] = 0; }
        }
    }
}

Opcode::QueueClose => {
    let state = channel::get_state(slots[a] as GcRef);
    state.close();
    for id in state.take_waiting_receivers() { scheduler.wake_fiber(FiberId::from_raw(id)); }
    for (id, _) in state.take_waiting_senders() { scheduler.wake_fiber(FiberId::from_raw(id)); }
}
```

### 6.11 GoStart Implementation

```rust
Opcode::GoStart => {
    // a=func_id_low/closure_reg, b=args_start, c=arg_slots, flags=is_closure|func_id_high
    // 1. Resolve function ID or closure
    // 2. Create new fiber
    // 3. Copy arguments to new fiber's stack
    // 4. Spawn fiber (add to ready queue)
}

Opcode::Yield => {
    return ExecResult::Yield;
}
```

### 6.12 Panic Unwinding

```rust
Opcode::Panic => {
    // a=panic_val_slot
    // 1. Set fiber.panic_state
    // 2. Start unwinding (UnwindingKind::Panic)
    // 3. Execute defers
    // 4. If recover() called, switch to UnwindingKind::Return
}

Opcode::Recover => {
    // a=dst_slot
    // 1. Check if in direct defer context (fiber.is_direct_defer_context())
    // 2. If yes, take panic value, write to dst, and switch unwinding mode
    // 3. If no, write nil to dst
}
```

**Panic Unwinding Flow**:
1. Set `panic_value`, execute all defers in current frame (LIFO)
2. `Recover` called in defer -> take `panic_value` and clear it, stop unwinding
3. All defers done but still have `panic_value` -> pop frame, continue unwinding
4. Stack empty but still have `panic_value` -> Fiber terminates

### 6.13 Select Implementation

**Data Structures**:

```rust
enum SelectCase {
    Send { chan_reg: u16, val_reg: u16, elem_slots: u16 },
    Recv { dst_reg: u16, chan_reg: u16, elem_slots: u16, has_ok: bool },
}

struct SelectState {
    cases: Vec<SelectCase>,
    has_default: bool,
    woken_index: Option<usize>,  // which case woke us
}
```

**Execution Flow**:

```rust
Opcode::SelectBegin => {
    fiber.select_state = Some(SelectState {
        cases: Vec::with_capacity(a as usize),
        has_default: (flags & 1) != 0,
        woken_index: None,
    });
}

Opcode::SelectSend => {
    let state = fiber.select_state.as_mut().unwrap();
    state.cases.push(SelectCase::Send {
        chan_reg: a, val_reg: b,
        elem_slots: queue_layout(pc).elem_layout.len() as u16,
    });
}

Opcode::SelectRecv => {
    let state = fiber.select_state.as_mut().unwrap();
    state.cases.push(SelectCase::Recv {
        dst_reg: a, chan_reg: b,
        elem_slots: queue_layout(pc).elem_layout.len() as u16,
        has_ok: (flags & 1) != 0,
    });
}

Opcode::SelectExec => {
    let state = fiber.select_state.as_mut().unwrap();
    
    // 1. After wakeup, execute selected case
    if let Some(idx) = state.woken_index.take() {
        execute_case(fiber, &state.cases[idx], slots);
        slots[a] = idx as u64;
        fiber.select_state = None;
        return ExecResult::Continue;
    }
    
    // 2. Try all cases (random order), collect ready ones
    let ready: Vec<usize> = state.cases.iter().enumerate()
        .filter(|(_, c)| case_is_ready(c, slots))
        .map(|(i, _)| i).collect();
    
    // 3. If any ready, randomly pick one to execute
    if !ready.is_empty() {
        let idx = ready[random() % ready.len()];
        execute_case(fiber, &state.cases[idx], slots);
        slots[a] = idx as u64;
        fiber.select_state = None;
        return ExecResult::Continue;
    }
    
    // 4. Has default, return -1
    if state.has_default {
        slots[a] = -1i64 as u64;
        fiber.select_state = None;
        return ExecResult::Continue;
    }
    
    // 5. All blocked, register on all channel wait queues
    for (i, case) in state.cases.iter().enumerate() {
        register_select_wait(fiber.id, i, case, slots);
    }
    return ExecResult::Yield;
}
```

**Wakeup Handling**:

```rust
fn wake_select(fiber_id: FiberId, case_index: usize) {
    let fiber = scheduler.get_fiber(fiber_id);
    let state = fiber.select_state.as_mut().unwrap();
    
    // Mark wakeup source
    state.woken_index = Some(case_index);
    
    // Unregister from other channels
    for (i, case) in state.cases.iter().enumerate() {
        if i != case_index {
            unregister_select_wait(fiber_id, case);
        }
    }
    
    scheduler.wake_fiber(FiberId::from_raw(fiber_id));
}
```
