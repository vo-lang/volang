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

### 1.1 Core Principles

- **8-byte fixed instruction format**: Simple decoding, cache-friendly
- **Register-based VM**: Each slot is 8 bytes on the stack
- **Cooperative scheduling**: Fiber-based concurrency (goroutines)

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
    SlotGetN,     // slots[a..a+flags] = slots[b + slots[c]*flags..]
    SlotSetN,     // slots[a + slots[b]*flags..] = slots[c..c+flags]

    // === GLOBAL: Global variables ===
    GlobalGet,    // slots[a] = globals[b]
    GlobalGetN,   // slots[a..a+flags] = globals[b..], flags=n
    GlobalSet,    // globals[a] = slots[b]
    GlobalSetN,   // globals[a..] = slots[b..b+flags], flags=n

    // === PTR: Heap pointer operations ===
    PtrNew,       // slots[a] = alloc(slots[b]), b=meta_reg, flags=slots
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
    And, Or, Xor, AndNot, Not, Shl, ShrS, ShrU,

    // === LOGIC: Logical operations ===
    BoolNot,

    // === JUMP: Control flow ===
    Jump,         // pc += sign_extend(b | (c << 16))
    JumpIf,       // if slots[a]: pc += sign_extend(b | (c << 16))
    JumpIfNot,    // if !slots[a]: pc += sign_extend(b | (c << 16))

    // === CALL: Function calls ===
    Call,         // call functions[a|(flags<<16)], args at b, c=(arg_slots<<8|ret_slots)
    CallExtern,   // call externs[a|(flags<<16)], args at b, c=(arg_slots<<8|ret_slots)
    CallClosure,  // call slots[a], args at b, c=(arg_slots<<8|ret_slots)
    CallIface,    // call iface at a, args at b, c=(arg_slots<<8|ret_slots), flags=method_idx
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
    MapNew,       // slots[a] = make(map), b=type_info_reg, c=(key_slots<<8)|val_slots
    MapGet,       // slots[a..] = map[key], b=map, c=meta_and_key
    MapSet,       // map[key] = val, a=map, b=meta_and_key, c=val_start
    MapDelete,    // delete(map, key), a=map, b=meta_and_key
    MapLen,       // slots[a] = len(slots[b])
    MapIterInit,  // a=iter_slot (7 slots), b=map_reg
    MapIterNext,  // a=key_slot, b=iter_slot, flags=key_slots|(val_slots<<4)

    // === CHAN: Channel operations ===
    ChanNew,      // slots[a] = make(chan T, cap), b=meta_reg, c=cap_reg, flags=elem_slots
    ChanSend,     // chan <- slots[b..b+flags], a=chan, flags=elem_slots
    ChanRecv,     // slots[a..] = <-chan, b=chan, flags=(elem_slots<<1)|has_ok
    ChanClose,    // close(slots[a])
    ChanLen,      // slots[a] = len(slots[b])
    ChanCap,      // slots[a] = cap(slots[b])

    // === SELECT: Select statement ===
    SelectBegin,  // a=case_count, flags: bit0=has_default
    SelectSend,   // a=chan_reg, b=val_reg, flags=elem_slots
    SelectRecv,   // a=dst_reg, b=chan_reg, flags=(elem_slots<<1)|has_ok
    SelectExec,   // slots[a] = chosen_index (-1=default)

    // === CLOSURE: Closure operations ===
    ClosureNew,   // slots[a] = new_closure(func_id=b|(flags<<16), capture_count=c)
    ClosureGet,   // slots[a] = slots[0].captures[b] (closure implicit in r0)
    ClosureSet,   // slots[0].captures[a] = slots[b] (closure implicit in r0)

    // === GO: Goroutine ===
    GoStart,      // start goroutine, a=func_id_low/closure_reg, b=args_start, c=arg_slots, flags=is_closure|func_id_high

    // === DEFER: Defer and error handling ===
    DeferPush,    // push defer: closure=slots[a]
    ErrDeferPush, // push errdefer: closure=slots[a]
    Panic,        // panic(slots[a])
    Recover,      // slots[a] = recover()

    // === IFACE: Interface operations ===
    IfaceAssign,  // dst=slots[a..a+2], src=slots[b], c=const_idx, flags=value_kind
    IfaceAssert,  // a=dst, b=src_iface, c=target_id
    IfaceEq,      // a = (b == c), b,c are 2-slot interfaces

    // === CONV: Type conversion ===
    ConvI2F,      // slots[a] = float64(slots[b])
    ConvF2I,      // slots[a] = int64(slots[b])
    ConvF64F32,   // slots[a] = float32(slots[b])
    ConvF32F64,   // slots[a] = float64(slots[b])
    Trunc,        // a = truncate(b), flags = target width (signed/unsigned + width)

    // === DEBUG: Debug operations ===
    IndexCheck,   // panic if a >= b (unsigned comparison)
}
```

### 3.1 Container Creation Instructions

All container creation instructions use registers to pass `ValueMeta` (via `LoadConst`), and `flags`/`c` to pass `elem_slots`:

| Instruction | Encoding | Description |
|-------------|----------|-------------|
| `PtrNew` | `a=dst, b=meta_reg, flags=slots` | `slots[b]` = ValueMeta, `flags` = object size in slots |
| `ArrayNew` | `a=dst, b=meta_reg, c=len_reg, flags=elem_slots` | `slots[b]` = elem ValueMeta, `slots[c]` = length |
| `SliceNew` | `a=dst, b=meta_reg, c=params, flags=elem_slots` | `slots[b]` = elem ValueMeta, `slots[c]` = len, `slots[c+1]` = cap |
| `ChanNew` | `a=dst, b=meta_reg, c=cap_reg, flags=elem_slots` | `slots[b]` = elem ValueMeta, `slots[c]` = capacity |
| `MapNew` | `a=dst, b=type_info_reg, c=(key_slots<<8)\|val_slots` | `slots[b]` = packed type info |
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

### 3.2 Built-in CallExtern Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `vo_print` | `(value, value_kind) -> ()` | debug output |

Other operations (container creation, string, closure, etc.) use dedicated Opcodes.

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
    pub ret_slots: u16,      // return value slots
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
    pub code: Vec<Instruction>,
    pub slot_types: Vec<SlotType>,  // for GC scanning
}

/// External function
pub struct ExternDef {
    pub name: String,
    pub param_slots: u16,
    pub ret_slots: u16,
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
    pub signature_rttid: u32,
}

/// Named type metadata (for itab building and type assertion)
pub struct NamedTypeMeta {
    pub name: String,
    pub underlying_meta: ValueMeta,             // [struct_meta_id:24 | value_kind:8]
    pub methods: HashMap<String, MethodInfo>,
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
    pub debug_info: DebugInfo,
}
```

**Key Points**:
- `slot_types` must match `local_slots` length
- `NamedTypeMeta` used by VM to build itab at runtime and for type assertion
- Methods belong to named type, not receiver form (T and *T share one namespace)
- `is_pointer_receiver` determines method set: T only has value receiver methods, *T has all
- `underlying_meta` provides access to physical layout (struct_meta_id) and value_kind

### 4.2 Serialization

```rust
impl Module {
    /// Serialize to bytes
    pub fn serialize(&self) -> Vec<u8>;
    
    /// Deserialize from bytes
    pub fn deserialize(bytes: &[u8]) -> Result<Self, BytecodeError>;
}
```

**File format**:
```
Magic: "VOB" (3 bytes)
Version: u32
struct_metas: [StructMeta]
interface_metas: [InterfaceMeta]
named_type_metas: [NamedTypeMeta]
constants: [Constant]
globals: [GlobalDef]
functions: [FunctionDef]
externs: [ExternDef]
entry_func: u32
```

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
    // a=func_id, b=arg_start, c=arg_slots, flags=ret_slots
    let func = &module.functions[a as usize];
    
    // Copy args (avoid overwrite after frame switch)
    let args: Vec<u64> = (0..c).map(|i| self.read_reg(fiber_id, b + i)).collect();
    
    // Push new frame
    fiber.push_frame(a, func.local_slots, b, flags);
    
    // Write args to new frame
    for (i, arg) in args.into_iter().enumerate() {
        fiber.write_reg(i as u16, arg);
    }
}
```

### 6.2 Closure Call (CallClosure)

```rust
Opcode::CallClosure => {
    // a=closure_reg, b=arg_start, c=arg_slots, flags=ret_slots
    let closure = self.read_reg(fiber_id, a) as GcRef;
    let func_id = closure::func_id(closure);
    let func = &module.functions[func_id as usize];
    
    // Copy args
    let args: Vec<u64> = (0..c).map(|i| self.read_reg(fiber_id, b + i)).collect();
    
    // Push new frame
    fiber.push_frame(func_id, func.local_slots, b, flags);
    
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
    // a=iface_reg, b=arg_start, c=(arg_slots<<8|ret_slots), flags=method_idx
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    let method_idx = inst.flags as usize;
    
    let slot0 = self.read_reg(fiber_id, inst.a);
    let slot1 = self.read_reg(fiber_id, inst.a + 1);
    
    // Extract itab_id from slot0 high 32 bits
    let itab_id = (slot0 >> 32) as u32;
    
    // Direct lookup, O(1)
    let func_id = self.itabs[itab_id as usize].methods[method_idx];
    
    // Call like regular function, receiver is slot1
    // ...
}
```

### 6.4 Itab Cache Management

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

### 6.5 Return Implementation (Unified Unwinding)

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

### 6.6 Defer Implementation

```rust
Opcode::DeferPush | Opcode::ErrDeferPush => {
    // a=closure_reg
    // Logic: Create DeferEntry, push to fiber.defer_stack
    // Arguments are captured/copied to heap if necessary
}
```

### 6.7 GC Root Scanning

GC scanning must cover:
1.  **Global Variables**: `vm.state.globals`.
2.  **Fiber Stacks**: Scan slots based on `SlotType`.
3.  **Defer Stack**: `fiber.defer_stack` (closures and args).
4.  **Unwinding State**: `fiber.unwinding` (pending defers, stored return values, panic state).
5.  **Panic State**: `fiber.panic_state` (panic value).
6.  **Trampoline Fibers**: Scan their stacks and states.


### 6.8 Channel Operations

```rust
Opcode::ChanSend => {
    // chan <- slots[b..b+elem_slots], a=chan, flags=elem_slots
    let chan = slots[a] as GcRef;
    let elem_slots = flags as usize;
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

Opcode::ChanRecv => {
    // slots[a..] = <-chan, b=chan, flags=(elem_slots<<1)|has_ok
    let chan = slots[b] as GcRef;
    let elem_slots = (flags >> 1) as usize;
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

Opcode::ChanClose => {
    let state = channel::get_state(slots[a] as GcRef);
    state.close();
    for id in state.take_waiting_receivers() { scheduler.wake_fiber(FiberId::from_raw(id)); }
    for (id, _) in state.take_waiting_senders() { scheduler.wake_fiber(FiberId::from_raw(id)); }
}
```

### 6.9 GoStart Implementation

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

### 6.10 Panic Unwinding

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

### 6.11 Select Implementation

**Data Structures**:

```rust
enum SelectCase {
    Send { chan_reg: u16, val_reg: u16, elem_slots: u8 },
    Recv { dst_reg: u16, chan_reg: u16, elem_slots: u8, has_ok: bool },
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
        elem_slots: if flags == 0 { 1 } else { flags as u8 },
    });
}

Opcode::SelectRecv => {
    let state = fiber.select_state.as_mut().unwrap();
    state.cases.push(SelectCase::Recv {
        dst_reg: a, chan_reg: b,
        elem_slots: if (flags >> 1) == 0 { 1 } else { (flags >> 1) as u8 },
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
