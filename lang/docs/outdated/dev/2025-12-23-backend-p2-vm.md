# Backend P2: vo-vm

**Parent**: [2025-12-23-backend-rewrite-plan.md](2025-12-23-backend-rewrite-plan.md)  
**Status**: Not Started  
**Est. Modules**: 36  
**Depends On**: P1 (runtime-core)

## Overview

Defines bytecode format and VM interpreter. This is Vo's core execution engine.

**Core Principles**:
- 8-byte fixed instruction format
- Register-based VM (slot = 8 bytes on stack)
- Cooperative scheduling (Fiber)

## Module List

### 1. Instruction Format (instruction.rs)

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

### 2. Opcode Enum (instruction.rs)

```rust
#[repr(u8)]
pub enum Opcode {
    // === LOAD: Load immediate/constant ===
    Nop = 0,
    LoadNil,      // slots[a] = nil
    LoadTrue,     // slots[a] = true
    LoadFalse,    // slots[a] = false
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
    PtrClone,     // slots[a] = clone(slots[b]), slots from src header
    PtrGet,       // slots[a] = heap[slots[b]].offset[c]
    PtrSet,       // heap[slots[a]].offset[b] = slots[c]
    PtrGetN,      // slots[a..a+flags] = heap[slots[b]].offset[c..]
    PtrSetN,      // heap[slots[a]].offset[b..] = slots[c..c+flags]
    
    // === ARITH: Integer arithmetic ===
    AddI, SubI, MulI, DivI, ModI, NegI,
    
    // === ARITH: Float arithmetic ===
    AddF, SubF, MulF, DivF, NegF,
    
    // === CMP: Integer comparison ===
    EqI, NeI, LtI, LeI, GtI, GeI,
    
    // === CMP: Float comparison ===
    EqF, NeF, LtF, LeF, GtF, GeF,
    
    // === CMP: Reference comparison ===
    EqRef, NeRef, IsNil,
    
    // === BIT: Bitwise operations ===
    And, Or, Xor, Not, Shl, ShrS, ShrU,
    
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
    StrSlice,     // slots[a] = str[lo:hi], b=str, c=params_start, lo=slots[c], hi=slots[c+1]
    StrEq, StrNe, StrLt, StrLe, StrGt, StrGe,
    
    // === ARRAY: Heap array operations ===
    ArrayNew,     // slots[a] = new array, b=meta_reg, c=len_reg, flags=elem_slots
    ArrayGet,     // slots[a..a+flags] = arr[idx], b=arr, c=idx, flags=elem_slots
    ArraySet,     // arr[idx] = slots[c..c+flags], a=arr, b=idx, flags=elem_slots
    ArrayLen,     // slots[a] = len(slots[b])
    
    // === SLICE: Slice operations ===
    SliceNew,     // slots[a] = make([]T, len, cap), b=meta_reg, c=params_start, flags=elem_slots
    SliceGet,     // slots[a..a+flags] = slice[idx], b=slice, c=idx, flags=elem_slots
    SliceSet,     // slice[idx] = slots[c..c+flags], a=slice, b=idx, flags=elem_slots
    SliceLen,     // slots[a] = len(slots[b])
    SliceCap,     // slots[a] = cap(slots[b])
    SliceSlice,   // slots[a] = slice[lo:hi:max], b=slice, c=params_start, flags: bit0=has_max
                  // lo=slots[c], hi=slots[c+1], max=slots[c+2] if has_max else cap
    SliceAppend,  // slots[a] = append(slice, slots[c..c+flags]), b=slice, flags=elem_slots
    
    // === MAP: Map operations (uses meta slot for key/val slots) ===
    // Meta slot format: slots[x] = (key_slots << 16) | (val_slots << 1) | has_ok
    // Key starts at slots[x+1..x+1+key_slots]
    MapNew,       // slots[a] = make(map), b=type_info_reg, c=(key_slots<<8)|val_slots
    MapGet,       // slots[a..] = map[key], b=map, c=meta_and_key
                  // slots[c] = (key_slots<<16)|(val_slots<<1)|has_ok, key=slots[c+1..]
    MapSet,       // map[key] = val, a=map, b=meta_and_key, c=val_start
                  // slots[b] = (key_slots<<8)|val_slots, key=slots[b+1..]
    MapDelete,    // delete(map, key), a=map, b=meta_and_key
                  // slots[b] = key_slots, key=slots[b+1..]
    MapLen,       // slots[a] = len(slots[b])
    
    // === CHAN: Channel operations ===
    ChanNew,      // slots[a] = make(chan T, cap), b=meta_reg, c=cap_reg, flags=elem_slots
    ChanSend,     // chan <- slots[b..b+flags], a=chan, flags=elem_slots
    ChanRecv,     // slots[a..] = <-chan, b=chan, flags=(elem_slots<<1)|has_ok
    ChanClose,    // close(slots[a])
    
    // === SELECT: Select statement ===
    SelectBegin,  // a=case_count, flags: bit0=has_default
    SelectSend,   // a=chan_reg, b=val_reg, flags=elem_slots
    SelectRecv,   // a=dst_reg, b=chan_reg, flags=(elem_slots<<1)|has_ok
    SelectExec,   // slots[a] = chosen_index (-1=default)
    
    // === ITER: Iterator (for-range, uses meta slot) ===
    // Meta slot format: slots[a] = (key_slots << 8) | val_slots
    // Container at slots[a+1]
    IterBegin,    // a=meta_and_container, b=type
                  // slots[a] = (key_slots<<8)|val_slots, container=slots[a+1]
    IterNext,     // a=key_dst, b=val_dst, c=done_offset; slots count from IterState
    IterEnd,      // end iteration, pop iter_stack
    
    // === CLOSURE: Closure operations ===
    ClosureNew,   // slots[a] = new_closure(func_id=b|(flags<<16), capture_count=c)
    ClosureGet,   // slots[a] = slots[0].captures[b] (closure implicit in r0)
    ClosureSet,   // slots[0].captures[a] = slots[b] (closure implicit in r0)
    
    // === GO: Goroutine ===
    GoCall,       // go slots[a]() (0-arg closure, same as defer)
    Yield,
    
    // === DEFER: Defer and error handling ===
    DeferPush,    // push defer: closure=slots[a]
    ErrDeferPush, // push errdefer: closure=slots[a]
    Panic,        // panic(slots[a])
    Recover,      // slots[a] = recover()
    // Note: defer uses 0-arg closure, executed automatically on Return
    
    // === IFACE: Interface operations (interface uses slots[a] and slots[a+1]) ===
    // slot0 format: [itab_id:32 | named_type_id:24 | value_kind:8]
    // slot1: data (immediate value or GcRef)
    // nil check: value_kind == Void (same as Go: typed nil is NOT nil interface)
    IfaceAssign,  // dst=slots[a..a+2], src=slots[b], c=const_idx, flags=value_kind
                  // c points to Int64 constant:
                  //   - 具体类型: (named_type_id << 32) | itab_id，编译时已建 itab
                  //   - Interface 右值: iface_meta_id（高32位为0），运行时建 itab
                  // - Method set check done at compile time
                  // - Struct/Array: deep copy (ptr_clone)
                  // - Interface: copy slot0, deep copy slot1 if Struct/Array
                  // - Others: direct copy
    IfaceAssert,  // a=dst, b=src_iface, c=target_id
                  // flags = assert_kind | (has_ok << 2)
                  // assert_kind: 0=Primitive, 1=Named, 2=Interface
                  // c meaning: Primitive→value_kind, Named→named_type_id, Interface→iface_meta_id
    
    // === CONV: Type conversion ===
    ConvI2F,      // slots[a] = float64(slots[b])
    ConvF2I,      // slots[a] = int64(slots[b])
    ConvI32I64,   // slots[a] = int64(int32(slots[b]))
    ConvI64I32,   // slots[a] = int32(slots[b])
    
    // === DEBUG: Debug operations ===
    // Note: Print uses CallExtern
    // Note: assert uses JumpIf + CallExtern(print) + Panic
}
```


### 2.1 Container Creation Instructions

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

### 2.2 Built-in CallExtern Functions

| Function | Signature | Description |
|------|------|------|
| `vo_print` | `(value, value_kind) -> ()` | debug output |

Other operations (container creation, string, closure, etc.) use dedicated Opcodes.

### 3. Bytecode Module (bytecode.rs)

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
}

/// Bytecode module
pub struct Module {
    pub name: String,
    pub struct_metas: Vec<StructMeta>,
    pub interface_metas: Vec<InterfaceMeta>,
    pub named_type_metas: Vec<NamedTypeMeta>,  // index = named_type_id
    pub constants: Vec<Constant>,
    pub globals: Vec<GlobalDef>,
    pub functions: Vec<FunctionDef>,
    pub externs: Vec<ExternDef>,
    pub entry_func: u32,
}

/// Struct metadata (physical layout only)
pub struct StructMeta {
    pub size_slots: u16,
    pub slot_types: Vec<SlotType>,
    pub field_names: Vec<String>,
    pub field_offsets: Vec<u16>,
}

/// Interface metadata
pub struct InterfaceMeta {
    pub name: String,
    pub method_names: Vec<String>,  // ordered method names
}

/// Method info for a named type
pub struct MethodInfo {
    pub func_id: u32,
    pub is_pointer_receiver: bool,
}

/// Named type metadata (for itab building and type assertion)
pub struct NamedTypeMeta {
    pub name: String,
    pub underlying_meta: ValueMeta,             // [struct_meta_id:24 | value_kind:8]
    pub methods: HashMap<String, MethodInfo>,
}
```

**Key Points**:
- `slot_types` must match `local_slots` length
- `NamedTypeMeta` used by VM to build itab at runtime and for type assertion
- Methods belong to named type, not receiver form (T and *T share one namespace)
- `is_pointer_receiver` determines method set: T only has value receiver methods, *T has all
- `underlying_meta` provides access to physical layout (struct_meta_id) and value_kind

### 4. Serialization (bytecode.rs cont.)

```rust
impl Module {
    /// Serialize to bytes
    pub fn serialize(&self) -> Vec<u8>;
    
    /// Deserialize from bytes
    pub fn deserialize(bytes: &[u8]) -> Result<Self, BytecodeError>;
}

/// File format
/// Magic: "VOB" (3 bytes)
/// Version: u32
/// struct_metas: [StructMeta]
/// interface_metas: [InterfaceMeta]
/// named_type_metas: [NamedTypeMeta]
/// constants: [Constant]
/// globals: [GlobalDef]
/// functions: [FunctionDef]
/// externs: [ExternDef]
/// entry_func: u32
/// Note: iface_dispatch removed, itab built lazily at runtime
```

### 5. VM Structure (vm.rs)

```rust
/// Call frame
pub struct CallFrame {
    pub func_id: u32,
    pub pc: usize,
    pub bp: usize,       // base pointer (stack bottom)
    pub ret_reg: u16,    // return value destination
    pub ret_count: u16,
}

/// Defer entry - uses closure uniformly
pub struct DeferEntry {
    pub frame_depth: usize,
    pub closure: GcRef,      // 0-arg closure
    pub is_errdefer: bool,
}

/// Iterator
pub enum Iterator {
    /// Heap array/slice: arr=underlying array GcRef
    HeapArray { arr: GcRef, len: u32, elem_slots: u8, pos: u32 },
    /// Stack array: base_slot relative to frame bp
    StackArray { bp: usize, base_slot: u16, len: u32, elem_slots: u8, pos: u32 },
    Map { map: GcRef, pos: usize },
    String { s: GcRef, byte_pos: usize },
    IntRange { cur: i64, end: i64, step: i64 },
    Channel { ch: GcRef },  // for v := range ch
}

/// Defer execution state (stored during Return)
/// Go semantics: return value is determined before defer executes,
/// so we must store ret_vals before running defer closures.
pub struct DeferState {
    pub pending: Vec<DeferEntry>,  // pending defers (LIFO)
    pub ret_vals: Vec<u64>,        // stored return values
    pub caller_ret_reg: u16,
    pub caller_ret_count: usize,
    pub is_error_return: bool,     // for errdefer decision
}

/// Fiber (coroutine)
pub struct Fiber {
    pub id: u32,
    pub status: FiberStatus,
    pub stack: Vec<u64>,
    pub frames: Vec<CallFrame>,
    pub defer_stack: Vec<DeferEntry>,
    pub defer_state: Option<DeferState>,  // active during Return with pending defers
    pub iter_stack: Vec<Iterator>,
    pub select_state: Option<SelectState>,  // active during select
    pub panic_value: Option<GcRef>,
}

pub enum FiberStatus {
    Running,
    Suspended,
    Dead,
}

/// Scheduler
pub struct Scheduler {
    pub fibers: Vec<Fiber>,
    pub ready_queue: VecDeque<u32>,
    pub current: Option<u32>,
}

/// Itab - interface method table (lazy built)
pub struct Itab {
    pub methods: Vec<u32>,  // method_idx -> func_id
}
// No need to store meta_id - use slot0.value_meta.meta_id() directly

/// VM main structure
pub struct Vm {
    pub module: Option<Module>,
    pub gc: Gc,
    pub scheduler: Scheduler,
    pub globals: Vec<u64>,
    // Itab cache (lazy built)
    pub itab_cache: HashMap<(u32, u32), u32>,  // (named_type_id, iface_meta_id) -> itab_id
    pub itabs: Vec<Itab>,                       // itab_id -> Itab
}
```

### 6. VM Execution (vm.rs cont.)

```rust
impl Vm {
    pub fn new() -> Self;
    
    /// Load module
    pub fn load(&mut self, module: Module);
    
    /// Run entry function
    pub fn run(&mut self) -> Result<(), VmError>;
    
    /// Execute single instruction
    fn exec_instruction(&mut self, fiber_id: u32) -> ExecResult;
    
    /// Read/write registers
    fn read_reg(&self, fiber_id: u32, reg: u16) -> u64;
    fn write_reg(&mut self, fiber_id: u32, reg: u16, val: u64);
}

enum ExecResult {
    Continue,
    Return,
    Yield,
    Panic(GcRef),
}
```

### 7. Instruction Execution Details

#### 7.1 Function Call (Call)

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

#### 7.2 Closure Call (CallClosure)

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

#### 7.3 Interface Call (CallIface)

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
```

#### 7.3.1 Itab Cache Management

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

#### 7.4 Return Implementation

```rust
Opcode::Return => {
    // a=ret_start, b=ret_count
    let ret_start = inst.a as usize;
    let ret_count = inst.b as usize;
    
    // 1. Store return values temporarily
    let ret_vals: Vec<u64> = (0..ret_count)
        .map(|i| self.read_reg(fiber_id, (ret_start + i) as u16))
        .collect();
    
    // 2. Collect current frame's defers (LIFO order)
    let frame_depth = fiber.frames.len();
    let mut pending: Vec<DeferEntry> = fiber.defer_stack
        .drain_filter(|d| d.frame_depth == frame_depth)
        .collect();
    pending.reverse();  // LIFO
    
    // 3. Check for errdefer, if present determine error return at runtime
    // Note: compiler ensures functions with errdefer have error return value (interface, 2 slots)
    let has_errdefer = pending.iter().any(|d| d.is_errdefer);
    let is_error = if has_errdefer && ret_count >= 2 {
        // error is interface, check value_kind != Void
        let err_header = ret_vals[ret_count - 2];
        (err_header & 0xFF) != 0  // value_kind != Void
    } else {
        false
    };
    
    // 4. Filter errdefer
    if !is_error {
        pending.retain(|d| !d.is_errdefer);
    }
    
    // 5. If has defer, enter defer execution mode
    if !pending.is_empty() {
        let caller_frame = &fiber.frames[fiber.frames.len() - 2];
        fiber.defer_state = Some(DeferState {
            pending,
            ret_vals,
            caller_ret_reg: caller_frame.ret_reg,
            caller_ret_count: ret_count,
            is_error_return: is_error,
        });
        
        // Execute first defer closure
        let first = fiber.defer_state.as_mut().unwrap().pending.pop().unwrap();
        // CallClosure(first.closure, no args)
        // ...
    } else {
        // 6. No defer, return directly
        let caller_frame = fiber.frames.pop().unwrap();
        for (i, val) in ret_vals.into_iter().enumerate() {
            self.write_reg(fiber_id, caller_frame.ret_reg + i as u16, val);
        }
    }
}
```

#### 7.5 Defer Execution Flow

**Execution Flow**:
1. `DeferPush` / `ErrDeferPush`: save closure to `defer_stack`
2. On `Return`:
   - Collect current frame's defers (LIFO)
   - If errdefer exists, check if last return value is non-nil -> is_error
   - errdefer only executes when is_error=true
   - Execute each via CallClosure
3. After defer closure returns, continue with next
4. All done, write stored return values to caller

**Codegen Constraint**: `errdefer` can only be used in functions with error return value, otherwise compile error.

**Codegen Note**: `defer foo(x, y)` should be wrapped as:
```vo
// Generate a 0-arg closure, capturing x, y
closure := func() { foo(x, y) }
DeferPush closure
```

#### 7.6 GC Root Scanning

```rust
impl Vm {
    /// Scan all GC roots
    pub fn scan_roots(&self, gc: &mut Gc) {
        // 1. Scan global variables
        for (i, &val) in self.globals.iter().enumerate() {
            let def = &self.module.as_ref().unwrap().globals[i];
            if needs_gc(def.value_kind) && val != 0 {
                gc.mark_gray(val as GcRef);
            }
        }
        
        // 2. Scan all Fiber stacks
        for fiber in &self.scheduler.fibers {
            for frame in &fiber.frames {
                let func = &module.functions[frame.func_id as usize];
                for (i, &st) in func.slot_types.iter().enumerate() {
                    let slot_idx = frame.bp + i;
                    match st {
                        SlotType::GcRef => {
                            let val = fiber.stack[slot_idx];
                            if val != 0 { gc.mark_gray(val as GcRef); }
                        }
                        SlotType::Interface1 => {
                            // Dynamically check previous slot
                            let header = fiber.stack[slot_idx - 1];
                            if needs_gc(extract_value_kind(header)) {
                                let val = fiber.stack[slot_idx];
                                if val != 0 { gc.mark_gray(val as GcRef); }
                            }
                        }
                        _ => {}
                    }
                }
            }
            
            // 3. Scan defer_stack
            for entry in &fiber.defer_stack {
                gc.mark_gray(entry.closure);
            }
            
            // 4. Scan defer_state
            if let Some(state) = &fiber.defer_state {
                for entry in &state.pending {
                    gc.mark_gray(entry.closure);
                }
                // Scan stored return values - types from FunctionDef.slot_types
                let func_id = fiber.frames.last().unwrap().func_id;
                let func_def = &module.functions[func_id as usize];
                let ret_start = func_def.slot_types.len() - func_def.ret_slots as usize;
                for (i, &val) in state.ret_vals.iter().enumerate() {
                    if func_def.slot_types[ret_start + i] == SlotType::GcRef && val != 0 {
                        gc.mark_gray(val as GcRef);
                    }
                }
            }
            
            // 5. Scan iter_stack
            for iter in &fiber.iter_stack {
                match iter {
                    Iterator::HeapArray { arr, .. } => gc.mark_gray(*arr),
                    Iterator::StackArray { .. } => {} // no GcRef, scanned via stack slots
                    Iterator::Map { map, .. } => gc.mark_gray(*map),
                    Iterator::String { s, .. } => gc.mark_gray(*s),
                    Iterator::Channel { ch, .. } => gc.mark_gray(*ch),
                    Iterator::IntRange { .. } => {}
                }
            }
        }
    }
}
```

#### 7.7 Channel Operations Implementation

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

#### 7.8 GoCall Implementation

```rust
Opcode::GoCall => {
    // go slots[a]()  (0-arg closure)
    let closure = slots[a] as GcRef;
    let func_id = closure::func_id(closure);
    let func = &module.functions[func_id as usize];
    
    let mut new_fiber = Fiber::new(scheduler.next_fiber_id());
    new_fiber.push_frame(func_id, func.local_slots, 0, 0);
    new_fiber.write_reg(0, closure as u64);  // closure as r0
    scheduler.spawn(new_fiber);
}

Opcode::Yield => {
    return ExecResult::Yield;
}
```

#### 7.9 Panic Unwinding

```rust
Opcode::Panic => {
    fiber.panic_value = Some(slots[a] as GcRef);
    return ExecResult::Panic(fiber.panic_value.unwrap());
}

Opcode::Recover => {
    // Only valid in defer function
    slots[a] = fiber.panic_value.take().map(|v| v as u64).unwrap_or(0);
}
```

**Panic Unwinding**:
1. Set `panic_value`, execute all defers in current frame (LIFO)
2. `Recover` called in defer -> take `panic_value` and clear it, stop unwinding
3. All defers done but still have `panic_value` -> pop frame, continue unwinding
4. Stack empty but still have `panic_value` -> Fiber terminates

#### 7.10 Select Implementation

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
