# Vo VM Design

This document describes the Vo VM architecture, instruction set, and execution model.

## 1. Overview

Vo VM is a register-based bytecode interpreter.

### Design Principles

1. **Static typing**: Types determined at compile time, values carry no type tags
2. **Type-specialized instructions**: Each numeric type has dedicated instructions
3. **8-byte fixed instructions**: Simple, uniform, supports large register numbers
4. **Multi-slot values**: struct/interface can occupy multiple consecutive registers
5. **Coroutine support**: Fiber implements goroutine
6. **Incremental GC**: Tri-color mark-sweep

## 2. Instruction Format

### 8-Byte Fixed Format

```
┌────────┬────────┬────────────────┬────────────────┬────────────────┐
│ op (8) │flag(8) │    d (16)      │    s0 (16)     │    s1 (16)     │
└────────┴────────┴────────────────┴────────────────┴────────────────┘
                         8 bytes
```

```rust
struct Instruction {
    op: u8,       // Opcode
    flags: u8,    // Flags/variant
    d: u16,       // Destination register
    s0: u16,      // Source operand 0
    s1: u16,      // Source operand 1
}
```

### Registers

- 16-bit numbering, up to 65536 registers
- Register = stack slot, 8 bytes each
- `rN` = `stack[bp + N]`

## 3. Execution Model

### Fiber (Coroutine)

```rust
struct Fiber {
    id: FiberId,
    status: FiberStatus,      // Running, Suspended, Dead
    
    // Execution state
    stack: Vec<u64>,          // Value stack
    frames: Vec<CallFrame>,   // Call frame stack
    
    // Iterator stack (for range loops)
    iter_stack: Vec<Iterator>,
    
    // Defer stack
    defer_stack: Vec<DeferEntry>,
    
    // Panic state
    panic_value: Option<GcRef>,
}

struct CallFrame {
    func_id: FuncId,
    pc: usize,           // Program counter
    bp: usize,           // Base pointer
    ret_reg: u16,        // Return value destination
    ret_count: u16,      // Number of return values
}

enum FiberStatus {
    Running,
    Suspended,
    Dead,
}
```

### Scheduler

```rust
struct Scheduler {
    fibers: Vec<Fiber>,
    ready_queue: VecDeque<FiberId>,
    current: Option<FiberId>,
}
```

Cooperative scheduling. Fiber yields on:
- Channel blocking
- `yield` instruction
- `runtime.Gosched()`

### Iterator (for range loops)

Stored in Fiber's iter_stack (zero allocation):

```rust
enum Iterator {
    Slice { ref_: GcRef, pos: usize },
    Map { ref_: GcRef, pos: usize },
    String { ref_: GcRef, byte_pos: usize },
    IntRange { cur: i64, end: i64, step: i64 },
}
```

### Defer Entry

Stored in Fiber's defer_stack (zero allocation):

```rust
struct DeferEntry {
    frame_depth: usize,
    func_id: FuncId,
    arg_count: u8,
    args: [u64; 8],    // Fixed size, covers most cases
}
```

## 4. Instruction Set

### 4.1 Load/Store

```asm
LOAD_NIL      d              # d = nil
LOAD_BOOL     d, imm         # d = true/false
LOAD_INT      d, imm         # d = immediate (16-bit)
LOAD_CONST    d, idx         # d = constant_pool[idx]
MOV           d, s           # d = s (single slot)
MOV_N         d, s, n        # Copy n slots
```

### 4.2 Numeric Type Strategy

**Design Decision**: Unified 64-bit storage with minimal type-specialized instructions.

| Go Type | VM Storage | Arithmetic | Notes |
|---------|-----------|------------|-------|
| int8, int16, int32, int64, int | i64 | I64 instructions | Sign extension at compile time |
| uint8, uint16, uint32, uint64, uint | i64 | I64 + U64 for div/cmp | Zero extension at compile time |
| float32 | f32 bits in u64 | F32 instructions | Separate precision |
| float64 | f64 bits in u64 | F64 instructions | Native 64-bit |
| complex64, complex128 | 2 slots | Emulated | Real + Imag |

**Rationale**:
- All integer types fit in 64-bit register
- Add/Sub/Mul: Same binary result for signed/unsigned (2's complement)
- Div/Mod/Cmp: Need separate signed vs unsigned variants
- Float32 needs separate instructions due to different precision

### 4.3 Arithmetic Instructions

```asm
# Signed integer (i64) - used for all int types
ADD_I64       d, s0, s1
SUB_I64       d, s0, s1
MUL_I64       d, s0, s1
DIV_I64       d, s0, s1      # Signed division
MOD_I64       d, s0, s1      # Signed modulo
NEG_I64       d, s

# Unsigned integer - only for division/modulo
DIV_U64       d, s0, s1      # Unsigned division
MOD_U64       d, s0, s1      # Unsigned modulo

# Float64
ADD_F64       d, s0, s1
SUB_F64       d, s0, s1
MUL_F64       d, s0, s1
DIV_F64       d, s0, s1
NEG_F64       d, s

# Float32
ADD_F32       d, s0, s1
SUB_F32       d, s0, s1
MUL_F32       d, s0, s1
DIV_F32       d, s0, s1
NEG_F32       d, s
```

### 4.4 Comparison

```asm
# Signed integer comparison
EQ_I64        d, s0, s1      # d = (s0 == s1)
NE_I64        d, s0, s1
LT_I64        d, s0, s1      # Signed less than
LE_I64        d, s0, s1
GT_I64        d, s0, s1
GE_I64        d, s0, s1

# Unsigned integer comparison
LT_U64        d, s0, s1      # Unsigned less than
LE_U64        d, s0, s1
GT_U64        d, s0, s1
GE_U64        d, s0, s1

# Float comparison (works for both f32 and f64)
EQ_F64        d, s0, s1
NE_F64        d, s0, s1
LT_F64        d, s0, s1
LE_F64        d, s0, s1
GT_F64        d, s0, s1
GE_F64        d, s0, s1

# Reference equality
EQ_REF        d, s0, s1      # Address comparison
```

### 4.5 Bitwise

```asm
BAND          d, s0, s1
BOR           d, s0, s1
BXOR          d, s0, s1
BNOT          d, s
SHL           d, s0, s1
SHR           d, s0, s1      # Arithmetic right shift (signed)
USHR          d, s0, s1      # Logical right shift (unsigned)
```

### 4.6 Type Conversion

```asm
# Integer truncation/extension (compile-time for same-size, runtime for cross-size)
I64_TO_I32    d, s           # Truncate to 32-bit
I32_TO_I64    d, s           # Sign extend
U32_TO_I64    d, s           # Zero extend

# Float conversion
F32_TO_F64    d, s           # Widen
F64_TO_F32    d, s           # Narrow (may lose precision)

# Int-Float conversion
I64_TO_F64    d, s
F64_TO_I64    d, s
I64_TO_F32    d, s
F32_TO_I64    d, s
```

### 4.7 Control Flow

```asm
JUMP          offset         # PC += offset
JUMP_IF       s, offset      # if s then jump
JUMP_IF_NOT   s, offset      # if !s then jump
```

### 4.8 Function Call

Unified CALL instruction handles both Vo functions and extern functions:

```asm
CALL          callable, arg_start, arg_count, ret_start
CALL_METHOD   recv, method_idx, arg_start, arg_count, ret_start
RETURN        ret_start, ret_count
```

Callable types (determined at runtime):

```rust
enum Callable {
    VoFunc { func_id: FuncId },
    ExternFunc { extern_fn: ExternFn },
    Closure { func_id: FuncId, upvalues: Vec<GcRef> },
}

// Extern function signature
type ExternFn = fn(&mut VmContext, args: &[u64], ret: &mut [u64]);
```

CALL execution:

```rust
fn exec_call(vm: &mut Vm, callable: &Callable, args: &[u64], ret: &mut [u64]) {
    match callable {
        VoFunc { func_id } => {
            // Push new call frame, execute bytecode
        }
        ExternFunc { extern_fn } => {
            // Pause GC, call extern function directly
            vm.gc.pause();
            extern_fn(&mut vm.ctx, args, ret);
            vm.gc.resume();
        }
        Closure { func_id, upvalues } => {
            // Set up upvalues, then execute like VoFunc
        }
    }
}
```

### 4.9 Object Operations

Struct/array allocation depends on escape analysis (see `memory-model.md`).

**Stack allocation (non-escaping)**:
```asm
# No ALLOC needed - struct fields are consecutive registers
# Field access: direct register offset
MOV           r1, 42               # s.field0 = 42 (s starts at r0)
MOV_N         d, s, n              # Copy n slots (struct assignment)
```

**Heap allocation (escaping)**:
```asm
ALLOC         d, type_id, slots    # Allocate heap object
GET_FIELD     d, obj, idx          # d = obj.fields[idx] (obj is GcRef)
SET_FIELD     obj, idx, s          # obj.fields[idx] = s
```

Codegen decides stack vs heap at compile time based on escape analysis.

### 4.10 Array/Slice

```asm
ARRAY_GET     d, arr, idx
ARRAY_SET     arr, idx, s
ARRAY_LEN     d, arr

SLICE_GET     d, slice, idx
SLICE_SET     slice, idx, s
SLICE_LEN     d, slice
SLICE_CAP     d, slice
SLICE_MAKE    d, type_id, len, cap
SLICE_SLICE   d, s, lo, hi
SLICE_APPEND  d, slice, elem
```

### 4.11 String

```asm
STR_CONCAT    d, s0, s1
STR_LEN       d, s
STR_INDEX     d, s, idx
```

### 4.12 Map

```asm
MAP_MAKE      d, type_id, cap
MAP_GET       d, ok, map, key_start
MAP_SET       map, key_start, val_start
MAP_DELETE    map, key_start
MAP_LEN       d, map
```

### 4.13 Channel

```asm
CHAN_MAKE     d, type_id, cap
CHAN_SEND     chan, val_start    # May block
CHAN_RECV     d, chan            # May block
CHAN_CLOSE    chan
```

### 4.14 Range Iteration

```asm
ITER_PUSH     container, type    # Push iterator onto stack
ITER_NEXT     key, val           # Get next key-value pair
ITER_POP                         # Pop iterator from stack
JUMP_DONE     offset             # Jump if iteration complete
```

### 4.15 Goroutine

```asm
GO            func_id, arg_start, arg_count
YIELD
```

### 4.16 Defer/Panic/Recover

```asm
DEFER_PUSH    func_id, arg_start, arg_count
PANIC         val
RECOVER       d
```

### 4.17 Interface

```asm
IFACE_BOX     d, s, type_id      # Box: d = [type_id, value/ref]
IFACE_UNBOX   d, iface, type_id  # Unbox: type assertion
IFACE_TYPE    d, iface           # Get type_id
```

## 5. Garbage Collection

### Incremental Tri-Color Mark-Sweep

```
White: Not visited (potentially garbage)
Gray:  Visited, children not scanned
Black: Visited, children scanned
```

### Root Set

- All Fiber stacks
- All Fiber iter_stacks (container references in iterators)
- All Fiber defer_stacks (references in defer arguments)
- Global variables

### Scanning

```rust
fn scan(obj: &GcObject) {
    let meta = &TYPE_TABLE[obj.header.type_id];
    
    match meta.kind {
        Interface => {
            // Special handling: check type_id to determine if data is pointer
            let actual_type = obj.slots[0];
            let data = obj.slots[1];
            if TYPE_TABLE[actual_type].is_reference_type() {
                mark_grey(data as GcRef);
            }
        }
        _ => {
            // Normal types: scan by ptr_bitmap
            for (i, is_ptr) in meta.ptr_bitmap.iter().enumerate() {
                if *is_ptr {
                    mark_grey(obj.slots[i] as GcRef);
                }
            }
        }
    }
}
```

## 6. Bytecode File Format

```
┌─────────────────────────────────────────┐
│  Magic: "VOB" (4 bytes)                │
│  Version (4 bytes)                      │
├─────────────────────────────────────────┤
│  Type Table                             │
│    count: u32                           │
│    TypeMeta[]                           │
├─────────────────────────────────────────┤
│  Constant Pool                          │
│    count: u32                           │
│    Constant[]                           │
├─────────────────────────────────────────┤
│  Function Table                         │
│    count: u32                           │
│    FunctionDef[]                        │
├─────────────────────────────────────────┤
│  Entry Point (main func_id)             │
└─────────────────────────────────────────┘

FunctionDef:
  - name_idx: u32        (index into constant pool)
  - param_slots: u16     (total slots for parameters)
  - local_slots: u16     (total slots for locals)
  - code_len: u32
  - code: [Instruction]
```

## 7. Summary

| Component | Design |
|-----------|--------|
| Instruction format | 8-byte fixed |
| Instruction dispatch | Type-specialized |
| Registers | 16-bit numbering, stack slots |
| struct | Stack (inline) or Heap (GcRef), escape analysis decides |
| array | Stack (inline) or Heap (GcRef), escape analysis decides |
| interface | 2-slot inline [type_id, data] |
| Iterator | Fiber internal stack |
| defer | Fiber internal stack |
| Goroutine | Fiber + cooperative scheduling |
| GC | Incremental tri-color mark-sweep |
