# Synchronous JIT Design

## 1. Overview

### Goals

1. Implement JIT compilation for hot functions using Cranelift
2. Keep JIT simple by only supporting synchronous operations
3. Single-tier JIT for simplicity (inlining deferred)
4. Minimal changes to existing VM

### Key Decisions

- **Synchronous JIT**: JIT functions execute synchronously within VM Fiber context
- **No separate goroutine model**: No corosensei, no stackful coroutines
- **Async operations excluded**: Functions with defer/recover/go/channel/select are not JIT-compiled
- **Compile-time iterator expansion**: All for-range loops expanded at compile time
- **Single-tier JIT**: Interpreter → JIT (Cranelift optimized, no inlining initially)
- **No deoptimization needed**: Vo is statically typed, no type speculation
- **No OSR**: Complexity not justified for edge cases
- **Restricted safepoints**: GC only at loop back-edges and function calls
- **GcRef spill to stack**: All GcRef variables spilled to stack slots for simple stack maps
- **Zero-copy args/ret**: JIT args/ret pointers point directly to VM stack

## 2. JIT Compilation

### Architecture

```
Tier 0: vo-vm Interpreter
        │
        │ call_count >= 100 OR loop_back_edge >= 1000
        ▼
JIT: Cranelift (optimized, no inlining)
        │
        │ Future: add inlining based on profiling data
        ▼
JIT + Inlining (optional, deferred)
```

### Performance Expectations

| Tier | vs Interpreter | Compilation Cost | Trigger |
|------|----------------|------------------|----------|
| Interpreter | 1x | 0 | — |
| JIT | 15-30x | ~2-5ms/function | call ≥100 or loop ≥1000 |
| JIT + Inlining | 30-80x | ~10-50ms/function | Future |

### Why No Deoptimization

Vo is statically typed. Unlike JavaScript/Python:
- `x + y` type is known at compile time (no type guards needed)
- No hidden class changes
- Interface dispatch uses inline cache, not speculation

### Why No OSR (On-Stack Replacement)

OSR solves "single-call function with hot loop" (e.g., `main()` with billion iterations).

Complexity:
- Multiple entry points per function
- Frame state capture and mapping
- Platform-specific trampolines
- ~1000+ lines of code

Workaround: Extract hot loops into separate functions.

### Hot Detection

Hot detection is done **in the VM interpreter**, not in bytecode. No bytecode modifications needed.

```rust
pub struct HotCounter {
    call_counts: HashMap<u32, u32>,      // func_id -> call count
    loop_counts: HashMap<u32, u32>,      // func_id -> back-edge count
}

impl HotCounter {
    /// Called in VM's exec_call
    pub fn record_call(&mut self, func_id: u32) -> bool {
        let count = self.call_counts.entry(func_id).or_insert(0);
        *count += 1;
        *count == 100  // trigger JIT compilation
    }
    
    /// Called in VM's exec_jump when target < current_pc (back-edge)
    pub fn record_loop_back_edge(&mut self, func_id: u32) -> bool {
        let count = self.loop_counts.entry(func_id).or_insert(0);
        *count += 1;
        *count == 1000 && self.call_counts.get(&func_id).copied().unwrap_or(0) < 100
    }
}
```

## 3. JIT Support Matrix

### Supported

| Feature | Notes |
|---------|-------|
| Arithmetic, comparison, bitwise ops | Direct Cranelift mapping |
| Branching, jumps | Direct mapping |
| Function calls | Both VM and JIT targets |
| Stack variable access | Cranelift stack slots |
| Heap variable access | GcRef load/store |
| Heap allocation | Call out to `vo_gc_alloc` |
| for-range array/slice/int | Compile-time expansion |
| for-range map/string | Compile-time expansion + runtime helper |
| panic() | Set flag, return to VM |
| Method calls, interface dispatch | Call out to runtime |
| Write barrier | Must inline |

### Not Supported

| Feature | Reason |
|---------|--------|
| defer | Unwind semantics require VM management |
| recover() | Panic state managed by VM |
| go statement | Goroutine creation is VM-only |
| channel send/recv/close | May block, requires yield |
| select | Channel multiplexing requires scheduler |
| for-range channel | Blocking iteration |

### Function-Level Decision

```rust
fn can_jit(func: &FunctionDef) -> bool {
    !func.has_defer
    && !func.has_recover
    && !func.has_go_stmt
    && !func.has_channel_op  // send, recv, close
    && !func.has_select
}
```

## 4. Architecture

```
vo-runtime-core (shared)
├── gc.rs              # GC implementation
├── objects/           # Channel, Closure, Map, etc.
└── jit_api.rs         # extern "C" functions for JIT

vo-vm (unchanged except iterator removal)
├── bytecode.rs
├── instruction.rs
├── fiber.rs           # Remove iter_stack
├── scheduler.rs
└── exec/

vo-codegen-vm (main changes)
└── lower/             # Expand all for-range at compile time

vo-jit (new)
├── translate.rs       # bytecode → Cranelift IR
├── compile.rs         # Cranelift → native code
├── cache.rs           # JIT code cache
└── trampoline.rs      # VM ↔ JIT bridge
```

## 5. For-Range Compile-Time Expansion

All for-range loops are expanded at **codegen** time (vo-codegen-vm). Both VM and JIT execute the same expanded bytecode. No runtime iterator state, no special iterator instructions.

This simplifies the VM significantly:
- No `Iterator` enum or `iter_stack`
- No `IterBegin/IterNext/IterEnd` opcodes
- For-range is just regular `Jump` + `JumpIf` + `ArrayGet`/`ChanRecv`/etc.

### Array/Slice

```go
// Source
for i, v := range arr { body }

// Expanded bytecode
__len := len(arr)
__idx := 0
loop:
    if __idx >= __len { goto end }
    i := __idx
    v := arr[__idx]
    body
    __idx++
    goto loop
end:
```

### Int Range

```go
// Source
for i := 0; i < n; i++ { body }

// Already a simple loop, no expansion needed
```

### Map

```go
// Source
for k, v := range m { body }

// Expanded bytecode
__cursor := 0  // local variable
loop:
    __k, __v, __ok := vo_map_iter_next(m, &__cursor)
    if !__ok { goto end }
    k := __k
    v := __v
    body
    goto loop
end:
```

Uses IndexMap for O(1) index-based access. Cursor is just `0..len`.

### String

```go
// Source
for i, r := range s { body }

// Expanded bytecode
__pos := 0  // byte position, local variable
loop:
    if __pos >= len(s) { goto end }
    i := __pos
    r, __width := vo_decode_rune(s, __pos)
    body
    __pos += __width
    goto loop
end:
```

### Channel

```go
// Source
for v := range ch { body }

// Expanded bytecode (uses existing ChanRecv)
loop:
    v, ok := <-ch  // ChanRecv with ok flag
    if !ok { goto end }
    body
    goto loop
end:
```

Function containing channel range will not be JIT-compiled (due to blocking ChanRecv).

## 6. VM Changes

### Remove Entirely

| Item | Location |
|------|----------|
| `Iterator` enum | fiber.rs |
| `Fiber.iter_stack` | fiber.rs |
| `Opcode::IterBegin` | instruction.rs |
| `Opcode::IterNext` | instruction.rs |
| `Opcode::IterEnd` | instruction.rs |
| `exec_iter_*` functions | exec/iter.rs |

All for-range loops are now compile-time expanded. No runtime iterator state needed.

## 7. Runtime API (vo-runtime-core/src/jit_api.rs)

```rust
/// JIT function signature
pub type JitFunc = extern "C" fn(
    ctx: *mut JitContext,
    args: *const u64,
    ret: *mut u64,
) -> JitResult;

#[repr(C)]
pub struct JitContext {
    pub gc: *mut Gc,
    pub gc_is_marking: *const bool,  // For inline write barrier check
    pub globals: *mut u64,
    pub panic_flag: *mut bool,
    pub safepoint_flag: *const bool, // GC requests pause
    pub vm: *mut c_void,             // Opaque, cast in trampoline
    pub fiber: *mut c_void,          // Opaque, cast in trampoline
}

#[repr(C)]
pub enum JitResult {
    Ok,
    Panic,
}

// GC allocation
#[no_mangle]
pub extern "C" fn vo_gc_alloc(gc: *mut Gc, slots: u32, meta: u32) -> u64;

// Write barrier - inline fast path in JIT, call slow path when marking
// JIT generates: if (*gc).is_marking { vo_gc_write_barrier_slow(...) } else { *field = val }
#[no_mangle]
pub extern "C" fn vo_gc_write_barrier_slow(gc: *mut Gc, field: *mut u64, old_val: u64);

// Map iteration (cursor is local variable, not runtime state)
#[no_mangle]
pub extern "C" fn vo_map_iter_next(
    map: u64,
    cursor: *mut u64,
    key: *mut u64,
    val: *mut u64,
) -> bool;

// String iteration
#[no_mangle]
pub extern "C" fn vo_decode_rune(s: u64, pos: u64, rune: *mut i32) -> u64; // returns next_pos

// Call VM function from JIT
#[no_mangle]
pub extern "C" fn vo_call_vm(
    ctx: *mut JitContext,
    func_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult;
```

// Panic
#[no_mangle]
pub extern "C" fn vo_panic(ctx: *mut JitContext, msg: u64);

// Safepoint - called when safepoint_flag is set
#[no_mangle]
pub extern "C" fn vo_gc_safepoint(ctx: *mut JitContext);
```

## 8. GC Integration

### Safepoint Strategy

GC only runs at **restricted safepoints** (not arbitrary allocation points):
- Loop back-edges
- Function call/return boundaries

This simplifies implementation while remaining compatible with incremental GC.

```rust
// JIT generates at loop back-edge:
loop_header:
    if *ctx.safepoint_flag {
        vo_gc_safepoint(ctx);  // May trigger GC
    }
    // ... loop body ...
    goto loop_header
```

### Stack Map

Precise stack maps record which stack slots contain GcRefs at each safepoint:

```rust
pub struct StackMapEntry {
    pub pc_offset: u32,           // Relative to function start
    pub gc_ref_slots: Vec<i32>,   // Offsets from frame pointer
}

pub struct CompiledFunction {
    pub code: *const u8,
    pub code_size: usize,
    pub stack_maps: Vec<StackMapEntry>,
}
```

During GC, scan JIT frames using stack maps:

```rust
fn scan_jit_stack(gc: &mut Gc, jit_frames: &[JitFrame], cache: &JitCache) {
    for frame in jit_frames {
        let func = cache.get(frame.func_id);
        if let Some(map) = func.find_stack_map(frame.pc_offset()) {
            for &slot_offset in &map.gc_ref_slots {
                let gc_ref = frame.read_slot(slot_offset);
                if gc_ref != 0 {
                    gc.mark(GcRef::from_raw(gc_ref));
                }
            }
        }
    }
}
```

### GcRef Spill Strategy

**All GcRef variables are spilled to Cranelift stack slots** (not kept in registers across safepoints).

This simplifies stack map generation:
- Stack map is static per function (list of stack slot offsets)
- No need to track register liveness at each safepoint
- Performance cost: ~10-15% (acceptable for simplicity)

```rust
struct GcRefTracker {
    /// Maps local variable index to stack slot (for GcRef types only)
    gc_ref_slots: HashMap<u32, StackSlot>,
}

impl GcRefTracker {
    fn allocate_gc_ref(&mut self, builder: &mut FunctionBuilder, var_idx: u32) -> StackSlot {
        let slot = builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            8,  // 64-bit GcRef
        ));
        self.gc_ref_slots.insert(var_idx, slot);
        slot
    }
    
    fn get_gc_ref_slots(&self) -> Vec<i32> {
        // Return all slot offsets for stack map
        self.gc_ref_slots.values().map(|s| s.offset()).collect()
    }
}
```

### Write Barrier (Inline Fast Path)

JIT generates inline code for write barrier:

```rust
// JIT-generated pseudo-code for pointer field write
fn write_ptr_field(gc: *mut Gc, obj: GcRef, offset: usize, new_val: u64) {
    let field = obj.field_ptr(offset);
    if unsafe { (*gc).is_marking } {
        let old_val = unsafe { *field };
        vo_gc_write_barrier_slow(gc, field, old_val);
    }
    unsafe { *field = new_val; }
}
```

Fast path (not marking) = 1 memory read + 1 branch + 1 write.
Slow path called only during GC marking phase.

## 9. JIT Execution Model

### VM Calls JIT

```rust
impl Vm {
    fn exec_call(&mut self, fiber: &mut Fiber, func_id: u32, ...) -> ExecResult {
        if let Some(jit_func) = self.jit_cache.get(func_id) {
            let ctx = JitContext {
                gc: &mut self.state.gc,
                globals: self.state.globals.as_mut_ptr(),
                panic_flag: &mut fiber.panic_flag,
            };
            match jit_func(&ctx, args_ptr, ret_ptr) {
                JitResult::Ok => ExecResult::Continue,
                JitResult::Panic => ExecResult::Panic,
            }
        } else {
            // Normal VM interpretation
            self.push_frame(func_id, ...);
            ExecResult::Continue
        }
    }
}
```

### JIT Calls VM (via trampoline)

```rust
#[no_mangle]
pub extern "C" fn vo_call_vm(
    ctx: *mut JitContext,
    func_id: u32,
    args: *const u64,
    ret: *mut u64,
) -> JitResult;
```

### Panic Handling

JIT functions check panic flag after calls:

```rust
// JIT-generated code (pseudo)
fn jit_function(ctx: *mut JitContext, args: *const u64, ret: *mut u64) -> JitResult {
    // ... computation ...
    
    // After any call that may panic:
    vo_call_vm(ctx, some_func_id, call_args, call_ret);
    if *(*ctx).panic_flag {
        return JitResult::Panic;
    }
    
    // ... continue ...
    JitResult::Ok
}
```

## 10. JIT Compiler Structure

```rust
pub struct JitCompiler {
    module: cranelift_jit::JITModule,
    cache: HashMap<u32, *const u8>,  // func_id -> native code
}

impl JitCompiler {
    pub fn compile(&mut self, func_id: u32, func: &FunctionDef) -> *const u8 {
        let mut ctx = self.module.make_context();
        let mut builder = FunctionBuilder::new(&mut ctx.func, ...);
        
        // Translate each bytecode instruction
        for inst in &func.code {
            self.translate_inst(&mut builder, inst);
        }
        
        builder.finalize();
        let id = self.module.declare_function(...)?;
        self.module.define_function(id, &mut ctx)?;
        self.module.finalize_definitions();
        
        self.module.get_finalized_function(id)
    }
    
    fn translate_inst(&self, builder: &mut FunctionBuilder, inst: &Instruction) {
        match inst.opcode() {
            Opcode::AddI => {
                let a = builder.use_var(inst.a());
                let b = builder.use_var(inst.b());
                let result = builder.ins().iadd(a, b);
                builder.def_var(inst.a(), result);
            }
            Opcode::Call => {
                // Generate call + panic check
            }
            // ... ~50 opcodes to translate
        }
    }
}
```

## 11. Implementation Steps

Note: Steps 1-4 (for-range expansion, iterator removal) are already completed.

### Phase 1: Basic JIT (no GC, no calls)

| Order | Task | Status |
|-------|------|--------|
| 1 | vo-codegen-vm: Expand array/slice/int for-range | ✅ Done |
| 2 | vo-codegen-vm: Expand map/string for-range | ✅ Done |
| 3 | vo-vm: Remove Iterator enum | ✅ Done |
| 4 | vo-vm: Remove IterBegin/IterNext/IterEnd opcodes | ✅ Done |
| 5 | vo-jit: Create crate structure | Pending |
| 6 | vo-jit: Compile `return constant` | Pending |
| 7 | vo-jit: Arithmetic, comparison, branch, local vars | Pending |
| 8 | vo-jit: Heap variable read/write (no GC) | Pending |

### Phase 2: VM ↔ JIT Integration

| Order | Task | Status |
|-------|------|--------|
| 9 | vo-jit: VM → JIT trampoline | Pending |
| 10 | vo-jit: JIT → VM trampoline (vo_call_vm) | Pending |
| 11 | vo-vm: Add hot counter in exec_call/exec_jump | Pending |
| 12 | vo-vm: Add JIT cache + dispatch in Call | Pending |

### Phase 3: GC Integration

| Order | Task | Status |
|-------|------|--------|
| 13 | vo-jit: GcRef spill + stack map generation | Pending |
| 14 | vo-jit: Safepoint + vo_gc_safepoint | Pending |
| 15 | vo-jit: Write barrier (inline fast path) | Pending |
| 16 | vo-runtime-core: GC stack map scanning | Pending |

### Phase 4: Optimization (Optional)

| Order | Task | Status |
|-------|------|--------|
| 17 | vo-jit: Inlining | Deferred |

## 12. File Changes

| File | Action |
|------|--------|
| `vo-jit/Cargo.toml` | Add: new crate with cranelift dependencies |
| `vo-jit/src/lib.rs` | Add: JitCompiler, JitCache |
| `vo-jit/src/compiler.rs` | Add: bytecode → Cranelift translation |
| `vo-jit/src/gc_tracking.rs` | Add: GcRef spill, stack map generation |
| `vo-runtime-core/src/jit_api.rs` | Add: JitContext, extern "C" API |
| `vo-runtime-core/src/gc.rs` | Modify: add stack map scanning |
| `vo-vm/src/hot_counter.rs` | Add: call/loop counting |
| `vo-vm/src/vm.rs` | Modify: add JIT cache, hot counter, dispatch |

## 13. Summary

### Core Approach

**Synchronous JIT with restricted safepoints**: JIT functions execute synchronously within VM Fiber context. GC only triggers at specific points (loop back-edges, function calls), avoiding the complexity of arbitrary-point GC while remaining compatible with incremental GC.

### Single-Tier JIT (Initially)

```
Interpreter (1x) → JIT (15-30x) → [Future: JIT + Inlining (30-80x)]
```

- **JIT**: Cranelift with optimizations, triggered at 100 calls or 1000 loop iterations
- **Inlining**: Deferred; add later based on profiling data

### What We Don't Do (and Why)

| Feature | Decision | Rationale |
|---------|----------|----------|
| Tiered JIT (T1/T2) | Single tier initially | Simpler; add tiers if compile time becomes issue |
| Deoptimization | Not needed | Static types → no type speculation |
| OSR | Not implemented | ~1000 LoC for edge case; workaround: extract hot loops |
| Arbitrary-point GC | Restricted to safepoints | Simpler stack map, compatible with incremental GC |
| JIT for async ops | Excluded | defer/go/channel need VM scheduler |
| GcRef in registers | Spill to stack | Simpler stack map; ~10-15% perf cost acceptable |

### GC Integration

- **GcRef spill**: All GcRef variables spilled to stack slots (not in registers across safepoints)
- **Static stack map**: One stack map per function (list of GcRef slot offsets)
- **Safepoints**: Loop back-edges and function calls check `safepoint_flag`
- **Write barrier**: Inline fast path (check `is_marking`), slow path for actual marking

### One-Line Summary

**A simplified single-tier JIT for statically-typed languages: Cranelift backend, GcRef spill for simple stack maps, restricted safepoints for GC, no deopt/OSR since static types eliminate the need.**
