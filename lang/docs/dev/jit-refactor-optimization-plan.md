# JIT Refactor & Optimization Plan

## Baseline Performance (2026-02-20)

```
Overall: Vo-JIT 5.47x vs C (Go=1.88x, LuaJIT=3.67x, Java=4.08x, Node=4.81x)

Key benchmarks:
  sum-array:          2.22x C  — tight loop, best case
  sieve:              1.78x C  — loop OSR, excellent
  fannkuch:           2.44x C  — loop + recursion, good
  binary-trees:       3.16x C  — alloc-heavy
  quicksort:          4.42x C  — recursion + array ops
  call-closure-mono:  5.08x Go — dynamic dispatch overhead
  call-iface-mono:    9.35x Go — iface dispatch overhead
  fibonacci:         11.77x C  — recursive call overhead
  nbody:             10.97x Go — float arithmetic + struct field access
  spectral-norm:      6.30x LuaJIT — float loops
```

## Architecture Overview

### Current Design

```
vo-jit/src/
├── lib.rs              JitCompiler, JitCache, JitFunc signature
├── translator.rs       IrEmitter trait, HelperFuncs, compute_memory_only_start
├── translate.rs        ~120 opcodes → Cranelift IR (shared by func+loop compilers)
├── func_compiler.rs    Full function compilation (FunctionCompiler)
├── loop_compiler.rs    Loop OSR compilation (LoopCompiler)
├── call_helpers.rs     Call emission: static/closure/iface/extern/IC
├── helpers.rs          HelperFuncIds, register_symbols, declare_helpers
├── intrinsics.rs       Math intrinsics (Sqrt, Floor, Ceil, Trunc, FMA)
└── loop_analysis.rs    Loop detection from HINT_LOOP instructions

vo-vm/src/vm/jit/
├── mod.rs              dispatch_jit_call, dispatch_loop_osr, handle_jit_result, materialize_jit_frames
├── context.rs          JitContextWrapper, build_jit_context
├── frame.rs            jit_push_frame, jit_pop_frame, jit_push_resume_point
└── callbacks/
    ├── closure_call.rs jit_prepare_closure_call, jit_prepare_iface_call
    ├── channel.rs      chan send/recv/close
    ├── port.rs         port send/recv/close
    ├── goroutine.rs    go_start, go_island
    ├── defer.rs        defer_push, recover
    ├── select.rs       select_begin/send/recv/exec
    ├── island.rs       island_new
    └── helpers.rs      shared callback utilities

vo-vm/src/vm/jit_mgr.rs  JitManager: hot tracking, compilation orchestration

vo-runtime/src/jit_api.rs JitContext, JitResult, DynCallIC, PreparedCall, all vo_* extern helpers
```

### Compilation Modes

1. **Full Function JIT**: hot function → Cranelift IR → native code. `(ctx, args, ret) -> JitResult`
2. **Loop OSR**: hot loop → Cranelift IR → native code. `(ctx, locals_ptr) -> JitResult`

### Call Dispatch Paths

| Call Type | Fast Path | Slow Path |
|-----------|-----------|-----------|
| **Static (Call)** | Direct FuncRef if callee compiled; otherwise jit_func_table[id] indirect | VM fallback via set_call_request + return Call |
| **Self-recursive** | Direct FuncRef, native stack args | Same as static non-OK |
| **Closure (CallClosure)** | IC hit → native stack, direct JIT call | IC miss → prepare_closure_call → PreparedCall |
| **Interface (CallIface)** | IC hit → native stack, direct JIT call | IC miss → prepare_iface_call → PreparedCall |
| **Extern (CallExtern)** | Intrinsics bypass FFI | vo_call_extern → jit_call_extern |
| **has_defer callee** | — (always VM) | emit_call_via_vm → return Call |

---

## Identified Optimization Opportunities

### O1. JitContext Caching (HIGH IMPACT, LOW RISK)

**Problem**: `build_jit_context()` rebuilds a ~450-byte JitContext from scratch for every JIT invocation (dispatch_jit_call, dispatch_loop_osr). Most fields (gc, globals, module, callbacks, func_table, etc.) are constant within a fiber's execution. Only 4 fields change per call: `stack_ptr`, `stack_cap`, `jit_bp`, `fiber_sp`.

**Current cost**: ~50 field assignments per JIT entry, plus Box allocation for JitOwnedState.

**Solution**: Cache JitContext on the Fiber. Rebuild only when VM state changes (module load, GC compaction). Per-call: only update the 4 dynamic fields + reset call/panic state.

**Estimated impact**: Eliminates ~90% of JitContext setup overhead. Matters most for short JIT functions called frequently (fibonacci, recursive-tree).

### O2. Eliminate ret Vec Allocation (HIGH IMPACT, LOW RISK)

**Problem**: `invoke_jit_and_handle()` allocates `let mut ret: Vec<u64> = vec![0u64; ret_slots.max(1)]` for EVERY JIT call. This is a heap allocation on the hot path.

**Solution**: Use a fixed-size stack buffer (e.g., `[u64; 16]`) for small ret_slots, only heap-allocate for large returns. Or better: write return values directly to caller's fiber.stack instead of a temporary buffer — the JIT already has `ret_ptr` and the VM copies from ret to fiber.stack anyway.

**Estimated impact**: Eliminates one malloc+free per JIT call. ~10-20ns per call.

### O3. Eliminate JitOwnedState Box Allocation (MEDIUM IMPACT, LOW RISK)

**Problem**: `build_jit_context()` allocates `Box::new(JitOwnedState { ... })` for panic_flag, is_user_panic, panic_msg pointers. This is a heap allocation per JIT entry.

**Solution**: Move JitOwnedState inline into cached JitContext (or into Fiber). The panic_flag etc. can be fields on Fiber or on the cached JitContext directly.

**Estimated impact**: Eliminates one malloc+free per JIT entry.

### O4. Unify FunctionCompiler and LoopCompiler Code Paths (MEDIUM IMPACT, ARCHITECTURE)

**Problem**: FunctionCompiler and LoopCompiler have significant code duplication:
- Both implement `IrEmitter` with nearly identical read_var/write_var/read_var_f64/write_var_f64
- Both implement `compile()` with similar scan_jump_targets → emit_prologue → translate loop
- Both have `emit_variable_spill()` / `store_vars_to_memory()` doing the same thing
- Both handle the same instruction set minus control flow differences

The duplication creates maintenance burden and makes it easy for bug fixes in one to miss the other.

**Solution**: Extract common state and methods into a shared `CompilerCore` struct that both FunctionCompiler and LoopCompiler delegate to. The IrEmitter methods (read_var, write_var, etc.) should live on CompilerCore. FunctionCompiler and LoopCompiler become thin wrappers that handle their specific prologue/epilogue and control flow differences.

**Estimated impact**: ~40% code reduction in vo-jit, easier maintenance. No runtime impact.

### O5. Reduce IR Bloat from Monomorphic IC (MEDIUM IMPACT, MEDIUM RISK)

**Problem**: emit_call_closure and emit_call_iface each generate ~200+ Cranelift IR instructions per call site due to the IC hit/miss/slot0/update logic. For functions with many dynamic calls, this leads to:
- Slow compilation (more IR → more time in regalloc)
- Poor instruction cache behavior at runtime
- Register pressure from many live values across IC branches

**Current per-callsite IR**: IC load → key compare → jit_ptr null check → branch → slot0 3-way dispatch → arg copy → ctx update → JIT call → result check → ctx restore → ... (IC miss path similarly complex)

**Solution**: Move the IC hit fast path into a single helper function call (`jit_ic_dispatch_closure`/`jit_ic_dispatch_iface`) that takes (ctx, closure_ref, ic_entry, user_args, arg_count, ret_ptr) and returns (result, is_hit). The helper does IC check + slot0 setup + ctx update + JIT call + ctx restore inline in Rust. JIT emits: 1 IC entry address computation + 1 helper call + branch on is_hit. IC miss path stays as-is (prepare callback).

This trades one additional function call on IC hit for dramatically smaller IR per callsite. Since the IC hit helper is the same code for every callsite, it stays hot in CPU icache.

**Estimated impact**: 
- Compilation time: ~30-50% faster for functions with many CallClosure/CallIface
- Runtime: Negligible impact on IC hit path (helper call vs inline is ~1-2ns, both L1-hot)
- Code size: ~60% smaller per dynamic callsite

### O6. Static Call Deferred Compilation (LOW IMPACT, LOW RISK)

**Problem**: emit_jit_call_with_fallback generates two complete code paths for every static Call:
1. Direct call path (callee compiled) + OK/non-OK handling
2. VM fallback path (callee not compiled) + set_call_request + return Call

When callee_func_ref is known at compile time (already compiled), the VM fallback path is dead code that still increases IR size and compilation time.

**Solution**: When callee_func_ref is Some (compile-time known callee), skip generating the VM fallback path entirely. The callee is guaranteed compiled, null check is unnecessary.

When callee_func_ref is None, the indirect path already handles both JIT and VM cases correctly.

**Estimated impact**: Modest IR reduction for functions calling already-compiled callees.

### O7. Inline Common Helper Operations (MEDIUM IMPACT, HIGH RISK)

**Problem**: Many frequent operations go through extern "C" function calls that could be partially inlined:
- `vo_str_len(s)`: loads GcRef header, reads len field → 2 loads. Currently a full call.
- `vo_slice_len(s)`: nil check + 1 load from slice struct. Currently a full call.
- `vo_chan_len(ch)`, `vo_map_len(m)`: similar simple patterns.
- `vo_gc_alloc(gc, meta, slots)`: most allocations hit the fast bump-allocator path.

**Solution**: Inline the fast paths:
- `str_len`: emit `if s==0 { 0 } else { load(s, STR_LEN_OFFSET) }` directly in IR
- `slice_len`/`slice_cap`: already done inline (see translate.rs slice_len/slice_cap using emit_nil_guarded_load)
- `gc_alloc`: inline bump allocation fast path, call slow path helper on overflow

The GC alloc inline is the highest-value optimization here. The bump allocator check is: `if gc.free_ptr + size <= gc.limit { result = gc.free_ptr; gc.free_ptr += size; } else { call slow_path }`. This eliminates the function call overhead for ~95% of allocations.

**Estimated impact**: 
- str_len inline: ~2-3ns per call saved
- gc_alloc inline: ~5-10ns per allocation saved, huge impact on alloc-heavy benchmarks (binary-trees)

### O8. Float Slot Optimization (LOW IMPACT, LOW RISK)

**Problem**: Every `read_var` / `write_var` checks `is_float_slot()` and potentially bitcasts. For functions with no float variables, this is wasted work. For functions with many float ops, the bitcast overhead adds up.

**Current**: `read_var` → if float, return as-is; if int slot but called from float context, bitcast. `read_var_f64` → if float slot, return as-is; if int slot, bitcast.

**Solution**: Already fairly well optimized with the dual read_var/read_var_f64 approach. The remaining optimization is to skip the `is_float_slot` check entirely when the function has no float slots (common for integer-heavy code). Add a `has_float_slots: bool` field computed once at function start.

**Estimated impact**: Negligible for most code. Minor improvement for tight integer loops.

### O9. HelperFuncs → Direct FuncRef Loading (LOW IMPACT, ARCHITECTURE)

**Problem**: HelperFuncs is a large struct (~80 Option<FuncRef> fields = ~1280 bytes) that is rebuilt via `get_helper_refs()` for every function compilation. Each helper is `module.declare_func_in_func()` which does a hash lookup.

**Solution**: Since helpers are module-global, pre-compute all FuncRefs once and reuse. The `declare_func_in_func` call is needed per `cranelift::Function` (new function context), but we can cache the FuncId → FuncRef mapping. Already partially done with `HelperFuncIds`, but the conversion to `HelperFuncs` (FuncRef) is per-compilation.

**Estimated impact**: Modest reduction in compilation time. No runtime impact.

---

## Implementation Plan

### Phase A: JitContext Caching + Allocation Elimination (O1 + O2 + O3)

**Goal**: Eliminate per-call overhead that dominates short-function benchmarks.

**Changes**:

1. **Move JitOwnedState into Fiber**: Add `panic_flag: bool`, `is_user_panic: bool`, `jit_panic_msg: InterfaceSlot` to Fiber struct. JitContext points to these directly. No more `Box<JitOwnedState>` per call.

2. **Cache JitContext on Vm**: Add `cached_jit_ctx: Option<JitContext>` to Vm. On first JIT call, build full context. On subsequent calls, only update dynamic fields:
   - `stack_ptr = fiber.stack_ptr()`
   - `stack_cap = fiber.stack.len()`
   - `jit_bp = jit_bp as u32`
   - `fiber_sp = fiber.sp as u32`
   - `fiber = fiber as *mut _`
   - Reset: `call_func_id=0, call_kind=0, is_error_return=0, ret_is_heap=0`
   - Reset: `panic_flag=false, is_user_panic=false`
   
   Invalidate cache when: module changes, GC moves (non-moving GC so never), extern_registry changes.

3. **Stack-buffer for ret**: Replace `vec![0u64; ret_slots.max(1)]` with `let mut ret_buf = [0u64; 16]; let ret = if ret_slots <= 16 { &mut ret_buf[..ret_slots.max(1)] } else { /* heap fallback */ }`.

**Risk**: Low. Functional semantics unchanged.
**Testing**: All existing tests must pass. Benchmark before/after.

### Phase B: Compiler Unification (O4)

**Goal**: Reduce code duplication, improve maintainability.

**Changes**:

1. Extract `CompilerCore<'a>` struct containing:
   - `builder: FunctionBuilder<'a>`
   - `vars: Vec<Variable>`
   - `blocks: HashMap<usize, Block>`
   - `vo_module: &'a VoModule`
   - `helpers: HelperFuncs`
   - `reg_consts: HashMap<u16, i64>`
   - `checked_non_nil: HashSet<u16>`
   - `memory_only_start: u16`
   - `slot_types: &'a [SlotType]`
   - `callee_func_refs: &'a [Option<FuncRef>]`

2. Implement `IrEmitter` on `CompilerCore`. FunctionCompiler and LoopCompiler hold a `CompilerCore` + their specific fields.

3. Move `declare_variables`, `scan_jump_targets`, `ensure_block`, `load_local`, `store_local`, `emit_variable_spill`, `conditional_jump`, `forloop`, `call`, `ret`, `panic` to CompilerCore where possible.

4. FunctionCompiler-specific: prologue (args from args_ptr), saved_jit_bp, fiber_stack_args_ptr
5. LoopCompiler-specific: prologue (all vars from locals_ptr), exit_block, store_vars_to_memory

**Risk**: Low. Pure refactor, no semantic changes.
**Testing**: All existing tests. Cranelift IR output should be identical.

### Phase C: IC Helper Consolidation (O5)

**Goal**: Reduce per-callsite IR size for dynamic calls.

**Changes**:

1. Create `vo_ic_dispatch_closure(ctx, closure_ref, ic_entry, user_args, arg_count, ret_ptr) -> (JitResult, bool_is_hit)` in jit_api.rs. This function:
   - Loads IC entry, checks key match + jit_ptr non-null
   - On hit: copies slot0 + user_args to native buffer, updates ctx, calls JIT function, restores ctx
   - Returns (JitResult::Ok, true) on IC hit + OK
   - Returns (result, true) on IC hit + non-OK (caller handles slow path)
   - Returns (_, false) on IC miss (caller falls through to prepare callback)

2. Similarly for `vo_ic_dispatch_iface`.

3. In emit_call_closure/emit_call_iface, replace the inline IC hit path with a single call to the helper. Keep IC miss path as-is.

4. The helper returns two values: use a packed return (result << 1 | is_hit) or output pointer.

**Risk**: Medium. Must validate IC hit path correctness matches inline version exactly.
**Testing**: All dynamic call benchmarks. IC hit rate should be identical.

### Phase D: GC Alloc Inline Fast Path (O7 partial)

**Goal**: Inline bump allocation for the ~95% fast case.

**Changes**:

1. Add `gc_alloc_ptr` and `gc_limit_ptr` fields to JitContext (pointers into Gc's bump allocator state).

2. In translate.rs `ptr_new`, emit inline:
   ```
   alloc_size = (slots + HEADER_SLOTS) * 8
   free_ptr = load(gc_alloc_ptr)
   new_free = free_ptr + alloc_size
   if new_free > load(gc_limit_ptr) { goto slow_path }
   store(gc_alloc_ptr, new_free)
   // write GcHeader at free_ptr
   result = free_ptr + HEADER_BYTES
   goto done
   slow_path:
   result = call vo_gc_alloc(gc, meta, slots)
   done:
   ```

3. Similarly for array/slice/closure allocations that go through gc_alloc internally.

**Risk**: High. Must match GC's allocation protocol exactly (header format, alignment, metadata). Requires intimate knowledge of GC internals.
**Testing**: Full GC stress tests. Memory corruption tests.

---

## Priority Order

| Priority | Item | Impact | Risk | Effort |
|----------|------|--------|------|--------|
| 1 | Phase A: JitContext + alloc | High | Low | Small |
| 2 | Phase B: Compiler unification | Medium (maintainability) | Low | Medium |
| 3 | Phase C: IC helper | Medium | Medium | Medium |
| 4 | O6: Dead code elim for static calls | Low | Low | Small |
| 5 | Phase D: GC alloc inline | High | High | Large |

## Implementation Results (2026-02-20)

### Completed

1. **Phase A1: Eliminate Box<JitOwnedState> per-call allocation**
   - Moved `panic_flag`, `is_user_panic`, `safepoint_flag`, `panic_msg` fields to `Fiber` struct
   - JitContext now points directly into Fiber's fields — no heap allocation per JIT call
   - Files: `vo-vm/src/fiber.rs`, `vo-vm/src/vm/jit/context.rs`

2. **Phase A3: Eliminate Vec<u64> per-call allocation for return values**
   - Replaced `vec![0u64; ret_slots]` with `[0u64; 16]` stack buffer
   - Heap fallback only for functions with >16 return slots (extremely rare)
   - File: `vo-vm/src/vm/jit/mod.rs`

3. **Constant divisor optimization for DivI/ModI/DivU/ModU**
   - When divisor is compile-time known constant: skip zero-check and overflow-check
   - DivI/ModI with const != 0 && const != -1: zero checks eliminated entirely
   - DivI/ModI with const == -1: only overflow check (MIN_INT64 / -1)
   - DivU/ModU with const != 0: zero check eliminated entirely
   - File: `vo-jit/src/translate.rs`

4. **Inline str_len and str_index bounds check**
   - String uses SliceData layout — len field at same offset as slice len
   - str_len: replaced extern call with `emit_nil_guarded_load` (2 loads vs function call)
   - str_index: bounds check uses inline len instead of calling vo_str_len
   - File: `vo-jit/src/translate.rs`

### Evaluated and Skipped (with rationale)

- **Phase A2: Cache JitContext on Vm** — Most fields are constant but JitContext rebuild
  is dominated by field assignments (~50 stores), not computation. Benefit too small vs
  complexity of cache invalidation logic.

- **Phase B: Compiler unification** — FunctionCompiler and LoopCompiler have different
  spill mechanisms (FuncCompiler recomputes fiber.stack address from ctx.stack_ptr +
  saved_jit_bp; LoopCompiler writes directly to locals_ptr). Extracting CompilerCore
  would require each compiler to implement IrEmitter and delegate ~20 methods, adding
  more boilerplate than it removes.

- **Phase C: IC helper consolidation** — IC hit fast path's core value is eliminating
  function call overhead. Moving it to a helper function adds ~2-4ns per IC hit (function
  call + arg passing), negating the IC's purpose for monomorphic callsites.

- **Phase D: GC alloc inline fast path** — GC uses per-object `alloc_zeroed` (system malloc),
  not a bump allocator. Inlining would require redesigning the GC allocator to use bump
  allocation with overflow-to-malloc, which is a GC project, not a JIT project.

### Test Results
- VM: 995 passed, 0 failed
- JIT: 995 passed, 0 failed

## Non-Goals (Explicitly Excluded)

- **Register-based ABI**: Previously attempted and reverted. Cranelift's regalloc behavior makes this counterproductive.
- **Frame stack inline**: Previously attempted. Current frame_stack approach with callback push/pop is good enough.
- **Tiered compilation**: Cranelift's compilation is fast enough that a single tier suffices.
- **Concurrent compilation**: Single-threaded VM means compilation can block without issue.

## Future Opportunities (Beyond Current Session)

1. **GC bump allocator** — Add arena/bump allocation to Gc, then inline the fast path in JIT.
   Would dramatically improve alloc-heavy benchmarks (binary-trees). Requires GC redesign.

2. **Function inlining** — For small pure functions called in tight loops (e.g., spectral-norm's
   `a(i,j)`), inline the callee's bytecode into the caller's IR. Eliminates JIT-to-JIT call
   overhead (ctx stores/restores). Requires bytecode-level or IR-level inlining pass.

3. **Codegen improvements** — Many benchmarks are bottlenecked by codegen quality (too many
   local slots, redundant Copy instructions, struct slice access generating multiple SliceGet).
   These are vo-codegen issues, not JIT issues.
