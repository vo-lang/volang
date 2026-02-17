# Fast ABI Optimization: Lessons Learned (2026-02-10)

## Goal

Optimize JIT-to-JIT self-recursive calls by replacing the memory-based external ABI with
a register-based "Fast ABI", aiming to reduce per-call overhead for hot recursive paths.

Benchmark target: `fibonacci(35)`.

## Baseline Architecture

External ABI (current production):
```
(ctx: ptr, args_ptr: ptr, ret_ptr: ptr) -> status: i32
```
- Args/rets passed via fiber.stack memory pointers
- ctx.jit_bp and ctx.fiber_sp updated in prologue
- Capacity check in prologue
- Native stack slots allocated for args/ret passing (~800B frame for fib)

## Fast ABI Design

Register-based inner function signature:
```
(ctx: ptr, bp: i64, arg0: i64, ...) -> (ret0: i64, ..., status: i64)
```

### Key Design Decisions

1. **Dual entry**: fast inner function + thin external wrapper (for VM calls).
2. **All i64 uniform types**: Avoids Cranelift type conversion instructions between i32/i64.
3. **Deferred ctx sync**: ctx.jit_bp/fiber_sp NOT written in prologue for pure computation
   functions. Only written on non-OK slow path.
4. **Deferred capacity check**: fiber.stack capacity only checked on non-OK slow path.
5. **Status in register**: Last return value is status (i64). Zero-latency check after call.
6. **Pure recursive optimization**: For functions that only call themselves, the non-OK path
   is trivial `return (zeros, status)` — no frame materialization needed.

## Iteration History

| Iteration | Change | fib(35) Release | vs Baseline |
|-----------|--------|-----------------|-------------|
| Baseline  | External ABI | ~0.142s | — |
| v1 | Fast ABI + ctx.fast_status (load from memory) | ~0.145s | **slower** |
| v2 | Status in register (i64 return) | ~0.140s | ~same |
| v3 | All i64 uniform types | ~0.138s | ~same |
| v4 | Pure recursive (trivial non-OK path) | **~0.099s** | **30% faster** |

## Root Cause: Why v1–v3 Failed

Despite the fast ABI hot path having fewer instructions and no memory indirection for
args/rets, performance was identical or worse than baseline. Investigation via machine
code disassembly (`objdump -D -b binary -m aarch64`) revealed:

### Cold-path register pressure pollutes hot-path codegen

The non-OK (cold) slow path was extremely complex:
- i64 → i32 truncation for ctx stores
- Capacity check + conditional grow
- `emit_variable_spill()` — spill all SSA vars to fiber.stack
- `push_frame()` — materialize callee frame
- Copy args from native stack to fiber.stack
- `push_resume_point()` — set up frame chain

Cranelift's register allocator is **whole-function** — it sees cold and hot paths together.
The cold path's ~15 live values and multiple indirect calls forced the allocator to:

1. **Spill hot-path values to stack** prematurely
2. **Save/restore 12 callee-saved registers** in prologue/epilogue

Evidence:
- v1–v3: 12 callee-saved regs saved, ~812 byte code size
- v4 (pure recursive, trivial cold path): 7 callee-saved regs, ~244 byte code size

The baseline also saves 12 callee-saved regs, but its hot path benefits from the stores
being non-blocking writes (store-buffer absorbed), while the Fast ABI's benefit of
*eliminating* those stores was offset by the cold-path register pressure.

### Key Insight

> **For Cranelift JIT, the complexity of cold paths directly degrades hot-path performance
> via register allocation pressure. A simpler cold path = better hot-path codegen.**
>
> Cranelift (and likely most compilers without profile-guided optimization) treats all
> basic blocks equally for register allocation. Cold-path annotations or block frequency
> hints would be needed to avoid this.

## Why v4 Worked

The "pure recursive" optimization recognizes functions whose only calls are to themselves
(like `fib`). For these:
- **PANIC**: Only possible non-OK status. Fast ABI functions have no defer, so no frame
  materialization needed — VM skips intermediate frames during unwind.
- **CALL/WAIT_IO**: Impossible (no external calls).

This means the non-OK path can be a trivial `return (zeros, status)`, eliminating ALL
cold-path complexity and freeing registers for the hot path.

## Complexity vs Benefit Assessment

### Complexity Added
- `FastAbiMode` enum (`Disabled` / `Enabled { self_fast_ref, eager_ctx_sync, pure_recursive }`)
- Dual entry compilation (`compile_dual_entry`, `compile_external_wrapper`)
- `call_self_recursive_fast_pure` + `call_self_recursive_fast` (two new call paths)
- `emit_non_ok_slow_path` i64→i32 truncation logic
- `emit_jit_call_with_fallback` dual i32/i64 handling
- Analysis functions: `is_pure_recursive`, `can_use_fast_abi`, `fast_abi_needs_eager_ctx_sync`
- `make_fast_sig` signature builder
- All `IrEmitter` trait methods need fast ABI awareness (`fast_abi_ret_slots`, etc.)

### Benefit Scope
- **30% speedup** only on pure self-recursive functions (fibonacci-like)
- **~0% speedup** on non-pure-recursive functions (cold path still complex)
- **Not applicable** to functions with defer, select, or calls to other functions
- Real-world Vo programs rarely have performance-critical pure-recursive functions

### Decision: Reverted

The complexity-to-benefit ratio was too high. The 30% speedup was narrow — only for a
specific class of functions that rarely appear in real Vo programs. The code complexity
nearly doubled in the JIT call path, making maintenance significantly harder.

## Actionable Takeaways

1. **Profile register pressure, not just instruction count.** Fewer instructions doesn't
   mean faster if the register allocator is forced into worse decisions.

2. **Cold-path complexity is a hidden cost.** Even unreachable-in-practice code paths
   affect register allocation. Keep cold paths minimal.

3. **Store ≠ Load for performance.** Stores are fire-and-forget (store-buffer absorbed).
   Loads are blocking. Eliminating stores saves less than eliminating loads.

4. **Disassemble the machine code.** Cranelift IR analysis alone misses critical codegen
   effects like callee-saved register count and spill patterns.

## Future Directions (if revisiting)

1. **Cranelift cold-path hints**: If Cranelift adds block frequency or cold annotations,
   the full Fast ABI could become competitive without simplifying the cold path.

2. **Tail call optimization**: For tail-recursive functions, loop conversion would be
   far more effective than any ABI optimization.

3. **Tiered compilation**: Hot functions could get more aggressive optimization (inlining,
   loop unrolling) which would dwarf ABI overhead savings.

4. **~~Separate cold-path function~~** (**DISPROVED**): Extracted the non-OK slow path
   (restore-ctx + push_frame + copy_args + push_resume_point) into a single opaque
   `extern "C"` callback. Result: **~45% slower** (0.150s vs 0.110s baseline).
   Why it failed:
   - 10-param callback exceeds AArch64's 8 register args → stack spills for params
   - Extra `call_indirect` through ctx function pointer adds indirect branch overhead
   - **The real register pressure source is SSA variables surviving from hot path to
     cold path for the spill, not the cold path's internal infrastructure.** The callback
     doesn't change which SSA values must be kept alive across the recursive call.
   - The inlined version's multiple small `call_indirect` (push_frame, push_resume_point)
     actually work well because constants are rematerialized post-spill and intermediate
     values don't need callee-saved regs (they're consumed within each call)
