<!--
Generated from lang/docs/spec/vm-jit-design.md
Generator: node scripts/ci/docs_sync.mjs
Source-Digest: sha256:774529f94dd58cf937c2a4c891bfcefcf271b456affa7ea7131ae7f95a32c222
Generated-At: 2026-01-21T13:06:28+08:00
-->
# Vo VM JIT Design v2 (Implementation Status)

## 1. Overview

The Vo JIT is a synchronous, method-based JIT compiler using Cranelift. It operates within the VM's execution model, allowing seamless switching between interpreted and compiled code.

### Core Architecture

*   **Synchronous Execution**: JIT functions execute synchronously on the VM thread.
*   **Shared Context**: JIT code shares the `JitContext` with the runtime.
*   **VM Stack Strategy (No StackMap)**: JIT functions operate directly on the VM's `Fiber` stack for local variables.
    *   **Int/Float**: Promoted to SSA variables (registers) for performance.
    *   **GcRef**: **Always** accessed via memory load/store from the VM stack. This ensures GC safety (no stale pointers in registers) without needing JIT stack maps.
*   **Fallback to VM**: Complex operations trigger a fallback to the VM runtime.

### Current Status

| Component | Status | Implementation Details |
|-----------|--------|------------------------|
| **Compiler** | ✅ Active | Uses `cranelift` for code generation. |
| **Dispatcher** | ✅ Active | `JitManager` handles dispatch. |
| **GC Support** | ✅ Simplified | Uses VM stack for roots. No JIT stack maps needed. |
| **Optimization** | ⚠️ Basic | Basic instruction mapping. |

## 2. JIT Compilation Pipeline

### 2.1 Triggering

Hot detection happens in the VM interpreter (`vo-vm`):

1.  **Function Call**: `JitManager::record_call` increments counter. Threshold: 100 calls.
2.  **Loop Backedge**: `JitManager::record_backedge` increments counter. Threshold: 50 iterations.

### 2.2 Compilation (`vo-jit`)

The `JitCompiler` translates Vo bytecode to Cranelift IR with a hybrid storage strategy:

1.  **Locals Ptr**: The JIT function receives a `locals_ptr` argument pointing to the VM stack frame.
2.  **GcRef Access**:
    - `Write`: Store directly to `locals_ptr + offset`.
    - `Read`: Load directly from `locals_ptr + offset`.
    - **Crucial**: GcRefs are NEVER kept in Cranelift SSA variables across instructions. This forces a reload after every potential safepoint, ensuring we see updated pointers if GC moved objects.
3.  **Primitive Access**:
    - `Int/Float`: Loaded into SSA variables (`Variable`) at start/use, spilled back only when necessary (or kept in sync if needed for debugging).
    - Optimizations like register allocation work freely for primitives.

### 2.3 Unsupported Features

Functions with `Defer`, `Recover`, `Go`, `Channel`, `Select` are not JIT compiled.

## 3. Runtime Integration (`vo-runtime`)

### 3.1 JitContext & Call Signature

**VM -> JIT Signature**:
```rust
extern "C" fn(
    ctx: *mut JitContext,
    locals: *mut u64,    // Pointer to VM stack frame (locals start)
    args: *const u64,    // Pointer to args (often within locals, or caller frame)
    ret: *mut u64        // Pointer to return slots
) -> JitResult
```

- **`locals`**: Points to `fiber.stack[frame.bp]`. Used as the "Shadow Stack" root for GC.

## 4. Implementation Details

### 4.1 "No StackMap" Strategy

We avoid implementing complex stack maps (PC -> Register Liveness) by relying on the VM's existing precise GC scanning:

1.  **Storage**: All `GcRef` local variables reside in the VM stack (`fiber.stack`).
2.  **Roots**: The VM GC scans `fiber.stack` conservatively or precisely (Vo is precise). Since JIT updates this stack synchronously, the VM sees the correct roots.
3.  **Safety**:
    - When JIT calls `vo_gc_alloc` or `vo_call_vm`, a GC may occur.
    - GC may move objects and update pointers in `fiber.stack`.
    - JIT code resumes. Since it **loads** GcRefs from `fiber.stack` on use (instead of using a stale register value), it sees the new address.
    - Primitives (i64, f64) are safe in registers because GC doesn't move/update them.

### 4.2 Control Flow

- **Branches**: Direct mapping to Cranelift branches.
- **Calls**: Trampolines to VM or Runtime helpers.

### 4.3 GC Integration

- **Allocation**: Calls `vo_gc_alloc`.
- **Write Barriers**: `PtrSet` checks flag. If GcRef, calls `vo_gc_write_barrier`.
- **Safepoints**: Implicit at every function call. No explicit `vo_gc_safepoint` needed for stack scanning, only for interrupting loops (which can check a flag).

### 4.4 For-Range Loops

Expanded to primitive operations by `vo-codegen`. No iterator state in JIT.

## 5. OSR (On-Stack Replacement)

OSR naturally fits this model. Since JIT and Interpreter use the exact same stack layout (`locals_ptr` -> VM Stack), switching from Interpreter to JIT in the middle of a loop is just a jump instruction with the current `bp`.

## 6. Future Work

1.  **Register Maps (Optional)**: If we want to optimize GcRef performance (keep in registers), we would need Register Maps. For now, memory access overhead is acceptable for simplicity.
2.  **Inline Write Barriers**: Optimize `PtrSet`.
3.  **JIT-to-JIT Calls**: Direct calls passing `locals_ptr` (requires allocating callee frame on VM stack).
