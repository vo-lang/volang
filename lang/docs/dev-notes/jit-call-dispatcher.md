# JIT Call Dispatcher Design

## Background: Why We Need This

### The Problem

When JIT function A calls JIT function B, and B returns `Call` or `WaitIo`:

```
VM frame: A (entry)
    ↓
JIT A → JIT B → B returns Call/WaitIo
               ↓
         ctx.call_resume_pc = B's resume PC
         ctx.call_func_id = C (function B wants to call)
```

**Problem**: JIT-to-JIT calls don't push VM frames. When B returns `Call`, the VM
has no frame for B, so it can't resume B after executing C - it would incorrectly
resume A instead, skipping B entirely.

### Previous Solutions Attempted

1. **Recursive `can_jit_to_jit_call` check**: Recursively check if callee might
   return Call/WaitIo. If yes, don't do JIT-to-JIT, use VM call instead.
   
   **Problems**:
   - Call graph is a directed cyclic graph (recursion, mutual calls)
   - Depth limit (16) is arbitrary, may cause false negatives
   - Expensive: O(depth * instructions) per call site at compile time

2. **Set `call_resume_pc` in caller's non-OK path**: When callee returns non-OK,
   caller overwrites `ctx.call_resume_pc` with its own resume PC.
   
   **Problem**: This corrupts callee's resume info, breaking the call chain.

### The Solution: Trampoline-based CallDispatcher

Instead of detecting which calls are "safe" for JIT-to-JIT, we:
1. Route ALL JIT calls through a Trampoline
2. Trampoline handles `Call` results by looping (no VM frame overhead)
3. Only `WaitIo` truly suspends and returns to VM scheduler

## Architecture

### Core Components

```
┌─────────────────────────────────────────────────────────────────┐
│                         CallDispatcher                          │
├─────────────────────────────────────────────────────────────────┤
│  - jit_func_table: *const JitFunc                               │
│  - resume_stack: Vec<ResumePoint>  (lightweight shadow frames)  │
├─────────────────────────────────────────────────────────────────┤
│  dispatch_call()     ← All JIT calls go through here            │
│  resume_from_io()    ← Resume after WaitIo                      │
│  call_one()          ← Execute single function (JIT or VM)      │
└─────────────────────────────────────────────────────────────────┘
```

### ResumePoint (Lightweight Shadow Frame)

```rust
struct ResumePoint {
    func_id: u32,      // Function to resume
    resume_pc: u32,    // PC to resume at
    bp: u32,           // Base pointer in fiber.stack
    ret_slots: u16,    // Return slots expected
}
```

This is much lighter than a full VM Frame (~32 bytes vs ~64 bytes).

### Call Flow

```
┌────────────────────────────────────────────────────────────────────┐
│                        dispatch_call()                              │
├────────────────────────────────────────────────────────────────────┤
│  push ResumePoint for caller                                        │
│                                                                     │
│  loop {                                                             │
│      result = call_one(target_func)                                 │
│                                                                     │
│      match result {                                                 │
│          Ok → pop ResumePoint, return Ok                            │
│          Panic → return Panic                                       │
│          Call → {                                                   │
│              // Callee wants to call another function               │
│              // DON'T return to VM - handle in trampoline!          │
│              push ResumePoint for callee                            │
│              target_func = ctx.call_func_id                         │
│              continue  // Loop handles the call                     │
│          }                                                          │
│          WaitIo → return Suspend (VM scheduler handles)             │
│      }                                                              │
│  }                                                                  │
└────────────────────────────────────────────────────────────────────┘
```

### Resume from I/O

```
┌────────────────────────────────────────────────────────────────────┐
│                      resume_from_io()                               │
├────────────────────────────────────────────────────────────────────┤
│  // resume_stack has the call chain saved                           │
│                                                                     │
│  while let Some(point) = resume_stack.last() {                      │
│      result = call_one_at_pc(point.func_id, point.resume_pc)        │
│                                                                     │
│      match result {                                                 │
│          Ok → pop, continue unwinding                               │
│          Call → push new point, loop                                │
│          WaitIo → return Suspend                                    │
│          Panic → return Panic                                       │
│      }                                                              │
│  }                                                                  │
│  return Ok  // All frames unwound                                   │
└────────────────────────────────────────────────────────────────────┘
```

## Performance Analysis

### Overhead per JIT Call

| Component | Cost |
|-----------|------|
| Push ResumePoint | ~5 instructions (store 4 fields) |
| Check result | ~3 instructions (compare + branch) |
| Pop ResumePoint | ~2 instructions |
| **Total** | **~10-15ns per call** |

### Comparison with Previous Approach

| Scenario | Old (can_jit_to_jit_call) | New (Trampoline) |
|----------|---------------------------|------------------|
| Pure function chain | 0 (direct call) | ~10ns/call |
| Chain with blocking | VM frame per level (~100ns/level) | Loop in trampoline (~10ns/level) |
| Compile time | O(depth * instructions) check | 0 |
| Correctness | ⚠️ May fail on deep/cyclic graphs | ✅ Always correct |

### When is Trampoline Faster?

- Call chain depth > 10 with any blocking operation
- Complex call graphs (recursion, mutual calls)
- Functions with many call sites (compile time savings)

### When is Direct Call Faster?

- Pure computation (fibonacci, tight loops)
- Very shallow call chains with no blocking

**Conclusion**: Trampoline is ~10ns slower per call for pure functions, but more
correct and faster for complex scenarios. This is an acceptable tradeoff.

## Implementation Plan

### Phase 1: Create CallDispatcher

1. Define `ResumePoint` struct
2. Implement `dispatch_call()` with trampoline loop
3. Implement `resume_from_io()` for I/O continuation

### Phase 2: Integrate with JIT Code Generation

1. Modify `emit_call_with_jit_check` to call `dispatch_call`
2. Remove `can_jit_to_jit_call` check
3. Simplify `scan_call_requests` (no longer need special handling)

### Phase 3: Update VM Integration

1. Modify `call_jit_with_frame` to use CallDispatcher
2. Simplify `handle_jit_reentry`
3. Update I/O resume path

### Phase 4: Cleanup

1. Remove `can_jit_to_jit_call` and related code
2. Remove unused resume block logic
3. Update documentation

## Alternatives Considered

### 1. Precompute `can_jit_to_jit_call` Table

Compute at module load time using SCC analysis.

**Pros**: O(1) lookup, no compile-time overhead
**Cons**: Still conservative (some safe calls rejected), complex implementation

### 2. Full VM Frames for All Calls

Push VM frame for every JIT call.

**Pros**: Simple, always correct
**Cons**: ~100ns overhead per call (10x worse than trampoline)

### 3. Coroutines (corosensei)

Use stackful coroutines for JIT execution.

**Pros**: Can yield/resume anywhere
**Cons**: Complex, memory overhead, not portable

**Decision**: Trampoline is the best balance of correctness, performance, and simplicity.
