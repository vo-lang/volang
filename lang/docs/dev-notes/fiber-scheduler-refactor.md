# Fiber Scheduler Refactor: Unified State Machine

**Status**: Revised after external review

## Overview

Current fiber scheduling system has scattered state across multiple fields (`FiberStatus`, `ParkReason`, `ready_queue` membership), leading to implicit invariants and complex code. This document describes a unified state machine design.

### Key Constraints (from review)

1. **FiberId enum must be preserved** — it's part of channel/port/island protocol, not just scheduler internals
2. **Trampoline fibers must stay in separate pool** — their scheduling semantics differ fundamentally from regular fibers
3. **Invariants must be enforced by Scheduler API** — not just documented

## Current Problems

### 1. Scattered State

```rust
// Current: 3 sources of truth
pub struct Fiber {
    pub status: FiberStatus,           // Running | Suspended | Dead
    pub park_reason: Option<ParkReason>, // Channel | Io
    // + implicit: ready_queue.contains(id)
}
```

To determine if a fiber is runnable:
```rust
// Must check both status AND queue membership
fiber.status == FiberStatus::Suspended && ready_queue.contains(&id)
```

### 2. Ambiguous `Suspended` State

`FiberStatus::Suspended` means two different things:
- **Runnable**: In `ready_queue`, waiting to be scheduled
- **Blocked**: Not in queue, waiting for external wake (channel/IO)

The only way to distinguish: `ready_queue.contains(&id)` - O(n) linear search.

### 3. Redundant Queue Membership Checks

```rust
// This pattern appears 6 times in scheduler.rs
if !self.ready_queue.contains(&id) {
    self.ready_queue.push_back(id);
}
```

### 4. Two Nearly Identical Methods

```rust
fn suspend_current(&mut self) {
    fiber.status = FiberStatus::Suspended;
    ready_queue.push_back(id);  // Runnable
}

fn block_current(&mut self) {
    fiber.status = FiberStatus::Suspended;
    // NOT pushed to queue - Blocked
}
```

Same status, different semantics - confusing.

### 5. Trampoline Fiber Special Cases

```rust
fn wake_fiber(&mut self, id: FiberId) {
    match id {
        FiberId::Regular(idx) => {
            // Push to ready_queue
        }
        FiberId::Trampoline(idx) => {
            // Just set status - no queue
        }
    }
}
```

---

## New Design: Unified State Machine

### Core Types

```rust
/// Fiber lifecycle state - single source of truth
/// Replaces: FiberStatus + ParkReason + ready_queue membership
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FiberState {
    /// In ready_queue, waiting to be scheduled
    Runnable,
    /// Currently being executed by VM
    Running,
    /// Blocked waiting for external event
    Blocked(BlockReason),
    /// Finished, slot can be recycled
    Dead,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlockReason {
    /// Waiting for channel/port send/recv (queue-like primitives)
    Queue,
    /// Waiting for I/O completion
    #[cfg(feature = "std")]
    Io(IoToken),
}

impl FiberState {
    #[inline]
    pub fn is_runnable(&self) -> bool {
        matches!(self, FiberState::Runnable)
    }
    
    #[inline]
    pub fn is_blocked(&self) -> bool {
        matches!(self, FiberState::Blocked(_))
    }
}
```

### Fiber Structure Changes

```rust
pub struct Fiber {
    pub id: u32,
    pub state: FiberState,  // Replaces: status + park_reason
    
    // Execution state (unchanged)
    pub stack: Vec<u64>,
    pub frames: Vec<CallFrame>,
    pub defer_stack: Vec<DeferEntry>,
    pub unwinding: Option<UnwindingState>,
    pub select_state: Option<SelectState>,
    pub panic_state: Option<PanicState>,
    pub panic_generation: u64,
    
    #[cfg(feature = "std")]
    pub resume_io_token: Option<IoToken>,
}
```

### Scheduler Structure Changes

```rust
pub struct Scheduler {
    /// Regular fibers indexed by id
    pub fibers: Vec<Box<Fiber>>,
    /// Free slots from dead fibers
    free_slots: Vec<u32>,
    /// Ready queue (only Runnable fiber IDs)
    pub ready_queue: VecDeque<u32>,
    /// Currently running fiber (if any)
    pub current: Option<u32>,
    
    /// Trampoline fibers (SEPARATE pool - different scheduling semantics)
    pub trampoline_fibers: Vec<Box<Fiber>>,
    /// Free slots in trampoline pool
    trampoline_free_slots: Vec<u32>,
    
    /// I/O token -> fiber ID mapping
    #[cfg(feature = "std")]
    io_waiters: HashMap<IoToken, FiberId>,
}
```

**Preserved (from review):**
- `trampoline_fibers` stays separate — trampoline scheduling is driven by `execute_jit_call_with_caller` loop, not `ready_queue`
- `FiberId` enum stays — it's part of channel/port/island wake protocol

**Changed:**
- `Fiber.status + park_reason` → `Fiber.state: FiberState`

---

## Panic/Recover and Scheduling

### Orthogonal State Machines

Fiber 有**两套正交的状态机**：

1. **FiberState**（调度状态）：`Runnable | Running | Blocked(_) | Dead`
2. **Execution State**（执行状态）：`panic_state + unwinding + panic_generation`

这两套状态**独立管理**，不合并。

### Panic 期间的调度状态（Regular Fiber）

| 阶段 | FiberState | panic_state | unwinding |
|------|------------|-------------|-----------|
| 正常执行 | `Running` | `None` | `None` |
| panic 触发 | `Running` | `Some(Recoverable/Fatal)` | `None` |
| defer 执行中 | `Running` | `Some(...)` | `Some(UnwindingState)` |
| recover() 成功 | `Running` | `None` | `Some(...)` → 转 Return 模式 |
| 所有 defer 完成，无 recover | `Running` → `Dead` | `Some(...)` | 清除 |

**关键点**（仅限 regular fiber）：panic unwinding 期间 FiberState 保持 `Running`，只有最终无法 recover 时才变 `Dead`。

**注意**：trampoline fiber 在同步解释循环中可能因 channel/port 阻塞进入 `Blocked(Queue)`；随后被 `wake(FiberId::Trampoline)` 设置回 `Running` 并继续执行，之后可能发生 panic。这与上表不矛盾——上表仅描述 regular fiber。

### Trampoline Panic 传播

当 trampoline fiber 执行 VM 代码时发生 panic：

```rust
// jit_glue.rs: execute_jit_call_with_caller
if result == JitResult::Panic && !caller_fiber_ptr.is_null() {
    // 仅传播 panic_state 和 panic_generation 给 caller fiber
    let trampoline_fiber = scheduler.trampoline_fiber_mut(trampoline_id);
    if let Some(panic_state) = trampoline_fiber.panic_state.take() {
        caller_fiber.panic_state = Some(panic_state);
        caller_fiber.panic_generation = trampoline_fiber.panic_generation;
    }
}
```

**传播内容**：仅 `panic_state` + `panic_generation`，不传播 `unwinding` 状态。
Caller fiber 会用自己的 defer_stack 开始新的 unwinding。

### Fatal vs Recoverable Panic

| 类型 | 来源 | 可否 recover | 执行 defer |
|------|------|-------------|-----------|
| `Recoverable` | 用户 `panic()`、runtime 错误（nil pointer 等） | ✅ | ✅ |
| `Fatal` | JIT 阻塞 deadlock、不支持的操作 | ❌ | ❌ 跳过 |

**判定规则**：
- Fatal 的判定来自 `panic_state == Some(PanicState::Fatal)`
- `handle_panic_unwind()` 开头检查：如果是 Fatal，直接返回 `ExecResult::Panic`，跳过所有 defer

### 不变量

6. **Panic invariant**（仅限 regular fiber）: panic unwinding 期间 FiberState 保持 `Running`
   - unwinding 不改变调度状态
   - 只有 `ExecResult::Panic` 返回后才 `kill_current()` → `Dead`
   - trampoline 在 panic 前可能处于 `Blocked(Queue)`，这不违反此 invariant（此规则仅适用于 regular fiber）

7. **Trampoline panic propagation**: trampoline 仅传播 `panic_state` + `panic_generation` 给 caller fiber
   - 不传播 `unwinding` 状态
   - caller fiber 用自己的 defer_stack 开始新的 unwinding

---

## State Transition Diagram

```
                    spawn()
                       │
                       ▼
                 ┌──────────┐
            ┌───►│ Runnable │◄─────────────────────┐
            │    └────┬─────┘                      │
            │         │                            │
            │         │ schedule_next()            │
            │         ▼                            │
            │    ┌──────────┐                      │
            │    │ Running  │──────────────────────┤
            │    └────┬─────┘   yield_current()    │
            │         │                            │
            │         │ block_for_*()              │
            │         ▼                            │
            │    ┌──────────────┐                  │
            └────│ Blocked(r)  │──────────────────►┘
                 └──────────────┘   wake()
                       │
                       │ kill() or panic propagates to main
                       ▼
                 ┌──────────┐
                 │   Dead   │
                 └──────────┘
```

---

## New Scheduler API

### State Transitions

```rust
impl Scheduler {
    // =========== Lifecycle ===========
    
    /// Spawn a new fiber, returns its id.
    /// Initial state: Runnable (added to ready_queue)
    pub fn spawn(&mut self, fiber: Fiber) -> u32;
    
    /// Acquire a trampoline fiber from pool for JIT->VM calls.
    /// Initial state: Running (NOT in ready_queue)
    /// Returns FiberId::Trampoline(idx) - never exposes raw u32
    pub fn acquire_trampoline(&mut self) -> FiberId;
    
    /// Release a trampoline fiber back to pool.
    /// Panics if id is not FiberId::Trampoline
    pub fn release_trampoline(&mut self, id: FiberId);
    
    // =========== Core State Transitions (by FiberId) ===========
    
    /// Block any fiber (regular or trampoline).
    /// Running -> Blocked(reason)
    /// This is the CANONICAL blocking API. Other block_* methods are sugar.
    pub fn block(&mut self, id: FiberId, reason: BlockReason);
    
    /// Wake a blocked fiber.
    /// For Regular: Blocked(*) -> Runnable (入队)
    /// For Trampoline: Blocked(*) -> Running (不入队，由 JIT glue 驱动)
    pub fn wake(&mut self, id: FiberId);
    
    // =========== Current Fiber Convenience API ===========
    
    /// Pick next runnable fiber and set it to Running.
    /// Runnable -> Running
    /// Returns regular fiber id (index into `fibers[]`), never trampoline.
    pub fn schedule_next(&mut self) -> Option<u32>;
    
    /// Current fiber yields CPU, remains runnable.
    /// Running -> Runnable (back to ready_queue)
    /// Only for regular fibers (trampoline should never call this).
    pub fn yield_current(&mut self);
    
    /// Current fiber blocks on queue (channel/port).
    /// Sugar for: block(FiberId::Regular(current.unwrap()), BlockReason::Queue)
    pub fn block_for_queue(&mut self);
    
    /// Current fiber blocks on I/O.
    /// Sugar for: block(FiberId::Regular(current.unwrap()), BlockReason::Io(token))
    /// ONLY for regular fibers. Trampoline calling this -> panic.
    #[cfg(feature = "std")]
    pub fn block_for_io(&mut self, token: IoToken);
    
    /// Kill current fiber.
    /// * -> Dead
    pub fn kill_current(&mut self) -> (Option<String>, Option<(u32, u32)>);
}
```

### Query Methods

```rust
impl Scheduler {
    /// Check if scheduler has work to do (can make forward progress).
    /// True if either:
    /// - ready_queue is not empty (there are Runnable fibers waiting), OR
    /// - there is a currently Running regular fiber.
    #[inline]
    pub fn has_work(&self) -> bool {
        !self.ready_queue.is_empty() || 
        self.current.map_or(false, |id| self.fibers[id as usize].state == FiberState::Running)
    }
    
    /// Check if there are blocked fibers (potential deadlock detection).
    pub fn has_blocked(&self) -> bool {
        self.fibers.iter().any(|f| f.state.is_blocked())
    }
    
    /// Check if there are I/O waiters.
    #[cfg(feature = "std")]
    pub fn has_io_waiters(&self) -> bool {
        !self.io_waiters.is_empty()
    }
    
    // =========== Fiber Access (by FiberId) ===========
    
    /// Get fiber by FiberId (dispatches to regular or trampoline pool).
    #[inline]
    pub fn get_fiber(&self, id: FiberId) -> &Fiber;
    
    /// Get mutable fiber by FiberId.
    #[inline]
    pub fn get_fiber_mut(&mut self, id: FiberId) -> &mut Fiber;
    
    // =========== Regular Fiber Access (by u32) ===========
    
    /// Get regular fiber by index.
    #[inline]
    pub fn fiber(&self, id: u32) -> &Fiber;
    
    /// Get mutable regular fiber by index.
    #[inline]
    pub fn fiber_mut(&mut self, id: u32) -> &mut Fiber;
    
    // =========== Trampoline Fiber Access ===========
    
    /// Get trampoline fiber by FiberId.
    /// Panics if id is not FiberId::Trampoline.
    #[inline]
    pub fn trampoline_fiber(&self, id: FiberId) -> &Fiber;
    
    /// Get mutable trampoline fiber by FiberId.
    #[inline]
    pub fn trampoline_fiber_mut(&mut self, id: FiberId) -> &mut Fiber;
    
    /// Get current fiber.
    #[inline]
    pub fn current_fiber(&self) -> Option<&Fiber>;
    
    /// Get mutable current fiber.
    #[inline]
    pub fn current_fiber_mut(&mut self) -> Option<&mut Fiber>;
}
```

---

## Invariants (Enforced by Scheduler API)

These invariants are **enforced at runtime** by Scheduler transition methods.
Violation = immediate panic (fail-fast, not defensive).

### State-Location Table

| State | In ready_queue | In io_waiters | current == id |
|-------|----------------|---------------|---------------|
| Runnable | ✅ | ❌ | ❌ |
| Running | ❌ | ❌ | ✅ |
| Blocked(Queue) | ❌ | ❌ | ❌ |
| Blocked(Io(t)) | ❌ | ✅ (key=t) | ❌ |
| Dead | ❌ | ❌ | ❌ |

### Critical Invariants (from review)

1. **Ready queue invariant**: `ready_queue` 中的 id 必须对应 `FiberState::Runnable`
   - `schedule_next()` pop 出非 Runnable 时立即 panic

2. **Current invariant**: `current = Some(id)` 当且仅当该 fiber 为 `Running`

3. **Wake invariant**: `wake()` 只允许对 `Blocked(_)` 调用
   - 对 Running/Runnable/Dead 调用视为逻辑错误 → panic

4. **Trampoline invariant**: trampoline fiber 永不进入 regular `ready_queue`
   - 否则会被 `schedule_next()` 捞走，破坏 JIT 同步调用控制流

### Trampoline Fiber Allowed States

Trampoline fiber 的调度语义与 regular fiber 不同，必须明确允许的状态集合：

| State | Allowed | Notes |
|-------|---------|-------|
| `Running` | ✅ | 正常执行状态 |
| `Blocked(Queue)` | ✅ | 仅在 `execute_jit_call_with_caller` 的 VM 解释循环中短暂出现，用于检测 deadlock → fatal panic |
| `Dead` | ✅ | 释放前标记（可选） |
| `Runnable` | ❌ **panic** | 禁止！Runnable 意味着在 ready_queue 中，会被 `schedule_next()` 捞走 |

实现时 `wake(FiberId::Trampoline)` 必须直接设置 `Running`，不入队。

5. **ID reuse invariant**: 跨线程/跨 island 唤醒必须不受 fiber id 复用影响
   - 如需要，引入 generation/ticket（correctness，不是防御性代码）

6. **Panic invariant**: panic unwinding 期间 FiberState 保持 `Running`
   - unwinding 不改变调度状态
   - 只有 `ExecResult::Panic` 返回后才 `kill_current()` → `Dead`

7. **Trampoline panic propagation**: trampoline 的 `panic_state` 必须传播给 caller fiber
   - 否则 caller 的 defer 无法 recover

8. **I/O invariant**: `block_for_io()` 只允许 regular current fiber
   - trampoline 调用 `block_for_io()` → panic
   - `io_waiters` 永不包含 `FiberId::Trampoline`
   - 原因：I/O polling 在 `run_scheduling_loop` 中，trampoline 由 JIT glue 驱动，两者不兼容

**No more `ready_queue.contains()` checks** - state is authoritative.

---

## Migration: Old → New

### FiberStatus Mapping

| Old | New |
|-----|-----|
| `Running` | `FiberState::Running` |
| `Suspended` + in queue | `FiberState::Runnable` |
| `Suspended` + not in queue + `park_reason: None` | `FiberState::Blocked(Queue)` |
| `Suspended` + `park_reason: Some(Channel)` | `FiberState::Blocked(Queue)` |
| `Suspended` + `park_reason: Some(Io{token})` | `FiberState::Blocked(Io(token))` |
| `Dead` | `FiberState::Dead` |

**重要**：`park_reason: None` 不再表示 channel blocking。新设计中所有 queue-like 阻塞都明确落到 `Blocked(Queue)`。

### Method Mapping

| Old | New |
|-----|-----|
| `suspend_current()` | `yield_current()` |
| `block_current()` | `block_for_queue()` (sugar) 或 `block(id, Queue)` |
| `park_current_for_io(token)` | `block_for_io(token)` (sugar) 或 `block(id, Io(token))` |
| `wake_fiber(FiberId)` | `wake(FiberId)` (preserved) |
| `acquire_trampoline_fiber()` | `acquire_trampoline()` |
| `release_trampoline_fiber(FiberId)` | `release_trampoline(FiberId)` |
| (new) | `block(FiberId, BlockReason)` — canonical blocking API |
| (new) | `get_fiber(FiberId)` / `get_fiber_mut(FiberId)` — type-safe access |

### FiberId Preserved

`FiberId` enum is **preserved**. Reasons:

1. **Protocol compatibility**: channel/port waiter queues store `fiber_id.to_raw()`
2. **Island wake**: `IslandCommand::WakeFiber { fiber_id }` uses raw encoding
3. **Compile-time safety**: prevents accidental `fibers[trampoline_id]` indexing bugs

---

## ExecResult Clarification

Current `ExecResult` semantics (from review):

```rust
pub enum ExecResult {
    Continue,      // Time slice expired, yield to scheduler
                   // (misleading name - actually triggers suspend_current())
    Return,        // Function returned, frames changed
    Yield,         // Explicit yield
    Block,         // Block on channel/port (-> Blocked)
    Panic,         // Panic unwinding
    Done,          // Fiber finished
    Osr(..),       // OSR request
    WaitIo{token}, // Block on I/O (-> Blocked)
}
```

After refactor, clarify semantics:

```rust
pub enum ExecResult {
    FrameChanged,       // Call/return changed frames, refetch locals
    TimesliceExpired,   // Renamed from Continue - yield to scheduler
    Block(BlockReason), // Block, let scheduler handle
    Panic,              // Panic, unwind or kill
    Done,               // Fiber finished
    Osr(u32, usize, usize), // OSR transition
}
```

Key changes:
- `Continue` (old) → `TimesliceExpired` (clearer semantics)
- `Block` + `WaitIo` → `Block(BlockReason)`
- `Yield` removed (same as `TimesliceExpired`)
- `Osr(..)` is preserved as an explicit control-flow event (not a timeslice signal)

### Implementation Correspondence Table

实现时需要对照现有代码：

| Old ExecResult | Old Scheduler Call | New ExecResult | New Scheduler Call |
|----------------|-------------------|----------------|-------------------|
| `Continue` | `suspend_current()` | `TimesliceExpired` | `yield_current()` |
| `Yield` | `suspend_current()` | `TimesliceExpired` | `yield_current()` |
| `Osr(..)` | `suspend_current()` | `Osr(..)` | (handled by VM/JIT OSR logic; no scheduler state change) |
| `Block` | `block_current()` | `Block(Queue)` | `block_for_queue()` |
| `WaitIo{token}` | `park_current_for_io(token)` | `Block(Io(token))` | `block_for_io(token)` |
| `Return` | (frames changed) | `FrameChanged` | (no state change) |
| `Done` | `kill_current()` | `Done` | `kill_current()` |
| `Panic` | `kill_current()` | `Panic` | `kill_current()` |

**关键点**：
- `ChanResult::Wake*` 返回 `ExecResult::Yield` → 新设计中映射到 `TimesliceExpired`（让被唤醒的 fiber 有机会先跑）
- `FrameChanged` 表示"call/return 改变了 frames，需要 refetch"，VM 主循环不改 fiber state
- `Osr(..)` 表示显式 OSR 事件：VM/JIT 执行 OSR 逻辑后继续执行（不是时间片到期信号）

---

## Implementation Order

1. **Add `FiberState` and `BlockReason`** to `fiber.rs`
2. **Migrate `Fiber`** - replace `status + park_reason` with `state: FiberState`
3. **Migrate `Scheduler` API** - rename methods, update state transitions
4. **Update `run_scheduling_loop`** - use new state machine
5. **Update `run_fiber`** - return clarified `ExecResult` variants
6. **Update JIT glue** - use updated Scheduler API (FiberId preserved)
7. **Update channel/port** - use `block_for_queue()`
8. **Remove old types** - `FiberStatus`, `ParkReason`
9. **Run full test suite**

**NOT changed:**
- `FiberId` enum (preserved for protocol compatibility)
- `trampoline_fibers` pool (preserved for scheduling semantics)

---

## Code Size Impact

| Component | Before | After | Change |
|-----------|--------|-------|--------|
| `FiberStatus` | 3 variants | 0 (removed) | -20 lines |
| `ParkReason` | 2 variants | 0 (removed) | -10 lines |
| `FiberId` | enum + methods | preserved | 0 |
| `FiberState` | 0 | 4 variants | +30 lines |
| `BlockReason` | 0 | 2 variants | +10 lines |
| `Scheduler` methods | ~200 lines | ~180 lines | -20 lines |
| `ready_queue.contains()` | 6 calls | 0 calls | -18 lines |

**Net: ~30 lines reduction + clearer semantics + preserved protocol compatibility**

---

## Testing Strategy

1. **Unit tests for state transitions** - verify invariants
2. **Existing test suite** - all 500+ tests must pass
3. **Channel stress test** - concurrent send/recv
4. **I/O stress test** - concurrent file/network ops
5. **JIT integration** - trampoline calls work correctly
6. **Deadlock detection** - verify panic on true deadlock
