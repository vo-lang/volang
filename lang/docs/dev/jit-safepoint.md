# Vo JIT Safepoint 机制设计

## 1. 目标

让 JIT 编译的代码能配合 VM 的协作式调度，确保：
- 长循环不会饿死其他 goroutine
- 时间片行为与 VM 一致（每 ~1000 条字节码指令让出一次）

## 2. 设计简化

由于 Vo 的特殊设计，我们不需要：

| 功能 | 原因不需要 |
|------|-----------|
| GC Stack Map | 非移动 GC，GcRef 指针不会改变 |
| 精确栈扫描 | 变量已在 fiber.stack 上，GC 可直接扫描 |
| 信号抢占 | 协作式调度足够，不需要异步抢占 |

## 3. 数据结构

### 3.1 JitContext 扩展

```rust
// vo-runtime/src/jit_api.rs
#[repr(C)]
pub struct JitContext {
    // ... 现有字段 ...
    
    /// 剩余指令配额，JIT 在 back-edge 递减
    /// 初始值 = TIME_SLICE (1000)
    /// <= 0 时触发 yield
    pub instruction_budget: i32,
}
```

### 3.2 JitResult 扩展

```rust
#[repr(C)]
pub enum JitResult {
    Ok = 0,
    Panic = 1,
    Call = 2,
    WaitIo = 3,
    Yield = 4,  // 新增：时间片耗尽
}
```

## 4. JIT 编译时

### 4.1 循环代价计算

- **LoopCompiler (OSR)**: `cost = end_pc - begin_pc`（循环体字节码指令数）
- **FunctionCompiler**: `cost = |jump_offset|`（回跳距离）

### 4.2 Budget 检查代码生成

在每个循环 back-edge（向后跳转）处插入：

```
budget = load ctx.instruction_budget
new_budget = budget - cost
store ctx.instruction_budget, new_budget
if new_budget <= 0:
    spill_all_vars()
    store ctx.instruction_budget, TIME_SLICE  // reset
    set resume_pc
    return JitResult::Yield
continue:
    jump loop_header
```

## 5. VM 侧处理

### 5.1 运行 fiber 前重置 budget

```rust
fn run_fiber(&mut self, fiber_id: FiberId) -> ExecResult {
    // 重置 JIT budget
    self.jit_context.instruction_budget = TIME_SLICE as i32;
    // ...
}
```

### 5.2 处理 Yield 结果

```rust
match jit_result {
    JitResult::Yield => ExecResult::TimesliceExpired,
    // ...
}
```

## 6. 执行流程

```
┌─────────────────────────────────────────────────────────────┐
│  Scheduler                                                   │
│  ┌─────────────────────────────────────────────────────────┐│
│  │ schedule_next() -> fiber                                ││
│  │ run_fiber(fiber) {                                      ││
│  │   ctx.instruction_budget = 1000                         ││
│  │   result = jit_function(ctx, ...)                       ││
│  │   if result == Yield {                                  ││
│  │     return TimesliceExpired  // 回到 scheduler          ││
│  │   }                                                     ││
│  │ }                                                       ││
│  └─────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  JIT Function                                                │
│  ┌─────────────────────────────────────────────────────────┐│
│  │ loop:                                                   ││
│  │   // ... loop body (cost = N instructions) ...          ││
│  │                                                         ││
│  │   // back-edge check                                    ││
│  │   ctx.budget -= N                                       ││
│  │   if ctx.budget <= 0 {                                  ││
│  │     spill_vars()                                        ││
│  │     ctx.budget = 1000  // reset                         ││
│  │     return JitResult::Yield                             ││
│  │   }                                                     ││
│  │   goto loop                                             ││
│  └─────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────┘
```

## 7. 性能影响

| 操作 | 指令数 | 频率 |
|------|--------|------|
| load budget | 1 | 每次 back-edge |
| sub | 1 | 每次 back-edge |
| store budget | 1 | 每次 back-edge |
| cmp + branch | 2 | 每次 back-edge |
| **总计** | **~5** | **每次循环迭代** |

预计开销：< 2%（远低于 Go 的 7.8%，因为只在 back-edge 检查，不需要 stack map）

## 8. 实现检查清单

- [ ] `JitContext`: 添加 `instruction_budget` 字段
- [ ] `JitResult`: 添加 `Yield` 变体
- [ ] `call_helpers.rs`: 添加 `emit_budget_check()` 函数
- [ ] `loop_compiler.rs`: 在 back-edge 调用 budget check
- [ ] `func_compiler.rs`: 在 back-edge 调用 budget check
- [ ] `jit_glue.rs`: 处理 `JitResult::Yield`
- [ ] 测试验证
