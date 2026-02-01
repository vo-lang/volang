# CallDispatcher 架构修复计划

## 问题概述

当前 JIT-to-JIT 调用链中，如果任意层出现 `WaitIo` 或需要 VM 介入的 `Call`，无法正确恢复 callee → caller 链。

### 根因分析

1. **dispatcher 放在 VmState 而非 Fiber** — 跨 fiber 状态污染
2. **缺少 offset discipline** — 嵌套调用没有区分参数区域
3. **args/ret slot 分离** — WaitIo resume 后 JIT 从错误位置读返回值
4. **execute_jit_with_dispatcher 未被使用** — VM Call 走 call_jit_with_frame

---

## 目标架构

```
VM 作为唯一权威 dispatcher，支持：
1. JIT→JIT 调用链任意层出现 WaitIo
2. 恢复正确的 callee → caller 链
3. 不引入额外 VM frame 链（dispatcher 内部处理）
```

### 核心不变量

1. **dispatcher 必须 per-fiber**（放 Fiber 上，不能放 VmState）
2. **ResumePoint.bp 必须是 offset-from-jit_bp**（不是 call_arg_start）
3. **JIT 所有被 VM 介入的 call 必须用 unified args/ret slot**

---

## Milestone 1: dispatcher per-fiber + 不变量恢复

### 目标
恢复到"每个 fiber 一个 dispatcher"，修复 offset 语义。

### 改动点

#### 1.1 vo-vm/src/fiber.rs
```rust
// 添加字段
pub struct Fiber {
    // ... existing fields ...
    #[cfg(feature = "std")]
    pub call_dispatcher: vo_runtime::call_dispatcher::CallDispatcher,
}
```

#### 1.2 vo-vm/src/vm/types.rs
```rust
// 删除 VmState.call_dispatcher 字段
pub struct VmState {
    // 删除: pub call_dispatcher: CallDispatcher,
}
```

#### 1.3 vo-vm/src/vm/jit_glue.rs

**build_jit_ctx 修改**：
```rust
pub fn build_jit_ctx(
    state: &mut VmState,
    dispatcher: &mut CallDispatcher,  // 新增：从 fiber 传入
    // ... other params ...
) -> JitContext {
    JitContext {
        // ...
        call_dispatcher: dispatcher as *mut _,  // 使用传入的 dispatcher
        // ...
    }
}
```

**execute_jit_with_dispatcher 修改**：
```rust
pub(super) fn execute_jit_with_dispatcher(...) -> ExecResult {
    let fiber = self.scheduler.get_fiber_mut(fiber_id);
    let dispatcher = &mut fiber.call_dispatcher;  // per-fiber
    // ...
}
```

**handle_jit_reentry 修改**：
```rust
pub(super) fn handle_jit_reentry(...) -> ExecResult {
    let fiber = self.scheduler.get_fiber_mut(fiber_id);
    // 先检查 top frame 是 JIT frame 且 pc > 0
    let frame = fiber.frames.last()?;
    if !frame.is_jit_frame || frame.pc == 0 {
        return ExecResult::FrameChanged;
    }
    // 然后检查 dispatcher
    if fiber.call_dispatcher.has_pending() {
        return self.handle_jit_reentry_with_dispatcher(fiber_id, module);
    }
    // fallback...
}
```

### 验收点
- 编译通过
- 两个 fiber 并发时 dispatcher 栈不互相影响

---

## Milestone 2: 正确的 VM dispatcher 状态机

### 目标
`execute_jit_with_dispatcher` 具备完整 offset discipline，支持嵌套 Call + WaitIo。

### 关键设计

#### 2.1 Offset Discipline

```rust
// execute_jit_with_dispatcher 中维护：
let jit_bp: usize;           // 固定，整个调用链的 base
let mut current_offset: usize = 0;  // 当前函数 args base = args_ptr.add(current_offset)

// JitResult::Call 时：
//   1. push ResumePoint(func_id=caller, resume_pc, bp=current_offset, ret_slots)
//   2. current_offset += ctx.call_arg_start
//   3. target_func_id = ctx.call_func_id

// JitResult::Ok 时：
//   1. pop resume_point
//   2. 恢复 current_offset = point.bp
//   3. target_func_id = point.func_id, resume_pc = point.resume_pc

// JitResult::WaitIo 时：
//   1. push ResumePoint(current state)
//   2. 保存 frame.pc, frame.wait_io_token
//   3. 不清 dispatcher，等待 resume
```

#### 2.2 ResumePoint.bp 语义

```rust
pub struct ResumePoint {
    pub func_id: u32,
    pub resume_pc: u32,
    pub bp: u32,        // offset-from-jit_bp（不是 call_arg_start！）
    pub ret_slots: u16,
}
```

### 改动点

#### vo-vm/src/vm/jit_glue.rs

```rust
pub(super) fn execute_jit_with_dispatcher(...) -> ExecResult {
    // ... setup ...
    
    let mut current_offset: usize = 0;
    
    loop {
        // 计算当前 args_ptr
        let current_args_ptr = unsafe { args_ptr.add(current_offset) };
        
        let result = if let Some(jit_func) = jit_func {
            jit_func(&mut ctx, current_args_ptr, current_args_ptr, target_start_pc)
        } else {
            // ... VM fallback ...
        };
        
        match result {
            JitResult::Ok => {
                if dispatcher.has_pending() {
                    let point = dispatcher.pop().unwrap();
                    current_offset = point.bp as usize;  // 恢复 offset
                    target_func_id = point.func_id;
                    target_start_pc = point.resume_pc;
                    continue;
                }
                return ExecResult::FrameChanged;
            }
            
            JitResult::Call => {
                // push 当前 resume point
                dispatcher.push(ResumePoint::new(
                    target_func_id,
                    ctx.call_resume_pc,
                    current_offset as u32,  // bp = current_offset
                    ctx.call_ret_slots,
                ));
                // 进入 callee
                current_offset += ctx.call_arg_start as usize;
                target_func_id = ctx.call_func_id;
                target_start_pc = ctx.call_entry_pc as u32;
                continue;
            }
            
            JitResult::WaitIo => {
                // push resume point
                dispatcher.push(ResumePoint::new(
                    target_func_id,
                    ctx.call_resume_pc,
                    current_offset as u32,
                    0,
                ));
                // 保存 frame state
                let fiber = self.scheduler.get_fiber_mut(fiber_id);
                let frame = fiber.current_frame_mut().unwrap();
                frame.pc = ctx.call_resume_pc as usize;
                frame.wait_io_token = ctx.wait_io_token;
                // block fiber
                self.scheduler.block_for_io(ctx.wait_io_token);
                return ExecResult::Block(...);
            }
            
            JitResult::Panic => { ... }
        }
    }
}
```

### 验收点
- JIT A→JIT B→JIT C，C WaitIo 后 resume，正确恢复 C→B→A
- 参数/返回值不串

---

## Milestone 3: 统一 JIT codegen args/ret 内存模型

### 目标
所有被 VM 介入的 call 使用 unified args/ret slot。

### 问题

当前 `emit_call_closure` 和 `emit_call_iface`：
```rust
let arg_slot = builder.create_sized_stack_slot(...);  // args
let ret_slot = builder.create_sized_stack_slot(...);  // rets（分开！）
```

VM 介入后写返回值到 `args` 区域，但 JIT 从 `ret_slot` 读 → 读到旧数据。

### 解决方案

使用单一 `args_ret_slot`，大小 = max(arg_slots, ret_slots)：

```rust
pub fn emit_call_closure<'a, E: IrEmitter<'a>>(...) {
    let buffer_size = arg_slots.max(ret_slots).max(1);
    let args_ret_slot = builder.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (buffer_size * 8) as u32,
        8,
    ));
    
    // Store args
    for i in 0..arg_slots {
        let val = emitter.read_var((arg_start + i) as u16);
        emitter.builder().ins().stack_store(val, args_ret_slot, (i * 8) as i32);
    }
    
    let ptr = emitter.builder().ins().stack_addr(types::I64, args_ret_slot, 0);
    // args_ptr = ret_ptr = ptr（同一个指针！）
    
    let call = emitter.builder().ins().call(call_closure_func, &[
        ctx, closure_ref, ptr, arg_count, ptr, ret_count  // 注意：两个 ptr 相同
    ]);
    
    // Load returns from same slot
    for i in 0..ret_slots {
        let val = emitter.builder().ins().stack_load(types::I64, args_ret_slot, (i * 8) as i32);
        emitter.write_var((arg_start + i) as u16, val);
    }
}
```

### 改动点

#### vo-jit/src/call_helpers.rs

- `emit_call_closure`: 统一 args/ret slot
- `emit_call_iface`: 统一 args/ret slot

### 验收点
- closure/iface call 普通 Ok 情况返回值正确
- WaitIo 场景返回值正确

---

## Milestone 4: 切换 VM Call 路径到 dispatcher

### 目标
让 dispatcher 方案真正上线。

### 策略选择：4A（推荐）

**仅对"可能产生 Call/WaitIo"的 call 走 dispatcher，纯计算 direct-call 保持原路径。**

#### JIT codegen 侧

对 `Opcode::Call`：
- 若 `can_jit_to_jit_call(...) == true`：使用直接 `call_indirect`（快）
- 否则：返回 `JitResult::Call`，由 VM dispatcher 接管

#### VM 侧

对 `JitResult::Call`：
- 若 callee 已编译 JIT：dispatcher loop 内直接调用（无 VM frame overhead）
- 若 callee 不可 JIT：push VM frame 执行

### 改动点

#### vo-vm/src/vm/mod.rs

```rust
#[cfg(feature = "jit")]
Opcode::Call => {
    // ... existing setup ...
    
    if let Some(jit_func) = jit_func {
        // 使用 execute_jit_with_dispatcher 替代 call_jit_with_frame
        let result = self.execute_jit_with_dispatcher(
            fiber_id, target_func_id, jit_bp, 0
        );
        // ... handle result ...
    } else {
        exec::exec_call(fiber, &inst, module);
    }
}
```

### 验收点
- 新增回归测试：JIT→JIT→blocking extern WaitIo→resume
- `./d.py test jit` 全过
- `./d.py test vm` 全过

---

## 实施顺序

```
Milestone 1 (止血)
    ↓
Milestone 3 (否则 M2 测试会 fail)
    ↓
Milestone 2 (状态机)
    ↓
Milestone 4 (上线)
    ↓
验收测试
```

**注意**：Milestone 3 必须在 Milestone 2 之前完成，否则 WaitIo 测试会因为 args/ret 不统一而 fail。

---

## 风险与回退

### 风险
1. 多 fiber 并发场景可能暴露新 bug
2. 性能可能有轻微回归（dispatcher overhead）

### 回退方案
每个 Milestone 完成后运行完整测试，如果测试 fail 且短时间无法修复，可以 revert 该 Milestone 的改动。

---

## 测试策略

1. **单元测试**：每个 Milestone 后运行 `./d.py test both`
2. **回归测试**：添加 `2026_02_02_jit_waitio_chain.vo` 测试 JIT→JIT→WaitIo→resume
3. **性能验证**：`./d.py bench all` 确认无重大回归
