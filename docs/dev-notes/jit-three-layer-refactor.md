# JIT 三层架构重构

## 问题

当前 JIT 相关代码分散在多个地方，导致：
1. 版本选择逻辑重复
2. 热度记录不一致
3. VM 和 JIT 代码混杂，难以维护

## 三层架构

```
┌─────────────────────────────────────────────────────────────┐
│  Layer 1: JitManager (vo-jit/src/manager.rs)                │
│  ─────────────────────────────────────────────────────────  │
│  职责:                                                       │
│  - 状态机: CompileState, FunctionJitInfo                    │
│  - 热度记录: record_call(), record_backedge()               │
│  - 编译触发: compile_full(), compile_osr()                  │
│  - 版本查询: get_entry(), get_osr_entry()                   │
│  - func_table 管理                                          │
│                                                             │
│  原则: 这是唯一的决策点，所有"用哪个版本"的决定在这里       │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 2: JIT Bridge (vo-jit/src/bridge.rs) [新建]          │
│  ─────────────────────────────────────────────────────────  │
│  职责:                                                       │
│  - extern "C" trampoline 函数                               │
│  - JitContext 构建                                          │
│  - 参数/返回值 marshalling                                  │
│  - JIT <-> VM 上下文切换                                    │
│                                                             │
│  包含:                                                       │
│  - vo_call_function: 统一调用入口                           │
│  - vm_interpret: 纯 VM 解释执行                             │
│  - itab_lookup_trampoline                                   │
│  - call_extern_trampoline                                   │
│                                                             │
│  原则: 所有 JIT/VM 切换的"脏活"在这里                       │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 3: Pure VM (vo-vm/src/vm.rs)                         │
│  ─────────────────────────────────────────────────────────  │
│  职责:                                                       │
│  - Vm struct, VmState                                       │
│  - Fiber, CallFrame, Stack                                  │
│  - run_fiber: 主执行循环                                    │
│  - exec 模块: 纯指令实现                                    │
│                                                             │
│  原则: 尽量不知道 JIT 的存在                                │
│  - #[cfg(feature = "jit")] 块最小化                        │
│  - Call 指令只负责 push frame                               │
│  - JIT 相关逻辑通过回调/trait 注入                          │
└─────────────────────────────────────────────────────────────┘
```

## 调用流程

### 1. JIT 代码调用函数

```
JIT 代码 Call 指令 (translate_call)
    │
    ├─→ jit_func_table[func_id] != null
    │       └─→ 直接调用 JIT 版本 (快路径)
    │
    └─→ jit_func_table[func_id] == null
            └─→ call vo_call_function (Bridge Layer)
```

### 2. vo_call_function (Bridge Layer)

```rust
// vo-jit/src/bridge.rs
pub extern "C" fn vo_call_function(
    ctx: *mut JitContext,
    func_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult {
    let jit_mgr = get_jit_manager(ctx);
    
    // 1. 检查 JIT 版本
    if let Some(jit_func) = jit_mgr.get_entry(func_id) {
        return jit_func(ctx, args, ret);
    }
    
    // 2. 记录热度，尝试编译
    if jit_mgr.record_call(func_id) {
        jit_mgr.compile_full(func_id, ...);
        if let Some(jit_func) = jit_mgr.get_entry(func_id) {
            return jit_func(ctx, args, ret);
        }
    }
    
    // 3. VM 解释执行
    vm_interpret(ctx, func_id, args, arg_count, ret, ret_count)
}
```

### 3. vm_interpret (Bridge Layer)

```rust
// vo-jit/src/bridge.rs
fn vm_interpret(
    ctx: *mut JitContext,
    func_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult {
    // 创建临时 fiber
    let mut fiber = Fiber::new(0);
    // ... 设置参数和栈帧
    
    // 执行循环
    loop {
        let inst = get_next_inst(&fiber, module);
        match inst.opcode() {
            Opcode::Call => {
                // 递归调用 vo_call_function
                // 但不创建新 fiber，使用当前 fiber 的栈
            }
            Opcode::Return => {
                // 如果是初始帧，返回
                // 否则 pop frame
            }
            _ => {
                // 执行其他指令
            }
        }
    }
}
```

## 关键设计决策

### 1. 单一入口点

所有函数调用最终都经过 `JitManager`：
- VM 主循环的 Call → 调用 JitManager.get_entry()
- JIT 代码的 Call → jit_func_table 或 vo_call_function
- vo_call_function → JitManager.get_entry()

### 2. 热度记录统一

只在 `JitManager.record_call()` 中记录：
- vo_call_function 调用 record_call
- VM 主循环的 Call 调用 record_call

### 3. Bridge Layer 隔离脏活

所有 JIT/VM 切换的复杂代码在 bridge.rs：
- JitContext 构建
- 临时 fiber 管理
- 参数 marshalling
- trampoline 函数

### 4. Pure VM 最小化 JIT 依赖

vo-vm 中的 JIT 相关代码：
- Vm struct 持有 `Option<JitManager>`
- run_fiber 中的 try_jit_call (可选)
- 其他通过 cfg 条件编译

## 文件变更

### 新建
- `vo-jit/src/bridge.rs`: JIT 粘连层

### 修改
- `vo-jit/src/manager.rs`: 纯状态机，移除执行逻辑
- `vo-jit/src/lib.rs`: 导出 bridge 模块
- `vo-vm/src/vm.rs`: 简化，移除 JIT 粘连代码

## 实施步骤

1. 创建 `bridge.rs`，移入 trampoline 函数
2. 实现 `vo_call_function` 统一入口
3. 实现 `vm_interpret` 纯解释执行
4. 简化 `vm.rs` 中的 JIT 相关代码
5. 测试验证
