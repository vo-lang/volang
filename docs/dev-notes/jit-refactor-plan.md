# JIT Refactor 设计文档

## 核心问题

FunctionCompiler 和 LoopCompiler 都需要翻译 bytecode 指令。

**处理不同的指令（6个）**：

| 指令 | FunctionCompiler | LoopCompiler |
|------|------------------|--------------|
| Jump | 跳到预创建的 block | 跳出循环 → 退出 VM |
| JumpIf | 同上 | 同上 |
| JumpIfNot | 同上 | 同上 |
| Return | 写 ret_ptr，返回 Ok | 退出到 VM |
| Call | JIT-to-JIT 优化 | 调 VM |
| Panic | 返回 Panic | 退出到 VM |

**其他所有指令**：处理逻辑完全相同，从老代码拷贝。

---

## 架构

```
┌─────────────────────────────────────────────────────┐
│                  translate.rs                       │
│                                                     │
│  translate_inst(e: &mut impl IrEmitter, inst)       │
│      │                                              │
│      ├── 所有相同指令 → Completed                   │
│      └── Jump/Call/Return/Panic → Unhandled         │
│                                                     │
└─────────────────────────────────────────────────────┘
                         │
          ┌──────────────┴──────────────┐
          │                             │
          ▼                             ▼
┌──────────────────┐         ┌──────────────────┐
│ FunctionCompiler │         │  LoopCompiler    │
│ impl IrEmitter   │         │  impl IrEmitter  │
│                  │         │                  │
│ 处理 Unhandled:  │         │ 处理 Unhandled:  │
│ - Jump → block   │         │ - Jump → 可能退出│
│ - Call → JIT opt │         │ - Call → 调 VM   │
│ - Return → ret   │         │ - Return → 退出  │
│ - Panic → panic  │         │ - Panic → 退出   │
└──────────────────┘         └──────────────────┘
```

---

## IrEmitter Trait

```rust
pub trait IrEmitter {
    // 基础
    fn builder(&mut self) -> &mut FunctionBuilder;
    fn read_var(&mut self, slot: u16) -> Value;
    fn write_var(&mut self, slot: u16, val: Value);
    
    // 上下文
    fn ctx_param(&mut self) -> Value;
    fn gc_ptr(&mut self) -> Value;
    fn globals_ptr(&mut self) -> Value;
    fn vo_module(&self) -> &VoModule;
    
    // Runtime 函数
    fn str_funcs(&self) -> &StringFuncs;
    fn map_funcs(&self) -> &MapFuncs;
    fn array_funcs(&self) -> &ArrayFuncs;
    fn slice_funcs(&self) -> &SliceFuncs;
    fn misc_funcs(&self) -> &MiscFuncs;
    fn gc_alloc_func(&self) -> Option<FuncRef>;
    fn call_vm_func(&self) -> Option<FuncRef>;
}
```

---

## TranslateResult

```rust
pub enum TranslateResult {
    Completed,   // 指令已处理
    Terminated,  // 块已终止 (panic)
    Unhandled,   // 需要 Compiler 特定处理
}
```

---

## 实现步骤

### 1. 备份当前代码
```bash
cp -r crates/vo-jit crates/vo-jit-backup
```

### 2. 定义 IrEmitter trait
- 文件：`translator.rs`
- 最小接口，只包含翻译指令需要的方法

### 3. 创建 translate_inst 入口
- 文件：`translate.rs`
- 从老代码拷贝指令翻译逻辑
- 6个不同的指令返回 `Unhandled`

### 4. FunctionCompiler
- impl IrEmitter
- 处理 6 个特定指令：Jump/JumpIf/JumpIfNot/Return/Call/Panic
- Call 有 JIT-to-JIT 优化

### 5. LoopCompiler
- impl IrEmitter
- 处理 6 个特定指令
- 跳出循环或遇到 Return/Panic 时退出到 VM

### 6. 测试验证
- `./scripts/test.sh` 全部通过
- `./benchmark/bench.rb vo` 验证性能

---

## 文件结构

```
crates/vo-jit/src/
├── lib.rs              # 模块导出
├── translator.rs       # IrEmitter trait
├── translate.rs        # translate_inst + 共享指令翻译
├── func_compiler.rs    # FunctionCompiler + 6个特定指令
├── loop_compiler.rs    # LoopCompiler + 6个特定指令
├── loop_analysis.rs    # 循环检测（不变）
├── jit_funcs.rs        # Runtime 函数导入（不变）
└── error.rs            # JitError（不变）
```

---

## 备注

- 指令翻译逻辑从老代码拷贝，不重写
- 每步完成后运行 `cargo check --package vo-jit`
- 保持增量提交
