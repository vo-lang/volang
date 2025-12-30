# OSR (On-Stack Replacement) Bug Analysis

## 问题现象

当运行 `jit_crash_minimal5.vo` 时，第 13 次迭代输出 `ptr@0x64` 而不是正确的 `ptr@0x0`。
- `0x64 = 100 = len(values)`
- 说明 `tree.right` 被错误地读取为 `len(values)` 的值

## 触发条件

1. 两个嵌套循环都需要累计到 50 次 back-edge 才触发 OSR
2. 问题出现在 makeTree 的 OSR 首次触发时（第 13 次迭代）

## 根本原因分析

### OSR 编译流程

```
1. emit_osr_prologue: 在 entry_block 中加载所有 locals，跳到 osr_pc block
2. 翻译 PC 0 到 code.len() 的所有指令
3. 对于非跳转目标的代码，如果前一个 block 已终止，创建 dummy block
```

### 问题所在

以 makeTree 为例，OSR 入口是 PC 1（内层循环头）：

```
bytecode:
  PC 0: LoadInt r1, 1      (不是跳转目标)
  PC 1: LoadInt r3, 5      (跳转目标 - 从 PC 8 back-edge)
  ...
  PC 8: Jump pc_-7         (跳到 PC 1)
  PC 9: SliceGet r6, r0[0] (循环结束后的代码)
```

OSR 编译时：
1. entry_block 定义所有变量（从 locals 加载），跳到 block_1
2. PC 0 不是跳转目标，且 entry_block 已终止 → 创建 dummy block
3. PC 0 在 dummy block 中翻译：`def_var(r1, 1)`
4. PC 1 是跳转目标 → 插入 `jump block_1`，切换到 block_1

**问题**：dummy block 只定义了 r1，但跳转到 block_1 时，Cranelift 需要为所有变量找到定义。
对于未定义的变量（如 r0 = values slice），Cranelift 使用了默认值 **0**！

### 生成的错误 IR

```
block3:  ; dummy block
    v43 = iconst.i64 0
    v41 -> v43           ; r0 被错误设为 0！
    v13 = iconst.i64 1   ; r1 = 1
    jump block2(v13, v41)  ; 传递 (r1, r0) = (1, 0)
```

当后续代码使用 r0（values slice）时，实际得到的是 0，导致：
```
v31 = load.i64 v30+8   ; v30 = 0, 所以读取地址 8
```
这会读取错误的内存位置。

## 修复尝试

### 尝试 1：跳过 osr_pc 之前的所有代码

```rust
for pc in osr_pc..self.func_def.code.len() { ... }
```

**问题**：如果有代码从 osr_pc 之后跳回到 osr_pc 之前（如外层循环），这些 block 不会被翻译，导致程序卡死。

### 尝试 2：跳过 osr_pc 之前的非跳转目标代码

```rust
if pc < osr_pc && !is_jump_target {
    continue;  // 不创建 dummy block
}
```

**问题**：仍然有问题，第 13 次迭代仍然输出 `ptr@0x64`。

原因：问题可能不只是 dummy block，还涉及到 Cranelift 的 SSA 变量传递机制。

### 尝试 3：保守策略（临时解决）

检测热循环时只编译 full JIT，不做 OSR 切换：

```rust
match jit_mgr.try_osr(...) {
    OsrResult::Ready(_) | OsrResult::ShouldCompile => {
        jit_mgr.compile_full(func_id, func_def, module);
        return None;  // 继续 VM 执行
    }
    OsrResult::NotHot => return None,
}
```

**优点**：所有测试通过
**缺点**：对于"只调用一次但循环很多次"的函数帮助不大

## 深层问题

真正的问题可能在于 **Cranelift SSA 构造** 如何处理多前驱 block 的变量：

1. block_1 有两个前驱：entry_block 和 dummy_block
2. 从 entry_block 来时，r0 = locals[0]（正确的 values slice）
3. 从 dummy_block 来时，r0 未定义 → Cranelift 用 0
4. Cranelift 在 block_1 插入 phi 节点合并这两个值

但由于 OSR 入口只会从 entry_block 进入（第一次），dummy_block 的路径理论上不可达。
问题是 Cranelift 不知道这一点，它按照控制流图构造 SSA。

## 尝试过的修复方案

### 方案 1：跳过 osr_pc 之前的非跳转目标代码
```rust
if pc < osr_pc && !is_jump_target {
    continue;  // 不创建 dummy block
}
```
**结果**：失败。问题仍然存在。

### 方案 2：dummy block 以 trap 结束
```rust
if pc < osr_pc && !is_jump_target {
    let dummy = self.builder.create_block();
    self.builder.switch_to_block(dummy);
    self.builder.ins().trap(TrapCode::user(1).unwrap());
    block_terminated = true;
    continue;
}
```
**结果**：失败。问题仍然存在。

### 分析

问题可能不仅仅在 dummy block，而是更深层的 SSA 构造问题：
- 即使 dummy block 不连接到后续 block，Cranelift 仍然可能在其他地方使用默认值
- 问题可能涉及 **循环回边** 的变量传递
- 需要使用 **显式 block 参数** 而不是隐式 Variable

## 可能的正确修复方向

1. **使用显式 block 参数**
   - 不依赖 Cranelift 的隐式 SSA Variable
   - 每个 block 显式声明参数，跳转时显式传递值
   - 工作量大，需要重写整个编译器

2. **重新设计 OSR 编译**
   - 从 osr_pc 开始构建新的 CFG
   - 不复用普通编译的 block 结构

## 当前状态

使用 **保守 OSR 策略**：
- 检测热循环时编译 full JIT 版本
- 不做运行时切换，继续 VM 执行
- 下次调用时使用 JIT 版本

**优点**：所有测试通过，程序正确运行
**缺点**：对于"只调用一次但循环很多次"的函数，性能优势有限
