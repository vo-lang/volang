# Loop Function Extraction for OSR

## 概述

将热循环提取为独立的 JIT 函数，避免 mid-function entry 的 SSA 复杂性。

**核心思想**：
- VM 继续执行原版 bytecode
- JIT 编译器在检测到热循环时，将循环体编译为独立函数
- OSR 触发时调用循环函数，返回后更新 VM 状态

## 1. 循环结构分析

### 1.1 Bytecode 中的循环模式

```
典型 for 循环:
  PC 0: LoadInt r1, 0           ; i = 0
  PC 1: LoadInt r3, 100         ; loop_header (条件检查)
  PC 2: LtI r2, r1, r3          ; i < 100
  PC 3: JumpIfNot r2, pc_exit   ; if not, exit
  PC 4: ...                     ; loop body
  PC 5: AddI r1, r1, 1          ; i++
  PC 6: Jump pc_-5              ; back_edge -> loop_header
  PC 7: ...                     ; exit point
```

### 1.2 循环边界

| 概念 | 定义 |
|------|------|
| `loop_header` | 循环条件检查的起始 PC |
| `back_edge` | 跳回 loop_header 的 Jump 指令 |
| `exit_pc` | 循环退出后的第一条指令 |
| `loop_body` | [loop_header, back_edge] 范围内的指令 |

### 1.3 检测方法

已有的 `scan_jump_targets` 可以识别 back-edge（`target_pc <= current_pc`）。
扩展它来记录完整的循环信息：

```rust
struct LoopInfo {
    header_pc: usize,      // 循环头
    back_edge_pc: usize,   // 回边指令位置
    exit_pc: usize,        // 退出位置
    live_in: Vec<u16>,     // 进入循环时的 live 变量（寄存器）
    live_out: Vec<u16>,    // 退出循环时的 live 变量
}
```

## 2. Live 变量分析

### 2.1 定义

- **live_in**: 在循环开始前定义，在循环内使用的变量
- **live_out**: 在循环内修改，在循环退出后使用的变量

### 2.2 简化方案

由于 Vo bytecode 是寄存器机，可以用简单的保守分析：

```rust
fn analyze_loop_liveness(code: &[Instruction], header: usize, back_edge: usize) -> (Vec<u16>, Vec<u16>) {
    let mut read_before_write = HashSet::new();  // live_in
    let mut written = HashSet::new();             // live_out (保守：所有写入的变量)
    
    for pc in header..=back_edge {
        let inst = &code[pc];
        // 如果读取的寄存器还没被写过，它是 live_in
        for reg in inst.read_regs() {
            if !written.contains(&reg) {
                read_before_write.insert(reg);
            }
        }
        // 记录所有写入的寄存器
        if let Some(dst) = inst.write_reg() {
            written.insert(dst);
        }
    }
    
    (read_before_write.into_iter().collect(), written.into_iter().collect())
}
```

### 2.3 更精确的方案（可选优化）

使用完整的数据流分析，只传递真正需要的变量。但初版可以用保守方案。

## 3. 循环函数设计

### 3.1 函数签名

```rust
// 循环函数签名
type LoopFunc = extern "C" fn(
    ctx: *mut JitContext,
    locals: *mut u64,      // 指向 VM 栈上的 locals
) -> LoopResult;

#[repr(C)]
struct LoopResult {
    status: u32,           // 0=continue_loop, 1=exit_normal, 2=break, 3=panic
    // 不需要返回值，直接修改 locals
}
```

### 3.2 循环函数行为

```
fn loop_func(ctx, locals):
    // 一次迭代
    load live_in from locals
    execute loop_body (one iteration)
    if condition_false:
        return LoopResult::ExitNormal
    store live_out to locals
    return LoopResult::Continue
```

### 3.3 调用方式

VM 调用循环函数的伪代码：

```rust
fn execute_loop_jit(loop_func, locals) {
    loop {
        match loop_func(ctx, locals) {
            LoopResult::Continue => continue,
            LoopResult::ExitNormal => break,
            LoopResult::Break => break,
            LoopResult::Panic => return panic,
        }
    }
}
```

## 4. Break/Continue 处理

### 4.1 Continue

`continue` 直接返回 `LoopResult::Continue`，VM 会再次调用循环函数。

### 4.2 Break

`break` 返回 `LoopResult::Break`，VM 跳到 exit_pc。

### 4.3 带标签的 Break/Continue

如果 Vo 支持带标签的 break/continue，需要：
- 返回值中包含目标标签
- VM 根据标签决定跳转位置

## 5. 嵌套循环

### 5.1 方案 A：只 JIT 最内层循环

简单，但可能遗漏优化机会。

### 5.2 方案 B：每层循环独立函数

```
outer_loop:
    inner_loop:
        ...

生成:
    fn inner_loop_func(ctx, locals) -> LoopResult
    fn outer_loop_func(ctx, locals) -> LoopResult {
        // 内部可以调用 inner_loop_func
    }
```

### 5.3 推荐

初版使用方案 A（只 JIT 最内层循环），后续可扩展到方案 B。

## 6. 实现架构

### 6.1 模块结构

```
vo-jit/
├── src/
│   ├── lib.rs
│   ├── compiler.rs       // 现有：full function 编译
│   ├── loop_compiler.rs  // 新增：循环函数编译
│   ├── loop_analysis.rs  // 新增：循环检测和 liveness 分析
│   └── translate.rs      // 现有：bytecode -> Cranelift IR
```

### 6.2 数据结构

```rust
// loop_analysis.rs
pub struct LoopInfo {
    pub header_pc: usize,
    pub back_edge_pc: usize,
    pub exit_targets: Vec<usize>,  // 可能有多个出口
    pub live_in: Vec<u16>,
    pub live_out: Vec<u16>,
    pub contains_break: bool,
    pub contains_continue: bool,
    pub inner_loops: Vec<LoopInfo>,  // 嵌套循环
}

pub fn analyze_loops(func_def: &FunctionDef) -> Vec<LoopInfo> { ... }

// loop_compiler.rs
pub struct LoopCompiler<'a> {
    jit_module: &'a mut JITModule,
    loop_info: &'a LoopInfo,
    func_def: &'a FunctionDef,
    // ...
}

impl LoopCompiler {
    pub fn compile(&mut self) -> Result<LoopFunc, JitError> { ... }
}
```

### 6.3 JitManager 扩展

```rust
// jit_mgr.rs
pub struct JitManager {
    // 现有
    func_table: Vec<*const u8>,
    osr_entries: HashMap<(u32, usize), OsrEntry>,
    
    // 新增
    loop_funcs: HashMap<(u32, usize), LoopFunc>,  // (func_id, header_pc) -> LoopFunc
}

impl JitManager {
    pub fn get_loop_func(&self, func_id: u32, header_pc: usize) -> Option<LoopFunc>;
    pub fn compile_loop(&mut self, func_id: u32, loop_info: &LoopInfo, ...) -> Result<LoopFunc, JitError>;
}
```

### 6.4 执行流程

```
1. VM 执行 bytecode
2. 遇到 back-edge，检查 loop_threshold
3. 如果热循环：
   a. 分析循环 (analyze_loops)
   b. 编译循环函数 (LoopCompiler::compile)
   c. 存入 loop_funcs
4. OSR 触发：
   a. 获取 loop_func
   b. 循环调用 loop_func 直到返回 Exit
   c. 更新 VM 栈上的 locals
   d. 跳转到 exit_pc 继续执行
```

## 7. 与现有 OSR 的区别

| 方面 | 现有 OSR | Loop Function |
|------|----------|---------------|
| 入口点 | 循环头（mid-function） | 函数头 |
| SSA | 需要处理 phi 节点 | 简单（从头开始） |
| 变量 | 从 VM 栈加载所有 locals | 只传递 live 变量（或直接用指针） |
| 执行 | 执行到函数结束 | 执行一次迭代 |
| 返回 | 复制返回值到调用方 | 直接修改 VM 栈 |

## 8. 代码量评估

### 8.1 新增文件

| 文件 | 行数估计 | 说明 |
|------|----------|------|
| `loop_analysis.rs` | ~200 | 循环检测、liveness 分析 |
| `loop_compiler.rs` | ~300 | 循环函数编译器 |

### 8.2 修改文件

| 文件 | 修改量 | 说明 |
|------|--------|------|
| `jit_mgr.rs` | ~50 | 添加 loop_funcs 管理 |
| `jit_glue.rs` | ~100 | OSR 触发时调用循环函数 |
| `lib.rs` | ~30 | 集成新模块 |

### 8.3 总计

- **新增代码**: ~500 行
- **修改代码**: ~180 行
- **总计**: ~680 行

### 8.4 复杂度评估

| 部分 | 复杂度 | 风险 |
|------|--------|------|
| 循环检测 | 低 | 低（已有 back-edge 检测） |
| Liveness 分析 | 中 | 低（保守方案简单） |
| 循环编译 | 中 | 中（复用现有 translate.rs） |
| Break/Continue | 中 | 中（需要仔细处理） |
| VM 集成 | 中 | 低（修改 try_osr） |

## 9. 实现计划

### Phase 1: 基础框架 (~300 行)
1. 循环检测 (`loop_analysis.rs`)
2. 简单 liveness 分析
3. 循环函数编译器框架

### Phase 2: 核心功能 (~250 行)
1. 循环体翻译（复用 `translate.rs`）
2. VM 栈直接读写
3. 正常退出处理

### Phase 3: 完善 (~130 行)
1. Break/Continue 支持
2. JitManager 集成
3. 测试和调试

## 10. 优势总结

1. **避免 SSA 问题**: 函数从头执行，没有 mid-function entry
2. **代码复用**: 复用现有的 `translate.rs`
3. **渐进式**: 可以逐步替换现有 OSR
4. **可维护**: 比 trace JIT 简单得多
5. **性能可预测**: 没有 trace 爆炸问题

## 11. 潜在问题

1. **函数调用开销**: 每次迭代一次调用（可通过展开多次迭代优化）
2. **live 变量分析精度**: 保守分析可能传递过多变量
3. **break 到外层循环**: 需要特殊处理

## 12. 后续优化方向

1. **多次迭代展开**: 一次调用执行 N 次迭代
2. **精确 liveness**: 减少变量传递
3. **嵌套循环支持**: 外层循环也 JIT
4. **内联**: 热循环函数可被内联到调用方
