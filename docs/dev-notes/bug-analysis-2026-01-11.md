# Bug Analysis - 2026-01-11

## 4 个 Bug 分析

### Bug 1: method_value

**问题**：`t.M` 作为 method value 时编译不正确。

**根本原因**：`compile_selector` 没有处理 `SelectionKind::MethodVal`，直接当成字段访问。

**正确方案**：
1. 在 `get_expr_source` 中检测 `MethodVal`，返回 `NeedsCompile`
2. 在 `compile_selector` 中添加 `MethodVal` 分支，调用 `compile_method_value`
3. `compile_method_value` 实现：
   - 指针接收者：直接创建闭包，捕获指针
   - 值接收者：需要 wrapper 函数，因为闭包捕获的是 boxed value，需要解包后调用原方法
4. Wrapper 函数生成放在 `context.rs` 的 `get_or_create_method_value_wrapper`

**关键修改文件**：
- `crates/vo-codegen/src/expr/mod.rs`
- `crates/vo-codegen/src/context.rs`

**注意**：Call 指令格式是 `emit_with_flags(Call, func_id_high, func_id_low, args_start, c)`，不是 `emit_op`。

---

### Bug 2: switch_break

**问题**：switch 内的 break 会错误退出外层 loop。

**根本原因**：switch 没有自己的 break context，break 直接作用到外层 loop。

**正确方案**：
1. 在 `LoopContext` 添加 `is_switch: bool` 字段
2. 添加 `enter_switch()` / `exit_switch()` 方法
3. `emit_continue` 需要跳过 switch context（continue 不应作用于 switch）

**关键修改文件**：
- `crates/vo-codegen/src/func.rs`
- `crates/vo-codegen/src/stmt.rs`（switch 编译调用 enter_switch/exit_switch）

---

### Bug 3: defer_capture (未解决)

**问题**：defer 闭包看到的是变量的原始值，而不是修改后的值。

```go
s := "hello"
defer func() { println(s) }()
s = "world"
// 应该打印 "world"，实际打印 "hello"
```

**错误的方案**（我尝试的）：
- 让 escaped reference types 也被 box
- 这是概念上的错误：reference types 本身就是引用，不应该再 box

**根本原因分析**：
- 闭包捕获 reference type 变量时，复制的是 GcRef 的值
- 当 `s = "world"` 时，修改的是栈上的 slot
- 闭包仍持有旧的 GcRef

**正确理解**：
- Go 语义：闭包捕获变量的**引用**，能看到变量的修改
- 当前 Vo 实现：reference types 被标记为 "no escape concept"（见 escape.rs 注释）
- 这导致 reference type 变量被闭包捕获时，不会被放到堆上共享

**待解决**：需要重新设计方案，可能需要修改 escape 分析逻辑。核心问题是如何让闭包和外部作用域共享 reference type 变量的**位置**，而不仅是**值**。

---

### Bug 4: short_var_redecl

**问题**：`p, q := p+1, p+2` 中，RHS 应该先全部求值，再赋给 LHS。

**根本原因**：当前实现逐个处理 name-value pair，导致 `p+2` 用的是已修改的 p。

**正确方案**：
1. Phase 1：遍历所有 RHS，求值到临时 slot
2. Phase 2：遍历所有 LHS，从临时 slot 赋值/定义

**关键修改文件**：
- `crates/vo-codegen/src/stmt.rs`（ShortVar 处理）

---

## 其他发现

### Call 指令格式
```rust
// 正确
emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c)

// 错误
emit_op(Opcode::Call, args_start, func_id, c)
```

### PtrSet vs PtrGet 参数顺序
- PtrSet: `a=ptr, b=offset, c=val`
- PtrGet: `a=dst, b=ptr, c=offset`

### is_reference_type 包括
- string, slice, map, channel, closure, pointer

### escape.rs 设计假设
```rust
// Reference types (slice, map, chan, closure, pointer) are already GcRef, no escape concept.
```
这个假设导致 reference types 被闭包捕获时不会被正确处理。

---

## 测试状态

回滚前：
- switch_break: 已修复
- short_var: 已修复  
- method_value: 已修复
- defer_capture: 错误方案，导致 dyn 测试回归

修改前基线：248 passed, 0 failed
