# VM 指令调试心得

> 记录于 2025-12-25，调试栈数组迭代 (IterNext/Jump) 问题

## 问题背景

栈数组 for-range 迭代出现死循环，表现为程序卡住不输出。

## 调试技巧

### 1. 先分析 bytecode，再运行

问题最初表现为"死循环"，但通过分析 bytecode 发现是 Jump 目标错误：

```bash
cargo run --bin vo -- run test.vo --codegen 2>&1 | grep -E "IterBegin|IterNext|Jump|IterEnd"
```

输出示例：
```
0024: IterBegin     r18, r6, kind=0
0025: IterNext      r16, r5, pc_5
0029: Jump          pc_-4
0030: IterEnd       r0
```

**关键**：bytecode 分析比盲目运行更能快速定位问题根源。

### 2. 分层调试，逐步缩小范围

```
codegen (emit) → bytecode → runtime (exec)
```

1. 先在 codegen 添加 `eprintln!` 确认参数正确
2. 再在 runtime 添加 debug 确认指令字段
3. 最后在具体分支确认执行流程

示例：
```rust
// codegen 层
eprintln!("[EMIT] IterNext: key_slot={}, val_slot={}", key_slot, val_slot);

// runtime 层
eprintln!("[ITER-NEXT] a={}, flags={}, done_offset={}", inst.a, inst.flags, done_offset);
```

### 3. 理解 VM 执行模型

**关键洞察**：VM 主循环在每条指令执行后会 `pc += 1`

这导致 Jump 的 offset 计算需要考虑这个 +1：
- `emit_jump_to`: `offset = target - current` (不减1)
- `exec_jump`: `pc = pc + offset - 1` (减1，因为 VM 会 +1)

错误配对会导致跳转偏移 1 个指令。

### 4. 处理死循环的输出捕获

死循环时无法直接看输出，用后台进程+超时可以捕获部分输出：

```bash
{ ./target/debug/vo run test.vo 2>&1 & pid=$!; sleep 1; kill $pid 2>/dev/null; } | head -10
```

### 5. 从输出模式推断问题

| 输出模式 | 推断 |
|---------|------|
| 只打印一次 `[ITER-NEXT]` | 不是死循环，是跳转后没回来 |
| 打印多次 `[ITER-BEGIN]` | Jump 跳错位置，跳到了 IterBegin 而不是 IterNext |
| 无限打印 `[ITER-NEXT]` | 真正的死循环，迭代器状态没更新 |

### 6. 检查指令格式冲突

问题：`patch_jump` 修改 b/c 字段会覆盖 IterNext 中存储的 `val_slot`

解决方案：把 `val_slot` 移到 `flags` 字段，避免被覆盖

```rust
// 之前：val_slot 在 b 字段，会被 patch_jump 覆盖
func.emit_op(Opcode::IterNext, key_slot, val_slot, 0);

// 之后：val_slot 在 flags 字段，不受影响
func.emit_with_flags(Opcode::IterNext, val_slot as u8, key_slot, 0, 0);
```

## 具体修复

### IterNext 指令格式变更

```
之前: a=key_slot, b=val_slot, c=0 (b/c 被 patch_jump 覆盖)
之后: a=key_slot, b/c=done_offset, flags=val_slot
```

### Jump offset 计算

```rust
// emit_jump_to
let offset = target as i32 - current;  // 不减1

// exec_jump
frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;  // 减1
```

## 经验总结

1. **不要急于运行** - 先静态分析 bytecode
2. **分层验证** - 每一层确认正确后再往下
3. **理解执行模型** - VM 的 pc += 1 是关键
4. **注意字段复用** - patch 操作可能覆盖其他数据
5. **输出模式分析** - 从输出频率推断问题类型
