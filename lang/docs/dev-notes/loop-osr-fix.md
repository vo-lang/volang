# Loop OSR Cranelift 编译问题及解决方案

## 问题描述

在实现 loop function 编译时，遇到 Cranelift 验证错误：
```
invalid block reference blockX
```

## 根本原因

`compile_loop` 中创建的 `header_block` 没有插入到 `self.blocks` HashMap 中。

当循环头部 PC 不是现有跳转目标时，我们创建一个新 block：
```rust
let header_block = if let Some(&block) = self.blocks.get(&header_pc) {
    block
} else {
    let block = self.builder.create_block();
    // BUG: 这里忘了插入到 self.blocks!
    block
};
```

后续代码在 `self.blocks.get(&pc)` 查找时找不到这个 block，导致 Cranelift IR 中引用了未注册的 block。

## 解决方案

```rust
let header_block = if let Some(&block) = self.blocks.get(&header_pc) {
    block
} else {
    let block = self.builder.create_block();
    self.blocks.insert(header_pc, block);  // 关键：插入到 blocks map
    block
};
```

## 其他注意事项

1. **不需要 `loop_panic_block`** - 之前尝试添加统一的 panic block 导致全函数编译卡住。保持原有的局部 panic block 处理即可。

2. **不需要 `scan_jump_targets_range`** - 原始的 `scan_jump_targets` 已经足够，关键是确保 header_block 被正确插入。

3. **Loop function 签名**: `(ctx: *mut, locals: *mut) -> i32`
   - 返回值是 exit_pc 或 LOOP_RESULT_PANIC

## 测试验证

```bash
# 循环 JIT 测试
cargo run --release --bin vo -- run --mode=jit benchmark/sum-array/sum.vo

# 应该通过，且比 VM 模式快 5x+
```

---

## Bug: loop_compiler write_var 不同步到内存 (2026-01-31)

### 问题现象

`gc_slice_grow.vo` 测试在 JIT 模式下失败：
```
s[999]: 998  // 应该是 999
assertion failed
```

循环 1000 次 append，最后一个元素的值是 998 而不是 999。

### 根本原因

`loop_compiler` 的 `write_var` 只更新 SSA 变量，不同步到 `locals_ptr` 内存：

```rust
fn write_var(&mut self, slot: u16, val: Value) {
    self.builder.def_var(self.vars[slot as usize], val);
    // 问题：没有同步到内存！
}
```

而 `slice_append` 使用 `var_addr(elem_slot)` 获取要 append 的值的内存地址：

```rust
let elem_slot = inst.c + if inst.flags == 0 { 2 } else { 1 };
let val_ptr = e.var_addr(elem_slot);  // 期望值在内存中
```

当 append 读取内存时，读到的是旧值（上一次迭代的值），而不是 SSA 变量中的最新值。

### 解决方案

修改 `loop_compiler` 的 `write_var` 使其同时写入 SSA 变量和内存：

```rust
fn write_var(&mut self, slot: u16, val: Value) {
    self.builder.def_var(self.vars[slot as usize], val);
    // Also sync to memory for var_addr access (e.g., slice_append reads element from memory)
    let offset = (slot as i32) * 8;
    self.builder.ins().store(MemFlags::trusted(), val, self.locals_ptr, offset);
}
```

### 对比 func_compiler

`func_compiler` 有 `sync_var` 方法同时更新 SSA 变量和 `locals_slot`，避免了此问题。

### 测试验证

```bash
./d.py test jit gc_slice_grow.vo  # 通过
./d.py test jit                    # 939 passed
```
