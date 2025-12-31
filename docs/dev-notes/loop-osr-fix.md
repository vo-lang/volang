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
