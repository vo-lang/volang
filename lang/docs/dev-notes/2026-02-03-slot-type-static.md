# Static Slot Type Allocation - 设计文档

## 1. 目标

- **GC 安全**: slot 类型在编译后不变，GC 可以准确扫描栈
- **JIT 优化**: Float 类型 slot 可以映射到 F64 寄存器
- **代码简化**: 统一的 slot 分配 API
- **调试友好**: slot 类型在整个函数生命周期内稳定

## 2. 问题分析

### 当前问题

原实现中，`end_temp_region` 恢复 `next_slot` 后，新分配会**覆盖**旧的 `slot_types`。这导致：
- GC 可能根据错误的类型扫描栈
- JIT 无法信任 slot 类型进行寄存器分配

### 解决方案

**静态类型原则**: slot 类型一旦分配就不可变。当类型不匹配时，分配新的 slot 而不是覆盖。

## 3. SlotType 设计

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u8)]
pub enum SlotType {
    #[default]
    Value = 0,      // 普通值 (int, bool, etc.) - I64 寄存器
    GcRef = 1,      // GC 引用 (slice, map, string, etc.)
    Interface0 = 2, // Interface 头部 (itab_id | rttid | value_kind)
    Interface1 = 3, // Interface 数据 (与 Interface0 成对)
    Float = 4,      // 浮点数 - F64 寄存器 (JIT 优化)
}
```

### GC 扫描规则

| SlotType | GC 行为 |
|----------|---------|
| Value | 跳过 |
| GcRef | 扫描为指针 |
| Interface0 | 读取 value_kind 决定 Interface1 是否扫描 |
| Interface1 | 由 Interface0 决定 |
| Float | 跳过 |

## 4. 算法设计

### alloc_slots 三种场景

```
Case 1: 完全在复用区域内 (end_slot <= slot_types.len())
  - 类型匹配 → 复用
  - 类型不匹配 → 分配到末尾

Case 2: 部分重叠 (slot < slot_types.len() < end_slot)
  - 重叠部分类型匹配 → 扩展剩余部分
  - 重叠部分类型不匹配 → 分配到末尾

Case 3: 无重叠 (slot >= slot_types.len())
  - 直接扩展 slot_types
```

## 5. API 设计

```rust
impl FuncBuilder {
    // === 唯一的核心分配 API ===
    
    /// 分配 slots，支持复用（同类型序列匹配）
    /// - 在 temp region 内：分配会被记录，region 结束时归还到 free list
    /// - 在 temp region 外：分配是永久的（如变量定义）
    pub fn alloc_slots(&mut self, types: &[SlotType]) -> u16;
    
    // === 便捷方法 ===
    
    #[inline] pub fn alloc_val(&mut self) -> u16 { self.alloc_slots(&[SlotType::Value]) }
    #[inline] pub fn alloc_ref(&mut self) -> u16 { self.alloc_slots(&[SlotType::GcRef]) }
    #[inline] pub fn alloc_float(&mut self) -> u16 { self.alloc_slots(&[SlotType::Float]) }
    #[inline] pub fn alloc_iface(&mut self) -> u16 { 
        self.alloc_slots(&[SlotType::Interface0, SlotType::Interface1]) 
    }
    
    // === Temp Region 管理（不变） ===
    
    pub fn begin_temp_region(&mut self);
    pub fn end_temp_region(&mut self);
}
```

## 6. 影响分析

### Slot 数量增加

| 场景 | 增幅 |
|------|------|
| 普通代码 | ~16% |
| Dyn 调用 (~>) | ~126% |

Dyn 调用增幅较大是因为 Value 和 Interface 类型频繁交替。

### 收益 vs 代价

**代价**:
- 每个函数的 `slot_types` 向量稍大
- 运行时栈帧稍大

**收益**:
- GC 安全：slot 类型永远正确
- JIT 优化：Float 类型可直接映射 F64 寄存器
- 调试友好：slot 类型在整个函数生命周期内不变

## 7. 迁移策略

### 现有 API 保持兼容

```rust
// 这些便捷方法内部调用 alloc_slots
pub fn alloc_temp_typed(&mut self, types: &[SlotType]) -> u16 {
    self.alloc_slots(types)
}

pub fn alloc_args(&mut self, n: usize) -> u16 {
    self.alloc_slots(&vec![SlotType::Value; n])
}

pub fn alloc_gcref(&mut self) -> u16 {
    self.alloc_slots(&[SlotType::GcRef])
}

pub fn alloc_interface(&mut self) -> u16 {
    self.alloc_slots(&[SlotType::Interface0, SlotType::Interface1])
}

pub fn alloc_interfaces(&mut self, n: usize) -> u16 {
    let types: Vec<_> = (0..n)
        .flat_map(|_| [SlotType::Interface0, SlotType::Interface1])
        .collect();
    self.alloc_slots(&types)
}
```

### 调用点无需修改

现有的 ~300 个调用点无需修改，因为便捷方法签名不变。

## 8. 实现细节

### alloc_slots 完整实现

```rust
pub fn alloc_slots(&mut self, types: &[SlotType]) -> u16 {
    if types.is_empty() { return 0; }
    let len = types.len();
    let slot = self.next_slot as usize;
    let end_slot = slot + len;
    
    // Helper: allocate fresh slots at the end of slot_types
    let alloc_fresh = |this: &mut Self| -> u16 {
        let fresh = this.slot_types.len() as u16;
        this.slot_types.extend_from_slice(types);
        this.next_slot = fresh + len as u16;
        fresh
    };
    
    // Case 1: Fully within existing slot_types (reuse region)
    if end_slot <= self.slot_types.len() {
        if self.slot_types[slot..end_slot] == *types {
            self.next_slot = end_slot as u16;
            return slot as u16;
        }
        return alloc_fresh(self);
    }
    
    // Case 2: Partial overlap with existing slot_types
    if slot < self.slot_types.len() {
        let overlap_len = self.slot_types.len() - slot;
        if self.slot_types[slot..] != types[..overlap_len] {
            return alloc_fresh(self);
        }
        // Extend with non-overlapping part
        self.slot_types.extend_from_slice(&types[overlap_len..]);
    } else {
        // Case 3: No overlap - just extend
        self.slot_types.extend_from_slice(types);
    }
    
    self.next_slot = end_slot as u16;
    slot as u16
}
```

## 9. 测试结果

- VM 测试: 958 passed, 0 failed
- JIT 测试: 948 passed, 0 failed
- Benchmark: Vo-JIT 11.38x vs C (无回归)

## 10. 文件改动

- `vo-common-core/src/types.rs` - 添加 `SlotType::Float`
- `vo-codegen/src/func.rs` - 静态类型分配逻辑
- `vo-codegen/src/expr/comparison.rs` - 处理 Float 比较
- `vo-runtime/src/objects/compare.rs` - 处理 Float hash/eq
- `d_py.py` - 移除废弃的 `--cache` 参数

## 11. 未来优化方向

1. **按类型分组的 free list**: 减少 dyn 场景的 slot 增幅
2. **放宽类型匹配规则**: 只对 Float 严格匹配，其他类型允许覆盖
3. **编译时分析**: 预测 temp region 内的类型模式，减少 mismatch
