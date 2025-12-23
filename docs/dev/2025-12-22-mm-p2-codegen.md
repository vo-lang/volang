# MM Phase 2: Codegen Changes

**Parent**: [2025-12-22-mm-memory-model-plan.md](2025-12-22-mm-memory-model-plan.md)  
**Status**: Planning  
**Est. Lines**: ~1100  
**Depends On**: P1 (Escape Analysis) ✅

## Overview

Modify codegen to generate different code based on escape analysis results:
- Non-escaping struct/array → stack allocation (inline slots)
- Escaping struct/array → heap allocation (GcRef)

## Current State Analysis

### 问题1: TypeInfo 无法访问逃逸信息

`vo-codegen-vm/src/type_info.rs` 使用 `TypeQuery` 包装，但无法访问 `escaped_vars`:

```rust
// vo-codegen-vm/src/type_info.rs:150-162
pub struct TypeInfo<'a> {
    pub query: TypeQuery<'a>,
    pub expr_types: &'a HashMap<ExprId, TypeAndValue>,
    pub type_expr_types: &'a HashMap<TypeExprId, TypeKey>,
    pub selections: &'a HashMap<ExprId, Selection>,
    // 缺少: escaped_vars
}
```

### 问题2: LocalVar 不追踪栈/堆状态

```rust
// vo-codegen-vm/src/func.rs:17-22
pub struct LocalVar {
    pub symbol: Symbol,
    pub slot: u16,
    pub slots: u16,
    // 缺少: is_stack
}
```

### 问题3: struct 总是堆分配

```rust
// vo-codegen-vm/src/stmt.rs:142-149
if is_struct {
    let ty = info.query.get_type(type_key.unwrap());
    for name in &spec.names {
        let src = alloc_empty_struct(ty, ctx, func, info)?;  // 总是堆分配
        let slot_types = &[SlotType::GcRef];
        define_local_with_init(name.symbol, slot_types, Some(src), func);
    }
}
```

### 问题4: 字段访问总是用 GetField/SetField

```rust
// vo-codegen-vm/src/expr.rs:782-792
// 总是用堆访问
let (current, byte_offset) = traverse_to_field(indices, base_reg, func);
func.emit_with_flags(Opcode::GetField, 3, dst + j as u16, current, slot_offset);
```

## Implementation Plan

### Phase 2.1: 数据流准备 (~150 lines)

#### 2.1.1 扩展 TypeInfo

**文件**: `vo-codegen-vm/src/type_info.rs`

```rust
pub struct TypeInfo<'a> {
    pub query: TypeQuery<'a>,
    pub expr_types: &'a HashMap<ExprId, TypeAndValue>,
    pub type_expr_types: &'a HashMap<TypeExprId, TypeKey>,
    pub selections: &'a HashMap<ExprId, Selection>,
    pub escaped_vars: &'a HashSet<ObjKey>,  // NEW
}

impl<'a> TypeInfo<'a> {
    /// Check if a local variable escapes (needs heap allocation).
    /// symbol -> ObjKey lookup through defs/uses
    pub fn is_escaped(&self, obj: ObjKey) -> bool {
        self.escaped_vars.contains(&obj)
    }
    
    /// For compound literals: check if the expression result escapes.
    /// This requires tracking where the result is assigned.
    pub fn expr_escapes(&self, expr_id: ExprId) -> bool {
        // For composite literals assigned to escaped variables
        // This is complex - defer to context tracking
        false
    }
}
```

#### 2.1.2 扩展 LocalVar

**文件**: `vo-codegen-vm/src/func.rs`

```rust
pub struct LocalVar {
    pub symbol: Symbol,
    pub slot: u16,
    pub slots: u16,
    pub is_stack: bool,  // NEW: true if stack-allocated (non-escaping)
}
```

添加辅助方法:

```rust
impl FuncBuilder {
    /// Define a stack-allocated local (non-escaping struct/array).
    pub fn define_local_stack(&mut self, symbol: Symbol, slots: u16, slot_types: &[SlotType]) -> u16 {
        let slot = self.next_slot;
        self.next_slot += slots;
        self.slot_types.extend_from_slice(slot_types);
        self.locals.push(LocalVar { symbol, slot, slots, is_stack: true });
        slot
    }
    
    /// Define a heap-allocated local (escaping or reference type).
    pub fn define_local_heap(&mut self, symbol: Symbol, slots: u16, slot_types: &[SlotType]) -> u16 {
        let slot = self.next_slot;
        self.next_slot += slots;
        self.slot_types.extend_from_slice(slot_types);
        self.locals.push(LocalVar { symbol, slot, slots, is_stack: false });
        slot
    }
    
    /// Check if a local variable is stack-allocated.
    pub fn is_stack_local(&self, symbol: Symbol) -> bool {
        self.lookup_local(symbol).map(|v| v.is_stack).unwrap_or(false)
    }
}
```

#### 2.1.3 ObjKey 查询

需要从 `Symbol` 查找对应的 `ObjKey`。通过 `TypeInfo.defs` 或 `TypeInfo.uses`:

**问题**: codegen 的 TypeInfo 目前不包含 `defs`。需要添加:

```rust
pub struct TypeInfo<'a> {
    // ...existing fields
    pub defs: &'a HashMap<Ident, Option<ObjKey>>,  // NEW
    pub escaped_vars: &'a HashSet<ObjKey>,          // NEW
}
```

或者，传入完整的 `vo_analysis::TypeInfo` 引用。

### Phase 2.2: stmt.rs 修改 (~200 lines)

#### 2.2.1 变量声明 (var)

**位置**: `compile_var_decl` (~L113-200)

```rust
if is_struct {
    let ty = info.query.get_type(type_key.unwrap());
    for name in &spec.names {
        // NEW: 查找 ObjKey 并检查逃逸
        let obj_key = info.lookup_def_obj(&name);
        let escapes = obj_key.map(|k| info.is_escaped(k)).unwrap_or(true);
        
        if escapes {
            // Escaping: heap allocation (现有逻辑)
            let src = alloc_empty_struct(ty, ctx, func, info)?;
            let slot_types = &[SlotType::GcRef];
            func.define_local_heap(name.symbol, 1, slot_types);
            // emit MOV or init
        } else {
            // Non-escaping: stack allocation (inline slots)
            let slot_types = info.struct_field_slot_types(ty);
            let dst = func.define_local_stack(name.symbol, slot_types.len() as u16, &slot_types);
            // Zero-initialize inline fields
            emit_zero_init(func, dst, &slot_types);
        }
    }
}
```

#### 2.2.2 短变量声明 (:=)

**位置**: `compile_short_var_decl` (~L304-319)

类似处理，但需要从 RHS 表达式推断类型和逃逸状态。

对于 `:=` 的复合字面量情况:
```vo
s := Point{x: 1, y: 2}  // s 是否逃逸?
```

逃逸分析已经标记了 `s`，这里只需查询。

### Phase 2.3: expr.rs 修改 (~300 lines)

#### 2.3.1 Selector 访问

**位置**: `compile_selector` (~L762-793)

```rust
fn compile_selector(/* ... */) -> Result<u16> {
    // Check if base is a stack-allocated struct
    if let ExprKind::Ident(ident) = &expr.kind {
        if func.is_stack_local(ident.symbol) {
            return compile_selector_stack(/* ... */);
        }
    }
    
    // Existing heap access logic
    let base_reg = compile_expr(expr, ctx, func, info)?;
    // ... GetField
}

/// Stack struct field access: direct register offset
fn compile_selector_stack(
    base_slot: u16,
    field_offset: usize,
    field_slots: usize,
    func: &mut FuncBuilder,
) -> Result<u16> {
    // Stack struct: fields are consecutive registers
    // field at offset N is at register (base_slot + N)
    Ok(base_slot + field_offset as u16)
}
```

#### 2.3.2 Selector 赋值

**位置**: `compile_assign` 中的 `ExprKind::Selector` 分支 (~L271-297)

```rust
ExprKind::Selector(sel) => {
    // Check if base is stack-allocated
    if is_stack_selector(sel, func) {
        // Direct register write
        let field_slot = base_slot + field_offset;
        func.emit_op(Opcode::Mov, field_slot, src, 0);
    } else {
        // Existing heap logic: SetField
        let base_reg = compile_expr(&sel.expr, ctx, func, info)?;
        // ...
    }
}
```

#### 2.3.3 复合字面量

**位置**: `compile_composite_lit` (~L871-1012)

对于 struct 字面量:
```rust
// Check if target variable escapes (need context from assignment)
if target_escapes {
    // Existing: Alloc + SetField
} else {
    // Stack: allocate inline slots and MOV each field
    let dst = func.alloc_temp_typed(&field_slot_types);
    for (i, elem) in elems.iter().enumerate() {
        let val = compile_expr(&elem.value, ctx, func, info)?;
        func.emit_op(Opcode::Mov, dst + i as u16, val, 0);
    }
}
```

**难点**: 复合字面量的逃逸状态取决于其使用位置:
- `s := Point{...}` → s 是否逃逸
- `foo(Point{...})` → 参数传递，可能逃逸

**简化方案**: 复合字面量暂时总是堆分配，后续优化。

### Phase 2.4: 地址取 (&x)

**位置**: `compile_unary` 中的 `UnaryOp::Addr` (~L274-296)

```rust
UnaryOp::Addr => {
    // 如果 x 已经在堆上 (is_stack: false)，直接返回 GcRef
    // 如果 x 在栈上，这不应该发生（逃逸分析应该标记它为堆）
    // 但作为安全检查:
    if func.is_stack_local(symbol) {
        panic!("BUG: taking address of stack-allocated variable that wasn't marked as escaped");
    }
    func.emit_op(Opcode::Mov, dst, src, 0)  // 返回 GcRef
}
```

### Phase 2.5: Array 处理 (~200 lines)

类似 struct，但更简单:
- Stack array: 连续 slots
- Heap array: GcRef + ArrayGet/ArraySet

### Phase 2.6: Cranelift 修改 (~250 lines)

**文件**: `vo-codegen-cranelift/src/translate.rs`

Cranelift 不直接处理逃逸分析，它翻译 bytecode。关键是 bytecode 已经包含正确的指令:

- 栈结构: `Mov` 操作寄存器
- 堆结构: `Alloc`, `GetField`, `SetField`

如果 bytecode 生成正确，Cranelift 翻译应该自动工作。

**可能需要的修改**:
- 新增 `MovN` 指令支持（多 slot MOV）
- 栈结构的 GC 扫描（多个 GcRef slot）

## Data Flow Diagram

```
vo-analysis
├── TypeInfo
│   ├── escaped_vars: HashSet<ObjKey>  ← 逃逸分析结果
│   └── defs: HashMap<Ident, ObjKey>   ← 定义映射
│
└── is_escaped(ObjKey) -> bool
        ↓
vo-codegen-vm
├── TypeInfo (wrapper)
│   ├── escaped_vars: &HashSet<ObjKey>  ← 引用
│   └── is_escaped(ObjKey) -> bool      ← 委托
│
├── FuncBuilder
│   └── LocalVar { is_stack: bool }     ← 追踪分配方式
│
├── stmt.rs
│   ├── compile_var_decl    → 查询逃逸，选择栈/堆
│   └── compile_short_var   → 同上
│
└── expr.rs
    ├── compile_selector    → 查询 is_stack，选择访问方式
    └── compile_composite   → 暂时总是堆分配
```

## Tasks Checklist

### 2.1 type_info.rs
- [ ] 添加 `escaped_vars: &'a HashSet<ObjKey>` 字段
- [ ] 添加 `defs: &'a HashMap<Ident, Option<ObjKey>>` 字段
- [ ] 实现 `is_escaped(ObjKey) -> bool`
- [ ] 实现 `lookup_def_obj(&Ident) -> Option<ObjKey>`

### 2.2 func.rs
- [ ] 扩展 `LocalVar` 添加 `is_stack: bool`
- [ ] 添加 `define_local_stack()` 方法
- [ ] 添加 `define_local_heap()` 方法
- [ ] 添加 `is_stack_local(symbol) -> bool` 查询

### 2.3 stmt.rs
- [ ] 修改 `compile_var_decl` 支持栈分配
- [ ] 修改 `compile_short_var_decl` 支持栈分配
- [ ] 添加栈 struct 零初始化

### 2.4 expr.rs
- [ ] 添加 `compile_selector_stack()` 函数
- [ ] 修改 `compile_selector` 分派到栈/堆
- [ ] 修改 assign 中的 selector 分支
- [ ] (可选) 修改 `compile_composite_lit` 支持栈

### 2.5 array 支持
- [ ] 栈 array 分配
- [ ] 栈 array 索引访问

### 2.6 codegen-cranelift
- [ ] 验证现有指令翻译正确
- [ ] (如需) 添加 MovN 支持

## Testing Strategy

1. **保持现有测试通过**: 默认行为不变（可通过 feature flag 或全部标记为 escape）
2. **增量启用**: 先支持 struct 栈分配，再支持 array
3. **新测试用例**:
   - 非逃逸 struct 局部变量
   - 逃逸 struct（取地址、闭包捕获、interface 赋值）
   - 混合场景

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| ObjKey 查询复杂 | 扩展 TypeInfo 包含必要映射 |
| 复合字面量逃逸判断 | 暂时总是堆分配 |
| 深拷贝逻辑变化 | 栈结构用 MovN 复制多 slot |
| Cranelift 兼容性 | bytecode 正确则自动兼容 |
