# Backend Rewrite Plan

**Date**: 2025-12-23  
**Status**: Planning  
**Design Docs**: 
- [docs/design/vm.md](../design/vm.md)
- [docs/design/memory-model.md](../design/memory-model.md)
- [docs/design/gc.md](../design/gc.md)
- [docs/spec/memory-model-and-instructions.md](../spec/memory-model-and-instructions.md)

## Overview

从零重写 Vo 后端，实现新内存模型（逃逸分析驱动的栈/堆分配）。

**核心原则**：
- 旧代码只作参考，不拷贝
- 按 crate 串行开发，每个 crate 完整实现后再下一个
- 先单元测试，最后集成测试

## Goals

1. 实现新内存模型：struct/array 默认栈分配，逃逸时堆分配
2. 统一 VM 和 JIT 的数据结构（共享 runtime-core）
3. 完整的指令集（~100 条）
4. 支持所有核心数据类型：int, float, bool, string, array, slice, map, struct, interface, closure, channel, pointer

## Sub-Plans

| Phase | Document | Crate | Description | Est. Modules |
|-------|----------|-------|-------------|--------------|
| P1 | [backend-p1-runtime-core.md](2025-12-23-backend-p1-runtime-core.md) | vo-runtime-core | 共享数据结构 + GC + 对象操作 | 28 |
| P2 | [backend-p2-vm.md](2025-12-23-backend-p2-vm.md) | vo-vm | Bytecode 格式 + VM 解释器 | 36 |
| P3 | [backend-p3-codegen-vm.md](2025-12-23-backend-p3-codegen-vm.md) | vo-codegen-vm | AST → Bytecode | 44 |
| P4 | [backend-p4-runtime-native.md](2025-12-23-backend-p4-runtime-native.md) | vo-runtime-native | JIT/AOT Runtime | 35 |
| P5 | [backend-p5-codegen-cranelift.md](2025-12-23-backend-p5-codegen-cranelift.md) | vo-codegen-cranelift | Bytecode → Cranelift IR | 34 |
| P6 | [backend-p6-aot-jit.md](2025-12-23-backend-p6-aot-jit.md) | vo-aot + vo-jit | 编译器入口 | 9 |

**Total**: ~186 模块

## Dependencies

```
P1 (runtime-core)  ─────────────────────────────────┐
    │                                               │
    ▼                                               ▼
P2 (vm)                                    P4 (runtime-native)
    │                                               │
    ▼                                               │
P3 (codegen-vm)                                     │
    │                                               │
    └───────────────────┬───────────────────────────┘
                        ▼
                P5 (codegen-cranelift)
                        │
                        ▼
                P6 (aot + jit)
```

**关键依赖**：
- P1 必须先完成（定义所有共享数据结构）
- P2 依赖 P1（使用 ValueKind, GcRef 等）
- P3 依赖 P2（生成 bytecode）
- P4 依赖 P1（封装 runtime-core 给 JIT）
- P5 依赖 P2 + P4（翻译 bytecode，调用 runtime）
- P6 依赖 P5（组装编译器）

## ⚠️ 关键设计决策（踩坑预防）

### 1. Closure 调用约定

**决策**: Closure 作为第一个参数传入被调用函数

```rust
// 调用方
CallClosure(closure, args...) → 传递 [closure, arg0, arg1, ...]

// 被调用方
fn closure_func(closure: GcRef, arg0, arg1, ...) {
    let captured = closure.get_upval(0);  // 通过 closure 访问捕获变量
}
```

**原因**: VM 和 JIT 统一，简化 func_table 设计。

### 2. Interface Dispatch Table

**决策**: 存在 `Module.iface_dispatch` 里，VM 和 JIT 共享

```rust
pub struct IfaceDispatchEntry {
    pub concrete_type_id: u16,
    pub iface_type_id: u16,
    pub method_funcs: Vec<u32>,  // 每个方法对应的 func_id
}
```

**注意**: codegen 时需要为每个 (concrete, interface) 对生成一个 entry。

### 3. GC Stack Map

**决策**: 
- VM: 使用 `FunctionDef.slot_types` 扫描栈
- JIT: 使用 `stack_map.rs` 注册 return_addr → GcRef 位置

**关键**: Cranelift 编译时调用 `declare_var_needs_stack_map()` 标记 GC 变量。

### 4. 栈 vs 堆 struct

**决策**: 编译期决定，用不同指令

| 场景 | 指令 |
|------|------|
| 栈 struct 分配 | 直接分配 slots，无指令 |
| 栈 struct 字段访问 | `Copy` (直接 slot 偏移) |
| 堆 struct 分配 | `PtrNew` |
| 堆 struct 字段访问 | `PtrGet` / `PtrSet` |

**注意**: codegen 必须查询逃逸分析结果决定走哪条路径。

### 5. VM 和 JIT Goroutine 分离

**决策**: 不统一，各自实现

| | VM | JIT |
|---|---|---|
| 协程 | `Fiber` (内置) | `Coroutine` (corosensei) |
| 调度 | 单线程协作式 | M:N work-stealing |
| Channel | `ChannelState` 直接用 | `Mutex<ChannelState>` |

**原因**: 性能需求不同，强行统一会降低两边效率。

### 6. 嵌套 Struct 扁平化

**决策**: 栈 struct 的嵌套字段内联展开

```vo
type Inner struct { a int; b string }
type Outer struct { inner Inner; c int }
var o Outer
// 栈布局: [o.inner.a, o.inner.b, o.c] = 3 slots
```

**注意**: `slot_types` 必须正确反映每个 slot 是 Value 还是 GcRef。

## Timeline (Estimated)

| Week | Phase | Work |
|------|-------|------|
| 1 | P1 | runtime-core: ValueKind, GcHeader, Gc, 对象操作 |
| 2 | P2 | vm: Opcode, Instruction, Module, bytecode 序列化 |
| 3 | P2 | vm: VM 执行器 (算术/控制流/函数调用) |
| 4 | P2 | vm: VM 执行器 (对象操作/并发/异常) |
| 5 | P3 | codegen-vm: 基础 (context, func, type_info) |
| 6 | P3 | codegen-vm: 表达式/语句 |
| 7 | P3 | codegen-vm: 复合类型/多态/并发 |
| 8 | P4 | runtime-native: gc_global, symbols, goroutine |
| 9 | P5 | codegen-cranelift: translate 基础 |
| 10 | P5 | codegen-cranelift: 完整指令翻译 |
| 11 | P6 | aot + jit: 编译器入口 |
| 12 | - | 集成测试 + Bug 修复 |

## Success Criteria

1. 所有 `test_data/*.vo` 测试通过 (VM)
2. 所有 `test_data/*.vo` 测试通过 (JIT/AOT)
3. 逃逸分析正确：栈变量不逃逸，堆变量正确分配
4. GC 正确：无内存泄漏，无悬挂指针
5. 并发正确：goroutine + channel + select 工作正常

## Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| Cranelift API 变化 | 高 | 锁定版本 0.113 |
| 栈扁平化复杂度 | 中 | 先实现堆版本，再优化栈版本 |
| GC stack_map 不准确 | 高 | 充分单元测试，用 valgrind 检查 |
| 指令集遗漏 | 低 | 参考现有实现，逐条对照 |

## 现有代码参考

重写时可参考（但不拷贝）：

| 模块 | 参考文件 | 注意事项 |
|------|----------|----------|
| GC | `vo-runtime-core/src/gc.rs` | 保留现有 GC 逻辑 |
| 对象布局 | `vo-runtime-core/src/objects/` | 新增 Boxed 类型 |
| 指令集 | `vo-vm/src/instruction.rs` | 新增栈操作指令 |
| VM 执行 | `vo-vm/src/vm.rs` | 重点参考 |
| Codegen | `vo-codegen-vm/src/` | 需要集成逃逸分析 |
| Cranelift 翻译 | `vo-codegen-cranelift/src/translate.rs` | stack_map 处理 |
| Runtime 符号 | `vo-runtime-native/src/symbols.rs` | 函数注册 |
