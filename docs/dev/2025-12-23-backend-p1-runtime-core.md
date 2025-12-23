# Backend P1: vo-runtime-core

**Parent**: [2025-12-23-backend-rewrite-plan.md](2025-12-23-backend-rewrite-plan.md)  
**Status**: Not Started  
**Est. Modules**: 28

## Overview

定义所有后端共享的数据结构：类型系统、GC、对象操作。这是整个后端的基础，VM 和 JIT 都依赖它。

**核心原则**：
- 保持 `no_std` 兼容（核心模块）
- 不包含 goroutine（VM 和 JIT 各自实现）
- 提供两套接口：Rust 方法（给 VM）和 extern "C"（给 JIT）

## 模块清单

### 1. 类型定义 (types.rs)

```rust
/// 值类型标签 - 区分所有类型
/// 参考: docs/spec/memory-model-and-instructions.md
#[repr(u8)]
pub enum ValueKind {
    // === 基础类型 (1 slot, 无 GC) ===
    Nil = 0, Bool = 1, 
    Int = 2, Int8 = 3, Int16 = 4, Int32 = 5, Int64 = 6,
    Uint = 7, Uint8 = 8, Uint16 = 9, Uint32 = 10, Uint64 = 11,
    Float32 = 12, Float64 = 13, 
    FuncPtr = 14,       // 裸函数指针（无捕获）
    
    // === 复合值类型 (多 slot, 可能含 GC 引用) ===
    Array = 16,         // [N]T - 元素内联
    Struct = 21,        // 字段内联
    Interface = 23,     // 2 slots: header + data
    
    // === 引用类型 (1 slot GcRef, 堆分配) ===
    String = 15,
    Slice = 17,
    Map = 18,
    Channel = 19,
    Closure = 20,       // 带捕获的闭包
    Pointer = 22,       // *T - 指向逃逸值的指针（统一表示所有逃逸值）
}

/// Slot 类型 - 用于 GC 扫描
#[repr(u8)]
pub enum SlotType {
    Value = 0,       // 非 GC 值
    GcRef = 1,       // GC 引用
    Interface0 = 2,  // interface 第一个 slot (header)
    Interface1 = 3,  // interface 第二个 slot (data, 需动态检查)
}
```

**⚠️ 关键设计 - Pointer 统一表示逃逸值**：

逃逸的基础类型（int/float/bool）和逃逸的 struct/array 都用 `Pointer` 表示：
- 栈上变量类型：`SlotType::GcRef`，值是 `Pointer`
- 堆对象 `GcHeader.value_kind`：记录**原始类型**（Int/Float/Bool/Struct/Array）
- **没有独立的 BoxedInt/BoxedFloat/BoxedBool 类型**

示例：
```
// 逃逸的 int
栈上: slot_type = GcRef, value = GcRef 指向堆
堆上: GcHeader { value_kind: Int, slots: 1 }, data: [42]

// 逃逸的 struct Point { x, y int }
栈上: slot_type = GcRef, value = GcRef 指向堆
堆上: GcHeader { value_kind: Struct, meta_id: 0, slots: 2 }, data: [x, y]
```

**其他注意**：
- `ValueKind` 存在 GcHeader 里，决定对象如何扫描
- `SlotType` 存在 FunctionDef.slot_types 里，决定栈如何扫描
- 两者配合使用，不要搞混

### 2. GC 核心 (gc.rs)

```rust
/// GC 对象头 - 8 字节
/// 布局: [mark:8 | gen:8 | slots:16 | meta_id:24 | value_kind:8]
#[repr(C)]
pub struct GcHeader {
    pub mark: u8,         // GcColor: White/Gray/Black
    pub gen: u8,          // GcGen: Young/Old/Touched
    pub slots: u16,       // 数据 slot 数量
    pub meta_id: [u8; 3], // 24-bit 元数据索引
    pub value_kind: u8,   // [is_array:1 | kind:7]
}

/// value_kind 字段拆分:
/// - bit 7 (0x80): is_array 标记
///   - 1 = Array 对象, kind 是元素类型
///   - 0 = 普通对象, kind 是对象类型
/// - bit 0-6: ValueKind 值 (0-127)
///
/// meta_id 含义随 kind 变化:
/// - Struct: struct_metas[] 索引
/// - Interface: interface_metas[] 索引
/// - Array (is_array=1):
///   - 元素是 Struct/Interface 时: 元素的 meta_id
///   - 元素是简单类型时: 0
/// - 其他: 0

/// GC 对象
#[repr(C)]
pub struct GcObject {
    pub header: GcHeader,
    // data: [u64; slots] 紧跟其后
}

pub type GcRef = *mut GcObject;

/// GC 主结构
pub struct Gc {
    all_objects: Vec<GcObjectEntry>,
    gray_queue: Vec<GcRef>,
    total_bytes: usize,
    threshold: usize,
    // ...
}
```

**关键接口**：

```rust
impl Gc {
    /// 分配对象
    pub fn alloc(&mut self, value_kind: u8, meta_id: u32, slots: u16) -> GcRef;
    
    /// 读写 slot (静态方法，不需要 &self)
    pub fn read_slot(obj: GcRef, idx: usize) -> u64;
    pub fn write_slot(obj: GcRef, idx: usize, val: u64);
    
    /// 标记
    pub fn mark_gray(&mut self, obj: GcRef);
    
    /// 收集 (需要提供 scan_object 回调)
    pub fn collect<F>(&mut self, scan_object: F) where F: FnMut(&mut Gc, GcRef);
    
    /// 写屏障 (增量 GC 用)
    pub fn write_barrier(&mut self, parent: GcRef, child: GcRef);
}
```

**⚠️ 注意**：
- `read_slot`/`write_slot` 是静态方法，不锁 GC
- `collect` 需要调用方提供 `scan_object` 回调，因为扫描逻辑在 `gc_types.rs`

### 3. 类型元信息 (types.rs 续)

```rust
/// 通用类型元信息
pub struct TypeMeta {
    pub name: String,
    pub slot_count: u16,
    pub slot_types: Vec<SlotType>,
}

/// Struct 专用元信息
pub struct StructMeta {
    pub name: String,
    pub field_names: Vec<String>,
    pub field_offsets: Vec<u16>,   // 每个字段的 slot 偏移
    pub slot_types: Vec<SlotType>, // 用于 GC 扫描
}

/// Interface 专用元信息
pub struct InterfaceMeta {
    pub name: String,
    pub method_names: Vec<String>,
}
```

### 4. 对象操作 (objects/*.rs)

每种对象类型一个子模块：

#### 4.1 String (objects/string.rs)

```rust
/// String 布局: [array: GcRef, start, len] (3 slots)
/// array 是一个 Array<u8>，支持 slice 语义

pub fn create(gc: &mut Gc, bytes: &[u8]) -> GcRef;
pub fn len(s: GcRef) -> usize;
pub fn index(s: GcRef, i: usize) -> u8;
pub fn concat(gc: &mut Gc, a: GcRef, b: GcRef) -> GcRef;
pub fn slice_of(gc: &mut Gc, s: GcRef, start: usize, end: usize) -> GcRef;
pub fn eq(a: GcRef, b: GcRef) -> bool;
pub fn cmp(a: GcRef, b: GcRef) -> i32;
```

#### 4.2 Array (objects/array.rs)

```rust
/// Array 布局: [header, data...] (2+N slots)
/// header = [len:48 | elem_slots:16]
/// 元素类型信息在 GcHeader.value_kind (is_array=1, kind=元素类型)
/// 元素的 meta_id 在 GcHeader.meta_id (Struct/Interface 元素时)

pub fn create(gc: &mut Gc, elem_kind: u8, elem_meta_id: u32, elem_slots: u16, len: usize) -> GcRef;
pub fn len(arr: GcRef) -> usize;
pub fn get(arr: GcRef, idx: usize) -> u64;
pub fn set(arr: GcRef, idx: usize, val: u64);
```

**⚠️ 注意**：
- `elem_slots` 决定每个元素占用的 slot 数（struct 可能占多个）
- 元素类型信息存在 GcHeader 中，GC 扫描时使用

#### 4.3 Slice (objects/slice.rs)

```rust
/// Slice 布局: [array_ref, start, len, cap] (4 slots)

pub fn create(gc: &mut Gc, array: GcRef, start: usize, len: usize, cap: usize) -> GcRef;
pub fn len(s: GcRef) -> usize;
pub fn cap(s: GcRef) -> usize;
pub fn get(s: GcRef, idx: usize) -> u64;
pub fn set(s: GcRef, idx: usize, val: u64);
pub fn append(gc: &mut Gc, s: GcRef, val: u64) -> GcRef;  // elem 信息从 slice.array 的 GcHeader 读取
pub fn slice_of(gc: &mut Gc, s: GcRef, start: usize, end: usize) -> GcRef;
```

#### 4.4 Map (objects/map.rs)

```rust
/// Map 使用 hash table 实现
/// 布局: [key_kind, val_kind, len, cap, ...buckets...]

pub fn create(gc: &mut Gc, key_kind: u8, val_kind: u8) -> GcRef;
pub fn len(m: GcRef) -> usize;
pub fn get(m: GcRef, key: u64) -> (u64, bool);  // (value, found)
pub fn set(gc: &mut Gc, m: GcRef, key: u64, val: u64);
pub fn delete(m: GcRef, key: u64);
```

**⚠️ 注意**：
- struct 作为 key 需要实现 hash，见 `StructHash` 指令
- Map 需要 std，用 `#[cfg(feature = "std")]` gate

#### 4.5 Closure (objects/closure.rs)

```rust
/// Closure 布局: [func_id, upval_count, upval[0], upval[1], ...]

pub fn create(gc: &mut Gc, func_id: u32, upval_count: usize) -> GcRef;
pub fn func_id(c: GcRef) -> u32;
pub fn upval_count(c: GcRef) -> usize;
pub fn get_upvalue(c: GcRef, idx: usize) -> u64;
pub fn set_upvalue(c: GcRef, idx: usize, val: u64);

/// Upval Box - 用于引用捕获
pub fn create_upval_box(gc: &mut Gc) -> GcRef;
pub fn get_upval_box(uv: GcRef) -> u64;
pub fn set_upval_box(uv: GcRef, val: u64);
```

**⚠️ 关键**：
- `upval` 可以是值（值捕获）或 `UpvalBox`（引用捕获）
- `UpvalBox` 是一个 1-slot 的 GcObject，用于共享可变状态

#### 4.6 Channel (objects/channel.rs)

```rust
/// Channel 状态机 - VM 和 JIT 共享核心逻辑
pub struct ChannelState {
    pub buffer: VecDeque<u64>,
    pub capacity: usize,
    pub closed: bool,
    // waiters 由上层管理 (VM: Fiber, JIT: Condvar)
}

impl ChannelState {
    pub fn new(capacity: usize) -> Self;
    pub fn try_send(&mut self, val: u64) -> Result<(), ChannelError>;
    pub fn try_recv(&mut self) -> Result<u64, ChannelError>;
    pub fn close(&mut self);
}

pub enum ChannelError {
    WouldBlock,  // 需要等待
    Closed,      // 已关闭
}
```

**⚠️ 注意**：
- `ChannelState` 只是核心逻辑
- VM 包装成 `Channel` 直接用
- JIT 包装成 `Mutex<ChannelState>` + Condvar

### 5. GC 对象扫描 (gc_types.rs)

```rust
/// 扫描 GC 对象，标记子引用
pub fn scan_object(gc: &mut Gc, obj: GcRef) {
    let header = unsafe { &(*obj).header };
    let value_kind = ValueKind::from_u8(header.value_kind);
    
    match value_kind {
        ValueKind::String | ValueKind::Slice => {
            // slot[0] 是 array_ref
            let child = Gc::read_slot(obj, 0);
            if child != 0 { gc.mark_gray(child as GcRef); }
        }
        ValueKind::Struct => {
            // 查 struct_metas[type_id].slot_types
            let slot_types = get_struct_slot_types(header.type_id);
            for (i, &st) in slot_types.iter().enumerate() {
                if st == SlotType::GcRef {
                    let child = Gc::read_slot(obj, i);
                    if child != 0 { gc.mark_gray(child as GcRef); }
                }
                // Interface1 需要动态检查
            }
        }
        ValueKind::Closure => {
            // 扫描所有 upvalues
            let count = closure::upval_count(obj);
            for i in 0..count {
                let uv = closure::get_upvalue(obj, i);
                if uv != 0 { gc.mark_gray(uv as GcRef); }
            }
        }
        // ... 其他类型
        _ => {}
    }
}
```

### 6. FFI 导出 (ffi.rs)

给 JIT 用的 `extern "C"` 接口：

```rust
#[no_mangle]
pub extern "C" fn vo_string_len(s: GcRef) -> i64 {
    string::len(s) as i64
}

#[no_mangle]
pub extern "C" fn vo_string_index(s: GcRef, i: i64) -> u8 {
    string::index(s, i as usize)
}

// ... 约 40 个函数
```

**⚠️ 注意**：
- 这些函数**不需要 GC 参数**，分配操作在 `gc_global.rs` (runtime-native)
- 返回值用 i64/u64，不用 usize（C ABI）

## Tasks Checklist

### types.rs
- [ ] ValueKind 枚举（无 Boxed 类型，Pointer 统一表示逃逸值）
- [ ] SlotType 枚举
- [ ] ValueKind 辅助方法 (is_ref_type, may_contain_gc_refs)
- [ ] TypeMeta / StructMeta / InterfaceMeta

### gc.rs
- [ ] GcHeader (8 字节)
- [ ] GcObject 结构
- [ ] Gc::alloc
- [ ] Gc::read_slot / write_slot (静态方法)
- [ ] Gc::mark_gray
- [ ] Gc::collect (带 scan_object 回调)
- [ ] Gc::write_barrier

### gc_types.rs
- [ ] scan_object 函数
- [ ] get_struct_slot_types 辅助函数
- [ ] struct_metas 全局表（或作为参数传入）

### objects/string.rs
- [ ] create, len, index
- [ ] concat, slice_of
- [ ] eq, cmp

### objects/array.rs
- [ ] create, len
- [ ] get, set

### objects/slice.rs
- [ ] create, len, cap
- [ ] get, set
- [ ] append, slice_of

### objects/map.rs
- [ ] create, len
- [ ] get, set, delete
- [ ] (需要 std feature)

### objects/closure.rs
- [ ] create, func_id, upval_count
- [ ] get_upvalue, set_upvalue
- [ ] create_upval_box, get_upval_box, set_upval_box

### objects/channel.rs
- [ ] ChannelState 结构
- [ ] try_send, try_recv, close

### ffi.rs
- [ ] 所有 extern "C" 导出（~40 个）

## 单元测试

```rust
#[test]
fn test_gc_alloc_and_free() {
    let mut gc = Gc::new();
    let obj = gc.alloc(ValueKind::Struct as u8, 0, 2);
    assert!(!obj.is_null());
    Gc::write_slot(obj, 0, 42);
    assert_eq!(Gc::read_slot(obj, 0), 42);
}

#[test]
fn test_string_concat() {
    let mut gc = Gc::new();
    let a = string::create(&mut gc, b"hello");
    let b = string::create(&mut gc, b" world");
    let c = string::concat(&mut gc, a, b);
    // 验证内容
}

#[test]
fn test_gc_collect() {
    let mut gc = Gc::new();
    // 创建一些对象
    // 不保留引用
    // collect
    // 验证被回收
}
```
