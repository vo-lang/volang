---
date: 2026-08-10
status: implemented
area: runtime
owner: volang
supersedes: []
superseded_by: []
---

# Packed Array/Slice 设计方案

## 概述

将堆上 array/slice 元素从 slot-based (8 bytes/elem) 改为按实际类型大小存储，减少内存浪费。

**Channel 不优化**：Channel buffer 是 per-element 独立 Box 分配，优化收益低，保持 slot-based。

## 方案选择

**方案 A（当前实现）**：只优化 primitive 类型，保留未来 struct 优化扩展性。

| 元素类型 | elem_bytes | 存储方式 |
|---------|-----------|---------|
| bool | 1 | packed |
| int8/uint8 | 1 | packed |
| int16/uint16 | 2 | packed |
| int32/uint32/float32 | 4 | packed |
| int64/uint64/float64/int/uint | 8 | slot-based |
| pointer/slice/map/chan/string | 8 | slot-based (GcRef) |
| interface | 16 | slot-based |
| struct (任何) | slots * 8 | slot-based (未来优化) |
| [N]T (任何) | slots * 8 | slot-based (未来优化) |

## 核心规则

### elem_bytes 计算

```rust
fn elem_bytes_for_heap(elem_type: TypeKey, tc_objs: &TCObjects) -> usize {
    let vk = type_value_kind(elem_type, tc_objs);
    match vk {
        // Packed: primitive 类型
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => 1,
        ValueKind::Int16 | ValueKind::Uint16 => 2,
        ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => 4,
        // Slot-based: 其他所有类型
        _ => type_slot_count(elem_type, tc_objs) as usize * 8,
    }
}
```

### 布局对比

```
[]bool (长度 8):
  旧: [8 bytes][8 bytes][8 bytes]... = 64 bytes
  新: [1 byte][1 byte][1 byte]... = 8 bytes (节省 87.5%)

[]int32 (长度 4):
  旧: [8 bytes][8 bytes][8 bytes][8 bytes] = 32 bytes
  新: [4 bytes][4 bytes][4 bytes][4 bytes] = 16 bytes (节省 50%)
```

---

## 🔴 当前代码存在的问题

### BUG 1: SliceData.start 语义不一致

**`SliceData.start` 应该是 element index**，但当前代码使用不一致：

| 函数 | `start` 被当作 | 是否正确 |
|------|---------------|---------|
| `slice_of` | element index | ✅ |
| `slice_of_with_cap` | element index | ✅ |
| `append` (乘 es) | element index | ✅ |
| `get/set` | **slot offset** | ❌ **BUG** |
| `get_n/set_n` | **slot offset** | ❌ **BUG** |

**证据**：

```rust
// slice.rs:74 - get 直接把 start + offset 传给 array::get
pub fn get(s: GcRef, offset: usize) -> u64 { 
    array::get(array_ref(s), start(s) + offset)  // ❌ 假设 start 是 slot offset
}

// slice.rs:103 - append 中 start 乘以 es
array::set_n(data.array, (data.start + cur_len) * es, val);  // ✅ start 是 element index
```

**影响**：当 `elem_slots > 1` 时（如 `[]interface{}`），`slice.get/set` 会访问错误位置。
目前没暴露是因为大部分测试用 `elem_slots=1` 的类型。

### BUG 2: array 函数参数语义混乱

当前 `array.rs` 函数参数名叫 `offset`，但语义是 **slot offset**，不是 element index：

```rust
// array.rs:75-77
pub fn get(arr: GcRef, offset: usize) -> u64 {
    unsafe { *data_ptr(arr).add(offset) }  // offset 是 slot 偏移
}
```

这导致调用者必须自己乘以 elem_slots，容易出错。

### BUG 3: VM 宏与 slice.rs 函数不一致

VM 使用宏直接访问内存，不调用 `slice::get/set`：

```rust
// vm.rs - 直接用宏
macro_rules! slice_get {
    ($s:expr, $offset:expr) => {{
        let arr = slice_array!($s);
        let start = slice_start!($s);
        array_get!(arr, start + $offset)  // start 被当作 slot offset
    }};
}

// vm.rs:1047-1055 - SliceGet 指令
let offset = idx * elem_slots;  // 先乘 elem_slots
slice_get!(s, offset + i);      // 再加 start
```

但 `slice.rs` 中 `append` 把 start 当作 element index。

**影响**：VM 和 runtime 函数对 start 的理解不一致。

### BUG 4: JIT translate 中 start 的处理与 VM 不一致

```rust
// translate.rs:1277-1280 - JIT 把 start 当作 element index
let total_idx = self.builder.ins().iadd(start, idx);
let slot_offset = self.builder.ins().imul(total_idx, elem_slots_val);  // (start + idx) * elem_slots
```

但 VM 宏做的是 `start + idx * elem_slots`。

**影响**：当 elem_slots > 1 时，VM 和 JIT 结果不一致。目前测试通过是因为 elem_slots=1。

### 问题 5: string.rs 中 array::create 调用

```rust
// string.rs:46
let arr = array::create(gc, ValueMeta::new(0, ValueKind::Uint8), 1, bytes.len());
```

当前传的是 `elem_slots=1`。改动后参数语义变为 `elem_bytes`，数值仍然是 1，但需要确认。

### 问题 6: exec_slice_append 需要同时知道 elem_bytes 和 elem_slots

**当前代码**：
```rust
// exec/slice.rs:55-67
let elem_slots = inst.flags as usize;
let val: &[u64] = &fiber.stack[src_start..src_start + elem_slots];
let result = slice::append(gc, elem_meta, elem_slots, s, val);
```

**问题**：改动后 `inst.flags` 变成 `elem_bytes`，但需要两个信息：
- `elem_bytes`：传给 `slice::append` 用于堆操作
- `elem_slots`：从栈读取数据（栈始终是 slot-based）

**正确实现**：
```rust
let elem_bytes = inst.flags as usize;
let elem_slots = (elem_bytes + 7) / 8;  // 从 elem_bytes 计算 slot 数量
let val: &[u64] = &fiber.stack[src_start..src_start + elem_slots];
let result = slice::append(gc, elem_meta, elem_bytes, s, val);
```

### 问题 7: GC scan_array 需要修改

`scan_array` 调用 `array::get(obj, i)`，改动后签名变化，**必须修改**：

```rust
// gc_types.rs - 当前代码
for i in 0..data_slots {
    let child = array::get(obj, i);  // ❌ 签名变了
    ...
}
```

**修复方案**：

```rust
fn scan_array(gc: &mut Gc, obj: GcRef) {
    let elem_kind = array::elem_kind(obj);
    if !elem_kind.may_contain_gc_refs() { return; }
    
    // 包含 GcRef 的类型，elem_bytes 一定是 8 的倍数
    let len = array::len(obj);
    let elem_bytes = array::elem_bytes(obj);
    let elem_slots = elem_bytes / 8;
    
    for idx in 0..len {
        for slot in 0..elem_slots {
            let byte_off = idx * elem_bytes + slot * 8;
            let child = unsafe { *(data_ptr_bytes(obj).add(byte_off) as *const u64) };
            if child != 0 { gc.mark_gray(child as GcRef); }
        }
    }
}
```

**核心规则**：packed 类型不包含 GcRef，所以 `may_contain_gc_refs()` 返回 false 时直接跳过。

### 问题 8: vo_copy 未实现

当前 codegen 已用 `CallExtern` 调用 `vo_copy`，但 **vo_copy 未实现且未注册**：

```rust
// vo-codegen/src/expr.rs:1530-1536 - 已有
"copy" => {
    let extern_id = _ctx.get_or_register_extern("vo_copy");
    ...
    func.emit_with_flags(Opcode::CallExtern, 2, dst, extern_id as u16, args_start);
}
```

**需要做的**：
1. 在 `vo-runtime/src/jit_api.rs` 实现 `vo_copy` 函数
2. 在 `get_runtime_symbols()` 注册 `vo_copy`

**不需要**新增 `SliceCopy` 指令，复用现有 extern call 机制即可。

### 问题 9: JIT flags=0 fallback 处理

文档 5.4 节给出了 VM 的 `flags=0` 处理：
```rust
let elem_bytes = if elem_bytes == 0 { array::elem_bytes(arr) } else { elem_bytes };
```

但 **JIT inline 实现（6.2 节）没有处理 `flags=0` 的情况**。需要补充：
```rust
// JIT 中处理 flags=0
let elem_bytes = inst.flags as usize;
if elem_bytes == 0 {
    // 生成代码从 ArrayHeader 读取 elem_bytes
    let elem_bytes_offset = 12; // ArrayHeader: len(8) + elem_meta(4) = 12
    let eb = self.builder.ins().load(types::I32, MemFlags::trusted(), arr, elem_bytes_offset);
    let eb_i64 = self.builder.ins().uextend(types::I64, eb);
    // 使用 eb_i64 作为 elem_bytes
}
```

### 问题 10: codegen 中 elem_bytes > 255 的检测

文档 "边界情况处理" 节说 `elem_bytes > 255` 时生成 `flags=0`，但 **没有说明 codegen 如何检测**。

需要在 vo-codegen 的 `array_elem_bytes` / `slice_elem_bytes` 使用处加判断：
```rust
let elem_bytes = info.slice_elem_bytes(type_key);
let flags = if elem_bytes > 255 { 0 } else { elem_bytes as u8 };
```

### 问题 11: for-range 展开需要使用 elem_bytes

for-range slice/array 在 codegen 阶段展开，当前使用 `elem_slots`。需要改为 `elem_bytes`。

影响的代码：
- `vo-codegen` 中 for-range 展开生成的 `ArrayGet`/`SliceGet` 指令的 flags

### 问题 12: nil slice append 丢失 elem_meta

**根本问题**：nil slice = GcRef 为 0，没有 SliceData 结构体存在，**无法获取 elem_meta**。

```rust
// exec/slice.rs - 当前代码
let elem_meta = if s.is_null() {
    ValueMeta::from_raw(0)  // ❌ 丢失类型信息！
} else {
    slice::elem_meta(s)
};
```

**解决方案**：SliceAppend 指令携带 elem_meta（连续栈模式，类似 MapSet）：

```
// 当前格式
SliceAppend: a=dst, b=slice, c=meta_and_elem, flags=0
// c: [elem_meta (1 slot)]
// c+1..: [elem (elem_slots)]
// per-PC ElemLayout: elem_bytes + logical slot layout
```

**Codegen**：
```rust
let meta_and_elem_reg = func.alloc_temp(1 + elem_slots);
let (b, c) = encode_i32(elem_meta as i32);
func.emit_op(Opcode::LoadInt, meta_and_elem_reg, b, c);  // meta
compile_expr_to(&call.args[1], meta_and_elem_reg + 1, ...);  // elem
func.emit_with_flags(Opcode::SliceAppend, elem_bytes as u8, dst, slice_reg, meta_and_elem_reg);
```

**VM exec**：
```rust
let meta = fiber.read_reg(inst.c) as u32;
let elem_meta = ValueMeta::from_raw(meta);
let elem_bytes = inst.flags as usize;
let elem_slots = (elem_bytes + 7) / 8;
let bp = frame.bp;
let val = &fiber.stack[bp + inst.c as usize + 1 .. bp + inst.c as usize + 1 + elem_slots];
let result = slice::append(gc, elem_meta, elem_bytes, s, val);
```

---

## 详细改动清单

### 1. vo-analysis/check/type_info.rs

新增函数：

```rust
/// 类型的实际字节大小（紧密排列，无 padding）
pub fn type_byte_size(type_key: TypeKey, tc_objs: &TCObjects) -> usize

/// 类型是否包含 GcRef（需要 GC 扫描）
pub fn type_has_gc_refs(type_key: TypeKey, tc_objs: &TCObjects) -> bool

/// 堆上元素的字节大小
pub fn elem_bytes_for_heap(type_key: TypeKey, tc_objs: &TCObjects) -> usize
```

### 2. vo-runtime/objects/array.rs

#### 2.1 ArrayHeader 不变

```rust
pub struct ArrayHeader {
    pub len: usize,
    pub elem_meta: ValueMeta,
    pub elem_bytes: u32,  // 当前已存储 elem_slots * 8，改为存实际字节数
}
```

#### 2.2 新增 helper 函数

```rust
/// 返回数据区的字节指针（跳过 header）
#[inline]
fn data_ptr_bytes(arr: GcRef) -> *mut u8 {
    unsafe { (arr as *mut u8).add(HEADER_SLOTS * 8) }
}
```

#### 2.3 函数签名变化

**所有函数参数从 slot offset 改为 element index + elem_bytes**：

```rust
// 旧签名
pub fn create(gc, elem_meta, elem_slots, length) -> GcRef
pub fn get(arr, slot_offset) -> u64
pub fn set(arr, slot_offset, val)
pub fn get_n(arr, slot_offset, dest: &mut [u64])
pub fn set_n(arr, slot_offset, src: &[u64])
pub fn copy_range(src, src_slot_off, dst, dst_slot_off, slot_count)

// 新签名
pub fn create(gc, elem_meta, elem_bytes, length) -> GcRef
pub fn get(arr, idx, elem_bytes) -> u64
pub fn set(arr, idx, val, elem_bytes)
pub fn get_n(arr, idx, dest: &mut [u64], elem_bytes)
pub fn set_n(arr, idx, src: &[u64], elem_bytes)
pub fn copy_range(src, src_idx, dst, dst_idx, count, elem_bytes)
```

#### 2.4 create 实现

```rust
pub fn create(gc: &mut Gc, elem_meta: ValueMeta, elem_bytes: usize, length: usize) -> GcRef {
    let data_bytes = length * elem_bytes;
    let data_slots = (data_bytes + 7) / 8;  // 向上取整到 8 字节边界
    let total_slots = HEADER_SLOTS + data_slots;
    let array_meta = ValueMeta::new(0, ValueKind::Array);
    let arr = gc.alloc(array_meta, total_slots as u16);
    let header = ArrayHeader::as_mut(arr);
    header.len = length;
    header.elem_meta = elem_meta;
    header.elem_bytes = elem_bytes as u32;
    arr
}
```

#### 2.5 get/set 实现

```rust
/// 读取单个元素（返回 u64，小于 8 字节的类型零扩展）
#[inline]
pub fn get(arr: GcRef, idx: usize, elem_bytes: usize) -> u64 {
    let byte_offset = idx * elem_bytes;
    let ptr = data_ptr_bytes(arr);
    unsafe {
        match elem_bytes {
            1 => *ptr.add(byte_offset) as u64,
            2 => *(ptr.add(byte_offset) as *const u16) as u64,
            4 => *(ptr.add(byte_offset) as *const u32) as u64,
            8 => *(ptr.add(byte_offset) as *const u64),
            _ => *(ptr.add(byte_offset) as *const u64),  // multi-slot: 只返回第一个 slot
        }
    }
}

/// 写入单个元素（val 是 u64，小于 8 字节的类型截断低位）
#[inline]
pub fn set(arr: GcRef, idx: usize, val: u64, elem_bytes: usize) {
    let byte_offset = idx * elem_bytes;
    let ptr = data_ptr_bytes(arr);
    unsafe {
        match elem_bytes {
            1 => *ptr.add(byte_offset) = val as u8,
            2 => *(ptr.add(byte_offset) as *mut u16) = val as u16,
            4 => *(ptr.add(byte_offset) as *mut u32) = val as u32,
            8 => *(ptr.add(byte_offset) as *mut u64) = val,
            _ => *(ptr.add(byte_offset) as *mut u64) = val,
        }
    }
}
```

#### 2.6 get_n/set_n 实现（多 slot 元素）

```rust
/// 读取元素到 dest（支持 packed 和 multi-slot）
pub fn get_n(arr: GcRef, idx: usize, dest: &mut [u64], elem_bytes: usize) {
    let byte_offset = idx * elem_bytes;
    let ptr = unsafe { data_ptr_bytes(arr).add(byte_offset) };
    match elem_bytes {
        1 => dest[0] = unsafe { *ptr } as u64,
        2 => dest[0] = unsafe { *(ptr as *const u16) } as u64,
        4 => dest[0] = unsafe { *(ptr as *const u32) } as u64,
        _ => {
            // slot-based: 复制所有 slots
            let elem_slots = (elem_bytes + 7) / 8;
            for i in 0..elem_slots {
                dest[i] = unsafe { *(ptr.add(i * 8) as *const u64) };
            }
        }
    }
}

/// 从 src 写入多 slot 元素
pub fn set_n(arr: GcRef, idx: usize, src: &[u64], elem_bytes: usize) {
    let byte_offset = idx * elem_bytes;
    let ptr = unsafe { data_ptr_bytes(arr).add(byte_offset) };
    // 对于 packed 类型，只写低位字节
    match elem_bytes {
        1 => unsafe { *ptr = src[0] as u8 },
        2 => unsafe { *(ptr as *mut u16) = src[0] as u16 },
        4 => unsafe { *(ptr as *mut u32) = src[0] as u32 },
        _ => {
            // slot-based: 复制所有 slots
            let elem_slots = (elem_bytes + 7) / 8;
            let slot_ptr = ptr as *mut u64;
            for i in 0..elem_slots {
                unsafe { *slot_ptr.add(i) = src[i] };
            }
        }
    }
}
```

#### 2.7 copy_range 实现

```rust
/// 复制元素范围（按 elem_bytes 复制）
pub fn copy_range(
    src: GcRef, src_idx: usize,
    dst: GcRef, dst_idx: usize,
    count: usize, elem_bytes: usize
) {
    let src_ptr = data_ptr_bytes(src).add(src_idx * elem_bytes);
    let dst_ptr = data_ptr_bytes(dst).add(dst_idx * elem_bytes);
    let byte_count = count * elem_bytes;
    unsafe {
        core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, byte_count);
    }
}
```

### 3. vo-runtime/objects/slice.rs

#### 3.1 SliceData.start 语义明确

**`start` 是 element index**，所有代码统一这个语义。

```rust
pub struct SliceData {
    pub array: GcRef,
    pub start: usize,  // element index（不是 byte offset，不是 slot offset）
    pub len: usize,    // element count
    pub cap: usize,    // element count
}
```

#### 3.2 函数签名变化

```rust
// 旧签名
pub fn create(gc, elem_meta, elem_slots, length, capacity) -> GcRef
pub fn get(s, offset) -> u64
pub fn set(s, offset, val)
pub fn get_n(s, offset, dest)
pub fn set_n(s, offset, src)
pub fn append(gc, em, es, s, val) -> GcRef

// 新签名
pub fn create(gc, elem_meta, elem_bytes, length, capacity) -> GcRef
pub fn get(s, idx, elem_bytes) -> u64
pub fn set(s, idx, val, elem_bytes)
pub fn get_n(s, idx, dest, elem_bytes)
pub fn set_n(s, idx, src, elem_bytes)
pub fn append(gc, em, elem_bytes, s, val) -> GcRef
```

#### 3.3 get/set 实现

```rust
#[inline]
pub fn get(s: GcRef, idx: usize, elem_bytes: usize) -> u64 {
    // start 是 element index，直接相加
    array::get(array_ref(s), start(s) + idx, elem_bytes)
}

#[inline]
pub fn set(s: GcRef, idx: usize, val: u64, elem_bytes: usize) {
    array::set(array_ref(s), start(s) + idx, val, elem_bytes);
}

pub fn get_n(s: GcRef, idx: usize, dest: &mut [u64], elem_bytes: usize) {
    array::get_n(array_ref(s), start(s) + idx, dest, elem_bytes);
}

pub fn set_n(s: GcRef, idx: usize, src: &[u64], elem_bytes: usize) {
    array::set_n(array_ref(s), start(s) + idx, src, elem_bytes);
}
```

#### 3.4 append 实现

```rust
pub fn append(gc: &mut Gc, em: ValueMeta, elem_bytes: usize, s: GcRef, val: &[u64]) -> GcRef {
    if s.is_null() {
        let new_arr = array::create(gc, em, elem_bytes, 4);
        array::set_n(new_arr, 0, val, elem_bytes);
        return from_array_range(gc, new_arr, 0, 1, 4);
    }
    let data = SliceData::as_ref(s);
    let cur_len = data.len;
    let cur_cap = data.cap;
    if cur_len < cur_cap {
        // idx = start + cur_len (element index)
        array::set_n(data.array, data.start + cur_len, val, elem_bytes);
        SliceData::as_mut(s).len = cur_len + 1;
        s
    } else {
        let new_cap = if cur_cap == 0 { 4 } else { cur_cap * 2 };
        let aem = elem_meta(s);
        let new_arr = array::create(gc, aem, elem_bytes, new_cap);
        // copy by element count
        array::copy_range(data.array, data.start, new_arr, 0, cur_len, elem_bytes);
        array::set_n(new_arr, cur_len, val, elem_bytes);
        from_array_range(gc, new_arr, 0, cur_len + 1, new_cap)
    }
}
```

### 4. vo-codegen

#### 4.1 TypeInfoWrapper 新增方法

```rust
/// 堆上数组元素的字节大小
pub fn array_elem_bytes(&self, type_key: TypeKey) -> usize {
    let elem_type = self.array_elem_type(type_key);
    elem_bytes_for_heap(elem_type, self.tc_objs())
}

/// 堆上 slice 元素的字节大小
pub fn slice_elem_bytes(&self, type_key: TypeKey) -> usize {
    let elem_type = self.slice_elem_type(type_key);
    elem_bytes_for_heap(elem_type, self.tc_objs())
}

/// Channel 元素字节大小（不优化，保持 slot-based）
pub fn chan_elem_bytes(&self, type_key: TypeKey) -> usize {
    self.chan_elem_slots(type_key) as usize * 8
}
```

#### 4.2 ContainerKind 改动

```rust
pub enum ContainerKind {
    StackArray { base_slot: u16, elem_slots: u16 },  // 栈数组保持 slot-based
    HeapArray { elem_bytes: u16 },   // elem_slots → elem_bytes
    Slice { elem_bytes: u16 },        // elem_slots → elem_bytes
    Map { key_slots: u16, val_slots: u16 },
    String,
}
```

#### 4.3 lvalue.rs 改动

所有使用 `elem_slots` 的地方改为 `elem_bytes`（仅 HeapArray 和 Slice）。

### 5. vo-vm 指令改动

#### 5.1 需要改 flags 语义的指令

| 指令 | 旧 flags | 新 flags |
|-----|---------|---------|
| `ArrayNew` | elem_slots | elem_bytes |
| `ArrayGet` | elem_slots | elem_bytes |
| `ArraySet` | elem_slots | elem_bytes |
| `SliceNew` | elem_slots | elem_bytes |
| `SliceGet` | elem_slots | elem_bytes |
| `SliceSet` | elem_slots | elem_bytes |
| `SliceAppend` | elem_slots | elem_bytes |

#### 5.2 不需要改的指令

| 指令 | 原因 |
|-----|------|
| `ChanSend/ChanRecv` | Channel 不优化 |
| `SlotGetN/SlotSetN` | 栈上数组，保持 slot-based |
| `PtrGetN/PtrSetN` | 指针访问堆 struct，slot-based |
| `GlobalGetN/GlobalSetN` | 全局变量，slot-based |

#### 5.3 vm.rs 宏修改（支持 packed）

```rust
/// 获取 array data 区的字节指针
macro_rules! array_data_ptr {
    ($arr:expr) => {
        unsafe { ($arr as *mut u8).add(ARRAY_DATA_OFFSET * 8) }
    };
}

/// 按 elem_bytes 读取单个元素（返回 u64）
macro_rules! array_get_packed {
    ($arr:expr, $idx:expr, $elem_bytes:expr) => {{
        let ptr = array_data_ptr!($arr);
        let byte_off = $idx * $elem_bytes;
        unsafe {
            match $elem_bytes {
                1 => *ptr.add(byte_off) as u64,
                2 => *(ptr.add(byte_off) as *const u16) as u64,
                4 => *(ptr.add(byte_off) as *const u32) as u64,
                _ => *(ptr.add(byte_off) as *const u64),
            }
        }
    }};
}

/// 按 elem_bytes 写入单个元素
macro_rules! array_set_packed {
    ($arr:expr, $idx:expr, $val:expr, $elem_bytes:expr) => {{
        let ptr = array_data_ptr!($arr);
        let byte_off = $idx * $elem_bytes;
        unsafe {
            match $elem_bytes {
                1 => *ptr.add(byte_off) = $val as u8,
                2 => *(ptr.add(byte_off) as *mut u16) = $val as u16,
                4 => *(ptr.add(byte_off) as *mut u32) = $val as u32,
                _ => *(ptr.add(byte_off) as *mut u64) = $val,
            }
        }
    }};
}

/// Slice get：start 是 element index
macro_rules! slice_get_packed {
    ($s:expr, $idx:expr, $elem_bytes:expr) => {{
        let arr = slice_array!($s);
        let start = slice_start!($s);
        array_get_packed!(arr, start + $idx, $elem_bytes)
    }};
}

/// Slice set：start 是 element index
macro_rules! slice_set_packed {
    ($s:expr, $idx:expr, $val:expr, $elem_bytes:expr) => {{
        let arr = slice_array!($s);
        let start = slice_start!($s);
        array_set_packed!(arr, start + $idx, $val, $elem_bytes)
    }};
}
```

#### 5.4 ArrayGet/SliceGet 实现

```rust
Opcode::ArrayGet => {
    let arr = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
    let idx = stack_get!(fiber.stack, bp + inst.c as usize) as usize;
    let elem_bytes = inst.flags as usize;
    let elem_bytes = if elem_bytes == 0 { array::elem_bytes(arr) } else { elem_bytes };
    
    if elem_bytes <= 8 {
        let val = array_get_packed!(arr, idx, elem_bytes);
        stack_set!(fiber.stack, bp + inst.a as usize, val);
    } else {
        // 多 slot: slot-based（elem_bytes 是 8 的倍数）
        let elem_slots = elem_bytes / 8;
        let dst = bp + inst.a as usize;
        for i in 0..elem_slots {
            let byte_off = idx * elem_bytes + i * 8;
            let val = unsafe { *(array_data_ptr!(arr).add(byte_off) as *const u64) };
            stack_set!(fiber.stack, dst + i, val);
        }
    }
    ExecResult::Continue
}
```

### 6. vo-jit 改动

#### 6.1 jit_api.rs 改动

**删除**（已被 translate.rs inline 实现）：
- `vo_array_get`
- `vo_array_set`
- `vo_slice_get`
- `vo_slice_set`

**保留并修改签名**：

```rust
// vo_array_new: elem_slots → elem_bytes
pub extern "C" fn vo_array_new(gc: *mut Gc, elem_meta: u32, elem_bytes: u32, len: u64) -> u64

// vo_slice_new: elem_slots → elem_bytes
pub extern "C" fn vo_slice_new(gc: *mut Gc, elem_meta: u32, elem_bytes: u32, len: u64, cap: u64) -> u64

// vo_slice_append: elem_slots → elem_bytes
pub extern "C" fn vo_slice_append(gc: *mut Gc, elem_meta: u32, elem_bytes: u32, s: u64, val_ptr: *const u64) -> u64
```

#### 6.2 translate.rs inline 实现

**translate_array_get**（包含 flags=0 处理，见问题 9）：

```rust
pub(crate) fn translate_array_get(&mut self, inst: &Instruction) {
    use vo_runtime::objects::array::HEADER_SLOTS;
    
    let arr = self.read_var(inst.b);
    let idx = self.read_var(inst.c);
    let elem_bytes_flag = inst.flags as usize;
    
    // flags=0 表示需要从 ArrayHeader 读取 elem_bytes
    let elem_bytes_val = if elem_bytes_flag == 0 {
        // ArrayHeader: len(8 bytes) + elem_meta(4 bytes) + elem_bytes(4 bytes)
        let eb = self.builder.ins().load(types::I32, MemFlags::trusted(), arr, 12);
        self.builder.ins().uextend(types::I64, eb)
    } else {
        self.builder.ins().iconst(types::I64, elem_bytes_flag as i64)
    };
    
    // byte_offset = HEADER_SLOTS * 8 + idx * elem_bytes
    let header_bytes = self.builder.ins().iconst(types::I64, (HEADER_SLOTS * 8) as i64);
    let idx_bytes = self.builder.ins().imul(idx, elem_bytes_val);
    let byte_offset = self.builder.ins().iadd(header_bytes, idx_bytes);
    let addr = self.builder.ins().iadd(arr, byte_offset);
    
    // 注意：flags=0 时需要动态分支，建议 codegen 尽量避免 flags=0
    let elem_bytes = elem_bytes_flag; // 如果 flags=0，需要动态处理
    match elem_bytes {
        1 => {
            let val = self.builder.ins().load(types::I8, MemFlags::trusted(), addr, 0);
            let val_i64 = self.builder.ins().uextend(types::I64, val);
            self.write_var(inst.a, val_i64);
        }
        2 => {
            let val = self.builder.ins().load(types::I16, MemFlags::trusted(), addr, 0);
            let val_i64 = self.builder.ins().uextend(types::I64, val);
            self.write_var(inst.a, val_i64);
        }
        4 => {
            let val = self.builder.ins().load(types::I32, MemFlags::trusted(), addr, 0);
            let val_i64 = self.builder.ins().uextend(types::I64, val);
            self.write_var(inst.a, val_i64);
        }
        _ => {
            // 8 bytes 或多 slot
            let elem_slots = (elem_bytes + 7) / 8;
            for i in 0..elem_slots {
                let slot_addr = self.builder.ins().iadd_imm(addr, (i * 8) as i64);
                let val = self.builder.ins().load(types::I64, MemFlags::trusted(), slot_addr, 0);
                self.write_var(inst.a + i as u16, val);
            }
        }
    }
}
```

**translate_array_set**：与 translate_array_get 类似，使用 `ireduce` + `store`。

**translate_slice_get/set**：

```rust
pub(crate) fn translate_slice_get(&mut self, inst: &Instruction) {
    use vo_runtime::objects::slice::{FIELD_ARRAY, FIELD_START};
    use vo_runtime::objects::array::HEADER_SLOTS;
    
    let s = self.read_var(inst.b);
    let idx = self.read_var(inst.c);
    let elem_bytes = inst.flags as usize;
    
    // 读取 array 和 start
    let arr = self.builder.ins().load(types::I64, MemFlags::trusted(), s, (FIELD_ARRAY * 8) as i32);
    let start = self.builder.ins().load(types::I64, MemFlags::trusted(), s, (FIELD_START * 8) as i32);
    
    // 计算: byte_offset = HEADER_SLOTS * 8 + (start + idx) * elem_bytes
    let total_idx = self.builder.ins().iadd(start, idx);
    let elem_bytes_val = self.builder.ins().iconst(types::I64, elem_bytes as i64);
    let idx_bytes = self.builder.ins().imul(total_idx, elem_bytes_val);
    let header_bytes = self.builder.ins().iconst(types::I64, (HEADER_SLOTS * 8) as i64);
    let byte_offset = self.builder.ins().iadd(header_bytes, idx_bytes);
    let addr = self.builder.ins().iadd(arr, byte_offset);
    
    // 按 elem_bytes load（同 translate_array_get）
    ...
}
```

### 7. vo_copy extern 函数（新增）

`copy(dst, src)` 内建函数需要新增 `vo_copy` extern 实现：

```rust
#[no_mangle]
pub extern "C" fn vo_copy(
    dst: u64,  // dst slice GcRef
    src: u64,  // src slice GcRef
) -> u64 {
    // 从 slice header 读取 elem_bytes
    let dst_arr = slice::array_ref(dst as GcRef);
    let elem_bytes = array::elem_bytes(dst_arr);
    
    let dst_len = slice::len(dst as GcRef);
    let src_len = slice::len(src as GcRef);
    let copy_len = dst_len.min(src_len);
    
    // 使用 copy_range
    let dst_start = slice::start(dst as GcRef);
    let src_start = slice::start(src as GcRef);
    let src_arr = slice::array_ref(src as GcRef);
    
    array::copy_range(src_arr, src_start, dst_arr, dst_start, copy_len, elem_bytes);
    
    copy_len as u64
}
```

**注意**：codegen 已用 `CallExtern` 调用 `vo_copy`，只需：
1. 在 `jit_api.rs` 实现上述函数
2. 在 `get_runtime_symbols()` 注册 `vo_copy`

**不需要**新增 `SliceCopy` 指令。

---

## 边界情况处理

### flags 字段限制 (u8 = 0-255)

| elem_bytes | 处理 |
|-----------|------|
| 1, 2, 4, 8 | packed primitives，直接用 flags |
| 16 (interface) | slot-based，flags=16 |
| > 255 | 不会发生：struct/array 用 `slots * 8`，但 flags 存不下时从 header 读取 |

**规则**：当 `elem_bytes > 255` 时，codegen 生成 `flags=0`，运行时从 ArrayHeader 读取 elem_bytes。

**注意**：`flags=0` 是特殊值，表示需要从 header 读取。实际 elem_bytes 不可能为 0。

### String 底层

String 底层已是 packed (elem_bytes=1)，本次改动不影响。

### 栈上数组

栈上数组保持 slot-based，`SlotGet/SlotGetN` 不变。

### 多维数组

`[][]int` 外层元素是 GcRef (elem_bytes=8)，保持 slot-based。

---

## 不需要改动的部分

### ptr_clone

`Gc::ptr_clone` 按 `GcHeader.slots` 复制整个对象：
- `slots = (data_bytes + 7) / 8` 包含了所有数据
- 复制时按 slot 复制，对 packed array 仍然正确

---

## 测试覆盖

### 基本功能

1. `[]bool` 基本读写
2. `[]byte` (uint8) 基本读写
3. `[]int8` (有符号) 基本读写
4. `[]int16` 基本读写
5. `[]int32` 基本读写
6. `[]float32` 基本读写
7. `[]int64` (slot-based) 确保不受影响
8. `[]interface{}` (multi-slot) 确保不受影响

### Slice 操作

9. `s[lo:hi]` 切片操作
10. `s[lo:hi:max]` 三参数切片
11. `append(s, v)` 追加（无扩容）
12. `append(s, v)` 追加（有扩容）
13. `copy(dst, src)` 复制
14. slice 切片后 append（验证 `start != 0` 时正确）

### 复杂场景

15. `[][]int` (slice of slice) GcRef 正确处理
16. `[3][4]bool` 多维数组
17. for-range 迭代 `[]bool`
18. for-range 迭代 `[]int32`
19. `append(nil, v)` 空 slice append
20. `[]interface{}` 的 slice 操作（验证 multi-slot）
21. `[][32]int` 大 struct 数组（验证 flags=0 fallback）

### VM/JIT 一致性

22. 每种 packed 类型同时跑 VM 和 JIT，结果必须一致

---

## 实施顺序

### 第一阶段：修复现有 BUG + SliceAppend

1. **统一 `start` 语义**：VM 宏和 JIT 都改为 `(start + idx) * elem_slots`（当前 JIT 是正确的）
2. **修复 `slice.rs` 的 `get/set`**：让它们也乘 `elem_slots`
3. **SliceAppend 携带 elem_meta**：改为连续栈模式 `c=meta_and_elem`（见问题 12）
4. **实现 vo_copy**：在 jit_api.rs 实现并注册

### 第二阶段：Packed Array 实现

1. **vo-runtime/objects/array.rs** - 改函数签名和实现
2. **vo-runtime/objects/slice.rs** - 改函数签名和实现
3. **vo-runtime/gc_types.rs** - 修改 scan_array（见问题 7）
4. **vo-runtime/jit_api.rs** - 修改 `vo_array_new`/`vo_slice_new`/`vo_slice_append` 签名，删除 `vo_array_get/set`/`vo_slice_get/set`
5. **vo-analysis** - 新增 `elem_bytes_for_heap`
6. **vo-codegen** - 新增 `array_elem_bytes`、`slice_elem_bytes`，改 ContainerKind，改 SliceAppend 生成
7. **vo-vm** - 改宏和指令实现
8. **vo-jit** - 改 translate.rs，**处理 `flags=0` fallback**
9. **测试** - 跑全量测试，新增 packed array 测试

---

## 未来扩展：struct 优化

如果要优化 `[]struct{x bool}` 为 1 byte/elem：

1. 修改 `elem_bytes_for_heap()` 添加 struct 的 packed 逻辑
2. ArrayGet/ArraySet 展开为 per-field 读写（编译时展开）
3. 或者运行时存储 struct 字段布局信息

核心挑战：栈是 slot-based，堆元素是 byte-based，需要转换逻辑。
