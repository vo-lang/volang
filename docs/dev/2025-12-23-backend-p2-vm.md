# Backend P2: vo-vm

**Parent**: [2025-12-23-backend-rewrite-plan.md](2025-12-23-backend-rewrite-plan.md)  
**Status**: Not Started  
**Est. Modules**: 36  
**Depends On**: P1 (runtime-core)

## Overview

定义 bytecode 格式和 VM 解释器。这是 Vo 的核心执行引擎。

**核心原则**：
- 8 字节固定指令格式
- 寄存器式 VM（slot = 栈上 8 字节）
- 协作式调度（Fiber）

## 模块清单

### 1. 指令格式 (instruction.rs)

```rust
/// 8 字节固定格式
#[repr(C)]
pub struct Instruction {
    pub op: u8,      // Opcode
    pub flags: u8,   // 标志/变体
    pub a: u16,      // 目标寄存器 或 操作数
    pub b: u16,      // 源操作数 0
    pub c: u16,      // 源操作数 1
}

impl Instruction {
    pub const fn new(op: Opcode, a: u16, b: u16, c: u16) -> Self;
    pub const fn with_flags(op: Opcode, flags: u8, a: u16, b: u16, c: u16) -> Self;
    
    /// 从 b, c 组合 32 位立即数
    pub fn imm32(&self) -> i32 {
        ((self.b as u32) | ((self.c as u32) << 16)) as i32
    }
}
```

### 2. Opcode 枚举 (instruction.rs)

```rust
#[repr(u8)]
pub enum Opcode {
    // === LOAD: 加载立即数/常量 ===
    Nop = 0,
    LoadNil,      // slots[a] = nil
    LoadTrue,     // slots[a] = true
    LoadFalse,    // slots[a] = false
    LoadInt,      // slots[a] = sign_extend(b | (c << 16))
    LoadConst,    // slots[a] = constants[b]
    
    // === COPY: 栈 slot 复制 ===
    Copy,         // slots[a] = slots[b]
    CopyN,        // slots[a..a+c] = slots[b..b+c]
    
    // === SLOT: 栈动态索引 (栈上数组用) ===
    SlotGet,      // slots[a] = slots[b + slots[c]]
    SlotSet,      // slots[a + slots[b]] = slots[c]
    SlotGetN,     // slots[a..a+flags] = slots[b + slots[c]*flags..]
    SlotSetN,     // slots[a + slots[b]*flags..] = slots[c..c+flags]
    
    // === GLOBAL: 全局变量 ===
    GlobalGet,    // slots[a] = globals[b]
    GlobalSet,    // globals[a] = slots[b]
    
    // === PTR: 堆指针操作 ===
    PtrNew,       // slots[a] = alloc(value_kind=flags, type_id=b, size=c)
    PtrClone,     // slots[a] = clone(slots[b])
    PtrGet,       // slots[a] = heap[slots[b]].offset[c]
    PtrSet,       // heap[slots[a]].offset[b] = slots[c]
    PtrGetN,      // slots[a..a+flags] = heap[slots[b]].offset[c..]
    PtrSetN,      // heap[slots[a]].offset[b..] = slots[c..c+flags]
    
    // === ARITH: 整数算术 ===
    AddI, SubI, MulI, DivI, ModI, NegI,
    
    // === ARITH: 浮点算术 ===
    AddF, SubF, MulF, DivF, NegF,
    
    // === CMP: 整数比较 ===
    EqI, NeI, LtI, LeI, GtI, GeI,
    
    // === CMP: 浮点比较 ===
    EqF, NeF, LtF, LeF, GtF, GeF,
    
    // === CMP: 引用比较 ===
    EqRef, NeRef, IsNil,
    
    // === BIT: 位运算 ===
    And, Or, Xor, Not, Shl, ShrS, ShrU,
    
    // === LOGIC: 逻辑运算 ===
    BoolNot,
    
    // === JUMP: 控制流 ===
    Jump,         // pc += sign_extend(b | (c << 16))
    JumpIf,       // if slots[a]: pc += sign_extend(b | (c << 16))
    JumpIfNot,    // if !slots[a]: pc += sign_extend(b | (c << 16))
    
    // === CALL: 函数调用 ===
    Call,         // call functions[a], args at b, arg_slots=c, ret_slots=flags
    CallExtern,   // call extern[a], args at b, arg_slots=c, ret_slots=flags
    CallClosure,  // call slots[a], args at b, arg_slots=c, ret_slots=flags
    CallIface,    // call iface at a, args at b, c=(arg_slots<<8|ret_slots), flags=method_idx
    Return,       // return values at a, ret_slots=b
    
    // === STR: 字符串操作 ===
    StrLen,       // slots[a] = len(slots[b])
    StrIndex,     // slots[a] = slots[b][slots[c]]
    StrConcat,    // slots[a] = slots[b] + slots[c]
    StrSlice,     // slots[a] = slots[b][slots[c]:slots[flags]]
    StrEq, StrNe, StrLt, StrLe, StrGt, StrGe,
    // 注: StrNew 用 CallExtern
    
    // === ARRAY: 堆数组操作 ===
    ArrayGet,     // slots[a] = slots[b][slots[c]]
    ArraySet,     // slots[a][slots[b]] = slots[c]
    ArrayLen,     // slots[a] = len(slots[b])
    // 注: ArrayNew 用 CallExtern
    
    // === SLICE: 切片操作 ===
    SliceGet,     // slots[a] = slots[b][slots[c]]
    SliceSet,     // slots[a][slots[b]] = slots[c]
    SliceLen,     // slots[a] = len(slots[b])
    SliceCap,     // slots[a] = cap(slots[b])
    SliceSlice,   // slots[a] = slots[b][slots[c]:slots[flags]]
    SliceAppend,  // slots[a] = append(slots[b], slots[c])
    // 注: SliceNew 用 CallExtern
    
    // === MAP: Map 操作 ===
    MapGet,       // slots[a] = slots[b][slots[c]], flags=1 时 ok 写到 slots[a+1]
    MapSet,       // slots[a][slots[b]] = slots[c]
    MapDelete,    // delete(slots[a], slots[b])
    MapLen,       // slots[a] = len(slots[b])
    // 注: MapNew 用 CallExtern
    
    // === CHAN: Channel 操作 ===
    ChanSend,     // slots[a] <- slots[b]
    ChanRecv,     // slots[a] = <-slots[b], flags=1 时 ok 写到 slots[c]
    ChanClose,    // close(slots[a])
    // 注: ChanNew 用 CallExtern
    
    // === SELECT: Select 语句 ===
    SelectBegin,  // begin select, case_count=a, has_default=b
    SelectSend,   // add send case: chan=a, val=b
    SelectRecv,   // add recv case: dst=a, chan=b, ok=c
    SelectEnd,    // execute select, chosen index → slots[a]
    
    // === ITER: 迭代器 (for-range) ===
    IterBegin,    // 开始迭代 slots[a], type=b (Slice/Map/String/IntRange/Channel)
    IterNext,     // slots[a], slots[b] = next; 失败跳 pc+=c
                  // Channel: slots[a]=value, slots[b]=unused, ok=false 时跳转
    IterEnd,      // 结束迭代，弹出 iter_stack
    
    // === CLOSURE: 闭包操作 ===
    ClosureGet,   // slots[a] = slots[0].captures[b]  (closure 隐式在 r0)
    ClosureSet,   // slots[0].captures[a] = slots[b]  (closure 隐式在 r0)
    // 注: ClosureNew 用 CallExtern，无 Upval 指令
    
    // === GO: Goroutine ===
    GoCall,       // go slots[a]()  (0 参数 closure，与 defer 一致)
    Yield,
    
    // === DEFER: Defer 和错误处理 ===
    DeferPush,    // push defer: closure=slots[a]
    ErrDeferPush, // push errdefer: closure=slots[a]
    Panic,        // panic(slots[a])
    Recover,      // slots[a] = recover()
    // 注: defer 统一用 0 参数 closure，Return 时自动执行
    
    // === IFACE: Interface 操作 (interface 占 slots[a] 和 slots[a+1]) ===
    IfaceInit,    // slots[a..a+2] = nil interface, iface_meta_id=b | (c << 16)
    IfaceAssign,  // 统一赋值: dst=slots[a..a+2], src_slots=b, src_vk=c, src_meta_id=flags<<16|...
                  // 处理所有情况: 值→iface, iface→iface, Struct/Array 深拷贝
    IfaceAssert,  // slots[a..], ok = slots[b..b+2].(type c), ok_reg=flags
    
    // === CONV: 类型转换 ===
    ConvI2F,      // slots[a] = float64(slots[b])
    ConvF2I,      // slots[a] = int64(slots[b])
    ConvI32I64,   // slots[a] = int64(int32(slots[b]))
    ConvI64I32,   // slots[a] = int32(slots[b])
    
    // === DEBUG: 调试操作 ===
    // 注: Print 用 CallExtern
    // 注: assert 用 JumpIf + CallExtern(print) + Panic 组合实现
}
```

**⚠️ 注意**：
- Opcode 值经过精心安排，同类指令连续
- flags 字段用途因指令而异（返回值数量、slot 数量等）
- imm32 用于跳转偏移，由 b 和 c 组合

### 2.1 内置 CallExtern 函数

以下操作通过 `CallExtern` 调用 runtime-core 函数实现：

| 类别 | 函数 | 签名 |
|------|------|------|
| **String** | `vo_string_create` | `(bytes_ptr, len) -> GcRef` |
| **Array** | `vo_array_create` | `(elem_kind, elem_meta_id, len) -> GcRef` |
| **Slice** | `vo_slice_create` | `(elem_kind, elem_meta_id, len, cap) -> GcRef` |
| **Map** | `vo_map_create` | `(key_kind, key_meta_id, val_kind, val_meta_id) -> GcRef` |
| **Channel** | `vo_channel_create` | `(elem_kind, elem_meta_id, cap) -> GcRef` |
| **Closure** | `vo_closure_create` | `(func_id, capture_count) -> GcRef` |
| **Debug** | `vo_print` | `(value, type_kind) -> ()` |

**注**：`elem_slots` / `key_slots` / `val_slots` 通过 `meta_id` 从 `struct_metas` 表查询，不在参数中传递。

**参数说明**：
- `elem_kind` / `key_kind` / `val_kind` - ValueKind 枚举值
- `elem_meta_id` / `key_meta_id` / `val_meta_id` - 类型 ID（Struct/Interface 时使用）
- `elem_slots` - 元素占用的 slot 数（用于索引计算）

**设计原则**：
- **分配操作** → CallExtern（内存分配开销大，函数调用开销可忽略）
- **读取操作** → 保留指令（高频、轻量）
- **VM 状态操作** → 保留指令（需要内部状态）

### 3. Bytecode 模块 (bytecode.rs)

```rust
/// 常量
pub enum Constant {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

/// 函数定义
pub struct FunctionDef {
    pub name: String,
    pub param_count: u16,    // 参数个数
    pub param_slots: u16,    // 参数占用的 slot 数
    pub local_slots: u16,    // 局部变量 slot 数
    pub ret_slots: u16,      // 返回值 slot 数
    pub code: Vec<Instruction>,
    pub slot_types: Vec<SlotType>,  // GC 扫描用
}

/// 外部函数
pub struct ExternDef {
    pub name: String,
    pub param_slots: u16,
    pub ret_slots: u16,
}

/// 全局变量
pub struct GlobalDef {
    pub name: String,
    pub slots: u16,
    pub value_kind: u8,
    pub type_id: u16,
}

/// Interface 方法分派
pub struct IfaceDispatchEntry {
    pub concrete_type_id: u16,
    pub iface_type_id: u16,
    pub method_funcs: Vec<u32>,  // 每个方法的 func_id
}

/// Bytecode 模块
pub struct Module {
    pub name: String,
    pub struct_metas: Vec<TypeMeta>,
    pub interface_metas: Vec<TypeMeta>,
    pub constants: Vec<Constant>,
    pub globals: Vec<GlobalDef>,
    pub functions: Vec<FunctionDef>,
    pub externs: Vec<ExternDef>,
    pub iface_dispatch: Vec<IfaceDispatchEntry>,
    pub entry_func: u32,
}
```

**⚠️ 关键**：
- `slot_types` 必须与 `local_slots` 长度一致
- `iface_dispatch` 在 codegen 时填充，VM 执行时查询

### 4. 序列化 (bytecode.rs 续)

```rust
impl Module {
    /// 序列化为字节
    pub fn serialize(&self) -> Vec<u8>;
    
    /// 从字节反序列化
    pub fn deserialize(bytes: &[u8]) -> Result<Self, BytecodeError>;
}

/// 文件格式
/// Magic: "VOB" (4 bytes)
/// Version: u32
/// struct_types: [TypeMeta]
/// interface_types: [TypeMeta]
/// constants: [Constant]
/// globals: [GlobalDef]
/// functions: [FunctionDef]
/// externs: [ExternDef]
/// iface_dispatch: [IfaceDispatchEntry]
/// entry_func: u32
```

### 5. VM 结构 (vm.rs)

```rust
/// 调用帧
pub struct CallFrame {
    pub func_id: u32,
    pub pc: usize,
    pub bp: usize,       // base pointer (栈底)
    pub ret_reg: u16,    // 返回值存放位置
    pub ret_count: u16,
}

/// Defer 条目 - 统一用 closure
pub struct DeferEntry {
    pub frame_depth: usize,
    pub closure: GcRef,      // 0 参数 closure
    pub is_errdefer: bool,
}

/// 迭代器
pub enum Iterator {
    Slice { arr: GcRef, pos: usize, len: usize },
    Map { map: GcRef, pos: usize },
    String { s: GcRef, byte_pos: usize },
    IntRange { cur: i64, end: i64, step: i64 },
    Channel { ch: GcRef },  // for v := range ch
}

/// Fiber (协程)
pub struct Fiber {
    pub id: u32,
    pub status: FiberStatus,
    pub stack: Vec<u64>,
    pub frames: Vec<CallFrame>,
    pub defer_stack: Vec<DeferEntry>,
    pub defer_state: Option<DeferState>,  // Return 时暂存
    pub iter_stack: Vec<Iterator>,
    pub panic_value: Option<GcRef>,
}

pub enum FiberStatus {
    Running,
    Suspended,
    Dead,
}

/// 调度器
pub struct Scheduler {
    pub fibers: Vec<Fiber>,
    pub ready_queue: VecDeque<u32>,
    pub current: Option<u32>,
}

/// VM 主结构
pub struct Vm {
    pub module: Option<Module>,
    pub gc: Gc,
    pub scheduler: Scheduler,
    pub globals: Vec<u64>,
}
```

### 6. VM 执行 (vm.rs 续)

```rust
impl Vm {
    pub fn new() -> Self;
    
    /// 加载模块
    pub fn load(&mut self, module: Module);
    
    /// 运行入口函数
    pub fn run(&mut self) -> Result<(), VmError>;
    
    /// 执行单条指令
    fn exec_instruction(&mut self, fiber_id: u32) -> ExecResult;
    
    /// 读写寄存器
    fn read_reg(&self, fiber_id: u32, reg: u16) -> u64;
    fn write_reg(&mut self, fiber_id: u32, reg: u16, val: u64);
}

enum ExecResult {
    Continue,
    Return,
    Yield,
    Panic(GcRef),
}
```

### 7. 指令执行详解

#### 7.1 函数调用 (Call)

```rust
Opcode::Call => {
    // a=func_id, b=arg_start, c=arg_slots, flags=ret_slots
    let func = &module.functions[a as usize];
    
    // 复制参数（避免帧切换后覆盖）
    let args: Vec<u64> = (0..c).map(|i| self.read_reg(fiber_id, b + i)).collect();
    
    // 推入新帧
    fiber.push_frame(a, func.local_slots, b, flags);
    
    // 写入参数到新帧
    for (i, arg) in args.into_iter().enumerate() {
        fiber.write_reg(i as u16, arg);
    }
}
```

#### 7.2 Closure 调用 (CallClosure)

```rust
Opcode::CallClosure => {
    // a=closure_reg, b=arg_start, c=arg_slots, flags=ret_slots
    let closure = self.read_reg(fiber_id, a) as GcRef;
    let func_id = closure::func_id(closure);
    let func = &module.functions[func_id as usize];
    
    // 复制参数
    let args: Vec<u64> = (0..c).map(|i| self.read_reg(fiber_id, b + i)).collect();
    
    // 推入新帧
    fiber.push_frame(func_id, func.local_slots, b, flags);
    
    // ⚠️ 关键：closure 作为 r0
    fiber.write_reg(0, closure as u64);
    
    // 参数从 r1 开始
    for (i, arg) in args.into_iter().enumerate() {
        fiber.write_reg((i + 1) as u16, arg);
    }
}
```

#### 7.3 Interface 调用 (CallIface)

```rust
Opcode::CallIface => {
    // a=iface_reg, b=arg_start, c=(arg_slots<<8|ret_slots), flags=method_idx
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    let method_idx = inst.flags as usize;
    
    let (slot0, slot1) = self.read_interface(fiber_id, inst.a);
    let concrete_type_id = extract_concrete_type(slot0);
    let iface_type_id = extract_iface_type(slot0);
    
    // 查 dispatch table
    let entry = module.find_dispatch(concrete_type_id, iface_type_id);
    let func_id = entry.method_funcs[method_idx];
    
    // 像普通函数一样调用，但 receiver 是 slot1
    // ...
```

#### 7.4 Return 实现

```rust
Opcode::Return => {
    // a=ret_start, b=ret_count
    let ret_start = inst.a as usize;
    let ret_count = inst.b as usize;
    
    // 1. 暂存返回值
    let ret_vals: Vec<u64> = (0..ret_count)
        .map(|i| self.read_reg(fiber_id, (ret_start + i) as u16))
        .collect();
    
    // 2. 收集当前帧的 defer（LIFO 顺序）
    let frame_depth = fiber.frames.len();
    let mut pending: Vec<DeferEntry> = fiber.defer_stack
        .drain_filter(|d| d.frame_depth == frame_depth)
        .collect();
    pending.reverse();  // LIFO
    
    // 3. 检查是否有 errdefer，若有则运行时判断 error return
    // 注: 编译器保证有 errdefer 的函数必有 error 返回值
    let has_errdefer = pending.iter().any(|d| d.is_errdefer);
    let is_error = if has_errdefer && ret_count > 0 {
        ret_vals[ret_count - 1] != 0
    } else {
        false
    };
    
    // 4. 过滤 errdefer
    if !is_error {
        pending.retain(|d| !d.is_errdefer);
    }
    
    // 5. 如果有 defer，进入 defer 执行模式
    if !pending.is_empty() {
        let caller_frame = &fiber.frames[fiber.frames.len() - 2];
        fiber.defer_state = Some(DeferState {
            pending,
            ret_vals,
            caller_ret_reg: caller_frame.ret_reg,
            caller_ret_count: ret_count,
            is_error_return: is_error,
        });
        
        // 执行第一个 defer closure
        let first = fiber.defer_state.as_mut().unwrap().pending.pop().unwrap();
        // CallClosure(first.closure, no args)
        // ...
    } else {
        // 6. 无 defer，直接返回
        let caller_frame = fiber.frames.pop().unwrap();
        for (i, val) in ret_vals.into_iter().enumerate() {
            self.write_reg(fiber_id, caller_frame.ret_reg + i as u16, val);
        }
    }
}
```

#### 7.5 Defer 数据结构

```rust
/// Defer 条目 - 统一用 closure
pub struct DeferEntry {
    pub frame_depth: usize,    // 关联的调用帧深度
    pub closure: GcRef,        // 0 参数 closure
    pub is_errdefer: bool,     // 是否是 errdefer
}

/// Defer 执行状态 (return 时暂存)
pub struct DeferState {
    pub pending: Vec<DeferEntry>,  // 待执行的 defer (LIFO)
    pub ret_vals: Vec<u64>,        // 暂存的返回值
    pub caller_ret_reg: u16,
    pub caller_ret_count: usize,
    pub is_error_return: bool,     // errdefer 判定用
}
```

**执行流程**：
1. `DeferPush` / `ErrDeferPush`: 保存 closure 到 `defer_stack`
2. `Return` 时:
   - 收集当前帧的 defers（LIFO）
   - 若存在 errdefer，检查最后一个返回值是否非 nil → is_error
   - errdefer 只在 is_error=true 时执行
   - 逐个 CallClosure 执行
3. defer closure 返回后继续执行下一个
4. 全部执行完，写入暂存的返回值给 caller

**Codegen 约束**：`errdefer` 只能在有 error 返回值的函数中使用，否则编译错误。

**Codegen 注意**：`defer foo(x, y)` 需包装为：
```vo
// 生成一个 0 参数 closure，捕获 x, y
closure := func() { foo(x, y) }
DeferPush closure
```

#### 7.6 GC 根扫描

```rust
impl Vm {
    /// 扫描所有 GC 根
    pub fn scan_roots(&self, gc: &mut Gc) {
        // 1. 扫描全局变量
        for (i, &val) in self.globals.iter().enumerate() {
            let def = &self.module.as_ref().unwrap().globals[i];
            if needs_gc(def.value_kind) && val != 0 {
                gc.mark_gray(val as GcRef);
            }
        }
        
        // 2. 扫描所有 Fiber 的栈
        for fiber in &self.scheduler.fibers {
            for frame in &fiber.frames {
                let func = &module.functions[frame.func_id as usize];
                for (i, &st) in func.slot_types.iter().enumerate() {
                    let slot_idx = frame.bp + i;
                    match st {
                        SlotType::GcRef => {
                            let val = fiber.stack[slot_idx];
                            if val != 0 { gc.mark_gray(val as GcRef); }
                        }
                        SlotType::Interface1 => {
                            // 动态检查前一个 slot
                            let header = fiber.stack[slot_idx - 1];
                            if needs_gc(extract_value_kind(header)) {
                                let val = fiber.stack[slot_idx];
                                if val != 0 { gc.mark_gray(val as GcRef); }
                            }
                        }
                        _ => {}
                    }
                }
            }
            
            // 3. 扫描 defer_stack
            for entry in &fiber.defer_stack {
                gc.mark_gray(entry.closure);
            }
            
            // 4. 扫描 defer_state
            if let Some(state) = &fiber.defer_state {
                for entry in &state.pending {
                    gc.mark_gray(entry.closure);
                }
            }
            
            // 5. 扫描 iter_stack
            for iter in &fiber.iter_stack {
                match iter {
                    Iterator::Slice { arr, .. } => gc.mark_gray(*arr),
                    Iterator::Map { map, .. } => gc.mark_gray(*map),
                    Iterator::String { s, .. } => gc.mark_gray(*s),
                    Iterator::Channel { ch, .. } => gc.mark_gray(*ch),
                    Iterator::IntRange { .. } => {}
                }
            }
        }
    }
}
```

## Tasks Checklist

### instruction.rs
- [ ] Instruction 结构
- [ ] Opcode 枚举（完整 ~100 个）
- [ ] 辅助方法 (imm32, opcode)

### bytecode.rs
- [ ] Constant 枚举
- [ ] FunctionDef
- [ ] ExternDef
- [ ] GlobalDef
- [ ] IfaceDispatchEntry
- [ ] TypeMeta (从 runtime-core 引入或重定义)
- [ ] Module
- [ ] Module::serialize
- [ ] Module::deserialize

### vm.rs - 结构
- [ ] CallFrame
- [ ] DeferEntry
- [ ] Iterator 枚举
- [ ] Fiber
- [ ] FiberStatus
- [ ] Scheduler
- [ ] Vm

### vm.rs - 执行
- [ ] exec 加载/存储 (Nop..CopyN)
- [ ] exec 全局变量 (GlobalGet, GlobalSet)
- [ ] exec 算术 i64
- [ ] exec 算术 f64
- [ ] exec 比较
- [ ] exec 位运算
- [ ] exec 控制流 (Jump, JumpIf, JumpIfNot)
- [ ] exec 函数调用 (Call, CallExtern, Return)
- [ ] exec CallClosure
- [ ] exec CallIface
- [ ] exec 堆内存 (PtrNew, PtrGet, PtrSet, ...)
- [ ] exec 栈动态索引 (SlotGet, SlotSet, SlotGetN, SlotSetN)
- [ ] exec Array/Slice
- [ ] exec String
- [ ] exec Map
- [ ] exec Interface (IfaceInit, IfaceAssign, IfaceAssert)
- [ ] exec Closure (ClosureGet, ClosureSet) — ClosureNew 用 CallExtern
- [ ] exec Goroutine (GoCall, Yield)
- [ ] exec Channel
- [ ] exec Select
- [ ] exec Defer/Panic/Recover (含 ErrDeferPush)
- [ ] exec Iterator
- [ ] exec 类型转换
- [ ] exec Debug (Print, AssertBegin, AssertArg, AssertEnd)

### vm.rs - 调度
- [ ] Scheduler 实现
- [ ] Fiber 切换
- [ ] Channel 阻塞/唤醒

### vm.rs - GC 集成
- [ ] scan_roots 实现
- [ ] 调用 gc.collect 的时机

## 单元测试

```rust
#[test]
fn test_arithmetic() {
    let mut vm = Vm::new();
    // 构造简单的加法 bytecode
    // 执行并验证结果
}

#[test]
fn test_function_call() {
    // 测试函数调用和返回
}

#[test]
fn test_closure_call() {
    // 测试闭包调用，验证 closure 作为 r0
}

#[test]
fn test_gc_roots() {
    // 测试 GC 根扫描正确性
}
```
