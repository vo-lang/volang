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
    Call,         // call functions[a], args at b, argc=c, retc=flags
    CallExtern,   // call extern function
    CallClosure,  // call closure at slots[a]
    CallIface,    // call interface method: iface at a, method=b, args at c
    Return,       // return values at a, count=b
    
    // === STR: 字符串操作 ===
    StrNew, StrConcat, StrLen, StrIndex, StrSlice,
    StrEq, StrNe, StrLt, StrLe, StrGt, StrGe,
    
    // === ARRAY: 堆数组操作 ===
    ArrayNew, ArrayGet, ArraySet, ArrayLen,
    
    // === SLICE: 切片操作 ===
    SliceNew, SliceGet, SliceSet, SliceLen, SliceCap, SliceSlice, SliceAppend,
    
    // === MAP: Map 操作 ===
    MapNew, MapGet, MapSet, MapDelete, MapLen,
    
    // === CHAN: Channel 操作 ===
    ChanNew, ChanSend, ChanRecv, ChanClose,
    
    // === SELECT: Select 语句 ===
    SelectBegin,  // begin select, case_count=a, has_default=b
    SelectSend,   // add send case: chan=a, val=b
    SelectRecv,   // add recv case: dst=a, chan=b, ok=c
    SelectEnd,    // execute select, chosen index → slots[a]
    
    // === ITER: 迭代器 (for-range) ===
    IterBegin, IterNext, IterEnd,
    
    // === CLOSURE: 闭包操作 ===
    ClosureNew,   // slots[a] = closure(func=b, cap_count=c)
    ClosureGet,   // slots[a] = closure.captures[b]
    ClosureSet,   // closure.captures[a] = slots[b]
    // 注: 无 Upval 指令，逃逸变量直接堆分配，closure 捕获 GcRef
    
    // === GO: Goroutine ===
    GoCall,       // go functions[a](args at b, argc=c)
    Yield,
    
    // === DEFER: Defer 和错误处理 ===
    DeferPush,    // push defer: func=a, args at b, argc=c
    DeferPop,     // pop and execute defers
    ErrDeferPush, // push errdefer (executes only on error)
    Panic,        // panic(slots[a])
    Recover,      // slots[a] = recover()
    
    // === IFACE: Interface 操作 ===
    IfaceInit,    // init interface at a, type_id=b | (c << 16)
    IfaceBox,     // box value: iface[a].data = slots[b], vk=flags, type_id=c
    IfaceUnbox,   // unbox: slots[a] = iface[b].data, expected_type=c
    IfaceAssert,  // type assert: slots[a] = slots[b].(type c), ok at flags
    
    // === CONV: 类型转换 ===
    ConvI2F,      // slots[a] = float64(slots[b])
    ConvF2I,      // slots[a] = int64(slots[b])
    ConvI32I64,   // slots[a] = int64(int32(slots[b]))
    ConvI64I32,   // slots[a] = int32(slots[b])
    
    // === DEBUG: 调试操作 ===
    Print,        // print slots[a], value_kind=b
    AssertBegin,  // if !slots[a]: begin assert, argc=b, line=c
    AssertArg,    // print assert arg slots[a], vk=b
    AssertEnd,    // end assert, terminate if failed
}
```

**⚠️ 注意**：
- Opcode 值经过精心安排，同类指令连续
- flags 字段用途因指令而异（返回值数量、slot 数量等）
- imm32 用于跳转偏移，由 b 和 c 组合

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
    pub struct_types: Vec<TypeMeta>,
    pub interface_types: Vec<TypeMeta>,
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

/// Defer 条目
pub struct DeferEntry {
    pub frame_depth: usize,
    pub func_id: u32,
    pub arg_count: u8,
    pub args: [u64; 8],  // 最多 8 个参数
}

/// 迭代器
pub enum Iterator {
    Slice { arr: GcRef, pos: usize, len: usize },
    Map { map: GcRef, pos: usize },
    String { s: GcRef, byte_pos: usize },
    IntRange { cur: i64, end: i64, step: i64 },
}

/// Fiber (协程)
pub struct Fiber {
    pub id: u32,
    pub status: FiberStatus,
    pub stack: Vec<u64>,
    pub frames: Vec<CallFrame>,
    pub defer_stack: Vec<DeferEntry>,
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
    // a=func_id, b=arg_start, c=arg_count, flags=ret_count
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
    // a=closure_reg, b=arg_start, c=arg_count, flags=ret_count
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
    // a=iface_reg, b=method_idx, c=arg_start
    // flags 低 4 位 = arg_count, 高 4 位 = ret_count
    let (slot0, slot1) = self.read_interface(fiber_id, a);
    let concrete_type_id = extract_concrete_type(slot0);
    let iface_type_id = extract_iface_type(slot0);
    
    // 查 dispatch table
    let entry = module.find_dispatch(concrete_type_id, iface_type_id);
    let func_id = entry.method_funcs[b as usize];
    
    // 像普通函数一样调用，但 receiver 是 slot1
    // ...
}
```

#### 7.4 Defer 实现

```rust
/// Defer 条目 - 固定 8 个参数槽
pub struct DeferEntry {
    pub frame_depth: usize,    // 关联的调用帧深度
    pub func_id: u32,          // defer 的函数
    pub arg_count: u8,         // 参数数量 (≤8)
    pub args: [u64; 8],        // 固定 8 个参数槽
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
1. `DeferPush`: 保存 func_id + 参数到 `defer_stack`
2. `Return` 时:
   - 判定是否 error return（最后返回值非 nil）
   - 收集当前帧的 defers（LIFO）
   - errdefer 只在 error return 时执行
   - 逐个推入调用帧执行
3. defer 函数返回后继续执行下一个
4. 全部执行完，写入暂存的返回值给 caller

#### 7.5 GC 根扫描

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
            // 4. 扫描 iter_stack
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
- [ ] exec Interface (IfaceInit, IfaceBox, IfaceUnbox, IfaceAssert)
- [ ] exec Closure (ClosureNew, ClosureGet, ClosureSet)
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
