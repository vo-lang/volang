# Backend 关键数据结构与接口

**Parent**: [2025-12-23-backend-rewrite-plan.md](2025-12-23-backend-rewrite-plan.md)

本文档汇总各 crate 的关键数据结构与接口，便于快速查阅。

---

## P1: vo-runtime-core

VM 和 JIT 共享的核心运行时。

### 类型系统 (types.rs)

```rust
/// 值类型标识 - 区分所有运行时类型
/// 参考: docs/spec/memory-model-and-instructions.md
#[repr(u8)]
pub enum ValueKind {
    // === 基础类型 (1 slot, 无 GC) ===
    Nil = 0, Bool = 1,
    Int = 2, Int8 = 3, Int16 = 4, Int32 = 5, Int64 = 6,
    Uint = 7, Uint8 = 8, Uint16 = 9, Uint32 = 10, Uint64 = 11,
    Float32 = 12, Float64 = 13, FuncPtr = 14,
    
    // === 复合值类型 (多 slot) ===
    Array = 16,         // [N]T - 元素内联
    Struct = 21,        // 字段内联
    Interface = 23,     // 2 slots: header + data
    
    // === 引用类型 (1 slot GcRef) ===
    String = 15, Slice = 17, Map = 18, Channel = 19,
    Closure = 20, Pointer = 22,  // Pointer 统一表示所有逃逸值
}

/// 栈 slot 类型 - GC 扫描用
#[repr(u8)]
pub enum SlotType {
    Value = 0,       // 非 GC 值
    GcRef = 1,       // GC 引用
    Interface0 = 2,  // interface header
    Interface1 = 3,  // interface data (动态判断)
}

/// 运行时类型元数据
pub struct TypeMeta {
    pub name: String,
    pub size_slots: u16,
    pub slot_types: Vec<SlotType>,
}
```

### GC (gc.rs)

```rust
/// GC 对象头 - 8 字节，紧跟数据
/// 布局: [mark:8 | gen:8 | slots:16 | meta_id:24 | value_kind:8]
#[repr(C)]
pub struct GcHeader {
    pub mark: u8,        // GcColor
    pub gen: u8,         // GcGen
    pub slots: u16,      // 数据 slot 数
    pub meta_id: [u8; 3], // 24-bit 元数据索引
    pub value_kind: u8,  // [is_array:1 | kind:7]
}

/// value_kind 字段说明:
/// - bit 7 (is_array): 1 = Array 对象, 0 = 其他
/// - bit 0-6 (kind): 元素/值的 ValueKind
///
/// meta_id 含义随 kind 变化:
/// - Struct: struct_metas 索引
/// - Interface: interface_metas 索引
/// - Array of Struct/Interface: 元素的 meta_id
/// - 其他: 0

/// GC 引用 - 指向堆对象数据区（GcHeader 之后）的指针
pub type GcRef = *mut u64;

pub struct Gc {
    all_objects: Vec<GcObjectEntry>,
    gray_queue: Vec<GcRef>,
    total_bytes: usize,
    threshold: usize,
}

impl Gc {
    pub fn alloc(&mut self, value_kind: u8, meta_id: u32, slots: u16) -> GcRef;
    pub fn read_slot(obj: GcRef, idx: usize) -> u64;   // 静态方法
    pub fn write_slot(obj: GcRef, idx: usize, val: u64);
    pub fn mark_gray(&mut self, obj: GcRef);
    pub fn collect(&mut self);
}
```

### 堆对象布局 (objects.rs)

| 类型 | 布局 | slots |
|------|------|-------|
| **String** | `{ array: GcRef, start, len }` | 3 |
| **Array** | `{ header, data... }` header=`[len:48 \| elem_slots:16]` 元素类型在 GcHeader | 1+len×elem_slots |
| **Slice** | `{ array: GcRef, start, len, cap }` | 4 |
| **Map** | `{ inner_ptr, key_kind, val_kind }` | 3 |
| **Closure** | `{ func_id, cap_count, captures... }` | 2+N |
| **Channel** | `{ state_ptr }` | 1 |
| **Escaped Primitive** | `{ value }` (GcHeader.value_kind=Int/Float/Bool) | 1 |
| **Struct** | `{ field0, field1, ... }` | N |

**Interface** (栈上 2 slots):
```
slot[0]: header = pack(iface_meta_id:24, reserved:8, value_meta_id:24, value_kind:8)
slot[1]: data   = 值或 GcRef
```

### 对象操作接口

**elem_slots 推断**：通过 `(vk, meta_id)` 从 `Module.struct_metas` 查询：
- 简单类型 → 1 slot
- Struct → `struct_metas[meta_id].size_slots`
- Interface → 2 slots

```rust
// String
string::create(gc, bytes) -> GcRef
string::len(s) -> usize
string::concat(gc, a, b) -> GcRef
string::slice_of(gc, s, start, end) -> GcRef

// Array - elem_slots 通过 (elem_kind, elem_meta_id) 推断
array::create(gc, elem_kind, elem_meta_id, len) -> GcRef
array::get(arr, idx) -> u64
array::set(arr, idx, val)
array::len(arr) -> usize

// Slice
slice::create(gc, array, start, len, cap) -> GcRef
slice::get(s, idx) -> u64
slice::set(s, idx, val)
slice::append(gc, s, val) -> GcRef  // elem 信息从 slice.array 的 GcHeader 读取

// Closure
closure::create(gc, func_id, cap_count) -> GcRef
closure::get_upvalue(c, idx) -> u64
closure::set_upvalue(c, idx, val)
```

---

## P2: vo-vm

Bytecode 定义和 VM 解释器。

### 指令 (instruction.rs)

```rust
/// 8 字节固定格式
#[repr(C)]
pub struct Instruction {
    pub op: u8,
    pub flags: u8,
    pub a: u16,
    pub b: u16,
    pub c: u16,
}

pub enum Opcode {
    // 见 spec 完整列表
    Nop, LoadNil, LoadInt, Copy, CopyN,
    GlobalGet, GlobalSet,
    // PtrNew: a=dst, b=meta_id_low16, c=meta_id_high8, flags=vk
    // meta_id = b | (c << 16) → 24 bits, size 从 struct_metas 查询
    PtrNew, PtrGet, PtrSet, PtrClone,
    AddI, SubI, MulI, DivI, ...
    Call, CallClosure, CallIface, Return,
    // ...
}
```

### Bytecode 模块 (bytecode.rs)

```rust
pub enum Constant {
    Nil, Bool(bool), Int(i64), Float(f64), String(String),
}

pub struct FunctionDef {
    pub name: String,
    pub param_count: u16,
    pub param_slots: u16,
    pub local_slots: u16,
    pub ret_slots: u16,
    pub slot_types: Vec<SlotType>,  // GC 扫描
    pub code: Vec<Instruction>,
}

pub struct GlobalDef {
    pub name: String,
    pub slots: u16,
    pub value_kind: u8,
    pub meta_id: u16,
}

pub struct IfaceDispatchEntry {
    pub concrete_meta_id: u16,
    pub iface_meta_id: u16,
    pub method_funcs: Vec<u32>,
}

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

### VM 结构 (vm.rs)

```rust
pub struct CallFrame {
    pub func_id: u32,
    pub bp: usize,       // base pointer
    pub pc: usize,
    pub ret_reg: u16,
    pub ret_count: u8,
}

pub struct DeferEntry {
    pub frame_depth: usize,
    pub closure: GcRef,      // 0 参数 closure
    pub is_errdefer: bool,
}

pub struct DeferState {
    pub pending: Vec<DeferEntry>,
    pub ret_vals: Vec<u64>,
    pub caller_ret_reg: u16,
    pub caller_ret_count: usize,
    pub is_error_return: bool,
}

pub struct Fiber {
    pub stack: Vec<u64>,
    pub frames: Vec<CallFrame>,
    pub defer_stack: Vec<DeferEntry>,
    pub defer_state: Option<DeferState>,
    pub panic_value: Option<u64>,
}

pub struct Vm {
    pub module: Option<Module>,
    pub gc: Gc,
    pub globals: Vec<u64>,
    pub scheduler: Scheduler,
}

impl Vm {
    pub fn new() -> Self;
    pub fn load_module(&mut self, module: Module);
    pub fn run(&mut self) -> VmResult;
}
```

---

## P3: vo-codegen-vm

AST → Bytecode 编译。

### 编译上下文 (context.rs)

```rust
pub struct CodegenContext {
    module: Module,
    func_indices: HashMap<FuncKey, u32>,
    extern_indices: HashMap<Symbol, u32>,
    global_indices: HashMap<Symbol, u32>,
    const_indices: HashMap<ConstKey, u16>,
    struct_meta_ids: HashMap<TypeKey, u16>,
    interface_meta_ids: HashMap<TypeKey, u16>,
    iface_dispatch_registered: HashSet<(u16, u16)>,
}

impl CodegenContext {
    pub fn register_func(&mut self, recv: Option<TypeKey>, name: Symbol) -> u32;
    pub fn register_struct_type(&mut self, key: TypeKey) -> u16;
    pub fn const_int(&mut self, val: i64) -> u16;
    pub fn const_string(&mut self, val: &str) -> u16;
    pub fn finish(self) -> Module;
}
```

### 函数构建器 (func.rs)

```rust
pub struct LocalVar {
    pub symbol: Symbol,
    pub slot: u16,
    pub slots: u16,
    pub is_stack: bool,  // true=栈, false=堆(GcRef)
}

pub struct FuncBuilder {
    name: String,
    next_slot: u16,
    locals: Vec<LocalVar>,
    slot_types: Vec<SlotType>,
    code: Vec<Instruction>,
    loop_stack: Vec<LoopContext>,
}

impl FuncBuilder {
    pub fn define_local(&mut self, sym: Symbol, slots: u16, slot_types: &[SlotType]) -> u16;
    pub fn define_local_heap(&mut self, sym: Symbol) -> u16;
    pub fn emit_op(&mut self, op: Opcode, a: u16, b: u16, c: u16);
    pub fn emit_jump(&mut self, op: Opcode) -> usize;
    pub fn patch_jump(&mut self, pc: usize, target: usize);
    pub fn finish(self) -> FunctionDef;
}
```

### 类型信息 (type_info.rs)

```rust
pub struct TypeInfo<'a> {
    query: &'a TypeQuery,
    expr_types: HashMap<NodeId, TypeKey>,
}

impl TypeInfo {
    pub fn is_escaped(&self, obj_key: ObjKey) -> bool;
    pub fn is_struct_type(&self, ty: TypeKey) -> bool;
    pub fn struct_size_slots(&self, ty: TypeKey) -> usize;
    pub fn struct_field_slot_types(&self, ty: TypeKey) -> Vec<SlotType>;
}
```

---

## P4: vo-runtime-native

JIT/AOT 专用运行时。

### 符号注册 (symbols.rs)

```rust
pub struct RuntimeSymbol {
    pub name: &'static str,
    pub ptr: *const u8,
}

pub struct RuntimeSymbols {
    symbols: Vec<RuntimeSymbol>,
}

impl RuntimeSymbols {
    pub fn new() -> Self;  // 注册所有 ~50 个运行时函数
    pub fn get(&self, name: &str) -> Option<*const u8>;
}
```

### extern "C" 接口

```rust
// === GC ===
fn vo_rt_alloc(vk: u8, meta_id: u32, slots: usize) -> GcRef;
fn vo_gc_read_slot(obj: GcRef, idx: usize) -> u64;
fn vo_gc_write_slot(obj: GcRef, idx: usize, val: u64);

// === 全局变量 ===
fn vo_rt_get_global(idx: u32) -> u64;
fn vo_rt_set_global(idx: u32, val: u64);

// === 函数表 ===
fn vo_func_table_ptr() -> *const *const u8;
fn vo_set_func_ptr(idx: u32, ptr: *const u8);

// === Goroutine ===
fn vo_go_spawn(func_ptr: *const u8, args: *const u64, argc: u32);
fn vo_yield();

// === Channel ===
fn vo_chan_new(cap: usize, elem_vk: u8) -> GcRef;
fn vo_chan_send(ch: GcRef, val: u64);
fn vo_chan_recv(ch: GcRef) -> u64;
fn vo_chan_close(ch: GcRef);

// === Select ===
fn vo_select_start(case_count: u32, has_default: u32);
fn vo_select_add_recv(ch: GcRef);
fn vo_select_add_send(ch: GcRef, val: u64);
fn vo_select_exec() -> i32;

// === Defer/Panic ===
fn vo_defer_push(func_ptr: *const u8, args: *const u64, argc: u32);
fn vo_defer_pop();
fn vo_panic(val: GcRef);
fn vo_recover() -> GcRef;

// === Stack Map ===
fn register_stack_map(return_addr: usize, entry: StackMapEntry);
fn scan_native_stack(gc: &mut Gc);
```

### Goroutine (goroutine.rs)

```rust
pub struct Goroutine {
    id: u64,
    coro: Option<Coroutine<YieldReason, ResumeInput, ()>>,
}

pub struct Scheduler {
    ready_queue: Injector<u64>,
    goroutines: Mutex<HashMap<u64, Goroutine>>,
}

pub struct Channel {
    inner: Mutex<ChannelState>,
    send_cv: Condvar,
    recv_cv: Condvar,
}
```

---

## P5: vo-codegen-cranelift

Bytecode → Cranelift IR 翻译。

### 运行时函数 (runtime.rs)

```rust
pub enum RuntimeFunc {
    Alloc, ReadSlot, WriteSlot,
    GetGlobal, SetGlobal,
    StringLen, StringConcat, ...
    ArrayCreate, ArrayGet, ArraySet, ...
    SliceCreate, SliceAppend, ...
    ClosureCreate, ClosureGet, ClosureSet,
    ChanNew, ChanSend, ChanRecv,
    GoSpawn, Yield,
    // ~43 个
}

impl RuntimeFunc {
    pub fn symbol_name(&self) -> &'static str;
    pub fn signature(&self) -> Signature;
}
```

### 函数翻译器 (translate.rs)

```rust
pub struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    module: &'a Module,
    func_def: &'a FunctionDef,
    variables: Vec<Variable>,
    blocks: Vec<Block>,
}

impl FunctionTranslator {
    pub fn translate(func_def: &FunctionDef, module: &Module) -> Function;
}
```

---

## P6: vo-aot + vo-jit

编译器入口。

### AOT 编译器

```rust
pub struct AotCompiler {
    module: ObjectModule,
    ctx: Context,
}

impl AotCompiler {
    pub fn new() -> Result<Self>;
    pub fn compile_module(&mut self, bytecode: &Module) -> Result<()>;
    pub fn finish(self) -> Result<ObjectOutput>;  // .o 文件
}
```

### JIT 编译器

```rust
pub struct JitCompiler {
    module: JITModule,
    ctx: Context,
}

impl JitCompiler {
    pub fn new() -> Result<Self>;
    pub fn compile_module(&mut self, bytecode: &Module) -> Result<()>;
    pub fn run(&self) -> Result<()>;
}
```

**初始化流程**:
1. 注册 RuntimeSymbols
2. 编译所有函数
3. finalize_definitions()
4. 填充 FUNC_TABLE
5. 调用 entry 函数
