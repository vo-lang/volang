# Backend P4: vo-runtime-native

**Parent**: [2025-12-23-backend-rewrite-plan.md](2025-12-23-backend-rewrite-plan.md)  
**Status**: Not Started  
**Est. Modules**: 35  
**Depends On**: P1 (runtime-core)

## Overview

为 JIT/AOT 提供 runtime 支持：全局 GC 管理、符号注册、goroutine 调度、stack map。

**核心原则**：
- 所有需要 GC 的操作通过全局 GC 实例
- extern "C" 接口供 Cranelift 生成的代码调用
- goroutine 使用 corosensei (stackful coroutine)

## 模块清单

### 1. 全局 GC (gc_global.rs)

```rust
use vo_runtime_core::gc::{Gc, GcRef};
use once_cell::sync::Lazy;
use parking_lot::Mutex;

/// 全局 GC 实例
static GLOBAL_GC: Lazy<Mutex<Gc>> = Lazy::new(|| Mutex::new(Gc::new()));

/// 全局变量存储
static GLOBALS: Mutex<Vec<u64>> = Mutex::new(Vec::new());

/// 全局变量是否是 GC 引用
static GLOBALS_IS_REF: Mutex<Vec<bool>> = Mutex::new(Vec::new());

/// 函数指针表
static FUNC_TABLE: RwLock<Vec<*const u8>> = RwLock::new(Vec::new());

/// 初始化
pub fn init_gc() {
    *GLOBAL_GC.lock() = Gc::new();
}

pub fn init_globals(size: usize, is_ref: Vec<bool>) {
    let mut globals = GLOBALS.lock();
    globals.clear();
    globals.resize(size, 0);
    *GLOBALS_IS_REF.lock() = is_ref;
}

pub fn init_func_table(size: usize) {
    let mut table = FUNC_TABLE.write();
    table.clear();
    table.resize(size, std::ptr::null());
}

pub fn set_func_ptr(func_id: u32, ptr: *const u8) {
    FUNC_TABLE.write()[func_id as usize] = ptr;
}
```

**extern "C" 接口**：

```rust
#[no_mangle]
pub extern "C" fn vo_rt_alloc(value_kind: u8, type_id: u16, slots: u16) -> GcRef {
    GLOBAL_GC.lock().alloc(value_kind, type_id, slots)
}

#[no_mangle]
pub extern "C" fn vo_gc_read_slot(obj: GcRef, idx: usize) -> u64 {
    Gc::read_slot(obj, idx)
}

#[no_mangle]
pub extern "C" fn vo_gc_write_slot(obj: GcRef, idx: usize, val: u64) {
    Gc::write_slot(obj, idx, val)
}

#[no_mangle]
pub extern "C" fn vo_rt_write_barrier(parent: GcRef, child: GcRef) {
    GLOBAL_GC.lock().write_barrier(parent, child)
}

#[no_mangle]
pub extern "C" fn vo_rt_get_global(idx: usize) -> u64 {
    GLOBALS.lock()[idx]
}

#[no_mangle]
pub extern "C" fn vo_rt_set_global(idx: usize, val: u64) {
    GLOBALS.lock()[idx] = val;
}

#[no_mangle]
pub extern "C" fn vo_func_table_ptr() -> *const *const u8 {
    FUNC_TABLE.read().as_ptr()
}
```

**⚠️ 注意**：
- `vo_gc_read_slot` / `vo_gc_write_slot` 是静态方法，不需要锁
- 但 `vo_rt_alloc` 需要锁 GC

### 2. 符号注册 (symbols.rs)

```rust
/// Runtime 符号
pub struct RuntimeSymbol {
    pub name: &'static str,
    pub ptr: *const u8,
}

/// 符号表
pub struct RuntimeSymbols {
    pub symbols: Vec<RuntimeSymbol>,
}

impl RuntimeSymbols {
    pub fn new() -> Self {
        let symbols = vec![
            // GC (5)
            RuntimeSymbol { name: "vo_rt_alloc", ptr: vo_rt_alloc as *const u8 },
            RuntimeSymbol { name: "vo_gc_read_slot", ptr: vo_gc_read_slot as *const u8 },
            RuntimeSymbol { name: "vo_gc_write_slot", ptr: vo_gc_write_slot as *const u8 },
            RuntimeSymbol { name: "vo_rt_write_barrier", ptr: vo_rt_write_barrier as *const u8 },
            RuntimeSymbol { name: "vo_rt_mark_gray", ptr: vo_rt_mark_gray as *const u8 },
            
            // Globals (2)
            RuntimeSymbol { name: "vo_rt_get_global", ptr: vo_rt_get_global as *const u8 },
            RuntimeSymbol { name: "vo_rt_set_global", ptr: vo_rt_set_global as *const u8 },
            
            // String (11)
            RuntimeSymbol { name: "vo_string_len", ptr: ffi::vo_string_len as *const u8 },
            RuntimeSymbol { name: "vo_string_index", ptr: ffi::vo_string_index as *const u8 },
            RuntimeSymbol { name: "vo_rt_string_concat", ptr: vo_rt_string_concat as *const u8 },
            RuntimeSymbol { name: "vo_rt_string_slice", ptr: vo_rt_string_slice as *const u8 },
            RuntimeSymbol { name: "vo_string_eq", ptr: ffi::vo_string_eq as *const u8 },
            // ... 更多 string 函数
            
            // Array (4)
            RuntimeSymbol { name: "vo_rt_array_create", ptr: vo_rt_array_create as *const u8 },
            RuntimeSymbol { name: "vo_array_len", ptr: ffi::vo_array_len as *const u8 },
            RuntimeSymbol { name: "vo_array_get", ptr: ffi::vo_array_get as *const u8 },
            RuntimeSymbol { name: "vo_array_set", ptr: ffi::vo_array_set as *const u8 },
            
            // Slice (7)
            RuntimeSymbol { name: "vo_rt_slice_create", ptr: vo_rt_slice_create as *const u8 },
            RuntimeSymbol { name: "vo_slice_len", ptr: ffi::vo_slice_len as *const u8 },
            RuntimeSymbol { name: "vo_slice_cap", ptr: ffi::vo_slice_cap as *const u8 },
            RuntimeSymbol { name: "vo_slice_get", ptr: ffi::vo_slice_get as *const u8 },
            RuntimeSymbol { name: "vo_slice_set", ptr: ffi::vo_slice_set as *const u8 },
            RuntimeSymbol { name: "vo_rt_slice_append", ptr: vo_rt_slice_append as *const u8 },
            RuntimeSymbol { name: "vo_rt_slice_slice", ptr: vo_rt_slice_slice as *const u8 },
            
            // Closure (6)
            RuntimeSymbol { name: "vo_rt_closure_create", ptr: vo_rt_closure_create as *const u8 },
            RuntimeSymbol { name: "vo_closure_func_id", ptr: ffi::vo_closure_func_id as *const u8 },
            RuntimeSymbol { name: "vo_closure_get_upvalue", ptr: ffi::vo_closure_get_upvalue as *const u8 },
            RuntimeSymbol { name: "vo_closure_set_upvalue", ptr: ffi::vo_closure_set_upvalue as *const u8 },
            RuntimeSymbol { name: "vo_rt_upval_box_create", ptr: vo_rt_upval_box_create as *const u8 },
            RuntimeSymbol { name: "vo_upval_box_get", ptr: ffi::vo_upval_box_get as *const u8 },
            RuntimeSymbol { name: "vo_upval_box_set", ptr: ffi::vo_upval_box_set as *const u8 },
            
            // Interface (3)
            RuntimeSymbol { name: "vo_interface_unbox_type", ptr: ffi::vo_interface_unbox_type as *const u8 },
            RuntimeSymbol { name: "vo_interface_unbox_data", ptr: ffi::vo_interface_unbox_data as *const u8 },
            RuntimeSymbol { name: "vo_interface_is_nil", ptr: ffi::vo_interface_is_nil as *const u8 },
            
            // Function table
            RuntimeSymbol { name: "vo_func_table_ptr", ptr: vo_func_table_ptr as *const u8 },
            
            // Extern dispatch
            RuntimeSymbol { name: "vo_extern_call", ptr: extern_dispatch::vo_extern_call as *const u8 },
            
            // Goroutine (6)
            RuntimeSymbol { name: "vo_go_spawn", ptr: goroutine::vo_go_spawn as *const u8 },
            RuntimeSymbol { name: "vo_yield", ptr: goroutine::vo_yield as *const u8 },
            RuntimeSymbol { name: "vo_chan_new", ptr: goroutine::vo_chan_new as *const u8 },
            RuntimeSymbol { name: "vo_chan_send", ptr: goroutine::vo_chan_send as *const u8 },
            RuntimeSymbol { name: "vo_chan_recv", ptr: goroutine::vo_chan_recv as *const u8 },
            RuntimeSymbol { name: "vo_chan_close", ptr: goroutine::vo_chan_close as *const u8 },
            
            // Defer/Panic (4)
            RuntimeSymbol { name: "vo_defer_push", ptr: goroutine::vo_defer_push as *const u8 },
            RuntimeSymbol { name: "vo_defer_pop", ptr: goroutine::vo_defer_pop as *const u8 },
            RuntimeSymbol { name: "vo_panic", ptr: goroutine::vo_panic as *const u8 },
            RuntimeSymbol { name: "vo_recover", ptr: goroutine::vo_recover as *const u8 },
            
            // Select (4)
            RuntimeSymbol { name: "vo_select_start", ptr: goroutine::vo_select_start as *const u8 },
            RuntimeSymbol { name: "vo_select_add_send", ptr: goroutine::vo_select_add_send as *const u8 },
            RuntimeSymbol { name: "vo_select_add_recv", ptr: goroutine::vo_select_add_recv as *const u8 },
            RuntimeSymbol { name: "vo_select_exec", ptr: goroutine::vo_select_exec as *const u8 },
            
            // Iterator (3)
            RuntimeSymbol { name: "vo_iter_begin", ptr: goroutine::vo_iter_begin as *const u8 },
            RuntimeSymbol { name: "vo_iter_next", ptr: goroutine::vo_iter_next as *const u8 },
            RuntimeSymbol { name: "vo_iter_end", ptr: goroutine::vo_iter_end as *const u8 },
            
            // Debug (4)
            RuntimeSymbol { name: "vo_debug_print", ptr: debug::vo_debug_print as *const u8 },
            RuntimeSymbol { name: "vo_assert_begin", ptr: debug::vo_assert_begin as *const u8 },
            RuntimeSymbol { name: "vo_assert_arg", ptr: debug::vo_assert_arg as *const u8 },
            RuntimeSymbol { name: "vo_assert_end", ptr: debug::vo_assert_end as *const u8 },
        ];
        
        Self { symbols }
    }
    
    pub fn get(&self, name: &str) -> Option<*const u8> {
        self.symbols.iter().find(|s| s.name == name).map(|s| s.ptr)
    }
}
```

**⚠️ 注意**：
- 区分 `vo_rt_*` (需要 GC) 和 `vo_*` (不需要 GC)
- `vo_rt_*` 函数在本模块实现，`vo_*` 从 `runtime-core/ffi.rs` 导入

### 3. Stack Map (stack_map.rs)

```rust
/// Stack map 条目
#[derive(Debug, Clone, Default)]
pub struct StackMapEntry {
    /// 栈指针偏移，指向 GcRef
    pub sp_offsets: Vec<i32>,
}

/// 全局 stack map 表
static STACK_MAPS: Lazy<RwLock<HashMap<usize, StackMapEntry>>> = 
    Lazy::new(|| RwLock::new(HashMap::new()));

/// 注册单个 stack map
pub fn register_stack_map(return_addr: usize, entry: StackMapEntry) {
    if entry.is_empty() { return; }
    STACK_MAPS.write().insert(return_addr, entry);
}

/// 批量注册 (AOT 用)
pub fn register_stack_maps_batch(base_addr: usize, maps: &[(u32, StackMapEntry)]) {
    let mut table = STACK_MAPS.write();
    for (offset, entry) in maps {
        if !entry.is_empty() {
            table.insert(base_addr + *offset as usize, entry.clone());
        }
    }
}

/// 查找
pub fn lookup_stack_map(return_addr: usize) -> Option<StackMapEntry> {
    STACK_MAPS.read().get(&return_addr).cloned()
}

/// 扫描原生栈
pub fn scan_native_stack(gc: &mut Gc) {
    backtrace::trace(|frame| {
        let ip = frame.ip() as usize;
        
        if let Some(entry) = lookup_stack_map(ip) {
            let sp = frame.sp() as usize;
            
            if sp != 0 {
                for &offset in &entry.sp_offsets {
                    let slot_addr = if offset >= 0 {
                        sp.wrapping_add(offset as usize)
                    } else {
                        sp.wrapping_sub((-offset) as usize)
                    };
                    
                    let gc_ref = unsafe { *(slot_addr as *const u64) };
                    if gc_ref != 0 {
                        gc.mark_gray(gc_ref as GcRef);
                    }
                }
            }
        }
        
        true // 继续遍历
    });
}
```

**⚠️ 关键**：
- JIT 编译后必须调用 `register_stack_map` 注册每个 safepoint
- `scan_native_stack` 在 GC collect 时被调用

### 4. Goroutine (goroutine.rs)

```rust
use corosensei::{Coroutine, CoroutineResult};
use crossbeam_deque::{Injector, Worker, Stealer};
use parking_lot::{Mutex, Condvar};

/// Goroutine
pub struct Goroutine {
    id: u64,
    coro: Option<Coroutine<YieldReason, ResumeInput, ()>>,
}

pub enum YieldReason {
    Yield,
    ChannelSend(GcRef, u64),
    ChannelRecv(GcRef),
}

pub enum ResumeInput {
    None,
    ChannelResult(Result<u64, ChannelError>),
}

/// 调度器
pub struct Scheduler {
    next_id: AtomicU64,
    ready_queue: Injector<u64>,
    goroutines: Mutex<HashMap<u64, Goroutine>>,
    // work-stealing
    workers: Vec<Worker<u64>>,
    stealers: Vec<Stealer<u64>>,
}

impl Scheduler {
    pub fn new(num_workers: usize) -> Self;
    
    pub fn spawn(&self, func_ptr: *const u8, args: &[u64]) -> u64;
    
    pub fn yield_current(&self);
    
    pub fn run(&self);
}

/// Channel (线程安全)
pub struct Channel {
    inner: Mutex<ChannelState>,  // 复用 runtime-core 的 ChannelState
    send_cv: Condvar,
    recv_cv: Condvar,
}

impl Channel {
    pub fn new(capacity: usize) -> Self;
    pub fn send(&self, val: u64);
    pub fn recv(&self) -> u64;
    pub fn close(&self);
}
```

**extern "C" 接口**：

```rust
#[no_mangle]
pub extern "C" fn vo_go_spawn(func_ptr: *const u8, arg_ptr: *const u64, arg_count: u32) {
    let args = unsafe { std::slice::from_raw_parts(arg_ptr, arg_count as usize) };
    SCHEDULER.spawn(func_ptr, args);
}

#[no_mangle]
pub extern "C" fn vo_yield() {
    SCHEDULER.yield_current();
}

#[no_mangle]
pub extern "C" fn vo_chan_new(capacity: i64) -> GcRef {
    // 分配 Channel 对象
    // ...
}

#[no_mangle]
pub extern "C" fn vo_chan_send(ch: GcRef, val: u64) {
    // 可能阻塞
    // ...
}

#[no_mangle]
pub extern "C" fn vo_chan_recv(ch: GcRef) -> u64 {
    // 可能阻塞
    // ...
}

#[no_mangle]
pub extern "C" fn vo_chan_close(ch: GcRef) {
    // ...
}
```

**⚠️ 注意**：
- goroutine 阻塞时让出执行权，不阻塞 OS 线程
- Channel 使用 Condvar 实现等待

### 5. Defer/Panic (goroutine.rs 续)

```rust
/// Defer 栈 (每个 goroutine 有一个)
thread_local! {
    static DEFER_STACK: RefCell<Vec<DeferEntry>> = RefCell::new(Vec::new());
    static PANIC_VALUE: RefCell<Option<GcRef>> = RefCell::new(None);
}

#[no_mangle]
pub extern "C" fn vo_defer_push(func_ptr: *const u8, arg_ptr: *const u64, arg_count: u32) {
    DEFER_STACK.with(|stack| {
        let args = unsafe { std::slice::from_raw_parts(arg_ptr, arg_count as usize) };
        stack.borrow_mut().push(DeferEntry {
            func_ptr,
            args: args.to_vec(),
        });
    });
}

#[no_mangle]
pub extern "C" fn vo_defer_pop() {
    DEFER_STACK.with(|stack| {
        if let Some(entry) = stack.borrow_mut().pop() {
            // 调用 deferred 函数
            call_func(entry.func_ptr, &entry.args);
        }
    });
}

#[no_mangle]
pub extern "C" fn vo_panic(val: GcRef) {
    PANIC_VALUE.with(|pv| {
        *pv.borrow_mut() = Some(val);
    });
    
    // 执行所有 defer
    DEFER_STACK.with(|stack| {
        while let Some(entry) = stack.borrow_mut().pop() {
            call_func(entry.func_ptr, &entry.args);
        }
    });
    
    // 真正 panic
    panic!("Vo panic");
}

#[no_mangle]
pub extern "C" fn vo_recover() -> GcRef {
    PANIC_VALUE.with(|pv| {
        pv.borrow_mut().take().unwrap_or(std::ptr::null_mut())
    })
}
```

### 6. Select (goroutine.rs 续)

**设计**：指令序列统一，VM 用 Fiber 切换，JIT 用 runtime 函数。

```rust
thread_local! {
    static SELECT_STATE: RefCell<Option<SelectState>> = RefCell::new(None);
}

pub struct SelectState {
    cases: Vec<SelectCase>,
    has_default: bool,
}

pub enum SelectCase {
    Send { chan: GcRef, val: u64 },
    Recv { chan: GcRef },
}

#[no_mangle]
pub extern "C" fn vo_select_start(case_count: u32, has_default: u32) {
    SELECT_STATE.with(|s| {
        *s.borrow_mut() = Some(SelectState {
            cases: Vec::with_capacity(case_count as usize),
            has_default: has_default != 0,
        });
    });
}

#[no_mangle]
pub extern "C" fn vo_select_add_recv(chan: GcRef) {
    SELECT_STATE.with(|s| {
        s.borrow_mut().as_mut().unwrap().cases.push(SelectCase::Recv { chan });
    });
}

#[no_mangle]
pub extern "C" fn vo_select_add_send(chan: GcRef, val: u64) {
    SELECT_STATE.with(|s| {
        s.borrow_mut().as_mut().unwrap().cases.push(SelectCase::Send { chan, val });
    });
}

#[no_mangle]
pub extern "C" fn vo_select_exec() -> i32 {
    SELECT_STATE.with(|s| {
        let state = s.borrow_mut().take().unwrap();
        do_select(state)  // 可能阻塞（协程切换）
    })
}

fn do_select(state: SelectState) -> i32 {
    // 1. 随机打乱 case 顺序
    // 2. 尝试每个 case（非阻塞检查）
    // 3. 如果有 ready 的，执行并返回 case index
    // 4. 如果都阻塞且有 default，返回 case_count (default)
    // 5. 否则阻塞等待（协程 yield）
}
```

**VM vs JIT**：
| | VM | JIT |
|---|---|---|
| SelectBegin | `fiber.select_state = Some(...)` | `vo_select_start(...)` |
| SelectRecv/Send | 填充 `select_state` | `vo_select_add_*` |
| SelectEnd | 执行，可能切换 Fiber | `vo_select_exec()`（阻塞或协程切换） |

### 7. Extern Dispatch (extern_dispatch.rs)

```rust
/// Extern 函数类型
pub type ExternFn = fn(&[u64], &mut [u64]) -> Result<(), String>;

/// Extern 函数注册表
static EXTERN_FNS: Lazy<RwLock<HashMap<u32, ExternFn>>> = 
    Lazy::new(|| RwLock::new(HashMap::new()));

pub fn register_extern(extern_id: u32, f: ExternFn) {
    EXTERN_FNS.write().insert(extern_id, f);
}

#[no_mangle]
pub extern "C" fn vo_extern_call(
    extern_id: u32,
    args: *const u64,
    arg_count: u32,
    rets: *mut u64,
    ret_count: u32,
) {
    let args_slice = unsafe { std::slice::from_raw_parts(args, arg_count as usize) };
    let rets_slice = unsafe { std::slice::from_raw_parts_mut(rets, ret_count as usize) };
    
    if let Some(f) = EXTERN_FNS.read().get(&extern_id) {
        if let Err(e) = f(args_slice, rets_slice) {
            // panic 或设置错误
            panic!("extern call failed: {}", e);
        }
    } else {
        panic!("unknown extern function: {}", extern_id);
    }
}
```

### 7. Stdlib Extern 实现 (extern_fns/mod.rs)

```rust
pub fn register_all() {
    // math
    register_extern(MATH_SQRT, |args, rets| {
        rets[0] = f64::from_bits(args[0]).sqrt().to_bits();
        Ok(())
    });
    
    // strings
    register_extern(STRINGS_INDEX, |args, rets| {
        let s = args[0] as GcRef;
        let substr = args[1] as GcRef;
        rets[0] = strings::index(s, substr) as u64;
        Ok(())
    });
    
    // ...
}
```

### 8. Debug (debug.rs)

```rust
#[no_mangle]
pub extern "C" fn vo_debug_print(val: u64, kind: u8) {
    let vk = ValueKind::from_u8(kind);
    match vk {
        ValueKind::Int => println!("{}", val as i64),
        ValueKind::Float64 => println!("{}", f64::from_bits(val)),
        ValueKind::Bool => println!("{}", val != 0),
        ValueKind::String => {
            let s = val as GcRef;
            let bytes = string::as_bytes(s);
            println!("{}", std::str::from_utf8(bytes).unwrap_or("<invalid utf8>"));
        }
        _ => println!("<value kind={:?}>", vk),
    }
}

#[no_mangle]
pub extern "C" fn vo_assert_begin() {
    // 开始收集 assert 参数
}

#[no_mangle]
pub extern "C" fn vo_assert_arg(val: u64, kind: u8) {
    // 收集一个参数
}

#[no_mangle]
pub extern "C" fn vo_assert_end(passed: bool) {
    if !passed {
        // 打印收集的参数
        panic!("assertion failed");
    }
}
```

## Tasks Checklist

### gc_global.rs
- [ ] GLOBAL_GC
- [ ] GLOBALS / GLOBALS_IS_REF
- [ ] FUNC_TABLE
- [ ] init_* 函数
- [ ] vo_rt_alloc
- [ ] vo_gc_read_slot / vo_gc_write_slot
- [ ] vo_rt_write_barrier
- [ ] vo_rt_get_global / vo_rt_set_global
- [ ] vo_func_table_ptr
- [ ] vo_rt_string_* (需要 GC 的 string 操作)
- [ ] vo_rt_array_* / vo_rt_slice_*
- [ ] vo_rt_closure_* / vo_rt_upval_box_*
- [ ] collect_garbage

### symbols.rs
- [ ] RuntimeSymbol / RuntimeSymbols
- [ ] 所有 ~70 个符号注册

### stack_map.rs
- [ ] StackMapEntry
- [ ] register_stack_map
- [ ] register_stack_maps_batch
- [ ] lookup_stack_map
- [ ] scan_native_stack

### goroutine.rs
- [ ] Goroutine 结构
- [ ] Scheduler
- [ ] Channel (Mutex<ChannelState>)
- [ ] vo_go_spawn
- [ ] vo_yield
- [ ] vo_chan_*

### defer/panic
- [ ] DEFER_STACK
- [ ] vo_defer_push / vo_defer_pop
- [ ] vo_panic / vo_recover

### select
- [ ] SelectState
- [ ] vo_select_*

### extern_dispatch.rs
- [ ] ExternFn 类型
- [ ] register_extern
- [ ] vo_extern_call

### extern_fns/
- [ ] math 函数
- [ ] strings 函数
- [ ] 其他 stdlib

### debug.rs
- [ ] vo_debug_print
- [ ] vo_assert_*

## 单元测试

```rust
#[test]
fn test_gc_global() {
    init_gc();
    let obj = vo_rt_alloc(ValueKind::Struct as u8, 0, 2);
    assert!(!obj.is_null());
}

#[test]
fn test_stack_map() {
    clear_stack_maps();
    register_stack_map(0x1000, StackMapEntry::new(vec![8, 16]));
    assert!(lookup_stack_map(0x1000).is_some());
}

#[test]
fn test_channel() {
    let ch = Channel::new(1);
    ch.send(42);
    assert_eq!(ch.recv(), 42);
}
```
