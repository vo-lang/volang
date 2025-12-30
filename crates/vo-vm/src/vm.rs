//! Virtual machine main structure.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

/// Unchecked stack read - SAFETY: caller ensures bp + offset is within bounds
macro_rules! stack_get {
    ($stack:expr, $idx:expr) => {
        unsafe { *$stack.get_unchecked($idx) }
    };
}

/// Unchecked stack write - SAFETY: caller ensures bp + offset is within bounds
macro_rules! stack_set {
    ($stack:expr, $idx:expr, $val:expr) => {
        unsafe { *$stack.get_unchecked_mut($idx) = $val }
    };
}

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::{array, slice, string};

// Reuse layout constants from vo-runtime
use array::HEADER_SLOTS as ARRAY_DATA_OFFSET;
use slice::{FIELD_ARRAY as SLICE_FIELD_ARRAY, FIELD_DATA_PTR as SLICE_FIELD_DATA_PTR,
            FIELD_LEN as SLICE_FIELD_LEN, FIELD_CAP as SLICE_FIELD_CAP};
use string::{FIELD_ARRAY as STRING_FIELD_ARRAY, FIELD_START as STRING_FIELD_START,
             FIELD_LEN as STRING_FIELD_LEN};

/// Write single element to packed array (val is u64, small types truncated)
macro_rules! array_set {
    ($arr:expr, $idx:expr, $val:expr, $elem_bytes:expr) => {{
        let byte_ptr = unsafe { ($arr as *mut u8).add(ARRAY_DATA_OFFSET * 8 + $idx * $elem_bytes) };
        match $elem_bytes {
            1 => unsafe { *byte_ptr = $val as u8 },
            2 => unsafe { *(byte_ptr as *mut u16) = $val as u16 },
            4 => unsafe { *(byte_ptr as *mut u32) = $val as u32 },
            _ => unsafe { *(byte_ptr as *mut u64) = $val },
        }
    }};
}

macro_rules! slice_array {
    ($s:expr) => {
        unsafe { *(($s as *const u64).add(SLICE_FIELD_ARRAY) as *const GcRef) }
    };
}

macro_rules! slice_data_ptr {
    ($s:expr) => {
        unsafe { *(($s as *const u64).add(SLICE_FIELD_DATA_PTR)) as *mut u8 }
    };
}

macro_rules! slice_len {
    ($s:expr) => {
        unsafe { *(($s as *const u64).add(SLICE_FIELD_LEN)) as usize }
    };
}

macro_rules! slice_cap {
    ($s:expr) => {
        unsafe { *(($s as *const u64).add(SLICE_FIELD_CAP)) as usize }
    };
}

/// Write single element to slice (val is u64, small types truncated)
macro_rules! slice_set {
    ($s:expr, $idx:expr, $val:expr, $elem_bytes:expr) => {{
        let ptr = unsafe { slice_data_ptr!($s).add($idx * $elem_bytes) };
        match $elem_bytes {
            1 => unsafe { *ptr = $val as u8 },
            2 => unsafe { *(ptr as *mut u16) = $val as u16 },
            4 => unsafe { *(ptr as *mut u32) = $val as u32 },
            _ => unsafe { *(ptr as *mut u64) = $val },
        }
    }};
}

macro_rules! string_array {
    ($s:expr) => {
        unsafe { *(($s as *const u64).add(STRING_FIELD_ARRAY) as *const GcRef) }
    };
}

macro_rules! string_start {
    ($s:expr) => {
        unsafe { *(($s as *const u32).add(STRING_FIELD_START)) as usize }
    };
}

macro_rules! string_len {
    ($s:expr) => {
        unsafe { *(($s as *const u32).add(STRING_FIELD_LEN)) as usize }
    };
}

macro_rules! string_index {
    ($s:expr, $idx:expr) => {{
        let arr = string_array!($s);
        let start = string_start!($s);
        unsafe { *((arr.add(ARRAY_DATA_OFFSET) as *const u8).add(start + $idx)) }
    }};
}

use crate::bytecode::Module;
use crate::exec::{self, ExternRegistry};
use crate::fiber::Fiber;
use crate::instruction::{Instruction, Opcode};
use crate::itab::ItabCache;
use crate::scheduler::Scheduler;

#[cfg(feature = "jit")]
use vo_jit::{JitManager, JitConfig, JitFunc, OsrResult};

#[cfg(feature = "jit")]
use vo_runtime::jit_api::{JitResult, JitContext};

// =============================================================================
// JIT Trampolines (inlined from jit_bridge)
// =============================================================================

#[cfg(feature = "jit")]
extern "C" fn itab_lookup_trampoline(
    itabs: *const std::ffi::c_void,
    itab_id: u32,
    method_idx: u32,
) -> u32 {
    unsafe {
        let itabs = itabs as *const crate::bytecode::Itab;
        let itab = &*itabs.add(itab_id as usize);
        itab.methods[method_idx as usize]
    }
}

#[cfg(feature = "jit")]
extern "C" fn call_extern_trampoline(
    registry: *const std::ffi::c_void,
    gc: *mut vo_runtime::gc::Gc,
    extern_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
) -> JitResult {
    use vo_runtime::ffi::{ExternResult, ExternRegistry};
    
    let registry = unsafe { &*(registry as *const ExternRegistry) };
    let gc = unsafe { &mut *gc };
    
    let mut temp_stack: Vec<u64> = (0..arg_count as usize)
        .map(|i| unsafe { *args.add(i) })
        .collect();
    
    let result = registry.call(extern_id, &mut temp_stack, 0, 0, arg_count as u16, 0, gc);
    
    match result {
        ExternResult::Ok => {
            for i in 0..arg_count as usize {
                unsafe { *ret.add(i) = temp_stack[i] };
            }
            JitResult::Ok
        }
        ExternResult::Yield => JitResult::Panic,
        ExternResult::Panic(_) => JitResult::Panic,
    }
}

#[cfg(feature = "jit")]
extern "C" fn vm_call_trampoline(
    vm: *mut std::ffi::c_void,
    _fiber: *mut std::ffi::c_void,
    func_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult {
    let vm = unsafe { &mut *(vm as *mut Vm) };
    vm.execute_jit_call(func_id, args, arg_count, ret, ret_count)
}

#[cfg(feature = "jit")]
fn build_jit_ctx(
    state: &mut VmState,
    jit_func_table: *const *const u8,
    jit_func_count: u32,
    vm_ptr: *mut std::ffi::c_void,
    fiber_ptr: *mut std::ffi::c_void,
    module_ptr: *const std::ffi::c_void,
    safepoint_flag: *const bool,
    panic_flag: *mut bool,
) -> JitContext {
    JitContext {
        gc: &mut state.gc as *mut _,
        globals: state.globals.as_mut_ptr(),
        safepoint_flag,
        panic_flag,
        vm: vm_ptr,
        fiber: fiber_ptr,
        call_vm_fn: Some(vm_call_trampoline),
        itabs: state.itab_cache.itabs_ptr(),
        itab_lookup_fn: Some(itab_lookup_trampoline),
        extern_registry: &state.extern_registry as *const _ as *const std::ffi::c_void,
        call_extern_fn: Some(call_extern_trampoline),
        itab_cache: &mut state.itab_cache as *mut _ as *mut std::ffi::c_void,
        module: module_ptr,
        iface_assert_fn: None,
        jit_func_table,
        jit_func_count,
    }
}

/// Time slice: number of instructions before forced yield check.
const TIME_SLICE: u32 = 1000;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecResult {
    Continue,
    Return,
    Yield,
    Block,  // Channel blocking - don't re-queue, wait for wake
    Panic,
    Done,
    /// OSR request: (func_id, backedge_pc, loop_header_pc)
    Osr(u32, usize, usize),
}

#[derive(Debug)]
pub enum VmError {
    NoEntryFunction,
    InvalidFunctionId(u32),
    StackOverflow,
    StackUnderflow,
    InvalidOpcode(u8),
    DivisionByZero,
    IndexOutOfBounds,
    NilPointerDereference,
    TypeAssertionFailed,
    PanicUnwound,
    SendOnClosedChannel,
}

/// VM mutable state that can be borrowed independently from scheduler.
pub struct VmState {
    pub gc: Gc,
    pub globals: Vec<u64>,
    pub itab_cache: ItabCache,
    pub extern_registry: ExternRegistry,
}

impl VmState {
    pub fn new() -> Self {
        Self {
            gc: Gc::new(),
            globals: Vec::new(),
            itab_cache: ItabCache::new(),
            extern_registry: ExternRegistry::new(),
        }
    }
}

impl Default for VmState {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Vm {
    pub module: Option<Module>,
    pub scheduler: Scheduler,
    pub state: VmState,
    /// JIT manager (only available with "jit" feature).
    #[cfg(feature = "jit")]
    pub jit_mgr: Option<JitManager>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            module: None,
            scheduler: Scheduler::new(),
            state: VmState::new(),
            #[cfg(feature = "jit")]
            jit_mgr: None,
        }
    }
    
    /// Create a VM with custom JIT thresholds.
    #[cfg(feature = "jit")]
    pub fn with_jit_thresholds(call_threshold: u32, loop_threshold: u32) -> Self {
        let mut vm = Self::new();
        let config = JitConfig {
            call_threshold,
            loop_threshold,
        };
        if let Ok(mgr) = JitManager::with_config(config) {
            vm.jit_mgr = Some(mgr);
        }
        vm
    }
    
    #[cfg(not(feature = "jit"))]
    pub fn with_jit_thresholds(_call_threshold: u32, _loop_threshold: u32) -> Self {
        Self::new()
    }

    /// Initialize JIT compiler (if jit feature is enabled).
    ///
    /// Call this after creating the VM to enable JIT compilation.
    /// If JIT initialization fails, the VM will continue with interpretation only.
    #[cfg(feature = "jit")]
    pub fn init_jit(&mut self) {
        match JitManager::new() {
            Ok(mgr) => {
                self.jit_mgr = Some(mgr);
            }
            Err(e) => {
                #[cfg(feature = "std")]
                eprintln!("Warning: JIT initialization failed: {}", e);
            }
        }
    }

    /// Check if JIT is available and enabled.
    #[cfg(feature = "jit")]
    pub fn has_jit(&self) -> bool {
        self.jit_mgr.is_some()
    }

    #[cfg(not(feature = "jit"))]
    pub fn has_jit(&self) -> bool {
        false
    }
}

// =============================================================================
// JitCallContext Implementation
// =============================================================================

#[cfg(feature = "jit")]
impl vo_runtime::jit_api::JitCallContext for Vm {
    fn read_args(&self, fiber_id: u32, arg_start: u16, arg_count: usize) -> Vec<u64> {
        use crate::scheduler::is_trampoline_fiber;
        let fiber = if is_trampoline_fiber(fiber_id) {
            self.scheduler.trampoline_fiber(fiber_id)
        } else {
            &self.scheduler.fibers[fiber_id as usize]
        };
        (0..arg_count)
            .map(|i| fiber.read_reg(arg_start + i as u16))
            .collect()
    }
    
    fn write_returns(&mut self, fiber_id: u32, ret_start: u16, values: &[u64]) {
        use crate::scheduler::is_trampoline_fiber;
        let fiber = if is_trampoline_fiber(fiber_id) {
            self.scheduler.trampoline_fiber_mut(fiber_id)
        } else {
            &mut self.scheduler.fibers[fiber_id as usize]
        };
        for (i, val) in values.iter().enumerate() {
            fiber.write_reg(ret_start + i as u16, *val);
        }
    }
    
    fn read_locals(&self, fiber_id: u32, bp: usize, local_count: usize) -> Vec<u64> {
        use crate::scheduler::is_trampoline_fiber;
        let fiber = if is_trampoline_fiber(fiber_id) {
            self.scheduler.trampoline_fiber(fiber_id)
        } else {
            &self.scheduler.fibers[fiber_id as usize]
        };
        fiber.stack[bp..bp + local_count].to_vec()
    }
    
    fn build_context(
        &mut self,
        fiber_id: u32,
        safepoint_flag: *const bool,
        panic_flag: *mut bool,
    ) -> vo_runtime::jit_api::JitContext {
        use crate::scheduler::is_trampoline_fiber;
        let jit_mgr = self.jit_mgr.as_ref().unwrap();
        let func_table_ptr = jit_mgr.func_table_ptr();
        let func_table_len = jit_mgr.func_table_len() as u32;
        let fiber_ptr = if is_trampoline_fiber(fiber_id) {
            self.scheduler.trampoline_fiber_mut(fiber_id) as *mut Fiber as *mut std::ffi::c_void
        } else {
            &mut self.scheduler.fibers[fiber_id as usize] as *mut Fiber as *mut std::ffi::c_void
        };
        let vm_ptr = self as *mut _ as *mut std::ffi::c_void;
        let module_ptr = self.module.as_ref().map(|m| m as *const _ as *const std::ffi::c_void).unwrap_or(std::ptr::null());
        
        build_jit_ctx(
            &mut self.state, func_table_ptr, func_table_len,
            vm_ptr, fiber_ptr, module_ptr,
            safepoint_flag, panic_flag,
        )
    }
}

impl Vm {
    /// Call a JIT function with a fresh context (raw pointer version).
    #[cfg(feature = "jit")]
    fn call_jit_direct(
        &mut self,
        jit_func: vo_jit::JitFunc,
        args: *mut u64,
        ret: *mut u64,
    ) -> JitResult {
        let jit_mgr = self.jit_mgr.as_ref().unwrap();
        let func_table_ptr = jit_mgr.func_table_ptr();
        let func_table_len = jit_mgr.func_table_len() as u32;
        let safepoint_flag = false;
        let mut panic_flag = false;
        let vm_ptr = self as *mut _ as *mut std::ffi::c_void;
        let module_ptr = self.module.as_ref()
            .map(|m| m as *const _ as *const std::ffi::c_void)
            .unwrap_or(std::ptr::null());
        
        let mut ctx = build_jit_ctx(
            &mut self.state, func_table_ptr, func_table_len,
            vm_ptr, std::ptr::null_mut(), module_ptr,
            &safepoint_flag, &mut panic_flag,
        );
        jit_func(&mut ctx, args, ret)
    }

    /// Call a JIT function with slices (for OSR).
    #[cfg(feature = "jit")]
    fn call_jit_with_slices(
        &mut self,
        jit_func: vo_jit::JitFunc,
        args: &mut [u64],
        ret: &mut [u64],
    ) -> JitResult {
        self.call_jit_direct(jit_func, args.as_mut_ptr(), ret.as_mut_ptr())
    }

    /// Execute a JIT->VM call. This is the core logic for vm_call_trampoline.
    /// Handles JIT compilation, hot function detection, and VM fallback.
    #[cfg(feature = "jit")]
    pub fn execute_jit_call(
        &mut self,
        func_id: u32,
        args: *const u64,
        arg_count: u32,
        ret: *mut u64,
        ret_count: u32,
    ) -> JitResult {
        let module = match &self.module {
            Some(m) => m as *const Module,
            None => return JitResult::Panic,
        };
        let module = unsafe { &*module };
        
        // Use JitManager unified entry point
        if let Some(jit_mgr) = self.jit_mgr.as_mut() {
            // 1. Check if JIT version exists
            if let Some(jit_func) = jit_mgr.get_entry(func_id) {
                return self.call_jit_direct(jit_func, args as *mut u64, ret);
            }
            
            // 2. Record call and try to compile if hot
            if jit_mgr.record_call(func_id) {
                let func_def = &module.functions[func_id as usize];
                if jit_mgr.compile_full(func_id, func_def, module).is_ok() {
                    if let Some(jit_func) = jit_mgr.get_entry(func_id) {
                        return self.call_jit_direct(jit_func, args as *mut u64, ret);
                    }
                }
            }
        }
        
        // 3. Fall back to VM interpretation using trampoline fiber
        let func_def = &module.functions[func_id as usize];
        let local_slots = func_def.local_slots;
        let param_slots = func_def.param_slots as usize;
        
        // Acquire a trampoline fiber from the pool
        let trampoline_id = self.scheduler.acquire_trampoline_fiber();
        
        {
            let fiber = self.scheduler.trampoline_fiber_mut(trampoline_id);
            
            // Setup initial frame
            fiber.push_frame(func_id, local_slots, 0, ret_count as u16);
            
            // Copy arguments
            for i in 0..param_slots.min(arg_count as usize) {
                fiber.stack[i] = unsafe { *args.add(i) };
            }
        }
        
        // Run the fiber until completion
        let result = loop {
            let exec_result = self.run_fiber(trampoline_id);
            match exec_result {
                ExecResult::Done => break JitResult::Ok,
                ExecResult::Panic => break JitResult::Panic,
                ExecResult::Osr(osr_func_id, backedge_pc, loop_header_pc) => {
                    // Handle OSR
                    if let Some(jit_mgr) = self.jit_mgr.as_mut() {
                        let osr_action = jit_mgr.try_osr(osr_func_id, backedge_pc, loop_header_pc);
                        
                        let osr_func = match osr_action {
                            OsrResult::Ready(ptr) => Some(ptr),
                            OsrResult::ShouldCompile => {
                                let osr_func_def = &module.functions[osr_func_id as usize];
                                jit_mgr.compile_osr(osr_func_id, loop_header_pc, osr_func_def, module).ok()
                            }
                            OsrResult::NotHot => None,
                        };
                        
                        if let Some(osr_ptr) = osr_func {
                            let osr_func_def = &module.functions[osr_func_id as usize];
                            let osr_local_slots = osr_func_def.local_slots as usize;
                            let osr_ret_slots = osr_func_def.ret_slots as usize;
                            
                            let fiber = self.scheduler.trampoline_fiber_mut(trampoline_id);
                            let frame = fiber.frames.last().unwrap();
                            let osr_bp = frame.bp;
                            let osr_ret_reg = frame.ret_reg as usize;
                            
                            let mut locals: Vec<u64> = fiber.stack[osr_bp..osr_bp + osr_local_slots].to_vec();
                            let mut ret_buf: Vec<u64> = vec![0; osr_ret_slots];
                            
                            let jit_result = self.call_jit_with_slices(osr_ptr, &mut locals, &mut ret_buf);
                            
                            let fiber = self.scheduler.trampoline_fiber_mut(trampoline_id);
                            match jit_result {
                                JitResult::Ok => {
                                    fiber.frames.pop();
                                    if fiber.frames.is_empty() {
                                        // Copy return values
                                        for (i, val) in ret_buf.iter().enumerate() {
                                            if i < osr_ret_slots {
                                                fiber.stack[i] = *val;
                                            }
                                        }
                                        break JitResult::Ok;
                                    }
                                    // Write to caller frame
                                    let caller_bp = fiber.frames.last().unwrap().bp;
                                    for (i, val) in ret_buf.iter().enumerate() {
                                        if i < osr_ret_slots {
                                            fiber.stack[caller_bp + osr_ret_reg + i] = *val;
                                        }
                                    }
                                }
                                JitResult::Panic => break JitResult::Panic,
                            }
                        }
                    }
                    // Continue execution
                }
                _ => {
                    // Continue for other results
                }
            }
        };
        
        // Copy return values
        if result == JitResult::Ok {
            let fiber = self.scheduler.trampoline_fiber(trampoline_id);
            for i in 0..(ret_count as usize) {
                if i < fiber.stack.len() {
                    unsafe { *ret.add(i) = fiber.stack[i] };
                }
            }
        }
        
        // Release the trampoline fiber back to the pool
        self.scheduler.release_trampoline_fiber(trampoline_id);
        
        result
    }

    /// Call a JIT function inline (used by resolve_call path).
    #[cfg(feature = "jit")]
    fn call_jit_inline(
        &mut self,
        fiber_id: u32,
        jit_func: JitFunc,
        arg_start: u16,
        arg_slots: usize,
        ret_slots: usize,
    ) -> ExecResult {
        use vo_runtime::jit_api::JitCallContext;
        
        let mut args = JitCallContext::read_args(self, fiber_id, arg_start, arg_slots);
        let mut ret_buf = vec![0u64; ret_slots];
        
        let safepoint_flag = false;
        let mut panic_flag = false;
        let mut jit_ctx = JitCallContext::build_context(self, fiber_id, &safepoint_flag, &mut panic_flag);
        let result = jit_func(&mut jit_ctx, args.as_mut_ptr(), ret_buf.as_mut_ptr());
        
        if result == JitResult::Ok {
            JitCallContext::write_returns(self, fiber_id, arg_start, &ret_buf);
        }
        
        match result {
            JitResult::Ok => ExecResult::Continue,
            JitResult::Panic => ExecResult::Panic,
        }
    }

    /// Try to perform OSR (On-Stack Replacement) at a loop back-edge.
    ///
    /// If the loop is hot enough, compile an OSR version and switch to JIT execution.
    /// Returns Some(ExecResult) if OSR was performed, None to continue VM execution.
    #[cfg(feature = "jit")]
    fn try_osr(
        &mut self,
        fiber_id: u32,
        func_id: u32,
        backedge_pc: usize,
        loop_header_pc: usize,
        bp: usize,
    ) -> Option<ExecResult> {
        // Get function info
        let module = self.module.as_ref()?;
        let func_def = &module.functions[func_id as usize];
        let local_slots = func_def.local_slots as usize;
        let ret_slots = func_def.ret_slots as usize;
        
        // Try to get OSR function
        let jit_mgr = self.jit_mgr.as_mut()?;
        let osr_func = match jit_mgr.try_osr(func_id, backedge_pc, loop_header_pc) {
            OsrResult::Ready(ptr) => ptr,
            OsrResult::ShouldCompile => {
                let module = self.module.as_ref()?;
                let func_def = &module.functions[func_id as usize];
                let jit_mgr = self.jit_mgr.as_mut()?;
                match jit_mgr.compile_osr(func_id, loop_header_pc, func_def, module) {
                    Ok(ptr) => ptr,
                    Err(_) => return None,
                }
            }
            OsrResult::NotHot => return None,
        };
        
        // Execute OSR call inline
        use vo_runtime::jit_api::JitCallContext;
        let mut locals = JitCallContext::read_locals(self, fiber_id, bp, local_slots);
        let mut ret_buf = vec![0u64; ret_slots];
        let safepoint_flag = false;
        let mut panic_flag = false;
        let mut jit_ctx = JitCallContext::build_context(self, fiber_id, &safepoint_flag, &mut panic_flag);
        let result = osr_func(&mut jit_ctx, locals.as_mut_ptr(), ret_buf.as_mut_ptr());
        
        // Handle result (VM-specific: pop frame, write to caller)
        match result {
            JitResult::Ok => {
                use crate::scheduler::is_trampoline_fiber;
                let fiber = if is_trampoline_fiber(fiber_id) {
                    self.scheduler.trampoline_fiber_mut(fiber_id)
                } else {
                    &mut self.scheduler.fibers[fiber_id as usize]
                };
                let frame = fiber.frames.pop()?;
                
                if fiber.frames.is_empty() {
                    return Some(ExecResult::Done);
                }
                
                let caller_bp = fiber.frames.last()?.bp;
                for (i, val) in ret_buf.iter().enumerate() {
                    if i < ret_slots {
                        fiber.stack[caller_bp + frame.ret_reg as usize + i] = *val;
                    }
                }
                
                // Trigger full compilation after successful OSR
                if let Some(jit_mgr) = self.jit_mgr.as_mut() {
                    let module = self.module.as_ref().unwrap();
                    let func_def = &module.functions[func_id as usize];
                    jit_mgr.on_osr_complete(func_id, func_def, module);
                }
                
                Some(ExecResult::Continue)
            }
            JitResult::Panic => Some(ExecResult::Panic),
        }
    }

    pub fn load(&mut self, module: Module) {
        // Register extern functions from module
        for (id, def) in module.externs.iter().enumerate() {
            if let Some(func) = vo_runtime::lookup_extern(&def.name) {
                self.state.extern_registry.register(id as u32, func);
            } else if let Some(func) = vo_runtime::lookup_extern_with_gc(&def.name) {
                self.state.extern_registry.register_with_gc(id as u32, func);
            }
        }
        
        let total_global_slots: usize = module.globals.iter().map(|g| g.slots as usize).sum();
        self.state.globals = vec![0u64; total_global_slots];
        // Initialize itab_cache from module's compile-time itabs
        self.state.itab_cache = ItabCache::from_module_itabs(module.itabs.clone());
        
        // Initialize JIT manager for this module
        #[cfg(feature = "jit")]
        if let Some(jit_mgr) = self.jit_mgr.as_mut() {
            jit_mgr.init(module.functions.len());
        }
        
        self.module = Some(module);
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        let module = self.module.as_ref().ok_or(VmError::NoEntryFunction)?;
        let entry_func = module.entry_func;

        if entry_func as usize >= module.functions.len() {
            return Err(VmError::InvalidFunctionId(entry_func));
        }

        let func = &module.functions[entry_func as usize];
        let mut fiber = Fiber::new(0);
        fiber.push_frame(entry_func, func.local_slots, 0, 0);
        self.scheduler.spawn(fiber);

        while self.scheduler.has_runnable() {
            let fiber_id = match self.scheduler.schedule_next() {
                Some(id) => id,
                None => break,
            };

            // Execute time slice for current fiber
            let result = self.run_fiber(fiber_id);
            
            match result {
                ExecResult::Continue => {
                    // Time slice exhausted, re-queue fiber
                    self.scheduler.suspend_current();
                }
                ExecResult::Return | ExecResult::Done => {
                    self.scheduler.kill_current();
                }
                ExecResult::Yield => {
                    self.scheduler.suspend_current();
                }
                ExecResult::Block => {
                    self.scheduler.block_current();
                }
                ExecResult::Panic => {
                    self.scheduler.kill_current();
                    return Err(VmError::PanicUnwound);
                }
                ExecResult::Osr(_, _, _) => {
                    // OSR result should not propagate here from run_fiber
                    // If it does, just continue
                    self.scheduler.suspend_current();
                }
            }
        }

        Ok(())
    }

    /// Run a fiber for up to TIME_SLICE instructions.
    /// Supports both regular fibers and trampoline fibers (distinguished by high bit).
    fn run_fiber(&mut self, fiber_id: u32) -> ExecResult {
        use crate::scheduler::is_trampoline_fiber;
        
        let module_ptr = match &self.module {
            Some(m) => m as *const Module,
            None => return ExecResult::Done,
        };
        // SAFETY: module_ptr is valid for the duration of run_fiber.
        let module = unsafe { &*module_ptr };
        
        let is_trampoline = is_trampoline_fiber(fiber_id);

        // Cache fiber pointer outside the loop
        // SAFETY: fiber_ptr is valid as long as we don't reallocate fibers vec (only GoStart does)
        let mut fiber_ptr = if is_trampoline {
            self.scheduler.trampoline_fiber_mut(fiber_id) as *mut Fiber
        } else {
            &mut self.scheduler.fibers[fiber_id as usize] as *mut Fiber
        };
        let mut fiber = unsafe { &mut *fiber_ptr };

        // SAFETY: We manually manage borrows via raw pointers to avoid borrow checker conflicts.
        // stack and frames are independent fields of fiber, accessed through raw pointers.
        let stack = unsafe { &mut *(&mut fiber.stack as *mut Vec<u64>) };
        let frames = unsafe { &mut *(&mut fiber.frames as *mut Vec<crate::fiber::CallFrame>) };
        
        // Macro to fetch/re-fetch frame pointer and related variables
        macro_rules! refetch_frame {
            ($frame_ptr:ident, $frame:ident, $func_id:ident, $bp:ident, $code:ident) => {
                $frame_ptr = match frames.last_mut() {
                    Some(f) => f as *mut crate::fiber::CallFrame,
                    None => return ExecResult::Done,
                };
                $frame = unsafe { &mut *$frame_ptr };
                $func_id = $frame.func_id;
                $bp = $frame.bp;
                $code = &module.functions[$func_id as usize].code;
            };
        }
        
        let mut frame_ptr: *mut crate::fiber::CallFrame;
        let mut frame: &mut crate::fiber::CallFrame;
        let mut func_id: u32;
        let mut bp: usize;
        let mut code: &[Instruction];
        refetch_frame!(frame_ptr, frame, func_id, bp, code);

        for _ in 0..TIME_SLICE {
            // SAFETY: codegen guarantees Return instruction at end of every function
            let inst = unsafe { *code.get_unchecked(frame.pc) };
            frame.pc += 1;

            // Single dispatch - all instructions handled here
            let result = match inst.opcode() {
                Opcode::Nop => ExecResult::Continue,

                Opcode::LoadInt => {
                    let val = inst.imm32() as i64 as u64;
                    stack_set!(stack, bp + inst.a as usize, val);
                    ExecResult::Continue
                }
                Opcode::LoadConst => {
                    exec::exec_load_const(stack, bp, &inst, &module.constants);
                    ExecResult::Continue
                }

                Opcode::Copy => {
                    let val = stack_get!(stack, bp + inst.b as usize);
                    stack_set!(stack, bp + inst.a as usize, val);
                    ExecResult::Continue
                }
                Opcode::CopyN => {
                    exec::exec_copy_n(stack, bp, &inst);
                    ExecResult::Continue
                }
                Opcode::SlotGet => {
                    exec::exec_slot_get(stack, bp, &inst);
                    ExecResult::Continue
                }
                Opcode::SlotSet => {
                    exec::exec_slot_set(stack, bp, &inst);
                    ExecResult::Continue
                }
                Opcode::SlotGetN => {
                    exec::exec_slot_get_n(stack, bp, &inst);
                    ExecResult::Continue
                }
                Opcode::SlotSetN => {
                    exec::exec_slot_set_n(stack, bp, &inst);
                    ExecResult::Continue
                }

                Opcode::GlobalGet => {
                    exec::exec_global_get(stack, bp, &inst, &self.state.globals);
                    ExecResult::Continue
                }
                Opcode::GlobalGetN => {
                    exec::exec_global_get_n(stack, bp, &inst, &self.state.globals);
                    ExecResult::Continue
                }
                Opcode::GlobalSet => {
                    exec::exec_global_set(&stack, bp, &inst, &mut self.state.globals);
                    ExecResult::Continue
                }
                Opcode::GlobalSetN => {
                    exec::exec_global_set_n(&stack, bp, &inst, &mut self.state.globals);
                    ExecResult::Continue
                }

                Opcode::PtrNew => {
                    exec::exec_ptr_new(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::PtrGet => {
                    exec::exec_ptr_get(stack, bp, &inst);
                    ExecResult::Continue
                }
                Opcode::PtrSet => {
                    exec::exec_ptr_set(&stack, bp, &inst);
                    ExecResult::Continue
                }
                Opcode::PtrGetN => {
                    exec::exec_ptr_get_n(stack, bp, &inst);
                    ExecResult::Continue
                }
                Opcode::PtrSetN => {
                    exec::exec_ptr_set_n(&stack, bp, &inst);
                    ExecResult::Continue
                }

                // Integer arithmetic - inline for hot path
                Opcode::AddI => {
                    let a = stack_get!(stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(stack, bp + inst.c as usize) as i64;
                    stack_set!(stack, bp + inst.a as usize, a.wrapping_add(b) as u64);
                    ExecResult::Continue
                }
                Opcode::SubI => {
                    let a = stack_get!(stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(stack, bp + inst.c as usize) as i64;
                    stack_set!(stack, bp + inst.a as usize, a.wrapping_sub(b) as u64);
                    ExecResult::Continue
                }
                Opcode::MulI => {
                    let a = stack_get!(stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(stack, bp + inst.c as usize) as i64;
                    stack_set!(stack, bp + inst.a as usize, a.wrapping_mul(b) as u64);
                    ExecResult::Continue
                }
                Opcode::DivI => {
                    let a = stack_get!(stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(stack, bp + inst.c as usize) as i64;
                    stack_set!(stack, bp + inst.a as usize, a.wrapping_div(b) as u64);
                    ExecResult::Continue
                }
                Opcode::ModI => {
                    let a = stack_get!(stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(stack, bp + inst.c as usize) as i64;
                    stack_set!(stack, bp + inst.a as usize, a.wrapping_rem(b) as u64);
                    ExecResult::Continue
                }
                Opcode::NegI => {
                    let a = stack_get!(stack, bp + inst.b as usize) as i64;
                    stack_set!(stack, bp + inst.a as usize, a.wrapping_neg() as u64);
                    ExecResult::Continue
                }

                // Float arithmetic
                Opcode::AddF => {
                    let a = f64::from_bits(stack_get!(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(stack, bp + inst.c as usize));
                    stack_set!(stack, bp + inst.a as usize, (a + b).to_bits());
                    ExecResult::Continue
                }
                Opcode::SubF => {
                    let a = f64::from_bits(stack_get!(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(stack, bp + inst.c as usize));
                    stack_set!(stack, bp + inst.a as usize, (a - b).to_bits());
                    ExecResult::Continue
                }
                Opcode::MulF => {
                    let a = f64::from_bits(stack_get!(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(stack, bp + inst.c as usize));
                    stack_set!(stack, bp + inst.a as usize, (a * b).to_bits());
                    ExecResult::Continue
                }
                Opcode::DivF => {
                    let a = f64::from_bits(stack_get!(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(stack, bp + inst.c as usize));
                    stack_set!(stack, bp + inst.a as usize, (a / b).to_bits());
                    ExecResult::Continue
                }
                Opcode::NegF => {
                    let a = f64::from_bits(stack_get!(stack, bp + inst.b as usize));
                    stack_set!(stack, bp + inst.a as usize, (-a).to_bits());
                    ExecResult::Continue
                }

                // Integer comparison - inline
                Opcode::EqI => {
                    let a = stack_get!(stack, bp + inst.b as usize);
                    let b = stack_get!(stack, bp + inst.c as usize);
                    stack_set!(stack, bp + inst.a as usize, (a == b) as u64);
                    ExecResult::Continue
                }
                Opcode::NeI => {
                    let a = stack_get!(stack, bp + inst.b as usize);
                    let b = stack_get!(stack, bp + inst.c as usize);
                    stack_set!(stack, bp + inst.a as usize, (a != b) as u64);
                    ExecResult::Continue
                }
                Opcode::LtI => {
                    let a = stack_get!(stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(stack, bp + inst.c as usize) as i64;
                    stack_set!(stack, bp + inst.a as usize, (a < b) as u64);
                    ExecResult::Continue
                }
                Opcode::LeI => {
                    let a = stack_get!(stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(stack, bp + inst.c as usize) as i64;
                    stack_set!(stack, bp + inst.a as usize, (a <= b) as u64);
                    ExecResult::Continue
                }
                Opcode::GtI => {
                    let a = stack_get!(stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(stack, bp + inst.c as usize) as i64;
                    stack_set!(stack, bp + inst.a as usize, (a > b) as u64);
                    ExecResult::Continue
                }
                Opcode::GeI => {
                    let a = stack_get!(stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(stack, bp + inst.c as usize) as i64;
                    stack_set!(stack, bp + inst.a as usize, (a >= b) as u64);
                    ExecResult::Continue
                }

                // Float comparison
                Opcode::EqF => {
                    let a = f64::from_bits(stack_get!(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(stack, bp + inst.c as usize));
                    stack_set!(stack, bp + inst.a as usize, (a == b) as u64);
                    ExecResult::Continue
                }
                Opcode::NeF => {
                    let a = f64::from_bits(stack_get!(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(stack, bp + inst.c as usize));
                    stack_set!(stack, bp + inst.a as usize, (a != b) as u64);
                    ExecResult::Continue
                }
                Opcode::LtF => {
                    let a = f64::from_bits(stack_get!(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(stack, bp + inst.c as usize));
                    stack_set!(stack, bp + inst.a as usize, (a < b) as u64);
                    ExecResult::Continue
                }
                Opcode::LeF => {
                    let a = f64::from_bits(stack_get!(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(stack, bp + inst.c as usize));
                    stack_set!(stack, bp + inst.a as usize, (a <= b) as u64);
                    ExecResult::Continue
                }
                Opcode::GtF => {
                    let a = f64::from_bits(stack_get!(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(stack, bp + inst.c as usize));
                    stack_set!(stack, bp + inst.a as usize, (a > b) as u64);
                    ExecResult::Continue
                }
                Opcode::GeF => {
                    let a = f64::from_bits(stack_get!(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(stack, bp + inst.c as usize));
                    stack_set!(stack, bp + inst.a as usize, (a >= b) as u64);
                    ExecResult::Continue
                }

                // Bitwise - inline
                Opcode::And => {
                    let a = stack_get!(stack, bp + inst.b as usize);
                    let b = stack_get!(stack, bp + inst.c as usize);
                    stack_set!(stack, bp + inst.a as usize, a & b);
                    ExecResult::Continue
                }
                Opcode::Or => {
                    let a = stack_get!(stack, bp + inst.b as usize);
                    let b = stack_get!(stack, bp + inst.c as usize);
                    stack_set!(stack, bp + inst.a as usize, a | b);
                    ExecResult::Continue
                }
                Opcode::Xor => {
                    let a = stack_get!(stack, bp + inst.b as usize);
                    let b = stack_get!(stack, bp + inst.c as usize);
                    stack_set!(stack, bp + inst.a as usize, a ^ b);
                    ExecResult::Continue
                }
                Opcode::AndNot => {
                    let a = stack_get!(stack, bp + inst.b as usize);
                    let b = stack_get!(stack, bp + inst.c as usize);
                    stack_set!(stack, bp + inst.a as usize, a & !b);
                    ExecResult::Continue
                }
                Opcode::Not => {
                    let a = stack_get!(stack, bp + inst.b as usize);
                    stack_set!(stack, bp + inst.a as usize, !a);
                    ExecResult::Continue
                }
                Opcode::Shl => {
                    let a = stack_get!(stack, bp + inst.b as usize);
                    let b = stack_get!(stack, bp + inst.c as usize) as u32;
                    stack_set!(stack, bp + inst.a as usize, a.wrapping_shl(b));
                    ExecResult::Continue
                }
                Opcode::ShrS => {
                    let a = stack_get!(stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(stack, bp + inst.c as usize) as u32;
                    stack_set!(stack, bp + inst.a as usize, a.wrapping_shr(b) as u64);
                    ExecResult::Continue
                }
                Opcode::ShrU => {
                    let a = stack_get!(stack, bp + inst.b as usize);
                    let b = stack_get!(stack, bp + inst.c as usize) as u32;
                    stack_set!(stack, bp + inst.a as usize, a.wrapping_shr(b));
                    ExecResult::Continue
                }
                Opcode::BoolNot => {
                    let a = stack_get!(stack, bp + inst.b as usize);
                    stack_set!(stack, bp + inst.a as usize, (a == 0) as u64);
                    ExecResult::Continue
                }

                // Jump - inline with OSR support
                #[cfg(feature = "jit")]
                Opcode::Jump => {
                    let offset = inst.imm32();
                    let backedge_pc = fiber.current_frame().unwrap().pc;
                    let loop_header_pc = (backedge_pc as i64 + offset as i64 - 1) as usize;
                    
                    // Check for back-edge (loop) and try OSR
                    if loop_header_pc < backedge_pc {
                        if let Some(result) = self.try_osr(fiber_id, func_id, backedge_pc, loop_header_pc, bp) {
                            return result;
                        }
                    }
                    
                    fiber.current_frame_mut().unwrap().pc = loop_header_pc;
                    ExecResult::Continue
                }
                #[cfg(not(feature = "jit"))]
                Opcode::Jump => {
                    let offset = inst.imm32();
                    let frame = fiber.current_frame_mut().unwrap();
                    frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
                    ExecResult::Continue
                }
                #[cfg(feature = "jit")]
                Opcode::JumpIf => {
                    let cond = stack_get!(stack, bp + inst.a as usize);
                    if cond != 0 {
                        let offset = inst.imm32();
                        let backedge_pc = frame.pc;
                        let loop_header_pc = (backedge_pc as i64 + offset as i64 - 1) as usize;
                        
                        if loop_header_pc < backedge_pc {
                            if let Some(result) = self.try_osr(fiber_id, func_id, backedge_pc, loop_header_pc, bp) {
                                return result;
                            }
                        }
                        frame.pc = loop_header_pc;
                    }
                    ExecResult::Continue
                }
                #[cfg(not(feature = "jit"))]
                Opcode::JumpIf => {
                    let cond = stack_get!(stack, bp + inst.a as usize);
                    if cond != 0 {
                        let offset = inst.imm32();
                        let frame = fiber.current_frame_mut().unwrap();
                        frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
                    }
                    ExecResult::Continue
                }
                #[cfg(feature = "jit")]
                Opcode::JumpIfNot => {
                    let cond = stack_get!(stack, bp + inst.a as usize);
                    if cond == 0 {
                        let offset = inst.imm32();
                        let backedge_pc = frame.pc;
                        let loop_header_pc = (backedge_pc as i64 + offset as i64 - 1) as usize;
                        
                        if loop_header_pc < backedge_pc {
                            if let Some(result) = self.try_osr(fiber_id, func_id, backedge_pc, loop_header_pc, bp) {
                                return result;
                            }
                        }
                        frame.pc = loop_header_pc;
                    }
                    ExecResult::Continue
                }
                #[cfg(not(feature = "jit"))]
                Opcode::JumpIfNot => {
                    let cond = stack_get!(stack, bp + inst.a as usize);
                    if cond == 0 {
                        let offset = inst.imm32();
                        let frame = fiber.current_frame_mut().unwrap();
                        frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
                    }
                    ExecResult::Continue
                }

                // Call instructions
                #[cfg(feature = "jit")]
                Opcode::Call => {
                    let target_func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
                    let arg_start = inst.b;
                    let arg_slots = (inst.c >> 8) as usize;
                    let ret_slots = (inst.c & 0xFF) as u16;
                    
                    // Try JIT via resolve_call
                    let target_func = &module.functions[target_func_id as usize];
                    let jit_func = self.jit_mgr.as_mut()
                        .and_then(|mgr| mgr.resolve_call(target_func_id, target_func, module));
                    
                    if let Some(jit_func) = jit_func {
                        self.call_jit_inline(fiber_id, jit_func, arg_start, arg_slots, ret_slots as usize)
                    } else {
                        exec::exec_call(stack, &mut fiber.frames, &inst, module)
                    }
                }
                #[cfg(not(feature = "jit"))]
                Opcode::Call => {
                    exec::exec_call(stack, &mut fiber.frames, &inst, module)
                }
                Opcode::CallExtern => {
                    exec::exec_call_extern(stack, bp, &inst, &module.externs, &self.state.extern_registry, &mut self.state.gc)
                }
                Opcode::CallClosure => {
                    exec::exec_call_closure(stack, &mut fiber.frames, &inst, module)
                }
                Opcode::CallIface => {
                    exec::exec_call_iface(stack, &mut fiber.frames, &inst, module, &self.state.itab_cache)
                }
                Opcode::Return => {
                    let func = &module.functions[func_id as usize];
                    let is_error_return = (inst.flags & 1) != 0;
                    exec::exec_return(stack, &mut fiber.frames, &mut fiber.defer_stack, &mut fiber.defer_state, &inst, func, module, is_error_return)
                }

                // String operations
                Opcode::StrNew => {
                    exec::exec_str_new(stack, bp, &inst, &module.constants, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::StrLen => {
                    let s = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let len = if s.is_null() { 0 } else { string_len!(s) };
                    stack_set!(stack, bp + inst.a as usize, len as u64);
                    ExecResult::Continue
                }
                Opcode::StrIndex => {
                    let s = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get!(stack, bp + inst.c as usize) as usize;
                    let byte = string_index!(s, idx);
                    stack_set!(stack, bp + inst.a as usize, byte as u64);
                    ExecResult::Continue
                }
                Opcode::StrConcat => {
                    exec::exec_str_concat(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::StrSlice => {
                    exec::exec_str_slice(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::StrEq => {
                    let a = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get!(stack, bp + inst.c as usize) as GcRef;
                    stack_set!(stack, bp + inst.a as usize, string::eq(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrNe => {
                    let a = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get!(stack, bp + inst.c as usize) as GcRef;
                    stack_set!(stack, bp + inst.a as usize, string::ne(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrLt => {
                    let a = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get!(stack, bp + inst.c as usize) as GcRef;
                    stack_set!(stack, bp + inst.a as usize, string::lt(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrLe => {
                    let a = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get!(stack, bp + inst.c as usize) as GcRef;
                    stack_set!(stack, bp + inst.a as usize, string::le(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrGt => {
                    let a = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get!(stack, bp + inst.c as usize) as GcRef;
                    stack_set!(stack, bp + inst.a as usize, string::gt(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrGe => {
                    let a = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get!(stack, bp + inst.c as usize) as GcRef;
                    stack_set!(stack, bp + inst.a as usize, string::ge(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrDecodeRune => {
                    let s = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let pos = stack_get!(stack, bp + inst.c as usize) as usize;
                    let (rune, width) = string::decode_rune_at(s, pos);
                    stack_set!(stack, bp + inst.a as usize, rune as u64);
                    stack_set!(stack, bp + inst.a as usize + 1, width as u64);
                    ExecResult::Continue
                }

                // Array operations
                Opcode::ArrayNew => {
                    exec::exec_array_new(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::ArrayGet => {
                    // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
                    let arr = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get!(stack, bp + inst.c as usize) as usize;
                    let dst = bp + inst.a as usize;
                    let off = idx as isize;
                    let base = array::data_ptr_bytes(arr);
                    let val = match inst.flags {
                        1 => unsafe { *base.offset(off) as u64 },
                        2 => unsafe { *(base.offset(off * 2) as *const u16) as u64 },
                        4 => unsafe { *(base.offset(off * 4) as *const u32) as u64 },
                        8 => unsafe { *(base.offset(off * 8) as *const u64) },
                        129 => unsafe { *base.offset(off) as i8 as i64 as u64 },
                        130 => unsafe { *(base.offset(off * 2) as *const i16) as i64 as u64 },
                        132 => unsafe { *(base.offset(off * 4) as *const i32) as i64 as u64 },
                        0x44 => unsafe { *(base.offset(off * 4) as *const u32) as u64 },
                        0 => {
                            // dynamic: elem_bytes in c+1 register
                            let elem_bytes = stack_get!(stack, bp + inst.c as usize + 1) as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                stack_set!(stack, dst + i, unsafe { *ptr });
                            }
                            return ExecResult::Continue;
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                stack_set!(stack, dst + i, unsafe { *ptr });
                            }
                            return ExecResult::Continue;
                        }
                    };
                    stack_set!(stack, dst, val);
                    ExecResult::Continue
                }
                Opcode::ArraySet => {
                    // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
                    let arr = stack_get!(stack, bp + inst.a as usize) as GcRef;
                    let idx = stack_get!(stack, bp + inst.b as usize) as usize;
                    let src = bp + inst.c as usize;
                    let off = idx as isize;
                    let base = array::data_ptr_bytes(arr);
                    let val = stack_get!(stack, src);
                    match inst.flags {
                        1 | 129 => unsafe { *base.offset(off) = val as u8 },
                        2 | 130 => unsafe { *(base.offset(off * 2) as *mut u16) = val as u16 },
                        4 | 132 => unsafe { *(base.offset(off * 4) as *mut u32) = val as u32 },
                        0x44 => unsafe { *(base.offset(off * 4) as *mut u32) = val as u32 },
                        8 => unsafe { *(base.offset(off * 8) as *mut u64) = val },
                        0 => {
                            // dynamic: elem_bytes in b+1 register
                            let elem_bytes = stack_get!(stack, bp + inst.b as usize + 1) as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                unsafe { *ptr = stack_get!(stack, src + i) };
                            }
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                unsafe { *ptr = stack_get!(stack, src + i) };
                            }
                        }
                    }
                    ExecResult::Continue
                }
                Opcode::ArrayAddr => {
                    // Get element address: a=dst, b=array_gcref, c=index, flags=elem_bytes
                    let arr = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get!(stack, bp + inst.c as usize) as usize;
                    let elem_bytes = inst.flags as usize;
                    let base = array::data_ptr_bytes(arr);
                    let addr = unsafe { base.add(idx * elem_bytes) } as u64;
                    stack_set!(stack, bp + inst.a as usize, addr);
                    ExecResult::Continue
                }

                // Slice operations
                Opcode::SliceNew => {
                    exec::exec_slice_new(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::SliceGet => {
                    // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
                    let s = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get!(stack, bp + inst.c as usize) as usize;
                    let base = slice_data_ptr!(s);
                    let dst = bp + inst.a as usize;
                    let val = match inst.flags {
                        1 => unsafe { *base.add(idx) as u64 },
                        2 => unsafe { *(base.add(idx * 2) as *const u16) as u64 },
                        4 => unsafe { *(base.add(idx * 4) as *const u32) as u64 },
                        8 => unsafe { *(base.add(idx * 8) as *const u64) },
                        129 => unsafe { *base.add(idx) as i8 as i64 as u64 },
                        130 => unsafe { *(base.add(idx * 2) as *const i16) as i64 as u64 },
                        132 => unsafe { *(base.add(idx * 4) as *const i32) as i64 as u64 },
                        0x44 => unsafe { *(base.add(idx * 4) as *const u32) as u64 },
                        0 => {
                            // dynamic: elem_bytes in c+1 register
                            let elem_bytes = stack_get!(stack, bp + inst.c as usize + 1) as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                stack_set!(stack, dst + i, unsafe { *ptr });
                            }
                            return ExecResult::Continue;
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                stack_set!(stack, dst + i, unsafe { *ptr });
                            }
                            return ExecResult::Continue;
                        }
                    };
                    stack_set!(stack, dst, val);
                    ExecResult::Continue
                }
                Opcode::SliceSet => {
                    // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
                    let s = stack_get!(stack, bp + inst.a as usize) as GcRef;
                    let idx = stack_get!(stack, bp + inst.b as usize) as usize;
                    let base = slice_data_ptr!(s);
                    let src = bp + inst.c as usize;
                    let val = stack_get!(stack, src);
                    match inst.flags {
                        1 | 129 => unsafe { *base.add(idx) = val as u8 },
                        2 | 130 => unsafe { *(base.add(idx * 2) as *mut u16) = val as u16 },
                        4 | 132 => unsafe { *(base.add(idx * 4) as *mut u32) = val as u32 },
                        0x44 => unsafe { *(base.add(idx * 4) as *mut u32) = val as u32 },
                        8 => unsafe { *(base.add(idx * 8) as *mut u64) = val },
                        0 => {
                            // dynamic: elem_bytes in b+1 register
                            let elem_bytes = stack_get!(stack, bp + inst.b as usize + 1) as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                unsafe { *ptr = stack_get!(stack, src + i) };
                            }
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                unsafe { *ptr = stack_get!(stack, src + i) };
                            }
                        }
                    }
                    ExecResult::Continue
                }
                Opcode::SliceLen => {
                    let s = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let len = if s.is_null() { 0 } else { slice_len!(s) };
                    stack_set!(stack, bp + inst.a as usize, len as u64);
                    ExecResult::Continue
                }
                Opcode::SliceCap => {
                    let s = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let cap = if s.is_null() { 0 } else { slice_cap!(s) };
                    stack_set!(stack, bp + inst.a as usize, cap as u64);
                    ExecResult::Continue
                }
                Opcode::SliceSlice => {
                    exec::exec_slice_slice(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::SliceAppend => {
                    exec::exec_slice_append(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::SliceAddr => {
                    // Get element address: a=dst, b=slice_reg, c=index, flags=elem_bytes
                    let s = stack_get!(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get!(stack, bp + inst.c as usize) as usize;
                    let elem_bytes = inst.flags as usize;
                    let base = slice_data_ptr!(s);
                    let addr = unsafe { base.add(idx * elem_bytes) } as u64;
                    stack_set!(stack, bp + inst.a as usize, addr);
                    ExecResult::Continue
                }

                // Map operations
                Opcode::MapNew => {
                    exec::exec_map_new(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::MapGet => {
                    exec::exec_map_get(stack, bp, &inst);
                    ExecResult::Continue
                }
                Opcode::MapSet => {
                    exec::exec_map_set(&stack, bp, &inst);
                    ExecResult::Continue
                }
                Opcode::MapDelete => {
                    exec::exec_map_delete(&stack, bp, &inst);
                    ExecResult::Continue
                }
                Opcode::MapLen => {
                    exec::exec_map_len(stack, bp, &inst);
                    ExecResult::Continue
                }
                Opcode::MapIterGet => {
                    exec::exec_map_iter_get(stack, bp, &inst);
                    ExecResult::Continue
                }

                // Channel operations - need scheduler access
                Opcode::ChanNew => {
                    exec::exec_chan_new(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::ChanSend => {
                    match exec::exec_chan_send(&stack, bp, fiber_id, &inst) {
                        exec::ChanResult::Continue => ExecResult::Continue,
                        exec::ChanResult::Yield => {
                            fiber.current_frame_mut().unwrap().pc -= 1;
                            ExecResult::Block
                        }
                        exec::ChanResult::Panic => ExecResult::Panic,
                        exec::ChanResult::Wake(id) => {
                            self.scheduler.wake(id);
                            ExecResult::Continue
                        }
                        exec::ChanResult::WakeMultiple(ids) => {
                            for id in ids { self.scheduler.wake(id); }
                            ExecResult::Continue
                        }
                    }
                }
                Opcode::ChanRecv => {
                    if is_trampoline {
                        panic!("ChanRecv not supported in trampoline fiber");
                    }
                    match exec::exec_chan_recv(stack, bp, fiber_id, &inst) {
                        exec::ChanResult::Continue => ExecResult::Continue,
                        exec::ChanResult::Yield => {
                            fiber.current_frame_mut().unwrap().pc -= 1;
                            ExecResult::Block
                        }
                        exec::ChanResult::Panic => ExecResult::Panic,
                        exec::ChanResult::Wake(id) => {
                            self.scheduler.wake(id);
                            ExecResult::Continue
                        }
                        exec::ChanResult::WakeMultiple(ids) => {
                            for id in ids { self.scheduler.wake(id); }
                            ExecResult::Continue
                        }
                    }
                }
                Opcode::ChanClose => {
                    if is_trampoline {
                        panic!("ChanClose not supported in trampoline fiber");
                    }
                    match exec::exec_chan_close(&stack, bp, &inst) {
                        exec::ChanResult::WakeMultiple(ids) => {
                            for id in ids { self.scheduler.wake(id); }
                        }
                        _ => {}
                    }
                    ExecResult::Continue
                }

                // Select operations - not supported in trampoline fibers
                Opcode::SelectBegin => {
                    if is_trampoline {
                        panic!("SelectBegin not supported in trampoline fiber");
                    }
                    exec::exec_select_begin(&mut fiber.select_state, &inst);
                    ExecResult::Continue
                }
                Opcode::SelectSend => {
                    if is_trampoline {
                        panic!("SelectSend not supported in trampoline fiber");
                    }
                    exec::exec_select_send(&mut fiber.select_state, &inst);
                    ExecResult::Continue
                }
                Opcode::SelectRecv => {
                    if is_trampoline {
                        panic!("SelectRecv not supported in trampoline fiber");
                    }
                    exec::exec_select_recv(&mut fiber.select_state, &inst);
                    ExecResult::Continue
                }
                Opcode::SelectExec => {
                    if is_trampoline {
                        panic!("SelectExec not supported in trampoline fiber");
                    }
                    exec::exec_select_exec(stack, bp, &mut fiber.select_state, &inst)
                }

                // Closure operations
                Opcode::ClosureNew => {
                    exec::exec_closure_new(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::ClosureGet => {
                    exec::exec_closure_get(stack, bp, &inst);
                    ExecResult::Continue
                }

                // Goroutine - needs scheduler
                // NOTE: spawn may reallocate fibers vec, must reload all pointers after
                Opcode::GoStart => {
                    let next_id = self.scheduler.fibers.len() as u32;
                    let go_result = exec::exec_go_start(&stack, bp, &inst, &module.functions, next_id);
                    self.scheduler.spawn(go_result.new_fiber);
                    // Reload fiber pointer after potential reallocation
                    fiber_ptr = &mut self.scheduler.fibers[fiber_id as usize] as *mut Fiber;
                    fiber = unsafe { &mut *fiber_ptr };
                    // Return Yield to restart loop with fresh stack/frames pointers
                    return ExecResult::Continue;
                }

                // Defer and error handling
                Opcode::DeferPush => {
                    exec::exec_defer_push(&stack, bp, &fiber.frames, &mut fiber.defer_stack, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::ErrDeferPush => {
                    exec::exec_err_defer_push(&stack, bp, &fiber.frames, &mut fiber.defer_stack, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::Panic => {
                    exec::exec_panic(&stack, bp, &mut fiber.panic_value, &inst)
                }
                Opcode::Recover => {
                    exec::exec_recover(stack, bp, &mut fiber.panic_value, &inst);
                    ExecResult::Continue
                }

                // Interface operations
                Opcode::IfaceAssign => {
                    exec::exec_iface_assign(stack, bp, &inst, &mut self.state.gc, &mut self.state.itab_cache, module);
                    ExecResult::Continue
                }
                Opcode::IfaceAssert => {
                    exec::exec_iface_assert(stack, bp, &inst, &mut self.state.itab_cache, module)
                }

                // Type conversion - inline
                Opcode::ConvI2F => {
                    let a = stack_get!(stack, bp + inst.b as usize) as i64;
                    stack_set!(stack, bp + inst.a as usize, (a as f64).to_bits());
                    ExecResult::Continue
                }
                Opcode::ConvF2I => {
                    let a = f64::from_bits(stack_get!(stack, bp + inst.b as usize));
                    stack_set!(stack, bp + inst.a as usize, a as i64 as u64);
                    ExecResult::Continue
                }
                Opcode::ConvI32I64 => {
                    let a = stack_get!(stack, bp + inst.b as usize) as i32;
                    stack_set!(stack, bp + inst.a as usize, a as i64 as u64);
                    ExecResult::Continue
                }
                Opcode::ConvI64I32 => {
                    let a = stack_get!(stack, bp + inst.b as usize) as i64;
                    stack_set!(stack, bp + inst.a as usize, a as i32 as u64);
                    ExecResult::Continue
                }
                Opcode::ConvF64F32 => {
                    let a = f64::from_bits(stack_get!(stack, bp + inst.b as usize));
                    stack_set!(stack, bp + inst.a as usize, (a as f32).to_bits() as u64);
                    ExecResult::Continue
                }
                Opcode::ConvF32F64 => {
                    let a = f32::from_bits(stack_get!(stack, bp + inst.b as usize) as u32);
                    stack_set!(stack, bp + inst.a as usize, (a as f64).to_bits());
                    ExecResult::Continue
                }

                Opcode::Invalid => ExecResult::Panic,
            };

            match result {
                ExecResult::Continue => continue,
                ExecResult::Return => {
                    let frames_empty = if is_trampoline {
                        self.scheduler.trampoline_fiber(fiber_id).frames.is_empty()
                    } else {
                        self.scheduler.fibers[fiber_id as usize].frames.is_empty()
                    };
                    if frames_empty {
                        return ExecResult::Done;
                    }
                    // Re-fetch frame_ptr and related variables after Call/Return/Defer
                    refetch_frame!(frame_ptr, frame, func_id, bp, code);
                }
                other => return other,
            }
        }

        ExecResult::Continue
    }


}


impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}
