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
use slice::{FIELD_ARRAY as SLICE_FIELD_ARRAY, FIELD_START as SLICE_FIELD_START,
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

macro_rules! slice_start {
    ($s:expr) => {
        unsafe { *(($s as *const u64).add(SLICE_FIELD_START)) as usize }
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
        let arr = slice_array!($s);
        let start = slice_start!($s);
        // start is element index
        array_set!(arr, start + $idx, $val, $elem_bytes)
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
use crate::hot_counter::HotCounter;
use crate::instruction::{Instruction, Opcode};
use crate::itab::ItabCache;
use crate::scheduler::Scheduler;

#[cfg(feature = "jit")]
use vo_jit::JitCompiler;

#[cfg(feature = "jit")]
use vo_runtime::jit_api::JitResult;

/// Itab lookup trampoline for JIT -> VM itab lookup.
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

/// Extern call trampoline for JIT -> extern function calls.
#[cfg(feature = "jit")]
extern "C" fn call_extern_trampoline(
    registry: *const std::ffi::c_void,
    gc: *mut vo_runtime::gc::Gc,
    extern_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
) -> JitResult {
    use vo_runtime::ffi::{ExternCall, ExternResult};
    
    let registry = unsafe { &*(registry as *const ExternRegistry) };
    let gc = unsafe { &mut *gc };
    
    // Create a temporary stack for the extern call
    let mut temp_stack: Vec<u64> = (0..arg_count as usize)
        .map(|i| unsafe { *args.add(i) })
        .collect();
    
    // Call through ExternRegistry
    let result = registry.call(
        extern_id,
        &mut temp_stack,
        0,     // bp
        0,     // arg_start
        arg_count as u16,
        0,     // ret_start (same as arg_start)
        gc,
    );
    
    match result {
        ExternResult::Ok => {
            // Copy return values (extern functions may return values in the same slots)
            for i in 0..arg_count as usize {
                unsafe { *ret.add(i) = temp_stack[i] };
            }
            JitResult::Ok
        }
        ExternResult::Yield => JitResult::Panic, // Yield not supported in JIT
        ExternResult::Panic(_) => JitResult::Panic,
    }
}

/// VM call trampoline for JIT -> VM calls.
/// This function is called by vo_call_vm when JIT code needs to call another function.
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
    // Safety: vm must be a valid pointer to Vm
    let vm = unsafe { &mut *(vm as *mut Vm) };
    let module = match &vm.module {
        Some(m) => m as *const Module,
        None => return JitResult::Panic,
    };
    let module = unsafe { &*module };
    
    let func = &module.functions[func_id as usize];
    
    // Create a temporary fiber for the call
    let mut fiber = Fiber::new(0);
    fiber.stack.resize(func.local_slots as usize, 0);
    
    // Copy args to fiber stack
    for i in 0..arg_count as usize {
        fiber.stack[i] = unsafe { *args.add(i) };
    }
    
    // Push initial frame
    fiber.frames.push(crate::fiber::CallFrame {
        func_id,
        pc: 0,
        bp: 0,
        ret_reg: 0,
        ret_count: ret_count as u16,
    });
    
    // Track if we've saved return values from the original function
    let mut saved_ret_vals: Option<Vec<u64>> = None;
    
    // Execute until done
    loop {
        // Get current frame
        let frame = match fiber.frames.last_mut() {
            Some(f) => f,
            None => break,
        };
        
        let current_func = &module.functions[frame.func_id as usize];
        if frame.pc >= current_func.code.len() {
            break;
        }
        
        let inst = current_func.code[frame.pc];
        let frame_func_id = frame.func_id;
        frame.pc += 1;
        
        // For Return instruction: save return values before exec_return modifies stack
        // Save when returning from the INITIAL frame (bp == 0) and not in defer execution
        if inst.opcode() == crate::instruction::Opcode::Return && fiber.defer_state.is_none() {
            let frame_bp = fiber.frames.last().map(|f| f.bp).unwrap_or(0);
            
            // Only save when returning from the initial frame (the one we created)
            if frame_bp == 0 {
                let ret_start = inst.a as usize;
                let ret_count_from_inst = inst.b as usize;
                
                // Bounds check before reading
                let mut vals = Vec::with_capacity(ret_count_from_inst);
                for i in 0..ret_count_from_inst {
                    let idx = ret_start + i;
                    if idx < fiber.stack.len() {
                        vals.push(fiber.stack[idx]);
                    }
                }
                saved_ret_vals = Some(vals);
            }
        }
        
        let bp = fiber.frames.last().map(|f| f.bp).unwrap_or(0);
        let result = exec_inst_inline(&mut fiber, &inst, frame_func_id, bp, module, &mut vm.state);
        
        match result {
            ExecResult::Continue => {}
            ExecResult::Return => {
                if fiber.frames.is_empty() {
                    break;
                }
            }
            ExecResult::Done => break,
            ExecResult::Panic => return JitResult::Panic,
            _ => {}
        }
    }
    
    // Copy saved return values to output buffer
    if let Some(vals) = saved_ret_vals {
        for (i, val) in vals.iter().enumerate() {
            if i < ret_count as usize {
                unsafe { *ret.add(i) = *val };
            }
        }
    }
    
    JitResult::Ok
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
    /// Hot function counter for JIT compilation decisions.
    pub hot_counter: HotCounter,
    /// JIT compiler (only available with "jit" feature).
    #[cfg(feature = "jit")]
    pub jit: Option<JitCompiler>,
    /// JIT function pointer table: jit_func_table[func_id] = pointer to JIT function.
    /// Initialized to null, filled in as functions are JIT compiled.
    /// Used for direct JIT-to-JIT calls.
    #[cfg(feature = "jit")]
    pub jit_func_table: Vec<*const u8>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            module: None,
            scheduler: Scheduler::new(),
            state: VmState::new(),
            hot_counter: HotCounter::new(),
            #[cfg(feature = "jit")]
            jit: None,
            #[cfg(feature = "jit")]
            jit_func_table: Vec::new(),
        }
    }
    
    /// Create a VM with custom hot counter thresholds.
    pub fn with_jit_thresholds(call_threshold: u32, loop_threshold: u32) -> Self {
        Self {
            module: None,
            scheduler: Scheduler::new(),
            state: VmState::new(),
            hot_counter: HotCounter::with_thresholds(call_threshold, loop_threshold),
            #[cfg(feature = "jit")]
            jit: None,
            #[cfg(feature = "jit")]
            jit_func_table: Vec::new(),
        }
    }

    /// Initialize JIT compiler (if jit feature is enabled).
    ///
    /// Call this after creating the VM to enable JIT compilation.
    /// If JIT initialization fails, the VM will continue with interpretation only.
    #[cfg(feature = "jit")]
    pub fn init_jit(&mut self) {
        match JitCompiler::new() {
            Ok(jit) => {
                self.jit = Some(jit);
            }
            Err(e) => {
                // Log error but continue - VM can run without JIT
                #[cfg(feature = "std")]
                eprintln!("Warning: JIT initialization failed: {}", e);
            }
        }
    }

    /// Check if JIT is available and enabled.
    #[cfg(feature = "jit")]
    pub fn has_jit(&self) -> bool {
        self.jit.is_some()
    }

    #[cfg(not(feature = "jit"))]
    pub fn has_jit(&self) -> bool {
        false
    }

    /// Try to call a function via JIT. Returns Some(ExecResult) if JIT was used,
    /// None if should fall back to interpreter.
    #[cfg(feature = "jit")]
    pub fn try_jit_call(
        &mut self,
        fiber_id: u32,
        func_id: u32,
        arg_start: u16,
        arg_slots: usize,
        ret_slots: u16,
    ) -> Option<ExecResult> {
        use vo_runtime::jit_api::{JitContext, JitResult};
        
        let jit = self.jit.as_ref()?;
        let func_ptr = unsafe { jit.get_func_ptr(func_id)? };
        
        // Prepare args array
        let args: Vec<u64> = {
            let fiber = &self.scheduler.fibers[fiber_id as usize];
            (0..arg_slots)
                .map(|i| fiber.read_reg(arg_start + i as u16))
                .collect()
        };
        let mut args = args;
        
        // Prepare return buffer
        let mut ret_buf: Vec<u64> = vec![0; ret_slots as usize];
        
        // Flags for safepoint and panic
        let safepoint_flag = false;
        let mut panic_flag = false;
        
        // Create JIT context
        let fiber_ptr = &mut self.scheduler.fibers[fiber_id as usize] as *mut Fiber;
        let mut ctx = JitContext {
            gc: &mut self.state.gc as *mut _,
            globals: self.state.globals.as_mut_ptr(),
            safepoint_flag: &safepoint_flag as *const bool,
            panic_flag: &mut panic_flag as *mut bool,
            vm: self as *mut _ as *mut std::ffi::c_void,
            fiber: fiber_ptr as *mut std::ffi::c_void,
            call_vm_fn: Some(vm_call_trampoline),
            itabs: self.state.itab_cache.itabs_ptr(),
            itab_lookup_fn: Some(itab_lookup_trampoline),
            extern_registry: &self.state.extern_registry as *const _ as *const std::ffi::c_void,
            call_extern_fn: Some(call_extern_trampoline),
            itab_cache: &mut self.state.itab_cache as *mut _ as *mut std::ffi::c_void,
            module: self.module.as_ref().map(|m| m as *const _ as *const std::ffi::c_void).unwrap_or(std::ptr::null()),
            iface_assert_fn: None, // TODO: implement interface assertion callback
            jit_func_table: self.jit_func_table.as_ptr() as *const *const u8,
            jit_func_count: self.jit_func_table.len() as u32,
        };
        
        // Call JIT function
        let result = func_ptr(
            &mut ctx,
            args.as_mut_ptr(),
            ret_buf.as_mut_ptr(),
        );
        
        match result {
            JitResult::Ok => {
                // Write return values to caller's registers
                let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                for (i, val) in ret_buf.iter().enumerate() {
                    fiber.write_reg(arg_start + i as u16, *val);
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
        
        // Initialize JIT function pointer table (all null initially)
        #[cfg(feature = "jit")]
        {
            let func_count = module.functions.len();
            self.jit_func_table = vec![std::ptr::null(); func_count];
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
            }
        }

        Ok(())
    }

    /// Run a fiber for up to TIME_SLICE instructions.
    fn run_fiber(&mut self, fiber_id: u32) -> ExecResult {
        let module_ptr = match &self.module {
            Some(m) => m as *const Module,
            None => return ExecResult::Done,
        };
        // SAFETY: module_ptr is valid for the duration of run_fiber.
        let module = unsafe { &*module_ptr };

        for _ in 0..TIME_SLICE {
            // Get fiber reference at start of each iteration
            // This is necessary because GoStart's spawn() may reallocate the fibers vec
            let fiber = &mut self.scheduler.fibers[fiber_id as usize];
            
            // Fetch instruction
            let frame = match fiber.current_frame_mut() {
                Some(f) => f,
                None => return ExecResult::Done,
            };
            let func_id = frame.func_id;
            let bp = frame.bp;
            let func = &module.functions[func_id as usize];
            if frame.pc >= func.code.len() {
                return ExecResult::Done;
            }
            let inst = func.code[frame.pc];
            frame.pc += 1;

            // Single dispatch - all instructions handled here
            let result = match inst.opcode() {
                Opcode::Nop => ExecResult::Continue,

                Opcode::LoadInt => {
                    let val = inst.imm32() as i64 as u64;
                    stack_set!(fiber.stack, bp + inst.a as usize, val);
                    ExecResult::Continue
                }
                Opcode::LoadConst => {
                    exec::exec_load_const(fiber, &inst, &module.constants);
                    ExecResult::Continue
                }

                Opcode::Copy => {
                    let val = stack_get!(fiber.stack, bp + inst.b as usize);
                    stack_set!(fiber.stack, bp + inst.a as usize, val);
                    ExecResult::Continue
                }
                Opcode::CopyN => {
                    exec::exec_copy_n(fiber, &inst);
                    ExecResult::Continue
                }
                Opcode::SlotGet => {
                    exec::exec_slot_get(fiber, &inst);
                    ExecResult::Continue
                }
                Opcode::SlotSet => {
                    exec::exec_slot_set(fiber, &inst);
                    ExecResult::Continue
                }
                Opcode::SlotGetN => {
                    exec::exec_slot_get_n(fiber, &inst);
                    ExecResult::Continue
                }
                Opcode::SlotSetN => {
                    exec::exec_slot_set_n(fiber, &inst);
                    ExecResult::Continue
                }

                Opcode::GlobalGet => {
                    exec::exec_global_get(fiber, &inst, &self.state.globals);
                    ExecResult::Continue
                }
                Opcode::GlobalGetN => {
                    exec::exec_global_get_n(fiber, &inst, &self.state.globals);
                    ExecResult::Continue
                }
                Opcode::GlobalSet => {
                    exec::exec_global_set(fiber, &inst, &mut self.state.globals);
                    ExecResult::Continue
                }
                Opcode::GlobalSetN => {
                    exec::exec_global_set_n(fiber, &inst, &mut self.state.globals);
                    ExecResult::Continue
                }

                Opcode::PtrNew => {
                    exec::exec_ptr_new(fiber, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::PtrGet => {
                    exec::exec_ptr_get(fiber, &inst);
                    ExecResult::Continue
                }
                Opcode::PtrSet => {
                    exec::exec_ptr_set(fiber, &inst);
                    ExecResult::Continue
                }
                Opcode::PtrGetN => {
                    exec::exec_ptr_get_n(fiber, &inst);
                    ExecResult::Continue
                }
                Opcode::PtrSetN => {
                    exec::exec_ptr_set_n(fiber, &inst);
                    ExecResult::Continue
                }

                // Integer arithmetic - inline for hot path
                Opcode::AddI => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
                    stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_add(b) as u64);
                    ExecResult::Continue
                }
                Opcode::SubI => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
                    stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_sub(b) as u64);
                    ExecResult::Continue
                }
                Opcode::MulI => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
                    stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_mul(b) as u64);
                    ExecResult::Continue
                }
                Opcode::DivI => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
                    stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_div(b) as u64);
                    ExecResult::Continue
                }
                Opcode::ModI => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
                    stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_rem(b) as u64);
                    ExecResult::Continue
                }
                Opcode::NegI => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
                    stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_neg() as u64);
                    ExecResult::Continue
                }

                // Float arithmetic
                Opcode::AddF => {
                    let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
                    stack_set!(fiber.stack, bp + inst.a as usize, (a + b).to_bits());
                    ExecResult::Continue
                }
                Opcode::SubF => {
                    let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
                    stack_set!(fiber.stack, bp + inst.a as usize, (a - b).to_bits());
                    ExecResult::Continue
                }
                Opcode::MulF => {
                    let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
                    stack_set!(fiber.stack, bp + inst.a as usize, (a * b).to_bits());
                    ExecResult::Continue
                }
                Opcode::DivF => {
                    let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
                    stack_set!(fiber.stack, bp + inst.a as usize, (a / b).to_bits());
                    ExecResult::Continue
                }
                Opcode::NegF => {
                    let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
                    stack_set!(fiber.stack, bp + inst.a as usize, (-a).to_bits());
                    ExecResult::Continue
                }

                // Integer comparison - inline
                Opcode::EqI => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize);
                    let b = stack_get!(fiber.stack, bp + inst.c as usize);
                    stack_set!(fiber.stack, bp + inst.a as usize, (a == b) as u64);
                    ExecResult::Continue
                }
                Opcode::NeI => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize);
                    let b = stack_get!(fiber.stack, bp + inst.c as usize);
                    stack_set!(fiber.stack, bp + inst.a as usize, (a != b) as u64);
                    ExecResult::Continue
                }
                Opcode::LtI => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
                    stack_set!(fiber.stack, bp + inst.a as usize, (a < b) as u64);
                    ExecResult::Continue
                }
                Opcode::LeI => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
                    stack_set!(fiber.stack, bp + inst.a as usize, (a <= b) as u64);
                    ExecResult::Continue
                }
                Opcode::GtI => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
                    stack_set!(fiber.stack, bp + inst.a as usize, (a > b) as u64);
                    ExecResult::Continue
                }
                Opcode::GeI => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
                    stack_set!(fiber.stack, bp + inst.a as usize, (a >= b) as u64);
                    ExecResult::Continue
                }

                // Float comparison
                Opcode::EqF => {
                    let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
                    stack_set!(fiber.stack, bp + inst.a as usize, (a == b) as u64);
                    ExecResult::Continue
                }
                Opcode::NeF => {
                    let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
                    stack_set!(fiber.stack, bp + inst.a as usize, (a != b) as u64);
                    ExecResult::Continue
                }
                Opcode::LtF => {
                    let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
                    stack_set!(fiber.stack, bp + inst.a as usize, (a < b) as u64);
                    ExecResult::Continue
                }
                Opcode::LeF => {
                    let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
                    stack_set!(fiber.stack, bp + inst.a as usize, (a <= b) as u64);
                    ExecResult::Continue
                }
                Opcode::GtF => {
                    let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
                    stack_set!(fiber.stack, bp + inst.a as usize, (a > b) as u64);
                    ExecResult::Continue
                }
                Opcode::GeF => {
                    let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
                    stack_set!(fiber.stack, bp + inst.a as usize, (a >= b) as u64);
                    ExecResult::Continue
                }

                // Bitwise - inline
                Opcode::And => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize);
                    let b = stack_get!(fiber.stack, bp + inst.c as usize);
                    stack_set!(fiber.stack, bp + inst.a as usize, a & b);
                    ExecResult::Continue
                }
                Opcode::Or => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize);
                    let b = stack_get!(fiber.stack, bp + inst.c as usize);
                    stack_set!(fiber.stack, bp + inst.a as usize, a | b);
                    ExecResult::Continue
                }
                Opcode::Xor => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize);
                    let b = stack_get!(fiber.stack, bp + inst.c as usize);
                    stack_set!(fiber.stack, bp + inst.a as usize, a ^ b);
                    ExecResult::Continue
                }
                Opcode::AndNot => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize);
                    let b = stack_get!(fiber.stack, bp + inst.c as usize);
                    stack_set!(fiber.stack, bp + inst.a as usize, a & !b);
                    ExecResult::Continue
                }
                Opcode::Not => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize);
                    stack_set!(fiber.stack, bp + inst.a as usize, !a);
                    ExecResult::Continue
                }
                Opcode::Shl => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize);
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as u32;
                    stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_shl(b));
                    ExecResult::Continue
                }
                Opcode::ShrS => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as u32;
                    stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_shr(b) as u64);
                    ExecResult::Continue
                }
                Opcode::ShrU => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize);
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as u32;
                    stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_shr(b));
                    ExecResult::Continue
                }
                Opcode::BoolNot => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize);
                    stack_set!(fiber.stack, bp + inst.a as usize, (a == 0) as u64);
                    ExecResult::Continue
                }

                // Jump - inline
                Opcode::Jump => {
                    let offset = inst.imm32();
                    let frame = fiber.current_frame_mut().unwrap();
                    frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
                    ExecResult::Continue
                }
                Opcode::JumpIf => {
                    let cond = stack_get!(fiber.stack, bp + inst.a as usize);
                    if cond != 0 {
                        let offset = inst.imm32();
                        let frame = fiber.current_frame_mut().unwrap();
                        frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
                    }
                    ExecResult::Continue
                }
                Opcode::JumpIfNot => {
                    let cond = stack_get!(fiber.stack, bp + inst.a as usize);
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
                    
                    let has_jit_func = self.jit.as_ref()
                        .and_then(|jit| unsafe { jit.get_func_ptr(target_func_id) })
                        .is_some();
                    
                    if has_jit_func {
                        self.try_jit_call(fiber_id, target_func_id, arg_start, arg_slots, ret_slots)
                            .unwrap_or(ExecResult::Continue)
                    } else {
                        if self.hot_counter.record_call(target_func_id) {
                            if let Some(jit) = &mut self.jit {
                                let target_func = &module.functions[target_func_id as usize];
                                if jit.can_jit(target_func, module) {
                                    if jit.compile(target_func_id, target_func, module).is_ok() {
                                        if let Some(ptr) = unsafe { jit.get_func_ptr(target_func_id) } {
                                            if (target_func_id as usize) < self.jit_func_table.len() {
                                                self.jit_func_table[target_func_id as usize] = ptr as *const u8;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                        exec::exec_call(fiber, &inst, module)
                    }
                }
                #[cfg(not(feature = "jit"))]
                Opcode::Call => {
                    exec::exec_call(fiber, &inst, module)
                }
                Opcode::CallExtern => {
                    exec::exec_call_extern(fiber, &inst, &module.externs, &self.state.extern_registry, &mut self.state.gc)
                }
                Opcode::CallClosure => {
                    exec::exec_call_closure(fiber, &inst, module)
                }
                Opcode::CallIface => {
                    exec::exec_call_iface(fiber, &inst, module, &self.state.itab_cache)
                }
                Opcode::Return => {
                    let func = &module.functions[func_id as usize];
                    let is_error_return = (inst.flags & 1) != 0;
                    exec::exec_return(fiber, &inst, func, module, is_error_return)
                }

                // String operations
                Opcode::StrNew => {
                    exec::exec_str_new(fiber, &inst, &module.constants, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::StrLen => {
                    let s = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
                    let len = if s.is_null() { 0 } else { string_len!(s) };
                    stack_set!(fiber.stack, bp + inst.a as usize, len as u64);
                    ExecResult::Continue
                }
                Opcode::StrIndex => {
                    let s = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get!(fiber.stack, bp + inst.c as usize) as usize;
                    let byte = string_index!(s, idx);
                    stack_set!(fiber.stack, bp + inst.a as usize, byte as u64);
                    ExecResult::Continue
                }
                Opcode::StrConcat => {
                    exec::exec_str_concat(fiber, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::StrSlice => {
                    exec::exec_str_slice(fiber, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::StrEq => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as GcRef;
                    stack_set!(fiber.stack, bp + inst.a as usize, string::eq(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrNe => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as GcRef;
                    stack_set!(fiber.stack, bp + inst.a as usize, string::ne(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrLt => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as GcRef;
                    stack_set!(fiber.stack, bp + inst.a as usize, string::lt(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrLe => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as GcRef;
                    stack_set!(fiber.stack, bp + inst.a as usize, string::le(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrGt => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as GcRef;
                    stack_set!(fiber.stack, bp + inst.a as usize, string::gt(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrGe => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get!(fiber.stack, bp + inst.c as usize) as GcRef;
                    stack_set!(fiber.stack, bp + inst.a as usize, string::ge(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrDecodeRune => {
                    let s = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
                    let pos = stack_get!(fiber.stack, bp + inst.c as usize) as usize;
                    let (rune, width) = string::decode_rune_at(s, pos);
                    stack_set!(fiber.stack, bp + inst.a as usize, rune as u64);
                    stack_set!(fiber.stack, bp + inst.a as usize + 1, width as u64);
                    ExecResult::Continue
                }

                // Array operations
                Opcode::ArrayNew => {
                    exec::exec_array_new(fiber, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::ArrayGet => {
                    // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
                    let arr = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get!(fiber.stack, bp + inst.c as usize) as usize;
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
                            let elem_bytes = stack_get!(fiber.stack, bp + inst.c as usize + 1) as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                stack_set!(fiber.stack, dst + i, unsafe { *ptr });
                            }
                            return ExecResult::Continue;
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                stack_set!(fiber.stack, dst + i, unsafe { *ptr });
                            }
                            return ExecResult::Continue;
                        }
                    };
                    stack_set!(fiber.stack, dst, val);
                    ExecResult::Continue
                }
                Opcode::ArraySet => {
                    // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
                    let arr = stack_get!(fiber.stack, bp + inst.a as usize) as GcRef;
                    let idx = stack_get!(fiber.stack, bp + inst.b as usize) as usize;
                    let src = bp + inst.c as usize;
                    let off = idx as isize;
                    let base = array::data_ptr_bytes(arr);
                    let val = stack_get!(fiber.stack, src);
                    match inst.flags {
                        1 | 129 => unsafe { *base.offset(off) = val as u8 },
                        2 | 130 => unsafe { *(base.offset(off * 2) as *mut u16) = val as u16 },
                        4 | 132 => unsafe { *(base.offset(off * 4) as *mut u32) = val as u32 },
                        0x44 => unsafe { *(base.offset(off * 4) as *mut u32) = val as u32 },
                        8 => unsafe { *(base.offset(off * 8) as *mut u64) = val },
                        0 => {
                            // dynamic: elem_bytes in b+1 register
                            let elem_bytes = stack_get!(fiber.stack, bp + inst.b as usize + 1) as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                unsafe { *ptr = stack_get!(fiber.stack, src + i) };
                            }
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                unsafe { *ptr = stack_get!(fiber.stack, src + i) };
                            }
                        }
                    }
                    ExecResult::Continue
                }

                // Slice operations
                Opcode::SliceNew => {
                    exec::exec_slice_new(fiber, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::SliceGet => {
                    // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
                    let s = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get!(fiber.stack, bp + inst.c as usize) as usize;
                    let arr = slice_array!(s);
                    let start = slice_start!(s);
                    let dst = bp + inst.a as usize;
                    let off = (start + idx) as isize;
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
                            let elem_bytes = stack_get!(fiber.stack, bp + inst.c as usize + 1) as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add((start + idx) * elem_bytes + i * 8) as *const u64 };
                                stack_set!(fiber.stack, dst + i, unsafe { *ptr });
                            }
                            return ExecResult::Continue;
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(off as usize * elem_bytes + i * 8) as *const u64 };
                                stack_set!(fiber.stack, dst + i, unsafe { *ptr });
                            }
                            return ExecResult::Continue;
                        }
                    };
                    stack_set!(fiber.stack, dst, val);
                    ExecResult::Continue
                }
                Opcode::SliceSet => {
                    // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
                    let s = stack_get!(fiber.stack, bp + inst.a as usize) as GcRef;
                    let idx = stack_get!(fiber.stack, bp + inst.b as usize) as usize;
                    let arr = slice_array!(s);
                    let start = slice_start!(s);
                    let src = bp + inst.c as usize;
                    let off = (start + idx) as isize;
                    let base = array::data_ptr_bytes(arr);
                    let val = stack_get!(fiber.stack, src);
                    match inst.flags {
                        1 | 129 => unsafe { *base.offset(off) = val as u8 },
                        2 | 130 => unsafe { *(base.offset(off * 2) as *mut u16) = val as u16 },
                        4 | 132 => unsafe { *(base.offset(off * 4) as *mut u32) = val as u32 },
                        0x44 => unsafe { *(base.offset(off * 4) as *mut u32) = val as u32 },
                        8 => unsafe { *(base.offset(off * 8) as *mut u64) = val },
                        0 => {
                            // dynamic: elem_bytes in b+1 register
                            let elem_bytes = stack_get!(fiber.stack, bp + inst.b as usize + 1) as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add((start + idx) * elem_bytes + i * 8) as *mut u64 };
                                unsafe { *ptr = stack_get!(fiber.stack, src + i) };
                            }
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(off as usize * elem_bytes + i * 8) as *mut u64 };
                                unsafe { *ptr = stack_get!(fiber.stack, src + i) };
                            }
                        }
                    }
                    ExecResult::Continue
                }
                Opcode::SliceLen => {
                    let s = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
                    let len = if s.is_null() { 0 } else { slice_len!(s) };
                    stack_set!(fiber.stack, bp + inst.a as usize, len as u64);
                    ExecResult::Continue
                }
                Opcode::SliceCap => {
                    let s = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
                    let cap = if s.is_null() { 0 } else { slice_cap!(s) };
                    stack_set!(fiber.stack, bp + inst.a as usize, cap as u64);
                    ExecResult::Continue
                }
                Opcode::SliceSlice => {
                    exec::exec_slice_slice(fiber, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::SliceAppend => {
                    exec::exec_slice_append(fiber, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }

                // Map operations
                Opcode::MapNew => {
                    exec::exec_map_new(fiber, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::MapGet => {
                    exec::exec_map_get(fiber, &inst);
                    ExecResult::Continue
                }
                Opcode::MapSet => {
                    exec::exec_map_set(fiber, &inst);
                    ExecResult::Continue
                }
                Opcode::MapDelete => {
                    exec::exec_map_delete(fiber, &inst);
                    ExecResult::Continue
                }
                Opcode::MapLen => {
                    exec::exec_map_len(fiber, &inst);
                    ExecResult::Continue
                }
                Opcode::MapIterGet => {
                    exec::exec_map_iter_get(fiber, &inst);
                    ExecResult::Continue
                }

                // Channel operations - need scheduler access
                Opcode::ChanNew => {
                    exec::exec_chan_new(fiber, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::ChanSend => {
                    match exec::exec_chan_send(fiber, &inst) {
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
                    match exec::exec_chan_recv(fiber, &inst) {
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
                    match exec::exec_chan_close(fiber, &inst) {
                        exec::ChanResult::WakeMultiple(ids) => {
                            for id in ids { self.scheduler.wake(id); }
                        }
                        _ => {}
                    }
                    ExecResult::Continue
                }

                // Select operations
                Opcode::SelectBegin => {
                    exec::exec_select_begin(fiber, &inst);
                    ExecResult::Continue
                }
                Opcode::SelectSend => {
                    exec::exec_select_send(fiber, &inst);
                    ExecResult::Continue
                }
                Opcode::SelectRecv => {
                    exec::exec_select_recv(fiber, &inst);
                    ExecResult::Continue
                }
                Opcode::SelectExec => {
                    exec::exec_select_exec(fiber, &inst)
                }

                // Closure operations
                Opcode::ClosureNew => {
                    exec::exec_closure_new(fiber, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::ClosureGet => {
                    exec::exec_closure_get(fiber, &inst);
                    ExecResult::Continue
                }

                // Goroutine - needs scheduler
                // NOTE: spawn may reallocate fibers vec, but loop re-acquires fiber each iteration
                Opcode::GoStart => {
                    // Must drop fiber borrow before accessing scheduler.fibers.len()
                    let next_id = self.scheduler.fibers.len() as u32;
                    let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                    let go_result = exec::exec_go_start(fiber, &inst, &module.functions, next_id);
                    self.scheduler.spawn(go_result.new_fiber);
                    ExecResult::Continue
                }

                // Defer and error handling
                Opcode::DeferPush => {
                    exec::exec_defer_push(fiber, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::ErrDeferPush => {
                    exec::exec_err_defer_push(fiber, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::Panic => {
                    exec::exec_panic(fiber, &inst)
                }
                Opcode::Recover => {
                    exec::exec_recover(fiber, &inst);
                    ExecResult::Continue
                }

                // Interface operations
                Opcode::IfaceAssign => {
                    exec::exec_iface_assign(fiber, &inst, &mut self.state.gc, &mut self.state.itab_cache, module);
                    ExecResult::Continue
                }
                Opcode::IfaceAssert => {
                    exec::exec_iface_assert(fiber, &inst, &mut self.state.itab_cache, module)
                }

                // Type conversion - inline
                Opcode::ConvI2F => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
                    stack_set!(fiber.stack, bp + inst.a as usize, (a as f64).to_bits());
                    ExecResult::Continue
                }
                Opcode::ConvF2I => {
                    let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
                    stack_set!(fiber.stack, bp + inst.a as usize, a as i64 as u64);
                    ExecResult::Continue
                }
                Opcode::ConvI32I64 => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as i32;
                    stack_set!(fiber.stack, bp + inst.a as usize, a as i64 as u64);
                    ExecResult::Continue
                }
                Opcode::ConvI64I32 => {
                    let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
                    stack_set!(fiber.stack, bp + inst.a as usize, a as i32 as u64);
                    ExecResult::Continue
                }
                Opcode::ConvF64F32 => {
                    let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
                    stack_set!(fiber.stack, bp + inst.a as usize, (a as f32).to_bits() as u64);
                    ExecResult::Continue
                }
                Opcode::ConvF32F64 => {
                    let a = f32::from_bits(stack_get!(fiber.stack, bp + inst.b as usize) as u32);
                    stack_set!(fiber.stack, bp + inst.a as usize, (a as f64).to_bits());
                    ExecResult::Continue
                }

                Opcode::Invalid => ExecResult::Panic,
            };

            match result {
                ExecResult::Continue => continue,
                ExecResult::Return => {
                    if self.scheduler.fibers[fiber_id as usize].frames.is_empty() {
                        return ExecResult::Done;
                    }
                }
                other => return other,
            }
        }

        ExecResult::Continue
    }


}

/// Inline instruction execution for vm_call_trampoline (JIT->VM calls).
/// This is a standalone function to avoid code duplication with run_fiber.
#[cfg(feature = "jit")]
fn exec_inst_inline(
    fiber: &mut Fiber,
    inst: &Instruction,
    func_id: u32,
    bp: usize,
    module: &Module,
    state: &mut VmState,
) -> ExecResult {
    match inst.opcode() {
        Opcode::Nop => ExecResult::Continue,
        Opcode::LoadInt => {
            let val = inst.imm32() as i64 as u64;
            stack_set!(fiber.stack, bp + inst.a as usize, val);
            ExecResult::Continue
        }
        Opcode::LoadConst => {
            exec::exec_load_const(fiber, inst, &module.constants);
            ExecResult::Continue
        }
        Opcode::Copy => {
            let val = stack_get!(fiber.stack, bp + inst.b as usize);
            stack_set!(fiber.stack, bp + inst.a as usize, val);
            ExecResult::Continue
        }
        Opcode::CopyN => { exec::exec_copy_n(fiber, inst); ExecResult::Continue }
        Opcode::SlotGet => { exec::exec_slot_get(fiber, inst); ExecResult::Continue }
        Opcode::SlotSet => { exec::exec_slot_set(fiber, inst); ExecResult::Continue }
        Opcode::SlotGetN => { exec::exec_slot_get_n(fiber, inst); ExecResult::Continue }
        Opcode::SlotSetN => { exec::exec_slot_set_n(fiber, inst); ExecResult::Continue }
        Opcode::GlobalGet => { exec::exec_global_get(fiber, inst, &state.globals); ExecResult::Continue }
        Opcode::GlobalGetN => { exec::exec_global_get_n(fiber, inst, &state.globals); ExecResult::Continue }
        Opcode::GlobalSet => { exec::exec_global_set(fiber, inst, &mut state.globals); ExecResult::Continue }
        Opcode::GlobalSetN => { exec::exec_global_set_n(fiber, inst, &mut state.globals); ExecResult::Continue }
        Opcode::PtrNew => { exec::exec_ptr_new(fiber, inst, &mut state.gc); ExecResult::Continue }
        Opcode::PtrGet => { exec::exec_ptr_get(fiber, inst); ExecResult::Continue }
        Opcode::PtrSet => { exec::exec_ptr_set(fiber, inst); ExecResult::Continue }
        Opcode::PtrGetN => { exec::exec_ptr_get_n(fiber, inst); ExecResult::Continue }
        Opcode::PtrSetN => { exec::exec_ptr_set_n(fiber, inst); ExecResult::Continue }
        // Integer arithmetic
        Opcode::AddI => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
            stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_add(b) as u64);
            ExecResult::Continue
        }
        Opcode::SubI => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
            stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_sub(b) as u64);
            ExecResult::Continue
        }
        Opcode::MulI => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
            stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_mul(b) as u64);
            ExecResult::Continue
        }
        Opcode::DivI => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
            stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_div(b) as u64);
            ExecResult::Continue
        }
        Opcode::ModI => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
            stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_rem(b) as u64);
            ExecResult::Continue
        }
        Opcode::NegI => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
            stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_neg() as u64);
            ExecResult::Continue
        }
        // Float arithmetic
        Opcode::AddF => {
            let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
            let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
            stack_set!(fiber.stack, bp + inst.a as usize, (a + b).to_bits());
            ExecResult::Continue
        }
        Opcode::SubF => {
            let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
            let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
            stack_set!(fiber.stack, bp + inst.a as usize, (a - b).to_bits());
            ExecResult::Continue
        }
        Opcode::MulF => {
            let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
            let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
            stack_set!(fiber.stack, bp + inst.a as usize, (a * b).to_bits());
            ExecResult::Continue
        }
        Opcode::DivF => {
            let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
            let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
            stack_set!(fiber.stack, bp + inst.a as usize, (a / b).to_bits());
            ExecResult::Continue
        }
        Opcode::NegF => {
            let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
            stack_set!(fiber.stack, bp + inst.a as usize, (-a).to_bits());
            ExecResult::Continue
        }
        // Integer comparison
        Opcode::EqI => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize);
            let b = stack_get!(fiber.stack, bp + inst.c as usize);
            stack_set!(fiber.stack, bp + inst.a as usize, (a == b) as u64);
            ExecResult::Continue
        }
        Opcode::NeI => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize);
            let b = stack_get!(fiber.stack, bp + inst.c as usize);
            stack_set!(fiber.stack, bp + inst.a as usize, (a != b) as u64);
            ExecResult::Continue
        }
        Opcode::LtI => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
            stack_set!(fiber.stack, bp + inst.a as usize, (a < b) as u64);
            ExecResult::Continue
        }
        Opcode::LeI => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
            stack_set!(fiber.stack, bp + inst.a as usize, (a <= b) as u64);
            ExecResult::Continue
        }
        Opcode::GtI => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
            stack_set!(fiber.stack, bp + inst.a as usize, (a > b) as u64);
            ExecResult::Continue
        }
        Opcode::GeI => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as i64;
            stack_set!(fiber.stack, bp + inst.a as usize, (a >= b) as u64);
            ExecResult::Continue
        }
        // Float comparison
        Opcode::EqF => {
            let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
            let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
            stack_set!(fiber.stack, bp + inst.a as usize, (a == b) as u64);
            ExecResult::Continue
        }
        Opcode::NeF => {
            let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
            let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
            stack_set!(fiber.stack, bp + inst.a as usize, (a != b) as u64);
            ExecResult::Continue
        }
        Opcode::LtF => {
            let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
            let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
            stack_set!(fiber.stack, bp + inst.a as usize, (a < b) as u64);
            ExecResult::Continue
        }
        Opcode::LeF => {
            let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
            let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
            stack_set!(fiber.stack, bp + inst.a as usize, (a <= b) as u64);
            ExecResult::Continue
        }
        Opcode::GtF => {
            let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
            let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
            stack_set!(fiber.stack, bp + inst.a as usize, (a > b) as u64);
            ExecResult::Continue
        }
        Opcode::GeF => {
            let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
            let b = f64::from_bits(stack_get!(fiber.stack, bp + inst.c as usize));
            stack_set!(fiber.stack, bp + inst.a as usize, (a >= b) as u64);
            ExecResult::Continue
        }
        // Bitwise
        Opcode::And => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize);
            let b = stack_get!(fiber.stack, bp + inst.c as usize);
            stack_set!(fiber.stack, bp + inst.a as usize, a & b);
            ExecResult::Continue
        }
        Opcode::Or => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize);
            let b = stack_get!(fiber.stack, bp + inst.c as usize);
            stack_set!(fiber.stack, bp + inst.a as usize, a | b);
            ExecResult::Continue
        }
        Opcode::Xor => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize);
            let b = stack_get!(fiber.stack, bp + inst.c as usize);
            stack_set!(fiber.stack, bp + inst.a as usize, a ^ b);
            ExecResult::Continue
        }
        Opcode::AndNot => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize);
            let b = stack_get!(fiber.stack, bp + inst.c as usize);
            stack_set!(fiber.stack, bp + inst.a as usize, a & !b);
            ExecResult::Continue
        }
        Opcode::Not => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize);
            stack_set!(fiber.stack, bp + inst.a as usize, !a);
            ExecResult::Continue
        }
        Opcode::Shl => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize);
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as u32;
            stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_shl(b));
            ExecResult::Continue
        }
        Opcode::ShrS => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as u32;
            stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_shr(b) as u64);
            ExecResult::Continue
        }
        Opcode::ShrU => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize);
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as u32;
            stack_set!(fiber.stack, bp + inst.a as usize, a.wrapping_shr(b));
            ExecResult::Continue
        }
        Opcode::BoolNot => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize);
            stack_set!(fiber.stack, bp + inst.a as usize, (a == 0) as u64);
            ExecResult::Continue
        }
        // Jump
        Opcode::Jump => {
            let offset = inst.imm32();
            let frame = fiber.current_frame_mut().unwrap();
            frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
            ExecResult::Continue
        }
        Opcode::JumpIf => {
            let cond = stack_get!(fiber.stack, bp + inst.a as usize);
            if cond != 0 {
                let offset = inst.imm32();
                let frame = fiber.current_frame_mut().unwrap();
                frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
            }
            ExecResult::Continue
        }
        Opcode::JumpIfNot => {
            let cond = stack_get!(fiber.stack, bp + inst.a as usize);
            if cond == 0 {
                let offset = inst.imm32();
                let frame = fiber.current_frame_mut().unwrap();
                frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
            }
            ExecResult::Continue
        }
        // Call
        Opcode::Call => exec::exec_call(fiber, inst, module),
        Opcode::CallExtern => exec::exec_call_extern(fiber, inst, &module.externs, &state.extern_registry, &mut state.gc),
        Opcode::CallClosure => exec::exec_call_closure(fiber, inst, module),
        Opcode::CallIface => exec::exec_call_iface(fiber, inst, module, &state.itab_cache),
        Opcode::Return => {
            let func = &module.functions[func_id as usize];
            let is_error_return = (inst.flags & 1) != 0;
            exec::exec_return(fiber, inst, func, module, is_error_return)
        }
        // String
        Opcode::StrNew => { exec::exec_str_new(fiber, inst, &module.constants, &mut state.gc); ExecResult::Continue }
        Opcode::StrLen => {
            let s = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
            let len = if s.is_null() { 0 } else { string_len!(s) };
            stack_set!(fiber.stack, bp + inst.a as usize, len as u64);
            ExecResult::Continue
        }
        Opcode::StrIndex => {
            let s = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
            let idx = stack_get!(fiber.stack, bp + inst.c as usize) as usize;
            let byte = string_index!(s, idx);
            stack_set!(fiber.stack, bp + inst.a as usize, byte as u64);
            ExecResult::Continue
        }
        Opcode::StrConcat => { exec::exec_str_concat(fiber, inst, &mut state.gc); ExecResult::Continue }
        Opcode::StrSlice => { exec::exec_str_slice(fiber, inst, &mut state.gc); ExecResult::Continue }
        Opcode::StrEq => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as GcRef;
            stack_set!(fiber.stack, bp + inst.a as usize, string::eq(a, b) as u64);
            ExecResult::Continue
        }
        Opcode::StrNe => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as GcRef;
            stack_set!(fiber.stack, bp + inst.a as usize, string::ne(a, b) as u64);
            ExecResult::Continue
        }
        Opcode::StrLt => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as GcRef;
            stack_set!(fiber.stack, bp + inst.a as usize, string::lt(a, b) as u64);
            ExecResult::Continue
        }
        Opcode::StrLe => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as GcRef;
            stack_set!(fiber.stack, bp + inst.a as usize, string::le(a, b) as u64);
            ExecResult::Continue
        }
        Opcode::StrGt => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as GcRef;
            stack_set!(fiber.stack, bp + inst.a as usize, string::gt(a, b) as u64);
            ExecResult::Continue
        }
        Opcode::StrGe => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
            let b = stack_get!(fiber.stack, bp + inst.c as usize) as GcRef;
            stack_set!(fiber.stack, bp + inst.a as usize, string::ge(a, b) as u64);
            ExecResult::Continue
        }
        Opcode::StrDecodeRune => {
            let s = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
            let pos = stack_get!(fiber.stack, bp + inst.c as usize) as usize;
            let (rune, width) = string::decode_rune_at(s, pos);
            stack_set!(fiber.stack, bp + inst.a as usize, rune as u64);
            stack_set!(fiber.stack, bp + inst.a as usize + 1, width as u64);
            ExecResult::Continue
        }
        // Array
        Opcode::ArrayNew => { exec::exec_array_new(fiber, inst, &mut state.gc); ExecResult::Continue }
        Opcode::ArrayGet => {
            // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
            let arr = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
            let idx = stack_get!(fiber.stack, bp + inst.c as usize) as usize;
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
                    let elem_bytes = stack_get!(fiber.stack, bp + inst.c as usize + 1) as usize;
                    for i in 0..(elem_bytes + 7) / 8 {
                        let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                        stack_set!(fiber.stack, dst + i, unsafe { *ptr });
                    }
                    return ExecResult::Continue;
                }
                _ => {
                    let elem_bytes = inst.flags as usize;
                    for i in 0..(elem_bytes + 7) / 8 {
                        let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                        stack_set!(fiber.stack, dst + i, unsafe { *ptr });
                    }
                    return ExecResult::Continue;
                }
            };
            stack_set!(fiber.stack, dst, val);
            ExecResult::Continue
        }
        Opcode::ArraySet => {
            // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
            let arr = stack_get!(fiber.stack, bp + inst.a as usize) as GcRef;
            let idx = stack_get!(fiber.stack, bp + inst.b as usize) as usize;
            let src = bp + inst.c as usize;
            let off = idx as isize;
            let base = array::data_ptr_bytes(arr);
            let val = stack_get!(fiber.stack, src);
            match inst.flags {
                1 | 129 => unsafe { *base.offset(off) = val as u8 },
                2 | 130 => unsafe { *(base.offset(off * 2) as *mut u16) = val as u16 },
                4 | 132 => unsafe { *(base.offset(off * 4) as *mut u32) = val as u32 },
                0x44 => unsafe { *(base.offset(off * 4) as *mut u32) = val as u32 },
                8 => unsafe { *(base.offset(off * 8) as *mut u64) = val },
                0 => {
                    // dynamic: elem_bytes in b+1 register
                    let elem_bytes = stack_get!(fiber.stack, bp + inst.b as usize + 1) as usize;
                    for i in 0..(elem_bytes + 7) / 8 {
                        let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                        unsafe { *ptr = stack_get!(fiber.stack, src + i) };
                    }
                }
                _ => {
                    let elem_bytes = inst.flags as usize;
                    for i in 0..(elem_bytes + 7) / 8 {
                        let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                        unsafe { *ptr = stack_get!(fiber.stack, src + i) };
                    }
                }
            }
            ExecResult::Continue
        }
        // Slice
        Opcode::SliceNew => { exec::exec_slice_new(fiber, inst, &mut state.gc); ExecResult::Continue }
        Opcode::SliceGet => {
            // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
            let s = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
            let idx = stack_get!(fiber.stack, bp + inst.c as usize) as usize;
            let arr = slice_array!(s);
            let start = slice_start!(s);
            let dst = bp + inst.a as usize;
            let off = (start + idx) as isize;
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
                    let elem_bytes = stack_get!(fiber.stack, bp + inst.c as usize + 1) as usize;
                    for i in 0..(elem_bytes + 7) / 8 {
                        let ptr = unsafe { base.add((start + idx) * elem_bytes + i * 8) as *const u64 };
                        stack_set!(fiber.stack, dst + i, unsafe { *ptr });
                    }
                    return ExecResult::Continue;
                }
                _ => {
                    let elem_bytes = inst.flags as usize;
                    for i in 0..(elem_bytes + 7) / 8 {
                        let ptr = unsafe { base.add(off as usize * elem_bytes + i * 8) as *const u64 };
                        stack_set!(fiber.stack, dst + i, unsafe { *ptr });
                    }
                    return ExecResult::Continue;
                }
            };
            stack_set!(fiber.stack, dst, val);
            ExecResult::Continue
        }
        Opcode::SliceSet => {
            // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
            let s = stack_get!(fiber.stack, bp + inst.a as usize) as GcRef;
            let idx = stack_get!(fiber.stack, bp + inst.b as usize) as usize;
            let arr = slice_array!(s);
            let start = slice_start!(s);
            let src = bp + inst.c as usize;
            let off = (start + idx) as isize;
            let base = array::data_ptr_bytes(arr);
            let val = stack_get!(fiber.stack, src);
            match inst.flags {
                1 | 129 => unsafe { *base.offset(off) = val as u8 },
                2 | 130 => unsafe { *(base.offset(off * 2) as *mut u16) = val as u16 },
                4 | 132 => unsafe { *(base.offset(off * 4) as *mut u32) = val as u32 },
                0x44 => unsafe { *(base.offset(off * 4) as *mut u32) = val as u32 },
                8 => unsafe { *(base.offset(off * 8) as *mut u64) = val },
                0 => {
                    // dynamic: elem_bytes in b+1 register
                    let elem_bytes = stack_get!(fiber.stack, bp + inst.b as usize + 1) as usize;
                    for i in 0..(elem_bytes + 7) / 8 {
                        let ptr = unsafe { base.add((start + idx) * elem_bytes + i * 8) as *mut u64 };
                        unsafe { *ptr = stack_get!(fiber.stack, src + i) };
                    }
                }
                _ => {
                    let elem_bytes = inst.flags as usize;
                    for i in 0..(elem_bytes + 7) / 8 {
                        let ptr = unsafe { base.add(off as usize * elem_bytes + i * 8) as *mut u64 };
                        unsafe { *ptr = stack_get!(fiber.stack, src + i) };
                    }
                }
            }
            ExecResult::Continue
        }
        Opcode::SliceLen => {
            let s = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
            let len = if s.is_null() { 0 } else { slice_len!(s) };
            stack_set!(fiber.stack, bp + inst.a as usize, len as u64);
            ExecResult::Continue
        }
        Opcode::SliceCap => {
            let s = stack_get!(fiber.stack, bp + inst.b as usize) as GcRef;
            let cap = if s.is_null() { 0 } else { slice_cap!(s) };
            stack_set!(fiber.stack, bp + inst.a as usize, cap as u64);
            ExecResult::Continue
        }
        Opcode::SliceSlice => { exec::exec_slice_slice(fiber, inst, &mut state.gc); ExecResult::Continue }
        Opcode::SliceAppend => { exec::exec_slice_append(fiber, inst, &mut state.gc); ExecResult::Continue }
        // Map
        Opcode::MapNew => { exec::exec_map_new(fiber, inst, &mut state.gc); ExecResult::Continue }
        Opcode::MapGet => { exec::exec_map_get(fiber, inst); ExecResult::Continue }
        Opcode::MapSet => { exec::exec_map_set(fiber, inst); ExecResult::Continue }
        Opcode::MapDelete => { exec::exec_map_delete(fiber, inst); ExecResult::Continue }
        Opcode::MapLen => { exec::exec_map_len(fiber, inst); ExecResult::Continue }
        Opcode::MapIterGet => { exec::exec_map_iter_get(fiber, inst); ExecResult::Continue }
        // Channel - not supported in JIT trampoline
        Opcode::ChanNew => { exec::exec_chan_new(fiber, inst, &mut state.gc); ExecResult::Continue }
        Opcode::ChanSend | Opcode::ChanRecv | Opcode::ChanClose => ExecResult::Panic,
        // Select - not supported in JIT trampoline
        Opcode::SelectBegin | Opcode::SelectSend | Opcode::SelectRecv | Opcode::SelectExec => ExecResult::Panic,
        // Closure
        Opcode::ClosureNew => { exec::exec_closure_new(fiber, inst, &mut state.gc); ExecResult::Continue }
        Opcode::ClosureGet => { exec::exec_closure_get(fiber, inst); ExecResult::Continue }
        // GoStart - not supported in JIT trampoline
        Opcode::GoStart => ExecResult::Panic,
        // Defer
        Opcode::DeferPush => { exec::exec_defer_push(fiber, inst, &mut state.gc); ExecResult::Continue }
        Opcode::ErrDeferPush => { exec::exec_err_defer_push(fiber, inst, &mut state.gc); ExecResult::Continue }
        Opcode::Panic => exec::exec_panic(fiber, inst),
        Opcode::Recover => { exec::exec_recover(fiber, inst); ExecResult::Continue }
        // Interface
        Opcode::IfaceAssign => { exec::exec_iface_assign(fiber, inst, &mut state.gc, &mut state.itab_cache, module); ExecResult::Continue }
        Opcode::IfaceAssert => exec::exec_iface_assert(fiber, inst, &mut state.itab_cache, module),
        // Conversion
        Opcode::ConvI2F => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
            stack_set!(fiber.stack, bp + inst.a as usize, (a as f64).to_bits());
            ExecResult::Continue
        }
        Opcode::ConvF2I => {
            let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
            stack_set!(fiber.stack, bp + inst.a as usize, a as i64 as u64);
            ExecResult::Continue
        }
        Opcode::ConvI32I64 => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as i32;
            stack_set!(fiber.stack, bp + inst.a as usize, a as i64 as u64);
            ExecResult::Continue
        }
        Opcode::ConvI64I32 => {
            let a = stack_get!(fiber.stack, bp + inst.b as usize) as i64;
            stack_set!(fiber.stack, bp + inst.a as usize, a as i32 as u64);
            ExecResult::Continue
        }
        Opcode::ConvF64F32 => {
            let a = f64::from_bits(stack_get!(fiber.stack, bp + inst.b as usize));
            stack_set!(fiber.stack, bp + inst.a as usize, (a as f32).to_bits() as u64);
            ExecResult::Continue
        }
        Opcode::ConvF32F64 => {
            let a = f32::from_bits(stack_get!(fiber.stack, bp + inst.b as usize) as u32);
            stack_set!(fiber.stack, bp + inst.a as usize, (a as f64).to_bits());
            ExecResult::Continue
        }
        Opcode::Invalid => ExecResult::Panic,
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}
