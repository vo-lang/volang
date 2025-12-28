//! Virtual machine main structure.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::Gc;

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
    
    // Create a temporary fiber for the call (id 0 is unused for temp fiber)
    let mut temp_fiber = Fiber::new(0);
    temp_fiber.stack.resize(func.local_slots as usize, 0);
    
    // Copy args to fiber stack
    for i in 0..arg_count as usize {
        let val = unsafe { *args.add(i) };
        temp_fiber.stack[i] = val;
    }
    
    // Push initial frame
    temp_fiber.frames.push(crate::fiber::CallFrame {
        func_id,
        pc: 0,
        bp: 0,
        ret_reg: 0,
        ret_count: ret_count as u16,
    });
    
    // Storage for return values (saved before pop_frame truncates stack)
    let mut ret_vals: Vec<u64> = Vec::new();
    
    // Execute until done
    loop {
        // Get frame info without holding mutable borrow
        let (frame_func_id, inst) = {
            let frame = match temp_fiber.current_frame_mut() {
                Some(f) => f,
                None => break,
            };
            let func = &module.functions[frame.func_id as usize];
            if frame.pc >= func.code.len() {
                break;
            }
            let inst = func.code[frame.pc];
            frame.pc += 1;
            (frame.func_id, inst)
        };
        
        // Special handling for Return: save return values BEFORE exec_inst
        // because exec_return will pop_frame and truncate the stack
        if inst.opcode() == crate::instruction::Opcode::Return {
            // Only save if this is the last frame (returning to JIT)
            if temp_fiber.frames.len() == 1 {
                let ret_start = inst.a as usize;
                let ret_count_inst = inst.b as usize;
                ret_vals = (0..ret_count_inst)
                    .map(|i| temp_fiber.read_reg((ret_start + i) as u16))
                    .collect();
            }
        }
        
        let result = Vm::exec_inst(&mut temp_fiber, &inst, frame_func_id, module, &mut vm.state);
        match result {
            ExecResult::Continue => continue,
            ExecResult::Return => {
                if temp_fiber.frames.is_empty() {
                    break;
                }
            }
            ExecResult::Panic => return JitResult::Panic,
            ExecResult::Done => break,
            _ => {}
        }
    }
    
    // Copy return values
    for i in 0..ret_count as usize {
        if i < ret_vals.len() {
            unsafe { *ret.add(i) = ret_vals[i] };
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
        // Use raw pointer to avoid borrow conflicts with try_jit_call
        let module_ptr = match &self.module {
            Some(m) => m as *const Module,
            None => return ExecResult::Done,
        };
        // SAFETY: module_ptr is valid for the duration of run_fiber because
        // self.module is not modified during execution.
        let module = unsafe { &*module_ptr };

        for _ in 0..TIME_SLICE {
            // Fetch instruction
            let (inst, func_id) = {
                let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                let frame = match fiber.current_frame_mut() {
                    Some(f) => f,
                    None => return ExecResult::Done,
                };
                let func = &module.functions[frame.func_id as usize];
                if frame.pc >= func.code.len() {
                    return ExecResult::Done;
                }
                let inst = func.code[frame.pc];
                frame.pc += 1;
                (inst, frame.func_id)
            };

            // Handle special ops that need scheduler access
            let op = inst.opcode();
            
            // Helper macro for channel ops with blocking support
            macro_rules! handle_chan_op {
                ($exec_fn:expr) => {{
                    let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                    match $exec_fn(fiber, &inst) {
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
                }};
            }
            
            let result = match op {
                Opcode::ChanSend => handle_chan_op!(exec::exec_chan_send),
                Opcode::ChanRecv => handle_chan_op!(exec::exec_chan_recv),
                Opcode::ChanClose => {
                    let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                    if let exec::ChanResult::WakeMultiple(ids) = exec::exec_chan_close(fiber, &inst) {
                        for id in ids { self.scheduler.wake(id); }
                    }
                    ExecResult::Continue
                }
                Opcode::GoStart => {
                    let functions = &module.functions;
                    let next_id = self.scheduler.fibers.len() as u32;
                    let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                    let go_result = exec::exec_go_start(fiber, &inst, functions, next_id);
                    self.scheduler.spawn(go_result.new_fiber);
                    ExecResult::Continue
                }
                #[cfg(feature = "jit")]
                Opcode::Call => {
                    let target_func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
                    let arg_start = inst.b;
                    let arg_slots = (inst.c >> 8) as usize;
                    let ret_slots = (inst.c & 0xFF) as u16;
                    
                    // Check if JIT is available for this function
                    let has_jit_func = self.jit.as_ref()
                        .and_then(|jit| unsafe { jit.get_func_ptr(target_func_id) })
                        .is_some();
                    
                    if has_jit_func {
                        // JIT path
                        self.try_jit_call(fiber_id, target_func_id, arg_start, arg_slots, ret_slots)
                            .unwrap_or(ExecResult::Continue)
                    } else {
                        // Record call for hot function detection
                        if self.hot_counter.record_call(target_func_id) {
                            // Function just became hot - try to compile it
                            if let Some(jit) = &mut self.jit {
                                let func = &module.functions[target_func_id as usize];
                                if jit.can_jit(func, module) {
                                    // Ignore compilation errors - fall back to interpreter
                                    let _ = jit.compile(target_func_id, func, module);
                                }
                            }
                        }
                        // Interpreter path
                        let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                        exec::exec_call(fiber, &inst, module)
                    }
                }
                _ => {
                    let fiber = &mut self.scheduler.fibers[fiber_id as usize];
                    Self::exec_inst(fiber, &inst, func_id, module, &mut self.state)
                }
            };

            match result {
                ExecResult::Continue => continue,
                ExecResult::Return => {
                    let fiber = &self.scheduler.fibers[fiber_id as usize];
                    if fiber.frames.is_empty() {
                        return ExecResult::Done;
                    }
                }
                other => return other,
            }
        }

        // Time slice exhausted
        ExecResult::Continue
    }


    /// Execute a single instruction.
    fn exec_inst(
        fiber: &mut Fiber,
        inst: &Instruction,
        func_id: u32,
        module: &Module,
        state: &mut VmState,
    ) -> ExecResult {
        let op = inst.opcode();

        match op {
            Opcode::Nop => ExecResult::Continue,

            Opcode::LoadInt => {
                exec::exec_load_int(fiber, inst);
                ExecResult::Continue
            }
            Opcode::LoadConst => {
                exec::exec_load_const(fiber, inst, &module.constants);
                ExecResult::Continue
            }

            Opcode::Copy => {
                exec::exec_copy(fiber, inst);
                ExecResult::Continue
            }
            Opcode::CopyN => {
                exec::exec_copy_n(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SlotGet => {
                exec::exec_slot_get(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SlotSet => {
                exec::exec_slot_set(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SlotGetN => {
                exec::exec_slot_get_n(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SlotSetN => {
                exec::exec_slot_set_n(fiber, inst);
                ExecResult::Continue
            }

            Opcode::GlobalGet => {
                exec::exec_global_get(fiber, inst, &state.globals);
                ExecResult::Continue
            }
            Opcode::GlobalGetN => {
                exec::exec_global_get_n(fiber, inst, &state.globals);
                ExecResult::Continue
            }
            Opcode::GlobalSet => {
                exec::exec_global_set(fiber, inst, &mut state.globals);
                ExecResult::Continue
            }
            Opcode::GlobalSetN => {
                exec::exec_global_set_n(fiber, inst, &mut state.globals);
                ExecResult::Continue
            }

            Opcode::PtrNew => {
                exec::exec_ptr_new(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::PtrGet => {
                exec::exec_ptr_get(fiber, inst);
                ExecResult::Continue
            }
            Opcode::PtrSet => {
                exec::exec_ptr_set(fiber, inst);
                ExecResult::Continue
            }
            Opcode::PtrGetN => {
                exec::exec_ptr_get_n(fiber, inst);
                ExecResult::Continue
            }
            Opcode::PtrSetN => {
                exec::exec_ptr_set_n(fiber, inst);
                ExecResult::Continue
            }

            Opcode::AddI => {
                exec::exec_add_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SubI => {
                exec::exec_sub_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::MulI => {
                exec::exec_mul_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::DivI => {
                exec::exec_div_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ModI => {
                exec::exec_mod_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::NegI => {
                exec::exec_neg_i(fiber, inst);
                ExecResult::Continue
            }

            Opcode::AddF => {
                exec::exec_add_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SubF => {
                exec::exec_sub_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::MulF => {
                exec::exec_mul_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::DivF => {
                exec::exec_div_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::NegF => {
                exec::exec_neg_f(fiber, inst);
                ExecResult::Continue
            }

            Opcode::EqI => {
                exec::exec_eq_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::NeI => {
                exec::exec_ne_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::LtI => {
                exec::exec_lt_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::LeI => {
                exec::exec_le_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::GtI => {
                exec::exec_gt_i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::GeI => {
                exec::exec_ge_i(fiber, inst);
                ExecResult::Continue
            }

            Opcode::EqF => {
                exec::exec_eq_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::NeF => {
                exec::exec_ne_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::LtF => {
                exec::exec_lt_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::LeF => {
                exec::exec_le_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::GtF => {
                exec::exec_gt_f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::GeF => {
                exec::exec_ge_f(fiber, inst);
                ExecResult::Continue
            }

            Opcode::And => {
                exec::exec_and(fiber, inst);
                ExecResult::Continue
            }
            Opcode::Or => {
                exec::exec_or(fiber, inst);
                ExecResult::Continue
            }
            Opcode::Xor => {
                exec::exec_xor(fiber, inst);
                ExecResult::Continue
            }
            Opcode::AndNot => {
                exec::exec_and_not(fiber, inst);
                ExecResult::Continue
            }
            Opcode::Not => {
                exec::exec_not(fiber, inst);
                ExecResult::Continue
            }
            Opcode::Shl => {
                exec::exec_shl(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ShrS => {
                exec::exec_shr_s(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ShrU => {
                exec::exec_shr_u(fiber, inst);
                ExecResult::Continue
            }
            Opcode::BoolNot => {
                exec::exec_bool_not(fiber, inst);
                ExecResult::Continue
            }

            Opcode::Jump => {
                exec::exec_jump(fiber, inst);
                ExecResult::Continue
            }
            Opcode::JumpIf => {
                exec::exec_jump_if(fiber, inst);
                ExecResult::Continue
            }
            Opcode::JumpIfNot => {
                exec::exec_jump_if_not(fiber, inst);
                ExecResult::Continue
            }

            Opcode::Call => {
                exec::exec_call(fiber, inst, module)
            }
            Opcode::CallExtern => {
                exec::exec_call_extern(fiber, inst, &module.externs, &state.extern_registry, &mut state.gc)
            }
            Opcode::CallClosure => {
                exec::exec_call_closure(fiber, inst, module)
            }
            Opcode::CallIface => {
                exec::exec_call_iface(fiber, inst, module, &state.itab_cache)
            }
            Opcode::Return => {
                let func = &module.functions[func_id as usize];
                let is_error_return = (inst.flags & 1) != 0;
                exec::exec_return(fiber, inst, func, module, is_error_return)
            }

            Opcode::StrNew => {
                exec::exec_str_new(fiber, inst, &module.constants, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::StrLen => {
                exec::exec_str_len(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrIndex => {
                exec::exec_str_index(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrConcat => {
                exec::exec_str_concat(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::StrSlice => {
                exec::exec_str_slice(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::StrEq => {
                exec::exec_str_eq(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrNe => {
                exec::exec_str_ne(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrLt => {
                exec::exec_str_lt(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrLe => {
                exec::exec_str_le(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrGt => {
                exec::exec_str_gt(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrGe => {
                exec::exec_str_ge(fiber, inst);
                ExecResult::Continue
            }
            Opcode::StrDecodeRune => {
                exec::exec_str_decode_rune(fiber, inst);
                ExecResult::Continue
            }

            Opcode::ArrayNew => {
                exec::exec_array_new(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::ArrayGet => {
                exec::exec_array_get(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ArraySet => {
                exec::exec_array_set(fiber, inst);
                ExecResult::Continue
            }

            Opcode::SliceNew => {
                exec::exec_slice_new(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::SliceGet => {
                exec::exec_slice_get(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SliceSet => {
                exec::exec_slice_set(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SliceLen => {
                exec::exec_slice_len(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SliceCap => {
                exec::exec_slice_cap(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SliceSlice => {
                exec::exec_slice_slice(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::SliceAppend => {
                exec::exec_slice_append(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }

            Opcode::MapNew => {
                exec::exec_map_new(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::MapGet => {
                exec::exec_map_get(fiber, inst);
                ExecResult::Continue
            }
            Opcode::MapSet => {
                exec::exec_map_set(fiber, inst);
                ExecResult::Continue
            }
            Opcode::MapDelete => {
                exec::exec_map_delete(fiber, inst);
                ExecResult::Continue
            }
            Opcode::MapLen => {
                exec::exec_map_len(fiber, inst);
                ExecResult::Continue
            }
            Opcode::MapIterGet => {
                exec::exec_map_iter_get(fiber, inst);
                ExecResult::Continue
            }

            Opcode::ChanNew => {
                exec::exec_chan_new(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::ChanSend => {
                // ChanSend returns ChanResult, convert to ExecResult
                // Wake handling needs scheduler access - return Yield to handle at higher level
                match exec::exec_chan_send(fiber, inst) {
                    exec::ChanResult::Continue => ExecResult::Continue,
                    exec::ChanResult::Yield => ExecResult::Yield,
                    exec::ChanResult::Panic => ExecResult::Panic,
                    exec::ChanResult::Wake(_) | exec::ChanResult::WakeMultiple(_) => ExecResult::Yield,
                }
            }
            Opcode::ChanRecv => {
                match exec::exec_chan_recv(fiber, inst) {
                    exec::ChanResult::Continue => ExecResult::Continue,
                    exec::ChanResult::Yield => ExecResult::Yield,
                    exec::ChanResult::Panic => ExecResult::Panic,
                    exec::ChanResult::Wake(_) | exec::ChanResult::WakeMultiple(_) => ExecResult::Yield,
                }
            }
            Opcode::ChanClose => {
                match exec::exec_chan_close(fiber, inst) {
                    exec::ChanResult::Continue => ExecResult::Continue,
                    exec::ChanResult::Yield => ExecResult::Yield,
                    exec::ChanResult::Panic => ExecResult::Panic,
                    exec::ChanResult::Wake(_) | exec::ChanResult::WakeMultiple(_) => ExecResult::Continue,
                }
            }

            Opcode::SelectBegin => {
                exec::exec_select_begin(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SelectSend => {
                exec::exec_select_send(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SelectRecv => {
                exec::exec_select_recv(fiber, inst);
                ExecResult::Continue
            }
            Opcode::SelectExec => {
                exec::exec_select_exec(fiber, inst)
            }


            Opcode::ClosureNew => {
                exec::exec_closure_new(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::ClosureGet => {
                exec::exec_closure_get(fiber, inst);
                ExecResult::Continue
            }

            Opcode::GoStart => {
                // GoStart needs special handling - returns new fiber to spawn
                // Handled in run_fiber, this path shouldn't be reached
                ExecResult::Yield
            }

            Opcode::DeferPush => {
                exec::exec_defer_push(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::ErrDeferPush => {
                exec::exec_err_defer_push(fiber, inst, &mut state.gc);
                ExecResult::Continue
            }
            Opcode::Panic => {
                exec::exec_panic(fiber, inst)
            }
            Opcode::Recover => {
                exec::exec_recover(fiber, inst);
                ExecResult::Continue
            }

            Opcode::IfaceAssign => {
                exec::exec_iface_assign(fiber, inst, &mut state.gc, &mut state.itab_cache, module);
                ExecResult::Continue
            }
            Opcode::IfaceAssert => {
                exec::exec_iface_assert(fiber, inst, &mut state.itab_cache, module)
            }

            Opcode::ConvI2F => {
                exec::exec_conv_i2f(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ConvF2I => {
                exec::exec_conv_f2i(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ConvI32I64 => {
                exec::exec_conv_i32_i64(fiber, inst);
                ExecResult::Continue
            }
            Opcode::ConvI64I32 => {
                exec::exec_conv_i64_i32(fiber, inst);
                ExecResult::Continue
            }

            Opcode::Invalid => ExecResult::Panic,
        }
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}
