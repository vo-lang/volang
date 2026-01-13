//! Virtual machine main structure.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::GcRef;
use vo_runtime::objects::{array, string};

mod helpers;
mod types;

pub use helpers::{stack_get, stack_set};
pub use types::{ExecResult, VmError, VmState, ErrorLocation, TIME_SLICE};

use helpers::{slice_data_ptr, slice_len, slice_cap, string_len, string_index, runtime_panic, panic_unwind, user_panic,
    ERR_NIL_POINTER, ERR_NIL_MAP_WRITE, ERR_UNHASHABLE_TYPE, ERR_UNCOMPARABLE_TYPE, ERR_NEGATIVE_SHIFT, ERR_NIL_FUNC_CALL};

use crate::bytecode::Module;
use crate::exec;
use crate::fiber::Fiber;
use crate::instruction::{Instruction, Opcode};
use crate::scheduler::Scheduler;
use vo_runtime::itab::ItabCache;

#[cfg(feature = "jit")]
mod jit_glue;

#[cfg(feature = "jit")]
pub mod jit_mgr;

#[cfg(feature = "jit")]
pub use jit_mgr::{JitManager, JitConfig};

pub struct Vm {
    /// JIT manager (only available with "jit" feature).
    /// IMPORTANT: Must be first field so it's dropped LAST (Rust drops in reverse order).
    /// JIT code memory must remain valid while scheduler/fibers are being dropped.
    #[cfg(feature = "jit")]
    pub jit_mgr: Option<JitManager>,
    pub module: Option<Module>,
    pub scheduler: Scheduler,
    pub state: VmState,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            #[cfg(feature = "jit")]
            jit_mgr: None,
            module: None,
            scheduler: Scheduler::new(),
            state: VmState::new(),
        }
    }
    
    /// Create a VM with custom JIT thresholds.
    #[cfg(feature = "jit")]
    pub fn with_jit_thresholds(call_threshold: u32, loop_threshold: u32) -> Self {
        Self::with_jit_config(JitConfig {
            call_threshold,
            loop_threshold,
            ..Default::default()
        })
    }
    
    #[cfg(not(feature = "jit"))]
    pub fn with_jit_thresholds(_call_threshold: u32, _loop_threshold: u32) -> Self {
        Self::new()
    }
    
    /// Create a VM with custom JIT configuration.
    #[cfg(feature = "jit")]
    pub fn with_jit_config(config: JitConfig) -> Self {
        let mut vm = Self::new();
        if let Ok(mgr) = JitManager::with_config(config) {
            vm.jit_mgr = Some(mgr);
        }
        vm
    }
    

    /// Initialize JIT compiler (if jit feature is enabled).
    ///
    /// Call this after creating the VM to enable JIT compilation.
    /// If JIT initialization fails, the VM will continue with interpretation only.
    /// Note: Does nothing if JIT manager already exists (e.g., from with_jit_thresholds).
    #[cfg(feature = "jit")]
    pub fn init_jit(&mut self) {
        if self.jit_mgr.is_some() {
            return; // Already initialized (e.g., by with_jit_thresholds)
        }
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

impl Vm {
    pub fn module(&self) -> Option<&Module> {
        self.module.as_ref()
    }

    pub fn load(&mut self, module: Module) {
        // Register extern functions from module
        for (id, def) in module.externs.iter().enumerate() {
            if let Some(func) = vo_runtime::lookup_extern(&def.name) {
                self.state.extern_registry.register(id as u32, func);
            } else if let Some(func) = vo_runtime::lookup_extern_with_context(&def.name) {
                self.state.extern_registry.register_with_context(id as u32, func);
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
                Some(id) => crate::scheduler::FiberId::Regular(id),
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
                    let _ = self.scheduler.kill_current();
                }
                ExecResult::Yield => {
                    self.scheduler.suspend_current();
                }
                ExecResult::Block => {
                    self.scheduler.block_current();
                }
                ExecResult::Panic => {
                    let (msg, loc_tuple) = self.scheduler.kill_current();
                    let loc = loc_tuple.map(|(func_id, pc)| ErrorLocation { func_id, pc });
                    return Err(VmError::PanicUnwound { msg, loc });
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
    
    /// Run one round of scheduler to let other fibers make progress.
    /// Used when trampoline fiber blocks on channel operations.
    fn run_scheduler_round(&mut self) {
        // Temporarily disable JIT to prevent nested trampoline calls
        #[cfg(feature = "jit")]
        let jit_mgr = self.jit_mgr.take();
        
        // Run all ready fibers once (or until they block/yield)
        let mut iterations = 0;
        let max_iterations = 1000; // Prevent infinite loops
        
        while let Some(id) = self.scheduler.schedule_next() {
            iterations += 1;
            if iterations > max_iterations {
                break;
            }
            
            let result = self.run_fiber(crate::scheduler::FiberId::Regular(id));
            
            match result {
                ExecResult::Continue => {
                    self.scheduler.suspend_current();
                }
                ExecResult::Return | ExecResult::Done => {
                    let _ = self.scheduler.kill_current();
                }
                ExecResult::Yield => {
                    self.scheduler.suspend_current();
                }
                ExecResult::Block => {
                    self.scheduler.block_current();
                }
                ExecResult::Panic => {
                    let _ = self.scheduler.kill_current();
                }
                ExecResult::Osr(_, _, _) => {
                    self.scheduler.suspend_current();
                }
            }
        }
        
        // Restore JIT manager
        #[cfg(feature = "jit")]
        { self.jit_mgr = jit_mgr; }
    }

    /// Run a fiber for up to TIME_SLICE instructions.
    /// Uses FiberId for type-safe fiber access.
    fn run_fiber(&mut self, fiber_id: crate::scheduler::FiberId) -> ExecResult {
        let module_ptr = match &self.module {
            Some(m) => m as *const Module,
            None => return ExecResult::Done,
        };
        // SAFETY: module_ptr is valid for the duration of run_fiber.
        let module = unsafe { &*module_ptr };

        // Cache fiber pointer outside the loop
        // SAFETY: fiber_ptr is valid as long as we don't reallocate fibers vec (only GoStart does)
        let mut fiber_ptr = self.scheduler.get_fiber_mut(fiber_id) as *mut Fiber;
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
                Opcode::Hint => {
                    #[cfg(feature = "jit")]
                    {
                        use vo_common_core::instruction::HINT_LOOP_BEGIN;
                        let hint_kind = inst.flags;
                        if hint_kind == HINT_LOOP_BEGIN {
                            let loop_pc = frame.pc - 1;
                            if let Some(new_pc) = self.try_osr(fiber_id, func_id, loop_pc, bp) {
                                frame.pc = new_pc;
                            }
                        }
                    }
                    ExecResult::Continue
                }

                Opcode::LoadInt => {
                    let val = inst.imm32() as i64 as u64;
                    stack_set(stack, bp + inst.a as usize, val);
                    ExecResult::Continue
                }
                Opcode::LoadConst => {
                    exec::exec_load_const(stack, bp, &inst, &module.constants);
                    ExecResult::Continue
                }

                Opcode::Copy => {
                    let val = stack_get(stack, bp + inst.b as usize);
                    stack_set(stack, bp + inst.a as usize, val);
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
                    if exec::exec_ptr_get(stack, bp, &inst) {
                        ExecResult::Continue
                    } else {
                        runtime_panic(&mut self.state.gc, fiber, stack, module, ERR_NIL_POINTER.to_string())
                    }
                }
                Opcode::PtrSet => {
                    if exec::exec_ptr_set(&stack, bp, &inst, &mut self.state.gc) {
                        ExecResult::Continue
                    } else {
                        runtime_panic(&mut self.state.gc, fiber, stack, module, ERR_NIL_POINTER.to_string())
                    }
                }
                Opcode::PtrGetN => {
                    if exec::exec_ptr_get_n(stack, bp, &inst) {
                        ExecResult::Continue
                    } else {
                        runtime_panic(&mut self.state.gc, fiber, stack, module, ERR_NIL_POINTER.to_string())
                    }
                }
                Opcode::PtrSetN => {
                    if exec::exec_ptr_set_n(&stack, bp, &inst) {
                        ExecResult::Continue
                    } else {
                        runtime_panic(&mut self.state.gc, fiber, stack, module, ERR_NIL_POINTER.to_string())
                    }
                }
                Opcode::PtrAdd => {
                    // a=dst, b=ptr, c=offset_slots: dst = ptr + offset * 8
                    let ptr = stack_get(stack, bp + inst.b as usize);
                    let offset = stack_get(stack, bp + inst.c as usize) as usize;
                    let addr = ptr + (offset * 8) as u64;
                    stack_set(stack, bp + inst.a as usize, addr);
                    ExecResult::Continue
                }

                // Integer arithmetic - inline for hot path
                Opcode::AddI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, a.wrapping_add(b) as u64);
                    ExecResult::Continue
                }
                Opcode::SubI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, a.wrapping_sub(b) as u64);
                    ExecResult::Continue
                }
                Opcode::MulI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, a.wrapping_mul(b) as u64);
                    ExecResult::Continue
                }
                Opcode::DivI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b == 0 {
                        runtime_panic(&mut self.state.gc, fiber, stack, module, "runtime error: integer divide by zero".to_string())
                    } else {
                        stack_set(stack, bp + inst.a as usize, a.wrapping_div(b) as u64);
                        ExecResult::Continue
                    }
                }
                Opcode::ModI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b == 0 {
                        runtime_panic(&mut self.state.gc, fiber, stack, module, "runtime error: integer divide by zero".to_string())
                    } else {
                        stack_set(stack, bp + inst.a as usize, a.wrapping_rem(b) as u64);
                        ExecResult::Continue
                    }
                }
                Opcode::NegI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, a.wrapping_neg() as u64);
                    ExecResult::Continue
                }

                // Float arithmetic
                Opcode::AddF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a + b).to_bits());
                    ExecResult::Continue
                }
                Opcode::SubF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a - b).to_bits());
                    ExecResult::Continue
                }
                Opcode::MulF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a * b).to_bits());
                    ExecResult::Continue
                }
                Opcode::DivF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a / b).to_bits());
                    ExecResult::Continue
                }
                Opcode::NegF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    stack_set(stack, bp + inst.a as usize, (-a).to_bits());
                    ExecResult::Continue
                }

                // Integer comparison - inline
                Opcode::EqI => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, (a == b) as u64);
                    ExecResult::Continue
                }
                Opcode::NeI => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, (a != b) as u64);
                    ExecResult::Continue
                }
                Opcode::LtI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, (a < b) as u64);
                    ExecResult::Continue
                }
                Opcode::LeI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, (a <= b) as u64);
                    ExecResult::Continue
                }
                Opcode::GtI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, (a > b) as u64);
                    ExecResult::Continue
                }
                Opcode::GeI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, (a >= b) as u64);
                    ExecResult::Continue
                }

                // Float comparison
                Opcode::EqF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a == b) as u64);
                    ExecResult::Continue
                }
                Opcode::NeF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a != b) as u64);
                    ExecResult::Continue
                }
                Opcode::LtF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a < b) as u64);
                    ExecResult::Continue
                }
                Opcode::LeF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a <= b) as u64);
                    ExecResult::Continue
                }
                Opcode::GtF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a > b) as u64);
                    ExecResult::Continue
                }
                Opcode::GeF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a >= b) as u64);
                    ExecResult::Continue
                }

                // Bitwise - inline
                Opcode::And => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, a & b);
                    ExecResult::Continue
                }
                Opcode::Or => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, a | b);
                    ExecResult::Continue
                }
                Opcode::Xor => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, a ^ b);
                    ExecResult::Continue
                }
                Opcode::AndNot => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, a & !b);
                    ExecResult::Continue
                }
                Opcode::Not => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    stack_set(stack, bp + inst.a as usize, !a);
                    ExecResult::Continue
                }
                Opcode::Shl => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b < 0 {
                        runtime_panic(&mut self.state.gc, fiber, stack, module, ERR_NEGATIVE_SHIFT.to_string())
                    } else {
                        // Go semantics: shift >= 64 returns 0
                        let result = if b >= 64 { 0 } else { a.wrapping_shl(b as u32) };
                        stack_set(stack, bp + inst.a as usize, result);
                        ExecResult::Continue
                    }
                }
                Opcode::ShrS => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b < 0 {
                        runtime_panic(&mut self.state.gc, fiber, stack, module, ERR_NEGATIVE_SHIFT.to_string())
                    } else {
                        // Go semantics: signed right shift >= 64 returns 0 (positive) or -1 (negative)
                        let result = if b >= 64 { if a < 0 { -1i64 } else { 0i64 } } else { a.wrapping_shr(b as u32) };
                        stack_set(stack, bp + inst.a as usize, result as u64);
                        ExecResult::Continue
                    }
                }
                Opcode::ShrU => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b < 0 {
                        runtime_panic(&mut self.state.gc, fiber, stack, module, ERR_NEGATIVE_SHIFT.to_string())
                    } else {
                        // Go semantics: unsigned right shift >= 64 returns 0
                        let result = if b >= 64 { 0 } else { a.wrapping_shr(b as u32) };
                        stack_set(stack, bp + inst.a as usize, result);
                        ExecResult::Continue
                    }
                }
                Opcode::BoolNot => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    stack_set(stack, bp + inst.a as usize, (a == 0) as u64);
                    ExecResult::Continue
                }

                // Jump - inline with OSR support
                Opcode::Jump => {
                    let offset = inst.imm32();
                    frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
                    ExecResult::Continue
                }
                Opcode::JumpIf => {
                    let cond = stack_get(stack, bp + inst.a as usize);
                    if cond != 0 {
                        let offset = inst.imm32();
                        frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
                    }
                    ExecResult::Continue
                }
                Opcode::JumpIfNot => {
                    let cond = stack_get(stack, bp + inst.a as usize);
                    if cond == 0 {
                        let offset = inst.imm32();
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
                    let call_ret_slots = (inst.c & 0xFF) as usize;
                    
                    // Try JIT via resolve_call
                    let target_func = &module.functions[target_func_id as usize];
                    let jit_func = self.jit_mgr.as_mut()
                        .and_then(|mgr| mgr.resolve_call(target_func_id, target_func, module));
                    
                    if let Some(jit_func) = jit_func {
                        // Use func_def.ret_slots for buffer allocation (JIT writes based on func definition)
                        // but only copy back call_ret_slots to caller's stack
                        let func_ret_slots = target_func.ret_slots as usize;
                        let result = self.call_jit_inline(fiber_id, jit_func, arg_start, arg_slots, func_ret_slots, call_ret_slots);
                        // JIT already set fiber.panic_state, just run unwind to execute defers
                        if matches!(result, ExecResult::Panic) {
                            panic_unwind(fiber, stack, module)
                        } else {
                            result
                        }
                    } else {
                        exec::exec_call(stack, &mut fiber.frames, &inst, module)
                    }
                }
                #[cfg(not(feature = "jit"))]
                Opcode::Call => {
                    exec::exec_call(stack, &mut fiber.frames, &inst, module)
                }
                Opcode::CallExtern => {
                    let mut extern_panic_msg: Option<String> = None;
                    // Get pointers for closure calling capability
                    let vm_ptr = self as *mut Vm as *mut std::ffi::c_void;
                    let fiber_ptr = fiber as *mut crate::fiber::Fiber as *mut std::ffi::c_void;
                    let result = exec::exec_call_extern(
                        stack,
                        bp,
                        &inst,
                        &module.externs,
                        &self.state.extern_registry,
                        &mut self.state.gc,
                        &module.struct_metas,
                        &module.interface_metas,
                        &module.named_type_metas,
                        &module.runtime_types,
                        &module.well_known,
                        &mut self.state.itab_cache,
                        &module.functions,
                        vm_ptr,
                        fiber_ptr,
                        Some(jit_glue::closure_call_trampoline),
                        &mut extern_panic_msg,
                    );
                    // Convert extern panic to recoverable runtime panic
                    if matches!(result, ExecResult::Panic) {
                        if let Some(msg) = extern_panic_msg {
                            runtime_panic(&mut self.state.gc, fiber, stack, module, msg)
                        } else {
                            result
                        }
                    } else {
                        result
                    }
                }
                Opcode::CallClosure => {
                    let closure_ref = stack[bp + inst.a as usize] as vo_runtime::gc::GcRef;
                    if closure_ref.is_null() {
                        runtime_panic(&mut self.state.gc, fiber, stack, module, ERR_NIL_FUNC_CALL.to_string())
                    } else {
                        exec::exec_call_closure(stack, &mut fiber.frames, &inst, module)
                    }
                }
                Opcode::CallIface => {
                    exec::exec_call_iface(stack, &mut fiber.frames, &inst, module, &self.state.itab_cache)
                }
                Opcode::Return => {
                    if fiber.is_direct_defer_context() {
                        panic_unwind(fiber, stack, module)
                    } else {
                        let func = &module.functions[func_id as usize];
                        let is_error_return = (inst.flags & 1) != 0;
                        exec::exec_return(stack, &mut fiber.frames, &mut fiber.defer_stack, &mut fiber.unwinding, &inst, func, module, is_error_return)
                    }
                }

                // String operations
                Opcode::StrNew => {
                    exec::exec_str_new(stack, bp, &inst, &module.constants, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::StrLen => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let len = if s.is_null() { 0 } else { string_len(s) };
                    stack_set(stack, bp + inst.a as usize, len as u64);
                    ExecResult::Continue
                }
                Opcode::StrIndex => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.c as usize) as usize;
                    // Bounds check
                    let len = if s.is_null() { 0 } else { string_len(s) };
                    if idx >= len {
                        runtime_panic(
                            &mut self.state.gc, fiber, stack, module,
                            format!("runtime error: index out of range [{}] with length {}", idx, len)
                        )
                    } else {
                        let byte = string_index(s, idx);
                        stack_set(stack, bp + inst.a as usize, byte as u64);
                        ExecResult::Continue
                    }
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
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    stack_set(stack, bp + inst.a as usize, string::eq(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrNe => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    stack_set(stack, bp + inst.a as usize, string::ne(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrLt => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    stack_set(stack, bp + inst.a as usize, string::lt(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrLe => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    stack_set(stack, bp + inst.a as usize, string::le(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrGt => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    stack_set(stack, bp + inst.a as usize, string::gt(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrGe => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    stack_set(stack, bp + inst.a as usize, string::ge(a, b) as u64);
                    ExecResult::Continue
                }
                Opcode::StrDecodeRune => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let pos = stack_get(stack, bp + inst.c as usize) as usize;
                    let (rune, width) = string::decode_rune_at(s, pos);
                    stack_set(stack, bp + inst.a as usize, rune as u64);
                    stack_set(stack, bp + inst.a as usize + 1, width as u64);
                    ExecResult::Continue
                }

                // Array operations
                Opcode::ArrayNew => {
                    exec::exec_array_new(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::ArrayGet => {
                    // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
                    let arr = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.c as usize) as usize;
                    // Bounds check
                    let len = array::len(arr);
                    if idx >= len {
                        runtime_panic(
                            &mut self.state.gc, fiber, stack, module,
                            format!("runtime error: index out of range [{}] with length {}", idx, len)
                        )
                    } else {
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
                            let elem_bytes = stack_get(stack, bp + inst.c as usize + 1) as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                stack_set(stack, dst + i, unsafe { *ptr });
                            }
                            return ExecResult::Continue;
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                stack_set(stack, dst + i, unsafe { *ptr });
                            }
                            return ExecResult::Continue;
                        }
                    };
                    stack_set(stack, dst, val);
                    ExecResult::Continue
                    }
                }
                Opcode::ArraySet => {
                    // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
                    let arr = stack_get(stack, bp + inst.a as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.b as usize) as usize;
                    // Bounds check
                    let len = array::len(arr);
                    if idx >= len {
                        runtime_panic(
                            &mut self.state.gc, fiber, stack, module,
                            format!("runtime error: index out of range [{}] with length {}", idx, len)
                        )
                    } else {
                    let src = bp + inst.c as usize;
                    let off = idx as isize;
                    let base = array::data_ptr_bytes(arr);
                    let val = stack_get(stack, src);
                    match inst.flags {
                        1 | 129 => unsafe { *base.offset(off) = val as u8 },
                        2 | 130 => unsafe { *(base.offset(off * 2) as *mut u16) = val as u16 },
                        4 | 132 => unsafe { *(base.offset(off * 4) as *mut u32) = val as u32 },
                        0x44 => unsafe { *(base.offset(off * 4) as *mut u32) = val as u32 },
                        8 => unsafe { *(base.offset(off * 8) as *mut u64) = val },
                        0 => {
                            // dynamic: elem_bytes in b+1 register
                            let elem_bytes = stack_get(stack, bp + inst.b as usize + 1) as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                unsafe { *ptr = stack_get(stack, src + i) };
                            }
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                unsafe { *ptr = stack_get(stack, src + i) };
                            }
                        }
                    }
                    ExecResult::Continue
                    }
                }
                Opcode::ArrayAddr => {
                    // Get element address: a=dst, b=array_gcref, c=index, flags=elem_bytes
                    let arr = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.c as usize) as usize;
                    let elem_bytes = inst.flags as usize;
                    let base = array::data_ptr_bytes(arr);
                    let addr = unsafe { base.add(idx * elem_bytes) } as u64;
                    stack_set(stack, bp + inst.a as usize, addr);
                    ExecResult::Continue
                }

                // Slice operations
                Opcode::SliceNew => {
                    exec::exec_slice_new(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::SliceGet => {
                    // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.c as usize) as usize;
                    // nil slice or out of bounds check
                    let len = if s.is_null() { 0 } else { slice_len(s) };
                    if idx >= len {
                        runtime_panic(
                            &mut self.state.gc, fiber, stack, module,
                            format!("runtime error: index out of range [{}] with length {}", idx, len)
                        )
                    } else {
                        let base = slice_data_ptr(s);
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
                                let elem_bytes = stack_get(stack, bp + inst.c as usize + 1) as usize;
                                for i in 0..(elem_bytes + 7) / 8 {
                                    let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                    stack_set(stack, dst + i, unsafe { *ptr });
                                }
                                return ExecResult::Continue;
                            }
                            _ => {
                                let elem_bytes = inst.flags as usize;
                                for i in 0..(elem_bytes + 7) / 8 {
                                    let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                    stack_set(stack, dst + i, unsafe { *ptr });
                                }
                                return ExecResult::Continue;
                            }
                        };
                        stack_set(stack, dst, val);
                        ExecResult::Continue
                    }
                }
                Opcode::SliceSet => {
                    // flags: 0=dynamic, 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
                    let s = stack_get(stack, bp + inst.a as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.b as usize) as usize;
                    // nil slice or out of bounds check
                    let len = if s.is_null() { 0 } else { slice_len(s) };
                    if idx >= len {
                        runtime_panic(
                            &mut self.state.gc, fiber, stack, module,
                            format!("runtime error: index out of range [{}] with length {}", idx, len)
                        )
                    } else {
                        let base = slice_data_ptr(s);
                        let src = bp + inst.c as usize;
                        let val = stack_get(stack, src);
                        match inst.flags {
                            1 | 129 => unsafe { *base.add(idx) = val as u8 },
                            2 | 130 => unsafe { *(base.add(idx * 2) as *mut u16) = val as u16 },
                            4 | 132 => unsafe { *(base.add(idx * 4) as *mut u32) = val as u32 },
                            0x44 => unsafe { *(base.add(idx * 4) as *mut u32) = val as u32 },
                            8 => unsafe { *(base.add(idx * 8) as *mut u64) = val },
                            0 => {
                                // dynamic: elem_bytes in b+1 register
                                let elem_bytes = stack_get(stack, bp + inst.b as usize + 1) as usize;
                                for i in 0..(elem_bytes + 7) / 8 {
                                    let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                    unsafe { *ptr = stack_get(stack, src + i) };
                                }
                            }
                            _ => {
                                let elem_bytes = inst.flags as usize;
                                for i in 0..(elem_bytes + 7) / 8 {
                                    let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                    unsafe { *ptr = stack_get(stack, src + i) };
                                }
                            }
                        }
                        ExecResult::Continue
                    }
                }
                Opcode::SliceLen => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let len = if s.is_null() { 0 } else { slice_len(s) };
                    stack_set(stack, bp + inst.a as usize, len as u64);
                    ExecResult::Continue
                }
                Opcode::SliceCap => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let cap = if s.is_null() { 0 } else { slice_cap(s) };
                    stack_set(stack, bp + inst.a as usize, cap as u64);
                    ExecResult::Continue
                }
                Opcode::SliceSlice => {
                    if exec::exec_slice_slice(stack, bp, &inst, &mut self.state.gc) {
                        ExecResult::Continue
                    } else {
                        let lo = stack_get(stack, bp + inst.c as usize);
                        let hi = stack_get(stack, bp + inst.c as usize + 1);
                        runtime_panic(
                            &mut self.state.gc, fiber, stack, module,
                            format!("runtime error: slice bounds out of range [{}:{}]", lo, hi)
                        )
                    }
                }
                Opcode::SliceAppend => {
                    exec::exec_slice_append(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::SliceAddr => {
                    // Get element address: a=dst, b=slice_reg, c=index, flags=elem_bytes
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.c as usize) as usize;
                    let elem_bytes = inst.flags as usize;
                    let base = slice_data_ptr(s);
                    let addr = unsafe { base.add(idx * elem_bytes) } as u64;
                    stack_set(stack, bp + inst.a as usize, addr);
                    ExecResult::Continue
                }

                // Map operations
                Opcode::MapNew => {
                    exec::exec_map_new(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::MapGet => {
                    exec::exec_map_get(stack, bp, &inst, Some(module));
                    ExecResult::Continue
                }
                Opcode::MapSet => {
                    // nil map write panics (Go semantics)
                    let m = stack_get(stack, bp + inst.a as usize) as GcRef;
                    if m.is_null() {
                        runtime_panic(&mut self.state.gc, fiber, stack, module, ERR_NIL_MAP_WRITE.to_string())
                    } else {
                        let ok = exec::exec_map_set(&stack, bp, &inst, &mut self.state.gc, Some(module));
                        if !ok {
                            runtime_panic(&mut self.state.gc, fiber, stack, module, ERR_UNHASHABLE_TYPE.to_string())
                        } else {
                            ExecResult::Continue
                        }
                    }
                }
                Opcode::MapDelete => {
                    // nil map delete is a no-op (Go semantics: delete from nil map does nothing)
                    let m = stack_get(stack, bp + inst.a as usize) as GcRef;
                    if m.is_null() {
                        return ExecResult::Continue;
                    }
                    exec::exec_map_delete(&stack, bp, &inst, Some(module));
                    ExecResult::Continue
                }
                Opcode::MapLen => {
                    exec::exec_map_len(stack, bp, &inst);
                    ExecResult::Continue
                }
                Opcode::MapIterInit => {
                    exec::exec_map_iter_init(stack, bp, &inst);
                    ExecResult::Continue
                }
                Opcode::MapIterNext => {
                    exec::exec_map_iter_next(stack, bp, &inst);
                    ExecResult::Continue
                }

                // Channel operations - need scheduler access
                Opcode::ChanNew => {
                    exec::exec_chan_new(stack, bp, &inst, &mut self.state.gc);
                    ExecResult::Continue
                }
                Opcode::ChanSend => {
                    match exec::exec_chan_send(&stack, bp, fiber_id.to_raw(), &inst) {
                        exec::ChanResult::Continue => ExecResult::Continue,
                        exec::ChanResult::Yield => {
                            fiber.current_frame_mut().unwrap().pc -= 1;
                            ExecResult::Block
                        }
                        exec::ChanResult::Panic => ExecResult::Panic,
                        exec::ChanResult::Wake(id) => {
                            self.scheduler.wake_fiber(crate::scheduler::FiberId::from_raw(id));
                            ExecResult::Continue
                        }
                        exec::ChanResult::WakeMultiple(ids) => {
                            for id in ids { self.scheduler.wake_fiber(crate::scheduler::FiberId::from_raw(id)); }
                            ExecResult::Continue
                        }
                    }
                }
                Opcode::ChanRecv => {
                    // Allow ChanRecv on trampoline - if it blocks, execute_jit_call will handle it
                    match exec::exec_chan_recv(stack, bp, fiber_id.to_raw(), &inst) {
                        exec::ChanResult::Continue => ExecResult::Continue,
                        exec::ChanResult::Yield => {
                            fiber.current_frame_mut().unwrap().pc -= 1;
                            ExecResult::Block
                        }
                        exec::ChanResult::Panic => ExecResult::Panic,
                        exec::ChanResult::Wake(id) => {
                            self.scheduler.wake_fiber(crate::scheduler::FiberId::from_raw(id));
                            ExecResult::Continue
                        }
                        exec::ChanResult::WakeMultiple(ids) => {
                            for id in ids { self.scheduler.wake_fiber(crate::scheduler::FiberId::from_raw(id)); }
                            ExecResult::Continue
                        }
                    }
                }
                Opcode::ChanClose => {
                    // Allow ChanClose on trampoline - it doesn't block
                    match exec::exec_chan_close(&stack, bp, &inst) {
                        exec::ChanResult::WakeMultiple(ids) => {
                            for id in ids { self.scheduler.wake_fiber(crate::scheduler::FiberId::from_raw(id)); }
                        }
                        _ => {}
                    }
                    ExecResult::Continue
                }
                Opcode::ChanLen => {
                    exec::exec_chan_len(stack, bp, &inst);
                    ExecResult::Continue
                }
                Opcode::ChanCap => {
                    exec::exec_chan_cap(stack, bp, &inst);
                    ExecResult::Continue
                }

                // Select operations - allowed on trampoline, if it yields execute_jit_call handles it
                Opcode::SelectBegin => {
                    exec::exec_select_begin(&mut fiber.select_state, &inst);
                    ExecResult::Continue
                }
                Opcode::SelectSend => {
                    exec::exec_select_send(&mut fiber.select_state, &inst);
                    ExecResult::Continue
                }
                Opcode::SelectRecv => {
                    exec::exec_select_recv(&mut fiber.select_state, &inst);
                    ExecResult::Continue
                }
                Opcode::SelectExec => {
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
                    // Trampoline fibers are in separate array, don't need reload
                    if let crate::scheduler::FiberId::Regular(idx) = fiber_id {
                        fiber_ptr = &mut self.scheduler.fibers[idx as usize] as *mut Fiber;
                        fiber = unsafe { &mut *fiber_ptr };
                    }
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
                    user_panic(fiber, stack, bp, inst.a, module)
                }
                Opcode::Recover => {
                    exec::exec_recover(stack, bp, fiber, &inst);
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
                Opcode::IfaceEq => {
                    let result = exec::exec_iface_eq(stack, bp, &inst, module);
                    if matches!(result, ExecResult::Panic) {
                        runtime_panic(
                            &mut self.state.gc, fiber, stack, module, ERR_UNCOMPARABLE_TYPE.to_string()
                        )
                    } else {
                        result
                    }
                }

                // Type conversion - inline
                Opcode::ConvI2F => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, (a as f64).to_bits());
                    ExecResult::Continue
                }
                Opcode::ConvF2I => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    stack_set(stack, bp + inst.a as usize, a as i64 as u64);
                    ExecResult::Continue
                }
                Opcode::ConvF64F32 => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    stack_set(stack, bp + inst.a as usize, (a as f32).to_bits() as u64);
                    ExecResult::Continue
                }
                Opcode::ConvF32F64 => {
                    let a = f32::from_bits(stack_get(stack, bp + inst.b as usize) as u32);
                    stack_set(stack, bp + inst.a as usize, (a as f64).to_bits());
                    ExecResult::Continue
                }
                Opcode::Trunc => {
                    let val = stack_get(stack, bp + inst.b as usize);
                    let flags = inst.flags;
                    let signed = (flags & 0x80) != 0;
                    let bytes = flags & 0x7F;
                    let result = match (bytes, signed) {
                        (1, true) => (val as i8) as i64 as u64,
                        (2, true) => (val as i16) as i64 as u64,
                        (4, true) => (val as i32) as i64 as u64,
                        (1, false) => (val as u8) as u64,
                        (2, false) => (val as u16) as u64,
                        (4, false) => (val as u32) as u64,
                        _ => val,
                    };
                    stack_set(stack, bp + inst.a as usize, result);
                    ExecResult::Continue
                }

                Opcode::IndexCheck => {
                    let idx = stack_get(stack, bp + inst.a as usize) as usize;
                    let len = stack_get(stack, bp + inst.b as usize) as usize;
                    if idx >= len {
                        runtime_panic(
                            &mut self.state.gc, fiber, stack, module,
                            format!("runtime error: index out of range [{}] with length {}", idx, len)
                        )
                    } else {
                        ExecResult::Continue
                    }
                }

                Opcode::Invalid => ExecResult::Panic,
            };

            match result {
                ExecResult::Continue => continue,
                ExecResult::Return => {
                    let frames_empty = self.scheduler.get_fiber(fiber_id).frames.is_empty();
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
