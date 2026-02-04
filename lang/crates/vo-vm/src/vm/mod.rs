//! Virtual machine main structure.

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(not(feature = "std"))]
use alloc::format;

#[cfg(feature = "std")]
use std::string::String;

#[cfg(feature = "std")]
use std::vec::Vec;

use vo_runtime::gc::GcRef;
use vo_runtime::objects::{array, string};

pub mod helpers;
mod types;
#[cfg(feature = "std")]
pub mod island_thread;
#[cfg(feature = "jit")]
mod jit_dispatch;

pub use helpers::{stack_get, stack_set};
pub use types::{ExecResult, VmError, VmState, ErrorLocation, TIME_SLICE, SchedulingOutcome, RuntimeTrapKind};
#[cfg(feature = "std")]
pub use types::IslandThread;

use helpers::{slice_data_ptr, slice_len, slice_cap, string_len, string_index, runtime_panic, runtime_panic_msg, runtime_trap, user_panic};

use crate::bytecode::Module;
use crate::exec;
use crate::fiber::{CallFrame, Fiber, FiberState};
use crate::scheduler::FiberId;
use crate::instruction::{Instruction, Opcode};
use crate::scheduler::Scheduler;
use vo_runtime::itab::ItabCache;

#[cfg(feature = "jit")]
pub mod jit_mgr;

#[cfg(feature = "jit")]
pub use jit_mgr::{JitManager, JitConfig};

pub struct Vm {
    /// JIT manager (only available with "jit" feature).
    ///
    /// Note: VM runtime execution is currently interpreter-only. The JIT manager is kept for
    /// compilation/codegen purposes while the execution integration is being rebuilt.
    /// IMPORTANT: Must be first field so it's dropped LAST (Rust drops in reverse order).
    /// JIT code memory must remain valid while scheduler/fibers are being dropped.
    #[cfg(feature = "jit")]
    pub jit_mgr: Option<JitManager>,
    pub module: Option<Module>,
    pub scheduler: Scheduler,
    pub state: VmState,
}

fn validate_externs_registered(registry: &vo_runtime::ExternRegistry, externs: &[vo_runtime::bytecode::ExternDef]) {
    let mut missing: Vec<(usize, &str)> = Vec::new();
    for (id, def) in externs.iter().enumerate() {
        if !registry.has(id as u32) {
            missing.push((id, def.name.as_str()));
        }
    }

    if missing.is_empty() {
        return;
    }

    let mut msg = String::from("unresolved extern functions:\n");
    for (id, name) in missing {
        msg.push_str(&format!("  - [{}] {}\n", id, name));
    }
    panic!("{}", msg);
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
    ///
    /// Note: thresholds currently affect compilation bookkeeping only.
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
    /// Note: VM runtime execution remains interpreter-only.
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
    
    pub fn set_program_args(&mut self, args: Vec<String>) {
        self.state.program_args = args;
    }

    #[cfg(feature = "std")]
    pub fn load(&mut self, module: Module) {
        self.load_with_extensions(module, None);
    }
    
    #[cfg(not(feature = "std"))]
    pub fn load(&mut self, module: Module) {
        vo_stdlib::register_externs(&mut self.state.extern_registry, &module.externs);

        self.finish_load(module);
    }

    /// Load a module with optional extension loader for native extensions.
    #[cfg(feature = "std")]
    pub fn load_with_extensions(
        &mut self,
        module: Module,
        ext_loader: Option<&vo_runtime::ext_loader::ExtensionLoader>,
    ) {
        #[cfg(not(target_arch = "wasm32"))]
        {
            vo_stdlib::register_externs(&mut self.state.extern_registry, &module.externs);
        }

        // Register extern functions from linkme distributed slices
        self.state.extern_registry.register_from_linkme(&module.externs);

        // Register extern functions from extension loader (if provided)
        if let Some(loader) = ext_loader {
            self.state.extern_registry.register_from_extension_loader(loader, &module.externs);
        }

        validate_externs_registered(&self.state.extern_registry, &module.externs);
        
        self.finish_load(module);
    }
    
    /// Finish loading a module (shared by load and load_with_extensions).
    fn finish_load(&mut self, module: Module) {
        let total_global_slots: usize = module.globals.iter().map(|g| g.slots as usize).sum();
        self.state.globals = vec![0u64; total_global_slots];
        // Initialize itab_cache from module's compile-time itabs
        self.state.itab_cache = ItabCache::from_module_itabs(module.itabs.clone());
        // Reset sentinel error cache for new module (prevents cross-module corruption)
        self.state.sentinel_errors = vo_runtime::SentinelErrorCache::new();
        
        // Initialize JIT manager state for this module
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

        match self.run_scheduling_loop(None)? {
            SchedulingOutcome::Blocked => self.report_deadlock(),
            _ => Ok(()),
        }
    }
    
    /// Run existing runnable fibers without spawning entry fiber.
    /// Used for event handling after initial run.
    pub fn run_scheduled(&mut self) -> Result<(), VmError> {
        match self.run_scheduling_loop(None)? {
            SchedulingOutcome::Blocked => self.report_deadlock(),
            _ => Ok(()),
        }
    }
    
    /// Core scheduling loop - runs fibers until all block or limit reached.
    /// Returns outcome without handling deadlock - caller decides the appropriate response.
    fn run_scheduling_loop(&mut self, max_iterations: Option<usize>) -> Result<SchedulingOutcome, VmError> {
        let mut iterations = 0;
        
        loop {
            if let Some(max) = max_iterations {
                iterations += 1;
                if iterations > max {
                    // Suspend current fiber before breaking so it can be rescheduled later
                    self.scheduler.yield_current();
                    break;
                }
            }
            
            // Process any wake commands from other islands
            #[cfg(feature = "std")]
            if let Some(ref rx) = self.state.main_cmd_rx {
                while let Ok(cmd) = rx.try_recv() {
                    match cmd {
                        vo_runtime::island::IslandCommand::WakeFiber { fiber_id } => {
                            self.scheduler.wake_fiber(crate::scheduler::FiberId::from_raw(fiber_id));
                        }
                        _ => {}
                    }
                }
            }
            
            // Check if we have runnable fibers
            if !self.scheduler.has_work() {
                // Poll I/O first to wake any fibers waiting on I/O
                #[cfg(feature = "std")]
                {
                    let woken = self.scheduler.poll_io(&mut self.state.io);
                    if woken > 0 {
                        continue;
                    }
                }
                
                // No runnable fibers - check if we have blocked fibers waiting for island wakes
                #[cfg(feature = "std")]
                if self.scheduler.has_blocked() && self.state.main_cmd_rx.is_some() {
                    // Wait for wake command from other islands
                    if let Some(ref rx) = self.state.main_cmd_rx {
                        match rx.recv_timeout(std::time::Duration::from_millis(100)) {
                            Ok(vo_runtime::island::IslandCommand::WakeFiber { fiber_id }) => {
                                self.scheduler.wake_fiber(crate::scheduler::FiberId::from_raw(fiber_id));
                                continue;
                            }
                            Ok(_) => continue,
                            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
                                // Also poll I/O during timeout wait
                                self.scheduler.poll_io(&mut self.state.io);
                                continue;
                            }
                            Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => break,
                        }
                    }
                }
                
                // Check if there are I/O waiters or blocked fibers - if so, keep polling
                #[cfg(feature = "std")]
                if self.scheduler.has_io_waiters() || self.scheduler.has_blocked() {
                    // Island VMs (current_island_id != 0) should return to let run_island_thread handle wake commands
                    if self.state.current_island_id != 0 && !self.scheduler.has_io_waiters() {
                        break;
                    }
                    // All fibers blocked, no I/O waiters, no island communication
                    // Return Blocked - caller decides if this is a deadlock
                    if !self.scheduler.has_io_waiters() && self.state.main_cmd_rx.is_none() {
                        return Ok(SchedulingOutcome::Blocked);
                    }

                    self.scheduler.poll_io(&mut self.state.io);
                    std::thread::sleep(std::time::Duration::from_millis(10));
                    continue;
                }
                
                return Ok(SchedulingOutcome::Completed);
            }
            
            let fiber_id = match self.scheduler.schedule_next() {
                Some(id) => crate::scheduler::FiberId::Regular(id),
                None => break,
            };

            let result = self.run_fiber(fiber_id);
            
            match result {
                ExecResult::TimesliceExpired | ExecResult::Osr(_, _, _) => {
                    self.scheduler.yield_current();
                }
                ExecResult::Block(reason) => {
                    match reason {
                        crate::fiber::BlockReason::Queue => {
                            self.scheduler.block_for_queue();
                        }
                        #[cfg(feature = "std")]
                        crate::fiber::BlockReason::Io(token) => {
                            let fiber = self.scheduler.current_fiber_mut().unwrap();
                            let frame = fiber.current_frame_mut().unwrap();
                            frame.pc -= 1;
                            self.scheduler.block_for_io(token);
                        }
                    }
                }
                ExecResult::Done => {
                    let _ = self.scheduler.kill_current();
                }
                ExecResult::Panic => {
                    let (trap_kind, msg, loc_tuple) = self.scheduler.kill_current();
                    let loc = loc_tuple.map(|(func_id, pc)| ErrorLocation { func_id, pc });
                    // For top-level callers, convert to error. For run_scheduler_round, just return.
                    if max_iterations.is_none() {
                        if let Some(kind) = trap_kind {
                            let msg = msg.expect("runtime trap should have message");
                            return Err(VmError::RuntimeTrap { kind, msg, loc });
                        }
                        return Err(VmError::PanicUnwound { msg, loc });
                    } else {
                        return Ok(SchedulingOutcome::Panicked);
                    }
                }
                ExecResult::FrameChanged => {
                    // Internal to run_fiber, should not reach here
                    self.scheduler.yield_current();
                }
            }
        }

        Ok(SchedulingOutcome::Completed)
    }
    
    
    /// Report deadlock with detailed fiber state.
    fn report_deadlock(&self) -> Result<(), VmError> {
        if let Some(module) = self.module.as_ref() {
            let mut msg = String::new();
            msg.push_str("vm deadlock: all fibers blocked\n");
            for (id, fiber) in self.scheduler.fibers.iter().enumerate() {
                if !fiber.state.is_blocked() {
                    continue;
                }
                msg.push_str(&format!("  fiber={} state={:?}\n", id, fiber.state));
                if let Some(frame) = fiber.frames.last() {
                    let func = &module.functions[frame.func_id as usize];
                    let code = &func.code;
                    let pc = frame.pc;
                    let prev_pc = pc.saturating_sub(1);
                    if let Some(inst) = code.get(prev_pc) {
                        msg.push_str(&format!(
                            "    at func={} pc={} inst@{}={:?}\n",
                            frame.func_id, pc, prev_pc, inst.opcode()
                        ));
                    }
                    if let Some(inst) = code.get(pc) {
                        msg.push_str(&format!("    next inst@{}={:?}\n", pc, inst.opcode()));
                    }
                }
            }
            return Err(VmError::Deadlock(msg));
        } else {
            return Err(VmError::Deadlock("vm deadlock: all fibers blocked".to_string()));
        }
    }
    
    /// Handle ChanResult from channel operations.
    /// 
    /// `advance_woken_pc`: When true, increment woken fiber's PC so it doesn't retry.
    /// - Recv waking a sender: true (sender's value was taken, send is complete)
    /// - Send waking a receiver: false (receiver needs to execute recv to get value)
    /// - Close waking waiters: false (they need to retry to see closed state)
    fn handle_chan_result(
        result: exec::ChanResult,
        gc: &mut vo_runtime::gc::Gc,
        fiber: &mut Fiber,
        stack: *mut vo_runtime::slot::Slot,
        module: &Module,
        scheduler: &mut Scheduler,
        advance_woken_pc: bool,
    ) -> ExecResult {
        match result {
            exec::ChanResult::Continue => ExecResult::FrameChanged,
            exec::ChanResult::Yield => {
                fiber.current_frame_mut().unwrap().pc -= 1;
                ExecResult::Block(crate::fiber::BlockReason::Queue)
            }
            exec::ChanResult::Wake(id) => {
                let fiber_id = crate::scheduler::FiberId::from_raw(id);
                if advance_woken_pc {
                    if let Some(frame) = scheduler.get_fiber_mut(fiber_id).current_frame_mut() {
                        frame.pc += 1;
                    }
                }
                scheduler.wake_fiber(fiber_id);
                ExecResult::TimesliceExpired
            }
            exec::ChanResult::WakeMultiple(ids) => {
                for id in ids {
                    let fiber_id = crate::scheduler::FiberId::from_raw(id);
                    if advance_woken_pc {
                        if let Some(frame) = scheduler.get_fiber_mut(fiber_id).current_frame_mut() {
                            frame.pc += 1;
                        }
                    }
                    scheduler.wake_fiber(fiber_id);
                }
                ExecResult::TimesliceExpired
            }
            exec::ChanResult::Trap(kind) => runtime_trap(gc, fiber, stack, module, kind),
        }
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
        // SAFETY: Box<Fiber> ensures stable addresses - fiber_ptr remains valid across Vec operations
        let fiber_ptr = self.scheduler.get_fiber_mut(fiber_id) as *mut Fiber;
        let fiber = unsafe { &mut *fiber_ptr };

        // SAFETY: We manually manage borrows via raw pointers to avoid borrow checker conflicts.
        // Get raw pointer to stack for fast access - fiber.ensure_capacity may invalidate this
        let mut stack = fiber.stack_ptr();
        let frames_ptr = &mut fiber.frames as *mut Vec<crate::fiber::CallFrame>;
        let frames = unsafe { &mut *frames_ptr };
        
        // Initialize frame variables using raw pointers
        let mut frame_ptr: *mut crate::fiber::CallFrame = match frames.last_mut() {
            Some(f) => f as *mut _,
            None => return ExecResult::Done,
        };
        let mut func_id: u32 = unsafe { (*frame_ptr).func_id };
        let mut bp: usize = unsafe { (*frame_ptr).bp };
        let mut code: &[Instruction] = &module.functions[func_id as usize].code;
        
        // Macro to refetch frame after Call/Return - only called when frame actually changes
        macro_rules! refetch {
            () => {{
                let frames = unsafe { &mut *frames_ptr };
                frame_ptr = match frames.last_mut() {
                    Some(f) => f as *mut _,
                    None => return ExecResult::Done,
                };
                func_id = unsafe { (*frame_ptr).func_id };
                bp = unsafe { (*frame_ptr).bp };
                code = &module.functions[func_id as usize].code;
            }};
        }

        for _ in 0..TIME_SLICE {
            let frame = unsafe { &mut *frame_ptr };
            let inst = unsafe { *code.get_unchecked(frame.pc) };
            frame.pc += 1;

            match inst.opcode() {
                // === SIMPLE INSTRUCTIONS: no frame change, just continue ===
                Opcode::Hint => {
                    // HINT_LOOP is now a no-op in VM - provides metadata for JIT analysis only.
                    // Hotspot detection moved to Jump instruction (back-edge detection).
                }

                Opcode::LoadInt => {
                    let val = inst.imm32() as i64 as u64;
                    stack_set(stack, bp + inst.a as usize, val);
                }
                Opcode::LoadConst => {
                    exec::exec_load_const(stack, bp, &inst, &module.constants);
                }

                Opcode::Copy => {
                    let val = stack_get(stack, bp + inst.b as usize);
                    stack_set(stack, bp + inst.a as usize, val);
                }
                Opcode::CopyN => {
                    exec::exec_copy_n(stack, bp, &inst);
                }
                Opcode::SlotGet => {
                    exec::exec_slot_get(stack, bp, &inst);
                }
                Opcode::SlotSet => {
                    exec::exec_slot_set(stack, bp, &inst);
                }
                Opcode::SlotGetN => {
                    exec::exec_slot_get_n(stack, bp, &inst);
                }
                Opcode::SlotSetN => {
                    exec::exec_slot_set_n(stack, bp, &inst);
                }

                Opcode::GlobalGet => {
                    exec::exec_global_get(stack, bp, &inst, &self.state.globals);
                }
                Opcode::GlobalGetN => {
                    exec::exec_global_get_n(stack, bp, &inst, &self.state.globals);
                }
                Opcode::GlobalSet => {
                    exec::exec_global_set(stack, bp, &inst, &mut self.state.globals);
                }
                Opcode::GlobalSetN => {
                    exec::exec_global_set_n(stack, bp, &inst, &mut self.state.globals);
                }

                Opcode::PtrNew => {
                    exec::exec_ptr_new(stack, bp, &inst, &mut self.state.gc);
                }
                Opcode::PtrGet => {
                    if !exec::exec_ptr_get(stack, bp, &inst) {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NilPointerDereference);
                    }
                }
                Opcode::PtrSet => {
                    if !exec::exec_ptr_set(stack, bp, &inst, &mut self.state.gc) {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NilPointerDereference);
                    }
                }
                Opcode::PtrGetN => {
                    if !exec::exec_ptr_get_n(stack, bp, &inst) {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NilPointerDereference);
                    }
                }
                Opcode::PtrSetN => {
                    if !exec::exec_ptr_set_n(stack, bp, &inst) {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NilPointerDereference);
                    }
                }
                Opcode::PtrAdd => {
                    let ptr = stack_get(stack, bp + inst.b as usize);
                    let offset = stack_get(stack, bp + inst.c as usize) as usize;
                    let addr = ptr + (offset * 8) as u64;
                    stack_set(stack, bp + inst.a as usize, addr);
                }

                // Integer arithmetic
                Opcode::AddI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, a.wrapping_add(b) as u64);
                }
                Opcode::SubI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, a.wrapping_sub(b) as u64);
                }
                Opcode::MulI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, a.wrapping_mul(b) as u64);
                }
                Opcode::DivI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b == 0 {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::DivisionByZero);
                    }
                    stack_set(stack, bp + inst.a as usize, a.wrapping_div(b) as u64);
                }
                Opcode::ModI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b == 0 {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::DivisionByZero);
                    }
                    stack_set(stack, bp + inst.a as usize, a.wrapping_rem(b) as u64);
                }
                Opcode::DivU => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    if b == 0 {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::DivisionByZero);
                    }
                    stack_set(stack, bp + inst.a as usize, a.wrapping_div(b));
                }
                Opcode::ModU => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    if b == 0 {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::DivisionByZero);
                    }
                    stack_set(stack, bp + inst.a as usize, a.wrapping_rem(b));
                }
                Opcode::NegI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, a.wrapping_neg() as u64);
                }

                // Float arithmetic
                Opcode::AddF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a + b).to_bits());
                }
                Opcode::SubF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a - b).to_bits());
                }
                Opcode::MulF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a * b).to_bits());
                }
                Opcode::DivF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a / b).to_bits());
                }
                Opcode::NegF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    stack_set(stack, bp + inst.a as usize, (-a).to_bits());
                }

                // Integer comparison
                Opcode::EqI => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, (a == b) as u64);
                }
                Opcode::NeI => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, (a != b) as u64);
                }
                Opcode::LtI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, (a < b) as u64);
                }
                Opcode::LeI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, (a <= b) as u64);
                }
                Opcode::GtI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, (a > b) as u64);
                }
                Opcode::GeI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, (a >= b) as u64);
                }
                
                // Unsigned integer comparison
                Opcode::LtU => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, (a < b) as u64);
                }
                Opcode::LeU => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, (a <= b) as u64);
                }
                Opcode::GtU => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, (a > b) as u64);
                }
                Opcode::GeU => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, (a >= b) as u64);
                }

                // Float comparison
                Opcode::EqF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a == b) as u64);
                }
                Opcode::NeF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a != b) as u64);
                }
                Opcode::LtF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a < b) as u64);
                }
                Opcode::LeF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a <= b) as u64);
                }
                Opcode::GtF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a > b) as u64);
                }
                Opcode::GeF => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let b = f64::from_bits(stack_get(stack, bp + inst.c as usize));
                    stack_set(stack, bp + inst.a as usize, (a >= b) as u64);
                }

                // Bitwise
                Opcode::And => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, a & b);
                }
                Opcode::Or => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, a | b);
                }
                Opcode::Xor => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, a ^ b);
                }
                Opcode::AndNot => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    stack_set(stack, bp + inst.a as usize, a & !b);
                }
                Opcode::Not => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    stack_set(stack, bp + inst.a as usize, !a);
                }
                Opcode::Shl => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b < 0 {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NegativeShift);
                    }
                    let result = if b >= 64 { 0 } else { a.wrapping_shl(b as u32) };
                    stack_set(stack, bp + inst.a as usize, result);
                }
                Opcode::ShrS => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b < 0 {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NegativeShift);
                    }
                    let result = if b >= 64 { if a < 0 { -1i64 } else { 0i64 } } else { a.wrapping_shr(b as u32) };
                    stack_set(stack, bp + inst.a as usize, result as u64);
                }
                Opcode::ShrU => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b < 0 {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NegativeShift);
                    }
                    let result = if b >= 64 { 0 } else { a.wrapping_shr(b as u32) };
                    stack_set(stack, bp + inst.a as usize, result);
                }
                Opcode::BoolNot => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    stack_set(stack, bp + inst.a as usize, (a == 0) as u64);
                }

                // Jump
                Opcode::Jump => {
                    let offset = inst.imm32();
                    let target_pc = (frame.pc as i64 + offset as i64 - 1) as usize;
                    
                    #[cfg(feature = "jit")]
                    if offset < 0 {
                        // Back-edge detected - this is a loop iteration
                        // target_pc is the loop_start (condition check)
                        if let Some(result_pc) = self.try_loop_osr(fiber_id, func_id, target_pc, bp) {
                            use jit_dispatch::{OSR_RESULT_FRAME_CHANGED, OSR_RESULT_WAITIO};
                            if result_pc == OSR_RESULT_FRAME_CHANGED {
                                // Loop made a Call - callee frame pushed, VM continues
                                let fiber = self.scheduler.get_fiber_mut(fiber_id);
                                stack = fiber.stack_ptr();
                                refetch!();
                                continue;
                            } else if result_pc == OSR_RESULT_WAITIO {
                                // Loop needs WaitIo
                                let fiber = self.scheduler.get_fiber_mut(fiber_id);
                                let token = fiber.resume_io_token
                                    .expect("OSR_RESULT_WAITIO but resume_io_token is None");
                                return ExecResult::Block(crate::fiber::BlockReason::Io(token));
                            } else {
                                // Normal exit - update PC and continue
                                let fiber = self.scheduler.get_fiber_mut(fiber_id);
                                fiber.current_frame_mut().unwrap().pc = result_pc;
                                stack = fiber.stack_ptr();
                                refetch!();
                                continue;
                            }
                        }
                    }
                    
                    frame.pc = target_pc;
                }
                Opcode::JumpIf => {
                    let cond = stack_get(stack, bp + inst.a as usize);
                    if cond != 0 {
                        let offset = inst.imm32();
                        frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
                    }
                }
                Opcode::JumpIfNot => {
                    let cond = stack_get(stack, bp + inst.a as usize);
                    if cond == 0 {
                        let offset = inst.imm32();
                        frame.pc = (frame.pc as i64 + offset as i64 - 1) as usize;
                    }
                }
                
                // ForLoop: idx++; if idx < limit goto offset
                Opcode::ForLoop => {
                    let idx = stack_get(stack, bp + inst.a as usize);
                    let limit = stack_get(stack, bp + inst.b as usize);
                    let offset = inst.c as i16;
                    let flags = inst.flags;
                    
                    // Increment or decrement
                    let decrement = (flags & 0x02) != 0;
                    let next_idx = if decrement {
                        idx.wrapping_sub(1)
                    } else {
                        idx.wrapping_add(1)
                    };
                    stack_set(stack, bp + inst.a as usize, next_idx);
                    
                    // Compare
                    let signed = (flags & 0x01) == 0;
                    let continue_loop = if decrement {
                        if signed { (next_idx as i64) > (limit as i64) }
                        else { next_idx > limit }
                    } else {
                        if signed { (next_idx as i64) < (limit as i64) }
                        else { next_idx < limit }
                    };
                    
                    if continue_loop {
                        let target_pc = (frame.pc as i64 + offset as i64) as usize;
                        
                        #[cfg(feature = "jit")]
                        {
                            // Back-edge: try OSR
                            if let Some(result_pc) = self.try_loop_osr(fiber_id, func_id, target_pc, bp) {
                                use jit_dispatch::{OSR_RESULT_FRAME_CHANGED, OSR_RESULT_WAITIO};
                                if result_pc == OSR_RESULT_FRAME_CHANGED {
                                    let fiber = self.scheduler.get_fiber_mut(fiber_id);
                                    stack = fiber.stack_ptr();
                                    refetch!();
                                    continue;
                                } else if result_pc == OSR_RESULT_WAITIO {
                                    let fiber = self.scheduler.get_fiber_mut(fiber_id);
                                    let token = fiber.resume_io_token
                                        .expect("OSR_RESULT_WAITIO but resume_io_token is None");
                                    return ExecResult::Block(crate::fiber::BlockReason::Io(token));
                                } else {
                                    let fiber = self.scheduler.get_fiber_mut(fiber_id);
                                    fiber.current_frame_mut().unwrap().pc = result_pc;
                                    stack = fiber.stack_ptr();
                                    refetch!();
                                    continue;
                                }
                            }
                        }
                        
                        frame.pc = target_pc;
                    }
                    // else: fall through (loop exit)
                }

                // === FRAME-CHANGING INSTRUCTIONS: must call refetch!() ===
                Opcode::Call => {
                    #[cfg(feature = "jit")]
                    {
                        let target_func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
                        if let Some(jit_mgr) = self.jit_mgr.as_mut() {
                            let target_func = &module.functions[target_func_id as usize];
                            if let Some(jit_func) = jit_mgr.resolve_call(target_func_id, target_func, module) {
                                // Execute via JIT
                                let result = jit_dispatch::dispatch_jit_call(
                                    self, fiber, &inst, module, jit_func, target_func_id
                                );
                                stack = fiber.stack_ptr();
                                match result {
                                    ExecResult::FrameChanged => {
                                        // JIT returned Ok or panic_unwind needs to execute defer
                                        refetch!();
                                    }
                                    other => return other,
                                }
                                continue;
                            }
                        }
                    }
                    // VM fallback path
                    exec::exec_call(fiber, &inst, module);
                    stack = fiber.stack_ptr();  // Re-fetch after potential stack growth
                    refetch!();
                }
                Opcode::CallExtern => {
                    let mut extern_panic_msg: Option<String> = None;
                    let vm_ptr = self as *mut Vm as *mut core::ffi::c_void;
                    let fiber_ptr = fiber as *mut crate::fiber::Fiber as *mut core::ffi::c_void;
                    let closure_call_fn: Option<vo_runtime::ffi::ClosureCallFn> = Some(closure_call_trampoline);
                    #[cfg(feature = "std")]
                    let resume_io_token = fiber.resume_io_token.take();
                    let result = exec::exec_call_extern(
                        &mut fiber.stack, bp, &inst, &module.externs, &self.state.extern_registry,
                        &mut self.state.gc, &module.struct_metas, &module.interface_metas,
                        &module.named_type_metas, &module.runtime_types, &mut self.state.itab_cache,
                        &module.functions, module, vm_ptr, fiber_ptr, closure_call_fn,
                        &mut extern_panic_msg, &module.well_known, &self.state.program_args,
                        &mut self.state.sentinel_errors,
                        #[cfg(feature = "std")] &mut self.state.io,
                        #[cfg(feature = "std")] resume_io_token,
                    );
                    if matches!(result, ExecResult::Panic) {
                        let r = if let Some(msg) = extern_panic_msg {
                            runtime_panic_msg(&mut self.state.gc, fiber, stack, module, msg)
                        } else { result };
                        if matches!(r, ExecResult::FrameChanged) { refetch!(); } else { return r; }
                    } else if matches!(result, ExecResult::FrameChanged) {
                        refetch!();
                    } else if !matches!(result, ExecResult::FrameChanged) {
                        return result;
                    }
                }
                Opcode::CallClosure => {
                    let closure_ref = stack_get(stack, bp + inst.a as usize) as vo_runtime::gc::GcRef;
                    if closure_ref.is_null() {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NilFuncCall);
                    }
                    exec::exec_call_closure(fiber, &inst, module);
                    stack = fiber.stack_ptr();  // Re-fetch after potential stack growth
                    refetch!();
                }
                Opcode::CallIface => {
                    exec::exec_call_iface(fiber, &inst, module, &self.state.itab_cache);
                    stack = fiber.stack_ptr();  // Re-fetch after potential stack growth
                    refetch!();
                }
                Opcode::Return => {
                    let result = if fiber.is_direct_defer_context() {
                        exec::handle_panic_unwind(fiber, module)
                    } else {
                        let func = &module.functions[func_id as usize];
                        let is_error_return = (inst.flags & 1) != 0;
                        exec::handle_return(fiber, &inst, func, module, is_error_return)
                    };
                    stack = fiber.stack_ptr();
                    if !matches!(result, ExecResult::FrameChanged) { return result; }
                    refetch!();
                }

                // String operations
                Opcode::StrNew => {
                    exec::exec_str_new(stack, bp, &inst, &module.constants, &mut self.state.gc);
                }
                Opcode::StrLen => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let len = if s.is_null() { 0 } else { string_len(s) };
                    stack_set(stack, bp + inst.a as usize, len as u64);
                }
                Opcode::StrIndex => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.c as usize) as usize;
                    let len = if s.is_null() { 0 } else { string_len(s) };
                    if idx >= len {
                        return runtime_panic(
                            &mut self.state.gc, fiber, stack, module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!("runtime error: index out of range [{}] with length {}", idx, len)
                        );
                    }
                    let byte = string_index(s, idx);
                    stack_set(stack, bp + inst.a as usize, byte as u64);
                }
                Opcode::StrConcat => {
                    exec::exec_str_concat(stack, bp, &inst, &mut self.state.gc);
                }
                Opcode::StrSlice => {
                    exec::exec_str_slice(stack, bp, &inst, &mut self.state.gc);
                }
                Opcode::StrEq => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    stack_set(stack, bp + inst.a as usize, string::eq(a, b) as u64);
                }
                Opcode::StrNe => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    stack_set(stack, bp + inst.a as usize, string::ne(a, b) as u64);
                }
                Opcode::StrLt => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    stack_set(stack, bp + inst.a as usize, string::lt(a, b) as u64);
                }
                Opcode::StrLe => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    stack_set(stack, bp + inst.a as usize, string::le(a, b) as u64);
                }
                Opcode::StrGt => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    stack_set(stack, bp + inst.a as usize, string::gt(a, b) as u64);
                }
                Opcode::StrGe => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    stack_set(stack, bp + inst.a as usize, string::ge(a, b) as u64);
                }
                Opcode::StrDecodeRune => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let pos = stack_get(stack, bp + inst.c as usize) as usize;
                    let (rune, width) = string::decode_rune_at(s, pos);
                    stack_set(stack, bp + inst.a as usize, rune as u64);
                    stack_set(stack, bp + inst.a as usize + 1, width as u64);
                }

                // Array operations
                Opcode::ArrayNew => {
                    exec::exec_array_new(stack, bp, &inst, &mut self.state.gc);
                }
                Opcode::ArrayGet => {
                    let arr = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.c as usize) as usize;
                    let len = array::len(arr);
                    if idx >= len {
                        return runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::IndexOutOfBounds,
                            format!("runtime error: index out of range [{}] with length {}", idx, len));
                    }
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
                            let elem_bytes = stack_get(stack, bp + inst.c as usize + 1) as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                stack_set(stack, dst + i, unsafe { *ptr });
                            }
                            continue;
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                stack_set(stack, dst + i, unsafe { *ptr });
                            }
                            continue;
                        }
                    };
                    stack_set(stack, dst, val);
                }
                Opcode::ArraySet => {
                    let arr = stack_get(stack, bp + inst.a as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.b as usize) as usize;
                    let len = array::len(arr);
                    if idx >= len {
                        return runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::IndexOutOfBounds,
                            format!("runtime error: index out of range [{}] with length {}", idx, len));
                    }
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
                }
                Opcode::ArrayAddr => {
                    let arr = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.c as usize) as usize;
                    let elem_bytes = inst.flags as usize;
                    let base = array::data_ptr_bytes(arr);
                    let addr = unsafe { base.add(idx * elem_bytes) } as u64;
                    stack_set(stack, bp + inst.a as usize, addr);
                }

                // Slice operations
                Opcode::SliceNew => {
                    if let Err(msg) = exec::exec_slice_new(stack, bp, &inst, &mut self.state.gc) {
                        return runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::MakeSlice, msg);
                    }
                }
                Opcode::SliceGet => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.c as usize) as usize;
                    let len = if s.is_null() { 0 } else { slice_len(s) };
                    if idx >= len {
                        return runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::IndexOutOfBounds,
                            format!("runtime error: index out of range [{}] with length {}", idx, len));
                    }
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
                            let elem_bytes = stack_get(stack, bp + inst.c as usize + 1) as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                stack_set(stack, dst + i, unsafe { *ptr });
                            }
                            continue;
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            for i in 0..(elem_bytes + 7) / 8 {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                stack_set(stack, dst + i, unsafe { *ptr });
                            }
                            continue;
                        }
                    };
                    stack_set(stack, dst, val);
                }
                Opcode::SliceSet => {
                    let s = stack_get(stack, bp + inst.a as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.b as usize) as usize;
                    let len = if s.is_null() { 0 } else { slice_len(s) };
                    if idx >= len {
                        return runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::IndexOutOfBounds,
                            format!("runtime error: index out of range [{}] with length {}", idx, len));
                    }
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
                }
                Opcode::SliceLen => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let len = if s.is_null() { 0 } else { slice_len(s) };
                    stack_set(stack, bp + inst.a as usize, len as u64);
                }
                Opcode::SliceCap => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let cap = if s.is_null() { 0 } else { slice_cap(s) };
                    stack_set(stack, bp + inst.a as usize, cap as u64);
                }
                Opcode::SliceSlice => {
                    if !exec::exec_slice_slice(stack, bp, &inst, &mut self.state.gc) {
                        let lo = stack_get(stack, bp + inst.c as usize);
                        let hi = stack_get(stack, bp + inst.c as usize + 1);
                        return runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::SliceBoundsOutOfRange,
                            format!("runtime error: slice bounds out of range [{}:{}]", lo, hi));
                    }
                }
                Opcode::SliceAppend => {
                    exec::exec_slice_append(stack, bp, &inst, &mut self.state.gc);
                }
                Opcode::SliceAddr => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.c as usize) as usize;
                    let elem_bytes = inst.flags as usize;
                    let base = slice_data_ptr(s);
                    let addr = unsafe { base.add(idx * elem_bytes) } as u64;
                    stack_set(stack, bp + inst.a as usize, addr);
                }

                // Map operations
                Opcode::MapNew => {
                    exec::exec_map_new(stack, bp, &inst, &mut self.state.gc);
                }
                Opcode::MapGet => {
                    exec::exec_map_get(stack, bp, &inst, Some(module));
                }
                Opcode::MapSet => {
                    let m = stack_get(stack, bp + inst.a as usize) as GcRef;
                    if m.is_null() {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NilMapWrite);
                    }
                    if !exec::exec_map_set(stack, bp, &inst, &mut self.state.gc, Some(module)) {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::UnhashableType);
                    }
                }
                Opcode::MapDelete => {
                    let m = stack_get(stack, bp + inst.a as usize) as GcRef;
                    if !m.is_null() {
                        exec::exec_map_delete(stack, bp, &inst, Some(module));
                    }
                }
                Opcode::MapLen => {
                    exec::exec_map_len(stack, bp, &inst);
                }
                Opcode::MapIterInit => {
                    exec::exec_map_iter_init(stack, bp, &inst);
                }
                Opcode::MapIterNext => {
                    exec::exec_map_iter_next(stack, bp, &inst);
                }

                // Channel operations
                Opcode::ChanNew => {
                    if let Err(msg) = exec::exec_chan_new(stack, bp, &inst, &mut self.state.gc) {
                        return runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::MakeChan, msg);
                    }
                }
                Opcode::ChanSend => {
                    let result = Self::handle_chan_result(
                        exec::exec_chan_send(stack, bp, fiber_id.to_raw(), &inst),
                        &mut self.state.gc, fiber, stack, module, &mut self.scheduler, false);
                    if matches!(result, ExecResult::FrameChanged) { refetch!(); } else { return result; }
                }
                Opcode::ChanRecv => {
                    let result = Self::handle_chan_result(
                        exec::exec_chan_recv(stack, bp, fiber_id.to_raw(), &inst),
                        &mut self.state.gc, fiber, stack, module, &mut self.scheduler, true);
                    if matches!(result, ExecResult::FrameChanged) { refetch!(); } else { return result; }
                }
                Opcode::ChanClose => {
                    let result = Self::handle_chan_result(
                        exec::exec_chan_close(stack, bp, &inst),
                        &mut self.state.gc, fiber, stack, module, &mut self.scheduler, false);
                    if matches!(result, ExecResult::FrameChanged) { refetch!(); } else { return result; }
                }
                Opcode::ChanLen => {
                    exec::exec_queue_get(stack, bp, &inst, vo_runtime::objects::channel::len);
                }
                Opcode::ChanCap => {
                    exec::exec_queue_get(stack, bp, &inst, vo_runtime::objects::queue_state::capacity);
                }

                // Select operations
                Opcode::SelectBegin => {
                    exec::exec_select_begin(&mut fiber.select_state, &inst);
                }
                Opcode::SelectSend => {
                    exec::exec_select_send(&mut fiber.select_state, &inst);
                }
                Opcode::SelectRecv => {
                    exec::exec_select_recv(&mut fiber.select_state, &inst);
                }
                Opcode::SelectExec => {
                    match exec::exec_select_exec(stack, bp, &mut fiber.select_state, &inst) {
                        exec::SelectResult::Continue => {}
                        exec::SelectResult::Block => {
                            frame.pc -= 1;
                            return ExecResult::TimesliceExpired;
                        }
                        exec::SelectResult::SendOnClosed => {
                            return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::SendOnClosedChannel);
                        }
                    }
                }

                // Closure operations
                Opcode::ClosureNew => {
                    exec::exec_closure_new(stack, bp, &inst, &mut self.state.gc);
                }
                Opcode::ClosureGet => {
                    exec::exec_closure_get(stack, bp, &inst);
                }

                // Goroutine - spawn new fiber
                Opcode::GoStart => {
                    let next_id = self.scheduler.fibers.len() as u32;
                    let go_result = exec::exec_go_start(stack, bp, &inst, &module.functions, next_id);
                    self.scheduler.spawn(go_result.new_fiber);
                }

                // Defer and error handling
                Opcode::DeferPush => {
                    let generation = fiber.effective_defer_generation();
                    exec::exec_defer_push(stack, bp, &fiber.frames, &mut fiber.defer_stack, &inst, &mut self.state.gc, generation);
                }
                Opcode::ErrDeferPush => {
                    let generation = fiber.effective_defer_generation();
                    exec::exec_err_defer_push(stack, bp, &fiber.frames, &mut fiber.defer_stack, &inst, &mut self.state.gc, generation);
                }
                Opcode::Panic => {
                    let result = user_panic(fiber, stack, bp, inst.a, module);
                    if matches!(result, ExecResult::FrameChanged) { refetch!(); } else { return result; }
                }
                Opcode::Recover => {
                    exec::exec_recover(stack, bp, fiber, &inst);
                }

                // Interface operations
                Opcode::IfaceAssign => {
                    exec::exec_iface_assign(stack, bp, &inst, &mut self.state.gc, &mut self.state.itab_cache, module);
                }
                Opcode::IfaceAssert => {
                    let result = exec::exec_iface_assert(stack, bp, &inst, &mut self.state.itab_cache, module);
                    if matches!(result, ExecResult::Panic) {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::TypeAssertionFailed);
                    }
                }
                Opcode::IfaceEq => {
                    let result = exec::exec_iface_eq(stack, bp, &inst, module);
                    if matches!(result, ExecResult::Panic) {
                        return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::UncomparableType);
                    }
                }

                // Type conversion
                Opcode::ConvI2F => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    stack_set(stack, bp + inst.a as usize, (a as f64).to_bits());
                }
                Opcode::ConvF2I => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    stack_set(stack, bp + inst.a as usize, a as i64 as u64);
                }
                Opcode::ConvF64F32 => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    stack_set(stack, bp + inst.a as usize, (a as f32).to_bits() as u64);
                }
                Opcode::ConvF32F64 => {
                    let a = f32::from_bits(stack_get(stack, bp + inst.b as usize) as u32);
                    stack_set(stack, bp + inst.a as usize, (a as f64).to_bits());
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
                }

                Opcode::IndexCheck => {
                    let idx = stack_get(stack, bp + inst.a as usize) as usize;
                    let len = stack_get(stack, bp + inst.b as usize) as usize;
                    if idx >= len {
                        return runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::IndexOutOfBounds,
                            format!("runtime error: index out of range [{}] with length {}", idx, len));
                    }
                }

                // === ISLAND/PORT: Cross-island operations ===
                #[cfg(feature = "std")]
                Opcode::IslandNew => {
                    let next_id = self.state.next_island_id;
                    self.state.next_island_id += 1;
                    let result = exec::exec_island_new(stack, bp, &inst, &mut self.state.gc, next_id);
                    
                    if self.state.island_registry.is_none() {
                        let (main_tx, main_rx) = std::sync::mpsc::channel::<vo_runtime::island::IslandCommand>();
                        let mut registry = std::collections::HashMap::new();
                        registry.insert(0u32, main_tx);
                        self.state.island_registry = Some(std::sync::Arc::new(std::sync::Mutex::new(registry)));
                        self.state.main_cmd_rx = Some(main_rx);
                    }
                    
                    let registry = self.state.island_registry.as_ref().unwrap().clone();
                    { let mut guard = registry.lock().unwrap(); guard.insert(next_id, result.command_tx.clone()); }
                    
                    let cmd_rx = result.command_rx;
                    let module_arc = std::sync::Arc::new(module.clone());
                    let registry_clone = registry.clone();
                    let join_handle = std::thread::spawn(move || {
                        island_thread::run_island_thread(next_id, module_arc, cmd_rx, registry_clone);
                    });
                    
                    self.state.island_threads.push(IslandThread {
                        handle: result.handle, command_tx: result.command_tx, join_handle: Some(join_handle),
                    });
                }
                #[cfg(not(feature = "std"))]
                Opcode::IslandNew => {
                    let _ = exec::exec_island_new(stack, bp, &inst, &mut self.state.gc, 0);
                }
                Opcode::PortNew => {
                    if let Err(msg) = exec::exec_port_new(stack, bp, &inst, &mut self.state.gc) {
                        return runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::MakePort, msg);
                    }
                }
                #[cfg(feature = "std")]
                Opcode::PortSend => {
                    let island_id = self.state.current_island_id;
                    let fid = fiber_id.to_raw() as u64;
                    match exec::exec_port_send(stack, bp, island_id, fid, &inst, &self.state.gc, &module.struct_metas, &module.runtime_types) {
                        exec::PortResult::Continue => {}
                        exec::PortResult::Yield => return ExecResult::Block(crate::fiber::BlockReason::Queue),
                        exec::PortResult::WakeRemote(waiter) => { self.state.wake_waiter(&waiter, &mut self.scheduler); }
                        exec::PortResult::SendOnClosed => {
                            return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::SendOnClosedChannel);
                        }
                        _ => {}
                    }
                }
                #[cfg(not(feature = "std"))]
                Opcode::PortSend => {
                    return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::PortNotSupported);
                }
                #[cfg(feature = "std")]
                Opcode::PortRecv => {
                    let island_id = self.state.current_island_id;
                    let fid = fiber_id.to_raw() as u64;
                    match exec::exec_port_recv(stack, bp, island_id, fid, &inst, &mut self.state.gc, &module.struct_metas, &module.runtime_types) {
                        exec::PortResult::Continue => {}
                        exec::PortResult::Yield => {
                            frame.pc -= 1;
                            return ExecResult::Block(crate::fiber::BlockReason::Queue);
                        }
                        exec::PortResult::WakeRemote(waiter) => { self.state.wake_waiter(&waiter, &mut self.scheduler); }
                        _ => {}
                    }
                }
                #[cfg(not(feature = "std"))]
                Opcode::PortRecv => {
                    return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::PortNotSupported);
                }
                #[cfg(feature = "std")]
                Opcode::PortClose => {
                    match exec::exec_port_close(stack, bp, &inst) {
                        exec::PortResult::Continue => {}
                        exec::PortResult::CloseNil => {
                            return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::CloseNilChannel);
                        }
                        exec::PortResult::Closed(waiters) => {
                            for waiter in &waiters { self.state.wake_waiter(waiter, &mut self.scheduler); }
                        }
                        _ => {}
                    }
                }
                #[cfg(not(feature = "std"))]
                Opcode::PortClose => {
                    match exec::exec_port_close(stack, bp, &inst) {
                        exec::PortResult::Continue => {}
                        exec::PortResult::CloseNil => {
                            return runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::CloseNilChannel);
                        }
                        _ => {}
                    }
                }
                Opcode::PortLen => {
                    exec::exec_queue_get(stack, bp, &inst, vo_runtime::objects::port::len);
                }
                Opcode::PortCap => {
                    exec::exec_queue_get(stack, bp, &inst, vo_runtime::objects::queue_state::capacity);
                }
                #[cfg(feature = "std")]
                Opcode::GoIsland => {
                    let result = exec::exec_go_island(stack, bp, &inst);
                    let island_id = vo_runtime::island::id(result.island);
                    
                    if island_id == 0 {
                        let closure_ref = stack_get(stack, bp + inst.b as usize) as vo_runtime::gc::GcRef;
                        let func_id = vo_runtime::objects::closure::func_id(closure_ref);
                        let local_slots = module.functions[func_id as usize].local_slots;
                        let mut new_fiber = crate::fiber::Fiber::new(0);
                        new_fiber.push_frame(func_id, local_slots, 0, 0);
                        new_fiber.stack[0] = closure_ref as u64;
                        self.scheduler.spawn(new_fiber);
                    } else {
                        let capture_count = result.capture_data.len() as u16;
                        let func_def = &module.functions[result.func_id as usize];
                        let data = exec::pack_closure_for_island(
                            &self.state.gc, &result, &func_def.capture_types, &func_def.param_types,
                            &module.struct_metas, &module.runtime_types,
                        );
                        let closure_data = vo_runtime::pack::PackedValue::from_data(data);
                        let cmd = vo_runtime::island::IslandCommand::SpawnFiber { closure_data, capture_slots: capture_count };
                        if let Some(ref registry) = self.state.island_registry {
                            if let Ok(guard) = registry.lock() {
                                if let Some(tx) = guard.get(&island_id) { let _ = tx.send(cmd); }
                            }
                        }
                    }
                }
                #[cfg(not(feature = "std"))]
                Opcode::GoIsland => {
                    let closure_ref = helpers::stack_get(stack, bp + inst.b as usize) as vo_runtime::gc::GcRef;
                    let func_id = vo_runtime::objects::closure::func_id(closure_ref);
                    let local_slots = module.functions[func_id as usize].local_slots;
                    let mut new_fiber = crate::fiber::Fiber::new(0);
                    new_fiber.push_frame(func_id, local_slots, 0, 0);
                    new_fiber.stack[0] = closure_ref as u64;
                    self.scheduler.spawn(new_fiber);
                }

                Opcode::Invalid => {
                    return ExecResult::Panic;
                }
            }
        }

        ExecResult::TimesliceExpired
    }

    /// Execute a function synchronously using a pooled callback fiber.
    /// Used by extern callbacks.
    /// Returns (success, panic_state).
    pub fn execute_closure_sync(
        &mut self,
        func_id: u32,
        args: &[u64],
        ret: *mut u64,
        ret_count: u32,
    ) -> (bool, Option<crate::fiber::PanicState>) {
        let module = match &self.module {
            Some(m) => m as *const Module,
            None => return (false, None),
        };
        let module = unsafe { &*module };
        
        let func_def = &module.functions[func_id as usize];
        
        // Acquire callback fiber and set up frame
        let fid = self.scheduler.acquire_callback_fiber();
        let fiber = self.scheduler.fiber_mut(fid);
        fiber.state = FiberState::Running;
        
        // Set up frame similar to exec_call
        let bp = fiber.sp;
        let local_slots = func_def.local_slots as usize;
        let new_sp = bp + local_slots;
        fiber.ensure_capacity(new_sp);
        
        // Zero the entire local slots area first (important for named returns and local vars)
        // write_bytes writes count * size_of::<T>() bytes, so pass local_slots (not * 8)
        unsafe { core::ptr::write_bytes(fiber.stack.as_mut_ptr().add(bp), 0, local_slots) };
        
        // Copy args (overwrites the zeros for arg slots)
        let n = (func_def.param_slots as usize).min(args.len());
        fiber.stack[bp..bp + n].copy_from_slice(&args[..n]);
        
        // Update sp and push frame
        fiber.sp = new_sp;
        fiber.frames.push(CallFrame::new(func_id, bp, 0, func_def.ret_slots));

        // Run to completion, with goroutine scheduling support
        let saved_current = self.scheduler.current;
        self.scheduler.current = Some(fid);
        
        let (success, panic_state) = loop {
            match self.run_fiber(FiberId::Regular(fid)) {
                ExecResult::Done => {
                    let fiber = self.scheduler.fiber(fid);
                    let n = (ret_count as usize).min(func_def.ret_slots as usize);
                    // Return values are at bp (start of function's local slots)
                    for i in 0..n {
                        unsafe { *ret.add(i) = fiber.stack[bp + i] };
                    }
                    break (true, None);
                }
                ExecResult::Panic => {
                    break (false, self.scheduler.fiber_mut(fid).panic_state.take());
                }
                ExecResult::Block(_) => {
                    // Callback fiber blocked (e.g., channel receive).
                    // Run other fibers (goroutines) until callback is unblocked.
                    self.scheduler.block_for_queue();
                    
                    let is_callback_runnable = |sched: &crate::scheduler::Scheduler| {
                        sched.fibers.get(fid as usize)
                            .map(|f| matches!(f.state, FiberState::Runnable))
                            .unwrap_or(false)
                    };
                    
                    while let Some(next_id) = self.scheduler.schedule_next() {
                        match self.run_fiber(FiberId::Regular(next_id)) {
                            ExecResult::Done | ExecResult::Panic => {
                                let _ = self.scheduler.kill_current();
                            }
                            ExecResult::Block(_) => self.scheduler.block_for_queue(),
                            ExecResult::TimesliceExpired => self.scheduler.yield_current(),
                            _ => {}
                        }
                        if is_callback_runnable(&self.scheduler) {
                            self.scheduler.current = Some(fid);
                            break;
                        }
                    }
                    
                    if is_callback_runnable(&self.scheduler) {
                        continue; // Resume callback fiber
                    } else {
                        break (false, None); // Deadlock or still blocked
                    }
                }
                _ => continue,
            }
        };

        self.scheduler.current = saved_current;
        self.scheduler.release_callback_fiber(fid);
        (success, panic_state)
    }
    
    // =========================================================================
    // Loop OSR
    // =========================================================================
    
    /// Try loop OSR at backedge. Returns result PC or None to continue VM.
    #[cfg(feature = "jit")]
    pub(crate) fn try_loop_osr(
        &mut self,
        fiber_id: crate::scheduler::FiberId,
        func_id: u32,
        loop_pc: usize,
        bp: usize,
    ) -> Option<usize> {
        let loop_func = self.get_or_compile_loop(func_id, loop_pc)?;
        jit_dispatch::dispatch_loop_osr(self, fiber_id, loop_func, bp)
    }
    
    /// Get compiled loop or compile if hot. Returns None if not ready.
    #[cfg(feature = "jit")]
    fn get_or_compile_loop(&mut self, func_id: u32, loop_pc: usize) -> Option<vo_jit::LoopFunc> {
        let module = self.module.as_ref()?;
        let func_def = &module.functions[func_id as usize];
        let jit_mgr = self.jit_mgr.as_mut()?;
        
        // Already compiled?
        if let Some(lf) = unsafe { jit_mgr.get_loop_func(func_id, loop_pc) } {
            return Some(lf);
        }
        
        // Already failed?
        if jit_mgr.is_loop_failed(func_id, loop_pc) {
            return None;
        }
        
        // Not hot yet?
        if !jit_mgr.record_backedge(func_id, loop_pc) {
            return None;
        }
        
        // Hot - try to compile
        let loop_info = match jit_mgr.find_loop(func_id, func_def, loop_pc) {
            Some(info) => info,
            None => {
                // Back-edge detected but no LoopInfo found - codegen bug or analysis bug
                // Mark as failed to avoid retrying
                jit_mgr.mark_loop_failed(func_id, loop_pc);
                return None;
            }
        };
        if !loop_info.is_jittable() {
            jit_mgr.mark_loop_failed(func_id, loop_pc);
            return None;
        }
        
        // Pre-compile Call targets so JIT-to-JIT calls can succeed
        let loop_end = loop_info.end_pc + 1;
        for pc in loop_info.begin_pc..loop_end {
            let inst = &func_def.code[pc];
            if inst.opcode() == Opcode::Call {
                let target_func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
                if !jit_mgr.is_compiled(target_func_id) && !jit_mgr.is_unsupported(target_func_id) {
                    let target_func = &module.functions[target_func_id as usize];
                    let _ = jit_mgr.compile_function(target_func_id, target_func, module);
                }
            }
        }
        
        match jit_mgr.compile_loop(func_id, func_def, module, &loop_info) {
            Ok(_) => unsafe { jit_mgr.get_loop_func(func_id, loop_pc) },
            Err(_) => {
                jit_mgr.mark_loop_failed(func_id, loop_pc);
                None
            }
        }
    }
}


impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

/// Trampoline for calling closures from extern functions.
/// This allows extern functions like dyn_call_closure to execute closures.
/// Uses a separate fiber because we're already inside run_fiber (would recurse).
pub extern "C" fn closure_call_trampoline(
    vm: *mut core::ffi::c_void,
    _caller_fiber: *mut core::ffi::c_void,
    closure_ref: u64,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> vo_runtime::ffi::ClosureCallResult {
    use vo_runtime::gc::GcRef;
    use vo_runtime::objects::closure;
    use helpers::build_closure_args;
    
    // In std mode, catch panics to prevent unwinding across FFI boundary
    #[cfg(feature = "std")]
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let vm = unsafe { &mut *(vm as *mut Vm) };
        let closure_gcref = closure_ref as GcRef;
        let func_id = closure::func_id(closure_gcref);
        
        let module = vm.module().expect("closure_call_trampoline: module not set");
        let func_def = &module.functions[func_id as usize];
        let full_args = build_closure_args(closure_ref, closure_gcref, func_def, args, arg_count);
        
        // Use execute_closure_sync which creates a separate fiber
        // (we're already inside run_fiber, can't call it recursively)
        let (success, _panic_state) = vm.execute_closure_sync(func_id, &full_args, ret, ret_count);
        success
    }));
    
    #[cfg(feature = "std")]
    return match result {
        Ok(true) => vo_runtime::ffi::ClosureCallResult::Ok,
        _ => vo_runtime::ffi::ClosureCallResult::Panic
    };
    
    // In no_std mode, no panic catching (panics will abort)
    #[cfg(not(feature = "std"))]
    {
        let vm = unsafe { &mut *(vm as *mut Vm) };
        let closure_gcref = closure_ref as GcRef;
        let func_id = closure::func_id(closure_gcref);
        
        let module = vm.module().expect("closure_call_trampoline: module not set");
        let func_def = &module.functions[func_id as usize];
        let full_args = build_closure_args(closure_ref, closure_gcref, func_def, args, arg_count);
        
        // Use execute_closure_sync which creates a separate fiber
        let (success, _panic_state) = vm.execute_closure_sync(func_id, &full_args, ret, ret_count);
        if success {
            vo_runtime::ffi::ClosureCallResult::Ok
        } else {
            vo_runtime::ffi::ClosureCallResult::Panic
        }
    }
}


