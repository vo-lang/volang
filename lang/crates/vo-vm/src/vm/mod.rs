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
mod jit;

pub use helpers::{stack_get, stack_set};
pub use types::{ExecResult, VmError, VmState, ErrorLocation, TIME_SLICE, SchedulingOutcome, RuntimeTrapKind};
#[cfg(feature = "std")]
pub use types::IslandThread;

use helpers::{slice_data_ptr, slice_len, slice_cap, string_len, string_index, runtime_panic, runtime_panic_msg, runtime_trap, user_panic};

use crate::bytecode::Module;
use crate::exec;
use crate::fiber::{CallFrame, Fiber};
/// Result of wait_for_work() — what the scheduling loop should do next.
enum WaitResult {
    /// Work became available, retry the loop.
    Retry,
    /// All fibers completed normally.
    Done,
    /// All fibers blocked (potential deadlock).
    Blocked,
    /// Some fibers waiting for host-side events; async loop must handle them.
    SuspendedForHostEvents,
    /// Island VM should return to its command loop.
    Break,
}

/// Processed port operation outcome.
#[cfg(feature = "std")]
enum PortAction {
    Continue,
    Block,
    Trap(RuntimeTrapKind),
    Wake(Vec<vo_runtime::objects::port::WaiterInfo>),
}
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
    
    /// Create a new island - shared by VM interpreter and JIT callbacks.
    /// Returns the island handle (GcRef).
    #[cfg(feature = "std")]
    pub fn create_island(&mut self) -> GcRef {
        let module = self.module.as_ref().expect("module required for create_island");
        let next_id = self.state.next_island_id;
        self.state.next_island_id += 1;
        
        // Create island channel and handle
        let (tx, rx) = std::sync::mpsc::channel::<vo_runtime::island::IslandCommand>();
        let handle = vo_runtime::island::create(&mut self.state.gc, next_id);
        
        // Initialize registry and main_cmd_rx if first island
        if self.state.island_registry.is_none() {
            let (main_tx, main_rx) = std::sync::mpsc::channel::<vo_runtime::island::IslandCommand>();
            let mut registry = std::collections::HashMap::new();
            registry.insert(0u32, main_tx);
            self.state.island_registry = Some(std::sync::Arc::new(std::sync::Mutex::new(registry)));
            self.state.main_cmd_rx = Some(main_rx);
        }
        
        // Register this island
        let registry = self.state.island_registry.as_ref().unwrap().clone();
        { let mut guard = registry.lock().unwrap(); guard.insert(next_id, tx.clone()); }
        
        // Spawn island thread with JIT config from main VM
        let module_arc = std::sync::Arc::new(module.clone());
        let registry_clone = registry.clone();
        #[cfg(feature = "jit")]
        let jit_config = self.jit_mgr.as_ref().map(|mgr| mgr.config().clone());
        let join_handle = std::thread::spawn(move || {
            #[cfg(feature = "jit")]
            island_thread::run_island_thread(next_id, module_arc, rx, registry_clone, jit_config);
            #[cfg(not(feature = "jit"))]
            island_thread::run_island_thread(next_id, module_arc, rx, registry_clone);
        });
        
        // Save thread handle
        self.state.island_threads.push(IslandThread {
            handle, command_tx: tx, join_handle: Some(join_handle),
        });
        
        handle
    }

    /// Spawn the entry function as a new fiber.  Called by `run()` only.
    fn spawn_entry(&mut self) -> Result<(), VmError> {
        let module = self.module.as_ref().ok_or(VmError::NoEntryFunction)?;
        let entry_func = module.entry_func;
        if entry_func as usize >= module.functions.len() {
            return Err(VmError::InvalidFunctionId(entry_func));
        }
        let func = &module.functions[entry_func as usize];
        let mut fiber = Fiber::new(0);
        fiber.push_frame(entry_func, func.local_slots, 0, 0);
        self.scheduler.spawn(fiber);
        Ok(())
    }

    /// Spawn the entry function and run all fibers.
    ///
    /// Returns `Ok(outcome)` where outcome is one of:
    /// - `Completed`              — program exited normally
    /// - `Blocked`                — all goroutines stuck on channels; call `deadlock_err()` for details
    /// - `SuspendedForHostEvents` — waiting for async host callbacks (WASM timer/HTTP, GUI events)
    ///
    /// Callers decide whether `Blocked` is a deadlock error or expected behaviour (e.g. GUI host VM).
    pub fn run(&mut self) -> Result<SchedulingOutcome, VmError> {
        self.spawn_entry()?;
        self.run_scheduling_loop(None)
    }

    /// Run existing fibers without spawning an entry fiber.
    ///
    /// Used for event dispatch after initial `run()`, island command handlers, and WASM async
    /// continuation.  Same outcome semantics as `run()`.
    pub fn run_scheduled(&mut self) -> Result<SchedulingOutcome, VmError> {
        self.run_scheduling_loop(None)
    }

    /// Build a `VmError::Deadlock` with current fiber diagnostics.
    ///
    /// Call this when `run()` / `run_scheduled()` returns `Ok(SchedulingOutcome::Blocked)` and
    /// you want to treat it as a fatal deadlock.
    pub fn deadlock_err(&self) -> VmError {
        self.report_deadlock().unwrap_err()
    }

    /// Core scheduling loop - runs fibers until all block or limit reached.
    /// Returns outcome without handling deadlock - caller decides the appropriate response.
    fn run_scheduling_loop(&mut self, max_iterations: Option<usize>) -> Result<SchedulingOutcome, VmError> {
        let mut iterations = 0;
        
        loop {
            if let Some(max) = max_iterations {
                iterations += 1;
                if iterations > max {
                    self.scheduler.yield_current();
                    break;
                }
            }
            
            self.process_island_commands();
            
            if !self.scheduler.has_work() {
                match self.wait_for_work() {
                    WaitResult::Retry => continue,
                    WaitResult::Done => return Ok(SchedulingOutcome::Completed),
                    WaitResult::Blocked => return Ok(SchedulingOutcome::Blocked),
                    WaitResult::SuspendedForHostEvents => return Ok(SchedulingOutcome::SuspendedForHostEvents),
                    WaitResult::Break => break,
                }
            }
            
            let fiber_id = match self.scheduler.schedule_next() {
                Some(id) => id,
                None => break,
            };

            // GC step at scheduling boundary — between fiber runs, all stacks are stable.
            self.gc_step();

            let result = self.run_fiber(fiber_id);
            match self.handle_exec_result(result, max_iterations.is_some()) {
                None => {} // continue scheduling
                Some(Ok(outcome)) => return Ok(outcome),
                Some(Err(e)) => return Err(e),
            }
        }

        Ok(SchedulingOutcome::Completed)
    }
    
    /// Process wake commands from other island threads (non-blocking).
    #[inline]
    fn process_island_commands(&mut self) {
        #[cfg(feature = "std")]
        if let Some(ref rx) = self.state.main_cmd_rx {
            while let Ok(cmd) = rx.try_recv() {
                if let vo_runtime::island::IslandCommand::WakeFiber { fiber_id } = cmd {
                    self.scheduler.wake_fiber(crate::scheduler::FiberId::from_raw(fiber_id));
                }
            }
        }
    }
    
    /// When no fibers are runnable, try to make progress via I/O polling or
    /// island command waiting. Returns what the scheduling loop should do next.
    fn wait_for_work(&mut self) -> WaitResult {
        // Try I/O polling first
        #[cfg(feature = "std")]
        {
            if self.scheduler.poll_io(&mut self.state.io) > 0 {
                return WaitResult::Retry;
            }
        }
        
        // Try waiting for island commands
        #[cfg(feature = "std")]
        if self.scheduler.has_blocked() && self.state.main_cmd_rx.is_some() {
            if let Some(ref rx) = self.state.main_cmd_rx {
                match rx.recv_timeout(std::time::Duration::from_millis(100)) {
                    Ok(vo_runtime::island::IslandCommand::WakeFiber { fiber_id }) => {
                        self.scheduler.wake_fiber(crate::scheduler::FiberId::from_raw(fiber_id));
                        return WaitResult::Retry;
                    }
                    Ok(_) => return WaitResult::Retry,
                    Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
                        self.scheduler.poll_io(&mut self.state.io);
                        return WaitResult::Retry;
                    }
                    Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
                        return WaitResult::Break;
                    }
                }
            }
        }
        
        // Check if there are waiters that might still make progress
        #[cfg(feature = "std")]
        if self.scheduler.has_io_waiters() || self.scheduler.has_blocked() {
            // Island VMs return to let run_island_thread handle commands
            if self.state.current_island_id != 0 {
                if self.scheduler.has_io_waiters() {
                    self.scheduler.poll_io(&mut self.state.io);
                }
                return WaitResult::Break;
            }
            // No I/O waiters and no island communication => true deadlock
            if !self.scheduler.has_io_waiters() && self.state.main_cmd_rx.is_none() {
                return WaitResult::Blocked;
            }
            // Keep polling
            self.scheduler.poll_io(&mut self.state.io);
            std::thread::sleep(std::time::Duration::from_millis(10));
            return WaitResult::Retry;
        }
        
        // If the only blocked fibers are host event waiters, signal the async loop.
        if self.scheduler.has_host_event_waiters() {
            return WaitResult::SuspendedForHostEvents;
        }

        WaitResult::Done
    }

    /// Wake a fiber blocked on a host-side event and schedule it to run.
    /// Called by the WASM async run loop after a host event fires.
    pub fn wake_host_event(&mut self, token: u64) {
        self.scheduler.wake_host_event(token);
    }

    /// Handle a fiber execution result. Returns:
    /// - `None`: continue scheduling loop
    /// - `Some(Ok(outcome))`: return this outcome
    /// - `Some(Err(e))`: return this error
    fn handle_exec_result(
        &mut self,
        result: ExecResult,
        is_bounded: bool,
    ) -> Option<Result<SchedulingOutcome, VmError>> {
        match result {
            ExecResult::TimesliceExpired => {
                self.scheduler.yield_current();
            }
            ExecResult::Block(reason) => {
                match reason {
                    crate::fiber::BlockReason::Queue => {
                        self.scheduler.block_for_queue();
                    }
                    crate::fiber::BlockReason::HostEvent(_) |
                    crate::fiber::BlockReason::HostEventReplay(_) => {
                        unreachable!("HostEvent blocked directly in scheduling loop, not via ExecResult");
                    }
                    #[cfg(feature = "std")]
                    crate::fiber::BlockReason::Io(token) => {
                        let fiber = self.scheduler.current_fiber_mut().unwrap();
                        fiber.current_frame_mut().unwrap().pc -= 1;
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
                if !is_bounded {
                    if let Some(kind) = trap_kind {
                        let msg = msg.expect("runtime trap should have message");
                        return Some(Err(VmError::RuntimeTrap { kind, msg, loc }));
                    }
                    return Some(Err(VmError::PanicUnwound { msg, loc }));
                } else {
                    return Some(Ok(SchedulingOutcome::Panicked));
                }
            }
            ExecResult::FrameChanged | ExecResult::CallClosure { .. } => {
                debug_assert!(false, "internal ExecResult leaked to scheduling loop: {:?}", result);
                self.scheduler.yield_current();
            }
        }
        None
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

    /// Handle channel operation result.
    /// 
    /// **Blocker sets resume PC:** Only recv decrements PC (to re-execute and fetch data).
    fn handle_chan_result(
        result: exec::ChanResult,
        gc: &mut vo_runtime::gc::Gc,
        fiber: &mut Fiber,
        stack: *mut vo_runtime::slot::Slot,
        module: &Module,
        scheduler: &mut Scheduler,
        is_recv: bool,
    ) -> ExecResult {
        match result {
            exec::ChanResult::Continue => ExecResult::FrameChanged,
            exec::ChanResult::Yield => {
                if is_recv { fiber.current_frame_mut().unwrap().pc -= 1; }
                ExecResult::Block(crate::fiber::BlockReason::Queue)
            }
            exec::ChanResult::Wake(waiter) => {
                scheduler.wake_channel_waiter(&waiter);
                ExecResult::TimesliceExpired
            }
            exec::ChanResult::WakeMultiple(waiters) => {
                for waiter in &waiters {
                    scheduler.wake_channel_waiter(waiter);
                }
                ExecResult::TimesliceExpired
            }
            exec::ChanResult::Trap(kind) => runtime_trap(gc, fiber, stack, module, kind),
        }
    }

    /// Process port operation result into a PortAction.
    /// Handles PC adjustment for recv blocking. Does not touch gc or scheduler.
    #[cfg(feature = "std")]
    fn process_port_result(
        result: exec::PortResult,
        fiber: &mut Fiber,
        is_recv: bool,
    ) -> PortAction {
        match result {
            exec::PortResult::Continue => PortAction::Continue,
            exec::PortResult::Yield => {
                if is_recv {
                    fiber.current_frame_mut().unwrap().pc -= 1;
                }
                PortAction::Block
            }
            exec::PortResult::WakeRemote(waiter) => PortAction::Wake(vec![waiter]),
            exec::PortResult::SendOnClosed => PortAction::Trap(RuntimeTrapKind::SendOnClosedChannel),
            exec::PortResult::CloseNil => PortAction::Trap(RuntimeTrapKind::CloseNilChannel),
            exec::PortResult::Closed(waiters) => PortAction::Wake(waiters),
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
        
        // Macro to handle panic/trap results that may return FrameChanged (when defer/recover exists).
        // Without this, `return runtime_trap(...)` would leak FrameChanged to the scheduling loop.
        macro_rules! handle_panic_result {
            ($result:expr) => {{
                let r = $result;
                if matches!(r, ExecResult::FrameChanged) {
                    stack = fiber.stack_ptr();
                    refetch!();
                    continue;
                } else {
                    return r;
                }
            }};
        }

        // Macro to handle loop OSR result - used by both Jump and ForLoop
        #[cfg(feature = "jit")]
        macro_rules! handle_loop_osr {
            ($target_pc:expr) => {{
                if let Some(osr_result) = jit::try_loop_osr(self, fiber_id, func_id, $target_pc, bp) {
                    match osr_result {
                        jit::OsrResult::FrameChanged => {
                            let fiber = self.scheduler.get_fiber_mut(fiber_id);
                            stack = fiber.stack_ptr();
                            refetch!();
                            continue;
                        }
                        #[cfg(feature = "std")]
                        jit::OsrResult::WaitIo => {
                            let fiber = self.scheduler.get_fiber_mut(fiber_id);
                            let token = fiber.resume_io_token
                                .expect("OsrResult::WaitIo but resume_io_token is None");
                            return ExecResult::Block(crate::fiber::BlockReason::Io(token));
                        }
                        jit::OsrResult::WaitQueue => {
                            return ExecResult::Block(crate::fiber::BlockReason::Queue);
                        }
                        jit::OsrResult::ExitPc(exit_pc) => {
                            let fiber = self.scheduler.get_fiber_mut(fiber_id);
                            fiber.current_frame_mut().unwrap().pc = exit_pc;
                            stack = fiber.stack_ptr();
                            refetch!();
                            continue;
                        }
                        jit::OsrResult::Panic => {
                            let fiber = self.scheduler.get_fiber_mut(fiber_id);
                            stack = fiber.stack_ptr();
                            handle_panic_result!(helpers::panic_unwind(fiber, stack, module));
                        }
                    }
                }
            }};
        }

        // Macro to handle PortAction result - used by PortSend, PortRecv, PortClose
        #[cfg(feature = "std")]
        macro_rules! handle_port_action {
            ($action:expr) => {
                match $action {
                    PortAction::Continue => { refetch!(); }
                    PortAction::Block => { return ExecResult::Block(crate::fiber::BlockReason::Queue); }
                    PortAction::Trap(kind) => { handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, kind)); }
                    PortAction::Wake(waiters) => {
                        for w in &waiters { self.state.wake_waiter(w, &mut self.scheduler); }
                        refetch!();
                    }
                }
            };
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
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NilPointerDereference));
                    }
                }
                Opcode::PtrSet => {
                    if !exec::exec_ptr_set(stack, bp, &inst, &mut self.state.gc) {
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NilPointerDereference));
                    }
                }
                Opcode::PtrGetN => {
                    if !exec::exec_ptr_get_n(stack, bp, &inst) {
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NilPointerDereference));
                    }
                }
                Opcode::PtrSetN => {
                    if !exec::exec_ptr_set_n(stack, bp, &inst) {
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NilPointerDereference));
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
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::DivisionByZero));
                    }
                    stack_set(stack, bp + inst.a as usize, a.wrapping_div(b) as u64);
                }
                Opcode::ModI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b == 0 {
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::DivisionByZero));
                    }
                    stack_set(stack, bp + inst.a as usize, a.wrapping_rem(b) as u64);
                }
                Opcode::DivU => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    if b == 0 {
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::DivisionByZero));
                    }
                    stack_set(stack, bp + inst.a as usize, a.wrapping_div(b));
                }
                Opcode::ModU => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    if b == 0 {
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::DivisionByZero));
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
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NegativeShift));
                    }
                    let result = if b >= 64 { 0 } else { a.wrapping_shl(b as u32) };
                    stack_set(stack, bp + inst.a as usize, result);
                }
                Opcode::ShrS => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b < 0 {
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NegativeShift));
                    }
                    let result = if b >= 64 { if a < 0 { -1i64 } else { 0i64 } } else { a.wrapping_shr(b as u32) };
                    stack_set(stack, bp + inst.a as usize, result as u64);
                }
                Opcode::ShrU => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b < 0 {
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NegativeShift));
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
                        handle_loop_osr!(target_pc);
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
                // flags: bit0=unsigned, bit1=decrement, bit2=inclusive
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
                    
                    // Compare: flags bit0=unsigned, bit2=inclusive
                    let unsigned = (flags & 0x01) != 0;
                    let inclusive = (flags & 0x04) != 0;
                    let (ni, li) = (next_idx as i64, limit as i64);
                    let continue_loop = match (decrement, unsigned, inclusive) {
                        // Increment: i < limit or i <= limit
                        (false, false, false) => ni < li,
                        (false, false, true)  => ni <= li,
                        (false, true, false)  => next_idx < limit,
                        (false, true, true)   => next_idx <= limit,
                        // Decrement: i > limit or i >= limit
                        (true, false, false)  => ni > li,
                        (true, false, true)   => ni >= li,
                        (true, true, false)   => next_idx > limit,
                        (true, true, true)    => next_idx >= limit,
                    };
                    
                    if continue_loop {
                        let target_pc = (frame.pc as i64 + offset as i64) as usize;
                        
                        #[cfg(feature = "jit")]
                        handle_loop_osr!(target_pc);
                        
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
                                let result = jit::dispatch_jit_call(
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
                    use vo_runtime::ffi::{ExternResult, ExternInvoke, ExternWorld, ExternFiberInputs};
                    // CallExtern: a=dst, b=extern_id, c=args_start, flags=arg_count
                    let extern_id = inst.b as u32;
                    let vm_ptr = self as *mut Vm as *mut core::ffi::c_void;
                    let fiber_ptr = fiber as *mut crate::fiber::Fiber as *mut core::ffi::c_void;
                    #[cfg(feature = "std")]
                    let resume_io_token = fiber.resume_io_token.take();
                    let resume_host_event_token = fiber.resume_host_event_token.take();
                    let (closure_replay_results, closure_replay_panicked) = fiber.closure_replay.take_for_extern();
                    let invoke = ExternInvoke {
                        extern_id,
                        bp: bp as u32,
                        arg_start: inst.c,
                        arg_slots: inst.flags as u16,
                        ret_start: inst.a,
                        ret_slots: 0, // not used by current dispatch
                    };
                    let world = ExternWorld {
                        gc: &mut self.state.gc,
                        module,
                        itab_cache: &mut self.state.itab_cache,
                        vm_opaque: vm_ptr,
                        program_args: &self.state.program_args,
                        sentinel_errors: &mut self.state.sentinel_errors,
                        #[cfg(feature = "std")]
                        io: &mut self.state.io,
                    };
                    let fiber_inputs = ExternFiberInputs {
                        fiber_opaque: fiber_ptr,
                        #[cfg(feature = "std")]
                        resume_io_token,
                        resume_host_event_token,
                        replay_results: closure_replay_results,
                        replay_panicked: closure_replay_panicked,
                    };
                    let extern_result = self.state.extern_registry.call(
                        &mut fiber.stack, invoke, world, fiber_inputs,
                    );
                    match extern_result {
                        ExternResult::Ok => { refetch!(); }
                        ExternResult::Panic(msg) => {
                            let r = runtime_panic_msg(&mut self.state.gc, fiber, stack, module, msg);
                            if matches!(r, ExecResult::FrameChanged) { refetch!(); } else { return r; }
                        }
                        ExternResult::NotRegistered(id) => {
                            let name = &module.externs[extern_id as usize].name;
                            let msg = format!("extern function '{}' (id={}) not registered", name, id);
                            let r = runtime_panic_msg(&mut self.state.gc, fiber, stack, module, msg);
                            if matches!(r, ExecResult::FrameChanged) { refetch!(); } else { return r; }
                        }
                        ExternResult::Yield => { return ExecResult::TimesliceExpired; }
                        ExternResult::Block => { return ExecResult::Block(crate::fiber::BlockReason::Queue); }
                        ExternResult::HostEventWait { token, delay_ms } => {
                            self.scheduler.block_for_host_event(token, delay_ms);
                            return ExecResult::TimesliceExpired;
                        }
                        ExternResult::HostEventWaitAndReplay { token } => {
                            // Undo PC so extern replays on wake (like CallClosure)
                            let frame = fiber.current_frame_mut().unwrap();
                            frame.pc -= 1;
                            self.scheduler.block_for_host_event_replay(token);
                            return ExecResult::TimesliceExpired;
                        }
                        #[cfg(feature = "std")]
                        ExternResult::WaitIo { token } => {
                            return ExecResult::Block(crate::fiber::BlockReason::Io(token));
                        }
                        ExternResult::CallClosure { closure_ref, args } => {
                            // Undo PC pre-increment so extern replays on return
                            let frame = fiber.current_frame_mut().unwrap();
                            frame.pc -= 1;
                            
                            // Push closure frame on current fiber using same layout as exec_call_closure
                            let closure_func_id = vo_runtime::objects::closure::func_id(closure_ref);
                            let func_def = &module.functions[closure_func_id as usize];
                            
                            let new_bp = fiber.sp;
                            let local_slots = func_def.local_slots as usize;
                            let new_sp = new_bp + local_slots;
                            fiber.ensure_capacity(new_sp);
                            // Zero frame slots: stale values in GcRef-typed slots cause GC segfault.
                            fiber.stack[new_bp..new_sp].fill(0);
                            
                            // Use call_layout for correct slot placement (matches exec_call_closure)
                            let layout = vo_runtime::objects::closure::call_layout(
                                closure_ref as u64,
                                closure_ref,
                                func_def.recv_slots as usize,
                                func_def.is_closure,
                            );
                            
                            let fstack = fiber.stack_ptr();
                            if let Some(slot0_val) = layout.slot0 {
                                helpers::stack_set(fstack, new_bp, slot0_val);
                            }
                            
                            // Copy args at the correct offset
                            for (i, &arg) in args.iter().enumerate() {
                                helpers::stack_set(fstack, new_bp + layout.arg_offset + i, arg);
                            }
                            
                            fiber.sp = new_sp;
                            fiber.frames.push(crate::fiber::CallFrame::new(
                                closure_func_id,
                                new_bp,
                                0, // ret_reg=0 (return values go via replay cache, not caller stack)
                                func_def.ret_slots,
                            ));
                            
                            // Mark replay depth so return path knows to cache results.
                            // Push previous depth so nested CallExterns don't clobber it.
                            fiber.closure_replay.push_depth(fiber.frames.len());
                            
                            stack = fiber.stack_ptr();
                            refetch!();
                        }
                    }
                }
                Opcode::CallClosure => {
                    let closure_ref = stack_get(stack, bp + inst.a as usize) as vo_runtime::gc::GcRef;
                    if closure_ref.is_null() {
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NilFuncCall));
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
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc, fiber, stack, module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!("runtime error: index out of range [{}] with length {}", idx, len)
                        ));
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
                        handle_panic_result!(runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::IndexOutOfBounds,
                            format!("runtime error: index out of range [{}] with length {}", idx, len)));
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
                        handle_panic_result!(runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::IndexOutOfBounds,
                            format!("runtime error: index out of range [{}] with length {}", idx, len)));
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
                        handle_panic_result!(runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::MakeSlice, msg));
                    }
                }
                Opcode::SliceGet => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.c as usize) as usize;
                    let len = if s.is_null() { 0 } else { slice_len(s) };
                    if idx >= len {
                        handle_panic_result!(runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::IndexOutOfBounds,
                            format!("runtime error: index out of range [{}] with length {}", idx, len)));
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
                        handle_panic_result!(runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::IndexOutOfBounds,
                            format!("runtime error: index out of range [{}] with length {}", idx, len)));
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
                        handle_panic_result!(runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::SliceBoundsOutOfRange,
                            format!("runtime error: slice bounds out of range [{}:{}]", lo, hi)));
                    }
                }
                Opcode::SliceAppend => {
                    exec::exec_slice_append(stack, bp, &inst, &mut self.state.gc, Some(module));
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
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::NilMapWrite));
                    }
                    if !exec::exec_map_set(stack, bp, &inst, &mut self.state.gc, Some(module)) {
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::UnhashableType));
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
                        handle_panic_result!(runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::MakeChan, msg));
                    }
                }
                Opcode::ChanSend => {
                    let result = Self::handle_chan_result(
                        exec::exec_chan_send(stack, bp, fiber_id.to_raw(), &inst, &mut self.state.gc, Some(module)),
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
                    exec::exec_select_begin(fiber, &inst);
                }
                Opcode::SelectSend => {
                    exec::exec_select_send(&mut fiber.select_state, &inst);
                }
                Opcode::SelectRecv => {
                    exec::exec_select_recv(&mut fiber.select_state, &inst);
                }
                Opcode::SelectExec => {
                    let fiber_id = fiber.id;
                    match exec::exec_select_exec(stack, bp, fiber_id, &mut fiber.select_state, &inst) {
                        exec::SelectResult::Continue => {}
                        exec::SelectResult::Block => {
                            // Waiters have been registered on all channels by exec_select_exec.
                            // Block this fiber - it will be woken when any channel is ready.
                            frame.pc -= 1;
                            return ExecResult::Block(crate::fiber::BlockReason::Queue);
                        }
                        exec::SelectResult::SendOnClosed => {
                            handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::SendOnClosedChannel));
                        }
                        exec::SelectResult::Wake(waiter) => {
                            self.scheduler.wake_channel_waiter(&waiter);
                            return ExecResult::TimesliceExpired;
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
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::TypeAssertionFailed));
                    }
                }
                Opcode::IfaceEq => {
                    let result = exec::exec_iface_eq(stack, bp, &inst, module);
                    if matches!(result, ExecResult::Panic) {
                        handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::UncomparableType));
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
                        handle_panic_result!(runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::IndexOutOfBounds,
                            format!("runtime error: index out of range [{}] with length {}", idx, len)));
                    }
                }

                // === ISLAND/PORT: Cross-island operations ===
                #[cfg(feature = "std")]
                Opcode::IslandNew => {
                    let handle = self.create_island();
                    stack_set(stack, bp + inst.a as usize, handle as u64);
                }
                #[cfg(not(feature = "std"))]
                Opcode::IslandNew => {
                    let _ = exec::exec_island_new(stack, bp, &inst, &mut self.state.gc, 0);
                }
                Opcode::PortNew => {
                    if let Err(msg) = exec::exec_port_new(stack, bp, &inst, &mut self.state.gc) {
                        handle_panic_result!(runtime_panic(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::MakePort, msg));
                    }
                }
                #[cfg(feature = "std")]
                Opcode::PortSend => {
                    let island_id = self.state.current_island_id;
                    let fid = fiber_id.to_raw() as u64;
                    let result = exec::exec_port_send(stack, bp, island_id, fid, &inst, &self.state.gc, &module.struct_metas, &module.runtime_types);
                    let action = Self::process_port_result(result, fiber, false);
                    handle_port_action!(action);
                }
                #[cfg(not(feature = "std"))]
                Opcode::PortSend => {
                    handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::PortNotSupported));
                }
                #[cfg(feature = "std")]
                Opcode::PortRecv => {
                    let island_id = self.state.current_island_id;
                    let fid = fiber_id.to_raw() as u64;
                    let result = exec::exec_port_recv(stack, bp, island_id, fid, &inst, &mut self.state.gc, &module.struct_metas, &module.runtime_types);
                    let action = Self::process_port_result(result, fiber, true);
                    handle_port_action!(action);
                }
                #[cfg(not(feature = "std"))]
                Opcode::PortRecv => {
                    handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::PortNotSupported));
                }
                #[cfg(feature = "std")]
                Opcode::PortClose => {
                    let result = exec::exec_port_close(stack, bp, &inst);
                    let action = Self::process_port_result(result, fiber, false);
                    handle_port_action!(action);
                }
                #[cfg(not(feature = "std"))]
                Opcode::PortClose => {
                    match exec::exec_port_close(stack, bp, &inst) {
                        exec::PortResult::Continue => {}
                        exec::PortResult::CloseNil => {
                            handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::CloseNilChannel));
                        }
                        exec::PortResult::NotSupported => {
                            handle_panic_result!(runtime_trap(&mut self.state.gc, fiber, stack, module, RuntimeTrapKind::PortNotSupported));
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

    /// Spawn a new fiber that calls a function with the given arguments.
    /// The fiber is added to the ready queue and will be executed by run_scheduled().
    /// Reuses a dead fiber's stack allocation when available to avoid repeated 64KB allocs.
    pub fn spawn_call(&mut self, func_id: u32, args: &[u64]) {
        let module = self.module.as_ref().expect("spawn_call: module not set");
        let func_def = &module.functions[func_id as usize];

        let fiber_id = self.scheduler.reuse_or_spawn();
        let fiber = self.scheduler.get_fiber_mut(fiber_id);

        let bp = fiber.sp;
        let local_slots = func_def.local_slots as usize;
        let new_sp = bp + local_slots;
        fiber.ensure_capacity(new_sp);

        // Zero locals, then copy args
        unsafe { core::ptr::write_bytes(fiber.stack.as_mut_ptr().add(bp), 0, local_slots) };
        let n = (func_def.param_slots as usize).min(args.len());
        fiber.stack[bp..bp + n].copy_from_slice(&args[..n]);

        fiber.sp = new_sp;
        fiber.frames.push(CallFrame::new(func_id, bp, 0, func_def.ret_slots));
    }
}


impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}
