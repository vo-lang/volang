//! Virtual machine main structure.

#[cfg(not(feature = "std"))]
use alloc::collections::VecDeque;
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(feature = "std")]
use std::collections::VecDeque;
#[cfg(feature = "std")]
use std::string::String;

#[cfg(feature = "std")]
use std::vec::Vec;

use vo_runtime::gc::GcRef;
use vo_runtime::objects::{array, string};

pub mod helpers;
mod island_shared;
#[cfg(feature = "std")]
pub mod island_thread;
#[cfg(feature = "jit")]
mod jit;
mod types;

pub use helpers::{stack_get, stack_set};
pub use types::EndpointRegistry;
#[cfg(feature = "std")]
pub use types::IslandThread;
pub use types::{
    ErrorLocation, ExecResult, RuntimeTrapKind, SchedulingOutcome, VmError, VmGcStepStats,
    VmRootScanMode, VmRootScanSnapshot, VmState, TIME_SLICE,
};

use helpers::{
    runtime_panic, runtime_panic_msg, runtime_trap, slice_cap, slice_data_ptr, slice_len,
    string_index, string_len, user_panic,
};

use crate::bytecode::{FunctionDef, Module};
use crate::exec;
use crate::fiber::{Fiber, FiberCapacityError};
/// Result of wait_for_work() — what the scheduling loop should do next.
enum WaitResult {
    /// Work became available, retry the loop.
    Retry,
    /// All fibers completed normally.
    Done,
    #[cfg(feature = "std")]
    Interrupted,
    /// All fibers blocked (potential deadlock).
    Blocked,
    /// Fibers are blocked waiting for host-routed island commands/responses.
    Suspended,
    /// Some fibers waiting for host-side events; async loop must handle them.
    SuspendedForHostEvents,
    /// Island VM should return to its command loop.
    #[cfg(feature = "std")]
    Break,
}

#[inline]
fn exec_result_allows_gc_step(result: &ExecResult) -> bool {
    !matches!(result, ExecResult::Block(_))
}

#[inline]
fn exec_result_marks_gc_fiber_roots_dirty(result: &ExecResult) -> bool {
    !matches!(result, ExecResult::Interrupted)
}

fn fiber_capacity_error_to_vm_error(err: FiberCapacityError) -> VmError {
    VmError::RuntimeTrap {
        kind: RuntimeTrapKind::StackOverflow,
        msg: err.message(),
        loc: None,
    }
}

use crate::instruction::{Instruction, Opcode};
use crate::scheduler::Scheduler;
use vo_runtime::itab::ItabCache;

#[cfg(feature = "jit")]
pub mod jit_mgr;

#[cfg(feature = "jit")]
pub use jit_mgr::{JitConfig, JitManager};

pub struct Vm {
    /// JIT manager (only available with "jit" feature).
    ///
    /// Note: VM runtime execution is currently interpreter-only. The JIT manager is kept for
    /// compilation/codegen purposes while the execution integration is being rebuilt.
    /// IMPORTANT: Must be first field so it's dropped LAST (Rust drops in reverse order).
    /// JIT code memory must remain valid while scheduler/fibers are being dropped.
    #[cfg(feature = "jit")]
    pub jit_mgr: Option<JitManager>,
    #[cfg(feature = "std")]
    extension_loader: Option<vo_runtime::ext_loader::ExtensionLoader>,
    #[cfg(feature = "std")]
    extension_specs: Option<Vec<vo_runtime::ext_loader::NativeExtensionSpec>>,
    pub module: Option<Module>,
    pub scheduler: Scheduler,
    pub state: VmState,
}

#[cfg(feature = "std")]
fn validate_externs_registered(
    registry: &vo_runtime::ExternRegistry,
    externs: &[vo_runtime::bytecode::ExternDef],
) -> Result<(), VmError> {
    let mut missing: Vec<(usize, &str)> = Vec::new();
    for (id, def) in externs.iter().enumerate() {
        if !registry.has(id as u32) {
            missing.push((id, def.name.as_str()));
        }
    }

    if missing.is_empty() {
        return Ok(());
    }

    let mut msg = String::from("unresolved extern functions:\n");
    for (id, name) in missing {
        msg.push_str(&format!("  - [{}] {}\n", id, name));
    }
    Err(VmError::Jit(msg))
}

#[cfg(debug_assertions)]
#[allow(clippy::too_many_arguments)]
fn debug_validate_extern_returns(
    gc: &vo_runtime::gc::Gc,
    module: &Module,
    fiber: &Fiber,
    fiber_id: crate::scheduler::FiberId,
    func_id: u32,
    extern_id: u32,
    bp: usize,
    inst: &Instruction,
) -> Result<(), String> {
    let Some(extern_def) = module.externs.get(extern_id as usize) else {
        return Ok(());
    };
    let Some(func) = module.functions.get(func_id as usize) else {
        return Ok(());
    };

    let ret_start = inst.a as usize;
    let ret_end = ret_start.saturating_add(extern_def.ret_slots as usize);
    let scan_end = ret_end.min(func.slot_types.len());
    let mut slot_idx = ret_start;
    while slot_idx < scan_end {
        let Some(slot_type) = func.slot_types.get(slot_idx) else {
            return Err(format!(
                "CallExtern return slot metadata missing caller_func={} caller_name={} extern={} ret_slot={}",
                func_id, func.name, extern_id, slot_idx
            ));
        };
        match *slot_type {
            vo_runtime::SlotType::GcRef => {
                let Some(stack_idx) = bp.checked_add(slot_idx) else {
                    return Err(format!(
                        "CallExtern return stack index overflow caller_func={} caller_name={} extern={} ret_slot={}",
                        func_id, func.name, extern_id, slot_idx
                    ));
                };
                let Some(&raw) = fiber.stack.get(stack_idx) else {
                    return Err(format!(
                        "CallExtern return stack index {} out of bounds for stack length {} caller_func={} caller_name={} extern={}",
                        stack_idx,
                        fiber.stack.len(),
                        func_id,
                        func.name,
                        extern_id
                    ));
                };
                if raw != 0 && gc.canonicalize_ref(raw as GcRef).is_none() {
                    let (in_all, in_index, index_len) = gc.debug_ref_membership(raw as GcRef);
                    return Err(format!(
                        "CallExtern returned invalid GcRef fiber={} caller_func={} caller_name={} extern={} extern_name={} ret_slot={} raw=0x{:016x} in_all_objects={} in_object_index={} object_index_len={}",
                        fiber_id.to_raw(),
                        func_id,
                        func.name,
                        extern_id,
                        extern_def.name,
                        slot_idx,
                        raw,
                        in_all,
                        in_index,
                        index_len,
                    ));
                }
                slot_idx += 1;
            }
            vo_runtime::SlotType::Interface0 => {
                if slot_idx + 1 >= ret_end || slot_idx + 1 >= fiber.stack.len().saturating_sub(bp) {
                    slot_idx += 1;
                    continue;
                }
                let Some(stack_idx0) = bp.checked_add(slot_idx) else {
                    return Err(format!(
                        "CallExtern interface return stack index overflow caller_func={} caller_name={} extern={} ret_slot={}",
                        func_id, func.name, extern_id, slot_idx
                    ));
                };
                let Some(stack_idx1) = stack_idx0.checked_add(1) else {
                    return Err(format!(
                        "CallExtern interface return pair index overflow caller_func={} caller_name={} extern={} ret_slot={}",
                        func_id, func.name, extern_id, slot_idx
                    ));
                };
                let Some(&slot0) = fiber.stack.get(stack_idx0) else {
                    return Err(format!(
                        "CallExtern interface return stack index {} out of bounds for stack length {} caller_func={} caller_name={} extern={}",
                        stack_idx0,
                        fiber.stack.len(),
                        func_id,
                        func.name,
                        extern_id
                    ));
                };
                let Some(&slot1) = fiber.stack.get(stack_idx1) else {
                    return Err(format!(
                        "CallExtern interface return stack index {} out of bounds for stack length {} caller_func={} caller_name={} extern={}",
                        stack_idx1,
                        fiber.stack.len(),
                        func_id,
                        func.name,
                        extern_id
                    ));
                };
                if vo_runtime::objects::interface::data_is_gc_ref(slot0)
                    && slot1 != 0
                    && gc.canonicalize_ref(slot1 as GcRef).is_none()
                {
                    let (in_all, in_index, index_len) = gc.debug_ref_membership(slot1 as GcRef);
                    return Err(format!(
                        "CallExtern returned invalid interface GcRef fiber={} caller_func={} caller_name={} extern={} extern_name={} ret_slot={} raw=0x{:016x} in_all_objects={} in_object_index={} object_index_len={}",
                        fiber_id.to_raw(),
                        func_id,
                        func.name,
                        extern_id,
                        extern_def.name,
                        slot_idx + 1,
                        slot1,
                        in_all,
                        in_index,
                        index_len,
                    ));
                }
                slot_idx += 2;
            }
            _ => {
                slot_idx += 1;
            }
        }
    }
    Ok(())
}

fn check_extern_frame_range(
    op: &'static str,
    func: &FunctionDef,
    bp: usize,
    stack_len: usize,
    start: u16,
    count: u16,
) -> Result<(), String> {
    if count == 0 {
        return Ok(());
    }

    let start = start as usize;
    let count = count as usize;
    let Some(end) = start.checked_add(count) else {
        return Err(format!(
            "CallExtern {op} range {start}..+{count} overflows slot index space in function {}",
            func.name
        ));
    };
    let local_slots = func.local_slots as usize;
    if end > local_slots {
        return Err(format!(
            "CallExtern {op} range {start}..{end} out of bounds for function {} with {local_slots} local slots",
            func.name
        ));
    }
    let Some(stack_end) = bp.checked_add(end) else {
        return Err(format!(
            "CallExtern {op} stack range bp {bp} + end {end} overflows stack index space in function {}",
            func.name
        ));
    };
    if stack_end > stack_len {
        return Err(format!(
            "CallExtern {op} stack range {}..{} out of bounds for stack length {stack_len} in function {}",
            bp + start,
            stack_end,
            func.name
        ));
    }
    Ok(())
}

impl Vm {
    pub fn new() -> Self {
        Self {
            #[cfg(feature = "jit")]
            jit_mgr: None,
            #[cfg(feature = "std")]
            extension_loader: None,
            #[cfg(feature = "std")]
            extension_specs: None,
            module: None,
            scheduler: Scheduler::new(),
            state: VmState::new(),
        }
    }

    #[cfg(feature = "std")]
    pub fn enable_external_island_transport(&mut self) {
        self.state.external_island_transport = true;
    }

    /// Create a VM with custom JIT thresholds.
    ///
    /// This is a best-effort convenience constructor: if JIT initialization
    /// fails, the VM is still created without a JIT manager. Use
    /// [`Vm::try_with_jit_config`] for strict `RunMode::Jit` paths.
    #[cfg(feature = "jit")]
    pub fn with_jit_thresholds(call_threshold: u32, loop_threshold: u32) -> Self {
        Self::with_best_effort_jit_config(JitConfig {
            call_threshold,
            loop_threshold,
            ..Default::default()
        })
    }

    #[cfg(not(feature = "jit"))]
    pub fn with_jit_thresholds(_call_threshold: u32, _loop_threshold: u32) -> Self {
        Self::new()
    }

    /// Create a VM with custom JIT configuration, best effort.
    ///
    /// This deliberately preserves the legacy non-strict API: JIT
    /// initialization errors are swallowed and the VM runs interpreter-only.
    /// Strict execution paths must call [`Vm::try_with_jit_config`] instead.
    #[cfg(feature = "jit")]
    pub fn with_best_effort_jit_config(config: JitConfig) -> Self {
        let mut vm = Self::new();
        if let Ok(mgr) = JitManager::with_config(config) {
            vm.jit_mgr = Some(mgr);
        }
        vm
    }

    /// Deprecated alias for [`Vm::with_best_effort_jit_config`].
    ///
    /// This method is non-strict and may return a VM without JIT enabled.
    /// New strict callers should use [`Vm::try_with_jit_config`].
    #[cfg(feature = "jit")]
    #[deprecated(
        note = "non-strict best-effort API; use try_with_jit_config for strict JIT or with_best_effort_jit_config for explicit fallback"
    )]
    pub fn with_jit_config(config: JitConfig) -> Self {
        Self::with_best_effort_jit_config(config)
    }

    #[cfg(feature = "jit")]
    #[allow(clippy::result_large_err)]
    pub fn try_with_jit_config(config: JitConfig) -> Result<Self, vo_jit::JitError> {
        let mut vm = Self::new();
        vm.jit_mgr = Some(JitManager::with_config(config)?);
        Ok(vm)
    }

    /// Strictly initialize the JIT compiler.
    ///
    /// Does nothing if a JIT manager already exists.
    #[cfg(feature = "jit")]
    #[allow(clippy::result_large_err)]
    pub fn try_init_jit(&mut self) -> Result<(), vo_jit::JitError> {
        if self.jit_mgr.is_some() {
            return Ok(());
        }
        self.jit_mgr = Some(JitManager::new()?);
        Ok(())
    }

    /// Best-effort legacy JIT initialization.
    ///
    /// This preserves the old non-strict behavior for embedding callers that
    /// opportunistically enable JIT. It prints a warning on failure and leaves
    /// the VM interpreter-only. Strict run paths must use [`Vm::try_init_jit`]
    /// or [`Vm::try_with_jit_config`].
    #[cfg(feature = "jit")]
    pub fn init_jit(&mut self) {
        if let Err(e) = self.try_init_jit() {
            #[cfg(feature = "std")]
            eprintln!("Warning: best-effort JIT initialization failed: {}", e);
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

    #[cfg(feature = "std")]
    pub fn set_interrupt_flag(
        &mut self,
        interrupt_flag: std::sync::Arc<std::sync::atomic::AtomicBool>,
    ) {
        self.state.interrupt_flag = Some(interrupt_flag);
    }

    #[cfg(feature = "std")]
    fn interrupt_requested(&self) -> bool {
        self.state
            .interrupt_flag
            .as_ref()
            .map(|flag| flag.load(std::sync::atomic::Ordering::SeqCst))
            .unwrap_or(false)
    }

    #[cfg(not(feature = "std"))]
    fn interrupt_requested(&self) -> bool {
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
    pub fn load(&mut self, module: Module) -> Result<(), VmError> {
        self.load_with_extensions(module, None)
    }

    #[cfg(not(feature = "std"))]
    pub fn load(&mut self, module: Module) -> Result<(), VmError> {
        vo_stdlib::register_externs(&mut self.state.extern_registry, &module.externs);

        self.finish_load(module);
        Ok(())
    }

    /// Load a module with optional extension loader for native extensions.
    #[cfg(feature = "std")]
    pub fn load_with_extensions(
        &mut self,
        module: Module,
        ext_loader: Option<vo_runtime::ext_loader::ExtensionLoader>,
    ) -> Result<(), VmError> {
        #[cfg(not(target_arch = "wasm32"))]
        {
            vo_stdlib::register_externs(&mut self.state.extern_registry, &module.externs);
        }

        // Register extern functions from linkme distributed slices
        self.state
            .extern_registry
            .register_from_linkme(&module.externs);

        // Register extern functions from extension loader (if provided)
        if let Some(loader) = ext_loader.as_ref() {
            self.state
                .extern_registry
                .register_from_extension_loader(loader, &module.externs);
        }

        validate_externs_registered(&self.state.extern_registry, &module.externs)?;

        self.extension_specs = ext_loader.as_ref().map(|loader| loader.specs().to_vec());
        self.extension_loader = ext_loader;

        self.finish_load(module);
        Ok(())
    }

    /// Broadcast a host bridge pointer to all loaded extension dylibs.
    ///
    /// Each dylib that exports `vo_ext_set_host_bridge` will receive the
    /// raw pointer.  Call this after `vo_ext::host::install` on the host side.
    ///
    /// # Safety
    ///
    /// `ptr` must come from `vo_ext::host::encode_bridge_ptr` and the
    /// bridge must remain alive until `clear_extension_bridges`.
    #[cfg(feature = "std")]
    pub unsafe fn broadcast_bridge(&self, ptr: usize) {
        if let Some(loader) = &self.extension_loader {
            loader.broadcast_bridge(ptr);
        }
    }

    /// Clear the host bridge reference from all loaded extension dylibs.
    #[cfg(feature = "std")]
    pub fn clear_extension_bridges(&self) {
        if let Some(loader) = &self.extension_loader {
            loader.clear_bridge_all();
        }
    }

    /// Finish loading a module (shared by load and load_with_extensions).
    fn finish_load(&mut self, module: Module) {
        let total_global_slots: usize = module.globals.iter().map(|g| g.slots as usize).sum();
        self.state.globals = vec![0u64; total_global_slots];
        self.state.mark_gc_all_roots_dirty();
        self.state.gc_root_scan = None;
        self.state.last_gc_step_stats = VmGcStepStats::default();
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
    pub fn create_island(&mut self) -> Result<GcRef, VmError> {
        let next_id = self.state.next_island_id;
        if self.state.external_island_transport {
            self.state.next_island_id += 1;
            return Ok(vo_runtime::island::create(&mut self.state.gc, next_id));
        }

        use vo_runtime::island_transport::{InThreadTransport, IslandSender};

        let module = self
            .module
            .as_ref()
            .ok_or_else(|| VmError::Jit("create_island requires loaded module".to_string()))?;
        self.state.next_island_id += 1;

        // Create transport pair for the new island
        let (island_sender, island_transport) = InThreadTransport::new();
        let island_sender: std::sync::Arc<dyn IslandSender> = std::sync::Arc::new(island_sender);
        let handle = vo_runtime::island::create(&mut self.state.gc, next_id);

        // Initialize registry and main transport if first island
        if self.state.island_registry.is_none() {
            let (main_sender, main_transport) = InThreadTransport::new();
            let main_sender: std::sync::Arc<dyn IslandSender> = std::sync::Arc::new(main_sender);
            let mut registry = std::collections::HashMap::new();
            registry.insert(0u32, main_sender.clone());
            self.state.island_registry = Some(std::sync::Arc::new(std::sync::Mutex::new(registry)));
            self.state.main_transport = Some(Box::new(main_transport));
            // Also register main island in island_senders
            self.state.island_senders.insert(0, main_sender);
        }

        // Register this island's sender in the shared registry
        let registry = self
            .state
            .island_registry
            .as_ref()
            .ok_or_else(|| VmError::Jit("create_island missing island registry".to_string()))?
            .clone();
        {
            let mut guard = registry
                .lock()
                .map_err(|_| VmError::Jit("create_island island registry poisoned".to_string()))?;
            guard.insert(next_id, island_sender.clone());
        }
        // Also register in island_senders
        self.state
            .island_senders
            .insert(next_id, island_sender.clone());

        // Spawn island thread with JIT config from main VM
        let module_arc = std::sync::Arc::new(module.clone());
        let registry_clone = registry.clone();
        let extension_specs = self.extension_specs.clone().unwrap_or_default();
        #[cfg(feature = "jit")]
        let jit_config = self.jit_mgr.as_ref().map(|mgr| mgr.config().clone());
        let join_handle = std::thread::spawn(move || {
            #[cfg(feature = "jit")]
            island_thread::run_island_thread(
                next_id,
                module_arc,
                island_transport,
                registry_clone,
                extension_specs,
                jit_config,
            );
            #[cfg(not(feature = "jit"))]
            island_thread::run_island_thread(
                next_id,
                module_arc,
                island_transport,
                registry_clone,
                extension_specs,
            );
        });

        // Save thread handle
        self.state.island_threads.push(IslandThread {
            island_id: next_id,
            join_handle: Some(join_handle),
        });

        Ok(handle)
    }

    /// Spawn the entry function as a new fiber.  Called by `run()` only.
    fn spawn_entry(&mut self) -> Result<(), VmError> {
        let module = self.module.as_ref().ok_or(VmError::NoEntryFunction)?;
        let entry_func = module.entry_func;
        let func = module
            .functions
            .get(entry_func as usize)
            .ok_or(VmError::InvalidFunctionId(entry_func))?;
        let mut fiber = Fiber::new(0);
        fiber.push_frame(entry_func, func.local_slots, func.gc_scan_slots, 0, 0);
        self.scheduler.spawn(fiber);
        Ok(())
    }

    /// Spawn the entry function and run all fibers.
    ///
    /// Returns `Ok(outcome)` where outcome is one of:
    /// - `Completed`              — program exited normally
    /// - `Blocked`                — all goroutines stuck on channels; call `deadlock_err()` for details
    /// - `Suspended`              — waiting for async host callbacks (WASM timer/HTTP, GUI events)
    ///
    /// Callers decide whether `Blocked` is a deadlock error or expected behaviour (e.g. GUI host VM).
    pub fn run(&mut self) -> Result<SchedulingOutcome, VmError> {
        self.spawn_entry()?;
        self.run_scheduling_loop(None)
    }

    /// Run island initialization only (global vars + user init functions, no main).
    ///
    /// Must be called on island VMs before processing SpawnFiber commands,
    /// otherwise global variables (including interface values) remain zero-initialized.
    pub fn run_init(&mut self) -> Result<SchedulingOutcome, VmError> {
        let module = self.module.as_ref().ok_or(VmError::NoEntryFunction)?;
        let init_func = module.island_init_func;
        let func = module
            .functions
            .get(init_func as usize)
            .ok_or(VmError::InvalidFunctionId(init_func))?;
        let mut fiber = Fiber::new(0);
        fiber.push_frame(init_func, func.local_slots, func.gc_scan_slots, 0, 0);
        self.scheduler.spawn(fiber);
        self.run_scheduling_loop(None)
    }

    /// Run existing fibers without spawning an entry fiber.
    ///
    /// Used for event dispatch after initial `run()`, island command handlers, and WASM async
    /// continuation.  Same outcome semantics as `run()`.
    pub fn run_scheduled(&mut self) -> Result<SchedulingOutcome, VmError> {
        self.run_scheduling_loop(None)
    }

    pub fn push_island_command(&mut self, cmd: vo_runtime::island::IslandCommand) {
        self.mark_gc_all_roots_dirty();
        self.state.command_queue.push_back(cmd);
    }

    pub fn take_outbound_commands(&mut self) -> VecDeque<(u32, vo_runtime::island::IslandCommand)> {
        core::mem::take(&mut self.state.outbound_commands)
    }

    pub fn current_island_id(&self) -> u32 {
        self.state.current_island_id
    }

    pub fn set_island_id(&mut self, id: u32) {
        self.state.current_island_id = id;
        if self.state.next_island_id <= id {
            self.state.next_island_id = id + 1;
        }
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
    fn run_scheduling_loop(
        &mut self,
        max_iterations: Option<usize>,
    ) -> Result<SchedulingOutcome, VmError> {
        let mut iterations = 0;

        loop {
            if self.interrupt_requested() {
                return Err(VmError::Interrupted);
            }
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
                    #[cfg(feature = "std")]
                    WaitResult::Interrupted => return Err(VmError::Interrupted),
                    WaitResult::Blocked => return Ok(SchedulingOutcome::Blocked),
                    WaitResult::Suspended => return Ok(SchedulingOutcome::Suspended),
                    WaitResult::SuspendedForHostEvents => {
                        return Ok(SchedulingOutcome::SuspendedForHostEvents)
                    }
                    #[cfg(feature = "std")]
                    WaitResult::Break => break,
                }
            }

            let fiber_id = match self.scheduler.schedule_next() {
                Some(id) => id,
                None => break,
            };

            let result = self.run_fiber(fiber_id);
            let gc_after_boundary = exec_result_allows_gc_step(&result);
            let mark_gc_fiber_roots_dirty = exec_result_marks_gc_fiber_roots_dirty(&result);

            let handled = self.handle_exec_result(result, max_iterations.is_some());
            // GC step at the scheduling boundary after the current fiber has
            // yielded/blocked/done. Stacks are stable here, and a newly-woken
            // fiber can handle latency-sensitive work (for example a render
            // frame request) before incremental GC uses the remaining slice.
            //
            // If this boundary parked the fiber on an external queue/event, return
            // to the host first. Running a GC slice after the app has reached its
            // next receive point can delay the remote sender that is supposed to
            // wake it, which shows up as request-send stalls in split render loops.
            if !matches!(handled, Some(Err(_))) {
                if mark_gc_fiber_roots_dirty {
                    self.mark_gc_fiber_roots_dirty(fiber_id);
                }
                if gc_after_boundary {
                    self.gc_step_after_fiber(None);
                }
            }
            match handled {
                None => {} // continue scheduling
                Some(Ok(outcome)) => return Ok(outcome),
                Some(Err(e)) => return Err(e),
            }
        }

        Ok(SchedulingOutcome::Completed)
    }

    /// Process commands from other island threads (non-blocking).
    #[inline]
    fn process_island_commands(&mut self) {
        let mut cmds = Vec::new();
        #[cfg(feature = "std")]
        if let Some(ref transport) = self.state.main_transport {
            while let Ok(Some(cmd)) = transport.try_recv() {
                cmds.push(cmd);
            }
        }
        while let Some(cmd) = self.state.command_queue.pop_front() {
            cmds.push(cmd);
        }
        if !cmds.is_empty() {
            self.mark_gc_all_roots_dirty();
        }
        for cmd in cmds {
            self.dispatch_island_command(cmd);
        }
        self.state.clear_endpoint_tombstones_if_quiescent();
    }

    /// Dispatch a single island command on the main island.
    fn dispatch_island_command(&mut self, cmd: vo_runtime::island::IslandCommand) {
        use vo_runtime::island::IslandCommand;
        match cmd {
            IslandCommand::SpawnFiber { closure_data } => {
                island_shared::handle_spawn_fiber(self, closure_data.data());
            }
            IslandCommand::WakeFiber { fiber_id } => {
                self.scheduler
                    .wake_fiber(crate::scheduler::FiberId::from_raw(fiber_id));
            }
            IslandCommand::EndpointRequest {
                endpoint_id,
                kind,
                from_island,
                fiber_id,
            } => {
                island_shared::handle_endpoint_request_command(
                    self,
                    endpoint_id,
                    kind,
                    from_island,
                    fiber_id,
                );
            }
            IslandCommand::EndpointResponse {
                endpoint_id,
                kind,
                fiber_id,
            } => {
                island_shared::handle_endpoint_response_command(self, endpoint_id, kind, fiber_id);
            }
            IslandCommand::Shutdown => {}
        }
    }

    /// When no fibers are runnable, try to make progress via I/O polling or
    /// island command waiting. Returns what the scheduling loop should do next.
    fn wait_for_work(&mut self) -> WaitResult {
        #[cfg(feature = "std")]
        if self.interrupt_requested() {
            return WaitResult::Interrupted;
        }
        // Try I/O polling first
        #[cfg(feature = "std")]
        {
            if self.scheduler.poll_io(&mut self.state.io) > 0 {
                self.mark_gc_all_roots_dirty();
                return WaitResult::Retry;
            }
        }

        if !self.state.command_queue.is_empty() {
            return WaitResult::Retry;
        }

        // Try waiting for island commands
        #[cfg(feature = "std")]
        if self.scheduler.has_blocked() && self.state.main_transport.is_some() {
            if let Some(ref transport) = self.state.main_transport {
                match transport.recv_timeout(std::time::Duration::from_millis(100)) {
                    Ok(cmd) => {
                        self.mark_gc_all_roots_dirty();
                        self.dispatch_island_command(cmd);
                        self.state.clear_endpoint_tombstones_if_quiescent();
                        return WaitResult::Retry;
                    }
                    Err(vo_runtime::island_transport::TransportError::Timeout) => {
                        self.scheduler.poll_io(&mut self.state.io);
                        return WaitResult::Retry;
                    }
                    Err(vo_runtime::island_transport::TransportError::Disconnected) => {
                        self.state.clear_endpoint_tombstones_if_quiescent();
                        return WaitResult::Break;
                    }
                }
            }
        }

        // If the only blocked fibers are host event waiters, signal the async loop.
        if self.scheduler.has_host_event_waiters() {
            self.state.clear_endpoint_tombstones_if_quiescent();
            return WaitResult::SuspendedForHostEvents;
        }

        if !self.state.outbound_commands.is_empty() || self.state.pending_island_responses > 0 {
            return WaitResult::Suspended;
        }

        // Check if there are waiters that might still make progress
        #[cfg(feature = "std")]
        if self.scheduler.has_io_waiters() || self.scheduler.has_blocked() {
            if self.state.current_island_id != 0 {
                if self.scheduler.has_io_waiters() {
                    self.scheduler.poll_io(&mut self.state.io);
                }
                self.state.clear_endpoint_tombstones_if_quiescent();
                return WaitResult::Break;
            }
            if !self.scheduler.has_io_waiters() && self.state.main_transport.is_none() {
                // If there are live cross-island endpoints, blocked fibers may be
                // waiting for remote island responses delivered via push_island_command.
                // Return Suspended so the host event loop keeps running.
                if self.state.endpoint_registry.has_live() {
                    self.state.clear_endpoint_tombstones_if_quiescent();
                    return WaitResult::Suspended;
                }
                self.state.clear_endpoint_tombstones_if_quiescent();
                return WaitResult::Blocked;
            }
            self.scheduler.poll_io(&mut self.state.io);
            std::thread::sleep(std::time::Duration::from_millis(10));
            return WaitResult::Retry;
        }

        #[cfg(not(feature = "std"))]
        if self.scheduler.has_blocked() {
            if self.state.endpoint_registry.has_live() {
                self.state.clear_endpoint_tombstones_if_quiescent();
                return WaitResult::Suspended;
            }
            self.state.clear_endpoint_tombstones_if_quiescent();
            return WaitResult::Blocked;
        }

        self.state.clear_endpoint_tombstones_if_quiescent();
        WaitResult::Done
    }

    /// Wake a fiber blocked on a host-side event and schedule it to run.
    /// Called by the WASM async run loop after a host event fires.
    pub fn wake_host_event(&mut self, token: u64) {
        self.mark_gc_all_roots_dirty();
        self.scheduler.wake_host_event(token);
    }

    /// Wake a fiber blocked on a host-side event, attaching opaque data.
    /// The FFI function reads the data on replay via `ctx.take_resume_host_event_data()`.
    pub fn wake_host_event_with_data(&mut self, token: u64, data: Vec<u8>) {
        self.mark_gc_all_roots_dirty();
        self.scheduler.wake_host_event_with_data(token, data);
    }

    /// Take the host output bytes written by an FFI function via `ctx.set_host_output()`.
    /// Returns `None` if no output was written since the last take.
    pub fn take_host_output(&mut self) -> Option<Vec<u8>> {
        self.state.host_output.take()
    }

    /// Clear any pending host output without reading it.
    pub fn clear_host_output(&mut self) {
        self.state.host_output = None;
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
            ExecResult::Interrupted => {
                return Some(Err(VmError::Interrupted));
            }
            ExecResult::Block(reason) => match reason {
                crate::fiber::BlockReason::Queue => {
                    self.scheduler.block_for_queue();
                }
                crate::fiber::BlockReason::HostEvent { token, delay_ms } => {
                    self.scheduler.block_for_host_event(token, delay_ms);
                }
                crate::fiber::BlockReason::HostEventReplay(token) => {
                    if let Err(err) = self.rewind_current_frame_for_replay("HostEventReplay") {
                        let _ = self.scheduler.kill_current();
                        return Some(Err(err));
                    }
                    self.scheduler.block_for_host_event_replay(token);
                }
                #[cfg(feature = "std")]
                crate::fiber::BlockReason::Io(token) => {
                    if let Err(err) = self.rewind_current_frame_for_replay("Io") {
                        let _ = self.scheduler.kill_current();
                        return Some(Err(err));
                    }
                    self.scheduler.block_for_io(token);
                }
            },
            ExecResult::Done => {
                let _ = self.scheduler.kill_current();
            }
            ExecResult::Panic => {
                let (trap_kind, msg, loc_tuple) = self.scheduler.kill_current();
                let loc = loc_tuple.map(|(func_id, pc)| ErrorLocation { func_id, pc });
                if !is_bounded {
                    if let Some(kind) = trap_kind {
                        let Some(msg) = msg else {
                            return Some(Err(VmError::Jit(format!(
                                "runtime trap {:?} missing panic payload",
                                kind
                            ))));
                        };
                        return Some(Err(VmError::RuntimeTrap { kind, msg, loc }));
                    }
                    return Some(Err(VmError::PanicUnwound { msg, loc }));
                } else {
                    return Some(Ok(SchedulingOutcome::Panicked));
                }
            }
            ExecResult::JitError(msg) => {
                let _ = self.scheduler.kill_current();
                return Some(Err(VmError::Jit(msg)));
            }
            ExecResult::FrameChanged | ExecResult::CallClosure { .. } => {
                debug_assert!(
                    false,
                    "internal ExecResult leaked to scheduling loop: {:?}",
                    result
                );
                self.scheduler.yield_current();
            }
        }
        None
    }

    fn rewind_current_frame_for_replay(&mut self, label: &str) -> Result<(), VmError> {
        let fiber = self.scheduler.current_fiber_mut().ok_or_else(|| {
            VmError::Jit(format!("{label} requires a current fiber before blocking"))
        })?;
        let frame = fiber.current_frame_mut().ok_or_else(|| {
            VmError::Jit(format!("{label} requires an active frame before blocking"))
        })?;
        frame.pc = frame
            .pc
            .checked_sub(1)
            .ok_or_else(|| VmError::Jit(format!("{label} cannot rewind pc 0")))?;
        Ok(())
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
                    let Some(func) = module.functions.get(frame.func_id as usize) else {
                        msg.push_str(&format!(
                            "    missing function id {} pc={}\n",
                            frame.func_id, frame.pc
                        ));
                        continue;
                    };
                    let code = &func.code;
                    let pc = frame.pc;
                    let prev_pc = pc.saturating_sub(1);
                    if let Some(inst) = code.get(prev_pc) {
                        msg.push_str(&format!(
                            "    at func={} pc={} inst@{}={:?}\n",
                            frame.func_id,
                            pc,
                            prev_pc,
                            inst.opcode()
                        ));
                    }
                    if let Some(inst) = code.get(pc) {
                        msg.push_str(&format!("    next inst@{}={:?}\n", pc, inst.opcode()));
                    }
                }
            }
            Err(VmError::Deadlock(msg))
        } else {
            Err(VmError::Deadlock(
                "vm deadlock: all fibers blocked".to_string(),
            ))
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
        let mut func = match module.functions.get(func_id as usize) {
            Some(func) => func,
            None => {
                return ExecResult::JitError(format!(
                    "active frame references missing function id {func_id}"
                ));
            }
        };
        let mut code: &[Instruction] = &func.code;

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
                func = match module.functions.get(func_id as usize) {
                    Some(func) => func,
                    None => {
                        return ExecResult::JitError(format!(
                            "active frame references missing function id {func_id}"
                        ));
                    }
                };
                code = &func.code;
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
                if let Some(osr_result) = jit::try_loop_osr(self, fiber_id, func_id, $target_pc, bp)
                {
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
                            let Some(token) = fiber.resume_io_token else {
                                return ExecResult::JitError(
                                    "OsrResult::WaitIo without resume_io_token".to_string(),
                                );
                            };
                            return ExecResult::Block(crate::fiber::BlockReason::Io(token));
                        }
                        jit::OsrResult::WaitQueue => {
                            return ExecResult::Block(crate::fiber::BlockReason::Queue);
                        }
                        jit::OsrResult::ExitPc(exit_pc) => {
                            let fiber = self.scheduler.get_fiber_mut(fiber_id);
                            let Some(frame) = fiber.current_frame_mut() else {
                                return ExecResult::JitError(
                                    "OsrResult::ExitPc without active frame".to_string(),
                                );
                            };
                            frame.pc = exit_pc;
                            stack = fiber.stack_ptr();
                            refetch!();
                            continue;
                        }
                        jit::OsrResult::Panic => {
                            let fiber = self.scheduler.get_fiber_mut(fiber_id);
                            stack = fiber.stack_ptr();
                            handle_panic_result!(helpers::panic_unwind(
                                &mut self.state.gc,
                                fiber,
                                stack,
                                module
                            ));
                        }
                        jit::OsrResult::JitError(msg) => {
                            return ExecResult::JitError(msg);
                        }
                    }
                }
            }};
        }

        macro_rules! handle_queue_action {
            ($action:expr) => {
                match $action {
                    exec::QueueAction::Continue => {
                        refetch!();
                    }
                    exec::QueueAction::Block => {
                        return ExecResult::Block(crate::fiber::BlockReason::Queue);
                    }
                    exec::QueueAction::ReplayThenBlock => {
                        let Some(frame) = fiber.current_frame_mut() else {
                            return ExecResult::JitError(
                                "Queue ReplayThenBlock without active frame".to_string(),
                            );
                        };
                        frame.pc = match frame.pc.checked_sub(1) {
                            Some(pc) => pc,
                            None => {
                                return ExecResult::JitError(
                                    "Queue ReplayThenBlock cannot rewind pc 0".to_string(),
                                );
                            }
                        };
                        return ExecResult::Block(crate::fiber::BlockReason::Queue);
                    }
                    exec::QueueAction::Trap(kind) => {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            kind
                        ));
                    }
                    exec::QueueAction::Malformed(msg) => {
                        return ExecResult::JitError(msg);
                    }
                    exec::QueueAction::Wake(waiter) => {
                        self.state.wake_waiter(&waiter, &mut self.scheduler);
                        return ExecResult::TimesliceExpired;
                    }
                    exec::QueueAction::Close {
                        waiters,
                        endpoint_id,
                    } => {
                        for waiter in &waiters {
                            self.state.wake_waiter(waiter, &mut self.scheduler);
                        }
                        if let Some(endpoint_id) = endpoint_id {
                            island_shared::finalize_closed_home_endpoint(self, endpoint_id, None);
                        }
                        return ExecResult::TimesliceExpired;
                    }
                    exec::QueueAction::RemoteSend {
                        endpoint_id,
                        home_island,
                        data,
                    } => {
                        self.state.send_endpoint_send_request(
                            home_island,
                            endpoint_id,
                            data,
                            fiber_id.to_raw() as u64,
                        );
                        return ExecResult::Block(crate::fiber::BlockReason::Queue);
                    }
                    exec::QueueAction::RemoteRecv {
                        endpoint_id,
                        home_island,
                    } => {
                        let Some(frame) = fiber.current_frame_mut() else {
                            return ExecResult::JitError(
                                "Queue RemoteRecv without active frame".to_string(),
                            );
                        };
                        frame.pc = match frame.pc.checked_sub(1) {
                            Some(pc) => pc,
                            None => {
                                return ExecResult::JitError(
                                    "Queue RemoteRecv cannot rewind pc 0".to_string(),
                                );
                            }
                        };
                        self.state.send_endpoint_recv_request(
                            home_island,
                            endpoint_id,
                            fiber_id.to_raw() as u64,
                        );
                        return ExecResult::Block(crate::fiber::BlockReason::Queue);
                    }
                    exec::QueueAction::RemoteRecvData {
                        endpoint_id,
                        target_island,
                        fiber_id,
                        data,
                    } => {
                        self.state.send_endpoint_recv_data_response(
                            target_island,
                            endpoint_id,
                            data,
                            fiber_id,
                        );
                        return ExecResult::TimesliceExpired;
                    }
                    exec::QueueAction::RemoteClose {
                        endpoint_id,
                        home_island,
                    } => {
                        self.state
                            .send_endpoint_close_request(home_island, endpoint_id);
                        self.mark_gc_all_roots_dirty();
                        self.state.endpoint_registry.mark_tombstone(endpoint_id);
                        refetch!();
                    }
                }
            };
        }

        for _ in 0..TIME_SLICE {
            if self.interrupt_requested() {
                return ExecResult::Interrupted;
            }

            #[cfg(feature = "jit")]
            {
                let frame = unsafe { &mut *frame_ptr };
                // JIT side exits may materialize a callee frame and return to
                // this interpreter loop. This is not frame elision: the VM
                // frame already exists, but deferred calls executing under the
                // unwind machine still need interpreter-owned ordering and
                // recover eligibility checks.
                if frame.pc == 0
                    && fiber.unwinding.is_none()
                    && vo_jit::can_enter_materialized_frame_for_jit(func)
                {
                    if let Some(jit_func) = self
                        .jit_mgr
                        .as_ref()
                        .and_then(|jit_mgr| jit_mgr.get_entry(func_id))
                    {
                        let result = jit::dispatch_jit_frame(self, fiber, module, jit_func);
                        stack = fiber.stack_ptr();
                        match result {
                            ExecResult::FrameChanged => {
                                refetch!();
                                continue;
                            }
                            other => return other,
                        }
                    }
                }
            }

            let frame = unsafe { &mut *frame_ptr };
            let pc = frame.pc;
            let Some(&inst) = code.get(pc) else {
                return ExecResult::JitError(format!(
                    "pc {pc} out of bounds for function {} with {} instructions",
                    func.name,
                    code.len()
                ));
            };
            frame.pc = pc + 1;

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
                    if let Err(msg) = exec::exec_load_const(stack, bp, &inst, &module.constants) {
                        return ExecResult::JitError(msg);
                    }
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
                    if let Err(msg) = exec::exec_global_get(stack, bp, &inst, &self.state.globals) {
                        return ExecResult::JitError(msg);
                    }
                }
                Opcode::GlobalGetN => {
                    if let Err(msg) = exec::exec_global_get_n(stack, bp, &inst, &self.state.globals)
                    {
                        return ExecResult::JitError(msg);
                    }
                }
                Opcode::GlobalSet => {
                    if let Err(msg) =
                        exec::exec_global_set(stack, bp, &inst, &mut self.state.globals)
                    {
                        return ExecResult::JitError(msg);
                    }
                }
                Opcode::GlobalSetN => {
                    if let Err(msg) =
                        exec::exec_global_set_n(stack, bp, &inst, &mut self.state.globals)
                    {
                        return ExecResult::JitError(msg);
                    }
                }

                Opcode::PtrNew => {
                    exec::exec_ptr_new(stack, bp, &inst, &mut self.state.gc);
                }
                Opcode::PtrGet => {
                    if !exec::exec_ptr_get(stack, bp, &inst) {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::NilPointerDereference
                        ));
                    }
                }
                Opcode::PtrSet => {
                    if !exec::exec_ptr_set(stack, bp, &inst, &mut self.state.gc) {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::NilPointerDereference
                        ));
                    }
                }
                Opcode::PtrGetN => {
                    if !exec::exec_ptr_get_n(stack, bp, &inst) {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::NilPointerDereference
                        ));
                    }
                }
                Opcode::PtrSetN => {
                    if !exec::exec_ptr_set_n(stack, bp, &inst) {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::NilPointerDereference
                        ));
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
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::DivisionByZero
                        ));
                    }
                    stack_set(stack, bp + inst.a as usize, a.wrapping_div(b) as u64);
                }
                Opcode::ModI => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b == 0 {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::DivisionByZero
                        ));
                    }
                    stack_set(stack, bp + inst.a as usize, a.wrapping_rem(b) as u64);
                }
                Opcode::DivU => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    if b == 0 {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::DivisionByZero
                        ));
                    }
                    stack_set(stack, bp + inst.a as usize, a.wrapping_div(b));
                }
                Opcode::ModU => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize);
                    if b == 0 {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::DivisionByZero
                        ));
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
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::NegativeShift
                        ));
                    }
                    let result = if b >= 64 { 0 } else { a.wrapping_shl(b as u32) };
                    stack_set(stack, bp + inst.a as usize, result);
                }
                Opcode::ShrS => {
                    let a = stack_get(stack, bp + inst.b as usize) as i64;
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b < 0 {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::NegativeShift
                        ));
                    }
                    let result = if b >= 64 {
                        if a < 0 {
                            -1i64
                        } else {
                            0i64
                        }
                    } else {
                        a.wrapping_shr(b as u32)
                    };
                    stack_set(stack, bp + inst.a as usize, result as u64);
                }
                Opcode::ShrU => {
                    let a = stack_get(stack, bp + inst.b as usize);
                    let b = stack_get(stack, bp + inst.c as usize) as i64;
                    if b < 0 {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::NegativeShift
                        ));
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
                        (false, false, true) => ni <= li,
                        (false, true, false) => next_idx < limit,
                        (false, true, true) => next_idx <= limit,
                        // Decrement: i > limit or i >= limit
                        (true, false, false) => ni > li,
                        (true, false, true) => ni >= li,
                        (true, true, false) => next_idx > limit,
                        (true, true, true) => next_idx >= limit,
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
                        let target_func_id = inst.static_call_func_id();
                        if let Some(jit_mgr) = self.jit_mgr.as_mut() {
                            let Some(target_func) = module.functions.get(target_func_id as usize)
                            else {
                                return ExecResult::JitError(format!(
                                    "missing call target function id {target_func_id}"
                                ));
                            };
                            match jit_mgr.resolve_call(target_func_id, target_func, module) {
                                Ok(Some(jit_func)) => {
                                    // Execute via JIT
                                    let result = jit::dispatch_jit_call(
                                        self,
                                        fiber,
                                        &inst,
                                        module,
                                        jit_func,
                                        target_func_id,
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
                                Ok(None) => {}
                                Err(err) => {
                                    return ExecResult::JitError(format!(
                                        "JIT call compilation failed for {}: {err}",
                                        target_func.name
                                    ));
                                }
                            }
                        }
                    }
                    // VM fallback path
                    handle_panic_result!(exec::exec_call(&mut self.state.gc, fiber, &inst, module));
                }
                Opcode::CallExtern => {
                    use vo_runtime::ffi::{
                        ExternFiberInputs, ExternInvoke, ExternResult, ExternWorld,
                    };
                    // CallExtern: a=dst, b=extern_id, c=args_start, flags=arg_count
                    let extern_id = inst.b as u32;
                    let vm_ptr = self as *mut Vm as *mut core::ffi::c_void;
                    let fiber_ptr = fiber as *mut crate::fiber::Fiber as *mut core::ffi::c_void;
                    #[cfg(feature = "std")]
                    let resume_io_token = fiber.resume_io_token.take();
                    let resume_host_event_token = fiber.resume_host_event_token.take();
                    let resume_host_event_data = fiber.resume_host_event_data.take();

                    let (closure_replay_results, closure_replay_panic_message) =
                        fiber.closure_replay.take_for_extern();
                    let Some(extern_def) = module.externs.get(extern_id as usize) else {
                        return ExecResult::JitError(format!(
                            "CallExtern missing extern id {extern_id}"
                        ));
                    };
                    let arg_slots = inst.flags as u16;
                    let ret_slots = extern_def.ret_slots;
                    if extern_def.param_slots != 0 && arg_slots != extern_def.param_slots {
                        return ExecResult::JitError(format!(
                            "CallExtern arg slot count {arg_slots} does not match extern {} param_slots {}",
                            extern_def.name,
                            extern_def.param_slots
                        ));
                    }
                    if let Err(msg) = check_extern_frame_range(
                        "arg",
                        func,
                        bp,
                        fiber.stack.len(),
                        inst.c,
                        arg_slots,
                    ) {
                        return ExecResult::JitError(msg);
                    }
                    if let Err(msg) = check_extern_frame_range(
                        "return",
                        func,
                        bp,
                        fiber.stack.len(),
                        inst.a,
                        ret_slots,
                    ) {
                        return ExecResult::JitError(msg);
                    }
                    let invoke = ExternInvoke {
                        extern_id,
                        bp: bp as u32,
                        arg_start: inst.c,
                        arg_slots: inst.flags as u16,
                        ret_start: inst.a,
                        ret_slots,
                    };
                    let world = ExternWorld {
                        gc: &mut self.state.gc,
                        module,
                        itab_cache: &mut self.state.itab_cache,
                        vm_opaque: vm_ptr,
                        program_args: &self.state.program_args,
                        output: &*self.state.output,
                        sentinel_errors: &mut self.state.sentinel_errors,
                        host_output: &mut self.state.host_output,
                        #[cfg(feature = "std")]
                        io: &mut self.state.io,
                    };
                    let fiber_inputs = ExternFiberInputs {
                        fiber_opaque: fiber_ptr,
                        #[cfg(feature = "std")]
                        resume_io_token,
                        resume_host_event_token,
                        resume_host_event_data,
                        replay_results: closure_replay_results,
                        replay_panic_message: closure_replay_panic_message,
                    };
                    let extern_result = self.state.extern_registry.call(
                        &mut fiber.stack,
                        invoke,
                        world,
                        fiber_inputs,
                    );
                    stack = fiber.stack_ptr();
                    #[cfg(debug_assertions)]
                    if let Err(msg) = debug_validate_extern_returns(
                        &self.state.gc,
                        module,
                        fiber,
                        fiber_id,
                        func_id,
                        extern_id,
                        bp,
                        &inst,
                    ) {
                        return ExecResult::JitError(msg);
                    }
                    match extern_result {
                        ExternResult::Ok => {
                            refetch!();
                        }
                        ExternResult::Panic(msg) => {
                            let r =
                                runtime_panic_msg(&mut self.state.gc, fiber, stack, module, msg);
                            if matches!(r, ExecResult::FrameChanged) {
                                refetch!();
                            } else {
                                return r;
                            }
                        }
                        ExternResult::NotRegistered(id) => {
                            let name = &extern_def.name;
                            let msg =
                                format!("extern function '{}' (id={}) not registered", name, id);
                            let r =
                                runtime_panic_msg(&mut self.state.gc, fiber, stack, module, msg);
                            if matches!(r, ExecResult::FrameChanged) {
                                refetch!();
                            } else {
                                return r;
                            }
                        }
                        ExternResult::Yield => {
                            return ExecResult::TimesliceExpired;
                        }
                        ExternResult::Block => {
                            return ExecResult::Block(crate::fiber::BlockReason::Queue);
                        }
                        ExternResult::HostEventWait { token, delay_ms } => {
                            return ExecResult::Block(crate::fiber::BlockReason::HostEvent {
                                token,
                                delay_ms,
                            });
                        }
                        ExternResult::HostEventWaitAndReplay { token } => {
                            return ExecResult::Block(crate::fiber::BlockReason::HostEventReplay(
                                token,
                            ));
                        }
                        #[cfg(feature = "std")]
                        ExternResult::WaitIo { token } => {
                            return ExecResult::Block(crate::fiber::BlockReason::Io(token));
                        }
                        ExternResult::CallClosure { closure_ref, args } => {
                            // Undo PC pre-increment so extern replays on return
                            let Some(frame) = fiber.current_frame_mut() else {
                                return ExecResult::JitError(
                                    "CallExtern closure replay requested without active frame"
                                        .to_string(),
                                );
                            };
                            frame.pc -= 1;

                            // Push closure frame on current fiber using same layout as exec_call_closure
                            let closure_func_id =
                                vo_runtime::objects::closure::func_id(closure_ref);
                            let Some(func_def) = module.functions.get(closure_func_id as usize)
                            else {
                                return ExecResult::JitError(format!(
                                    "CallExtern closure replay missing function id {closure_func_id}"
                                ));
                            };

                            let new_bp = fiber.sp;
                            let local_slots = func_def.local_slots as usize;
                            if fiber.try_reserve_slots_at(new_bp, local_slots).is_err() {
                                handle_panic_result!(runtime_trap(
                                    &mut self.state.gc,
                                    fiber,
                                    stack,
                                    module,
                                    RuntimeTrapKind::StackOverflow,
                                ));
                            }

                            // Use call_layout for correct slot placement (matches exec_call_closure)
                            let layout = vo_runtime::objects::closure::call_layout(
                                closure_ref as u64,
                                closure_ref,
                                func_def.recv_slots as usize,
                                func_def.is_closure,
                            );

                            if layout.slot0.is_some() && layout.arg_offset > 1 {
                                fiber.zero_slots_at(new_bp + 1, layout.arg_offset - 1);
                            }
                            fiber.zero_slots_tail_at(
                                new_bp,
                                func_def.gc_scan_slots as usize,
                                layout.arg_offset + args.len(),
                            );

                            let fstack = fiber.stack_ptr();
                            if let Some(slot0_val) = layout.slot0 {
                                helpers::stack_set(fstack, new_bp, slot0_val);
                            }

                            fiber.copy_slots_from_slice(new_bp + layout.arg_offset, &args);

                            fiber.push_call_frame(
                                closure_func_id,
                                new_bp,
                                0, // ret_reg=0 (return values go via replay cache, not caller stack)
                                func_def.ret_slots,
                                func_def.gc_scan_slots,
                            );

                            // Mark replay depth so return path knows to cache results.
                            // Push previous depth so nested CallExterns don't clobber it.
                            fiber.closure_replay.push_depth(fiber.frames.len());

                            stack = fiber.stack_ptr();
                            refetch!();
                        }
                    }
                }
                Opcode::CallClosure => {
                    let closure_ref =
                        stack_get(stack, bp + inst.a as usize) as vo_runtime::gc::GcRef;
                    if closure_ref.is_null() {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::NilFuncCall
                        ));
                    }
                    handle_panic_result!(exec::exec_call_closure(
                        &mut self.state.gc,
                        fiber,
                        &inst,
                        module
                    ));
                }
                Opcode::CallIface => {
                    handle_panic_result!(exec::exec_call_iface(
                        &mut self.state.gc,
                        fiber,
                        &inst,
                        module,
                        &self.state.itab_cache
                    ));
                }
                Opcode::Return => {
                    let result = if fiber.is_direct_defer_context() {
                        exec::handle_panic_unwind(&mut self.state.gc, fiber, module)
                    } else {
                        let is_error_return = (inst.flags & 1) != 0;
                        exec::handle_return(
                            &mut self.state.gc,
                            fiber,
                            &inst,
                            func,
                            module,
                            is_error_return,
                        )
                    };
                    stack = fiber.stack_ptr();
                    if !matches!(result, ExecResult::FrameChanged) {
                        return result;
                    }
                    refetch!();
                }

                // String operations
                Opcode::StrNew => {
                    if let Err(msg) =
                        exec::exec_str_new(stack, bp, &inst, &module.constants, &mut self.state.gc)
                    {
                        return ExecResult::JitError(msg);
                    }
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
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx, len
                            )
                        ));
                    }
                    let byte = string_index(s, idx);
                    stack_set(stack, bp + inst.a as usize, byte as u64);
                }
                Opcode::StrConcat => {
                    exec::exec_str_concat(stack, bp, &inst, &mut self.state.gc);
                }
                Opcode::StrSlice => {
                    if !exec::exec_str_slice(stack, bp, &inst, &mut self.state.gc) {
                        let lo = stack_get(stack, bp + inst.c as usize);
                        let hi = stack_get(stack, bp + inst.c as usize + 1);
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::SliceBoundsOutOfRange,
                            format!("runtime error: slice bounds out of range [{}:{}]", lo, hi)
                        ));
                    }
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
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx, len
                            )
                        ));
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
                            for i in 0..elem_bytes.div_ceil(8) {
                                let ptr =
                                    unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                stack_set(stack, dst + i, unsafe { *ptr });
                            }
                            continue;
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            for i in 0..elem_bytes.div_ceil(8) {
                                let ptr =
                                    unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
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
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx, len
                            )
                        ));
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
                        8 => {
                            unsafe { *(base.offset(off * 8) as *mut u64) = val };
                            // Write barrier for GcRef elements (string, slice, map, pointer, etc.)
                            let em = array::elem_meta(arr);
                            if em.value_kind().may_contain_gc_refs() {
                                self.state.gc.write_barrier(arr, val as GcRef);
                            }
                        }
                        0 => {
                            let elem_bytes = stack_get(stack, bp + inst.b as usize + 1) as usize;
                            let elem_slots = elem_bytes.div_ceil(8);
                            for i in 0..elem_slots {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                unsafe { *ptr = stack_get(stack, src + i) };
                            }
                            // Write barrier for multi-slot elements that may contain GcRefs
                            let em = array::elem_meta(arr);
                            if em.value_kind().may_contain_gc_refs() {
                                let vals: Vec<u64> =
                                    (0..elem_slots).map(|i| stack_get(stack, src + i)).collect();
                                vo_runtime::gc_types::typed_write_barrier_by_meta(
                                    &mut self.state.gc,
                                    arr,
                                    &vals,
                                    em,
                                    Some(module),
                                );
                            }
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            let elem_slots = elem_bytes.div_ceil(8);
                            for i in 0..elem_slots {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                unsafe { *ptr = stack_get(stack, src + i) };
                            }
                            // Write barrier for multi-slot elements that may contain GcRefs
                            if elem_bytes >= 8 {
                                let em = array::elem_meta(arr);
                                if em.value_kind().may_contain_gc_refs() {
                                    let vals: Vec<u64> = (0..elem_slots)
                                        .map(|i| stack_get(stack, src + i))
                                        .collect();
                                    vo_runtime::gc_types::typed_write_barrier_by_meta(
                                        &mut self.state.gc,
                                        arr,
                                        &vals,
                                        em,
                                        Some(module),
                                    );
                                }
                            }
                        }
                    }
                }
                Opcode::ArrayAddr => {
                    let arr = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.c as usize) as usize;
                    let len = array::len(arr);
                    if idx >= len {
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx, len
                            )
                        ));
                    }
                    let elem_bytes = match inst.flags {
                        0 => stack_get(stack, bp + inst.c as usize + 1) as usize,
                        0x81 => 1,
                        0x82 => 2,
                        0x84 | 0x44 => 4,
                        f => f as usize,
                    };
                    let base = array::data_ptr_bytes(arr);
                    let addr = unsafe { base.add(idx * elem_bytes) } as u64;
                    stack_set(stack, bp + inst.a as usize, addr);
                }

                // Slice operations
                Opcode::SliceNew => {
                    if let Err(msg) = exec::exec_slice_new(stack, bp, &inst, &mut self.state.gc) {
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::MakeSlice,
                            msg
                        ));
                    }
                }
                Opcode::SliceGet => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.c as usize) as usize;
                    let len = if s.is_null() { 0 } else { slice_len(s) };
                    if idx >= len {
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx, len
                            )
                        ));
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
                            for i in 0..elem_bytes.div_ceil(8) {
                                let ptr =
                                    unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
                                stack_set(stack, dst + i, unsafe { *ptr });
                            }
                            continue;
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            for i in 0..elem_bytes.div_ceil(8) {
                                let ptr =
                                    unsafe { base.add(idx * elem_bytes + i * 8) as *const u64 };
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
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx, len
                            )
                        ));
                    }
                    let base = slice_data_ptr(s);
                    let src = bp + inst.c as usize;
                    let val = stack_get(stack, src);
                    match inst.flags {
                        1 | 129 => unsafe { *base.add(idx) = val as u8 },
                        2 | 130 => unsafe { *(base.add(idx * 2) as *mut u16) = val as u16 },
                        4 | 132 => unsafe { *(base.add(idx * 4) as *mut u32) = val as u32 },
                        0x44 => unsafe { *(base.add(idx * 4) as *mut u32) = val as u32 },
                        8 => {
                            unsafe { *(base.add(idx * 8) as *mut u64) = val };
                            // Write barrier: backing array may be BLACK, val may be WHITE GcRef
                            let arr_ref = vo_runtime::objects::slice::array_ref(s);
                            if !arr_ref.is_null() {
                                let em = array::elem_meta(arr_ref);
                                if em.value_kind().may_contain_gc_refs() {
                                    self.state.gc.write_barrier(arr_ref, val as GcRef);
                                }
                            }
                        }
                        0 => {
                            let elem_bytes = stack_get(stack, bp + inst.b as usize + 1) as usize;
                            let elem_slots = elem_bytes.div_ceil(8);
                            for i in 0..elem_slots {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                unsafe { *ptr = stack_get(stack, src + i) };
                            }
                            // Write barrier for multi-slot elements that may contain GcRefs
                            let arr_ref = vo_runtime::objects::slice::array_ref(s);
                            if !arr_ref.is_null() {
                                let em = array::elem_meta(arr_ref);
                                if em.value_kind().may_contain_gc_refs() {
                                    let vals: Vec<u64> = (0..elem_slots)
                                        .map(|i| stack_get(stack, src + i))
                                        .collect();
                                    vo_runtime::gc_types::typed_write_barrier_by_meta(
                                        &mut self.state.gc,
                                        arr_ref,
                                        &vals,
                                        em,
                                        Some(module),
                                    );
                                }
                            }
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            let elem_slots = elem_bytes.div_ceil(8);
                            for i in 0..elem_slots {
                                let ptr = unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                unsafe { *ptr = stack_get(stack, src + i) };
                            }
                            // Write barrier for multi-slot elements that may contain GcRefs
                            if elem_bytes >= 8 {
                                let arr_ref = vo_runtime::objects::slice::array_ref(s);
                                if !arr_ref.is_null() {
                                    let em = array::elem_meta(arr_ref);
                                    if em.value_kind().may_contain_gc_refs() {
                                        let vals: Vec<u64> = (0..elem_slots)
                                            .map(|i| stack_get(stack, src + i))
                                            .collect();
                                        vo_runtime::gc_types::typed_write_barrier_by_meta(
                                            &mut self.state.gc,
                                            arr_ref,
                                            &vals,
                                            em,
                                            Some(module),
                                        );
                                    }
                                }
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
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::SliceBoundsOutOfRange,
                            format!("runtime error: slice bounds out of range [{}:{}]", lo, hi)
                        ));
                    }
                }
                Opcode::SliceAppend => {
                    exec::exec_slice_append(stack, bp, &inst, &mut self.state.gc, Some(module));
                }
                Opcode::SliceAddr => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx = stack_get(stack, bp + inst.c as usize) as usize;
                    let len = if s.is_null() { 0 } else { slice_len(s) };
                    if idx >= len {
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx, len
                            )
                        ));
                    }
                    let elem_bytes = match inst.flags {
                        0 => stack_get(stack, bp + inst.c as usize + 1) as usize,
                        0x81 => 1,
                        0x82 => 2,
                        0x84 | 0x44 => 4,
                        f => f as usize,
                    };
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
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::NilMapWrite
                        ));
                    }
                    if !exec::exec_map_set(stack, bp, &inst, &mut self.state.gc, Some(module)) {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::UnhashableType
                        ));
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
                Opcode::QueueNew => {
                    if let Err(msg) = exec::exec_queue_new(stack, bp, &inst, &mut self.state.gc) {
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            exec::queue_new_trap_kind(inst.flags),
                            msg
                        ));
                    }
                }
                Opcode::QueueSend => {
                    if fiber.consume_remote_send_closed() {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::SendOnClosedChannel
                        ));
                    }
                    let ch = helpers::stack_get(stack, bp + inst.a as usize) as GcRef;
                    let elem_slots = inst.flags as usize;
                    let src_start = bp + inst.b as usize;
                    let src: Vec<u64> = (0..elem_slots)
                        .map(|i| helpers::stack_get(stack, src_start + i))
                        .collect();
                    handle_queue_action!(exec::queue_send_core(
                        ch,
                        &src,
                        self.state.current_island_id,
                        fiber_id.to_raw() as u64,
                        &mut self.state,
                        &module.struct_metas,
                        &module.runtime_types,
                        Some(module),
                    ));
                }
                Opcode::QueueRecv => {
                    if let Some(recv_response) = fiber.take_remote_recv_response() {
                        let elem_slots = inst.recv_elem_slots() as usize;
                        let has_ok = inst.recv_has_ok();
                        let dst_start = bp + inst.a as usize;
                        exec::replay_remote_queue_recv_response(
                            &mut self.state.gc,
                            recv_response,
                            elem_slots,
                            has_ok,
                            &module.struct_metas,
                            &module.runtime_types,
                            &mut self.state.endpoint_registry,
                            |i, value| helpers::stack_set(stack, dst_start + i, value),
                        );
                        self.mark_gc_all_roots_dirty();
                        refetch!();
                        continue;
                    }
                    handle_queue_action!(exec::exec_queue_recv(
                        stack,
                        bp,
                        self.state.current_island_id,
                        fiber_id.to_raw(),
                        &inst,
                    ));
                }
                Opcode::QueueClose => {
                    handle_queue_action!(exec::exec_queue_close(stack, bp, &inst));
                }
                Opcode::QueueLen => {
                    exec::exec_queue_get(stack, bp, &inst, exec::queue_len);
                }
                Opcode::QueueCap => {
                    exec::exec_queue_get(
                        stack,
                        bp,
                        &inst,
                        vo_runtime::objects::queue_state::capacity,
                    );
                }

                // Select operations
                Opcode::SelectBegin => {
                    exec::exec_select_begin(fiber, inst.a as usize, (inst.flags & 1) != 0);
                }
                Opcode::SelectSend => {
                    if let Err(msg) =
                        exec::exec_select_send(&mut fiber.select_state, inst.a, inst.b, inst.flags)
                    {
                        return ExecResult::JitError(msg);
                    }
                }
                Opcode::SelectRecv => {
                    if let Err(msg) = exec::exec_select_recv(
                        &mut fiber.select_state,
                        inst.a,
                        inst.b,
                        inst.recv_elem_slots() as u8,
                        inst.recv_has_ok(),
                    ) {
                        return ExecResult::JitError(msg);
                    }
                }
                Opcode::SelectExec => {
                    let fiber_id = fiber.id;
                    match exec::exec_select_exec(
                        stack,
                        bp,
                        self.state.current_island_id,
                        fiber_id,
                        &mut fiber.select_state,
                        inst.a,
                    ) {
                        exec::SelectResult::Continue => {}
                        exec::SelectResult::Block => {
                            // Waiters have been registered on all channels by exec_select_exec.
                            // Block this fiber - it will be woken when any channel is ready.
                            frame.pc -= 1;
                            return ExecResult::Block(crate::fiber::BlockReason::Queue);
                        }
                        exec::SelectResult::SendOnClosed => {
                            handle_panic_result!(runtime_trap(
                                &mut self.state.gc,
                                fiber,
                                stack,
                                module,
                                RuntimeTrapKind::SendOnClosedChannel
                            ));
                        }
                        exec::SelectResult::UnsupportedRemotePort => {
                            handle_panic_result!(runtime_panic_msg(
                                &mut self.state.gc,
                                fiber,
                                stack,
                                module,
                                crate::vm::helpers::ERR_SELECT_REMOTE_UNSUPPORTED.to_string(),
                            ));
                        }
                        exec::SelectResult::Wake(waiter) => {
                            self.state.wake_waiter(&waiter, &mut self.scheduler);
                            return ExecResult::TimesliceExpired;
                        }
                        exec::SelectResult::Malformed(msg) => {
                            return ExecResult::JitError(msg);
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
                    if inst.call_shape_is_closure() {
                        let closure_ref =
                            stack_get(stack, bp + inst.a as usize) as vo_runtime::gc::GcRef;
                        if closure_ref.is_null() {
                            handle_panic_result!(runtime_trap(
                                &mut self.state.gc,
                                fiber,
                                stack,
                                module,
                                RuntimeTrapKind::NilFuncCall
                            ));
                        }
                    }
                    let next_id = self.scheduler.fibers.len() as u32;
                    match exec::exec_go_start(stack, bp, &inst, &module.functions, next_id) {
                        Ok(go_result) => {
                            self.scheduler.spawn(go_result.new_fiber);
                        }
                        Err(exec::GoStartError::Trap(kind)) => {
                            handle_panic_result!(runtime_trap(
                                &mut self.state.gc,
                                fiber,
                                stack,
                                module,
                                kind
                            ));
                        }
                        Err(exec::GoStartError::Malformed(msg)) => {
                            return ExecResult::JitError(msg);
                        }
                    }
                }

                // Defer and error handling
                Opcode::DeferPush => {
                    let generation = fiber.effective_defer_generation();
                    if let Err(msg) = exec::exec_defer_push(
                        stack,
                        bp,
                        &fiber.frames,
                        func,
                        &mut fiber.defer_stack,
                        &inst,
                        &mut self.state.gc,
                        generation,
                    ) {
                        return ExecResult::JitError(msg);
                    }
                }
                Opcode::ErrDeferPush => {
                    let generation = fiber.effective_defer_generation();
                    if let Err(msg) = exec::exec_err_defer_push(
                        stack,
                        bp,
                        &fiber.frames,
                        func,
                        &mut fiber.defer_stack,
                        &inst,
                        &mut self.state.gc,
                        generation,
                    ) {
                        return ExecResult::JitError(msg);
                    }
                }
                Opcode::Panic => {
                    let result = user_panic(&mut self.state.gc, fiber, stack, bp, inst.a, module);
                    if matches!(result, ExecResult::FrameChanged) {
                        refetch!();
                    } else {
                        return result;
                    }
                }
                Opcode::Recover => {
                    exec::exec_recover(stack, bp, fiber, &inst);
                }

                // Interface operations
                Opcode::IfaceAssign => {
                    if let Err(msg) = exec::exec_iface_assign(
                        stack,
                        bp,
                        &inst,
                        &mut self.state.gc,
                        &mut self.state.itab_cache,
                        module,
                    ) {
                        return ExecResult::JitError(msg);
                    }
                }
                Opcode::IfaceAssert => {
                    let result = exec::exec_iface_assert(
                        stack,
                        bp,
                        &inst,
                        &mut self.state.itab_cache,
                        module,
                    );
                    match result {
                        ExecResult::Panic => {
                            handle_panic_result!(runtime_trap(
                                &mut self.state.gc,
                                fiber,
                                stack,
                                module,
                                RuntimeTrapKind::TypeAssertionFailed
                            ));
                        }
                        ExecResult::JitError(msg) => return ExecResult::JitError(msg),
                        _ => {}
                    }
                }
                Opcode::IfaceEq => {
                    let result = exec::exec_iface_eq(stack, bp, &inst, module);
                    if matches!(result, ExecResult::Panic) {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::UncomparableType
                        ));
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
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx, len
                            )
                        ));
                    }
                }

                // === ISLAND/CHANNEL: Cross-island operations ===
                #[cfg(feature = "std")]
                Opcode::IslandNew => {
                    let handle = match self.create_island() {
                        Ok(handle) => handle,
                        Err(VmError::Jit(msg)) => return ExecResult::JitError(msg),
                        Err(err) => {
                            return ExecResult::JitError(format!("IslandNew failed: {err:?}"));
                        }
                    };
                    stack_set(stack, bp + inst.a as usize, handle as u64);
                }
                #[cfg(not(feature = "std"))]
                Opcode::IslandNew => {
                    let island_id = self.state.next_island_id;
                    self.state.next_island_id += 1;
                    let _ = exec::exec_island_new(stack, bp, &inst, &mut self.state.gc, island_id);
                }
                Opcode::GoIsland => {
                    let island_ref =
                        stack_get(stack, bp + inst.a as usize) as vo_runtime::gc::GcRef;
                    if island_ref.is_null() {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::NilPointerDereference
                        ));
                    }
                    let closure_ref =
                        stack_get(stack, bp + inst.b as usize) as vo_runtime::gc::GcRef;
                    if closure_ref.is_null() {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::NilFuncCall
                        ));
                    }
                    let result = exec::exec_go_island(stack, bp, &inst);
                    let island_id = vo_runtime::island::id(result.island);

                    if island_id == self.state.current_island_id {
                        if module.functions.get(result.func_id as usize).is_none() {
                            return ExecResult::JitError(format!(
                                "GoIsland missing closure target function id {}",
                                result.func_id
                            ));
                        }
                        let new_fiber = match unsafe {
                            helpers::try_build_closure_fiber_from_args_ptr(
                                &module.functions,
                                self.scheduler.fibers.len() as u32,
                                closure_ref as u64,
                                stack.add(bp + inst.c as usize),
                                inst.flags as u32,
                            )
                        } {
                            Ok(new_fiber) => new_fiber,
                            Err(RuntimeTrapKind::StackOverflow) => {
                                handle_panic_result!(runtime_trap(
                                    &mut self.state.gc,
                                    fiber,
                                    stack,
                                    module,
                                    RuntimeTrapKind::StackOverflow
                                ));
                            }
                            Err(_) => {
                                handle_panic_result!(runtime_trap(
                                    &mut self.state.gc,
                                    fiber,
                                    stack,
                                    module,
                                    RuntimeTrapKind::NilFuncCall
                                ));
                            }
                        };
                        self.scheduler.spawn(new_fiber);
                    } else {
                        let Some(func_def) = module.functions.get(result.func_id as usize) else {
                            return ExecResult::JitError(format!(
                                "GoIsland missing closure target function id {}",
                                result.func_id
                            ));
                        };
                        if let Err(msg) = exec::prepare_queue_handles_for_transfer(
                            &result,
                            island_id,
                            &func_def.capture_types,
                            &func_def.param_types,
                            &module.struct_metas,
                            &module.runtime_types,
                            &mut self.state,
                        ) {
                            return ExecResult::JitError(format!(
                                "GoIsland queue-transfer metadata contract error: {msg}"
                            ));
                        }
                        let data = exec::pack_closure_for_island(
                            &self.state.gc,
                            &result,
                            &func_def.capture_types,
                            &func_def.param_types,
                            &module.struct_metas,
                            &module.runtime_types,
                        );
                        let closure_data = vo_runtime::pack::PackedValue::from_data(data);
                        self.state
                            .send_spawn_fiber_to_island(island_id, closure_data);
                    }
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
    pub fn spawn_call(&mut self, func_id: u32, args: &[u64]) -> Result<(), VmError> {
        let module = self.module.as_ref().ok_or(VmError::NoEntryFunction)?;
        let func_def = module
            .functions
            .get(func_id as usize)
            .ok_or(VmError::InvalidFunctionId(func_id))?;
        let param_slots = func_def.param_slots as usize;
        let local_slots = func_def.local_slots as usize;
        let gc_scan_slots = func_def.gc_scan_slots as usize;
        if param_slots > local_slots {
            return Err(VmError::Jit(format!(
                "spawn_call function {} metadata invalid: param_slots {} exceed local_slots {}",
                func_id, func_def.param_slots, func_def.local_slots
            )));
        }
        if gc_scan_slots > local_slots {
            return Err(VmError::Jit(format!(
                "spawn_call function {} metadata invalid: gc_scan_slots {} exceed local_slots {}",
                func_id, func_def.gc_scan_slots, func_def.local_slots
            )));
        }
        if args.len() != param_slots {
            return Err(VmError::Jit(format!(
                "spawn_call arg slot count mismatch for function {}: expected {} slots, got {}",
                func_id,
                func_def.param_slots,
                args.len()
            )));
        }
        let ret_slots = func_def.ret_slots;
        let gc_scan_slots_u16 = func_def.gc_scan_slots;

        let fiber_id = self.scheduler.reuse_or_spawn();
        let fiber = self.scheduler.get_fiber_mut(fiber_id);

        let bp = fiber.sp;
        fiber
            .try_reserve_slots_at(bp, local_slots)
            .map_err(fiber_capacity_error_to_vm_error)?;

        fiber.zero_slots_tail_at(bp, gc_scan_slots, args.len());
        fiber.copy_slots_from_slice(bp, args);

        fiber
            .try_push_call_frame(func_id, bp, 0, ret_slots, gc_scan_slots_u16)
            .map_err(fiber_capacity_error_to_vm_error)?;
        Ok(())
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fiber::Fiber;
    #[cfg(feature = "std")]
    use std::sync::atomic::AtomicBool;
    #[cfg(feature = "std")]
    use std::sync::Arc;
    use vo_runtime::bytecode::{Constant, ExternDef, FunctionDef, InterfaceMeta, StructMeta};
    use vo_runtime::ffi::{ExternCallContext, ExternResult};
    use vo_runtime::island::{EndpointResponseKind, IslandCommand};
    use vo_runtime::{SlotType, ValueKind, ValueMeta};

    fn gc_test_module() -> Module {
        gc_test_module_with_root_slots(1)
    }

    fn gc_test_module_with_root_slots(root_slots: u16) -> Module {
        let mut module = Module::new("gc-test".to_string());
        module.struct_metas.push(StructMeta {
            slot_types: Vec::new(),
            fields: Vec::new(),
            field_index: Default::default(),
        });
        module.struct_metas.push(StructMeta {
            slot_types: Vec::new(),
            fields: Vec::new(),
            field_index: Default::default(),
        });
        module.functions.push(FunctionDef {
            name: "root_frame".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: root_slots,
            gc_scan_slots: root_slots,
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            jit_metadata: Vec::new(),
            code: Vec::new(),
            slot_types: vec![SlotType::GcRef; root_slots as usize],
            borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&vec![
                SlotType::GcRef;
                root_slots as usize
            ]),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        });
        module
    }

    fn malformed_single_instruction_module(
        name: &str,
        code: Vec<Instruction>,
        constants: Vec<Constant>,
    ) -> Module {
        let mut module = Module::new(name.to_string());
        module.constants = constants;
        module.functions.push(FunctionDef {
            name: "main".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 4,
            gc_scan_slots: 0,
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            jit_metadata: vec![vo_runtime::bytecode::JitInstructionMetadata::None; code.len()],
            code,
            slot_types: vec![SlotType::Value; 4],
            borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&[
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ]),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        });
        module
    }

    #[test]
    fn malformed_load_const_index_is_vm_error_instead_of_index_panic() {
        let module = malformed_single_instruction_module(
            "malformed-load-const",
            vec![Instruction::new(Opcode::LoadConst, 0, 0, 0)],
            Vec::new(),
        );
        let mut vm = Vm::new();
        vm.load(module).unwrap();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(
                    msg.contains("LoadConst constant index 0 out of bounds"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("malformed LoadConst should be a VM error, got {other:?}"),
            Err(_) => panic!("malformed LoadConst must not panic"),
        }
    }

    #[test]
    fn malformed_str_new_missing_constant_is_vm_error_instead_of_index_panic() {
        let module = malformed_single_instruction_module(
            "malformed-str-new-missing",
            vec![Instruction::new(Opcode::StrNew, 0, 0, 0)],
            Vec::new(),
        );
        let mut vm = Vm::new();
        vm.load(module).unwrap();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(
                    msg.contains("StrNew constant index 0 out of bounds"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("malformed StrNew should be a VM error, got {other:?}"),
            Err(_) => panic!("malformed StrNew missing constant must not panic"),
        }
    }

    #[test]
    fn malformed_str_new_non_string_constant_is_vm_error_instead_of_nil_fill() {
        let module = malformed_single_instruction_module(
            "malformed-str-new-non-string",
            vec![Instruction::new(Opcode::StrNew, 0, 0, 0)],
            vec![Constant::Int(7)],
        );
        let mut vm = Vm::new();
        vm.load(module).unwrap();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(msg.contains("StrNew constant 0 expected string"), "{msg}");
            }
            Ok(other) => panic!("malformed StrNew should be a VM error, got {other:?}"),
            Err(_) => panic!("malformed StrNew non-string constant must not panic"),
        }
    }

    #[test]
    fn malformed_pc_fallthrough_is_vm_error_instead_of_unsafe_fetch_abort() {
        let module = malformed_single_instruction_module(
            "malformed-pc-fallthrough",
            vec![Instruction::new(Opcode::Hint, 0, 0, 0)],
            Vec::new(),
        );
        let mut vm = Vm::new();
        vm.load(module).unwrap();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(msg.contains("pc 1 out of bounds"), "{msg}");
            }
            Ok(other) => panic!("pc fallthrough should be a VM error, got {other:?}"),
            Err(_) => panic!("pc fallthrough must not panic or abort"),
        }
    }

    #[test]
    fn malformed_global_index_is_vm_error_instead_of_index_panic() {
        let module = malformed_single_instruction_module(
            "malformed-global-get",
            vec![Instruction::new(Opcode::GlobalGet, 0, 0, 0)],
            Vec::new(),
        );
        let mut vm = Vm::new();
        vm.load(module).unwrap();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(
                    msg.contains("GlobalGet global range 0..1 out of bounds"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("malformed GlobalGet should be a VM error, got {other:?}"),
            Err(_) => panic!("malformed GlobalGet must not panic"),
        }
    }

    #[test]
    fn malformed_go_start_function_id_is_vm_error_instead_of_index_panic() {
        let module = malformed_single_instruction_module(
            "malformed-go-start",
            vec![Instruction::with_flags(Opcode::GoStart, 0, 7, 0, 0)],
            Vec::new(),
        );
        let mut vm = Vm::new();
        vm.load(module).unwrap();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(msg.contains("GoStart missing function id 7"), "{msg}");
            }
            Ok(other) => panic!("malformed GoStart should be a VM error, got {other:?}"),
            Err(_) => panic!("malformed GoStart must not panic"),
        }
    }

    #[test]
    fn malformed_go_start_closure_target_is_vm_error_instead_of_nil_call_trap() {
        let module = malformed_single_instruction_module(
            "malformed-go-start-closure",
            vec![Instruction::with_flags(Opcode::GoStart, 1, 0, 0, 0)],
            Vec::new(),
        );
        let mut vm = Vm::new();
        vm.load(module).unwrap();
        let closure_ref = vo_runtime::objects::closure::create(&mut vm.state.gc, 7, 0);
        let fid = vm.scheduler.spawn(Fiber::new(0));
        {
            let fiber = vm.scheduler.get_fiber_mut(fid);
            fiber.push_frame(0, 4, 0, 0, 0);
            fiber.stack[0] = closure_ref as u64;
        }

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run_scheduled()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(
                    msg.contains("GoStart missing closure target function id 7"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("malformed closure GoStart should be a VM error, got {other:?}"),
            Err(_) => panic!("malformed closure GoStart must not panic"),
        }
    }

    #[test]
    fn malformed_iface_assign_constant_is_vm_error_instead_of_index_panic() {
        let module = malformed_single_instruction_module(
            "malformed-iface-assign",
            vec![Instruction::with_flags(
                Opcode::IfaceAssign,
                ValueKind::Int as u8,
                0,
                1,
                0,
            )],
            Vec::new(),
        );
        let mut vm = Vm::new();
        vm.load(module).unwrap();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(
                    msg.contains("IfaceAssign constant index 0 out of bounds"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("malformed IfaceAssign should be a VM error, got {other:?}"),
            Err(_) => panic!("malformed IfaceAssign must not panic"),
        }
    }

    #[test]
    fn malformed_select_without_begin_is_vm_error_instead_of_state_panic() {
        let module = malformed_single_instruction_module(
            "malformed-select-send",
            vec![Instruction::with_flags(Opcode::SelectSend, 1, 0, 1, 0)],
            Vec::new(),
        );
        let mut vm = Vm::new();
        vm.load(module).unwrap();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(
                    msg.contains("SelectSend without active SelectBegin"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("malformed SelectSend should be a VM error, got {other:?}"),
            Err(_) => panic!("malformed SelectSend must not panic"),
        }
    }

    #[test]
    fn malformed_iface_assert_metadata_is_vm_error_instead_of_panic() {
        let mut module = malformed_single_instruction_module(
            "malformed-iface-assert",
            vec![Instruction::with_flags(Opcode::IfaceAssert, 1, 2, 0, 1)],
            Vec::new(),
        );
        module.interface_metas = vec![
            InterfaceMeta {
                name: "unused".to_string(),
                method_names: Vec::new(),
                methods: Vec::new(),
            },
            InterfaceMeta {
                name: "empty".to_string(),
                method_names: Vec::new(),
                methods: Vec::new(),
            },
        ];
        let mut vm = Vm::new();
        vm.load(module).unwrap();
        let fid = vm.scheduler.spawn(Fiber::new(0));
        {
            let fiber = vm.scheduler.get_fiber_mut(fid);
            fiber.push_frame(0, 4, 0, 0, 0);
            fiber.stack[0] = vo_runtime::objects::interface::pack_slot0(0, 0, ValueKind::Int64);
            fiber.stack[1] = 123;
        }

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run_scheduled()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(msg.contains("IfaceAssert metadata missing"), "{msg}");
            }
            Ok(other) => panic!("malformed IfaceAssert should be a VM error, got {other:?}"),
            Err(_) => panic!("malformed IfaceAssert metadata must not panic"),
        }
    }

    #[test]
    fn malformed_defer_arg_layout_is_vm_error_instead_of_metadata_panic() {
        let module = malformed_single_instruction_module(
            "malformed-defer-push",
            vec![Instruction::new(Opcode::DeferPush, 0, 3, 2)],
            Vec::new(),
        );
        let mut vm = Vm::new();
        vm.load(module).unwrap();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(msg.contains("DeferArgLayout metadata missing"), "{msg}");
            }
            Ok(other) => panic!("malformed DeferPush should be a VM error, got {other:?}"),
            Err(_) => panic!("malformed DeferPush must not panic"),
        }
    }

    #[test]
    fn corrupted_frame_function_id_is_vm_error_instead_of_index_panic() {
        let module =
            malformed_single_instruction_module("corrupted-frame-func-id", Vec::new(), Vec::new());
        let mut vm = Vm::new();
        vm.load(module).unwrap();
        let fid = vm.scheduler.spawn(Fiber::new(0));
        vm.scheduler.get_fiber_mut(fid).push_frame(7, 0, 0, 0, 0);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run_scheduled()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(
                    msg.contains("active frame references missing function id 7"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("corrupted frame should be a VM error, got {other:?}"),
            Err(_) => panic!("corrupted frame func_id must not panic"),
        }
    }

    #[test]
    fn malformed_call_extern_id_is_vm_error_instead_of_index_panic() {
        let module = malformed_single_instruction_module(
            "malformed-call-extern",
            vec![Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0)],
            Vec::new(),
        );
        let mut vm = Vm::new();
        vm.load(module).unwrap();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(msg.contains("CallExtern missing extern id 0"), "{msg}");
            }
            Ok(other) => panic!("malformed CallExtern should be a VM error, got {other:?}"),
            Err(_) => panic!("malformed CallExtern extern id must not panic"),
        }
    }

    #[test]
    fn malformed_go_island_closure_target_is_vm_error_instead_of_helper_panic() {
        let module = malformed_single_instruction_module(
            "malformed-go-island",
            vec![Instruction::with_flags(Opcode::GoIsland, 0, 0, 1, 2)],
            Vec::new(),
        );
        let mut vm = Vm::new();
        vm.load(module).unwrap();
        let island = vo_runtime::island::create(&mut vm.state.gc, vm.state.current_island_id);
        let closure_ref = vo_runtime::objects::closure::create(&mut vm.state.gc, 7, 0);
        let fid = vm.scheduler.spawn(Fiber::new(0));
        {
            let fiber = vm.scheduler.get_fiber_mut(fid);
            fiber.push_frame(0, 4, 0, 0, 0);
            fiber.stack[0] = island as u64;
            fiber.stack[1] = closure_ref as u64;
        }

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run_scheduled()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(
                    msg.contains("GoIsland missing closure target function id 7"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("malformed GoIsland should be a VM error, got {other:?}"),
            Err(_) => panic!("malformed GoIsland closure target must not panic"),
        }
    }

    fn extern_returns_missing_closure(ctx: &mut ExternCallContext<'_>) -> ExternResult {
        let closure_ref = vo_runtime::objects::closure::create(ctx.gc(), 7, 0);
        ExternResult::CallClosure {
            closure_ref,
            args: Vec::new(),
        }
    }

    #[test]
    fn extern_call_closure_missing_target_is_vm_error_instead_of_index_panic() {
        let mut module = malformed_single_instruction_module(
            "malformed-extern-call-closure",
            vec![Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0)],
            Vec::new(),
        );
        module.externs.push(ExternDef {
            name: "missing_closure".to_string(),
            param_slots: 0,
            ret_slots: 0,
            is_blocking: false,
            param_kinds: Vec::new(),
        });
        let mut vm = Vm::new();
        vm.finish_load(module);
        vm.state
            .extern_registry
            .register(0, extern_returns_missing_closure);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(
                    msg.contains("CallExtern closure replay missing function id 7"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("malformed extern CallClosure should be a VM error, got {other:?}"),
            Err(_) => panic!("malformed extern CallClosure target must not panic"),
        }
    }

    #[cfg(debug_assertions)]
    fn extern_returns_invalid_gcref(ctx: &mut ExternCallContext<'_>) -> ExternResult {
        ctx.set_slot(ctx.ret_start(), 0xdead_beef);
        ExternResult::Ok
    }

    fn extern_reads_first_arg(ctx: &mut ExternCallContext<'_>) -> ExternResult {
        let _ = ctx.slot(ctx.arg_start());
        ExternResult::Ok
    }

    fn extern_writes_return_start(ctx: &mut ExternCallContext<'_>) -> ExternResult {
        ctx.set_slot(ctx.ret_start(), 123);
        ExternResult::Ok
    }

    #[cfg(debug_assertions)]
    #[test]
    fn extern_invalid_gcref_return_is_vm_error_instead_of_debug_panic() {
        let mut module = malformed_single_instruction_module(
            "extern-invalid-gcref-return",
            vec![Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0)],
            Vec::new(),
        );
        module.functions[0].slot_types = vec![SlotType::GcRef];
        module.externs.push(ExternDef {
            name: "invalid_gcref".to_string(),
            param_slots: 0,
            ret_slots: 1,
            is_blocking: false,
            param_kinds: Vec::new(),
        });
        let mut vm = Vm::new();
        vm.finish_load(module);
        vm.state
            .extern_registry
            .register(0, extern_returns_invalid_gcref);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(msg.contains("CallExtern returned invalid GcRef"), "{msg}");
            }
            Ok(other) => panic!("invalid extern GcRef return should be a VM error, got {other:?}"),
            Err(_) => panic!("invalid extern GcRef return must not panic"),
        }
    }

    #[test]
    fn call_extern_arg_range_outside_frame_is_vm_error_instead_of_silent_read() {
        let mut module = malformed_single_instruction_module(
            "extern-arg-out-of-frame",
            vec![
                Instruction::with_flags(Opcode::CallExtern, 1, 0, 0, 3),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            Vec::new(),
        );
        module.functions[0].local_slots = 1;
        module.functions[0].slot_types = vec![SlotType::Value];
        module.externs.push(ExternDef {
            name: "reads_arg".to_string(),
            param_slots: 1,
            ret_slots: 0,
            is_blocking: false,
            param_kinds: Vec::new(),
        });
        let mut vm = Vm::new();
        vm.finish_load(module);
        vm.state.extern_registry.register(0, extern_reads_first_arg);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(msg.contains("CallExtern arg range 3..4"), "{msg}");
            }
            Ok(other) => panic!("out-of-frame extern arg should be a VM error, got {other:?}"),
            Err(_) => panic!("out-of-frame extern arg must not panic"),
        }
    }

    #[test]
    fn call_extern_return_range_outside_frame_is_vm_error_instead_of_silent_write() {
        let mut module = malformed_single_instruction_module(
            "extern-ret-out-of-frame",
            vec![
                Instruction::with_flags(Opcode::CallExtern, 0, 3, 0, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            Vec::new(),
        );
        module.functions[0].local_slots = 1;
        module.functions[0].slot_types = vec![SlotType::Value];
        module.externs.push(ExternDef {
            name: "writes_ret".to_string(),
            param_slots: 0,
            ret_slots: 1,
            is_blocking: false,
            param_kinds: Vec::new(),
        });
        let mut vm = Vm::new();
        vm.finish_load(module);
        vm.state
            .extern_registry
            .register(0, extern_writes_return_start);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(msg.contains("CallExtern return range 3..4"), "{msg}");
            }
            Ok(other) => panic!("out-of-frame extern return should be a VM error, got {other:?}"),
            Err(_) => panic!("out-of-frame extern return must not panic"),
        }
    }

    #[test]
    fn call_extern_arg_slot_count_mismatch_is_vm_error_instead_of_abi_guess() {
        let mut module = malformed_single_instruction_module(
            "extern-arg-count-mismatch",
            vec![
                Instruction::with_flags(Opcode::CallExtern, 1, 0, 0, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
            Vec::new(),
        );
        module.functions[0].local_slots = 2;
        module.functions[0].slot_types = vec![SlotType::Value, SlotType::Value];
        module.externs.push(ExternDef {
            name: "reads_arg".to_string(),
            param_slots: 2,
            ret_slots: 0,
            is_blocking: false,
            param_kinds: Vec::new(),
        });
        let mut vm = Vm::new();
        vm.finish_load(module);
        vm.state.extern_registry.register(0, extern_reads_first_arg);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(
                    msg.contains(
                        "CallExtern arg slot count 1 does not match extern reads_arg param_slots 2"
                    ),
                    "{msg}"
                );
            }
            Ok(other) => panic!("extern arg count mismatch should be a VM error, got {other:?}"),
            Err(_) => panic!("extern arg count mismatch must not panic"),
        }
    }

    #[test]
    fn spawn_call_without_module_returns_error_instead_of_expect_panic() {
        let mut vm = Vm::new();

        let result =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.spawn_call(0, &[])));

        match result {
            Ok(Err(VmError::NoEntryFunction)) => {}
            Ok(other) => {
                panic!("spawn_call without module should return NoEntryFunction, got {other:?}")
            }
            Err(_) => panic!("spawn_call without module must not panic"),
        }
    }

    #[test]
    fn spawn_call_missing_function_returns_error_instead_of_index_panic() {
        let module =
            malformed_single_instruction_module("spawn-call-missing-func", Vec::new(), Vec::new());
        let mut vm = Vm::new();
        vm.load(module).unwrap();

        let result =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.spawn_call(7, &[])));

        match result {
            Ok(Err(VmError::InvalidFunctionId(7))) => {}
            Ok(other) => {
                panic!("spawn_call missing function should return InvalidFunctionId, got {other:?}")
            }
            Err(_) => panic!("spawn_call missing function must not panic"),
        }
    }

    #[test]
    fn spawn_call_arg_count_mismatch_is_vm_error_instead_of_silent_zero_fill() {
        let mut module =
            malformed_single_instruction_module("spawn-call-arg-count", Vec::new(), Vec::new());
        module.functions[0].param_slots = 1;
        module.functions[0].local_slots = 1;
        module.functions[0].slot_types = vec![SlotType::Value];
        let mut vm = Vm::new();
        vm.load(module).unwrap();

        let result =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.spawn_call(0, &[])));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(msg.contains("spawn_call arg slot count mismatch"), "{msg}");
            }
            Ok(other) => panic!("spawn_call arg mismatch should be a VM error, got {other:?}"),
            Err(_) => panic!("spawn_call arg mismatch must not panic"),
        }
    }

    #[test]
    fn host_event_replay_without_current_fiber_is_vm_error_instead_of_unwrap_panic() {
        let mut vm = Vm::new();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            vm.handle_exec_result(
                ExecResult::Block(crate::fiber::BlockReason::HostEventReplay(42)),
                false,
            )
        }));

        match result {
            Ok(Some(Err(VmError::Jit(msg)))) => {
                assert!(
                    msg.contains("HostEventReplay requires a current fiber"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("missing current fiber should be a VM error, got {other:?}"),
            Err(_) => panic!("missing current fiber must not panic"),
        }
    }

    #[test]
    fn host_event_replay_without_frame_is_vm_error_instead_of_unwrap_panic() {
        let mut vm = Vm::new();
        vm.scheduler.spawn(Fiber::new(0));
        vm.scheduler.schedule_next().unwrap();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            vm.handle_exec_result(
                ExecResult::Block(crate::fiber::BlockReason::HostEventReplay(42)),
                false,
            )
        }));

        match result {
            Ok(Some(Err(VmError::Jit(msg)))) => {
                assert!(
                    msg.contains("HostEventReplay requires an active frame"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("missing current frame should be a VM error, got {other:?}"),
            Err(_) => panic!("missing current frame must not panic"),
        }
    }

    #[test]
    fn host_event_replay_at_pc_zero_is_vm_error_instead_of_underflow_panic() {
        let mut vm = Vm::new();
        let fid = vm.scheduler.spawn(Fiber::new(0));
        {
            let fiber = vm.scheduler.get_fiber_mut(fid);
            fiber.push_frame(0, 0, 0, 0, 0);
            fiber.current_frame_mut().unwrap().pc = 0;
        }
        vm.scheduler.schedule_next().unwrap();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            vm.handle_exec_result(
                ExecResult::Block(crate::fiber::BlockReason::HostEventReplay(42)),
                false,
            )
        }));

        match result {
            Ok(Some(Err(VmError::Jit(msg)))) => {
                assert!(msg.contains("HostEventReplay cannot rewind pc 0"), "{msg}");
            }
            Ok(other) => panic!("pc underflow should be a VM error, got {other:?}"),
            Err(_) => panic!("pc underflow must not panic"),
        }
    }

    #[test]
    fn runtime_trap_without_message_is_vm_error_instead_of_expect_panic() {
        let mut vm = Vm::new();
        let fid = vm.scheduler.spawn(Fiber::new(0));
        {
            let fiber = vm.scheduler.get_fiber_mut(fid);
            fiber.panic_trap_kind = Some(RuntimeTrapKind::StackOverflow);
        }
        vm.scheduler.schedule_next().unwrap();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            vm.handle_exec_result(ExecResult::Panic, false)
        }));

        match result {
            Ok(Some(Err(VmError::Jit(msg)))) => {
                assert!(
                    msg.contains("runtime trap StackOverflow missing panic payload"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("missing runtime trap payload should be a VM error, got {other:?}"),
            Err(_) => panic!("missing runtime trap payload must not panic"),
        }
    }

    #[test]
    fn deadlock_report_corrupted_frame_is_diagnostic_instead_of_index_panic() {
        let module =
            malformed_single_instruction_module("deadlock-corrupted-frame", Vec::new(), Vec::new());
        let mut vm = Vm::new();
        vm.load(module).unwrap();
        let fid = vm.scheduler.spawn(Fiber::new(0));
        vm.scheduler.get_fiber_mut(fid).push_frame(7, 0, 0, 0, 0);
        vm.scheduler.schedule_next().unwrap();
        vm.scheduler.block_for_queue();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.deadlock_err()));

        match result {
            Ok(VmError::Deadlock(msg)) => {
                assert!(msg.contains("missing function id 7"), "{msg}");
            }
            Ok(other) => panic!("deadlock diagnostic should return Deadlock, got {other:?}"),
            Err(_) => panic!("deadlock diagnostic with corrupt frame must not panic"),
        }
    }

    #[cfg(feature = "std")]
    #[test]
    fn create_island_without_module_returns_error_instead_of_expect_panic() {
        let mut vm = Vm::new();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.create_island()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(
                    msg.contains("create_island requires loaded module"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("create_island without module should be a VM error, got {other:?}"),
            Err(_) => panic!("create_island without module must not panic"),
        }
    }

    #[cfg(feature = "std")]
    #[test]
    fn load_missing_extern_returns_error_instead_of_registration_panic() {
        let mut module = malformed_single_instruction_module(
            "load-missing-extern",
            vec![Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0)],
            Vec::new(),
        );
        module.externs.push(ExternDef {
            name: "definitely_missing.extern".to_string(),
            param_slots: 0,
            ret_slots: 0,
            is_blocking: false,
            param_kinds: Vec::new(),
        });
        let mut vm = Vm::new();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.load(module)));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(msg.contains("unresolved extern functions"), "{msg}");
                assert!(msg.contains("definitely_missing.extern"), "{msg}");
            }
            Ok(other) => panic!("missing extern load should be a VM error, got {other:?}"),
            Err(_) => panic!("missing extern load must not panic"),
        }
    }

    fn run_gc_until_pause(vm: &mut Vm) {
        for _ in 0..10_000 {
            if !vm.state.gc.should_step() && vm.state.gc.state() == vo_runtime::gc::GcState::Pause {
                return;
            }
            vm.gc_step_after_fiber(None);
        }
        panic!(
            "GC did not reach pause state; state={:?} root_scan_pending={}",
            vm.state.gc.state(),
            vm.state.gc_root_scan.is_some(),
        );
    }

    fn run_until_atomic_root_scan_pending(vm: &mut Vm) {
        for _ in 0..10_000 {
            vm.gc_step_after_fiber(None);
            if vm.state.gc.state() == vo_runtime::gc::GcState::Atomic
                && vm.state.gc_root_scan.is_some()
            {
                return;
            }
        }
        panic!(
            "GC did not enter pending atomic root scan; state={:?} root_scan_pending={}",
            vm.state.gc.state(),
            vm.state.gc_root_scan.is_some(),
        );
    }

    #[test]
    fn run_scheduled_returns_suspended_when_waiting_for_island_response() {
        let mut vm = Vm::new();

        let fid = vm.scheduler.spawn(Fiber::new(0));
        vm.scheduler.schedule_next().unwrap();
        vm.scheduler.block_for_queue();
        vm.state.pending_island_responses = 1;

        let outcome = vm.run_scheduled().unwrap();

        assert_eq!(fid.to_raw(), 0);
        assert_eq!(outcome, SchedulingOutcome::Suspended);
    }

    #[test]
    fn command_queue_endpoint_response_wakes_blocked_fiber() {
        let mut vm = Vm::new();

        let fid = vm.scheduler.spawn(Fiber::new(0));
        {
            let fiber = vm.scheduler.get_fiber_mut(fid);
            fiber.push_frame(0, 0, 0, 0, 0);
            fiber.current_frame_mut().unwrap().pc = 1;
        }

        vm.scheduler.schedule_next().unwrap();
        vm.scheduler.block_for_queue();
        vm.state.pending_island_responses = 1;

        vm.push_island_command(IslandCommand::EndpointResponse {
            endpoint_id: 42,
            kind: EndpointResponseKind::SendAck { closed: true },
            fiber_id: fid.to_raw() as u64,
        });

        vm.process_island_commands();

        let fiber = vm.scheduler.get_fiber_mut(fid);
        assert!(fiber.consume_remote_send_closed());
        assert_eq!(fiber.current_frame().unwrap().pc, 0);
        assert_eq!(vm.state.pending_island_responses, 0);
        assert_eq!(
            vm.scheduler.get_fiber(fid).state,
            crate::fiber::FiberState::Runnable
        );
    }

    #[cfg(feature = "jit")]
    #[test]
    fn strict_jit_call_to_missing_function_is_jit_error_instead_of_index_panic() {
        let mut module = Module::new("missing-call-target-test".to_string());
        module.functions.push(FunctionDef {
            name: "main".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 1,
            gc_scan_slots: 0,
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: true,
            has_call_extern: false,
            code: vec![Instruction::with_flags(Opcode::Call, 0, 7, 0, 0)],
            jit_metadata: vec![vo_runtime::bytecode::JitInstructionMetadata::None],
            slot_types: vec![SlotType::Value],
            borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&[
                SlotType::Value,
            ]),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        });

        let mut vm = Vm::try_with_jit_config(JitConfig {
            call_threshold: 1,
            loop_threshold: 1,
            debug_ir: false,
        })
        .expect("strict JIT VM");
        vm.load(module).unwrap();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

        match result {
            Ok(Err(VmError::Jit(msg))) => {
                assert!(msg.contains("missing call target function id 7"), "{msg}");
            }
            Ok(other) => panic!("missing call target should be a JitError, got {other:?}"),
            Err(_) => panic!("missing call target must not panic in strict JIT mode"),
        }
    }

    #[test]
    fn dirty_fiber_root_scan_rescues_late_sweep_root() {
        let mut vm = Vm::new();
        vm.finish_load(gc_test_module());
        let fid = vm.scheduler.spawn(Fiber::new(0));
        {
            let fiber = vm.scheduler.get_fiber_mut(fid);
            fiber.push_frame(0, 1, 1, 0, 0);
        }

        let root = vm.state.gc.alloc(ValueMeta::new(1, ValueKind::Struct), 0);

        vm.gc_step();
        assert_eq!(vm.state.gc.state(), vo_runtime::gc::GcState::Atomic);
        vm.gc_step();
        assert_eq!(vm.state.gc.state(), vo_runtime::gc::GcState::Sweep);
        assert!(!vm.state.gc_roots_dirty_all);

        vm.scheduler.get_fiber_mut(fid).stack[0] = root as u64;
        vm.gc_step_after_fiber(Some(fid));

        let stats = vm.last_gc_step_stats();
        assert!(!stats.dirty_all_before);
        assert_eq!(stats.dirty_fiber_count, 1);
        assert!(stats.dirty_roots_scanned);
        assert!(!stats.full_roots_scanned);
        assert_eq!(stats.gc.root_scan_calls, 1);
        assert_eq!(vm.state.gc.canonicalize_ref(root), Some(root));
    }

    #[test]
    fn full_vm_root_scan_is_budgeted() {
        const ROOTS: u16 = 2048;
        let mut vm = Vm::new();
        vm.finish_load(gc_test_module_with_root_slots(ROOTS));
        let fid = vm.scheduler.spawn(Fiber::new(0));
        {
            let fiber = vm.scheduler.get_fiber_mut(fid);
            fiber.push_frame(0, ROOTS, ROOTS, 0, 0);
        }

        let meta = ValueMeta::new(1, ValueKind::Struct);
        for idx in 0..ROOTS as usize {
            let root = vm.state.gc.alloc(meta, 0);
            vm.scheduler.get_fiber_mut(fid).stack[idx] = root as u64;
        }

        vm.gc_step_after_fiber(None);
        let stats = vm.last_gc_step_stats();
        assert_eq!(stats.gc.root_scan_calls, 1);
        assert_eq!(stats.gc.root_scan_work_bytes, 8192);
        assert_eq!(stats.gc.object_scans, 0);
        assert!(vm.state.gc_root_scan.is_some());
        assert!(vm.state.gc_roots_dirty_all);

        vm.gc_step_after_fiber(None);
        let stats = vm.last_gc_step_stats();
        assert_eq!(stats.gc.root_scan_calls, 1);
        assert_eq!(stats.gc.root_scan_work_bytes, 8192);
        assert!(stats.full_roots_scanned);
        assert!(vm.state.gc_root_scan.is_none());
        assert!(!vm.state.gc_roots_dirty_all);
    }

    #[test]
    fn pending_start_cycle_root_scan_restarts_when_roots_mutate() {
        const ROOTS: u16 = 2048;
        let mut vm = Vm::new();
        vm.finish_load(gc_test_module_with_root_slots(ROOTS));
        let fid = vm.scheduler.spawn(Fiber::new(0));
        {
            let fiber = vm.scheduler.get_fiber_mut(fid);
            fiber.push_frame(0, ROOTS, ROOTS, 0, 0);
        }

        let meta = ValueMeta::new(1, ValueKind::Struct);
        for idx in 0..ROOTS as usize {
            let root = vm.state.gc.alloc(meta, 0);
            vm.scheduler.get_fiber_mut(fid).stack[idx] = root as u64;
        }
        let late_root = vm.state.gc.alloc(meta, 0);

        vm.gc_step_after_fiber(None);
        assert_eq!(vm.state.gc.state(), vo_runtime::gc::GcState::Propagate);
        assert!(vm.state.gc_root_scan.is_some());
        assert_eq!(vm.last_gc_step_stats().gc.root_scan_work_bytes, 8192);

        vm.scheduler.get_fiber_mut(fid).stack[0] = late_root as u64;
        vm.gc_step_after_fiber(Some(fid));

        let stats = vm.last_gc_step_stats();
        assert!(!stats.full_roots_scanned);
        assert!(vm.state.gc_root_scan.is_some());
        assert_eq!(stats.gc.root_scan_calls, 1);
        assert_eq!(stats.gc.root_scan_work_bytes, 8192);

        run_gc_until_pause(&mut vm);
        assert_eq!(vm.state.gc.canonicalize_ref(late_root), Some(late_root));
    }

    #[test]
    fn pending_atomic_root_scan_restarts_when_roots_mutate() {
        const ROOTS: u16 = 2048;
        let mut vm = Vm::new();
        vm.finish_load(gc_test_module_with_root_slots(ROOTS));
        let fid = vm.scheduler.spawn(Fiber::new(0));
        {
            let fiber = vm.scheduler.get_fiber_mut(fid);
            fiber.push_frame(0, ROOTS, ROOTS, 0, 0);
        }

        let meta = ValueMeta::new(1, ValueKind::Struct);
        for idx in 0..ROOTS as usize {
            let root = vm.state.gc.alloc(meta, 0);
            vm.scheduler.get_fiber_mut(fid).stack[idx] = root as u64;
        }
        let late_root = vm.state.gc.alloc(meta, 0);

        run_until_atomic_root_scan_pending(&mut vm);
        assert!(vm.state.gc_root_scan.is_some());

        vm.scheduler.get_fiber_mut(fid).stack[0] = late_root as u64;
        vm.gc_step_after_fiber(Some(fid));

        let stats = vm.last_gc_step_stats();
        assert!(!stats.full_roots_scanned);
        assert!(vm.state.gc_root_scan.is_some());
        assert_eq!(stats.gc.root_scan_calls, 1);
        assert_eq!(stats.gc.root_scan_work_bytes, 8192);

        run_gc_until_pause(&mut vm);
        assert_eq!(vm.state.gc.canonicalize_ref(late_root), Some(late_root));
    }

    #[test]
    fn finish_load_resets_pending_gc_root_scan_state() {
        const ROOTS: u16 = 2048;
        let mut vm = Vm::new();
        vm.finish_load(gc_test_module_with_root_slots(ROOTS));
        let fid = vm.scheduler.spawn(Fiber::new(0));
        {
            let fiber = vm.scheduler.get_fiber_mut(fid);
            fiber.push_frame(0, ROOTS, ROOTS, 0, 0);
        }

        for idx in 0..ROOTS as usize {
            let root = vm.state.gc.alloc(ValueMeta::new(1, ValueKind::Struct), 0);
            vm.scheduler.get_fiber_mut(fid).stack[idx] = root as u64;
        }

        vm.gc_step_after_fiber(None);
        assert!(vm.state.gc_root_scan.is_some());

        vm.state.gc_roots_dirty_all = false;
        vm.state.gc_dirty_fibers.push(fid.to_raw());
        let epoch_before = vm.state.gc_dirty_epoch;

        vm.finish_load(gc_test_module());

        assert!(vm.state.gc_root_scan.is_none());
        assert!(vm.state.gc_roots_dirty_all);
        assert!(vm.state.gc_dirty_fibers.is_empty());
        assert_eq!(vm.state.gc_dirty_epoch, epoch_before.wrapping_add(1));
        assert_eq!(vm.state.last_gc_step_stats.gc.root_scan_calls, 0);
        assert!(!vm.state.last_gc_step_stats.full_roots_scanned);
        assert!(!vm.state.last_gc_step_stats.dirty_roots_scanned);
    }

    #[test]
    fn stable_sweep_step_skips_vm_root_scan_when_roots_unchanged() {
        let mut vm = Vm::new();
        vm.finish_load(gc_test_module());
        let fid = vm.scheduler.spawn(Fiber::new(0));
        {
            let fiber = vm.scheduler.get_fiber_mut(fid);
            fiber.push_frame(0, 1, 1, 0, 0);
        }

        let root = vm.state.gc.alloc(ValueMeta::new(1, ValueKind::Struct), 0);
        vm.scheduler.get_fiber_mut(fid).stack[0] = root as u64;

        vm.gc_step_after_fiber(None);
        assert_eq!(vm.state.gc.state(), vo_runtime::gc::GcState::Atomic);
        assert!(vm.last_gc_step_stats().full_roots_scanned);

        vm.gc_step_after_fiber(None);
        assert_eq!(vm.state.gc.state(), vo_runtime::gc::GcState::Sweep);
        assert!(vm.last_gc_step_stats().full_roots_scanned);
        assert!(!vm.state.gc_roots_dirty_all);

        vm.gc_step_after_fiber(None);

        let stats = vm.last_gc_step_stats();
        assert!(stats.stable_roots_skipped);
        assert!(!stats.dirty_all_before);
        assert_eq!(stats.dirty_fiber_count, 0);
        assert!(!stats.full_roots_scanned);
        assert!(!stats.dirty_roots_scanned);
        assert_eq!(stats.gc.root_scan_calls, 0);
        assert_eq!(stats.gc.root_scan_skips, 1);
        assert_eq!(vm.state.gc.canonicalize_ref(root), Some(root));
    }

    #[test]
    fn duplicate_dirty_fiber_mark_does_not_advance_epoch_without_active_scan() {
        let mut vm = Vm::new();
        let fid = vm.scheduler.spawn(Fiber::new(0));
        vm.state.gc_roots_dirty_all = false;
        vm.state.gc_dirty_epoch = 7;

        vm.mark_gc_fiber_roots_dirty(fid);
        assert_eq!(vm.state.gc_dirty_epoch, 8);
        assert_eq!(vm.state.gc_dirty_fibers, vec![fid.to_raw()]);

        vm.mark_gc_fiber_roots_dirty(fid);
        assert_eq!(vm.state.gc_dirty_epoch, 8);
        assert_eq!(vm.state.gc_dirty_fibers, vec![fid.to_raw()]);

        vm.state.gc_root_scan = Some(VmRootScanSnapshot {
            kind: vo_runtime::gc::GcRootScanKind::Sweep,
            mode: VmRootScanMode::DirtyFibers,
            dirty_epoch: vm.state.gc_dirty_epoch,
            dirty_fibers: vec![fid.to_raw()],
            roots: Vec::new(),
            cursor: 0,
        });
        vm.mark_gc_fiber_roots_dirty(fid);
        assert_eq!(vm.state.gc_dirty_epoch, 9);
    }

    #[cfg(feature = "std")]
    #[test]
    fn run_scheduled_returns_interrupted_when_interrupt_flag_is_set() {
        let mut vm = Vm::new();
        vm.set_interrupt_flag(Arc::new(AtomicBool::new(true)));

        let err = vm.run_scheduled().unwrap_err();

        assert!(matches!(err, VmError::Interrupted));
    }

    #[test]
    fn handle_exec_result_propagates_interrupted_error() {
        let mut vm = Vm::new();

        let result = vm.handle_exec_result(ExecResult::Interrupted, false);

        assert!(matches!(result, Some(Err(VmError::Interrupted))));
    }

    #[test]
    fn blocked_exec_results_return_to_host_before_gc() {
        assert!(!exec_result_allows_gc_step(&ExecResult::Block(
            crate::fiber::BlockReason::Queue,
        )));
        assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Block(
            crate::fiber::BlockReason::Queue
        )));
        assert!(!exec_result_allows_gc_step(&ExecResult::Block(
            crate::fiber::BlockReason::HostEvent {
                token: 1,
                delay_ms: 0,
            },
        )));
        assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Block(
            crate::fiber::BlockReason::HostEvent {
                token: 1,
                delay_ms: 0,
            }
        )));
        assert!(!exec_result_allows_gc_step(&ExecResult::Block(
            crate::fiber::BlockReason::HostEventReplay(1),
        )));
        assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Block(
            crate::fiber::BlockReason::HostEventReplay(1)
        )));
        #[cfg(feature = "std")]
        assert!(!exec_result_allows_gc_step(&ExecResult::Block(
            crate::fiber::BlockReason::Io(1),
        )));
        #[cfg(feature = "std")]
        assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Block(
            crate::fiber::BlockReason::Io(1)
        )));

        assert!(exec_result_allows_gc_step(&ExecResult::TimesliceExpired));
        assert!(exec_result_marks_gc_fiber_roots_dirty(
            &ExecResult::TimesliceExpired
        ));
        assert!(exec_result_allows_gc_step(&ExecResult::Done));
        assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Done));
        assert!(!exec_result_marks_gc_fiber_roots_dirty(
            &ExecResult::Interrupted
        ));
    }
}
