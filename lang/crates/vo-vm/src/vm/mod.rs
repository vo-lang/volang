//! Virtual machine main structure.

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
#[cfg(not(feature = "std"))]
use alloc::collections::VecDeque;
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::sync::Arc;
#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(feature = "std")]
use std::collections::VecDeque;
#[cfg(feature = "std")]
use std::string::String;

#[cfg(feature = "std")]
use std::sync::Arc;
#[cfg(feature = "std")]
use std::vec::Vec;

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::{array, interface, string};
use vo_runtime::output::OutputSink;

mod extern_call;
pub(crate) mod helpers;
mod island_shared;
#[cfg(feature = "std")]
pub mod island_thread;
#[cfg(feature = "jit")]
mod jit;
mod types;

pub(crate) use extern_call::prepare_extern_closure_replay_call;
#[cfg(feature = "jit")]
pub(crate) use extern_call::prepare_typed_extern_closure_replay_setup;
pub(crate) use helpers::{stack_get, stack_set};
pub(crate) use island_shared::endpoint_response_from_authorized_source;
pub use types::EndpointRegistry;
pub(crate) use types::EndpointRegistrySnapshot;
pub use types::{
    ErrorLocation, ExecResult, GcRootEffect, RuntimeTrapKind, SchedulingOutcome,
    VmConstructionError, VmError, VmFiberRootScanStage, VmGcStepStats, VmIdentityExhausted,
    VmRootScanMode, VmRootScanSnapshot, VmRootScanStage, VmState, TIME_SLICE,
};
#[cfg(feature = "std")]
pub use types::{HostServicesUpdateError, IslandThread};

use extern_call::{apply_extern_replay_scope_effect, extern_result_to_transition, ExternBoundary};
use helpers::{
    runtime_panic, runtime_panic_msg, runtime_trap, slice_cap, slice_data_ptr, slice_len,
    string_index, string_len, user_panic,
};

#[cfg(feature = "jit")]
use crate::bytecode::ExternJitRoute;
use crate::bytecode::{FunctionDef, Module, TransferType};
use crate::exec;
use crate::fiber::{Fiber, FiberCapacityError};
use crate::runtime_boundary::{
    replay_current_instruction_policy, IslandCommandEffect, ResumePolicy, RuntimeBoundary,
    RuntimeCommand, RuntimeTransition, WakeCommand,
};
use vo_common_core::bytecode::{JitInstructionMetadata, ReturnFlags};

#[inline]
fn queue_layout_for_pc(func: &FunctionDef, pc: usize) -> Option<&[vo_runtime::SlotType]> {
    match func.jit_metadata.get(pc) {
        Some(JitInstructionMetadata::QueueLayout { elem_layout }) => Some(elem_layout.as_slice()),
        _ => None,
    }
}

#[inline]
fn conv_int_bits_to_float_bits(value: u64, flags: u8) -> u64 {
    let unsigned = flags & crate::instruction::CONV_FLAG_UNSIGNED != 0;
    if flags & crate::instruction::CONV_FLAG_FLOAT32 != 0 {
        if unsigned {
            (value as f32).to_bits() as u64
        } else {
            (value as i64 as f32).to_bits() as u64
        }
    } else {
        let converted = if unsigned {
            value as f64
        } else {
            value as i64 as f64
        };
        converted.to_bits()
    }
}

#[inline]
fn conv_f64_to_int_bits(value: f64, flags: u8) -> u64 {
    let unsigned = flags & crate::instruction::CONV_FLAG_UNSIGNED != 0;
    match (unsigned, crate::instruction::conv_f2i_width_bits(flags)) {
        (true, 8) => value as u8 as u64,
        (true, 16) => value as u16 as u64,
        (true, 32) => value as u32 as u64,
        (true, _) => value as u64,
        (false, 8) => value as i8 as i64 as u64,
        (false, 16) => value as i16 as i64 as u64,
        (false, 32) => value as i32 as i64 as u64,
        (false, _) => value as i64 as u64,
    }
}

fn map_key_value_layout_for_pc(
    func: &FunctionDef,
    pc: usize,
) -> Option<(&[vo_runtime::SlotType], &[vo_runtime::SlotType])> {
    match func.jit_metadata.get(pc) {
        Some(JitInstructionMetadata::MapGet {
            key_layout,
            val_layout,
            ..
        })
        | Some(JitInstructionMetadata::MapSet {
            key_layout,
            val_layout,
        })
        | Some(JitInstructionMetadata::MapIterNext {
            key_layout,
            val_layout,
        }) => Some((key_layout.as_slice(), val_layout.as_slice())),
        _ => None,
    }
}

fn map_new_layout_for_pc(
    func: &FunctionDef,
    pc: usize,
) -> Option<(&[vo_runtime::SlotType], &[vo_runtime::SlotType])> {
    match func.jit_metadata.get(pc) {
        Some(JitInstructionMetadata::MapNew {
            key_layout,
            val_layout,
        }) => Some((key_layout.as_slice(), val_layout.as_slice())),
        _ => None,
    }
}

fn map_get_layout_for_pc(
    func: &FunctionDef,
    pc: usize,
) -> Option<(&[vo_runtime::SlotType], &[vo_runtime::SlotType], bool)> {
    match func.jit_metadata.get(pc) {
        Some(JitInstructionMetadata::MapGet {
            key_layout,
            val_layout,
            has_ok,
        }) => Some((key_layout.as_slice(), val_layout.as_slice(), *has_ok)),
        _ => None,
    }
}

fn slot_elem_slots_for_pc(func: &FunctionDef, pc: usize) -> Option<usize> {
    match func.jit_metadata.get(pc) {
        Some(JitInstructionMetadata::SlotLayout { elem_layout }) => Some(elem_layout.len()),
        _ => None,
    }
}

fn map_key_layout_for_pc(func: &FunctionDef, pc: usize) -> Option<&[vo_runtime::SlotType]> {
    match func.jit_metadata.get(pc) {
        Some(JitInstructionMetadata::MapDelete { key_layout }) => Some(key_layout.as_slice()),
        _ => None,
    }
}

/// Result of wait_for_work() — what the scheduling loop should do next.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IslandIdMismatch {
    pub have: u32,
    pub got: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IslandTargetError {
    Mismatch(IslandIdMismatch),
    IdentityExhausted { requested: u32 },
}

impl core::fmt::Display for IslandTargetError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Mismatch(mismatch) => write!(
                f,
                "island id mismatch: VM owns {}, command targets {}",
                mismatch.have, mismatch.got
            ),
            Self::IdentityExhausted { requested } => write!(
                f,
                "cannot adopt island id {requested}: no successor identity remains"
            ),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for IslandTargetError {}

#[inline]
fn exec_result_allows_gc_step(result: &ExecResult) -> bool {
    !matches!(
        result,
        ExecResult::Block(_)
            | ExecResult::Transition(RuntimeTransition {
                boundary: RuntimeBoundary::Block(_),
                ..
            })
    )
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

#[cfg(feature = "jit")]
fn can_enter_materialized_frame_at_pc(
    func: &FunctionDef,
    pc: usize,
    resolved_externs: &crate::bytecode::ResolvedExternTable,
) -> bool {
    if !vo_jit::can_enter_materialized_frame_for_jit(func) {
        return false;
    }
    let Some(inst) = func.code.get(pc).copied() else {
        return false;
    };
    if inst.opcode() != Opcode::CallExtern {
        return true;
    }
    match resolved_externs
        .get(inst.b as u32)
        .map(|resolved| resolved.jit_route)
    {
        Some(ExternJitRoute::VmMaterializeBeforeCall) | None => false,
        Some(_) => true,
    }
}

use crate::instruction::{Instruction, Opcode};
use crate::scheduler::Scheduler;
use vo_runtime::itab::ItabCache;

#[cfg(feature = "jit")]
mod jit_mgr;

#[cfg(feature = "jit")]
use jit_mgr::JitManager;
#[cfg(feature = "jit")]
pub use jit_mgr::{JitConfig, JitExecutionStats, JitSideExitReason, JitSideExitReasonStats};

#[cfg(feature = "jit")]
#[derive(Default)]
enum VmJitState {
    #[default]
    Disabled,
    BestEffort(JitManager),
    Strict(JitManager),
}

#[cfg(feature = "jit")]
impl VmJitState {
    fn manager(&self) -> Option<&JitManager> {
        match self {
            Self::Disabled => None,
            Self::BestEffort(manager) | Self::Strict(manager) => Some(manager),
        }
    }

    fn manager_mut(&mut self) -> Option<&mut JitManager> {
        match self {
            Self::Disabled => None,
            Self::BestEffort(manager) | Self::Strict(manager) => Some(manager),
        }
    }

    fn is_enabled(&self) -> bool {
        !matches!(self, Self::Disabled)
    }

    fn is_strict(&self) -> bool {
        matches!(self, Self::Strict(_))
    }

    #[allow(clippy::result_large_err)]
    fn ensure_strict(&mut self) -> Result<&mut JitManager, vo_jit::JitError> {
        let manager = match core::mem::replace(self, Self::Disabled) {
            Self::Disabled => JitManager::new()?,
            Self::BestEffort(manager) | Self::Strict(manager) => manager,
        };
        *self = Self::Strict(manager);
        match self {
            Self::Strict(manager) => Ok(manager),
            Self::Disabled | Self::BestEffort(_) => Err(vo_jit::JitError::Internal(
                "failed to enter strict JIT state".to_string(),
            )),
        }
    }

    fn set_best_effort(&mut self, manager: JitManager) {
        *self = Self::BestEffort(manager);
    }

    fn set_strict(&mut self, manager: JitManager) {
        *self = Self::Strict(manager);
    }
}

pub struct Vm {
    #[cfg(feature = "std")]
    extension_loader: Option<vo_runtime::ext_loader::ExtensionLoader>,
    #[cfg(feature = "std")]
    extension_specs: Option<Vec<vo_runtime::ext_loader::NativeExtensionSpec>>,
    pub(crate) module: Option<Arc<Module>>,
    pub(crate) scheduler: Scheduler,
    pub(crate) state: VmState,
    exit_code: Option<i32>,
    pending_exit_code: Option<i32>,
    /// Remains true after the first fiber begins execution, including after
    /// every scheduler slot has reached a terminal state.
    execution_started: bool,
    /// JIT state is declared last so Rust's declaration-order field drop keeps
    /// executable memory alive until module, scheduler, and VM state are gone.
    /// Strict JIT entry points validate all function metadata before admission.
    #[cfg(feature = "jit")]
    jit: VmJitState,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StdlibRegistrationMode {
    TargetDefaults,
    EmbedderConfigured,
}

/// Owns the active module and fiber outside `Vm` for one execution slice.
/// Drop restores both owners during normal return and panic unwinding.
struct DetachedFiberExecution<'vm> {
    vm: &'vm mut Vm,
    fiber_id: crate::scheduler::FiberId,
    module: Arc<Module>,
    fiber: Option<Box<Fiber>>,
}

impl<'vm> DetachedFiberExecution<'vm> {
    fn try_new(vm: &'vm mut Vm, fiber_id: crate::scheduler::FiberId) -> Option<Self> {
        let module = vm.module.as_ref()?.clone();
        let fiber = vm.scheduler.detach_for_execution(fiber_id)?;
        Some(Self {
            vm,
            fiber_id,
            module,
            fiber: Some(fiber),
        })
    }

    fn run(&mut self) -> ExecResult {
        let module = self.module.as_ref();
        let Some(fiber) = self.fiber.as_mut() else {
            return ExecResult::JitError(
                "detached fiber execution attempted after ownership was restored".to_string(),
            );
        };
        self.vm.run_detached_fiber(self.fiber_id, fiber, module)
    }

    fn restore(&mut self) {
        if let Some(fiber) = self.fiber.take() {
            self.vm
                .scheduler
                .reattach_after_execution(self.fiber_id, fiber);
        }
    }
}

impl Drop for DetachedFiberExecution<'_> {
    fn drop(&mut self) {
        self.restore();
    }
}

#[cfg(feature = "jit")]
#[allow(clippy::result_large_err)]
fn validate_strict_jit_module(
    verified: vo_common_core::verifier::VerifiedModule<'_>,
) -> Result<(), vo_jit::JitError> {
    vo_jit::verify_module_after_common(verified)?;
    Ok(())
}

#[cfg(feature = "jit")]
fn strict_jit_load_error(err: vo_jit::JitError) -> VmError {
    VmError::Jit(err.to_string())
}

fn invalid_module_metadata(msg: impl Into<String>) -> VmError {
    VmError::Jit(format!("invalid module metadata: {}", msg.into()))
}

fn validate_vm_module(
    module: &Module,
) -> Result<vo_common_core::verifier::VerifiedModule<'_>, VmError> {
    vo_common_core::verifier::verify_module(module)
        .map_err(|err| invalid_module_metadata(err.to_string()))
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
    let ret_end = ret_start.saturating_add(extern_def.returns.slots as usize);
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

#[cfg(feature = "std")]
fn gc_env_flag_from<F>(get_env: &F, name: &str) -> bool
where
    F: Fn(&str) -> Option<String>,
{
    get_env(name)
        .map(|value| {
            matches!(
                value.as_str(),
                "1" | "true" | "TRUE" | "yes" | "YES" | "on" | "ON"
            )
        })
        .unwrap_or(false)
}

impl Vm {
    pub fn new() -> Self {
        Self::try_new().expect("VM construction failed")
    }

    pub fn try_new() -> Result<Self, VmConstructionError> {
        #[cfg(feature = "std")]
        return Self::try_new_with_state_factory(VmState::try_new);
        #[cfg(not(feature = "std"))]
        let state = VmState::try_new().map_err(VmConstructionError::Infallible)?;
        #[cfg(not(feature = "std"))]
        Ok(Self::from_state(state))
    }

    #[cfg(feature = "std")]
    fn try_new_with_state_factory(
        factory: impl FnOnce() -> std::io::Result<VmState>,
    ) -> Result<Self, VmConstructionError> {
        let state = factory().map_err(VmConstructionError::Io)?;
        Ok(Self::from_state(state))
    }

    fn from_state(state: VmState) -> Self {
        let mut vm = Self {
            #[cfg(feature = "jit")]
            jit: VmJitState::Disabled,
            #[cfg(feature = "std")]
            extension_loader: None,
            #[cfg(feature = "std")]
            extension_specs: None,
            module: None,
            scheduler: Scheduler::new(),
            state,
            exit_code: None,
            pending_exit_code: None,
            execution_started: false,
        };
        vm.apply_gc_environment();
        vm
    }

    fn apply_gc_environment(&mut self) {
        #[cfg(feature = "std")]
        {
            self.apply_gc_environment_from(|name| std::env::var(name).ok());
        }
    }

    #[cfg(feature = "std")]
    fn apply_gc_environment_from<F>(&mut self, get_env: F)
    where
        F: Fn(&str) -> Option<String>,
    {
        let debug_alias = gc_env_flag_from(&get_env, "VO_GC_DEBUG");
        if debug_alias || gc_env_flag_from(&get_env, "VO_GC_STRESS") {
            self.set_gc_stress_every_step(true);
        }
        if debug_alias || gc_env_flag_from(&get_env, "VO_GC_VERIFY") {
            self.set_gc_verify_after_step(true);
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
    #[deprecated(
        note = "non-strict best-effort API; use try_with_jit_config for strict JIT or with_best_effort_jit_config for explicit fallback"
    )]
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
    /// JIT initialization errors are swallowed and the VM runs interpreter-only.
    /// Strict execution paths must call [`Vm::try_with_jit_config`] instead.
    #[cfg(feature = "jit")]
    pub fn with_best_effort_jit_config(config: JitConfig) -> Self {
        let mut vm = Self::new();
        if let Ok(mgr) = JitManager::with_config(config) {
            vm.jit.set_best_effort(mgr);
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
    pub fn try_with_jit_config(config: JitConfig) -> Result<Self, VmConstructionError> {
        let mut vm = Self::try_new()?;
        vm.jit
            .set_strict(JitManager::with_config(config).map_err(VmConstructionError::Jit)?);
        Ok(vm)
    }

    /// Strictly initialize the JIT compiler.
    ///
    /// If a module is already loaded, validates strict JIT metadata before the
    /// VM can enter JIT mode and sizes dispatch tables for the loaded module.
    #[cfg(feature = "jit")]
    #[allow(clippy::result_large_err)]
    pub fn try_init_jit(&mut self) -> Result<(), vo_jit::JitError> {
        if let Some(module) = self.module.as_ref() {
            let verified = vo_common_core::verifier::verify_module(module)
                .map_err(|err| vo_jit::JitError::Internal(err.to_string()))?;
            validate_strict_jit_module(verified)?;
        }
        let jit_mgr = self.jit.ensure_strict()?;
        if let Some(module) = self.module.as_ref() {
            if jit_mgr.func_table_len() != module.functions.len() {
                jit_mgr.init(module.functions.len());
            }
        }
        Ok(())
    }

    /// Best-effort JIT initialization.
    ///
    /// Embedding callers may use this to opportunistically enable JIT. It
    /// prints a warning on failure and leaves the VM interpreter-only. Strict
    /// run paths must use [`Vm::try_init_jit`] or [`Vm::try_with_jit_config`].
    #[cfg(feature = "jit")]
    pub fn init_jit_best_effort(&mut self) {
        if self.jit.is_enabled() {
            return;
        }
        match JitManager::new() {
            Ok(mut mgr) => {
                if let Some(module) = self.module.as_ref() {
                    mgr.init(module.functions.len());
                }
                self.jit.set_best_effort(mgr);
            }
            Err(e) => {
                #[cfg(feature = "std")]
                eprintln!("Warning: best-effort JIT initialization failed: {}", e);
            }
        }
    }

    /// Deprecated alias for [`Vm::init_jit_best_effort`].
    ///
    /// This method is non-strict and may leave the VM without JIT enabled.
    /// New strict callers should use [`Vm::try_init_jit`].
    #[cfg(feature = "jit")]
    #[deprecated(
        note = "non-strict best-effort API; use try_init_jit for strict JIT or init_jit_best_effort for explicit fallback"
    )]
    pub fn init_jit(&mut self) {
        self.init_jit_best_effort();
    }

    /// Check if JIT is available and enabled.
    #[cfg(feature = "jit")]
    pub fn has_jit(&self) -> bool {
        self.jit.is_enabled()
    }

    #[cfg(feature = "jit")]
    pub fn jit_execution_stats(&self) -> JitExecutionStats {
        self.jit
            .manager()
            .map(|mgr| mgr.execution_stats())
            .unwrap_or_default()
    }

    #[cfg(feature = "jit")]
    pub fn jit_code_memory_stats(&self) -> vo_jit::JitCodeMemoryStats {
        self.jit
            .manager()
            .map(|mgr| mgr.code_memory_stats())
            .unwrap_or_default()
    }

    #[cfg(feature = "jit")]
    pub fn jit_unsupported_function_count(&self) -> usize {
        self.jit
            .manager()
            .map(|mgr| mgr.unsupported_function_count())
            .unwrap_or(0)
    }

    #[cfg(feature = "jit")]
    pub fn jit_function_compile_error(&self, func_id: u32) -> Option<&str> {
        self.jit
            .manager()
            .and_then(|mgr| mgr.function_compile_error(func_id))
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
    fn ensure_can_load_module(&self) -> Result<(), VmError> {
        if self.module.is_some() || !self.scheduler.fibers.is_empty() {
            return Err(VmError::Jit(
                "Vm::load cannot replace a loaded or previously run module; create a new Vm"
                    .to_string(),
            ));
        }
        Ok(())
    }

    pub fn module(&self) -> Option<&Module> {
        self.module.as_deref()
    }

    /// Exit status requested by `os.Exit`, retained for every later host poll.
    pub fn exit_code(&self) -> Option<i32> {
        self.exit_code.or(self.pending_exit_code)
    }

    #[cfg(feature = "std")]
    fn request_guest_exit(&mut self, code: i32) {
        if self.exit_code.is_none() && self.pending_exit_code.is_none() {
            self.pending_exit_code = Some(code);
        }
    }

    fn terminal_outcome(&mut self) -> Option<SchedulingOutcome> {
        if let Some(code) = self.exit_code {
            return Some(SchedulingOutcome::Exited(code));
        }
        let code = self.pending_exit_code.take()?;
        self.terminate_guest(code);
        Some(SchedulingOutcome::Exited(code))
    }

    /// Commit an `os.Exit` request at a scheduler boundary. This method must
    /// only run after an active interpreter/JIT fiber has returned its lease.
    fn terminate_guest(&mut self, code: i32) {
        if self.exit_code.is_some() {
            return;
        }
        self.exit_code = Some(code);
        self.pending_exit_code = None;

        #[cfg(feature = "std")]
        self.state.shutdown_island_threads();

        self.scheduler = Scheduler::new();
        self.state.command_queue.clear();
        self.state.outbound_commands.clear();
        self.state.endpoint_registry = EndpointRegistry::new();
        self.state.pending_island_responses = 0;
        #[cfg(feature = "jit")]
        {
            self.state.pending_runtime_transitions.clear();
            self.state.jit_osr_borrow_lease_depth = 0;
        }
        self.state.gc_root_scan = None;
        self.state.gc_dirty_fibers.clear();
        self.state.gc_roots_dirty_all = true;

        #[cfg(feature = "std")]
        self.state.io.shutdown();
    }

    /// Borrow the extern registry during the VM configuration phase.
    ///
    /// Loading freezes provider identities together with the resolved extern
    /// table. Replacing or mutating the registry after that point would split
    /// those two snapshots, so post-load callers must construct a new VM.
    pub fn extern_registry_mut(&mut self) -> Result<&mut vo_runtime::ExternRegistry, VmError> {
        if self.module.is_some()
            || self.state.extern_registry.is_frozen()
            || !self.scheduler.fibers.is_empty()
        {
            return Err(VmError::Jit(
                "extern providers can only be configured before Vm::load; create a new Vm"
                    .to_string(),
            ));
        }
        Ok(&mut self.state.extern_registry)
    }

    pub fn set_output_sink(&mut self, sink: Arc<dyn OutputSink>) {
        self.state.output = sink;
    }

    /// Install or replace the VM-scoped services exposed to native
    /// extensions. The owner is cloned into every child island created after
    /// this call.
    ///
    /// Service generations stay immutable once any VM fiber has started or an
    /// in-thread child island exists, so one execution observes one provider
    /// set.
    #[cfg(feature = "std")]
    pub fn set_host_services(
        &mut self,
        services: vo_runtime::host_services::SharedHostServices,
    ) -> Result<(), HostServicesUpdateError> {
        if !self.state.island_threads.is_empty() {
            return Err(HostServicesUpdateError::ActiveChildIslands {
                count: self.state.island_threads.len(),
            });
        }
        if self.execution_started {
            return Err(HostServicesUpdateError::ExecutionStarted);
        }
        self.state.host_services = Some(services);
        Ok(())
    }

    /// Remove the VM-scoped native-extension services.
    #[cfg(feature = "std")]
    pub fn clear_host_services(&mut self) -> Result<(), HostServicesUpdateError> {
        if !self.state.island_threads.is_empty() {
            return Err(HostServicesUpdateError::ActiveChildIslands {
                count: self.state.island_threads.len(),
            });
        }
        if self.execution_started {
            return Err(HostServicesUpdateError::ExecutionStarted);
        }
        self.state.host_services = None;
        Ok(())
    }

    #[cfg(feature = "std")]
    pub fn has_host_services(&self) -> bool {
        self.state.host_services.is_some()
    }

    pub fn set_program_args(&mut self, args: Vec<String>) {
        self.state.program_args = args.into_iter().map(String::into_bytes).collect();
    }

    pub fn set_program_args_bytes(&mut self, args: Vec<Vec<u8>>) {
        self.state.program_args = args;
    }

    #[cfg(feature = "std")]
    pub fn load(&mut self, module: Module) -> Result<(), VmError> {
        self.load_with_extensions(module, None)
    }

    #[cfg(not(feature = "std"))]
    pub fn load(&mut self, module: Module) -> Result<(), VmError> {
        self.load_with_stdlib_registration(module, StdlibRegistrationMode::TargetDefaults)
    }

    /// Load a module after the embedder has configured every non-VM extern
    /// provider through [`Vm::extern_registry_mut`].
    ///
    /// This skips automatic stdlib provider registration. The VM still adds
    /// its authoritative runtime providers, validates and resolves the full
    /// extern table transactionally, then freezes provider identities. The
    /// embedder must install both portable stdlib providers and its platform
    /// providers before calling this method.
    #[cfg(not(feature = "std"))]
    pub fn load_with_embedder_externs(&mut self, module: Module) -> Result<(), VmError> {
        self.load_with_stdlib_registration(module, StdlibRegistrationMode::EmbedderConfigured)
    }

    #[cfg(not(feature = "std"))]
    fn load_with_stdlib_registration(
        &mut self,
        module: Module,
        stdlib_registration: StdlibRegistrationMode,
    ) -> Result<(), VmError> {
        self.ensure_can_load_module()?;
        {
            let verified = validate_vm_module(&module)?;
            let mut staged_extern_registry = self.state.extern_registry.clone();
            if stdlib_registration == StdlibRegistrationMode::TargetDefaults {
                vo_stdlib::register_externs(&mut staged_extern_registry, &module.externs).map_err(
                    |err| VmError::Jit(format!("extern provider registration failed: {err}")),
                )?;
            }
            // VM-owned runtime providers are authoritative for VM state such
            // as the active fiber's materialized call stack. Register them
            // before extension tables so a conflicting provider is rejected.
            crate::runtime_externs::register_externs(&mut staged_extern_registry, &module.externs)
                .map_err(|err| {
                    VmError::Jit(format!("extern provider registration failed: {err}"))
                })?;
            let resolved_externs = staged_extern_registry
                .resolve_module_externs(&module.externs)
                .map_err(|err| VmError::Jit(format!("extern contract resolution failed: {err}")))?;
            #[cfg(not(feature = "jit"))]
            let _ = verified;

            #[cfg(feature = "jit")]
            if self.jit.is_strict() {
                validate_strict_jit_module(verified).map_err(strict_jit_load_error)?;
            }
            staged_extern_registry.freeze();
            self.state.extern_registry = staged_extern_registry;
            self.state.resolved_externs = resolved_externs;
        }

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
        self.load_shared_with_extensions(Arc::new(module), ext_loader)
    }

    /// Load a module after the embedder has configured every non-VM extern
    /// provider through [`Vm::extern_registry_mut`].
    ///
    /// This skips automatic stdlib and native-extension provider discovery.
    /// The VM still adds its authoritative runtime providers, validates and
    /// resolves the full extern table transactionally, then freezes provider
    /// identities. The embedder must install both portable stdlib providers
    /// and its platform providers before calling this method.
    #[cfg(feature = "std")]
    pub fn load_with_embedder_externs(&mut self, module: Module) -> Result<(), VmError> {
        self.load_shared_with_extensions_and_stdlib_registration(
            Arc::new(module),
            None,
            StdlibRegistrationMode::EmbedderConfigured,
        )
    }

    /// Load an immutable module already owned by a VM family. Island VMs use
    /// this path so bytecode and metadata are shared instead of deep-cloned.
    #[cfg(feature = "std")]
    pub(crate) fn load_shared_with_extensions(
        &mut self,
        module: Arc<Module>,
        ext_loader: Option<vo_runtime::ext_loader::ExtensionLoader>,
    ) -> Result<(), VmError> {
        self.load_shared_with_extensions_and_stdlib_registration(
            module,
            ext_loader,
            StdlibRegistrationMode::TargetDefaults,
        )
    }

    #[cfg(feature = "std")]
    fn load_shared_with_extensions_and_stdlib_registration(
        &mut self,
        module: Arc<Module>,
        ext_loader: Option<vo_runtime::ext_loader::ExtensionLoader>,
        stdlib_registration: StdlibRegistrationMode,
    ) -> Result<(), VmError> {
        self.ensure_can_load_module()?;
        {
            let verified = validate_vm_module(&module)?;
            let mut staged_extern_registry = self.state.extern_registry.clone();
            #[cfg(not(target_arch = "wasm32"))]
            if stdlib_registration == StdlibRegistrationMode::TargetDefaults {
                vo_stdlib::register_externs(&mut staged_extern_registry, &module.externs).map_err(
                    |err| VmError::Jit(format!("extern provider registration failed: {err}")),
                )?;
            }

            crate::runtime_externs::register_externs(&mut staged_extern_registry, &module.externs)
                .map_err(|err| {
                    VmError::Jit(format!("extern provider registration failed: {err}"))
                })?;

            // Build one owner-aware native catalog from linkme and dynamic
            // providers before resolving any extern. Longest module ownership
            // is selected before exact function lookup, independent of load
            // order and provider transport.
            if stdlib_registration == StdlibRegistrationMode::TargetDefaults {
                staged_extern_registry
                    .register_from_extension_catalogs(ext_loader.as_ref(), &module.externs)
                    .map_err(|err| {
                        VmError::Jit(format!("extern contract resolution failed: {err}"))
                    })?;
            }

            let resolved_externs = staged_extern_registry
                .resolve_module_externs(&module.externs)
                .map_err(|err| VmError::Jit(format!("extern contract resolution failed: {err}")))?;
            #[cfg(not(feature = "jit"))]
            let _ = verified;

            #[cfg(feature = "jit")]
            if self.jit.is_strict() {
                validate_strict_jit_module(verified).map_err(strict_jit_load_error)?;
            }
            staged_extern_registry.freeze();
            self.state.extern_registry = staged_extern_registry;
            self.state.resolved_externs = resolved_externs;
        }

        self.extension_specs = ext_loader.as_ref().map(|loader| loader.specs().to_vec());
        self.extension_loader = ext_loader;

        self.finish_load_shared(module);
        Ok(())
    }

    /// Finish loading a module (shared by load and load_with_extensions).
    #[cfg(any(test, not(feature = "std")))]
    fn finish_load(&mut self, module: Module) {
        self.finish_load_shared(Arc::new(module));
    }

    fn finish_load_shared(&mut self, module: Arc<Module>) {
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
        if let Some(jit_mgr) = self.jit.manager_mut() {
            jit_mgr.init(module.functions.len());
        }

        self.module = Some(module);
    }

    /// Create a new island - shared by VM interpreter and JIT callbacks.
    /// Returns the island handle (GcRef).
    #[cfg(feature = "std")]
    pub fn create_island(&mut self) -> Result<GcRef, VmError> {
        let module = if self.state.external_island_transport {
            None
        } else {
            Some(
                self.module
                    .as_ref()
                    .ok_or_else(|| {
                        VmError::Jit("create_island requires loaded module".to_string())
                    })?
                    .clone(),
            )
        };
        self.create_island_with_shared_module(module)
    }

    /// Create an island while an execution lease owns the active module.
    #[cfg(feature = "std")]
    pub(crate) fn create_island_for_execution(
        &mut self,
        _module: &Module,
    ) -> Result<GcRef, VmError> {
        self.create_island()
    }

    #[cfg(feature = "std")]
    fn create_island_with_shared_module(
        &mut self,
        module: Option<Arc<Module>>,
    ) -> Result<GcRef, VmError> {
        const ISLAND_STARTUP_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(10);
        self.create_island_with_shared_module_and_timeout(module, ISLAND_STARTUP_TIMEOUT)
    }

    #[cfg(feature = "std")]
    fn create_island_with_shared_module_and_timeout(
        &mut self,
        module: Option<Arc<Module>>,
        startup_timeout: std::time::Duration,
    ) -> Result<GcRef, VmError> {
        if !self.state.external_island_transport && module.is_none() {
            return Err(VmError::Jit(
                "create_island requires loaded module".to_string(),
            ));
        }
        let next_id = self
            .state
            .allocate_island_id()
            .map_err(|error| VmError::Jit(error.to_string()))?;
        if self.state.external_island_transport {
            return Ok(vo_runtime::island::create(&mut self.state.gc, next_id));
        }

        use vo_runtime::island_transport::{InThreadTransport, IslandSender};

        let module = module.expect("loaded module checked before island identity allocation");

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
        let registry_clone = registry.clone();
        let extension_specs = self.extension_specs.clone().unwrap_or_default();
        let host_services = self.state.host_services.clone();
        #[cfg(feature = "jit")]
        let jit_config = self.jit.manager().map(|mgr| mgr.config().clone());
        let (event_tx, event_rx) = std::sync::mpsc::channel();
        let startup_interrupt = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let child_interrupt = startup_interrupt.clone();
        let join_handle = std::thread::spawn(move || {
            #[cfg(feature = "jit")]
            let result = island_thread::run_island_thread(
                next_id,
                module,
                island_transport,
                registry_clone,
                extension_specs,
                host_services,
                jit_config,
                child_interrupt,
                &event_tx,
            );
            #[cfg(not(feature = "jit"))]
            let result = island_thread::run_island_thread(
                next_id,
                module,
                island_transport,
                registry_clone,
                extension_specs,
                host_services,
                child_interrupt,
                &event_tx,
            );
            let terminal = match result {
                Ok(island_thread::IslandThreadOutcome::Shutdown) => {
                    types::IslandThreadEvent::Exited
                }
                Ok(island_thread::IslandThreadOutcome::GuestExited(code)) => {
                    types::IslandThreadEvent::GuestExited(code)
                }
                Err(error) => types::IslandThreadEvent::Failed(error),
            };
            let _ = event_tx.send(terminal);
        });

        let startup = event_rx.recv_timeout(startup_timeout);
        if !matches!(startup, Ok(types::IslandThreadEvent::Ready)) {
            let guest_exit_code = match &startup {
                Ok(types::IslandThreadEvent::GuestExited(code)) => Some(*code),
                _ => None,
            };
            startup_interrupt.store(true, std::sync::atomic::Ordering::SeqCst);
            let _ = island_sender.send_command(
                self.state.current_island_id,
                vo_runtime::island::IslandCommand::Shutdown,
            );
            let timed_out = matches!(startup, Err(std::sync::mpsc::RecvTimeoutError::Timeout));
            if timed_out {
                // Initialization may currently be inside a synchronous extern
                // that cannot observe cancellation yet. Keep every ownership
                // edge alive so later polling or VM shutdown can join it.
                self.state.island_threads.push(IslandThread {
                    island_id: next_id,
                    join_handle: Some(join_handle),
                    events: event_rx,
                    interrupt_flag: startup_interrupt,
                    lifecycle: types::IslandThreadLifecycle::Stopping,
                });
            } else {
                let _ = join_handle.join();
                if let Ok(mut guard) = registry.lock() {
                    guard.remove(&next_id);
                }
                self.state.island_senders.remove(&next_id);
            }
            if let Some(code) = guest_exit_code {
                self.request_guest_exit(code);
            }
            let message = match startup {
                Ok(types::IslandThreadEvent::Failed(error)) => error,
                Ok(types::IslandThreadEvent::GuestExited(code)) => {
                    format!(
                        "island {next_id} requested guest exit with status {code} during startup"
                    )
                }
                Ok(types::IslandThreadEvent::Exited) => {
                    format!("island {next_id} exited during startup")
                }
                Ok(types::IslandThreadEvent::Ready) => {
                    format!("island {next_id} reported an inconsistent startup state")
                }
                Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
                    format!("island {next_id} startup timed out after {startup_timeout:?}")
                }
                Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
                    format!("island {next_id} terminated before reporting startup status")
                }
            };
            return Err(VmError::Jit(message));
        }

        // Save thread handle
        self.state.island_threads.push(IslandThread {
            island_id: next_id,
            join_handle: Some(join_handle),
            events: event_rx,
            interrupt_flag: startup_interrupt,
            lifecycle: types::IslandThreadLifecycle::Running,
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
        self.scheduler
            .try_spawn(fiber)
            .map_err(|err| VmError::Jit(err.to_string()))?;
        Ok(())
    }

    /// Spawn the entry function and run all fibers.
    ///
    /// Returns `Ok(outcome)` where outcome is one of:
    /// - `Completed`              — program exited normally
    /// - `Exited(code)`           — program called `os.Exit(code)`
    /// - `Blocked`                — all goroutines stuck on channels; call `deadlock_err()` for details
    /// - `Suspended`              — waiting for async host callbacks (WASM timer/HTTP, GUI events)
    /// - `SuspendedForHostEvents` — host events are registered and must be driven before resuming
    /// - `Panicked`               — a bounded run observed a panic terminal
    ///
    /// Callers decide whether `Blocked` is a deadlock error or expected behaviour (e.g. GUI host VM).
    pub fn run(&mut self) -> Result<SchedulingOutcome, VmError> {
        if let Some(outcome) = self.terminal_outcome() {
            return Ok(outcome);
        }
        self.spawn_entry()?;
        self.run_scheduling_loop(None)
    }

    /// Run island initialization only (global vars + user init functions, no main).
    ///
    /// Must be called on island VMs before processing SpawnFiber commands,
    /// otherwise global variables (including interface values) remain zero-initialized.
    pub fn run_init(&mut self) -> Result<SchedulingOutcome, VmError> {
        if let Some(outcome) = self.terminal_outcome() {
            return Ok(outcome);
        }
        let module = self.module.as_ref().ok_or(VmError::NoEntryFunction)?;
        let init_func = module.island_init_func;
        let func = module
            .functions
            .get(init_func as usize)
            .ok_or(VmError::InvalidFunctionId(init_func))?;
        let mut fiber = Fiber::new(0);
        fiber.push_frame(init_func, func.local_slots, func.gc_scan_slots, 0, 0);
        self.scheduler
            .try_spawn(fiber)
            .map_err(|err| VmError::Jit(err.to_string()))?;
        self.run_scheduling_loop(None)
    }

    /// Run existing fibers without spawning an entry fiber.
    ///
    /// Used for event dispatch after initial `run()`, island command handlers, and WASM async
    /// continuation.  Same outcome semantics as `run()`.
    pub fn run_scheduled(&mut self) -> Result<SchedulingOutcome, VmError> {
        if let Some(outcome) = self.terminal_outcome() {
            return Ok(outcome);
        }
        self.run_scheduling_loop(None)
    }

    pub fn push_targeted_island_command(
        &mut self,
        target_island_id: u32,
        cmd: vo_runtime::island::IslandCommand,
    ) -> Result<(), IslandTargetError> {
        self.push_targeted_island_command_from(self.state.current_island_id, target_island_id, cmd)
    }

    pub fn push_targeted_island_command_from(
        &mut self,
        source_island_id: u32,
        target_island_id: u32,
        cmd: vo_runtime::island::IslandCommand,
    ) -> Result<(), IslandTargetError> {
        let current_island_id = self.state.current_island_id;
        if current_island_id == 0 {
            self.adopt_island_id(target_island_id)?;
        } else if current_island_id != target_island_id {
            return Err(IslandTargetError::Mismatch(IslandIdMismatch {
                have: current_island_id,
                got: target_island_id,
            }));
        }
        self.push_island_command_from(source_island_id, cmd);
        Ok(())
    }

    pub fn push_island_command(&mut self, cmd: vo_runtime::island::IslandCommand) {
        self.push_island_command_from(self.state.current_island_id, cmd);
    }

    pub fn push_island_command_from(
        &mut self,
        source_island_id: u32,
        cmd: vo_runtime::island::IslandCommand,
    ) {
        self.mark_gc_all_roots_dirty();
        self.state
            .command_queue
            .push_back(vo_runtime::island::IslandCommandEnvelope::new(
                source_island_id,
                cmd,
            ));
    }

    pub fn take_outbound_commands(
        &mut self,
    ) -> VecDeque<(u32, vo_runtime::island::IslandCommandEnvelope)> {
        core::mem::take(&mut self.state.outbound_commands)
    }

    /// Encode and drain all outbound island commands atomically.
    ///
    /// The queue remains unchanged if any frame cannot be represented or its
    /// destination buffer cannot be reserved.
    pub fn try_take_outbound_transport_frames(
        &mut self,
    ) -> Result<Vec<Vec<u8>>, vo_runtime::island_msg::IslandMessageEncodeError> {
        let frame_count = self.state.outbound_commands.len();
        let mut frames = Vec::new();
        frames.try_reserve_exact(frame_count).map_err(|_| {
            vo_runtime::island_msg::IslandMessageEncodeError::AllocationFailed {
                field: "outbound island frame batch",
                requested: frame_count,
            }
        })?;
        for (target_island_id, envelope) in &self.state.outbound_commands {
            frames.push(vo_runtime::island_msg::encode_island_transport_frame(
                *target_island_id,
                envelope.source_island_id,
                &envelope.command,
            )?);
        }
        self.state.outbound_commands.clear();
        Ok(frames)
    }

    pub fn has_outbound_commands(&self) -> bool {
        !self.state.outbound_commands.is_empty()
    }

    pub fn take_pending_host_events(&mut self) -> Vec<crate::scheduler::PendingHostEvent> {
        self.scheduler.take_pending_host_events()
    }

    pub fn host_event_key(
        &self,
        source: crate::scheduler::HostWaitSource,
        token: u64,
    ) -> Option<crate::scheduler::HostWaitKey> {
        self.scheduler.host_event_key(source, token)
    }

    pub fn current_island_id(&self) -> u32 {
        self.state.current_island_id
    }

    fn adopt_island_id(&mut self, id: u32) -> Result<(), IslandTargetError> {
        let successor = id
            .checked_add(1)
            .ok_or(IslandTargetError::IdentityExhausted { requested: id })?;
        self.state.current_island_id = id;
        if self
            .state
            .next_island_id
            .is_some_and(|next_island_id| next_island_id <= id)
        {
            self.state.next_island_id = Some(successor);
        }
        Ok(())
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
        if !self.scheduler.fibers.is_empty() {
            self.execution_started = true;
        }
        let mut iterations = 0;

        loop {
            if let Some(outcome) = self.terminal_outcome() {
                return Ok(outcome);
            }
            if self.interrupt_requested() {
                return Err(VmError::Interrupted);
            }
            if let Some(max) = max_iterations {
                iterations += 1;
                if iterations > max {
                    self.apply_runtime_transition(
                        self.scheduler.current,
                        RuntimeTransition::new(
                            RuntimeBoundary::Yield,
                            ResumePolicy::PreserveFramePc,
                            GcRootEffect::None,
                        ),
                    )?;
                    break;
                }
            }

            self.process_island_commands()?;
            if let Some(outcome) = self.terminal_outcome() {
                return Ok(outcome);
            }

            if !self.scheduler.has_work() {
                match self.wait_for_work()? {
                    WaitResult::Retry => continue,
                    WaitResult::Done => return Ok(SchedulingOutcome::Completed),
                    #[cfg(feature = "std")]
                    WaitResult::Interrupted => return Err(VmError::Interrupted),
                    WaitResult::Blocked => return Ok(SchedulingOutcome::Blocked),
                    WaitResult::Suspended => return Ok(SchedulingOutcome::Suspended),
                    WaitResult::SuspendedForHostEvents => {
                        return Ok(SchedulingOutcome::SuspendedForHostEvents);
                    }
                    #[cfg(feature = "std")]
                    WaitResult::Break => break,
                }
            }

            let fiber_id = match self.next_fiber_for_turn() {
                Some(id) => id,
                None => break,
            };

            let result = self.run_fiber(fiber_id);
            let _runtime_boundary = Self::runtime_boundary_for_exec_result(&result);
            let gc_after_boundary = exec_result_allows_gc_step(&result);
            let gc_root_effect = if exec_result_marks_gc_fiber_roots_dirty(&result) {
                GcRootEffect::CurrentFiberDirty
            } else {
                GcRootEffect::None
            };

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
            if !matches!(
                handled,
                Some(Err(_)) | Some(Ok(SchedulingOutcome::Exited(_)))
            ) {
                self.apply_runtime_transition(
                    Some(fiber_id),
                    RuntimeTransition::continue_with_gc_roots(gc_root_effect),
                )?;
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

    fn next_fiber_for_turn(&mut self) -> Option<crate::scheduler::FiberId> {
        if let Some(id) = self.scheduler.current {
            if self
                .scheduler
                .try_get_fiber(id)
                .is_some_and(|fiber| fiber.state.is_running())
            {
                return Some(id);
            }
        }
        self.scheduler.schedule_next()
    }

    /// Process commands from other island threads (non-blocking).
    #[inline]
    fn process_island_commands(&mut self) -> Result<(), VmError> {
        #[cfg(feature = "std")]
        if let Some(code) = self.poll_island_thread_events()? {
            self.terminate_guest(code);
            return Ok(());
        }
        let mut cmds = Vec::new();
        #[cfg(feature = "std")]
        if let Some(ref transport) = self.state.main_transport {
            while let Ok(Some(envelope)) = transport.try_recv() {
                cmds.push(envelope);
            }
        }
        while let Some(envelope) = self.state.command_queue.pop_front() {
            cmds.push(envelope);
        }
        if !cmds.is_empty() {
            self.mark_gc_all_roots_dirty();
        }
        for envelope in cmds {
            self.dispatch_queued_island_command_from(envelope.source_island_id, envelope.command)?;
        }
        self.state.clear_endpoint_tombstones_if_quiescent();
        Ok(())
    }

    #[cfg(feature = "std")]
    fn poll_island_thread_events(&mut self) -> Result<Option<i32>, VmError> {
        let mut index = 0;
        while index < self.state.island_threads.len() {
            if self.state.island_threads[index].lifecycle == types::IslandThreadLifecycle::Stopping
            {
                let mut guest_exit = None;
                loop {
                    match self.state.island_threads[index].events.try_recv() {
                        Ok(types::IslandThreadEvent::GuestExited(code)) => {
                            guest_exit.get_or_insert(code);
                        }
                        Ok(
                            types::IslandThreadEvent::Ready
                            | types::IslandThreadEvent::Failed(_)
                            | types::IslandThreadEvent::Exited,
                        ) => {}
                        Err(std::sync::mpsc::TryRecvError::Empty) => break,
                        Err(std::sync::mpsc::TryRecvError::Disconnected) => break,
                    }
                }

                if let Some(code) = guest_exit {
                    return Ok(Some(code));
                }

                let finished = self.state.island_threads[index]
                    .join_handle
                    .as_ref()
                    .is_none_or(std::thread::JoinHandle::is_finished);
                if finished {
                    let mut island = self.state.island_threads.remove(index);
                    if let Some(handle) = island.join_handle.take() {
                        let _ = handle.join();
                    }
                    self.state.island_senders.remove(&island.island_id);
                    if let Some(registry) = &self.state.island_registry {
                        if let Ok(mut registry) = registry.lock() {
                            registry.remove(&island.island_id);
                        }
                    }
                    continue;
                }

                index += 1;
                continue;
            }

            let island = &mut self.state.island_threads[index];
            match island.events.try_recv() {
                Ok(types::IslandThreadEvent::Ready) => {
                    return Err(VmError::Jit(format!(
                        "island {} reported duplicate startup readiness",
                        island.island_id
                    )));
                }
                Ok(types::IslandThreadEvent::Failed(error)) => {
                    return Err(VmError::Jit(format!(
                        "island {} failed: {error}",
                        island.island_id
                    )));
                }
                Ok(types::IslandThreadEvent::GuestExited(code)) => {
                    return Ok(Some(code));
                }
                Ok(types::IslandThreadEvent::Exited) => {
                    return Err(VmError::Jit(format!(
                        "island {} exited while the parent VM was active",
                        island.island_id
                    )));
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                    return Err(VmError::Jit(format!(
                        "island {} disconnected without a terminal event",
                        island.island_id
                    )));
                }
            }
            index += 1;
        }
        Ok(None)
    }

    /// Dispatch a single island command on the main island.
    pub(crate) fn preflight_endpoint_request_command(
        &self,
        endpoint_id: u64,
        kind: &vo_runtime::island::EndpointRequestKind,
        from_island: u32,
        fiber_key: u64,
        wait_id: u64,
    ) -> Result<(), VmError> {
        island_shared::preflight_endpoint_request_command(
            self,
            endpoint_id,
            kind,
            from_island,
            fiber_key,
            wait_id,
        )
    }

    pub(crate) fn dispatch_island_command(
        &mut self,
        cmd: vo_runtime::island::IslandCommand,
    ) -> Result<(), VmError> {
        self.dispatch_island_command_from(self.state.current_island_id, cmd)
    }

    pub(crate) fn dispatch_island_command_from(
        &mut self,
        source_island_id: u32,
        cmd: vo_runtime::island::IslandCommand,
    ) -> Result<(), VmError> {
        use vo_runtime::island::IslandCommand;
        match cmd {
            IslandCommand::SpawnFiber { closure_data } => {
                island_shared::handle_spawn_fiber(self, closure_data.data())
                    .map_err(|err| VmError::Jit(err.to_string()))?;
            }
            IslandCommand::WakeFiber { waiter } => {
                if source_island_id != self.state.current_island_id {
                    return Err(VmError::Jit(
                        "WakeFiber transport source was rejected".to_string(),
                    ));
                }
                let outcome = self.apply_runtime_command(RuntimeCommand::island_wake(waiter));
                if !outcome.applied || !outcome.payload_accepted {
                    return Err(VmError::Jit(
                        "island wake command was rejected by the VM".to_string(),
                    ));
                }
            }
            IslandCommand::EndpointRequest {
                endpoint_id,
                kind,
                from_island,
                fiber_key,
                wait_id,
            } => {
                if source_island_id != from_island {
                    return Err(VmError::Jit(
                        "endpoint request transport source was rejected".to_string(),
                    ));
                }
                island_shared::handle_endpoint_request_command(
                    self,
                    endpoint_id,
                    kind,
                    from_island,
                    fiber_key,
                    wait_id,
                )?;
            }
            IslandCommand::EndpointResponse {
                endpoint_id,
                kind,
                from_island,
                fiber_key,
                wait_id,
            } => {
                if source_island_id != from_island {
                    return Err(VmError::Jit(
                        "endpoint response transport source was rejected".to_string(),
                    ));
                }
                island_shared::handle_endpoint_response_command(
                    self,
                    endpoint_id,
                    kind,
                    from_island,
                    fiber_key,
                    wait_id,
                )?;
            }
            IslandCommand::Shutdown => {}
        }
        Ok(())
    }

    fn dispatch_queued_island_command_from(
        &mut self,
        source_island_id: u32,
        cmd: vo_runtime::island::IslandCommand,
    ) -> Result<(), VmError> {
        match cmd {
            vo_runtime::island::IslandCommand::WakeFiber { .. } => Err(VmError::Jit(
                "WakeFiber transport ingress was rejected".to_string(),
            )),
            command => self.dispatch_island_command_from(source_island_id, command),
        }
    }

    #[cfg(feature = "std")]
    pub(crate) fn poll_io_ready_commands(&mut self) -> usize {
        let ready = self.scheduler.poll_io_ready_tokens(&mut self.state.io);
        let mut applied = 0;
        for token in ready {
            let Some(key) = self.scheduler.io_wait_key(token) else {
                continue;
            };
            let outcome = self.apply_runtime_command(RuntimeCommand::io_ready(key));
            if outcome.applied {
                applied += 1;
            }
        }
        applied
    }

    /// When no fibers are runnable, try to make progress via I/O polling or
    /// island command waiting. Returns what the scheduling loop should do next.
    fn wait_for_work(&mut self) -> Result<WaitResult, VmError> {
        #[cfg(feature = "std")]
        if self.interrupt_requested() {
            return Ok(WaitResult::Interrupted);
        }
        // Try I/O polling first
        #[cfg(feature = "std")]
        {
            if self.poll_io_ready_commands() > 0 {
                return Ok(WaitResult::Retry);
            }
        }

        if !self.state.command_queue.is_empty() {
            return Ok(WaitResult::Retry);
        }

        if !self.state.outbound_commands.is_empty() || self.state.pending_island_responses > 0 {
            return Ok(WaitResult::Suspended);
        }

        // Host event waiters are owned by the host loop; an idle island
        // transport must not mask that suspension point.
        if self.scheduler.has_host_event_waiters() {
            self.state.clear_endpoint_tombstones_if_quiescent();
            return Ok(WaitResult::SuspendedForHostEvents);
        }

        // A transport handle alone is not evidence that a blocked local queue
        // can make progress. If no I/O waiters, live endpoints, or pending
        // island responses exist, surface the blocked/deadlock state instead
        // of spinning through transport timeouts forever.
        #[cfg(feature = "std")]
        if self.scheduler.has_blocked()
            && !self.scheduler.has_io_waiters()
            && self.state.current_island_id == 0
            && !self.state.endpoint_registry.has_live()
        {
            self.state.clear_endpoint_tombstones_if_quiescent();
            return Ok(WaitResult::Blocked);
        }

        // Try waiting for island commands
        #[cfg(feature = "std")]
        if self.scheduler.has_blocked() && self.state.main_transport.is_some() {
            if let Some(ref transport) = self.state.main_transport {
                match transport.recv_timeout(std::time::Duration::from_millis(100)) {
                    Ok(envelope) => {
                        self.mark_gc_all_roots_dirty();
                        self.dispatch_queued_island_command_from(
                            envelope.source_island_id,
                            envelope.command,
                        )?;
                        self.state.clear_endpoint_tombstones_if_quiescent();
                        return Ok(WaitResult::Retry);
                    }
                    Err(vo_runtime::island_transport::TransportError::Timeout) => {
                        self.poll_io_ready_commands();
                        return Ok(WaitResult::Retry);
                    }
                    Err(vo_runtime::island_transport::TransportError::Disconnected) => {
                        self.state.clear_endpoint_tombstones_if_quiescent();
                        return Ok(WaitResult::Break);
                    }
                }
            }
        }

        // Check if there are waiters that might still make progress
        #[cfg(feature = "std")]
        if self.scheduler.has_io_waiters() || self.scheduler.has_blocked() {
            if self.state.current_island_id != 0 {
                if self.scheduler.has_io_waiters() {
                    self.poll_io_ready_commands();
                }
                self.state.clear_endpoint_tombstones_if_quiescent();
                return Ok(WaitResult::Break);
            }
            if !self.scheduler.has_io_waiters() && self.state.main_transport.is_none() {
                // If there are live cross-island endpoints, blocked fibers may be
                // waiting for remote island responses delivered via push_island_command.
                // Return Suspended so the host event loop keeps running.
                if self.state.endpoint_registry.has_live() {
                    self.state.clear_endpoint_tombstones_if_quiescent();
                    return Ok(WaitResult::Suspended);
                }
                self.state.clear_endpoint_tombstones_if_quiescent();
                return Ok(WaitResult::Blocked);
            }
            self.poll_io_ready_commands();
            std::thread::sleep(std::time::Duration::from_millis(10));
            return Ok(WaitResult::Retry);
        }

        #[cfg(not(feature = "std"))]
        if self.scheduler.has_blocked() {
            if self.state.endpoint_registry.has_live() {
                self.state.clear_endpoint_tombstones_if_quiescent();
                return Ok(WaitResult::Suspended);
            }
            self.state.clear_endpoint_tombstones_if_quiescent();
            return Ok(WaitResult::Blocked);
        }

        self.state.clear_endpoint_tombstones_if_quiescent();
        Ok(WaitResult::Done)
    }

    /// Wake a fiber blocked on a host-side event and schedule it to run.
    /// Called by host loops after carrying back the VM-issued host wait key.
    pub fn wake_host_event(&mut self, key: crate::scheduler::HostWaitKey) -> bool {
        self.apply_runtime_command(RuntimeCommand::host_event_wake(key))
            .applied
    }

    /// Compatibility adapter for timer-only callers still carrying source-local tokens.
    pub fn wake_host_event_legacy_timer_token(&mut self, token: u64) -> bool {
        let _ = token;
        false
    }

    /// Compatibility adapter for replay-only callers still carrying source-local tokens.
    pub fn wake_host_event_legacy_replay_token(&mut self, token: u64) -> bool {
        let _ = token;
        false
    }

    /// Wake a fiber blocked on a host-side event, attaching opaque data.
    /// The FFI function reads the data on replay via `ctx.take_resume_host_event_data()`.
    pub fn wake_host_event_with_data(
        &mut self,
        key: crate::scheduler::HostWaitKey,
        data: Vec<u8>,
    ) -> bool {
        self.apply_runtime_command(RuntimeCommand::host_event_wake_with_data(key, data))
            .payload_accepted
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
        if let Some(code) = self.pending_exit_code.take() {
            self.terminate_guest(code);
            return Some(Ok(SchedulingOutcome::Exited(code)));
        }
        match result {
            ExecResult::Transition(transition) => {
                if let Err(err) = self.apply_runtime_transition(self.scheduler.current, transition)
                {
                    return Some(Err(err));
                }
            }
            ExecResult::TimesliceExpired => {
                if let Err(err) = self.apply_runtime_transition(
                    self.scheduler.current,
                    RuntimeTransition::new(
                        RuntimeBoundary::Yield,
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::None,
                    ),
                ) {
                    return Some(Err(err));
                }
            }
            ExecResult::Interrupted => {
                return Some(Err(VmError::Interrupted));
            }
            ExecResult::Exit(code) => {
                self.terminate_guest(code);
                return Some(Ok(SchedulingOutcome::Exited(code)));
            }
            ExecResult::Block(reason) => {
                if let Err(err) = self.apply_runtime_transition(
                    self.scheduler.current,
                    RuntimeTransition::new(
                        RuntimeBoundary::Block(reason),
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::None,
                    ),
                ) {
                    return Some(Err(err));
                }
            }
            ExecResult::Done => {
                if let Err(err) = self.apply_runtime_transition(
                    self.scheduler.current,
                    RuntimeTransition::new(
                        RuntimeBoundary::Done,
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::None,
                    ),
                ) {
                    return Some(Err(err));
                }
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
                return Some(
                    self.apply_runtime_transition(
                        self.scheduler.current,
                        RuntimeTransition::fatal_infra(msg),
                    )
                    .map(|_| SchedulingOutcome::Completed),
                );
            }
            ExecResult::FrameChanged | ExecResult::CallClosure { .. } => {
                debug_assert!(
                    false,
                    "internal ExecResult leaked to scheduling loop: {:?}",
                    result
                );
                if let Err(err) = self.apply_runtime_transition(
                    self.scheduler.current,
                    RuntimeTransition::new(
                        RuntimeBoundary::Yield,
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::None,
                    ),
                ) {
                    return Some(Err(err));
                }
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
        let result = {
            let Some(mut execution) = DetachedFiberExecution::try_new(self, fiber_id) else {
                return ExecResult::Done;
            };
            execution.run()
        };

        #[cfg(feature = "jit")]
        let result = self.attach_pending_runtime_transitions(result);
        result
    }

    /// Execute with the active module and fiber owned outside `Vm`.
    ///
    /// This makes the JIT/FFI callback contract structurally disjoint: callbacks
    /// may borrow VM services while the active fiber and immutable module remain
    /// independent values, with runtime transitions committed after reattachment.
    fn run_detached_fiber(
        &mut self,
        fiber_id: crate::scheduler::FiberId,
        fiber: &mut Fiber,
        module: &Module,
    ) -> ExecResult {
        fiber.execution_budget = TIME_SLICE;
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

        macro_rules! refetch_after_frame_change {
            () => {{
                self.mark_gc_fiber_roots_dirty(fiber_id);
                refetch!();
            }};
        }

        // Macro to handle panic/trap results that may return FrameChanged (when defer/recover exists).
        // Without this, `return runtime_trap(...)` would leak FrameChanged to the scheduling loop.
        macro_rules! handle_panic_result {
            ($result:expr) => {{
                let r = $result;
                if matches!(r, ExecResult::FrameChanged) {
                    #[cfg(feature = "jit")]
                    if !self.state.pending_runtime_transitions.is_empty() {
                        return ExecResult::FrameChanged;
                    }
                    stack = fiber.stack_ptr();
                    refetch_after_frame_change!();
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
                if let Some(osr_result) =
                    jit::try_loop_osr(self, fiber, module, func_id, $target_pc, bp)
                {
                    match osr_result {
                        jit::OsrResult::Exit(code) => {
                            return ExecResult::Exit(code);
                        }
                        jit::OsrResult::FrameChanged => {
                            if !self.state.pending_runtime_transitions.is_empty() {
                                return ExecResult::FrameChanged;
                            }
                            stack = fiber.stack_ptr();
                            refetch_after_frame_change!();
                            continue;
                        }
                        jit::OsrResult::Transition(transition) => {
                            return ExecResult::Transition(transition);
                        }
                        jit::OsrResult::ExitPc(exit_pc) => {
                            let Some(frame) = fiber.current_frame_mut() else {
                                return ExecResult::JitError(
                                    "OsrResult::ExitPc without active frame".to_string(),
                                );
                            };
                            frame.pc = exit_pc;
                            if !self.state.pending_runtime_transitions.is_empty() {
                                return ExecResult::FrameChanged;
                            }
                            stack = fiber.stack_ptr();
                            refetch_after_frame_change!();
                            continue;
                        }
                        jit::OsrResult::Panic => {
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
                    exec::QueueAction::Block { waiter } => {
                        if let Some(waiter) = waiter.as_ref() {
                            fiber.begin_queue_wait(waiter);
                        } else {
                            fiber.clear_queue_wait();
                        }
                        return ExecResult::Block(crate::fiber::BlockReason::Queue);
                    }
                    exec::QueueAction::ReplayThenBlock { waiter } => {
                        let resume =
                            match replay_current_instruction_policy(fiber, "Queue ReplayThenBlock")
                            {
                                Ok(resume) => resume,
                                Err(msg) => return ExecResult::JitError(msg),
                            };
                        if let Some(waiter) = waiter.as_ref() {
                            fiber.begin_queue_wait(waiter);
                        } else {
                            fiber.clear_queue_wait();
                        }
                        return ExecResult::Transition(RuntimeTransition::new(
                            RuntimeBoundary::Block(crate::fiber::BlockReason::Queue),
                            resume,
                            GcRootEffect::CurrentFiberDirty,
                        ));
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
                    exec::QueueAction::Wake { waiter, payload } => {
                        let mut transition = RuntimeTransition::new(
                            RuntimeBoundary::Yield,
                            ResumePolicy::PreserveFramePc,
                            GcRootEffect::CurrentFiberDirty,
                        );
                        transition.wakes.push(match payload {
                            Some(payload) => WakeCommand::queue_waiter_with_result(waiter, payload),
                            None => WakeCommand::queue_waiter(waiter),
                        });
                        return ExecResult::Transition(transition);
                    }
                    exec::QueueAction::Close {
                        receivers,
                        senders,
                        endpoint_id,
                        rollback,
                    } => {
                        let mut transition = RuntimeTransition::new(
                            RuntimeBoundary::Yield,
                            ResumePolicy::PreserveFramePc,
                            GcRootEffect::CurrentFiberDirty,
                        );
                        transition.set_rollback(rollback);
                        for waiter in receivers {
                            transition.push_queue_close_wake(WakeCommand::queue_closed_receiver(
                                waiter,
                                endpoint_id,
                            ));
                        }
                        for waiter in senders {
                            transition.push_queue_close_wake(WakeCommand::queue_closed_sender(
                                waiter,
                                endpoint_id,
                            ));
                        }
                        if let Some(endpoint_id) = endpoint_id {
                            island_shared::append_closed_home_endpoint_effects(
                                self,
                                endpoint_id,
                                None,
                                &mut transition,
                            );
                        }
                        return ExecResult::Transition(transition);
                    }
                    exec::QueueAction::RemoteSend {
                        endpoint_id,
                        home_island,
                        data,
                        mut island_effects,
                        transfer_commit,
                    } => {
                        fiber.clear_queue_wait();
                        let fiber_key = fiber.endpoint_response_key();
                        let wait_id = match fiber.try_begin_remote_endpoint_send_wait(endpoint_id) {
                            Ok(wait_id) => wait_id,
                            Err(err) => return ExecResult::JitError(err.to_string()),
                        };
                        let mut transition = RuntimeTransition::new(
                            RuntimeBoundary::Block(crate::fiber::BlockReason::Queue),
                            ResumePolicy::PreserveFramePc,
                            GcRootEffect::CurrentFiberDirty,
                        );
                        transition.island_commands.append(&mut island_effects);
                        transition.island_commands.push(
                            IslandCommandEffect::endpoint_send_request(
                                home_island,
                                endpoint_id,
                                data,
                                self.state.current_island_id,
                                fiber_key,
                                wait_id,
                            ),
                        );
                        if let Some(rollback) = transfer_commit.into_runtime_rollback() {
                            transition.set_rollback(rollback);
                        }
                        return ExecResult::Transition(transition);
                    }
                    exec::QueueAction::RemoteRecv {
                        endpoint_id,
                        home_island,
                    } => {
                        let resume =
                            match replay_current_instruction_policy(fiber, "Queue RemoteRecv") {
                                Ok(resume) => resume,
                                Err(msg) => return ExecResult::JitError(msg),
                            };
                        fiber.clear_queue_wait();
                        let fiber_key = fiber.endpoint_response_key();
                        let wait_id = match fiber.try_begin_remote_endpoint_recv_wait(endpoint_id) {
                            Ok(wait_id) => wait_id,
                            Err(err) => return ExecResult::JitError(err.to_string()),
                        };
                        let mut transition = RuntimeTransition::new(
                            RuntimeBoundary::Block(crate::fiber::BlockReason::Queue),
                            resume,
                            GcRootEffect::CurrentFiberDirty,
                        );
                        transition.island_commands.push(
                            IslandCommandEffect::endpoint_recv_request(
                                home_island,
                                endpoint_id,
                                self.state.current_island_id,
                                fiber_key,
                                wait_id,
                            ),
                        );
                        return ExecResult::Transition(transition);
                    }
                    exec::QueueAction::RemoteSendAck {
                        endpoint_id,
                        target_island,
                        fiber_key,
                        wait_id,
                        closed,
                        rollback,
                    } => {
                        let mut transition = RuntimeTransition::new(
                            RuntimeBoundary::Yield,
                            ResumePolicy::PreserveFramePc,
                            GcRootEffect::CurrentFiberDirty,
                        );
                        transition
                            .island_commands
                            .push(IslandCommandEffect::endpoint_response(
                                target_island,
                                self.state.current_island_id,
                                endpoint_id,
                                vo_runtime::island::EndpointResponseKind::SendAck { closed },
                                fiber_key,
                                wait_id,
                            ));
                        if let Some(rollback) = rollback {
                            transition.set_rollback(rollback);
                        }
                        return ExecResult::Transition(transition);
                    }
                    exec::QueueAction::RemoteRecvData {
                        endpoint_id,
                        target_island,
                        fiber_key,
                        wait_id,
                        data,
                        mut island_effects,
                        rollback,
                    } => {
                        let mut transition = RuntimeTransition::new(
                            RuntimeBoundary::Yield,
                            ResumePolicy::PreserveFramePc,
                            GcRootEffect::CurrentFiberDirty,
                        );
                        transition.island_commands.append(&mut island_effects);
                        transition.island_commands.push(
                            IslandCommandEffect::endpoint_recv_data_response(
                                target_island,
                                self.state.current_island_id,
                                endpoint_id,
                                data,
                                fiber_key,
                                wait_id,
                            ),
                        );
                        if let Some(rollback) = rollback {
                            transition.set_rollback(rollback);
                        }
                        return ExecResult::Transition(transition);
                    }
                    exec::QueueAction::RemoteClose {
                        endpoint_id,
                        home_island,
                        rollback,
                    } => {
                        let mut transition = RuntimeTransition::new(
                            RuntimeBoundary::Yield,
                            ResumePolicy::PreserveFramePc,
                            GcRootEffect::AllRootsDirty,
                        );
                        transition.set_rollback(rollback);
                        transition.island_commands.push(
                            IslandCommandEffect::endpoint_close_request(
                                home_island,
                                endpoint_id,
                                self.state.current_island_id,
                            ),
                        );
                        transition.endpoint_tombstones.push(
                            crate::runtime_boundary::EndpointTombstone::with_response_source(
                                endpoint_id,
                                home_island,
                            ),
                        );
                        return ExecResult::Transition(transition);
                    }
                }
            };
        }

        while fiber.execution_budget > 0 {
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
                    && can_enter_materialized_frame_at_pc(
                        func,
                        frame.pc,
                        &self.state.resolved_externs,
                    )
                {
                    let jit_func = if let Some(jit_mgr) = self.jit.manager_mut() {
                        let env = vo_jit::JitCompileEnv {
                            externs: &self.state.resolved_externs,
                            backend_caps: Default::default(),
                        };
                        match jit_mgr.resolve_call(func_id, func, module, env) {
                            Ok(entry) => entry,
                            Err(err) => {
                                return ExecResult::JitError(format!(
                                    "JIT frame-entry compilation failed for {}: {err}",
                                    func.name
                                ));
                            }
                        }
                    } else {
                        None
                    };
                    if let Some(jit_func) = jit_func {
                        let result = jit::dispatch_jit_frame(self, fiber, module, jit_func);
                        stack = fiber.stack_ptr();
                        match result {
                            ExecResult::FrameChanged => {
                                if !self.state.pending_runtime_transitions.is_empty() {
                                    return ExecResult::FrameChanged;
                                }
                                refetch_after_frame_change!();
                                continue;
                            }
                            other => return other,
                        }
                    }
                }
            }

            fiber.execution_budget -= 1;

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
                    let Some(elem_slots) = slot_elem_slots_for_pc(func, pc) else {
                        return ExecResult::JitError(format!(
                            "SlotGetN at pc {pc} is missing SlotLayout metadata"
                        ));
                    };
                    exec::exec_slot_get_n(stack, bp, &inst, elem_slots);
                }
                Opcode::SlotSetN => {
                    let Some(elem_slots) = slot_elem_slots_for_pc(func, pc) else {
                        return ExecResult::JitError(format!(
                            "SlotSetN at pc {pc} is missing SlotLayout metadata"
                        ));
                    };
                    exec::exec_slot_set_n(stack, bp, &inst, elem_slots);
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
                    let b = stack_get(stack, bp + inst.c as usize);
                    if inst.flags & crate::instruction::SHIFT_FLAG_RHS_UNSIGNED == 0
                        && (b as i64) < 0
                    {
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
                    let b = stack_get(stack, bp + inst.c as usize);
                    if inst.flags & crate::instruction::SHIFT_FLAG_RHS_UNSIGNED == 0
                        && (b as i64) < 0
                    {
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
                    let b = stack_get(stack, bp + inst.c as usize);
                    if inst.flags & crate::instruction::SHIFT_FLAG_RHS_UNSIGNED == 0
                        && (b as i64) < 0
                    {
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
                    handle_panic_result!(exec::exec_call(&mut self.state.gc, fiber, &inst, module));
                }
                Opcode::CallExtern => {
                    use vo_runtime::ffi::{ExternFiberInputs, ExternInvoke, ExternWorld};
                    // CallExtern: a=dst, b=extern_id, c=args_start; metadata owns layouts.
                    let extern_id = inst.b as u32;
                    let fetched_pc = unsafe { (*frame_ptr).pc }.checked_sub(1).ok_or_else(|| {
                        ExecResult::JitError("CallExtern cannot derive fetched pc 0".to_string())
                    });
                    let fetched_pc = match fetched_pc {
                        Ok(pc) => pc as u32,
                        Err(result) => return result,
                    };
                    let vm_ptr = self as *mut Vm as *mut core::ffi::c_void;
                    let fiber_ptr = fiber as *mut crate::fiber::Fiber as *mut core::ffi::c_void;

                    let Some(_extern_def) = module.externs.get(extern_id as usize) else {
                        return ExecResult::JitError(format!(
                            "CallExtern missing extern id {extern_id}"
                        ));
                    };
                    let Some(resolved_extern) = self.state.resolved_externs.get(extern_id).cloned()
                    else {
                        return ExecResult::JitError(format!(
                            "CallExtern id {extern_id} missing resolved extern entry"
                        ));
                    };
                    let (arg_slots, callsite_ret_slots) = match func
                        .jit_metadata
                        .get(fetched_pc as usize)
                    {
                        Some(JitInstructionMetadata::CallExternLayout {
                            arg_layout,
                            ret_layout,
                        }) => {
                            let Ok(arg_slots) = u16::try_from(arg_layout.len()) else {
                                return ExecResult::JitError(format!(
                                    "CallExtern argument layout has {} slots, exceeding u16::MAX",
                                    arg_layout.len()
                                ));
                            };
                            (arg_slots, ret_layout.len())
                        }
                        other => {
                            return ExecResult::JitError(format!(
                                "CallExtern missing authoritative metadata at pc {fetched_pc}: {other:?}"
                            ));
                        }
                    };
                    let ret_slots = resolved_extern.returns.slots;
                    if callsite_ret_slots != usize::from(ret_slots) {
                        return ExecResult::JitError(format!(
                            "CallExtern return layout has {callsite_ret_slots} slots but extern {} returns {ret_slots}",
                            resolved_extern.name
                        ));
                    }
                    if !resolved_extern.params.accepts_slots(arg_slots) {
                        return ExecResult::JitError(format!(
                            "CallExtern arg slot count {arg_slots} does not match extern {} params {}",
                            resolved_extern.name,
                            resolved_extern.params.display_name()
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
                        arg_slots,
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
                        host_services: self.state.host_services.as_deref(),
                        #[cfg(feature = "std")]
                        io: &mut self.state.io,
                    };
                    let (closure_replay_results, closure_replay_panic_message) =
                        fiber.closure_replay.snapshot_for_extern(fiber.frames.len());
                    #[cfg(feature = "std")]
                    let resume_io_token = fiber.resume_io_token.take();
                    let resume_host_event_token = fiber.resume_host_event_token.take();
                    let resume_host_event_data = fiber.resume_host_event_data.take();
                    let fiber_inputs = ExternFiberInputs {
                        fiber_opaque: fiber_ptr,
                        #[cfg(feature = "std")]
                        resume_io_token,
                        resume_host_event_token,
                        resume_host_event_data,
                        replay_results: closure_replay_results,
                        replay_panic_message: closure_replay_panic_message,
                    };
                    let extern_result = match self.state.extern_registry.call_resolved(
                        &mut fiber.stack,
                        invoke,
                        world,
                        fiber_inputs,
                        &resolved_extern,
                    ) {
                        Ok(result) => result,
                        Err(err) => {
                            fiber.closure_replay.finish_extern_terminal();
                            return ExecResult::JitError(err.to_string());
                        }
                    };
                    stack = fiber.stack_ptr();
                    #[cfg(debug_assertions)]
                    if !matches!(&extern_result, vo_runtime::ffi::ExternResult::Exit(_)) {
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
                            fiber.closure_replay.finish_extern_terminal();
                            return ExecResult::JitError(msg);
                        }
                    }
                    let transition =
                        extern_result_to_transition(&resolved_extern, extern_result, fetched_pc);
                    apply_extern_replay_scope_effect(fiber, transition.replay_scope);
                    match transition.boundary {
                        ExternBoundary::Continue => {
                            refetch!();
                        }
                        ExternBoundary::Exit(code) => {
                            return ExecResult::Exit(code);
                        }
                        ExternBoundary::Panic(msg) => {
                            let r =
                                runtime_panic_msg(&mut self.state.gc, fiber, stack, module, msg);
                            if matches!(r, ExecResult::FrameChanged) {
                                refetch_after_frame_change!();
                            } else {
                                return r;
                            }
                        }
                        ExternBoundary::FatalInfra(msg) => {
                            return ExecResult::JitError(msg);
                        }
                        ExternBoundary::Yield => {
                            return ExecResult::TimesliceExpired;
                        }
                        ExternBoundary::QueueBlock => {
                            return ExecResult::Block(crate::fiber::BlockReason::Queue);
                        }
                        ExternBoundary::HostEventWait { token, delay_ms } => {
                            return ExecResult::Block(crate::fiber::BlockReason::HostEvent {
                                token,
                                delay_ms,
                            });
                        }
                        ExternBoundary::HostEventWaitAndReplay { token, source } => {
                            return ExecResult::Transition(RuntimeTransition::new(
                                RuntimeBoundary::Block(
                                    crate::fiber::BlockReason::HostEventReplay { token, source },
                                ),
                                transition.resume,
                                GcRootEffect::CurrentFiberDirty,
                            ));
                        }
                        #[cfg(feature = "std")]
                        ExternBoundary::WaitIo(token) => {
                            return ExecResult::Transition(RuntimeTransition::new(
                                RuntimeBoundary::Block(crate::fiber::BlockReason::Io(token)),
                                transition.resume,
                                GcRootEffect::CurrentFiberDirty,
                            ));
                        }
                        ExternBoundary::CallClosure { closure_ref, args } => {
                            let result = prepare_extern_closure_replay_call(
                                &mut self.state.gc,
                                fiber,
                                module,
                                &self.state.itab_cache,
                                closure_ref,
                                args,
                                transition.resume,
                            );
                            stack = fiber.stack_ptr();
                            match result {
                                ExecResult::FrameChanged => refetch_after_frame_change!(),
                                other => {
                                    fiber.closure_replay.finish_extern_terminal();
                                    return other;
                                }
                            }
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
                        let Some(return_flags) = ReturnFlags::from_bits(inst.flags) else {
                            return ExecResult::JitError(format!(
                                "Return at pc {pc} has invalid flags 0x{:02x}",
                                inst.flags
                            ));
                        };
                        let is_error_return = return_flags.is_error_return();
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
                    refetch_after_frame_change!();
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
                    let idx_raw = stack_get(stack, bp + inst.c as usize);
                    let len = if s.is_null() { 0 } else { string_len(s) };
                    if idx_raw >= len as u64 {
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx_raw, len
                            )
                        ));
                    }
                    let idx = idx_raw as usize;
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
                    // Safety: verified bytecode supplies live string operands.
                    stack_set(stack, bp + inst.a as usize, unsafe { string::eq(a, b) }
                        as u64);
                }
                Opcode::StrNe => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    // Safety: verified bytecode supplies live string operands.
                    stack_set(stack, bp + inst.a as usize, unsafe { string::ne(a, b) }
                        as u64);
                }
                Opcode::StrLt => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    // Safety: verified bytecode supplies live string operands.
                    stack_set(stack, bp + inst.a as usize, unsafe { string::lt(a, b) }
                        as u64);
                }
                Opcode::StrLe => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    // Safety: verified bytecode supplies live string operands.
                    stack_set(stack, bp + inst.a as usize, unsafe { string::le(a, b) }
                        as u64);
                }
                Opcode::StrGt => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    // Safety: verified bytecode supplies live string operands.
                    stack_set(stack, bp + inst.a as usize, unsafe { string::gt(a, b) }
                        as u64);
                }
                Opcode::StrGe => {
                    let a = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let b = stack_get(stack, bp + inst.c as usize) as GcRef;
                    // Safety: verified bytecode supplies live string operands.
                    stack_set(stack, bp + inst.a as usize, unsafe { string::ge(a, b) }
                        as u64);
                }
                Opcode::StrDecodeRune => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let pos = stack_get(stack, bp + inst.c as usize) as usize;
                    // Safety: verified bytecode supplies a live string operand.
                    let (rune, width) = unsafe { string::decode_rune_at(s, pos) };
                    stack_set(stack, bp + inst.a as usize, rune as u64);
                    stack_set(stack, bp + inst.a as usize + 1, width as u64);
                }

                // Array operations
                Opcode::ArrayNew => {
                    if let Err(msg) = exec::exec_array_new(stack, bp, &inst, &mut self.state.gc) {
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::MakeSlice,
                            msg.to_string()
                        ));
                    }
                }
                Opcode::ArrayGet => {
                    let arr = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx_raw = stack_get(stack, bp + inst.c as usize);
                    // Safety: verifier guarantees ArrayGet's operand is a live array.
                    let len = unsafe { array::len(arr) };
                    if idx_raw >= len as u64 {
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx_raw, len
                            )
                        ));
                    }
                    let idx = idx_raw as usize;
                    let dst = bp + inst.a as usize;
                    let off = idx as isize;
                    let base = unsafe { array::data_ptr_bytes(arr) };
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
                    let idx_raw = stack_get(stack, bp + inst.b as usize);
                    // Safety: verifier guarantees ArraySet's operand is a live array.
                    let len = unsafe { array::len(arr) };
                    if idx_raw >= len as u64 {
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx_raw, len
                            )
                        ));
                    }
                    let idx = idx_raw as usize;
                    let src = bp + inst.c as usize;
                    let off = idx as isize;
                    let base = unsafe { array::data_ptr_bytes(arr) };
                    let val = stack_get(stack, src);
                    match inst.flags {
                        1 | 129 => unsafe { *base.offset(off) = val as u8 },
                        2 | 130 => unsafe { *(base.offset(off * 2) as *mut u16) = val as u16 },
                        4 | 132 => unsafe { *(base.offset(off * 4) as *mut u32) = val as u32 },
                        0x44 => unsafe { *(base.offset(off * 4) as *mut u32) = val as u32 },
                        8 => {
                            let em = unsafe { array::elem_meta(arr) };
                            if em.value_kind().may_contain_gc_refs() {
                                if let Err(err) =
                                    vo_runtime::gc_types::try_typed_write_barrier_by_meta(
                                        &mut self.state.gc,
                                        arr,
                                        &[val],
                                        em,
                                        Some(module),
                                    )
                                {
                                    return ExecResult::JitError(err.to_string());
                                }
                            }
                            unsafe { *(base.offset(off * 8) as *mut u64) = val };
                        }
                        0 => {
                            let elem_bytes = stack_get(stack, bp + inst.b as usize + 1) as usize;
                            let elem_slots = elem_bytes.div_ceil(8);
                            // Write barrier for multi-slot elements that may contain GcRefs
                            let em = unsafe { array::elem_meta(arr) };
                            if em.value_kind().may_contain_gc_refs() {
                                // Safety: verified ArraySet metadata keeps this source range
                                // inside the active frame for the duration of the barrier/copy.
                                let vals = unsafe {
                                    core::slice::from_raw_parts(stack.add(src), elem_slots)
                                };
                                if let Err(err) =
                                    vo_runtime::gc_types::try_typed_write_barrier_by_meta(
                                        &mut self.state.gc,
                                        arr,
                                        vals,
                                        em,
                                        Some(module),
                                    )
                                {
                                    return ExecResult::JitError(err.to_string());
                                }
                                for (i, val) in vals.iter().enumerate() {
                                    let ptr =
                                        unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                    unsafe { *ptr = *val };
                                }
                            } else {
                                for i in 0..elem_slots {
                                    let ptr =
                                        unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                    unsafe { *ptr = stack_get(stack, src + i) };
                                }
                            }
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            let elem_slots = elem_bytes.div_ceil(8);
                            // Write barrier for multi-slot elements that may contain GcRefs
                            let em = unsafe { array::elem_meta(arr) };
                            if elem_bytes >= 8 && em.value_kind().may_contain_gc_refs() {
                                // Safety: verified ArraySet metadata keeps this source range
                                // inside the active frame for the duration of the barrier/copy.
                                let vals = unsafe {
                                    core::slice::from_raw_parts(stack.add(src), elem_slots)
                                };
                                if let Err(err) =
                                    vo_runtime::gc_types::try_typed_write_barrier_by_meta(
                                        &mut self.state.gc,
                                        arr,
                                        vals,
                                        em,
                                        Some(module),
                                    )
                                {
                                    return ExecResult::JitError(err.to_string());
                                }
                                for (i, val) in vals.iter().enumerate() {
                                    let ptr =
                                        unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                    unsafe { *ptr = *val };
                                }
                            } else {
                                for i in 0..elem_slots {
                                    let ptr =
                                        unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                    unsafe { *ptr = stack_get(stack, src + i) };
                                }
                            }
                        }
                    }
                }
                Opcode::ArrayAddr => {
                    let arr = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx_raw = stack_get(stack, bp + inst.c as usize);
                    // Safety: verifier guarantees ArrayAddr's operand is a live array.
                    let len = unsafe { array::len(arr) };
                    if idx_raw >= len as u64 {
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx_raw, len
                            )
                        ));
                    }
                    let idx = idx_raw as usize;
                    let elem_bytes = match inst.flags {
                        0 => stack_get(stack, bp + inst.c as usize + 1) as usize,
                        0x81 => 1,
                        0x82 => 2,
                        0x84 | 0x44 => 4,
                        f => f as usize,
                    };
                    let base = unsafe { array::data_ptr_bytes(arr) };
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
                    let idx_raw = stack_get(stack, bp + inst.c as usize);
                    let len = if s.is_null() { 0 } else { slice_len(s) };
                    if idx_raw >= len as u64 {
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx_raw, len
                            )
                        ));
                    }
                    let idx = idx_raw as usize;
                    let base = slice_data_ptr(s);
                    let dst = bp + inst.a as usize;
                    if unsafe { vo_runtime::objects::slice::uses_flat_slot_storage(s) } {
                        let stride = unsafe { vo_runtime::objects::slice::storage_stride(s) };
                        let elem_slots = stride / vo_runtime::slot::SLOT_BYTES;
                        let dest =
                            unsafe { core::slice::from_raw_parts_mut(stack.add(dst), elem_slots) };
                        unsafe {
                            vo_runtime::objects::slice::read_logical_slots(s, idx, dest);
                        }
                        continue;
                    }
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
                    let idx_raw = stack_get(stack, bp + inst.b as usize);
                    let len = if s.is_null() { 0 } else { slice_len(s) };
                    if idx_raw >= len as u64 {
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx_raw, len
                            )
                        ));
                    }
                    let idx = idx_raw as usize;
                    let base = slice_data_ptr(s);
                    let src = bp + inst.c as usize;
                    let val = stack_get(stack, src);
                    if unsafe { vo_runtime::objects::slice::uses_flat_slot_storage(s) } {
                        let stride = unsafe { vo_runtime::objects::slice::storage_stride(s) };
                        let elem_slots = stride / vo_runtime::slot::SLOT_BYTES;
                        let vals =
                            unsafe { core::slice::from_raw_parts(stack.add(src), elem_slots) };
                        let elem_meta = unsafe { vo_runtime::objects::slice::elem_meta(s) };
                        let owner = unsafe { vo_runtime::objects::slice::owner_ref(s) };
                        if !owner.is_null() && elem_meta.value_kind().may_contain_gc_refs() {
                            if let Err(err) = vo_runtime::gc_types::try_typed_write_barrier_by_meta(
                                &mut self.state.gc,
                                owner,
                                vals,
                                elem_meta,
                                Some(module),
                            ) {
                                return ExecResult::JitError(err.to_string());
                            }
                        }
                        unsafe {
                            vo_runtime::objects::slice::write_logical_slots(s, idx, vals);
                        }
                        continue;
                    }
                    match inst.flags {
                        1 | 129 => unsafe { *base.add(idx) = val as u8 },
                        2 | 130 => unsafe { *(base.add(idx * 2) as *mut u16) = val as u16 },
                        4 | 132 => unsafe { *(base.add(idx * 4) as *mut u32) = val as u32 },
                        0x44 => unsafe { *(base.add(idx * 4) as *mut u32) = val as u32 },
                        8 => {
                            let owner = unsafe { vo_runtime::objects::slice::owner_ref(s) };
                            if !owner.is_null() {
                                let em = unsafe { vo_runtime::objects::slice::elem_meta(s) };
                                if em.value_kind().may_contain_gc_refs() {
                                    if let Err(err) =
                                        vo_runtime::gc_types::try_typed_write_barrier_by_meta(
                                            &mut self.state.gc,
                                            owner,
                                            &[val],
                                            em,
                                            Some(module),
                                        )
                                    {
                                        return ExecResult::JitError(err.to_string());
                                    }
                                }
                            }
                            unsafe { *(base.add(idx * 8) as *mut u64) = val };
                        }
                        0 => {
                            let elem_bytes = stack_get(stack, bp + inst.b as usize + 1) as usize;
                            let elem_slots = elem_bytes.div_ceil(8);
                            // Write barrier for multi-slot elements that may contain GcRefs
                            let owner = unsafe { vo_runtime::objects::slice::owner_ref(s) };
                            let needs_barrier = if owner.is_null() {
                                false
                            } else {
                                unsafe { vo_runtime::objects::slice::elem_meta(s) }
                                    .value_kind()
                                    .may_contain_gc_refs()
                            };
                            if needs_barrier {
                                // Safety: verified SliceSet metadata keeps this source range
                                // inside the active frame for the duration of the barrier/copy.
                                let vals = unsafe {
                                    core::slice::from_raw_parts(stack.add(src), elem_slots)
                                };
                                let em = unsafe { vo_runtime::objects::slice::elem_meta(s) };
                                if let Err(err) =
                                    vo_runtime::gc_types::try_typed_write_barrier_by_meta(
                                        &mut self.state.gc,
                                        owner,
                                        vals,
                                        em,
                                        Some(module),
                                    )
                                {
                                    return ExecResult::JitError(err.to_string());
                                }
                                for (i, val) in vals.iter().enumerate() {
                                    let ptr =
                                        unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                    unsafe { *ptr = *val };
                                }
                            } else {
                                for i in 0..elem_slots {
                                    let ptr =
                                        unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                    unsafe { *ptr = stack_get(stack, src + i) };
                                }
                            }
                        }
                        _ => {
                            let elem_bytes = inst.flags as usize;
                            let elem_slots = elem_bytes.div_ceil(8);
                            // Write barrier for multi-slot elements that may contain GcRefs
                            let owner = unsafe { vo_runtime::objects::slice::owner_ref(s) };
                            let needs_barrier = elem_bytes >= 8
                                && !owner.is_null()
                                && unsafe { vo_runtime::objects::slice::elem_meta(s) }
                                    .value_kind()
                                    .may_contain_gc_refs();
                            if needs_barrier {
                                // Safety: verified SliceSet metadata keeps this source range
                                // inside the active frame for the duration of the barrier/copy.
                                let vals = unsafe {
                                    core::slice::from_raw_parts(stack.add(src), elem_slots)
                                };
                                let em = unsafe { vo_runtime::objects::slice::elem_meta(s) };
                                if let Err(err) =
                                    vo_runtime::gc_types::try_typed_write_barrier_by_meta(
                                        &mut self.state.gc,
                                        owner,
                                        vals,
                                        em,
                                        Some(module),
                                    )
                                {
                                    return ExecResult::JitError(err.to_string());
                                }
                                for (i, val) in vals.iter().enumerate() {
                                    let ptr =
                                        unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                    unsafe { *ptr = *val };
                                }
                            } else {
                                for i in 0..elem_slots {
                                    let ptr =
                                        unsafe { base.add(idx * elem_bytes + i * 8) as *mut u64 };
                                    unsafe { *ptr = stack_get(stack, src + i) };
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
                    if let Err(msg) =
                        exec::exec_slice_append(stack, bp, &inst, &mut self.state.gc, Some(module))
                    {
                        return ExecResult::JitError(msg);
                    }
                }
                Opcode::SliceAddr => {
                    let s = stack_get(stack, bp + inst.b as usize) as GcRef;
                    let idx_raw = stack_get(stack, bp + inst.c as usize);
                    let len = if s.is_null() { 0 } else { slice_len(s) };
                    if idx_raw >= len as u64 {
                        handle_panic_result!(runtime_panic(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::IndexOutOfBounds,
                            format!(
                                "runtime error: index out of range [{}] with length {}",
                                idx_raw, len
                            )
                        ));
                    }
                    let idx = idx_raw as usize;
                    let elem_bytes =
                        if unsafe { vo_runtime::objects::slice::uses_flat_slot_storage(s) } {
                            unsafe { vo_runtime::objects::slice::storage_stride(s) }
                        } else {
                            match inst.flags {
                                0 => stack_get(stack, bp + inst.c as usize + 1) as usize,
                                0x81 => 1,
                                0x82 => 2,
                                0x84 | 0x44 => 4,
                                f => f as usize,
                            }
                        };
                    let base = slice_data_ptr(s);
                    let addr = unsafe { base.add(idx * elem_bytes) } as u64;
                    stack_set(stack, bp + inst.a as usize, addr);
                }

                // Map operations
                Opcode::MapNew => {
                    let Some((key_layout, val_layout)) = map_new_layout_for_pc(func, pc) else {
                        return ExecResult::JitError(format!(
                            "MapNew at pc {pc} is missing MapNew metadata"
                        ));
                    };
                    if let Err(msg) = exec::exec_map_new(
                        stack,
                        bp,
                        &inst,
                        &mut self.state.gc,
                        key_layout,
                        val_layout,
                    ) {
                        return ExecResult::JitError(msg);
                    }
                }
                Opcode::MapGet => match exec::exec_map_get_with_layout_using_scratch(
                    stack,
                    bp,
                    &inst,
                    &self.state.gc,
                    Some(module),
                    map_get_layout_for_pc(func, pc),
                    &mut fiber.map_scratch,
                ) {
                    Ok(true) => {}
                    Ok(false) => {
                        handle_panic_result!(runtime_trap(
                            &mut self.state.gc,
                            fiber,
                            stack,
                            module,
                            RuntimeTrapKind::UnhashableType
                        ));
                    }
                    Err(msg) => return ExecResult::JitError(msg),
                },
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
                    match exec::exec_map_set_with_layout_using_scratch(
                        stack,
                        bp,
                        &inst,
                        &mut self.state.gc,
                        Some(module),
                        map_key_value_layout_for_pc(func, pc),
                        &mut fiber.map_scratch,
                    ) {
                        Ok(true) => {}
                        Ok(false) => {
                            handle_panic_result!(runtime_trap(
                                &mut self.state.gc,
                                fiber,
                                stack,
                                module,
                                RuntimeTrapKind::UnhashableType
                            ));
                        }
                        Err(msg) => return ExecResult::JitError(msg),
                    }
                }
                Opcode::MapDelete => {
                    let m = stack_get(stack, bp + inst.a as usize) as GcRef;
                    if !m.is_null() {
                        match exec::exec_map_delete_with_layout_using_scratch(
                            stack,
                            bp,
                            &inst,
                            &self.state.gc,
                            Some(module),
                            map_key_layout_for_pc(func, pc),
                            &mut fiber.map_scratch,
                        ) {
                            Ok(true) => {}
                            Ok(false) => {
                                handle_panic_result!(runtime_trap(
                                    &mut self.state.gc,
                                    fiber,
                                    stack,
                                    module,
                                    RuntimeTrapKind::UnhashableType
                                ));
                            }
                            Err(msg) => return ExecResult::JitError(msg),
                        }
                    }
                }
                Opcode::MapLen => {
                    if let Err(msg) = exec::exec_map_len(stack, bp, &inst, &self.state.gc) {
                        return ExecResult::JitError(msg);
                    }
                }
                Opcode::MapIterInit => {
                    if let Err(msg) = exec::exec_map_iter_init(stack, bp, &inst, &self.state.gc) {
                        return ExecResult::JitError(msg);
                    }
                }
                Opcode::MapIterNext => {
                    if let Err(msg) = exec::exec_map_iter_next_with_layout(
                        stack,
                        bp,
                        &inst,
                        Some(&self.state.gc),
                        Some(module),
                        map_key_value_layout_for_pc(func, pc),
                    ) {
                        return ExecResult::JitError(msg);
                    }
                }

                // Channel operations
                Opcode::QueueNew => {
                    let Some(elem_layout) = queue_layout_for_pc(func, pc) else {
                        return ExecResult::JitError(format!(
                            "QueueNew missing QueueLayout metadata at pc {pc}"
                        ));
                    };
                    if let Err(msg) = exec::exec_queue_new(
                        stack,
                        bp,
                        &inst,
                        &mut self.state.gc,
                        module,
                        elem_layout,
                    ) {
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
                    let Some(elem_layout) = queue_layout_for_pc(func, pc) else {
                        return ExecResult::JitError(format!(
                            "QueueSend missing QueueLayout metadata at pc {pc}"
                        ));
                    };
                    let elem_slots = elem_layout.len();
                    let src_start = bp + inst.b as usize;
                    // Safety: QueueSend verification guarantees the payload range lies in
                    // the active frame; queue_send_core snapshots it before suspension.
                    let src =
                        unsafe { core::slice::from_raw_parts(stack.add(src_start), elem_slots) };
                    handle_queue_action!(exec::queue_send_core_with_layout(
                        ch,
                        src,
                        Some(elem_layout),
                        self.state.current_island_id,
                        fiber.wake_key_packed(),
                        &mut self.state,
                        &module.struct_metas,
                        &module.runtime_types,
                        Some(module),
                    ));
                }
                Opcode::QueueRecv => {
                    if fiber.remote_recv_response.is_some() {
                        let raw_ch = helpers::stack_get(stack, bp + inst.b as usize) as GcRef;
                        let ch = match exec::validate_queue_handle(
                            &self.state.gc,
                            raw_ch,
                            "QueueRecv replay",
                        ) {
                            Ok(ch) => ch,
                            Err(err) => return ExecResult::JitError(err),
                        };
                        let elem_meta = unsafe { vo_runtime::objects::queue_state::elem_meta(ch) };
                        let elem_rttid =
                            unsafe { vo_runtime::objects::queue_state::elem_rttid(ch) };
                        let Some(elem_layout) = queue_layout_for_pc(func, pc) else {
                            return ExecResult::JitError(format!(
                                "QueueRecv missing QueueLayout metadata at pc {pc}"
                            ));
                        };
                        let elem_slots = elem_layout.len();
                        let queue_elem_slots =
                            unsafe { vo_runtime::objects::queue_state::elem_slots(ch) } as usize;
                        if elem_slots != queue_elem_slots {
                            return ExecResult::JitError(format!(
                                "QueueRecv replay element slot count {elem_slots} does not match queue metadata {queue_elem_slots}"
                            ));
                        }
                        if let Err(msg) = exec::validate_queue_payload_layout(
                            ch,
                            elem_layout,
                            "QueueRecv replay",
                            Some(module),
                        ) {
                            return ExecResult::JitError(msg);
                        }
                        let has_ok = inst.recv_has_ok();
                        let dst_start = bp + inst.a as usize;
                        let Some(recv_response) = fiber.remote_recv_response.clone() else {
                            return ExecResult::JitError(
                                "QueueRecv replay lost its pending remote response".to_string(),
                            );
                        };
                        if let Err(err) = exec::replay_remote_queue_recv_response(
                            &mut self.state.gc,
                            recv_response,
                            elem_meta,
                            elem_rttid,
                            elem_slots,
                            has_ok,
                            &module.struct_metas,
                            &module.named_type_metas,
                            &module.runtime_types,
                            &mut self.state.endpoint_registry,
                            |i, value| helpers::stack_set(stack, dst_start + i, value),
                        ) {
                            return ExecResult::JitError(err.to_string());
                        }
                        fiber.remote_recv_response = None;
                        self.mark_gc_all_roots_dirty();
                        refetch!();
                        continue;
                    }
                    handle_queue_action!(exec::exec_queue_recv(
                        stack,
                        bp,
                        self.state.current_island_id,
                        fiber.wake_key_packed(),
                        &inst,
                        &self.state,
                        Some(module),
                        queue_layout_for_pc(func, pc),
                    ));
                }
                Opcode::QueueClose => {
                    let ch = helpers::stack_get(stack, bp + inst.a as usize) as GcRef;
                    if let Err(msg) = exec::preflight_queue_close_routes(&self.state, ch) {
                        return ExecResult::JitError(msg);
                    }
                    handle_queue_action!(exec::exec_queue_close(stack, bp, &inst, &self.state));
                }
                Opcode::QueueLen => {
                    handle_queue_action!(exec::exec_queue_get(
                        stack,
                        bp,
                        &inst,
                        &self.state.gc,
                        |ch| unsafe { exec::queue_len(ch) }
                    ));
                }
                Opcode::QueueCap => {
                    handle_queue_action!(exec::exec_queue_get(
                        stack,
                        bp,
                        &inst,
                        &self.state.gc,
                        |ch| unsafe { vo_runtime::objects::queue_state::capacity(ch) },
                    ));
                }

                // Select operations
                Opcode::SelectBegin => {
                    if let Err(err) = exec::exec_select_begin(fiber, inst.a, (inst.flags & 1) != 0)
                    {
                        return ExecResult::JitError(err.to_string());
                    }
                }
                Opcode::SelectSend => {
                    let Some(elem_layout) = queue_layout_for_pc(func, pc) else {
                        return ExecResult::JitError(format!(
                            "SelectSend missing QueueLayout metadata at pc {pc}"
                        ));
                    };
                    let elem_slots = match u16::try_from(elem_layout.len()) {
                        Ok(slots) => slots,
                        Err(_) => {
                            return ExecResult::JitError(format!(
                                "SelectSend QueueLayout width exceeds u16::MAX at pc {pc}"
                            ))
                        }
                    };
                    if let Err(msg) = exec::exec_select_send_with_layout(
                        &mut fiber.select_state,
                        inst.a,
                        inst.b,
                        elem_slots,
                        Some(elem_layout.to_vec()),
                        inst.c,
                    ) {
                        return ExecResult::JitError(msg);
                    }
                }
                Opcode::SelectRecv => {
                    let Some(elem_layout) = queue_layout_for_pc(func, pc) else {
                        return ExecResult::JitError(format!(
                            "SelectRecv missing QueueLayout metadata at pc {pc}"
                        ));
                    };
                    let elem_slots = match u16::try_from(elem_layout.len()) {
                        Ok(slots) => slots,
                        Err(_) => {
                            return ExecResult::JitError(format!(
                                "SelectRecv QueueLayout width exceeds u16::MAX at pc {pc}"
                            ))
                        }
                    };
                    if let Err(msg) = exec::exec_select_recv_with_layout(
                        &mut fiber.select_state,
                        inst.a,
                        inst.b,
                        elem_slots,
                        Some(elem_layout.to_vec()),
                        inst.recv_has_ok(),
                        inst.c,
                    ) {
                        return ExecResult::JitError(msg);
                    }
                }
                Opcode::SelectExec => {
                    match exec::exec_select_exec(
                        exec::SelectExecContext {
                            stack,
                            bp,
                            island_id: self.state.current_island_id,
                            fiber_key: fiber.wake_key_packed(),
                            vm_state: &mut self.state,
                            module: Some(module),
                        },
                        &mut fiber.select_state,
                        inst.a,
                    ) {
                        exec::SelectResult::Continue => {}
                        exec::SelectResult::Block => {
                            // Waiters have been registered on all channels by exec_select_exec.
                            // Block this fiber - it will be woken when any channel is ready.
                            let resume = match replay_current_instruction_policy(
                                fiber,
                                "SelectExec block",
                            ) {
                                Ok(resume) => resume,
                                Err(msg) => return ExecResult::JitError(msg),
                            };
                            fiber.clear_queue_wait();
                            return ExecResult::Transition(RuntimeTransition::new(
                                RuntimeBoundary::Block(crate::fiber::BlockReason::Queue),
                                resume,
                                GcRootEffect::CurrentFiberDirty,
                            ));
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
                        exec::SelectResult::Wake { waiter, payload } => {
                            let mut transition = RuntimeTransition::new(
                                RuntimeBoundary::Yield,
                                ResumePolicy::PreserveFramePc,
                                GcRootEffect::AllRootsDirty,
                            );
                            transition.wakes.push(match payload {
                                Some(payload) => {
                                    WakeCommand::queue_waiter_with_result(waiter, payload)
                                }
                                None => WakeCommand::queue_waiter(waiter),
                            });
                            return ExecResult::Transition(transition);
                        }
                        exec::SelectResult::RemoteSendAck {
                            endpoint_id,
                            target_island,
                            fiber_key,
                            wait_id,
                            closed,
                            rollback,
                        } => {
                            let mut transition = RuntimeTransition::new(
                                RuntimeBoundary::Yield,
                                ResumePolicy::PreserveFramePc,
                                GcRootEffect::CurrentFiberDirty,
                            );
                            transition.island_commands.push(
                                IslandCommandEffect::endpoint_response(
                                    target_island,
                                    self.state.current_island_id,
                                    endpoint_id,
                                    vo_runtime::island::EndpointResponseKind::SendAck { closed },
                                    fiber_key,
                                    wait_id,
                                ),
                            );
                            if let Some(rollback) = rollback {
                                transition.set_rollback(rollback);
                            }
                            return ExecResult::Transition(transition);
                        }
                        exec::SelectResult::RemoteRecvData {
                            endpoint_id,
                            target_island,
                            fiber_key,
                            wait_id,
                            data,
                            mut island_effects,
                            rollback,
                        } => {
                            let mut transition = RuntimeTransition::new(
                                RuntimeBoundary::Yield,
                                ResumePolicy::PreserveFramePc,
                                GcRootEffect::CurrentFiberDirty,
                            );
                            transition.island_commands.append(&mut island_effects);
                            transition.island_commands.push(
                                IslandCommandEffect::endpoint_recv_data_response(
                                    target_island,
                                    self.state.current_island_id,
                                    endpoint_id,
                                    data,
                                    fiber_key,
                                    wait_id,
                                ),
                            );
                            if let Some(rollback) = rollback {
                                transition.set_rollback(rollback);
                            }
                            return ExecResult::Transition(transition);
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
                    if let Err(err) = exec::exec_closure_get(&self.state.gc, stack, bp, &inst) {
                        return ExecResult::JitError(err.to_string());
                    }
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
                    let (callsite_arg_layout, callsite_ret_layout) =
                        match crate::frame_call::call_layout_for_callsite(func, pc, "GoStart") {
                            Ok(layout) => layout,
                            Err(err) => return ExecResult::JitError(err),
                        };
                    let next_id = match self.scheduler.next_spawn_identity_hint() {
                        Ok(next_id) => next_id,
                        Err(error) => return ExecResult::JitError(error.to_string()),
                    };
                    match exec::exec_go_start(
                        &self.state.gc,
                        stack,
                        bp,
                        &inst,
                        module,
                        next_id,
                        callsite_arg_layout,
                        callsite_ret_layout,
                    ) {
                        Ok(go_result) => {
                            let mut transition = RuntimeTransition::new(
                                RuntimeBoundary::Yield,
                                ResumePolicy::PreserveFramePc,
                                GcRootEffect::AllRootsDirty,
                            );
                            transition.spawns.push(go_result.new_fiber);
                            return ExecResult::Transition(transition);
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
                        module,
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
                        module,
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
                        refetch_after_frame_change!();
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
                    let Some(JitInstructionMetadata::IfaceAssertLayout {
                        assert_kind,
                        target_id,
                        result_layout,
                    }) = func.jit_metadata.get(pc)
                    else {
                        return ExecResult::JitError(
                            "missing IfaceAssertLayout metadata".to_string(),
                        );
                    };
                    let result = exec::exec_iface_assert(
                        stack,
                        bp,
                        &inst,
                        *assert_kind,
                        *target_id,
                        result_layout,
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
                    let a = stack_get(stack, bp + inst.b as usize);
                    let result = conv_int_bits_to_float_bits(a, inst.flags);
                    stack_set(stack, bp + inst.a as usize, result);
                }
                Opcode::ConvF2I => {
                    let a = f64::from_bits(stack_get(stack, bp + inst.b as usize));
                    let result = conv_f64_to_int_bits(a, inst.flags);
                    stack_set(stack, bp + inst.a as usize, result);
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
                    let idx = stack_get(stack, bp + inst.a as usize);
                    let len = stack_get(stack, bp + inst.b as usize);
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
                    let handle = match self.create_island_for_execution(module) {
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
                    let island_id = match self.state.allocate_island_id() {
                        Ok(island_id) => island_id,
                        Err(error) => return ExecResult::JitError(error.to_string()),
                    };
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
                    let island_handle = match crate::frame_call::validate_island_handle(
                        &self.state.gc,
                        island_ref as u64,
                        "GoIsland",
                    ) {
                        Ok(island_handle) => island_handle,
                        Err(err) => return ExecResult::JitError(err),
                    };
                    let closure_target = match crate::frame_call::validate_closure_target(
                        &self.state.gc,
                        module,
                        closure_ref as u64,
                        "GoIsland",
                    ) {
                        Ok(target) => target,
                        Err(err) => return ExecResult::JitError(err),
                    };
                    let (callsite_arg_layout, callsite_ret_layout) =
                        match crate::frame_call::call_layout_for_callsite(func, pc, "GoIsland") {
                            Ok(layout) => layout,
                            Err(err) => return ExecResult::JitError(err),
                        };
                    if !callsite_ret_layout.is_empty() {
                        return ExecResult::JitError(format!(
                            "GoIsland callsite return layout must be empty, got {callsite_ret_layout:?}"
                        ));
                    }
                    if let Err(err) = crate::frame_call::validate_closure_arg_shape(
                        "GoIsland",
                        &closure_target,
                        callsite_arg_layout.len(),
                    ) {
                        return ExecResult::JitError(err);
                    }
                    if let Err(err) = crate::frame_call::validate_closure_callsite_arg_layout(
                        "GoIsland",
                        &closure_target,
                        callsite_arg_layout,
                    ) {
                        return ExecResult::JitError(err);
                    }
                    let result = exec::exec_go_island(
                        stack,
                        bp,
                        &inst,
                        callsite_arg_layout.len(),
                        island_handle,
                        &closure_target,
                    );
                    // Safety: `island_handle` was validated by the call target above.
                    let island_id = unsafe { vo_runtime::island::id(island_handle) };

                    if island_id == self.state.current_island_id {
                        let new_fiber = match unsafe {
                            helpers::try_build_validated_closure_fiber_from_args_ptr(
                                match self.scheduler.next_spawn_identity_hint() {
                                    Ok(next_id) => next_id,
                                    Err(error) => {
                                        return ExecResult::JitError(error.to_string());
                                    }
                                },
                                &closure_target,
                                stack.add(bp + inst.c as usize),
                                u32::try_from(callsite_arg_layout.len()).unwrap_or(u32::MAX),
                            )
                        } {
                            Ok(new_fiber) => new_fiber,
                            Err(helpers::ClosureFiberBuildError::Trap(
                                RuntimeTrapKind::StackOverflow,
                            )) => {
                                handle_panic_result!(runtime_trap(
                                    &mut self.state.gc,
                                    fiber,
                                    stack,
                                    module,
                                    RuntimeTrapKind::StackOverflow
                                ));
                            }
                            Err(helpers::ClosureFiberBuildError::Trap(_)) => {
                                handle_panic_result!(runtime_trap(
                                    &mut self.state.gc,
                                    fiber,
                                    stack,
                                    module,
                                    RuntimeTrapKind::NilFuncCall
                                ));
                            }
                            Err(helpers::ClosureFiberBuildError::Malformed(msg)) => {
                                return ExecResult::JitError(msg);
                            }
                        };
                        let mut transition = RuntimeTransition::new(
                            RuntimeBoundary::Yield,
                            ResumePolicy::PreserveFramePc,
                            GcRootEffect::AllRootsDirty,
                        );
                        transition.spawns.push(new_fiber);
                        return ExecResult::Transition(transition);
                    } else {
                        let func_def = closure_target.func;
                        let (result, capture_types) = if result.receiver_capture_slots == 0 {
                            (result, func_def.capture_types.clone())
                        } else {
                            match exec::direct_method_receiver_transfer_plan(
                                module,
                                result.func_id,
                                func_def,
                                result.receiver_capture_slots,
                            ) {
                                Ok(plan) => (
                                    exec::apply_direct_method_receiver_transfer_plan(result, plan),
                                    vec![plan.transfer_type],
                                ),
                                Err(msg) => return ExecResult::JitError(msg),
                            }
                        };
                        let param_types = match exec::go_island_sender_param_transfer_types(
                            module,
                            result.func_id,
                            func_def,
                            result.arg_data.len(),
                        ) {
                            Ok(param_types) => param_types,
                            Err(msg) => return ExecResult::JitError(msg),
                        };
                        let mut island_effects = Vec::new();
                        let transfer_commit = match exec::prepare_queue_handles_for_transfer(
                            &result,
                            island_id,
                            &capture_types,
                            &param_types,
                            &module.struct_metas,
                            &module.named_type_metas,
                            &module.runtime_types,
                            &mut self.state,
                            &mut island_effects,
                        ) {
                            Ok(commit) => commit,
                            Err(msg) => {
                                return ExecResult::JitError(format!(
                                    "GoIsland queue-transfer metadata contract error: {msg}"
                                ));
                            }
                        };
                        let data = exec::pack_closure_for_island(
                            &self.state.gc,
                            &result,
                            &capture_types,
                            &param_types,
                            &module.struct_metas,
                            &module.named_type_metas,
                            &module.runtime_types,
                        )
                        .map_err(|msg| {
                            format!("GoIsland closure pack metadata contract error: {msg}")
                        });
                        let data = match data {
                            Ok(data) => data,
                            Err(msg) => {
                                transfer_commit
                                    .restore_committed_local_endpoint_state(&mut self.state);
                                return ExecResult::JitError(msg);
                            }
                        };
                        let closure_data = vo_runtime::pack::PackedValue::from_data(data);
                        let mut transition = RuntimeTransition::new(
                            RuntimeBoundary::Continue,
                            ResumePolicy::PreserveFramePc,
                            GcRootEffect::None,
                        );
                        transition.island_commands.append(&mut island_effects);
                        transition
                            .island_commands
                            .push(IslandCommandEffect::spawn_fiber(island_id, closure_data));
                        if let Some(rollback) = transfer_commit.into_runtime_rollback() {
                            transition.set_rollback(rollback);
                        }
                        return ExecResult::Transition(transition);
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
        crate::frame_call::validate_function_arg_shape("spawn_call", func_id, func_def, args.len())
            .map_err(VmError::Jit)?;
        let local_slots = func_def.local_slots as usize;
        let gc_scan_slots = func_def.gc_scan_slots as usize;
        let mut args = args.to_vec();
        validate_spawn_call_args(
            &self.state.gc,
            module,
            &self.state.itab_cache,
            func_id,
            func_def,
            &mut args,
        )?;
        let ret_slots = func_def.ret_slots;
        let gc_scan_slots_u16 = func_def.gc_scan_slots;

        let fiber_id = self
            .scheduler
            .try_reuse_or_spawn()
            .map_err(|err| VmError::Jit(err.to_string()))?;
        self.mark_gc_all_roots_dirty();
        let fiber = self.scheduler.get_fiber_mut(fiber_id);

        let bp = fiber.sp;
        fiber
            .try_reserve_slots_at(bp, local_slots)
            .map_err(fiber_capacity_error_to_vm_error)?;

        fiber.zero_slots_tail_at(bp, gc_scan_slots, args.len());
        fiber.copy_slots_from_slice(bp, &args);

        fiber
            .try_push_call_frame(func_id, bp, 0, ret_slots, gc_scan_slots_u16)
            .map_err(fiber_capacity_error_to_vm_error)?;
        Ok(())
    }

    /// Spawn a new fiber that calls a closure with user arguments.
    pub fn spawn_closure_call(&mut self, closure_ref: GcRef, args: &[u64]) -> Result<(), VmError> {
        let module = self.module.as_ref().ok_or(VmError::NoEntryFunction)?;
        let (func_id, full_args) = {
            let target = crate::frame_call::validate_closure_target(
                &self.state.gc,
                module,
                closure_ref as u64,
                "spawn_closure_call",
            )
            .map_err(VmError::Jit)?;
            let expected_arg_slots = target
                .user_arg_slots("spawn_closure_call")
                .map_err(VmError::Jit)?;
            if args.len() != expected_arg_slots {
                return Err(VmError::Jit(format!(
                    "spawn_closure_call arg slot count {} does not match expected {} for func_id={} name={}",
                    args.len(),
                    expected_arg_slots,
                    target.func_id,
                    target.func.name
                )));
            }

            let mut full_args =
                Vec::with_capacity(target.layout.arg_offset.saturating_add(args.len()));
            for idx in 0..target.layout.receiver_capture_count {
                full_args.push(target.capture(idx));
            }
            full_args.extend(target.layout.slot0);
            full_args.extend_from_slice(args);
            (target.func_id, full_args)
        };

        self.spawn_call(func_id, &full_args)
    }
}

fn validate_spawn_call_args(
    gc: &Gc,
    module: &Module,
    itab_cache: &ItabCache,
    func_id: u32,
    func_def: &FunctionDef,
    args: &mut [u64],
) -> Result<(), VmError> {
    let param_slots = func_def.param_slots as usize;
    let Some(arg_slot_types) = func_def.slot_types.get(..param_slots) else {
        return Err(VmError::Jit(format!(
            "spawn_call function {} missing arg slot metadata: param_slots {} slot_types {}",
            func_id,
            func_def.param_slots,
            func_def.slot_types.len()
        )));
    };

    crate::frame_call::validate_gc_visible_payload_values(
        gc,
        args,
        arg_slot_types,
        "spawn_call",
        func_id,
        &func_def.name,
    )
    .map_err(VmError::Jit)?;

    let Some(plan) = spawn_call_transfer_plan(module, func_id, func_def, param_slots)? else {
        return Ok(());
    };
    validate_spawn_call_transfer_args(
        gc,
        module,
        itab_cache,
        func_id,
        func_def,
        args,
        arg_slot_types,
        &plan,
    )
}

struct SpawnCallTransferPlan {
    value_slot_offset: usize,
    required_end_slot: usize,
    transfers: Vec<TransferType>,
}

fn spawn_call_transfer_plan(
    module: &Module,
    func_id: u32,
    func_def: &FunctionDef,
    arg_slots: usize,
) -> Result<Option<SpawnCallTransferPlan>, VmError> {
    let declared_slots = transfer_type_slot_count(&func_def.param_types).map_err(VmError::Jit)?;
    if declared_slots == arg_slots {
        return Ok(Some(SpawnCallTransferPlan {
            value_slot_offset: 0,
            required_end_slot: arg_slots,
            transfers: func_def.param_types.clone(),
        }));
    }

    let closure_slot_offset = usize::from(func_def.is_closure);
    if closure_slot_offset != 0 && declared_slots + closure_slot_offset == arg_slots {
        return Ok(Some(SpawnCallTransferPlan {
            value_slot_offset: closure_slot_offset,
            required_end_slot: arg_slots,
            transfers: func_def.param_types.clone(),
        }));
    }

    let recv_slots = func_def.recv_slots as usize;
    if recv_slots != 0 && declared_slots + recv_slots == arg_slots {
        let plan = exec::direct_method_receiver_transfer_plan(
            module,
            func_id,
            func_def,
            func_def.recv_slots,
        )
        .map_err(VmError::Jit)?;
        if plan.raw_capture_slots != func_def.recv_slots {
            return Err(VmError::Jit(format!(
                "spawn_call method receiver for func_id={} name={} requires receiver-inclusive param_types",
                func_id, func_def.name
            )));
        }
        let mut transfers = Vec::with_capacity(func_def.param_types.len() + 1);
        transfers.push(plan.transfer_type);
        transfers.extend_from_slice(&func_def.param_types);
        return Ok(Some(SpawnCallTransferPlan {
            value_slot_offset: 0,
            required_end_slot: arg_slots,
            transfers,
        }));
    }

    if func_def.param_types.is_empty() {
        let metadata_start = if recv_slots == 0 {
            closure_slot_offset
        } else {
            recv_slots
        };
        let metadata_slots = func_def
            .slot_types
            .get(metadata_start..arg_slots)
            .ok_or_else(|| {
                VmError::Jit(format!(
                    "spawn_call metadata prefix {} exceeds args {} for function {} ({})",
                    metadata_start, arg_slots, func_id, func_def.name
                ))
            })?;
        if spawn_call_slot_types_require_transfer_metadata(metadata_slots) {
            return Err(VmError::Jit(format!(
                "spawn_call missing param_types for GC-visible args func={} name={}",
                func_id, func_def.name
            )));
        }
        if recv_slots == 0 {
            if closure_slot_offset != 0 {
                return Ok(Some(SpawnCallTransferPlan {
                    value_slot_offset: closure_slot_offset,
                    required_end_slot: closure_slot_offset,
                    transfers: Vec::new(),
                }));
            }
            return Ok(None);
        }
        let plan = exec::direct_method_receiver_transfer_plan(
            module,
            func_id,
            func_def,
            func_def.recv_slots,
        )
        .map_err(VmError::Jit)?;
        if plan.raw_capture_slots != func_def.recv_slots {
            return Err(VmError::Jit(format!(
                "spawn_call method receiver for func_id={} name={} requires receiver-inclusive param_types",
                func_id, func_def.name
            )));
        }
        return Ok(Some(SpawnCallTransferPlan {
            value_slot_offset: 0,
            required_end_slot: recv_slots,
            transfers: vec![plan.transfer_type],
        }));
    }

    Err(VmError::Jit(format!(
        "spawn_call param_types slots {} do not match args {} for function {} ({})",
        declared_slots, arg_slots, func_id, func_def.name
    )))
}

fn spawn_call_slot_types_require_transfer_metadata(slot_types: &[vo_runtime::SlotType]) -> bool {
    slot_types.iter().any(|slot| {
        matches!(
            slot,
            vo_runtime::SlotType::GcRef
                | vo_runtime::SlotType::Interface0
                | vo_runtime::SlotType::Interface1
        )
    })
}

fn transfer_type_slot_count(transfers: &[TransferType]) -> Result<usize, String> {
    transfers.iter().try_fold(0usize, |acc, transfer| {
        acc.checked_add(transfer.slots as usize)
            .ok_or_else(|| "spawn_call transfer metadata slot count overflow".to_string())
    })
}

fn validate_spawn_call_transfer_args(
    gc: &Gc,
    module: &Module,
    itab_cache: &ItabCache,
    func_id: u32,
    func_def: &FunctionDef,
    args: &mut [u64],
    slot_types: &[vo_runtime::SlotType],
    plan: &SpawnCallTransferPlan,
) -> Result<(), VmError> {
    let mut slot_idx = plan.value_slot_offset;
    for transfer in &plan.transfers {
        let width = transfer.slots as usize;
        let end = slot_idx.checked_add(width).ok_or_else(|| {
            VmError::Jit(format!(
                "spawn_call arg transfer slot overflow for function {} ({})",
                func_id, func_def.name
            ))
        })?;
        if end > args.len() {
            return Err(VmError::Jit(format!(
                "spawn_call arg transfer exceeds values for function {} ({}): transfer_end={} values={}",
                func_id,
                func_def.name,
                end,
                args.len()
            )));
        }
        let transfer_meta = validate_spawn_call_transfer_layout(
            module, slot_types, slot_idx, transfer, func_id, func_def,
        )?;
        if transfer_meta.value_kind() == vo_runtime::ValueKind::Interface {
            validate_spawn_call_interface_arg(
                gc, module, itab_cache, args, slot_idx, transfer, func_id, func_def,
            )?;
        } else {
            validate_spawn_call_concrete_arg(gc, args, slot_idx, transfer_meta, func_id, func_def)?;
        }
        slot_idx = end;
    }
    if slot_idx != plan.required_end_slot {
        return Err(VmError::Jit(format!(
            "spawn_call param_types consumed {} slots but transfer plan requires {} for function {} ({})",
            slot_idx, plan.required_end_slot, func_id, func_def.name
        )));
    }
    Ok(())
}

fn validate_spawn_call_transfer_layout(
    module: &Module,
    slot_types: &[vo_runtime::SlotType],
    slot_idx: usize,
    transfer: &TransferType,
    func_id: u32,
    func_def: &FunctionDef,
) -> Result<vo_runtime::ValueMeta, VmError> {
    let expected_meta = vo_runtime::ValueMeta::from_raw(transfer.meta_raw);
    let expected_rttid = vo_runtime::ValueRttid::from_raw(transfer.rttid_raw);
    if expected_meta.value_kind() != expected_rttid.value_kind() {
        return Err(VmError::Jit(format!(
            "spawn_call param metadata kind {:?} does not match RTTID kind {:?} for function {} ({}) slot={}",
            expected_meta.value_kind(),
            expected_rttid.value_kind(),
            func_id,
            func_def.name,
            slot_idx
        )));
    }
    let Some(canonical_meta) = module.canonical_value_meta_for_value_rttid(expected_rttid) else {
        return Err(VmError::Jit(format!(
            "spawn_call param RTTID cannot be resolved for function {} ({}) slot={}",
            func_id, func_def.name, slot_idx
        )));
    };
    if expected_meta != canonical_meta {
        return Err(VmError::Jit(format!(
            "spawn_call param metadata raw 0x{:x} does not match RTTID canonical raw 0x{:x} for function {} ({}) slot={}",
            expected_meta.to_raw(),
            canonical_meta.to_raw(),
            func_id,
            func_def.name,
            slot_idx
        )));
    }
    let Some(expected_layout) = module.slot_layout_for_value_rttid(expected_rttid) else {
        return Err(VmError::Jit(format!(
            "spawn_call param RTTID cannot resolve slot layout for function {} ({}) slot={}",
            func_id, func_def.name, slot_idx
        )));
    };
    if transfer.slots as usize != expected_layout.len() {
        return Err(VmError::Jit(format!(
            "spawn_call param transfer has {} slots but RTTID layout has {} for function {} ({}) slot={}",
            transfer.slots,
            expected_layout.len(),
            func_id,
            func_def.name,
            slot_idx
        )));
    }
    let end = slot_idx + expected_layout.len();
    if slot_types.get(slot_idx..end) != Some(expected_layout.as_slice()) {
        return Err(VmError::Jit(format!(
            "spawn_call param slot layout mismatch for function {} ({}) slot={}",
            func_id, func_def.name, slot_idx
        )));
    }
    Ok(expected_meta)
}

fn validate_spawn_call_concrete_arg(
    gc: &Gc,
    args: &mut [u64],
    slot_idx: usize,
    expected_meta: vo_runtime::ValueMeta,
    func_id: u32,
    func_def: &FunctionDef,
) -> Result<(), VmError> {
    let value_kind = expected_meta.value_kind();
    let Some(expected_header_kind) = spawn_call_heap_header_kind_for_value_kind(value_kind) else {
        return Ok(());
    };
    let raw = args[slot_idx];
    if raw == 0 {
        return Ok(());
    }
    let Some(canonical) = gc.canonicalize_ref(raw as GcRef) else {
        let (in_all, in_index, index_len) = gc.debug_ref_membership(raw as GcRef);
        return Err(VmError::Jit(format!(
            "spawn_call invalid GcRef arg func={} name={} slot={} raw=0x{:016x} in_all_objects={} in_object_index={} object_index_len={}",
            func_id,
            func_def.name,
            slot_idx,
            raw,
            in_all,
            in_index,
            index_len
        )));
    };
    args[slot_idx] = canonical as u64;
    let header = unsafe { Gc::header(canonical) };
    if header.kind() != expected_header_kind {
        return Err(VmError::Jit(format!(
            "spawn_call param object kind {:?} does not match expected {:?} for value kind {:?} func={} name={} slot={}",
            header.kind(),
            expected_header_kind,
            value_kind,
            func_id,
            func_def.name,
            slot_idx
        )));
    }
    if value_kind == vo_runtime::ValueKind::Pointer && header.meta_id() != expected_meta.meta_id() {
        return Err(VmError::Jit(format!(
            "spawn_call pointer param meta_id {} does not match expected {} for func={} name={} slot={}",
            header.meta_id(),
            expected_meta.meta_id(),
            func_id,
            func_def.name,
            slot_idx
        )));
    }
    Ok(())
}

fn spawn_call_heap_header_kind_for_value_kind(
    value_kind: vo_runtime::ValueKind,
) -> Option<vo_runtime::ValueKind> {
    match value_kind {
        vo_runtime::ValueKind::String
        | vo_runtime::ValueKind::Slice
        | vo_runtime::ValueKind::Map
        | vo_runtime::ValueKind::Channel
        | vo_runtime::ValueKind::Port
        | vo_runtime::ValueKind::Closure
        | vo_runtime::ValueKind::Island => Some(value_kind),
        vo_runtime::ValueKind::Pointer => Some(vo_runtime::ValueKind::Struct),
        _ => None,
    }
}

fn validate_spawn_call_interface_arg(
    gc: &Gc,
    module: &Module,
    itab_cache: &ItabCache,
    args: &mut [u64],
    slot_idx: usize,
    transfer: &TransferType,
    func_id: u32,
    func_def: &FunctionDef,
) -> Result<(), VmError> {
    if transfer.slots != 2 {
        return Err(VmError::Jit(format!(
            "spawn_call interface param has {} slots for function {} ({}) slot={}",
            transfer.slots, func_id, func_def.name, slot_idx
        )));
    }
    let expected_meta = vo_runtime::ValueMeta::from_raw(transfer.meta_raw);
    let expected_iface_meta_id = expected_meta.meta_id();
    let Some(expected_iface) = module.interface_metas.get(expected_iface_meta_id as usize) else {
        return Err(VmError::Jit(format!(
            "spawn_call interface param references missing interface {} for function {} ({}) slot={}",
            expected_iface_meta_id, func_id, func_def.name, slot_idx
        )));
    };
    let slot0 = args[slot_idx];
    let slot1 = args[slot_idx + 1];
    let value_kind = interface::unpack_value_kind(slot0);
    if value_kind == vo_runtime::ValueKind::Void {
        if slot1 != 0 {
            return Err(VmError::Jit(format!(
                "spawn_call nil interface arg has nonzero data for function {} ({}) slot={}",
                func_id, func_def.name, slot_idx
            )));
        }
        return Ok(());
    }
    if value_kind == vo_runtime::ValueKind::Interface {
        return Err(VmError::Jit(format!(
            "spawn_call raw interface-kind arg for function {} ({}) slot={}",
            func_id, func_def.name, slot_idx
        )));
    }
    let rttid = interface::unpack_rttid(slot0);
    if module
        .canonical_value_meta_for_value_rttid(vo_runtime::ValueRttid::new(rttid, value_kind))
        .is_none()
    {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg has non-canonical RTTID/kind for function {} ({}) slot={} rttid={} kind={:?}",
            func_id, func_def.name, slot_idx, rttid, value_kind
        )));
    }
    validate_spawn_call_interface_data_object(
        gc, module, args, slot_idx, slot0, slot1, rttid, value_kind, func_id, func_def,
    )?;
    let itab_id = interface::unpack_itab_id(slot0);
    if expected_iface.methods.is_empty() {
        return Ok(());
    }
    if itab_id == 0 {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg missing itab for function {} ({}) slot={} iface_meta_id={}",
            func_id, func_def.name, slot_idx, expected_iface_meta_id
        )));
    }
    let Some(named_type_id) =
        named_type_id_from_spawn_call_interface_value(module, rttid, value_kind)
    else {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg is not a named value for function {} ({}) slot={} rttid={} kind={:?}",
            func_id, func_def.name, slot_idx, rttid, value_kind
        )));
    };
    let Some(expected_methods) = vo_runtime::itab::expected_interface_itab_methods(
        named_type_id,
        expected_iface_meta_id,
        value_kind == vo_runtime::ValueKind::Pointer,
        &module.named_type_metas,
        &module.interface_metas,
    ) else {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg does not implement expected interface for function {} ({}) slot={} named_type_id={} iface_meta_id={}",
            func_id, func_def.name, slot_idx, named_type_id, expected_iface_meta_id
        )));
    };
    let Some(actual_itab) = itab_cache.get_itab(itab_id) else {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg references missing itab {} for function {} ({}) slot={}",
            itab_id, func_id, func_def.name, slot_idx
        )));
    };
    if actual_itab.iface_meta_id != expected_iface_meta_id
        || actual_itab.methods != expected_methods
    {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg itab {} does not match expected interface for function {} ({}) slot={} iface_meta_id={}",
            itab_id, func_id, func_def.name, slot_idx, expected_iface_meta_id
        )));
    }
    Ok(())
}

fn validate_spawn_call_interface_data_object(
    gc: &Gc,
    module: &Module,
    args: &mut [u64],
    slot_idx: usize,
    slot0: u64,
    slot1: u64,
    rttid: u32,
    value_kind: vo_runtime::ValueKind,
    func_id: u32,
    func_def: &FunctionDef,
) -> Result<(), VmError> {
    if !interface::data_is_gc_ref(slot0) {
        return Ok(());
    }
    if slot1 == 0 {
        if matches!(
            value_kind,
            vo_runtime::ValueKind::Struct | vo_runtime::ValueKind::Array
        ) {
            return Err(VmError::Jit(format!(
                "spawn_call interface arg data missing object for aggregate value kind {:?} func={} name={} slot={}",
                value_kind,
                func_id,
                func_def.name,
                slot_idx + 1
            )));
        }
        return Ok(());
    }
    let Some(canonical) = gc.canonicalize_ref(slot1 as GcRef) else {
        let (in_all, in_index, index_len) = gc.debug_ref_membership(slot1 as GcRef);
        return Err(VmError::Jit(format!(
            "spawn_call invalid interface GcRef arg func={} name={} slot={} raw=0x{:016x} in_all_objects={} in_object_index={} object_index_len={}",
            func_id,
            func_def.name,
            slot_idx + 1,
            slot1,
            in_all,
            in_index,
            index_len
        )));
    };
    args[slot_idx + 1] = canonical as u64;
    let header = unsafe { Gc::header(canonical) };
    let Some(expected_meta) =
        module.canonical_value_meta_for_value_rttid(vo_runtime::ValueRttid::new(rttid, value_kind))
    else {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg data RTTID cannot be resolved for function {} ({}) slot={} rttid={} kind={:?}",
            func_id,
            func_def.name,
            slot_idx + 1,
            rttid,
            value_kind
        )));
    };
    match value_kind {
        vo_runtime::ValueKind::Struct | vo_runtime::ValueKind::Pointer => {
            validate_spawn_call_interface_data_kind(
                header.kind(),
                vo_runtime::ValueKind::Struct,
                value_kind,
                func_id,
                func_def,
                slot_idx,
            )?;
            if header.meta_id() != expected_meta.meta_id() {
                return Err(VmError::Jit(format!(
                    "spawn_call interface arg data meta_id {} does not match expected {} for function {} ({}) slot={}",
                    header.meta_id(),
                    expected_meta.meta_id(),
                    func_id,
                    func_def.name,
                    slot_idx + 1
                )));
            }
            validate_spawn_call_struct_data_slots(
                module,
                header.meta_id(),
                header.slots as usize,
                func_id,
                func_def,
                slot_idx,
            )?;
        }
        vo_runtime::ValueKind::Array => {
            validate_spawn_call_interface_array_data(
                module, canonical, header, rttid, func_id, func_def, slot_idx,
            )?;
        }
        _ => {
            if let Some(expected_kind) = spawn_call_interface_data_heap_kind(value_kind) {
                validate_spawn_call_interface_data_kind(
                    header.kind(),
                    expected_kind,
                    value_kind,
                    func_id,
                    func_def,
                    slot_idx,
                )?;
            }
        }
    }
    Ok(())
}

fn spawn_call_interface_data_heap_kind(
    value_kind: vo_runtime::ValueKind,
) -> Option<vo_runtime::ValueKind> {
    match value_kind {
        vo_runtime::ValueKind::String
        | vo_runtime::ValueKind::Slice
        | vo_runtime::ValueKind::Map
        | vo_runtime::ValueKind::Channel
        | vo_runtime::ValueKind::Port
        | vo_runtime::ValueKind::Closure
        | vo_runtime::ValueKind::Island => Some(value_kind),
        _ => None,
    }
}

fn validate_spawn_call_interface_data_kind(
    actual: vo_runtime::ValueKind,
    expected: vo_runtime::ValueKind,
    value_kind: vo_runtime::ValueKind,
    func_id: u32,
    func_def: &FunctionDef,
    slot_idx: usize,
) -> Result<(), VmError> {
    if actual != expected {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg data object kind {:?} does not match expected {:?} for value kind {:?} func={} name={} slot={}",
            actual,
            expected,
            value_kind,
            func_id,
            func_def.name,
            slot_idx + 1
        )));
    }
    Ok(())
}

fn validate_spawn_call_struct_data_slots(
    module: &Module,
    struct_meta_id: u32,
    actual_slots: usize,
    func_id: u32,
    func_def: &FunctionDef,
    slot_idx: usize,
) -> Result<(), VmError> {
    let Some(struct_meta) = module.struct_metas.get(struct_meta_id as usize) else {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg data references missing StructMeta id {} for function {} ({}) slot={}",
            struct_meta_id,
            func_id,
            func_def.name,
            slot_idx + 1
        )));
    };
    validate_spawn_call_data_slot_width(
        actual_slots,
        struct_meta.slot_types.len(),
        func_id,
        func_def,
        slot_idx,
    )
}

fn validate_spawn_call_data_slot_width(
    actual_slots: usize,
    expected_slots: usize,
    func_id: u32,
    func_def: &FunctionDef,
    slot_idx: usize,
) -> Result<(), VmError> {
    if actual_slots != expected_slots {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg data allocation slots {} do not match expected {} for function {} ({}) slot={}",
            actual_slots,
            expected_slots,
            func_id,
            func_def.name,
            slot_idx + 1
        )));
    }
    Ok(())
}

fn validate_spawn_call_interface_array_data(
    module: &Module,
    array_ref: GcRef,
    header: &vo_runtime::gc::GcHeader,
    rttid: u32,
    func_id: u32,
    func_def: &FunctionDef,
    slot_idx: usize,
) -> Result<(), VmError> {
    let value_rttid = vo_runtime::ValueRttid::new(rttid, vo_runtime::ValueKind::Array);
    let Some((expected_len, expected_elem_rttid)) =
        interface_array_runtime_type(module, value_rttid)
    else {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg array data layout missing for function {} ({}) slot={} rttid={}",
            func_id,
            func_def.name,
            slot_idx + 1,
            rttid
        )));
    };
    let Some(expected_elem_meta) = module.canonical_value_meta_for_value_rttid(expected_elem_rttid)
    else {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg array data element RTTID cannot be resolved for function {} ({}) slot={} rttid={}",
            func_id,
            func_def.name,
            slot_idx + 1,
            expected_elem_rttid.rttid()
        )));
    };
    let Some(expected_elem_bytes) = sequence_element_physical_bytes(module, expected_elem_rttid)
    else {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg array data element layout missing for function {} ({}) slot={} rttid={}",
            func_id,
            func_def.name,
            slot_idx + 1,
            expected_elem_rttid.rttid()
        )));
    };
    match header.kind() {
        vo_runtime::ValueKind::Array => {}
        vo_runtime::ValueKind::Struct => {
            return validate_spawn_call_interface_array_value_slot_box(
                module, header, rttid, func_id, func_def, slot_idx,
            );
        }
        actual => {
            return Err(VmError::Jit(format!(
                "spawn_call interface arg data object kind {:?} does not match expected Array or Struct for value kind Array func={} name={} slot={}",
                actual,
                func_id,
                func_def.name,
                slot_idx + 1
            )));
        }
    }
    // Safety: the spawn validator canonicalized the object and checked its array kind.
    let actual_len = unsafe { array::len(array_ref) };
    let actual_elem_meta = unsafe { array::elem_meta(array_ref) };
    let actual_elem_bytes = unsafe { array::elem_bytes(array_ref) };
    if actual_len != expected_len
        || actual_elem_meta != expected_elem_meta
        || actual_elem_bytes != expected_elem_bytes
    {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg array data layout mismatch for function {} ({}) slot={}: len {} expected {}, elem_meta 0x{:x} expected 0x{:x}, elem_bytes {} expected {}",
            func_id,
            func_def.name,
            slot_idx + 1,
            actual_len,
            expected_len,
            actual_elem_meta.to_raw(),
            expected_elem_meta.to_raw(),
            actual_elem_bytes,
            expected_elem_bytes
        )));
    }
    Ok(())
}

fn validate_spawn_call_interface_array_value_slot_box(
    module: &Module,
    header: &vo_runtime::gc::GcHeader,
    rttid: u32,
    func_id: u32,
    func_def: &FunctionDef,
    slot_idx: usize,
) -> Result<(), VmError> {
    let Some(expected_layout) = module.slot_layout_for_value_rttid(vo_runtime::ValueRttid::new(
        rttid,
        vo_runtime::ValueKind::Array,
    )) else {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg array data layout missing for function {} ({}) slot={} rttid={}",
            func_id,
            func_def.name,
            slot_idx + 1,
            rttid
        )));
    };
    let Some(struct_meta) = module.struct_metas.get(header.meta_id() as usize) else {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg array data value-slot box references missing StructMeta id {} for function {} ({}) slot={}",
            header.meta_id(),
            func_id,
            func_def.name,
            slot_idx + 1
        )));
    };
    if struct_meta.slot_types != expected_layout {
        return Err(VmError::Jit(format!(
            "spawn_call interface arg array data value-slot box layout {:?} does not match Array slot layout {:?} for function {} ({}) slot={}",
            struct_meta.slot_types,
            expected_layout,
            func_id,
            func_def.name,
            slot_idx + 1
        )));
    }
    validate_spawn_call_data_slot_width(
        header.slots as usize,
        expected_layout.len(),
        func_id,
        func_def,
        slot_idx,
    )
}

fn interface_array_runtime_type(
    module: &Module,
    value_rttid: vo_runtime::ValueRttid,
) -> Option<(usize, vo_runtime::ValueRttid)> {
    let mut current = value_rttid;
    let limit = module.runtime_types.len() + module.named_type_metas.len() + 1;
    for _ in 0..limit {
        match module.runtime_types.get(current.rttid() as usize)? {
            vo_runtime::RuntimeType::Array { len, elem }
                if current.value_kind() == vo_runtime::ValueKind::Array =>
            {
                return Some((*len as usize, *elem));
            }
            vo_runtime::RuntimeType::Named { id, .. } => {
                let named = module.named_type_metas.get(*id as usize)?;
                if named.underlying_rttid.value_kind() != vo_runtime::ValueKind::Array {
                    return None;
                }
                current = named.underlying_rttid;
            }
            _ => return None,
        }
    }
    None
}

fn sequence_element_physical_bytes(
    module: &Module,
    value_rttid: vo_runtime::ValueRttid,
) -> Option<usize> {
    match value_rttid.value_kind() {
        vo_runtime::ValueKind::Void => Some(0),
        vo_runtime::ValueKind::Bool
        | vo_runtime::ValueKind::Int8
        | vo_runtime::ValueKind::Uint8 => Some(1),
        vo_runtime::ValueKind::Int16 | vo_runtime::ValueKind::Uint16 => Some(2),
        vo_runtime::ValueKind::Int32
        | vo_runtime::ValueKind::Uint32
        | vo_runtime::ValueKind::Float32 => Some(4),
        _ => module
            .slot_layout_for_value_rttid(value_rttid)
            .and_then(|layout| layout.len().checked_mul(vo_runtime::slot::SLOT_BYTES)),
    }
}

fn named_type_id_from_spawn_call_interface_value(
    module: &Module,
    rttid: u32,
    value_kind: vo_runtime::ValueKind,
) -> Option<u32> {
    match module.runtime_types.get(rttid as usize)? {
        vo_runtime::RuntimeType::Named { id, .. } => Some(*id),
        vo_runtime::RuntimeType::Pointer(elem) if value_kind == vo_runtime::ValueKind::Pointer => {
            match module.runtime_types.get(elem.rttid() as usize)? {
                vo_runtime::RuntimeType::Named { id, .. } => Some(*id),
                _ => None,
            }
        }
        _ => None,
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
