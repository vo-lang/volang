//! Structured call types for the extern function interface.
//!
//! These types work with `ExternCallContext` to provide a compact calling convention
//! for extern function dispatch.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::gc::Gc;
#[cfg(feature = "std")]
use crate::io::IoRuntime;
use crate::itab::ItabCache;
use crate::output::OutputSink;
use vo_common_core::bytecode::ModuleRuntimeMetadata;
use vo_common_core::types::SlotType;

use super::SentinelErrorCache;

// =============================================================================
// ExternInvoke: compact call descriptor
// =============================================================================

/// Compact call descriptor that describes how to interpret a `stack: &mut [u64]`
/// as an extern call frame.
///
/// **Key property**: args and returns may overlap (required by the JIT path).
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct ExternInvoke {
    /// Extern function ID in the registry.
    pub extern_id: u32,
    /// Base pointer (frame start in the stack slice).
    pub bp: u32,
    /// Argument start slot (relative to bp).
    pub arg_start: u16,
    /// Argument slot count (u64 slots, not parameter count).
    pub arg_slots: u16,
    /// Return value start slot (relative to bp).
    pub ret_start: u16,
    /// Return value slot count (u64 slots, not return count).
    pub ret_slots: u16,
}

// =============================================================================
// ExternWorld: borrowed runtime state (no fiber-owned fields)
// =============================================================================

/// Scheduler-bound requests issued by the public `runtime/mem` providers.
///
/// Providers only enqueue work here. The VM consumes it after the active
/// extern call returns to a safe boundary where complete roots are available.
#[derive(Default)]
pub struct RuntimeMemRequests {
    work_units: usize,
    collect: bool,
}

impl RuntimeMemRequests {
    #[inline]
    pub fn has_pending(&self) -> bool {
        self.collect || self.work_units != 0
    }

    #[inline]
    pub(super) fn request_step(&mut self, work_units: usize) {
        self.work_units = self.work_units.max(work_units);
    }

    #[inline]
    pub(super) fn request_collect(&mut self) {
        self.collect = true;
    }

    #[inline]
    pub fn take(&mut self) -> (bool, usize) {
        let pending = (self.collect, self.work_units);
        *self = Self::default();
        pending
    }
}

/// Groups all borrowed "world state" needed by extern functions.
///
/// These are naturally owned by the VM runtime state. Fiber-owned inputs
/// (replay state, resume tokens) belong in `ExternFiberInputs`, not here.
#[non_exhaustive]
pub struct ExternWorld<'env> {
    pub gc: &'env mut Gc,
    pub module: ModuleRuntimeMetadata<'env>,
    pub itab_cache: &'env mut ItabCache,

    pub runtime_mem_requests: Option<&'env mut RuntimeMemRequests>,

    pub program_args: &'env [Vec<u8>],

    /// Output sink for fmt.Print / println.
    pub output: &'env dyn OutputSink,
    pub sentinel_errors: &'env mut SentinelErrorCache,

    /// Generic byte output channel (FFI → Host).
    /// FFI functions write here; the host reads after `run_scheduled()` returns.
    pub host_output: &'env mut Option<Vec<u8>>,

    /// Validated V2 table plus the authoritative caller endpoint for this VM.
    /// Adapter code must take caller identity from this binding.
    pub host_services_v2: Option<&'env crate::host_services_v2::HostServicesV2Binding>,

    #[cfg(feature = "std")]
    pub io: Option<&'env mut IoRuntime>,
}

impl<'env> ExternWorld<'env> {
    #[inline]
    pub fn new(
        gc: &'env mut Gc,
        module: ModuleRuntimeMetadata<'env>,
        itab_cache: &'env mut ItabCache,
        program_args: &'env [Vec<u8>],
        output: &'env dyn OutputSink,
        sentinel_errors: &'env mut SentinelErrorCache,
        host_output: &'env mut Option<Vec<u8>>,
    ) -> Self {
        Self {
            gc,
            module,
            itab_cache,
            runtime_mem_requests: None,
            program_args,
            output,
            sentinel_errors,
            host_output,
            host_services_v2: None,
            #[cfg(feature = "std")]
            io: None,
        }
    }

    #[inline]
    pub fn with_runtime_mem_requests(mut self, requests: &'env mut RuntimeMemRequests) -> Self {
        self.runtime_mem_requests = Some(requests);
        self
    }

    #[inline]
    pub fn with_host_services_v2(
        mut self,
        binding: Option<&'env crate::host_services_v2::HostServicesV2Binding>,
    ) -> Self {
        self.host_services_v2 = binding;
        self
    }

    #[cfg(feature = "std")]
    #[inline]
    pub fn with_io(mut self, io: &'env mut IoRuntime) -> Self {
        self.io = Some(io);
        self
    }
}

// =============================================================================
// ExternFiberInputs: one-shot fiber-derived inputs
// =============================================================================

/// One typed cached closure result used while replaying an extern call.
///
/// The values are exposed to extern code through `resume_closure_result()`;
/// the slot metadata travels with the payload so VM-owned replay logs remain
/// precise GC roots while the extern is re-executed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternReplayResult {
    pub values: Vec<u64>,
    pub slot_types: Vec<SlotType>,
}

impl ExternReplayResult {
    #[inline]
    pub fn new(values: Vec<u64>, slot_types: Vec<SlotType>) -> Self {
        Self { values, slot_types }
    }
}

/// One-shot inputs derived from the active fiber immediately before calling
/// an extern function. Replay results are snapshots; the VM keeps the
/// authoritative typed replay log on the fiber until the extern terminates.
#[derive(Default)]
pub struct ExternFiberInputs {
    /// Opaque pointer to the current fiber.
    pub fiber_opaque: *mut core::ffi::c_void,

    /// I/O completion token that woke this fiber. Present only on the
    /// PC re-execution path (second execution of the same `CallExtern`
    /// after the runtime resumes the fiber).
    pub resume_io_token: Option<u64>,

    /// Host event token that woke this fiber. Present only on the PC re-execution
    /// path (second execution of the same `CallExtern`) after `HostEventWaitAndReplay`.
    pub resume_host_event_token: Option<u64>,

    /// Opaque data attached by the host when waking via `wake_host_event_with_data`.
    /// FFI function reads on replay via `take_resume_host_event_data()`.
    pub resume_host_event_data: Option<Vec<u8>>,

    /// Cached closure results from previous `CallClosure` round-trips.
    /// Consumed in order via `ExternCallContext.replay_index`.
    pub replay_results: Vec<ExternReplayResult>,

    /// Original panic message captured when the replayed closure unwound.
    /// `is_some()` also serves as the "panicked" flag.
    pub replay_panic_message: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::RuntimeMemRequests;

    #[test]
    fn runtime_mem_requests_coalesce_and_clear() {
        let mut requests = RuntimeMemRequests::default();
        requests.request_step(8);
        requests.request_step(3);
        requests.request_collect();

        assert_eq!(requests.take(), (true, 8));
        assert_eq!(requests.take(), (false, 0));
    }
}
