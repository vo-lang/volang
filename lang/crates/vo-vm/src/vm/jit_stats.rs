#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum JitSideExitReason {
    // Explicit JIT/interpreter handoffs belong here: native side exits plus
    // cold/not-hot interpreter handoffs. Compile, metadata, and internal ABI
    // failures return JitError and are not side-exit reasons.
    InterpretedCold = 0,
    RegularCall = 1,
    PreparedDynamicCall = 2,
    Yield = 3,
    QueueBlock = 4,
    WaitIo = 5,
    WaitQueue = 6,
    Replay = 7,
    LoopNotHot = 8,
    HostEvent = 9,
    LoopMetadataUnavailable = 10,
    InterpretedUnsupported = 11,
    InterpretedFeedbackDisabled = 12,
    InterpretedResourceRejected = 13,
    InterpretedCompilerFault = 14,
    GcSafepoint = 15,
}

impl JitSideExitReason {
    pub const ALL: [Self; 16] = [
        Self::InterpretedCold,
        Self::RegularCall,
        Self::PreparedDynamicCall,
        Self::Yield,
        Self::QueueBlock,
        Self::WaitIo,
        Self::WaitQueue,
        Self::Replay,
        Self::LoopNotHot,
        Self::HostEvent,
        Self::LoopMetadataUnavailable,
        Self::InterpretedUnsupported,
        Self::InterpretedFeedbackDisabled,
        Self::InterpretedResourceRejected,
        Self::InterpretedCompilerFault,
        Self::GcSafepoint,
    ];
    pub const COUNT: usize = Self::ALL.len();

    pub const fn as_str(self) -> &'static str {
        match self {
            Self::InterpretedCold => "interpreted_cold",
            Self::RegularCall => "regular_call",
            Self::PreparedDynamicCall => "prepared_dynamic_call",
            Self::Yield => "yield",
            Self::QueueBlock => "queue_block",
            Self::WaitIo => "wait_io",
            Self::WaitQueue => "wait_queue",
            Self::Replay => "replay",
            Self::LoopNotHot => "loop_not_hot",
            Self::HostEvent => "host_event",
            Self::LoopMetadataUnavailable => "loop_metadata_unavailable",
            Self::InterpretedUnsupported => "interpreted_unsupported",
            Self::InterpretedFeedbackDisabled => "interpreted_feedback_disabled",
            Self::InterpretedResourceRejected => "interpreted_resource_rejected",
            Self::InterpretedCompilerFault => "interpreted_compiler_fault",
            Self::GcSafepoint => "gc_safepoint",
        }
    }

    #[inline]
    const fn index(self) -> usize {
        self as usize
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct JitSideExitReasonStats {
    counts: [u64; JitSideExitReason::COUNT],
}

impl JitSideExitReasonStats {
    #[inline]
    pub fn get(self, reason: JitSideExitReason) -> u64 {
        self.counts[reason.index()]
    }

    #[inline]
    pub fn total(self) -> u64 {
        self.counts.iter().sum()
    }

    #[inline]
    #[cfg(any(feature = "jit", test))]
    pub(crate) fn increment(&mut self, reason: JitSideExitReason) {
        self.counts[reason.index()] = self.counts[reason.index()].saturating_add(1);
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct JitExecutionStats {
    /// VM-to-JIT full-function dispatches that reached the native entry.
    pub function_entries: u64,
    /// Loop OSR dispatches that reached the native entry.
    pub loop_entries: u64,
    pub side_exit_reasons: JitSideExitReasonStats,
    pub low_progress_function_disables: u64,
    pub low_progress_loop_disables: u64,
    /// Runtime-dominated short functions disabled after repeated low-work OK returns.
    pub runtime_dominated_function_disables: u64,
    /// Successfully published full-function artifacts for this VM owner.
    pub function_compilations: u64,
    /// Successfully published loop artifacts for this VM owner.
    pub loop_compilations: u64,
    /// Publications that reused code already compiled by the shared Island family.
    pub compilation_cache_hits: u64,
    /// Wall-clock compilation time, excluding execution, in nanoseconds.
    pub compilation_time_ns: u64,
    /// Newly emitted native code attributed to successful publications.
    pub compiled_code_bytes: u64,
    /// Valid closure prepare callbacks reached after native IC misses.
    pub closure_prepare_callbacks: u64,
    /// Valid interface prepare callbacks reached after native IC misses.
    pub iface_prepare_callbacks: u64,
    /// Fiber stack windows reserved by dynamic-call prepare callbacks.
    pub prepared_frame_reservations: u64,
    /// Sum of callee local slots reserved by dynamic-call prepare callbacks.
    pub prepared_frame_slots_reserved: u64,
    /// Prepared misses dispatched directly to an already compiled callee.
    pub prepared_jit_dispatches: u64,
    /// Prepared misses handed to the VM because no eligible native entry exists.
    pub prepared_vm_dispatches: u64,
    /// Dynamic-call misses that published an eligible native IC entry.
    pub dynamic_ic_publications: u64,
    /// Taken allocation polls that entered the VM safepoint callback.
    pub gc_safepoint_callbacks: u64,
    /// Native JIT frames validated through precise stack maps.
    pub native_root_frames_scanned: u64,
    /// Exact native root slots visited through precise stack maps.
    pub native_roots_scanned: u64,
    /// Native frames whose interface/tagged roots require VM materialization.
    pub native_root_conditional_frames: u64,
    /// Bounded native scans completed by materializing into the VM scanner.
    pub native_root_scan_budget_exhaustions: u64,
}

impl JitExecutionStats {
    pub fn executed_jit_code(self) -> bool {
        self.function_entries > 0 || self.loop_entries > 0
    }

    pub fn side_exit_count(self, reason: JitSideExitReason) -> u64 {
        self.side_exit_reasons.get(reason)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn side_exit_reason_catalog_is_complete_and_machine_readable() {
        let mut seen = [false; JitSideExitReason::COUNT];
        for reason in JitSideExitReason::ALL {
            let index = reason.index();
            assert!(!seen[index], "duplicate side-exit index {index}");
            seen[index] = true;
            assert!(!reason.as_str().is_empty());
            assert!(reason
                .as_str()
                .bytes()
                .all(|byte| byte.is_ascii_lowercase() || byte == b'_'));
        }
        assert!(seen.into_iter().all(|value| value));
    }

    #[test]
    fn side_exit_reason_stats_count_by_canonical_reason() {
        let mut stats = JitSideExitReasonStats::default();
        stats.increment(JitSideExitReason::InterpretedCold);
        stats.increment(JitSideExitReason::InterpretedCold);
        stats.increment(JitSideExitReason::WaitIo);

        assert_eq!(stats.get(JitSideExitReason::InterpretedCold), 2);
        assert_eq!(stats.get(JitSideExitReason::WaitIo), 1);
        assert_eq!(stats.total(), 3);
    }
}
