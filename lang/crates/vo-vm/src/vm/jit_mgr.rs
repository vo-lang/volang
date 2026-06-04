#![allow(clippy::result_large_err)]
//! JIT Manager - Unified JIT state machine for function compilation.
//!
//! Manages compilation states, hot counters, and version dispatch for all functions.
//!
//! ## Architecture
//!
//! The JIT supports two compilation modes:
//! 1. **Full function compilation** - Triggered when a function becomes hot (many calls)
//! 2. **Loop OSR (On-Stack Replacement)** - Triggered when a loop backedge becomes hot

use vo_runtime::bytecode::{FunctionDef, Module as VoModule};

use std::collections::HashMap;

use vo_jit::loop_analysis::try_analyze_loops_with_module;
use vo_jit::{JitCompiler, JitError, JitFunc, LoopFunc, LoopInfo};

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
}

impl JitSideExitReason {
    pub const COUNT: usize = 9;

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
    fn increment(&mut self, reason: JitSideExitReason) {
        self.counts[reason.index()] += 1;
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct JitExecutionStats {
    pub function_entries: u64,
    pub loop_entries: u64,
    pub side_exit_reasons: JitSideExitReasonStats,
}

impl JitExecutionStats {
    pub fn executed_jit_code(self) -> bool {
        self.function_entries > 0 || self.loop_entries > 0
    }

    pub fn side_exit_count(self, reason: JitSideExitReason) -> u64 {
        self.side_exit_reasons.get(reason)
    }
}

// =============================================================================
// Configuration
// =============================================================================

/// JIT configuration.
#[derive(Debug, Clone)]
pub struct JitConfig {
    /// Call count threshold for full function compilation.
    pub call_threshold: u32,
    /// Backedge count threshold for loop OSR compilation.
    pub loop_threshold: u32,
    /// Print Cranelift IR for compiled functions.
    pub debug_ir: bool,
}

impl Default for JitConfig {
    fn default() -> Self {
        Self {
            call_threshold: 100,
            loop_threshold: 50,
            debug_ir: false,
        }
    }
}

// =============================================================================
// Compile State
// =============================================================================

/// Function compilation state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompileState {
    /// Not compiled, use VM interpreter.
    Interpreted,
    /// Has full function JIT version.
    FullyCompiled,
    /// Cannot JIT (unsupported features), never retry.
    Unsupported,
}

// =============================================================================
// Function JIT Info
// =============================================================================

/// Per-function JIT information.
pub struct FunctionJitInfo {
    /// Compilation state.
    pub state: CompileState,

    /// Full function JIT entry.
    pub full_entry: Option<JitFunc>,

    /// Call count (for triggering full compilation).
    pub call_count: u32,

    /// Loop backedge counts: (begin_pc) -> count.
    pub loop_counts: HashMap<usize, u32>,

    /// Analyzed loops (lazily populated).
    pub loops: Option<Vec<LoopInfo>>,

    /// Last loop analysis error, if OSR discovery failed for this function.
    pub loop_analysis_error: Option<String>,

    /// Last full-function compilation error, if the function was disabled.
    pub compile_error: Option<String>,

    /// Loops that failed compilation (never retry).
    pub failed_loops: std::collections::HashSet<usize>,
}

impl FunctionJitInfo {
    fn new() -> Self {
        Self {
            state: CompileState::Interpreted,
            full_entry: None,
            call_count: 0,
            loop_counts: HashMap::new(),
            loops: None,
            loop_analysis_error: None,
            compile_error: None,
            failed_loops: std::collections::HashSet::new(),
        }
    }
}

// =============================================================================
// JIT Manager
// =============================================================================

/// JIT Manager - Unified state machine for JIT compilation.
pub struct JitManager {
    /// Per-function JIT info.
    funcs: Vec<FunctionJitInfo>,

    /// Fast dispatch table: func_id -> full_entry pointer (null = use VM).
    /// Used by JIT code for direct JIT-to-JIT calls.
    func_table: Vec<*const u8>,

    /// Direct call table: only populated for functions that can elide a VM frame.
    /// Used by prepare_closure_call / prepare_iface_call to decide if the IC hit
    /// native-stack fast path is safe.
    direct_call_table: Vec<*const u8>,

    /// Cranelift compiler.
    compiler: JitCompiler,

    /// Configuration.
    config: JitConfig,

    /// Counts of JIT-compiled code that was actually entered during this VM run.
    execution_stats: JitExecutionStats,

    /// Reusable scratch buffer for direct callee snapshots.
    available_direct_callees_buf: Vec<u32>,
}

// SAFETY: func_table contains raw pointers to JIT code which is thread-safe to read.
unsafe impl Send for JitManager {}
unsafe impl Sync for JitManager {}

impl JitManager {
    /// Create a new JIT manager.
    pub fn new() -> Result<Self, JitError> {
        Ok(Self {
            funcs: Vec::new(),
            func_table: Vec::new(),
            direct_call_table: Vec::new(),
            compiler: JitCompiler::new()?,
            config: JitConfig::default(),
            execution_stats: JitExecutionStats::default(),
            available_direct_callees_buf: Vec::new(),
        })
    }

    /// Create a new JIT manager with custom config.
    pub fn with_config(config: JitConfig) -> Result<Self, JitError> {
        let compiler = JitCompiler::with_debug(config.debug_ir)?;
        Ok(Self {
            funcs: Vec::new(),
            func_table: Vec::new(),
            direct_call_table: Vec::new(),
            compiler,
            config,
            execution_stats: JitExecutionStats::default(),
            available_direct_callees_buf: Vec::new(),
        })
    }

    /// Initialize for a module (call after module load).
    pub fn init(&mut self, func_count: usize) {
        self.funcs = (0..func_count).map(|_| FunctionJitInfo::new()).collect();
        self.func_table = vec![std::ptr::null(); func_count];
        self.direct_call_table = vec![std::ptr::null(); func_count];
        self.execution_stats = JitExecutionStats::default();
    }

    /// Get function table pointer for JIT code.
    #[inline]
    pub fn func_table_ptr(&self) -> *const *const u8 {
        self.func_table.as_ptr()
    }

    /// Get function table length.
    #[inline]
    pub fn func_table_len(&self) -> usize {
        self.func_table.len()
    }

    /// Get direct call table pointer for JIT code.
    /// Only contains entries for functions safe for JIT-to-JIT direct calls
    /// (no defer and no nested calls).
    #[inline]
    pub fn direct_call_table_ptr(&self) -> *const *const u8 {
        self.direct_call_table.as_ptr()
    }

    /// Get direct call table length.
    #[inline]
    pub fn direct_call_table_len(&self) -> usize {
        self.direct_call_table.len()
    }

    /// Get JIT configuration (for passing to island threads).
    #[inline]
    pub fn config(&self) -> &JitConfig {
        &self.config
    }

    #[inline]
    pub fn execution_stats(&self) -> JitExecutionStats {
        self.execution_stats
    }

    #[inline]
    pub fn record_function_entry(&mut self) {
        self.execution_stats.function_entries += 1;
    }

    #[inline]
    pub fn record_loop_entry(&mut self) {
        self.execution_stats.loop_entries += 1;
    }

    #[inline]
    pub fn record_side_exit(&mut self, reason: JitSideExitReason) {
        self.execution_stats.side_exit_reasons.increment(reason);
    }

    fn rebuild_available_direct_callees(&self, out: &mut Vec<u32>) {
        out.clear();
        if out.capacity() < self.func_table.len() {
            out.reserve(self.func_table.len() - out.capacity());
        }

        for (id, ptr) in self.func_table.iter().enumerate() {
            if !ptr.is_null() {
                out.push(id as u32);
            }
        }
    }

    // =========================================================================
    // Query API
    // =========================================================================

    /// Get full function JIT entry for dispatch (O(1)).
    /// Returns None if should use VM.
    #[inline]
    pub fn get_entry(&self, func_id: u32) -> Option<JitFunc> {
        let ptr = self.func_table.get(func_id as usize)?;
        if ptr.is_null() {
            None
        } else {
            Some(unsafe { std::mem::transmute::<*const u8, JitFunc>(*ptr) })
        }
    }

    /// Resolve which version to use for a function call.
    /// Returns Some(jit_func) if JIT version available, None for explicit cold interpreter handoff.
    /// Also handles hot tracking and triggers compilation when threshold reached.
    pub fn resolve_call(
        &mut self,
        func_id: u32,
        func_def: &FunctionDef,
        module: &VoModule,
    ) -> Result<Option<JitFunc>, JitError> {
        // 1. Already have JIT version?
        if let Some(jit_func) = self.get_entry(func_id) {
            return Ok(Some(jit_func));
        }

        if self.is_unsupported(func_id)? {
            let msg = self
                .funcs
                .get(func_id as usize)
                .and_then(|info| info.compile_error.as_deref())
                .unwrap_or("function is marked unsupported for JIT");
            return Err(JitError::Internal(format!(
                "function {} cannot be JIT-compiled: {msg}",
                func_def.name
            )));
        }

        // 2. Record call, compile if hot
        if self.record_call(func_id)? {
            match self.compile_full(func_id, func_def, module) {
                Ok(()) => {
                    if let Some(entry) = self.get_entry(func_id) {
                        return Ok(Some(entry));
                    }
                }
                Err(err) => return Err(err),
            }
        }

        // 3. Fall back to VM only because the function is not hot yet.
        self.record_side_exit(JitSideExitReason::InterpretedCold);
        Ok(None)
    }

    // =========================================================================
    // Recording API
    // =========================================================================

    /// Record a function call. Returns true if the function should be compiled.
    pub fn record_call(&mut self, func_id: u32) -> Result<bool, JitError> {
        let id = func_id as usize;
        let info = self
            .funcs
            .get_mut(id)
            .ok_or(JitError::FunctionNotFound(func_id))?;
        info.call_count += 1;
        Ok(
            info.call_count >= self.config.call_threshold
                && info.state == CompileState::Interpreted,
        )
    }

    /// Record a loop backedge hit. Returns true if loop OSR should be triggered.
    pub fn record_backedge(
        &mut self,
        func_id: u32,
        loop_begin_pc: usize,
    ) -> Result<bool, JitError> {
        let id = func_id as usize;
        let info = self
            .funcs
            .get_mut(id)
            .ok_or(JitError::FunctionNotFound(func_id))?;

        let count = info.loop_counts.entry(loop_begin_pc).or_insert(0);
        *count += 1;
        Ok(*count >= self.config.loop_threshold)
    }

    /// Get or analyze loops for a function.
    pub fn get_loops(
        &mut self,
        func_id: u32,
        func_def: &FunctionDef,
        module: &VoModule,
    ) -> Result<&[LoopInfo], JitError> {
        let id = func_id as usize;
        let info = self
            .funcs
            .get_mut(id)
            .ok_or(JitError::FunctionNotFound(func_id))?;

        if info.loops.is_none() {
            match try_analyze_loops_with_module(func_def, module) {
                Ok(loops) => {
                    info.loop_analysis_error = None;
                    info.loops = Some(loops);
                }
                Err(err) => {
                    info.loop_analysis_error = Some(err.to_string());
                    return Err(err.into());
                }
            }
        }

        info.loops.as_deref().ok_or_else(|| {
            JitError::Internal(format!("loop analysis missing for function {func_id}"))
        })
    }

    /// Find loop info by begin_pc.
    pub fn find_loop(
        &mut self,
        func_id: u32,
        func_def: &FunctionDef,
        module: &VoModule,
        begin_pc: usize,
    ) -> Result<Option<LoopInfo>, JitError> {
        let loops = self.get_loops(func_id, func_def, module)?;
        Ok(loops.iter().find(|l| l.begin_pc == begin_pc).cloned())
    }

    /// Return the last explicit loop-analysis failure recorded for a function.
    pub fn last_loop_analysis_error(&self, func_id: u32) -> Option<&str> {
        self.funcs
            .get(func_id as usize)
            .and_then(|info| info.loop_analysis_error.as_deref())
    }

    // =========================================================================
    // Compilation API
    // =========================================================================

    /// Compile full function version.
    pub fn compile_full(
        &mut self,
        func_id: u32,
        func_def: &FunctionDef,
        module: &VoModule,
    ) -> Result<(), JitError> {
        let idx = func_id as usize;
        let current_state = match self.funcs.get(idx) {
            Some(i) => i.state,
            None => return Err(JitError::FunctionNotFound(func_id)),
        };

        // Already compiled
        if current_state == CompileState::FullyCompiled {
            return Ok(());
        }

        // Build a snapshot of currently compiled functions so codegen can emit
        // direct FuncRef calls for non-self static callees when possible.
        let mut available_direct_callees = std::mem::take(&mut self.available_direct_callees_buf);
        self.rebuild_available_direct_callees(&mut available_direct_callees);

        // Compile
        let compile_result =
            self.compiler
                .compile(func_id, func_def, module, &available_direct_callees);
        self.available_direct_callees_buf = available_direct_callees;
        if let Err(e) = compile_result {
            if let Some(info) = self.funcs.get_mut(idx) {
                info.state = CompileState::Unsupported;
                info.compile_error = Some(e.to_string());
            }
            return Err(e);
        }

        // Get function pointer
        let ptr = unsafe { self.compiler.get_func_ptr(func_id) }
            .ok_or_else(|| JitError::Internal("compiled but no pointer".into()))?;

        // Update state
        if let Some(info) = self.funcs.get_mut(idx) {
            info.full_entry = Some(ptr);
            info.state = CompileState::FullyCompiled;
            info.compile_error = None;
        }
        self.func_table[idx] = ptr as *const u8;

        // Only populate direct_call_table if the callee can run on the IC hit
        // native-stack path without hiding roots, panic/unwind state, or frame
        // observation from the VM.
        if vo_jit::can_elide_frame_for_direct_jit(func_def) {
            self.direct_call_table[idx] = ptr as *const u8;
        }

        Ok(())
    }

    /// Check if function is already compiled.
    pub fn is_compiled(&self, func_id: u32) -> Result<bool, JitError> {
        self.funcs
            .get(func_id as usize)
            .map(|info| info.state == CompileState::FullyCompiled)
            .ok_or(JitError::FunctionNotFound(func_id))
    }

    /// Check if function is marked as unsupported.
    pub fn is_unsupported(&self, func_id: u32) -> Result<bool, JitError> {
        self.funcs
            .get(func_id as usize)
            .map(|info| info.state == CompileState::Unsupported)
            .ok_or(JitError::FunctionNotFound(func_id))
    }

    /// Mark function as unsupported.
    pub fn mark_unsupported(&mut self, func_id: u32) -> Result<(), JitError> {
        let info = self
            .funcs
            .get_mut(func_id as usize)
            .ok_or(JitError::FunctionNotFound(func_id))?;
        info.state = CompileState::Unsupported;
        info.compile_error = Some("function marked unsupported".to_string());
        Ok(())
    }

    /// Compile function (alias for compile_full).
    pub fn compile_function(
        &mut self,
        func_id: u32,
        func_def: &FunctionDef,
        module: &VoModule,
    ) -> Result<(), JitError> {
        self.compile_full(func_id, func_def, module)
    }

    /// Check if a loop has failed compilation.
    pub fn is_loop_failed(&self, func_id: u32, begin_pc: usize) -> Result<bool, JitError> {
        self.funcs
            .get(func_id as usize)
            .map(|info| info.failed_loops.contains(&begin_pc))
            .ok_or(JitError::FunctionNotFound(func_id))
    }

    /// Mark a loop as failed (never retry).
    pub fn mark_loop_failed(&mut self, func_id: u32, begin_pc: usize) -> Result<(), JitError> {
        let info = self
            .funcs
            .get_mut(func_id as usize)
            .ok_or(JitError::FunctionNotFound(func_id))?;
        info.failed_loops.insert(begin_pc);
        Ok(())
    }

    /// Compile a loop for OSR.
    pub fn compile_loop(
        &mut self,
        func_id: u32,
        func_def: &FunctionDef,
        module: &VoModule,
        loop_info: &LoopInfo,
    ) -> Result<(), JitError> {
        if self.funcs.get(func_id as usize).is_none() {
            return Err(JitError::FunctionNotFound(func_id));
        }
        let mut available_direct_callees = std::mem::take(&mut self.available_direct_callees_buf);
        self.rebuild_available_direct_callees(&mut available_direct_callees);
        let compile_result = self.compiler.compile_loop(
            func_id,
            func_def,
            module,
            loop_info,
            &available_direct_callees,
        );
        self.available_direct_callees_buf = available_direct_callees;
        compile_result
    }

    /// Get loop function pointer.
    /// # Safety
    /// The returned function pointer must only be called with the correct ABI.
    pub unsafe fn get_loop_func(&self, func_id: u32, begin_pc: usize) -> Option<LoopFunc> {
        self.compiler.get_loop_func_ptr(func_id, begin_pc)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn empty_func() -> FunctionDef {
        FunctionDef {
            name: "f".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 0,
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
            code: Vec::new(),
            jit_metadata: Vec::new(),
            slot_types: Vec::new(),
            borrowed_scan_slots_prefix: Vec::new(),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    #[test]
    fn side_exit_reason_stats_are_machine_readable() {
        let mut stats = JitSideExitReasonStats::default();
        stats.increment(JitSideExitReason::InterpretedCold);
        stats.increment(JitSideExitReason::InterpretedCold);
        stats.increment(JitSideExitReason::WaitIo);

        assert_eq!(stats.get(JitSideExitReason::InterpretedCold), 2);
        assert_eq!(stats.get(JitSideExitReason::WaitIo), 1);
        assert_eq!(stats.total(), 3);
    }

    #[test]
    fn manager_records_side_exit_reasons() {
        let mut manager = JitManager::new().expect("jit manager");
        manager.record_side_exit(JitSideExitReason::RegularCall);
        manager.record_side_exit(JitSideExitReason::Replay);

        let stats = manager.execution_stats();
        assert_eq!(stats.side_exit_count(JitSideExitReason::RegularCall), 1);
        assert_eq!(stats.side_exit_count(JitSideExitReason::Replay), 1);
    }

    #[test]
    fn unsupported_function_error_is_not_counted_as_side_exit() {
        let func = empty_func();
        let mut module = VoModule::new("m".to_string());
        module.functions.push(func.clone());
        let mut manager = JitManager::new().expect("jit manager");
        manager.init(1);
        manager.mark_unsupported(0).expect("mark unsupported");

        let err = manager
            .resolve_call(0, &func, &module)
            .expect_err("unsupported function must fail fast");

        assert!(
            err.to_string().contains("cannot be JIT-compiled"),
            "unexpected error: {err}"
        );
        assert_eq!(manager.execution_stats().side_exit_reasons.total(), 0);
    }

    #[test]
    fn manager_rejects_out_of_range_func_ids_without_panicking() {
        let func = empty_func();
        let mut module = VoModule::new("m".to_string());
        module.functions.push(func.clone());
        let mut manager = JitManager::new().expect("jit manager");
        manager.init(1);

        assert!(matches!(
            manager.resolve_call(7, &func, &module),
            Err(JitError::FunctionNotFound(7))
        ));
        assert!(matches!(
            manager.record_backedge(7, 0),
            Err(JitError::FunctionNotFound(7))
        ));
        assert!(matches!(
            manager.mark_unsupported(7),
            Err(JitError::FunctionNotFound(7))
        ));
        assert!(matches!(
            manager.is_loop_failed(7, 0),
            Err(JitError::FunctionNotFound(7))
        ));
    }
}
