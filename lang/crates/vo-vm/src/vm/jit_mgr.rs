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

use vo_common_core::verifier::VerifiedModule;
#[cfg(test)]
use vo_runtime::bytecode::FunctionDef;
use vo_runtime::bytecode::LoadedModule;
#[cfg(test)]
use vo_runtime::bytecode::Module as VoModule;
use vo_runtime::jit_api::JitResult;

use std::collections::HashMap;
use std::sync::{Arc, Mutex, MutexGuard, OnceLock};

use vo_jit::{JitCompileEnv, JitCompiler, JitError, JitFailureKind, JitFunc, LoopFunc, LoopInfo};

use super::{JitExecutionStats, JitSideExitReason};

const LOW_PROGRESS_BUDGET_DELTA: u32 = 32;
const LOW_PROGRESS_EXIT_LIMIT: u8 = 8;
const DISABLED_LOW_PROGRESS_STREAK: u8 = u8::MAX;

#[inline]
fn update_low_progress_streak(
    streak: &mut u8,
    result: JitResult,
    budget_before: u32,
    budget_after: u32,
) -> bool {
    match result {
        JitResult::Ok => *streak = 0,
        JitResult::WaitIo
        | JitResult::WaitQueue
        | JitResult::Replay
        | JitResult::ExternSuspend
        | JitResult::RuntimeTransition => match budget_before.checked_sub(budget_after) {
            Some(delta) if delta <= LOW_PROGRESS_BUDGET_DELTA => *streak = streak.saturating_add(1),
            _ => *streak = 0,
        },
        JitResult::Call | JitResult::Panic | JitResult::JitError => return false,
    }
    *streak >= LOW_PROGRESS_EXIT_LIMIT
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
    /// Maximum page-granular native-code bytes retained by one Island family.
    pub code_memory_limit_bytes: usize,
    /// Maximum reusable analysis bytes retained by one Island family.
    pub analysis_memory_limit_bytes: usize,
}

impl Default for JitConfig {
    fn default() -> Self {
        Self {
            call_threshold: 100,
            loop_threshold: 50,
            debug_ir: false,
            code_memory_limit_bytes: vo_jit::DEFAULT_JIT_CODE_MEMORY_LIMIT_BYTES,
            analysis_memory_limit_bytes: vo_jit::MAX_JIT_ANALYSIS_BYTES,
        }
    }
}

// =============================================================================
// Compile State
// =============================================================================

/// Function compilation state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CompileState {
    /// Not compiled, use VM interpreter.
    Interpreted,
    /// Has full function JIT version.
    FullyCompiled,
    /// Compilation was rejected and this artifact should stay interpreted.
    Failed(JitFailureKind),
}

#[derive(Default)]
struct LoopJitState {
    backedge_count: u32,
    low_progress_exit_streak: u8,
    failure: Option<JitFailureKind>,
    entry: Option<LoopFunc>,
}

// =============================================================================
// Function JIT Info
// =============================================================================

/// Per-function JIT information.
struct FunctionJitInfo {
    /// Compilation state.
    state: CompileState,

    /// Call count (for triggering full compilation).
    call_count: u32,

    /// Hotness and execution feedback keyed by loop begin pc.
    loop_states: HashMap<usize, LoopJitState>,

    /// Last full-function compilation error, if the function was disabled.
    compile_error: Option<String>,

    /// Consecutive low-progress boundaries, or the disabled sentinel.
    full_low_progress_exit_streak: u8,
}

impl FunctionJitInfo {
    fn new() -> Self {
        Self {
            state: CompileState::Interpreted,
            call_count: 0,
            loop_states: HashMap::new(),
            compile_error: None,
            full_low_progress_exit_streak: 0,
        }
    }
}

// =============================================================================
// JIT Manager
// =============================================================================

/// Executable code owner shared by one process-local Island family.
///
/// Compilation is serialized, while finalized functions execute without this
/// lock. Cranelift allocates newly finalized code in fresh pages and leaves
/// previously published entry points stable until the compiler is dropped.
pub(super) struct SharedJitCode {
    compiler: Mutex<JitCompiler>,
    module: OnceLock<Arc<LoadedModule>>,
}

// SAFETY: every access that mutates the compiler is serialized by `compiler`.
// JitCompiler and Cranelift's memory owner can move between threads, and
// finalized code pages remain immutable while other family members execute.
// The last Arc owner drops the compiler only after every manager is gone.
unsafe impl Send for SharedJitCode {}
unsafe impl Sync for SharedJitCode {}

impl SharedJitCode {
    fn new(config: &JitConfig) -> Result<Self, JitError> {
        Ok(Self {
            compiler: Mutex::new(JitCompiler::with_resource_limits(
                config.debug_ir,
                config.code_memory_limit_bytes,
                config.analysis_memory_limit_bytes,
            )?),
            module: OnceLock::new(),
        })
    }

    pub(super) fn retain_module(&self, module: &Arc<LoadedModule>) -> Result<(), JitError> {
        // Serialize the one-time owner publication with compiler binding. This
        // keeps strict verification failures transactional and prevents a
        // concurrent best-effort manager from publishing a different image.
        let _compiler = self.lock()?;
        if let Some(bound) = self.module.get() {
            return if Arc::ptr_eq(bound, module) {
                Ok(())
            } else {
                Err(JitError::ModuleScopeChanged)
            };
        }

        if self.module.set(module.clone()).is_err()
            && !self
                .module
                .get()
                .is_some_and(|bound| Arc::ptr_eq(bound, module))
        {
            return Err(JitError::ModuleScopeChanged);
        }
        Ok(())
    }

    fn bind_verified_module(&self, module: &Arc<LoadedModule>) -> Result<(), JitError> {
        let mut compiler = self.lock()?;
        if let Some(bound) = self.module.get() {
            if !Arc::ptr_eq(bound, module) {
                return Err(JitError::ModuleScopeChanged);
            }
            return compiler.bind_loaded_module_scope(Arc::clone(module));
        }

        // Bind before publishing the shared owner. The compiler retains its own
        // Arc, so a failed publication cannot leave a dangling module identity.
        compiler.bind_loaded_module_scope(Arc::clone(module))?;
        self.module.set(module.clone()).map_err(|_| {
            JitError::Internal("shared JIT module owner was published out of order".to_string())
        })
    }

    fn lock_verified(
        &self,
        verified: VerifiedModule<'_>,
    ) -> Result<MutexGuard<'_, JitCompiler>, JitError> {
        let retained = self.module.get().ok_or_else(|| {
            JitError::Internal("shared JIT compiler has no retained module owner".to_string())
        })?;
        if !verified.matches(retained.module()) {
            return Err(JitError::ModuleScopeChanged);
        }
        let mut compiler = self.lock()?;
        compiler.bind_loaded_module_scope(Arc::clone(retained))?;
        Ok(compiler)
    }

    fn lock(&self) -> Result<MutexGuard<'_, JitCompiler>, JitError> {
        self.compiler
            .lock()
            .map_err(|_| JitError::Internal("shared JIT compiler lock poisoned".to_string()))
    }

    fn code_memory_stats(&self) -> vo_jit::JitCodeMemoryStats {
        match self.compiler.lock() {
            Ok(compiler) => compiler.code_memory_stats(),
            Err(poisoned) => poisoned.into_inner().code_memory_stats(),
        }
    }

    fn analysis_memory_stats(&self) -> vo_jit::JitAnalysisMemoryStats {
        match self.compiler.lock() {
            Ok(compiler) => compiler.analysis_memory_stats(),
            Err(poisoned) => poisoned.into_inner().analysis_memory_stats(),
        }
    }

    fn analyzed_loops(
        &self,
        verified: VerifiedModule<'_>,
        func_id: u32,
    ) -> Result<Arc<[LoopInfo]>, JitError> {
        let mut compiler = self.lock_verified(verified)?;
        compiler.analyzed_loaded_loops(func_id)
    }
}

/// JIT Manager - Unified state machine for JIT compilation.
pub struct JitManager {
    /// Per-function JIT info.
    funcs: Vec<FunctionJitInfo>,

    /// Fast dispatch table: func_id -> full_entry pointer (null = use VM).
    /// Used by JIT code for direct JIT-to-JIT calls.
    func_table: Vec<*const u8>,

    /// Cranelift compiler and executable code shared by related Islands.
    shared_code: Arc<SharedJitCode>,

    /// Configuration.
    config: JitConfig,

    /// Counts of JIT-compiled code that was actually entered during this VM run.
    execution_stats: JitExecutionStats,
}

// SAFETY: the raw function pointers refer to code owned by `shared_code`, which
// moves with the manager and may also be retained by related Islands. All
// mutable dispatch policy and execution counters still have one VM-thread
// owner. JitManager deliberately has no Sync implementation.
unsafe impl Send for JitManager {}

impl JitManager {
    /// Create a new JIT manager.
    pub fn new() -> Result<Self, JitError> {
        Self::with_config(JitConfig::default())
    }

    /// Create a new JIT manager with custom config.
    pub fn with_config(config: JitConfig) -> Result<Self, JitError> {
        let shared_code = Arc::new(SharedJitCode::new(&config)?);
        Ok(Self::with_shared_code(config, shared_code))
    }

    pub(super) fn with_shared_code(mut config: JitConfig, shared_code: Arc<SharedJitCode>) -> Self {
        config.code_memory_limit_bytes = shared_code.code_memory_stats().limit_bytes;
        config.analysis_memory_limit_bytes = shared_code.analysis_memory_stats().limit_bytes;
        Self {
            funcs: Vec::new(),
            func_table: Vec::new(),
            shared_code,
            config,
            execution_stats: JitExecutionStats::default(),
        }
    }

    pub(super) fn shared_code(&self) -> Arc<SharedJitCode> {
        self.shared_code.clone()
    }

    /// Initialize for a module (call after module load).
    pub fn init(&mut self, func_count: usize) {
        self.funcs = (0..func_count).map(|_| FunctionJitInfo::new()).collect();
        self.func_table = vec![std::ptr::null(); func_count];
        self.execution_stats = JitExecutionStats::default();
    }

    /// Bind and initialize from the common-verifier-owned immutable image.
    pub fn init_verified(&mut self, module: &Arc<LoadedModule>) -> Result<(), JitError> {
        self.shared_code.bind_verified_module(module)?;
        if self.func_table.len() != module.functions.len() {
            self.init(module.functions.len());
        }
        Ok(())
    }

    pub(super) fn init_best_effort(&mut self, module: &Arc<LoadedModule>) -> Result<(), JitError> {
        self.shared_code.retain_module(module)?;
        self.init(module.functions.len());
        Ok(())
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
    pub fn code_memory_stats(&self) -> vo_jit::JitCodeMemoryStats {
        self.shared_code.code_memory_stats()
    }

    #[inline]
    pub fn analysis_memory_stats(&self) -> vo_jit::JitAnalysisMemoryStats {
        self.shared_code.analysis_memory_stats()
    }

    pub fn unsupported_function_count(&self) -> usize {
        self.funcs
            .iter()
            .filter(|info| info.state == CompileState::Failed(JitFailureKind::SemanticUnsupported))
            .count()
    }

    pub fn resource_rejected_function_count(&self) -> usize {
        self.function_failure_count(JitFailureKind::ResourceRejected)
    }

    pub fn compiler_fault_function_count(&self) -> usize {
        self.function_failure_count(JitFailureKind::CompilerFault)
    }

    fn function_failure_count(&self, kind: JitFailureKind) -> usize {
        self.funcs
            .iter()
            .filter(|info| info.state == CompileState::Failed(kind))
            .count()
    }

    pub fn function_failure_kind(&self, func_id: u32) -> Option<JitFailureKind> {
        match self.funcs.get(func_id as usize)?.state {
            CompileState::Failed(kind) => Some(kind),
            CompileState::Interpreted | CompileState::FullyCompiled => None,
        }
    }

    pub fn function_compile_error(&self, func_id: u32) -> Option<&str> {
        self.funcs
            .get(func_id as usize)
            .and_then(|info| info.compile_error.as_deref())
    }

    #[inline]
    pub fn record_function_entry(&mut self) {
        self.execution_stats.function_entries =
            self.execution_stats.function_entries.saturating_add(1);
    }

    #[inline]
    pub fn record_loop_entry(&mut self) {
        self.execution_stats.loop_entries = self.execution_stats.loop_entries.saturating_add(1);
    }

    #[inline]
    pub fn record_side_exit(&mut self, reason: JitSideExitReason) {
        self.execution_stats.side_exit_reasons.increment(reason);
    }

    /// Feed one completed full-function invocation back into dispatch policy.
    ///
    /// The native body stays in the compiler cache. Repeated cooperative exits
    /// with little bytecode progress only removes its shared dispatch entry.
    pub(crate) fn record_function_outcome(
        &mut self,
        func_id: u32,
        result: JitResult,
        budget_before: u32,
        budget_after: u32,
    ) -> Result<bool, JitError> {
        let idx = func_id as usize;
        let should_disable = {
            let info = self
                .funcs
                .get_mut(idx)
                .ok_or(JitError::FunctionNotFound(func_id))?;
            if info.state != CompileState::FullyCompiled
                || info.full_low_progress_exit_streak == DISABLED_LOW_PROGRESS_STREAK
            {
                return Ok(false);
            }
            update_low_progress_streak(
                &mut info.full_low_progress_exit_streak,
                result,
                budget_before,
                budget_after,
            )
        };
        if !should_disable {
            return Ok(false);
        }

        self.funcs[idx].full_low_progress_exit_streak = DISABLED_LOW_PROGRESS_STREAK;
        self.execution_stats.low_progress_function_disables = self
            .execution_stats
            .low_progress_function_disables
            .saturating_add(1);
        self.func_table[idx] = std::ptr::null();
        Ok(true)
    }

    /// Feed one completed loop OSR invocation back into dispatch policy.
    pub(crate) fn record_loop_outcome(
        &mut self,
        func_id: u32,
        loop_pc: usize,
        result: JitResult,
        budget_before: u32,
        budget_after: u32,
    ) -> Result<bool, JitError> {
        let info = self
            .funcs
            .get_mut(func_id as usize)
            .ok_or(JitError::FunctionNotFound(func_id))?;
        let state = info.loop_states.entry(loop_pc).or_default();
        if state.low_progress_exit_streak == DISABLED_LOW_PROGRESS_STREAK {
            return Ok(false);
        }
        if !update_low_progress_streak(
            &mut state.low_progress_exit_streak,
            result,
            budget_before,
            budget_after,
        ) {
            return Ok(false);
        }
        state.low_progress_exit_streak = DISABLED_LOW_PROGRESS_STREAK;
        self.execution_stats.low_progress_loop_disables = self
            .execution_stats
            .low_progress_loop_disables
            .saturating_add(1);
        Ok(true)
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

    pub(crate) fn interpreter_reason(
        &self,
        func_id: u32,
    ) -> Result<Option<JitSideExitReason>, JitError> {
        if self.get_entry(func_id).is_some() {
            return Ok(None);
        }
        if self.is_full_entry_disabled(func_id)? {
            return Ok(Some(JitSideExitReason::InterpretedFeedbackDisabled));
        }
        if let Some(kind) = self.function_failure_kind(func_id) {
            return Ok(Some(match kind {
                JitFailureKind::SemanticUnsupported => JitSideExitReason::InterpretedUnsupported,
                JitFailureKind::ResourceRejected => JitSideExitReason::InterpretedResourceRejected,
                JitFailureKind::CompilerFault => JitSideExitReason::InterpretedCompilerFault,
            }));
        }
        Ok(Some(JitSideExitReason::InterpretedCold))
    }

    /// Resolve which version to use for a function call.
    /// Returns Some(jit_func) if JIT version available, None for explicit cold interpreter handoff.
    /// Also handles hot tracking and triggers compilation when threshold reached. Callers that
    /// commit a JIT-to-VM handoff own the matching side-exit statistics.
    pub fn resolve_call(
        &mut self,
        func_id: u32,
        verified: VerifiedModule<'_>,
        env: JitCompileEnv<'_>,
    ) -> Result<Option<JitFunc>, JitError> {
        // 1. Already have JIT version?
        if let Some(jit_func) = self.get_entry(func_id) {
            return Ok(Some(jit_func));
        }

        if self.is_full_entry_disabled(func_id)? || self.is_compile_failed(func_id)? {
            return Ok(None);
        }

        // 2. Record call, compile if hot
        if self.record_call(func_id)? {
            self.compile_full(func_id, verified, env)?;
            if let Some(entry) = self.get_entry(func_id) {
                return Ok(Some(entry));
            }
        }

        // 3. Fall back to VM only because the function is not hot yet.
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
        info.call_count = info.call_count.saturating_add(1);
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

        let state = info.loop_states.entry(loop_begin_pc).or_default();
        state.backedge_count = state.backedge_count.saturating_add(1);
        Ok(state.backedge_count >= self.config.loop_threshold)
    }

    /// Find loop info by begin_pc.
    pub fn find_loop(
        &self,
        func_id: u32,
        verified: VerifiedModule<'_>,
        begin_pc: usize,
    ) -> Result<Option<LoopInfo>, JitError> {
        self.funcs
            .get(func_id as usize)
            .ok_or(JitError::FunctionNotFound(func_id))?;
        let loops = self.shared_code.analyzed_loops(verified, func_id)?;
        Ok(loops.iter().find(|l| l.begin_pc == begin_pc).cloned())
    }

    // =========================================================================
    // Compilation API
    // =========================================================================

    /// Compile full function version.
    pub fn compile_full(
        &mut self,
        func_id: u32,
        verified: VerifiedModule<'_>,
        env: JitCompileEnv<'_>,
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

        let compile_result = (|| {
            let mut compiler = self.shared_code.lock_verified(verified)?;
            compiler.compile_loaded(func_id, env)?;
            unsafe { compiler.get_func_ptr(func_id) }
                .ok_or_else(|| JitError::Internal("compiled but no pointer".into()))
        })();
        let ptr = match compile_result {
            Ok(ptr) => ptr,
            Err(e) => {
                if let Some(info) = self.funcs.get_mut(idx) {
                    info.state = CompileState::Failed(e.failure_kind());
                    info.compile_error = Some(e.to_string());
                }
                return Err(e);
            }
        };

        // Update state
        if let Some(info) = self.funcs.get_mut(idx) {
            info.state = CompileState::FullyCompiled;
            info.compile_error = None;
            info.full_low_progress_exit_streak = 0;
        }
        self.func_table[idx] = ptr as *const u8;

        Ok(())
    }

    /// Check if this function has a terminal compilation failure.
    pub fn is_compile_failed(&self, func_id: u32) -> Result<bool, JitError> {
        self.funcs
            .get(func_id as usize)
            .map(|info| matches!(info.state, CompileState::Failed(_)))
            .ok_or(JitError::FunctionNotFound(func_id))
    }

    /// Check whether execution feedback disabled an otherwise compiled entry.
    fn is_full_entry_disabled(&self, func_id: u32) -> Result<bool, JitError> {
        self.funcs
            .get(func_id as usize)
            .map(|info| info.full_low_progress_exit_streak == DISABLED_LOW_PROGRESS_STREAK)
            .ok_or(JitError::FunctionNotFound(func_id))
    }

    /// Mark function as unsupported.
    #[cfg(test)]
    fn mark_unsupported(&mut self, func_id: u32) -> Result<(), JitError> {
        let info = self
            .funcs
            .get_mut(func_id as usize)
            .ok_or(JitError::FunctionNotFound(func_id))?;
        info.state = CompileState::Failed(JitFailureKind::SemanticUnsupported);
        info.compile_error = Some("function marked unsupported".to_string());
        Ok(())
    }

    /// Check if a loop has failed compilation.
    pub fn is_loop_failed(&self, func_id: u32, begin_pc: usize) -> Result<bool, JitError> {
        self.funcs
            .get(func_id as usize)
            .map(|info| {
                info.loop_states
                    .get(&begin_pc)
                    .is_some_and(|state| state.failure.is_some())
            })
            .ok_or(JitError::FunctionNotFound(func_id))
    }

    /// Check whether execution feedback disabled a compiled OSR entry.
    pub(crate) fn is_loop_disabled(&self, func_id: u32, begin_pc: usize) -> Result<bool, JitError> {
        self.funcs
            .get(func_id as usize)
            .map(|info| {
                info.loop_states
                    .get(&begin_pc)
                    .map(|state| state.low_progress_exit_streak)
                    == Some(DISABLED_LOW_PROGRESS_STREAK)
            })
            .ok_or(JitError::FunctionNotFound(func_id))
    }

    /// Mark a loop as failed (never retry under the current JIT policy).
    pub fn mark_loop_failed(
        &mut self,
        func_id: u32,
        begin_pc: usize,
        failure: JitFailureKind,
    ) -> Result<(), JitError> {
        let info = self
            .funcs
            .get_mut(func_id as usize)
            .ok_or(JitError::FunctionNotFound(func_id))?;
        info.loop_states.entry(begin_pc).or_default().failure = Some(failure);
        Ok(())
    }

    /// Compile a loop for OSR.
    pub fn compile_loop(
        &mut self,
        func_id: u32,
        verified: VerifiedModule<'_>,
        env: JitCompileEnv<'_>,
        loop_info: &LoopInfo,
    ) -> Result<LoopFunc, JitError> {
        if self.funcs.get(func_id as usize).is_none() {
            return Err(JitError::FunctionNotFound(func_id));
        }
        let compile_result = (|| {
            let mut compiler = self.shared_code.lock_verified(verified)?;
            compiler.compile_loaded_loop(func_id, env, loop_info)?;
            let loop_func = unsafe { compiler.get_loop_func_ptr(func_id, loop_info.begin_pc) }
                .ok_or_else(|| {
                    JitError::Internal(format!(
                        "compiled loop at pc {} but no function pointer was registered",
                        loop_info.begin_pc
                    ))
                })?;
            Ok::<LoopFunc, JitError>(loop_func)
        })();
        let loop_func = match compile_result {
            Ok(loop_func) => loop_func,
            Err(error) => {
                self.mark_loop_failed(func_id, loop_info.begin_pc, error.failure_kind())?;
                return Err(error);
            }
        };
        self.funcs[func_id as usize]
            .loop_states
            .entry(loop_info.begin_pc)
            .or_default()
            .entry = Some(loop_func);
        Ok(loop_func)
    }

    /// Get an Island-local published loop entry without touching the shared
    /// compiler lock on a steady-state backedge.
    pub fn get_loop_entry(&self, func_id: u32, begin_pc: usize) -> Option<LoopFunc> {
        self.funcs
            .get(func_id as usize)?
            .loop_states
            .get(&begin_pc)?
            .entry
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::{jit::build_jit_context, Vm};
    use vo_runtime::bytecode::InstructionMetadata;
    use vo_runtime::instruction::{Instruction, Opcode};

    extern "C" fn dormant_jit_entry(
        _ctx: *mut vo_runtime::jit_api::JitContext,
        _args: *mut u64,
        _ret: *mut u64,
    ) -> JitResult {
        JitResult::Ok
    }

    fn manager_with_active_entry() -> JitManager {
        let mut manager = JitManager::new().expect("jit manager");
        manager.init(1);
        manager.funcs[0].state = CompileState::FullyCompiled;
        let ptr = dormant_jit_entry as *const u8;
        manager.func_table[0] = ptr;
        manager
    }

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
            instruction_metadata: Vec::new(),
            slot_types: Vec::new(),
            borrowed_scan_slots_prefix: Vec::new(),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    fn valid_jit_func(name: &str, code: Vec<Instruction>) -> FunctionDef {
        let mut func = empty_func();
        func.name = name.to_string();
        func.instruction_metadata = vec![InstructionMetadata::None; code.len()];
        func.code = code;
        func.borrowed_scan_slots_prefix =
            FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types);
        (func.has_calls, func.has_call_extern) = FunctionDef::compute_call_flags(&func.code);
        func
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
    fn cooperative_boundary_feedback_classification_is_narrow() {
        for result in [
            JitResult::WaitIo,
            JitResult::WaitQueue,
            JitResult::Replay,
            JitResult::ExternSuspend,
            JitResult::RuntimeTransition,
        ] {
            let mut streak = 0;
            assert!(!update_low_progress_streak(
                &mut streak,
                result,
                100,
                100 - LOW_PROGRESS_BUDGET_DELTA,
            ));
            assert_eq!(streak, 1, "{result:?} should participate");
        }

        let mut streak = LOW_PROGRESS_EXIT_LIMIT;
        for result in [JitResult::Call, JitResult::Panic, JitResult::JitError] {
            assert!(!update_low_progress_streak(&mut streak, result, 100, 100,));
            assert_eq!(
                streak, LOW_PROGRESS_EXIT_LIMIT,
                "{result:?} should leave the streak unchanged"
            );
        }

        let mut streak = LOW_PROGRESS_EXIT_LIMIT - 1;
        assert!(!update_low_progress_streak(
            &mut streak,
            JitResult::Ok,
            100,
            100,
        ));
        assert_eq!(streak, 0);
        streak = LOW_PROGRESS_EXIT_LIMIT - 1;
        assert!(!update_low_progress_streak(
            &mut streak,
            JitResult::WaitIo,
            100,
            100 - LOW_PROGRESS_BUDGET_DELTA - 1,
        ));
        assert_eq!(streak, 0);
    }

    #[test]
    fn repeated_low_progress_boundaries_disable_full_entry() {
        let mut manager = manager_with_active_entry();

        for _ in 1..LOW_PROGRESS_EXIT_LIMIT {
            assert!(!manager
                .record_function_outcome(0, JitResult::WaitQueue, 100, 100)
                .expect("record function outcome"));
        }
        assert!(manager.get_entry(0).is_some());
        assert!(!manager
            .record_function_outcome(0, JitResult::Call, 100, 100)
            .expect("Call leaves feedback unchanged"));
        assert!(manager
            .record_function_outcome(0, JitResult::RuntimeTransition, 100, 100)
            .expect("record disabling outcome"));

        assert!(manager.is_full_entry_disabled(0).expect("function state"));
        assert_eq!(
            manager.interpreter_reason(0).expect("interpreter reason"),
            Some(JitSideExitReason::InterpretedFeedbackDisabled)
        );
        assert!(manager.get_entry(0).is_none());
        assert!(manager.func_table[0].is_null());
        assert_eq!(manager.execution_stats().low_progress_function_disables, 1);
        assert!(!manager
            .record_function_outcome(0, JitResult::WaitQueue, 100, 100)
            .expect("disabled entry ignores later outcomes"));
        assert_eq!(manager.execution_stats().low_progress_function_disables, 1);
    }

    #[test]
    fn compiled_caller_observes_later_callee_dispatch_disable() {
        let caller = valid_jit_func(
            "caller",
            vec![
                Instruction::new(Opcode::Call, 1, 0, 0),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ],
        );
        let callee = valid_jit_func("callee", vec![Instruction::new(Opcode::Return, 0, 0, 0)]);
        let mut module = VoModule::new("jit-disabled-callee-dispatch".to_string());
        module.functions = vec![caller, callee];

        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit VM");
        vm.load(module).expect("load valid call module");
        let loaded = vm.module.as_ref().expect("loaded module").clone();
        let externs = vo_runtime::bytecode::ResolvedExternTable::empty();
        let env = JitCompileEnv {
            externs: &externs,
            backend_caps: Default::default(),
        };

        let caller_entry = {
            let manager = vm.jit.manager_mut().expect("jit manager");
            manager
                .compile_full(1, loaded.verified_module(), env)
                .expect("compile callee first");
            manager
                .compile_full(0, loaded.verified_module(), env)
                .expect("compile caller after callee");
            manager.get_entry(0).expect("compiled caller entry")
        };

        for attempt in 0..LOW_PROGRESS_EXIT_LIMIT {
            let disabled = vm
                .jit
                .manager_mut()
                .expect("jit manager")
                .record_function_outcome(1, JitResult::WaitQueue, 100, 100)
                .expect("record callee outcome");
            assert_eq!(disabled, attempt + 1 == LOW_PROGRESS_EXIT_LIMIT);
        }
        assert!(vm
            .jit
            .manager()
            .expect("jit manager")
            .get_entry(1)
            .is_none());

        let mut fiber = Fiber::new(1);
        fiber.execution_budget = 100;
        let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("JIT context");
        let mut args = [0_u64; 1];
        let mut ret = [0xfeed_u64];

        let result = caller_entry(ctx.as_ptr(), args.as_mut_ptr(), ret.as_mut_ptr());

        assert_eq!(result, JitResult::Call);
        assert_eq!(ctx.call_func_id(), 1);
        assert_eq!(ctx.call_resume_pc(), 1);
        assert_eq!(ret, [0xfeed]);
    }

    #[test]
    fn loop_feedback_is_isolated_by_function_and_pc() {
        let mut manager = JitManager::new().expect("jit manager");
        manager.init(2);

        for _ in 1..LOW_PROGRESS_EXIT_LIMIT {
            manager
                .record_loop_outcome(0, 7, JitResult::WaitQueue, 100, 100)
                .expect("record loop outcome");
        }
        manager
            .record_loop_outcome(0, 9, JitResult::WaitQueue, 100, 100)
            .expect("record second loop outcome");
        manager
            .record_loop_outcome(1, 7, JitResult::WaitQueue, 100, 100)
            .expect("record other function outcome");
        assert!(manager
            .record_loop_outcome(0, 7, JitResult::RuntimeTransition, 100, 100)
            .expect("disable hot loop"));

        assert!(manager.is_loop_disabled(0, 7).expect("loop state"));
        assert!(!manager.is_loop_disabled(0, 9).expect("loop state"));
        assert!(!manager.is_loop_disabled(1, 7).expect("loop state"));
        assert_eq!(manager.execution_stats().low_progress_loop_disables, 1);
        assert!(!manager
            .record_loop_outcome(0, 7, JitResult::WaitQueue, 100, 100)
            .expect("disabled loop ignores later outcomes"));
        assert_eq!(manager.execution_stats().low_progress_loop_disables, 1);
        manager.init(2);
        assert_eq!(manager.execution_stats(), JitExecutionStats::default());
    }

    #[test]
    fn unsupported_function_stays_on_interpreter_without_side_exit_noise() {
        let func = valid_jit_func("f", vec![Instruction::new(Opcode::Return, 0, 0, 0)]);
        let mut module = VoModule::new("m".to_string());
        module.functions.push(func.clone());
        let mut manager = JitManager::new().expect("jit manager");
        manager.init(1);
        manager.mark_unsupported(0).expect("mark unsupported");
        let externs = vo_runtime::bytecode::ResolvedExternTable::empty();
        let env = JitCompileEnv {
            externs: &externs,
            backend_caps: Default::default(),
        };
        let verified = vo_common_core::verifier::verify_module(&module).expect("valid module");

        let entry = manager
            .resolve_call(0, verified, env)
            .expect("unsupported function should remain interpretable");

        assert!(entry.is_none());
        assert_eq!(manager.unsupported_function_count(), 1);
        assert_eq!(
            manager.function_compile_error(0),
            Some("function marked unsupported")
        );
        assert_eq!(manager.execution_stats().side_exit_reasons.total(), 0);
    }

    #[test]
    fn manager_rejects_out_of_range_func_ids_without_panicking() {
        let func = valid_jit_func("f", vec![Instruction::new(Opcode::Return, 0, 0, 0)]);
        let mut module = VoModule::new("m".to_string());
        module.functions.push(func.clone());
        let mut manager = JitManager::new().expect("jit manager");
        manager.init(1);
        let externs = vo_runtime::bytecode::ResolvedExternTable::empty();
        let env = JitCompileEnv {
            externs: &externs,
            backend_caps: Default::default(),
        };
        let verified = vo_common_core::verifier::verify_module(&module).expect("valid module");

        assert!(matches!(
            manager.resolve_call(7, verified, env),
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

    #[test]
    fn related_managers_compile_one_shared_function_artifact() {
        let func = valid_jit_func("shared", vec![Instruction::new(Opcode::Return, 0, 0, 0)]);
        let mut module = VoModule::new("shared-jit-family".to_string());
        module.functions.push(func);
        let config = JitConfig {
            call_threshold: 1,
            ..JitConfig::default()
        };
        let mut owner = Vm::try_with_jit_config(config.clone()).expect("owner VM");
        owner.load(module).expect("load shared module");
        let loaded = owner.module.as_ref().expect("loaded module").clone();
        let shared_code = owner
            .jit
            .manager()
            .expect("owner JIT manager")
            .shared_code();

        let barrier = Arc::new(std::sync::Barrier::new(2));
        let mut workers = Vec::new();
        for _ in 0..2 {
            let loaded = loaded.clone();
            let shared_code = shared_code.clone();
            let config = config.clone();
            let barrier = barrier.clone();
            workers.push(std::thread::spawn(move || {
                let mut manager = JitManager::with_shared_code(config, shared_code);
                manager
                    .init_verified(&loaded)
                    .expect("bind shared verified module");
                let externs = vo_runtime::bytecode::ResolvedExternTable::empty();
                barrier.wait();
                manager
                    .compile_full(
                        0,
                        loaded.verified_module(),
                        JitCompileEnv {
                            externs: &externs,
                            backend_caps: Default::default(),
                        },
                    )
                    .expect("compile shared function");
                manager.get_entry(0).expect("local published entry") as usize
            }));
        }

        let entries: Vec<_> = workers
            .into_iter()
            .map(|worker| worker.join().expect("JIT worker"))
            .collect();
        assert_eq!(entries[0], entries[1]);
        assert_eq!(
            owner.jit_code_memory_stats().function_count,
            1,
            "the Island family must own one compiled artifact"
        );
        assert!(
            owner
                .jit
                .manager()
                .expect("owner JIT manager")
                .get_entry(0)
                .is_none(),
            "sharing code must not publish another Island's local dispatch state"
        );
    }

    #[test]
    fn shared_jit_retains_module_until_last_family_owner_drops() {
        let mut module = VoModule::new("shared-jit-lifetime".to_string());
        module.functions.push(valid_jit_func(
            "retained",
            vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        ));
        let loaded =
            Arc::new(vo_common_core::verifier::verify_loaded_module(module).expect("valid module"));
        let weak = Arc::downgrade(&loaded);

        let mut owner = JitManager::new().expect("owner manager");
        owner.init_verified(&loaded).expect("bind owner module");
        let shared_code = owner.shared_code();
        let child_config = JitConfig {
            code_memory_limit_bytes: 0,
            analysis_memory_limit_bytes: 0,
            ..Default::default()
        };
        let mut child = JitManager::with_shared_code(child_config, shared_code.clone());
        assert_eq!(
            child.config().code_memory_limit_bytes,
            vo_jit::DEFAULT_JIT_CODE_MEMORY_LIMIT_BYTES,
            "a related manager must report the shared family budget"
        );
        assert_eq!(
            child.config().analysis_memory_limit_bytes,
            vo_jit::MAX_JIT_ANALYSIS_BYTES,
            "a related manager must report the shared family analysis budget"
        );
        child.init_verified(&loaded).expect("bind child module");

        drop(loaded);
        drop(owner);
        assert!(weak.upgrade().is_some());
        drop(child);
        drop(shared_code);
        assert!(weak.upgrade().is_none());
    }

    #[test]
    fn shared_jit_rejects_equal_but_distinct_module_images() {
        let make_module = || {
            let mut module = VoModule::new("shared-jit-identity".to_string());
            module.functions.push(valid_jit_func(
                "same",
                vec![Instruction::new(Opcode::Return, 0, 0, 0)],
            ));
            Arc::new(vo_common_core::verifier::verify_loaded_module(module).expect("valid module"))
        };
        let first = make_module();
        let second = make_module();
        let second_weak = Arc::downgrade(&second);

        let mut owner = JitManager::new().expect("owner manager");
        owner.init_verified(&first).expect("bind first image");
        let mut peer = JitManager::with_shared_code(JitConfig::default(), owner.shared_code());
        assert!(matches!(
            peer.init_verified(&second),
            Err(JitError::ModuleScopeChanged)
        ));

        drop(second);
        assert!(second_weak.upgrade().is_none());
    }
}
