#![allow(clippy::result_large_err)]
//! JIT compiler for Vo bytecode using Cranelift.

mod analysis;
mod call_helpers;
mod capability;
mod compile_common;
mod contract;
mod effects;
mod func_compiler;
mod helpers;
mod intrinsics;
pub mod loop_analysis;
mod loop_compiler;
mod metadata;
mod native_frame;
mod native_stack_map;
mod semantics;
#[cfg(test)]
mod test_fixtures;
mod translate;
mod translator;
mod verifier;

pub use loop_analysis::LoopInfo;
pub use loop_compiler::LoopFunc;
pub use native_stack_map::{JitArtifactMetadata, NativeRootKind, NativeStackMap, NativeStackRoot};

use func_compiler::FunctionCompiler;
use loop_compiler::{CompiledLoop, LoopCompiler};
use verifier::JitMetadataError;

use std::collections::HashMap;
use std::mem::ManuallyDrop;
use std::sync::Arc;

use cranelift_codegen::ir::{types, AbiParam, Signature};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::FunctionBuilderContext;
use cranelift_jit::{ArenaMemoryProvider, JITBuilder, JITModule};
use cranelift_module::{Module, ModuleReloc};

use vo_runtime::bytecode::{
    DynamicCallsiteMap, FunctionDef, LoadedModule, Module as VoModule, ResolvedExternTable,
};
use vo_runtime::instruction::Opcode;
use vo_runtime::jit_api::{JitContext, JitResult};

use helpers::{HelperFuncIds, HelperRefs};

#[cfg(test)]
fn test_frontend_config() -> cranelift_codegen::isa::TargetFrontendConfig {
    cranelift_native::builder()
        .expect("native ISA")
        .finish(settings::Flags::new(settings::builder()))
        .expect("native ISA flags")
        .frontend_config()
}

/// Default persistent native-code budget for one JIT module / Island family.
pub const DEFAULT_JIT_CODE_MEMORY_LIMIT_BYTES: usize = 64 * 1024 * 1024;
/// Maximum explicit native stack storage reserved by one compiled artifact.
pub const MAX_JIT_NATIVE_FRAME_BYTES: usize = 256 * 1024;
/// Default persistent budget for shared JIT analysis state.
pub const MAX_JIT_ANALYSIS_BYTES: usize = 64 * 1024 * 1024;
/// Maximum retained native stack-map and frame-state metadata per JIT family.
pub const MAX_JIT_METADATA_BYTES: usize = 16 * 1024 * 1024;
/// Maximum estimated transient compiler work owned by one artifact.
///
/// Cranelift IR is substantially wider than Vo bytecode. Bounding the input
/// shape before analysis and translation prevents one valid but adversarial
/// function from consuming an unbounded amount of host memory.
pub const MAX_JIT_COMPILE_WORK_BYTES: usize = 256 * 1024 * 1024;

#[inline]
fn function_needs_native_root_frame(func: &FunctionDef) -> bool {
    func.slot_types.iter().any(|slot| {
        matches!(
            slot,
            vo_runtime::SlotType::GcRef | vo_runtime::SlotType::Interface0
        )
    })
}

// =============================================================================
// JitError
// =============================================================================

/// Stable classification used by VM dispatch policy and observability.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JitFailureKind {
    /// The verified program uses a construct this JIT backend cannot compile.
    SemanticUnsupported,
    /// A configured JIT memory or compilation-work budget rejected the artifact.
    ResourceRejected,
    /// The request reached an unexpected compiler, metadata, or scope failure.
    CompilerFault,
}

#[derive(Debug)]
pub enum JitError {
    Module(cranelift_module::ModuleError),
    Codegen(cranelift_codegen::CodegenError),
    FunctionNotFound(u32),
    InvalidOsrTarget(usize),
    ModuleScopeChanged,
    CompileEnvScopeChanged,
    FunctionScopeChanged,
    LoopScopeChanged,
    UnsupportedOpcode(Opcode),
    InvalidMetadata(JitMetadataError),
    LoopAnalysis(loop_analysis::LoopAnalysisError),
    MissingJitLayout {
        pc: usize,
        opcode: Opcode,
        layout: &'static str,
    },
    CodeMemoryLimitExceeded {
        limit_bytes: usize,
        used_bytes: usize,
        requested_bytes: usize,
    },
    NativeFrameLimitExceeded {
        limit_bytes: usize,
        requested_bytes: usize,
    },
    AnalysisResourceLimitExceeded {
        limit_bytes: usize,
        requested_bytes: usize,
    },
    MetadataResourceLimitExceeded {
        limit_bytes: usize,
        used_bytes: usize,
        requested_bytes: usize,
    },
    CompileWorkLimitExceeded {
        limit_bytes: usize,
        requested_bytes: usize,
    },
    CodeMemoryReservationFailed {
        requested_bytes: usize,
        message: String,
    },
    Internal(String),
}

impl JitError {
    pub const fn failure_kind(&self) -> JitFailureKind {
        match self {
            Self::UnsupportedOpcode(_) | Self::MissingJitLayout { .. } => {
                JitFailureKind::SemanticUnsupported
            }
            Self::CodeMemoryLimitExceeded { .. }
            | Self::NativeFrameLimitExceeded { .. }
            | Self::AnalysisResourceLimitExceeded { .. }
            | Self::MetadataResourceLimitExceeded { .. }
            | Self::CompileWorkLimitExceeded { .. }
            | Self::CodeMemoryReservationFailed { .. }
            | Self::Module(cranelift_module::ModuleError::Allocation { .. }) => {
                JitFailureKind::ResourceRejected
            }
            Self::Module(_)
            | Self::Codegen(_)
            | Self::FunctionNotFound(_)
            | Self::InvalidOsrTarget(_)
            | Self::ModuleScopeChanged
            | Self::CompileEnvScopeChanged
            | Self::FunctionScopeChanged
            | Self::LoopScopeChanged
            | Self::InvalidMetadata(_)
            | Self::LoopAnalysis(_)
            | Self::Internal(_) => JitFailureKind::CompilerFault,
        }
    }
}

impl std::fmt::Display for JitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JitError::Module(e) => write!(f, "Cranelift module error: {}", e),
            JitError::Codegen(e) => write!(f, "Cranelift codegen error: {}", e),
            JitError::FunctionNotFound(id) => write!(f, "function not found: {}", id),
            JitError::InvalidOsrTarget(pc) => write!(f, "invalid OSR target PC: {}", pc),
            JitError::ModuleScopeChanged => write!(
                f,
                "JIT compiler is already bound to a different verified module"
            ),
            JitError::CompileEnvScopeChanged => write!(
                f,
                "JIT compiler is already bound to a different resolved extern/backend scope"
            ),
            JitError::FunctionScopeChanged => write!(
                f,
                "JIT compile request function does not match the verified module function"
            ),
            JitError::LoopScopeChanged => write!(
                f,
                "JIT compiler already cached a different loop scope for this function and pc"
            ),
            JitError::UnsupportedOpcode(op) => write!(f, "unsupported opcode: {:?}", op),
            JitError::InvalidMetadata(e) => write!(f, "invalid JIT metadata: {}", e),
            JitError::LoopAnalysis(e) => write!(f, "loop analysis failed: {}", e),
            JitError::MissingJitLayout { pc, opcode, layout } => {
                write!(f, "missing JIT {layout} layout for {opcode:?} at pc {pc}")
            }
            JitError::CodeMemoryLimitExceeded {
                limit_bytes,
                used_bytes,
                requested_bytes,
            } => write!(
                f,
                "JIT code memory limit exceeded: limit {limit_bytes} bytes, used {used_bytes} bytes, requested {requested_bytes} bytes"
            ),
            JitError::NativeFrameLimitExceeded {
                limit_bytes,
                requested_bytes,
            } => write!(
                f,
                "JIT native frame limit exceeded: limit {limit_bytes} bytes, requested {requested_bytes} bytes"
            ),
            JitError::AnalysisResourceLimitExceeded {
                limit_bytes,
                requested_bytes,
            } => write!(
                f,
                "JIT analysis resource limit exceeded: limit {limit_bytes} bytes, requested {requested_bytes} bytes"
            ),
            JitError::MetadataResourceLimitExceeded {
                limit_bytes,
                used_bytes,
                requested_bytes,
            } => write!(
                f,
                "JIT metadata resource limit exceeded: limit {limit_bytes} bytes, used {used_bytes} bytes, requested {requested_bytes} bytes"
            ),
            JitError::CompileWorkLimitExceeded {
                limit_bytes,
                requested_bytes,
            } => write!(
                f,
                "JIT compile work limit exceeded: limit {limit_bytes} bytes, requested {requested_bytes} bytes"
            ),
            JitError::CodeMemoryReservationFailed {
                requested_bytes,
                message,
            } => write!(
                f,
                "JIT native memory reservation of {requested_bytes} bytes failed: {message}"
            ),
            JitError::Internal(msg) => write!(f, "internal error: {}", msg),
        }
    }
}

impl std::error::Error for JitError {}

impl From<cranelift_module::ModuleError> for JitError {
    fn from(e: cranelift_module::ModuleError) -> Self {
        JitError::Module(e)
    }
}

impl From<cranelift_codegen::CodegenError> for JitError {
    fn from(e: cranelift_codegen::CodegenError) -> Self {
        JitError::Codegen(e)
    }
}

impl From<JitMetadataError> for JitError {
    fn from(e: JitMetadataError) -> Self {
        JitError::InvalidMetadata(e)
    }
}

impl From<loop_analysis::LoopAnalysisError> for JitError {
    fn from(e: loop_analysis::LoopAnalysisError) -> Self {
        JitError::LoopAnalysis(e)
    }
}

// =============================================================================
// CompiledFunction
// =============================================================================

pub struct CompiledFunction {
    code_ptr: *const u8,
    metadata: Arc<JitArtifactMetadata>,
}

unsafe impl Send for CompiledFunction {}
unsafe impl Sync for CompiledFunction {}

pub type JitFunc = extern "C" fn(ctx: *mut JitContext, args: *mut u64, ret: *mut u64) -> JitResult;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JitBackendCaps {
    pub extern_suspend: bool,
}

impl Default for JitBackendCaps {
    fn default() -> Self {
        Self {
            extern_suspend: true,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct JitCompileEnv<'a> {
    pub externs: &'a ResolvedExternTable,
    pub backend_caps: JitBackendCaps,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct JitCompileEnvScope {
    externs: ResolvedExternTable,
    backend_caps: JitBackendCaps,
}

impl JitCompileEnvScope {
    fn from_env(env: JitCompileEnv<'_>) -> Self {
        Self {
            externs: env.externs.clone(),
            backend_caps: env.backend_caps,
        }
    }

    fn matches(&self, env: JitCompileEnv<'_>) -> bool {
        self.backend_caps == env.backend_caps
            && (self.externs.shares_storage_with(env.externs) || self.externs == *env.externs)
    }
}

// =============================================================================
// JitCache
// =============================================================================

struct JitCache {
    functions: Vec<Option<CompiledFunction>>,
    loops: HashMap<(u32, usize), CompiledLoop>,
    analyses: Vec<Option<Arc<analysis::FunctionAnalysis>>>,
    analysis_access_ticks: Vec<u64>,
    analysis_tick: u64,
    analysis_eviction_count: usize,
    rejected_analysis_count: usize,
    rejected_functions: Vec<Option<CodeMemoryRejection>>,
    rejected_loops: HashMap<(u32, usize), (LoopInfo, CodeMemoryRejection)>,
    function_bytes: usize,
    loop_bytes: usize,
    function_committed_bytes: usize,
    loop_committed_bytes: usize,
    code_allocation_granularity_bytes: usize,
    analysis_bytes: usize,
    analysis_memory_limit_bytes: usize,
    metadata_bytes: usize,
    metadata_memory_limit_bytes: usize,
    code_memory_limit_bytes: usize,
}

impl JitCache {
    fn new(
        code_memory_limit_bytes: usize,
        analysis_memory_limit_bytes: usize,
        metadata_memory_limit_bytes: usize,
    ) -> Self {
        Self {
            functions: Vec::new(),
            loops: HashMap::new(),
            analyses: Vec::new(),
            analysis_access_ticks: Vec::new(),
            analysis_tick: 0,
            analysis_eviction_count: 0,
            rejected_analysis_count: 0,
            rejected_functions: Vec::new(),
            rejected_loops: HashMap::new(),
            function_bytes: 0,
            loop_bytes: 0,
            function_committed_bytes: 0,
            loop_committed_bytes: 0,
            code_allocation_granularity_bytes: region::page::size(),
            analysis_bytes: 0,
            analysis_memory_limit_bytes,
            metadata_bytes: 0,
            metadata_memory_limit_bytes,
            code_memory_limit_bytes,
        }
    }
    fn bind_function_count(&mut self, count: usize) {
        if self.functions.is_empty() {
            self.functions.resize_with(count, || None);
            self.analyses.resize_with(count, || None);
            self.analysis_access_ticks.resize(count, 0);
            self.rejected_functions.resize_with(count, || None);
        }
    }
    fn insert(&mut self, func_id: u32, func: CompiledFunction) {
        self.functions[func_id as usize] = Some(func);
    }
    fn contains(&self, func_id: u32) -> bool {
        self.functions
            .get(func_id as usize)
            .is_some_and(Option::is_some)
    }
    /// # Safety
    /// The returned function pointer must only be called with the correct ABI.
    /// `self` must remain alive, unmoved through destruction, and not be dropped
    /// until every invocation has returned and every copy of the pointer has
    /// been permanently retired.
    unsafe fn get_func_ptr(&self, func_id: u32) -> Option<JitFunc> {
        self.functions
            .get(func_id as usize)
            .and_then(Option::as_ref)
            .map(|f| std::mem::transmute(f.code_ptr))
    }
    fn get_loop(&self, func_id: u32, begin_pc: usize) -> Option<&CompiledLoop> {
        self.loops.get(&(func_id, begin_pc))
    }
    fn insert_loop(&mut self, func_id: u32, begin_pc: usize, compiled: CompiledLoop) {
        self.loops.insert((func_id, begin_pc), compiled);
    }
    fn get_or_analyze(
        &mut self,
        func_id: u32,
        func: &FunctionDef,
        vo_module: &VoModule,
        dynamic_callsites: Arc<DynamicCallsiteMap>,
    ) -> Result<Arc<analysis::FunctionAnalysis>, JitError> {
        self.analysis_tick = self.analysis_tick.saturating_add(1);
        if let Some(analysis) = self.analyses.get(func_id as usize).and_then(Option::as_ref) {
            self.analysis_access_ticks[func_id as usize] = self.analysis_tick;
            return Ok(Arc::clone(analysis));
        }
        let analysis = match analysis::FunctionAnalysis::for_function(
            func_id,
            func,
            vo_module,
            dynamic_callsites,
            self.analysis_memory_limit_bytes,
        ) {
            Ok(analysis) => Arc::new(analysis),
            Err(JitError::AnalysisResourceLimitExceeded {
                requested_bytes, ..
            }) => {
                self.rejected_analysis_count = self.rejected_analysis_count.saturating_add(1);
                return Err(JitError::AnalysisResourceLimitExceeded {
                    limit_bytes: self.analysis_memory_limit_bytes,
                    requested_bytes,
                });
            }
            Err(error) => return Err(error),
        };
        let retained_bytes = analysis.retained_bytes();
        while retained_bytes
            > self
                .analysis_memory_limit_bytes
                .saturating_sub(self.analysis_bytes)
        {
            let candidate = self
                .analyses
                .iter()
                .enumerate()
                .filter(|(index, entry)| {
                    *index != func_id as usize
                        && entry
                            .as_ref()
                            .is_some_and(|analysis| Arc::strong_count(analysis) == 1)
                })
                .min_by_key(|(index, _)| self.analysis_access_ticks[*index])
                .map(|(index, _)| index);
            let Some(candidate) = candidate else {
                self.rejected_analysis_count = self.rejected_analysis_count.saturating_add(1);
                return Err(JitError::AnalysisResourceLimitExceeded {
                    limit_bytes: self.analysis_memory_limit_bytes,
                    requested_bytes: self.analysis_bytes.saturating_add(retained_bytes),
                });
            };
            if let Some(evicted) = self.analyses[candidate].take() {
                self.analysis_bytes = self.analysis_bytes.saturating_sub(evicted.retained_bytes());
                self.analysis_access_ticks[candidate] = 0;
                self.analysis_eviction_count = self.analysis_eviction_count.saturating_add(1);
            }
        }
        self.analysis_bytes = self.analysis_bytes.saturating_add(retained_bytes);
        self.analysis_access_ticks[func_id as usize] = self.analysis_tick;
        self.analyses[func_id as usize] = Some(Arc::clone(&analysis));
        Ok(analysis)
    }
    fn committed_artifact_bytes(&self, emitted_bytes: usize) -> usize {
        if emitted_bytes == 0 {
            return 0;
        }
        let page = self.code_allocation_granularity_bytes.max(1);
        emitted_bytes
            .checked_add(page - 1)
            .map(|bytes| bytes / page * page)
            .unwrap_or(usize::MAX)
    }
    fn ensure_code_capacity(&self, requested_bytes: usize) -> Result<(), JitError> {
        let used_bytes = self
            .function_committed_bytes
            .saturating_add(self.loop_committed_bytes);
        if requested_bytes > self.code_memory_limit_bytes.saturating_sub(used_bytes) {
            return Err(JitError::CodeMemoryLimitExceeded {
                limit_bytes: self.code_memory_limit_bytes,
                used_bytes,
                requested_bytes,
            });
        }
        Ok(())
    }
    fn ensure_artifact_slot(&self) -> Result<(), JitError> {
        self.ensure_code_capacity(self.code_allocation_granularity_bytes)
    }
    fn ensure_metadata_capacity(&self, requested_bytes: usize) -> Result<(), JitError> {
        if requested_bytes
            > self
                .metadata_memory_limit_bytes
                .saturating_sub(self.metadata_bytes)
        {
            return Err(JitError::MetadataResourceLimitExceeded {
                limit_bytes: self.metadata_memory_limit_bytes,
                used_bytes: self.metadata_bytes,
                requested_bytes,
            });
        }
        Ok(())
    }
    fn record_function_allocation(
        &mut self,
        emitted_bytes: usize,
        committed_bytes: usize,
        metadata_bytes: usize,
    ) {
        self.function_bytes = self.function_bytes.saturating_add(emitted_bytes);
        self.function_committed_bytes = self
            .function_committed_bytes
            .saturating_add(committed_bytes);
        self.metadata_bytes = self.metadata_bytes.saturating_add(metadata_bytes);
    }
    fn record_loop_allocation(
        &mut self,
        emitted_bytes: usize,
        committed_bytes: usize,
        metadata_bytes: usize,
    ) {
        self.loop_bytes = self.loop_bytes.saturating_add(emitted_bytes);
        self.loop_committed_bytes = self.loop_committed_bytes.saturating_add(committed_bytes);
        self.metadata_bytes = self.metadata_bytes.saturating_add(metadata_bytes);
    }
    fn rejected_function(&self, func_id: u32) -> Option<JitError> {
        self.rejected_functions
            .get(func_id as usize)
            .and_then(Option::as_ref)
            .map(|rejection| rejection.to_error(self.code_memory_limit_bytes))
    }
    fn reject_function(&mut self, func_id: u32, rejection: CodeMemoryRejection) {
        let retained = &mut self.rejected_functions[func_id as usize];
        if retained.is_none() {
            *retained = Some(rejection);
        }
    }
    fn rejected_loop(
        &self,
        func_id: u32,
        loop_info: &LoopInfo,
    ) -> Result<Option<JitError>, JitError> {
        let Some((cached_info, rejection)) =
            self.rejected_loops.get(&(func_id, loop_info.begin_pc))
        else {
            return Ok(None);
        };
        if cached_info != loop_info {
            return Err(JitError::LoopScopeChanged);
        }
        Ok(Some(rejection.to_error(self.code_memory_limit_bytes)))
    }
    fn reject_loop(&mut self, func_id: u32, loop_info: &LoopInfo, rejection: CodeMemoryRejection) {
        self.rejected_loops
            .entry((func_id, loop_info.begin_pc))
            .or_insert_with(|| (loop_info.clone(), rejection));
    }
    fn code_memory_stats(&self) -> JitCodeMemoryStats {
        JitCodeMemoryStats {
            function_count: self.functions.iter().flatten().count(),
            loop_count: self.loops.len(),
            function_bytes: self.function_bytes,
            loop_bytes: self.loop_bytes,
            function_committed_bytes: self.function_committed_bytes,
            loop_committed_bytes: self.loop_committed_bytes,
            allocation_granularity_bytes: self.code_allocation_granularity_bytes,
            limit_bytes: self.code_memory_limit_bytes,
            rejected_artifact_count: self
                .rejected_functions
                .iter()
                .flatten()
                .count()
                .saturating_add(self.rejected_loops.len()),
        }
    }
    fn analysis_memory_stats(&self) -> JitAnalysisMemoryStats {
        JitAnalysisMemoryStats {
            analysis_count: self.analyses.iter().flatten().count(),
            retained_bytes: self.analysis_bytes,
            limit_bytes: self.analysis_memory_limit_bytes,
            rejected_analysis_count: self.rejected_analysis_count,
            eviction_count: self.analysis_eviction_count,
        }
    }
    fn metadata_memory_stats(&self) -> JitMetadataMemoryStats {
        JitMetadataMemoryStats {
            retained_bytes: self.metadata_bytes,
            limit_bytes: self.metadata_memory_limit_bytes,
        }
    }
    /// # Safety
    /// The returned function pointer must only be called with the correct ABI.
    /// `self` must outlive every use and every copy of the returned pointer.
    unsafe fn get_loop_func_ptr(&self, func_id: u32, begin_pc: usize) -> Option<LoopFunc> {
        self.loops
            .get(&(func_id, begin_pc))
            .map(|l| std::mem::transmute(l.code_ptr))
    }
    fn get_function_metadata(&self, func_id: u32) -> Option<&JitArtifactMetadata> {
        self.functions
            .get(func_id as usize)
            .and_then(Option::as_ref)
            .map(|function| function.metadata.as_ref())
    }
    fn get_loop_metadata(&self, func_id: u32, begin_pc: usize) -> Option<&JitArtifactMetadata> {
        self.loops
            .get(&(func_id, begin_pc))
            .map(|compiled| compiled.metadata.as_ref())
    }
}

#[derive(Debug, Clone, Copy)]
struct CodeMemoryRejection {
    used_bytes: usize,
    requested_bytes: usize,
}

impl CodeMemoryRejection {
    fn from_error(error: &JitError) -> Option<Self> {
        let JitError::CodeMemoryLimitExceeded {
            used_bytes,
            requested_bytes,
            ..
        } = error
        else {
            return None;
        };
        Some(Self {
            used_bytes: *used_bytes,
            requested_bytes: *requested_bytes,
        })
    }

    fn to_error(self, limit_bytes: usize) -> JitError {
        JitError::CodeMemoryLimitExceeded {
            limit_bytes,
            used_bytes: self.used_bytes,
            requested_bytes: self.requested_bytes,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct JitCodeMemoryStats {
    pub function_count: usize,
    pub loop_count: usize,
    pub function_bytes: usize,
    pub loop_bytes: usize,
    pub function_committed_bytes: usize,
    pub loop_committed_bytes: usize,
    pub allocation_granularity_bytes: usize,
    pub limit_bytes: usize,
    pub rejected_artifact_count: usize,
}

impl JitCodeMemoryStats {
    pub fn total_emitted_bytes(self) -> usize {
        self.function_bytes.saturating_add(self.loop_bytes)
    }

    /// Native pages charged to the hard code-memory budget.
    pub fn total_bytes(self) -> usize {
        self.function_committed_bytes
            .saturating_add(self.loop_committed_bytes)
    }

    pub fn remaining_bytes(self) -> usize {
        self.limit_bytes.saturating_sub(self.total_bytes())
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct JitAnalysisMemoryStats {
    pub analysis_count: usize,
    pub retained_bytes: usize,
    pub limit_bytes: usize,
    pub rejected_analysis_count: usize,
    pub eviction_count: usize,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct JitMetadataMemoryStats {
    pub retained_bytes: usize,
    pub limit_bytes: usize,
}

impl JitMetadataMemoryStats {
    pub fn remaining_bytes(self) -> usize {
        self.limit_bytes.saturating_sub(self.retained_bytes)
    }
}

impl JitAnalysisMemoryStats {
    pub fn remaining_bytes(self) -> usize {
        self.limit_bytes.saturating_sub(self.retained_bytes)
    }
}

#[derive(Debug, Clone, Copy)]
enum JitArtifactKind {
    Function,
    Loop,
}

// =============================================================================
// JitCompiler
// =============================================================================

pub struct JitCompiler {
    module: ManuallyDrop<JITModule>,
    ctx: cranelift_codegen::Context,
    func_ctx: FunctionBuilderContext,
    cache: JitCache,
    helper_funcs: HelperFuncIds,
    #[cfg(test)]
    verified_module_identity: Option<*const VoModule>,
    loaded_module: Option<Arc<LoadedModule>>,
    verified_env: Option<JitCompileEnvScope>,
    dynamic_callsites: Option<Arc<DynamicCallsiteMap>>,
    entry_eligibility: Option<Arc<[JitFrameEntryEligibility]>>,
    debug_ir: bool,
}

impl JitCompiler {
    pub fn new() -> Result<Self, JitError> {
        Self::with_debug(false)
    }

    pub fn with_debug(debug_ir: bool) -> Result<Self, JitError> {
        Self::with_code_memory_limit(debug_ir, DEFAULT_JIT_CODE_MEMORY_LIMIT_BYTES)
    }

    pub fn with_code_memory_limit(
        debug_ir: bool,
        code_memory_limit_bytes: usize,
    ) -> Result<Self, JitError> {
        Self::with_resource_limits(debug_ir, code_memory_limit_bytes, MAX_JIT_ANALYSIS_BYTES)
    }

    pub fn with_resource_limits(
        debug_ir: bool,
        code_memory_limit_bytes: usize,
        analysis_memory_limit_bytes: usize,
    ) -> Result<Self, JitError> {
        Self::with_all_resource_limits(
            debug_ir,
            code_memory_limit_bytes,
            analysis_memory_limit_bytes,
            MAX_JIT_METADATA_BYTES,
        )
    }

    pub fn with_all_resource_limits(
        debug_ir: bool,
        code_memory_limit_bytes: usize,
        analysis_memory_limit_bytes: usize,
        metadata_memory_limit_bytes: usize,
    ) -> Result<Self, JitError> {
        let mut flag_builder = settings::builder();
        flag_builder
            .set("opt_level", "speed")
            .map_err(|e| JitError::Internal(e.to_string()))?;

        let isa_builder =
            cranelift_native::builder().map_err(|e| JitError::Internal(e.to_string()))?;
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .map_err(|e| JitError::Internal(e.to_string()))?;

        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        let arena_bytes = code_memory_limit_bytes.max(region::page::size());
        let arena = ArenaMemoryProvider::new_with_size(arena_bytes).map_err(|error| {
            JitError::CodeMemoryReservationFailed {
                requested_bytes: arena_bytes,
                message: error.to_string(),
            }
        })?;
        builder.memory_provider(Box::new(arena));

        // Register runtime helper symbols
        helpers::register_symbols(&mut builder);

        let mut module = JITModule::new(builder);
        let ctx = module.make_context();
        let ptr_type = module.target_config().pointer_type();
        let helper_funcs = helpers::declare_helpers(&mut module, ptr_type)?;

        Ok(Self {
            module: ManuallyDrop::new(module),
            ctx,
            func_ctx: FunctionBuilderContext::new(),
            cache: JitCache::new(
                code_memory_limit_bytes,
                analysis_memory_limit_bytes,
                metadata_memory_limit_bytes,
            ),
            helper_funcs,
            #[cfg(test)]
            verified_module_identity: None,
            loaded_module: None,
            verified_env: None,
            dynamic_callsites: None,
            entry_eligibility: None,
            debug_ir,
        })
    }

    #[cfg(test)]
    fn verify_module_once(&mut self, vo_module: &VoModule) -> Result<(), JitError> {
        let identity = core::ptr::from_ref(vo_module);
        if let Some(verified_identity) = self.verified_module_identity {
            return if verified_identity == identity {
                Ok(())
            } else {
                Err(JitError::ModuleScopeChanged)
            };
        }
        verifier::verify_module(vo_module)?;
        self.verified_module_identity = Some(identity);
        self.cache.bind_function_count(vo_module.functions.len());
        self.dynamic_callsites = Some(Arc::new(DynamicCallsiteMap::for_module(vo_module)));
        Ok(())
    }

    /// Bind the compiler to the common-verifier-owned immutable image. The
    /// compiler retains the owner, so codegen never relies on a caller-managed
    /// raw lifetime contract.
    pub fn bind_loaded_module_scope(&mut self, loaded: Arc<LoadedModule>) -> Result<(), JitError> {
        let vo_module = loaded.module();
        if let Some(bound) = &self.loaded_module {
            if !Arc::ptr_eq(bound, &loaded) {
                return Err(JitError::ModuleScopeChanged);
            }
            return Ok(());
        }
        self.cache.bind_function_count(vo_module.functions.len());
        self.dynamic_callsites = Some(loaded.shared_dynamic_callsite_map());
        #[cfg(test)]
        {
            self.verified_module_identity = Some(core::ptr::from_ref(vo_module));
        }
        self.loaded_module = Some(loaded);
        Ok(())
    }

    fn verify_env_once(&mut self, env: JitCompileEnv<'_>) -> Result<(), JitError> {
        if let Some(verified) = &self.verified_env {
            if verified.matches(env) {
                return Ok(());
            }
            return Err(JitError::CompileEnvScopeChanged);
        }
        self.verified_env = Some(JitCompileEnvScope::from_env(env));
        Ok(())
    }

    fn entry_eligibility(
        &mut self,
        vo_module: &VoModule,
        env: JitCompileEnv<'_>,
    ) -> Arc<[JitFrameEntryEligibility]> {
        Arc::clone(self.entry_eligibility.get_or_insert_with(|| {
            Arc::from(crate::contract::module_frame_entry_eligibility(
                vo_module, env,
            ))
        }))
    }

    #[cfg(test)]
    fn verify_function_scope(
        &self,
        func_id: u32,
        func: &FunctionDef,
        vo_module: &VoModule,
    ) -> Result<(), JitError> {
        let Some(module_func) = vo_module.functions.get(func_id as usize) else {
            return Err(JitError::FunctionNotFound(func_id));
        };
        if !std::ptr::eq(module_func, func) && module_func != func {
            return Err(JitError::FunctionScopeChanged);
        }
        Ok(())
    }

    fn finalize_function(
        &mut self,
        func_id_cl: cranelift_module::FuncId,
        name: &str,
        artifact_kind: JitArtifactKind,
    ) -> Result<(*const u8, Arc<JitArtifactMetadata>), JitError> {
        let compile_result: Result<(usize, usize, Arc<JitArtifactMetadata>, usize), JitError> =
            (|| {
                cranelift_codegen::verifier::verify_function(
                    &self.ctx.func,
                    self.module.isa().flags(),
                )
                .map_err(|errors| {
                    JitError::Internal(format!(
                        "Cranelift IR verification failed for {name}: {errors}"
                    ))
                })?;
                if self.debug_ir {
                    eprintln!("[JIT VERIFY OK] {}", name);
                }

                self.ctx
                    .compile(self.module.isa(), &mut Default::default())
                    .map_err(cranelift_module::ModuleError::from)?;
                let compiled = self.ctx.compiled_code().ok_or_else(|| {
                    JitError::Internal(format!("missing compiled code for {name}"))
                })?;
                let code_size = compiled.code_info().total_size as usize;
                let committed_size = self.cache.committed_artifact_bytes(code_size);
                self.cache.ensure_code_capacity(committed_size)?;
                let native_stack_maps = compiled
                    .buffer
                    .user_stack_maps()
                    .iter()
                    .map(|(return_address, frame_size, map)| {
                        let source = compiled
                            .buffer
                            .get_srclocs_sorted()
                            .iter()
                            .find(|source| {
                                source.start < *return_address && *return_address <= source.end
                            })
                            .ok_or_else(|| {
                                JitError::Internal(format!(
                                    "native stack map for {name} has no safepoint source location"
                                ))
                            })?;
                        let safepoint_id = source.loc.bits().checked_sub(1).ok_or_else(|| {
                            JitError::Internal(format!(
                                "native stack map for {name} has an invalid safepoint source location"
                            ))
                        })?;
                        Ok((
                            safepoint_id,
                            *return_address,
                            *frame_size,
                            map.entries().collect::<Vec<_>>(),
                        ))
                    })
                    .collect::<Result<Vec<_>, JitError>>()?;
                let metadata = Arc::new(JitArtifactMetadata::from_entries(
                    code_size,
                    native_stack_maps,
                    name,
                )?);
                let metadata_bytes = metadata.retained_bytes();
                self.cache.ensure_metadata_capacity(metadata_bytes)?;
                let relocs = compiled
                    .buffer
                    .relocs()
                    .iter()
                    .map(|reloc| ModuleReloc::from_mach_reloc(reloc, &self.ctx.func, func_id_cl))
                    .collect::<Vec<_>>();
                self.module
                    .define_function_bytes(
                        func_id_cl,
                        u64::from(compiled.buffer.alignment),
                        compiled.code_buffer(),
                        &relocs,
                    )
                    .map_err(JitError::from)?;
                Ok((code_size, committed_size, metadata, metadata_bytes))
            })();
        self.module.clear_context(&mut self.ctx);
        let (code_size, committed_size, metadata, metadata_bytes) = compile_result?;

        match artifact_kind {
            JitArtifactKind::Function => {
                self.cache
                    .record_function_allocation(code_size, committed_size, metadata_bytes)
            }
            JitArtifactKind::Loop => {
                self.cache
                    .record_loop_allocation(code_size, committed_size, metadata_bytes)
            }
        }
        self.module.finalize_definitions()?;

        Ok((self.module.get_finalized_function(func_id_cl), metadata))
    }

    fn finish_translation(&mut self, result: Result<(), JitError>) -> Result<(), JitError> {
        if result.is_err() {
            // FunctionBuilderContext clears itself on finalize. A lowering
            // error exits before finalize, so discard that partial state.
            self.func_ctx = FunctionBuilderContext::new();
            self.module.clear_context(&mut self.ctx);
        }
        result
    }

    fn verify_native_frame_budget(&self) -> Result<(), JitError> {
        let requested_bytes = self
            .ctx
            .func
            .sized_stack_slots
            .values()
            .try_fold(0usize, |total, slot| total.checked_add(slot.size as usize))
            .unwrap_or(usize::MAX);
        if requested_bytes > MAX_JIT_NATIVE_FRAME_BYTES {
            return Err(JitError::NativeFrameLimitExceeded {
                limit_bytes: MAX_JIT_NATIVE_FRAME_BYTES,
                requested_bytes,
            });
        }
        Ok(())
    }

    fn verify_compile_work_budget(func: &FunctionDef) -> Result<(), JitError> {
        // The factors are conservative ownership estimates for Cranelift IR,
        // block/fact maps, and per-slot compiler state. All arithmetic saturates
        // so malformed or future wider inputs fail closed.
        let requested_bytes = func
            .code
            .len()
            .saturating_mul(512)
            .saturating_add(func.instruction_metadata.len().saturating_mul(32))
            .saturating_add(usize::from(func.local_slots).saturating_mul(64));
        if requested_bytes > MAX_JIT_COMPILE_WORK_BYTES {
            return Err(JitError::CompileWorkLimitExceeded {
                limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
                requested_bytes,
            });
        }
        Ok(())
    }

    /// Compile a function from the retained, commonly verified module image.
    pub fn compile_loaded(&mut self, func_id: u32, env: JitCompileEnv<'_>) -> Result<(), JitError> {
        let loaded = Arc::clone(
            self.loaded_module
                .as_ref()
                .ok_or_else(|| JitError::Internal("JIT compiler has no loaded module".into()))?,
        );
        let vo_module = loaded.module();
        let func = vo_module
            .functions
            .get(func_id as usize)
            .ok_or(JitError::FunctionNotFound(func_id))?;
        self.compile_bound(func_id, func, vo_module, env)
    }

    fn compile_bound(
        &mut self,
        func_id: u32,
        func: &FunctionDef,
        vo_module: &VoModule,
        env: JitCompileEnv<'_>,
    ) -> Result<(), JitError> {
        self.verify_env_once(env)?;
        let entry_eligibility = self.entry_eligibility(vo_module, env);

        if self.cache.contains(func_id) {
            return Ok(());
        }
        if let Some(error) = self.cache.rejected_function(func_id) {
            return Err(error);
        }
        if let Err(error) = self.cache.ensure_artifact_slot() {
            if let Some(rejection) = CodeMemoryRejection::from_error(&error) {
                self.cache.reject_function(func_id, rejection);
            }
            return Err(error);
        }
        Self::verify_compile_work_budget(func)?;
        let dynamic_callsites = self
            .dynamic_callsites
            .as_ref()
            .expect("verified module must carry dynamic callsite facts");
        let analysis =
            self.cache
                .get_or_analyze(func_id, func, vo_module, Arc::clone(dynamic_callsites))?;

        // Clear any residual state from previous compilation
        self.ctx.clear();

        let module = &self.module;
        let target_config = module.target_config();
        let ptr_type = target_config.pointer_type();
        let mut sig = Signature::new(target_config.default_call_conv);
        sig.params.push(AbiParam::new(ptr_type)); // ctx
        sig.params.push(AbiParam::new(ptr_type)); // args
        sig.params.push(AbiParam::new(ptr_type)); // ret
        sig.returns.push(AbiParam::new(types::I32));

        self.ctx.func.signature = sig;
        self.ctx.func.name = cranelift_codegen::ir::UserFuncName::user(0, func_id);

        let helpers = HelperRefs::new(&mut self.module, self.helper_funcs);
        let compile_result = {
            let compiler = FunctionCompiler::new(
                &mut self.ctx.func,
                &mut self.func_ctx,
                func_id,
                func,
                vo_module,
                env,
                &entry_eligibility,
                helpers,
                &analysis,
            );
            compiler.compile(target_config)
        };
        self.finish_translation(compile_result)?;
        if function_needs_native_root_frame(func) {
            let native_frame_result = native_frame::instrument_function(
                &mut self.ctx.func,
                ptr_type,
                func_id,
                vo_runtime::jit_api::JitNativeFrame::ARTIFACT_FUNCTION,
                u32::MAX,
                func.slot_types.contains(&vo_runtime::SlotType::Interface0),
            );
            self.finish_translation(native_frame_result)?;
        }
        let frame_budget_result = self.verify_native_frame_budget();
        self.finish_translation(frame_budget_result)?;

        let func_name = format!("vo_jit_{}", func_id);
        let func_id_cl = self.module.declare_function(
            &func_name,
            cranelift_module::Linkage::Local,
            &self.ctx.func.signature,
        )?;

        if self.debug_ir {
            eprintln!("=== JIT IR for func_{} {} ===", func_id, func.name);
            eprintln!("{}", self.ctx.func.display());
        }

        let finalize_result = self.finalize_function(
            func_id_cl,
            &format!("func_{} {}", func_id, func.name),
            JitArtifactKind::Function,
        );
        if let Err(error) = &finalize_result {
            if let Some(rejection) = CodeMemoryRejection::from_error(error) {
                self.cache.reject_function(func_id, rejection);
            }
        }
        let (code_ptr, metadata) = finalize_result?;
        let compiled = CompiledFunction { code_ptr, metadata };
        self.cache.insert(func_id, compiled);
        Ok(())
    }

    /// Unit-test adapter for malformed-module and scope-rejection coverage.
    #[cfg(test)]
    pub fn compile(
        &mut self,
        func_id: u32,
        func: &FunctionDef,
        vo_module: &VoModule,
        env: JitCompileEnv<'_>,
    ) -> Result<(), JitError> {
        self.verify_module_once(vo_module)?;
        self.verify_function_scope(func_id, func, vo_module)?;
        self.compile_bound(func_id, func, vo_module, env)
    }

    /// Compile an OSR loop from the retained, commonly verified module image.
    pub fn compile_loaded_loop(
        &mut self,
        func_id: u32,
        env: JitCompileEnv<'_>,
        loop_info: &LoopInfo,
    ) -> Result<(), JitError> {
        let loaded = Arc::clone(
            self.loaded_module
                .as_ref()
                .ok_or_else(|| JitError::Internal("JIT compiler has no loaded module".into()))?,
        );
        let vo_module = loaded.module();
        let func = vo_module
            .functions
            .get(func_id as usize)
            .ok_or(JitError::FunctionNotFound(func_id))?;
        self.compile_bound_loop(func_id, func, vo_module, env, loop_info)
    }

    /// Return the shared loop catalogue retained with the function analysis.
    pub fn analyzed_loaded_loops(&mut self, func_id: u32) -> Result<Arc<[LoopInfo]>, JitError> {
        let loaded = Arc::clone(
            self.loaded_module
                .as_ref()
                .ok_or_else(|| JitError::Internal("JIT compiler has no loaded module".into()))?,
        );
        let vo_module = loaded.module();
        let func = vo_module
            .functions
            .get(func_id as usize)
            .ok_or(JitError::FunctionNotFound(func_id))?;
        Self::verify_compile_work_budget(func)?;
        let dynamic_callsites = self
            .dynamic_callsites
            .as_ref()
            .expect("verified module must carry dynamic callsite facts");
        let analysis =
            self.cache
                .get_or_analyze(func_id, func, vo_module, Arc::clone(dynamic_callsites))?;
        Ok(analysis.shared_loops())
    }

    fn compile_bound_loop(
        &mut self,
        func_id: u32,
        func: &FunctionDef,
        vo_module: &VoModule,
        env: JitCompileEnv<'_>,
        loop_info: &LoopInfo,
    ) -> Result<(), JitError> {
        validate_loop_bounds(func, loop_info)?;
        let begin_pc = loop_info.begin_pc;
        self.verify_env_once(env)?;
        let entry_eligibility = self.entry_eligibility(vo_module, env);

        if let Some(cached_loop) = self.cache.get_loop(func_id, begin_pc) {
            if cached_loop.loop_info != *loop_info {
                return Err(JitError::LoopScopeChanged);
            }
            return Ok(());
        }
        if let Some(error) = self.cache.rejected_loop(func_id, loop_info)? {
            return Err(error);
        }
        if let Err(error) = self.cache.ensure_artifact_slot() {
            if let Some(rejection) = CodeMemoryRejection::from_error(&error) {
                self.cache.reject_loop(func_id, loop_info, rejection);
            }
            return Err(error);
        }
        Self::verify_compile_work_budget(func)?;
        let dynamic_callsites = self
            .dynamic_callsites
            .as_ref()
            .expect("verified module must carry dynamic callsite facts");
        let analysis =
            self.cache
                .get_or_analyze(func_id, func, vo_module, Arc::clone(dynamic_callsites))?;

        // Clear any residual state from previous compilation
        self.ctx.clear();

        let module = &self.module;
        let target_config = module.target_config();
        let ptr_type = target_config.pointer_type();
        let mut sig = Signature::new(target_config.default_call_conv);
        sig.params.push(AbiParam::new(ptr_type)); // ctx
        sig.params.push(AbiParam::new(ptr_type)); // locals_ptr
        sig.returns.push(AbiParam::new(types::I32));

        self.ctx.func.signature = sig;
        self.ctx.func.name = cranelift_codegen::ir::UserFuncName::user(
            func_id
                .checked_add(1)
                .expect("verified function id must leave namespace zero for full functions"),
            begin_pc as u32,
        );

        let helpers = HelperRefs::new(&mut self.module, self.helper_funcs);
        let compile_result = {
            let compiler = LoopCompiler::new(
                &mut self.ctx.func,
                &mut self.func_ctx,
                func_id,
                func,
                vo_module,
                env,
                &entry_eligibility,
                loop_info,
                helpers,
                &analysis,
            )?;
            compiler.compile(target_config)
        };
        self.finish_translation(compile_result)?;
        if function_needs_native_root_frame(func) {
            let native_frame_result = native_frame::instrument_function(
                &mut self.ctx.func,
                ptr_type,
                func_id,
                vo_runtime::jit_api::JitNativeFrame::ARTIFACT_OSR_LOOP,
                u32::try_from(begin_pc).unwrap_or(u32::MAX),
                func.slot_types.contains(&vo_runtime::SlotType::Interface0),
            );
            self.finish_translation(native_frame_result)?;
        }
        let frame_budget_result = self.verify_native_frame_budget();
        self.finish_translation(frame_budget_result)?;

        let func_name = format!("vo_loop_{}_{}", func_id, begin_pc);
        let func_id_cl = self.module.declare_function(
            &func_name,
            cranelift_module::Linkage::Local,
            &self.ctx.func.signature,
        )?;

        let finalize_result = self.finalize_function(
            func_id_cl,
            &format!("loop_{}_{}", func_id, begin_pc),
            JitArtifactKind::Loop,
        );
        if let Err(error) = &finalize_result {
            if let Some(rejection) = CodeMemoryRejection::from_error(error) {
                self.cache.reject_loop(func_id, loop_info, rejection);
            }
        }
        let (code_ptr, metadata) = finalize_result?;
        let compiled = CompiledLoop {
            code_ptr,
            loop_info: loop_info.clone(),
            metadata,
        };
        self.cache.insert_loop(func_id, begin_pc, compiled);
        Ok(())
    }

    /// Unit-test adapter for malformed-module and scope-rejection coverage.
    #[cfg(test)]
    pub fn compile_loop(
        &mut self,
        func_id: u32,
        func: &FunctionDef,
        vo_module: &VoModule,
        env: JitCompileEnv<'_>,
        loop_info: &LoopInfo,
    ) -> Result<(), JitError> {
        self.verify_module_once(vo_module)?;
        self.verify_function_scope(func_id, func, vo_module)?;
        self.compile_bound_loop(func_id, func, vo_module, env, loop_info)
    }

    pub fn code_memory_stats(&self) -> JitCodeMemoryStats {
        self.cache.code_memory_stats()
    }

    pub fn analysis_memory_stats(&self) -> JitAnalysisMemoryStats {
        self.cache.analysis_memory_stats()
    }
    pub fn metadata_memory_stats(&self) -> JitMetadataMemoryStats {
        self.cache.metadata_memory_stats()
    }
    pub fn function_metadata(&self, func_id: u32) -> Option<&JitArtifactMetadata> {
        self.cache.get_function_metadata(func_id)
    }
    pub fn function_metadata_handle(&self, func_id: u32) -> Option<Arc<JitArtifactMetadata>> {
        self.cache
            .functions
            .get(func_id as usize)
            .and_then(Option::as_ref)
            .map(|function| Arc::clone(&function.metadata))
    }
    pub fn loop_metadata(&self, func_id: u32, begin_pc: usize) -> Option<&JitArtifactMetadata> {
        self.cache.get_loop_metadata(func_id, begin_pc)
    }
    pub fn loop_metadata_handle(
        &self,
        func_id: u32,
        begin_pc: usize,
    ) -> Option<Arc<JitArtifactMetadata>> {
        self.cache
            .loops
            .get(&(func_id, begin_pc))
            .map(|loop_artifact| Arc::clone(&loop_artifact.metadata))
    }
    /// # Safety
    /// The returned function pointer must only be called with the correct ABI.
    pub unsafe fn get_func_ptr(&self, func_id: u32) -> Option<JitFunc> {
        self.cache.get_func_ptr(func_id)
    }
    /// # Safety
    /// The returned function pointer must only be called with the correct ABI.
    pub unsafe fn get_loop_func_ptr(&self, func_id: u32, begin_pc: usize) -> Option<LoopFunc> {
        self.cache.get_loop_func_ptr(func_id, begin_pc)
    }
}

impl Drop for JitCompiler {
    fn drop(&mut self) {
        // SAFETY: Drop runs exactly once and `module` is never taken elsewhere.
        let module = unsafe { ManuallyDrop::take(&mut self.module) };
        // SAFETY: JitCompiler owns every published code pointer. Safe VM owners
        // drop the compiler only after native execution has returned. Raw
        // getters carry an explicit unsafe lifetime obligation for other users.
        unsafe { module.free_memory() };
    }
}

fn validate_loop_bounds(func: &FunctionDef, loop_info: &LoopInfo) -> Result<(), JitError> {
    if loop_info.begin_pc > loop_info.end_pc || loop_info.end_pc >= func.code.len() {
        return Err(JitError::InvalidOsrTarget(loop_info.begin_pc));
    }
    Ok(())
}

/// Check if a function may use the native-stack direct JIT path that elides a
/// materialized VM frame.
///
/// This is stricter than "can be JIT-compiled". Managed allocation is admitted
/// because allocation helpers poll before consuming capacity and materialize
/// on pending collection. Panic/unwind, calls, scheduling, frame observation,
/// interfaces, write barriers, and closure materialization remain excluded.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JitFrameEntryEligibility {
    pub frame_elided: bool,
    pub prepared_shadow: bool,
    /// Native callers must publish precise roots while this callee runs.
    pub may_gc: bool,
}

pub fn jit_frame_entry_eligibility(
    func: &vo_runtime::bytecode::FunctionDef,
) -> JitFrameEntryEligibility {
    let contract = crate::contract::function_contract(func);
    jit_frame_entry_eligibility_for_contract(func, contract)
}

pub(crate) fn jit_frame_entry_eligibility_for_contract(
    func: &vo_runtime::bytecode::FunctionDef,
    contract: crate::contract::EffectContract,
) -> JitFrameEntryEligibility {
    let has_direct_returns = func.heap_ret_gcref_count == 0;
    JitFrameEntryEligibility {
        frame_elided: has_direct_returns && contract.permits_frame_elision(),
        prepared_shadow: has_direct_returns && contract.permits_prepared_shadow_frame(),
        may_gc: contract.may_gc,
    }
}

#[cfg(test)]
pub(crate) fn jit_frame_entry_eligibility_in_env(
    func: &vo_runtime::bytecode::FunctionDef,
    module: &vo_runtime::bytecode::Module,
    env: JitCompileEnv<'_>,
) -> JitFrameEntryEligibility {
    jit_frame_entry_eligibility_for_contract(
        func,
        crate::contract::function_contract_in_env(func, module, env),
    )
}

pub fn can_elide_frame_for_direct_jit(func: &vo_runtime::bytecode::FunctionDef) -> bool {
    jit_frame_entry_eligibility(func).frame_elided
}

/// Check if a prepared call's complete shadow stack window may enter JIT code.
///
/// A shadow window has precise slots and a current JIT function id, but it has
/// no `Fiber::CallFrame` until a non-OK result is materialized. It can therefore
/// run instructions that need spilling or trap handling. Allocation uses the
/// same pre-capacity poll. Unwind, scheduling, and instructions that own or
/// observe frame transitions remain excluded.
pub fn can_enter_prepared_shadow_frame_for_jit(func: &vo_runtime::bytecode::FunctionDef) -> bool {
    jit_frame_entry_eligibility(func).prepared_shadow
}

/// Check if a materialized VM frame may re-enter its compiled JIT body.
///
/// This is intentionally broader than frame elision: the frame already exists,
/// so callees that allocate or make nested calls can still execute as JIT.
/// Functions with defer/recover state stay in the interpreter because their
/// correctness depends on VM-visible defer ordering and recover eligibility.
pub fn can_enter_materialized_frame_for_jit(func: &vo_runtime::bytecode::FunctionDef) -> bool {
    !func.has_defer
}

#[cfg(test)]
mod tests;
