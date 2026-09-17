//! Compiler-independent native ABI policy, image records, and telemetry.
use crate::JitArtifactMetadata;
use std::sync::Arc;
use vo_runtime::bytecode::ResolvedExternTable;

/// Default persistent native-code budget for one JIT module / Island family.
pub const DEFAULT_JIT_CODE_MEMORY_LIMIT_BYTES: usize = 64 * 1024 * 1024;
/// Maximum explicit native stack storage reserved by one compiled artifact.
///
/// Large generated UI render functions can require more than 256 KiB while
/// remaining well within the compiler and code-memory budgets. Keep a strict
/// ceiling with enough headroom for production application shells.
pub const MAX_JIT_NATIVE_FRAME_BYTES: usize = 384 * 1024;
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
    /// Number of retained per-function analyses. Module-wide facts are shared
    /// and reflected in `retained_bytes`.
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

/// Runtime metadata and stable linker symbol for one compiled Vo function.
#[derive(Debug, Clone)]
pub struct NativeAotFunction {
    pub func_id: u32,
    pub symbol: String,
    pub metadata: Arc<JitArtifactMetadata>,
    pub entry_eligibility: JitFrameEntryEligibility,
    pub continuation: Option<NativeAotContinuation>,
}

/// Static re-entry from canonical VM slots. Lane zero carries the bytecode PC;
/// argument lanes are unused because execution has already initialized locals.
#[derive(Debug, Clone)]
pub struct NativeAotContinuation {
    pub symbol: String,
    pub pcs: Arc<[u32]>,
    pub metadata: Arc<JitArtifactMetadata>,
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
    /// Entry from a dynamic closure/interface callback may use a prepared
    /// shadow frame.
    pub prepared_shadow: bool,
    /// A statically resolved native caller may use a prepared shadow frame.
    /// Module analysis proves this over the complete static callee closure.
    pub static_prepared_shadow: bool,
    /// Native callers must publish precise roots while this callee runs.
    pub may_gc: bool,
}

pub fn jit_frame_entry_eligibility(
    func: &vo_runtime::bytecode::FunctionDef,
) -> JitFrameEntryEligibility {
    let contract = crate::entry_contract::function_contract(func);
    jit_frame_entry_eligibility_for_contract(func, contract)
}

pub(crate) fn jit_frame_entry_eligibility_for_contract(
    func: &vo_runtime::bytecode::FunctionDef,
    contract: vo_common_core::execution_effects::EffectContract,
) -> JitFrameEntryEligibility {
    let has_direct_returns = func.heap_ret_gcref_count == 0;
    JitFrameEntryEligibility {
        frame_elided: has_direct_returns && contract.permits_frame_elision(),
        prepared_shadow: has_direct_returns && contract.permits_prepared_shadow_frame(),
        static_prepared_shadow: has_direct_returns && contract.permits_prepared_shadow_frame(),
        may_gc: contract.may_gc,
    }
}

#[cfg(all(test, feature = "compiler"))]
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
