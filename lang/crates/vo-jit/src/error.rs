//! Native execution and optional compilation errors.
use crate::{loop_analysis, verifier::JitMetadataError};
use vo_runtime::instruction::Opcode;

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
    /// This build contains native execution only.
    CompilerUnavailable,
    #[cfg(feature = "compiler")]
    Module(cranelift_module::ModuleError),
    #[cfg(feature = "compiler")]
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
    CompilerPoisoned(String),
    Internal(String),
}

impl JitError {
    pub const fn failure_kind(&self) -> JitFailureKind {
        match self {
            Self::CompilerUnavailable
            | Self::UnsupportedOpcode(_)
            | Self::MissingJitLayout { .. } => JitFailureKind::SemanticUnsupported,
            Self::CodeMemoryLimitExceeded { .. }
            | Self::NativeFrameLimitExceeded { .. }
            | Self::AnalysisResourceLimitExceeded { .. }
            | Self::MetadataResourceLimitExceeded { .. }
            | Self::CompileWorkLimitExceeded { .. }
            | Self::CodeMemoryReservationFailed { .. } => JitFailureKind::ResourceRejected,
            #[cfg(feature = "compiler")]
            Self::Module(cranelift_module::ModuleError::Allocation { .. }) => {
                JitFailureKind::ResourceRejected
            }
            #[cfg(feature = "compiler")]
            Self::Module(_) | Self::Codegen(_) => JitFailureKind::CompilerFault,
            Self::FunctionNotFound(_)
            | Self::InvalidOsrTarget(_)
            | Self::ModuleScopeChanged
            | Self::CompileEnvScopeChanged
            | Self::FunctionScopeChanged
            | Self::LoopScopeChanged
            | Self::InvalidMetadata(_)
            | Self::LoopAnalysis(_)
            | Self::CompilerPoisoned(_)
            | Self::Internal(_) => JitFailureKind::CompilerFault,
        }
    }
}

impl std::fmt::Display for JitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            #[cfg(feature = "compiler")]
            JitError::Module(e) => write!(f, "Cranelift module error: {}", e),
            #[cfg(feature = "compiler")]
            JitError::Codegen(e) => write!(f, "Cranelift codegen error: {}", e),
            JitError::CompilerUnavailable => write!(f, "runtime compilation is unavailable in this build"),
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
            JitError::CompilerPoisoned(message) => {
                write!(f, "JIT compiler cannot publish more artifacts: {message}")
            }
            JitError::Internal(msg) => write!(f, "internal error: {}", msg),
        }
    }
}

impl std::error::Error for JitError {}

#[cfg(feature = "compiler")]
impl From<cranelift_module::ModuleError> for JitError {
    fn from(e: cranelift_module::ModuleError) -> Self {
        JitError::Module(e)
    }
}

#[cfg(feature = "compiler")]
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
