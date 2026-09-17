#![allow(clippy::result_large_err)]
//! JIT compiler for Vo bytecode using Cranelift.

mod abi;
#[cfg(feature = "compiler")]
mod analysis;
#[cfg(feature = "compiler")]
mod aot;
mod aot_format;
#[cfg(feature = "compiler")]
mod artifact;
#[cfg(feature = "compiler")]
mod call_graph;
#[cfg(feature = "compiler")]
mod call_helpers;
#[cfg(test)]
#[cfg(feature = "compiler")]
mod capability;
#[cfg(feature = "compiler")]
mod compile_common;
#[cfg(feature = "compiler")]
mod contract;
#[cfg(feature = "compiler")]
mod effects;
#[cfg(feature = "compiler")]
mod escape;
#[cfg(feature = "compiler")]
mod func_compiler;
#[cfg(feature = "compiler")]
mod helpers;
#[cfg(feature = "compiler")]
mod intrinsics;
#[cfg(feature = "compiler")]
mod ir;
#[cfg(feature = "compiler")]
mod ir_constants;
pub mod loop_analysis;
#[cfg(feature = "compiler")]
mod loop_compiler;
#[cfg(feature = "compiler")]
mod metadata;
#[cfg(feature = "compiler")]
mod native_frame;
mod native_stack_map;
#[cfg(feature = "compiler")]
mod optimizer;
#[cfg(test)]
#[cfg(feature = "compiler")]
mod semantics;
#[cfg(feature = "compiler")]
mod shape;
#[cfg(test)]
#[cfg(feature = "compiler")]
mod test_fixtures;
#[cfg(feature = "compiler")]
mod translate;
#[cfg(feature = "compiler")]
mod translator;
mod verifier;

pub use abi::LoopFunc;
pub use abi::{invoke_native_from_frame, JitFunc, NativeJitFunc, NATIVE_ARG_LANES};
#[cfg(feature = "compiler")]
pub use aot::{
    compile_native_object, NativeAotObject, NativeAotOptions, NATIVE_AOT_FUNCTION_COUNT_SYMBOL,
    NATIVE_AOT_FUNCTION_TABLE_SYMBOL, NATIVE_AOT_METADATA_BYTES_SYMBOL,
    NATIVE_AOT_METADATA_LEN_SYMBOL, NATIVE_AOT_MODULE_BYTES_SYMBOL, NATIVE_AOT_MODULE_LEN_SYMBOL,
    NATIVE_AOT_START_SYMBOL,
};
pub use aot_format::{
    decode_native_aot_metadata, encode_native_aot_metadata, NativeAotMetadata,
    NATIVE_AOT_ABI_VERSION,
};
pub use loop_analysis::LoopInfo;
pub use native_stack_map::{
    DeoptFrameState, DeoptValue, DeoptValueKind, DeoptValueLocation, JitArtifactMetadata,
    NativeRootKind, NativeStackMap, NativeStackRoot,
};
pub use vo_runtime::jit_api::JitTier;

mod entry_contract;
mod error;
mod runtime;
pub use error::*;
pub use runtime::*;
#[cfg(feature = "compiler")]
mod compiler;
#[cfg(feature = "compiler")]
pub use compiler::*;
