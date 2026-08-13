use cranelift_codegen::ir::{types, AbiParam, Signature};
use cranelift_codegen::isa::CallConv;

use vo_runtime::jit_api::{JitContext, JitResult};

/// Number of raw machine-word argument lanes carried by the internal native
/// entry. Wider signatures keep their remaining arguments in `frame_ptr`.
///
/// Lanes deliberately use the raw `u64` representation. Float slots cross the
/// boundary with an explicit bitcast, so one uniform dispatch table can serve
/// every verified Vo function shape.
#[cfg(target_arch = "aarch64")]
pub const NATIVE_ARG_LANES: usize = 5;
#[cfg(all(target_arch = "x86_64", not(target_os = "windows")))]
pub const NATIVE_ARG_LANES: usize = 3;
#[cfg(all(target_arch = "x86_64", target_os = "windows"))]
pub const NATIVE_ARG_LANES: usize = 1;
#[cfg(not(any(target_arch = "aarch64", target_arch = "x86_64")))]
pub const NATIVE_ARG_LANES: usize = 1;

/// VM-facing entry. It preserves the compact stable boundary used by the
/// interpreter and callbacks; the generated bridge thunk loads argument lanes
/// and enters the native body.
pub type JitFunc =
    extern "C" fn(ctx: *mut JitContext, frame_ptr: *mut u64, ret_ptr: *mut u64) -> JitResult;

/// JIT-facing entry. Static compiled calls pass their SSA arguments directly
/// through the raw-word lanes and retain `frame_ptr` for wide or materialized
/// frames.
#[cfg(target_arch = "aarch64")]
pub type NativeJitFunc = extern "C" fn(
    ctx: *mut JitContext,
    frame_ptr: *mut u64,
    ret_ptr: *mut u64,
    lane0: u64,
    lane1: u64,
    lane2: u64,
    lane3: u64,
    lane4: u64,
) -> JitResult;

#[cfg(all(target_arch = "x86_64", not(target_os = "windows")))]
pub type NativeJitFunc = extern "C" fn(
    ctx: *mut JitContext,
    frame_ptr: *mut u64,
    ret_ptr: *mut u64,
    lane0: u64,
    lane1: u64,
    lane2: u64,
) -> JitResult;

#[cfg(any(
    all(target_arch = "x86_64", target_os = "windows"),
    not(any(target_arch = "aarch64", target_arch = "x86_64"))
))]
pub type NativeJitFunc = extern "C" fn(
    ctx: *mut JitContext,
    frame_ptr: *mut u64,
    ret_ptr: *mut u64,
    lane0: u64,
) -> JitResult;

pub(crate) fn bridge_signature(
    call_conv: CallConv,
    pointer_type: cranelift_codegen::ir::Type,
) -> Signature {
    let mut signature = Signature::new(call_conv);
    signature.params.push(AbiParam::new(pointer_type));
    signature.params.push(AbiParam::new(pointer_type));
    signature.params.push(AbiParam::new(pointer_type));
    signature.returns.push(AbiParam::new(types::I32));
    signature
}

pub(crate) fn native_signature(
    call_conv: CallConv,
    pointer_type: cranelift_codegen::ir::Type,
) -> Signature {
    let mut signature = bridge_signature(call_conv, pointer_type);
    for _ in 0..NATIVE_ARG_LANES {
        signature.params.push(AbiParam::new(types::I64));
    }
    signature
}
