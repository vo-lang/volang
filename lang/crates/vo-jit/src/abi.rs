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

/// Unified VM/JIT native entry. `frame_ptr` always identifies the callee's
/// verified fiber-stack window. Static calls additionally pass the leading
/// argument words in lanes so the callee can enter SSA without reloading them.
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

pub type JitFunc = NativeJitFunc;

pub(crate) fn native_signature(
    call_conv: CallConv,
    pointer_type: cranelift_codegen::ir::Type,
) -> Signature {
    let mut signature = Signature::new(call_conv);
    signature.params.push(AbiParam::new(pointer_type));
    signature.params.push(AbiParam::new(pointer_type));
    signature.params.push(AbiParam::new(pointer_type));
    for _ in 0..NATIVE_ARG_LANES {
        signature.params.push(AbiParam::new(types::I64));
    }
    signature.returns.push(AbiParam::new(types::I32));
    signature
}

/// Invoke the unified native ABI from a verified VM frame window.
///
/// # Safety
/// `frame_ptr` must identify the active fiber stack at `ctx.jit_bp`, address at
/// least `param_slots` initialized words, and `entry` must carry
/// [`NativeJitFunc`]'s ABI.
#[inline]
pub unsafe fn invoke_native_from_frame(
    entry: NativeJitFunc,
    ctx: *mut JitContext,
    frame_ptr: *mut u64,
    ret_ptr: *mut u64,
    param_slots: usize,
) -> JitResult {
    #[inline]
    unsafe fn lane(frame_ptr: *mut u64, param_slots: usize, index: usize) -> u64 {
        if index < param_slots {
            unsafe { *frame_ptr.add(index) }
        } else {
            0
        }
    }

    #[cfg(target_arch = "aarch64")]
    return entry(
        ctx,
        frame_ptr,
        ret_ptr,
        unsafe { lane(frame_ptr, param_slots, 0) },
        unsafe { lane(frame_ptr, param_slots, 1) },
        unsafe { lane(frame_ptr, param_slots, 2) },
        unsafe { lane(frame_ptr, param_slots, 3) },
        unsafe { lane(frame_ptr, param_slots, 4) },
    );

    #[cfg(all(target_arch = "x86_64", not(target_os = "windows")))]
    return entry(
        ctx,
        frame_ptr,
        ret_ptr,
        unsafe { lane(frame_ptr, param_slots, 0) },
        unsafe { lane(frame_ptr, param_slots, 1) },
        unsafe { lane(frame_ptr, param_slots, 2) },
    );

    #[cfg(any(
        all(target_arch = "x86_64", target_os = "windows"),
        not(any(target_arch = "aarch64", target_arch = "x86_64"))
    ))]
    return entry(ctx, frame_ptr, ret_ptr, unsafe {
        lane(frame_ptr, param_slots, 0)
    });
}
