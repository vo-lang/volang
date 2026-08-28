use cranelift_codegen::ir::{types, AbiParam, Signature};
use cranelift_codegen::isa::CallConv;

use vo_runtime::jit_api::{JitContext, JitResult};

/// Fixed lane count in the versioned native ABI.
///
/// Keeping this independent of the compiler host makes object generation
/// deterministic across supported native targets. Platform ABIs may place
/// overflow lanes on the native stack; the logical signature stays stable.
/// Float slots cross the boundary with an explicit bitcast, so one uniform
/// dispatch table can serve every verified Vo function shape.
pub const NATIVE_ARG_LANES: usize = 5;

/// Unified VM/JIT native entry. `frame_bp` identifies the callee's verified
/// fiber-stack window by stable slot index; generated code reconstructs a raw
/// pointer only when it actually accesses frame memory. Static calls
/// additionally pass the leading argument words in lanes so the callee can
/// enter SSA without reloading them.
pub type NativeJitFunc = extern "C" fn(
    ctx: *mut JitContext,
    frame_bp: u64,
    ret_ptr: *mut u64,
    lane0: u64,
    lane1: u64,
    lane2: u64,
    lane3: u64,
    lane4: u64,
) -> JitResult;

pub type JitFunc = NativeJitFunc;

pub(crate) fn native_signature(
    call_conv: CallConv,
    pointer_type: cranelift_codegen::ir::Type,
) -> Signature {
    let mut signature = Signature::new(call_conv);
    signature.params.push(AbiParam::new(pointer_type));
    signature.params.push(AbiParam::new(types::I64));
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
/// least `param_slots` initialized words, `ctx` must be valid, and `entry` must
/// carry [`NativeJitFunc`]'s ABI.
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

    // The VM owns the raw pointer at this boundary. Native callees receive the
    // canonical slot index, which remains valid if a callback relocates the
    // fiber stack.
    let frame_bp = u64::from(unsafe { (*ctx).jit_bp });

    entry(
        ctx,
        frame_bp,
        ret_ptr,
        unsafe { lane(frame_ptr, param_slots, 0) },
        unsafe { lane(frame_ptr, param_slots, 1) },
        unsafe { lane(frame_ptr, param_slots, 2) },
        unsafe { lane(frame_ptr, param_slots, 3) },
        unsafe { lane(frame_ptr, param_slots, 4) },
    )
}
