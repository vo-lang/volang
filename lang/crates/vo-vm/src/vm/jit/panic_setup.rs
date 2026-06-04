#[cfg(not(feature = "std"))]
use alloc::string::String;

use vo_runtime::bytecode::Module;
use vo_runtime::jit_api::JitRuntimeTrapKind;
use vo_runtime::objects::interface::InterfaceSlot;

use crate::fiber::{Fiber, FiberCapacityError};
use crate::vm::{helpers, RuntimeTrapKind};

use super::context::JitContextWrapper;
use super::materialize::{materialize_jit_frames, JitFrameMaterializeError};

pub(super) struct JitPanicInfo {
    pub(super) trap_kind: Option<RuntimeTrapKind>,
    pub(super) msg: InterfaceSlot,
}

pub(super) enum SetupJitPanicError {
    Capacity(FiberCapacityError),
    MaterializationInvariant(&'static str),
    MissingPayload,
    MissingLocation(&'static str),
}

fn runtime_trap_from_jit(kind: JitRuntimeTrapKind) -> Option<RuntimeTrapKind> {
    match kind {
        JitRuntimeTrapKind::None => None,
        JitRuntimeTrapKind::NilPointerDereference => Some(RuntimeTrapKind::NilPointerDereference),
        JitRuntimeTrapKind::NilMapWrite => Some(RuntimeTrapKind::NilMapWrite),
        JitRuntimeTrapKind::UnhashableType => Some(RuntimeTrapKind::UnhashableType),
        JitRuntimeTrapKind::UncomparableType => Some(RuntimeTrapKind::UncomparableType),
        JitRuntimeTrapKind::NegativeShift => Some(RuntimeTrapKind::NegativeShift),
        JitRuntimeTrapKind::NilFuncCall => Some(RuntimeTrapKind::NilFuncCall),
        JitRuntimeTrapKind::TypeAssertionFailed => Some(RuntimeTrapKind::TypeAssertionFailed),
        JitRuntimeTrapKind::DivisionByZero => Some(RuntimeTrapKind::DivisionByZero),
        JitRuntimeTrapKind::IndexOutOfBounds => Some(RuntimeTrapKind::IndexOutOfBounds),
        JitRuntimeTrapKind::SliceBoundsOutOfRange => Some(RuntimeTrapKind::SliceBoundsOutOfRange),
        JitRuntimeTrapKind::MakeSlice => Some(RuntimeTrapKind::MakeSlice),
        JitRuntimeTrapKind::MakeChan => Some(RuntimeTrapKind::MakeChan),
        JitRuntimeTrapKind::MakePort => Some(RuntimeTrapKind::MakePort),
        JitRuntimeTrapKind::SendOnClosedChannel => Some(RuntimeTrapKind::SendOnClosedChannel),
        JitRuntimeTrapKind::SendOnNilChannel => Some(RuntimeTrapKind::SendOnNilChannel),
        JitRuntimeTrapKind::RecvOnNilChannel => Some(RuntimeTrapKind::RecvOnNilChannel),
        JitRuntimeTrapKind::CloseNilChannel => Some(RuntimeTrapKind::CloseNilChannel),
        JitRuntimeTrapKind::CloseClosedChannel => Some(RuntimeTrapKind::CloseClosedChannel),
        JitRuntimeTrapKind::StackOverflow => Some(RuntimeTrapKind::StackOverflow),
    }
}

fn jit_runtime_trap_message(kind: RuntimeTrapKind, arg0: u64, arg1: u64) -> String {
    match kind {
        RuntimeTrapKind::IndexOutOfBounds => {
            format!(
                "runtime error: index out of range [{}] with length {}",
                arg0 as i64, arg1 as i64
            )
        }
        RuntimeTrapKind::SliceBoundsOutOfRange => {
            format!(
                "runtime error: slice bounds out of range [{}:{}]",
                arg0 as i64, arg1 as i64
            )
        }
        RuntimeTrapKind::MakeSlice => helpers::makeslice_error_message(arg0 as i32).to_string(),
        RuntimeTrapKind::MakeChan | RuntimeTrapKind::MakePort => {
            helpers::make_queue_error_message(kind).to_string()
        }
        _ => helpers::runtime_trap_message(kind).to_string(),
    }
}

fn interface_string(gc: &mut vo_runtime::gc::Gc, msg: String) -> InterfaceSlot {
    let msg_str = vo_runtime::objects::string::new_from_string(gc, msg);
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    InterfaceSlot::new(slot0, msg_str as u64)
}

/// Shared JIT panic setup: materialize frames, capture source location, resolve panic message.
///
/// For user panics the message comes from ctx (set by JIT extern callback); for runtime errors
/// or VM-side-exit panics it may already be in fiber.panic_state. Missing
/// payloads are treated as JIT bridge errors rather than guessed defaults.
///
/// Source location uses `runtime_trap_pc` for typed traps and `user_panic_pc`
/// for explicit or extern panics. `call_resume_pc` is reserved for
/// WaitIo/Replay/call materialization and is not a panic location substitute.
///
/// Returns the resolved panic message and optional runtime-trap kind; caller
/// must restore it on the fiber before invoking panic unwinding.
pub(super) fn setup_jit_panic(
    ctx: &JitContextWrapper,
    fiber: &mut Fiber,
    gc: &mut vo_runtime::gc::Gc,
    module: &Module,
) -> Result<JitPanicInfo, SetupJitPanicError> {
    if ctx.is_user_panic() {
        fiber.set_recoverable_panic(ctx.panic_msg());
    } else if let Some(jit_kind) = JitRuntimeTrapKind::from_u8(ctx.ctx.runtime_trap_kind) {
        if let Some(kind) = runtime_trap_from_jit(jit_kind) {
            let msg = jit_runtime_trap_message(
                kind,
                ctx.ctx.runtime_trap_arg0,
                ctx.ctx.runtime_trap_arg1,
            );
            fiber.set_recoverable_trap(kind, interface_string(gc, msg));
        }
    }
    let (trap_kind, panic_msg) = fiber
        .take_recoverable_panic_with_kind()
        .ok_or(SetupJitPanicError::MissingPayload)?;

    let trap_pc = (ctx.ctx.runtime_trap_pc != u32::MAX).then_some(ctx.ctx.runtime_trap_pc);
    let user_panic_pc = (ctx.ctx.user_panic_pc != u32::MAX).then_some(ctx.ctx.user_panic_pc);

    materialize_jit_frames(fiber, module, 0).map_err(|err| match err {
        JitFrameMaterializeError::Capacity(err) => SetupJitPanicError::Capacity(err),
        JitFrameMaterializeError::Invariant(err) => {
            SetupJitPanicError::MaterializationInvariant(err)
        }
    })?;

    if fiber.panic_source_loc.is_none() {
        let (pc, missing) = if trap_kind.is_some() {
            (trap_pc, "runtime_trap_pc")
        } else {
            (user_panic_pc, "user_panic_pc")
        };
        let pc = pc.ok_or(SetupJitPanicError::MissingLocation(missing))?;
        fiber.panic_source_loc = fiber.current_frame().map(|f| (f.func_id, pc));
    }

    Ok(JitPanicInfo {
        trap_kind,
        msg: panic_msg,
    })
}
