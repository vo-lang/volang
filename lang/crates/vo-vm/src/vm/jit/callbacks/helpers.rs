//! JIT callback helper functions.

use vo_runtime::jit_api::{JitContext, JitResult, JitRuntimeTrapKind};
use vo_runtime::objects::interface::InterfaceSlot;

use crate::fiber::Fiber;
use crate::vm::{RuntimeTrapKind, Vm};

pub struct JitCallbackBorrow<'a> {
    pub vm: &'a mut Vm,
    pub fiber: &'a mut Fiber,
}

/// Decode the VM/Fiber borrow carried by a JIT callback context.
///
/// # Safety
///
/// `ctx` must be a non-null pointer created by `build_jit_context` for the
/// currently executing fiber. Its `vm` and `fiber` fields must still point to
/// the same live VM and fiber, and the callback must not retain the returned
/// references after it returns to JIT code.
#[inline]
pub unsafe fn borrow_context<'a>(ctx: *mut JitContext) -> JitCallbackBorrow<'a> {
    let ctx = &mut *ctx;
    let vm = &mut *(ctx.vm as *mut Vm);
    let fiber = &mut *(ctx.fiber as *mut Fiber);
    JitCallbackBorrow { vm, fiber }
}

/// Compatibility helper for callbacks that only need VM/Fiber.
#[inline]
pub unsafe fn extract_context<'a>(ctx: *mut JitContext) -> (&'a mut Vm, &'a mut Fiber) {
    let borrow = borrow_context(ctx);
    (borrow.vm, borrow.fiber)
}

/// Helper: set panic message on fiber and return JitResult::Panic.
pub fn set_jit_panic(gc: &mut vo_runtime::gc::Gc, fiber: &mut Fiber, msg: &str) -> JitResult {
    let panic_str = vo_runtime::objects::string::new_from_string(gc, msg.to_string());
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    fiber.set_recoverable_panic(InterfaceSlot::new(slot0, panic_str as u64));
    JitResult::Panic
}

pub fn set_jit_trap(
    gc: &mut vo_runtime::gc::Gc,
    fiber: &mut Fiber,
    kind: RuntimeTrapKind,
    msg: &str,
) -> JitResult {
    let panic_str = vo_runtime::objects::string::new_from_string(gc, msg.to_string());
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    fiber.set_recoverable_trap(kind, InterfaceSlot::new(slot0, panic_str as u64));
    JitResult::Panic
}

pub fn record_runtime_trap(ctx: &mut JitContext, kind: JitRuntimeTrapKind, pc: u32) {
    unsafe {
        *ctx.panic_flag = true;
        *ctx.is_user_panic = false;
    }
    ctx.runtime_trap_kind = kind as u8;
    ctx.runtime_trap_arg0 = 0;
    ctx.runtime_trap_arg1 = 0;
    ctx.runtime_trap_pc = pc;
}

pub extern "C" fn jit_stack_overflow(ctx: *mut JitContext) -> JitResult {
    let (vm, fiber) = unsafe { extract_context(ctx) };
    set_jit_trap(
        &mut vm.state.gc,
        fiber,
        RuntimeTrapKind::StackOverflow,
        "runtime error: stack overflow",
    )
}
