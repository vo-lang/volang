//! JIT callback helper functions.

use vo_runtime::jit_api::JitResult;
use vo_runtime::objects::interface::InterfaceSlot;

use crate::fiber::Fiber;

/// Helper: set panic message on fiber and return JitResult::Panic.
pub fn set_jit_panic(gc: &mut vo_runtime::gc::Gc, fiber: &mut Fiber, msg: &str) -> JitResult {
    let panic_str = vo_runtime::objects::string::new_from_string(gc, msg.to_string());
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    fiber.set_recoverable_panic(InterfaceSlot::new(slot0, panic_str as u64));
    JitResult::Panic
}
