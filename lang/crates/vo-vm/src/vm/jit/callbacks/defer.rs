//! JIT callbacks for defer/recover operations.

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::jit_api::JitContext;
use vo_runtime::InterfaceSlot;
use vo_runtime::{ValueKind, ValueMeta};

use crate::fiber::{DeferArgLayout, DeferEntry, Fiber};

/// Push a defer entry from JIT code.
///
/// Called by JIT-compiled code when executing DeferPush or ErrDeferPush instructions.
/// The defer entry is stored in fiber.defer_stack with frame_depth = fiber.frames.len().
pub extern "C" fn jit_defer_push(
    ctx: *mut JitContext,
    func_id: u32,
    is_closure: u32,
    closure_ref: u64,
    arg_start: u32,
    args_ptr: *const u64,
    arg_count: u32,
    is_errdefer: u32,
) {
    let ctx_ref = unsafe { &*ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    let gc = unsafe { &mut *ctx_ref.gc };
    let module = unsafe { &*ctx_ref.module };

    let is_closure = is_closure != 0;
    let arg_slots = arg_count as u16;
    let frame_depth = fiber.frames.len();
    let generation = fiber.effective_defer_generation();
    let caller_frame = fiber
        .frames
        .last()
        .copied()
        .expect("jit_defer_push: missing caller frame");
    let caller_func = module
        .functions
        .get(caller_frame.func_id as usize)
        .expect("jit_defer_push: missing caller function metadata");
    let arg_layout = DeferArgLayout::try_from_caller_slot_types(
        &caller_func.slot_types,
        caller_frame.func_id,
        caller_frame.pc.saturating_sub(1) as u32,
        arg_start as u16,
        arg_slots,
    )
    .unwrap_or_else(|err| panic!("{err}"));

    // Match VM push_defer_entry semantics (exec/defer.rs)
    let (fid, closure): (u32, GcRef) = if is_closure {
        (0, closure_ref as GcRef)
    } else {
        (func_id, core::ptr::null_mut())
    };

    let args: GcRef = if arg_slots > 0 {
        let args_ref = gc.alloc(ValueMeta::new(0, ValueKind::Void), arg_slots);
        for i in 0..arg_slots {
            let val = unsafe { *args_ptr.add(i as usize) };
            unsafe { Gc::write_slot(args_ref, i as usize, val) };
        }
        args_ref
    } else {
        core::ptr::null_mut()
    };

    fiber.defer_stack.push(DeferEntry {
        frame_depth,
        func_id: fid,
        closure,
        args,
        arg_layout,
        is_closure,
        is_errdefer: is_errdefer != 0,
        registered_at_generation: generation,
    });
}

/// Execute recover() from JIT code.
///
/// Called by JIT-compiled code when executing Recover instruction.
/// Returns 1 if panic was recovered, 0 otherwise.
/// Result (interface{}) is written to result_ptr (2 slots).
pub extern "C" fn jit_recover(ctx: *mut JitContext, result_ptr: *mut u64) -> u32 {
    let ctx_ref = unsafe { &*ctx };
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };

    // Check if in valid recover context
    if !fiber.is_direct_defer_context() {
        // Not in direct defer context - return nil without consuming panic
        unsafe {
            *result_ptr = 0;
            *result_ptr.add(1) = 0;
        }
        return 0;
    }

    // Try to take the panic value
    let recovered = fiber.take_recoverable_panic();
    let val = recovered.unwrap_or(InterfaceSlot::nil());

    // Store result (interface{} = 2 slots)
    unsafe {
        *result_ptr = val.slot0;
        *result_ptr.add(1) = val.slot1;
    }

    // If recover succeeded, switch from Panic to Return mode
    if recovered.is_some() {
        fiber.switch_panic_to_return_mode();
        1
    } else {
        0
    }
}
