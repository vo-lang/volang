//! Trampolines for synchronous function execution.
//!
//! These are extern "C" entry points that allow:
//! - JIT code to call back into VM for non-jittable functions
//! - Extern functions to execute closures

use vo_runtime::gc::GcRef;
use vo_runtime::objects::closure;

use super::helpers::build_closure_args;
use super::Vm;


/// Trampoline for calling closures from extern functions.
/// This allows extern functions like dyn_call_closure to execute closures.
/// Uses a separate fiber because we're already inside run_fiber (would recurse).
pub extern "C" fn closure_call_trampoline(
    vm: *mut core::ffi::c_void,
    _caller_fiber: *mut core::ffi::c_void,
    closure_ref: u64,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> vo_runtime::ffi::ClosureCallResult {
    // In std mode, catch panics to prevent unwinding across FFI boundary
    #[cfg(feature = "std")]
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let vm = unsafe { &mut *(vm as *mut Vm) };
        let closure_gcref = closure_ref as GcRef;
        let func_id = closure::func_id(closure_gcref);
        
        let module = vm.module().expect("closure_call_trampoline: module not set");
        let func_def = &module.functions[func_id as usize];
        let full_args = build_closure_args(closure_ref, closure_gcref, func_def, args, arg_count);
        
        // Use execute_func_sync which creates a separate fiber
        // (we're already inside run_fiber, can't call it recursively)
        let (success, _panic_state) = vm.execute_func_sync(func_id, &full_args, ret, ret_count);
        success
    }));
    
    #[cfg(feature = "std")]
    return match result {
        Ok(true) => vo_runtime::ffi::ClosureCallResult::Ok,
        _ => vo_runtime::ffi::ClosureCallResult::Panic
    };
    
    // In no_std mode, no panic catching (panics will abort)
    #[cfg(not(feature = "std"))]
    {
        let vm = unsafe { &mut *(vm as *mut Vm) };
        let closure_gcref = closure_ref as GcRef;
        let func_id = closure::func_id(closure_gcref);
        
        let module = vm.module().expect("closure_call_trampoline: module not set");
        let func_def = &module.functions[func_id as usize];
        let full_args = build_closure_args(closure_ref, closure_gcref, func_def, args, arg_count);
        
        // Use execute_func_sync which creates a separate fiber
        let (success, _panic_state) = vm.execute_func_sync(func_id, &full_args, ret, ret_count);
        if success {
            vo_runtime::ffi::ClosureCallResult::Ok
        } else {
            vo_runtime::ffi::ClosureCallResult::Panic
        }
    }
}
