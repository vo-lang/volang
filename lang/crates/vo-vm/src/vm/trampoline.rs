//! Trampolines for synchronous function execution.
//!
//! These are extern "C" entry points that allow:
//! - JIT code to call back into VM for non-jittable functions
//! - Extern functions to execute closures

use vo_runtime::gc::GcRef;
use vo_runtime::objects::closure;

use super::helpers::build_closure_args;
use super::Vm;

/// Trampoline for JIT code to call VM-interpreted functions.
///
/// This is used when a JIT-compiled function calls another function
/// that isn't JIT-compiled (VM fallback path in JIT-to-JIT calls).
///
/// # Safety
/// All pointers must be valid. Called from JIT-generated code.
#[cfg(feature = "jit")]
pub extern "C" fn vm_call_trampoline(
    vm: *mut core::ffi::c_void,
    _fiber: *mut core::ffi::c_void,
    func_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> vo_runtime::jit_api::JitResult {
    let vm = unsafe { &mut *(vm as *mut Vm) };
    let args_slice = unsafe { std::slice::from_raw_parts(args, arg_count as usize) };

    // Trigger func JIT compilation for the target function if not already compiled.
    // This ensures that future JIT-to-JIT calls can use the compiled version.
    if let Some(jit_mgr) = vm.jit_mgr.as_mut() {
        if !jit_mgr.is_compiled(func_id) && !jit_mgr.is_unsupported(func_id) {
            let module = vm.module.as_ref().unwrap();
            let func_def = &module.functions[func_id as usize];
            let _ = jit_mgr.compile_function(func_id, func_def, module);
        }
    }

    // Execute using callback fiber
    let (success, _panic_state) = vm.execute_func_sync(func_id, args_slice, ret, ret_count);

    if success {
        vo_runtime::jit_api::JitResult::Ok
    } else {
        vo_runtime::jit_api::JitResult::Panic
    }
}

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
