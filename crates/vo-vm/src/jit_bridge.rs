//! JIT Bridge Layer - Handles JIT/VM transitions and context switching.
//!
//! This module contains all the "glue code" between JIT and VM:
//! - vo_call_function: Entry point for JIT code to call back into VM
//! - Trampolines for JIT -> VM callbacks (itab lookup, extern calls)
//! - JitContext builder
//! - High-level JIT call functions using JitCallContext trait
//! - OSR compilation helpers
//! - JIT function calling utilities

use vo_runtime::jit_api::{JitContext, JitResult, JitCallContext};
use vo_runtime::ffi::{ExternResult, ExternRegistry};
use vo_jit::{JitManager, JitFunc};

// =============================================================================
// Trampolines for JIT -> VM callbacks
// =============================================================================

/// Itab lookup trampoline for JIT -> VM itab lookup.
pub extern "C" fn itab_lookup_trampoline(
    itabs: *const std::ffi::c_void,
    itab_id: u32,
    method_idx: u32,
) -> u32 {
    unsafe {
        let itabs = itabs as *const crate::bytecode::Itab;
        let itab = &*itabs.add(itab_id as usize);
        itab.methods[method_idx as usize]
    }
}

/// Extern call trampoline for JIT -> extern function calls.
pub extern "C" fn call_extern_trampoline(
    registry: *const std::ffi::c_void,
    gc: *mut vo_runtime::gc::Gc,
    extern_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
) -> JitResult {
    let registry = unsafe { &*(registry as *const ExternRegistry) };
    let gc = unsafe { &mut *gc };
    
    // Create a temporary stack for the extern call
    let mut temp_stack: Vec<u64> = (0..arg_count as usize)
        .map(|i| unsafe { *args.add(i) })
        .collect();
    
    // Call through ExternRegistry
    let result = registry.call(
        extern_id,
        &mut temp_stack,
        0,     // bp
        0,     // arg_start
        arg_count as u16,
        0,     // ret_start (same as arg_start)
        gc,
    );
    
    match result {
        ExternResult::Ok => {
            // Copy return values (extern functions may return values in the same slots)
            for i in 0..arg_count as usize {
                unsafe { *ret.add(i) = temp_stack[i] };
            }
            JitResult::Ok
        }
        ExternResult::Yield => JitResult::Panic, // Yield not supported in JIT
        ExternResult::Panic(_) => JitResult::Panic,
    }
}

// =============================================================================
// JitContext Builder
// =============================================================================

/// Helper to build JitContext from components.
/// Centralizes the JitContext construction to avoid code duplication.
pub fn build_jit_ctx(
    gc: *mut vo_runtime::gc::Gc,
    globals: *mut u64,
    jit_func_table: *const *const u8,
    jit_func_count: u32,
    vm_ptr: *mut std::ffi::c_void,
    fiber_ptr: *mut std::ffi::c_void,
    module_ptr: *const std::ffi::c_void,
    itabs: *const std::ffi::c_void,
    extern_registry: *const std::ffi::c_void,
    itab_cache: *mut std::ffi::c_void,
    call_vm_fn: Option<extern "C" fn(*mut std::ffi::c_void, *mut std::ffi::c_void, u32, *const u64, u32, *mut u64, u32) -> JitResult>,
    safepoint_flag: *const bool,
    panic_flag: *mut bool,
) -> JitContext {
    JitContext {
        gc,
        globals,
        safepoint_flag,
        panic_flag,
        vm: vm_ptr,
        fiber: fiber_ptr,
        call_vm_fn,
        itabs,
        itab_lookup_fn: Some(itab_lookup_trampoline),
        extern_registry,
        call_extern_fn: Some(call_extern_trampoline),
        itab_cache,
        module: module_ptr,
        iface_assert_fn: None,
        jit_func_table,
        jit_func_count,
    }
}

// =============================================================================
// JIT -> VM Call Trampoline
// =============================================================================

/// VM call trampoline for JIT -> VM calls.
/// This is the **unified entry point** for all function calls from JIT code.
/// It delegates to Vm::execute_jit_call which handles JIT/VM selection.
pub extern "C" fn vm_call_trampoline(
    vm: *mut std::ffi::c_void,
    _fiber: *mut std::ffi::c_void,
    func_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult {
    use crate::vm::Vm;
    
    // Safety: vm must be a valid pointer to Vm
    let vm = unsafe { &mut *(vm as *mut Vm) };
    vm.execute_jit_call(func_id, args, arg_count, ret, ret_count)
}

// =============================================================================
// JIT -> VM Call Entry Point
// =============================================================================

/// The unified function call entry point for JIT-generated code.
/// 
/// This is called by JIT code when `jit_func_table[func_id]` is null.
/// It delegates to the VM's call_vm_fn callback.
#[no_mangle]
pub extern "C" fn vo_call_function(
    ctx: *mut JitContext,
    func_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult {
    let ctx = unsafe { &mut *ctx };
    
    let vm = ctx.vm;
    if vm.is_null() {
        return JitResult::Panic;
    }
    
    if let Some(call_vm) = ctx.call_vm_fn {
        return call_vm(vm, ctx.fiber, func_id, args, arg_count, ret, ret_count);
    }
    
    JitResult::Panic
}

// =============================================================================
// JIT Function Call Helpers
// =============================================================================

/// Call a JIT function with the given arguments.
/// Returns the result and writes return values to ret_buf.
pub fn call_jit_function(
    jit_func: JitFunc,
    ctx: &mut JitContext,
    args: &mut [u64],
    ret_buf: &mut [u64],
) -> JitResult {
    jit_func(ctx, args.as_mut_ptr(), ret_buf.as_mut_ptr())
}

// =============================================================================
// OSR Helpers
// =============================================================================

/// OSR compilation result.
pub enum OsrResult {
    /// Already have OSR version, use it
    Ready(JitFunc),
    /// Should compile now (hot enough)
    ShouldCompile,
    /// Not hot enough yet, continue VM
    NotHot,
}

/// Try to perform OSR at a loop back-edge.
/// Returns the appropriate action for OSR.
pub fn try_osr_compile(
    jit_mgr: &mut JitManager,
    func_id: u32,
    backedge_pc: usize,
    loop_header_pc: usize,
) -> OsrResult {
    // Check if already have OSR version
    if let Some(ptr) = jit_mgr.get_osr_entry(func_id, loop_header_pc) {
        return OsrResult::Ready(ptr);
    }
    
    // Record backedge and check if should compile
    match jit_mgr.record_backedge(func_id, backedge_pc, loop_header_pc) {
        Some(_) => OsrResult::ShouldCompile,
        None => OsrResult::NotHot,
    }
}

// =============================================================================
// High-level JIT Call using JitCallContext
// =============================================================================

/// Execute a JIT function call using the JitCallContext trait.
/// This is the main entry point for JIT calls from VM.
///
/// The caller should obtain jit_func first, then call this function.
/// This avoids borrow conflicts between jit_mgr and the JitCallContext.
pub fn execute_jit_call<C: JitCallContext>(
    ctx: &mut C,
    jit_func: JitFunc,
    fiber_id: u32,
    arg_start: u16,
    arg_count: usize,
    ret_count: usize,
) -> JitResult {
    // Read args from fiber
    let mut args = ctx.read_args(fiber_id, arg_start, arg_count);
    let mut ret_buf = vec![0u64; ret_count];
    
    // Build JitContext
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut jit_ctx = ctx.build_context(fiber_id, &safepoint_flag, &mut panic_flag);
    
    // Call JIT function
    let result = call_jit_function(jit_func, &mut jit_ctx, &mut args, &mut ret_buf);
    
    // Write return values back
    if result == JitResult::Ok {
        ctx.write_returns(fiber_id, arg_start, &ret_buf);
    }
    
    result
}

/// Execute an OSR function call using the JitCallContext trait.
/// This handles the JIT execution part of OSR.
///
/// The caller should obtain osr_func first, then call this function.
/// Returns (JitResult, ret_buf).
pub fn execute_osr_call<C: JitCallContext>(
    ctx: &mut C,
    osr_func: JitFunc,
    fiber_id: u32,
    bp: usize,
    local_count: usize,
    ret_count: usize,
) -> (JitResult, Vec<u64>) {
    // Read locals from fiber
    let mut locals = ctx.read_locals(fiber_id, bp, local_count);
    let mut ret_buf = vec![0u64; ret_count];
    
    // Build JitContext
    let safepoint_flag = false;
    let mut panic_flag = false;
    let mut jit_ctx = ctx.build_context(fiber_id, &safepoint_flag, &mut panic_flag);
    
    // Call OSR function
    let result = call_jit_function(osr_func, &mut jit_ctx, &mut locals, &mut ret_buf);
    
    (result, ret_buf)
}
