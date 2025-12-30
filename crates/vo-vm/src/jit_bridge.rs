//! JIT Bridge Layer - Handles JIT/VM transitions and context switching.
//!
//! This module contains all the "glue code" between JIT and VM:
//! - vo_call_function: Entry point for JIT code to call back into VM
//! - High-level JIT call functions using JitCallContext trait
//! - OSR compilation helpers
//! - JIT function calling utilities

use vo_runtime::jit_api::{JitContext, JitResult, JitCallContext};
use vo_jit::{JitManager, JitFunc};

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
