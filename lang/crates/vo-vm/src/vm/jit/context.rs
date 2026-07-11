//! JIT context management.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::bytecode::Module;
use vo_runtime::jit_api::JitContext;
use vo_runtime::objects::interface::InterfaceSlot;

use crate::fiber::Fiber;
use crate::vm::Vm;

use super::callbacks;
use super::frame::{jit_pop_frame, jit_push_frame, jit_push_resume_point};

static LEGACY_SAFEPOINT_FLAG: bool = false;

/// JIT context wrapper.
///
/// Panic state (panic_flag, is_user_panic, panic_msg) lives on Fiber directly,
/// eliminating the per-call Box allocation. JitContext pointers point into Fiber.
pub struct JitContextWrapper {
    pub ctx: JitContext,
}

impl JitContextWrapper {
    pub fn as_ptr(&mut self) -> *mut JitContext {
        &mut self.ctx
    }

    pub fn panic_msg(&self) -> InterfaceSlot {
        unsafe { *self.ctx.panic_msg }
    }

    pub fn is_user_panic(&self) -> bool {
        unsafe { *self.ctx.is_user_panic }
    }

    pub fn call_func_id(&self) -> u32 {
        self.ctx.call_func_id
    }

    pub fn call_arg_start(&self) -> u32 {
        self.ctx.call_arg_start
    }

    pub fn call_resume_pc(&self) -> u32 {
        self.ctx.call_resume_pc
    }

    pub fn call_ret_reg(&self) -> u16 {
        self.ctx.call_ret_reg
    }

    #[cfg(feature = "std")]
    pub fn wait_io_token(&self) -> u64 {
        self.ctx.wait_io_token
    }

    pub fn ret_start(&self) -> u16 {
        self.ctx.ret_start
    }
}

pub fn build_jit_context(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
) -> Result<JitContextWrapper, String> {
    // Extract jit_mgr values first to avoid borrow conflicts
    let (jit_func_table, jit_func_count, direct_call_table, direct_call_count, ic_table) = {
        let jit_mgr = vm.jit.manager_mut().ok_or_else(|| {
            "JIT context requested without an initialized JIT manager".to_string()
        })?;
        (
            jit_mgr.func_table_ptr(),
            jit_mgr.func_table_len() as u32,
            jit_mgr.direct_call_table_ptr(),
            jit_mgr.direct_call_table_len() as u32,
            jit_mgr.ensure_dynamic_call_ic(),
        )
    };

    // Reset panic state on Fiber (no heap allocation needed)
    fiber.jit_panic_flag = false;
    fiber.jit_is_user_panic = false;
    fiber.jit_panic_msg = InterfaceSlot::default();
    fiber.jit_infra_error_message.clear();

    let ctx = JitContext {
        gc: &mut vm.state.gc as *mut _,
        globals: vm.state.globals.as_mut_ptr(),
        safepoint_flag: &LEGACY_SAFEPOINT_FLAG,
        panic_flag: &mut fiber.jit_panic_flag as *mut bool,
        is_user_panic: &mut fiber.jit_is_user_panic as *mut bool,
        panic_msg: &mut fiber.jit_panic_msg as *mut InterfaceSlot,
        user_panic_pc: u32::MAX,
        runtime_trap_kind: vo_runtime::jit_api::JitRuntimeTrapKind::None as u8,
        runtime_trap_arg0: 0,
        runtime_trap_arg1: 0,
        runtime_trap_pc: u32::MAX,
        current_func_id: u32::MAX,
        infra_error_message: &mut fiber.jit_infra_error_message as *mut String,
        vm: vm as *mut Vm as *mut core::ffi::c_void,
        fiber: fiber as *mut Fiber as *mut core::ffi::c_void,
        itab_cache: &mut vm.state.itab_cache as *mut _,
        extern_registry: &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
        #[cfg(feature = "std")]
        call_extern_fn: Some(super::jit_call_extern),
        #[cfg(not(feature = "std"))]
        call_extern_fn: None,
        module: module as *const Module,
        jit_func_table,
        jit_func_count,
        direct_call_table,
        direct_call_count,
        program_args: &vm.state.program_args as *const Vec<String>,
        sentinel_errors: &mut vm.state.sentinel_errors as *mut _,
        output: &*vm.state.output as *const dyn vo_runtime::output::OutputSink,
        host_output: &mut vm.state.host_output as *mut _,
        #[cfg(feature = "std")]
        io: &mut vm.state.io as *mut _,
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_ret_reg: 0,
        call_kind: 0,
        #[cfg(feature = "std")]
        wait_io_token: 0,
        loop_exit_pc: 0,
        // Fiber stack access fields - will be updated before JIT call
        stack_ptr: fiber.stack_ptr(),
        stack_cap: fiber.stack.len() as u32,
        stack_limit: crate::fiber::MAX_JIT_NATIVE_STACK_SLOTS as u32,
        call_depth: 0,
        call_depth_limit: crate::fiber::MAX_JIT_CALL_DEPTH as u32,
        jit_bp: 0, // Will be set in dispatch_jit_call
        fiber_sp: fiber.sp as u32,
        push_frame_fn: Some(jit_push_frame),
        pop_frame_fn: Some(jit_pop_frame),
        stack_overflow_fn: Some(callbacks::jit_stack_overflow),
        push_resume_point_fn: Some(jit_push_resume_point),
        // Callbacks
        create_island_fn: Some(callbacks::jit_create_island),
        queue_len_fn: Some(callbacks::jit_queue_len),
        queue_cap_fn: Some(callbacks::jit_queue_cap),
        queue_close_fn: Some(callbacks::jit_queue_close),
        queue_send_fn: Some(callbacks::jit_queue_send),
        queue_recv_fn: Some(callbacks::jit_queue_recv),
        go_start_fn: Some(callbacks::jit_go_start),
        go_island_fn: Some(callbacks::jit_go_island),
        defer_push_fn: Some(callbacks::jit_defer_push),
        recover_fn: Some(callbacks::jit_recover),
        select_begin_fn: Some(callbacks::jit_select_begin),
        select_send_fn: Some(callbacks::jit_select_send),
        select_recv_fn: Some(callbacks::jit_select_recv),
        select_exec_fn: Some(callbacks::jit_select_exec),
        is_error_return: 0,
        ret_gcref_start: 0,
        ret_is_heap: 0,
        ret_start: 0,
        prepare_closure_call_fn: Some(callbacks::jit_prepare_closure_call),
        prepare_iface_call_fn: Some(callbacks::jit_prepare_iface_call),
        ic_table,
        execution_budget: fiber.execution_budget,
    };

    ctx.validate_required_callbacks().map_err(|field| {
        format!("JIT context missing required callback or ABI pointer: {field}")
    })?;

    Ok(JitContextWrapper { ctx })
}

#[cfg(test)]
mod tests {
    use super::super::test_support::function;
    use super::*;

    #[test]
    fn vm_build_jit_context_without_manager_is_jit_error() {
        let mut vm = Vm::new();
        let mut module = Module::new("jit-context-missing-manager-test".to_string());
        module.functions.push(function(1, 0));
        let mut fiber = Fiber::new(7);

        let err = match build_jit_context(&mut vm, &mut fiber, &module) {
            Ok(_) => panic!("missing JIT manager must fail fast"),
            Err(err) => err,
        };

        assert!(
            err.contains("initialized JIT manager"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn jit_context_inherits_active_fiber_scheduler_budget() {
        let mut vm = Vm::try_with_jit_config(crate::vm::JitConfig::default()).expect("JIT VM");
        let mut module = Module::new("jit-context-budget-test".to_string());
        module.functions.push(function(1, 0));
        vm.load(module).expect("load module");
        let module = vm.module.as_ref().expect("loaded module").clone();
        let mut fiber = Fiber::new(7);
        fiber.execution_budget = 17;

        let ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("JIT context");

        assert_eq!(ctx.ctx.execution_budget, 17);
    }
}
