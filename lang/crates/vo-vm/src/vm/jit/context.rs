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
use super::frame::{jit_push_frame, jit_pop_frame, jit_push_resume_point};

/// JIT context with owned storage for mutable fields.
///
/// The JitContext contains pointers to mutable state (panic_flag, is_user_panic, panic_msg).
/// This wrapper owns those values to ensure they outlive the JIT call.
pub struct JitContextWrapper {
    pub ctx: JitContext,
    // Owned storage - pointers in ctx point to these
    _panic_flag: Box<bool>,
    _is_user_panic: Box<bool>,
    _panic_msg: Box<InterfaceSlot>,
    _safepoint_flag: Box<bool>,
}

impl JitContextWrapper {
    pub fn as_ptr(&mut self) -> *mut JitContext {
        &mut self.ctx
    }

    pub fn panic_msg(&self) -> InterfaceSlot {
        *self._panic_msg
    }

    pub fn is_user_panic(&self) -> bool {
        *self._is_user_panic
    }

    pub fn call_func_id(&self) -> u32 {
        self.ctx.call_func_id
    }

    pub fn call_arg_start(&self) -> u16 {
        self.ctx.call_arg_start
    }

    pub fn call_resume_pc(&self) -> u32 {
        self.ctx.call_resume_pc
    }

    pub fn call_ret_slots(&self) -> u16 {
        self.ctx.call_ret_slots
    }

    #[cfg(feature = "std")]
    pub fn wait_io_token(&self) -> u64 {
        self.ctx.wait_io_token
    }

    pub fn ret_start(&self) -> u16 {
        self.ctx.ret_start
    }
}

pub fn build_jit_context(vm: &mut Vm, fiber: &mut Fiber, module: &Module) -> JitContextWrapper {
    // Extract jit_mgr values first to avoid borrow conflicts
    let (jit_func_table, jit_func_count) = {
        let jit_mgr = vm.jit_mgr.as_ref().unwrap();
        (jit_mgr.func_table_ptr(), jit_mgr.func_table_len() as u32)
    };

    let mut panic_flag = Box::new(false);
    let mut is_user_panic = Box::new(false);
    let mut panic_msg = Box::new(InterfaceSlot::default());
    let mut safepoint_flag = Box::new(false);

    let ctx = JitContext {
        gc: &mut vm.state.gc as *mut _,
        globals: vm.state.globals.as_mut_ptr(),
        safepoint_flag: &*safepoint_flag as *const bool,
        panic_flag: &mut *panic_flag as *mut bool,
        is_user_panic: &mut *is_user_panic as *mut bool,
        panic_msg: &mut *panic_msg as *mut InterfaceSlot,
        vm: vm as *mut Vm as *mut core::ffi::c_void,
        fiber: fiber as *mut Fiber as *mut core::ffi::c_void,
        call_vm_fn: Some(crate::vm::vm_call_trampoline),
        itab_cache: &mut vm.state.itab_cache as *mut _,
        extern_registry: &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
        #[cfg(feature = "std")]
        call_extern_fn: Some(super::jit_call_extern),
        #[cfg(not(feature = "std"))]
        call_extern_fn: None,
        module: module as *const Module as *const vo_runtime::bytecode::Module,
        jit_func_table,
        jit_func_count,
        program_args: &vm.state.program_args as *const Vec<String>,
        sentinel_errors: &mut vm.state.sentinel_errors as *mut _,
        #[cfg(feature = "std")]
        io: &mut vm.state.io as *mut _,
        call_func_id: 0,
        call_arg_start: 0,
        call_resume_pc: 0,
        call_ret_slots: 0,
        call_kind: 0,
        call_arg_slots: 0,
        call_closure_ref: 0,
        call_iface_recv: 0,
        #[cfg(feature = "std")]
        wait_io_token: 0,
        loop_exit_pc: 0,
        // Fiber stack access fields - will be updated before JIT call
        stack_ptr: fiber.stack_ptr(),
        stack_cap: fiber.stack.len() as u32,
        jit_bp: 0, // Will be set in dispatch_jit_call
        fiber_sp: fiber.sp as u32,
        push_frame_fn: Some(jit_push_frame),
        pop_frame_fn: Some(jit_pop_frame),
        push_resume_point_fn: Some(jit_push_resume_point),
        // Callbacks
        create_island_fn: Some(callbacks::jit_create_island),
        chan_close_fn: Some(callbacks::jit_chan_close),
        port_close_fn: Some(callbacks::jit_port_close),
        chan_send_fn: Some(callbacks::jit_chan_send),
        chan_recv_fn: Some(callbacks::jit_chan_recv),
        port_send_fn: Some(callbacks::jit_port_send),
        port_recv_fn: Some(callbacks::jit_port_recv),
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
    };

    JitContextWrapper {
        ctx,
        _panic_flag: panic_flag,
        _is_user_panic: is_user_panic,
        _panic_msg: panic_msg,
        _safepoint_flag: safepoint_flag,
    }
}
