//! JIT dispatch: bridge between VM interpreter and JIT-compiled code.
//!
//! ## Overview
//!
//! When VM encounters a `Call` instruction for a JIT-compiled function:
//! 1. `dispatch_jit_call` prepares args/ret buffers and VM frame
//! 2. JIT function executes natively
//! 3. Results (Ok/Panic) are translated back to VM state
//!
//! ## Current Limitations (Step 1)
//!
//! - `JitResult::Call` not yet handled (needs Step 2: resume stack)
//! - `JitResult::WaitIo` not yet handled (needs Step 3: shadow frames)
//! - `CallExtern` excluded from JIT compilation

use vo_runtime::bytecode::Module;
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::{JitContext, JitResult};
use vo_runtime::objects::interface::InterfaceSlot;

use crate::fiber::{CallFrame, Fiber};
use crate::vm::{helpers, ExecResult, Vm};

/// Execute a JIT-compiled function call.
///
/// This function:
/// 1. Prepares args/ret buffers from caller's stack
/// 2. Pushes a temporary VM frame (for panic/defer support)
/// 3. Calls the JIT function
/// 4. Handles the result (Ok/Panic/Call/WaitIo)
///
/// Returns `ExecResult::FrameChanged` on success (return values written to stack).
pub fn dispatch_jit_call(
    vm: &mut Vm,
    fiber: &mut Fiber,
    inst: &Instruction,
    module: &Module,
    jit_func: vo_jit::JitFunc,
    func_id: u32,
) -> ExecResult {
    let caller_bp = fiber.frames.last().map_or(0, |f| f.bp);
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;

    let func_def = &module.functions[func_id as usize];

    // Prepare args buffer (copy from caller's stack)
    let mut args: Vec<u64> = vec![0u64; func_def.local_slots as usize];
    for i in 0..arg_slots {
        args[i] = fiber.stack[caller_bp + arg_start + i];
    }

    // Prepare ret buffer
    let mut ret: Vec<u64> = vec![0u64; ret_slots.max(1)];

    // Build JitContext
    let mut ctx = build_jit_context(vm, fiber, module);

    // Push a temporary frame so panic_unwind has correct frame info
    let jit_bp = fiber.sp;
    fiber.ensure_capacity(jit_bp + func_def.local_slots as usize);
    fiber.sp = jit_bp + func_def.local_slots as usize;
    fiber.frames.push(CallFrame::new(func_id, jit_bp, 0, func_def.ret_slots));

    // Call JIT function
    let result = jit_func(ctx.as_ptr(), args.as_mut_ptr(), ret.as_mut_ptr());

    handle_jit_result(vm, fiber, module, result, ctx, caller_bp, arg_start, ret_slots, jit_bp, &ret)
}

/// JIT context with owned storage for mutable fields.
///
/// The JitContext contains pointers to mutable state (panic_flag, panic_msg).
/// This wrapper owns those values to ensure they outlive the JIT call.
struct JitContextWrapper {
    ctx: JitContext,
    // Owned storage - pointers in ctx point to these
    _panic_flag: Box<bool>,
    _panic_msg: Box<InterfaceSlot>,
    _safepoint_flag: Box<bool>,
}

impl JitContextWrapper {
    fn as_ptr(&mut self) -> *mut JitContext {
        &mut self.ctx
    }

    fn panic_msg(&self) -> InterfaceSlot {
        *self._panic_msg
    }
}

fn build_jit_context(vm: &mut Vm, fiber: &mut Fiber, module: &Module) -> JitContextWrapper {
    // Extract jit_mgr values first to avoid borrow conflicts
    let (jit_func_table, jit_func_count) = {
        let jit_mgr = vm.jit_mgr.as_ref().unwrap();
        (jit_mgr.func_table_ptr(), jit_mgr.func_table_len() as u32)
    };

    let mut panic_flag = Box::new(false);
    let mut panic_msg = Box::new(InterfaceSlot::default());
    let mut safepoint_flag = Box::new(false);

    let ctx = JitContext {
        gc: &mut vm.state.gc as *mut _,
        globals: vm.state.globals.as_mut_ptr(),
        safepoint_flag: &*safepoint_flag as *const bool,
        panic_flag: &mut *panic_flag as *mut bool,
        panic_msg: &mut *panic_msg as *mut InterfaceSlot,
        vm: vm as *mut Vm as *mut core::ffi::c_void,
        fiber: fiber as *mut Fiber as *mut core::ffi::c_void,
        call_vm_fn: Some(jit_call_vm_trampoline),
        itab_cache: &mut vm.state.itab_cache as *mut _,
        extern_registry: &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
        call_extern_fn: None, // CallExtern excluded from JIT in Step 1
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
        #[cfg(feature = "std")]
        wait_io_token: 0,
        loop_exit_pc: 0,
    };

    JitContextWrapper {
        ctx,
        _panic_flag: panic_flag,
        _panic_msg: panic_msg,
        _safepoint_flag: safepoint_flag,
    }
}

fn handle_jit_result(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    result: JitResult,
    ctx: JitContextWrapper,
    caller_bp: usize,
    arg_start: usize,
    ret_slots: usize,
    jit_bp: usize,
    ret: &[u64],
) -> ExecResult {
    match result {
        JitResult::Ok => {
            // Pop the JIT frame
            fiber.frames.pop();
            fiber.sp = jit_bp;

            // Copy return values back to caller's stack
            for i in 0..ret_slots {
                fiber.stack[caller_bp + arg_start + i] = ret[i];
            }
            ExecResult::FrameChanged
        }
        JitResult::Panic => {
            let mut panic_msg = ctx.panic_msg();
            
            // Check if panic_msg is nil (runtime panic like nil pointer)
            if panic_msg.slot0 == 0 {
                // Create default nil pointer message
                let msg_str = vo_runtime::objects::string::new_from_string(
                    &mut vm.state.gc,
                    helpers::ERR_NIL_POINTER.to_string(),
                );
                let slot0 = vo_runtime::objects::interface::pack_slot0(
                    0, 0, vo_runtime::ValueKind::String
                );
                panic_msg = InterfaceSlot::new(slot0, msg_str as u64);
            }
            
            // Set panic state on fiber
            fiber.set_recoverable_panic(panic_msg);
            let stack_ptr = fiber.stack_ptr();
            helpers::panic_unwind(fiber, stack_ptr, module)
        }
        JitResult::Call => {
            // TODO(Step 2): Handle JIT returning Call for VM fallback
            panic!("JIT returned Call - not yet implemented (Step 2)")
        }
        JitResult::WaitIo => {
            // TODO(Step 3): Handle JIT returning WaitIo for blocking IO
            panic!("JIT returned WaitIo - not yet implemented (Step 3)")
        }
    }
}

/// Trampoline for JIT code to call VM-interpreted functions.
///
/// This is used when a JIT-compiled function calls another function
/// that isn't JIT-compiled (VM fallback path in JIT-to-JIT calls).
///
/// # Safety
/// All pointers must be valid. Called from JIT-generated code.
pub extern "C" fn jit_call_vm_trampoline(
    vm: *mut core::ffi::c_void,
    fiber: *mut core::ffi::c_void,
    func_id: u32,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> JitResult {
    let vm = unsafe { &mut *(vm as *mut Vm) };
    let args_slice = unsafe { std::slice::from_raw_parts(args, arg_count as usize) };

    // Execute using callback fiber
    let (success, _panic_state) = vm.execute_closure_sync(func_id, args_slice, ret, ret_count);

    if success {
        JitResult::Ok
    } else {
        JitResult::Panic
    }
}
