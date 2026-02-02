//! Shared call emission helpers for FunctionCompiler and LoopCompiler.
//!
//! This module consolidates call_closure, call_iface, and related logic
//! to ensure consistent behavior and reduce bug risk from code duplication.

use cranelift_codegen::ir::{types, InstBuilder, MemFlags, SigRef, StackSlotData, StackSlotKind, Value};
use cranelift_codegen::ir::condcodes::IntCC;

use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitContext;

use crate::translator::IrEmitter;

// JitResult constants for readability
pub const JIT_RESULT_OK: i32 = 0;
pub const JIT_RESULT_PANIC: i32 = 1;
pub const JIT_RESULT_CALL: i32 = 2;
pub const JIT_RESULT_WAIT_IO: i32 = 3;

/// Create signature for vo_jit_push_frame: (ctx, func_id, local_slots, ret_reg, ret_slots, caller_resume_pc) -> args_ptr
pub fn import_push_frame_sig<'a, E: IrEmitter<'a>>(emitter: &mut E) -> SigRef {
    emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ctx
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // func_id
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // local_slots
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // ret_reg
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // ret_slots
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // caller_resume_pc
        sig.returns.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // callee_args_ptr
        sig
    })
}

/// Create signature for vo_jit_pop_frame: (ctx) -> ()
pub fn import_pop_frame_sig<'a, E: IrEmitter<'a>>(emitter: &mut E) -> SigRef {
    emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ctx
        sig
    })
}

/// Create signature for JIT function: (ctx, args_ptr, ret_ptr) -> JitResult
pub fn import_jit_func_sig<'a, E: IrEmitter<'a>>(emitter: &mut E) -> SigRef {
    emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ctx
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // args_ptr
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ret_ptr
        sig.returns.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // JitResult
        sig
    })
}

/// Configuration for call emission - captures the differences between compilers.
/// NOTE: Currently only used by placeholder functions (emit_call_closure, emit_call_iface).
#[allow(dead_code)]
pub struct CallConfig {
    /// Resume PC to store before call (for WaitIo handling). None = don't set.
    pub resume_pc: Option<usize>,
    /// Whether to spill variables on non-ok result before returning.
    pub spill_on_non_ok: bool,
}

/// Emit a closure call instruction.
/// 
/// NOTE: CallClosure is currently NOT jittable (see is_func_jittable).
/// This function exists as a placeholder for Phase 5 unified dispatch implementation.
/// Until then, functions with CallClosure will fall back to VM execution.
#[allow(dead_code)]
pub fn emit_call_closure<'a, E: IrEmitter<'a>>(
    _emitter: &mut E,
    _inst: &Instruction,
    _config: CallConfig,
) {
    // CallClosure is excluded from JIT in is_func_jittable() until Phase 5.
    // If we reach here, something is wrong with the jittability check.
    unreachable!("CallClosure should not be JIT-compiled until Phase 5 unified dispatch")
}

/// Emit an interface method call instruction.
/// 
/// NOTE: CallIface is currently NOT jittable (see is_func_jittable).
/// This function exists as a placeholder for Phase 5 unified dispatch implementation.
/// Until then, functions with CallIface will fall back to VM execution.
#[allow(dead_code)]
pub fn emit_call_iface<'a, E: IrEmitter<'a>>(
    _emitter: &mut E,
    _inst: &Instruction,
    _config: CallConfig,
) {
    // CallIface is excluded from JIT in is_func_jittable() until Phase 5.
    // If we reach here, something is wrong with the jittability check.
    unreachable!("CallIface should not be JIT-compiled until Phase 5 unified dispatch")
}

/// Emit an extern function call instruction.
/// 
/// Both FunctionCompiler and LoopCompiler should use this to ensure consistent behavior.
pub fn emit_call_extern<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    inst: &Instruction,
    config: CallExternConfig,
) {
    let call_extern_func = emitter.helpers().call_extern.expect("call_extern helper not registered");
    
    let dst = inst.a as usize;
    let extern_id = inst.b as u32;
    let arg_start = inst.c as usize;
    let arg_count = inst.flags as usize;
    
    // Get extern info
    let extern_def = &emitter.vo_module().externs[extern_id as usize];
    let is_blocking = extern_def.is_blocking;
    let extern_ret_slots = extern_def.ret_slots as usize;
    let buffer_size = arg_count.max(extern_ret_slots).max(1);
    
    // Limit copy-back to available variables
    let available_vars = emitter.local_slot_count().saturating_sub(dst);
    let copy_back_slots = extern_ret_slots.min(available_vars);
    
    let builder = emitter.builder();
    let slot = builder.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (buffer_size * 8) as u32,
        8,
    ));
    
    // Store arguments to stack slot
    for i in 0..arg_count {
        let val = emitter.read_var((arg_start + i) as u16);
        emitter.builder().ins().stack_store(val, slot, (i * 8) as i32);
    }
    
    let ctx = emitter.ctx_param();
    let args_ptr = emitter.builder().ins().stack_addr(types::I64, slot, 0);
    let extern_id_val = emitter.builder().ins().iconst(types::I32, extern_id as i64);
    let arg_count_val = emitter.builder().ins().iconst(types::I32, arg_count as i64);
    let ret_slots_val = emitter.builder().ins().iconst(types::I32, extern_ret_slots as i64);
    
    if is_blocking && config.handle_waitio_specially {
        // FunctionCompiler path: special WaitIo handling
        // Spill before call for blocking externs
        emitter.spill_all_vars();
        
        let call = emitter.builder().ins().call(call_extern_func, &[ctx, extern_id_val, args_ptr, arg_count_val, args_ptr, ret_slots_val]);
        let result = emitter.builder().inst_results(call)[0];
        
        // Check for WaitIo
        let wait_io_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_WAIT_IO as i64);
        let is_wait_io = emitter.builder().ins().icmp(IntCC::Equal, result, wait_io_val);
        
        let wait_io_block = emitter.builder().create_block();
        let continue_block = emitter.builder().create_block();
        
        emitter.builder().ins().brif(is_wait_io, wait_io_block, &[], continue_block, &[]);
        
        // WaitIo path: set resume_pc and return WaitIo
        emitter.builder().switch_to_block(wait_io_block);
        emitter.builder().seal_block(wait_io_block);
        
        let resume_pc_val = emitter.builder().ins().iconst(types::I32, config.current_pc as i64);
        emitter.builder().ins().store(MemFlags::trusted(), resume_pc_val, ctx, JitContext::OFFSET_CALL_RESUME_PC);
        emitter.builder().ins().return_(&[wait_io_val]);
        
        // Continue path: check for panic
        emitter.builder().switch_to_block(continue_block);
        emitter.builder().seal_block(continue_block);
        
        check_call_result(emitter, result, false); // Already spilled above
    } else {
        // LoopCompiler path: set resume_pc, unified check_call_result
        let resume_pc_val = emitter.builder().ins().iconst(types::I32, config.current_pc as i64);
        emitter.builder().ins().store(MemFlags::trusted(), resume_pc_val, ctx, JitContext::OFFSET_CALL_RESUME_PC);
        
        let call = emitter.builder().ins().call(call_extern_func, &[ctx, extern_id_val, args_ptr, arg_count_val, args_ptr, ret_slots_val]);
        let result = emitter.builder().inst_results(call)[0];
        
        check_call_result(emitter, result, config.spill_on_non_ok);
    }
    
    // Copy return values back
    for i in 0..copy_back_slots {
        let val = emitter.builder().ins().stack_load(types::I64, slot, (i * 8) as i32);
        emitter.write_var((dst + i) as u16, val);
    }
}

/// Configuration for extern call emission.
pub struct CallExternConfig {
    /// Current PC (for resume_pc setting).
    pub current_pc: usize,
    /// Whether to spill variables on non-ok result (LoopCompiler needs this).
    pub spill_on_non_ok: bool,
    /// Whether to handle WaitIo specially (FunctionCompiler does, LoopCompiler doesn't).
    pub handle_waitio_specially: bool,
}

/// Emit a call via VM using Call request mechanism.
/// 
/// This is used when the callee is not jittable (has defer/channel/select/etc).
/// Instead of synchronously calling vo_call_vm, we:
/// 1. Set call request info in JitContext
/// 2. Return JitResult::Call
/// 3. VM executes the callee and continues in the interpreter
pub fn emit_call_via_vm<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    config: CallViaVmConfig,
) {
    let set_call_request_func = emitter.helpers().set_call_request.expect("set_call_request helper not registered");
    
    // Spill all variables to fiber.stack before returning Call
    emitter.spill_all_vars();
    
    let ctx = emitter.ctx_param();
    let func_id_val = emitter.builder().ins().iconst(types::I32, config.func_id as i64);
    let arg_start_val = emitter.builder().ins().iconst(types::I32, config.arg_start as i64);
    let resume_pc_val = emitter.builder().ins().iconst(types::I32, config.resume_pc as i64);
    let ret_slots_val = emitter.builder().ins().iconst(types::I32, config.ret_slots as i64);
    
    emitter.builder().ins().call(set_call_request_func, &[ctx, func_id_val, arg_start_val, resume_pc_val, ret_slots_val]);
    
    // Return JitResult::Call
    let call_result = emitter.builder().ins().iconst(types::I32, JIT_RESULT_CALL as i64);
    emitter.builder().ins().return_(&[call_result]);
}

/// Configuration for call via VM.
pub struct CallViaVmConfig {
    pub func_id: u32,
    pub arg_start: usize,
    pub resume_pc: usize,
    pub ret_slots: usize,
}

/// Configuration for JIT-to-JIT call with fallback.
pub struct JitCallWithFallbackConfig {
    pub func_id: u32,
    pub arg_start: usize,
    pub arg_slots: usize,
    pub call_ret_slots: usize,
    pub func_ret_slots: usize,
    /// Callee's local_slots (from FunctionDef.local_slots)
    pub callee_local_slots: usize,
}

/// Emit a JIT-to-JIT call with runtime check for compiled callee.
/// 
/// With the fiber.stack ABI, we must:
/// 1. Push a new frame in fiber.stack via vo_jit_push_frame
/// 2. Copy args from caller's frame to callee's frame
/// 3. Call the callee (JIT or VM)
/// 4. Pop the frame via vo_jit_pop_frame on success
/// 5. Copy return values back to caller's frame
/// 
/// If jit_func_table[func_id] != null: direct JIT call
/// If jit_func_table[func_id] == null: fallback to VM via call_vm helper
/// 
/// When callee returns Call/WaitIo, we propagate it to the caller.
pub fn emit_jit_call_with_fallback<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    config: JitCallWithFallbackConfig,
) {
    let ctx = emitter.ctx_param();
    let panic_ret_val = emitter.panic_return_value();
    
    // IMPORTANT: Read args BEFORE calling push_frame, because push_frame may
    // reallocate fiber.stack, making caller's args_ptr a dangling pointer.
    let mut arg_values = Vec::with_capacity(config.arg_slots);
    for i in 0..config.arg_slots {
        arg_values.push(emitter.read_var((config.arg_start + i) as u16));
    }
    
    // Load push_frame_fn from JitContext
    let push_frame_fn_ptr = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_PUSH_FRAME_FN
    );
    
    // Call vo_jit_push_frame to allocate callee frame in fiber.stack
    // Also passes caller_resume_pc to update caller's frame.pc for nested Call handling
    let push_frame_sig = import_push_frame_sig(emitter);
    
    let func_id_val = emitter.builder().ins().iconst(types::I32, config.func_id as i64);
    let local_slots_val = emitter.builder().ins().iconst(types::I32, config.callee_local_slots as i64);
    let ret_reg_val = emitter.builder().ins().iconst(types::I32, config.arg_start as i64);
    let ret_slots_val = emitter.builder().ins().iconst(types::I32, config.call_ret_slots as i64);
    // Resume at current_pc + 1 (after this call instruction)
    let current_pc = emitter.current_pc();
    let caller_resume_pc_val = emitter.builder().ins().iconst(types::I32, (current_pc + 1) as i64);
    
    let push_call = emitter.builder().ins().call_indirect(
        push_frame_sig, push_frame_fn_ptr,
        &[ctx, func_id_val, local_slots_val, ret_reg_val, ret_slots_val, caller_resume_pc_val]
    );
    let callee_args_ptr = emitter.builder().inst_results(push_call)[0];
    
    // Copy pre-read args to callee's frame
    for (i, val) in arg_values.iter().enumerate() {
        emitter.builder().ins().store(MemFlags::trusted(), *val, callee_args_ptr, (i * 8) as i32);
    }
    
    // Create ret_slot for return values (temporary Cranelift slot, OK for local use)
    let ret_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (config.func_ret_slots.max(1) * 8) as u32,
        8,
    ));
    let ret_ptr = emitter.builder().ins().stack_addr(types::I64, ret_slot, 0);
    
    // Load jit_func_table pointer from ctx
    let jit_func_table = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_FUNC_TABLE
    );
    
    // Calculate address: jit_func_table + func_id * 8
    let func_id_i64 = emitter.builder().ins().iconst(types::I64, config.func_id as i64);
    let offset = emitter.builder().ins().imul_imm(func_id_i64, 8);
    let func_ptr_addr = emitter.builder().ins().iadd(jit_func_table, offset);
    
    // Load function pointer
    let jit_func_ptr = emitter.builder().ins().load(types::I64, MemFlags::trusted(), func_ptr_addr, 0);
    
    // Check if null
    let zero = emitter.builder().ins().iconst(types::I64, 0);
    let is_null = emitter.builder().ins().icmp(IntCC::Equal, jit_func_ptr, zero);
    
    // Create blocks
    let jit_call_block = emitter.builder().create_block();
    let vm_call_block = emitter.builder().create_block();
    let merge_block = emitter.builder().create_block();
    
    emitter.builder().ins().brif(is_null, vm_call_block, &[], jit_call_block, &[]);
    
    // === JIT-to-JIT call path ===
    emitter.builder().switch_to_block(jit_call_block);
    emitter.builder().seal_block(jit_call_block);
    
    // Create signature for JIT function: (ctx, args, ret) -> JitResult
    let sig = import_jit_func_sig(emitter);
    
    let jit_call = emitter.builder().ins().call_indirect(sig, jit_func_ptr, &[ctx, callee_args_ptr, ret_ptr]);
    let jit_result = emitter.builder().inst_results(jit_call)[0];
    
    // Check result: only OK should continue, all others propagate
    let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter.builder().ins().icmp(IntCC::Equal, jit_result, ok_val);
    
    let jit_non_ok_block = emitter.builder().create_block();
    let jit_ok_block = emitter.builder().create_block();
    
    emitter.builder().ins().brif(is_ok, jit_ok_block, &[], jit_non_ok_block, &[]);
    
    // JIT non-OK path - propagate result (Panic, Call, or WaitIo)
    // Callee's frame is already in fiber.stack, VM will handle it.
    // NOTE: Caller's frame.pc was already set by push_frame to caller_resume_pc,
    // so VM can continue at the right place after callee returns.
    emitter.builder().switch_to_block(jit_non_ok_block);
    emitter.builder().seal_block(jit_non_ok_block);
    emitter.builder().ins().return_(&[jit_result]);
    
    // JIT OK path
    emitter.builder().switch_to_block(jit_ok_block);
    emitter.builder().seal_block(jit_ok_block);
    emitter.builder().ins().jump(merge_block, &[]);
    
    // === VM call path ===
    emitter.builder().switch_to_block(vm_call_block);
    emitter.builder().seal_block(vm_call_block);
    
    let call_vm_func = emitter.helpers().call_vm.unwrap();
    let func_id_val_vm = emitter.builder().ins().iconst(types::I32, config.func_id as i64);
    let arg_count = emitter.builder().ins().iconst(types::I32, config.arg_slots as i64);
    let ret_count = emitter.builder().ins().iconst(types::I32, config.func_ret_slots as i64);
    
    let vm_call = emitter.builder().ins().call(call_vm_func, &[ctx, func_id_val_vm, callee_args_ptr, arg_count, ret_ptr, ret_count]);
    let vm_result = emitter.builder().inst_results(vm_call)[0];
    
    // Check VM result for panic
    let panic_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_PANIC as i64);
    let vm_is_panic = emitter.builder().ins().icmp(IntCC::Equal, vm_result, panic_val);
    
    let vm_panic_block = emitter.builder().create_block();
    let vm_ok_block = emitter.builder().create_block();
    
    emitter.builder().ins().brif(vm_is_panic, vm_panic_block, &[], vm_ok_block, &[]);
    
    // VM panic path
    emitter.builder().switch_to_block(vm_panic_block);
    emitter.builder().seal_block(vm_panic_block);
    let vm_panic_val = emitter.builder().ins().iconst(types::I32, panic_ret_val as i64);
    emitter.builder().ins().return_(&[vm_panic_val]);
    
    // VM OK path
    emitter.builder().switch_to_block(vm_ok_block);
    emitter.builder().seal_block(vm_ok_block);
    emitter.builder().ins().jump(merge_block, &[]);
    
    // === Merge block - pop frame and copy return values ===
    emitter.builder().switch_to_block(merge_block);
    emitter.builder().seal_block(merge_block);
    
    // Pop callee's frame
    let pop_frame_fn_ptr = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_POP_FRAME_FN
    );
    let pop_frame_sig = import_pop_frame_sig(emitter);
    emitter.builder().ins().call_indirect(pop_frame_sig, pop_frame_fn_ptr, &[ctx]);
    
    // Copy return values to caller's frame
    for i in 0..config.call_ret_slots {
        let val = emitter.builder().ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
        emitter.write_var((config.arg_start + i) as u16, val);
    }
}

/// Check call result and handle non-Ok cases.
/// 
/// JitResult: Ok=0, Panic=1, Call=2, WaitIo=3
/// 
/// For non-Ok results:
/// - If spill_vars is true: spill all variables to memory before returning
/// - Return the JitResult directly for VM to handle
fn check_call_result<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    result: Value,
    spill_vars: bool,
) {
    let ok_block = emitter.builder().create_block();
    let non_ok_block = emitter.builder().create_block();
    
    let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter.builder().ins().icmp(IntCC::Equal, result, ok_val);
    emitter.builder().ins().brif(is_ok, ok_block, &[], non_ok_block, &[]);
    
    // Non-Ok path
    emitter.builder().switch_to_block(non_ok_block);
    emitter.builder().seal_block(non_ok_block);
    
    if spill_vars {
        emitter.spill_all_vars();
    }
    
    emitter.builder().ins().return_(&[result]);
    
    // Ok path - continue execution
    emitter.builder().switch_to_block(ok_block);
    emitter.builder().seal_block(ok_block);
}
