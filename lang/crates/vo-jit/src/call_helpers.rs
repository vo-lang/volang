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

/// Create signature for vo_jit_pop_frame: (ctx, caller_bp) -> ()
pub fn import_pop_frame_sig<'a, E: IrEmitter<'a>>(emitter: &mut E) -> SigRef {
    emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ctx
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // caller_bp
        sig
    })
}

/// Create signature for vo_jit_push_resume_point: (ctx, func_id, resume_pc, bp, caller_bp, ret_reg, ret_slots) -> ()
pub fn import_push_resume_point_sig<'a, E: IrEmitter<'a>>(emitter: &mut E) -> SigRef {
    emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ctx
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // func_id
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // resume_pc
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // bp
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // caller_bp
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // ret_reg
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // ret_slots
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
/// CallClosure: inst.a = closure_slot, inst.b = arg_start, inst.c = (arg_slots << 8) | ret_slots
/// 
/// Strategy (Phase 4 MVP): Always use VM fallback via Call mechanism.
/// JIT-to-JIT direct calls for closures will be Phase 5.
pub fn emit_call_closure<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    inst: &Instruction,
    _config: CallConfig,
) {
    let closure_slot = inst.a as usize;
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    
    let ctx = emitter.ctx_param();
    
    // Read closure_ref
    let closure_ref = emitter.read_var(closure_slot as u16);
    
    // Check nil closure
    let zero = emitter.builder().ins().iconst(types::I64, 0);
    let is_nil = emitter.builder().ins().icmp(IntCC::Equal, closure_ref, zero);
    let nil_block = emitter.builder().create_block();
    let continue_block = emitter.builder().create_block();
    emitter.builder().ins().brif(is_nil, nil_block, &[], continue_block, &[]);
    
    // Nil closure -> panic
    emitter.builder().switch_to_block(nil_block);
    emitter.builder().seal_block(nil_block);
    let panic_result = emitter.builder().ins().iconst(types::I32, JIT_RESULT_PANIC as i64);
    emitter.builder().ins().return_(&[panic_result]);
    
    emitter.builder().switch_to_block(continue_block);
    emitter.builder().seal_block(continue_block);
    
    // Spill all variables to fiber.stack before returning Call
    // This ensures args are in memory where VM expects them
    emitter.spill_all_vars();
    
    // Get func_id from closure
    let closure_get_func_id = emitter.helpers().closure_get_func_id.expect("closure_get_func_id helper not registered");
    let call = emitter.builder().ins().call(closure_get_func_id, &[closure_ref]);
    let func_id = emitter.builder().inst_results(call)[0];
    
    // Call vo_set_closure_call_request to set up VM fallback
    let set_closure_call_request = emitter.helpers().set_closure_call_request.expect("set_closure_call_request helper not registered");
    let arg_start_val = emitter.builder().ins().iconst(types::I32, arg_start as i64);
    let resume_pc = emitter.current_pc() + 1;
    let resume_pc_val = emitter.builder().ins().iconst(types::I32, resume_pc as i64);
    let ret_slots_val = emitter.builder().ins().iconst(types::I32, ret_slots as i64);
    let arg_slots_val = emitter.builder().ins().iconst(types::I32, arg_slots as i64);
    
    emitter.builder().ins().call(
        set_closure_call_request,
        &[ctx, func_id, arg_start_val, resume_pc_val, ret_slots_val, arg_slots_val, closure_ref]
    );
    
    // Return Call to let VM handle the closure call
    let call_result = emitter.builder().ins().iconst(types::I32, JIT_RESULT_CALL as i64);
    emitter.builder().ins().return_(&[call_result]);
    
    // Create a dummy continuation block for any instructions after this call.
    // This block is unreachable since we always return Call.
    // VM will handle those instructions after closure call completes.
    let continuation = emitter.builder().create_block();
    emitter.builder().switch_to_block(continuation);
    emitter.builder().seal_block(continuation);
}

/// Emit an interface method call instruction.
/// 
/// CallIface: inst.a = iface_slot (2 slots), inst.b = arg_start, inst.c = (arg_slots << 8) | ret_slots
///            inst.flags = method_idx
/// 
/// Strategy (Phase 4 MVP): Always use VM fallback via Call mechanism.
/// JIT-to-JIT direct calls for interface methods will be Phase 5.
pub fn emit_call_iface<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    inst: &Instruction,
    _config: CallConfig,
) {
    let iface_slot = inst.a as usize;
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    let method_idx = inst.flags as u32;
    
    let ctx = emitter.ctx_param();
    
    // Read interface slots
    let slot0 = emitter.read_var(iface_slot as u16);
    let slot1 = emitter.read_var((iface_slot + 1) as u16);
    
    // Spill all variables to fiber.stack before returning Call
    emitter.spill_all_vars();
    
    // Get func_id from itab
    let iface_get_func_id = emitter.helpers().iface_get_func_id.expect("iface_get_func_id helper not registered");
    let method_idx_val = emitter.builder().ins().iconst(types::I32, method_idx as i64);
    let call = emitter.builder().ins().call(iface_get_func_id, &[ctx, slot0, method_idx_val]);
    let func_id = emitter.builder().inst_results(call)[0];
    
    // Call vo_set_iface_call_request to set up VM fallback
    // slot1 is the receiver value
    let set_iface_call_request = emitter.helpers().set_iface_call_request.expect("set_iface_call_request helper not registered");
    let arg_start_val = emitter.builder().ins().iconst(types::I32, arg_start as i64);
    let resume_pc = emitter.current_pc() + 1;
    let resume_pc_val = emitter.builder().ins().iconst(types::I32, resume_pc as i64);
    let ret_slots_val = emitter.builder().ins().iconst(types::I32, ret_slots as i64);
    let arg_slots_val = emitter.builder().ins().iconst(types::I32, arg_slots as i64);
    
    emitter.builder().ins().call(
        set_iface_call_request,
        &[ctx, func_id, arg_start_val, resume_pc_val, ret_slots_val, arg_slots_val, slot1]
    );
    
    // Return Call to let VM handle the interface call
    let call_result = emitter.builder().ins().iconst(types::I32, JIT_RESULT_CALL as i64);
    emitter.builder().ins().return_(&[call_result]);
    
    // Create a dummy continuation block for any instructions after this call.
    // This block is unreachable since we always return Call.
    // VM will handle those instructions after interface call completes.
    let continuation = emitter.builder().create_block();
    emitter.builder().switch_to_block(continuation);
    emitter.builder().seal_block(continuation);
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
/// Fast path (JIT-to-JIT):
/// - Args passed via native stack slot (no fiber.stack access)
/// - No push_frame/pop_frame calls
/// 
/// Slow path (Call/WaitIo or VM fallback):
/// - Spill vars and materialize callee frame to fiber.stack
/// 
/// If jit_func_table[func_id] != null: direct JIT call
/// If jit_func_table[func_id] == null: fallback to VM via call_vm helper
pub fn emit_jit_call_with_fallback<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    config: JitCallWithFallbackConfig,
) {
    let ctx = emitter.ctx_param();
    let panic_ret_val = emitter.panic_return_value();
    
    // Save caller_bp for slow path (needed for resume_point)
    let caller_bp = emitter.builder().ins().load(
        types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_BP
    );
    
    // Read args from caller's locals_slot
    let mut arg_values = Vec::with_capacity(config.arg_slots);
    for i in 0..config.arg_slots {
        arg_values.push(emitter.read_var((config.arg_start + i) as u16));
    }
    
    // Create args_slot on native stack (NOT fiber.stack) for passing args to callee
    let args_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (config.callee_local_slots.max(1) * 8) as u32,
        8,
    ));
    let args_ptr = emitter.builder().ins().stack_addr(types::I64, args_slot, 0);
    
    // Copy args to native stack slot
    for (i, val) in arg_values.iter().enumerate() {
        emitter.builder().ins().stack_store(*val, args_slot, (i * 8) as i32);
    }
    
    // Create ret_slot for return values
    let ret_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (config.func_ret_slots.max(1) * 8) as u32,
        8,
    ));
    let ret_ptr = emitter.builder().ins().stack_addr(types::I64, ret_slot, 0);
    
    // Constants for slow path
    let func_id_val = emitter.builder().ins().iconst(types::I32, config.func_id as i64);
    let local_slots_val = emitter.builder().ins().iconst(types::I32, config.callee_local_slots as i64);
    let ret_reg_val = emitter.builder().ins().iconst(types::I32, config.arg_start as i64);
    let ret_slots_val = emitter.builder().ins().iconst(types::I32, config.call_ret_slots as i64);
    let current_pc = emitter.current_pc();
    let caller_resume_pc_val = emitter.builder().ins().iconst(types::I32, (current_pc + 1) as i64);
    
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
    
    // === JIT-to-JIT call path (FAST PATH) ===
    emitter.builder().switch_to_block(jit_call_block);
    emitter.builder().seal_block(jit_call_block);
    
    // Save old fiber_sp for restoration after call
    let old_fiber_sp = emitter.builder().ins().load(
        types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_FIBER_SP
    );
    
    // Inline update ctx.jit_bp and ctx.fiber_sp for callee's correct saved_jit_bp
    // new_bp = old_fiber_sp, new_sp = new_bp + callee_local_slots
    let new_bp = old_fiber_sp;
    let new_sp = emitter.builder().ins().iadd_imm(new_bp, config.callee_local_slots as i64);
    emitter.builder().ins().store(MemFlags::trusted(), new_bp, ctx, JitContext::OFFSET_JIT_BP);
    emitter.builder().ins().store(MemFlags::trusted(), new_sp, ctx, JitContext::OFFSET_FIBER_SP);
    
    // Create signature for JIT function: (ctx, args, ret) -> JitResult
    let sig = import_jit_func_sig(emitter);
    
    // Call with args on native stack (args_ptr points to args_slot)
    let jit_call = emitter.builder().ins().call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
    let jit_result = emitter.builder().inst_results(jit_call)[0];
    
    // Check result: only OK should continue, all others propagate
    let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter.builder().ins().icmp(IntCC::Equal, jit_result, ok_val);
    
    let jit_non_ok_block = emitter.builder().create_block();
    let jit_ok_block = emitter.builder().create_block();
    
    emitter.builder().ins().brif(is_ok, jit_ok_block, &[], jit_non_ok_block, &[]);
    
    // JIT non-OK path (SLOW PATH) - propagate result (Panic, Call, or WaitIo)
    // Need to materialize caller and callee frames to fiber.stack for VM
    emitter.builder().switch_to_block(jit_non_ok_block);
    emitter.builder().seal_block(jit_non_ok_block);
    
    // Spill caller's variables to fiber.stack
    emitter.spill_all_vars();
    
    // Materialize callee's frame in fiber.stack (slow path only)
    // This calls push_frame to create the callee frame that VM expects
    let push_frame_fn_ptr = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_PUSH_FRAME_FN
    );
    let push_frame_sig = import_push_frame_sig(emitter);
    let push_call = emitter.builder().ins().call_indirect(
        push_frame_sig, push_frame_fn_ptr,
        &[ctx, func_id_val, local_slots_val, ret_reg_val, ret_slots_val, caller_resume_pc_val]
    );
    let callee_fiber_args_ptr = emitter.builder().inst_results(push_call)[0];
    
    // Copy args from native stack to fiber.stack (callee needs them there)
    for i in 0..config.arg_slots {
        let val = emitter.builder().ins().stack_load(types::I64, args_slot, (i * 8) as i32);
        emitter.builder().ins().store(MemFlags::trusted(), val, callee_fiber_args_ptr, (i * 8) as i32);
    }
    
    // Push resume point
    let push_resume_point_fn_ptr = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_PUSH_RESUME_POINT_FN
    );
    let push_resume_point_sig = import_push_resume_point_sig(emitter);
    
    // callee's bp is current ctx.jit_bp (set by push_frame we just called)
    let callee_bp = emitter.builder().ins().load(
        types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_BP
    );
    
    emitter.builder().ins().call_indirect(
        push_resume_point_sig, push_resume_point_fn_ptr,
        &[ctx, func_id_val, caller_resume_pc_val, callee_bp, caller_bp, ret_reg_val, ret_slots_val]
    );
    
    emitter.builder().ins().return_(&[jit_result]);
    
    // JIT OK path (FAST PATH) - restore ctx.jit_bp and ctx.fiber_sp
    emitter.builder().switch_to_block(jit_ok_block);
    emitter.builder().seal_block(jit_ok_block);
    
    // Restore ctx.jit_bp and ctx.fiber_sp (inline pop)
    emitter.builder().ins().store(MemFlags::trusted(), caller_bp, ctx, JitContext::OFFSET_JIT_BP);
    emitter.builder().ins().store(MemFlags::trusted(), old_fiber_sp, ctx, JitContext::OFFSET_FIBER_SP);
    
    emitter.builder().ins().jump(merge_block, &[]);
    
    // === VM call path ===
    // VM needs args in fiber.stack, so we call push_frame here
    emitter.builder().switch_to_block(vm_call_block);
    emitter.builder().seal_block(vm_call_block);
    
    // Push frame for VM (VM expects fiber.stack layout)
    let vm_push_frame_fn_ptr = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_PUSH_FRAME_FN
    );
    let vm_push_call = emitter.builder().ins().call_indirect(
        push_frame_sig, vm_push_frame_fn_ptr,
        &[ctx, func_id_val, local_slots_val, ret_reg_val, ret_slots_val, caller_resume_pc_val]
    );
    let vm_callee_args_ptr = emitter.builder().inst_results(vm_push_call)[0];
    
    // Copy args from native stack to fiber.stack for VM
    for i in 0..config.arg_slots {
        let val = emitter.builder().ins().stack_load(types::I64, args_slot, (i * 8) as i32);
        emitter.builder().ins().store(MemFlags::trusted(), val, vm_callee_args_ptr, (i * 8) as i32);
    }
    
    let call_vm_func = emitter.helpers().call_vm.unwrap();
    let arg_count = emitter.builder().ins().iconst(types::I32, config.arg_slots as i64);
    let ret_count = emitter.builder().ins().iconst(types::I32, config.func_ret_slots as i64);
    
    let vm_call = emitter.builder().ins().call(call_vm_func, &[ctx, func_id_val, vm_callee_args_ptr, arg_count, ret_ptr, ret_count]);
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
    
    // VM OK path - need to pop_frame since VM path called push_frame
    emitter.builder().switch_to_block(vm_ok_block);
    emitter.builder().seal_block(vm_ok_block);
    
    let pop_frame_fn_ptr = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_POP_FRAME_FN
    );
    let pop_frame_sig = import_pop_frame_sig(emitter);
    emitter.builder().ins().call_indirect(pop_frame_sig, pop_frame_fn_ptr, &[ctx, caller_bp]);
    
    emitter.builder().ins().jump(merge_block, &[]);
    
    // === Merge block - copy return values (no pop_frame for JIT fast path) ===
    emitter.builder().switch_to_block(merge_block);
    emitter.builder().seal_block(merge_block);
    
    // Copy return values to caller's locals_slot
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
