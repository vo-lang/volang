//! Shared call emission helpers for FunctionCompiler and LoopCompiler.
//!
//! This module consolidates call_closure, call_iface, and related logic
//! to ensure consistent behavior and reduce bug risk from code duplication.

use cranelift_codegen::ir::{types, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Value};
use cranelift_codegen::ir::condcodes::IntCC;

use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitContext;

use crate::translator::IrEmitter;

/// Configuration for call emission - captures the differences between compilers.
pub struct CallConfig {
    /// Resume PC to store before call (for WaitIo handling). None = don't set.
    pub resume_pc: Option<usize>,
    /// Whether to spill variables on non-ok result before returning.
    pub spill_on_non_ok: bool,
}

/// Emit a closure call instruction.
/// 
/// Both FunctionCompiler and LoopCompiler should use this to ensure consistent behavior.
pub fn emit_call_closure<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    inst: &Instruction,
    config: CallConfig,
) {
    let call_closure_func = match emitter.helpers().call_closure {
        Some(f) => f,
        None => return,
    };
    
    let closure_ref = emitter.read_var(inst.a);
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    
    let builder = emitter.builder();
    
    // Create stack slots for args and return values
    let arg_slot = builder.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (arg_slots.max(1) * 8) as u32,
        8,
    ));
    let ret_slot = builder.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (ret_slots.max(1) * 8) as u32,
        8,
    ));
    
    // Store arguments to stack slot
    for i in 0..arg_slots {
        let val = emitter.read_var((arg_start + i) as u16);
        emitter.builder().ins().stack_store(val, arg_slot, (i * 8) as i32);
    }
    
    let ctx = emitter.ctx_param();
    let args_ptr = emitter.builder().ins().stack_addr(types::I64, arg_slot, 0);
    let ret_ptr = emitter.builder().ins().stack_addr(types::I64, ret_slot, 0);
    let arg_count = emitter.builder().ins().iconst(types::I32, arg_slots as i64);
    let ret_count = emitter.builder().ins().iconst(types::I32, ret_slots as i64);
    
    // Set resume_pc if configured (for WaitIo handling)
    if let Some(resume_pc) = config.resume_pc {
        let resume_pc_val = emitter.builder().ins().iconst(types::I32, resume_pc as i64);
        emitter.builder().ins().store(MemFlags::trusted(), resume_pc_val, ctx, JitContext::OFFSET_CALL_RESUME_PC);
    }
    
    let call = emitter.builder().ins().call(call_closure_func, &[ctx, closure_ref, args_ptr, arg_count, ret_ptr, ret_count]);
    let result = emitter.builder().inst_results(call)[0];
    
    check_call_result(emitter, result, config.spill_on_non_ok);
    
    // Copy return values back
    for i in 0..ret_slots {
        let val = emitter.builder().ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
        emitter.write_var((arg_start + i) as u16, val);
    }
}

/// Emit an interface method call instruction.
/// 
/// Both FunctionCompiler and LoopCompiler should use this to ensure consistent behavior.
pub fn emit_call_iface<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    inst: &Instruction,
    config: CallConfig,
) {
    let call_iface_func = match emitter.helpers().call_iface {
        Some(f) => f,
        None => return,
    };
    
    let slot0 = emitter.read_var(inst.a);
    let slot1 = emitter.read_var(inst.a + 1);
    let method_idx = inst.flags as u32;
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    
    let builder = emitter.builder();
    
    // Create stack slots for args and return values
    let arg_slot = builder.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (arg_slots.max(1) * 8) as u32,
        8,
    ));
    let ret_slot = builder.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (ret_slots.max(1) * 8) as u32,
        8,
    ));
    
    // Store arguments to stack slot
    for i in 0..arg_slots {
        let val = emitter.read_var((arg_start + i) as u16);
        emitter.builder().ins().stack_store(val, arg_slot, (i * 8) as i32);
    }
    
    let ctx = emitter.ctx_param();
    let args_ptr = emitter.builder().ins().stack_addr(types::I64, arg_slot, 0);
    let ret_ptr = emitter.builder().ins().stack_addr(types::I64, ret_slot, 0);
    let method_idx_val = emitter.builder().ins().iconst(types::I32, method_idx as i64);
    let arg_count = emitter.builder().ins().iconst(types::I32, arg_slots as i64);
    let ret_count = emitter.builder().ins().iconst(types::I32, ret_slots as i64);
    let func_id = emitter.builder().ins().iconst(types::I32, 0);
    
    // Set resume_pc if configured (for WaitIo handling)
    if let Some(resume_pc) = config.resume_pc {
        let resume_pc_val = emitter.builder().ins().iconst(types::I32, resume_pc as i64);
        emitter.builder().ins().store(MemFlags::trusted(), resume_pc_val, ctx, JitContext::OFFSET_CALL_RESUME_PC);
    }
    
    let call = emitter.builder().ins().call(call_iface_func, &[
        ctx, slot0, slot1, method_idx_val, args_ptr, arg_count, ret_ptr, ret_count, func_id
    ]);
    let result = emitter.builder().inst_results(call)[0];
    
    check_call_result(emitter, result, config.spill_on_non_ok);
    
    // Copy return values back
    for i in 0..ret_slots {
        let val = emitter.builder().ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
        emitter.write_var((arg_start + i) as u16, val);
    }
}

/// Emit an extern function call instruction.
/// 
/// Both FunctionCompiler and LoopCompiler should use this to ensure consistent behavior.
pub fn emit_call_extern<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    inst: &Instruction,
    config: CallExternConfig,
) {
    let call_extern_func = match emitter.helpers().call_extern {
        Some(f) => f,
        None => return,
    };
    
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
        
        // Check for WaitIo (result == 3)
        let wait_io_val = emitter.builder().ins().iconst(types::I32, 3);
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
/// 3. VM executes the callee and resumes JIT at resume_pc
pub fn emit_call_via_vm<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    config: CallViaVmConfig,
) {
    let set_call_request_func = match emitter.helpers().set_call_request {
        Some(f) => f,
        None => return,
    };
    
    // Spill all variables to fiber.stack before returning Call
    emitter.spill_all_vars();
    
    let ctx = emitter.ctx_param();
    let func_id_val = emitter.builder().ins().iconst(types::I32, config.func_id as i64);
    let arg_start_val = emitter.builder().ins().iconst(types::I32, config.arg_start as i64);
    let resume_pc_val = emitter.builder().ins().iconst(types::I32, config.resume_pc as i64);
    let ret_slots_val = emitter.builder().ins().iconst(types::I32, config.ret_slots as i64);
    
    emitter.builder().ins().call(set_call_request_func, &[ctx, func_id_val, arg_start_val, resume_pc_val, ret_slots_val]);
    
    // Return JitResult::Call = 2
    let call_result = emitter.builder().ins().iconst(types::I32, 2);
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
pub struct JitCallConfig {
    pub func_id: u32,
    pub arg_start: usize,
    pub arg_slots: usize,
    pub call_ret_slots: usize,
    pub func_ret_slots: usize,
    pub resume_pc: usize,
    /// If true, use Call request for VM fallback (LoopCompiler).
    /// If false, use call_vm helper (FunctionCompiler).
    pub use_call_request_fallback: bool,
}

/// Emit a JIT-to-JIT call with runtime check for compiled callee.
/// 
/// If jit_func_table[func_id] != null: direct JIT call
/// If jit_func_table[func_id] == null: fallback to VM (via call_vm or Call request)
/// 
/// IMPORTANT: This function maintains the original structure to avoid phi node explosion:
/// - Stack slots created BEFORE branching (shared by both paths)
/// - Arguments stored BEFORE branching
/// - Return values written in merge_block (not in individual ok_blocks)
pub fn emit_call_with_jit_check<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    config: JitCallConfig,
) {
    let ctx = emitter.ctx_param();
    let panic_ret_val = emitter.panic_return_value();
    
    // === BEFORE BRANCHING: Create shared stack slots and store arguments ===
    // This is critical - doing this after branching causes phi node explosion
    let arg_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (config.arg_slots.max(1) * 8) as u32,
        8,
    ));
    let ret_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (config.func_ret_slots.max(1) * 8) as u32,
        8,
    ));
    
    // Store arguments to stack slot BEFORE branching
    for i in 0..config.arg_slots {
        let val = emitter.read_var((config.arg_start + i) as u16);
        emitter.builder().ins().stack_store(val, arg_slot, (i * 8) as i32);
    }
    
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
    
    // Create blocks for the two paths
    let jit_call_block = emitter.builder().create_block();
    let vm_call_block = emitter.builder().create_block();
    let merge_block = emitter.builder().create_block();
    
    // Branch based on whether function is compiled
    emitter.builder().ins().brif(is_null, vm_call_block, &[], jit_call_block, &[]);
    
    // === JIT-to-JIT call path ===
    emitter.builder().switch_to_block(jit_call_block);
    emitter.builder().seal_block(jit_call_block);
    
    // Use shared stack slots
    let args_ptr = emitter.builder().ins().stack_addr(types::I64, arg_slot, 0);
    let ret_ptr = emitter.builder().ins().stack_addr(types::I64, ret_slot, 0);
    
    // Create signature for JIT function: (ctx: ptr, args: ptr, ret: ptr) -> i32
    let sig = emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
        sig.returns.push(cranelift_codegen::ir::AbiParam::new(types::I32));
        sig
    });
    
    // Indirect call through function pointer
    let jit_call = emitter.builder().ins().call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
    let jit_result = emitter.builder().inst_results(jit_call)[0];
    
    // Check result for panic (result == 1)
    let one = emitter.builder().ins().iconst(types::I32, 1);
    let is_panic = emitter.builder().ins().icmp(IntCC::Equal, jit_result, one);
    
    let jit_panic_block = emitter.builder().create_block();
    let jit_ok_block = emitter.builder().create_block();
    
    emitter.builder().ins().brif(is_panic, jit_panic_block, &[], jit_ok_block, &[]);
    
    // JIT panic path
    emitter.builder().switch_to_block(jit_panic_block);
    emitter.builder().seal_block(jit_panic_block);
    let panic_val = emitter.builder().ins().iconst(types::I32, panic_ret_val as i64);
    emitter.builder().ins().return_(&[panic_val]);
    
    // JIT OK path - just jump to merge (return values loaded in merge_block)
    emitter.builder().switch_to_block(jit_ok_block);
    emitter.builder().seal_block(jit_ok_block);
    emitter.builder().ins().jump(merge_block, &[]);
    
    // === VM call path ===
    emitter.builder().switch_to_block(vm_call_block);
    emitter.builder().seal_block(vm_call_block);
    
    if config.use_call_request_fallback {
        // LoopCompiler path: use Call request mechanism
        // This path returns directly, doesn't go to merge_block
        emitter.spill_all_vars();
        
        let set_call_request_func = emitter.helpers().set_call_request.unwrap();
        let func_id_val = emitter.builder().ins().iconst(types::I32, config.func_id as i64);
        let arg_start_val = emitter.builder().ins().iconst(types::I32, config.arg_start as i64);
        let resume_pc_val = emitter.builder().ins().iconst(types::I32, config.resume_pc as i64);
        let ret_slots_val = emitter.builder().ins().iconst(types::I32, config.call_ret_slots as i64);
        
        emitter.builder().ins().call(set_call_request_func, &[ctx, func_id_val, arg_start_val, resume_pc_val, ret_slots_val]);
        
        let call_result = emitter.builder().ins().iconst(types::I32, 2); // JitResult::Call
        emitter.builder().ins().return_(&[call_result]);
    } else {
        // FunctionCompiler path: use call_vm helper with shared stack slots
        let call_vm_func = emitter.helpers().call_vm.unwrap();
        
        // Use shared stack slots (args already stored before branching)
        let args_ptr_vm = emitter.builder().ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr_vm = emitter.builder().ins().stack_addr(types::I64, ret_slot, 0);
        let func_id_val = emitter.builder().ins().iconst(types::I32, config.func_id as i64);
        let arg_count = emitter.builder().ins().iconst(types::I32, config.arg_slots as i64);
        let ret_count = emitter.builder().ins().iconst(types::I32, config.func_ret_slots as i64);
        
        let vm_call = emitter.builder().ins().call(call_vm_func, &[ctx, func_id_val, args_ptr_vm, arg_count, ret_ptr_vm, ret_count]);
        let vm_result = emitter.builder().inst_results(vm_call)[0];
        
        // Check VM result for panic (define one_vm here, not reuse 'one' from jit_call_block)
        let one_vm = emitter.builder().ins().iconst(types::I32, 1);
        let vm_is_panic = emitter.builder().ins().icmp(IntCC::Equal, vm_result, one_vm);
        
        let vm_panic_block = emitter.builder().create_block();
        let vm_ok_block = emitter.builder().create_block();
        
        emitter.builder().ins().brif(vm_is_panic, vm_panic_block, &[], vm_ok_block, &[]);
        
        // VM panic path
        emitter.builder().switch_to_block(vm_panic_block);
        emitter.builder().seal_block(vm_panic_block);
        let vm_panic_val = emitter.builder().ins().iconst(types::I32, panic_ret_val as i64);
        emitter.builder().ins().return_(&[vm_panic_val]);
        
        // VM OK path - just jump to merge (return values loaded in merge_block)
        emitter.builder().switch_to_block(vm_ok_block);
        emitter.builder().seal_block(vm_ok_block);
        emitter.builder().ins().jump(merge_block, &[]);
    }
    
    // === Merge block (load return values from shared ret_slot) ===
    emitter.builder().switch_to_block(merge_block);
    emitter.builder().seal_block(merge_block);
    
    // Load return values and write to variables - ONLY here, not in ok_blocks
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
    
    let zero = emitter.builder().ins().iconst(types::I32, 0);
    let is_ok = emitter.builder().ins().icmp(IntCC::Equal, result, zero);
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
