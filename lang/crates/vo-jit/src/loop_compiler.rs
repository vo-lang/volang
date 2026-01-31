//! Loop compiler for OSR (On-Stack Replacement).

use std::collections::HashMap;

use cranelift_codegen::ir::{types, Block, Function, InstBuilder, MemFlags, Value};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};

use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::jit_api::JitContext;
use crate::loop_analysis::LoopInfo;
use crate::translate::translate_inst;
use crate::translator::{HelperFuncs, IrEmitter, TranslateResult};
use crate::JitError;

pub const LOOP_RESULT_PANIC: u32 = u32::MAX;

pub type LoopFunc = extern "C" fn(*mut crate::JitContext, *mut u64) -> u32;

pub struct CompiledLoop {
    pub code_ptr: *const u8,
    pub loop_info: LoopInfo,
}

unsafe impl Send for CompiledLoop {}
unsafe impl Sync for CompiledLoop {}

pub struct LoopCompiler<'a> {
    builder: FunctionBuilder<'a>,
    func_def: &'a FunctionDef,
    vo_module: &'a VoModule,
    loop_info: &'a LoopInfo,
    vars: Vec<Variable>,
    blocks: HashMap<usize, Block>,
    entry_block: Block,
    exit_block: Block,
    current_pc: usize,
    locals_ptr: Value,
    ctx_ptr: Value,
    helpers: HelperFuncs,
    reg_consts: HashMap<u16, i64>,
}

impl<'a> LoopCompiler<'a> {
    pub fn new(
        func: &'a mut Function,
        func_ctx: &'a mut FunctionBuilderContext,
        func_def: &'a FunctionDef,
        vo_module: &'a VoModule,
        loop_info: &'a LoopInfo,
        helpers: HelperFuncs,
    ) -> Self {
        let mut builder = FunctionBuilder::new(func, func_ctx);
        let entry_block = builder.create_block();
        let exit_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        
        Self {
            builder,
            func_def,
            vo_module,
            loop_info,
            vars: Vec::new(),
            blocks: HashMap::new(),
            entry_block,
            exit_block,
            current_pc: 0,
            locals_ptr: Value::from_u32(0),
            ctx_ptr: Value::from_u32(0),
            helpers,
            reg_consts: HashMap::new(),
        }
    }

    pub fn compile(mut self) -> Result<(), JitError> {
        self.declare_variables();
        self.scan_jump_targets();
        
        // Exactly like func_compiler: entry_block -> prologue -> sequential compile
        self.builder.switch_to_block(self.entry_block);
        self.emit_prologue();
        
        let mut block_terminated = false;  // Same as func_compiler!
        
        // Compile from after LOOP_BEGIN hint to after LOOP_END hint (includes back-edge jump)
        for pc in (self.loop_info.begin_pc + 1)..(self.loop_info.end_pc + 2) {
            self.current_pc = pc;
            
            if let Some(&block) = self.blocks.get(&pc) {
                if !block_terminated {
                    self.builder.ins().jump(block, &[]);
                }
                self.builder.switch_to_block(block);
                block_terminated = false;
            } else if block_terminated {
                let dummy = self.builder.create_block();
                self.builder.switch_to_block(dummy);
                block_terminated = false;
            }
            
            let inst = &self.func_def.code[pc];
            if inst.opcode() == Opcode::Hint {
                continue;
            }
            
            block_terminated = self.translate_instruction(inst)?;
        }
        
        // If last instruction didn't terminate, jump to exit
        if !block_terminated {
            self.builder.ins().jump(self.exit_block, &[]);
        }
        
        // Exit block: store vars back to memory, then return exit_pc
        self.builder.switch_to_block(self.exit_block);
        self.store_vars_to_memory();
        let exit_pc = self.builder.ins().iconst(types::I32, self.loop_info.exit_pc as i64);
        self.builder.ins().return_(&[exit_pc]);
        
        self.builder.seal_all_blocks();
        self.builder.finalize();
        
        Ok(())
    }

    fn declare_variables(&mut self) {
        let num_slots = self.func_def.local_slots as usize;
        self.vars.reserve(num_slots);
        
        for i in 0..num_slots {
            let var = Variable::from_u32(i as u32);
            self.builder.declare_var(var, types::I64);
            self.vars.push(var);
        }
    }

    fn scan_jump_targets(&mut self) {
        let loop_end = self.loop_info.end_pc + 2;
        for pc in (self.loop_info.begin_pc + 1)..loop_end {
            let inst = &self.func_def.code[pc];
            match inst.opcode() {
                Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot => {
                    let offset = inst.imm32();
                    let raw_target = (pc as i32 + offset) as usize;
                    // Map LOOP_BEGIN to loop_header (begin_pc + 1)
                    let target = if raw_target == self.loop_info.begin_pc {
                        self.loop_info.begin_pc + 1
                    } else {
                        raw_target
                    };
                    // Create block for targets within loop body (after begin_pc)
                    if target > self.loop_info.begin_pc && target < loop_end {
                        self.ensure_block(target);
                    }
                }
                _ => {}
            }
        }
    }

    fn ensure_block(&mut self, pc: usize) -> Block {
        if let Some(&block) = self.blocks.get(&pc) {
            block
        } else {
            let block = self.builder.create_block();
            self.blocks.insert(pc, block);
            block
        }
    }

    fn emit_prologue(&mut self) {
        let params = self.builder.block_params(self.entry_block);
        self.ctx_ptr = params[0];
        self.locals_ptr = params[1];
        
        // Load all variables from memory - like func_compiler loads from args
        for i in 0..self.vars.len() {
            let offset = (i * 8) as i32;
            let val = self.builder.ins().load(types::I64, MemFlags::trusted(), self.locals_ptr, offset);
            self.builder.def_var(self.vars[i], val);
        }
    }
    
    fn store_vars_to_memory(&mut self) {
        // Write modified variables back to memory
        for &slot in &self.loop_info.live_out {
            let val = self.builder.use_var(self.vars[slot as usize]);
            let offset = (slot as i32) * 8;
            self.builder.ins().store(MemFlags::trusted(), val, self.locals_ptr, offset);
        }
    }

    fn translate_instruction(&mut self, inst: &Instruction) -> Result<bool, JitError> {
        match translate_inst(self, inst)? {
            TranslateResult::Completed => return Ok(false),
            TranslateResult::Terminated => return Ok(true),
            TranslateResult::Unhandled => {}
        }
        
        match inst.opcode() {
            Opcode::Jump => { self.jump(inst); Ok(true) }
            Opcode::JumpIf => { self.jump_if(inst); Ok(false) }
            Opcode::JumpIfNot => { self.jump_if_not(inst); Ok(false) }
            Opcode::Return => { self.ret(inst); Ok(true) }
            Opcode::Panic => { self.panic(inst); Ok(true) }
            Opcode::Call => { self.call(inst); Ok(false) }
            Opcode::CallExtern => { self.call_extern(inst); Ok(false) }
            Opcode::CallClosure => { self.call_closure(inst); Ok(false) }
            Opcode::CallIface => { self.call_iface(inst); Ok(false) }
            _ => {
                // Unsupported - exit to VM (variables already in memory)
                let ret_pc = self.builder.ins().iconst(types::I32, self.current_pc as i64);
                self.builder.ins().return_(&[ret_pc]);
                Ok(true)
            }
        }
    }

    fn jump(&mut self, inst: &Instruction) {
        let offset = inst.imm32();
        let raw_target = (self.current_pc as i32 + offset) as usize;
        let loop_end = self.loop_info.end_pc + 2;
        
        // Back-edge: jump to loop header (condition check)
        if raw_target == self.loop_info.begin_pc || raw_target == self.loop_info.begin_pc + 1 {
            self.do_emit_safepoint();
            let loop_header = self.blocks[&(self.loop_info.begin_pc + 1)];
            self.builder.ins().jump(loop_header, &[]);
        } else if raw_target <= self.loop_info.begin_pc || raw_target >= loop_end {
            // Jump outside loop - exit to VM
            self.store_vars_to_memory();
            let ret_pc = self.builder.ins().iconst(types::I32, raw_target as i64);
            self.builder.ins().return_(&[ret_pc]);
        } else {
            // Jump within loop body
            let block = self.blocks[&raw_target];
            self.builder.ins().jump(block, &[]);
        }
    }

    fn jump_if(&mut self, inst: &Instruction) {
        let cond = self.read_var(inst.a);
        let offset = inst.imm32();
        let target = (self.current_pc as i32 + offset) as usize;
        let loop_end = self.loop_info.end_pc + 2;
        
        let fall_through = self.builder.create_block();
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(IntCC::NotEqual, cond, zero);
        
        if target < self.loop_info.begin_pc || target >= loop_end {
            let exit_block = self.builder.create_block();
            self.builder.ins().brif(cmp, exit_block, &[], fall_through, &[]);
            
            self.builder.switch_to_block(exit_block);
            self.builder.seal_block(exit_block);
            self.store_vars_to_memory();
            let ret_pc = self.builder.ins().iconst(types::I32, target as i64);
            self.builder.ins().return_(&[ret_pc]);
        } else {
            let target_block = self.blocks[&target];
            self.builder.ins().brif(cmp, target_block, &[], fall_through, &[]);
        }
        
        self.builder.switch_to_block(fall_through);
        self.builder.seal_block(fall_through);
    }

    fn jump_if_not(&mut self, inst: &Instruction) {
        let cond = self.read_var(inst.a);
        let offset = inst.imm32();
        let target = (self.current_pc as i32 + offset) as usize;
        let loop_end = self.loop_info.end_pc + 2;
        
        let fall_through = self.builder.create_block();
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(IntCC::Equal, cond, zero);
        
        if target < self.loop_info.begin_pc || target >= loop_end {
            let exit_block = self.builder.create_block();
            self.builder.ins().brif(cmp, exit_block, &[], fall_through, &[]);
            
            self.builder.switch_to_block(exit_block);
            self.builder.seal_block(exit_block);
            self.store_vars_to_memory();
            let ret_pc = self.builder.ins().iconst(types::I32, target as i64);
            self.builder.ins().return_(&[ret_pc]);
        } else {
            let target_block = self.blocks[&target];
            self.builder.ins().brif(cmp, target_block, &[], fall_through, &[]);
        }
        
        self.builder.switch_to_block(fall_through);
        self.builder.seal_block(fall_through);
    }

    fn ret(&mut self, _inst: &Instruction) {
        // Return inside loop - store vars and return to VM
        self.store_vars_to_memory();
        let ret_pc = self.builder.ins().iconst(types::I32, self.current_pc as i64);
        self.builder.ins().return_(&[ret_pc]);
    }

    fn panic(&mut self, inst: &Instruction) {
        if let Some(panic_func) = self.helpers.panic {
            let ctx = self.ctx_ptr;
            // Panic message is an interface (2 slots): slot0=metadata, slot1=data
            // Note: Panic instruction uses inst.a for the register (not inst.b)
            let msg_slot0 = self.read_var(inst.a);
            let msg_slot1 = self.read_var(inst.a + 1);
            self.builder.ins().call(panic_func, &[ctx, msg_slot0, msg_slot1]);
        }
        let panic_val = self.builder.ins().iconst(types::I32, LOOP_RESULT_PANIC as i64);
        self.builder.ins().return_(&[panic_val]);
    }

    fn call(&mut self, inst: &Instruction) {
        self.do_emit_safepoint();
        
        let func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
        let arg_start = inst.b as usize;
        let arg_slots = (inst.c >> 8) as usize;
        let call_ret_slots = (inst.c & 0xFF) as usize;
        
        // Get target function ret_slots for buffer sizing
        let target_func = &self.vo_module.functions[func_id as usize];
        let func_ret_slots = target_func.ret_slots as usize;
        
        // Check if callee is jittable
        let callee_jittable = crate::is_func_jittable(target_func);
        
        if callee_jittable {
            self.call_with_jit_check(func_id, arg_start, arg_slots, call_ret_slots, func_ret_slots);
        } else {
            self.call_via_vm(func_id, arg_start, arg_slots, call_ret_slots, func_ret_slots);
        }
    }
    
    fn call_via_vm(&mut self, func_id: u32, arg_start: usize, arg_slots: usize, call_ret_slots: usize, func_ret_slots: usize) {
        let call_vm_func = match self.helpers.call_vm {
            Some(f) => f,
            None => return,
        };
        
        let arg_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (arg_slots.max(1) * 8) as u32,
            8,
        ));
        let ret_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (func_ret_slots.max(1) * 8) as u32,
            8,
        ));
        
        for i in 0..arg_slots {
            let val = self.read_var((arg_start + i) as u16);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        let args_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        let func_id_val = self.builder.ins().iconst(types::I32, func_id as i64);
        let arg_count = self.builder.ins().iconst(types::I32, arg_slots as i64);
        let ret_count = self.builder.ins().iconst(types::I32, func_ret_slots as i64);
        
        let ctx = self.ctx_ptr;
        let call = self.builder.ins().call(call_vm_func, &[ctx, func_id_val, args_ptr, arg_count, ret_ptr, ret_count]);
        let result = self.builder.inst_results(call)[0];
        
        self.check_call_result(result);
        
        for i in 0..call_ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.write_var((arg_start + i) as u16, val);
        }
    }
    
    fn call_with_jit_check(&mut self, func_id: u32, arg_start: usize, arg_slots: usize, call_ret_slots: usize, func_ret_slots: usize) {
        use cranelift_codegen::ir::condcodes::IntCC;
        
        let ctx = self.ctx_ptr;
        
        // Create stack slots for args and return values
        let arg_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (arg_slots.max(1) * 8) as u32,
            8,
        ));
        let ret_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (func_ret_slots.max(1) * 8) as u32,
            8,
        ));
        
        // Store arguments
        for i in 0..arg_slots {
            let val = self.read_var((arg_start + i) as u16);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        // Load jit_func_table pointer from ctx
        let jit_func_table = self.builder.ins().load(types::I64, cranelift_codegen::ir::MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_FUNC_TABLE);
        
        // Calculate address: jit_func_table + func_id * 8
        let func_id_i64 = self.builder.ins().iconst(types::I64, func_id as i64);
        let offset = self.builder.ins().imul_imm(func_id_i64, 8);
        let func_ptr_addr = self.builder.ins().iadd(jit_func_table, offset);
        
        // Load function pointer
        let jit_func_ptr = self.builder.ins().load(types::I64, cranelift_codegen::ir::MemFlags::trusted(), func_ptr_addr, 0);
        
        // Check if null
        let zero = self.builder.ins().iconst(types::I64, 0);
        let is_null = self.builder.ins().icmp(IntCC::Equal, jit_func_ptr, zero);
        
        // Create blocks
        let jit_call_block = self.builder.create_block();
        let vm_call_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        
        self.builder.ins().brif(is_null, vm_call_block, &[], jit_call_block, &[]);
        
        // === JIT-to-JIT call path ===
        self.builder.switch_to_block(jit_call_block);
        self.builder.seal_block(jit_call_block);
        
        let args_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        
        // Create signature for JIT function
        let sig = self.builder.func.import_signature({
            let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(types::I32));
            sig
        });
        
        let jit_call = self.builder.ins().call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
        let jit_result = self.builder.inst_results(jit_call)[0];
        
        // Check for panic (create one constant in this block)
        let jit_one = self.builder.ins().iconst(types::I32, 1);
        let is_panic = self.builder.ins().icmp(IntCC::Equal, jit_result, jit_one);
        
        let jit_panic_block = self.builder.create_block();
        let jit_ok_block = self.builder.create_block();
        
        self.builder.ins().brif(is_panic, jit_panic_block, &[], jit_ok_block, &[]);
        
        // JIT panic - return panic code
        self.builder.switch_to_block(jit_panic_block);
        self.builder.seal_block(jit_panic_block);
        let panic_val = self.builder.ins().iconst(types::I32, crate::LOOP_RESULT_PANIC as i64);
        self.builder.ins().return_(&[panic_val]);
        
        // JIT OK - jump to merge
        self.builder.switch_to_block(jit_ok_block);
        self.builder.seal_block(jit_ok_block);
        self.builder.ins().jump(merge_block, &[]);
        
        // === VM call path ===
        self.builder.switch_to_block(vm_call_block);
        self.builder.seal_block(vm_call_block);
        
        if let Some(call_vm_func) = self.helpers.call_vm {
            let args_ptr_vm = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
            let ret_ptr_vm = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
            let func_id_val = self.builder.ins().iconst(types::I32, func_id as i64);
            let arg_count = self.builder.ins().iconst(types::I32, arg_slots as i64);
            let ret_count = self.builder.ins().iconst(types::I32, func_ret_slots as i64);
            
            let vm_call = self.builder.ins().call(call_vm_func, &[ctx, func_id_val, args_ptr_vm, arg_count, ret_ptr_vm, ret_count]);
            let vm_result = self.builder.inst_results(vm_call)[0];
            
            // Create one constant in this block (can't use value from jit_call_block)
            let vm_one = self.builder.ins().iconst(types::I32, 1);
            let vm_is_panic = self.builder.ins().icmp(IntCC::Equal, vm_result, vm_one);
            
            let vm_panic_block = self.builder.create_block();
            let vm_ok_block = self.builder.create_block();
            
            self.builder.ins().brif(vm_is_panic, vm_panic_block, &[], vm_ok_block, &[]);
            
            self.builder.switch_to_block(vm_panic_block);
            self.builder.seal_block(vm_panic_block);
            let vm_panic_val = self.builder.ins().iconst(types::I32, crate::LOOP_RESULT_PANIC as i64);
            self.builder.ins().return_(&[vm_panic_val]);
            
            self.builder.switch_to_block(vm_ok_block);
            self.builder.seal_block(vm_ok_block);
            self.builder.ins().jump(merge_block, &[]);
        } else {
            self.builder.ins().jump(merge_block, &[]);
        }
        
        // === Merge block ===
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        
        for i in 0..call_ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.write_var((arg_start + i) as u16, val);
        }
    }

    fn call_extern(&mut self, inst: &Instruction) {
        let call_extern_func = match self.helpers.call_extern {
            Some(f) => f,
            None => return,
        };
        
        self.do_emit_safepoint();
        
        let dst = inst.a as usize;
        let extern_id = inst.b as u32;
        let arg_start = inst.c as usize;
        let arg_count = inst.flags as usize;
        
        // Get ret_slots from extern definition for buffer sizing
        let extern_ret_slots = self.vo_module.externs[extern_id as usize].ret_slots as usize;
        let buffer_size = arg_count.max(extern_ret_slots).max(1);
        
        // Limit copy-back to available variables
        let available_vars = (self.func_def.local_slots as usize).saturating_sub(dst);
        let copy_back_slots = extern_ret_slots.min(available_vars);
        
        let slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (buffer_size * 8) as u32,
            8,
        ));
        
        for i in 0..arg_count {
            let val = self.read_var((arg_start + i) as u16);
            self.builder.ins().stack_store(val, slot, (i * 8) as i32);
        }
        
        let args_ptr = self.builder.ins().stack_addr(types::I64, slot, 0);
        let extern_id_val = self.builder.ins().iconst(types::I32, extern_id as i64);
        let arg_count_val = self.builder.ins().iconst(types::I32, arg_count as i64);
        let ret_slots_val = self.builder.ins().iconst(types::I32, extern_ret_slots as i64);
        
        let ctx = self.ctx_ptr;
        let call = self.builder.ins().call(call_extern_func, &[ctx, extern_id_val, args_ptr, arg_count_val, args_ptr, ret_slots_val]);
        let result = self.builder.inst_results(call)[0];
        
        self.check_call_result(result);
        
        for i in 0..copy_back_slots {
            let val = self.builder.ins().stack_load(types::I64, slot, (i * 8) as i32);
            self.write_var((dst + i) as u16, val);
        }
    }

    fn call_closure(&mut self, inst: &Instruction) {
        let call_closure_func = match self.helpers.call_closure {
            Some(f) => f,
            None => return,
        };
        
        self.do_emit_safepoint();
        
        let closure_ref = self.read_var(inst.a);
        let arg_start = inst.b as usize;
        let arg_slots = (inst.c >> 8) as usize;
        let ret_slots = (inst.c & 0xFF) as usize;
        
        let arg_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (arg_slots.max(1) * 8) as u32,
            8,
        ));
        let ret_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (ret_slots.max(1) * 8) as u32,
            8,
        ));
        
        for i in 0..arg_slots {
            let val = self.read_var((arg_start + i) as u16);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        let args_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        let arg_count = self.builder.ins().iconst(types::I32, arg_slots as i64);
        let ret_count = self.builder.ins().iconst(types::I32, ret_slots as i64);
        
        let ctx = self.ctx_ptr;
        let call = self.builder.ins().call(call_closure_func, &[ctx, closure_ref, args_ptr, arg_count, ret_ptr, ret_count]);
        let result = self.builder.inst_results(call)[0];
        
        self.check_call_result(result);
        
        let local_count = self.func_def.local_slots as usize;
        for i in 0..ret_slots {
            if arg_start + i < local_count {
                let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
                self.write_var((arg_start + i) as u16, val);
            }
        }
    }

    fn call_iface(&mut self, inst: &Instruction) {
        let call_iface_func = match self.helpers.call_iface {
            Some(f) => f,
            None => return,
        };
        
        self.do_emit_safepoint();
        
        let slot0 = self.read_var(inst.a);
        let slot1 = self.read_var(inst.a + 1);
        let method_idx = inst.flags as u32;
        let arg_start = inst.b as usize;
        let arg_slots = (inst.c >> 8) as usize;
        let ret_slots = (inst.c & 0xFF) as usize;
        
        let arg_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (arg_slots.max(1) * 8) as u32,
            8,
        ));
        let ret_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (ret_slots.max(1) * 8) as u32,
            8,
        ));
        
        for i in 0..arg_slots {
            let val = self.read_var((arg_start + i) as u16);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        let args_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        let method_idx_val = self.builder.ins().iconst(types::I32, method_idx as i64);
        let arg_count = self.builder.ins().iconst(types::I32, arg_slots as i64);
        let ret_count = self.builder.ins().iconst(types::I32, ret_slots as i64);
        let func_id = self.builder.ins().iconst(types::I32, 0);
        
        let ctx = self.ctx_ptr;
        let call = self.builder.ins().call(call_iface_func, &[
            ctx, slot0, slot1, method_idx_val, args_ptr, arg_count, ret_ptr, ret_count, func_id
        ]);
        let result = self.builder.inst_results(call)[0];
        
        self.check_call_result(result);
        
        for i in 0..ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.write_var((arg_start + i) as u16, val);
        }
    }

    fn check_call_result(&mut self, result: Value) {
        let panic_block = self.builder.create_block();
        let ok_block = self.builder.create_block();
        
        let zero = self.builder.ins().iconst(types::I32, 0);
        let is_panic = self.builder.ins().icmp(IntCC::NotEqual, result, zero);
        self.builder.ins().brif(is_panic, panic_block, &[], ok_block, &[]);
        
        self.builder.switch_to_block(panic_block);
        self.builder.seal_block(panic_block);
        let panic_val = self.builder.ins().iconst(types::I32, LOOP_RESULT_PANIC as i64);
        self.builder.ins().return_(&[panic_val]);
        
        self.builder.switch_to_block(ok_block);
        self.builder.seal_block(ok_block);
    }

    fn do_emit_safepoint(&mut self) {
        let safepoint_func = match self.helpers.safepoint {
            Some(f) => f,
            None => return,
        };
        
        let ctx_ptr = self.ctx_ptr;
        let flag_ptr = self.builder.ins().load(types::I64, MemFlags::trusted(), ctx_ptr, 16);
        let flag = self.builder.ins().load(types::I8, MemFlags::trusted(), flag_ptr, 0);
        
        let call_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        
        self.builder.ins().brif(flag, call_block, &[], merge_block, &[]);
        
        self.builder.switch_to_block(call_block);
        self.builder.seal_block(call_block);
        let ctx_for_call = self.ctx_ptr;
        self.builder.ins().call(safepoint_func, &[ctx_for_call]);
        self.builder.ins().jump(merge_block, &[]);
        
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
    }
}

impl<'a> IrEmitter<'a> for LoopCompiler<'a> {
    fn builder(&mut self) -> &mut FunctionBuilder<'a> { &mut self.builder }
    fn read_var(&mut self, slot: u16) -> Value { self.builder.use_var(self.vars[slot as usize]) }
    fn write_var(&mut self, slot: u16, val: Value) { self.builder.def_var(self.vars[slot as usize], val) }
    fn ctx_param(&mut self) -> Value { self.ctx_ptr }
    fn gc_ptr(&mut self) -> Value {
        self.builder.ins().load(types::I64, MemFlags::trusted(), self.ctx_ptr, 0)
    }
    fn globals_ptr(&mut self) -> Value {
        self.builder.ins().load(types::I64, MemFlags::trusted(), self.ctx_ptr, 8)
    }
    fn vo_module(&self) -> &VoModule { self.vo_module }
    fn current_pc(&self) -> usize { self.current_pc }
    fn emit_safepoint(&mut self) { self.do_emit_safepoint() }
    fn helpers(&self) -> &HelperFuncs { &self.helpers }
    fn set_reg_const(&mut self, reg: u16, val: i64) { self.reg_consts.insert(reg, val); }
    fn get_reg_const(&self, reg: u16) -> Option<i64> { self.reg_consts.get(&reg).copied() }
    fn panic_return_value(&self) -> i32 { LOOP_RESULT_PANIC as i32 }
    fn var_addr(&mut self, slot: u16) -> Value {
        let offset = (slot as i64) * 8;
        self.builder.ins().iadd_imm(self.locals_ptr, offset)
    }
}
