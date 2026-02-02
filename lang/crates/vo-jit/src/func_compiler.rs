//! Function compiler: bytecode -> Cranelift IR.

use std::collections::HashMap;

use cranelift_codegen::ir::{types, Block, Function, FuncRef, InstBuilder, MemFlags, Value};
use cranelift_codegen::ir::StackSlotData;
use cranelift_codegen::ir::StackSlotKind;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};

use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::jit_api::JitContext;
use crate::translate::translate_inst;
use crate::translator::{HelperFuncs, IrEmitter, TranslateResult};
use crate::JitError;

pub struct FunctionCompiler<'a> {
    builder: FunctionBuilder<'a>,
    func_id: u32,
    func_def: &'a FunctionDef,
    vo_module: &'a VoModule,
    vars: Vec<Variable>,
    blocks: HashMap<usize, Block>,
    entry_block: Block,
    current_pc: usize,
    helpers: HelperFuncs,
    reg_consts: HashMap<u16, i64>,
    /// FuncRef for self-recursive calls (direct call optimization)
    self_func_ref: Option<FuncRef>,
    /// Saved jit_bp from function entry, used to recompute args_ptr after stack reallocation
    saved_jit_bp: Option<Variable>,
    /// Cranelift stack slot for local variables (native stack, not fiber.stack).
    /// Used by var_addr for SlotSet/SlotGet. Only spilled to fiber.stack on Call/WaitIo.
    locals_slot: Option<cranelift_codegen::ir::StackSlot>,
}

impl<'a> FunctionCompiler<'a> {
    pub fn new(
        func: &'a mut Function,
        func_ctx: &'a mut FunctionBuilderContext,
        func_id: u32,
        func_def: &'a FunctionDef,
        vo_module: &'a VoModule,
        helpers: HelperFuncs,
        self_func_ref: Option<FuncRef>,
    ) -> Self {
        let mut builder = FunctionBuilder::new(func, func_ctx);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        
        Self {
            builder,
            func_id,
            func_def,
            vo_module,
            vars: Vec::new(),
            blocks: HashMap::new(),
            entry_block,
            current_pc: 0,
            helpers,
            reg_consts: HashMap::new(),
            self_func_ref,
            saved_jit_bp: None,
            locals_slot: None,
        }
    }

    pub fn compile(mut self) -> Result<(), JitError> {
        self.declare_variables();
        self.scan_jump_targets();
        
        self.builder.switch_to_block(self.entry_block);
        self.emit_prologue();
        
        let mut block_terminated = false;
        
        for pc in 0..self.func_def.code.len() {
            self.current_pc = pc;
            
            if let Some(&block) = self.blocks.get(&pc) {
                if !block_terminated {
                    self.builder.ins().jump(block, &[]);
                }
                self.builder.switch_to_block(block);
            } else if block_terminated {
                let dummy = self.builder.create_block();
                self.builder.switch_to_block(dummy);
            }
            
            let inst = &self.func_def.code[pc];
            block_terminated = self.translate_instruction(inst)?;
        }
        
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
        for (pc, inst) in self.func_def.code.iter().enumerate() {
            match inst.opcode() {
                Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot => {
                    let offset = inst.imm32();
                    let target = (pc as i32 + offset) as usize;
                    self.ensure_block(target);
                }
                _ => {}
            }
        }
    }

    fn ensure_block(&mut self, pc: usize) {
        if !self.blocks.contains_key(&pc) {
            let block = self.builder.create_block();
            self.blocks.insert(pc, block);
        }
    }

    /// Spill all variables from native stack (locals_slot) to fiber.stack.
    /// Called on slow path (Call/WaitIo) so VM can see the current state.
    fn emit_variable_spill(&mut self) {
        let locals_slot = self.locals_slot.unwrap();
        let args_ptr = self.args_ptr();
        let num_slots = self.vars.len();
        
        // Copy from locals_slot (native stack) to fiber.stack (args_ptr)
        for i in 0..num_slots {
            let offset = (i * 8) as i32;
            let val = self.builder.ins().stack_load(types::I64, locals_slot, offset);
            self.builder.ins().store(MemFlags::trusted(), val, args_ptr, offset);
        }
    }
    
    /// Compute args_ptr dynamically from ctx.stack_ptr + saved_jit_bp.
    /// This is necessary because fiber.stack may reallocate during nested calls.
    fn args_ptr(&mut self) -> Value {
        let ctx = self.builder.block_params(self.entry_block)[0];
        let stack_ptr = self.builder.ins().load(
            types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_STACK_PTR
        );
        let jit_bp = self.builder.use_var(self.saved_jit_bp.unwrap());
        // args_ptr = stack_ptr + jit_bp * 8
        let bp_offset = self.builder.ins().imul_imm(jit_bp, 8);
        self.builder.ins().iadd(stack_ptr, bp_offset)
    }

    fn emit_prologue(&mut self) {
        // entry_block has no predecessors (it's the function entry point)
        self.builder.seal_block(self.entry_block);
        
        let params = self.builder.block_params(self.entry_block);
        let ctx = params[0];
        let args_ptr = params[1];  // Points directly to fiber.stack[jit_bp] at entry
        let _ret = params[2];
        
        // Save jit_bp from ctx at function entry.
        // This is needed to recompute args_ptr after stack reallocation.
        let jit_bp_var = Variable::from_u32((self.vars.len() + 1000) as u32);
        self.builder.declare_var(jit_bp_var, types::I64);
        // jit_bp is u32 in JitContext, load as i32 and extend to i64
        let jit_bp_i32 = self.builder.ins().load(
            types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_BP
        );
        let jit_bp_i64 = self.builder.ins().uextend(types::I64, jit_bp_i32);
        self.builder.def_var(jit_bp_var, jit_bp_i64);
        self.saved_jit_bp = Some(jit_bp_var);
        
        // Create Cranelift stack slot for local variables (native stack).
        // This is used by var_addr for SlotSet/SlotGet operations.
        // Variables are stored here instead of fiber.stack on the fast path.
        let num_slots = self.vars.len();
        let locals_slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (num_slots * 8) as u32,
            8,
        ));
        self.locals_slot = Some(locals_slot);
        
        let param_slots = self.func_def.param_slots as usize;
        
        // Load params from fiber.stack (args_ptr) into SSA vars AND native stack slot.
        // Params come from caller via fiber.stack, we copy them to our native stack.
        for i in 0..param_slots {
            let offset = (i * 8) as i32;
            let val = self.builder.ins().load(types::I64, MemFlags::trusted(), args_ptr, offset);
            self.builder.def_var(self.vars[i], val);
            // Also store to locals_slot for var_addr access
            self.builder.ins().stack_store(val, locals_slot, offset);
        }
        
        // Initialize non-param SSA vars to 0 and store to locals_slot
        let zero = self.builder.ins().iconst(types::I64, 0);
        for i in param_slots..num_slots {
            self.builder.def_var(self.vars[i], zero);
            self.builder.ins().stack_store(zero, locals_slot, (i * 8) as i32);
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
            Opcode::Call => Ok(self.call(inst)),
            Opcode::CallExtern => {
                crate::call_helpers::emit_call_extern(self, inst, crate::call_helpers::CallExternConfig {
                    current_pc: self.current_pc,
                    spill_on_non_ok: false,
                    handle_waitio_specially: true,
                });
                Ok(false)
            }
            Opcode::CallClosure => {
                crate::call_helpers::emit_call_closure(self, inst, crate::call_helpers::CallConfig {
                    resume_pc: None,
                    spill_on_non_ok: false,
                });
                Ok(false)
            }
            Opcode::CallIface => {
                crate::call_helpers::emit_call_iface(self, inst, crate::call_helpers::CallConfig {
                    resume_pc: None,
                    spill_on_non_ok: false,
                });
                Ok(false)
            }
            other => Err(JitError::UnsupportedOpcode(other)),
        }
    }

    fn jump(&mut self, inst: &Instruction) {
        let offset = inst.imm32();
        let target = (self.current_pc as i32 + offset) as usize;
        let block = self.blocks[&target];
        
        self.builder.ins().jump(block, &[]);
    }

    fn jump_if(&mut self, inst: &Instruction) {
        self.conditional_jump(inst, IntCC::NotEqual);
    }

    fn jump_if_not(&mut self, inst: &Instruction) {
        self.conditional_jump(inst, IntCC::Equal);
    }

    /// Load variable from native stack (locals_slot).
    fn load_local(&mut self, slot: u16) -> Value {
        let locals_slot = self.locals_slot.unwrap();
        self.builder.ins().stack_load(types::I64, locals_slot, (slot as i32) * 8)
    }
    
    /// Store value to both SSA variable and native stack (locals_slot).
    fn store_local(&mut self, slot: u16, val: Value) {
        self.builder.def_var(self.vars[slot as usize], val);
        let locals_slot = self.locals_slot.unwrap();
        self.builder.ins().stack_store(val, locals_slot, (slot as i32) * 8);
    }

    fn conditional_jump(&mut self, inst: &Instruction, cmp_cond: IntCC) {
        let cond = self.load_local(inst.a);
        let offset = inst.imm32();
        let target = (self.current_pc as i32 + offset) as usize;
        let target_block = self.blocks[&target];
        let fall_through = self.builder.create_block();
        
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(cmp_cond, cond, zero);
        self.builder.ins().brif(cmp, target_block, &[], fall_through, &[]);
        
        self.builder.switch_to_block(fall_through);
        self.builder.seal_block(fall_through);
    }

    fn ret(&mut self, inst: &Instruction) {
        use vo_common_core::bytecode::RETURN_FLAG_HEAP_RETURNS;
        let ret_ptr = self.builder.block_params(self.entry_block)[2];
        let heap_returns = (inst.flags & RETURN_FLAG_HEAP_RETURNS) != 0;
        
        if heap_returns {
            // Escaped named returns: GcRefs store the actual values
            // Need to dereference each GcRef to get the value
            let gcref_start = inst.a as usize;
            let gcref_count = inst.b as usize;
            
            let mut ret_offset = 0i32;
            for i in 0..gcref_count {
                let gcref = self.load_local((gcref_start + i) as u16);
                
                // Get per-ref slot count from FunctionDef (supports mixed sizes)
                let slots_for_this_ref = self.func_def.heap_ret_slots.get(i).copied().unwrap_or(1) as usize;
                
                // Dereference GcRef to get actual value(s)
                for j in 0..slots_for_this_ref {
                    let val = self.builder.ins().load(types::I64, MemFlags::trusted(), gcref, (j * 8) as i32);
                    self.builder.ins().store(MemFlags::trusted(), val, ret_ptr, ret_offset);
                    ret_offset += 8;
                }
            }
        } else {
            let ret_slots = self.func_def.ret_slots as usize;
            let ret_reg = inst.a as usize;
            
            for i in 0..ret_slots {
                let val = self.load_local((ret_reg + i) as u16);
                let offset = (i * 8) as i32;
                self.builder.ins().store(MemFlags::trusted(), val, ret_ptr, offset);
            }
        }
        
        let ok = self.builder.ins().iconst(types::I32, 0);
        self.builder.ins().return_(&[ok]);
    }

    fn panic(&mut self, inst: &Instruction) {
        if let Some(panic_func) = self.helpers.panic {
            let ctx = self.builder.block_params(self.entry_block)[0];
            // Panic message is an interface (2 slots): slot0=metadata, slot1=data
            // Note: Panic instruction uses inst.a for the register (not inst.b)
            let msg_slot0 = self.load_local(inst.a);
            let msg_slot1 = self.load_local(inst.a + 1);
            self.builder.ins().call(panic_func, &[ctx, msg_slot0, msg_slot1]);
        }
        let panic_val = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_val]);
    }

    /// Returns true if the block was terminated
    fn call(&mut self, inst: &Instruction) -> bool {
        let target_func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
        let arg_start = inst.b as usize;
        let arg_slots = (inst.c >> 8) as usize;
        let call_ret_slots = (inst.c & 0xFF) as usize;
        
        // Get target function info
        let target_func = &self.vo_module.functions[target_func_id as usize];
        
        // Self-recursive call: skip can_jit_to_jit_call check (would fail due to depth limit)
        // We know the current function is jittable since we're compiling it
        if target_func_id == self.func_id {
            self.call_self_recursive(arg_start, arg_slots, call_ret_slots, target_func);
            return false;
        }
        
        // Check if callee can be called via JIT-to-JIT (no blocking externs, etc)
        let callee_jittable = crate::can_jit_to_jit_call(target_func, self.vo_module);
        
        if callee_jittable {
            // JIT-to-JIT direct call with runtime check for compiled callee
            // If callee returns Call/WaitIo, we propagate it to VM
            crate::call_helpers::emit_jit_call_with_fallback(self, crate::call_helpers::JitCallWithFallbackConfig {
                func_id: target_func_id,
                arg_start,
                arg_slots,
                call_ret_slots,
                func_ret_slots: target_func.ret_slots as usize,
                callee_local_slots: target_func.local_slots as usize,
            });
            false // Block not terminated - we have a merge block for continuation
        } else {
            // Callee is NOT jittable (has defer/channel/select/etc)
            // Use Call request mechanism: return JitResult::Call, VM executes callee,
            // then continues execution in interpreter
            crate::call_helpers::emit_call_via_vm(self, crate::call_helpers::CallViaVmConfig {
                func_id: target_func_id,
                arg_start,
                resume_pc: self.current_pc + 1,
                ret_slots: call_ret_slots,
            });
            true // Block IS terminated - call_via_vm generates return instruction
        }
    }
    
    /// Optimized self-recursive call using direct call instead of call_indirect.
    /// 
    /// Fast path: args passed via native stack slot, no push_frame/pop_frame.
    /// Slow path (Call/WaitIo): materialize callee frame to fiber.stack.
    fn call_self_recursive(&mut self, arg_start: usize, arg_slots: usize, call_ret_slots: usize, target_func: &vo_runtime::bytecode::FunctionDef) {
        let func_ret_slots = target_func.ret_slots as usize;
        let callee_local_slots = target_func.local_slots as usize;
        let ctx = self.builder.block_params(self.entry_block)[0];
        
        // Save caller_bp for slow path
        let caller_bp = self.builder.ins().load(
            types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_BP
        );
        
        // Read args from locals_slot
        let locals_slot = self.locals_slot.unwrap();
        let mut arg_values = Vec::with_capacity(arg_slots);
        for i in 0..arg_slots {
            let val = self.builder.ins().stack_load(
                types::I64, locals_slot, ((arg_start + i) * 8) as i32
            );
            arg_values.push(val);
        }
        
        // Create args_slot on native stack for passing args to callee
        let args_slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (callee_local_slots.max(1) * 8) as u32,
            8,
        ));
        let args_ptr = self.builder.ins().stack_addr(types::I64, args_slot, 0);
        
        // Copy args to native stack slot
        for (i, val) in arg_values.iter().enumerate() {
            self.builder.ins().stack_store(*val, args_slot, (i * 8) as i32);
        }
        
        // Create ret_slot for return values
        let ret_slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (func_ret_slots.max(1) * 8) as u32,
            8,
        ));
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        
        // Constants for slow path
        let func_id_val = self.builder.ins().iconst(types::I32, self.func_id as i64);
        let local_slots_val = self.builder.ins().iconst(types::I32, callee_local_slots as i64);
        let ret_reg_val = self.builder.ins().iconst(types::I32, arg_start as i64);
        let ret_slots_val = self.builder.ins().iconst(types::I32, call_ret_slots as i64);
        let caller_resume_pc_val = self.builder.ins().iconst(types::I32, (self.current_pc + 1) as i64);
        
        // Save old fiber_sp for restoration after call
        let old_fiber_sp = self.builder.ins().load(
            types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_FIBER_SP
        );
        
        // Inline update ctx.jit_bp and ctx.fiber_sp for callee's correct saved_jit_bp
        let new_bp = old_fiber_sp;
        let new_sp = self.builder.ins().iadd_imm(new_bp, callee_local_slots as i64);
        self.builder.ins().store(MemFlags::trusted(), new_bp, ctx, JitContext::OFFSET_JIT_BP);
        self.builder.ins().store(MemFlags::trusted(), new_sp, ctx, JitContext::OFFSET_FIBER_SP);
        
        // Call callee with args on native stack (FAST PATH)
        let result = if let Some(func_ref) = self.self_func_ref {
            let call = self.builder.ins().call(func_ref, &[ctx, args_ptr, ret_ptr]);
            self.builder.inst_results(call)[0]
        } else {
            let jit_func_table = self.builder.ins().load(types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_FUNC_TABLE);
            let func_id_i64 = self.builder.ins().iconst(types::I64, self.func_id as i64);
            let offset = self.builder.ins().imul_imm(func_id_i64, 8);
            let func_ptr_addr = self.builder.ins().iadd(jit_func_table, offset);
            let jit_func_ptr = self.builder.ins().load(types::I64, MemFlags::trusted(), func_ptr_addr, 0);
            
            let sig = crate::call_helpers::import_jit_func_sig(self);
            
            let call = self.builder.ins().call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
            self.builder.inst_results(call)[0]
        };
        
        // Check result
        let ok_val = self.builder.ins().iconst(types::I32, crate::call_helpers::JIT_RESULT_OK as i64);
        let is_ok = self.builder.ins().icmp(IntCC::Equal, result, ok_val);
        
        let non_ok_block = self.builder.create_block();
        let ok_block = self.builder.create_block();
        
        self.builder.ins().brif(is_ok, ok_block, &[], non_ok_block, &[]);
        
        // Non-OK path (SLOW PATH): materialize frames to fiber.stack
        self.builder.switch_to_block(non_ok_block);
        self.builder.seal_block(non_ok_block);
        
        // Spill caller's variables to fiber.stack
        self.emit_variable_spill();
        
        // Materialize callee's frame in fiber.stack
        let push_frame_fn_ptr = self.builder.ins().load(
            types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_PUSH_FRAME_FN
        );
        let push_frame_sig = crate::call_helpers::import_push_frame_sig(self);
        let push_call = self.builder.ins().call_indirect(
            push_frame_sig, push_frame_fn_ptr,
            &[ctx, func_id_val, local_slots_val, ret_reg_val, ret_slots_val, caller_resume_pc_val]
        );
        let callee_fiber_args_ptr = self.builder.inst_results(push_call)[0];
        
        // Copy args from native stack to fiber.stack
        for i in 0..arg_slots {
            let val = self.builder.ins().stack_load(types::I64, args_slot, (i * 8) as i32);
            self.builder.ins().store(MemFlags::trusted(), val, callee_fiber_args_ptr, (i * 8) as i32);
        }
        
        // Push resume point
        let push_resume_point_fn_ptr = self.builder.ins().load(
            types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_PUSH_RESUME_POINT_FN
        );
        let push_resume_point_sig = crate::call_helpers::import_push_resume_point_sig(self);
        
        let callee_bp = self.builder.ins().load(
            types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_BP
        );
        
        self.builder.ins().call_indirect(
            push_resume_point_sig, push_resume_point_fn_ptr,
            &[ctx, func_id_val, caller_resume_pc_val, callee_bp, caller_bp, ret_reg_val, ret_slots_val]
        );
        
        self.builder.ins().return_(&[result]);
        
        // OK path (FAST PATH) - restore ctx.jit_bp and ctx.fiber_sp
        self.builder.switch_to_block(ok_block);
        self.builder.seal_block(ok_block);
        
        // Restore ctx.jit_bp and ctx.fiber_sp (inline pop)
        self.builder.ins().store(MemFlags::trusted(), caller_bp, ctx, JitContext::OFFSET_JIT_BP);
        self.builder.ins().store(MemFlags::trusted(), old_fiber_sp, ctx, JitContext::OFFSET_FIBER_SP);
        
        // Copy return values to caller's locals_slot
        for i in 0..call_ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.store_local((arg_start + i) as u16, val);
        }
    }
}

impl<'a> IrEmitter<'a> for FunctionCompiler<'a> {
    fn builder(&mut self) -> &mut FunctionBuilder<'a> { &mut self.builder }
    fn read_var(&mut self, slot: u16) -> Value { self.load_local(slot) }
    fn write_var(&mut self, slot: u16, val: Value) { self.store_local(slot, val); }
    fn ctx_param(&mut self) -> Value { self.builder.block_params(self.entry_block)[0] }
    fn gc_ptr(&mut self) -> Value {
        let ctx = self.ctx_param();
        self.builder.ins().load(types::I64, MemFlags::trusted(), ctx, 0)
    }
    fn globals_ptr(&mut self) -> Value {
        let ctx = self.ctx_param();
        self.builder.ins().load(types::I64, MemFlags::trusted(), ctx, 8)
    }
    fn vo_module(&self) -> &VoModule { self.vo_module }
    fn current_pc(&self) -> usize { self.current_pc }
    fn helpers(&self) -> &HelperFuncs { &self.helpers }
    fn set_reg_const(&mut self, reg: u16, val: i64) { self.reg_consts.insert(reg, val); }
    fn get_reg_const(&self, reg: u16) -> Option<i64> { self.reg_consts.get(&reg).copied() }
    fn panic_return_value(&self) -> i32 { 1 }
    fn var_addr(&mut self, slot: u16) -> Value {
        // Return address in native stack (locals_slot) - NOT fiber.stack!
        let locals_slot = self.locals_slot.unwrap();
        self.builder.ins().stack_addr(types::I64, locals_slot, (slot as i32) * 8)
    }
    fn spill_all_vars(&mut self) {
        self.emit_variable_spill();
    }
    fn local_slot_count(&self) -> usize {
        self.vars.len()
    }
    fn func_id(&self) -> u32 {
        self.func_id
    }
}
