//! Function compiler: bytecode -> Cranelift IR.

use std::collections::HashMap;

use cranelift_codegen::ir::{types, Block, Function, InstBuilder, MemFlags, StackSlot, Value};
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
    locals_slot: Option<StackSlot>,
    /// Resume blocks for Call requests. Key is resume_pc (pc after Call instruction).
    resume_blocks: HashMap<usize, Block>,
}

impl<'a> FunctionCompiler<'a> {
    pub fn new(
        func: &'a mut Function,
        func_ctx: &'a mut FunctionBuilderContext,
        func_id: u32,
        func_def: &'a FunctionDef,
        vo_module: &'a VoModule,
        helpers: HelperFuncs,
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
            locals_slot: None,
            resume_blocks: HashMap::new(),
        }
    }

    pub fn compile(mut self) -> Result<(), JitError> {
        self.declare_variables();
        self.scan_jump_targets();
        self.scan_call_requests();
        
        self.builder.switch_to_block(self.entry_block);
        self.emit_prologue();
        
        let mut block_terminated = false;
        
        for pc in 0..self.func_def.code.len() {
            self.current_pc = pc;
            
            // Check if this PC has a resume block (from Call request)
            // Note: resume_block is the entry point when resuming from WaitIo/Call.
            // In the main compile loop, we just switch to it as a normal block.
            // emit_variable_restore is done in prologue when start_pc matches.
            if let Some(&resume_block) = self.resume_blocks.get(&pc) {
                if !block_terminated {
                    self.builder.ins().jump(resume_block, &[]);
                }
                self.builder.switch_to_block(resume_block);
                // block_terminated will be set by translate_instruction below
            } else if let Some(&block) = self.blocks.get(&pc) {
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

    /// Emit code to restore all SSA variables from fiber.stack.
    /// Called at resume points after JIT returned Call and VM executed callee.
    fn emit_variable_restore(&mut self) {
        let args = self.builder.block_params(self.entry_block)[1];
        for i in 0..self.vars.len() {
            let val = self.builder.ins().load(types::I64, MemFlags::trusted(), args, (i * 8) as i32);
            self.builder.def_var(self.vars[i], val);
            if let Some(slot) = self.locals_slot {
                self.builder.ins().stack_store(val, slot, (i * 8) as i32);
            }
        }
    }

    /// Emit code to spill all SSA variables to fiber.stack.
    /// Called before returning Call so VM can see/restore state.
    fn emit_variable_spill(&mut self) {
        let args = self.builder.block_params(self.entry_block)[1];
        for i in 0..self.vars.len() {
            let val = self.builder.use_var(self.vars[i]);
            self.builder.ins().store(MemFlags::trusted(), val, args, (i * 8) as i32);
        }
    }

    /// Scan for Call instructions to non-jittable callees and blocking extern calls.
    /// These require resume blocks for multi-entry support.
    fn scan_call_requests(&mut self) {
        for (pc, inst) in self.func_def.code.iter().enumerate() {
            let needs_resume = match inst.opcode() {
                Opcode::Call => {
                    let target_func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
                    let target_func = &self.vo_module.functions[target_func_id as usize];
                    !crate::is_func_jittable(target_func)
                }
                Opcode::CallExtern => {
                    // waitio_ extern calls may return WaitIo and need resume
                    // Resume should re-execute the same CallExtern (not pc+1)
                    let extern_id = inst.b as usize;
                    self.vo_module.externs[extern_id].name.contains("_waitio_")
                }
                _ => false,
            };
            
            if needs_resume {
                // For waitio extern: resume at same PC to re-execute and get result
                // For Call to non-jittable: resume at pc+1 after callee returns
                let is_waitio_extern = inst.opcode() == Opcode::CallExtern;
                let resume_pc = if is_waitio_extern { pc } else { pc + 1 };
                let resume_block = self.builder.create_block();
                self.resume_blocks.insert(resume_pc, resume_block);
            }
        }
    }

    fn emit_prologue(&mut self) {
        let params = self.builder.block_params(self.entry_block);
        let _ctx = params[0];
        let args = params[1];
        let _ret = params[2];
        let start_pc = params[3];
        
        let num_slots = self.func_def.local_slots as usize;
        let param_slots = self.func_def.param_slots as usize;
        
        // Create stack slot for locals (used by SlotGet/SlotSet for stack arrays)
        // Always create locals_slot if we have resume blocks (for state restoration)
        let need_locals_slot = num_slots > 0 || !self.resume_blocks.is_empty();
        let actual_slots = if num_slots > 0 { num_slots } else if need_locals_slot { self.vars.len() } else { 0 };
        
        if need_locals_slot && actual_slots > 0 {
            let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                (actual_slots * 8) as u32,
                8,
            ));
            self.locals_slot = Some(slot);
        }
        
        // Generate dispatch logic if we have resume blocks
        if !self.resume_blocks.is_empty() {
            let normal_entry_block = self.builder.create_block();
            
            // Check if start_pc == 0 (normal entry)
            let zero = self.builder.ins().iconst(types::I32, 0);
            let is_normal = self.builder.ins().icmp(IntCC::Equal, start_pc, zero);
            
            // Create restore wrapper blocks that do emit_variable_restore then jump to resume_block
            let mut resume_pcs: Vec<usize> = self.resume_blocks.keys().copied().collect();
            resume_pcs.sort();
            
            // Create restore wrappers for each resume point
            let mut restore_wrappers: HashMap<usize, Block> = HashMap::new();
            for &resume_pc in &resume_pcs {
                let wrapper = self.builder.create_block();
                restore_wrappers.insert(resume_pc, wrapper);
            }
            
            if resume_pcs.len() == 1 {
                // Single resume point: simple branch to restore wrapper
                let resume_pc = resume_pcs[0];
                let restore_wrapper = restore_wrappers[&resume_pc];
                self.builder.ins().brif(is_normal, normal_entry_block, &[], restore_wrapper, &[]);
            } else {
                // Multiple resume points: chain of comparisons
                let first_check_block = self.builder.create_block();
                self.builder.ins().brif(is_normal, normal_entry_block, &[], first_check_block, &[]);
                
                let mut current_block = first_check_block;
                for (i, &resume_pc) in resume_pcs.iter().enumerate() {
                    self.builder.switch_to_block(current_block);
                    
                    let restore_wrapper = restore_wrappers[&resume_pc];
                    let pc_val = self.builder.ins().iconst(types::I32, resume_pc as i64);
                    let is_this_pc = self.builder.ins().icmp(IntCC::Equal, start_pc, pc_val);
                    
                    if i == resume_pcs.len() - 1 {
                        self.builder.ins().brif(is_this_pc, restore_wrapper, &[], normal_entry_block, &[]);
                    } else {
                        let next_check = self.builder.create_block();
                        self.builder.ins().brif(is_this_pc, restore_wrapper, &[], next_check, &[]);
                        current_block = next_check;
                    }
                }
            }
            
            // Generate restore wrapper blocks: restore variables then jump to resume_block
            for &resume_pc in &resume_pcs {
                let wrapper = restore_wrappers[&resume_pc];
                let resume_block = self.resume_blocks[&resume_pc];
                
                self.builder.switch_to_block(wrapper);
                self.emit_variable_restore();
                self.builder.ins().jump(resume_block, &[]);
                self.builder.seal_block(wrapper);
            }
            
            // Switch to normal entry block for standard initialization
            self.builder.switch_to_block(normal_entry_block);
        }
        
        // Normal entry initialization
        if let Some(slot) = self.locals_slot {
            // Initialize all slots to 0 (important for array initial values)
            let zero = self.builder.ins().iconst(types::I64, 0);
            for i in 0..actual_slots {
                self.builder.ins().stack_store(zero, slot, (i * 8) as i32);
            }
            
            // Load params from args into both SSA vars and stack_slot
            for i in 0..param_slots {
                let offset = (i * 8) as i32;
                let val = self.builder.ins().load(types::I64, MemFlags::trusted(), args, offset);
                self.builder.def_var(self.vars[i], val);
                self.builder.ins().stack_store(val, slot, offset);
            }
            
            // Initialize non-param SSA vars to 0
            let zero = self.builder.ins().iconst(types::I64, 0);
            for i in param_slots..self.vars.len() {
                self.builder.def_var(self.vars[i], zero);
            }
        } else {
            // No locals slot, just initialize SSA vars
            let zero = self.builder.ins().iconst(types::I64, 0);
            for i in 0..self.vars.len() {
                self.builder.def_var(self.vars[i], zero);
            }
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
            Opcode::CallExtern => { self.call_extern(inst, self.current_pc); Ok(false) }
            Opcode::CallClosure => { self.call_closure(inst); Ok(false) }
            Opcode::CallIface => { self.call_iface(inst); Ok(false) }
            _ => Err(JitError::UnsupportedOpcode(inst.opcode())),
        }
    }

    fn jump(&mut self, inst: &Instruction) {
        let offset = inst.imm32();
        let target = (self.current_pc as i32 + offset) as usize;
        let block = self.blocks[&target];
        
        if offset < 0 {
            self.do_emit_safepoint();
        }
        
        self.builder.ins().jump(block, &[]);
    }

    fn jump_if(&mut self, inst: &Instruction) {
        self.conditional_jump(inst, IntCC::NotEqual);
    }

    fn jump_if_not(&mut self, inst: &Instruction) {
        self.conditional_jump(inst, IntCC::Equal);
    }

    /// Sync value to both SSA variable and locals_slot (for var_addr access after calls)
    fn sync_var(&mut self, slot: u16, val: Value) {
        self.builder.def_var(self.vars[slot as usize], val);
        if let Some(locals_slot) = self.locals_slot {
            self.builder.ins().stack_store(val, locals_slot, (slot as i32) * 8);
        }
    }

    fn conditional_jump(&mut self, inst: &Instruction, cmp_cond: IntCC) {
        let cond = self.builder.use_var(self.vars[inst.a as usize]);
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
                // Read GcRef from locals_slot (must use stack because SSA may be corrupted after calls)
                let gcref = if let Some(locals_slot) = self.locals_slot {
                    self.builder.ins().stack_load(types::I64, locals_slot, ((gcref_start + i) * 8) as i32)
                } else {
                    self.builder.use_var(self.vars[gcref_start + i])
                };
                
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
            // Normal return: read values directly from SSA variables
            let ret_slots = self.func_def.ret_slots as usize;
            let ret_reg = inst.a as usize;
            
            for i in 0..ret_slots {
                let val = self.builder.use_var(self.vars[ret_reg + i]);
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
            let msg_slot0 = self.builder.use_var(self.vars[inst.a as usize]);
            let msg_slot1 = self.builder.use_var(self.vars[inst.a as usize + 1]);
            self.builder.ins().call(panic_func, &[ctx, msg_slot0, msg_slot1]);
        }
        let panic_val = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_val]);
    }

    /// Returns true if the block was terminated
    fn call(&mut self, inst: &Instruction) -> bool {
        self.do_emit_safepoint();
        
        let target_func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
        let arg_start = inst.b as usize;
        let arg_slots = (inst.c >> 8) as usize;
        let call_ret_slots = (inst.c & 0xFF) as usize;
        
        // Get target function info
        let target_func = &self.vo_module.functions[target_func_id as usize];
        
        // Check if callee is jittable (statically known at compile time)
        let callee_jittable = crate::is_func_jittable(target_func);
        
        if callee_jittable {
            // Self-recursive call optimization: direct call without jit_func_table check
            if target_func_id == self.func_id {
                self.call_self_recursive(arg_start, arg_slots, call_ret_slots, target_func);
            } else {
                // Callee is jittable - try JIT-to-JIT call, fallback to vo_call_vm if not compiled yet
                self.call_with_jit_check(inst, target_func_id, arg_start, arg_slots, call_ret_slots, target_func);
            }
            false // Block not terminated - we have a merge block for continuation
        } else {
            // Callee is NOT jittable (has defer/channel/select/etc)
            // Use Call request mechanism: return JitResult::Call, VM executes callee,
            // then resumes JIT at resume_pc
            self.call_via_vm(target_func_id, arg_start, arg_slots, call_ret_slots, target_func);
            true // Block IS terminated - call_via_vm generates return instruction
        }
    }
    
    /// Optimized self-recursive call - load from jit_func_table without null check
    fn call_self_recursive(&mut self, arg_start: usize, arg_slots: usize, call_ret_slots: usize, target_func: &vo_runtime::bytecode::FunctionDef) {
        let func_ret_slots = target_func.ret_slots as usize;
        let ctx = self.builder.block_params(self.entry_block)[0];
        
        // Create stack slots for args and return values
        let arg_slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (arg_slots.max(1) * 8) as u32,
            8,
        ));
        let ret_slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (func_ret_slots.max(1) * 8) as u32,
            8,
        ));
        
        // Store arguments to stack slot
        for i in 0..arg_slots {
            let val = self.builder.use_var(self.vars[arg_start + i]);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        let args_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        
        // Load self from jit_func_table (we know it's compiled since we're executing it)
        let jit_func_table = self.builder.ins().load(types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_FUNC_TABLE);
        let func_id_i64 = self.builder.ins().iconst(types::I64, self.func_id as i64);
        let offset = self.builder.ins().imul_imm(func_id_i64, 8);
        let func_ptr_addr = self.builder.ins().iadd(jit_func_table, offset);
        let jit_func_ptr = self.builder.ins().load(types::I64, MemFlags::trusted(), func_ptr_addr, 0);
        
        // Create signature for indirect call
        let sig = self.builder.func.import_signature({
            let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(types::I32));
            sig
        });
        
        // Indirect call to self (no null check needed - we know it's compiled)
        let call = self.builder.ins().call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
        let result = self.builder.inst_results(call)[0];
        
        // Check result for panic
        let one = self.builder.ins().iconst(types::I32, 1);
        let is_panic = self.builder.ins().icmp(IntCC::Equal, result, one);
        
        let panic_block = self.builder.create_block();
        let ok_block = self.builder.create_block();
        
        self.builder.ins().brif(is_panic, panic_block, &[], ok_block, &[]);
        
        // Panic path
        self.builder.switch_to_block(panic_block);
        self.builder.seal_block(panic_block);
        let panic_val = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_val]);
        
        // OK path - load return values
        self.builder.switch_to_block(ok_block);
        self.builder.seal_block(ok_block);
        
        for i in 0..call_ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.sync_var((arg_start + i) as u16, val);
        }
    }
    
    /// Call via VM for non-jittable callees using Call request mechanism.
    /// Instead of synchronously calling vo_call_vm, we:
    /// 1. Set call request info in JitContext
    /// 2. Return JitResult::Call
    /// 3. VM executes the callee and resumes JIT at resume_pc
    fn call_via_vm(&mut self, func_id: u32, arg_start: usize, _arg_slots: usize, _call_ret_slots: usize, _target_func: &vo_runtime::bytecode::FunctionDef) {
        let set_call_request_func = match self.helpers.set_call_request {
            Some(f) => f,
            None => return,
        };
        
        let resume_pc = self.current_pc + 1;
        
        // Spill all variables to fiber.stack before returning Call
        self.emit_variable_spill();
        
        // Call vo_set_call_request(ctx, func_id, arg_start, resume_pc)
        let ctx = self.builder.block_params(self.entry_block)[0];
        let func_id_val = self.builder.ins().iconst(types::I32, func_id as i64);
        let arg_start_val = self.builder.ins().iconst(types::I32, arg_start as i64);
        let resume_pc_val = self.builder.ins().iconst(types::I32, resume_pc as i64);
        
        self.builder.ins().call(set_call_request_func, &[ctx, func_id_val, arg_start_val, resume_pc_val]);
        
        // Return JitResult::Call = 2
        let call_result = self.builder.ins().iconst(types::I32, 2);
        self.builder.ins().return_(&[call_result]);
        
        // NOTE: Resume block code (variable restoration) will be generated by compile loop
        // when it processes the resume_pc. We don't generate it here because Cranelift
        // doesn't allow switching to a block that's already filled.
    }
    
    /// JIT-to-JIT call with runtime check for compiled callee
    fn call_with_jit_check(
        &mut self, 
        _inst: &Instruction,
        func_id: u32,
        arg_start: usize,
        arg_slots: usize,
        call_ret_slots: usize,
        target_func: &vo_runtime::bytecode::FunctionDef,
    ) {
        // For jittable callees, we have two paths:
        // 1. If jit_func_table[func_id] != null: direct JIT-to-JIT call
        // 2. If jit_func_table[func_id] == null: fallback to vo_call_vm
        //
        // To access jit_func_table, we need to load it from JitContext.
        // JitContext layout (64-bit, offsets in bytes):
        //   gc: 0, globals: 8, safepoint_flag: 16, panic_flag: 24, panic_msg: 32,
        //   vm: 40, fiber: 48, call_vm_fn: 56, itab_cache: 64, extern_registry: 72,
        //   call_extern_fn: 80, module: 88, jit_func_table: 96, jit_func_count: 104, ...
        
        let ctx = self.builder.block_params(self.entry_block)[0];
        let func_ret_slots = target_func.ret_slots as usize;
        
        // Create stack slots for args and return values (shared by both paths)
        let arg_slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (arg_slots.max(1) * 8) as u32,
            8,
        ));
        let ret_slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (func_ret_slots.max(1) * 8) as u32,
            8,
        ));
        
        // Store arguments to stack slot
        for i in 0..arg_slots {
            let val = self.builder.use_var(self.vars[arg_start + i]);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        // Load jit_func_table pointer from ctx (offset 96)
        let jit_func_table = self.builder.ins().load(types::I64, MemFlags::trusted(), ctx, 96);
        
        // Calculate address: jit_func_table + func_id * 8
        let func_id_i64 = self.builder.ins().iconst(types::I64, func_id as i64);
        let offset = self.builder.ins().imul_imm(func_id_i64, 8);
        let func_ptr_addr = self.builder.ins().iadd(jit_func_table, offset);
        
        // Load function pointer
        let jit_func_ptr = self.builder.ins().load(types::I64, MemFlags::trusted(), func_ptr_addr, 0);
        
        // Check if null
        let zero = self.builder.ins().iconst(types::I64, 0);
        let is_null = self.builder.ins().icmp(IntCC::Equal, jit_func_ptr, zero);
        
        // Create blocks for the two paths
        let jit_call_block = self.builder.create_block();
        let vm_call_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        
        // Branch based on whether function is compiled
        self.builder.ins().brif(is_null, vm_call_block, &[], jit_call_block, &[]);
        
        // === JIT-to-JIT call path ===
        self.builder.switch_to_block(jit_call_block);
        self.builder.seal_block(jit_call_block);
        
        // Prepare call arguments: (ctx, args, ret)
        let args_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        
        // Create signature for JIT function: (ctx: ptr, args: ptr, ret: ptr) -> i32
        let ptr_type = types::I64;
        let sig = self.builder.func.import_signature({
            let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type));
            sig.returns.push(cranelift_codegen::ir::AbiParam::new(types::I32));
            sig
        });
        
        // Indirect call through function pointer
        let jit_call = self.builder.ins().call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
        let jit_result = self.builder.inst_results(jit_call)[0];
        
        // Check result for panic (result == 1)
        let one = self.builder.ins().iconst(types::I32, 1);
        let is_panic = self.builder.ins().icmp(IntCC::Equal, jit_result, one);
        
        let jit_panic_block = self.builder.create_block();
        let jit_ok_block = self.builder.create_block();
        
        self.builder.ins().brif(is_panic, jit_panic_block, &[], jit_ok_block, &[]);
        
        // JIT panic path
        self.builder.switch_to_block(jit_panic_block);
        self.builder.seal_block(jit_panic_block);
        let panic_val = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_val]);
        
        // JIT OK path - jump to merge
        self.builder.switch_to_block(jit_ok_block);
        self.builder.seal_block(jit_ok_block);
        self.builder.ins().jump(merge_block, &[]);
        
        // === VM call path (fallback when JIT not compiled) ===
        self.builder.switch_to_block(vm_call_block);
        self.builder.seal_block(vm_call_block);
        
        let call_vm_func = self.helpers.call_vm.unwrap();
        let args_ptr_vm = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr_vm = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        let func_id_val = self.builder.ins().iconst(types::I32, func_id as i64);
        let arg_count = self.builder.ins().iconst(types::I32, arg_slots as i64);
        let ret_count = self.builder.ins().iconst(types::I32, func_ret_slots as i64);
        
        let vm_call = self.builder.ins().call(call_vm_func, &[ctx, func_id_val, args_ptr_vm, arg_count, ret_ptr_vm, ret_count]);
        let vm_result = self.builder.inst_results(vm_call)[0];
        
        // Check VM result for panic
        let vm_is_panic = self.builder.ins().icmp(IntCC::Equal, vm_result, one);
        
        let vm_panic_block = self.builder.create_block();
        let vm_ok_block = self.builder.create_block();
        
        self.builder.ins().brif(vm_is_panic, vm_panic_block, &[], vm_ok_block, &[]);
        
        // VM panic path
        self.builder.switch_to_block(vm_panic_block);
        self.builder.seal_block(vm_panic_block);
        let vm_panic_val = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[vm_panic_val]);
        
        // VM OK path - jump to merge
        self.builder.switch_to_block(vm_ok_block);
        self.builder.seal_block(vm_ok_block);
        self.builder.ins().jump(merge_block, &[]);
        
        // === Merge block (load return values) ===
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        
        for i in 0..call_ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.sync_var((arg_start + i) as u16, val);
        }
    }
    

    fn call_extern(&mut self, inst: &Instruction, pc: usize) {
        let call_extern_func = match self.helpers.call_extern {
            Some(f) => f,
            None => return,
        };
        
        self.do_emit_safepoint();
        
        let dst = inst.a as usize;
        let extern_id = inst.b as u32;
        let arg_start = inst.c as usize;
        let arg_count = inst.flags as usize;
        
        // Check if this is a waitio extern (may return WaitIo)
        let extern_def = &self.vo_module.externs[extern_id as usize];
        let is_blocking = extern_def.name.contains("_waitio_");
        let extern_ret_slots = extern_def.ret_slots as usize;
        let buffer_size = arg_count.max(extern_ret_slots).max(1);
        
        // Limit copy-back to available variables
        let available_vars = self.vars.len().saturating_sub(dst);
        let copy_back_slots = extern_ret_slots.min(available_vars);
        
        let slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (buffer_size * 8) as u32,
            8,
        ));
        
        for i in 0..arg_count {
            let val = self.builder.use_var(self.vars[arg_start + i]);
            self.builder.ins().stack_store(val, slot, (i * 8) as i32);
        }
        
        let ctx = self.builder.block_params(self.entry_block)[0];
        let args_ptr = self.builder.ins().stack_addr(types::I64, slot, 0);
        let extern_id_val = self.builder.ins().iconst(types::I32, extern_id as i64);
        let arg_count_val = self.builder.ins().iconst(types::I32, arg_count as i64);
        let ret_slots_val = self.builder.ins().iconst(types::I32, extern_ret_slots as i64);
        
        if is_blocking {
            // Blocking extern: spill before call, check WaitIo, restore after
            self.emit_variable_spill();
            
            let call = self.builder.ins().call(call_extern_func, &[ctx, extern_id_val, args_ptr, arg_count_val, args_ptr, ret_slots_val]);
            let result = self.builder.inst_results(call)[0];
            
            // Check for WaitIo (result == 3)
            let wait_io_val = self.builder.ins().iconst(types::I32, 3);
            let is_wait_io = self.builder.ins().icmp(IntCC::Equal, result, wait_io_val);
            
            let wait_io_block = self.builder.create_block();
            let continue_block = self.builder.create_block();
            
            self.builder.ins().brif(is_wait_io, wait_io_block, &[], continue_block, &[]);
            
            // WaitIo path: set resume_pc and return WaitIo
            self.builder.switch_to_block(wait_io_block);
            self.builder.seal_block(wait_io_block);
            
            // Set call_resume_pc in JitContext
            // Resume at same PC to re-execute extern and get result
            let resume_pc = pc;
            let resume_pc_val = self.builder.ins().iconst(types::I32, resume_pc as i64);
            self.builder.ins().store(MemFlags::trusted(), resume_pc_val, ctx, JitContext::OFFSET_CALL_RESUME_PC);
            
            // Return WaitIo (3)
            self.builder.ins().return_(&[wait_io_val]);
            
            // Continue path: check for panic and proceed
            // Note: do NOT emit_variable_restore here - that's only for resume path
            // In normal execution, variables are already correct in SSA
            self.builder.switch_to_block(continue_block);
            self.builder.seal_block(continue_block);
            
            self.check_call_result(result);
        } else {
            // Non-blocking extern: simple call
            let call = self.builder.ins().call(call_extern_func, &[ctx, extern_id_val, args_ptr, arg_count_val, args_ptr, ret_slots_val]);
            let result = self.builder.inst_results(call)[0];
            self.check_call_result(result);
        }
        
        // Copy return values to destination variables
        for i in 0..copy_back_slots {
            let val = self.builder.ins().stack_load(types::I64, slot, (i * 8) as i32);
            self.sync_var((dst + i) as u16, val);
        }
    }

    fn call_closure(&mut self, inst: &Instruction) {
        let call_closure_func = match self.helpers.call_closure {
            Some(f) => f,
            None => return,
        };
        
        self.do_emit_safepoint();
        
        let closure_ref = self.builder.use_var(self.vars[inst.a as usize]);
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
            if arg_start + i < self.vars.len() {
                let val = self.builder.use_var(self.vars[arg_start + i]);
                self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
            }
        }
        
        let ctx = self.builder.block_params(self.entry_block)[0];
        let args_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        let arg_count = self.builder.ins().iconst(types::I32, arg_slots as i64);
        let ret_count = self.builder.ins().iconst(types::I32, ret_slots as i64);
        
        let call = self.builder.ins().call(call_closure_func, &[ctx, closure_ref, args_ptr, arg_count, ret_ptr, ret_count]);
        let result = self.builder.inst_results(call)[0];
        
        self.check_call_result(result);
        
        // Must use write_var to sync to locals_slot for var_addr access
        for i in 0..ret_slots {
            if arg_start + i < self.vars.len() {
                let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
                self.sync_var((arg_start + i) as u16, val);
            }
        }
    }

    fn call_iface(&mut self, inst: &Instruction) {
        let call_iface_func = match self.helpers.call_iface {
            Some(f) => f,
            None => return,
        };
        
        self.do_emit_safepoint();
        
        let slot0 = self.builder.use_var(self.vars[inst.a as usize]);
        let slot1 = self.builder.use_var(self.vars[inst.a as usize + 1]);
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
            let val = self.builder.use_var(self.vars[arg_start + i]);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        let ctx = self.builder.block_params(self.entry_block)[0];
        let args_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        let method_idx_val = self.builder.ins().iconst(types::I32, method_idx as i64);
        let arg_count = self.builder.ins().iconst(types::I32, arg_slots as i64);
        let ret_count = self.builder.ins().iconst(types::I32, ret_slots as i64);
        let func_id = self.builder.ins().iconst(types::I32, 0);
        
        let call = self.builder.ins().call(call_iface_func, &[
            ctx, slot0, slot1, method_idx_val, args_ptr, arg_count, ret_ptr, ret_count, func_id
        ]);
        let result = self.builder.inst_results(call)[0];
        
        self.check_call_result(result);
        
        // Must use write_var to sync to locals_slot for var_addr access
        for i in 0..ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.sync_var((arg_start + i) as u16, val);
        }
    }

    fn check_call_result(&mut self, result: Value) {
        let not_ok_block = self.builder.create_block();
        let ok_block = self.builder.create_block();
        
        // JitResult: Ok=0, Panic=1, Block=2
        // If not Ok, return result as-is; VM scheduler handles Panic vs Block
        let zero = self.builder.ins().iconst(types::I32, 0);
        let is_ok = self.builder.ins().icmp(IntCC::Equal, result, zero);
        self.builder.ins().brif(is_ok, ok_block, &[], not_ok_block, &[]);
        
        self.builder.switch_to_block(not_ok_block);
        self.builder.seal_block(not_ok_block);
        self.builder.ins().return_(&[result]);
        
        self.builder.switch_to_block(ok_block);
        self.builder.seal_block(ok_block);
    }

    fn do_emit_safepoint(&mut self) {
        let safepoint_func = match self.helpers.safepoint {
            Some(f) => f,
            None => return,
        };
        
        let ctx = self.builder.block_params(self.entry_block)[0];
        let flag_ptr = self.builder.ins().load(types::I64, MemFlags::trusted(), ctx, 16);
        let flag = self.builder.ins().load(types::I8, MemFlags::trusted(), flag_ptr, 0);
        
        let call_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        
        self.builder.ins().brif(flag, call_block, &[], merge_block, &[]);
        
        self.builder.switch_to_block(call_block);
        self.builder.seal_block(call_block);
        self.builder.ins().call(safepoint_func, &[ctx]);
        self.builder.ins().jump(merge_block, &[]);
        
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
    }
}

impl<'a> IrEmitter<'a> for FunctionCompiler<'a> {
    fn builder(&mut self) -> &mut FunctionBuilder<'a> { &mut self.builder }
    fn read_var(&mut self, slot: u16) -> Value { self.builder.use_var(self.vars[slot as usize]) }
    fn write_var(&mut self, slot: u16, val: Value) {
        self.builder.def_var(self.vars[slot as usize], val);
        // Also sync to stack_slot for SlotGet/SlotSet operations
        if let Some(locals_slot) = self.locals_slot {
            self.builder.ins().stack_store(val, locals_slot, (slot as i32) * 8);
        }
    }
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
    fn emit_safepoint(&mut self) { self.do_emit_safepoint() }
    fn helpers(&self) -> &HelperFuncs { &self.helpers }
    fn set_reg_const(&mut self, reg: u16, val: i64) { self.reg_consts.insert(reg, val); }
    fn get_reg_const(&self, reg: u16) -> Option<i64> { self.reg_consts.get(&reg).copied() }
    fn panic_return_value(&self) -> i32 { 1 }
    fn var_addr(&mut self, slot: u16) -> Value {
        let locals_slot = self.locals_slot.expect("var_addr called but no locals_slot");
        self.builder.ins().stack_addr(types::I64, locals_slot, (slot as i32) * 8)
    }
}
