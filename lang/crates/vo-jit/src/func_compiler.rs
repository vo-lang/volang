//! Function compiler: bytecode -> Cranelift IR.

use std::collections::HashMap;

use cranelift_codegen::ir::{types, Block, Function, FuncRef, InstBuilder, MemFlags, StackSlot, Value};
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
    /// FuncRef for self-recursive calls (direct call optimization)
    self_func_ref: Option<FuncRef>,
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
            locals_slot: None,
            resume_blocks: HashMap::new(),
            self_func_ref,
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

    /// Scan for instructions that need resume blocks for multi-entry support.
    /// 
    /// With CallDispatcher, jittable calls are handled entirely by the dispatcher
    /// (it manages its own resume_stack). We only need resume blocks for:
    /// 1. Calls to non-jittable functions (use Call request mechanism)
    /// 2. Blocking extern calls (may return WaitIo)
    fn scan_call_requests(&mut self) {
        for (pc, inst) in self.func_def.code.iter().enumerate() {
            let resume_pc = match inst.opcode() {
                Opcode::Call => {
                    let target_func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
                    let target_func = &self.vo_module.functions[target_func_id as usize];
                    // Only create resume block for calls that may return Call/WaitIo
                    if !crate::can_jit_to_jit_call(target_func, self.vo_module) {
                        Some(pc + 1) // Resume after call returns
                    } else {
                        None
                    }
                }
                Opcode::CallExtern => {
                    // Blocking extern calls may return WaitIo and need resume
                    let extern_id = inst.b as usize;
                    if self.vo_module.externs[extern_id].is_blocking {
                        Some(pc) // Resume at same PC to re-execute and get result
                    } else {
                        None
                    }
                }
                _ => None,
            };
            
            if let Some(resume_pc) = resume_pc {
                if !self.resume_blocks.contains_key(&resume_pc) {
                    let resume_block = self.builder.create_block();
                    self.resume_blocks.insert(resume_pc, resume_block);
                }
            }
        }
    }

    fn emit_prologue(&mut self) {
        // entry_block has no predecessors (it's the function entry point)
        self.builder.seal_block(self.entry_block);
        
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
                    self.builder.seal_block(current_block);  // Seal the check block
                    
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
            self.builder.seal_block(normal_entry_block);
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
            _ => Err(JitError::UnsupportedOpcode(inst.opcode())),
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
            // VM uses CallDispatcher at entry level to handle the resume
            crate::call_helpers::emit_jit_call_with_fallback(self, crate::call_helpers::JitCallWithFallbackConfig {
                func_id: target_func_id,
                arg_start,
                arg_slots,
                call_ret_slots,
                func_ret_slots: target_func.ret_slots as usize,
                resume_pc: self.current_pc + 1,
            });
            false // Block not terminated - we have a merge block for continuation
        } else {
            // Callee is NOT jittable (has defer/channel/select/etc)
            // Use Call request mechanism: return JitResult::Call, VM executes callee,
            // then resumes JIT at resume_pc
            crate::call_helpers::emit_call_via_vm(self, crate::call_helpers::CallViaVmConfig {
                func_id: target_func_id,
                arg_start,
                resume_pc: self.current_pc + 1,
                ret_slots: call_ret_slots,
            });
            true // Block IS terminated - call_via_vm generates return instruction
        }
    }
    
    /// Optimized self-recursive call using direct call instead of call_indirect
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
        let start_pc = self.builder.ins().iconst(types::I32, 0);
        
        // Use direct call if we have self_func_ref (much faster for recursion)
        let result = if let Some(func_ref) = self.self_func_ref {
            // Direct call - no function table lookup needed
            let call = self.builder.ins().call(func_ref, &[ctx, args_ptr, ret_ptr, start_pc]);
            self.builder.inst_results(call)[0]
        } else {
            // Fallback to indirect call via function table
            let jit_func_table = self.builder.ins().load(types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_FUNC_TABLE);
            let func_id_i64 = self.builder.ins().iconst(types::I64, self.func_id as i64);
            let offset = self.builder.ins().imul_imm(func_id_i64, 8);
            let func_ptr_addr = self.builder.ins().iadd(jit_func_table, offset);
            let jit_func_ptr = self.builder.ins().load(types::I64, MemFlags::trusted(), func_ptr_addr, 0);
            
            let sig = self.builder.func.import_signature({
                let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
                sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
                sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
                sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
                sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32));
                sig.returns.push(cranelift_codegen::ir::AbiParam::new(types::I32));
                sig
            });
            
            let call = self.builder.ins().call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr, start_pc]);
            self.builder.inst_results(call)[0]
        };
        
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
    fn helpers(&self) -> &HelperFuncs { &self.helpers }
    fn set_reg_const(&mut self, reg: u16, val: i64) { self.reg_consts.insert(reg, val); }
    fn get_reg_const(&self, reg: u16) -> Option<i64> { self.reg_consts.get(&reg).copied() }
    fn panic_return_value(&self) -> i32 { 1 }
    fn var_addr(&mut self, slot: u16) -> Value {
        let locals_slot = self.locals_slot.expect("var_addr called but no locals_slot");
        self.builder.ins().stack_addr(types::I64, locals_slot, (slot as i32) * 8)
    }
    fn spill_all_vars(&mut self) {
        self.emit_variable_spill();
    }
    fn local_slot_count(&self) -> usize {
        self.vars.len()
    }
}
