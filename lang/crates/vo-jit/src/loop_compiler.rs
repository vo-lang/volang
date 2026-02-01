//! Loop compiler for OSR (On-Stack Replacement).

use std::collections::HashMap;

use cranelift_codegen::ir::{types, Block, Function, InstBuilder, MemFlags, Value};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};

use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::jit_api::{JitContext, JitResult};
use crate::loop_analysis::LoopInfo;
use crate::translate::translate_inst;
use crate::translator::{HelperFuncs, IrEmitter, TranslateResult};
use crate::JitError;

/// Loop function signature. Returns JitResult like function JIT.
/// On Ok, loop_exit_pc in JitContext contains the PC to resume at.
pub type LoopFunc = extern "C" fn(*mut JitContext, *mut u64, u32) -> JitResult;

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
    /// Resume blocks for Call/WaitIo requests. Key is resume_pc.
    resume_blocks: HashMap<usize, Block>,
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
            resume_blocks: HashMap::new(),
        }
    }

    pub fn compile(mut self) -> Result<(), JitError> {
        self.declare_variables();
        self.scan_jump_targets();
        self.scan_call_requests();
        
        // Exactly like func_compiler: entry_block -> prologue -> sequential compile
        self.builder.switch_to_block(self.entry_block);
        self.emit_prologue();
        
        let mut block_terminated = false;  // Same as func_compiler!
        
        // Compile from after LOOP_BEGIN hint to after LOOP_END hint (includes back-edge jump)
        for pc in (self.loop_info.begin_pc + 1)..(self.loop_info.end_pc + 2) {
            self.current_pc = pc;
            
            // Check if this PC has a resume block (from Call/WaitIo request)
            if let Some(&resume_block) = self.resume_blocks.get(&pc) {
                if !block_terminated {
                    self.builder.ins().jump(resume_block, &[]);
                }
                self.builder.switch_to_block(resume_block);
                block_terminated = false;
            } else if let Some(&block) = self.blocks.get(&pc) {
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
        
        // Always create block for loop header (back-edge target)
        self.ensure_block(self.loop_info.begin_pc + 1);
        
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

    /// Scan for instructions that need resume blocks for multi-entry support.
    /// Includes: all call types and blocking extern calls.
    fn scan_call_requests(&mut self) {
        let loop_end = self.loop_info.end_pc + 2;
        for pc in (self.loop_info.begin_pc + 1)..loop_end {
            let inst = &self.func_def.code[pc];
            let resume_pc = match inst.opcode() {
                Opcode::Call | Opcode::CallClosure | Opcode::CallIface => {
                    Some(pc + 1) // Resume after call returns
                }
                Opcode::CallExtern => {
                    // Blocking extern calls may return WaitIo
                    let extern_id = inst.b as usize;
                    if self.vo_module.externs[extern_id].is_blocking {
                        Some(pc) // Resume at same PC to re-execute
                    } else {
                        None
                    }
                }
                _ => None,
            };
            
            if let Some(resume_pc) = resume_pc {
                if !self.resume_blocks.contains_key(&resume_pc) {
                    // Reuse existing jump target block if present, otherwise create new
                    let resume_block = if let Some(&existing) = self.blocks.get(&resume_pc) {
                        existing
                    } else {
                        self.builder.create_block()
                    };
                    self.resume_blocks.insert(resume_pc, resume_block);
                }
            }
        }
    }

    /// Emit code to restore all SSA variables from locals_ptr (fiber.stack).
    fn emit_variable_restore(&mut self) {
        for i in 0..self.vars.len() {
            let offset = (i * 8) as i32;
            let val = self.builder.ins().load(types::I64, MemFlags::trusted(), self.locals_ptr, offset);
            self.builder.def_var(self.vars[i], val);
        }
    }

    fn emit_prologue(&mut self) {
        let params = self.builder.block_params(self.entry_block);
        self.ctx_ptr = params[0];
        self.locals_ptr = params[1];
        let start_pc = params[2];
        
        // Generate dispatch logic if we have resume blocks
        if !self.resume_blocks.is_empty() {
            let normal_entry_block = self.builder.create_block();
            
            // Check if start_pc == 0 (normal entry)
            let zero = self.builder.ins().iconst(types::I32, 0);
            let is_normal = self.builder.ins().icmp(IntCC::Equal, start_pc, zero);
            
            // Create restore wrapper blocks
            let mut resume_pcs: Vec<usize> = self.resume_blocks.keys().copied().collect();
            resume_pcs.sort();
            
            let mut restore_wrappers: HashMap<usize, Block> = HashMap::new();
            for &resume_pc in &resume_pcs {
                let wrapper = self.builder.create_block();
                restore_wrappers.insert(resume_pc, wrapper);
            }
            
            if resume_pcs.len() == 1 {
                let resume_pc = resume_pcs[0];
                let restore_wrapper = restore_wrappers[&resume_pc];
                self.builder.ins().brif(is_normal, normal_entry_block, &[], restore_wrapper, &[]);
            } else {
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
            
            // Generate restore wrapper blocks
            for &resume_pc in &resume_pcs {
                let wrapper = restore_wrappers[&resume_pc];
                let resume_block = self.resume_blocks[&resume_pc];
                
                self.builder.switch_to_block(wrapper);
                self.emit_variable_restore();
                self.builder.ins().jump(resume_block, &[]);
                self.builder.seal_block(wrapper);
            }
            
            // Switch to normal entry block
            self.builder.switch_to_block(normal_entry_block);
        }
        
        // Normal entry: load all variables from memory
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
            Opcode::Call => { Ok(self.call(inst)) }
            Opcode::CallExtern => {
                crate::call_helpers::emit_call_extern(self, inst, crate::call_helpers::CallExternConfig {
                    current_pc: self.current_pc,
                    spill_on_non_ok: true,
                    handle_waitio_specially: false,
                });
                Ok(false)
            }
            Opcode::CallClosure => {
                crate::call_helpers::emit_call_closure(self, inst, crate::call_helpers::CallConfig {
                    resume_pc: Some(self.current_pc),
                    spill_on_non_ok: true,
                });
                Ok(false)
            }
            Opcode::CallIface => {
                crate::call_helpers::emit_call_iface(self, inst, crate::call_helpers::CallConfig {
                    resume_pc: Some(self.current_pc),
                    spill_on_non_ok: true,
                });
                Ok(false)
            }
            _ => {
                // Unsupported - exit to VM (variables already in memory)
                self.store_vars_to_memory();
                self.emit_loop_exit(self.current_pc as u32);
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
            let loop_header = self.blocks[&(self.loop_info.begin_pc + 1)];
            self.builder.ins().jump(loop_header, &[]);
        } else if raw_target <= self.loop_info.begin_pc || raw_target >= loop_end {
            // Jump outside loop - exit to VM
            self.store_vars_to_memory();
            self.emit_loop_exit(raw_target as u32);
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
            self.emit_loop_exit(target as u32);
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
            self.emit_loop_exit(target as u32);
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
        self.emit_loop_exit(self.current_pc as u32);
    }
    
    /// Emit code to exit loop normally with given exit_pc.
    /// Stores exit_pc to ctx.loop_exit_pc and returns JitResult::Ok.
    fn emit_loop_exit(&mut self, exit_pc: u32) {
        let ctx = self.ctx_ptr;
        let exit_pc_val = self.builder.ins().iconst(types::I32, exit_pc as i64);
        self.builder.ins().store(MemFlags::trusted(), exit_pc_val, ctx, JitContext::OFFSET_LOOP_EXIT_PC);
        let ok_val = self.builder.ins().iconst(types::I32, JitResult::Ok as i64);
        self.builder.ins().return_(&[ok_val]);
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
        let panic_val = self.builder.ins().iconst(types::I32, JitResult::Panic as i64);
        self.builder.ins().return_(&[panic_val]);
    }

    /// Returns true if block is terminated.
    /// Try JIT-to-JIT call if callee is compiled, otherwise fall back to VM.
    fn call(&mut self, inst: &Instruction) -> bool {
        let func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
        let arg_start = inst.b as usize;
        let arg_slots = (inst.c >> 8) as usize;
        let call_ret_slots = (inst.c & 0xFF) as usize;
        
        // Get target function info
        let target_func = &self.vo_module.functions[func_id as usize];
        let callee_jittable = crate::is_func_jittable(target_func);
        
        if callee_jittable && self.helpers.call_vm.is_some() {
            // Try JIT-to-JIT call with fallback to VM
            crate::call_helpers::emit_call_with_jit_check(self, crate::call_helpers::JitCallConfig {
                func_id,
                arg_start,
                arg_slots,
                call_ret_slots,
                func_ret_slots: target_func.ret_slots as usize,
                resume_pc: self.current_pc + 1,
                use_call_request_fallback: true,
            });
            false // Block not terminated - we have a merge block
        } else {
            // Not jittable or no call_vm helper - use Call request mechanism
            crate::call_helpers::emit_call_via_vm(self, crate::call_helpers::CallViaVmConfig {
                func_id,
                arg_start,
                resume_pc: self.current_pc + 1,
                ret_slots: call_ret_slots,
            });
            true // Block terminated with return
        }
    }
    
    /// Emit code to spill all SSA variables to fiber.stack.
    /// Called before returning Call so VM can see/restore state.
    fn emit_variable_spill(&mut self) {
        let locals_ptr = self.locals_ptr;
        for i in 0..self.vars.len() {
            let val = self.builder.use_var(self.vars[i]);
            self.builder.ins().store(MemFlags::trusted(), val, locals_ptr, (i * 8) as i32);
        }
    }
    
}

impl<'a> IrEmitter<'a> for LoopCompiler<'a> {
    fn builder(&mut self) -> &mut FunctionBuilder<'a> { &mut self.builder }
    fn read_var(&mut self, slot: u16) -> Value { self.builder.use_var(self.vars[slot as usize]) }
    fn write_var(&mut self, slot: u16, val: Value) {
        self.builder.def_var(self.vars[slot as usize], val);
        // Also sync to memory for var_addr access (e.g., slice_append reads element from memory)
        let offset = (slot as i32) * 8;
        self.builder.ins().store(MemFlags::trusted(), val, self.locals_ptr, offset);
    }
    fn ctx_param(&mut self) -> Value { self.ctx_ptr }
    fn gc_ptr(&mut self) -> Value {
        self.builder.ins().load(types::I64, MemFlags::trusted(), self.ctx_ptr, 0)
    }
    fn globals_ptr(&mut self) -> Value {
        self.builder.ins().load(types::I64, MemFlags::trusted(), self.ctx_ptr, 8)
    }
    fn vo_module(&self) -> &VoModule { self.vo_module }
    fn current_pc(&self) -> usize { self.current_pc }
    fn helpers(&self) -> &HelperFuncs { &self.helpers }
    fn set_reg_const(&mut self, reg: u16, val: i64) { self.reg_consts.insert(reg, val); }
    fn get_reg_const(&self, reg: u16) -> Option<i64> { self.reg_consts.get(&reg).copied() }
    fn panic_return_value(&self) -> i32 { JitResult::Panic as i32 }
    fn var_addr(&mut self, slot: u16) -> Value {
        let offset = (slot as i64) * 8;
        self.builder.ins().iadd_imm(self.locals_ptr, offset)
    }
    fn spill_all_vars(&mut self) {
        self.emit_variable_spill();
    }
    fn local_slot_count(&self) -> usize {
        self.vars.len()
    }
}
