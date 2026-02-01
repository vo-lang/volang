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

    /// Scan for Call instructions and blocking extern calls that need resume blocks.
    fn scan_call_requests(&mut self) {
        let loop_end = self.loop_info.end_pc + 2;
        for pc in (self.loop_info.begin_pc + 1)..loop_end {
            let inst = &self.func_def.code[pc];
            let needs_resume = match inst.opcode() {
                Opcode::Call => true, // All calls in loop need resume support
                Opcode::CallExtern => {
                    // waitio_ extern calls may return WaitIo
                    let extern_id = inst.b as usize;
                    self.vo_module.externs[extern_id].name.contains("_waitio_")
                }
                Opcode::CallClosure | Opcode::CallIface => true,
                _ => false,
            };
            
            if needs_resume {
                // For waitio extern: resume at same PC to re-execute
                // For Call: resume at pc+1 after callee returns
                let is_waitio_extern = inst.opcode() == Opcode::CallExtern 
                    && self.vo_module.externs[inst.b as usize].name.contains("_waitio_");
                let resume_pc = if is_waitio_extern { pc } else { pc + 1 };
                
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
            Opcode::CallExtern => { self.call_extern(inst); Ok(false) }
            Opcode::CallClosure => { self.call_closure(inst); Ok(false) }
            Opcode::CallIface => { self.call_iface(inst); Ok(false) }
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
            self.do_emit_safepoint();
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
        self.do_emit_safepoint();
        
        let func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
        let arg_start = inst.b as usize;
        let arg_slots = (inst.c >> 8) as usize;
        let call_ret_slots = (inst.c & 0xFF) as usize;
        
        // Get target function info
        let target_func = &self.vo_module.functions[func_id as usize];
        let callee_jittable = crate::is_func_jittable(target_func);
        
        if callee_jittable && self.helpers.call_vm.is_some() {
            // Try JIT-to-JIT call with fallback to VM
            self.call_with_jit_check(func_id, arg_start, arg_slots, call_ret_slots, target_func);
            false // Block not terminated - we have a merge block
        } else {
            // Not jittable or no call_vm helper - use Call request mechanism
            self.call_via_vm(func_id, arg_start, call_ret_slots);
            true // Block terminated with return
        }
    }
    
    /// JIT-to-JIT call with runtime check for compiled callee.
    /// If jit_func_table[func_id] != null: direct JIT call
    /// If jit_func_table[func_id] == null: use Call request (cheaper than vo_call_vm)
    fn call_with_jit_check(
        &mut self,
        func_id: u32,
        arg_start: usize,
        arg_slots: usize,
        call_ret_slots: usize,
        target_func: &FunctionDef,
    ) {
        let ctx = self.ctx_ptr;
        let func_ret_slots = target_func.ret_slots as usize;
        
        // Load jit_func_table pointer from ctx
        let jit_func_table = self.builder.ins().load(types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_FUNC_TABLE);
        
        // Calculate address: jit_func_table + func_id * 8
        let func_id_i64 = self.builder.ins().iconst(types::I64, func_id as i64);
        let offset = self.builder.ins().imul_imm(func_id_i64, 8);
        let func_ptr_addr = self.builder.ins().iadd(jit_func_table, offset);
        
        // Load function pointer
        let jit_func_ptr = self.builder.ins().load(types::I64, MemFlags::trusted(), func_ptr_addr, 0);
        
        // Check if null
        let zero_i64 = self.builder.ins().iconst(types::I64, 0);
        let is_null = self.builder.ins().icmp(IntCC::Equal, jit_func_ptr, zero_i64);
        
        // Create blocks for the two paths
        let jit_call_block = self.builder.create_block();
        let vm_call_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        
        // Branch based on whether function is compiled
        self.builder.ins().brif(is_null, vm_call_block, &[], jit_call_block, &[]);
        
        // === JIT-to-JIT call path ===
        self.builder.switch_to_block(jit_call_block);
        self.builder.seal_block(jit_call_block);
        
        // Create stack slots for args and return values (only needed for JIT path)
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
        
        // Copy arguments from loop variables to stack slot
        for i in 0..arg_slots {
            let val = self.read_var((arg_start + i) as u16);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        let args_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        
        // Create signature for JIT function: (ctx: ptr, args: ptr, ret: ptr) -> i32
        let sig = self.builder.func.import_signature({
            let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
            sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
            sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64));
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
        
        // JIT panic path - propagate panic
        self.builder.switch_to_block(jit_panic_block);
        self.builder.seal_block(jit_panic_block);
        let panic_val = self.builder.ins().iconst(types::I32, JitResult::Panic as i64);
        self.builder.ins().return_(&[panic_val]);
        
        // JIT OK path - load return values and jump to merge
        self.builder.switch_to_block(jit_ok_block);
        self.builder.seal_block(jit_ok_block);
        
        // Load return values back to loop variables
        for i in 0..call_ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.write_var((arg_start + i) as u16, val);
        }
        self.builder.ins().jump(merge_block, &[]);
        
        // === VM call path (use Call request - cheaper than vo_call_vm) ===
        self.builder.switch_to_block(vm_call_block);
        self.builder.seal_block(vm_call_block);
        
        // Spill variables and use Call request mechanism
        self.emit_variable_spill();
        
        let set_call_request_func = self.helpers.set_call_request.unwrap();
        let resume_pc = self.current_pc + 1;
        let func_id_val = self.builder.ins().iconst(types::I32, func_id as i64);
        let arg_start_val = self.builder.ins().iconst(types::I32, arg_start as i64);
        let resume_pc_val = self.builder.ins().iconst(types::I32, resume_pc as i64);
        let ret_slots_val = self.builder.ins().iconst(types::I32, call_ret_slots as i64);
        
        self.builder.ins().call(set_call_request_func, &[ctx, func_id_val, arg_start_val, resume_pc_val, ret_slots_val]);
        
        let call_result = self.builder.ins().iconst(types::I32, JitResult::Call as i64);
        self.builder.ins().return_(&[call_result]);
        
        // === Merge block (only reached from JIT path) ===
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
    }
    
    fn call_via_vm(&mut self, func_id: u32, arg_start: usize, call_ret_slots: usize) {
        // Use Call request mechanism - same as func_compiler.
        let set_call_request_func = match self.helpers.set_call_request {
            Some(f) => f,
            None => return,
        };
        
        // Spill all loop variables to fiber.stack before returning Call
        self.emit_variable_spill();
        
        // Set call request: vo_set_call_request(ctx, func_id, arg_start, resume_pc, ret_slots)
        let ctx = self.ctx_ptr;
        let resume_pc = self.current_pc + 1;
        let func_id_val = self.builder.ins().iconst(types::I32, func_id as i64);
        let arg_start_val = self.builder.ins().iconst(types::I32, arg_start as i64);
        let resume_pc_val = self.builder.ins().iconst(types::I32, resume_pc as i64);
        let ret_slots_val = self.builder.ins().iconst(types::I32, call_ret_slots as i64);
        
        self.builder.ins().call(set_call_request_func, &[ctx, func_id_val, arg_start_val, resume_pc_val, ret_slots_val]);
        
        // Return JitResult::Call to signal VM to handle the call
        let call_result = self.builder.ins().iconst(types::I32, JitResult::Call as i64);
        self.builder.ins().return_(&[call_result]);
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
        
        // Set resume_pc before calling extern, in case it returns WaitIo
        // Use current_pc (not +1) so extern is re-executed on resume to get result
        let resume_pc = self.current_pc;
        let resume_pc_val = self.builder.ins().iconst(types::I32, resume_pc as i64);
        self.builder.ins().store(MemFlags::trusted(), resume_pc_val, ctx, JitContext::OFFSET_CALL_RESUME_PC);
        
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
        
        // Set resume_pc before calling, in case it returns WaitIo
        // Use current_pc (not +1) so call is re-executed on resume
        let resume_pc = self.current_pc;
        let resume_pc_val = self.builder.ins().iconst(types::I32, resume_pc as i64);
        self.builder.ins().store(MemFlags::trusted(), resume_pc_val, ctx, JitContext::OFFSET_CALL_RESUME_PC);
        
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
        
        // Set resume_pc before calling, in case it returns WaitIo
        // Use current_pc (not +1) so call is re-executed on resume
        let resume_pc = self.current_pc;
        let resume_pc_val = self.builder.ins().iconst(types::I32, resume_pc as i64);
        self.builder.ins().store(MemFlags::trusted(), resume_pc_val, ctx, JitContext::OFFSET_CALL_RESUME_PC);
        
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

    /// Check call result and handle non-Ok cases.
    /// JitResult: Ok=0, Panic=1, Call=2, WaitIo=3
    /// For non-Ok, spill variables and return JitResult directly.
    fn check_call_result(&mut self, result: Value) {
        use cranelift_codegen::ir::condcodes::IntCC;
        
        let ok_block = self.builder.create_block();
        let non_ok_block = self.builder.create_block();
        
        let zero = self.builder.ins().iconst(types::I32, 0);
        let is_ok = self.builder.ins().icmp(IntCC::Equal, result, zero);
        self.builder.ins().brif(is_ok, ok_block, &[], non_ok_block, &[]);
        
        // Non-Ok path: spill and return JitResult as-is
        self.builder.switch_to_block(non_ok_block);
        self.builder.seal_block(non_ok_block);
        self.emit_variable_spill();
        self.builder.ins().return_(&[result]);
        
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
    fn emit_safepoint(&mut self) { self.do_emit_safepoint() }
    fn helpers(&self) -> &HelperFuncs { &self.helpers }
    fn set_reg_const(&mut self, reg: u16, val: i64) { self.reg_consts.insert(reg, val); }
    fn get_reg_const(&self, reg: u16) -> Option<i64> { self.reg_consts.get(&reg).copied() }
    fn panic_return_value(&self) -> i32 { JitResult::Panic as i32 }
    fn var_addr(&mut self, slot: u16) -> Value {
        let offset = (slot as i64) * 8;
        self.builder.ins().iadd_imm(self.locals_ptr, offset)
    }
}
