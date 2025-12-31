//! Function compiler: bytecode -> Cranelift IR.

use std::collections::HashMap;

use cranelift_codegen::ir::{types, Block, Function, InstBuilder, MemFlags, Value};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};

use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::SlotType;

use crate::gc_tracking::{GcRefTracker, StackMap};
use crate::translate::translate_inst;
use crate::translator::{HelperFuncs, IrEmitter, TranslateResult};
use crate::JitError;

pub struct FunctionCompiler<'a> {
    builder: FunctionBuilder<'a>,
    func_def: &'a FunctionDef,
    vo_module: &'a VoModule,
    gc_tracker: GcRefTracker,
    vars: Vec<Variable>,
    blocks: HashMap<usize, Block>,
    entry_block: Block,
    current_pc: usize,
    helpers: HelperFuncs,
    reg_consts: HashMap<u16, i64>,
}

impl<'a> FunctionCompiler<'a> {
    pub fn new(
        func: &'a mut Function,
        func_ctx: &'a mut FunctionBuilderContext,
        func_def: &'a FunctionDef,
        vo_module: &'a VoModule,
        helpers: HelperFuncs,
    ) -> Self {
        let mut builder = FunctionBuilder::new(func, func_ctx);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        
        Self {
            builder,
            func_def,
            vo_module,
            gc_tracker: GcRefTracker::new(),
            vars: Vec::new(),
            blocks: HashMap::new(),
            entry_block,
            current_pc: 0,
            helpers,
            reg_consts: HashMap::new(),
        }
    }

    pub fn compile(mut self) -> Result<StackMap, JitError> {
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
                block_terminated = false;
            } else if block_terminated {
                let dummy = self.builder.create_block();
                self.builder.switch_to_block(dummy);
                block_terminated = false;
            }
            
            let inst = &self.func_def.code[pc];
            block_terminated = self.translate_instruction(inst)?;
        }
        
        self.builder.seal_all_blocks();
        self.builder.finalize();
        
        Ok(self.gc_tracker.build_stack_map())
    }

    fn declare_variables(&mut self) {
        let num_slots = self.func_def.local_slots as usize;
        self.vars.reserve(num_slots);
        
        for i in 0..num_slots {
            let var = Variable::from_u32(i as u32);
            self.builder.declare_var(var, types::I64);
            self.vars.push(var);
            
            if i < self.func_def.slot_types.len() {
                if self.func_def.slot_types[i] == SlotType::GcRef {
                    self.gc_tracker.mark_gc_ref(i as u16);
                }
            }
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

    fn emit_prologue(&mut self) {
        let params = self.builder.block_params(self.entry_block);
        let _ctx = params[0];
        let args = params[1];
        let _ret = params[2];
        
        let param_slots = self.func_def.param_slots as usize;
        for i in 0..param_slots {
            let offset = (i * 8) as i32;
            let val = self.builder.ins().load(types::I64, MemFlags::trusted(), args, offset);
            self.builder.def_var(self.vars[i], val);
        }
        
        let zero = self.builder.ins().iconst(types::I64, 0);
        for i in param_slots..self.vars.len() {
            self.builder.def_var(self.vars[i], zero);
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
        let cond = self.builder.use_var(self.vars[inst.a as usize]);
        let offset = inst.imm32();
        let target = (self.current_pc as i32 + offset) as usize;
        let target_block = self.blocks[&target];
        let fall_through = self.builder.create_block();
        
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(IntCC::NotEqual, cond, zero);
        self.builder.ins().brif(cmp, target_block, &[], fall_through, &[]);
        
        self.builder.switch_to_block(fall_through);
        self.builder.seal_block(fall_through);
    }

    fn jump_if_not(&mut self, inst: &Instruction) {
        let cond = self.builder.use_var(self.vars[inst.a as usize]);
        let offset = inst.imm32();
        let target = (self.current_pc as i32 + offset) as usize;
        let target_block = self.blocks[&target];
        let fall_through = self.builder.create_block();
        
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(IntCC::Equal, cond, zero);
        self.builder.ins().brif(cmp, target_block, &[], fall_through, &[]);
        
        self.builder.switch_to_block(fall_through);
        self.builder.seal_block(fall_through);
    }

    fn ret(&mut self, _inst: &Instruction) {
        let ret_slots = self.func_def.ret_slots as usize;
        let ret_reg = _inst.a as usize;
        let ret_ptr = self.builder.block_params(self.entry_block)[2];
        
        for i in 0..ret_slots {
            let val = self.builder.use_var(self.vars[ret_reg + i]);
            let offset = (i * 8) as i32;
            self.builder.ins().store(MemFlags::trusted(), val, ret_ptr, offset);
        }
        
        let ok = self.builder.ins().iconst(types::I32, 0);
        self.builder.ins().return_(&[ok]);
    }

    fn panic(&mut self, inst: &Instruction) {
        if let Some(panic_func) = self.helpers.panic {
            let ctx = self.builder.block_params(self.entry_block)[0];
            let msg = self.builder.use_var(self.vars[inst.b as usize]);
            self.builder.ins().call(panic_func, &[ctx, msg]);
        }
        let panic_val = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_val]);
    }

    fn call(&mut self, inst: &Instruction) {
        let call_vm_func = match self.helpers.call_vm {
            Some(f) => f,
            None => return,
        };
        
        self.do_emit_safepoint();
        
        let func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
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
        let func_id_val = self.builder.ins().iconst(types::I32, func_id as i64);
        let arg_count = self.builder.ins().iconst(types::I32, arg_slots as i64);
        let ret_count = self.builder.ins().iconst(types::I32, ret_slots as i64);
        
        let call = self.builder.ins().call(call_vm_func, &[ctx, func_id_val, args_ptr, arg_count, ret_ptr, ret_count]);
        let result = self.builder.inst_results(call)[0];
        
        self.check_call_result(result);
        
        for i in 0..ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.builder.def_var(self.vars[arg_start + i], val);
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
        
        let slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (arg_count.max(1) * 8) as u32,
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
        
        let call = self.builder.ins().call(call_extern_func, &[ctx, extern_id_val, args_ptr, arg_count_val, args_ptr]);
        let result = self.builder.inst_results(call)[0];
        
        self.check_call_result(result);
        
        for i in 0..arg_count {
            let val = self.builder.ins().stack_load(types::I64, slot, (i * 8) as i32);
            self.builder.def_var(self.vars[dst + i], val);
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
            let val = self.builder.use_var(self.vars[arg_start + i]);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        let ctx = self.builder.block_params(self.entry_block)[0];
        let args_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        let arg_count = self.builder.ins().iconst(types::I32, arg_slots as i64);
        let ret_count = self.builder.ins().iconst(types::I32, ret_slots as i64);
        
        let call = self.builder.ins().call(call_closure_func, &[ctx, closure_ref, args_ptr, arg_count, ret_ptr, ret_count]);
        let result = self.builder.inst_results(call)[0];
        
        self.check_call_result(result);
        
        for i in 0..ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.builder.def_var(self.vars[arg_start + i], val);
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
        
        for i in 0..ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.builder.def_var(self.vars[arg_start + i], val);
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
        let panic_val = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_val]);
        
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
    fn write_var(&mut self, slot: u16, val: Value) { self.builder.def_var(self.vars[slot as usize], val) }
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
}
