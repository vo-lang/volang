//! Bytecode to Cranelift IR translation.
//!
//! Translates GoX bytecode to Cranelift IR with support for:
//! - Control flow (jumps, branches)
//! - Function calls (GoX and runtime functions)
//! - Memory operations (GC allocation, field access)
//! - Composite types (arrays, slices, maps)

use anyhow::{Result, bail};
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::types::{F64, I64};
use cranelift_codegen::ir::{Block, Function, FuncRef, InstBuilder};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::Module;
use std::collections::{HashMap, HashSet};

use gox_vm::bytecode::Constant;
use gox_vm::instruction::{Instruction, Opcode};

use crate::context::CompileContext;
use crate::runtime::RuntimeFunc;

/// Translates a single function's bytecode to Cranelift IR.
pub struct FunctionTranslator {
    /// Variables for local slots
    variables: Vec<Variable>,
    /// Number of local slots
    local_count: usize,
    /// Block map: bytecode PC -> Cranelift Block
    blocks: HashMap<usize, Block>,
    /// Declared function references for calls
    func_refs: HashMap<u32, FuncRef>,
    /// Runtime function references
    runtime_refs: HashMap<RuntimeFunc, FuncRef>,
}

impl FunctionTranslator {
    /// Create a new translator.
    /// 
    /// Note: local_slots from FunctionDef may not include return value registers.
    /// We scan the bytecode to find the actual max register used.
    pub fn new(local_slots: usize, code: &[Instruction]) -> Self {
        // Scan bytecode to find max register index used
        let mut max_reg = local_slots;
        for inst in code {
            max_reg = max_reg.max(inst.a as usize + 1);
            max_reg = max_reg.max(inst.b as usize + 1);
            max_reg = max_reg.max(inst.c as usize + 1);
        }
        
        Self {
            variables: Vec::new(),
            local_count: max_reg,
            blocks: HashMap::new(),
            func_refs: HashMap::new(),
            runtime_refs: HashMap::new(),
        }
    }

    /// Translate bytecode to Cranelift IR.
    pub fn translate<M: Module>(
        &mut self,
        func: &mut Function,
        code: &[Instruction],
        ctx: &mut CompileContext,
        module: &mut M,
    ) -> Result<()> {
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(func, &mut builder_ctx);

        // Phase 1: Find all jump targets and create blocks
        self.create_blocks(&mut builder, code);

        // Phase 2: Declare variables
        self.declare_variables(&mut builder);

        // Phase 3: Start with entry block
        // Check if we have a separate entry block (for back-edge to PC 0 case)
        let has_separate_entry = self.blocks.contains_key(&usize::MAX);
        let entry_block = if has_separate_entry {
            *self.blocks.get(&usize::MAX).unwrap()
        } else {
            *self.blocks.get(&0).unwrap()
        };
        
        builder.switch_to_block(entry_block);
        builder.append_block_params_for_function_params(entry_block);

        // Initialize params from block params
        let param_count = builder.func.signature.params.len();
        for i in 0..param_count {
            let param_val = builder.block_params(entry_block)[i];
            builder.def_var(self.variables[i], param_val);
        }

        // Initialize remaining locals to 0
        let zero = builder.ins().iconst(I64, 0);
        for i in param_count..self.local_count {
            builder.def_var(self.variables[i], zero);
        }
        
        // If we have a separate entry, jump to PC 0 block
        if has_separate_entry {
            let pc0_block = *self.blocks.get(&0).unwrap();
            builder.ins().jump(pc0_block, &[]);
            builder.switch_to_block(pc0_block);
        }

        // Phase 4: Translate instructions
        let mut block_terminated = false;
        for (pc, inst) in code.iter().enumerate() {
            // Switch to new block if this PC is a jump target
            if let Some(&block) = self.blocks.get(&pc) {
                if pc > 0 {
                    // Add fallthrough jump if previous block wasn't terminated
                    if !block_terminated {
                        builder.ins().jump(block, &[]);
                    }
                    builder.switch_to_block(block);
                }
                block_terminated = false;
            } else if block_terminated {
                // Skip dead code (unreachable instructions after terminator)
                continue;
            }

            // Check if this instruction is a terminator
            block_terminated = matches!(
                inst.opcode(),
                Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot | Opcode::Return
            );

            self.translate_instruction(&mut builder, inst, pc, code, ctx, module)?;
        }

        // Seal all blocks
        for &block in self.blocks.values() {
            builder.seal_block(block);
        }

        builder.finalize();
        Ok(())
    }

    /// Find all jump targets and create blocks.
    fn create_blocks(&mut self, builder: &mut FunctionBuilder, code: &[Instruction]) {
        self.blocks.clear();

        // Find jump targets first
        let mut targets = HashSet::new();
        let mut has_back_edge_to_zero = false;
        
        for (pc, inst) in code.iter().enumerate() {
            match inst.opcode() {
                Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot => {
                    // VM does: pc = pc + offset - 1, then pc++, so target = pc + offset
                    let offset = inst.imm32();
                    let target = (pc as i32 + offset) as usize;
                    targets.insert(target);
                    
                    // Check for back-edge to PC 0
                    if target == 0 && pc > 0 {
                        has_back_edge_to_zero = true;
                    }
                    
                    // Also need block after conditional jump for fall-through
                    if inst.opcode() != Opcode::Jump {
                        targets.insert(pc + 1);
                    }
                }
                _ => {}
            }
        }

        // Entry block - Cranelift doesn't allow back-edges to entry block
        // If there's a back-edge to PC 0, we need a separate entry that jumps to PC 0 block
        if has_back_edge_to_zero {
            // Create real entry block (no predecessors)
            let entry = builder.create_block();
            self.blocks.insert(usize::MAX, entry); // Special marker for real entry
            
            // Create PC 0 block (can have back-edges)
            let pc0_block = builder.create_block();
            self.blocks.insert(0, pc0_block);
        } else {
            // Normal case: entry block at PC 0
            let entry = builder.create_block();
            self.blocks.insert(0, entry);
        }

        // Create blocks for other targets
        for target in targets {
            if !self.blocks.contains_key(&target) {
                let block = builder.create_block();
                self.blocks.insert(target, block);
            }
        }
    }

    /// Declare variables for all local slots.
    fn declare_variables(&mut self, builder: &mut FunctionBuilder) {
        self.variables.clear();
        for i in 0..self.local_count {
            let var = Variable::from_u32(i as u32);
            builder.declare_var(var, I64);
            self.variables.push(var);
        }
    }

    /// Get or import a GoX function reference.
    fn get_gox_func_ref<M: Module>(
        &mut self,
        builder: &mut FunctionBuilder,
        module: &mut M,
        ctx: &CompileContext,
        func_idx: u32,
    ) -> Result<FuncRef> {
        if let Some(&func_ref) = self.func_refs.get(&func_idx) {
            return Ok(func_ref);
        }

        let func_id = ctx.get_gox_func(func_idx)
            .ok_or_else(|| anyhow::anyhow!("Function {} not declared", func_idx))?;
        let func_ref = module.declare_func_in_func(func_id, builder.func);
        self.func_refs.insert(func_idx, func_ref);
        Ok(func_ref)
    }

    /// Get or import a runtime function reference.
    fn get_runtime_func_ref<M: Module>(
        &mut self,
        builder: &mut FunctionBuilder,
        module: &mut M,
        ctx: &mut CompileContext,
        rt_func: RuntimeFunc,
    ) -> Result<FuncRef> {
        if let Some(&func_ref) = self.runtime_refs.get(&rt_func) {
            return Ok(func_ref);
        }

        let func_id = ctx.get_or_declare_runtime(module, rt_func)?;
        let func_ref = module.declare_func_in_func(func_id, builder.func);
        self.runtime_refs.insert(rt_func, func_ref);
        Ok(func_ref)
    }

    /// Translate a single instruction.
    fn translate_instruction<M: Module>(
        &mut self,
        builder: &mut FunctionBuilder,
        inst: &Instruction,
        pc: usize,
        _code: &[Instruction],
        ctx: &mut CompileContext,
        module: &mut M,
    ) -> Result<()> {

        match inst.opcode() {
            // ==================== Load/Store ====================
            Opcode::Nop => {}

            Opcode::LoadNil | Opcode::LoadFalse => {
                let zero = builder.ins().iconst(I64, 0);
                builder.def_var(self.variables[inst.a as usize], zero);
            }

            Opcode::LoadTrue => {
                let one = builder.ins().iconst(I64, 1);
                builder.def_var(self.variables[inst.a as usize], one);
            }

            Opcode::LoadInt => {
                let imm = inst.imm32() as i64;
                let val = builder.ins().iconst(I64, imm);
                builder.def_var(self.variables[inst.a as usize], val);
            }

            Opcode::LoadConst => {
                let const_idx = inst.b;
                match ctx.get_constant(const_idx) {
                    Some(Constant::Int(v)) => {
                        let val = builder.ins().iconst(I64, *v);
                        builder.def_var(self.variables[inst.a as usize], val);
                    }
                    Some(Constant::Float(v)) => {
                        let val = builder.ins().f64const(*v);
                        let bits = builder.ins().bitcast(I64, cranelift_codegen::ir::MemFlags::new(), val);
                        builder.def_var(self.variables[inst.a as usize], bits);
                    }
                    Some(Constant::Bool(v)) => {
                        let val = builder.ins().iconst(I64, if *v { 1 } else { 0 });
                        builder.def_var(self.variables[inst.a as usize], val);
                    }
                    Some(Constant::Nil) => {
                        let val = builder.ins().iconst(I64, 0);
                        builder.def_var(self.variables[inst.a as usize], val);
                    }
                    Some(Constant::String(s)) => {
                        // Create string data and call runtime to create string object
                        let str_len = s.len();
                        let data_id = ctx.get_or_create_string_data(module, const_idx)?;
                        let gv = module.declare_data_in_func(data_id, builder.func);
                        let ptr = builder.ins().global_value(I64, gv);
                        let len = builder.ins().iconst(I64, str_len as i64);
                        let type_id = builder.ins().iconst(cranelift_codegen::ir::types::I32, 1); // STRING type_id
                        
                        let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::StringFromPtr)?;
                        let call = builder.ins().call(func_ref, &[ptr, len, type_id]);
                        let result = builder.inst_results(call)[0];
                        builder.def_var(self.variables[inst.a as usize], result);
                    }
                    None => bail!("Constant {} not found", const_idx),
                }
            }

            Opcode::Mov => {
                let val = builder.use_var(self.variables[inst.b as usize]);
                builder.def_var(self.variables[inst.a as usize], val);
            }

            Opcode::MovN => {
                for i in 0..inst.c {
                    let val = builder.use_var(self.variables[(inst.b + i) as usize]);
                    builder.def_var(self.variables[(inst.a + i) as usize], val);
                }
            }

            // ==================== Globals ====================
            Opcode::GetGlobal => {
                // a = globals[b]
                let idx = builder.ins().iconst(I64, inst.b as i64);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::GetGlobal)?;
                let call = builder.ins().call(func_ref, &[idx]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::SetGlobal => {
                // globals[a] = b
                let idx = builder.ins().iconst(I64, inst.a as i64);
                let val = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::SetGlobal)?;
                builder.ins().call(func_ref, &[idx, val]);
            }

            // ==================== Arithmetic (i64) ====================
            Opcode::AddI64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let result = builder.ins().iadd(lhs, rhs);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::SubI64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let result = builder.ins().isub(lhs, rhs);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::MulI64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let result = builder.ins().imul(lhs, rhs);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::DivI64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let result = builder.ins().sdiv(lhs, rhs);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::ModI64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let result = builder.ins().srem(lhs, rhs);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::NegI64 => {
                let val = builder.use_var(self.variables[inst.b as usize]);
                let result = builder.ins().ineg(val);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            // ==================== Arithmetic (f64) ====================
            Opcode::AddF64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let lhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), lhs);
                let rhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), rhs);
                let result = builder.ins().fadd(lhs_f, rhs_f);
                let result_i = builder.ins().bitcast(I64, cranelift_codegen::ir::MemFlags::new(), result);
                builder.def_var(self.variables[inst.a as usize], result_i);
            }

            Opcode::SubF64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let lhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), lhs);
                let rhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), rhs);
                let result = builder.ins().fsub(lhs_f, rhs_f);
                let result_i = builder.ins().bitcast(I64, cranelift_codegen::ir::MemFlags::new(), result);
                builder.def_var(self.variables[inst.a as usize], result_i);
            }

            Opcode::MulF64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let lhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), lhs);
                let rhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), rhs);
                let result = builder.ins().fmul(lhs_f, rhs_f);
                let result_i = builder.ins().bitcast(I64, cranelift_codegen::ir::MemFlags::new(), result);
                builder.def_var(self.variables[inst.a as usize], result_i);
            }

            Opcode::DivF64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let lhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), lhs);
                let rhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), rhs);
                let result = builder.ins().fdiv(lhs_f, rhs_f);
                let result_i = builder.ins().bitcast(I64, cranelift_codegen::ir::MemFlags::new(), result);
                builder.def_var(self.variables[inst.a as usize], result_i);
            }

            Opcode::NegF64 => {
                let val = builder.use_var(self.variables[inst.b as usize]);
                let val_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), val);
                let result = builder.ins().fneg(val_f);
                let result_i = builder.ins().bitcast(I64, cranelift_codegen::ir::MemFlags::new(), result);
                builder.def_var(self.variables[inst.a as usize], result_i);
            }

            // ==================== Comparison (i64) ====================
            Opcode::EqI64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let cmp = builder.ins().icmp(IntCC::Equal, lhs, rhs);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::NeI64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let cmp = builder.ins().icmp(IntCC::NotEqual, lhs, rhs);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::LtI64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let cmp = builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::LeI64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let cmp = builder.ins().icmp(IntCC::SignedLessThanOrEqual, lhs, rhs);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::GtI64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let cmp = builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::GeI64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let cmp = builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            // ==================== Comparison (f64) ====================
            Opcode::EqF64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let lhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), lhs);
                let rhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), rhs);
                let cmp = builder.ins().fcmp(FloatCC::Equal, lhs_f, rhs_f);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::NeF64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let lhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), lhs);
                let rhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), rhs);
                let cmp = builder.ins().fcmp(FloatCC::NotEqual, lhs_f, rhs_f);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::LtF64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let lhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), lhs);
                let rhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), rhs);
                let cmp = builder.ins().fcmp(FloatCC::LessThan, lhs_f, rhs_f);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::LeF64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let lhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), lhs);
                let rhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), rhs);
                let cmp = builder.ins().fcmp(FloatCC::LessThanOrEqual, lhs_f, rhs_f);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::GtF64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let lhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), lhs);
                let rhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), rhs);
                let cmp = builder.ins().fcmp(FloatCC::GreaterThan, lhs_f, rhs_f);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::GeF64 => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let lhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), lhs);
                let rhs_f = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), rhs);
                let cmp = builder.ins().fcmp(FloatCC::GreaterThanOrEqual, lhs_f, rhs_f);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            // ==================== Reference comparison ====================
            Opcode::EqRef => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let cmp = builder.ins().icmp(IntCC::Equal, lhs, rhs);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::NeRef => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let cmp = builder.ins().icmp(IntCC::NotEqual, lhs, rhs);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::IsNil => {
                let val = builder.use_var(self.variables[inst.b as usize]);
                let zero = builder.ins().iconst(I64, 0);
                let cmp = builder.ins().icmp(IntCC::Equal, val, zero);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            // ==================== Bitwise ====================
            Opcode::Band => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let result = builder.ins().band(lhs, rhs);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::Bor => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let result = builder.ins().bor(lhs, rhs);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::Bxor => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let result = builder.ins().bxor(lhs, rhs);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::Bnot => {
                let val = builder.use_var(self.variables[inst.b as usize]);
                let result = builder.ins().bnot(val);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::Shl => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let result = builder.ins().ishl(lhs, rhs);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::Shr => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let result = builder.ins().sshr(lhs, rhs);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::Ushr => {
                let lhs = builder.use_var(self.variables[inst.b as usize]);
                let rhs = builder.use_var(self.variables[inst.c as usize]);
                let result = builder.ins().ushr(lhs, rhs);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            // ==================== Logical ====================
            Opcode::Not => {
                let val = builder.use_var(self.variables[inst.b as usize]);
                let zero = builder.ins().iconst(I64, 0);
                let cmp = builder.ins().icmp(IntCC::Equal, val, zero);
                let result = builder.ins().uextend(I64, cmp);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            // ==================== Control flow ====================
            Opcode::Jump => {
                let offset = inst.imm32();
                let target_pc = (pc as i32 + offset) as usize;
                let target_block = self.blocks.get(&target_pc)
                    .ok_or_else(|| anyhow::anyhow!("Jump target block not found: {}", target_pc))?;
                builder.ins().jump(*target_block, &[]);
            }

            Opcode::JumpIf => {
                let cond = builder.use_var(self.variables[inst.a as usize]);
                let offset = inst.imm32();
                let target_pc = (pc as i32 + offset) as usize;
                let fallthrough_pc = pc + 1;

                let target_block = *self.blocks.get(&target_pc)
                    .ok_or_else(|| anyhow::anyhow!("JumpIf target block not found"))?;
                let fallthrough_block = *self.blocks.get(&fallthrough_pc)
                    .ok_or_else(|| anyhow::anyhow!("JumpIf fallthrough block not found"))?;

                let zero = builder.ins().iconst(I64, 0);
                let cmp = builder.ins().icmp(IntCC::NotEqual, cond, zero);
                builder.ins().brif(cmp, target_block, &[], fallthrough_block, &[]);
            }

            Opcode::JumpIfNot => {
                let cond = builder.use_var(self.variables[inst.a as usize]);
                let offset = inst.imm32();
                let target_pc = (pc as i32 + offset) as usize;
                let fallthrough_pc = pc + 1;

                let target_block = *self.blocks.get(&target_pc)
                    .ok_or_else(|| anyhow::anyhow!("JumpIfNot target block not found"))?;
                let fallthrough_block = *self.blocks.get(&fallthrough_pc)
                    .ok_or_else(|| anyhow::anyhow!("JumpIfNot fallthrough block not found"))?;

                let zero = builder.ins().iconst(I64, 0);
                let cmp = builder.ins().icmp(IntCC::Equal, cond, zero);
                builder.ins().brif(cmp, target_block, &[], fallthrough_block, &[]);
            }

            // ==================== Function call ====================
            Opcode::Call => {
                let func_idx = inst.a as u32;
                let arg_start = inst.b;
                let arg_count = inst.c as usize;

                let func_ref = self.get_gox_func_ref(builder, module, ctx, func_idx)?;

                // Collect arguments
                let mut args = Vec::with_capacity(arg_count);
                for i in 0..arg_count {
                    args.push(builder.use_var(self.variables[(arg_start as usize) + i]));
                }

                // Call
                let call = builder.ins().call(func_ref, &args);

                // Store ALL return values from the callee (based on Cranelift's actual returns)
                // This matches VM behavior where return values are copied based on callee's Return
                let results: Vec<_> = builder.inst_results(call).to_vec();
                for (i, result) in results.into_iter().enumerate() {
                    builder.def_var(self.variables[(arg_start as usize) + i], result);
                }
            }

            Opcode::Return => {
                let ret_count = inst.b as usize;
                if ret_count == 0 {
                    builder.ins().return_(&[]);
                } else {
                    let mut rets = Vec::with_capacity(ret_count);
                    for i in 0..ret_count {
                        rets.push(builder.use_var(self.variables[(inst.a + i as u16) as usize]));
                    }
                    builder.ins().return_(&rets);
                }
            }

            // ==================== Object operations ====================
            Opcode::Alloc => {
                let type_id = inst.b as i64;
                let extra_slots = inst.c as i64;
                
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::GcAlloc)?;
                let type_id_val = builder.ins().iconst(cranelift_codegen::ir::types::I32, type_id);
                let slots_val = builder.ins().iconst(I64, extra_slots);
                
                let call = builder.ins().call(func_ref, &[type_id_val, slots_val]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::GetField => {
                let obj = builder.use_var(self.variables[inst.b as usize]);
                let field_idx = inst.c as i64;
                
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::GcReadSlot)?;
                let idx_val = builder.ins().iconst(I64, field_idx);
                
                let call = builder.ins().call(func_ref, &[obj, idx_val]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::SetField => {
                let obj = builder.use_var(self.variables[inst.a as usize]);
                let field_idx = inst.b as i64;
                let val = builder.use_var(self.variables[inst.c as usize]);
                
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::GcWriteSlot)?;
                let idx_val = builder.ins().iconst(I64, field_idx);
                
                builder.ins().call(func_ref, &[obj, idx_val, val]);
            }

            Opcode::GetFieldN => {
                // a=dest_start, b=obj, c=count, flags=field_idx
                let obj = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::GcReadSlot)?;
                for i in 0..inst.c {
                    let idx = builder.ins().iconst(I64, (inst.flags + i as u8) as i64);
                    let call = builder.ins().call(func_ref, &[obj, idx]);
                    let val = builder.inst_results(call)[0];
                    builder.def_var(self.variables[(inst.a + i) as usize], val);
                }
            }

            Opcode::SetFieldN => {
                // a=obj, b=src_start, c=count, flags=field_idx
                let obj = builder.use_var(self.variables[inst.a as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::GcWriteSlot)?;
                for i in 0..inst.c {
                    let val = builder.use_var(self.variables[(inst.b + i) as usize]);
                    let idx = builder.ins().iconst(I64, (inst.flags + i as u8) as i64);
                    builder.ins().call(func_ref, &[obj, idx, val]);
                }
            }

            // ==================== Array operations ====================
            Opcode::ArrayNew => {
                // a=dest, b=elem_type, c=len
                let type_id = builder.ins().iconst(cranelift_codegen::ir::types::I32, 1); // ARRAY type_id
                let elem_type = builder.ins().iconst(cranelift_codegen::ir::types::I32, inst.b as i64);
                let elem_size = builder.ins().iconst(I64, 1); // Default elem_size=1
                let len = builder.ins().iconst(I64, inst.c as i64);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::ArrayCreate)?;
                let call = builder.ins().call(func_ref, &[type_id, elem_type, elem_size, len]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::ArrayGet => {
                let arr = builder.use_var(self.variables[inst.b as usize]);
                let idx = builder.use_var(self.variables[inst.c as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::ArrayGet)?;
                let call = builder.ins().call(func_ref, &[arr, idx]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::ArraySet => {
                let arr = builder.use_var(self.variables[inst.a as usize]);
                let idx = builder.use_var(self.variables[inst.b as usize]);
                let val = builder.use_var(self.variables[inst.c as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::ArraySet)?;
                builder.ins().call(func_ref, &[arr, idx, val]);
            }

            Opcode::ArrayLen => {
                let arr = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::ArrayLen)?;
                let call = builder.ins().call(func_ref, &[arr]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            // ==================== Slice operations ====================
            Opcode::SliceNew => {
                // a=dest, b=array, c=start, flags=end
                let arr = builder.use_var(self.variables[inst.b as usize]);
                let start = builder.ins().iconst(I64, inst.c as i64);
                let end = inst.flags as i64;
                let len = builder.ins().iconst(I64, end - inst.c as i64);
                // Cap = array_len - start (simplified, using end - start for now)
                let cap = builder.ins().iconst(I64, end - inst.c as i64);
                let type_id = builder.ins().iconst(cranelift_codegen::ir::types::I32, 1); // SLICE type_id
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::SliceCreate)?;
                let call = builder.ins().call(func_ref, &[type_id, arr, start, len, cap]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::SliceSlice => {
                // a=dest, b=slice, c=start_reg, flags=end_reg
                let slice = builder.use_var(self.variables[inst.b as usize]);
                let start = builder.use_var(self.variables[inst.c as usize]);
                let end = builder.use_var(self.variables[inst.flags as usize]);
                let type_id = builder.ins().iconst(cranelift_codegen::ir::types::I32, 1); // SLICE type_id
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::SliceSlice)?;
                let call = builder.ins().call(func_ref, &[type_id, slice, start, end]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::SliceAppend => {
                // a=dest, b=slice, c=value
                let slice = builder.use_var(self.variables[inst.b as usize]);
                let val = builder.use_var(self.variables[inst.c as usize]);
                let type_id = builder.ins().iconst(cranelift_codegen::ir::types::I32, 1); // SLICE type_id
                let arr_type_id = builder.ins().iconst(cranelift_codegen::ir::types::I32, 1); // ARRAY type_id
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::SliceAppend)?;
                let call = builder.ins().call(func_ref, &[type_id, arr_type_id, slice, val]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::SliceLen => {
                let slice = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::SliceLen)?;
                let call = builder.ins().call(func_ref, &[slice]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::SliceCap => {
                let slice = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::SliceCap)?;
                let call = builder.ins().call(func_ref, &[slice]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::SliceGet => {
                let slice = builder.use_var(self.variables[inst.b as usize]);
                let idx = builder.use_var(self.variables[inst.c as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::SliceGet)?;
                let call = builder.ins().call(func_ref, &[slice, idx]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::SliceSet => {
                let slice = builder.use_var(self.variables[inst.a as usize]);
                let idx = builder.use_var(self.variables[inst.b as usize]);
                let val = builder.use_var(self.variables[inst.c as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::SliceSet)?;
                builder.ins().call(func_ref, &[slice, idx, val]);
            }

            // ==================== Map operations ====================
            Opcode::MapNew => {
                let key_type = inst.b as i64;
                let val_type = inst.c as i64;
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::MapCreate)?;
                let kt = builder.ins().iconst(cranelift_codegen::ir::types::I32, key_type);
                let vt = builder.ins().iconst(cranelift_codegen::ir::types::I32, val_type);
                let call = builder.ins().call(func_ref, &[kt, vt]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::MapLen => {
                let m = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::MapLen)?;
                let call = builder.ins().call(func_ref, &[m]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::MapGet => {
                let m = builder.use_var(self.variables[inst.a as usize]);
                let key = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::MapGet)?;
                let call = builder.ins().call(func_ref, &[m, key]);
                // Copy results to avoid borrow conflict
                let results: Vec<_> = builder.inst_results(call).to_vec();
                builder.def_var(self.variables[inst.a as usize], results[0]); // value
                // ok stored at inst.c if needed
                if inst.c > 0 {
                    let ok = builder.ins().uextend(I64, results[1]);
                    builder.def_var(self.variables[inst.c as usize], ok);
                }
            }

            Opcode::MapSet => {
                let m = builder.use_var(self.variables[inst.a as usize]);
                let key = builder.use_var(self.variables[inst.b as usize]);
                let val = builder.use_var(self.variables[inst.c as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::MapSet)?;
                builder.ins().call(func_ref, &[m, key, val]);
            }

            Opcode::MapDelete => {
                let m = builder.use_var(self.variables[inst.a as usize]);
                let key = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::MapDelete)?;
                builder.ins().call(func_ref, &[m, key]);
            }

            // ==================== String operations ====================
            Opcode::StrLen => {
                let s = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::StringLen)?;
                let call = builder.ins().call(func_ref, &[s]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::StrConcat => {
                // gox_rt_string_concat(type_id, a, b) -> GcRef
                let a = builder.use_var(self.variables[inst.b as usize]);
                let b = builder.use_var(self.variables[inst.c as usize]);
                let type_id = builder.ins().iconst(cranelift_codegen::ir::types::I32, 1); // STRING type_id
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::StringConcat)?;
                let call = builder.ins().call(func_ref, &[type_id, a, b]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::StrEq => {
                let a = builder.use_var(self.variables[inst.b as usize]);
                let b = builder.use_var(self.variables[inst.c as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::StringEq)?;
                let call = builder.ins().call(func_ref, &[a, b]);
                let result = builder.inst_results(call)[0];
                let result_i64 = builder.ins().uextend(I64, result);
                builder.def_var(self.variables[inst.a as usize], result_i64);
            }

            Opcode::StrNe => {
                let a = builder.use_var(self.variables[inst.b as usize]);
                let b = builder.use_var(self.variables[inst.c as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::StringNe)?;
                let call = builder.ins().call(func_ref, &[a, b]);
                let result = builder.inst_results(call)[0];
                let result_i64 = builder.ins().uextend(I64, result);
                builder.def_var(self.variables[inst.a as usize], result_i64);
            }

            Opcode::StrIndex => {
                // a = b[c] (get byte at index)
                let s = builder.use_var(self.variables[inst.b as usize]);
                let idx = builder.use_var(self.variables[inst.c as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::StringIndex)?;
                let call = builder.ins().call(func_ref, &[s, idx]);
                let result = builder.inst_results(call)[0];
                let result_i64 = builder.ins().uextend(I64, result);
                builder.def_var(self.variables[inst.a as usize], result_i64);
            }

            // ==================== Type conversion ====================
            Opcode::I64ToF64 => {
                let val = builder.use_var(self.variables[inst.b as usize]);
                let float_val = builder.ins().fcvt_from_sint(F64, val);
                let bits = builder.ins().bitcast(I64, cranelift_codegen::ir::MemFlags::new(), float_val);
                builder.def_var(self.variables[inst.a as usize], bits);
            }

            Opcode::F64ToI64 => {
                let val = builder.use_var(self.variables[inst.b as usize]);
                let float_val = builder.ins().bitcast(F64, cranelift_codegen::ir::MemFlags::new(), val);
                let int_val = builder.ins().fcvt_to_sint(I64, float_val);
                builder.def_var(self.variables[inst.a as usize], int_val);
            }

            Opcode::I32ToI64 => {
                // Sign-extend i32 to i64
                let val = builder.use_var(self.variables[inst.b as usize]);
                let i32_val = builder.ins().ireduce(cranelift_codegen::ir::types::I32, val);
                let result = builder.ins().sextend(I64, i32_val);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::I64ToI32 => {
                // Truncate i64 to i32, then sign-extend back to i64 for storage
                let val = builder.use_var(self.variables[inst.b as usize]);
                let i32_val = builder.ins().ireduce(cranelift_codegen::ir::types::I32, val);
                let result = builder.ins().sextend(I64, i32_val);
                builder.def_var(self.variables[inst.a as usize], result);
            }

            // ==================== Interface ====================
            Opcode::BoxInterface => {
                // a=dest (2 slots), b=type_id, c=value
                // Interface is stored as 2 i64 slots: [type_id, data]
                let type_id = builder.ins().iconst(I64, inst.b as i64);
                let val = builder.use_var(self.variables[inst.c as usize]);
                builder.def_var(self.variables[inst.a as usize], type_id);
                builder.def_var(self.variables[(inst.a + 1) as usize], val);
            }

            Opcode::UnboxInterface => {
                // a=dest, b=interface (2 slots), c=type_reg
                // Extract type_id and data from interface slots
                let type_id = builder.use_var(self.variables[inst.b as usize]);
                let data = builder.use_var(self.variables[(inst.b + 1) as usize]);
                builder.def_var(self.variables[inst.a as usize], data);
                builder.def_var(self.variables[inst.c as usize], type_id);
            }

            Opcode::TypeAssert => {
                // a=dest, b=interface (2 slots), c=expected_type, flags=ok_reg
                let actual_type = builder.use_var(self.variables[inst.b as usize]);
                let data = builder.use_var(self.variables[(inst.b + 1) as usize]);
                let expected_type = builder.ins().iconst(I64, inst.c as i64);
                
                // Compare types
                let types_match = builder.ins().icmp(IntCC::Equal, actual_type, expected_type);
                let ok = builder.ins().uextend(I64, types_match);
                
                // Select data or 0 based on match
                let zero = builder.ins().iconst(I64, 0);
                let result = builder.ins().select(types_match, data, zero);
                
                builder.def_var(self.variables[inst.a as usize], result);
                builder.def_var(self.variables[inst.flags as usize], ok);
            }

            // ==================== Channel operations ====================
            Opcode::ChanNew => {
                // a=dest, b=elem_type, c=capacity
                // For AOT, we create the channel via runtime
                // Channel is stored as GcRef, capacity in c
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::GcAlloc)?;
                let type_id = builder.ins().iconst(cranelift_codegen::ir::types::I32, inst.b as i64);
                let cap = builder.ins().iconst(I64, inst.c as i64);
                let call = builder.ins().call(func_ref, &[type_id, cap]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::ChanSend => {
                // a=chan, b=value
                let ch = builder.use_var(self.variables[inst.a as usize]);
                let val = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::ChanSend)?;
                builder.ins().call(func_ref, &[ch, val]);
            }

            Opcode::ChanRecv => {
                // a=dest, b=chan, c=ok_dest
                let ch = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::ChanRecv)?;
                // Pass null for ok_ptr if c is 0
                let ok_ptr = builder.ins().iconst(I64, 0);
                let call = builder.ins().call(func_ref, &[ch, ok_ptr]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::ChanClose => {
                // a=chan
                let ch = builder.use_var(self.variables[inst.a as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::ChanClose)?;
                builder.ins().call(func_ref, &[ch]);
            }

            // ==================== Goroutine operations ====================
            Opcode::Go => {
                // a=func_id, b=arg_start, c=arg_count
                let func_idx = inst.a as u32;
                let arg_start = inst.b;
                let arg_count = inst.c as usize;
                
                // Get function address
                let target_func_ref = self.get_gox_func_ref(builder, module, ctx, func_idx)?;
                let func_addr = builder.ins().func_addr(I64, target_func_ref);
                
                // Allocate stack space for arguments (even if 0, pass valid pointer)
                let slot = builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
                    cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                    ((arg_count.max(1)) * 8) as u32,
                    8,
                ));
                let args_ptr = builder.ins().stack_addr(I64, slot, 0);
                
                // Store arguments to stack
                for i in 0..arg_count {
                    let arg = builder.use_var(self.variables[(arg_start as usize) + i]);
                    let offset = (i * 8) as i32;
                    builder.ins().store(
                        cranelift_codegen::ir::MemFlags::new(),
                        arg,
                        args_ptr,
                        offset,
                    );
                }
                
                // Call runtime to spawn goroutine
                let spawn_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::GoSpawn)?;
                let count = builder.ins().iconst(I64, arg_count as i64);
                builder.ins().call(spawn_ref, &[func_addr, args_ptr, count]);
            }

            Opcode::Yield => {
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::GoYield)?;
                builder.ins().call(func_ref, &[]);
            }

            // ==================== Defer/Panic/Recover ====================
            Opcode::DeferPush => {
                // a=func_id, b=arg_start, c=arg_count
                let func_idx = inst.a as u32;
                let arg_start = inst.b;
                let arg_count = inst.c as usize;
                
                // Get function address
                let target_func_ref = self.get_gox_func_ref(builder, module, ctx, func_idx)?;
                let func_addr = builder.ins().func_addr(I64, target_func_ref);
                
                // Allocate stack space for arguments
                let slot = builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
                    cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                    ((arg_count.max(1)) * 8) as u32,
                    8,
                ));
                let args_ptr = builder.ins().stack_addr(I64, slot, 0);
                
                // Store arguments to stack
                for i in 0..arg_count {
                    let arg = builder.use_var(self.variables[(arg_start as usize) + i]);
                    let offset = (i * 8) as i32;
                    builder.ins().store(
                        cranelift_codegen::ir::MemFlags::new(),
                        arg,
                        args_ptr,
                        offset,
                    );
                }
                
                // Call runtime to push defer
                let defer_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::DeferPush)?;
                let count = builder.ins().iconst(I64, arg_count as i64);
                builder.ins().call(defer_ref, &[func_addr, args_ptr, count]);
            }

            Opcode::DeferPop => {
                // Execute all defers for current frame
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::DeferPop)?;
                builder.ins().call(func_ref, &[]);
            }

            Opcode::ErrDeferPush => {
                // For now, treat errdefer same as defer
                // TODO: Add is_errdefer flag to runtime
                let func_idx = inst.a as u32;
                let arg_start = inst.b;
                let arg_count = inst.c as usize;
                
                let target_func_ref = self.get_gox_func_ref(builder, module, ctx, func_idx)?;
                let func_addr = builder.ins().func_addr(I64, target_func_ref);
                
                let slot = builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
                    cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                    ((arg_count.max(1)) * 8) as u32,
                    8,
                ));
                let args_ptr = builder.ins().stack_addr(I64, slot, 0);
                
                for i in 0..arg_count {
                    let arg = builder.use_var(self.variables[(arg_start as usize) + i]);
                    let offset = (i * 8) as i32;
                    builder.ins().store(
                        cranelift_codegen::ir::MemFlags::new(),
                        arg,
                        args_ptr,
                        offset,
                    );
                }
                
                let defer_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::DeferPush)?;
                let count = builder.ins().iconst(I64, arg_count as i64);
                builder.ins().call(defer_ref, &[func_addr, args_ptr, count]);
            }

            Opcode::Panic => {
                // a=value
                let val = builder.use_var(self.variables[inst.a as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::Panic)?;
                builder.ins().call(func_ref, &[val]);
                // Panic doesn't return, but Cranelift needs control flow
                // The runtime will longjmp or abort
            }

            Opcode::Recover => {
                // a=dest
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::Recover)?;
                let call = builder.ins().call(func_ref, &[]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            // ==================== Select ====================
            Opcode::SelectStart => {
                // a=case_count, b=has_default
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::SelectStart)?;
                let case_count = builder.ins().iconst(I64, inst.a as i64);
                let has_default = builder.ins().iconst(I64, inst.b as i64);
                builder.ins().call(func_ref, &[case_count, has_default]);
            }

            Opcode::SelectSend => {
                // a=chan, b=value
                let chan = builder.use_var(self.variables[inst.a as usize]);
                let val = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::SelectAddSend)?;
                builder.ins().call(func_ref, &[chan, val]);
            }

            Opcode::SelectRecv => {
                // a=dest, b=chan, c=ok_dest
                let chan = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::SelectAddRecv)?;
                let call = builder.ins().call(func_ref, &[chan]);
                let results = builder.inst_results(call);
                let value = results[0];
                let ok = results[1];
                builder.def_var(self.variables[inst.a as usize], value);
                if inst.c != 0 {
                    builder.def_var(self.variables[inst.c as usize], ok);
                }
            }

            Opcode::SelectEnd => {
                // a=dest (chosen case index)
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::SelectExec)?;
                let call = builder.ins().call(func_ref, &[]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            // ==================== Iterator ====================
            Opcode::IterBegin => {
                // a=container, b=iter_type (0=slice, 1=map, 2=string)
                let container = builder.use_var(self.variables[inst.a as usize]);
                let iter_type = builder.ins().iconst(I64, inst.b as i64);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::IterBegin)?;
                let call = builder.ins().call(func_ref, &[container, iter_type]);
                let handle = builder.inst_results(call)[0];
                // Store iterator handle in a dedicated variable
                // Use a high register index to avoid conflicts
                let iter_var_idx = self.variables.len().saturating_sub(1);
                if iter_var_idx < self.variables.len() {
                    builder.def_var(self.variables[iter_var_idx], handle);
                }
            }

            Opcode::IterNext => {
                // a=index_dest, b=value_dest, c=done_offset
                // Get iterator handle from dedicated variable
                let iter_var_idx = self.variables.len().saturating_sub(1);
                let handle = if iter_var_idx < self.variables.len() {
                    builder.use_var(self.variables[iter_var_idx])
                } else {
                    builder.ins().iconst(I64, 0)
                };
                
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::IterNext)?;
                let call = builder.ins().call(func_ref, &[handle]);
                let results = builder.inst_results(call);
                let done = results[0];
                let key = results[1];
                let value = results[2];
                
                // Store key and value
                builder.def_var(self.variables[inst.a as usize], key);
                builder.def_var(self.variables[inst.b as usize], value);
                
                // Branch based on done flag
                // done=1 means jump to done_offset, done=0 means continue
                let zero = builder.ins().iconst(I64, 0);
                let is_done = builder.ins().icmp(IntCC::NotEqual, done, zero);
                
                // Calculate target PC for done case
                let done_offset = inst.c as i16 as i32;
                let target_pc = (pc as i32 + done_offset) as usize;
                
                if let Some(&target_block) = self.blocks.get(&target_pc) {
                    let continue_block = builder.create_block();
                    builder.ins().brif(is_done, target_block, &[], continue_block, &[]);
                    builder.switch_to_block(continue_block);
                }
            }

            Opcode::IterEnd => {
                // Get iterator handle and free it
                let iter_var_idx = self.variables.len().saturating_sub(1);
                let handle = if iter_var_idx < self.variables.len() {
                    builder.use_var(self.variables[iter_var_idx])
                } else {
                    builder.ins().iconst(I64, 0)
                };
                
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::IterEnd)?;
                builder.ins().call(func_ref, &[handle]);
            }

            // ==================== Closure ====================
            Opcode::ClosureNew => {
                // a=dest, b=func_id, c=upvalue_count
                let type_id = builder.ins().iconst(cranelift_codegen::ir::types::I32, 1); // CLOSURE type_id
                let func_id = builder.ins().iconst(cranelift_codegen::ir::types::I32, inst.b as i64);
                let upval_count = builder.ins().iconst(I64, inst.c as i64);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::ClosureCreate)?;
                let call = builder.ins().call(func_ref, &[type_id, func_id, upval_count]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::ClosureGet => {
                // a=dest, b=closure, c=upval_idx
                let closure = builder.use_var(self.variables[inst.b as usize]);
                let idx = builder.ins().iconst(I64, inst.c as i64);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::ClosureGetUpvalue)?;
                let call = builder.ins().call(func_ref, &[closure, idx]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::ClosureSet => {
                // a=closure, b=upval_idx, c=value
                let closure = builder.use_var(self.variables[inst.a as usize]);
                let idx = builder.ins().iconst(I64, inst.b as i64);
                let val = builder.use_var(self.variables[inst.c as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::ClosureSetUpvalue)?;
                builder.ins().call(func_ref, &[closure, idx, val]);
            }

            Opcode::ClosureCall => {
                // a=closure_reg, b=arg_start, c=arg_count, flags=ret_count
                // Get the function ID from the closure
                let closure = builder.use_var(self.variables[inst.a as usize]);
                let func_ref_get = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::ClosureFuncId)?;
                let call = builder.ins().call(func_ref_get, &[closure]);
                let func_id_val = builder.inst_results(call)[0];
                
                // For now, closure calls are complex - we'd need indirect calls
                // Just store the closure for later use (simplified)
                // TODO: Implement proper indirect function calls for closures
                let _ = func_id_val;
                
                // For basic support, we skip the actual call implementation
                // This would need indirect_call support in Cranelift
            }

            Opcode::UpvalNew => {
                // a=dest: create new upval_box for reference capture
                let type_id = builder.ins().iconst(cranelift_codegen::ir::types::I32, 1); // CLOSURE type_id
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::UpvalBoxCreate)?;
                let call = builder.ins().call(func_ref, &[type_id]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::UpvalGet => {
                // a=dest, b=upval_box: read value from upval_box
                let uv = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::UpvalBoxGet)?;
                let call = builder.ins().call(func_ref, &[uv]);
                let result = builder.inst_results(call)[0];
                builder.def_var(self.variables[inst.a as usize], result);
            }

            Opcode::UpvalSet => {
                // a=upval_box, b=value: write value to upval_box
                let uv = builder.use_var(self.variables[inst.a as usize]);
                let val = builder.use_var(self.variables[inst.b as usize]);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::UpvalBoxSet)?;
                builder.ins().call(func_ref, &[uv, val]);
            }

            // ==================== Debug/Assert ====================
            Opcode::DebugPrint => {
                // a=value_reg, b=type_tag
                let val = builder.use_var(self.variables[inst.a as usize]);
                let type_tag = builder.ins().iconst(cranelift_codegen::ir::types::I8, inst.b as i64);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::DebugPrint)?;
                builder.ins().call(func_ref, &[val, type_tag]);
            }

            Opcode::AssertBegin => {
                // a=cond, b=arg_count, c=line
                // For AOT, we always call the runtime and let it handle the logic
                let cond = builder.use_var(self.variables[inst.a as usize]);
                let arg_count = builder.ins().iconst(I64, inst.b as i64);
                let line = builder.ins().iconst(I64, inst.c as i64);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::AssertBegin)?;
                let call = builder.ins().call(func_ref, &[cond, arg_count, line]);
                // Result indicates if we should skip (1) or continue (0)
                // For simplicity in AOT, we always execute all instructions
                // The runtime functions handle the "skip" logic internally
                let _result = builder.inst_results(call)[0];
            }

            Opcode::AssertArg => {
                // a=value_reg, b=type_tag
                let val = builder.use_var(self.variables[inst.a as usize]);
                let type_tag = builder.ins().iconst(cranelift_codegen::ir::types::I8, inst.b as i64);
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::AssertArg)?;
                builder.ins().call(func_ref, &[val, type_tag]);
            }

            Opcode::AssertEnd => {
                // No args - check if assert failed and terminate if so
                let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::AssertEnd)?;
                builder.ins().call(func_ref, &[]);
            }

            // ==================== Not yet implemented ====================
            _ => {
                bail!("Opcode {:?} not yet implemented in AOT compiler", inst.opcode());
            }
        }

        Ok(())
    }
}
