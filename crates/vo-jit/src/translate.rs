//! Instruction translation: individual bytecode -> Cranelift IR.
//!
//! This module contains the translation methods for each bytecode instruction.
//! All methods are implemented on FunctionCompiler.

use cranelift_codegen::ir::{types, InstBuilder, Value};
use cranelift_codegen::ir::condcodes::{IntCC, FloatCC};

use vo_runtime::instruction::Instruction;

use crate::compiler::FunctionCompiler;

impl FunctionCompiler<'_> {
    // =========================================================================
    // Load instructions
    // =========================================================================

    pub(crate) fn translate_load_int(&mut self, inst: &Instruction) {
        let val = self.builder.ins().iconst(types::I64, inst.imm32() as i64);
        self.write_var(inst.a, val);
    }

    pub(crate) fn translate_load_const(&mut self, inst: &Instruction) {
        use vo_runtime::bytecode::Constant;
        
        let const_idx = inst.b as usize;
        let val = match &self.vo_module.constants[const_idx] {
            Constant::Nil => {
                self.reg_consts.insert(inst.a, 0);
                self.builder.ins().iconst(types::I64, 0)
            }
            Constant::Bool(b) => {
                self.reg_consts.insert(inst.a, *b as i64);
                self.builder.ins().iconst(types::I64, *b as i64)
            }
            Constant::Int(i) => {
                self.reg_consts.insert(inst.a, *i);
                self.builder.ins().iconst(types::I64, *i)
            }
            Constant::Float(f) => {
                let bits = f.to_bits() as i64;
                self.reg_consts.insert(inst.a, bits);
                self.builder.ins().iconst(types::I64, bits)
            }
            Constant::String(_) => {
                // String constants are handled by StrNew, LoadConst just returns 0
                self.builder.ins().iconst(types::I64, 0)
            }
        };
        self.write_var(inst.a, val);
    }

    // =========================================================================
    // Copy instructions
    // =========================================================================

    pub(crate) fn translate_copy(&mut self, inst: &Instruction) {
        let val = self.read_var(inst.b);
        self.write_var(inst.a, val);
    }

    pub(crate) fn translate_copy_n(&mut self, inst: &Instruction) {
        // Copy N slots from b to a
        let n = inst.c as usize;
        for i in 0..n {
            let val = self.read_var(inst.b + i as u16);
            self.write_var(inst.a + i as u16, val);
        }
    }

    // =========================================================================
    // Integer arithmetic
    // =========================================================================

    pub(crate) fn translate_add_i(&mut self, inst: &Instruction) {
        // AddI dst, src1, src2: dst = src1 + src2
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let result = self.builder.ins().iadd(a, b);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_sub_i(&mut self, inst: &Instruction) {
        // SubI dst, src1, src2: dst = src1 - src2
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let result = self.builder.ins().isub(a, b);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_mul_i(&mut self, inst: &Instruction) {
        // MulI dst, src1, src2: dst = src1 * src2
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let result = self.builder.ins().imul(a, b);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_div_i(&mut self, inst: &Instruction) {
        // DivI dst, src1, src2: dst = src1 / src2
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        // TODO: Add division by zero check?
        let result = self.builder.ins().sdiv(a, b);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_mod_i(&mut self, inst: &Instruction) {
        // ModI dst, src1, src2: dst = src1 % src2
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let result = self.builder.ins().srem(a, b);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_neg_i(&mut self, inst: &Instruction) {
        // NegI dst, src: dst = -src
        let a = self.read_var(inst.b);
        let result = self.builder.ins().ineg(a);
        self.write_var(inst.a, result);
    }

    // =========================================================================
    // Float arithmetic
    // =========================================================================

    pub(crate) fn translate_add_f(&mut self, inst: &Instruction) {
        // AddF dst, src1, src2: dst = src1 + src2
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        // Reinterpret i64 as f64
        let a_f = self.builder.ins().bitcast(types::F64, cranelift_codegen::ir::MemFlags::new(), a);
        let b_f = self.builder.ins().bitcast(types::F64, cranelift_codegen::ir::MemFlags::new(), b);
        let result = self.builder.ins().fadd(a_f, b_f);
        let result_i = self.builder.ins().bitcast(types::I64, cranelift_codegen::ir::MemFlags::new(), result);
        self.write_var(inst.a, result_i);
    }

    pub(crate) fn translate_sub_f(&mut self, inst: &Instruction) {
        // SubF dst, src1, src2: dst = src1 - src2
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let a_f = self.builder.ins().bitcast(types::F64, cranelift_codegen::ir::MemFlags::new(), a);
        let b_f = self.builder.ins().bitcast(types::F64, cranelift_codegen::ir::MemFlags::new(), b);
        let result = self.builder.ins().fsub(a_f, b_f);
        let result_i = self.builder.ins().bitcast(types::I64, cranelift_codegen::ir::MemFlags::new(), result);
        self.write_var(inst.a, result_i);
    }

    pub(crate) fn translate_mul_f(&mut self, inst: &Instruction) {
        // MulF dst, src1, src2: dst = src1 * src2
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let a_f = self.builder.ins().bitcast(types::F64, cranelift_codegen::ir::MemFlags::new(), a);
        let b_f = self.builder.ins().bitcast(types::F64, cranelift_codegen::ir::MemFlags::new(), b);
        let result = self.builder.ins().fmul(a_f, b_f);
        let result_i = self.builder.ins().bitcast(types::I64, cranelift_codegen::ir::MemFlags::new(), result);
        self.write_var(inst.a, result_i);
    }

    pub(crate) fn translate_div_f(&mut self, inst: &Instruction) {
        // DivF dst, src1, src2: dst = src1 / src2
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let a_f = self.builder.ins().bitcast(types::F64, cranelift_codegen::ir::MemFlags::new(), a);
        let b_f = self.builder.ins().bitcast(types::F64, cranelift_codegen::ir::MemFlags::new(), b);
        let result = self.builder.ins().fdiv(a_f, b_f);
        let result_i = self.builder.ins().bitcast(types::I64, cranelift_codegen::ir::MemFlags::new(), result);
        self.write_var(inst.a, result_i);
    }

    pub(crate) fn translate_neg_f(&mut self, inst: &Instruction) {
        // NegF dst, src: dst = -src
        let a = self.read_var(inst.b);
        let a_f = self.builder.ins().bitcast(types::F64, cranelift_codegen::ir::MemFlags::new(), a);
        let result = self.builder.ins().fneg(a_f);
        let result_i = self.builder.ins().bitcast(types::I64, cranelift_codegen::ir::MemFlags::new(), result);
        self.write_var(inst.a, result_i);
    }

    // =========================================================================
    // Integer comparison
    // =========================================================================

    pub(crate) fn translate_cmp_i(&mut self, inst: &Instruction, cc: IntCC) {
        // LeI dst, src1, src2: dst = src1 <= src2
        // inst.a = dst, inst.b = src1, inst.c = src2
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let cmp = self.builder.ins().icmp(cc, a, b);
        // Convert bool to i64 (0 or 1)
        let result = self.builder.ins().uextend(types::I64, cmp);
        self.write_var(inst.a, result);
    }

    // =========================================================================
    // Float comparison
    // =========================================================================

    pub(crate) fn translate_cmp_f(&mut self, inst: &Instruction, cc: FloatCC) {
        // LeF dst, src1, src2: dst = src1 <= src2
        // inst.a = dst, inst.b = src1, inst.c = src2
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let a_f = self.builder.ins().bitcast(types::F64, cranelift_codegen::ir::MemFlags::new(), a);
        let b_f = self.builder.ins().bitcast(types::F64, cranelift_codegen::ir::MemFlags::new(), b);
        let cmp = self.builder.ins().fcmp(cc, a_f, b_f);
        let result = self.builder.ins().uextend(types::I64, cmp);
        self.write_var(inst.a, result);
    }

    // =========================================================================
    // Bitwise operations
    // =========================================================================

    pub(crate) fn translate_and(&mut self, inst: &Instruction) {
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let result = self.builder.ins().band(a, b);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_or(&mut self, inst: &Instruction) {
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let result = self.builder.ins().bor(a, b);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_xor(&mut self, inst: &Instruction) {
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let result = self.builder.ins().bxor(a, b);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_and_not(&mut self, inst: &Instruction) {
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let not_b = self.builder.ins().bnot(b);
        let result = self.builder.ins().band(a, not_b);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_not(&mut self, inst: &Instruction) {
        let a = self.read_var(inst.b);
        let result = self.builder.ins().bnot(a);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_shl(&mut self, inst: &Instruction) {
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let result = self.builder.ins().ishl(a, b);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_shr_s(&mut self, inst: &Instruction) {
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let result = self.builder.ins().sshr(a, b);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_shr_u(&mut self, inst: &Instruction) {
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let result = self.builder.ins().ushr(a, b);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_bool_not(&mut self, inst: &Instruction) {
        let a = self.read_var(inst.b);
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(IntCC::Equal, a, zero);
        let result = self.builder.ins().uextend(types::I64, cmp);
        self.write_var(inst.a, result);
    }

    // =========================================================================
    // Control flow
    // =========================================================================

    pub(crate) fn translate_jump(&mut self, inst: &Instruction) {
        // Jump offset is relative to current PC
        let offset = inst.imm32();
        let target_pc = (self.current_pc as i32 + offset) as usize;
        
        // Check if this is a back-edge (loop)
        if target_pc <= self.current_pc {
            self.emit_safepoint();
        }
        
        let target_block = self.blocks[&target_pc];
        self.builder.ins().jump(target_block, &[]);
    }

    pub(crate) fn translate_jump_if(&mut self, inst: &Instruction) {
        let cond = self.read_var(inst.a);
        // Jump offset is relative to current PC
        let offset = inst.imm32();
        let target_pc = (self.current_pc as i32 + offset) as usize;
        
        let target_block = self.blocks[&target_pc];
        let fallthrough_block = self.builder.create_block();
        
        // If cond != 0, jump to target
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(IntCC::NotEqual, cond, zero);
        self.builder.ins().brif(cmp, target_block, &[], fallthrough_block, &[]);
        
        self.builder.switch_to_block(fallthrough_block);
        self.builder.seal_block(fallthrough_block);
    }

    pub(crate) fn translate_jump_if_not(&mut self, inst: &Instruction) {
        let cond = self.read_var(inst.a);
        // Jump offset is relative to current PC
        let offset = inst.imm32();
        let target_pc = (self.current_pc as i32 + offset) as usize;
        
        let target_block = self.blocks[&target_pc];
        let fallthrough_block = self.builder.create_block();
        
        // If cond == 0, jump to target
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(IntCC::Equal, cond, zero);
        self.builder.ins().brif(cmp, target_block, &[], fallthrough_block, &[]);
        
        self.builder.switch_to_block(fallthrough_block);
        self.builder.seal_block(fallthrough_block);
    }

    // =========================================================================
    // Function calls
    // =========================================================================

    pub(crate) fn translate_call(&mut self, inst: &Instruction) {
        // Direct JIT-to-JIT call optimization:
        // 1. Load function pointer from jit_func_table[func_id]
        // 2. If non-null, call JIT function directly
        // 3. Otherwise, fall back to vo_call_vm
        
        let call_vm_func = match self.call_vm_func {
            Some(f) => f,
            None => return,
        };
        
        self.emit_safepoint();
        
        // Decode instruction fields
        let func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
        let arg_start = inst.b as usize;
        let arg_slots = (inst.c >> 8) as usize;
        let ret_slots = (inst.c & 0xFF) as usize;
        
        // Allocate stack space for args and ret buffers
        let arg_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (arg_slots.max(1) * 8) as u32,
            3,
        ));
        let ret_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (ret_slots.max(1) * 8) as u32,
            3,
        ));
        
        // Copy args to stack buffer
        for i in 0..arg_slots {
            let val = self.read_var((arg_start + i) as u16);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        // Get pointers to buffers
        let arg_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        let ctx = self.get_ctx_param();
        
        // JitContext layout (offset in bytes):
        // jit_func_table: offset 112 (after iface_assert_fn)
        const JIT_FUNC_TABLE_OFFSET: i32 = 112;
        const JIT_FUNC_COUNT_OFFSET: i32 = 120;
        
        // Load jit_func_table pointer from ctx
        let func_table = self.builder.ins().load(
            types::I64,
            cranelift_codegen::ir::MemFlags::trusted(),
            ctx,
            JIT_FUNC_TABLE_OFFSET,
        );

        // Fail-fast: bounds check func_id against jit_func_count before indexing
        let func_count = self.builder.ins().load(
            types::I32,
            cranelift_codegen::ir::MemFlags::trusted(),
            ctx,
            JIT_FUNC_COUNT_OFFSET,
        );
        let func_id_i32 = self.builder.ins().iconst(types::I32, func_id as i64);
        let in_bounds = self.builder.ins().icmp(IntCC::UnsignedLessThan, func_id_i32, func_count);
        let in_bounds_block = self.builder.create_block();
        let oob_block = self.builder.create_block();
        self.builder.ins().brif(in_bounds, in_bounds_block, &[], oob_block, &[]);

        self.builder.switch_to_block(oob_block);
        self.builder.seal_block(oob_block);
        let panic_result = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_result]);

        self.builder.switch_to_block(in_bounds_block);
        self.builder.seal_block(in_bounds_block);
        
        // Calculate &jit_func_table[func_id] = func_table + func_id * 8
        let func_id_i64 = self.builder.ins().iconst(types::I64, func_id as i64);
        let offset = self.builder.ins().imul_imm(func_id_i64, 8);
        let func_ptr_addr = self.builder.ins().iadd(func_table, offset);
        
        // Load function pointer: jit_func_table[func_id]
        let func_ptr = self.builder.ins().load(
            types::I64,
            cranelift_codegen::ir::MemFlags::trusted(),
            func_ptr_addr,
            0,
        );
        
        // Check if func_ptr is non-null
        let zero = self.builder.ins().iconst(types::I64, 0);
        let is_jit = self.builder.ins().icmp(IntCC::NotEqual, func_ptr, zero);
        
        let jit_call_block = self.builder.create_block();
        let vm_call_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I32); // result param
        
        self.builder.ins().brif(is_jit, jit_call_block, &[], vm_call_block, &[]);
        
        // === JIT call block ===
        self.builder.switch_to_block(jit_call_block);
        self.builder.seal_block(jit_call_block);
        
        // Build signature for JIT function: (ctx, args, ret) -> i32
        let ptr_type = types::I64;
        let call_conv = self.builder.func.signature.call_conv;
        let mut sig = cranelift_codegen::ir::Signature::new(call_conv);
        sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type)); // ctx
        sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type)); // args
        sig.params.push(cranelift_codegen::ir::AbiParam::new(ptr_type)); // ret
        sig.returns.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // JitResult
        
        let sig_ref = self.builder.import_signature(sig);
        let jit_call = self.builder.ins().call_indirect(sig_ref, func_ptr, &[ctx, arg_ptr, ret_ptr]);
        let jit_result = self.builder.inst_results(jit_call)[0];
        self.builder.ins().jump(merge_block, &[jit_result]);
        
        // === VM call block (fallback) ===
        self.builder.switch_to_block(vm_call_block);
        self.builder.seal_block(vm_call_block);
        
        let func_id_val = self.builder.ins().iconst(types::I32, func_id as i64);
        let arg_count_val = self.builder.ins().iconst(types::I32, arg_slots as i64);
        let ret_count_val = self.builder.ins().iconst(types::I32, ret_slots as i64);
        
        let vm_call = self.builder.ins().call(
            call_vm_func,
            &[ctx, func_id_val, arg_ptr, arg_count_val, ret_ptr, ret_count_val],
        );
        let vm_result = self.builder.inst_results(vm_call)[0];
        self.builder.ins().jump(merge_block, &[vm_result]);
        
        // === Merge block ===
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        let result = self.builder.block_params(merge_block)[0];
        
        // Check for panic (result != 0)
        let panic_block = self.builder.create_block();
        let continue_block = self.builder.create_block();
        
        self.builder.ins().brif(result, panic_block, &[], continue_block, &[]);
        
        // Panic block: return JitResult::Panic
        self.builder.switch_to_block(panic_block);
        self.builder.seal_block(panic_block);
        let panic_result = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_result]);
        
        // Continue block: copy return values
        self.builder.switch_to_block(continue_block);
        self.builder.seal_block(continue_block);
        
        for i in 0..ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.write_var((arg_start + i) as u16, val);
        }
    }

    pub(crate) fn translate_call_extern(&mut self, inst: &Instruction) {
        // CallExtern: a=dst, b=extern_id, c=args_start, flags=arg_count
        // Return values are written starting at dst (inst.a)
        // In VM, ret_start = arg_start (reuses argument slots), but we use inst.a as dst
        let call_extern_func = match self.call_extern_func {
            Some(f) => f,
            None => return,
        };
        
        self.emit_safepoint();
        
        let dst = inst.a;
        let extern_id = inst.b as u32;
        let arg_start = inst.c as usize;
        let arg_count = inst.flags as usize;
        
        // Allocate stack space for args buffer
        let arg_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (arg_count.max(1) * 8) as u32,
            3,
        ));
        
        // Copy args to stack buffer
        for i in 0..arg_count {
            let val = self.read_var((arg_start + i) as u16);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        // Get pointer to arg buffer (also used for return values)
        let arg_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        
        // Call vo_call_extern(ctx, extern_id, args, arg_count, ret)
        let ctx = self.get_ctx_param();
        let extern_id_val = self.builder.ins().iconst(types::I32, extern_id as i64);
        let arg_count_val = self.builder.ins().iconst(types::I32, arg_count as i64);
        
        let call = self.builder.ins().call(
            call_extern_func,
            &[ctx, extern_id_val, arg_ptr, arg_count_val, arg_ptr], // ret = args (same buffer)
        );
        let result = self.builder.inst_results(call)[0];
        
        // Check for panic (result != 0)
        let panic_block = self.builder.create_block();
        let continue_block = self.builder.create_block();
        
        self.builder.ins().brif(result, panic_block, &[], continue_block, &[]);
        
        // Panic block: return JitResult::Panic
        self.builder.switch_to_block(panic_block);
        self.builder.seal_block(panic_block);
        let panic_result = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_result]);
        
        // Continue block: copy return values back to dst slots
        self.builder.switch_to_block(continue_block);
        self.builder.seal_block(continue_block);
        
        // Extern functions return values in the same buffer (reusing arg slots)
        // Copy them to dst (inst.a). Number of return values = arg_count (same buffer reuse)
        for i in 0..arg_count {
            let val = self.builder.ins().stack_load(types::I64, arg_slot, (i * 8) as i32);
            self.write_var(dst + i as u16, val);
        }
    }

    pub(crate) fn translate_call_closure(&mut self, inst: &Instruction) {
        // CallClosure: a = closure slot, b = arg start, c = (arg_slots << 8) | ret_slots
        let call_closure_func = match self.call_closure_func {
            Some(f) => f,
            None => return,
        };
        
        self.emit_safepoint();
        
        let closure_ref = self.read_var(inst.a);
        let arg_start = inst.b as usize;
        let arg_slots = (inst.c >> 8) as usize;
        let ret_slots = (inst.c & 0xFF) as usize;
        
        // Allocate stack space for args and ret buffers (min 8 bytes to avoid zero-size slots)
        let arg_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (arg_slots.max(1) * 8) as u32,
            3,
        ));
        let ret_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (ret_slots.max(1) * 8) as u32,
            3,
        ));
        
        // Copy args to stack buffer (excluding closure)
        for i in 0..arg_slots {
            let val = self.read_var((arg_start + i) as u16);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        // Get pointers to buffers
        let arg_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        
        // Call vo_call_closure(ctx, closure_ref, args, arg_count, ret, ret_count)
        let ctx = self.get_ctx_param();
        let arg_count_val = self.builder.ins().iconst(types::I32, arg_slots as i64);
        let ret_count_val = self.builder.ins().iconst(types::I32, ret_slots as i64);
        
        let call = self.builder.ins().call(
            call_closure_func,
            &[ctx, closure_ref, arg_ptr, arg_count_val, ret_ptr, ret_count_val],
        );
        let result = self.builder.inst_results(call)[0];
        
        // Check for panic
        let panic_block = self.builder.create_block();
        let continue_block = self.builder.create_block();
        
        self.builder.ins().brif(result, panic_block, &[], continue_block, &[]);
        
        self.builder.switch_to_block(panic_block);
        self.builder.seal_block(panic_block);
        let panic_result = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_result]);
        
        self.builder.switch_to_block(continue_block);
        self.builder.seal_block(continue_block);
        
        // Copy return values
        for i in 0..ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.write_var((arg_start + i) as u16, val);
        }
    }

    pub(crate) fn translate_call_iface(&mut self, inst: &Instruction) {
        // CallIface: a = iface slot (2 slots), b = arg start, c = (arg_slots << 8) | ret_slots, flags = method_idx
        let call_iface_func = match self.call_iface_func {
            Some(f) => f,
            None => return,
        };
        
        self.emit_safepoint();
        
        let iface_slot0 = self.read_var(inst.a);
        let iface_slot1 = self.read_var(inst.a + 1);
        let method_idx = inst.flags as u32;
        let arg_start = inst.b as usize;
        let arg_slots = (inst.c >> 8) as usize;
        let ret_slots = (inst.c & 0xFF) as usize;
        
        // Allocate stack space for args and ret buffers (min 8 bytes to avoid zero-size slots)
        let arg_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (arg_slots.max(1) * 8) as u32,
            3,
        ));
        let ret_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (ret_slots.max(1) * 8) as u32,
            3,
        ));
        
        // Copy args to stack buffer
        for i in 0..arg_slots {
            let val = self.read_var((arg_start + i) as u16);
            self.builder.ins().stack_store(val, arg_slot, (i * 8) as i32);
        }
        
        // Get pointers to buffers
        let arg_ptr = self.builder.ins().stack_addr(types::I64, arg_slot, 0);
        let ret_ptr = self.builder.ins().stack_addr(types::I64, ret_slot, 0);
        
        // Extract itab_id from slot0 and lookup func_id
        // For now, we pass 0 as func_id and let runtime resolve it
        // TODO: Access itab table from JitContext for O(1) method lookup
        let itab_id = self.builder.ins().ushr_imm(iface_slot0, 32);
        let itab_id_i32 = self.builder.ins().ireduce(types::I32, itab_id);
        
        // Call vo_call_iface(ctx, slot0, slot1, method_idx, args, arg_count, ret, ret_count, func_id)
        let ctx = self.get_ctx_param();
        let method_idx_val = self.builder.ins().iconst(types::I32, method_idx as i64);
        let arg_count_val = self.builder.ins().iconst(types::I32, arg_slots as i64);
        let ret_count_val = self.builder.ins().iconst(types::I32, ret_slots as i64);
        // func_id = 0 means runtime should resolve it
        let func_id_val = self.builder.ins().iconst(types::I32, 0);
        
        let call = self.builder.ins().call(
            call_iface_func,
            &[ctx, iface_slot0, iface_slot1, method_idx_val, arg_ptr, arg_count_val, ret_ptr, ret_count_val, func_id_val],
        );
        let result = self.builder.inst_results(call)[0];
        
        // Check for panic
        let panic_block = self.builder.create_block();
        let continue_block = self.builder.create_block();
        
        self.builder.ins().brif(result, panic_block, &[], continue_block, &[]);
        
        self.builder.switch_to_block(panic_block);
        self.builder.seal_block(panic_block);
        let panic_result = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic_result]);
        
        self.builder.switch_to_block(continue_block);
        self.builder.seal_block(continue_block);
        
        // Copy return values
        for i in 0..ret_slots {
            let val = self.builder.ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            self.write_var((arg_start + i) as u16, val);
        }
        
        let _ = itab_id_i32; // Will be used for O(1) lookup later
    }

    pub(crate) fn translate_return(&mut self, inst: &Instruction) {
        // Copy return values to ret pointer
        let ret_ptr = self.get_ret_param();
        let ret_slots = self.func_def.ret_slots as usize;
        let ret_reg = inst.a as usize;
        
        for i in 0..ret_slots {
            let val = self.read_var((ret_reg + i) as u16);
            let offset = (i * 8) as i32;
            self.builder.ins().store(
                cranelift_codegen::ir::MemFlags::trusted(),
                val,
                ret_ptr,
                offset,
            );
        }
        
        // Return JitResult::Ok (0)
        let ok = self.builder.ins().iconst(types::I32, 0);
        self.builder.ins().return_(&[ok]);
    }

    // =========================================================================
    // Global variables
    // =========================================================================

    pub(crate) fn translate_global_get(&mut self, inst: &Instruction) {
        // GlobalGet: a = dst slot, b = global index
        let globals_ptr = self.load_globals_ptr();
        let offset = (inst.b as i32) * 8;
        let val = self.builder.ins().load(
            types::I64,
            cranelift_codegen::ir::MemFlags::trusted(),
            globals_ptr,
            offset,
        );
        self.write_var(inst.a, val);
    }

    pub(crate) fn translate_global_get_n(&mut self, inst: &Instruction) {
        // GlobalGetN: a = dst slot start, b = global index start, flags = count
        let globals_ptr = self.load_globals_ptr();
        let count = inst.flags as usize;
        for i in 0..count {
            let offset = ((inst.b as usize + i) * 8) as i32;
            let val = self.builder.ins().load(
                types::I64,
                cranelift_codegen::ir::MemFlags::trusted(),
                globals_ptr,
                offset,
            );
            self.write_var(inst.a + i as u16, val);
        }
    }

    pub(crate) fn translate_global_set(&mut self, inst: &Instruction) {
        // GlobalSet: a = global index, b = src slot
        let globals_ptr = self.load_globals_ptr();
        let val = self.read_var(inst.b);
        let offset = (inst.a as i32) * 8;
        self.builder.ins().store(
            cranelift_codegen::ir::MemFlags::trusted(),
            val,
            globals_ptr,
            offset,
        );
    }

    pub(crate) fn translate_global_set_n(&mut self, inst: &Instruction) {
        // GlobalSetN: a = global index start, b = src slot start, flags = count
        let globals_ptr = self.load_globals_ptr();
        let count = inst.flags as usize;
        for i in 0..count {
            let val = self.read_var(inst.b + i as u16);
            let offset = ((inst.a as usize + i) * 8) as i32;
            self.builder.ins().store(
                cranelift_codegen::ir::MemFlags::trusted(),
                val,
                globals_ptr,
                offset,
            );
        }
    }

    // =========================================================================
    // Pointer operations (heap access)
    // =========================================================================

    pub(crate) fn translate_ptr_new(&mut self, inst: &Instruction) {
        // PtrNew: a = dst slot, b = meta_raw slot, flags = slots count
        // Call vo_gc_alloc(gc, meta, slots) -> GcRef
        let gc_alloc_func = match self.gc_alloc_func {
            Some(f) => f,
            None => return,
        };
        
        let gc_ptr = self.load_gc_ptr();
        let meta_raw = self.read_var(inst.b);
        let meta_i32 = self.builder.ins().ireduce(types::I32, meta_raw);
        let slots = self.builder.ins().iconst(types::I32, inst.flags as i64);
        
        let call = self.builder.ins().call(gc_alloc_func, &[gc_ptr, meta_i32, slots]);
        let gc_ref = self.builder.inst_results(call)[0];
        
        self.write_var(inst.a, gc_ref);
    }

    pub(crate) fn translate_ptr_get(&mut self, inst: &Instruction) {
        // Load from GcRef
        let ptr = self.read_var(inst.b);
        let offset = (inst.c as i32) * 8;
        let val = self.builder.ins().load(
            types::I64,
            cranelift_codegen::ir::MemFlags::trusted(),
            ptr,
            offset,
        );
        self.write_var(inst.a, val);
    }

    pub(crate) fn translate_ptr_set(&mut self, inst: &Instruction) {
        // PtrSet: a = ptr, b = offset, c = val
        let ptr = self.read_var(inst.a);
        let offset = (inst.b as i32) * 8;
        let val = self.read_var(inst.c);
        
        // NOTE: Write barrier integration
        // When storing a GcRef into a heap object during GC marking phase,
        // we need to call vo_gc_write_barrier to maintain tri-color invariant.
        // The check would be:
        //   if gc.is_marking && slot_types[inst.b] == GcRef {
        //       vo_gc_write_barrier(gc, ptr, offset, val)
        //   }
        // For now, we skip the barrier since:
        // 1. vo_gc_write_barrier is not yet implemented
        // 2. We need slot_types info passed to FunctionCompiler
        // TODO: Implement write barrier when GC marking is integrated
        
        self.builder.ins().store(
            cranelift_codegen::ir::MemFlags::trusted(),
            val,
            ptr,
            offset,
        );
    }

    pub(crate) fn translate_ptr_get_n(&mut self, inst: &Instruction) {
        // PtrGetN: a = dst slot start, b = ptr slot, c = offset, flags = count
        let ptr = self.read_var(inst.b);
        let count = inst.flags as usize;
        for i in 0..count {
            let offset = ((inst.c as usize + i) * 8) as i32;
            let val = self.builder.ins().load(
                types::I64,
                cranelift_codegen::ir::MemFlags::trusted(),
                ptr,
                offset,
            );
            self.write_var(inst.a + i as u16, val);
        }
    }

    pub(crate) fn translate_ptr_set_n(&mut self, inst: &Instruction) {
        // PtrSetN: a = ptr slot, b = offset, c = src slot start, flags = count
        let ptr = self.read_var(inst.a);
        let count = inst.flags as usize;
        for i in 0..count {
            let val = self.read_var(inst.c + i as u16);
            let offset = ((inst.b as usize + i) * 8) as i32;
            self.builder.ins().store(
                cranelift_codegen::ir::MemFlags::trusted(),
                val,
                ptr,
                offset,
            );
        }
    }

    // =========================================================================
    // Stack slot dynamic access
    // =========================================================================

    pub(crate) fn translate_slot_get(&mut self, inst: &Instruction) {
        // SlotGet: a = dst, b = base slot, c = idx slot
        // Stack array dynamic access: read slot[base + idx]
        let idx = self.read_var(inst.c);
        let base = inst.b as usize;
        
        // Base slot contains pointer to stack array data
        let base_ptr = self.read_var(base as u16);
        let byte_offset = self.builder.ins().imul_imm(idx, 8);
        let addr = self.builder.ins().iadd(base_ptr, byte_offset);
        let val = self.builder.ins().load(types::I64, cranelift_codegen::ir::MemFlags::trusted(), addr, 0);
        self.write_var(inst.a, val);
    }

    pub(crate) fn translate_slot_set(&mut self, inst: &Instruction) {
        // SlotSet: a = base slot, b = idx slot, c = src
        let idx = self.read_var(inst.b);
        let val = self.read_var(inst.c);
        let base = inst.a as usize;
        
        let base_val = self.read_var(base as u16);
        let byte_offset = self.builder.ins().imul_imm(idx, 8);
        let addr = self.builder.ins().iadd(base_val, byte_offset);
        self.builder.ins().store(cranelift_codegen::ir::MemFlags::trusted(), val, addr, 0);
    }

    pub(crate) fn translate_slot_get_n(&mut self, inst: &Instruction) {
        // SlotGetN: a = dst, b = base slot, c = idx slot, flags = elem_slots
        let idx = self.read_var(inst.c);
        let elem_slots = inst.flags as usize;
        let base = inst.b as usize;
        
        let base_val = self.read_var(base as u16);
        let elem_byte_offset = self.builder.ins().imul_imm(idx, (elem_slots * 8) as i64);
        let start_addr = self.builder.ins().iadd(base_val, elem_byte_offset);
        
        for i in 0..elem_slots {
            let addr = self.builder.ins().iadd_imm(start_addr, (i * 8) as i64);
            let val = self.builder.ins().load(types::I64, cranelift_codegen::ir::MemFlags::trusted(), addr, 0);
            self.write_var(inst.a + i as u16, val);
        }
    }

    pub(crate) fn translate_slot_set_n(&mut self, inst: &Instruction) {
        // SlotSetN: a = base slot, b = idx slot, c = src, flags = elem_slots
        let idx = self.read_var(inst.b);
        let elem_slots = inst.flags as usize;
        let base = inst.a as usize;
        
        let base_val = self.read_var(base as u16);
        let elem_byte_offset = self.builder.ins().imul_imm(idx, (elem_slots * 8) as i64);
        let start_addr = self.builder.ins().iadd(base_val, elem_byte_offset);
        
        for i in 0..elem_slots {
            let val = self.read_var(inst.c + i as u16);
            let addr = self.builder.ins().iadd_imm(start_addr, (i * 8) as i64);
            self.builder.ins().store(cranelift_codegen::ir::MemFlags::trusted(), val, addr, 0);
        }
    }

    // =========================================================================
    // String operations
    // =========================================================================

    pub(crate) fn translate_str_new(&mut self, inst: &Instruction) {
        // StrNew: a = dst, b = const_idx
        // Get string constant and create GcRef via vo_str_new
        use vo_runtime::bytecode::Constant;
        
        let str_new_func = match self.str_funcs.str_new {
            Some(f) => f,
            None => return,
        };
        
        let const_idx = inst.b as usize;
        if let Constant::String(s) = &self.vo_module.constants[const_idx] {
            // Create a global data section for the string bytes
            // For now, we'll embed the string bytes as a series of iconst + stores
            // This is inefficient but works for small strings
            // TODO: Use Cranelift's data sections for constant strings
            
            let gc_ptr = self.load_gc_ptr();
            let len = s.len();
            
            if len == 0 {
                // Empty string is null
                let zero = self.builder.ins().iconst(types::I64, 0);
                self.write_var(inst.a, zero);
            } else {
                // Allocate stack space for string bytes
                let stack_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
                    cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                    len as u32,
                    0,
                ));
                
                // Copy bytes to stack
                let bytes = s.as_bytes();
                for (i, &b) in bytes.iter().enumerate() {
                    let byte_val = self.builder.ins().iconst(types::I8, b as i64);
                    self.builder.ins().stack_store(byte_val, stack_slot, i as i32);
                }
                
                // Get pointer to stack data
                let data_ptr = self.builder.ins().stack_addr(types::I64, stack_slot, 0);
                let len_val = self.builder.ins().iconst(types::I64, len as i64);
                
                // Call vo_str_new(gc, data, len)
                let call = self.builder.ins().call(str_new_func, &[gc_ptr, data_ptr, len_val]);
                let result = self.builder.inst_results(call)[0];
                self.write_var(inst.a, result);
            }
        } else {
            let zero = self.builder.ins().iconst(types::I64, 0);
            self.write_var(inst.a, zero);
        }
    }

    pub(crate) fn translate_str_len(&mut self, inst: &Instruction) {
        // StrLen: a = dst, b = str
        let str_len_func = match self.str_funcs.str_len {
            Some(f) => f,
            None => return,
        };
        
        let s = self.read_var(inst.b);
        let call = self.builder.ins().call(str_len_func, &[s]);
        let result = self.builder.inst_results(call)[0];
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_str_index(&mut self, inst: &Instruction) {
        // StrIndex: a = dst, b = str, c = idx
        let str_index_func = match self.str_funcs.str_index {
            Some(f) => f,
            None => return,
        };
        
        let s = self.read_var(inst.b);
        let idx = self.read_var(inst.c);
        let call = self.builder.ins().call(str_index_func, &[s, idx]);
        let result = self.builder.inst_results(call)[0];
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_str_concat(&mut self, inst: &Instruction) {
        // StrConcat: a = dst, b = str1, c = str2
        let str_concat_func = match self.str_funcs.str_concat {
            Some(f) => f,
            None => return,
        };
        
        let gc_ptr = self.load_gc_ptr();
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let call = self.builder.ins().call(str_concat_func, &[gc_ptr, a, b]);
        let result = self.builder.inst_results(call)[0];
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_str_slice(&mut self, inst: &Instruction) {
        // StrSlice: a = dst, b = str, c = lo_slot (hi is c+1)
        let str_slice_func = match self.str_funcs.str_slice {
            Some(f) => f,
            None => return,
        };
        
        let gc_ptr = self.load_gc_ptr();
        let s = self.read_var(inst.b);
        let lo = self.read_var(inst.c);
        let hi = self.read_var(inst.c + 1);
        let call = self.builder.ins().call(str_slice_func, &[gc_ptr, s, lo, hi]);
        let result = self.builder.inst_results(call)[0];
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_str_eq(&mut self, inst: &Instruction) {
        // StrEq: a = dst, b = str1, c = str2
        let str_eq_func = match self.str_funcs.str_eq {
            Some(f) => f,
            None => return,
        };
        
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let call = self.builder.ins().call(str_eq_func, &[a, b]);
        let result = self.builder.inst_results(call)[0];
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_str_ne(&mut self, inst: &Instruction) {
        // StrNe: a = dst, b = str1, c = str2
        let str_eq_func = match self.str_funcs.str_eq {
            Some(f) => f,
            None => return,
        };
        
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let call = self.builder.ins().call(str_eq_func, &[a, b]);
        let eq_result = self.builder.inst_results(call)[0];
        // Negate: result = eq_result == 0 ? 1 : 0
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(IntCC::Equal, eq_result, zero);
        let result = self.builder.ins().uextend(types::I64, cmp);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_str_lt(&mut self, inst: &Instruction) {
        // StrLt: a = dst, b = str1, c = str2
        let str_cmp_func = match self.str_funcs.str_cmp {
            Some(f) => f,
            None => return,
        };
        
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let call = self.builder.ins().call(str_cmp_func, &[a, b]);
        let cmp_result = self.builder.inst_results(call)[0];
        // result = cmp_result < 0 ? 1 : 0
        let zero = self.builder.ins().iconst(types::I32, 0);
        let cmp = self.builder.ins().icmp(IntCC::SignedLessThan, cmp_result, zero);
        let result = self.builder.ins().uextend(types::I64, cmp);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_str_le(&mut self, inst: &Instruction) {
        let str_cmp_func = match self.str_funcs.str_cmp {
            Some(f) => f,
            None => return,
        };
        
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let call = self.builder.ins().call(str_cmp_func, &[a, b]);
        let cmp_result = self.builder.inst_results(call)[0];
        let zero = self.builder.ins().iconst(types::I32, 0);
        let cmp = self.builder.ins().icmp(IntCC::SignedLessThanOrEqual, cmp_result, zero);
        let result = self.builder.ins().uextend(types::I64, cmp);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_str_gt(&mut self, inst: &Instruction) {
        let str_cmp_func = match self.str_funcs.str_cmp {
            Some(f) => f,
            None => return,
        };
        
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let call = self.builder.ins().call(str_cmp_func, &[a, b]);
        let cmp_result = self.builder.inst_results(call)[0];
        let zero = self.builder.ins().iconst(types::I32, 0);
        let cmp = self.builder.ins().icmp(IntCC::SignedGreaterThan, cmp_result, zero);
        let result = self.builder.ins().uextend(types::I64, cmp);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_str_ge(&mut self, inst: &Instruction) {
        let str_cmp_func = match self.str_funcs.str_cmp {
            Some(f) => f,
            None => return,
        };
        
        let a = self.read_var(inst.b);
        let b = self.read_var(inst.c);
        let call = self.builder.ins().call(str_cmp_func, &[a, b]);
        let cmp_result = self.builder.inst_results(call)[0];
        let zero = self.builder.ins().iconst(types::I32, 0);
        let cmp = self.builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, cmp_result, zero);
        let result = self.builder.ins().uextend(types::I64, cmp);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_str_decode_rune(&mut self, inst: &Instruction) {
        // StrDecodeRune: a = rune_slot, b = str, c = pos
        // Writes: rune at a, width at a+1
        let str_decode_rune_func = match self.str_funcs.str_decode_rune {
            Some(f) => f,
            None => return,
        };
        
        let s = self.read_var(inst.b);
        let pos = self.read_var(inst.c);
        let call = self.builder.ins().call(str_decode_rune_func, &[s, pos]);
        let packed = self.builder.inst_results(call)[0];
        
        // Unpack: rune = packed >> 32, width = packed & 0xFFFFFFFF
        let rune = self.builder.ins().ushr_imm(packed, 32);
        let width = self.builder.ins().band_imm(packed, 0xFFFFFFFF);
        
        self.write_var(inst.a, rune);
        self.write_var(inst.a + 1, width);
    }

    // =========================================================================
    // Array operations
    // =========================================================================

    pub(crate) fn translate_array_new(&mut self, inst: &Instruction) {
        // ArrayNew: a = dst, b = elem_meta_slot, c = len_slot, flags = elem_slots
        let array_new_func = match self.array_funcs.array_new {
            Some(f) => f,
            None => return,
        };
        
        let gc_ptr = self.load_gc_ptr();
        let meta_raw = self.read_var(inst.b);
        let meta_i32 = self.builder.ins().ireduce(types::I32, meta_raw);
        let elem_slots = self.builder.ins().iconst(types::I32, inst.flags as i64);
        let len = self.read_var(inst.c);
        
        let call = self.builder.ins().call(array_new_func, &[gc_ptr, meta_i32, elem_slots, len]);
        let arr_ref = self.builder.inst_results(call)[0];
        
        self.write_var(inst.a, arr_ref);
    }

    pub(crate) fn translate_array_get(&mut self, inst: &Instruction) {
        use vo_runtime::objects::array::HEADER_SLOTS;
        use cranelift_codegen::ir::MemFlags;
        
        let arr = self.read_var(inst.b);
        let idx = self.read_var(inst.c);
        let header_bytes = (HEADER_SLOTS * 8) as i64;
        
        // flags: 0=dynamic (elem_bytes in c+1 via LoadConst), 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
        // float32 stored as f32 bits, no special handling needed (just u32)
        let (elem_bytes, needs_sext) = match inst.flags {
            0 => (self.get_const_from_reg(inst.c + 1) as usize, false),  // dynamic: elem_bytes in c+1
            0x81 => (1, true),   // int8
            0x82 => (2, true),   // int16
            0x84 => (4, true),   // int32
            0x44 => (4, false),  // float32 (f32 bits as u32)
            f => (f as usize, false),
        };
        
        if elem_bytes <= 8 {
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            let off = self.builder.ins().iadd_imm(off, header_bytes);
            let addr = self.builder.ins().iadd(arr, off);
            
            let val = match elem_bytes {
                1 => {
                    let v8 = self.builder.ins().load(types::I8, MemFlags::trusted(), addr, 0);
                    if needs_sext { self.builder.ins().sextend(types::I64, v8) }
                    else { self.builder.ins().uextend(types::I64, v8) }
                }
                2 => {
                    let v16 = self.builder.ins().load(types::I16, MemFlags::trusted(), addr, 0);
                    if needs_sext { self.builder.ins().sextend(types::I64, v16) }
                    else { self.builder.ins().uextend(types::I64, v16) }
                }
                4 => {
                    let v32 = self.builder.ins().load(types::I32, MemFlags::trusted(), addr, 0);
                    if needs_sext { self.builder.ins().sextend(types::I64, v32) }
                    else { self.builder.ins().uextend(types::I64, v32) }
                }
                _ => self.builder.ins().load(types::I64, MemFlags::trusted(), addr, 0),
            };
            self.write_var(inst.a, val);
        } else {
            let elem_slots = (elem_bytes + 7) / 8;
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            let off = self.builder.ins().iadd_imm(off, header_bytes);
            for i in 0..elem_slots {
                let slot_off = self.builder.ins().iadd_imm(off, (i * 8) as i64);
                let addr = self.builder.ins().iadd(arr, slot_off);
                let val = self.builder.ins().load(types::I64, MemFlags::trusted(), addr, 0);
                self.write_var(inst.a + i as u16, val);
            }
        }
    }

    pub(crate) fn translate_array_set(&mut self, inst: &Instruction) {
        use vo_runtime::objects::array::HEADER_SLOTS;
        use cranelift_codegen::ir::MemFlags;
        
        let arr = self.read_var(inst.a);
        let idx = self.read_var(inst.b);
        let val = self.read_var(inst.c);
        let header_bytes = (HEADER_SLOTS * 8) as i64;
        
        // flags: 0=dynamic (elem_bytes in b+1 via LoadConst), 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
        // float32 stored as f32 bits, no special handling needed
        let elem_bytes = match inst.flags {
            0 => self.get_const_from_reg(inst.b + 1) as usize,  // dynamic: elem_bytes in b+1
            0x81 => 1,  // int8
            0x82 => 2,  // int16
            0x84 | 0x44 => 4,  // int32 or float32
            f => f as usize,
        };
        
        if elem_bytes <= 8 {
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            let off = self.builder.ins().iadd_imm(off, header_bytes);
            let addr = self.builder.ins().iadd(arr, off);
            
            match elem_bytes {
                1 => {
                    let v8 = self.builder.ins().ireduce(types::I8, val);
                    self.builder.ins().store(MemFlags::trusted(), v8, addr, 0);
                }
                2 => {
                    let v16 = self.builder.ins().ireduce(types::I16, val);
                    self.builder.ins().store(MemFlags::trusted(), v16, addr, 0);
                }
                4 => {
                    let v32 = self.builder.ins().ireduce(types::I32, val);
                    self.builder.ins().store(MemFlags::trusted(), v32, addr, 0);
                }
                _ => {
                    self.builder.ins().store(MemFlags::trusted(), val, addr, 0);
                }
            }
        } else {
            let elem_slots = (elem_bytes + 7) / 8;
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            let off = self.builder.ins().iadd_imm(off, header_bytes);
            for i in 0..elem_slots {
                let v = self.read_var(inst.c + i as u16);
                let slot_off = self.builder.ins().iadd_imm(off, (i * 8) as i64);
                let addr = self.builder.ins().iadd(arr, slot_off);
                self.builder.ins().store(MemFlags::trusted(), v, addr, 0);
            }
        }
    }

    // =========================================================================
    // Slice operations
    // =========================================================================

    pub(crate) fn translate_slice_new(&mut self, inst: &Instruction) {
        // SliceNew: a = dst, b = elem_meta_slot, c = len_slot (cap at c+1), flags = elem_flags
        let slice_new_func = match self.slice_funcs.slice_new {
            Some(f) => f,
            None => return,
        };
        
        // flags: 0=dynamic (read from c+2 via LoadConst), 1-63=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
        let elem_bytes: usize = match inst.flags {
            0 => {
                // dynamic: elem_bytes loaded via LoadConst, we need to find the const index
                // Look at the instruction that loads c+2 - it should be a LoadConst
                // For now, read from register (the value is already loaded)
                // TODO: Could optimize by reading from const table directly
                0 // Will use register value
            }
            0x81 => 1,   // int8
            0x82 => 2,   // int16
            0x84 | 0x44 => 4,   // int32 or float32
            f => f as usize,
        };
        let elem_bytes_val = if inst.flags == 0 {
            let eb = self.read_var(inst.c + 2);
            self.builder.ins().ireduce(types::I32, eb)
        } else {
            self.builder.ins().iconst(types::I32, elem_bytes as i64)
        };
        
        let gc_ptr = self.load_gc_ptr();
        let meta_raw = self.read_var(inst.b);
        let meta_i32 = self.builder.ins().ireduce(types::I32, meta_raw);
        let len = self.read_var(inst.c);
        let cap = self.read_var(inst.c + 1);
        
        let call = self.builder.ins().call(slice_new_func, &[gc_ptr, meta_i32, elem_bytes_val, len, cap]);
        let slice_ref = self.builder.inst_results(call)[0];
        
        self.write_var(inst.a, slice_ref);
    }

    pub(crate) fn translate_slice_get(&mut self, inst: &Instruction) {
        use vo_runtime::objects::slice::FIELD_DATA_PTR;
        use cranelift_codegen::ir::MemFlags;
        
        let s = self.read_var(inst.b);
        let idx = self.read_var(inst.c);
        
        // flags: 0=dynamic (elem_bytes in c+1 via LoadConst), 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
        // float32 stored as f32 bits, no special handling needed
        let (elem_bytes, needs_sext) = match inst.flags {
            0 => {
                let eb = self.get_const_from_reg(inst.c + 1);
                (eb as usize, false)
            }
            0x81 => (1, true),   // int8
            0x82 => (2, true),   // int16
            0x84 => (4, true),   // int32
            0x44 => (4, false),  // float32 (f32 bits as u32)
            f => (f as usize, false),
        };
        
        // Load data_ptr directly - no need to load array and compute offset
        let data_ptr = self.builder.ins().load(types::I64, MemFlags::trusted(), s, (FIELD_DATA_PTR * 8) as i32);
        
        if elem_bytes <= 8 {
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            let addr = self.builder.ins().iadd(data_ptr, off);
            
            let val = match elem_bytes {
                1 => {
                    let v8 = self.builder.ins().load(types::I8, MemFlags::trusted(), addr, 0);
                    if needs_sext { self.builder.ins().sextend(types::I64, v8) }
                    else { self.builder.ins().uextend(types::I64, v8) }
                }
                2 => {
                    let v16 = self.builder.ins().load(types::I16, MemFlags::trusted(), addr, 0);
                    if needs_sext { self.builder.ins().sextend(types::I64, v16) }
                    else { self.builder.ins().uextend(types::I64, v16) }
                }
                4 => {
                    let v32 = self.builder.ins().load(types::I32, MemFlags::trusted(), addr, 0);
                    if needs_sext { self.builder.ins().sextend(types::I64, v32) }
                    else { self.builder.ins().uextend(types::I64, v32) }
                }
                _ => self.builder.ins().load(types::I64, MemFlags::trusted(), addr, 0),
            };
            self.write_var(inst.a, val);
        } else {
            let elem_slots = (elem_bytes + 7) / 8;
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            for i in 0..elem_slots {
                let slot_off = self.builder.ins().iadd_imm(off, (i * 8) as i64);
                let addr = self.builder.ins().iadd(data_ptr, slot_off);
                let val = self.builder.ins().load(types::I64, MemFlags::trusted(), addr, 0);
                self.write_var(inst.a + i as u16, val);
            }
        }
    }

    pub(crate) fn translate_slice_set(&mut self, inst: &Instruction) {
        use vo_runtime::objects::slice::FIELD_DATA_PTR;
        use cranelift_codegen::ir::MemFlags;
        
        let s = self.read_var(inst.a);
        let idx = self.read_var(inst.b);
        let val = self.read_var(inst.c);
        
        // flags: 0=dynamic (elem_bytes in b+1 via LoadConst), 1-8=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
        // float32 stored as f32 bits, no special handling needed
        let elem_bytes = match inst.flags {
            0 => self.get_const_from_reg(inst.b + 1) as usize,  // dynamic: elem_bytes in b+1
            0x81 => 1,  // int8
            0x82 => 2,  // int16
            0x84 | 0x44 => 4,  // int32 or float32
            f => f as usize,
        };
        
        // Load data_ptr directly - no need to load array and compute offset
        let data_ptr = self.builder.ins().load(types::I64, MemFlags::trusted(), s, (FIELD_DATA_PTR * 8) as i32);
        
        if elem_bytes <= 8 {
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            let addr = self.builder.ins().iadd(data_ptr, off);
            
            match elem_bytes {
                1 => {
                    let v8 = self.builder.ins().ireduce(types::I8, val);
                    self.builder.ins().store(MemFlags::trusted(), v8, addr, 0);
                }
                2 => {
                    let v16 = self.builder.ins().ireduce(types::I16, val);
                    self.builder.ins().store(MemFlags::trusted(), v16, addr, 0);
                }
                4 => {
                    let v32 = self.builder.ins().ireduce(types::I32, val);
                    self.builder.ins().store(MemFlags::trusted(), v32, addr, 0);
                }
                _ => {
                    self.builder.ins().store(MemFlags::trusted(), val, addr, 0);
                }
            }
        } else {
            let elem_slots = (elem_bytes + 7) / 8;
            let eb = self.builder.ins().iconst(types::I64, elem_bytes as i64);
            let off = self.builder.ins().imul(idx, eb);
            for i in 0..elem_slots {
                let v = self.read_var(inst.c + i as u16);
                let slot_off = self.builder.ins().iadd_imm(off, (i * 8) as i64);
                let addr = self.builder.ins().iadd(data_ptr, slot_off);
                self.builder.ins().store(MemFlags::trusted(), v, addr, 0);
            }
        }
    }

    pub(crate) fn translate_slice_len(&mut self, inst: &Instruction) {
        // SliceLen: a = dst, b = slice
        let slice_len_func = match self.slice_funcs.slice_len {
            Some(f) => f,
            None => return,
        };
        
        let s = self.read_var(inst.b);
        let call = self.builder.ins().call(slice_len_func, &[s]);
        let result = self.builder.inst_results(call)[0];
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_slice_cap(&mut self, inst: &Instruction) {
        // SliceCap: a = dst, b = slice
        let slice_cap_func = match self.slice_funcs.slice_cap {
            Some(f) => f,
            None => return,
        };
        
        let s = self.read_var(inst.b);
        let call = self.builder.ins().call(slice_cap_func, &[s]);
        let result = self.builder.inst_results(call)[0];
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_slice_slice(&mut self, inst: &Instruction) {
        // SliceSlice: a = dst, b = slice/array, c = lo (hi at c+1, max at c+2 if three-index)
        // flags: bit0 = is_array, bit1 = has_max
        let is_array = (inst.flags & 1) != 0;
        let has_max = (inst.flags & 2) != 0;
        
        let gc_ptr = self.load_gc_ptr();
        let src = self.read_var(inst.b);
        let lo = self.read_var(inst.c);
        let hi = self.read_var(inst.c + 1);
        
        if is_array {
            // Array slicing: arr[lo:hi] or arr[lo:hi:max]
            if has_max {
                let slice_from_array3_func = match self.slice_funcs.slice_from_array3 {
                    Some(f) => f,
                    None => return,
                };
                let max = self.read_var(inst.c + 2);
                let call = self.builder.ins().call(slice_from_array3_func, &[gc_ptr, src, lo, hi, max]);
                let result = self.builder.inst_results(call)[0];
                self.write_var(inst.a, result);
            } else {
                let slice_from_array_func = match self.slice_funcs.slice_from_array {
                    Some(f) => f,
                    None => return,
                };
                let call = self.builder.ins().call(slice_from_array_func, &[gc_ptr, src, lo, hi]);
                let result = self.builder.inst_results(call)[0];
                self.write_var(inst.a, result);
            }
        } else {
            // Slice slicing: s[lo:hi] or s[lo:hi:max]
            if has_max {
                let slice_slice3_func = match self.slice_funcs.slice_slice3 {
                    Some(f) => f,
                    None => return,
                };
                let max = self.read_var(inst.c + 2);
                let call = self.builder.ins().call(slice_slice3_func, &[gc_ptr, src, lo, hi, max]);
                let result = self.builder.inst_results(call)[0];
                self.write_var(inst.a, result);
            } else {
                let slice_slice_func = match self.slice_funcs.slice_slice {
                    Some(f) => f,
                    None => return,
                };
                let call = self.builder.ins().call(slice_slice_func, &[gc_ptr, src, lo, hi]);
                let result = self.builder.inst_results(call)[0];
                self.write_var(inst.a, result);
            }
        }
    }

    pub(crate) fn translate_slice_append(&mut self, inst: &Instruction) {
        // SliceAppend: a=dst, b=slice, c=meta_and_elem, flags=elem_flags
        // When flags!=0: c=[elem_meta], c+1..=[elem]
        // When flags==0: c=[elem_meta], c+1=[elem_bytes], c+2..=[elem]
        let slice_append_func = match self.slice_funcs.slice_append {
            Some(f) => f,
            None => return,
        };
        
        // flags: 0=dynamic (read from c+1 via LoadConst), 1-63=direct, 0x81=int8, 0x82=int16, 0x84=int32, 0x44=float32
        let (elem_bytes, elem_offset): (usize, u16) = match inst.flags {
            0 => (0, 2),  // dynamic: elem_bytes in c+1, elem at c+2 (0 means read from register)
            0x81 => (1, 1),   // int8
            0x82 => (2, 1),   // int16
            0x84 | 0x44 => (4, 1),   // int32 or float32
            f => (f as usize, 1),
        };
        let is_dynamic = inst.flags == 0;
        
        let gc_ptr = self.load_gc_ptr();
        let s = self.read_var(inst.b);
        
        // Read elem_meta from c (first slot of meta_and_elem)
        let elem_meta_i64 = self.read_var(inst.c);
        let elem_meta = self.builder.ins().ireduce(types::I32, elem_meta_i64);
        let elem_bytes_val = if is_dynamic {
            let eb = self.read_var(inst.c + 1);
            self.builder.ins().ireduce(types::I32, eb)
        } else {
            self.builder.ins().iconst(types::I32, elem_bytes as i64)
        };
        
        // Allocate fixed size (256 bytes = 32 slots) for simplicity
        let val_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            256,
            3,
        ));
        
        // Copy slots (max 32 for 256 bytes)
        let max_slots = if is_dynamic { 32 } else { (elem_bytes + 7) / 8 };
        for i in 0..max_slots {
            let val = self.read_var(inst.c + elem_offset + i as u16);
            self.builder.ins().stack_store(val, val_slot, (i * 8) as i32);
        }
        
        let val_ptr = self.builder.ins().stack_addr(types::I64, val_slot, 0);
        
        let call = self.builder.ins().call(slice_append_func, &[gc_ptr, elem_meta, elem_bytes_val, s, val_ptr]);
        let result = self.builder.inst_results(call)[0];
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_slice_addr(&mut self, inst: &Instruction) {
        // SliceAddr: a=dst, b=slice, c=index, flags=elem_bytes
        // Returns data_ptr + idx * elem_bytes
        use vo_runtime::objects::slice::FIELD_DATA_PTR;
        use cranelift_codegen::ir::MemFlags;
        
        let s = self.read_var(inst.b);
        let idx = self.read_var(inst.c);
        let elem_bytes = inst.flags as i64;
        
        // Load data_ptr from slice
        let data_ptr = self.builder.ins().load(types::I64, MemFlags::trusted(), s, (FIELD_DATA_PTR * 8) as i32);
        
        // addr = data_ptr + idx * elem_bytes
        let eb = self.builder.ins().iconst(types::I64, elem_bytes);
        let off = self.builder.ins().imul(idx, eb);
        let addr = self.builder.ins().iadd(data_ptr, off);
        
        self.write_var(inst.a, addr);
    }

    pub(crate) fn translate_array_addr(&mut self, inst: &Instruction) {
        // ArrayAddr: a=dst, b=array_gcref, c=index, flags=elem_bytes
        // Returns data_ptr + idx * elem_bytes (data starts after ArrayHeader)
        use vo_runtime::objects::array::HEADER_SLOTS;
        
        let arr = self.read_var(inst.b);
        let idx = self.read_var(inst.c);
        let elem_bytes = inst.flags as i64;
        
        // data_ptr = arr + HEADER_SLOTS * 8 (ArrayHeader is 2 slots = 16 bytes)
        let data_ptr = self.builder.ins().iadd_imm(arr, (HEADER_SLOTS * 8) as i64);
        
        // addr = data_ptr + idx * elem_bytes
        let eb = self.builder.ins().iconst(types::I64, elem_bytes);
        let off = self.builder.ins().imul(idx, eb);
        let addr = self.builder.ins().iadd(data_ptr, off);
        
        self.write_var(inst.a, addr);
    }

    // =========================================================================
    // Map operations
    // =========================================================================

    pub(crate) fn translate_map_new(&mut self, inst: &Instruction) {
        // MapNew: a = dst, b = packed_meta_slot, c = (key_slots << 8) | val_slots
        // packed_meta = (key_meta << 32) | val_meta
        let map_new_func = match self.map_funcs.map_new {
            Some(f) => f,
            None => return,
        };
        
        let gc_ptr = self.load_gc_ptr();
        let packed_meta = self.read_var(inst.b);
        
        // Unpack key_meta (high 32) and val_meta (low 32)
        let key_meta = self.builder.ins().ushr_imm(packed_meta, 32);
        let key_meta_i32 = self.builder.ins().ireduce(types::I32, key_meta);
        let val_meta_i32 = self.builder.ins().ireduce(types::I32, packed_meta);
        
        let key_slots = (inst.c >> 8) as i64;
        let val_slots = (inst.c & 0xFF) as i64;
        let key_slots_val = self.builder.ins().iconst(types::I32, key_slots);
        let val_slots_val = self.builder.ins().iconst(types::I32, val_slots);
        
        let call = self.builder.ins().call(map_new_func, &[gc_ptr, key_meta_i32, val_meta_i32, key_slots_val, val_slots_val]);
        let map_ref = self.builder.inst_results(call)[0];
        
        self.write_var(inst.a, map_ref);
    }

    pub(crate) fn translate_map_get(&mut self, inst: &Instruction) {
        // MapGet: a = dst, b = map, c = key_start, flags = (key_slots << 4) | val_slots
        let map_get_func = match self.map_funcs.map_get {
            Some(f) => f,
            None => return,
        };
        
        let m = self.read_var(inst.b);
        let key_slots = (inst.flags >> 4) as usize;
        let val_slots = (inst.flags & 0xF) as usize;
        
        // Allocate stack space for key and value
        let key_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (key_slots.max(1) * 8) as u32,
            3,
        ));
        let val_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (val_slots.max(1) * 8) as u32,
            3,
        ));
        
        // Copy key to stack
        for i in 0..key_slots {
            let val = self.read_var(inst.c + i as u16);
            self.builder.ins().stack_store(val, key_slot, (i * 8) as i32);
        }
        
        let key_ptr = self.builder.ins().stack_addr(types::I64, key_slot, 0);
        let val_ptr = self.builder.ins().stack_addr(types::I64, val_slot, 0);
        let key_slots_val = self.builder.ins().iconst(types::I32, key_slots as i64);
        let val_slots_val = self.builder.ins().iconst(types::I32, val_slots as i64);
        
        let call = self.builder.ins().call(map_get_func, &[m, key_ptr, key_slots_val, val_ptr, val_slots_val]);
        let found = self.builder.inst_results(call)[0];
        
        // Copy value back from stack (dst = inst.a for value, found flag at dst + val_slots)
        for i in 0..val_slots {
            let val = self.builder.ins().stack_load(types::I64, val_slot, (i * 8) as i32);
            self.write_var(inst.a + i as u16, val);
        }
        self.write_var(inst.a + val_slots as u16, found);
    }

    pub(crate) fn translate_map_set(&mut self, inst: &Instruction) {
        // MapSet: a = map, b = key_start, c = val_start, flags = (key_slots << 4) | val_slots
        let map_set_func = match self.map_funcs.map_set {
            Some(f) => f,
            None => return,
        };
        
        let m = self.read_var(inst.a);
        let key_slots = (inst.flags >> 4) as usize;
        let val_slots = (inst.flags & 0xF) as usize;
        
        // Allocate stack space for key and value
        let key_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (key_slots.max(1) * 8) as u32,
            3,
        ));
        let val_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (val_slots.max(1) * 8) as u32,
            3,
        ));
        
        // Copy key and value to stack
        for i in 0..key_slots {
            let val = self.read_var(inst.b + i as u16);
            self.builder.ins().stack_store(val, key_slot, (i * 8) as i32);
        }
        for i in 0..val_slots {
            let val = self.read_var(inst.c + i as u16);
            self.builder.ins().stack_store(val, val_slot, (i * 8) as i32);
        }
        
        let key_ptr = self.builder.ins().stack_addr(types::I64, key_slot, 0);
        let val_ptr = self.builder.ins().stack_addr(types::I64, val_slot, 0);
        let key_slots_val = self.builder.ins().iconst(types::I32, key_slots as i64);
        let val_slots_val = self.builder.ins().iconst(types::I32, val_slots as i64);
        
        self.builder.ins().call(map_set_func, &[m, key_ptr, key_slots_val, val_ptr, val_slots_val]);
    }

    pub(crate) fn translate_map_delete(&mut self, inst: &Instruction) {
        // MapDelete: a = map, b = key_start, flags = key_slots
        let map_delete_func = match self.map_funcs.map_delete {
            Some(f) => f,
            None => return,
        };
        
        let m = self.read_var(inst.a);
        let key_slots = inst.flags as usize;
        
        // Allocate stack space for key
        let key_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (key_slots.max(1) * 8) as u32,
            3,
        ));
        
        // Copy key to stack
        for i in 0..key_slots {
            let val = self.read_var(inst.b + i as u16);
            self.builder.ins().stack_store(val, key_slot, (i * 8) as i32);
        }
        
        let key_ptr = self.builder.ins().stack_addr(types::I64, key_slot, 0);
        let key_slots_val = self.builder.ins().iconst(types::I32, key_slots as i64);
        
        self.builder.ins().call(map_delete_func, &[m, key_ptr, key_slots_val]);
    }

    pub(crate) fn translate_map_len(&mut self, inst: &Instruction) {
        // MapLen: a = dst, b = map
        let map_len_func = match self.map_funcs.map_len {
            Some(f) => f,
            None => return,
        };
        
        let m = self.read_var(inst.b);
        let call = self.builder.ins().call(map_len_func, &[m]);
        let result = self.builder.inst_results(call)[0];
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_map_iter_get(&mut self, inst: &Instruction) {
        // MapIterGet: a = dst (key+val+done), b = map, c = idx, flags = (key_slots << 4) | val_slots
        let map_iter_get_func = match self.map_funcs.map_iter_get {
            Some(f) => f,
            None => return,
        };
        
        let m = self.read_var(inst.b);
        let idx = self.read_var(inst.c);
        let key_slots = (inst.flags >> 4) as usize;
        let val_slots = (inst.flags & 0xF) as usize;
        
        // Allocate stack space for key and value
        let key_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (key_slots.max(1) * 8) as u32,
            3,
        ));
        let val_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (val_slots.max(1) * 8) as u32,
            3,
        ));
        
        let key_ptr = self.builder.ins().stack_addr(types::I64, key_slot, 0);
        let val_ptr = self.builder.ins().stack_addr(types::I64, val_slot, 0);
        let key_slots_val = self.builder.ins().iconst(types::I32, key_slots as i64);
        let val_slots_val = self.builder.ins().iconst(types::I32, val_slots as i64);
        
        let call = self.builder.ins().call(map_iter_get_func, &[m, idx, key_ptr, key_slots_val, val_ptr, val_slots_val]);
        let done = self.builder.inst_results(call)[0];
        
        // Copy key and value back from stack
        for i in 0..key_slots {
            let val = self.builder.ins().stack_load(types::I64, key_slot, (i * 8) as i32);
            self.write_var(inst.a + i as u16, val);
        }
        for i in 0..val_slots {
            let val = self.builder.ins().stack_load(types::I64, val_slot, (i * 8) as i32);
            self.write_var(inst.a + key_slots as u16 + i as u16, val);
        }
        self.write_var(inst.a + (key_slots + val_slots) as u16, done);
    }

    // =========================================================================
    // Channel operations (only ChanNew is JIT-able)
    // =========================================================================

    pub(crate) fn translate_chan_new(&mut self, inst: &Instruction) {
        // ChanNew: a = dst, b = elem_meta_slot, c = cap_slot, flags = elem_slots
        let chan_new_func = match self.misc_funcs.chan_new {
            Some(f) => f,
            None => return,
        };
        
        let gc_ptr = self.load_gc_ptr();
        let elem_meta = self.read_var(inst.b);
        let elem_meta_i32 = self.builder.ins().ireduce(types::I32, elem_meta);
        let elem_slots = self.builder.ins().iconst(types::I32, inst.flags as i64);
        let cap = self.read_var(inst.c);
        
        let call = self.builder.ins().call(chan_new_func, &[gc_ptr, elem_meta_i32, elem_slots, cap]);
        let chan_ref = self.builder.inst_results(call)[0];
        
        self.write_var(inst.a, chan_ref);
    }

    // =========================================================================
    // Closure operations
    // =========================================================================

    pub(crate) fn translate_closure_new(&mut self, inst: &Instruction) {
        // ClosureNew: a = dst, b = func_id_low, c = capture_count, flags = func_id_high
        let closure_new_func = match self.misc_funcs.closure_new {
            Some(f) => f,
            None => return,
        };
        
        let gc_ptr = self.load_gc_ptr();
        let func_id = ((inst.flags as u32) << 16) | (inst.b as u32);
        let capture_count = inst.c as u32;
        
        let func_id_val = self.builder.ins().iconst(types::I32, func_id as i64);
        let capture_count_val = self.builder.ins().iconst(types::I32, capture_count as i64);
        
        let call = self.builder.ins().call(closure_new_func, &[gc_ptr, func_id_val, capture_count_val]);
        let closure_ref = self.builder.inst_results(call)[0];
        
        self.write_var(inst.a, closure_ref);
    }

    pub(crate) fn translate_closure_get(&mut self, inst: &Instruction) {
        // ClosureGet: a = dst, b = capture_idx
        // Closure is always in slot 0 of current function
        use vo_runtime::objects::closure::HEADER_SLOTS;
        
        let closure = self.read_var(0);
        let capture_idx = inst.b as usize;
        
        // Offset = (HEADER_SLOTS + capture_idx) * 8 bytes
        let offset = ((HEADER_SLOTS + capture_idx) * 8) as i32;
        let val = self.builder.ins().load(
            types::I64,
            cranelift_codegen::ir::MemFlags::trusted(),
            closure,
            offset,
        );
        self.write_var(inst.a, val);
    }

    // =========================================================================
    // Interface operations
    // =========================================================================

    pub(crate) fn translate_iface_assign(&mut self, inst: &Instruction) {
        // IfaceAssign: a=dst (2 slots), b=src, c=const_idx, flags=value_kind
        // For concrete type -> interface (simple case):
        // slot0 = pack(itab_id, rttid, vk), slot1 = src or clone(src)
        //
        // Complex cases (interface->interface) need runtime itab lookup
        // TODO: Add vo_iface_assign runtime helper for complex cases
        
        use vo_runtime::bytecode::Constant;
        
        let vk = inst.flags;
        let src = self.read_var(inst.b);
        
        // Read packed constant: (rttid << 32) | itab_id
        let const_idx = inst.c as usize;
        let (rttid, itab_id) = if let Constant::Int(packed) = &self.vo_module.constants[const_idx] {
            let rttid = (*packed >> 32) as u32;
            let itab_id = (*packed & 0xFFFFFFFF) as u32;
            (rttid, itab_id)
        } else {
            (0, 0)
        };
        
        // Pack slot0: (itab_id << 32) | (rttid << 8) | vk
        let itab_shifted = (itab_id as u64) << 32;
        let rttid_shifted = (rttid as u64) << 8;
        let slot0_val = itab_shifted | rttid_shifted | (vk as u64);
        let slot0 = self.builder.ins().iconst(types::I64, slot0_val as i64);
        
        // slot1 depends on value kind
        // ValueKind::Struct = 7, Array = 8 need ptr_clone (deep copy for value semantics)
        // ValueKind::Interface = 11 is complex (skip for now)
        let slot1 = if vk == 7 || vk == 8 {
            // Struct/Array: need deep copy via vo_ptr_clone
            if let Some(ptr_clone_func) = self.misc_funcs.ptr_clone {
                let gc_ptr = self.load_gc_ptr();
                let call = self.builder.ins().call(ptr_clone_func, &[gc_ptr, src]);
                self.builder.inst_results(call)[0]
            } else {
                // Fallback: just copy pointer (incorrect but allows compilation)
                src
            }
        } else if vk == 11 {
            // Interface -> Interface: need runtime support
            // For now, just copy slot1 from source
            self.read_var(inst.b + 1)
        } else {
            // Primitive types: direct copy
            src
        };
        
        self.write_var(inst.a, slot0);
        self.write_var(inst.a + 1, slot1);
    }

    pub(crate) fn translate_iface_assert(&mut self, inst: &Instruction) {
        // IfaceAssert: a=dst, b=src (2 slots), c=target_id, flags=assert_kind|(has_ok<<2)|(target_slots<<3)
        let iface_assert_func = match self.misc_funcs.iface_assert {
            Some(f) => f,
            None => return,
        };
        
        let ctx = self.get_ctx_param();
        let slot0 = self.read_var(inst.b);
        let slot1 = self.read_var(inst.b + 1);
        let target_id = self.builder.ins().iconst(types::I32, inst.c as i64);
        let flags = self.builder.ins().iconst(types::I16, inst.flags as i64);
        
        let has_ok = ((inst.flags >> 2) & 0x1) != 0;
        let assert_kind = inst.flags & 0x3;
        let target_slots = (inst.flags >> 3) as usize;
        
        // Allocate stack space for result
        let result_slots = if assert_kind == 1 { 3 } else { (target_slots.max(1) + 1) };
        let result_slot = self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (result_slots * 8) as u32,
            3,
        ));
        let dst_ptr = self.builder.ins().stack_addr(types::I64, result_slot, 0);
        
        let call = self.builder.ins().call(iface_assert_func, &[ctx, slot0, slot1, target_id, flags, dst_ptr]);
        let result = self.builder.inst_results(call)[0];
        
        // Check for panic (result == 0 means panic when !has_ok)
        if !has_ok {
            let zero = self.builder.ins().iconst(types::I64, 0);
            let is_panic = self.builder.ins().icmp(IntCC::Equal, result, zero);
            
            let panic_block = self.builder.create_block();
            let continue_block = self.builder.create_block();
            
            self.builder.ins().brif(is_panic, panic_block, &[], continue_block, &[]);
            
            self.builder.switch_to_block(panic_block);
            self.builder.seal_block(panic_block);
            let panic_result = self.builder.ins().iconst(types::I32, 1);
            self.builder.ins().return_(&[panic_result]);
            
            self.builder.switch_to_block(continue_block);
            self.builder.seal_block(continue_block);
        }
        
        // Copy results from stack to destination slots
        let dst_slots = if assert_kind == 1 { 2 } else { target_slots.max(1) };
        for i in 0..dst_slots {
            let val = self.builder.ins().stack_load(types::I64, result_slot, (i * 8) as i32);
            self.write_var(inst.a + i as u16, val);
        }
        
        // Copy ok flag if present
        if has_ok {
            let ok_offset = if assert_kind == 1 { 2 } else { target_slots.max(1) };
            let ok_val = self.builder.ins().stack_load(types::I64, result_slot, (ok_offset * 8) as i32);
            self.write_var(inst.a + ok_offset as u16, ok_val);
        }
    }

    // =========================================================================
    // Type conversion
    // =========================================================================

    pub(crate) fn translate_conv_i2f(&mut self, inst: &Instruction) {
        let a = self.read_var(inst.b);
        let f = self.builder.ins().fcvt_from_sint(types::F64, a);
        let result = self.builder.ins().bitcast(types::I64, cranelift_codegen::ir::MemFlags::new(), f);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_conv_f2i(&mut self, inst: &Instruction) {
        let a = self.read_var(inst.b);
        let f = self.builder.ins().bitcast(types::F64, cranelift_codegen::ir::MemFlags::new(), a);
        let result = self.builder.ins().fcvt_to_sint(types::I64, f);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_conv_i32_i64(&mut self, inst: &Instruction) {
        let a = self.read_var(inst.b);
        // Sign extend 32-bit to 64-bit
        let truncated = self.builder.ins().ireduce(types::I32, a);
        let result = self.builder.ins().sextend(types::I64, truncated);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_conv_i64_i32(&mut self, inst: &Instruction) {
        let a = self.read_var(inst.b);
        // Truncate 64-bit to 32-bit, then zero-extend back
        let truncated = self.builder.ins().ireduce(types::I32, a);
        let result = self.builder.ins().uextend(types::I64, truncated);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_conv_f64_f32(&mut self, inst: &Instruction) {
        use cranelift_codegen::ir::MemFlags;
        let a = self.read_var(inst.b);
        // a is f64 bits stored as i64, convert to f32 bits stored as i64
        let f64_val = self.builder.ins().bitcast(types::F64, MemFlags::new(), a);
        let f32_val = self.builder.ins().fdemote(types::F32, f64_val);
        let f32_bits = self.builder.ins().bitcast(types::I32, MemFlags::new(), f32_val);
        let result = self.builder.ins().uextend(types::I64, f32_bits);
        self.write_var(inst.a, result);
    }

    pub(crate) fn translate_conv_f32_f64(&mut self, inst: &Instruction) {
        use cranelift_codegen::ir::MemFlags;
        let a = self.read_var(inst.b);
        // a is f32 bits stored in low 32 bits of i64, convert to f64 bits
        let f32_bits = self.builder.ins().ireduce(types::I32, a);
        let f32_val = self.builder.ins().bitcast(types::F32, MemFlags::new(), f32_bits);
        let f64_val = self.builder.ins().fpromote(types::F64, f32_val);
        let result = self.builder.ins().bitcast(types::I64, MemFlags::new(), f64_val);
        self.write_var(inst.a, result);
    }

    // =========================================================================
    // Panic
    // =========================================================================

    pub(crate) fn translate_panic(&mut self, inst: &Instruction) {
        let panic_func = match self.panic_func {
            Some(f) => f,
            None => {
                let panic = self.builder.ins().iconst(types::I32, 1);
                self.builder.ins().return_(&[panic]);
                return;
            }
        };
        
        let ctx = self.get_ctx_param();
        let msg = self.read_var(inst.b);
        
        self.builder.ins().call(panic_func, &[ctx, msg]);
        
        let panic = self.builder.ins().iconst(types::I32, 1);
        self.builder.ins().return_(&[panic]);
    }
}
