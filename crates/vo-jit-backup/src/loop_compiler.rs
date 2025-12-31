//! Loop function compiler for OSR.
//!
//! Compiles a loop body into an independent JIT function that can be called
//! from VM during OSR. This avoids the SSA complexity of mid-function entry.
//!
//! ## Return Value Convention
//!
//! The loop function returns `u32` which is interpreted as:
//! - **Normal exit / break**: Returns `exit_pc` (the PC after the loop)
//! - **Return inside loop**: Returns the PC of the `Return` instruction
//! - **Panic**: Returns `LOOP_RESULT_PANIC` (u32::MAX)
//!
//! The VM resumes execution at the returned PC. For `Return` instructions,
//! the VM handles the full return sequence.

use std::collections::HashMap;

use cranelift_codegen::ir::{types, Block, InstBuilder, Value};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::Module;
use cranelift_codegen::ir::FuncRef;

use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::SlotType;

use cranelift_codegen::ir::condcodes::{IntCC, FloatCC};

use crate::func_compiler::{StringFuncs, MapFuncs, ArrayFuncs, SliceFuncs, MiscFuncs};
use crate::gc_tracking::{GcRefTracker, StackMap};
use crate::loop_analysis::LoopInfo;
use crate::translator::BytecodeTranslator;
use crate::JitError;

/// Special return value indicating panic occurred in loop.
pub const LOOP_RESULT_PANIC: u32 = u32::MAX;

/// Loop function signature:
/// ```ignore
/// extern "C" fn(ctx: *mut JitContext, locals: *mut u64) -> u32 (exit_pc or LOOP_RESULT_PANIC)
/// ```
pub type LoopFunc = extern "C" fn(*mut crate::JitContext, *mut u64) -> u32;

/// Compiled loop information.
pub struct CompiledLoop {
    pub code_ptr: *const u8,
    pub loop_info: LoopInfo,
    pub stack_map: StackMap,
}

unsafe impl Send for CompiledLoop {}
unsafe impl Sync for CompiledLoop {}

/// Compiles a loop body to Cranelift IR for OSR.
pub struct LoopCompiler<'a> {
    builder: FunctionBuilder<'a>,
    func_def: &'a FunctionDef,
    vo_module: &'a VoModule,
    loop_info: &'a LoopInfo,
    gc_tracker: GcRefTracker,
    vars: Vec<Variable>,
    blocks: HashMap<usize, Block>,
    entry_block: Block,
    exit_block: Block,
    current_pc: usize,
    locals_ptr: Value,
    ctx_ptr: Value,
    safepoint_func: Option<FuncRef>,
    call_vm_func: Option<FuncRef>,
    gc_alloc_func: Option<FuncRef>,
    call_closure_func: Option<FuncRef>,
    call_iface_func: Option<FuncRef>,
    panic_func: Option<FuncRef>,
    call_extern_func: Option<FuncRef>,
    str_funcs: StringFuncs,
    map_funcs: MapFuncs,
    array_funcs: ArrayFuncs,
    slice_funcs: SliceFuncs,
    misc_funcs: MiscFuncs,
    reg_consts: HashMap<u16, i64>,
}

impl<'a> LoopCompiler<'a> {
    pub fn new(
        func: &'a mut cranelift_codegen::ir::Function,
        func_ctx: &'a mut FunctionBuilderContext,
        func_def: &'a FunctionDef,
        vo_module: &'a VoModule,
        loop_info: &'a LoopInfo,
        safepoint_func: Option<FuncRef>,
        call_vm_func: Option<FuncRef>,
        gc_alloc_func: Option<FuncRef>,
        call_closure_func: Option<FuncRef>,
        call_iface_func: Option<FuncRef>,
        panic_func: Option<FuncRef>,
        call_extern_func: Option<FuncRef>,
        str_funcs: StringFuncs,
        map_funcs: MapFuncs,
        array_funcs: ArrayFuncs,
        slice_funcs: SliceFuncs,
        misc_funcs: MiscFuncs,
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
            gc_tracker: GcRefTracker::new(),
            vars: Vec::new(),
            blocks: HashMap::new(),
            entry_block,
            exit_block,
            current_pc: 0,
            locals_ptr: Value::from_u32(0), // Will be set in compile()
            ctx_ptr: Value::from_u32(0),    // Will be set in compile()
            safepoint_func,
            call_vm_func,
            gc_alloc_func,
            call_closure_func,
            call_iface_func,
            panic_func,
            call_extern_func,
            str_funcs,
            map_funcs,
            array_funcs,
            slice_funcs,
            misc_funcs,
            reg_consts: HashMap::new(),
        }
    }

    /// Compile the loop and return the stack map.
    pub fn compile(mut self) -> Result<StackMap, JitError> {
        // 1. Declare variables for all local slots
        self.declare_variables();
        
        // 2. Scan loop bytecode to find jump targets
        self.scan_jump_targets();
        
        // 3. Create loop header block
        let loop_header = self.ensure_block(self.loop_info.begin_pc + 1); // +1 to skip HINT_LOOP_BEGIN
        
        // 4. Switch to entry block and emit prologue
        self.builder.switch_to_block(self.entry_block);
        self.emit_prologue();
        
        // Jump to loop header
        self.builder.ins().jump(loop_header, &[]);
        
        // 5. Translate loop body (begin_pc+1 to end_pc-1, skipping HINT instructions)
        // Start with block_terminated = true since entry_block is now terminated
        let mut block_terminated = true;
        
        for pc in (self.loop_info.begin_pc + 1)..self.loop_info.end_pc {
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
            
            // Skip HINT instructions
            if inst.opcode() == Opcode::Hint {
                continue;
            }
            
            let terminated = self.translate_inst(inst)?;
            if terminated {
                block_terminated = true;
            }
        }
        
        // 6. Setup exit block - write back locals and return exit_pc
        self.builder.switch_to_block(self.exit_block);
        self.emit_epilogue();
        let exit_pc_val = self.builder.ins().iconst(types::I32, self.loop_info.exit_pc as i64);
        self.builder.ins().return_(&[exit_pc_val]);
        
        // 7. Seal all blocks
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
                let slot_type = self.func_def.slot_types[i];
                if slot_type == SlotType::GcRef {
                    self.gc_tracker.mark_gc_ref(i as u16);
                }
            }
        }
    }

    fn scan_jump_targets(&mut self) {
        let begin = self.loop_info.begin_pc + 1;
        let end = self.loop_info.end_pc;
        
        for pc in begin..end {
            let inst = &self.func_def.code[pc];
            match inst.opcode() {
                Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot => {
                    let offset = inst.imm32();
                    let target = (pc as i32 + offset) as usize;
                    
                    // Only create blocks for targets within loop or exit
                    if (target >= begin && target < end) || target == self.loop_info.exit_pc {
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
        
        // Load all local slots from locals pointer
        let num_slots = self.func_def.local_slots as usize;
        for i in 0..num_slots {
            let offset = (i * 8) as i32;
            let val = self.builder.ins().load(
                types::I64,
                cranelift_codegen::ir::MemFlags::trusted(),
                self.locals_ptr,
                offset,
            );
            self.builder.def_var(self.vars[i], val);
        }
    }

    fn emit_epilogue(&mut self) {
        // Write back all local slots to locals pointer
        let num_slots = self.func_def.local_slots as usize;
        for i in 0..num_slots {
            let val = self.builder.use_var(self.vars[i]);
            let offset = (i * 8) as i32;
            self.builder.ins().store(
                cranelift_codegen::ir::MemFlags::trusted(),
                val,
                self.locals_ptr,
                offset,
            );
        }
    }

    fn translate_inst(&mut self, inst: &Instruction) -> Result<bool, JitError> {
        use crate::translate::{translate_inst, TranslateResult};
        
        // Try shared translation first
        match translate_inst(self, inst)? {
            TranslateResult::Completed => return Ok(false),
            TranslateResult::Terminated => return Ok(true),
            TranslateResult::Unhandled => {}
        }
        
        // Handle loop-specific instructions
        let terminated = match inst.opcode() {
            Opcode::Jump => {
                let offset = inst.imm32();
                let target = (self.current_pc as i32 + offset) as usize;
                
                if target == self.loop_info.exit_pc || target >= self.loop_info.end_pc {
                    self.emit_epilogue();
                    let exit_val = self.builder.ins().iconst(types::I32, target as i64);
                    self.builder.ins().return_(&[exit_val]);
                } else if let Some(&block) = self.blocks.get(&target) {
                    self.builder.ins().jump(block, &[]);
                } else {
                    return Err(JitError::Internal(format!("jump to unknown target {} in loop", target)));
                }
                true
            }
            
            Opcode::JumpIf => {
                let cond_slot = inst.a;
                let offset = inst.imm32();
                let target = (self.current_pc as i32 + offset) as usize;
                let fall_through = self.current_pc + 1;
                
                let cond = self.builder.use_var(self.vars[cond_slot as usize]);
                let zero = self.builder.ins().iconst(types::I64, 0);
                let cmp = self.builder.ins().icmp(IntCC::NotEqual, cond, zero);
                
                let then_block = if target == self.loop_info.exit_pc || target >= self.loop_info.end_pc {
                    self.exit_block
                } else {
                    self.ensure_block(target)
                };
                let else_block = self.ensure_block(fall_through);
                
                self.builder.ins().brif(cmp, then_block, &[], else_block, &[]);
                true
            }
            
            Opcode::JumpIfNot => {
                let cond_slot = inst.a;
                let offset = inst.imm32();
                let target = (self.current_pc as i32 + offset) as usize;
                let fall_through = self.current_pc + 1;
                
                let cond = self.builder.use_var(self.vars[cond_slot as usize]);
                let zero = self.builder.ins().iconst(types::I64, 0);
                let cmp = self.builder.ins().icmp(IntCC::Equal, cond, zero);
                
                let then_block = if target == self.loop_info.exit_pc || target >= self.loop_info.end_pc {
                    self.exit_block
                } else {
                    self.ensure_block(target)
                };
                let else_block = self.ensure_block(fall_through);
                
                self.builder.ins().brif(cmp, then_block, &[], else_block, &[]);
                true
            }
            
            Opcode::Return => {
                self.emit_epilogue();
                let ret_pc = self.builder.ins().iconst(types::I32, self.current_pc as i64);
                self.builder.ins().return_(&[ret_pc]);
                true
            }
            
            Opcode::Panic => {
                let panic_val = self.builder.ins().iconst(types::I32, LOOP_RESULT_PANIC as i64);
                self.builder.ins().return_(&[panic_val]);
                true
            }
            
            Opcode::Call => {
                crate::translate::translate_call_vm(self, inst);
                false
            }
            
            Opcode::CallExtern => {
                crate::translate::translate_call_extern_vm(self, inst);
                false
            }
            
            Opcode::CallClosure => {
                crate::translate::translate_call_closure_vm(self, inst);
                false
            }
            
            Opcode::CallIface => {
                crate::translate::translate_call_iface_vm(self, inst);
                false
            }
            
            // Unsupported in loop - exit to VM
            _ => {
                self.emit_epilogue();
                let ret_pc = self.builder.ins().iconst(types::I32, self.current_pc as i64);
                self.builder.ins().return_(&[ret_pc]);
                true
            }
        };
        Ok(terminated)
    }

    // Accessors for translate module compatibility
    pub fn get_var(&self, slot: usize) -> Variable {
        self.vars[slot]
    }

    pub fn use_var(&mut self, slot: usize) -> Value {
        self.builder.use_var(self.vars[slot])
    }

    pub fn def_var(&mut self, slot: usize, val: Value) {
        self.builder.def_var(self.vars[slot], val)
    }
}

// =============================================================================
// BytecodeTranslator implementation
// =============================================================================

impl<'a> BytecodeTranslator for LoopCompiler<'a> {
    fn read_var(&mut self, slot: u16) -> Value {
        self.builder.use_var(self.vars[slot as usize])
    }
    
    fn write_var(&mut self, slot: u16, val: Value) {
        self.builder.def_var(self.vars[slot as usize], val)
    }
    
    fn get_ctx_param(&mut self) -> Value {
        self.ctx_ptr
    }
    
    fn vo_module(&self) -> &VoModule {
        self.vo_module
    }
    
    fn current_pc(&self) -> usize {
        self.current_pc
    }
    
    fn set_reg_const(&mut self, reg: u16, val: i64) {
        self.reg_consts.insert(reg, val);
    }
    
    fn get_const_from_reg(&self, reg: u16) -> i64 {
        *self.reg_consts.get(&reg).expect("JIT: register not set by LoadConst")
    }
    
    fn load_globals_ptr(&mut self) -> Value {
        let ctx = self.ctx_ptr;
        self.builder.ins().load(types::I64, cranelift_codegen::ir::MemFlags::trusted(), ctx, 8)
    }
    
    fn load_gc_ptr(&mut self) -> Value {
        let ctx = self.ctx_ptr;
        self.builder.ins().load(types::I64, cranelift_codegen::ir::MemFlags::trusted(), ctx, 0)
    }
    
    fn emit_safepoint(&mut self) {
        // TODO: Implement safepoint for loop compiler
    }
    
    // Cranelift IR operations
    fn ins_iconst(&mut self, val: i64) -> Value {
        self.builder.ins().iconst(types::I64, val)
    }
    
    fn ins_f64const(&mut self, bits: u64) -> Value {
        self.builder.ins().f64const(f64::from_bits(bits))
    }
    
    fn ins_iadd(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().iadd(a, b)
    }
    
    fn ins_isub(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().isub(a, b)
    }
    
    fn ins_imul(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().imul(a, b)
    }
    
    fn ins_sdiv(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().sdiv(a, b)
    }
    
    fn ins_srem(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().srem(a, b)
    }
    
    fn ins_ineg(&mut self, a: Value) -> Value {
        self.builder.ins().ineg(a)
    }
    
    fn ins_fadd(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().fadd(a, b)
    }
    
    fn ins_fsub(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().fsub(a, b)
    }
    
    fn ins_fmul(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().fmul(a, b)
    }
    
    fn ins_fdiv(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().fdiv(a, b)
    }
    
    fn ins_fneg(&mut self, a: Value) -> Value {
        self.builder.ins().fneg(a)
    }
    
    fn ins_bitcast_i64_to_f64(&mut self, val: Value) -> Value {
        self.builder.ins().bitcast(types::F64, cranelift_codegen::ir::MemFlags::new(), val)
    }
    
    fn ins_bitcast_f64_to_i64(&mut self, val: Value) -> Value {
        self.builder.ins().bitcast(types::I64, cranelift_codegen::ir::MemFlags::new(), val)
    }
    
    fn ins_icmp(&mut self, cc: IntCC, a: Value, b: Value) -> Value {
        self.builder.ins().icmp(cc, a, b)
    }
    
    fn ins_fcmp(&mut self, cc: FloatCC, a: Value, b: Value) -> Value {
        self.builder.ins().fcmp(cc, a, b)
    }
    
    fn ins_uextend_i8_to_i64(&mut self, val: Value) -> Value {
        self.builder.ins().uextend(types::I64, val)
    }
    
    fn ins_band(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().band(a, b)
    }
    
    fn ins_bor(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().bor(a, b)
    }
    
    fn ins_bxor(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().bxor(a, b)
    }
    
    fn ins_bnot(&mut self, a: Value) -> Value {
        self.builder.ins().bnot(a)
    }
    
    fn ins_ishl(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().ishl(a, b)
    }
    
    fn ins_sshr(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().sshr(a, b)
    }
    
    fn ins_ushr(&mut self, a: Value, b: Value) -> Value {
        self.builder.ins().ushr(a, b)
    }
    
    fn ins_ushr_imm(&mut self, a: Value, imm: i64) -> Value {
        self.builder.ins().ushr_imm(a, imm)
    }
    
    fn ins_load(&mut self, ty: types::Type, ptr: Value, offset: i32) -> Value {
        self.builder.ins().load(ty, cranelift_codegen::ir::MemFlags::trusted(), ptr, offset)
    }
    
    fn ins_store(&mut self, val: Value, ptr: Value, offset: i32) {
        self.builder.ins().store(cranelift_codegen::ir::MemFlags::trusted(), val, ptr, offset);
    }
    
    fn ins_call(&mut self, func: FuncRef, args: &[Value]) -> Option<Value> {
        let call = self.builder.ins().call(func, args);
        self.builder.inst_results(call).first().copied()
    }
    
    fn ins_select(&mut self, cond: Value, a: Value, b: Value) -> Value {
        self.builder.ins().select(cond, a, b)
    }
    
    fn ins_imul_imm(&mut self, a: Value, imm: i64) -> Value {
        self.builder.ins().imul_imm(a, imm)
    }
    
    fn ins_iadd_imm(&mut self, a: Value, imm: i64) -> Value {
        self.builder.ins().iadd_imm(a, imm)
    }
    
    fn ins_ireduce_i32(&mut self, val: Value) -> Value {
        self.builder.ins().ireduce(types::I32, val)
    }
    
    fn ins_ireduce_i16(&mut self, val: Value) -> Value {
        self.builder.ins().ireduce(types::I16, val)
    }
    
    fn ins_ireduce_i8(&mut self, val: Value) -> Value {
        self.builder.ins().ireduce(types::I8, val)
    }
    
    fn ins_sextend_i8_to_i64(&mut self, val: Value) -> Value {
        self.builder.ins().sextend(types::I64, val)
    }
    
    fn ins_sextend_i16_to_i64(&mut self, val: Value) -> Value {
        self.builder.ins().sextend(types::I64, val)
    }
    
    fn ins_sextend_i32_to_i64(&mut self, val: Value) -> Value {
        self.builder.ins().sextend(types::I64, val)
    }
    
    fn ins_uextend_i32_to_i64(&mut self, val: Value) -> Value {
        self.builder.ins().uextend(types::I64, val)
    }
    
    fn ins_uextend_i16_to_i64(&mut self, val: Value) -> Value {
        self.builder.ins().uextend(types::I64, val)
    }
    
    fn ins_load_i8(&mut self, ptr: Value, offset: i32) -> Value {
        self.builder.ins().load(types::I8, cranelift_codegen::ir::MemFlags::trusted(), ptr, offset)
    }
    
    fn ins_load_i16(&mut self, ptr: Value, offset: i32) -> Value {
        self.builder.ins().load(types::I16, cranelift_codegen::ir::MemFlags::trusted(), ptr, offset)
    }
    
    fn ins_load_i32(&mut self, ptr: Value, offset: i32) -> Value {
        self.builder.ins().load(types::I32, cranelift_codegen::ir::MemFlags::trusted(), ptr, offset)
    }
    
    fn ins_store_i8(&mut self, val: Value, ptr: Value, offset: i32) {
        self.builder.ins().store(cranelift_codegen::ir::MemFlags::trusted(), val, ptr, offset);
    }
    
    fn ins_store_i16(&mut self, val: Value, ptr: Value, offset: i32) {
        self.builder.ins().store(cranelift_codegen::ir::MemFlags::trusted(), val, ptr, offset);
    }
    
    fn ins_store_i32(&mut self, val: Value, ptr: Value, offset: i32) {
        self.builder.ins().store(cranelift_codegen::ir::MemFlags::trusted(), val, ptr, offset);
    }
    
    fn ins_fcvt_from_sint(&mut self, val: Value) -> Value {
        self.builder.ins().fcvt_from_sint(types::F64, val)
    }
    
    fn ins_fcvt_to_sint(&mut self, val: Value) -> Value {
        self.builder.ins().fcvt_to_sint(types::I64, val)
    }
    
    fn ins_fdemote(&mut self, val: Value) -> Value {
        self.builder.ins().fdemote(types::F32, val)
    }
    
    fn ins_fpromote(&mut self, val: Value) -> Value {
        self.builder.ins().fpromote(types::F64, val)
    }
    
    fn create_stack_slot(&mut self, size: u32) -> cranelift_codegen::ir::StackSlot {
        self.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            size,
            3,
        ))
    }
    
    fn ins_stack_store(&mut self, val: Value, slot: cranelift_codegen::ir::StackSlot, offset: i32) {
        self.builder.ins().stack_store(val, slot, offset);
    }
    
    fn ins_stack_load(&mut self, slot: cranelift_codegen::ir::StackSlot, offset: i32) -> Value {
        self.builder.ins().stack_load(types::I64, slot, offset)
    }
    
    fn ins_stack_addr(&mut self, slot: cranelift_codegen::ir::StackSlot) -> Value {
        self.builder.ins().stack_addr(types::I64, slot, 0)
    }
    
    fn create_block(&mut self) -> cranelift_codegen::ir::Block {
        self.builder.create_block()
    }
    
    fn switch_to_block(&mut self, block: cranelift_codegen::ir::Block) {
        self.builder.switch_to_block(block);
    }
    
    fn seal_block(&mut self, block: cranelift_codegen::ir::Block) {
        self.builder.seal_block(block);
    }
    
    fn ins_brif(&mut self, cond: Value, then_block: cranelift_codegen::ir::Block, else_block: cranelift_codegen::ir::Block) {
        self.builder.ins().brif(cond, then_block, &[], else_block, &[]);
    }
    
    fn ins_return(&mut self, val: Value) {
        self.builder.ins().return_(&[val]);
    }
    
    fn panic_return_value(&self) -> i32 {
        LOOP_RESULT_PANIC as i32
    }
    
    // Helper FuncRefs
    fn safepoint_func(&self) -> Option<FuncRef> { self.safepoint_func }
    fn call_vm_func(&self) -> Option<FuncRef> { self.call_vm_func }
    fn gc_alloc_func(&self) -> Option<FuncRef> { self.gc_alloc_func }
    fn call_closure_func(&self) -> Option<FuncRef> { self.call_closure_func }
    fn call_iface_func(&self) -> Option<FuncRef> { self.call_iface_func }
    fn panic_func(&self) -> Option<FuncRef> { self.panic_func }
    fn call_extern_func(&self) -> Option<FuncRef> { self.call_extern_func }
    fn str_funcs(&self) -> &StringFuncs { &self.str_funcs }
    fn map_funcs(&self) -> &MapFuncs { &self.map_funcs }
    fn array_funcs(&self) -> &ArrayFuncs { &self.array_funcs }
    fn slice_funcs(&self) -> &SliceFuncs { &self.slice_funcs }
    fn misc_funcs(&self) -> &MiscFuncs { &self.misc_funcs }
}
