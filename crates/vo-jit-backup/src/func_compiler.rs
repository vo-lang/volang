//! Function compiler: bytecode -> Cranelift IR.
//!
//! This module handles the compilation of a single Vo function to native code.

use std::collections::HashMap;

use cranelift_codegen::ir::{types, AbiParam, Block, Function, InstBuilder, Value};
use cranelift_codegen::ir::condcodes::{IntCC, FloatCC};
use cranelift_codegen::isa::CallConv;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::Module;

use vo_runtime::bytecode::{FunctionDef, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::SlotType;

use cranelift_codegen::ir::FuncRef;

use crate::gc_tracking::{GcRefTracker, StackMap};
use crate::translator::BytecodeTranslator;
use crate::JitError;

// =============================================================================
// FunctionCompiler
// =============================================================================

/// Compiles a single Vo function to Cranelift IR.
///
/// # Lifetime
/// - `'a`: Lifetime of the FunctionBuilderContext
/// - References to FunctionDef and VoModule must outlive the compilation
pub struct FunctionCompiler<'a> {
    /// Cranelift function builder.
    pub(crate) builder: FunctionBuilder<'a>,
    /// The Vo function being compiled.
    pub(crate) func_def: &'a FunctionDef,
    /// The Vo module (for constants, other functions, etc.).
    pub(crate) vo_module: &'a VoModule,
    /// GcRef tracker for stack map generation.
    pub(crate) gc_tracker: GcRefTracker,
    /// Cranelift Variable for each bytecode slot.
    /// Index = slot index in bytecode.
    pub(crate) vars: Vec<Variable>,
    /// Block for each bytecode PC that is a jump target.
    /// Lazily created when needed.
    pub(crate) blocks: HashMap<usize, Block>,
    /// The entry block.
    pub(crate) entry_block: Block,
    /// Current bytecode PC being translated.
    pub(crate) current_pc: usize,
    /// FuncRef for vo_gc_safepoint runtime helper.
    pub(crate) safepoint_func: Option<FuncRef>,
    /// FuncRef for vo_call_vm runtime helper.
    pub(crate) call_vm_func: Option<FuncRef>,
    /// FuncRef for vo_gc_alloc runtime helper.
    pub(crate) gc_alloc_func: Option<FuncRef>,
    /// FuncRef for vo_call_closure runtime helper.
    pub(crate) call_closure_func: Option<FuncRef>,
    /// FuncRef for vo_call_iface runtime helper.
    pub(crate) call_iface_func: Option<FuncRef>,
    /// FuncRef for vo_panic runtime helper.
    pub(crate) panic_func: Option<FuncRef>,
    /// FuncRef for vo_call_extern runtime helper.
    pub(crate) call_extern_func: Option<FuncRef>,
    /// String helper FuncRefs.
    pub(crate) str_funcs: StringFuncs,
    /// Map helper FuncRefs.
    pub(crate) map_funcs: MapFuncs,
    /// Array helper FuncRefs.
    pub(crate) array_funcs: ArrayFuncs,
    /// Slice helper FuncRefs.
    pub(crate) slice_funcs: SliceFuncs,
    /// Misc helper FuncRefs.
    pub(crate) misc_funcs: MiscFuncs,
    /// Map of register -> constant value for LoadConst instructions.
    /// Used by dynamic elem_bytes lookups.
    pub(crate) reg_consts: HashMap<u16, i64>,
}

/// FuncRefs for string operations.
#[derive(Default, Clone, Copy)]
pub struct StringFuncs {
    pub str_new: Option<FuncRef>,
    pub str_len: Option<FuncRef>,
    pub str_index: Option<FuncRef>,
    pub str_concat: Option<FuncRef>,
    pub str_slice: Option<FuncRef>,
    pub str_eq: Option<FuncRef>,
    pub str_cmp: Option<FuncRef>,
    pub str_decode_rune: Option<FuncRef>,
}

/// FuncRefs for map operations.
#[derive(Default, Clone, Copy)]
pub struct MapFuncs {
    pub map_new: Option<FuncRef>,
    pub map_len: Option<FuncRef>,
    pub map_get: Option<FuncRef>,
    pub map_set: Option<FuncRef>,
    pub map_delete: Option<FuncRef>,
    pub map_iter_get: Option<FuncRef>,
}

/// FuncRefs for array operations.
#[derive(Default, Clone, Copy)]
pub struct ArrayFuncs {
    pub array_new: Option<FuncRef>,
    pub array_len: Option<FuncRef>,
}

/// FuncRefs for slice operations.
#[derive(Default, Clone, Copy)]
pub struct SliceFuncs {
    pub slice_new: Option<FuncRef>,
    pub slice_len: Option<FuncRef>,
    pub slice_cap: Option<FuncRef>,
    pub slice_append: Option<FuncRef>,
    pub slice_slice: Option<FuncRef>,
    pub slice_slice3: Option<FuncRef>,
    pub slice_from_array: Option<FuncRef>,
    pub slice_from_array3: Option<FuncRef>,
}

/// FuncRefs for closure, channel, and interface.
#[derive(Default, Clone, Copy)]
pub struct MiscFuncs {
    pub ptr_clone: Option<FuncRef>,
    pub closure_new: Option<FuncRef>,
    pub chan_new: Option<FuncRef>,
    pub iface_assert: Option<FuncRef>,
}

impl<'a> FunctionCompiler<'a> {
    /// Create a new function compiler.
    ///
    /// # Arguments
    /// - `func`: The Cranelift function to build into
    /// - `func_ctx`: Reusable FunctionBuilderContext
    /// - `func_def`: The Vo function definition
    /// - `vo_module`: The Vo module
    pub fn new(
        func: &'a mut Function,
        func_ctx: &'a mut FunctionBuilderContext,
        func_def: &'a FunctionDef,
        vo_module: &'a VoModule,
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
        
        // Create entry block
        let entry_block = builder.create_block();
        
        // Add function parameters (ctx, args, ret)
        // All are pointers (i64 on 64-bit platforms)
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
    
    /// Get constant value for a register (set by LoadConst).
    /// Used for dynamic elem_bytes lookups.
    pub fn get_const_from_reg(&self, reg: u16) -> i64 {
        *self.reg_consts.get(&reg).expect("JIT: register not set by LoadConst")
    }

    /// Compile the function and return the stack map.
    ///
    /// After calling this, the Cranelift function is ready for code generation.
    pub fn compile(mut self) -> Result<StackMap, JitError> {
        // 1. Declare variables for all local slots
        self.declare_variables();
        
        // 2. Scan bytecode to find jump targets and create blocks
        self.scan_jump_targets();
        
        // 3. Switch to entry block and emit prologue
        self.builder.switch_to_block(self.entry_block);
        self.emit_prologue();
        
        // Track whether current block is terminated (by Return/Jump)
        let mut block_terminated = false;
        
        // 4. Translate each instruction
        for pc in 0..self.func_def.code.len() {
            self.current_pc = pc;
            
            // If this PC is a jump target, switch to its block
            if let Some(&block) = self.blocks.get(&pc) {
                // Only emit fall-through jump if current block is not terminated
                if !block_terminated {
                    self.builder.ins().jump(block, &[]);
                }
                self.builder.switch_to_block(block);
                block_terminated = false;
            } else if block_terminated {
                // Current block is terminated but this PC is not a jump target.
                // This is dead code - create a dummy block to hold it.
                let dummy = self.builder.create_block();
                self.builder.switch_to_block(dummy);
                block_terminated = false;
            }
            
            let inst = &self.func_def.code[pc];
            self.translate_inst(inst)?;
            
            // Check if this instruction terminates the block
            match inst.opcode() {
                Opcode::Return | Opcode::Jump | Opcode::Panic => {
                    block_terminated = true;
                }
                Opcode::JumpIf | Opcode::JumpIfNot => {
                    // Conditional jumps create a new fall-through block
                    // The translate_jump_if/jump_if_not methods handle this
                    block_terminated = false;
                }
                _ => {}
            }
        }
        
        // 5. Seal all blocks before finalize
        self.builder.seal_all_blocks();
        
        // 6. Finalize
        self.builder.finalize();
        
        Ok(self.gc_tracker.build_stack_map())
    }

    // TODO: Loop OSR compilation will be implemented here
    // pub fn compile_loop_osr(...) -> Result<StackMap, JitError> { ... }

    /// Declare Cranelift variables for all local slots.
    fn declare_variables(&mut self) {
        let num_slots = self.func_def.local_slots as usize;
        self.vars.reserve(num_slots);
        
        for i in 0..num_slots {
            let var = Variable::from_u32(i as u32);
            // All slots are i64 (Vo uses 64-bit slots)
            self.builder.declare_var(var, types::I64);
            self.vars.push(var);
            
            // Track GcRef slots for stack map
            if i < self.func_def.slot_types.len() {
                let slot_type = self.func_def.slot_types[i];
                if slot_type == SlotType::GcRef {
                    self.gc_tracker.mark_gc_ref(i as u16);
                }
                // TODO: Handle Interface0/Interface1 slots
            }
        }
    }

    /// Scan bytecode to find all jump targets and create blocks for them.
    fn scan_jump_targets(&mut self) {
        for (pc, inst) in self.func_def.code.iter().enumerate() {
            match inst.opcode() {
                Opcode::Jump => {
                    // Jump offset is relative to current PC
                    let offset = inst.imm32();
                    let target = (pc as i32 + offset) as usize;
                    self.ensure_block(target);
                }
                Opcode::JumpIf | Opcode::JumpIfNot => {
                    // Jump offset is relative to current PC
                    let offset = inst.imm32();
                    let target = (pc as i32 + offset) as usize;
                    self.ensure_block(target);
                    // Also need block for fall-through
                    // (handled implicitly by sequential translation)
                }
                _ => {}
            }
        }
    }

    /// Ensure a block exists for the given PC.
    fn ensure_block(&mut self, pc: usize) {
        if !self.blocks.contains_key(&pc) {
            let block = self.builder.create_block();
            self.blocks.insert(pc, block);
        }
    }

    /// Emit function prologue: load parameters into local variables.
    fn emit_prologue(&mut self) {
        // Get function parameters: ctx, args, ret
        let params = self.builder.block_params(self.entry_block);
        let _ctx = params[0];   // JitContext*
        let args = params[1];   // args: *mut u64
        let _ret = params[2];   // ret: *mut u64
        
        // Load arguments into local variables (slots 0..param_slots)
        let param_slots = self.func_def.param_slots as usize;
        for i in 0..param_slots {
            // args[i] -> slot[i]
            let offset = (i * 8) as i32;
            let val = self.builder.ins().load(
                types::I64,
                cranelift_codegen::ir::MemFlags::trusted(),
                args,
                offset,
            );
            self.builder.def_var(self.vars[i], val);
        }
        
        // Initialize remaining slots to 0
        let zero = self.builder.ins().iconst(types::I64, 0);
        for i in param_slots..self.vars.len() {
            self.builder.def_var(self.vars[i], zero);
        }
    }

    /// Translate a single bytecode instruction.
    fn translate_inst(&mut self, inst: &Instruction) -> Result<(), JitError> {
        use crate::translate::{translate_inst, TranslateResult};
        
        // Try shared translation first
        match translate_inst(self, inst)? {
            TranslateResult::Completed => return Ok(()),
            TranslateResult::Terminated => return Ok(()),
            TranslateResult::Unhandled => {}
        }
        
        // Handle FunctionCompiler-specific instructions (control flow, calls, etc.)
        match inst.opcode() {
            // Control flow - needs self.blocks
            Opcode::Jump => self.translate_jump(inst),
            Opcode::JumpIf => self.translate_jump_if(inst),
            Opcode::JumpIfNot => self.translate_jump_if_not(inst),
            Opcode::Return => self.translate_return(inst),
            
            // Function calls - FunctionCompiler has JIT-to-JIT optimization
            Opcode::Call => self.translate_call(inst),
            Opcode::CallExtern => self.translate_call_extern(inst),
            Opcode::CallClosure => self.translate_call_closure(inst),
            Opcode::CallIface => self.translate_call_iface(inst),
            
            // String new - needs constant table content access
            Opcode::StrNew => self.translate_str_new(inst),
            
            // Interface assign - needs constant table content access
            Opcode::IfaceAssign => self.translate_iface_assign(inst),
            
            // These should have been filtered out by can_jit()
            Opcode::DeferPush
            | Opcode::ErrDeferPush
            | Opcode::Recover
            | Opcode::GoStart
            | Opcode::ChanSend
            | Opcode::ChanRecv
            | Opcode::ChanClose
            | Opcode::SelectBegin
            | Opcode::SelectSend
            | Opcode::SelectRecv
            | Opcode::SelectExec => {
                return Err(JitError::Internal(format!(
                    "opcode {:?} should not appear in JIT-compiled function",
                    inst.opcode()
                )));
            }
            
            Opcode::Invalid => {
                return Err(JitError::Internal("invalid opcode".to_string()));
            }
            
            _ => unreachable!("opcode {:?} should have been handled by translate_inst", inst.opcode())
        }
        
        Ok(())
    }

    // =========================================================================
    // Helper methods
    // =========================================================================

    /// Read a variable (slot) value.
    #[inline]
    pub(crate) fn read_var(&mut self, slot: u16) -> Value {
        self.builder.use_var(self.vars[slot as usize])
    }

    /// Write a value to a variable (slot).
    #[inline]
    pub(crate) fn write_var(&mut self, slot: u16, val: Value) {
        self.builder.def_var(self.vars[slot as usize], val)
    }

    /// Get block params (ctx, args, ret).
    pub(crate) fn get_ctx_param(&mut self) -> Value {
        self.builder.block_params(self.entry_block)[0]
    }

    pub(crate) fn get_args_param(&mut self) -> Value {
        self.builder.block_params(self.entry_block)[1]
    }

    pub(crate) fn get_ret_param(&mut self) -> Value {
        self.builder.block_params(self.entry_block)[2]
    }

    /// Load globals pointer from JitContext.
    /// JitContext layout: gc(0), globals(8), safepoint_flag(16), ...
    pub(crate) fn load_globals_ptr(&mut self) -> Value {
        let ctx = self.get_ctx_param();
        self.builder.ins().load(
            types::I64,
            cranelift_codegen::ir::MemFlags::trusted(),
            ctx,
            8, // offset to globals
        )
    }

    /// Load GC pointer from JitContext.
    /// JitContext layout: gc(0), globals(8), safepoint_flag(16), ...
    pub(crate) fn load_gc_ptr(&mut self) -> Value {
        let ctx = self.get_ctx_param();
        self.builder.ins().load(
            types::I64,
            cranelift_codegen::ir::MemFlags::trusted(),
            ctx,
            0, // offset to gc
        )
    }

    /// Emit a safepoint check (for GC).
    ///
    /// Called at loop back-edges and before function calls.
    /// Checks JitContext.safepoint_flag and calls vo_gc_safepoint if set.
    pub(crate) fn emit_safepoint(&mut self) {
        let safepoint_func = match self.safepoint_func {
            Some(f) => f,
            None => return, // No safepoint function registered
        };
        
        // JitContext layout (offsets in bytes):
        // 0: gc (*mut Gc)
        // 8: globals (*mut u64)
        // 16: safepoint_flag (*const bool)
        // 24: panic_flag (*mut bool)
        // 32: vm (*mut c_void)
        // 40: fiber (*mut c_void)
        
        let ctx = self.get_ctx_param();
        
        // Load safepoint_flag pointer from ctx+16
        let flag_ptr = self.builder.ins().load(
            types::I64,
            cranelift_codegen::ir::MemFlags::trusted(),
            ctx,
            16, // offset to safepoint_flag
        );
        
        // Load the actual flag value (bool = i8)
        let flag = self.builder.ins().load(
            types::I8,
            cranelift_codegen::ir::MemFlags::trusted(),
            flag_ptr,
            0,
        );
        
        // Create blocks for the branch
        let call_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        
        // Branch: if flag != 0, call safepoint
        self.builder.ins().brif(flag, call_block, &[], merge_block, &[]);
        
        // Call block: call vo_gc_safepoint(ctx)
        self.builder.switch_to_block(call_block);
        self.builder.seal_block(call_block);
        self.builder.ins().call(safepoint_func, &[ctx]);
        self.builder.ins().jump(merge_block, &[]);
        
        // Continue in merge block
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
    }
}

// =============================================================================
// BytecodeTranslator implementation
// =============================================================================

impl<'a> BytecodeTranslator for FunctionCompiler<'a> {
    fn read_var(&mut self, slot: u16) -> Value {
        self.builder.use_var(self.vars[slot as usize])
    }
    
    fn write_var(&mut self, slot: u16, val: Value) {
        self.builder.def_var(self.vars[slot as usize], val)
    }
    
    fn get_ctx_param(&mut self) -> Value {
        self.builder.block_params(self.entry_block)[0]
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
        let ctx = self.get_ctx_param();
        self.builder.ins().load(types::I64, cranelift_codegen::ir::MemFlags::trusted(), ctx, 8)
    }
    
    fn load_gc_ptr(&mut self) -> Value {
        let ctx = self.get_ctx_param();
        self.builder.ins().load(types::I64, cranelift_codegen::ir::MemFlags::trusted(), ctx, 0)
    }
    
    fn emit_safepoint(&mut self) {
        FunctionCompiler::emit_safepoint(self)
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
        1 // JitResult::Panic
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
