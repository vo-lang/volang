//! Statement compilation.

use vo_analysis::objects::TypeKey;
use vo_common::symbol::Symbol;
use vo_syntax::ast::{Block, Expr, Stmt, StmtKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::expr::compile_expr_to;
use crate::func::{ExprSource, FuncBuilder, StorageKind};
use crate::type_info::{encode_i32, TypeInfoWrapper};

// =============================================================================
// StmtCompiler - Unified statement compilation context
// =============================================================================

/// Statement compiler - unified context for all statement compilation.
/// Centralizes type decisions for variable allocation and initialization.
pub struct StmtCompiler<'a, 'b> {
    pub ctx: &'a mut CodegenContext,
    pub func: &'a mut FuncBuilder,
    pub info: &'b TypeInfoWrapper<'b>,
}

impl<'a, 'b> StmtCompiler<'a, 'b> {
    pub fn new(
        ctx: &'a mut CodegenContext,
        func: &'a mut FuncBuilder,
        info: &'b TypeInfoWrapper<'b>,
    ) -> Self {
        Self { ctx, func, info }
    }

    /// Define a local variable with optional initialization.
    /// This is the single entry point for all variable definitions.
    /// All type/escape decisions are centralized here.
    pub fn define_local(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        escapes: bool,
        init: Option<&Expr>,
    ) -> Result<StorageKind, CodegenError> {
        let storage = self.alloc_storage(sym, type_key, escapes)?;
        
        if let Some(expr) = init {
            self.emit_init(storage, expr, type_key)?;
        } else {
            self.emit_zero_init(storage, type_key);
        }
        
        Ok(storage)
    }

    /// Allocate storage for a variable based on type and escape analysis.
    /// This is the single decision point for storage strategy.
    fn alloc_storage(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        escapes: bool,
    ) -> Result<StorageKind, CodegenError> {
        let slots = self.info.type_slot_count(type_key);
        let slot_types = self.info.type_slot_types(type_key);

        if self.info.is_reference_type(type_key) {
            // Reference types: 1 slot GcRef IS the value
            let slot = self.func.define_local_reference(sym);
            Ok(StorageKind::Reference { slot })
        } else if escapes {
            if self.info.is_array(type_key) {
                self.alloc_escaped_array(sym, type_key)
            } else {
                self.alloc_escaped_boxed(sym, type_key, slots, &slot_types)
            }
        } else {
            // Stack allocation
            let slot = self.func.define_local_stack(sym, slots, &slot_types);
            Ok(StorageKind::StackValue { slot, slots })
        }
    }

    /// Allocate escaped array: [GcHeader][ArrayHeader][elems]
    fn alloc_escaped_array(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
    ) -> Result<StorageKind, CodegenError> {
        let elem_slots = self.info.array_elem_slots(type_key);
        let gcref_slot = self.func.define_local_heap_array(sym, elem_slots);

        let arr_len = self.info.array_len(type_key);
        let elem_meta_idx = self.ctx.get_or_create_array_elem_meta(type_key, self.info);

        // emit ArrayNew: a=dst, b=elem_meta_idx, c=len, flags=elem_slots
        let meta_reg = self.func.alloc_temp(1);
        self.func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);

        let len_reg = self.func.alloc_temp(1);
        let (b, c) = encode_i32(arr_len as i32);
        self.func.emit_op(Opcode::LoadInt, len_reg, b, c);

        self.func.emit_with_flags(Opcode::ArrayNew, elem_slots as u8, gcref_slot, meta_reg, len_reg);

        Ok(StorageKind::HeapArray { gcref_slot, elem_slots })
    }

    /// Allocate escaped boxed value (struct/primitive/interface): [GcHeader][data]
    fn alloc_escaped_boxed(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        slots: u16,
        slot_types: &[vo_common_core::types::SlotType],
    ) -> Result<StorageKind, CodegenError> {
        let gcref_slot = self.func.define_local_heap_boxed(sym, slots);

        let meta_idx = self.ctx.get_or_create_value_meta(Some(type_key), slots, slot_types);
        let meta_reg = self.func.alloc_temp(1);
        self.func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        self.func.emit_with_flags(Opcode::PtrNew, slots as u8, gcref_slot, meta_reg, 0);

        Ok(StorageKind::HeapBoxed { gcref_slot, value_slots: slots })
    }

    /// Emit initialization for a variable.
    fn emit_init(
        &mut self,
        storage: StorageKind,
        expr: &Expr,
        target_type: TypeKey,
    ) -> Result<(), CodegenError> {
        match storage {
            StorageKind::HeapArray { gcref_slot, elem_slots } => {
                compile_escaped_array_init(gcref_slot, expr, target_type, elem_slots, self.ctx, self.func, self.info)
            }
            StorageKind::HeapBoxed { gcref_slot, value_slots } => {
                let tmp = self.func.alloc_temp(value_slots);
                self.compile_value(expr, tmp, target_type)?;
                self.func.emit_ptr_set(gcref_slot, 0, tmp, value_slots);
                Ok(())
            }
            StorageKind::StackValue { slot, slots: _ } => {
                self.compile_value(expr, slot, target_type)
            }
            StorageKind::Reference { slot } => {
                compile_expr_to(expr, slot, self.ctx, self.func, self.info)
            }
            StorageKind::Global { .. } => {
                unreachable!("define_local doesn't create Global storage")
            }
        }
    }

    /// Compile expression value with automatic interface conversion.
    /// This is the single point for handling concrete-to-interface conversion.
    pub fn compile_value(
        &mut self,
        expr: &Expr,
        dst: u16,
        target_type: TypeKey,
    ) -> Result<(), CodegenError> {
        compile_value_to(expr, dst, target_type, self.ctx, self.func, self.info)
    }

    /// Emit zero initialization for a variable.
    fn emit_zero_init(&mut self, storage: StorageKind, _type_key: TypeKey) {
        match storage {
            StorageKind::HeapArray { .. } | StorageKind::HeapBoxed { .. } => {
                // Heap allocations are already zero-initialized by PtrNew/ArrayNew
            }
            StorageKind::StackValue { slot, slots } => {
                for i in 0..slots {
                    self.func.emit_op(Opcode::LoadNil, slot + i, 0, 0);
                }
            }
            StorageKind::Reference { slot } => {
                self.func.emit_op(Opcode::LoadNil, slot, 0, 0);
            }
            StorageKind::Global { .. } => {
                unreachable!("define_local doesn't create Global storage")
            }
        }
    }

    /// Define a local variable and initialize from an already-compiled slot.
    /// Used for comma-ok cases where the value is already in a temp slot.
    pub fn define_local_from_slot(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        escapes: bool,
        src_slot: u16,
    ) -> Result<StorageKind, CodegenError> {
        let slots = self.info.type_slot_count(type_key);
        let slot_types = self.info.type_slot_types(type_key);

        if self.info.is_reference_type(type_key) {
            let slot = self.func.define_local_reference(sym);
            self.func.emit_copy(slot, src_slot, 1);
            Ok(StorageKind::Reference { slot })
        } else if escapes && !self.info.is_array(type_key) {
            // HeapBoxed: allocate and copy from src_slot
            let gcref_slot = self.func.define_local_heap_boxed(sym, slots);
            let meta_idx = self.ctx.get_or_create_value_meta(Some(type_key), slots, &slot_types);
            let meta_reg = self.func.alloc_temp(1);
            self.func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
            self.func.emit_with_flags(Opcode::PtrNew, slots as u8, gcref_slot, meta_reg, 0);
            self.func.emit_ptr_set(gcref_slot, 0, src_slot, slots);
            Ok(StorageKind::HeapBoxed { gcref_slot, value_slots: slots })
        } else {
            // Stack: just copy
            let slot = self.func.define_local_stack(sym, slots, &slot_types);
            self.func.emit_copy(slot, src_slot, slots);
            Ok(StorageKind::StackValue { slot, slots })
        }
    }

    /// Store a value from an already-compiled slot to an existing storage.
    /// Used for re-assignment in short var declarations.
    pub fn store_from_slot(&mut self, storage: StorageKind, src_slot: u16, slots: u16) {
        match storage {
            StorageKind::StackValue { slot, .. } => {
                self.func.emit_copy(slot, src_slot, slots);
            }
            StorageKind::HeapBoxed { gcref_slot, value_slots } => {
                self.func.emit_ptr_set(gcref_slot, 0, src_slot, value_slots);
            }
            StorageKind::Reference { slot } => {
                self.func.emit_copy(slot, src_slot, 1);
            }
            _ => {}
        }
    }
}

/// Compile expression to dst with automatic interface conversion.
/// Standalone version of StmtCompiler::compile_value for use outside StmtCompiler.
pub fn compile_value_to(
    expr: &Expr,
    dst: u16,
    target_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let expr_type = info.expr_type(expr.id);
    if info.is_interface(target_type) && !info.is_interface(expr_type) {
        compile_iface_assign(dst, expr, target_type, ctx, func, info)
    } else {
        compile_expr_to(expr, dst, ctx, func, info)
    }
}
// Helper functions
// =============================================================================

/// Index-based loop for for-range expansion (array, slice, string, map).
struct IndexLoop {
    idx_slot: u16,
    loop_start: usize,
    end_jump: usize,
}

impl IndexLoop {
    /// Begin: __idx := 0, loop: if __idx >= __len { goto end }
    fn begin(func: &mut FuncBuilder, len_slot: u16, label: Option<vo_common::Symbol>) -> Self {
        let idx_slot = func.alloc_temp(1);
        func.emit_op(Opcode::LoadInt, idx_slot, 0, 0);
        
        let loop_start = func.current_pc();
        func.enter_loop(loop_start, label);
        
        let cmp_slot = func.alloc_temp(1);
        func.emit_op(Opcode::GeI, cmp_slot, idx_slot, len_slot);
        let end_jump = func.emit_jump(Opcode::JumpIf, cmp_slot);
        
        Self { idx_slot, loop_start, end_jump }
    }
    
    /// Emit: i := __idx
    fn emit_key(&self, func: &mut FuncBuilder, key_slot: Option<u16>) {
        if let Some(k) = key_slot {
            func.emit_op(Opcode::Copy, k, self.idx_slot, 0);
        }
    }
    
    /// End: __idx++, goto loop, patch breaks/continues
    fn end(self, func: &mut FuncBuilder) {
        let post_pc = func.current_pc();
        let one = func.alloc_temp(1);
        func.emit_op(Opcode::LoadInt, one, 1, 0);
        func.emit_op(Opcode::AddI, self.idx_slot, self.idx_slot, one);
        func.emit_jump_to(Opcode::Jump, 0, self.loop_start);
        
        func.patch_jump(self.end_jump, func.current_pc());
        let (breaks, continues) = func.exit_loop();
        for pc in breaks { func.patch_jump(pc, func.current_pc()); }
        for pc in continues { func.patch_jump(pc, post_pc); }
    }
}

/// Define or lookup a range variable (key or value) using StmtCompiler.
/// - If `define` is true: declare new variable with proper escape handling
/// - If `define` is false: lookup existing variable
/// Gets type from identifier definition when available.
fn range_var_slot(
    sc: &mut StmtCompiler,
    var: Option<&Expr>,
    fallback_type: TypeKey,
    define: bool,
) -> Result<u16, CodegenError> {
    match var {
        Some(expr) => {
            if let vo_syntax::ast::ExprKind::Ident(ident) = &expr.kind {
                if define {
                    let obj_key = sc.info.get_def(ident);
                    let type_key = sc.info.obj_type(obj_key, "range var must have type");
                    let escapes = sc.info.is_escaped(obj_key);
                    let storage = sc.define_local(ident.symbol, type_key, escapes, None)?;
                    Ok(storage.slot())
                } else {
                    Ok(sc.func.lookup_local(ident.symbol)
                        .expect("range variable not found")
                        .storage.slot())
                }
            } else if define {
                let slots = sc.info.type_slot_count(fallback_type);
                Ok(sc.func.alloc_temp(slots))
            } else {
                crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)
            }
        }
        None => {
            let slots = sc.info.type_slot_count(fallback_type);
            Ok(sc.func.alloc_temp(slots))
        }
    }
}

/// Compile a statement.
pub fn compile_stmt(
    stmt: &Stmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    compile_stmt_with_label(stmt, ctx, func, info, None)
}

/// Compile a statement with optional label (for labeled loops).
fn compile_stmt_with_label(
    stmt: &Stmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
    label: Option<vo_common::Symbol>,
) -> Result<(), CodegenError> {
    match &stmt.kind {
        // === Variable declaration ===
        StmtKind::Var(var_decl) => {
            let mut sc = StmtCompiler::new(ctx, func, info);
            for spec in &var_decl.specs {
                for (i, name) in spec.names.iter().enumerate() {
                    let type_key = spec.ty.as_ref()
                        .map(|ty| info.type_expr_type(ty.id))
                        .or_else(|| spec.values.get(i).map(|v| info.expr_type(v.id)))
                        .expect("variable declaration must have type annotation or initializer");

                    let obj_key = info.get_def(name);
                    let escapes = info.is_escaped(obj_key);
                    let init = spec.values.get(i);

                    sc.define_local(name.symbol, type_key, escapes, init)?;
                }
            }
        }

        // === Short variable declaration ===
        StmtKind::ShortVar(short_var) => {
            // Check for multi-value case: v1, v2, ... := f() where f() returns a tuple
            // This includes comma-ok (2 values) and multi-return functions (3+ values)
            let is_multi_value = short_var.values.len() == 1 
                && short_var.names.len() >= 2
                && info.is_tuple(info.expr_type(short_var.values[0].id));

            if is_multi_value {
                // Multi-value: compile expr once, then distribute to variables
                let tuple_type = info.expr_type(short_var.values[0].id);
                let total_slots = info.type_slot_count(tuple_type);
                let tmp_base = func.alloc_temp(total_slots);
                compile_expr_to(&short_var.values[0], tmp_base, ctx, func, info)?;

                let mut sc = StmtCompiler::new(ctx, func, info);
                let mut offset = 0u16;
                for (i, name) in short_var.names.iter().enumerate() {
                    let elem_type = info.tuple_elem_type(tuple_type, i);
                    let elem_slots = info.type_slot_count(elem_type);

                    if info.project.interner.resolve(name.symbol) == Some("_") {
                        offset += elem_slots;
                        continue;
                    }

                    let is_def = info.project.type_info.defs.contains_key(&name.id);
                    if is_def {
                        let obj_key = info.get_def(name);
                        let escapes = info.is_escaped(obj_key);
                        sc.define_local_from_slot(name.symbol, elem_type, escapes, tmp_base + offset)?;
                    } else if let Some(local) = sc.func.lookup_local(name.symbol) {
                        sc.store_from_slot(local.storage, tmp_base + offset, elem_slots);
                    }
                    offset += elem_slots;
                }
            } else {
                // Normal case: N variables = N expressions
                let mut sc = StmtCompiler::new(ctx, func, info);
                for (i, name) in short_var.names.iter().enumerate() {
                    if info.project.interner.resolve(name.symbol) == Some("_") {
                        continue;
                    }

                    let type_key = short_var.values.get(i)
                        .map(|v| info.expr_type(v.id))
                        .expect("short var must have value");

                    let is_def = info.project.type_info.defs.contains_key(&name.id);
                    if is_def {
                        let obj_key = info.get_def(name);
                        let escapes = info.is_escaped(obj_key);
                        let init = short_var.values.get(i);
                        sc.define_local(name.symbol, type_key, escapes, init)?;
                    } else if let Some(local) = sc.func.lookup_local(name.symbol) {
                        let storage = local.storage;
                        if let Some(expr) = short_var.values.get(i) {
                            let src = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                            sc.func.emit_storage_store(storage, src);
                        }
                    }
                }
            }
        }

        // === Assignment ===
        StmtKind::Assign(assign) => {
            use vo_syntax::ast::AssignOp;
            
            // Check for multi-value case: v1, v2, ... = f() where f() returns a tuple
            // This includes comma-ok (2 values) and multi-return functions (3+ values)
            let is_multi_value = assign.op == AssignOp::Assign
                && assign.rhs.len() == 1
                && assign.lhs.len() >= 2
                && info.is_tuple(info.expr_type(assign.rhs[0].id));
            
            if is_multi_value {
                // Multi-value assignment: compile expr once, then distribute to variables
                let tuple_type = info.expr_type(assign.rhs[0].id);
                let total_slots = info.type_slot_count(tuple_type);
                let tmp_base = func.alloc_temp(total_slots);
                compile_expr_to(&assign.rhs[0], tmp_base, ctx, func, info)?;
                
                let mut offset = 0u16;
                for (i, lhs_expr) in assign.lhs.iter().enumerate() {
                    let elem_type = info.tuple_elem_type(tuple_type, i);
                    let elem_slots = info.type_slot_count(elem_type);
                    
                    // Skip blank identifier
                    if let vo_syntax::ast::ExprKind::Ident(ident) = &lhs_expr.kind {
                        if info.project.interner.resolve(ident.symbol) == Some("_") {
                            offset += elem_slots;
                            continue;
                        }
                    }
                    
                    // Get lhs location and copy from temp
                    let lhs_source = crate::expr::get_expr_source(lhs_expr, ctx, func, info);
                    if let ExprSource::Location(storage) = lhs_source {
                        func.emit_storage_store(storage, tmp_base + offset);
                    }
                    offset += elem_slots;
                }
            } else {
                // Normal case: N lhs = N rhs
                for (lhs, rhs) in assign.lhs.iter().zip(assign.rhs.iter()) {
                    if assign.op == AssignOp::Assign {
                        compile_assign(lhs, rhs, ctx, func, info)?;
                    } else {
                        // Compound assignment (+=, -=, etc.)
                        compile_compound_assign(lhs, rhs, assign.op, ctx, func, info)?;
                    }
                }
            }
        }

        // === Expression statement ===
        StmtKind::Expr(expr) => {
            let _ = crate::expr::compile_expr(expr, ctx, func, info)?;
        }

        // === Return ===
        StmtKind::Return(ret) => {
            if ret.values.is_empty() {
                // Bare return - check for named returns
                let named_syms = func.named_return_symbols();
                if named_syms.is_empty() {
                    func.emit_op(Opcode::Return, 0, 0, 0);
                } else {
                    // Look up named return variables in locals
                    let named_return_info: Vec<_> = named_syms.iter()
                        .map(|&sym| {
                            let local = func.lookup_local(sym)
                                .expect("named return variable must exist in locals");
                            (local.storage.slot(), local.storage.value_slots())
                        })
                        .collect();
                    
                    // Copy named return values to return area
                    let total_ret_slots: u16 = named_return_info.iter().map(|(_, s)| *s).sum();
                    let ret_start = func.alloc_temp(total_ret_slots);
                    let mut offset = 0u16;
                    for &(slot, slots) in &named_return_info {
                        func.emit_copy(ret_start + offset, slot, slots);
                        offset += slots;
                    }
                    func.emit_op(Opcode::Return, ret_start, total_ret_slots, 0);
                }
            } else {
                // Get function's return types (clone to avoid borrow issues)
                let ret_types: Vec<_> = func.return_types().to_vec();
                
                // Calculate total return slots needed (use declared return types)
                let mut total_ret_slots = 0u16;
                for ret_type in &ret_types {
                    total_ret_slots += info.type_slot_count(*ret_type);
                }
                
                // Allocate space for return values
                let ret_start = func.alloc_temp(total_ret_slots);
                
                // Compile return values with interface conversion if needed
                let mut offset = 0u16;
                for (i, result) in ret.values.iter().enumerate() {
                    let ret_type = ret_types.get(i).copied();
                    if let Some(rt) = ret_type {
                        let slots = info.type_slot_count(rt);
                        compile_value_to(result, ret_start + offset, rt, ctx, func, info)?;
                        offset += slots;
                    } else {
                        let slots = info.expr_slots(result.id);
                        compile_expr_to(result, ret_start + offset, ctx, func, info)?;
                        offset += slots;
                    }
                }
                func.emit_op(Opcode::Return, ret_start, total_ret_slots, 0);
            }
        }

        // === If statement ===
        StmtKind::If(if_stmt) => {
            // Init statement
            if let Some(init) = &if_stmt.init {
                compile_stmt(init, ctx, func, info)?;
            }

            // Condition
            let cond_reg = crate::expr::compile_expr(&if_stmt.cond, ctx, func, info)?;
            let else_jump = func.emit_jump(Opcode::JumpIfNot, cond_reg);

            // Then branch
            compile_block(&if_stmt.then, ctx, func, info)?;

            if let Some(else_body) = &if_stmt.else_ {
                let end_jump = func.emit_jump(Opcode::Jump, 0);
                func.patch_jump(else_jump, func.current_pc());
                compile_stmt(else_body, ctx, func, info)?;
                func.patch_jump(end_jump, func.current_pc());
            } else {
                func.patch_jump(else_jump, func.current_pc());
            }
        }

        // === For statement ===
        StmtKind::For(for_stmt) => {
            use vo_syntax::ast::ForClause;

            match &for_stmt.clause {
                ForClause::Cond(cond_opt) => {
                    // while-style: for cond { } or infinite: for { }
                    let loop_start = func.current_pc();
                    func.enter_loop(loop_start, label);

                    let end_jump = if let Some(cond) = cond_opt {
                        let cond_reg = crate::expr::compile_expr(cond, ctx, func, info)?;
                        Some(func.emit_jump(Opcode::JumpIfNot, cond_reg))
                    } else {
                        None
                    };

                    compile_block(&for_stmt.body, ctx, func, info)?;
                    func.emit_jump_to(Opcode::Jump, 0, loop_start);

                    if let Some(j) = end_jump {
                        func.patch_jump(j, func.current_pc());
                    }
                    let (break_patches, _) = func.exit_loop();
                    for pc in break_patches {
                        func.patch_jump(pc, func.current_pc());
                    }
                }

                ForClause::Three { init, cond, post } => {
                    // C-style: for init; cond; post { }
                    if let Some(init) = init {
                        compile_stmt(init, ctx, func, info)?;
                    }

                    let loop_start = func.current_pc();

                    let end_jump = if let Some(cond) = cond {
                        let cond_reg = crate::expr::compile_expr(cond, ctx, func, info)?;
                        Some(func.emit_jump(Opcode::JumpIfNot, cond_reg))
                    } else {
                        None
                    };

                    // continue_pc=0 means "patch later" - continue should go to post
                    func.enter_loop(0, label);
                    compile_block(&for_stmt.body, ctx, func, info)?;

                    // Post statement - this is where continue should jump to
                    let post_pc = func.current_pc();
                    if let Some(post) = post {
                        compile_stmt(post, ctx, func, info)?;
                    }

                    func.emit_jump_to(Opcode::Jump, 0, loop_start);

                    if let Some(j) = end_jump {
                        func.patch_jump(j, func.current_pc());
                    }

                    let (break_patches, continue_patches) = func.exit_loop();
                    
                    // Patch break jumps to after loop
                    for pc in break_patches {
                        func.patch_jump(pc, func.current_pc());
                    }
                    
                    // Patch continue jumps to post statement
                    for pc in continue_patches {
                        func.patch_jump(pc, post_pc);
                    }
                }

                ForClause::Range { key, value, define, expr } => {
                    // All for-range loops expanded at compile time. No runtime iterator state.
                    let range_type = info.expr_type(expr.id);
                    let mut sc = StmtCompiler::new(ctx, func, info);
                    
                    if info.is_array(range_type) {
                        let es = info.array_elem_slots(range_type);
                        let et = info.array_elem_type(range_type);
                        let len = info.array_len(range_type) as i64;
                        let src = crate::expr::get_expr_source(expr, sc.ctx, sc.func, sc.info);
                        let (reg, stk, base) = match src {
                            ExprSource::Location(StorageKind::StackValue { slot, .. }) => (0, true, slot),
                            _ => (crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?, false, 0),
                        };
                        let (ks, vs) = (range_var_slot(&mut sc, key.as_ref(), et, *define)?,
                                        range_var_slot(&mut sc, value.as_ref(), et, *define)?);
                        let ls = sc.func.alloc_temp(1);
                        sc.func.emit_op(Opcode::LoadInt, ls, len as u16, (len >> 16) as u16);
                        let lp = IndexLoop::begin(sc.func, ls, label);
                        lp.emit_key(sc.func, key.as_ref().map(|_| ks));
                        if value.is_some() {
                            let op = if stk { Opcode::SlotGetN } else { Opcode::ArrayGet };
                            sc.func.emit_with_flags(op, es as u8, vs, if stk { base } else { reg }, lp.idx_slot);
                        }
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        lp.end(sc.func);
                        
                    } else if info.is_slice(range_type) {
                        let es = info.slice_elem_slots(range_type);
                        let et = info.slice_elem_type(range_type);
                        let reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                        let (ks, vs) = (range_var_slot(&mut sc, key.as_ref(), et, *define)?,
                                        range_var_slot(&mut sc, value.as_ref(), et, *define)?);
                        let ls = sc.func.alloc_temp(1);
                        sc.func.emit_op(Opcode::SliceLen, ls, reg, 0);
                        let lp = IndexLoop::begin(sc.func, ls, label);
                        lp.emit_key(sc.func, key.as_ref().map(|_| ks));
                        if value.is_some() { sc.func.emit_with_flags(Opcode::SliceGet, es as u8, vs, reg, lp.idx_slot); }
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        lp.end(sc.func);
                        
                    } else if info.is_string(range_type) {
                        // String: iterate by rune (variable width)
                        let reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                        let (ks, vs) = (range_var_slot(&mut sc, key.as_ref(), range_type, *define)?,
                                        range_var_slot(&mut sc, value.as_ref(), range_type, *define)?);
                        let (pos, len, cmp) = (sc.func.alloc_temp(1), sc.func.alloc_temp(1), sc.func.alloc_temp(1));
                        // StrDecodeRune writes (rune, width) to consecutive slots
                        let rune_width = sc.func.alloc_temp(2);
                        
                        sc.func.emit_op(Opcode::LoadInt, pos, 0, 0);
                        sc.func.emit_op(Opcode::StrLen, len, reg, 0);
                        
                        let loop_start = sc.func.current_pc();
                        sc.func.enter_loop(loop_start, label);
                        sc.func.emit_op(Opcode::GeI, cmp, pos, len);
                        let end_jump = sc.func.emit_jump(Opcode::JumpIf, cmp);
                        
                        sc.func.emit_op(Opcode::StrDecodeRune, rune_width, reg, pos);
                        if key.is_some() { sc.func.emit_op(Opcode::Copy, ks, pos, 0); }
                        if value.is_some() { sc.func.emit_op(Opcode::Copy, vs, rune_width, 0); }
                        
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        
                        let post_pc = sc.func.current_pc();
                        sc.func.emit_op(Opcode::AddI, pos, pos, rune_width + 1);
                        sc.func.emit_jump_to(Opcode::Jump, 0, loop_start);
                        
                        sc.func.patch_jump(end_jump, sc.func.current_pc());
                        let (breaks, continues) = sc.func.exit_loop();
                        for pc in breaks { sc.func.patch_jump(pc, sc.func.current_pc()); }
                        for pc in continues { sc.func.patch_jump(pc, post_pc); }
                        
                    } else if info.is_map(range_type) {
                        let reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                        let (kn, vn) = info.map_key_val_slots(range_type);
                        let (kt, vt) = info.map_key_val_types(range_type);
                        let (ks, vs) = (range_var_slot(&mut sc, key.as_ref(), kt, *define)?,
                                        range_var_slot(&mut sc, value.as_ref(), vt, *define)?);
                        let ls = sc.func.alloc_temp(1);
                        sc.func.emit_op(Opcode::MapLen, ls, reg, 0);
                        let lp = IndexLoop::begin(sc.func, ls, label);
                        sc.func.emit_with_flags(Opcode::MapIterGet, (kn as u8) | ((vn as u8) << 4), ks, reg, lp.idx_slot);
                        if value.is_some() && vs != ks + kn {
                            if vn == 1 { sc.func.emit_op(Opcode::Copy, vs, ks + kn, 0); }
                            else { sc.func.emit_with_flags(Opcode::CopyN, vn as u8, vs, ks + kn, 0); }
                        }
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        lp.end(sc.func);
                        
                    } else if info.is_chan(range_type) {
                        let chan_reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                        let elem_type = info.chan_elem_type(range_type);
                        let elem_slots = info.chan_elem_slots(range_type);
                        
                        // Channel: use value or key (Go semantics: single var is value)
                        let var_expr = value.as_ref().or(key.as_ref());
                        let val_slot = range_var_slot(&mut sc, var_expr, elem_type, *define)?;
                        
                        // ok slot
                        let ok_slot = sc.func.alloc_temp(1);
                        
                        // loop:
                        let loop_start = sc.func.current_pc();
                        sc.func.enter_loop(loop_start, label);
                        
                        // v, ok := <-ch
                        // ChanRecv: a=val_slot, b=chan_reg, c=ok_slot, flags=elem_slots|0x80 (with_ok)
                        let recv_flags = (elem_slots as u8) | 0x80;
                        sc.func.emit_with_flags(Opcode::ChanRecv, recv_flags, val_slot, chan_reg, ok_slot);
                        
                        // if !ok { goto end }
                        let end_jump = sc.func.emit_jump(Opcode::JumpIfNot, ok_slot);
                        
                        // body
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        
                        // goto loop (continue target is loop_start for channel)
                        sc.func.emit_jump_to(Opcode::Jump, 0, loop_start);
                        
                        // end:
                        sc.func.patch_jump(end_jump, sc.func.current_pc());
                        let (break_patches, continue_patches) = sc.func.exit_loop();
                        for pc in break_patches {
                            sc.func.patch_jump(pc, sc.func.current_pc());
                        }
                        for pc in continue_patches {
                            sc.func.patch_jump(pc, loop_start);
                        }
                        
                    } else {
                        return Err(CodegenError::UnsupportedStmt("for-range unsupported type".to_string()));
                    }
                }
            }
        }

        // === Block ===
        StmtKind::Block(block) => {
            compile_block(block, ctx, func, info)?;
        }

        // === Break ===
        StmtKind::Break(brk) => {
            func.emit_break(brk.label.as_ref().map(|l| l.symbol));
        }

        // === Continue ===
        StmtKind::Continue(cont) => {
            func.emit_continue(cont.label.as_ref().map(|l| l.symbol));
        }

        // === Empty ===
        StmtKind::Empty => {}

        // === Defer ===
        StmtKind::Defer(defer_stmt) => {
            // Defer is implemented as pushing a closure to defer stack
            // The call expression becomes a closure that will be called on function exit
            compile_defer(&defer_stmt.call, ctx, func, info)?;
        }

        // === Go ===
        StmtKind::Go(go_stmt) => {
            compile_go(&go_stmt.call, ctx, func, info)?;
        }

        // === Send (channel send) ===
        StmtKind::Send(send_stmt) => {
            let chan_reg = crate::expr::compile_expr(&send_stmt.chan, ctx, func, info)?;
            let val_reg = crate::expr::compile_expr(&send_stmt.value, ctx, func, info)?;
            let chan_type = info.expr_type(send_stmt.chan.id);
            let elem_slots = info.chan_elem_slots(chan_type) as u8;
            func.emit_with_flags(Opcode::ChanSend, elem_slots, chan_reg, val_reg, 0);
        }

        // === Select ===
        StmtKind::Select(select_stmt) => {
            compile_select(select_stmt, ctx, func, info)?;
        }

        // === Switch ===
        StmtKind::Switch(switch_stmt) => {
            compile_switch(switch_stmt, ctx, func, info)?;
        }

        // === Labeled statement ===
        StmtKind::Labeled(labeled) => {
            // Register label position for goto
            func.define_label(labeled.label.symbol);
            // Pass label to inner statement (for labeled break/continue)
            compile_stmt_with_label(&labeled.stmt, ctx, func, info, Some(labeled.label.symbol))?;
        }

        // === Inc/Dec ===
        StmtKind::IncDec(inc_dec) => {
            use crate::lvalue::{emit_lvalue_load, emit_lvalue_store};
            
            let lv = crate::lvalue::resolve_lvalue(&inc_dec.expr, ctx, func, info)?;
            let tmp = func.alloc_temp(1);
            emit_lvalue_load(&lv, tmp, func);
            
            let one = func.alloc_temp(1);
            func.emit_op(Opcode::LoadInt, one, 1, 0);
            
            if inc_dec.is_inc {
                func.emit_op(Opcode::AddI, tmp, tmp, one);
            } else {
                func.emit_op(Opcode::SubI, tmp, tmp, one);
            }
            
            emit_lvalue_store(&lv, tmp, func);
        }

        // === TypeSwitch ===
        StmtKind::TypeSwitch(type_switch) => {
            compile_type_switch(type_switch, ctx, func, info)?;
        }

        // === ErrDefer ===
        StmtKind::ErrDefer(err_defer) => {
            compile_defer_impl(&err_defer.call, ctx, func, info, true)?;
        }

        // === Fail ===
        StmtKind::Fail(fail_stmt) => {
            // Fail returns zero values for all non-error returns, plus the error value
            // This is equivalent to: return <zero-values>, err
            
            // Get function's return types
            let ret_types: Vec<_> = func.return_types().to_vec();
            
            // Calculate total return slots needed
            let mut total_ret_slots = 0u16;
            for ret_type in &ret_types {
                total_ret_slots += info.type_slot_count(*ret_type);
            }
            
            // Allocate space for return values
            let ret_start = func.alloc_temp(total_ret_slots);
            
            // Initialize all slots to zero/nil first
            // Use LoadNil for proper GC-safe initialization
            for i in 0..total_ret_slots {
                func.emit_op(Opcode::LoadNil, ret_start + i, 0, 0);
            }
            
            // Compile the error expression into the last return slot(s)
            // The error is the last return value
            if !ret_types.is_empty() {
                let error_type = *ret_types.last().unwrap();
                let error_slots = info.type_slot_count(error_type);
                let error_start = ret_start + total_ret_slots - error_slots;
                compile_value_to(&fail_stmt.error, error_start, error_type, ctx, func, info)?;
            }
            
            func.emit_op(Opcode::Return, ret_start, total_ret_slots, 0);
        }

        // === Goto ===
        StmtKind::Goto(goto_stmt) => {
            func.emit_goto(goto_stmt.label.symbol);
        }

        // === Fallthrough ===
        StmtKind::Fallthrough => {
            // Handled in switch compilation - skipped here
        }

        // === Const declaration (in block) ===
        StmtKind::Const(_const_decl) => {
            // Constants are compile-time, no runtime code needed
        }

        // === Type declaration (in block) ===
        StmtKind::Type(_type_decl) => {
            // Type declarations are compile-time, no runtime code needed
        }
    }

    Ok(())
}

/// Compile a block.
pub fn compile_block(
    block: &Block,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    for stmt in &block.stmts {
        compile_stmt(stmt, ctx, func, info)?;
    }
    Ok(())
}

/// Compile defer statement
/// DeferPush instruction format:
/// - a: func_id (flags bit 0 = 0) or closure_reg (flags bit 0 = 1)
/// - b: arg_start
/// - c: arg_slots
/// - flags bit 0: is_closure
fn compile_defer(
    call: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    compile_defer_impl(call, ctx, func, info, false)
}

fn compile_defer_impl(
    call: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
    is_errdefer: bool,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::ExprKind;
    
    let opcode = if is_errdefer { Opcode::ErrDeferPush } else { Opcode::DeferPush };
    
    if let ExprKind::Call(call_expr) = &call.kind {
        // Compute total arg slots
        let func_type = info.expr_type(call_expr.func.id);
        let param_types = info.func_param_types(func_type);
        
        let mut total_arg_slots = 0u16;
        for (i, arg) in call_expr.args.iter().enumerate() {
            let param_type = param_types.get(i).copied();
            let slots = if let Some(pt) = param_type {
                info.type_slot_count(pt)
            } else {
                info.expr_slots(arg.id)
            };
            total_arg_slots += slots;
        }
        
        // Check if it's a regular function call
        if let ExprKind::Ident(ident) = &call_expr.func.kind {
            if let Some(func_idx) = ctx.get_function_index(ident.symbol) {
                // Regular function call - compile args
                let args_start = if total_arg_slots > 0 {
                    func.alloc_temp(total_arg_slots)
                } else {
                    0
                };
                
                let mut offset = 0u16;
                for (i, arg) in call_expr.args.iter().enumerate() {
                    let param_type = param_types.get(i).copied();
                    if let Some(pt) = param_type {
                        let slots = info.type_slot_count(pt);
                        compile_value_to(arg, args_start + offset, pt, ctx, func, info)?;
                        offset += slots;
                    } else {
                        let arg_slots = info.expr_slots(arg.id);
                        crate::expr::compile_expr_to(arg, args_start + offset, ctx, func, info)?;
                        offset += arg_slots;
                    }
                }
                
                // DeferPush: a=func_id_low, b=arg_start, c=arg_slots, flags=func_id_high<<1
                let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_idx);
                let flags = func_id_high << 1;  // bit 0 = 0 (not closure)
                func.emit_with_flags(opcode, flags, func_id_low, args_start, total_arg_slots);
                return Ok(());
            }
            
            // Check if it's a local variable (closure)
            if func.lookup_local(ident.symbol).is_some() || func.lookup_capture(ident.symbol).is_some() {
                let closure_reg = crate::expr::compile_expr(&call_expr.func, ctx, func, info)?;
                
                let args_start = if total_arg_slots > 0 {
                    func.alloc_temp(total_arg_slots)
                } else {
                    0
                };
                
                let mut offset = 0u16;
                for arg in &call_expr.args {
                    let arg_slots = info.expr_slots(arg.id);
                    crate::expr::compile_expr_to(arg, args_start + offset, ctx, func, info)?;
                    offset += arg_slots;
                }
                
                // DeferPush: a=closure_reg, b=arg_start, c=arg_slots, flags=1 (is_closure)
                func.emit_with_flags(opcode, 1, closure_reg, args_start, total_arg_slots);
                return Ok(());
            }
        }
        
        // Generic case: expression returning a closure
        let closure_reg = crate::expr::compile_expr(&call_expr.func, ctx, func, info)?;
        
        let args_start = if total_arg_slots > 0 {
            func.alloc_temp(total_arg_slots)
        } else {
            0
        };
        
        let mut offset = 0u16;
        for arg in &call_expr.args {
            let arg_slots = info.expr_slots(arg.id);
            crate::expr::compile_expr_to(arg, args_start + offset, ctx, func, info)?;
            offset += arg_slots;
        }
        
        func.emit_with_flags(opcode, 1, closure_reg, args_start, total_arg_slots);
        return Ok(());
    }
    
    Err(CodegenError::UnsupportedStmt("defer requires a call expression".to_string()))
}

/// Compile go statement
/// GoStart: a=func_id/closure, b=args_start, c=arg_slots, flags bit0=is_closure
fn compile_go(
    call: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::ExprKind;
    
    if let ExprKind::Call(call_expr) = &call.kind {
        // Compute total arg slots
        let func_type = info.expr_type(call_expr.func.id);
        let param_types = info.func_param_types(func_type);
        
        let mut total_arg_slots = 0u16;
        for (i, arg) in call_expr.args.iter().enumerate() {
            let param_type = param_types.get(i).copied();
            let slots = if let Some(pt) = param_type {
                info.type_slot_count(pt)
            } else {
                info.expr_slots(arg.id)
            };
            total_arg_slots += slots;
        }
        
        // Check if it's a regular function call
        if let ExprKind::Ident(ident) = &call_expr.func.kind {
            if let Some(func_idx) = ctx.get_function_index(ident.symbol) {
                // Regular function - compile args
                let args_start = if total_arg_slots > 0 {
                    func.alloc_temp(total_arg_slots)
                } else {
                    0
                };
                
                let mut offset = 0u16;
                for (i, arg) in call_expr.args.iter().enumerate() {
                    let param_type = param_types.get(i).copied();
                    if let Some(pt) = param_type {
                        let slots = info.type_slot_count(pt);
                        compile_value_to(arg, args_start + offset, pt, ctx, func, info)?;
                        offset += slots;
                    } else {
                        let arg_slots = info.expr_slots(arg.id);
                        crate::expr::compile_expr_to(arg, args_start + offset, ctx, func, info)?;
                        offset += arg_slots;
                    }
                }
                
                // GoStart: a=func_id_low, b=args_start, c=arg_slots, flags=func_id_high<<1
                let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_idx);
                let flags = func_id_high << 1;  // bit 0 = 0 (not closure)
                func.emit_with_flags(Opcode::GoStart, flags, func_id_low, args_start, total_arg_slots);
                return Ok(());
            }
            
            // Check if it's a local variable (closure)
            if func.lookup_local(ident.symbol).is_some() || func.lookup_capture(ident.symbol).is_some() {
                let closure_reg = crate::expr::compile_expr(&call_expr.func, ctx, func, info)?;
                
                let args_start = if total_arg_slots > 0 {
                    func.alloc_temp(total_arg_slots)
                } else {
                    0
                };
                
                let mut offset = 0u16;
                for arg in &call_expr.args {
                    let arg_slots = info.expr_slots(arg.id);
                    crate::expr::compile_expr_to(arg, args_start + offset, ctx, func, info)?;
                    offset += arg_slots;
                }
                
                // GoStart: a=closure_reg, b=args_start, c=arg_slots, flags=1 (is_closure)
                func.emit_with_flags(Opcode::GoStart, 1, closure_reg, args_start, total_arg_slots);
                return Ok(());
            }
        }
        
        // Generic case: expression returning a closure
        let closure_reg = crate::expr::compile_expr(&call_expr.func, ctx, func, info)?;
        
        let args_start = if total_arg_slots > 0 {
            func.alloc_temp(total_arg_slots)
        } else {
            0
        };
        
        let mut offset = 0u16;
        for arg in &call_expr.args {
            let arg_slots = info.expr_slots(arg.id);
            crate::expr::compile_expr_to(arg, args_start + offset, ctx, func, info)?;
            offset += arg_slots;
        }
        
        func.emit_with_flags(Opcode::GoStart, 1, closure_reg, args_start, total_arg_slots);
        return Ok(());
    }
    
    Err(CodegenError::UnsupportedStmt("go requires a call expression".to_string()))
}

/// Compile select statement
fn compile_select(
    select_stmt: &vo_syntax::ast::SelectStmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::CommClause;
    
    // Count cases and check for default
    let case_count = select_stmt.cases.len() as u16;
    let has_default = select_stmt.cases.iter().any(|c| c.comm.is_none());
    let flags = if has_default { 1u8 } else { 0u8 };
    
    // SelectBegin: a=case_count, flags=has_default
    func.emit_with_flags(Opcode::SelectBegin, flags, case_count, 0, 0);
    
    // Add each case
    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        match &case.comm {
            None => {
                // Default case - no instruction needed, handled by SelectExec
            }
            Some(CommClause::Send(send)) => {
                // SelectSend: a=chan_reg, b=val_reg, flags=elem_slots
                let chan_reg = crate::expr::compile_expr(&send.chan, ctx, func, info)?;
                let val_reg = crate::expr::compile_expr(&send.value, ctx, func, info)?;
                let chan_type = info.expr_type(send.chan.id);
                let elem_slots = info.chan_elem_slots(chan_type) as u8;
                func.emit_with_flags(Opcode::SelectSend, elem_slots, chan_reg, val_reg, case_idx as u16);
            }
            Some(CommClause::Recv(recv)) => {
                // SelectRecv: a=dst_reg, b=chan_reg, flags=(elem_slots<<1|has_ok)
                let chan_reg = crate::expr::compile_expr(&recv.expr, ctx, func, info)?;
                let chan_type = info.expr_type(recv.expr.id);
                let elem_slots = info.chan_elem_slots(chan_type);
                
                // Allocate destination for received value
                let has_ok = recv.lhs.len() > 1;
                let dst_slots = if has_ok { elem_slots + 1 } else { elem_slots };
                let dst_reg = func.alloc_temp(dst_slots);
                
                let flags = ((elem_slots as u8) << 1) | (if has_ok { 1 } else { 0 });
                func.emit_with_flags(Opcode::SelectRecv, flags, dst_reg, chan_reg, case_idx as u16);
            }
        }
    }
    
    // SelectExec: a=result_reg (chosen case index, -1 for default)
    let result_reg = func.alloc_temp(1);
    func.emit_op(Opcode::SelectExec, result_reg, 0, 0);
    
    // Generate switch on result to jump to appropriate case body
    let mut case_jumps = Vec::new();
    let mut end_jumps = Vec::new();
    
    for (case_idx, _case) in select_stmt.cases.iter().enumerate() {
        // Compare result_reg with case_idx
        let cmp_tmp = func.alloc_temp(1);
        let idx_val = case_idx as i32;
        if _case.comm.is_none() {
            // Default case: check if result == -1
            let (b, c) = encode_i32(-1);
            func.emit_op(Opcode::LoadInt, cmp_tmp, b, c);
        } else {
            let (b, c) = encode_i32(idx_val);
            func.emit_op(Opcode::LoadInt, cmp_tmp, b, c);
        }
        func.emit_op(Opcode::EqI, cmp_tmp, result_reg, cmp_tmp);
        case_jumps.push((case_idx, func.emit_jump(Opcode::JumpIf, cmp_tmp)));
    }
    
    // Jump past all cases if no match (shouldn't happen)
    let fallthrough_jump = func.emit_jump(Opcode::Jump, 0);
    
    // Compile case bodies
    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        // Patch the jump for this case
        for (idx, jump_pc) in &case_jumps {
            if *idx == case_idx {
                func.patch_jump(*jump_pc, func.current_pc());
            }
        }
        
        // Define variables for recv case if needed
        if let Some(CommClause::Recv(recv)) = &case.comm {
            if recv.define && !recv.lhs.is_empty() {
                // Define the received value variable(s)
                let chan_type = info.expr_type(recv.expr.id);
                let elem_slots = info.chan_elem_slots(chan_type);
                
                for (i, name) in recv.lhs.iter().enumerate() {
                    if i == 0 {
                        // First variable gets the value
                        let slot_types = vec![vo_common_core::types::SlotType::Value; elem_slots as usize];
                        func.define_local_stack(name.symbol, elem_slots, &slot_types);
                    } else {
                        // Second variable gets the ok bool
                        func.define_local_stack(name.symbol, 1, &[vo_common_core::types::SlotType::Value]);
                    }
                }
            }
        }
        
        // Compile case body
        for stmt in &case.body {
            compile_stmt(stmt, ctx, func, info)?;
        }
        
        // Jump to end
        end_jumps.push(func.emit_jump(Opcode::Jump, 0));
    }
    
    // Patch fallthrough and end jumps
    func.patch_jump(fallthrough_jump, func.current_pc());
    for jump_pc in end_jumps {
        func.patch_jump(jump_pc, func.current_pc());
    }
    
    Ok(())
}


/// Compile type switch statement
/// Uses IfaceAssert instruction for type checking and value extraction.
fn compile_type_switch(
    type_switch: &vo_syntax::ast::TypeSwitchStmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Init statement
    if let Some(init) = &type_switch.init {
        compile_stmt(init, ctx, func, info)?;
    }
    
    // Compile the expression being type-switched (must be interface type)
    // type_switch.expr is x.(type), we need to compile the inner expression x
    let inner_expr = if let vo_syntax::ast::ExprKind::TypeAssert(ta) = &type_switch.expr.kind {
        &ta.expr
    } else {
        &type_switch.expr
    };
    let expr_reg = crate::expr::compile_expr(inner_expr, ctx, func, info)?;
    
    // Store interface for case comparisons
    let iface_slot = func.alloc_temp(2);
    func.emit_op(Opcode::Copy, iface_slot, expr_reg, 0);
    func.emit_op(Opcode::Copy, iface_slot + 1, expr_reg + 1, 0);
    
    // Collect case jumps and info for variable binding
    let mut case_jumps: Vec<(usize, usize)> = Vec::new(); // (case_idx, jump_pc)
    let mut end_jumps: Vec<usize> = Vec::new();
    let mut default_case_idx: Option<usize> = None;
    
    // Generate type checks for each case using IfaceAssert
    for (case_idx, case) in type_switch.cases.iter().enumerate() {
        if case.types.is_empty() || case.types.iter().all(|t| t.is_none()) {
            // Default case
            default_case_idx = Some(case_idx);
        } else {
            // Type case - check each type
            for type_opt in &case.types {
                if let Some(type_expr) = type_opt {
                    let type_key = info.type_expr_type(type_expr.id);
                    
                    // Determine assert_kind and target_id (same logic as compile_type_assert)
                    let (assert_kind, target_id): (u8, u32) = if info.is_interface(type_key) {
                        // Interface: assert_kind=1, target_id=iface_meta_id
                        let iface_meta_id = ctx.get_or_create_interface_meta_id(type_key, &info.project.tc_objs);
                        (1, iface_meta_id)
                    } else {
                        // All other types: assert_kind=0, target_id=rttid
                        let rt = crate::type_key_to_runtime_type_simple(type_key, info, &info.project.interner, ctx);
                        let rttid = ctx.intern_rttid(rt);
                        (0, rttid)
                    };
                    
                    // Allocate temp for IfaceAssert result (value + ok)
                    let target_slots = info.type_slot_count(type_key) as u8;
                    let result_slots = if assert_kind == 1 { 2 } else { target_slots as u16 };
                    let result_reg = func.alloc_temp(result_slots + 1); // +1 for ok bool
                    let ok_slot = result_reg + result_slots;
                    
                    // IfaceAssert: a=dst, b=src_iface, c=target_id
                    // flags = assert_kind | (has_ok << 2) | (target_slots << 3)
                    let flags = assert_kind | (1 << 2) | ((target_slots) << 3);
                    func.emit_with_flags(Opcode::IfaceAssert, flags, result_reg, iface_slot, target_id as u16);
                    
                    // Jump to case body if ok is true
                    case_jumps.push((case_idx, func.emit_jump(Opcode::JumpIf, ok_slot)));
                }
            }
        }
    }
    
    // Jump to default or end if no case matched
    let no_match_jump = if let Some(default_idx) = default_case_idx {
        Some((default_idx, func.emit_jump(Opcode::Jump, 0)))
    } else {
        Some((usize::MAX, func.emit_jump(Opcode::Jump, 0)))
    };
    
    // Compile case bodies
    let mut case_body_starts: Vec<usize> = Vec::new();
    for (case_idx, case) in type_switch.cases.iter().enumerate() {
        case_body_starts.push(func.current_pc());
        
        // If assign variable is specified, bind it to the asserted value
        if let Some(assign_name) = &type_switch.assign {
            if !case.types.is_empty() {
                if let Some(Some(type_expr)) = case.types.first() {
                    let type_key = info.type_expr_type(type_expr.id);
                    let slots = info.type_slot_count(type_key);
                    let slot_types = info.type_slot_types(type_key);
                    
                    // Define local variable for the asserted value
                    let var_slot = func.define_local_stack(assign_name.symbol, slots, &slot_types);
                    
                    // Re-do IfaceAssert to extract value (we know it will succeed)
                    // This is simpler than trying to reuse the check result
                    let (assert_kind, target_id): (u8, u32) = if info.is_interface(type_key) {
                        let iface_meta_id = ctx.get_or_create_interface_meta_id(type_key, &info.project.tc_objs);
                        (1, iface_meta_id)
                    } else {
                        let rt = crate::type_key_to_runtime_type_simple(type_key, info, &info.project.interner, ctx);
                        let rttid = ctx.intern_rttid(rt);
                        (0, rttid)
                    };
                    
                    let target_slots = slots as u8;
                    // IfaceAssert without ok (has_ok=0), result goes directly to var_slot
                    let flags = assert_kind | ((target_slots) << 3);
                    func.emit_with_flags(Opcode::IfaceAssert, flags, var_slot, iface_slot, target_id as u16);
                }
            }
        }
        
        // Compile case body
        for stmt in &case.body {
            compile_stmt(stmt, ctx, func, info)?;
        }
        
        // Jump to end
        end_jumps.push(func.emit_jump(Opcode::Jump, 0));
        
        let _ = case_idx;
    }
    
    let end_pc = func.current_pc();
    
    // Patch case jumps
    for (case_idx, jump_pc) in &case_jumps {
        if *case_idx < case_body_starts.len() {
            func.patch_jump(*jump_pc, case_body_starts[*case_idx]);
        }
    }
    
    // Patch no match jump
    if let Some((idx, jump_pc)) = no_match_jump {
        if idx < case_body_starts.len() {
            func.patch_jump(jump_pc, case_body_starts[idx]);
        } else {
            func.patch_jump(jump_pc, end_pc);
        }
    }
    
    // Patch end jumps
    for jump_pc in end_jumps {
        func.patch_jump(jump_pc, end_pc);
    }
    
    Ok(())
}

/// Compile switch statement
fn compile_switch(
    switch_stmt: &vo_syntax::ast::SwitchStmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Init statement
    if let Some(init) = &switch_stmt.init {
        compile_stmt(init, ctx, func, info)?;
    }
    
    // Compile tag expression (if present)
    let tag_reg = if let Some(tag) = &switch_stmt.tag {
        Some(crate::expr::compile_expr(tag, ctx, func, info)?)
    } else {
        None
    };
    
    // Collect case jumps and body positions
    let mut case_jumps: Vec<usize> = Vec::new();
    let mut end_jumps: Vec<usize> = Vec::new();
    let mut default_jump: Option<usize> = None;
    
    // Generate comparison and conditional jumps for each case
    for case in &switch_stmt.cases {
        if case.exprs.is_empty() {
            // Default case - will jump here if no other case matches
            default_jump = Some(func.emit_jump(Opcode::Jump, 0));
        } else {
            // Regular case - compare with each expression
            for case_expr in &case.exprs {
                let case_val = crate::expr::compile_expr(case_expr, ctx, func, info)?;
                let cmp_result = func.alloc_temp(1);
                
                if let Some(tag) = tag_reg {
                    // Compare tag with case value
                    let tag_type = switch_stmt.tag.as_ref().map(|t| info.expr_type(t.id));
                    let is_string = tag_type.map(|t| info.is_string(t)).unwrap_or(false);
                    
                    if is_string {
                        func.emit_op(Opcode::StrEq, cmp_result, tag, case_val);
                    } else {
                        func.emit_op(Opcode::EqI, cmp_result, tag, case_val);
                    }
                } else {
                    // No tag - case_expr should be boolean
                    func.emit_op(Opcode::Copy, cmp_result, case_val, 0);
                }
                
                case_jumps.push(func.emit_jump(Opcode::JumpIf, cmp_result));
            }
        }
    }
    
    // Jump to default or end if no case matched
    let no_match_jump = if default_jump.is_some() {
        None
    } else {
        Some(func.emit_jump(Opcode::Jump, 0))
    };
    
    // Compile case bodies
    let mut case_body_starts: Vec<usize> = Vec::new();
    for case in &switch_stmt.cases {
        case_body_starts.push(func.current_pc());
        
        // Compile statements, track if ends with fallthrough
        let mut has_fallthrough = false;
        for stmt in &case.body {
            if matches!(stmt.kind, StmtKind::Fallthrough) {
                has_fallthrough = true;
            } else {
                compile_stmt(stmt, ctx, func, info)?;
            }
        }
        
        // Jump to end unless case ends with fallthrough
        if !has_fallthrough {
            end_jumps.push(func.emit_jump(Opcode::Jump, 0));
        }
    }
    
    let end_pc = func.current_pc();
    
    // Patch jumps
    let mut case_idx = 0;
    let mut jump_idx = 0;
    for case in &switch_stmt.cases {
        if case.exprs.is_empty() {
            // Default case
            if let Some(jump_pc) = default_jump {
                func.patch_jump(jump_pc, case_body_starts[case_idx]);
            }
        } else {
            // Regular case - patch all expression jumps
            for _ in &case.exprs {
                if jump_idx < case_jumps.len() {
                    func.patch_jump(case_jumps[jump_idx], case_body_starts[case_idx]);
                    jump_idx += 1;
                }
            }
        }
        case_idx += 1;
    }
    
    // Patch no match jump
    if let Some(jump_pc) = no_match_jump {
        func.patch_jump(jump_pc, end_pc);
    }
    
    // Patch end jumps
    for jump_pc in end_jumps {
        func.patch_jump(jump_pc, end_pc);
    }
    
    Ok(())
}

/// Compile assignment using LValue abstraction.
fn compile_assign(
    lhs: &vo_syntax::ast::Expr,
    rhs: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use crate::lvalue::{resolve_lvalue, emit_lvalue_store, lvalue_slots};
    
    // Handle blank identifier: compile RHS for side effects only
    if let vo_syntax::ast::ExprKind::Ident(ident) = &lhs.kind {
        if info.project.interner.resolve(ident.symbol) == Some("_") {
            let _ = crate::expr::compile_expr(rhs, ctx, func, info)?;
            return Ok(());
        }
    }
    
    // Resolve LHS to an LValue
    let lv = resolve_lvalue(lhs, ctx, func, info)?;
    let slots = lvalue_slots(&lv);
    
    // Get LHS type for interface check
    let lhs_type = info.expr_type(lhs.id);
    
    // Handle interface assignment specially
    if info.is_interface(lhs_type) {
        return compile_assign_to_interface(&lv, rhs, lhs_type, ctx, func, info);
    }
    
    // Compile RHS to temp, then store to LValue
    let tmp = func.alloc_temp(slots);
    compile_expr_to(rhs, tmp, ctx, func, info)?;
    emit_lvalue_store(&lv, tmp, func);
    
    Ok(())
}

/// Compile assignment to an interface LValue.
fn compile_assign_to_interface(
    lv: &crate::lvalue::LValue,
    rhs: &vo_syntax::ast::Expr,
    iface_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use crate::lvalue::emit_lvalue_store;
    
    // Interface is always 2 slots
    let tmp = func.alloc_temp(2);
    compile_iface_assign(tmp, rhs, iface_type, ctx, func, info)?;
    emit_lvalue_store(lv, tmp, func);
    Ok(())
}

/// Compile compound assignment (+=, -=, *=, etc.) using LValue abstraction.
fn compile_compound_assign(
    lhs: &vo_syntax::ast::Expr,
    rhs: &vo_syntax::ast::Expr,
    op: vo_syntax::ast::AssignOp,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::AssignOp;
    use crate::lvalue::{resolve_lvalue, emit_lvalue_load, emit_lvalue_store, lvalue_slots};
    
    // Get the operation opcode based on AssignOp and type
    let lhs_type = info.expr_type(lhs.id);
    let is_float = info.is_float(lhs_type);
    let is_unsigned = info.is_unsigned(lhs_type);
    
    let opcode = match (op, is_float, is_unsigned) {
        (AssignOp::Add, false, _) => Opcode::AddI,
        (AssignOp::Add, true, _) => Opcode::AddF,
        (AssignOp::Sub, false, _) => Opcode::SubI,
        (AssignOp::Sub, true, _) => Opcode::SubF,
        (AssignOp::Mul, false, _) => Opcode::MulI,
        (AssignOp::Mul, true, _) => Opcode::MulF,
        (AssignOp::Div, false, _) => Opcode::DivI,
        (AssignOp::Div, true, _) => Opcode::DivF,
        (AssignOp::Rem, _, _) => Opcode::ModI,
        (AssignOp::And, _, _) => Opcode::And,
        (AssignOp::Or, _, _) => Opcode::Or,
        (AssignOp::Xor, _, _) => Opcode::Xor,
        (AssignOp::AndNot, _, _) => Opcode::AndNot,
        (AssignOp::Shl, _, _) => Opcode::Shl,
        (AssignOp::Shr, _, false) => Opcode::ShrS,
        (AssignOp::Shr, _, true) => Opcode::ShrU,
        (AssignOp::Assign, _, _) => unreachable!("plain assign handled separately"),
    };
    
    // Resolve LHS to an LValue
    let lv = resolve_lvalue(lhs, ctx, func, info)?;
    let slots = lvalue_slots(&lv);
    
    // Compile RHS
    let rhs_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
    
    // Read current value, apply operation, write back
    let tmp = func.alloc_temp(slots);
    emit_lvalue_load(&lv, tmp, func);
    func.emit_op(opcode, tmp, tmp, rhs_reg);
    emit_lvalue_store(&lv, tmp, func);
    
    Ok(())
}

/// Compile interface assignment: dst = src where dst is interface type
/// Handles both concrete->interface and interface->interface
/// Used for both assignment statements and function call arguments
pub fn compile_iface_assign(
    dst_slot: u16,
    rhs: &vo_syntax::ast::Expr,
    iface_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let src_type = info.expr_type(rhs.id);
    let src_vk = info.type_value_kind(src_type);
    let iface_meta_id = ctx.get_or_create_interface_meta_id(iface_type, &info.project.tc_objs);
    
    
    let const_idx = if src_vk == vo_common_core::ValueKind::Interface {
        ctx.register_iface_assign_const_interface(iface_meta_id)
    } else if src_vk == vo_common_core::ValueKind::Void {
        // nil interface: rttid=0, itab_id=0, so packed=0
        ctx.const_int(0)
    } else {
        // Use RuntimeType for structural equality in rttid lookup
        let rt = crate::type_key_to_runtime_type_simple(src_type, info, &info.project.interner, ctx);
        let rttid = ctx.intern_rttid(rt);
        let named_type_id = ctx.get_named_type_id(src_type);
        ctx.register_iface_assign_const_concrete(rttid, named_type_id, iface_meta_id)
    };
    
    let is_value_type = src_vk == vo_common_core::ValueKind::Struct || src_vk == vo_common_core::ValueKind::Array;
    
    if is_value_type {
        let src_slots = info.type_slot_count(src_type);
        let src_slot_types = info.type_slot_types(src_type);
        let meta_idx = ctx.get_or_create_value_meta(Some(src_type), src_slots, &src_slot_types);
        
        let tmp_data = func.alloc_temp(src_slots);
        compile_expr_to(rhs, tmp_data, ctx, func, info)?;
        
        let gcref_slot = func.alloc_temp(1);
        let meta_reg = func.alloc_temp(1);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        func.emit_with_flags(Opcode::PtrNew, src_slots as u8, gcref_slot, meta_reg, 0);
        func.emit_ptr_set(gcref_slot, 0, tmp_data, src_slots);
        
        func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst_slot, gcref_slot, const_idx);
    } else {
        let src_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
        func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst_slot, src_reg, const_idx);
    }
    Ok(())
}

/// Initialize an escaped (heap-allocated) array from a composite literal.
fn compile_escaped_array_init(
    arr_slot: u16,
    value: &vo_syntax::ast::Expr,
    _array_type: vo_analysis::objects::TypeKey,
    elem_slots: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::ExprKind;
    
    if let ExprKind::CompositeLit(lit) = &value.kind {
        // Array literal: [N]T{e1, e2, ...}
        for (i, elem) in lit.elems.iter().enumerate() {
            // Compile element value to temp
            let val_reg = crate::expr::compile_expr(&elem.value, ctx, func, info)?;
            
            // Load index
            let idx_reg = func.alloc_temp(1);
            func.emit_op(Opcode::LoadInt, idx_reg, i as u16, 0);
            
            // ArraySet: a=arr, b=idx, c=val, flags=elem_slots
            func.emit_with_flags(Opcode::ArraySet, elem_slots as u8, arr_slot, idx_reg, val_reg);
        }
    } else {
        // Non-literal initialization: compile entire value and copy element by element
        // This handles cases like: var a [3]int = someOtherArray
        let src_reg = crate::expr::compile_expr(value, ctx, func, info)?;
        let arr_len = info.array_len(info.expr_type(value.id));
        
        for i in 0..arr_len as usize {
            // Load index
            let idx_reg = func.alloc_temp(1);
            func.emit_op(Opcode::LoadInt, idx_reg, i as u16, 0);
            
            // ArraySet from src position
            let src_offset = src_reg + (i as u16) * elem_slots;
            func.emit_with_flags(Opcode::ArraySet, elem_slots as u8, arr_slot, idx_reg, src_offset);
        }
    }
    
    Ok(())
}

