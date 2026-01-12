//! Statement compilation.

use vo_analysis::objects::{ObjKey, TypeKey};
use vo_common::symbol::Symbol;
use vo_runtime::SlotType;
use vo_syntax::ast::{Block, Expr, Stmt, StmtKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::expr::{compile_expr_to, emit_int_trunc, get_expr_source};
use crate::func::{ExprSource, FuncBuilder, StorageKind};
use crate::type_info::{encode_i32, TypeInfoWrapper};

/// IfaceAssert flags for protocol dispatch: has_ok=1, dst_slots=2, src_slots=2
const IFACE_ASSERT_WITH_OK: u8 = 1 | (1 << 2) | (2 << 3);

/// All protocol interfaces have exactly one method at index 0
const PROTOCOL_METHOD_IDX: u8 = 0;

/// Emit error propagation and return: fill return slots with nil, copy error, and return.
/// `error_src` is the register containing the error (2 slots).
fn emit_error_propagate_return(
    error_src: u16,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) {
    let ret_types: Vec<_> = func.return_types().to_vec();
    let mut total_ret_slots = 0u16;
    let mut ret_slot_types = Vec::new();
    for ret_type in &ret_types {
        total_ret_slots += info.type_slot_count(*ret_type);
        ret_slot_types.extend(info.type_slot_types(*ret_type));
    }
    let ret_start = func.alloc_temp_typed(&ret_slot_types);
    for i in 0..total_ret_slots {
        func.emit_op(Opcode::LoadInt, ret_start + i, 0, 0);
    }
    if !ret_types.is_empty() {
        let error_type = *ret_types.last().unwrap();
        let error_slots = info.type_slot_count(error_type);
        let error_start = ret_start + total_ret_slots - error_slots;
        func.emit_copy(error_start, error_src, error_slots);
    }
    func.emit_with_flags(Opcode::Return, 1, ret_start, total_ret_slots, 0);
}

/// Emit error short-circuit for (any, error) tuple base in dynamic assignment.
/// If base has an error (slot+2 != nil), fills return slots with nil and propagates error.
fn emit_dyn_assign_error_short_circuit(
    base_reg: u16,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) {
    let ok_jump = func.emit_jump(Opcode::JumpIfNot, base_reg + 2);
    emit_error_propagate_return(base_reg + 2, func, info);
    func.patch_jump(ok_jump, func.current_pc());
}

/// Emit Return with heap_returns flag for escaped named returns.
/// VM will read values from heap GcRefs after defer execution.
fn emit_heap_returns(func: &mut FuncBuilder, named_return_slots: &[(u16, u16, bool)]) {
    let gcref_count = named_return_slots.len() as u16;
    let gcref_start = named_return_slots[0].0;
    // value_slots_per_ref: assumes all named returns have same slot count
    // TODO: support mixed slot counts if needed
    let value_slots = named_return_slots[0].1;
    func.emit_with_flags(Opcode::Return, 0x02, gcref_start, gcref_count, value_slots);
}

/// Compute IfaceAssert parameters for a target type.
/// Returns (assert_kind, target_id) where:
/// - assert_kind: 0 for concrete types (rttid), 1 for interface types (iface_meta_id)
/// - target_id: the corresponding id for runtime type checking
fn compute_iface_assert_params(
    type_key: TypeKey,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> (u8, u32) {
    if info.is_interface(type_key) {
        let iface_meta_id = info.get_or_create_interface_meta_id(type_key, ctx);
        (1, iface_meta_id)
    } else {
        let rt = info.type_to_runtime_type(type_key, ctx);
        let rttid = ctx.intern_rttid(rt);
        (0, rttid)
    }
}

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
    /// 
    /// IMPORTANT: For shadowing cases like `i := i`, we must compile the init
    /// expression BEFORE registering the new variable, so the RHS references
    /// the outer variable, not the new (uninitialized) one.
    pub fn define_local(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        escapes: bool,
        init: Option<&Expr>,
        obj_key: Option<ObjKey>,
    ) -> Result<StorageKind, CodegenError> {
        if let Some(expr) = init {
            // Compile init expression FIRST, before registering variable.
            // This ensures shadowing like `i := i` references the outer `i`.
            let slot_types = self.info.type_slot_types(type_key);
            let tmp = self.func.alloc_temp_typed(&slot_types);
            self.compile_value(expr, tmp, type_key)?;
            
            // Now allocate storage and register the variable name
            let storage = self.alloc_storage(sym, type_key, escapes, obj_key)?;
            
            // Copy from temp to the new storage
            self.store_from_slot(storage, tmp, &self.info.type_slot_types(type_key));
            Ok(storage)
        } else {
            let storage = self.alloc_storage(sym, type_key, escapes, obj_key)?;
            self.emit_zero_init(storage, type_key);
            Ok(storage)
        }
    }

    /// Allocate storage for a variable based on type and escape analysis.
    /// This is the single decision point for storage strategy.
    /// 
    /// `obj_key`: Used to check if variable is captured by closure (for reference type boxing decision)
    fn alloc_storage(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        escapes: bool,
        obj_key: Option<ObjKey>,
    ) -> Result<StorageKind, CodegenError> {
        let slots = self.info.type_slot_count(type_key);
        let slot_types = self.info.type_slot_types(type_key);

        // Use centralized boxing decision logic
        let needs_box = obj_key.map_or(escapes, |k| self.info.needs_boxing(k, type_key));

        if self.info.is_reference_type(type_key) {
            if needs_box {
                // Reference type captured by closure: box to share storage
                self.alloc_escaped_boxed(sym, type_key, slots, &slot_types)
            } else {
                // Reference type not captured: 1 slot GcRef IS the value
                let slot = self.func.define_local_reference(sym);
                Ok(StorageKind::Reference { slot })
            }
        } else if needs_box {
            // Non-reference types that escape need boxing
            if self.info.is_array(type_key) {
                self.alloc_escaped_array(sym, type_key)
            } else {
                self.alloc_escaped_boxed(sym, type_key, slots, &slot_types)
            }
        } else if self.info.is_array(type_key) {
            // Stack array: memory semantics, accessed via SlotGet/SlotSet
            let elem_slots = self.info.array_elem_slots(type_key);
            let len = self.info.array_len(type_key) as u16;
            let base_slot = self.func.define_local_stack_array(sym, slots, elem_slots, len, &slot_types);
            Ok(StorageKind::StackArray { base_slot, elem_slots, len })
        } else {
            // Stack value (struct/primitive): register semantics
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
        let elem_bytes = self.info.array_elem_bytes(type_key);
        let elem_type = self.info.array_elem_type(type_key);
        let elem_vk = self.info.type_value_kind(elem_type);
        let gcref_slot = self.func.define_local_heap_array(sym, elem_slots, elem_bytes as u16, elem_vk);

        let arr_len = self.info.array_len(type_key);
        let elem_meta_idx = self.ctx.get_or_create_array_elem_meta(type_key, self.info);

        // emit ArrayNew: a=dst, b=elem_meta_idx, c=len, flags=elem_flags
        let meta_reg = self.func.alloc_temp_typed(&[SlotType::Value]);
        self.func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);

        let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
        // When flags=0 (dynamic), put len and elem_bytes in consecutive registers
        let num_regs = if flags == 0 { 2 } else { 1 };
        let len_reg = self.func.alloc_temp_typed(&vec![SlotType::Value; num_regs]);
        let (b, c) = encode_i32(arr_len as i32);
        self.func.emit_op(Opcode::LoadInt, len_reg, b, c);
        if flags == 0 {
            let eb_idx = self.ctx.const_int(elem_bytes as i64);
            self.func.emit_op(Opcode::LoadConst, len_reg + 1, eb_idx, 0);
        }
        self.func.emit_with_flags(Opcode::ArrayNew, flags, gcref_slot, meta_reg, len_reg);

        Ok(StorageKind::HeapArray { gcref_slot, elem_slots, elem_bytes: elem_bytes as u16, elem_vk })
    }

    /// Allocate escaped boxed value (struct/primitive/interface): [GcHeader][data]
    fn alloc_escaped_boxed(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        slots: u16,
        slot_types: &[vo_runtime::SlotType],
    ) -> Result<StorageKind, CodegenError> {
        let gcref_slot = self.func.define_local_heap_boxed(sym, slots);

        let rttid = self.ctx.intern_type_key(type_key, self.info);
        let meta_idx = self.ctx.get_or_create_value_meta_with_rttid(rttid, slot_types, None);
        let meta_reg = self.func.alloc_temp_typed(&[SlotType::Value]);
        self.func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        self.func.emit_with_flags(Opcode::PtrNew, slots as u8, gcref_slot, meta_reg, 0);

        Ok(StorageKind::HeapBoxed { gcref_slot, value_slots: slots })
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
            StorageKind::StackArray { .. } => {
                // Stack arrays rely on JIT/VM prologue zero-initialization
                // No codegen needed - locals_slot is zeroed before function execution
            }
            StorageKind::StackValue { slot, slots } => {
                for i in 0..slots {
                    self.func.emit_op(Opcode::LoadInt, slot + i, 0, 0);
                }
            }
            StorageKind::Reference { slot } => {
                self.func.emit_op(Opcode::LoadInt, slot, 0, 0);
            }
            StorageKind::Global { .. } => {
                unreachable!("define_local doesn't create Global storage")
            }
        }
    }

    /// Define a local variable and initialize from an already-compiled slot.
    /// Used for comma-ok cases where the value is already in a temp slot.
    /// 
    /// This is unified: alloc_storage + store_from_slot
    pub fn define_local_from_slot(
        &mut self,
        sym: Symbol,
        type_key: TypeKey,
        escapes: bool,
        src_slot: u16,
        obj_key: Option<ObjKey>,
    ) -> Result<StorageKind, CodegenError> {
        let slot_types = self.info.type_slot_types(type_key);
        let storage = self.alloc_storage(sym, type_key, escapes, obj_key)?;
        self.store_from_slot(storage, src_slot, &slot_types);
        Ok(storage)
    }

    /// Store a value from an already-compiled slot to an existing storage.
    /// This copies the VALUE CONTENT, not just a reference.
    /// For HeapArray, this copies elements one by one (initialization semantic).
    /// For variable reassignment semantic (copying GcRef), use func.emit_storage_store.
    pub fn store_from_slot(&mut self, storage: StorageKind, src_slot: u16, slot_types: &[vo_runtime::SlotType]) {
        match storage {
            // HeapArray needs special handling: copy elements, not GcRef
            StorageKind::HeapArray { gcref_slot, elem_slots, elem_bytes, elem_vk } => {
                let arr_len = slot_types.len() as u16 / elem_slots;
                for i in 0..arr_len {
                    let idx_reg = self.func.alloc_temp_typed(&[SlotType::Value]);
                    self.func.emit_op(Opcode::LoadInt, idx_reg, i, 0);
                    let src_offset = src_slot + i * elem_slots;
                    self.func.emit_array_set(gcref_slot, idx_reg, src_offset, elem_bytes as usize, elem_vk, self.ctx);
                }
            }
            // All other cases delegate to emit_storage_store
            _ => {
                self.func.emit_storage_store(storage, src_slot, slot_types);
            }
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
    if info.is_interface(target_type) {
        compile_iface_assign(dst, expr, target_type, ctx, func, info)
    } else {
        compile_expr_to(expr, dst, ctx, func, info)?;
        // Apply truncation for narrow integer types (Go semantics)
        emit_int_trunc(dst, target_type, func, info);
        Ok(())
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
    /// Begin: __idx := 0, HINT_LOOP_BEGIN, loop: if __idx >= __len { goto end }
    fn begin(func: &mut FuncBuilder, len_slot: u16, label: Option<vo_common::Symbol>) -> Self {
        let idx_slot = func.alloc_temp_typed(&[SlotType::Value]);
        func.emit_op(Opcode::LoadInt, idx_slot, 0, 0);
        
        let loop_start = func.current_pc();
        func.enter_loop(loop_start, label);
        
        let cmp_slot = func.alloc_temp_typed(&[SlotType::Value]);
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
    
    /// End: __idx++, HINT_LOOP_END, goto loop, patch breaks/continues, finalize HINT_LOOP_BEGIN
    fn end(self, func: &mut FuncBuilder) {
        let post_pc = func.current_pc();
        let one = func.alloc_temp_typed(&[SlotType::Value]);
        func.emit_op(Opcode::LoadInt, one, 1, 0);
        func.emit_op(Opcode::AddI, self.idx_slot, self.idx_slot, one);
        
        // exit_loop emits HINT_LOOP_END and patches flags
        let exit_info = func.exit_loop();
        
        func.emit_jump_to(Opcode::Jump, 0, self.loop_start);
        
        let exit_pc = func.current_pc();
        func.patch_jump(self.end_jump, exit_pc);
        
        // Finalize HINT_LOOP_BEGIN with correct exit_pc
        func.finalize_loop_hint(exit_info.begin_pc, exit_pc);
        
        for pc in exit_info.break_patches { func.patch_jump(pc, exit_pc); }
        for pc in exit_info.continue_patches { func.patch_jump(pc, post_pc); }
    }
}

/// Define or lookup a range variable (key or value) using StmtCompiler.
/// - If `define` is true: declare new variable with proper escape handling
/// - If `define` is false: lookup existing variable
/// - Blank identifier `_` always gets a temp slot (never defined or looked up)
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
                // Blank identifier `_` - allocate temp slot, never define or lookup
                let is_blank = sc.info.project.interner.resolve(ident.symbol) == Some("_");
                if is_blank {
                    let slot_types = sc.info.type_slot_types(fallback_type);
                    return Ok(sc.func.alloc_temp_typed(&slot_types));
                }
                
                if define {
                    let obj_key = sc.info.get_def(ident);
                    let type_key = sc.info.obj_type(obj_key, "range var must have type");
                    let escapes = sc.info.is_escaped(obj_key);
                    let storage = sc.define_local(ident.symbol, type_key, escapes, None, Some(obj_key))?;
                    Ok(storage.slot())
                } else {
                    Ok(sc.func.lookup_local(ident.symbol)
                        .expect("range variable not found")
                        .storage.slot())
                }
            } else if define {
                let slot_types = sc.info.type_slot_types(fallback_type);
                Ok(sc.func.alloc_temp_typed(&slot_types))
            } else {
                crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)
            }
        }
        None => {
            let slot_types = sc.info.type_slot_types(fallback_type);
            Ok(sc.func.alloc_temp_typed(&slot_types))
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

                    sc.define_local(name.symbol, type_key, escapes, init, Some(obj_key))?;
                }
            }
        }

        // === Short variable declaration ===
        StmtKind::ShortVar(short_var) => {
            let is_blank = |name: &vo_syntax::ast::Ident| {
                info.project.interner.resolve(name.symbol) == Some("_")
            };
            
            // Check for multi-value case: v1, v2, ... := f() where f() returns a tuple
            let is_multi_value = short_var.values.len() == 1 
                && short_var.names.len() >= 2
                && info.is_tuple(info.expr_type(short_var.values[0].id));

            if is_multi_value {
                // Multi-value: compile expr once, then distribute to variables
                let tuple_type = info.expr_type(short_var.values[0].id);
                let tuple_slot_types = info.type_slot_types(tuple_type);
                let tmp_base = func.alloc_temp_typed(&tuple_slot_types);
                compile_expr_to(&short_var.values[0], tmp_base, ctx, func, info)?;

                let mut sc = StmtCompiler::new(ctx, func, info);
                let mut offset = 0u16;
                for (i, name) in short_var.names.iter().enumerate() {
                    let elem_type = info.tuple_elem_type(tuple_type, i);
                    let elem_slots = info.type_slot_count(elem_type);

                    if is_blank(name) {
                        offset += elem_slots;
                        continue;
                    }

                    // Apply truncation for narrow integer types (Go semantics)
                    emit_int_trunc(tmp_base + offset, elem_type, sc.func, info);

                    if info.is_def(name) {
                        let obj_key = info.get_def(name);
                        let escapes = info.is_escaped(obj_key);
                        sc.define_local_from_slot(name.symbol, elem_type, escapes, tmp_base + offset, Some(obj_key))?;
                    } else if let Some(local) = sc.func.lookup_local(name.symbol) {
                        let elem_slot_types = info.type_slot_types(elem_type);
                        sc.store_from_slot(local.storage, tmp_base + offset, &elem_slot_types);
                    }
                    offset += elem_slots;
                }
            } else {
                // Normal case: N variables = N expressions
                // Go spec: RHS evaluated first, then assigned (handles p, q := p+1, p+2)
                
                // Phase 1: Evaluate all RHS to temps
                let mut rhs_temps: Vec<Option<(u16, vo_analysis::objects::TypeKey)>> = Vec::new();
                for (i, name) in short_var.names.iter().enumerate() {
                    let expr = &short_var.values[i];
                    if is_blank(name) {
                        // Evaluate for side effects only
                        let _ = crate::expr::compile_expr(expr, ctx, func, info)?;
                        rhs_temps.push(None);
                    } else {
                        let type_key = info.expr_type(expr.id);
                        let slot_types = info.type_slot_types(type_key);
                        let tmp = func.alloc_temp_typed(&slot_types);
                        compile_expr_to(expr, tmp, ctx, func, info)?;
                        // Apply truncation for narrow integer types (Go semantics)
                        emit_int_trunc(tmp, type_key, func, info);
                        rhs_temps.push(Some((tmp, type_key)));
                    }
                }
                
                // Phase 2: Assign temps to LHS
                let mut sc = StmtCompiler::new(ctx, func, info);
                for (i, name) in short_var.names.iter().enumerate() {
                    let Some((tmp, type_key)) = rhs_temps[i] else { continue };
                    
                    if info.is_def(name) {
                        let obj_key = info.get_def(name);
                        let escapes = info.is_escaped(obj_key);
                        sc.define_local_from_slot(name.symbol, type_key, escapes, tmp, Some(obj_key))?;
                    } else if let Some(local) = sc.func.lookup_local(name.symbol) {
                        let slot_types = sc.info.type_slot_types(type_key);
                        sc.func.emit_storage_store(local.storage, tmp, &slot_types);
                    }
                }
            }
        }

        // === Assignment ===
        StmtKind::Assign(assign) => {
            use vo_syntax::ast::AssignOp;

            if assign.op == AssignOp::Assign && assign.lhs.len() == 1 && assign.rhs.len() == 1 {
                if let vo_syntax::ast::ExprKind::DynAccess(dyn_access) = &assign.lhs[0].kind {
                    match &dyn_access.op {
                        vo_syntax::ast::DynAccessOp::Field(ident) => {
                            let base_type = info.expr_type(dyn_access.base.id);
                            let any_type = info.any_type();
                            
                            // Get base as any (2 slots)
                            // - interface/any: compile directly
                            // - (any, error) tuple: compile and short-circuit on error
                            // - other types: box to any
                            let any_base_reg = if info.is_tuple_any_error(base_type) {
                                // (any, error) tuple: 4 slots with interface types
                                let base_reg = func.alloc_temp_typed(&[
                                    SlotType::Interface0, SlotType::Interface1,  // any
                                    SlotType::Interface0, SlotType::Interface1,  // error
                                ]);
                                compile_expr_to(&dyn_access.base, base_reg, ctx, func, info)?;
                                emit_dyn_assign_error_short_circuit(base_reg, func, info);
                                base_reg  // first 2 slots are the any value
                            } else if info.is_interface(base_type) {
                                let base_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
                                compile_expr_to(&dyn_access.base, base_reg, ctx, func, info)?;
                                base_reg
                            } else {
                                let any_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
                                compile_iface_assign(any_reg, &dyn_access.base, any_type, ctx, func, info)?;
                                any_reg
                            };

                            let field_name = info.project.interner.resolve(ident.symbol)
                                .ok_or_else(|| CodegenError::Internal("cannot resolve field name".to_string()))?;

                            // Protocol-first: check if base implements SetAttrObject via IfaceAssert
                            // Use builtin protocol meta_id (no dependency on user imports)
                            let end_jump = if let Some(set_attr_iface_meta_id) = ctx.builtin_protocols().set_attr_object_meta_id {
                                // IfaceAssert with has_ok flag to check interface implementation
                                let iface_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1, SlotType::Value]);
                                func.emit_with_flags(Opcode::IfaceAssert, IFACE_ASSERT_WITH_OK, iface_reg, any_base_reg, set_attr_iface_meta_id as u16);
                                let fallback_jump = func.emit_jump(Opcode::JumpIfNot, iface_reg + 2);

                                // Protocol method call: DynSetAttr(name string, value any) error
                                let args_start = func.alloc_temp_typed(&[
                                    SlotType::GcRef,  // string
                                    SlotType::Interface0, SlotType::Interface1,  // any value
                                ]);
                                let name_idx = ctx.const_string(field_name);
                                func.emit_op(Opcode::StrNew, args_start, name_idx, 0);
                                compile_iface_assign(args_start + 1, &assign.rhs[0], any_type, ctx, func, info)?;

                                let c = crate::type_info::encode_call_args(3, 2);
                                func.emit_with_flags(Opcode::CallIface, PROTOCOL_METHOD_IDX, iface_reg, args_start, c);

                                // Check error from protocol method
                                let ok_err_jump = func.emit_jump(Opcode::JumpIfNot, args_start);
                                emit_error_propagate_return(args_start, func, info);
                                func.patch_jump(ok_err_jump, func.current_pc());
                                let end_jump = func.emit_jump(Opcode::Jump, 0);

                                func.patch_jump(fallback_jump, func.current_pc());
                                Some(end_jump)
                            } else {
                                None
                            };

                            // Reflection path: extern dyn_set_attr for types not implementing SetAttrObject
                            let args_start = func.alloc_temp_typed(&[
                                SlotType::Interface0, SlotType::Interface1,  // base any
                                SlotType::GcRef,  // name string
                                SlotType::Interface0, SlotType::Interface1,  // value any
                            ]);
                            func.emit_copy(args_start, any_base_reg, 2);
                            let name_idx = ctx.const_string(field_name);
                            func.emit_op(Opcode::StrNew, args_start + 2, name_idx, 0);
                            compile_iface_assign(args_start + 3, &assign.rhs[0], any_type, ctx, func, info)?;
                            let extern_id = ctx.get_or_register_extern("dyn_set_attr");
                            let err_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
                            func.emit_with_flags(Opcode::CallExtern, 5, err_reg, extern_id as u16, args_start);

                            let done_jump = func.emit_jump(Opcode::JumpIfNot, err_reg);
                            emit_error_propagate_return(err_reg, func, info);
                            func.patch_jump(done_jump, func.current_pc());
                            
                            if let Some(end_jump) = end_jump {
                                func.patch_jump(end_jump, func.current_pc());
                            }
                            return Ok(());
                        }
                        vo_syntax::ast::DynAccessOp::Index(key_expr) => {
                            let base_type = info.expr_type(dyn_access.base.id);
                            let any_type = info.any_type();
                            
                            // Get base as any (2 slots)
                            // - (any, error) tuple: compile and short-circuit on error
                            // - interface/any: compile directly
                            // - other types: box to any
                            let any_base_reg = if info.is_tuple_any_error(base_type) {
                                let base_reg = func.alloc_temp_typed(&[
                                    SlotType::Interface0, SlotType::Interface1,
                                    SlotType::Interface0, SlotType::Interface1,
                                ]);
                                compile_expr_to(&dyn_access.base, base_reg, ctx, func, info)?;
                                emit_dyn_assign_error_short_circuit(base_reg, func, info);
                                base_reg
                            } else if info.is_interface(base_type) {
                                let base_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
                                compile_expr_to(&dyn_access.base, base_reg, ctx, func, info)?;
                                base_reg
                            } else {
                                let any_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
                                compile_iface_assign(any_reg, &dyn_access.base, any_type, ctx, func, info)?;
                                any_reg
                            };

                            // Protocol-first: check if base implements SetIndexObject via IfaceAssert
                            // Use builtin protocol meta_id (no dependency on user imports)
                            let end_jump = if let Some(set_index_iface_meta_id) = ctx.builtin_protocols().set_index_object_meta_id {
                                // IfaceAssert with has_ok flag to check interface implementation
                                let iface_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1, SlotType::Value]);
                                func.emit_with_flags(Opcode::IfaceAssert, IFACE_ASSERT_WITH_OK, iface_reg, any_base_reg, set_index_iface_meta_id as u16);
                                let fallback_jump = func.emit_jump(Opcode::JumpIfNot, iface_reg + 2);

                                // Protocol method call: DynSetIndex(key any, value any) error
                                let args_start = func.alloc_temp_typed(&[
                                    SlotType::Interface0, SlotType::Interface1,
                                    SlotType::Interface0, SlotType::Interface1,
                                ]);
                                compile_iface_assign(args_start, key_expr, any_type, ctx, func, info)?;
                                compile_iface_assign(args_start + 2, &assign.rhs[0], any_type, ctx, func, info)?;

                                let c = crate::type_info::encode_call_args(4, 2);
                                func.emit_with_flags(Opcode::CallIface, PROTOCOL_METHOD_IDX, iface_reg, args_start, c);

                                // Check error from protocol method
                                let ok_err_jump = func.emit_jump(Opcode::JumpIfNot, args_start);
                                emit_error_propagate_return(args_start, func, info);
                                func.patch_jump(ok_err_jump, func.current_pc());
                                let end_jump = func.emit_jump(Opcode::Jump, 0);

                                func.patch_jump(fallback_jump, func.current_pc());
                                Some(end_jump)
                            } else {
                                None
                            };

                            // Reflection path: extern dyn_set_index for types not implementing SetIndexObject
                            let args_start = func.alloc_temp_typed(&[
                                SlotType::Interface0, SlotType::Interface1,  // base
                                SlotType::Interface0, SlotType::Interface1,  // key
                                SlotType::Interface0, SlotType::Interface1,  // value
                            ]);
                            func.emit_copy(args_start, any_base_reg, 2);
                            compile_iface_assign(args_start + 2, key_expr, any_type, ctx, func, info)?;
                            compile_iface_assign(args_start + 4, &assign.rhs[0], any_type, ctx, func, info)?;
                            let extern_id = ctx.get_or_register_extern("dyn_set_index");
                            let err_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
                            func.emit_with_flags(Opcode::CallExtern, 6, err_reg, extern_id as u16, args_start);

                            let done_jump = func.emit_jump(Opcode::JumpIfNot, err_reg);
                            emit_error_propagate_return(err_reg, func, info);
                            func.patch_jump(done_jump, func.current_pc());
                            
                            if let Some(end_jump) = end_jump {
                                func.patch_jump(end_jump, func.current_pc());
                            }
                            return Ok(());
                        }
                        _ => {}
                    }
                }
            }
            
            // Check for multi-value case: v1, v2, ... = f() where f() returns a tuple
            // This includes comma-ok (2 values) and multi-return functions (3+ values)
            let is_multi_value = assign.op == AssignOp::Assign
                && assign.rhs.len() == 1
                && assign.lhs.len() >= 2
                && info.is_tuple(info.expr_type(assign.rhs[0].id));
            
            if is_multi_value {
                // Multi-value assignment: compile expr once, then distribute to variables
                let tuple_type = info.expr_type(assign.rhs[0].id);
                let tuple_slot_types = info.type_slot_types(tuple_type);
                let tmp_base = func.alloc_temp_typed(&tuple_slot_types);
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
                    
                    // Apply truncation for narrow integer types (Go semantics)
                    emit_int_trunc(tmp_base + offset, elem_type, func, info);
                    
                    // Get lhs location and copy from temp
                    let lhs_source = crate::expr::get_expr_source(lhs_expr, ctx, func, info);
                    if let ExprSource::Location(storage) = lhs_source {
                        let slot_types = info.type_slot_types(elem_type);
                        func.emit_storage_store(storage, tmp_base + offset, &slot_types);
                    }
                    offset += elem_slots;
                }
            } else if assign.op == AssignOp::Assign && assign.lhs.len() > 1 {
                // Parallel assignment: a, b = b, a
                // Must evaluate all RHS first, then assign to LHS to avoid interference
                use crate::lvalue::{resolve_lvalue, emit_lvalue_store};
                
                // 1. Evaluate all RHS to temporaries
                let mut rhs_temps = Vec::with_capacity(assign.rhs.len());
                for rhs in &assign.rhs {
                    let rhs_type = info.expr_type(rhs.id);
                    let rhs_slots = info.expr_slots(rhs.id);
                    let rhs_slot_types = info.type_slot_types(rhs_type);
                    let tmp = func.alloc_temp_typed(&rhs_slot_types);
                    compile_expr_to(rhs, tmp, ctx, func, info)?;
                    rhs_temps.push((tmp, rhs_slots, rhs_type));
                }
                
                // 2. Assign temporaries to LHS using LValue system
                for (lhs, (tmp, _slots, _rhs_type)) in assign.lhs.iter().zip(rhs_temps.iter()) {
                    // Skip blank identifier
                    if let vo_syntax::ast::ExprKind::Ident(ident) = &lhs.kind {
                        if info.project.interner.resolve(ident.symbol) == Some("_") {
                            continue;
                        }
                    }
                    
                    let lhs_type = info.expr_type(lhs.id);
                    // Apply truncation for narrow integer types (Go semantics)
                    emit_int_trunc(*tmp, lhs_type, func, info);
                    let lv = resolve_lvalue(lhs, ctx, func, info)?;
                    let slot_types = info.type_slot_types(lhs_type);
                    emit_lvalue_store(&lv, *tmp, ctx, func, &slot_types);
                }
            } else {
                // Single assignment or compound assignment
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
                // Bare return - use pre-recorded slot info (not affected by shadow variables)
                let named_return_slots: Vec<_> = func.named_return_slots().to_vec();
                if named_return_slots.is_empty() {
                    func.emit_op(Opcode::Return, 0, 0, 0);
                } else {
                    // Check if ALL named returns are escaped (for defer named return semantics)
                    // When all are escaped, we pass GcRefs and let VM read after defer
                    let all_escaped = named_return_slots.iter().all(|(_, _, escaped)| *escaped);
                    
                    if all_escaped {
                        emit_heap_returns(func, &named_return_slots);
                    } else {
                        // Mixed or non-escaped: copy to return area as before
                        let total_ret_slots: u16 = named_return_slots.iter().map(|(_, s, _)| *s).sum();
                        // Build slot types from named return variables
                        let mut ret_slot_types = Vec::new();
                        for (_, slots, _) in &named_return_slots {
                            for _ in 0..*slots {
                                ret_slot_types.push(SlotType::Value);  // Conservative: named returns are already properly typed at definition
                            }
                        }
                        let ret_start = func.alloc_temp_typed(&ret_slot_types);
                        let mut offset = 0u16;
                        for &(slot, slots, escaped) in &named_return_slots {
                            if escaped {
                                // Escaped variable: slot is GcRef, need PtrGet to read value
                                if slots == 1 {
                                    func.emit_op(Opcode::PtrGet, ret_start + offset, slot, 0);
                                } else {
                                    func.emit_with_flags(Opcode::PtrGetN, slots as u8, ret_start + offset, slot, 0);
                                }
                            } else {
                                func.emit_copy(ret_start + offset, slot, slots);
                            }
                            offset += slots;
                        }
                        func.emit_op(Opcode::Return, ret_start, total_ret_slots, 0);
                    }
                }
            } else {
                // Check if we have escaped named returns - if so, we need special handling
                // to ensure defer can modify the return values
                let named_return_slots: Vec<_> = func.named_return_slots().to_vec();
                let all_escaped = !named_return_slots.is_empty() 
                    && named_return_slots.iter().all(|(_, _, escaped)| *escaped);
                
                if all_escaped {
                    // For escaped named returns with explicit return values:
                    // 1. Store return values into the heap-allocated named return variables
                    // 2. Use heap_returns mode so VM reads from heap after defer
                    let ret_types: Vec<_> = func.return_types().to_vec();
                    
                    // Store each return value into the corresponding named return variable
                    for (i, result) in ret.values.iter().enumerate() {
                        let (gcref_slot, slots, _) = named_return_slots[i];
                        let ret_type = ret_types.get(i).copied();
                        
                        // Compile value to temp, then store to heap
                        let temp_slot_types = ret_type.map(|rt| info.type_slot_types(rt)).unwrap_or_else(|| vec![SlotType::Value; slots as usize]);
                        let temp = func.alloc_temp_typed(&temp_slot_types);
                        if let Some(rt) = ret_type {
                            compile_value_to(result, temp, rt, ctx, func, info)?;
                        } else {
                            compile_expr_to(result, temp, ctx, func, info)?;
                        }
                        
                        // Store to heap: PtrSet gcref[0..slots] = temp
                        if slots == 1 {
                            func.emit_op(Opcode::PtrSet, gcref_slot, 0, temp);
                        } else {
                            func.emit_with_flags(Opcode::PtrSetN, slots as u8, gcref_slot, 0, temp);
                        }
                    }
                    
                    emit_heap_returns(func, &named_return_slots);
                } else {
                    // Get function's return types (clone to avoid borrow issues)
                    let ret_types: Vec<_> = func.return_types().to_vec();
                    
                    // Calculate total return slots needed (use declared return types)
                    let mut total_ret_slots = 0u16;
                    for ret_type in &ret_types {
                        total_ret_slots += info.type_slot_count(*ret_type);
                    }
                    
                    // Optimization: single return value that's already in a usable slot
                    let optimized = if ret.values.len() == 1 && ret_types.len() == 1 {
                        let result = &ret.values[0];
                        let ret_type = ret_types[0];
                        let expr_type = info.expr_type(result.id);
                        
                        // Only optimize if types match (no interface conversion needed)
                        if expr_type == ret_type {
                            if let ExprSource::Location(StorageKind::StackValue { slot, slots }) = 
                                get_expr_source(result, ctx, func, info) 
                            {
                                // Direct return from existing slot
                                func.emit_op(Opcode::Return, slot, slots, 0);
                                true
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    } else {
                        false
                    };
                    
                    if !optimized {
                        // Standard path: allocate space and compile return values
                        let mut ret_slot_types = Vec::new();
                        for ret_type in &ret_types {
                            ret_slot_types.extend(info.type_slot_types(*ret_type));
                        }
                        let ret_start = func.alloc_temp_typed(&ret_slot_types);
                        
                        // Check for multi-value case: return f() where f() returns a tuple
                        let is_multi_value = ret.values.len() == 1 
                            && ret_types.len() >= 2
                            && info.is_tuple(info.expr_type(ret.values[0].id));
                        
                        if is_multi_value {
                            // Multi-value: compile expr once to get all return values
                            compile_expr_to(&ret.values[0], ret_start, ctx, func, info)?;
                        } else {
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
                        }
                        func.emit_op(Opcode::Return, ret_start, total_ret_slots, 0);
                    }
                }
            }
        }

        // === If statement ===
        StmtKind::If(if_stmt) => {
            // Enter scope for init variable shadowing (Go semantics: if x := 1; x > 0 { } creates new scope)
            func.enter_scope();
            
            // Init statement
            if let Some(init) = &if_stmt.init {
                compile_stmt(init, ctx, func, info)?;
            }

            // Optimize: if x == nil / if x != nil → direct JumpIf/JumpIfNot
            let else_jump = if let vo_syntax::ast::ExprKind::Binary(bin) = &if_stmt.cond.kind {
                if let Some((value_expr, is_eq)) = crate::expr::match_nil_comparison(bin, info) {
                    let val_reg = crate::expr::compile_expr(value_expr, ctx, func, info)?;
                    if is_eq {
                        // x == nil: skip then when x != 0, i.e., JumpIf
                        func.emit_jump(Opcode::JumpIf, val_reg)
                    } else {
                        // x != nil: skip then when x == 0, i.e., JumpIfNot
                        func.emit_jump(Opcode::JumpIfNot, val_reg)
                    }
                } else {
                    let cond_reg = crate::expr::compile_expr(&if_stmt.cond, ctx, func, info)?;
                    func.emit_jump(Opcode::JumpIfNot, cond_reg)
                }
            } else {
                let cond_reg = crate::expr::compile_expr(&if_stmt.cond, ctx, func, info)?;
                func.emit_jump(Opcode::JumpIfNot, cond_reg)
            };

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
            
            // Exit scope (restore any shadowed variables from init)
            func.exit_scope();
        }

        // === For statement ===
        StmtKind::For(for_stmt) => {
            use vo_syntax::ast::ForClause;

            match &for_stmt.clause {
                ForClause::Cond(cond_opt) => {
                    // while-style: for cond { } or infinite: for { }
                    let loop_start = func.current_pc();
                    let begin_pc = func.enter_loop(loop_start, label);

                    let end_jump = if let Some(cond) = cond_opt {
                        let cond_reg = crate::expr::compile_expr(cond, ctx, func, info)?;
                        Some(func.emit_jump(Opcode::JumpIfNot, cond_reg))
                    } else {
                        None
                    };

                    compile_block(&for_stmt.body, ctx, func, info)?;
                    
                    // exit_loop emits HINT_LOOP_END and patches flags
                    let exit_info = func.exit_loop();
                    
                    func.emit_jump_to(Opcode::Jump, 0, loop_start);

                    let exit_pc = func.current_pc();
                    if let Some(j) = end_jump {
                        func.patch_jump(j, exit_pc);
                    }
                    
                    // Finalize HINT_LOOP_BEGIN: exit_pc=0 for infinite loop, actual exit_pc otherwise
                    let hint_exit_pc = if cond_opt.is_some() { exit_pc } else { 0 };
                    func.finalize_loop_hint(begin_pc, hint_exit_pc);
                    
                    for pc in exit_info.break_patches {
                        func.patch_jump(pc, exit_pc);
                    }
                    
                    // Patch continue jumps to loop_start (re-evaluate condition)
                    for pc in exit_info.continue_patches {
                        func.patch_jump(pc, loop_start);
                    }
                }

                ForClause::Three { init, cond, post } => {
                    // C-style: for init; cond; post { }
                    // Enter scope for init variable shadowing (Go semantics: for i := 0; ... creates new scope)
                    func.enter_scope();
                    
                    if let Some(init) = init {
                        compile_stmt(init, ctx, func, info)?;
                    }

                    let loop_start = func.current_pc();

                    // continue_pc=0 means "patch later" - continue should go to post
                    let begin_pc = func.enter_loop(0, label);

                    let end_jump = if let Some(cond) = cond {
                        let cond_reg = crate::expr::compile_expr(cond, ctx, func, info)?;
                        Some(func.emit_jump(Opcode::JumpIfNot, cond_reg))
                    } else {
                        None
                    };

                    // Body runs in its own scope - use compile_block_no_scope + manual scope
                    // because post statement must run in outer scope (same as init/cond)
                    func.enter_scope();
                    compile_block_no_scope(&for_stmt.body, ctx, func, info)?;
                    func.exit_scope();

                    // Post statement runs in outer scope (same as init/cond)
                    let post_pc = func.current_pc();
                    if let Some(post) = post {
                        compile_stmt(post, ctx, func, info)?;
                    }

                    // exit_loop emits HINT_LOOP_END and patches flags
                    let exit_info = func.exit_loop();
                    
                    func.emit_jump_to(Opcode::Jump, 0, loop_start);

                    let exit_pc = func.current_pc();
                    if let Some(j) = end_jump {
                        func.patch_jump(j, exit_pc);
                    }

                    // Finalize HINT_LOOP_BEGIN with exit_pc
                    func.finalize_loop_hint(begin_pc, exit_pc);
                    
                    // Patch break jumps to after loop
                    for pc in exit_info.break_patches {
                        func.patch_jump(pc, exit_pc);
                    }
                    
                    // Patch continue jumps to post statement
                    for pc in exit_info.continue_patches {
                        func.patch_jump(pc, post_pc);
                    }
                    
                    // Exit scope (restore any shadowed variables from init)
                    func.exit_scope();
                }

                ForClause::Range { key, value, define, expr } => {
                    // All for-range loops expanded at compile time. No runtime iterator state.
                    // Enter scope for key/value variable shadowing (Go semantics: for k, v := range ... creates new scope)
                    func.enter_scope();
                    
                    let range_type = info.expr_type(expr.id);
                    let mut sc = StmtCompiler::new(ctx, func, info);
                    
                    if info.is_array(range_type) {
                        let es = info.array_elem_slots(range_type);
                        let eb = info.array_elem_bytes(range_type);
                        let et = info.array_elem_type(range_type);
                        let len = info.array_len(range_type) as i64;
                        let src = crate::expr::get_expr_source(expr, sc.ctx, sc.func, sc.info);
                        let (reg, stk, base) = match src {
                            ExprSource::Location(StorageKind::StackArray { base_slot, .. }) => (0, true, base_slot),
                            _ => (crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?, false, 0),
                        };
                        let evk = info.type_value_kind(et);
                        let (ks, vs) = (range_var_slot(&mut sc, key.as_ref(), et, *define)?,
                                        range_var_slot(&mut sc, value.as_ref(), et, *define)?);
                        let ls = sc.func.alloc_temp_typed(&[SlotType::Value]);
                        sc.func.emit_op(Opcode::LoadInt, ls, len as u16, (len >> 16) as u16);
                        let lp = IndexLoop::begin(sc.func, ls, label);
                        lp.emit_key(sc.func, key.as_ref().map(|_| ks));
                        if value.is_some() {
                            // Stack array uses elem_slots, heap array uses elem_bytes
                            if stk {
                                sc.func.emit_with_flags(Opcode::SlotGetN, es as u8, vs, base, lp.idx_slot);
                            } else {
                                sc.func.emit_array_get(vs, reg, lp.idx_slot, eb, evk, sc.ctx);
                            }
                        }
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        lp.end(sc.func);
                        
                    } else if info.is_slice(range_type) {
                        let eb = info.slice_elem_bytes(range_type);
                        let et = info.slice_elem_type(range_type);
                        let evk = info.type_value_kind(et);
                        let reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                        let (ks, vs) = (range_var_slot(&mut sc, key.as_ref(), et, *define)?,
                                        range_var_slot(&mut sc, value.as_ref(), et, *define)?);
                        let ls = sc.func.alloc_temp_typed(&[SlotType::Value]);
                        sc.func.emit_op(Opcode::SliceLen, ls, reg, 0);
                        let lp = IndexLoop::begin(sc.func, ls, label);
                        lp.emit_key(sc.func, key.as_ref().map(|_| ks));
                        if value.is_some() {
                            sc.func.emit_slice_get(vs, reg, lp.idx_slot, eb, evk, sc.ctx);
                        }
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        lp.end(sc.func);
                        
                    } else if info.is_string(range_type) {
                        // String: iterate by rune (variable width)
                        let reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                        let (ks, vs) = (range_var_slot(&mut sc, key.as_ref(), range_type, *define)?,
                                        range_var_slot(&mut sc, value.as_ref(), range_type, *define)?);
                        let (pos, len, cmp) = (sc.func.alloc_temp_typed(&[SlotType::Value]), sc.func.alloc_temp_typed(&[SlotType::Value]), sc.func.alloc_temp_typed(&[SlotType::Value]));
                        // StrDecodeRune writes (rune, width) to consecutive slots
                        let rune_width = sc.func.alloc_temp_typed(&[SlotType::Value, SlotType::Value]);
                        
                        sc.func.emit_op(Opcode::LoadInt, pos, 0, 0);
                        sc.func.emit_op(Opcode::StrLen, len, reg, 0);
                        
                        let loop_start = sc.func.current_pc();
                        let begin_pc = sc.func.enter_loop(loop_start, label);
                        sc.func.emit_op(Opcode::GeI, cmp, pos, len);
                        let end_jump = sc.func.emit_jump(Opcode::JumpIf, cmp);
                        
                        sc.func.emit_op(Opcode::StrDecodeRune, rune_width, reg, pos);
                        if key.is_some() { sc.func.emit_op(Opcode::Copy, ks, pos, 0); }
                        if value.is_some() { sc.func.emit_op(Opcode::Copy, vs, rune_width, 0); }
                        
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        
                        let post_pc = sc.func.current_pc();
                        sc.func.emit_op(Opcode::AddI, pos, pos, rune_width + 1);
                        
                        // exit_loop emits HINT_LOOP_END and patches flags
                        let exit_info = sc.func.exit_loop();
                        
                        sc.func.emit_jump_to(Opcode::Jump, 0, loop_start);
                        
                        let exit_pc = sc.func.current_pc();
                        sc.func.patch_jump(end_jump, exit_pc);
                        
                        // Finalize HINT_LOOP_BEGIN with exit_pc
                        sc.func.finalize_loop_hint(begin_pc, exit_pc);
                        
                        for pc in exit_info.break_patches { sc.func.patch_jump(pc, exit_pc); }
                        for pc in exit_info.continue_patches { sc.func.patch_jump(pc, post_pc); }
                        
                    } else if info.is_map(range_type) {
                        // Map iteration using stateful iterator
                        const MAP_ITER_SLOTS: usize = vo_runtime::objects::map::MAP_ITER_SLOTS;
                        
                        let map_reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                        let (kn, vn) = info.map_key_val_slots(range_type);
                        let (kt, vt) = info.map_key_val_types(range_type);
                        let (ks, vs) = (range_var_slot(&mut sc, key.as_ref(), kt, *define)?,
                                        range_var_slot(&mut sc, value.as_ref(), vt, *define)?);
                        
                        let iter_slot = sc.func.alloc_temp_typed(&[SlotType::Value; MAP_ITER_SLOTS]);
                        let ok_slot = sc.func.alloc_temp_typed(&[SlotType::Value]);
                        
                        // MapIterInit: a=iter_slot, b=map_reg
                        sc.func.emit_op(Opcode::MapIterInit, iter_slot, map_reg, 0);
                        
                        // loop:
                        let loop_start = sc.func.current_pc();
                        let begin_pc = sc.func.enter_loop(loop_start, label);
                        
                        // MapIterNext: a=key_slot, b=iter_slot, c=ok_slot, flags=kn|(vn<<4)
                        sc.func.emit_with_flags(Opcode::MapIterNext, (kn as u8) | ((vn as u8) << 4), ks, iter_slot, ok_slot);
                        
                        // if !ok { goto end }
                        let end_jump = sc.func.emit_jump(Opcode::JumpIfNot, ok_slot);
                        
                        // Copy value to vs if needed (value is written at ks + kn)
                        if value.is_some() && vs != ks + kn {
                            if vn == 1 { sc.func.emit_op(Opcode::Copy, vs, ks + kn, 0); }
                            else { sc.func.emit_with_flags(Opcode::CopyN, vn as u8, vs, ks + kn, 0); }
                        }
                        
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        
                        let post_pc = sc.func.current_pc();
                        sc.func.emit_jump(Opcode::Jump, 0);
                        let loop_jump_pc = sc.func.current_pc() - 1;
                        sc.func.patch_jump(loop_jump_pc, loop_start);
                        
                        let exit_pc = sc.func.current_pc();
                        sc.func.patch_jump(end_jump, exit_pc);
                        
                        let exit_info = sc.func.exit_loop();
                        sc.func.finalize_loop_hint(begin_pc, exit_pc);
                        
                        for pc in exit_info.break_patches { sc.func.patch_jump(pc, exit_pc); }
                        for pc in exit_info.continue_patches { sc.func.patch_jump(pc, post_pc); }
                        
                    } else if info.is_chan(range_type) {
                        let chan_reg = crate::expr::compile_expr(expr, sc.ctx, sc.func, sc.info)?;
                        let elem_type = info.chan_elem_type(range_type);
                        let elem_slots = info.chan_elem_slots(range_type);
                        
                        // Channel: use value or key (Go semantics: single var is value)
                        let var_expr = value.as_ref().or(key.as_ref());
                        let val_slot = range_var_slot(&mut sc, var_expr, elem_type, *define)?;
                        
                        // ok slot
                        let ok_slot = sc.func.alloc_temp_typed(&[SlotType::Value]);
                        
                        // loop:
                        let loop_start = sc.func.current_pc();
                        let begin_pc = sc.func.enter_loop(loop_start, label);
                        
                        // v, ok := <-ch
                        // ChanRecv: a=val_slot, b=chan_reg, c=ok_slot
                        // flags format: (elem_slots << 1) | has_ok
                        let recv_flags = ((elem_slots as u8) << 1) | 1;
                        sc.func.emit_with_flags(Opcode::ChanRecv, recv_flags, val_slot, chan_reg, ok_slot);
                        
                        // if !ok { goto end }
                        let end_jump = sc.func.emit_jump(Opcode::JumpIfNot, ok_slot);
                        
                        // body
                        compile_block(&for_stmt.body, sc.ctx, sc.func, sc.info)?;
                        
                        // exit_loop emits HINT_LOOP_END and patches flags
                        let exit_info = sc.func.exit_loop();
                        
                        // goto loop (continue target is loop_start for channel)
                        sc.func.emit_jump_to(Opcode::Jump, 0, loop_start);
                        
                        // end:
                        let exit_pc = sc.func.current_pc();
                        sc.func.patch_jump(end_jump, exit_pc);
                        
                        // Finalize HINT_LOOP_BEGIN with exit_pc
                        sc.func.finalize_loop_hint(begin_pc, exit_pc);
                        
                        for pc in exit_info.break_patches {
                            sc.func.patch_jump(pc, exit_pc);
                        }
                        for pc in exit_info.continue_patches {
                            sc.func.patch_jump(pc, loop_start);
                        }
                        
                    } else {
                        return Err(CodegenError::UnsupportedStmt("for-range unsupported type".to_string()));
                    }
                    
                    // Exit scope (restore any shadowed key/value variables)
                    func.exit_scope();
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
            // Mark current loop (if any) as containing defer
            func.mark_loop_has_defer();
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
            let chan_type = info.expr_type(send_stmt.chan.id);
            let elem_type = info.chan_elem_type(chan_type);
            let elem_slots = info.chan_elem_slots(chan_type) as u8;
            let val_reg = crate::expr::compile_expr_to_type(&send_stmt.value, elem_type, ctx, func, info)?;
            func.emit_with_flags(Opcode::ChanSend, elem_slots, chan_reg, val_reg, 0);
        }

        // === Select ===
        StmtKind::Select(select_stmt) => {
            compile_select(select_stmt, ctx, func, info)?;
        }

        // === Switch ===
        StmtKind::Switch(switch_stmt) => {
            compile_switch_with_label(switch_stmt, label, ctx, func, info)?;
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
            let tmp = func.alloc_temp_typed(&[SlotType::Value]);
            emit_lvalue_load(&lv, tmp, ctx, func);
            
            let one = func.alloc_temp_typed(&[SlotType::Value]);
            func.emit_op(Opcode::LoadInt, one, 1, 0);
            
            if inc_dec.is_inc {
                func.emit_op(Opcode::AddI, tmp, tmp, one);
            } else {
                func.emit_op(Opcode::SubI, tmp, tmp, one);
            }
            
            // Inc/dec on integers - no GC refs
            emit_lvalue_store(&lv, tmp, ctx, func, &[vo_runtime::SlotType::Value]);
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
            let mut fail_ret_slot_types = Vec::new();
            for ret_type in &ret_types {
                fail_ret_slot_types.extend(info.type_slot_types(*ret_type));
            }
            let ret_start = func.alloc_temp_typed(&fail_ret_slot_types);
            
            // Initialize all slots to zero/nil first
            for i in 0..total_ret_slots {
                func.emit_op(Opcode::LoadInt, ret_start + i, 0, 0);
            }
            
            // Compile the error expression into the last return slot(s)
            // The error is the last return value
            if !ret_types.is_empty() {
                let error_type = *ret_types.last().unwrap();
                let error_slots = info.type_slot_count(error_type);
                let error_start = ret_start + total_ret_slots - error_slots;
                compile_value_to(&fail_stmt.error, error_start, error_type, ctx, func, info)?;
            }
            
            // flags bit 0 = 1 indicates error return (for errdefer)
            func.emit_with_flags(Opcode::Return, 1, ret_start, total_ret_slots, 0);
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

/// Compile a block with automatic scope management.
/// Variables defined in the block will be scoped - shadowed outer variables
/// are restored when the block exits.
pub fn compile_block(
    block: &Block,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    func.enter_scope();
    for stmt in &block.stmts {
        compile_stmt(stmt, ctx, func, info)?;
    }
    func.exit_scope();
    Ok(())
}

/// Compile a block WITHOUT scope management.
/// Used when the caller needs manual control over scope boundaries
/// (e.g., ForClause::Three where post statement runs in outer scope).
fn compile_block_no_scope(
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
    
    let ExprKind::Call(call_expr) = &call.kind else {
        return Err(CodegenError::UnsupportedStmt("defer requires a call expression".to_string()));
    };
    
    // Method call (e.g., res.close())
    if let ExprKind::Selector(sel) = &call_expr.func.kind {
        return compile_defer_method_call(call_expr, sel, opcode, ctx, func, info);
    }
    
    // Regular function call
    if let ExprKind::Ident(ident) = &call_expr.func.kind {
        if let Some(func_idx) = ctx.get_function_index(ident.symbol) {
            let (args_start, total_arg_slots) = compile_defer_args_with_types(call_expr, ctx, func, info)?;
            emit_defer_func(opcode, func_idx, args_start, total_arg_slots, func);
            return Ok(());
        }
    }
    
    // Closure call (local variable or generic expression)
    let closure_reg = crate::expr::compile_expr(&call_expr.func, ctx, func, info)?;
    let (args_start, total_arg_slots) = compile_defer_args_simple(call_expr, ctx, func, info)?;
    emit_defer_closure(opcode, closure_reg, args_start, total_arg_slots, func);
    Ok(())
}

fn compile_defer_args_with_types(
    call_expr: &vo_syntax::ast::CallExpr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(u16, u16), CodegenError> {
    let func_type = info.expr_type(call_expr.func.id);
    let param_types = info.func_param_types(func_type);
    
    let total_arg_slots: u16 = call_expr.args.iter().enumerate()
        .map(|(i, arg)| param_types.get(i).map(|&pt| info.type_slot_count(pt)).unwrap_or_else(|| info.expr_slots(arg.id)))
        .sum();
    
    let args_start = func.alloc_args(total_arg_slots);
    crate::expr::call::compile_args_with_types(&call_expr.args, &param_types, args_start, ctx, func, info)?;
    
    Ok((args_start, total_arg_slots))
}

fn compile_defer_args_simple(
    call_expr: &vo_syntax::ast::CallExpr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(u16, u16), CodegenError> {
    crate::expr::call::compile_args_simple(&call_expr.args, ctx, func, info)
}

#[inline]
fn emit_defer_func(opcode: Opcode, func_idx: u32, args_start: u16, arg_slots: u16, func: &mut FuncBuilder) {
    let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_idx);
    func.emit_with_flags(opcode, func_id_high << 1, func_id_low, args_start, arg_slots);
}

#[inline]
fn emit_defer_closure(opcode: Opcode, closure_reg: u16, args_start: u16, arg_slots: u16, func: &mut FuncBuilder) {
    func.emit_with_flags(opcode, 1, closure_reg, args_start, arg_slots);
}

fn compile_defer_method_call(
    call_expr: &vo_syntax::ast::CallExpr,
    sel: &vo_syntax::ast::SelectorExpr,
    opcode: Opcode,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::ExprKind;
    
    let recv_type = info.expr_type(sel.expr.id);
    let method_name = info.project.interner.resolve(sel.sel.symbol)
        .ok_or_else(|| CodegenError::Internal("cannot resolve method name".to_string()))?;
    let method_sym = sel.sel.symbol;
    
    let selection = info.get_selection(call_expr.func.id);
    let is_interface_recv = info.is_interface(recv_type);
    
    let call_info = crate::embed::resolve_method_call(
        recv_type,
        method_name,
        method_sym,
        selection,
        is_interface_recv,
        ctx,
        &info.project.tc_objs,
        &info.project.interner,
    ).ok_or_else(|| CodegenError::UnsupportedExpr(format!("method {} not found", method_name)))?;
    
    // Extract func_id from Static dispatch (defer only works with static calls)
    let (func_id, expects_ptr_recv) = match call_info.dispatch {
        crate::embed::MethodDispatch::Static { func_id, expects_ptr_recv } => (func_id, expects_ptr_recv),
        _ => return Err(CodegenError::UnsupportedExpr("defer on interface call not supported".to_string())),
    };
    
    let base_type = if call_info.recv_is_pointer { info.pointer_base(recv_type) } else { recv_type };
    let actual_recv_type = call_info.actual_recv_type(base_type);
    let recv_storage = match &sel.expr.kind {
        ExprKind::Ident(ident) => func.lookup_local(ident.symbol).map(|l| l.storage),
        _ => None,
    };
    
    let recv_slots = if expects_ptr_recv { 1 } else { info.type_slot_count(actual_recv_type) };
    let other_arg_slots: u16 = call_expr.args.iter().map(|arg| info.expr_slots(arg.id)).sum();
    let total_arg_slots = recv_slots + other_arg_slots;
    let args_start = func.alloc_args(total_arg_slots);
    
    crate::expr::emit_receiver(
        &sel.expr, args_start, recv_type, recv_storage,
        &call_info, actual_recv_type, ctx, func, info
    )?;
    
    let mut offset = recv_slots;
    for arg in &call_expr.args {
        let slots = info.expr_slots(arg.id);
        crate::expr::compile_expr_to(arg, args_start + offset, ctx, func, info)?;
        offset += slots;
    }
    
    emit_defer_func(opcode, func_id, args_start, total_arg_slots, func);
    Ok(())
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
                    func.alloc_temp_typed(&vec![SlotType::Value; total_arg_slots as usize])
                } else {
                    0
                };
                
                crate::expr::call::compile_args_with_types(&call_expr.args, &param_types, args_start, ctx, func, info)?;
                
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
                    func.alloc_temp_typed(&vec![SlotType::Value; total_arg_slots as usize])
                } else {
                    0
                };
                
                crate::expr::call::compile_args_simple(&call_expr.args, ctx, func, info)?;
                
                // GoStart: a=closure_reg, b=args_start, c=arg_slots, flags=1 (is_closure)
                func.emit_with_flags(Opcode::GoStart, 1, closure_reg, args_start, total_arg_slots);
                return Ok(());
            }
        }
        
        // Generic case: expression returning a closure
        let closure_reg = crate::expr::compile_expr(&call_expr.func, ctx, func, info)?;
        
        let args_start = if total_arg_slots > 0 {
            func.alloc_temp_typed(&vec![SlotType::Value; total_arg_slots as usize])
        } else {
            0
        };
        
        crate::expr::call::compile_args_simple(&call_expr.args, ctx, func, info)?;
        
        func.emit_with_flags(Opcode::GoStart, 1, closure_reg, args_start, total_arg_slots);
        return Ok(());
    }
    
    Err(CodegenError::UnsupportedStmt("go requires a call expression".to_string()))
}

/// Info for recv case variable binding
struct RecvCaseInfo {
    dst_reg: u16,
    elem_slots: u16,
    has_ok: bool,
    chan_type: TypeKey,
}

/// Compile select statement
fn compile_select(
    select_stmt: &vo_syntax::ast::SelectStmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::CommClause;
    
    let case_count = select_stmt.cases.len();
    let has_default = select_stmt.cases.iter().any(|c| c.comm.is_none());
    
    // SelectBegin: a=case_count, flags=has_default
    func.emit_with_flags(Opcode::SelectBegin, has_default as u8, case_count as u16, 0, 0);
    
    // Phase 1: Emit SelectSend/SelectRecv for each case
    let mut recv_infos: Vec<Option<RecvCaseInfo>> = Vec::with_capacity(case_count);
    
    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        match &case.comm {
            None => {
                recv_infos.push(None);
            }
            Some(CommClause::Send(send)) => {
                let chan_reg = crate::expr::compile_expr(&send.chan, ctx, func, info)?;
                let val_reg = crate::expr::compile_expr(&send.value, ctx, func, info)?;
                let chan_type = info.expr_type(send.chan.id);
                let elem_slots = info.chan_elem_slots(chan_type) as u8;
                func.emit_with_flags(Opcode::SelectSend, elem_slots, chan_reg, val_reg, case_idx as u16);
                recv_infos.push(None);
            }
            Some(CommClause::Recv(recv)) => {
                let chan_reg = crate::expr::compile_expr(&recv.expr, ctx, func, info)?;
                let chan_type = info.expr_type(recv.expr.id);
                let elem_slots = info.chan_elem_slots(chan_type);
                let has_ok = recv.lhs.len() > 1;
                
                // Allocate destination: elem slots + optional ok bool
                let total_slots = elem_slots + if has_ok { 1 } else { 0 };
                let dst_reg = func.alloc_temp_typed(&vec![SlotType::Value; total_slots as usize]);
                
                let flags = ((elem_slots as u8) << 1) | (has_ok as u8);
                func.emit_with_flags(Opcode::SelectRecv, flags, dst_reg, chan_reg, case_idx as u16);
                recv_infos.push(Some(RecvCaseInfo { dst_reg, elem_slots, has_ok, chan_type }));
            }
        }
    }
    
    // SelectExec: returns chosen case index (-1 for default)
    let result_reg = func.alloc_temp_typed(&[SlotType::Value]);
    func.emit_op(Opcode::SelectExec, result_reg, 0, 0);
    
    // Phase 2: Generate dispatch jumps
    let mut case_jumps: Vec<usize> = Vec::with_capacity(case_count);
    let cmp_tmp = func.alloc_temp_typed(&[SlotType::Value]);
    
    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        let target_idx = if case.comm.is_none() { -1i32 } else { case_idx as i32 };
        let (b, c) = encode_i32(target_idx);
        func.emit_op(Opcode::LoadInt, cmp_tmp, b, c);
        func.emit_op(Opcode::EqI, cmp_tmp, result_reg, cmp_tmp);
        case_jumps.push(func.emit_jump(Opcode::JumpIf, cmp_tmp));
    }
    
    let fallthrough_jump = func.emit_jump(Opcode::Jump, 0);
    
    // Phase 3: Compile case bodies
    let mut end_jumps: Vec<usize> = Vec::with_capacity(case_count);
    
    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        func.patch_jump(case_jumps[case_idx], func.current_pc());
        
        // Bind recv variables
        if let Some(CommClause::Recv(recv)) = &case.comm {
            if let Some(ref recv_info) = recv_infos[case_idx] {
                bind_recv_variables(func, info, recv, recv_info);
            }
        }
        
        for stmt in &case.body {
            compile_stmt(stmt, ctx, func, info)?;
        }
        
        end_jumps.push(func.emit_jump(Opcode::Jump, 0));
    }
    
    // Patch all end jumps
    let end_pc = func.current_pc();
    func.patch_jump(fallthrough_jump, end_pc);
    for jump_pc in end_jumps {
        func.patch_jump(jump_pc, end_pc);
    }
    
    Ok(())
}

/// Bind variables for a recv case (either define new or assign to existing)
fn bind_recv_variables(
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
    recv: &vo_syntax::ast::RecvStmt,
    recv_info: &RecvCaseInfo,
) {
    if recv.lhs.is_empty() {
        return;
    }
    
    let elem_type = info.chan_elem_type(recv_info.chan_type);
    let elem_slot_types = info.type_slot_types(elem_type);
    
    // First variable: received value
    let first = &recv.lhs[0];
    if recv.define {
        func.define_local_at(first.symbol, recv_info.dst_reg, recv_info.elem_slots);
    } else if let Some(local) = func.lookup_local(first.symbol) {
        func.emit_storage_store(local.storage, recv_info.dst_reg, &elem_slot_types);
    }
    
    // Second variable: ok bool (if present)
    if recv_info.has_ok && recv.lhs.len() > 1 {
        let second = &recv.lhs[1];
        let ok_reg = recv_info.dst_reg + recv_info.elem_slots;
        if recv.define {
            func.define_local_at(second.symbol, ok_reg, 1);
        } else if let Some(local) = func.lookup_local(second.symbol) {
            func.emit_storage_store(local.storage, ok_reg, &[SlotType::Value]);
        }
    }
}


/// Compile type switch statement
/// Uses IfaceAssert instruction for type checking and value extraction.
fn compile_type_switch(
    type_switch: &vo_syntax::ast::TypeSwitchStmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Enter scope for init variable shadowing (Go semantics: type switch x := v.(type) creates new scope)
    func.enter_scope();
    
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
    let iface_slot = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
    func.emit_op(Opcode::Copy, iface_slot, expr_reg, 0);
    func.emit_op(Opcode::Copy, iface_slot + 1, expr_reg + 1, 0);
    
    // Collect case jumps and info for variable binding
    let mut case_jumps: Vec<(usize, usize)> = Vec::new(); // (case_idx, jump_pc)
    let mut end_jumps: Vec<usize> = Vec::new();
    let mut default_case_idx: Option<usize> = None;
    
    // Generate type checks for each case using IfaceAssert
    for (case_idx, case) in type_switch.cases.iter().enumerate() {
        if case.types.is_empty() {
            // Default case (no types specified)
            default_case_idx = Some(case_idx);
        } else {
            // Check if this is a nil-only case (all types are None)
            let is_nil_only_case = case.types.iter().all(|t| t.is_none());
            
            if is_nil_only_case {
                // case nil: check if interface is nil (value_kind == Void)
                // nil interface has slot0 with value_kind = 0 (Void) in low 8 bits
                let ok_slot = func.alloc_temp_typed(&[SlotType::Value]);
                let mask_slot = func.alloc_temp_typed(&[SlotType::Value]);
                let vk_slot = func.alloc_temp_typed(&[SlotType::Value]);
                
                // Load mask 0xFF to extract value_kind
                func.emit_op(Opcode::LoadInt, mask_slot, 0xFF, 0);
                // Extract value_kind: vk = slot0 & 0xFF
                func.emit_op(Opcode::And, vk_slot, iface_slot, mask_slot);
                // Check if value_kind == 0 (Void means nil)
                func.emit_op(Opcode::LoadInt, ok_slot, 0, 0);
                func.emit_op(Opcode::EqI, ok_slot, vk_slot, ok_slot);
                
                // Jump to case body if ok is true (interface is nil)
                case_jumps.push((case_idx, func.emit_jump(Opcode::JumpIf, ok_slot)));
            } else {
                // Type case - check each type
                for type_opt in &case.types {
                    if let Some(type_expr) = type_opt {
                        let type_key = info.type_expr_type(type_expr.id);
                        
                        let (assert_kind, target_id) = compute_iface_assert_params(type_key, ctx, info);
                        
                        // Allocate temp for IfaceAssert result (value + ok)
                        let target_slots = info.type_slot_count(type_key) as u8;
                        let result_slots: u16 = if assert_kind == 1 { 2 } else { target_slots as u16 };
                        // result + ok bool
                        let mut assert_result_types = info.type_slot_types(type_key);
                        assert_result_types.push(SlotType::Value); // ok bool
                        let result_reg = func.alloc_temp_typed(&assert_result_types); // +1 for ok bool
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
        
        // Enter scope for case-local variables (Go semantics: each case has its own scope)
        func.enter_scope();
        
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
                    let (assert_kind, target_id) = compute_iface_assert_params(type_key, ctx, info);
                    
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
        
        // Exit case scope
        func.exit_scope();
        
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
    
    // Exit outer scope (restore any shadowed variables from init)
    func.exit_scope();
    
    Ok(())
}

/// Compile switch statement with optional label (for break support).
fn compile_switch_with_label(
    switch_stmt: &vo_syntax::ast::SwitchStmt,
    label: Option<vo_common::symbol::Symbol>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Enter scope for init variable shadowing (Go semantics: switch x := 1; x { } creates new scope)
    func.enter_scope();
    
    // Init statement
    if let Some(init) = &switch_stmt.init {
        compile_stmt(init, ctx, func, info)?
    }
    
    // Compile tag expression (if present)
    let tag_reg = if let Some(tag) = &switch_stmt.tag {
        Some(crate::expr::compile_expr(tag, ctx, func, info)?)
    } else {
        None
    };
    
    // Enter switch context for break support
    func.enter_switch(label);
    
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
                let cmp_result = func.alloc_temp_typed(&[SlotType::Value]);
                
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
        
        // Enter scope for case-local variables (Go semantics: each case has its own scope)
        func.enter_scope();
        
        // Compile statements, track if ends with fallthrough
        let mut has_fallthrough = false;
        for stmt in &case.body {
            if matches!(stmt.kind, StmtKind::Fallthrough) {
                has_fallthrough = true;
            } else {
                compile_stmt(stmt, ctx, func, info)?;
            }
        }
        
        // Exit case scope
        func.exit_scope();
        
        // Jump to end unless case ends with fallthrough
        if !has_fallthrough {
            end_jumps.push(func.emit_jump(Opcode::Jump, 0));
        }
    }
    
    // Exit switch context and get break patches
    let break_patches = func.exit_switch();
    
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
    
    // Patch end jumps (implicit break at end of case)
    for jump_pc in end_jumps {
        func.patch_jump(jump_pc, end_pc);
    }
    
    // Patch explicit break statements
    for jump_pc in break_patches {
        func.patch_jump(jump_pc, end_pc);
    }
    
    // Exit scope (restore any shadowed variables from init)
    func.exit_scope();
    
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
    use crate::lvalue::{resolve_lvalue, emit_lvalue_store};
    
    // Handle blank identifier: compile RHS for side effects only
    if let vo_syntax::ast::ExprKind::Ident(ident) = &lhs.kind {
        if info.project.interner.resolve(ident.symbol) == Some("_") {
            let _ = crate::expr::compile_expr(rhs, ctx, func, info)?;
            return Ok(());
        }
    }
    
    // Resolve LHS to an LValue
    let lv = resolve_lvalue(lhs, ctx, func, info)?;
    
    // Get LHS type for interface check
    let lhs_type = info.expr_type(lhs.id);
    
    // Handle interface assignment specially
    if info.is_interface(lhs_type) {
        return compile_assign_to_interface(&lv, rhs, lhs_type, ctx, func, info);
    }
    
    // Compile RHS to temp, then store to LValue
    let slot_types = info.type_slot_types(lhs_type);
    let tmp = func.alloc_temp_typed(&slot_types);
    compile_expr_to(rhs, tmp, ctx, func, info)?;
    
    // Apply truncation for narrow integer types (Go semantics)
    emit_int_trunc(tmp, lhs_type, func, info);
    
    emit_lvalue_store(&lv, tmp, ctx, func, &slot_types);
    
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
    let tmp = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
    compile_iface_assign(tmp, rhs, iface_type, ctx, func, info)?;
    // Interface data slot may contain GcRef
    // Interface: slot0=header, slot1=data (may be GcRef)
    emit_lvalue_store(lv, tmp, ctx, func, &[vo_runtime::SlotType::Value, vo_runtime::SlotType::Interface1]);
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
    use crate::lvalue::{resolve_lvalue, emit_lvalue_load, emit_lvalue_store, LValue};
    use crate::func::StorageKind;
    
    // Get the operation opcode based on AssignOp and type
    let lhs_type = info.expr_type(lhs.id);
    let is_float = info.is_float(lhs_type);
    let is_string = info.is_string(lhs_type);
    let is_unsigned = info.is_unsigned(lhs_type);
    
    let opcode = match (op, is_float, is_string, is_unsigned) {
        (AssignOp::Add, false, false, _) => Opcode::AddI,
        (AssignOp::Add, true, false, _) => Opcode::AddF,
        (AssignOp::Add, _, true, _) => Opcode::StrConcat,
        (AssignOp::Sub, false, _, _) => Opcode::SubI,
        (AssignOp::Sub, true, _, _) => Opcode::SubF,
        (AssignOp::Mul, false, _, _) => Opcode::MulI,
        (AssignOp::Mul, true, _, _) => Opcode::MulF,
        (AssignOp::Div, false, _, _) => Opcode::DivI,
        (AssignOp::Div, true, _, _) => Opcode::DivF,
        (AssignOp::Rem, _, _, _) => Opcode::ModI,
        (AssignOp::And, _, _, _) => Opcode::And,
        (AssignOp::Or, _, _, _) => Opcode::Or,
        (AssignOp::Xor, _, _, _) => Opcode::Xor,
        (AssignOp::AndNot, _, _, _) => Opcode::AndNot,
        (AssignOp::Shl, _, _, _) => Opcode::Shl,
        (AssignOp::Shr, _, _, false) => Opcode::ShrS,
        (AssignOp::Shr, _, _, true) => Opcode::ShrU,
        (AssignOp::Assign, _, _, _) => unreachable!("plain assign handled separately"),
    };
    
    // Resolve LHS to an LValue
    let lv = resolve_lvalue(lhs, ctx, func, info)?;
    
    // Compile RHS (may return existing slot for variables)
    let rhs_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
    
    // Fast path: single-slot stack variable - operate directly, no load/store needed
    if let LValue::Variable(StorageKind::StackValue { slot, slots: 1 }) = &lv {
        func.emit_op(opcode, *slot, *slot, rhs_reg);
        // Apply truncation for narrow integer types (Go semantics)
        emit_int_trunc(*slot, lhs_type, func, info);
        return Ok(());
    }
    
    // General path: read current value, apply operation, write back
    // Use proper slot type for strings (GcRef) vs numeric types (Value)
    let slot_type = if is_string { vo_runtime::SlotType::GcRef } else { vo_runtime::SlotType::Value };
    let tmp = func.alloc_temp_typed(&[slot_type]);
    emit_lvalue_load(&lv, tmp, ctx, func);
    func.emit_op(opcode, tmp, tmp, rhs_reg);
    // Apply truncation for narrow integer types (Go semantics)
    emit_int_trunc(tmp, lhs_type, func, info);
    emit_lvalue_store(&lv, tmp, ctx, func, &[slot_type]);
    
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
    
    // Optimization: if src is any (empty interface), just copy - no itab rebuild needed
    // because any has no methods, itab is always 0
    if src_vk == vo_runtime::ValueKind::Interface && info.is_empty_interface(src_type) {
        let src_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
        func.emit_copy(dst_slot, src_reg, 2);
        return Ok(());
    }
    
    let iface_meta_id = info.get_or_create_interface_meta_id(iface_type, ctx);
    
    let const_idx = if src_vk == vo_runtime::ValueKind::Interface {
        ctx.register_iface_assign_const_interface(iface_meta_id)
    } else if src_vk == vo_runtime::ValueKind::Void {
        // nil interface: rttid=0, itab_id=0, so packed=0
        ctx.const_int(0)
    } else {
        // Use RuntimeType for structural equality in rttid lookup
        let rt = info.type_to_runtime_type(src_type, ctx);
        let rttid = ctx.intern_rttid(rt);
        // For pointer types, get the base type (methods are on the base type)
        let base_type = if info.is_pointer(src_type) {
            info.pointer_base(src_type)
        } else {
            src_type
        };
        // Pass type_key for lookup_field_or_method during itab building
        ctx.register_iface_assign_const_concrete(rttid, Some(base_type), iface_meta_id, info.tc_objs())
    };
    
    if src_vk.needs_boxing() {
        // IfaceAssign does ptr_clone internally for Struct/Array, so we just need to
        // pass a GcRef. The VM ensures value semantics (deep copy).
        let expr_source = crate::expr::get_expr_source(rhs, ctx, func, info);
        match expr_source {
            crate::func::ExprSource::Location(crate::func::StorageKind::HeapBoxed { gcref_slot, .. }) => {
                // HeapBoxed struct: pass GcRef directly, IfaceAssign will ptr_clone
                func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst_slot, gcref_slot, const_idx);
            }
            crate::func::ExprSource::Location(crate::func::StorageKind::HeapArray { gcref_slot, .. }) => {
                // HeapArray has different layout: [GcHeader][ArrayHeader(2 slots)][elems...]
                // We need to create a boxed value with just the element data.
                // IfaceAssign expects a boxed value layout: [GcHeader][data...]
                let src_slots = info.type_slot_count(src_type);
                let src_slot_types = info.type_slot_types(src_type);
                let rttid = ctx.intern_type_key(src_type, info);
                let meta_idx = ctx.get_or_create_value_meta_with_rttid(rttid, &src_slot_types, None);
                
                // Read element data from array (skip ArrayHeader at offset 2)
                let tmp_data = func.alloc_temp_typed(&src_slot_types);
                const ARRAY_HEADER_SLOTS: u16 = 2;
                func.emit_ptr_get(tmp_data, gcref_slot, ARRAY_HEADER_SLOTS, src_slots);
                
                // Create new boxed value and copy data
                let new_gcref_slot = func.alloc_temp_typed(&[SlotType::GcRef]);
                let meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
                func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
                func.emit_with_flags(Opcode::PtrNew, src_slots as u8, new_gcref_slot, meta_reg, 0);
                func.emit_ptr_set(new_gcref_slot, 0, tmp_data, src_slots);
                
                func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst_slot, new_gcref_slot, const_idx);
            }
            _ => {
                // Stack value or expression: allocate box and copy data
                let src_slots = info.type_slot_count(src_type);
                let src_slot_types = info.type_slot_types(src_type);
                let rttid = ctx.intern_type_key(src_type, info);
                let meta_idx = ctx.get_or_create_value_meta_with_rttid(rttid, &src_slot_types, None);
                
                let tmp_data = func.alloc_temp_typed(&src_slot_types);
                compile_expr_to(rhs, tmp_data, ctx, func, info)?;
                
                let gcref_slot = func.alloc_temp_typed(&[SlotType::GcRef]);
                let meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
                func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
                func.emit_with_flags(Opcode::PtrNew, src_slots as u8, gcref_slot, meta_reg, 0);
                func.emit_ptr_set(gcref_slot, 0, tmp_data, src_slots);
                
                func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst_slot, gcref_slot, const_idx);
            }
        }
    } else {
        let src_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
        func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst_slot, src_reg, const_idx);
    }
    Ok(())
}

/// Emit IfaceAssign from concrete type value already in a register.
/// Used for map index expressions where key is interface type.
pub fn emit_iface_assign_from_concrete(
    dst_slot: u16,
    src_reg: u16,
    src_type: vo_analysis::objects::TypeKey,
    iface_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let src_vk = info.type_value_kind(src_type);
    let iface_meta_id = info.get_or_create_interface_meta_id(iface_type, ctx);
    
    let const_idx = if src_vk == vo_runtime::ValueKind::Void {
        ctx.const_int(0)
    } else {
        let rt = info.type_to_runtime_type(src_type, ctx);
        let rttid = ctx.intern_rttid(rt);
        let base_type = if info.is_pointer(src_type) {
            info.pointer_base(src_type)
        } else {
            src_type
        };
        ctx.register_iface_assign_const_concrete(rttid, Some(base_type), iface_meta_id, info.tc_objs())
    };
    
    if src_vk.needs_boxing() {
        // Struct/Array: allocate box and copy data
        let src_slots = info.type_slot_count(src_type);
        let src_slot_types = info.type_slot_types(src_type);
        let rttid = ctx.intern_type_key(src_type, info);
        let meta_idx = ctx.get_or_create_value_meta_with_rttid(rttid, &src_slot_types, None);
        
        let gcref_slot = func.alloc_temp_typed(&[SlotType::GcRef]);
        let meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        func.emit_with_flags(Opcode::PtrNew, src_slots as u8, gcref_slot, meta_reg, 0);
        func.emit_ptr_set(gcref_slot, 0, src_reg, src_slots);
        
        func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst_slot, gcref_slot, const_idx);
    } else {
        func.emit_with_flags(Opcode::IfaceAssign, src_vk as u8, dst_slot, src_reg, const_idx);
    }
    Ok(())
}

