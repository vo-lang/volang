//! Expression compilation.

pub mod builtin;
pub mod call;
pub mod conversion;
pub mod dyn_access;
pub mod literal;

use vo_runtime::SlotType;
use vo_syntax::ast::{BinaryOp, Expr, ExprKind, UnaryOp};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{ExprSource, FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

// Re-export commonly used items
pub use call::{emit_receiver};
pub use literal::{compile_const_value, get_const_value};

// =============================================================================
// Tuple Expansion Helper
// =============================================================================

/// Represents a compiled tuple with its elements ready for iteration.
pub struct CompiledTuple {
    /// Base register where tuple is stored
    pub base: u16,
    /// Type of the tuple
    pub tuple_type: vo_analysis::objects::TypeKey,
}

impl CompiledTuple {
    /// Compile a tuple expression and return handle for iterating elements.
    pub fn compile(
        expr: &Expr,
        ctx: &mut CodegenContext,
        func: &mut FuncBuilder,
        info: &TypeInfoWrapper,
    ) -> Result<Self, CodegenError> {
        let tuple_type = info.expr_type(expr.id);
        let slot_types = info.type_slot_types(tuple_type);
        let base = func.alloc_temp_typed(&slot_types);
        compile_expr_to(expr, base, ctx, func, info)?;
        Ok(Self { base, tuple_type })
    }

    /// Iterate over tuple elements, calling f for each (elem_slot, elem_type).
    pub fn for_each_element<F>(&self, info: &TypeInfoWrapper, mut f: F)
    where
        F: FnMut(u16, vo_analysis::objects::TypeKey),
    {
        let mut offset = 0u16;
        for i in 0..info.tuple_len(self.tuple_type) {
            let elem_type = info.tuple_elem_type(self.tuple_type, i);
            f(self.base + offset, elem_type);
            offset += info.type_slot_count(elem_type);
        }
    }
    
    /// Iterate over tuple elements with fallible callback.
    pub fn for_each_element_result<F, E>(&self, info: &TypeInfoWrapper, mut f: F) -> Result<(), E>
    where
        F: FnMut(u16, vo_analysis::objects::TypeKey) -> Result<(), E>,
    {
        let mut offset = 0u16;
        for i in 0..info.tuple_len(self.tuple_type) {
            let elem_type = info.tuple_elem_type(self.tuple_type, i);
            f(self.base + offset, elem_type)?;
            offset += info.type_slot_count(elem_type);
        }
        Ok(())
    }
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Check if a selector expression is a package-qualified name (e.g., task.PriorityHigh)
fn is_pkg_qualified_name(sel: &vo_syntax::ast::SelectorExpr, info: &TypeInfoWrapper) -> bool {
    if let ExprKind::Ident(ident) = &sel.expr.kind {
        let obj = info.get_use(ident);
        info.obj_is_pkg(obj)
    } else {
        false
    }
}

/// Get ExprSource for an expression - determines where the value comes from.
pub fn get_expr_source(
    expr: &Expr,
    ctx: &CodegenContext,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> ExprSource {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            if let Some(local) = func.lookup_local(ident.symbol) {
                return ExprSource::Location(local.storage);
            }
            let obj_key = info.get_use(ident);
            if let Some(global_idx) = ctx.get_global_index(obj_key) {
                if let Some(type_key) = info.try_obj_type(obj_key) {
                    // Global arrays are stored as GcRef (1 slot)
                    let slots = if info.is_array(type_key) { 1 } else { info.type_slot_count(type_key) };
                    return ExprSource::Location(StorageKind::Global { index: global_idx as u16, slots });
                }
            }
        }
        ExprKind::Selector(sel) => {
            if is_pkg_qualified_name(sel, info) {
                return ExprSource::NeedsCompile;
            }
            // Check if this is a method value (t.M) - needs special handling
            if let Some(selection) = info.get_selection(expr.id) {
                if *selection.kind() == vo_analysis::selection::SelectionKind::MethodVal {
                    return ExprSource::NeedsCompile;
                }
                // Indirect selection (embedded pointer fields) requires runtime deref
                if selection.indirect() {
                    return ExprSource::NeedsCompile;
                }
            }
            let expr_type = info.expr_type(expr.id);
            let field_slots = info.type_slot_count(expr_type);
            let recv_type = info.expr_type(sel.expr.id);
            if info.is_pointer(recv_type) {
                return ExprSource::NeedsCompile;
            }
            if let ExprSource::Location(StorageKind::StackValue { slot: base_slot, .. }) = 
                get_expr_source(&sel.expr, ctx, func, info) 
            {
                if let Some(field_name) = info.project.interner.resolve(sel.sel.symbol) {
                    let (offset, _) = info.selector_field_offset(expr.id, recv_type, field_name);
                    return ExprSource::Location(StorageKind::StackValue { 
                        slot: base_slot + offset, 
                        slots: field_slots 
                    });
                }
            }
        }
        ExprKind::Paren(inner) => {
            return get_expr_source(inner, ctx, func, info);
        }
        _ => {}
    }
    ExprSource::NeedsCompile
}

/// Get the GcRef slot from a StorageKind.
pub fn get_gcref_slot(storage: &StorageKind) -> Option<u16> {
    match storage {
        StorageKind::HeapBoxed { gcref_slot, .. } => Some(*gcref_slot),
        StorageKind::HeapArray { gcref_slot, .. } => Some(*gcref_slot),
        StorageKind::Reference { slot } => Some(*slot),
        StorageKind::StackValue { .. } | StorageKind::StackArray { .. } | StorageKind::Global { .. } => None,
    }
}

/// Compile a map key expression, boxing to interface if needed.
/// Used for static map index/set/delete and map literal Expr keys.
pub fn compile_map_key_expr(
    index_expr: &Expr,
    key_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let index_type = info.expr_type(index_expr.id);
    let needs_boxing = info.is_interface(key_type) && !info.is_interface(index_type);
    
    if needs_boxing {
        let src_reg = compile_expr(index_expr, ctx, func, info)?;
        let key_slot_types = info.type_slot_types(key_type);
        let iface_reg = func.alloc_temp_typed(&key_slot_types);
        crate::stmt::emit_iface_assign_from_concrete(
            iface_reg, src_reg, index_type, key_type, ctx, func, info
        )?;
        Ok(iface_reg)
    } else {
        compile_expr(index_expr, ctx, func, info)
    }
}

/// Compile an element expression to a destination slot, boxing to interface if the target type is interface.
/// Common pattern for array/slice/map element initialization.
pub fn compile_elem_to(
    elem_expr: &Expr,
    dst: u16,
    target_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    if info.is_interface(target_type) {
        crate::stmt::compile_iface_assign(dst, elem_expr, target_type, ctx, func, info)
    } else {
        compile_expr_to(elem_expr, dst, ctx, func, info)
    }
}

/// Get the GcRef slot of an escaped variable without copying.
fn get_escaped_var_gcref(
    expr: &Expr,
    ctx: &CodegenContext,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> Option<u16> {
    match get_expr_source(expr, ctx, func, info) {
        ExprSource::Location(storage) => get_gcref_slot(&storage),
        _ => None,
    }
}

/// Match nil comparison in if condition for optimization.
pub fn match_nil_comparison<'a>(
    bin: &'a vo_syntax::ast::BinaryExpr,
    info: &TypeInfoWrapper,
) -> Option<(&'a Expr, bool)> {
    let is_nil = |e: &Expr| -> bool {
        if let ExprKind::Ident(id) = &e.kind {
            info.project.interner.resolve(id.symbol) == Some("nil")
        } else {
            false
        }
    };
    match bin.op {
        BinaryOp::Eq | BinaryOp::NotEq => {
            let is_eq = bin.op == BinaryOp::Eq;
            if is_nil(&bin.right) {
                Some((&bin.left, is_eq))
            } else if is_nil(&bin.left) {
                Some((&bin.right, is_eq))
            } else {
                None
            }
        }
        _ => None,
    }
}

// =============================================================================
// Main Entry Points
// =============================================================================

/// Compile expression, return result slot.
pub fn compile_expr(
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    if let ExprSource::Location(storage) = get_expr_source(expr, ctx, func, info) {
        match storage {
            StorageKind::StackValue { slot, slots: 1 } => return Ok(slot),
            StorageKind::Reference { slot } => return Ok(slot),
            _ => {}
        }
    }
    // Use expression type's slot types to ensure GcRefs (strings, pointers, etc.) are tracked by GC
    let expr_type = info.expr_type(expr.id);
    let slot_types = info.type_slot_types(expr_type);
    let dst = func.alloc_temp_typed(&slot_types);
    compile_expr_to(expr, dst, ctx, func, info)?;
    // Truncate narrow integer types to ensure Go semantics (operations in 64-bit, result in type width)
    emit_int_trunc(dst, expr_type, func, info);
    Ok(dst)
}

/// Compile expression with implicit conversion to target type.
/// Handles cases like struct -> interface conversion automatically.
pub fn compile_expr_to_type(
    expr: &Expr,
    target_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let src_type = info.expr_type(expr.id);
    
    // Check if implicit interface conversion is needed
    if info.is_interface(target_type) && !info.is_interface(src_type) {
        let dst = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]); // interface is 2 slots
        crate::stmt::compile_iface_assign(dst, expr, target_type, ctx, func, info)?;
        Ok(dst)
    } else {
        compile_expr(expr, ctx, func, info)
    }
}

/// Compile expression to specified slot.
pub fn compile_expr_to(
    expr: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    match &expr.kind {
        // === Literals ===
        ExprKind::IntLit(_) | ExprKind::RuneLit(_) | ExprKind::FloatLit(_) | ExprKind::StringLit(_) => {
            let val = get_const_value(expr.id, info)
                .ok_or_else(|| CodegenError::Internal("literal has no const value".to_string()))?;
            let target_type = info.expr_type(expr.id);
            compile_const_value(val, dst, target_type, ctx, func, info)?;
        }

        // === Identifier ===
        ExprKind::Ident(ident) => {
            if info.project.interner.resolve(ident.symbol) == Some("nil") {
                func.emit_op(Opcode::LoadInt, dst, 0, 0);
                return Ok(());
            }
            if let Some(val) = get_const_value(expr.id, info) {
                let target_type = info.expr_type(expr.id);
                compile_const_value(val, dst, target_type, ctx, func, info)?;
                return Ok(());
            }
            match get_expr_source(expr, ctx, func, info) {
                ExprSource::Location(storage) => {
                    func.emit_storage_load(storage, dst);
                }
                ExprSource::NeedsCompile => {
                    let obj_key = info.get_use(ident);
                    // Closure capture: ClosureGet returns GcRef to the captured storage
                    if let Some(capture) = func.lookup_capture(ident.symbol) {
                        func.emit_op(Opcode::ClosureGet, dst, capture.index, 0);
                        
                        // Arrays: capture stores GcRef to [ArrayHeader][elems], use directly
                        // Others: capture stores GcRef to box [value], need PtrGet to read value
                        let type_key = info.obj_type(obj_key, "captured var must have type");
                        if !info.is_array(type_key) {
                            let value_slots = info.type_slot_count(type_key);
                            func.emit_ptr_get(dst, dst, 0, value_slots);
                        }
                    } else {
                        // Function reference: create closure with no captures
                        if let Some(func_idx) = ctx.get_func_by_objkey(obj_key) {
                            func.emit_closure_new(dst, func_idx, 0);
                        } else {
                            return Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)));
                        }
                    }
                }
            }
        }

        // === Binary operations ===
        ExprKind::Binary(bin) => {
            if let Some(val) = get_const_value(expr.id, info) {
                let target_type = info.expr_type(expr.id);
                compile_const_value(val, dst, target_type, ctx, func, info)?;
                return Ok(());
            }
            
            // Short-circuit evaluation MUST be handled before computing operands
            if matches!(bin.op, BinaryOp::LogAnd | BinaryOp::LogOr) {
                return compile_short_circuit(expr, &bin.op, &bin.left, &bin.right, dst, ctx, func, info);
            }
            
            let operand_type = info.expr_type(bin.left.id);
            
            // Interface comparison with IfaceEq opcode for proper deep comparison
            // Skip if comparing with nil (use simple EqI for nil checks)
            if info.is_interface(operand_type) && matches!(bin.op, BinaryOp::Eq | BinaryOp::NotEq) {
                let check_nil = |e: &Expr| -> bool {
                    if let ExprKind::Ident(id) = &e.kind {
                        info.project.interner.resolve(id.symbol) == Some("nil")
                    } else {
                        false
                    }
                };
                let left_is_nil = check_nil(&bin.left);
                let right_is_nil = check_nil(&bin.right);
                
                // Only use IfaceEq for non-nil comparisons (deep comparison needed)
                if !left_is_nil && !right_is_nil {
                    let left_reg = func.alloc_interfaces(1);
                    let right_reg = func.alloc_interfaces(1);
                    compile_expr_to(&bin.left, left_reg, ctx, func, info)?;
                    compile_expr_to(&bin.right, right_reg, ctx, func, info)?;
                    func.emit_op(Opcode::IfaceEq, dst, left_reg, right_reg);
                    if bin.op == BinaryOp::NotEq {
                        func.emit_op(Opcode::BoolNot, dst, dst, 0);
                    }
                    return Ok(());
                }
                // For nil comparisons, fall through to use EqI (simpler and correct)
            }
            
            // Array/struct comparison: compare all slots
            if (info.is_array(operand_type) || info.is_struct(operand_type)) 
                && matches!(bin.op, BinaryOp::Eq | BinaryOp::NotEq) {
                return compile_composite_comparison(&bin.op, &bin.left, &bin.right, operand_type, dst, ctx, func, info);
            }
            
            let left_reg = compile_expr(&bin.left, ctx, func, info)?;
            let right_reg = compile_expr(&bin.right, ctx, func, info)?;
            
            let is_float = info.is_float(operand_type);
            let is_float32 = info.is_float32(operand_type);
            let is_string = info.is_string(operand_type);
            let is_unsigned = info.is_unsigned(operand_type);

            // float32 arithmetic: convert f32 bits -> f64, operate, convert back
            let (actual_left, actual_right) = if is_float32 {
                let tmp_left = func.alloc_temp_typed(&[SlotType::Value]);
                let tmp_right = func.alloc_temp_typed(&[SlotType::Value]);
                func.emit_op(Opcode::ConvF32F64, tmp_left, left_reg, 0);
                func.emit_op(Opcode::ConvF32F64, tmp_right, right_reg, 0);
                (tmp_left, tmp_right)
            } else {
                (left_reg, right_reg)
            };

            let opcode = match (&bin.op, is_float, is_string, is_unsigned) {
                (BinaryOp::Add, false, false, _) => Opcode::AddI,
                (BinaryOp::Add, true, false, _) => Opcode::AddF,
                (BinaryOp::Add, _, true, _) => Opcode::StrConcat,
                (BinaryOp::Sub, false, _, _) => Opcode::SubI,
                (BinaryOp::Sub, true, _, _) => Opcode::SubF,
                (BinaryOp::Mul, false, _, _) => Opcode::MulI,
                (BinaryOp::Mul, true, _, _) => Opcode::MulF,
                (BinaryOp::Div, false, _, false) => Opcode::DivI,
                (BinaryOp::Div, false, _, true) => Opcode::DivU,
                (BinaryOp::Div, true, _, _) => Opcode::DivF,
                (BinaryOp::Rem, false, _, false) => Opcode::ModI,
                (BinaryOp::Rem, false, _, true) => Opcode::ModU,
                (BinaryOp::Eq, false, false, _) => Opcode::EqI,
                (BinaryOp::Eq, true, false, _) => Opcode::EqF,
                (BinaryOp::Eq, _, true, _) => Opcode::StrEq,
                (BinaryOp::NotEq, false, false, _) => Opcode::NeI,
                (BinaryOp::NotEq, true, false, _) => Opcode::NeF,
                (BinaryOp::NotEq, _, true, _) => Opcode::StrNe,
                (BinaryOp::Lt, false, false, false) => Opcode::LtI,
                (BinaryOp::Lt, false, false, true) => Opcode::LtU,
                (BinaryOp::Lt, true, false, _) => Opcode::LtF,
                (BinaryOp::Lt, _, true, _) => Opcode::StrLt,
                (BinaryOp::LtEq, false, false, false) => Opcode::LeI,
                (BinaryOp::LtEq, false, false, true) => Opcode::LeU,
                (BinaryOp::LtEq, true, false, _) => Opcode::LeF,
                (BinaryOp::LtEq, _, true, _) => Opcode::StrLe,
                (BinaryOp::Gt, false, false, false) => Opcode::GtI,
                (BinaryOp::Gt, false, false, true) => Opcode::GtU,
                (BinaryOp::Gt, true, false, _) => Opcode::GtF,
                (BinaryOp::Gt, _, true, _) => Opcode::StrGt,
                (BinaryOp::GtEq, false, false, false) => Opcode::GeI,
                (BinaryOp::GtEq, false, false, true) => Opcode::GeU,
                (BinaryOp::GtEq, true, false, _) => Opcode::GeF,
                (BinaryOp::GtEq, _, true, _) => Opcode::StrGe,
                (BinaryOp::And, _, _, _) => Opcode::And,
                (BinaryOp::Or, _, _, _) => Opcode::Or,
                (BinaryOp::Xor, _, _, _) => Opcode::Xor,
                (BinaryOp::Shl, _, _, _) => Opcode::Shl,
                (BinaryOp::Shr, _, _, false) => Opcode::ShrS,
                (BinaryOp::Shr, _, _, true) => Opcode::ShrU,
                // LogAnd/LogOr handled earlier with short-circuit evaluation
                (BinaryOp::AndNot, _, _, _) => Opcode::AndNot,
                _ => return Err(CodegenError::UnsupportedExpr(format!("binary op {:?}", bin.op))),
            };

            func.emit_op(opcode, dst, actual_left, actual_right);
            // float32 arithmetic result: convert f64 back to f32 bits
            // (comparison results are bool, don't need conversion)
            let is_arith = matches!(bin.op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div);
            if is_float32 && is_arith {
                func.emit_op(Opcode::ConvF64F32, dst, dst, 0);
            }
        }

        // === Unary operations ===
        ExprKind::Unary(unary) => {
            if let Some(val) = get_const_value(expr.id, info) {
                let target_type = info.expr_type(expr.id);
                compile_const_value(val, dst, target_type, ctx, func, info)?;
                return Ok(());
            }
            match unary.op {
                UnaryOp::Addr => {
                    compile_addr_of(&unary.operand, dst, ctx, func, info)?;
                }
                UnaryOp::Deref => {
                    compile_deref(&unary.operand, dst, ctx, func, info)?;
                }
                UnaryOp::Neg => {
                    let operand = compile_expr(&unary.operand, ctx, func, info)?;
                    let type_key = info.expr_type(expr.id);
                    let is_float = info.is_float(type_key);
                    let opcode = if is_float { Opcode::NegF } else { Opcode::NegI };
                    func.emit_op(opcode, dst, operand, 0);
                }
                UnaryOp::Not => {
                    let operand = compile_expr(&unary.operand, ctx, func, info)?;
                    func.emit_op(Opcode::BoolNot, dst, operand, 0);
                }
                UnaryOp::BitNot => {
                    let operand = compile_expr(&unary.operand, ctx, func, info)?;
                    func.emit_op(Opcode::Not, dst, operand, 0);
                }
                UnaryOp::Pos => {
                    compile_expr_to(&unary.operand, dst, ctx, func, info)?;
                }
            }
        }

        // === Parentheses ===
        ExprKind::Paren(inner) => {
            compile_expr_to(inner, dst, ctx, func, info)?;
        }

        // === Selector ===
        ExprKind::Selector(sel) => {
            compile_selector(expr, sel, dst, ctx, func, info)?;
        }

        // === Index ===
        ExprKind::Index(idx) => {
            compile_index(expr, idx, dst, ctx, func, info)?;
        }

        // === Slice ===
        ExprKind::Slice(slice_expr) => {
            compile_slice_expr(expr, slice_expr, dst, ctx, func, info)?;
        }

        // === Type assertion ===
        ExprKind::TypeAssert(type_assert) => {
            compile_type_assert(expr, type_assert, dst, ctx, func, info)?;
        }

        // === Channel receive ===
        ExprKind::Receive(chan_expr) => {
            compile_receive(expr, chan_expr, dst, ctx, func, info)?;
        }

        // === Type conversion ===
        ExprKind::Conversion(conv) => {
            conversion::compile_conversion(expr, conv, dst, ctx, func, info)?;
        }

        // === Composite literal ===
        ExprKind::CompositeLit(lit) => {
            literal::compile_composite_lit(expr, lit, dst, ctx, func, info)?;
        }

        // === Type as expression ===
        ExprKind::TypeAsExpr(_) => {
            func.emit_op(Opcode::LoadInt, dst, 0, 0);
        }

        // === Try unwrap ===
        ExprKind::TryUnwrap(inner) => {
            compile_try_unwrap(inner, dst, ctx, func, info)?;
        }

        // === Dynamic access ===
        ExprKind::DynAccess(dyn_access_expr) => {
            dyn_access::compile_dyn_access(expr, dyn_access_expr, dst, ctx, func, info)?;
        }

        // === Call ===
        ExprKind::Call(call_expr) => {
            call::compile_call(expr, call_expr, dst, ctx, func, info)?;
        }

        // === Function literal ===
        ExprKind::FuncLit(func_lit) => {
            literal::compile_func_lit(expr, func_lit, dst, ctx, func, info)?;
        }
    }

    Ok(())
}

// =============================================================================
// Expression Compilation Helpers
// =============================================================================

fn compile_short_circuit(
    _expr: &Expr,
    op: &BinaryOp,
    left: &Expr,
    right: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    compile_expr_to(left, dst, ctx, func, info)?;
    let skip_jump = match op {
        BinaryOp::LogAnd => func.emit_jump(Opcode::JumpIfNot, dst),
        BinaryOp::LogOr => func.emit_jump(Opcode::JumpIf, dst),
        _ => unreachable!(),
    };
    compile_expr_to(right, dst, ctx, func, info)?;
    func.patch_jump(skip_jump, func.current_pc());
    Ok(())
}

fn compile_selector(
    expr: &Expr,
    sel: &vo_syntax::ast::SelectorExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    if is_pkg_qualified_name(sel, info) {
        return compile_pkg_qualified_name(expr, sel, dst, ctx, func, info);
    }

    // Check if this is a method value (t.M) or method expression (T.M / (*T).M)
    if let Some(selection) = info.get_selection(expr.id) {
        match selection.kind() {
            vo_analysis::selection::SelectionKind::MethodVal => {
                return compile_method_value(expr, sel, selection, dst, ctx, func, info);
            }
            vo_analysis::selection::SelectionKind::MethodExpr => {
                return compile_method_expr(expr, sel, selection, dst, ctx, func, info);
            }
            vo_analysis::selection::SelectionKind::FieldVal => {}
        }
    }

    if let ExprSource::Location(storage) = get_expr_source(expr, ctx, func, info) {
        func.emit_storage_load(storage, dst);
        return Ok(());
    }

    let recv_type = info.expr_type(sel.expr.id);
    let field_name = info
        .project
        .interner
        .resolve(sel.sel.symbol)
        .ok_or_else(|| CodegenError::Internal("cannot resolve field".to_string()))?;

    // Check for indirect selection (embedded pointer fields require runtime deref)
    if let Some(selection) = info.get_selection(expr.id) {
        if selection.indirect() {
            return compile_indirect_selector(sel, selection.indices(), dst, ctx, func, info);
        }
    }

    let is_ptr = info.is_pointer(recv_type);
    if is_ptr {
        let ptr_reg = compile_expr(&sel.expr, ctx, func, info)?;
        let base_type = info.pointer_base(recv_type);
        let (offset, slots) = info.selector_field_offset(expr.id, base_type, field_name);
        func.emit_ptr_get(dst, ptr_reg, offset, slots);
        return Ok(());
    }

    let base_reg = compile_expr(&sel.expr, ctx, func, info)?;
    let (offset, slots) = info.selector_field_offset(expr.id, recv_type, field_name);
    func.emit_copy(dst, base_reg + offset, slots);
    Ok(())
}

/// Result of traversing an indirect field path.
/// Contains the final location info needed to read or write the target field.
pub struct IndirectFieldResult {
    /// Register containing the base (pointer if is_ptr, value otherwise)
    pub base_reg: u16,
    /// Whether base_reg contains a pointer
    pub is_ptr: bool,
    /// Offset to the final field
    pub offset: u16,
    /// Slot count of the final field
    pub slots: u16,
}

/// Traverse an indirect field path (embedded pointer fields), generating runtime
/// pointer dereference instructions at each pointer step.
/// Returns the final location info for the target field.
pub fn traverse_indirect_field(
    sel: &vo_syntax::ast::SelectorExpr,
    indices: &[usize],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<IndirectFieldResult, CodegenError> {
    let recv_type = info.expr_type(sel.expr.id);
    
    let mut current_type = if info.is_pointer(recv_type) {
        info.pointer_base(recv_type)
    } else {
        recv_type
    };
    
    let base_reg = compile_expr(&sel.expr, ctx, func, info)?;
    let mut is_ptr = info.is_pointer(recv_type);
    let mut current_reg = base_reg;
    
    for (i, &idx) in indices.iter().enumerate() {
        let is_last = i == indices.len() - 1;
        let (field_offset, field_slots, field_type) = info.struct_field_offset_by_index_with_type(current_type, idx);
        
        if is_last {
            return Ok(IndirectFieldResult {
                base_reg: current_reg,
                is_ptr,
                offset: field_offset,
                slots: field_slots,
            });
        }
        
        let field_is_ptr = info.is_pointer(field_type);
        
        if is_ptr {
            if field_is_ptr {
                let tmp = func.alloc_temp_typed(&[vo_runtime::SlotType::GcRef]);
                func.emit_ptr_get(tmp, current_reg, field_offset, 1);
                current_reg = tmp;
                current_type = info.pointer_base(field_type);
                is_ptr = true;
            } else {
                let tmp = func.alloc_temp_typed(&info.type_slot_types(field_type));
                func.emit_ptr_get(tmp, current_reg, field_offset, field_slots);
                current_reg = tmp;
                current_type = field_type;
                is_ptr = false;
            }
        } else {
            if field_is_ptr {
                let tmp = func.alloc_temp_typed(&[vo_runtime::SlotType::GcRef]);
                func.emit_copy(tmp, current_reg + field_offset, 1);
                current_reg = tmp;
                current_type = info.pointer_base(field_type);
                is_ptr = true;
            } else {
                current_reg = current_reg + field_offset;
                current_type = field_type;
                is_ptr = false;
            }
        }
    }
    
    Err(CodegenError::Internal("traverse_indirect_field: empty indices".to_string()))
}

/// Compile selector with indirect access (embedded pointer fields).
fn compile_indirect_selector(
    sel: &vo_syntax::ast::SelectorExpr,
    indices: &[usize],
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let result = traverse_indirect_field(sel, indices, ctx, func, info)?;
    
    if result.is_ptr {
        func.emit_ptr_get(dst, result.base_reg, result.offset, result.slots);
    } else {
        func.emit_copy(dst, result.base_reg + result.offset, result.slots);
    }
    
    Ok(())
}

/// Compile method value expression (t.M where M is a method).
/// Creates a closure that captures the receiver and calls the method.
fn compile_method_value(
    _expr: &Expr,
    sel: &vo_syntax::ast::SelectorExpr,
    selection: &vo_analysis::selection::Selection,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let recv_type = info.expr_type(sel.expr.id);
    let method_name = info.project.interner.resolve(sel.sel.symbol)
        .ok_or_else(|| CodegenError::Internal("cannot resolve method name".to_string()))?;
    
    // Interface method value: capture interface, use CallIface in wrapper
    if info.is_interface(recv_type) {
        return compile_interface_method_value(sel, recv_type, method_name, dst, ctx, func, info);
    }
    
    // Use resolve_method_call - same as method call compilation
    let call_info = crate::embed::resolve_method_call(
        recv_type,
        method_name,
        sel.sel.symbol,
        Some(selection),
        false, // not interface
        ctx,
        &info.project.tc_objs,
        &info.project.interner,
    ).ok_or_else(|| CodegenError::Internal(format!("method {} not found on type {:?}", method_name, recv_type)))?;
    
    // Handle different dispatch types
    match call_info.dispatch {
        crate::embed::MethodDispatch::Static { func_id, expects_ptr_recv } => {
            return compile_method_value_static(
                sel, recv_type, func_id, expects_ptr_recv, dst, ctx, func, info
            );
        }
        crate::embed::MethodDispatch::EmbeddedInterface { embed_offset, iface_type, method_idx } => {
            return compile_method_value_embedded_iface(
                sel, recv_type, embed_offset, iface_type, method_idx, method_name, dst, ctx, func, info
            );
        }
        crate::embed::MethodDispatch::Interface { .. } => {
            return Err(CodegenError::Internal("unexpected interface dispatch in method value".to_string()));
        }
    }
}

/// Compile method value for static dispatch (direct or promoted method).
fn compile_method_value_static(
    sel: &vo_syntax::ast::SelectorExpr,
    recv_type: vo_analysis::objects::TypeKey,
    method_func_id: u32,
    expects_ptr_recv: bool,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let method_name = info.project.interner.resolve(sel.sel.symbol)
        .ok_or_else(|| CodegenError::Internal("cannot resolve method name".to_string()))?;
    
    let expr_is_ptr = info.is_pointer(recv_type);
    
    if expects_ptr_recv {
        let ptr_reg = if expr_is_ptr {
            compile_expr(&sel.expr, ctx, func, info)?
        } else {
            compile_addressable_to_ptr(&sel.expr, ctx, func, info)?
        };
        
        let wrapper_id = ctx.get_or_create_method_value_wrapper_ptr(
            recv_type,
            method_func_id,
            method_name,
            info
        )?;
        
        func.emit_closure_new(dst, wrapper_id, 1);
        func.emit_ptr_set_with_barrier(dst, 1, ptr_reg, 1, true);
    } else {
        let recv_slots = info.type_slot_count(recv_type);
        let recv_slot_types = info.type_slot_types(recv_type);
        let recv_reg = func.alloc_temp_typed(&recv_slot_types);
        compile_expr_to(&sel.expr, recv_reg, ctx, func, info)?;
        
        let meta_idx = ctx.get_or_create_value_meta(recv_type, info);
        let meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        
        let boxed_reg = func.alloc_temp_typed(&[SlotType::GcRef]);
        func.emit_with_flags(Opcode::PtrNew, recv_slots as u8, boxed_reg, meta_reg, 0);
        func.emit_ptr_set(boxed_reg, 0, recv_reg, recv_slots);
        
        let wrapper_id = ctx.get_or_create_method_value_wrapper(
            recv_type, 
            method_func_id, 
            &method_name,
            info
        )?;
        
        func.emit_closure_new(dst, wrapper_id, 1);
        func.emit_ptr_set_with_barrier(dst, 1, boxed_reg, 1, true);
    }
    
    Ok(())
}

/// Compile method value for embedded interface dispatch.
/// The outer struct contains an embedded interface field, and we're taking
/// a method value from that interface.
/// 
/// Simpler approach: extract the embedded interface value at compile time,
/// then use the existing interface method value machinery.
fn compile_method_value_embedded_iface(
    sel: &vo_syntax::ast::SelectorExpr,
    recv_type: vo_analysis::objects::TypeKey,
    embed_offset: u16,
    iface_type: vo_analysis::objects::TypeKey,
    method_idx: u32,
    method_name: &str,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Compile the receiver expression
    let recv_slots = info.type_slot_count(recv_type);
    let recv_slot_types = info.type_slot_types(recv_type);
    let recv_reg = func.alloc_temp_typed(&recv_slot_types);
    compile_expr_to(&sel.expr, recv_reg, ctx, func, info)?;
    
    // Extract the embedded interface (2 slots) from the receiver at embed_offset
    let iface_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
    func.emit_copy(iface_reg, recv_reg + embed_offset, 2);
    
    // Get method signature slots from interface type
    let (param_slots, ret_slots) = info.get_interface_method_slots(iface_type, method_name)
        .ok_or_else(|| CodegenError::Internal(format!(
            "method {} not found on interface {:?}", method_name, iface_type
        )))?;
    
    // Use existing interface method value wrapper
    let wrapper_id = ctx.get_or_create_method_value_wrapper_iface(
        method_idx,
        param_slots,
        ret_slots,
        method_name,
    )?;
    
    // Create closure with 2 captures (interface slot0 + data)
    func.emit_closure_new(dst, wrapper_id, 2);
    func.emit_ptr_set_with_barrier(dst, 1, iface_reg, 2, true);
    
    Ok(())
}

/// Compile interface method value: iface.Method
/// Creates a closure that captures the interface value and calls via itab dispatch.
fn compile_interface_method_value(
    sel: &vo_syntax::ast::SelectorExpr,
    recv_type: vo_analysis::objects::TypeKey,
    method_name: &str,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Get method index for this interface type
    let method_idx = ctx.get_interface_method_index(
        recv_type, 
        method_name, 
        &info.project.tc_objs, 
        &info.project.interner
    );
    
    // Get method signature slots from interface type
    let (param_slots, ret_slots) = info.get_interface_method_slots(recv_type, method_name)
        .ok_or_else(|| CodegenError::Internal(format!(
            "method {} not found on interface {:?}", method_name, recv_type
        )))?;
    
    // Compile interface expression (2 slots: slot0 + data)
    let iface_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
    compile_expr_to(&sel.expr, iface_reg, ctx, func, info)?;
    
    // Get or create wrapper function that uses CallIface
    let wrapper_id = ctx.get_or_create_method_value_wrapper_iface(
        method_idx,
        param_slots,
        ret_slots,
        method_name,
    )?;
    
    // Create closure with 2 captures (interface slot0 + data)
    func.emit_closure_new(dst, wrapper_id, 2);
    // Set captures at offset 1 (after ClosureHeader)
    func.emit_ptr_set_with_barrier(dst, 1, iface_reg, 2, true);
    
    Ok(())
}

/// Compile method expression (T.M or (*T).M).
/// Returns a function where the receiver becomes the first parameter.
/// Unlike method value, method expression does not capture a receiver.
fn compile_method_expr(
    _expr: &Expr,
    sel: &vo_syntax::ast::SelectorExpr,
    selection: &vo_analysis::selection::Selection,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let recv_type = selection.recv().ok_or_else(|| {
        CodegenError::Internal("method expression has no receiver type".to_string())
    })?;
    let method_name = info.project.interner.resolve(sel.sel.symbol)
        .ok_or_else(|| CodegenError::Internal("cannot resolve method name".to_string()))?;
    
    let call_info = crate::embed::resolve_method_call(
        recv_type,
        method_name,
        sel.sel.symbol,
        Some(selection),
        false,
        ctx,
        &info.project.tc_objs,
        &info.project.interner,
    ).ok_or_else(|| CodegenError::Internal(format!(
        "method {} not found on type {:?}", method_name, recv_type
    )))?;
    
    let (method_func_id, expects_ptr_recv) = match call_info.dispatch {
        crate::embed::MethodDispatch::Static { func_id, expects_ptr_recv } => (func_id, expects_ptr_recv),
        _ => return Err(CodegenError::Internal("method expression requires static dispatch".to_string())),
    };
    
    // For promoted methods (embedding path is not empty), generate a wrapper
    let final_func_id = if !call_info.embed_path.steps.is_empty() {
        // Get the base type (strip pointer if recv_type is *T)
        let base_type = if vo_analysis::check::type_info::is_pointer(recv_type, &info.project.tc_objs) {
            let underlying = vo_analysis::typ::underlying_type(recv_type, &info.project.tc_objs);
            if let vo_analysis::typ::Type::Pointer(p) = &info.project.tc_objs.types[underlying] {
                p.base()
            } else {
                recv_type
            }
        } else {
            recv_type
        };
        
        crate::wrapper::generate_method_expr_promoted_wrapper(
            ctx,
            base_type,
            &call_info.embed_path,
            method_func_id,
            expects_ptr_recv,
            call_info.recv_is_pointer,
            method_name,
            &info.project.tc_objs,
        )
    } else {
        method_func_id
    };
    
    func.emit_closure_new(dst, final_func_id, 0);
    Ok(())
}

fn compile_pkg_qualified_name(
    expr: &Expr,
    sel: &vo_syntax::ast::SelectorExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let obj_key = info.get_use(&sel.sel);
    let lobj = &info.project.tc_objs.lobjs[obj_key];
    
    match lobj.entity_type() {
        vo_analysis::obj::EntityType::Const { val } => {
            let type_key = info.expr_type(expr.id);
            compile_const_value(val, dst, type_key, ctx, func, info)
        }
        vo_analysis::obj::EntityType::Var(_) => {
            // Use ObjKey to avoid cross-package Symbol collision
            if let Some(global_idx) = ctx.get_global_index(obj_key) {
                let type_key = info.expr_type(expr.id);
                let slots = info.type_slot_count(type_key);
                if slots == 1 {
                    func.emit_op(Opcode::GlobalGet, dst, global_idx as u16, 0);
                } else {
                    func.emit_with_flags(Opcode::GlobalGetN, slots as u8, dst, global_idx as u16, 0);
                }
                Ok(())
            } else {
                Err(CodegenError::Internal(format!("pkg var not registered: {:?}", sel.sel.symbol)))
            }
        }
        vo_analysis::obj::EntityType::Func { has_body, .. } => {
            if *has_body {
                // Vo function - create closure reference
                // Use ObjKey to avoid cross-package Symbol collision
                if let Some(func_idx) = ctx.get_func_by_objkey(obj_key) {
                    func.emit_closure_new(dst, func_idx, 0);
                    Ok(())
                } else {
                    Err(CodegenError::Internal(format!("pkg func not registered: {:?}", sel.sel.symbol)))
                }
            } else {
                // Extern function - cannot be used as value
                let func_name = info.project.interner.resolve(sel.sel.symbol).unwrap_or("?");
                Err(CodegenError::UnsupportedExpr(format!("extern function {} cannot be used as value", func_name)))
            }
        }
        _ => Err(CodegenError::UnsupportedExpr("unsupported pkg member".to_string())),
    }
}

fn compile_index(
    expr: &Expr,
    idx: &vo_syntax::ast::IndexExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let container_type = info.expr_type(idx.expr.id);
    
    if info.is_map(container_type) {
        let map_reg = compile_expr(&idx.expr, ctx, func, info)?;
        let (key_slots, val_slots) = info.map_key_val_slots(container_type);
        let (key_type, _) = info.map_key_val_types(container_type);
        let key_reg = compile_map_key_expr(&idx.index, key_type, ctx, func, info)?;
        let result_type = info.expr_type(expr.id);
        let is_comma_ok = info.is_tuple(result_type);
        let meta = crate::type_info::encode_map_get_meta(key_slots, val_slots, is_comma_ok);
        let mut map_get_slot_types = vec![SlotType::Value]; // meta
        map_get_slot_types.extend(info.type_slot_types(key_type)); // key
        let meta_reg = func.alloc_temp_typed(&map_get_slot_types);
        let meta_idx = ctx.const_int(meta as i64);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        func.emit_copy(meta_reg + 1, key_reg, key_slots);
        func.emit_op(Opcode::MapGet, dst, map_reg, meta_reg);
    } else {
        let lv = crate::lvalue::resolve_lvalue(expr, ctx, func, info)?;
        crate::lvalue::emit_lvalue_load(&lv, dst, ctx, func);
    }
    Ok(())
}

fn compile_slice_expr(
    _expr: &Expr,
    slice_expr: &vo_syntax::ast::SliceExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use crate::type_info::encode_i32;
    
    let container_type = info.expr_type(slice_expr.expr.id);
    let has_max = slice_expr.max.is_some();
    
    // Compile container
    let container_reg = compile_expr(&slice_expr.expr, ctx, func, info)?;
    
    // Compile lo bound (default 0)
    let lo_reg = if let Some(lo) = &slice_expr.low {
        compile_expr(lo, ctx, func, info)?
    } else {
        let tmp = func.alloc_temp_typed(&[SlotType::Value]);
        func.emit_op(Opcode::LoadInt, tmp, 0, 0);
        tmp
    };
    
    // Compile hi bound (default len)
    let hi_reg = if let Some(hi) = &slice_expr.high {
        compile_expr(hi, ctx, func, info)?
    } else {
        let tmp = func.alloc_temp_typed(&[SlotType::Value]);
        if info.is_string(container_type) {
            func.emit_op(Opcode::StrLen, tmp, container_reg, 0);
        } else if info.is_slice(container_type) {
            func.emit_op(Opcode::SliceLen, tmp, container_reg, 0);
        } else if info.is_array(container_type) {
            let len = info.array_len(container_type) as i32;
            let (b, c) = encode_i32(len);
            func.emit_op(Opcode::LoadInt, tmp, b, c);
        } else {
            func.emit_op(Opcode::LoadInt, tmp, 0, 0);
        }
        tmp
    };
    
    // Compile max bound for three-index slice (default: no limit, use cap)
    let max_reg = if let Some(max) = &slice_expr.max {
        Some(compile_expr(max, ctx, func, info)?)
    } else {
        None
    };
    
    // Prepare params: slots[c]=lo, slots[c+1]=hi, slots[c+2]=max (if present)
    let param_count = if has_max { 3 } else { 2 };
    let params_start = func.alloc_temp_typed(&vec![SlotType::Value; param_count as usize]);
    func.emit_op(Opcode::Copy, params_start, lo_reg, 0);
    func.emit_op(Opcode::Copy, params_start + 1, hi_reg, 0);
    if let Some(max_r) = max_reg {
        func.emit_op(Opcode::Copy, params_start + 2, max_r, 0);
    }
    
    // flags encoding:
    //   bit0: 1 = input is array (not slice)
    //   bit1: 1 = has max (three-index slice)
    let flags_has_max = if has_max { 0b10 } else { 0 };
    
    if info.is_string(container_type) {
        // StrSlice: a=dst, b=str, c=params_start (strings don't support 3-index)
        func.emit_op(Opcode::StrSlice, dst, container_reg, params_start);
    } else if info.is_slice(container_type) {
        // SliceSlice: a=dst, b=slice, c=params_start
        func.emit_with_flags(Opcode::SliceSlice, flags_has_max, dst, container_reg, params_start);
    } else if info.is_array(container_type) {
        // Array slicing creates a slice - the array MUST be escaped
        let flags = 0b01 | flags_has_max; // bit0=1 for array
        if let Some(gcref_slot) = get_escaped_var_gcref(&slice_expr.expr, ctx, func, info) {
            func.emit_with_flags(Opcode::SliceSlice, flags, dst, gcref_slot, params_start);
        } else {
            func.emit_with_flags(Opcode::SliceSlice, flags, dst, container_reg, params_start);
        }
    } else {
        return Err(CodegenError::Internal("slice on unsupported type".to_string()));
    }
    
    Ok(())
}

fn compile_type_assert(
    expr: &Expr,
    type_assert: &vo_syntax::ast::TypeAssertExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let iface_reg = compile_expr(&type_assert.expr, ctx, func, info)?;
    let target_type = type_assert.ty.as_ref()
        .map(|ty| info.type_expr_type(ty.id))
        .expect("type assertion must have target type");
    let target_slots = info.type_slot_count(target_type) as u8;
    let result_type = info.expr_type(expr.id);
    let has_ok = info.is_tuple(result_type);

    let (assert_kind, target_id): (u8, u32) = if info.is_interface(target_type) {
        let iface_meta_id = info.get_or_create_interface_meta_id(target_type, ctx);
        (1, iface_meta_id)
    } else {
        let rt = info.type_to_runtime_type(target_type, ctx);
        let rttid = ctx.intern_rttid(rt);
        (0, rttid)
    };

    let flags = assert_kind | (if has_ok { 1 << 2 } else { 0 }) | ((target_slots) << 3);
    func.emit_with_flags(Opcode::IfaceAssert, flags, dst, iface_reg, target_id as u16);
    Ok(())
}

fn compile_receive(
    expr: &Expr,
    chan_expr: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let chan_reg = compile_expr(chan_expr, ctx, func, info)?;
    let chan_type = info.expr_type(chan_expr.id);
    let elem_slots = info.chan_elem_slots(chan_type);
    let result_slots = info.expr_slots(expr.id);
    let has_ok = result_slots > elem_slots;
    let flags = ((elem_slots as u8) << 1) | (if has_ok { 1 } else { 0 });
    func.emit_with_flags(Opcode::ChanRecv, flags, dst, chan_reg, 0);
    Ok(())
}

fn compile_try_unwrap(
    inner: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let inner_type = info.expr_type(inner.id);
    let inner_slots = info.type_slot_count(inner_type);
    let inner_slot_types = info.type_slot_types(inner_type);
    let inner_start = func.alloc_temp_typed(&inner_slot_types);
    compile_expr_to(inner, inner_start, ctx, func, info)?;
    
    let error_slots = 2u16;
    let error_start = inner_start + inner_slots - error_slots;
    let skip_fail_jump = func.emit_jump(Opcode::JumpIfNot, error_start);
    
    // Check if function returns error - determines propagate vs panic behavior
    if func.has_error_return(info) {
        // Propagate mode: return zero values with error
        let ret_types: Vec<_> = func.return_types().to_vec();
        let mut total_ret_slots = 0u16;
        for ret_type in &ret_types {
            total_ret_slots += info.type_slot_count(*ret_type);
        }
        
        let mut ret_slot_types = Vec::new();
        for ret_type in &ret_types {
            ret_slot_types.extend(info.type_slot_types(*ret_type));
        }
        let ret_start = func.alloc_temp_typed(&ret_slot_types);
        for i in 0..total_ret_slots {
            func.emit_op(Opcode::LoadInt, ret_start + i, 0, 0);
        }
        
        if !ret_types.is_empty() {
            let ret_error_slots = info.type_slot_count(*ret_types.last().unwrap());
            let ret_error_start = ret_start + total_ret_slots - ret_error_slots;
            func.emit_copy(ret_error_start, error_start, ret_error_slots);
        }
        
        func.emit_with_flags(Opcode::Return, 1, ret_start, total_ret_slots, 0);
    } else {
        // Panic mode: panic with error directly
        let panic_extern = ctx.get_or_register_extern("panic_with_error");
        func.emit_with_flags(Opcode::CallExtern, 2, error_start, panic_extern as u16, error_start);
    }
    
    func.patch_jump(skip_fail_jump, func.current_pc());
    
    let value_slots = inner_slots - error_slots;
    if value_slots > 0 {
        func.emit_copy(dst, inner_start, value_slots);
    }
    Ok(())
}

/// Get pointer to an addressable expression, returning the register containing the pointer.
/// Used for implicit address-of when calling pointer receiver methods on values.
fn compile_addressable_to_ptr(
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let dst = func.alloc_temp_typed(&[SlotType::GcRef]);
    compile_addr_of(expr, dst, ctx, func, info)?;
    Ok(dst)
}

/// Get the GcRef and offset for an addressable expression.
/// 
/// Returns (gcref_slot, total_offset) if the expression is addressable and on heap.
/// Used for:
/// - Taking address of variables: &x
/// - Taking address of struct fields: &outer.field
/// - Passing pointer receiver to methods: x.Method() where Method has *T receiver
/// 
/// Handles: Ident, Selector (nested field access)
pub(crate) fn get_addressable_gcref(
    expr: &Expr,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> Option<(u16, u16)> {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            let local = func.lookup_local(ident.symbol)?;
            let gcref = get_gcref_slot(&local.storage)?;
            Some((gcref, 0))
        }
        ExprKind::Selector(sel) => {
            let recv_type = info.expr_type(sel.expr.id);
            // Pointer type: need to compile and dereference, not addressable this way
            if info.is_pointer(recv_type) {
                return None;
            }
            let (gcref_slot, base_offset) = get_addressable_gcref(&sel.expr, func, info)?;
            let field_name = info.project.interner.resolve(sel.sel.symbol)?;
            let (field_offset, _) = info.struct_field_offset(recv_type, field_name);
            Some((gcref_slot, base_offset + field_offset))
        }
        ExprKind::Paren(inner) => get_addressable_gcref(inner, func, info),
        _ => None,
    }
}

/// Get pointer to an expression. Unified handler for:
/// - Method receiver when expects_ptr_recv=true
/// - Address-of operator (&x)
/// 
/// Handles all cases where we need a pointer to an expression's value.
pub(crate) fn compile_expr_to_ptr(
    expr: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let expr_type = info.expr_type(expr.id);
    
    // Case 1: Expression is already pointer type → just compile it
    if info.is_pointer(expr_type) {
        let reg = compile_expr(expr, ctx, func, info)?;
        func.emit_copy(dst, reg, 1);
        return Ok(());
    }
    
    // Case 2: Index expression → get element address
    if let ExprKind::Index(index_expr) = &expr.kind {
        let container_type = info.expr_type(index_expr.expr.id);
        if info.is_slice(container_type) || info.is_array(container_type) {
            return crate::lvalue::compile_index_addr(&index_expr.expr, &index_expr.index, dst, ctx, func, info);
        }
    }
    
    // Case 3: Selector on pointer base (c2.pt where c2: *Container, pt: Point)
    if let ExprKind::Selector(sel) = &expr.kind {
        let base_type = info.expr_type(sel.expr.id);
        if info.is_pointer(base_type) {
            let field_name = info.project.interner.resolve(sel.sel.symbol)
                .ok_or_else(|| CodegenError::Internal("cannot resolve field name".to_string()))?;
            let ptr_base = info.pointer_base(base_type);
            let (field_offset, _) = info.struct_field_offset(ptr_base, field_name);
            if field_offset == 0 {
                let ptr_reg = compile_expr(&sel.expr, ctx, func, info)?;
                func.emit_copy(dst, ptr_reg, 1);
                return Ok(());
            }
            return Err(CodegenError::Internal(
                format!("cannot take address of field at non-zero offset {} in pointer-based access", field_offset)
            ));
        }
    }
    
    // Case 4: Captured variable in closure → get GcRef via ClosureGet
    if let ExprKind::Ident(ident) = &expr.kind {
        if let Some(capture) = func.lookup_capture(ident.symbol) {
            // Captured variables are stored as GcRef in closure environment
            // ClosureGet retrieves the GcRef which IS the pointer we need
            func.emit_op(Opcode::ClosureGet, dst, capture.index, 0);
            return Ok(());
        }
    }
    
    // Case 5: Addressable on heap (variable, field at offset 0)
    if let Some((gcref_slot, offset)) = get_addressable_gcref(expr, func, info) {
        if offset == 0 {
            func.emit_copy(dst, gcref_slot, 1);
            return Ok(());
        }
        return Err(CodegenError::Internal(
            format!("cannot take address of field at non-zero offset {}", offset)
        ));
    }
    
    Err(CodegenError::UnsupportedExpr(
        format!("cannot get pointer to expression")
    ))
}

/// Compile address-of operator (&x).
/// Spec: only struct types can have their address taken.
fn compile_addr_of(
    operand: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Case 1: &CompositeLit{} - allocate struct on heap (special case, not in compile_expr_to_ptr)
    if let ExprKind::CompositeLit(lit) = &operand.kind {
        let type_key = info.expr_type(operand.id);
        let slots = info.type_slot_count(type_key);
        let meta_idx = ctx.get_or_create_value_meta(type_key, info);
        let meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        func.emit_with_flags(Opcode::PtrNew, slots as u8, dst, meta_reg, 0);
        
        for (i, elem) in lit.elems.iter().enumerate() {
            let (offset, field_slots, field_type) = if let Some(key) = &elem.key {
                if let vo_syntax::ast::CompositeLitKey::Ident(field_ident) = key {
                    let field_name = info.project.interner.resolve(field_ident.symbol)
                        .ok_or_else(|| CodegenError::Internal("cannot resolve field name".to_string()))?;
                    info.struct_field_offset_with_type(type_key, field_name)
                } else {
                    continue;
                }
            } else {
                info.struct_field_offset_by_index_with_type(type_key, i)
            };
            
            if info.is_interface(field_type) {
                let field_slot_types = info.type_slot_types(field_type);
                let tmp = func.alloc_temp_typed(&field_slot_types);
                crate::stmt::compile_value_to(&elem.value, tmp, field_type, ctx, func, info)?;
                func.emit_ptr_set_with_barrier(dst, offset, tmp, field_slots, true);
            } else {
                let may_gc_ref = info.type_value_kind(field_type).may_contain_gc_refs();
                let field_slot_types = info.type_slot_types(field_type);
                let tmp = func.alloc_temp_typed(&field_slot_types);
                compile_expr_to(&elem.value, tmp, ctx, func, info)?;
                func.emit_ptr_set_with_barrier(dst, offset, tmp, field_slots, may_gc_ref);
            }
        }
        return Ok(());
    }
    
    // All other cases: delegate to compile_expr_to_ptr
    compile_expr_to_ptr(operand, dst, ctx, func, info)
}

fn compile_deref(
    operand: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let ptr_reg = compile_expr(operand, ctx, func, info)?;
    let ptr_type = info.expr_type(operand.id);
    let elem_slots = info.pointer_elem_slots(ptr_type);
    func.emit_ptr_get(dst, ptr_reg, 0, elem_slots);
    Ok(())
}

/// Compile array/struct comparison (==, !=) by comparing all slots.
fn compile_composite_comparison(
    op: &BinaryOp,
    left: &Expr,
    right: &Expr,
    composite_type: vo_analysis::objects::TypeKey,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let slot_types = info.type_slot_types(composite_type);
    let slot_vks = info.type_slot_value_kinds(composite_type);
    let left_reg = compile_expr(left, ctx, func, info)?;
    let right_reg = compile_expr(right, ctx, func, info)?;
    compile_slot_comparison(op, left_reg, right_reg, &slot_types, &slot_vks, dst, func)
}

/// Unified slot-by-slot comparison for array/struct.
fn compile_slot_comparison(
    op: &BinaryOp,
    left_reg: u16,
    right_reg: u16,
    slot_types: &[SlotType],
    slot_vks: &[vo_runtime::ValueKind],
    dst: u16,
    func: &mut FuncBuilder,
) -> Result<(), CodegenError> {
    let total_slots = slot_types.len() as u16;
    
    if total_slots == 0 {
        func.emit_op(Opcode::LoadInt, dst, 1, 0);
        if *op == BinaryOp::NotEq {
            func.emit_op(Opcode::BoolNot, dst, dst, 0);
        }
        return Ok(());
    }
    
    // Reusable temp registers
    let left_val = func.alloc_temp_typed(&[SlotType::Value]);
    let right_val = func.alloc_temp_typed(&[SlotType::Value]);
    let idx_reg = func.alloc_temp_typed(&[SlotType::Value]);
    let tmp_cmp = func.alloc_temp_typed(&[SlotType::Value]);
    
    func.emit_op(Opcode::LoadInt, dst, 1, 0);
    
    let mut i = 0u16;
    while i < total_slots {
        func.emit_op(Opcode::LoadInt, idx_reg, i, 0);
        func.emit_op(Opcode::SlotGet, left_val, left_reg, idx_reg);
        func.emit_op(Opcode::SlotGet, right_val, right_reg, idx_reg);
        
        match slot_types[i as usize] {
            SlotType::Interface0 => {
                // Interface: load both slots, use IfaceEq
                let left_iface = func.alloc_interfaces(1);
                let right_iface = func.alloc_interfaces(1);
                
                func.emit_op(Opcode::Copy, left_iface, left_val, 0);
                func.emit_op(Opcode::Copy, right_iface, right_val, 0);
                
                func.emit_op(Opcode::LoadInt, idx_reg, i + 1, 0);
                func.emit_op(Opcode::SlotGet, left_val, left_reg, idx_reg);
                func.emit_op(Opcode::SlotGet, right_val, right_reg, idx_reg);
                func.emit_op(Opcode::Copy, left_iface + 1, left_val, 0);
                func.emit_op(Opcode::Copy, right_iface + 1, right_val, 0);
                
                func.emit_op(Opcode::IfaceEq, tmp_cmp, left_iface, right_iface);
                func.emit_op(Opcode::And, dst, dst, tmp_cmp);
                i += 2;
            }
            SlotType::Interface1 => {
                i += 1;
            }
            SlotType::GcRef => {
                // Check if this is a string
                let vk = slot_vks.get(i as usize).copied().unwrap_or(vo_runtime::ValueKind::Void);
                let cmp_op = if vk == vo_runtime::ValueKind::String {
                    Opcode::StrEq
                } else {
                    Opcode::EqI
                };
                func.emit_op(cmp_op, tmp_cmp, left_val, right_val);
                func.emit_op(Opcode::And, dst, dst, tmp_cmp);
                i += 1;
            }
            SlotType::Value => {
                func.emit_op(Opcode::EqI, tmp_cmp, left_val, right_val);
                func.emit_op(Opcode::And, dst, dst, tmp_cmp);
                i += 1;
            }
        }
    }
    
    if *op == BinaryOp::NotEq {
        func.emit_op(Opcode::BoolNot, dst, dst, 0);
    }
    
    Ok(())
}

/// Emit truncation for narrow integer types (int8/16/32, uint8/16/32).
/// This ensures Go semantics where operations are done in 64-bit but
/// results are truncated to the target type width.
/// 
/// On 32-bit platforms, Int and Uint are also truncated to 32 bits.
/// 
/// Safe to call on any type - non-narrow-integer types are no-ops.
pub fn emit_int_trunc(
    reg: u16,
    type_key: vo_analysis::objects::TypeKey,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) {
    if !info.is_int(type_key) {
        return;
    }
    use vo_runtime::ValueKind;
    let vk = info.type_value_kind(type_key);
    // flags: high bit (0x80) = signed, low bits = byte width
    match vk {
        ValueKind::Int8 => func.emit_with_flags(Opcode::Trunc, 0x81, reg, reg, 0),
        ValueKind::Int16 => func.emit_with_flags(Opcode::Trunc, 0x82, reg, reg, 0),
        ValueKind::Int32 => func.emit_with_flags(Opcode::Trunc, 0x84, reg, reg, 0),
        ValueKind::Uint8 => func.emit_with_flags(Opcode::Trunc, 0x01, reg, reg, 0),
        ValueKind::Uint16 => func.emit_with_flags(Opcode::Trunc, 0x02, reg, reg, 0),
        ValueKind::Uint32 => func.emit_with_flags(Opcode::Trunc, 0x04, reg, reg, 0),
        // On 32-bit platforms, Int and Uint are 32-bit and need truncation
        #[cfg(target_pointer_width = "32")]
        ValueKind::Int => func.emit_with_flags(Opcode::Trunc, 0x84, reg, reg, 0),
        #[cfg(target_pointer_width = "32")]
        ValueKind::Uint => func.emit_with_flags(Opcode::Trunc, 0x04, reg, reg, 0),
        _ => {} // No truncation needed for Int64, Uint64, and Int/Uint on 64-bit
    }
}
