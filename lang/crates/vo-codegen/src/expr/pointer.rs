//! Pointer and address-related expression compilation.
//!
//! Handles address-of (&x), dereference (*x), and getting pointers to expressions.

use vo_runtime::SlotType;
use vo_syntax::ast::{Expr, ExprKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

use super::{compile_expr, compile_expr_to, get_gcref_slot};

/// Get the GcRef and offset for an addressable expression.
/// 
/// Returns (gcref_slot, total_offset) if the expression is addressable and on heap.
/// Used for:
/// - Taking address of variables: &x
/// - Taking address of struct fields: &outer.field
/// - Passing pointer receiver to methods: x.Method() where Method has *T receiver
/// 
/// Handles: Ident, Selector (nested field access)
pub fn get_addressable_gcref(
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
pub fn compile_expr_to_ptr(
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
pub fn compile_addr_of(
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
        let meta_reg = func.alloc_slots(&[SlotType::Value]);
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
                let tmp = func.alloc_slots(&field_slot_types);
                crate::assign::emit_assign(tmp, crate::assign::AssignSource::Expr(&elem.value), field_type, ctx, func, info)?;
                func.emit_ptr_set_with_barrier(dst, offset, tmp, field_slots, true);
            } else {
                let may_gc_ref = info.type_value_kind(field_type).may_contain_gc_refs();
                let field_slot_types = info.type_slot_types(field_type);
                let tmp = func.alloc_slots(&field_slot_types);
                compile_expr_to(&elem.value, tmp, ctx, func, info)?;
                func.emit_ptr_set_with_barrier(dst, offset, tmp, field_slots, may_gc_ref);
            }
        }
        return Ok(());
    }
    
    // All other cases: delegate to compile_expr_to_ptr
    compile_expr_to_ptr(operand, dst, ctx, func, info)
}

/// Compile dereference operator (*x).
pub fn compile_deref(
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

