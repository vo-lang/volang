//! Selector expression compilation.
//!
//! Handles field access (x.field), package-qualified names (pkg.Name),
//! and indirect selection through embedded pointer fields.

use vo_syntax::ast::{Expr, ExprKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{ExprSource, FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

use super::{compile_expr, get_expr_source};
use super::literal::compile_const_value;

/// Check if a selector expression is a package-qualified name (e.g., task.PriorityHigh)
pub fn is_pkg_qualified_name(sel: &vo_syntax::ast::SelectorExpr, info: &TypeInfoWrapper) -> bool {
    if let ExprKind::Ident(ident) = &sel.expr.kind {
        let obj = info.get_use(ident);
        info.obj_is_pkg(obj)
    } else {
        false
    }
}

/// Compile a selector expression.
pub fn compile_selector(
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
                return super::method_value::compile_method_value(sel, selection, dst, ctx, func, info);
            }
            vo_analysis::selection::SelectionKind::MethodExpr => {
                return super::method_value::compile_method_expr(expr, sel, selection, dst, ctx, func, info);
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
    
    // Determine initial state based on storage kind
    // HeapBoxed/Reference: GcRef slot acts as pointer, skip loading entire struct
    let base_source = get_expr_source(&sel.expr, ctx, func, info);
    let (mut current_reg, mut is_ptr, mut accumulated_offset) = match &base_source {
        ExprSource::Location(StorageKind::HeapBoxed { gcref_slot, stores_pointer, .. }) => {
            if *stores_pointer {
                // HeapBoxed stores a pointer - read pointer first, then access fields
                let ptr_reg = func.alloc_slots(&[vo_runtime::SlotType::GcRef]);
                func.emit_ptr_get(ptr_reg, *gcref_slot, 0, 1);
                (ptr_reg, true, 0u16)
            } else {
                // HeapBoxed stores value directly - gcref_slot acts as pointer to data
                (*gcref_slot, true, 0u16)
            }
        }
        ExprSource::Location(StorageKind::Reference { slot: gcref_slot }) => {
            (*gcref_slot, true, 0u16)
        }
        _ => {
            let base_reg = compile_expr(&sel.expr, ctx, func, info)?;
            (base_reg, info.is_pointer(recv_type), 0u16)
        }
    };
    
    for (i, &idx) in indices.iter().enumerate() {
        let (field_offset, field_slots, field_type) = info.struct_field_offset_by_index_with_type(current_type, idx);
        
        if i == indices.len() - 1 {
            return Ok(IndirectFieldResult {
                base_reg: current_reg,
                is_ptr,
                offset: accumulated_offset + field_offset,
                slots: field_slots,
            });
        }
        
        if info.is_pointer(field_type) {
            // Pointer field: load pointer value, reset offset
            let tmp = func.alloc_slots(&[vo_runtime::SlotType::GcRef]);
            if is_ptr {
                func.emit_ptr_get(tmp, current_reg, accumulated_offset + field_offset, 1);
            } else {
                func.emit_copy(tmp, current_reg + accumulated_offset + field_offset, 1);
            }
            current_reg = tmp;
            current_type = info.pointer_base(field_type);
            accumulated_offset = 0;
            is_ptr = true;
        } else {
            // Value field: accumulate offset
            accumulated_offset += field_offset;
            current_type = field_type;
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

/// Compile package-qualified name (e.g., pkg.Const, pkg.Var, pkg.Func).
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
