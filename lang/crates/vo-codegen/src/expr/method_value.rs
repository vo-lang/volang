//! Method value and method expression compilation.
//!
//! - Method value: `t.M` where M is a method, creates a closure capturing the receiver
//! - Method expression: `T.M` or `(*T).M`, returns a function with receiver as first param

use vo_runtime::SlotType;
use vo_syntax::ast::Expr;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

use super::{compile_expr, compile_expr_to};

/// Compile method value expression (t.M where M is a method).
/// Creates a closure that captures the receiver and calls the method.
pub fn compile_method_value(
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

    let (method_recv_slots, method_param_slots, method_ret_slots) =
        info.method_signature_slots(selection.obj())
            .ok_or_else(|| CodegenError::Internal("cannot get method signature slots".to_string()))?;
    
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
                sel,
                recv_type,
                func_id,
                expects_ptr_recv,
                &call_info.embed_path,
                dst,
                ctx,
                func,
                info,
                method_recv_slots,
                method_param_slots,
                method_ret_slots,
            );
        }
        crate::embed::MethodDispatch::EmbeddedInterface { .. } => {
            return compile_method_value_embedded_iface(
                sel, recv_type, &call_info, method_name, dst, ctx, func, info
            );
        }
        crate::embed::MethodDispatch::Interface { .. } => {
            return Err(CodegenError::Internal("unexpected interface dispatch in method value".to_string()));
        }
    }
}

/// Box a value into a heap-allocated pointer.
/// Returns the register holding the boxed pointer.
fn emit_box_value(
    reg: u16,
    slots: u16,
    value_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> u16 {
    let meta_idx = ctx.get_or_create_value_meta(value_type, info);
    let meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
    func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
    let boxed = func.alloc_temp_typed(&[SlotType::GcRef]);
    func.emit_with_flags(Opcode::PtrNew, slots as u8, boxed, meta_reg, 0);
    func.emit_ptr_set(boxed, 0, reg, slots);
    boxed
}

/// Compile method value for static dispatch (direct or promoted method).
/// 
/// Method values capture the receiver in a closure. The capture strategy depends on
/// whether the method expects a pointer or value receiver:
/// - Pointer receiver: capture the pointer directly
/// - Value receiver: box the value and capture the box (so wrapper can deref)
fn compile_method_value_static(
    sel: &vo_syntax::ast::SelectorExpr,
    recv_type: vo_analysis::objects::TypeKey,
    method_func_id: u32,
    expects_ptr_recv: bool,
    embed_path: &crate::embed::EmbedPathInfo,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
    method_recv_slots: u16,
    method_param_slots: u16,
    method_ret_slots: u16,
) -> Result<(), CodegenError> {
    let recv = crate::embed::extract_receiver(&sel.expr, recv_type, embed_path, expects_ptr_recv, ctx, func, info)?;
    
    // Convert receiver to pointer (box if needed)
    let (ptr_reg, capture_type) = match recv {
        crate::embed::ReceiverValue::Pointer { reg, pointee_type } => (reg, pointee_type),
        crate::embed::ReceiverValue::Value { reg, value_type, slots } => {
            let boxed = emit_box_value(reg, slots, value_type, ctx, func, info);
            (boxed, value_type)
        }
    };
    
    // Create wrapper: needs_deref = !expects_ptr_recv (value receiver needs unboxing)
    let wrapper_id = ctx.get_or_create_method_value_wrapper(
        capture_type, method_func_id,
        !expects_ptr_recv, // needs_deref
        method_recv_slots, method_param_slots, method_ret_slots,
    )?;
    
    func.emit_closure_new(dst, wrapper_id, 1);
    func.emit_ptr_set_with_barrier(dst, 1, ptr_reg, 1, true);
    Ok(())
}

/// Common logic for creating interface method value closure.
/// Both direct interface and embedded interface method values use this.
fn emit_iface_method_value_closure(
    iface_type: vo_analysis::objects::TypeKey,
    method_idx: u32,
    method_name: &str,
    iface_reg: u16,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let (param_slots, ret_slots) = info.get_interface_method_slots(iface_type, method_name)
        .ok_or_else(|| CodegenError::Internal(format!(
            "method {} not found on interface {:?}", method_name, iface_type
        )))?;
    
    let wrapper_id = ctx.get_or_create_method_value_wrapper_iface(
        method_idx, param_slots, ret_slots, method_name,
    )?;
    
    func.emit_closure_new(dst, wrapper_id, 2);
    func.emit_ptr_set_with_barrier(dst, 1, iface_reg, 2, true);
    Ok(())
}

/// Compile method value for embedded interface dispatch.
fn compile_method_value_embedded_iface(
    sel: &vo_syntax::ast::SelectorExpr,
    recv_type: vo_analysis::objects::TypeKey,
    call_info: &crate::embed::MethodCallInfo,
    method_name: &str,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let (iface_type, method_idx) = match &call_info.dispatch {
        crate::embed::MethodDispatch::EmbeddedInterface { iface_type, method_idx } => (*iface_type, *method_idx),
        _ => return Err(CodegenError::Internal("expected EmbeddedInterface dispatch".to_string())),
    };
    
    let recv_is_ptr = info.is_pointer(recv_type);
    let recv_reg = compile_expr(&sel.expr, ctx, func, info)?;
    let iface_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
    let start = crate::embed::TraverseStart::new(recv_reg, recv_is_ptr);
    call_info.emit_target(func, start, iface_reg);
    
    emit_iface_method_value_closure(iface_type, method_idx, method_name, iface_reg, dst, ctx, func, info)
}

/// Compile interface method value: iface.Method
fn compile_interface_method_value(
    sel: &vo_syntax::ast::SelectorExpr,
    recv_type: vo_analysis::objects::TypeKey,
    method_name: &str,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let method_idx = ctx.get_interface_method_index(
        recv_type, method_name, &info.project.tc_objs, &info.project.interner
    );
    
    let iface_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
    compile_expr_to(&sel.expr, iface_reg, ctx, func, info)?;
    
    emit_iface_method_value_closure(recv_type, method_idx, method_name, iface_reg, dst, ctx, func, info)
}

/// Compile method expression (T.M or (*T).M).
/// Returns a function where the receiver becomes the first parameter.
/// Unlike method value, method expression does not capture a receiver.
pub fn compile_method_expr(
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
    
    // Handle method expression on interface type (e.g., Reader.Read)
    // This is valid Go syntax: returns func(Reader) ReturnType
    if info.is_interface(recv_type) {
        return compile_interface_method_expr(recv_type, method_name, dst, ctx, func, info);
    }
    
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
    
    let final_func_id = match call_info.dispatch {
        crate::embed::MethodDispatch::Static { func_id, expects_ptr_recv } => {
            // For promoted methods (embedding path is not empty), generate a wrapper
            if !call_info.embed_path.steps.is_empty() {
                crate::wrapper::generate_method_expr_promoted_wrapper(
                    ctx,
                    base_type,
                    &call_info.embed_path,
                    func_id,
                    expects_ptr_recv,
                    call_info.recv_is_pointer,
                    method_name,
                    &info.project.tc_objs,
                )
            } else {
                func_id
            }
        }
        crate::embed::MethodDispatch::EmbeddedInterface { iface_type, .. } => {
            // Method expression on embedded interface - generate wrapper
            // Get the method obj from interface type
            let method_obj = get_interface_method_obj(iface_type, method_name, &info.project.tc_objs)
                .ok_or_else(|| CodegenError::Internal(format!(
                    "method {} not found in embedded interface", method_name
                )))?;
            
            crate::wrapper::generate_method_expr_embedded_iface_wrapper(
                ctx,
                base_type,
                &call_info.embed_path,
                iface_type,
                method_name,
                method_obj,
                call_info.recv_is_pointer,
                &info.project.tc_objs,
                &info.project.interner,
            )
        }
        crate::embed::MethodDispatch::Interface { .. } => {
            // This shouldn't happen as we handle interface recv_type above
            return Err(CodegenError::Internal("unexpected interface dispatch in method expression".to_string()));
        }
    };
    
    func.emit_closure_new(dst, final_func_id, 0);
    Ok(())
}

/// Compile method expression on interface type (e.g., Reader.Read).
/// Returns a function that takes the interface as first parameter and does CallIface.
fn compile_interface_method_expr(
    iface_type: vo_analysis::objects::TypeKey,
    method_name: &str,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let method_idx = ctx.get_interface_method_index(
        iface_type, method_name, &info.project.tc_objs, &info.project.interner
    );
    
    let (param_slots, ret_slots) = info.get_interface_method_slots(iface_type, method_name)
        .ok_or_else(|| CodegenError::Internal(format!(
            "method {} not found on interface {:?}", method_name, iface_type
        )))?;
    
    let wrapper_id = crate::wrapper::generate_method_expr_iface_wrapper(
        ctx, method_idx, param_slots, ret_slots, method_name,
    );
    
    func.emit_closure_new(dst, wrapper_id, 0);
    Ok(())
}

/// Get the ObjKey for a method in an interface type by name.
pub fn get_interface_method_obj(
    iface_type: vo_analysis::objects::TypeKey,
    method_name: &str,
    tc_objs: &vo_analysis::objects::TCObjects,
) -> Option<vo_analysis::objects::ObjKey> {
    let underlying = vo_analysis::typ::underlying_type(iface_type, tc_objs);
    if let vo_analysis::typ::Type::Interface(iface) = &tc_objs.types[underlying] {
        let all_methods = iface.all_methods();
        let methods = all_methods.as_ref().map(|v| v.as_slice()).unwrap_or(iface.methods());
        for &method in methods {
            if tc_objs.lobjs[method].name() == method_name {
                return Some(method);
            }
        }
    }
    None
}
