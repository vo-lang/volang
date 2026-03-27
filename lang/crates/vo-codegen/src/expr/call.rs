//! Function and method call compilation.

use vo_analysis::objects::{ObjKey, TypeKey};
use vo_analysis::selection::{Selection, SelectionKind};
use vo_common::symbol::Symbol;
use vo_common::abi::abi_lookup_name;
use vo_runtime::SlotType;
use vo_syntax::ast::{Expr, ExprKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

use super::{compile_expr, compile_expr_to};

/// Compute slot types for the arg region of a call buffer, mirroring calc_method_arg_slots.
/// For variadic (non-spread) calls the packed slice contributes a single GcRef slot.
pub(crate) fn calc_arg_slot_types(
    call: &vo_syntax::ast::CallExpr,
    param_types: &[TypeKey],
    is_variadic: bool,
    info: &TypeInfoWrapper,
) -> Vec<SlotType> {
    calc_arg_slot_types_for_args(&call.args, call.spread, param_types, is_variadic, info)
}

/// Compute slot types for the arg region of a call buffer, mirroring calc_method_arg_slots.
/// For variadic (non-spread) calls the packed slice contributes a single GcRef slot.
pub(crate) fn calc_arg_slot_types_for_args(
    args: &[Expr],
    spread: bool,
    param_types: &[TypeKey],
    is_variadic: bool,
    info: &TypeInfoWrapper,
) -> Vec<SlotType> {
    let arg_info = info.get_call_arg_info(args, param_types);
    if arg_info.tuple_expand.is_some() {
        return param_types.iter().flat_map(|&t| info.type_slot_types(t)).collect();
    }
    if is_variadic && !spread {
        let n_fixed = num_fixed_params(param_types, is_variadic);
        let mut types: Vec<SlotType> = param_types.iter().take(n_fixed)
            .flat_map(|&t| info.type_slot_types(t))
            .collect();
        types.push(SlotType::GcRef);
        types
    } else {
        param_types.iter().flat_map(|&t| info.type_slot_types(t)).collect()
    }
}

pub(crate) fn strip_paren_expr(mut expr: &Expr) -> &Expr {
    while let ExprKind::Paren(inner) = &expr.kind {
        expr = inner;
    }
    expr
}

fn is_type_name_expr(expr: &Expr, info: &TypeInfoWrapper) -> bool {
    match &strip_paren_expr(expr).kind {
        ExprKind::Ident(ident) => {
            let obj_key = info.get_use(ident);
            info.project.tc_objs.lobjs[obj_key].entity_type().is_type_name()
        }
        ExprKind::Selector(sel) => {
            if let ExprKind::Ident(pkg_ident) = &strip_paren_expr(&sel.expr).kind {
                if info.package_path(pkg_ident).is_some() {
                    let obj_key = info.get_use(&sel.sel);
                    return info.project.tc_objs.lobjs[obj_key].entity_type().is_type_name();
                }
            }
            false
        }
        _ => false,
    }
}

fn explicit_interface_conversion_source<'a>(expr: &'a Expr, info: &TypeInfoWrapper) -> Option<&'a Expr> {
    let expr = strip_paren_expr(expr);
    if !info.is_interface(info.expr_type(expr.id)) {
        return None;
    }

    let source = match &expr.kind {
        ExprKind::Call(call) if !call.spread && call.args.len() == 1 && is_type_name_expr(&call.func, info) => {
            &call.args[0]
        }
        ExprKind::Conversion(conv) => &conv.expr,
        _ => return None,
    };

    let source = strip_paren_expr(source);
    let source_type = info.expr_type(source.id);
    if info.is_interface(source_type) {
        return None;
    }
    Some(source)
}

pub(crate) struct MonomorphicIfaceTarget<'a> {
    pub recv_expr: &'a Expr,
    pub recv_type: TypeKey,
    pub method_obj: ObjKey,
    pub call_info: crate::embed::MethodCallInfo,
}

pub(crate) fn resolve_monomorphic_iface_target<'a>(
    recv_expr: &'a Expr,
    method_sym: Symbol,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<Option<MonomorphicIfaceTarget<'a>>, CodegenError> {
    let recv_expr = match explicit_interface_conversion_source(recv_expr, info) {
        Some(expr) => expr,
        None => return Ok(None),
    };

    let recv_type = info.expr_type(recv_expr.id);
    let method_name = info.project.interner.resolve(method_sym)
        .ok_or_else(|| CodegenError::Internal("cannot resolve method name".to_string()))?;

    let (method_obj, indices, indirect) = match vo_analysis::lookup::lookup_field_or_method(
        recv_type,
        true,
        Some(info.package_key()),
        method_name,
        &info.project.tc_objs,
    ) {
        vo_analysis::lookup::LookupResult::Entry(method_obj, indices, indirect) => {
            (method_obj, indices, indirect)
        }
        vo_analysis::lookup::LookupResult::NotFound => return Ok(None),
        vo_analysis::lookup::LookupResult::Ambiguous(_) => {
            return Err(CodegenError::Internal(format!(
                "ambiguous monomorphic interface target: {}",
                method_name,
            )));
        }
        vo_analysis::lookup::LookupResult::BadMethodReceiver => {
            return Err(CodegenError::Internal(format!(
                "bad monomorphic interface receiver: {}",
                method_name,
            )));
        }
    };

    let selection = Selection::new(
        SelectionKind::MethodVal,
        Some(recv_type),
        method_obj,
        indices,
        indirect,
        &info.project.tc_objs,
    );
    let call_info = crate::embed::resolve_method_call(
        recv_type,
        method_name,
        method_sym,
        Some(&selection),
        false,
        ctx,
        &info.project.tc_objs,
        &info.project.interner,
    ).ok_or_else(|| {
        CodegenError::Internal(format!(
            "monomorphic interface target not found: type_key={:?}.{}",
            recv_type,
            method_name,
        ))
    })?;

    Ok(Some(MonomorphicIfaceTarget {
        recv_expr,
        recv_type,
        method_obj,
        call_info,
    }))
}

fn emit_direct_func_call(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    callee_expr: &Expr,
    func_idx: u32,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let ret_slots = info.type_slot_count(info.expr_type(expr.id));
    let ret_slot_types = info.type_slot_types(info.expr_type(expr.id));
    let func_type = info.expr_type(callee_expr.id);
    let param_types = info.func_param_types(func_type);
    let is_variadic = info.is_variadic(func_type);
    let total_arg_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
    let arg_slot_types = calc_arg_slot_types(call, &param_types, is_variadic, info);
    let args_start = func.alloc_call_buffer(&arg_slot_types, &ret_slot_types);

    compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;

    let c = crate::type_info::encode_call_args(total_arg_slots as u16, ret_slots as u16);
    let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_idx);
    func.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);

    let ret_start = args_start + total_arg_slots as u16;
    if ret_slots > 0 && dst != ret_start {
        func.emit_copy(dst, ret_start, ret_slots);
    }
    Ok(())
}

// =============================================================================
// Call Expression - Main Entry
// =============================================================================

/// Compile a call expression.
pub fn compile_call(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let callee_expr = strip_paren_expr(&call.func);

    // Check if method call (selector expression)
    if let ExprKind::Selector(sel) = &callee_expr.kind {
        if let Some(selection) = info.get_selection(callee_expr.id) {
            if matches!(selection.kind(), SelectionKind::MethodExpr) {
                return compile_method_expr_call(expr, call, sel, selection, dst, ctx, func, info);
            }
        }
        return compile_method_call(expr, call, callee_expr, sel, dst, ctx, func, info);
    }
    
    // Check if builtin or type conversion
    if let ExprKind::Ident(ident) = &callee_expr.kind {
        // Use analysis phase info for builtin detection - correctly handles variable shadowing
        if let Some(builtin_id) = info.expr_builtin(callee_expr.id) {
            return super::builtin::compile_builtin_call_by_id(expr, builtin_id, call, dst, ctx, func, info);
        }
        
        // Check if this is a type conversion (ident refers to a type, not a function)
        // Type conversions look like function calls: T(x)
        {
            let obj_key = info.get_use(ident);
            let obj = &info.project.tc_objs.lobjs[obj_key];
            if obj.entity_type().is_type_name() {
                // This is a type conversion
                if call.args.len() == 1 {
                    return super::conversion::compile_type_conversion(&call.args[0], dst, expr, ctx, func, info);
                } else if call.args.is_empty() {
                    // Zero value - already handled by default initialization
                    return Ok(());
                }
            }
        }
    }
    
    // Get return slot count and types for this call (needed for correct GC slot_types in buffer)
    let ret_slots = info.type_slot_count(info.expr_type(expr.id));
    let ret_slot_types = info.type_slot_types(info.expr_type(expr.id));
    
    // Get function type and parameter types for interface conversion
    let func_type = info.expr_type(callee_expr.id);
    let param_types = info.func_param_types(func_type);
    let is_variadic = info.is_variadic(func_type);
    
    // Compute total arg slots using calc_method_arg_slots (handles variadic packing)
    let total_arg_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);

    if let ExprKind::FuncLit(func_lit) = &callee_expr.kind {
        if info.closure_captures(callee_expr.id).is_empty() {
            let (func_id, captures) = super::literal::lower_func_lit(callee_expr, func_lit, ctx, info)?;
            if !captures.is_empty() {
                panic!("zero-capture func literal lowering returned captures");
            }
            return emit_direct_func_call(expr, call, callee_expr, func_id, dst, ctx, func, info);
        }
    }
    
    // Check if calling a closure (local variable with Signature type)
    if let ExprKind::Ident(ident) = &callee_expr.kind {
        let obj_key = info.get_use(ident);
        
        // Check if it's a closure (local, capture, or global variable)
        let is_closure = func.lookup_local(ident.symbol).is_some() 
            || func.lookup_capture(ident.symbol).is_some()
            || ctx.get_global_index(obj_key).is_some();
        
        if is_closure {
            let closure_reg = compile_expr(callee_expr, ctx, func, info)?;
            let arg_slot_types = calc_arg_slot_types(call, &param_types, is_variadic, info);
            let args_start = func.alloc_dynamic_call_buffer(&[SlotType::Value], &arg_slot_types, &ret_slot_types);
            compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;
            
            let c = crate::type_info::encode_call_args(total_arg_slots as u16, ret_slots as u16);
            func.emit_op(Opcode::CallClosure, closure_reg, args_start, c);
            
            let ret_start = args_start + total_arg_slots as u16;
            if ret_slots > 0 && dst != ret_start {
                func.emit_copy(dst, ret_start, ret_slots);
            }
            return Ok(());
        }
        
        // Function call - check if it's a Vo function (has body) or extern (no body)
        let obj = &info.project.tc_objs.lobjs[obj_key];
        
        if obj.entity_type().func_has_body() {
            let func_idx = ctx.get_func_by_objkey(obj_key)
                .ok_or_else(|| CodegenError::Internal(format!("function not registered: {:?}", ident.symbol)))?;
            return emit_direct_func_call(expr, call, callee_expr, func_idx, dst, ctx, func, info);
        } else {
            // Extern function (no body) - use CallExtern instruction
            let func_name = info.project.interner.resolve(ident.symbol)
                .ok_or_else(|| CodegenError::Internal("cannot resolve function name".to_string()))?;
            let pkg_key = obj.pkg();
            let pkg_name = pkg_key
                .map(|pk| info.project.tc_objs.pkgs[pk].abi_path().to_string())
                .unwrap_or_else(|| "main".to_string());
            let extern_name = abi_lookup_name(&pkg_name, func_name);
            return compile_extern_call(call, &extern_name, dst, ctx, func, info);
        }
    }
    
    // Non-ident function call (e.g., expression returning a closure)
    let closure_reg = compile_expr(callee_expr, ctx, func, info)?;
    compile_closure_call_from_reg(expr, call, callee_expr, closure_reg, dst, ctx, func, info)
}

pub fn compile_method_expr_call(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
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
    let recv_arg = call.args.first().ok_or_else(|| {
        CodegenError::Internal("method expression call missing receiver argument".to_string())
    })?;
    let forwarded_args = &call.args[1..];
    let method_name = info.project.interner.resolve(sel.sel.symbol)
        .ok_or_else(|| CodegenError::Internal("cannot resolve method name".to_string()))?;
    if info.is_interface(recv_type) {
        if let Some(target) = resolve_monomorphic_iface_target(recv_arg, sel.sel.symbol, ctx, info)? {
            let method_type = info.project.tc_objs.lobjs[target.method_obj].typ()
                .ok_or_else(|| CodegenError::Internal("method type missing".to_string()))?;
            return compile_method_dispatch_with_args(
                expr,
                target.recv_expr,
                target.recv_type,
                method_type,
                forwarded_args,
                call.spread,
                method_name,
                &target.call_info,
                dst,
                ctx,
                func,
                info,
            );
        }
    }

    let is_interface_recv = info.is_interface(recv_type);
    let call_info = crate::embed::resolve_method_call(
        recv_type,
        method_name,
        sel.sel.symbol,
        Some(selection),
        is_interface_recv,
        ctx,
        &info.project.tc_objs,
        &info.project.interner,
    ).ok_or_else(|| {
        CodegenError::Internal(format!("method not found: type_key={:?}.{}", recv_type, method_name))
    })?;

    let method_type = info.project.tc_objs.lobjs[selection.obj()].typ()
        .ok_or_else(|| CodegenError::Internal("method type missing".to_string()))?;
    compile_method_dispatch_with_args(
        expr,
        recv_arg,
        recv_type,
        method_type,
        forwarded_args,
        call.spread,
        method_name,
        &call_info,
        dst,
        ctx,
        func,
        info,
    )
}

/// Compile closure call when closure is already in a register.
/// Used for func field calls like h.logic(args) and IIFE calls.
pub fn compile_closure_call_from_reg(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    callee_expr: &Expr,
    closure_reg: u16,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let ret_slots = info.type_slot_count(info.expr_type(expr.id)) as u16;
    
    // Get function type from the closure expression to handle variadic properly
    let func_type = info.expr_type(callee_expr.id);
    let param_types = info.func_param_types(func_type);
    let is_variadic = info.is_variadic(func_type);
    
    // Calculate arg slots with variadic packing
    let total_arg_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
    
    let ret_slot_types = info.type_slot_types(info.expr_type(expr.id));
    let arg_slot_types = calc_arg_slot_types(call, &param_types, is_variadic, info);
    let args_start = func.alloc_dynamic_call_buffer(&[SlotType::Value], &arg_slot_types, &ret_slot_types);
    compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;
    
    let c = crate::type_info::encode_call_args(total_arg_slots, ret_slots);
    func.emit_op(Opcode::CallClosure, closure_reg, args_start, c);
    
    let ret_start = args_start + total_arg_slots as u16;
    if ret_slots > 0 && dst != ret_start {
        func.emit_copy(dst, ret_start, ret_slots);
    }
    
    Ok(())
}

// =============================================================================
// Method Call
// =============================================================================

/// Emit code to pass receiver to method.
/// 
/// Unified logic: extract initial register from storage, then delegate to emit_embed_path_traversal.
/// Special case: when expects_ptr_recv=true and storage=None, use compile_expr_to_ptr for auto-addressing.
pub fn emit_receiver(
    sel_expr: &Expr,
    args_start: u16,
    recv_type: TypeKey,
    recv_storage: Option<StorageKind>,
    call_info: &crate::embed::MethodCallInfo,
    actual_recv_type: TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let recv_is_ptr = info.is_pointer(recv_type);
    let value_slots = info.type_slot_count(actual_recv_type);
    let expects_ptr_recv = call_info.expects_ptr_recv();
    let embed_path = &call_info.embed_path;
    let total_offset = embed_path.total_offset;
    
    // Special case: expression needing pointer with no embedding path - use compile_expr_to_ptr
    // This handles auto-addressing (escaping stack values to heap when pointer needed)
    // Only applies when there's no embed path to traverse (no pointer steps, zero offset)
    if recv_storage.is_none() && expects_ptr_recv && total_offset == 0 && !embed_path.has_pointer_step {
        return super::compile_expr_to_ptr(sel_expr, args_start, ctx, func, info);
    }
    
    // Determine initial register, pointer state, and base offset from storage
    let start = match recv_storage {
        Some(StorageKind::HeapBoxed { gcref_slot, stores_pointer, .. }) => {
            if stores_pointer {
                // Pointer variable captured by closure - read pointer from box first
                let actual_ptr = func.alloc_slots(&[SlotType::GcRef]);
                func.emit_ptr_get(actual_ptr, gcref_slot, 0, 1);
                crate::embed::TraverseStart::new(actual_ptr, true)
            } else {
                crate::embed::TraverseStart::new(gcref_slot, true)
            }
        }
        Some(StorageKind::HeapArray { gcref_slot, .. }) => {
            // HeapArray layout: [GcHeader][ArrayHeader(2 slots)][elems...]
            // Use base_offset to skip ArrayHeader
            const ARRAY_HEADER_SLOTS: u16 = 2;
            crate::embed::TraverseStart::with_base_offset(gcref_slot, true, ARRAY_HEADER_SLOTS)
        }
        Some(StorageKind::StackValue { slot, .. }) => {
            crate::embed::TraverseStart::new(slot, recv_is_ptr)
        }
        _ => {
            // Expression result - compile and use
            let reg = compile_expr(sel_expr, ctx, func, info)?;
            crate::embed::TraverseStart::new(reg, recv_is_ptr)
        }
    };
    
    // Delegate to unified traversal logic
    crate::embed::emit_embed_path_traversal(func, start, &embed_path.steps, expects_ptr_recv, value_slots, args_start);
    
    Ok(())
}

fn compile_method_call(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    callee_expr: &Expr,
    sel: &vo_syntax::ast::SelectorExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // 1. Check for package function call or type conversion (e.g., bytes.Contains, json.Number)
    if let ExprKind::Ident(pkg_ident) = &sel.expr.kind {
        // Check if it's a package reference
        if info.package_path(pkg_ident).is_some() {
            // Check if sel.sel refers to a type (type conversion: pkg.Type(x))
            let obj_key = info.get_use(&sel.sel);
            let obj = &info.project.tc_objs.lobjs[obj_key];
            if obj.entity_type().is_type_name() {
                // This is a type conversion: pkg.Type(x)
                if call.args.len() == 1 {
                    return super::conversion::compile_type_conversion(&call.args[0], dst, expr, ctx, func, info);
                } else if call.args.is_empty() {
                    // Zero value - already handled by default initialization
                    return Ok(());
                }
            }
            
            // Check if it's a Vo function (has body) or extern (no body)
            if obj.entity_type().func_has_body() {
                // Vo function - use normal Call with proper interface conversion
                // Use ObjKey to avoid cross-package Symbol collision
                let func_idx = ctx.get_func_by_objkey(obj_key)
                    .ok_or_else(|| CodegenError::Internal(format!("pkg func not registered: {:?}", sel.sel.symbol)))?;
                return emit_direct_func_call(expr, call, callee_expr, func_idx, dst, ctx, func, info);
            }
            // Extern function - use CallExtern
            if let Ok(extern_name) = get_extern_name(sel, info) {
                return compile_extern_call(call, &extern_name, dst, ctx, func, info);
            }
        }
    }

    let recv_type = info.expr_type(sel.expr.id);
    
    let method_name = info.project.interner.resolve(sel.sel.symbol)
        .ok_or_else(|| CodegenError::Internal("cannot resolve method name".to_string()))?;
    
    // Check if this is a func field call or method expression call
    let selection = info.get_selection(callee_expr.id);
    if let Some(sel_info) = selection {
        match sel_info.kind() {
            SelectionKind::FieldVal | SelectionKind::MethodExpr => {
                // FieldVal: struct field of function type
                // MethodExpr: T.Method(recv, args...) or (*T).Method(recv, args...)
                // Both compile to closure call
                let field_type = info.expr_type(callee_expr.id);
                if info.is_func_type(field_type) {
                    let closure_reg = compile_expr(callee_expr, ctx, func, info)?;
                    return compile_closure_call_from_reg(expr, call, callee_expr, closure_reg, dst, ctx, func, info);
                }
            }
            SelectionKind::MethodVal => {
                // Method value is handled below via resolve_method_call
            }
        }
    }

    if info.is_interface(recv_type) {
        if let Some(target) = resolve_monomorphic_iface_target(&sel.expr, sel.sel.symbol, ctx, info)? {
            let method_type = info.project.tc_objs.lobjs[target.method_obj].typ()
                .ok_or_else(|| CodegenError::Internal("method type missing".to_string()))?;
            return compile_method_dispatch_with_args(
                expr,
                target.recv_expr,
                target.recv_type,
                method_type,
                &call.args,
                call.spread,
                method_name,
                &target.call_info,
                dst,
                ctx,
                func,
                info,
            );
        }
    }
    
    // Use unified method call resolution
    let is_interface_recv = info.is_interface(recv_type);
    
    let call_info = crate::embed::resolve_method_call(
        recv_type,
        method_name,
        sel.sel.symbol,
        selection,
        is_interface_recv,
        ctx,
        &info.project.tc_objs,
        &info.project.interner,
    ).ok_or_else(|| {
        CodegenError::Internal(format!("method not found: type_key={:?}.{}", recv_type, method_name))
    })?;

    let method_type = info.expr_type(callee_expr.id);
    compile_method_dispatch_with_args(
        expr,
        &sel.expr,
        recv_type,
        method_type,
        &call.args,
        call.spread,
        method_name,
        &call_info,
        dst,
        ctx,
        func,
        info,
    )
}

fn emit_static_method_call(
    expr: &Expr,
    recv_expr: &Expr,
    recv_type: TypeKey,
    method_type: TypeKey,
    args: &[Expr],
    spread: bool,
    call_info: &crate::embed::MethodCallInfo,
    func_id: u32,
    expects_ptr_recv: bool,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let base_type = if call_info.recv_is_pointer {
        info.pointer_base(recv_type)
    } else {
        recv_type
    };
    let actual_recv_type = call_info.actual_recv_type(base_type);
    let is_variadic = info.is_variadic(method_type);
    let param_types = info.func_param_types(method_type);
    let recv_slots = if expects_ptr_recv { 1 } else { info.type_slot_count(actual_recv_type) };
    let arg_slots = calc_method_arg_slots_for_args(args, spread, &param_types, is_variadic, info);
    let total_slots = recv_slots + arg_slots;
    let ret_type = info.expr_type(expr.id);
    let ret_slots = info.type_slot_count(ret_type);
    let ret_slot_types = info.type_slot_types(ret_type);
    let recv_slot_types: Vec<SlotType> = if expects_ptr_recv {
        vec![SlotType::GcRef]
    } else {
        info.type_slot_types(actual_recv_type)
    };
    let arg_slot_types_only = calc_arg_slot_types_for_args(args, spread, &param_types, is_variadic, info);
    let mut all_arg_slot_types = recv_slot_types;
    all_arg_slot_types.extend(arg_slot_types_only);
    let args_start = func.alloc_call_buffer(&all_arg_slot_types, &ret_slot_types);

    let recv_storage = if let ExprKind::Ident(ident) = &recv_expr.kind {
        func.lookup_local(ident.symbol).map(|local| local.storage)
    } else {
        None
    };
    emit_receiver(
        recv_expr,
        args_start,
        recv_type,
        recv_storage,
        call_info,
        actual_recv_type,
        ctx,
        func,
        info,
    )?;
    compile_method_args_for_args(args, spread, &param_types, is_variadic, args_start + recv_slots, ctx, func, info)?;

    let c = crate::type_info::encode_call_args(total_slots, ret_slots);
    let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_id);
    func.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);

    let ret_start = args_start + total_slots as u16;
    if ret_slots > 0 && dst != ret_start {
        func.emit_copy(dst, ret_start, ret_slots);
    }
    Ok(())
}

fn emit_interface_call_with_args(
    expr: &Expr,
    args: &[Expr],
    spread: bool,
    iface_type: vo_analysis::objects::TypeKey,
    method_idx: u32,
    iface_slot: u16,
    method_name: &str,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let (param_types, is_variadic) = info.get_interface_method_signature(iface_type, method_name);
    let arg_slots = calc_method_arg_slots_for_args(args, spread, &param_types, is_variadic, info);
    let ret_type = info.expr_type(expr.id);
    let ret_slots = info.type_slot_count(ret_type);
    let ret_slot_types = info.type_slot_types(ret_type);
    let arg_slot_types = calc_arg_slot_types_for_args(args, spread, &param_types, is_variadic, info);
    let args_start = func.alloc_dynamic_call_buffer(&[SlotType::Value], &arg_slot_types, &ret_slot_types);
    
    compile_method_args_for_args(args, spread, &param_types, is_variadic, args_start, ctx, func, info)?;
    
    let c = crate::type_info::encode_call_args(arg_slots, ret_slots);
    func.emit_with_flags(Opcode::CallIface, method_idx as u8, iface_slot, args_start, c);
    
    let ret_start = args_start + arg_slots as u16;
    if ret_slots > 0 && dst != ret_start {
        func.emit_copy(dst, ret_start, ret_slots);
    }
    Ok(())
}

/// Dispatch method call based on MethodCallInfo.
fn compile_method_dispatch_with_args(
    expr: &Expr,
    recv_expr: &Expr,
    recv_type: TypeKey,
    method_type: TypeKey,
    args: &[Expr],
    spread: bool,
    method_name: &str,
    call_info: &crate::embed::MethodCallInfo,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use crate::embed::MethodDispatch;

    match &call_info.dispatch {
        MethodDispatch::Interface { method_idx } => {
            // Interface dispatch - interface is the receiver directly
            let iface_slot = compile_expr(recv_expr, ctx, func, info)?;
            emit_interface_call_with_args(expr, args, spread, recv_type, *method_idx, iface_slot, method_name, dst, ctx, func, info)
        }
        MethodDispatch::EmbeddedInterface { iface_type, method_idx } => {
            // Embedded interface dispatch - extract interface first
            let recv_is_ptr = info.is_pointer(recv_type);
            let recv_reg = compile_expr(recv_expr, ctx, func, info)?;
            let iface_slot = func.alloc_interface();
            let start = crate::embed::TraverseStart::new(recv_reg, recv_is_ptr);
            call_info.emit_target(func, start, iface_slot);
            
            emit_interface_call_with_args(expr, args, spread, *iface_type, *method_idx, iface_slot, method_name, dst, ctx, func, info)
        }
        MethodDispatch::Static { func_id, expects_ptr_recv } => {
            // Static call
            emit_static_method_call(
                expr,
                recv_expr,
                recv_type,
                method_type,
                args,
                spread,
                call_info,
                *func_id,
                *expects_ptr_recv,
                dst,
                ctx,
                func,
                info,
            )
        }
    }
}

// =============================================================================
// Extern Call
// =============================================================================

/// Compile extern package function call (e.g., fmt.Println).
pub fn compile_extern_call(
    call: &vo_syntax::ast::CallExpr,
    extern_name: &str,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_vm::bytecode::ExtSlotKind;

    let func_type = info.expr_type(call.func.id);
    let is_variadic = info.is_variadic(func_type);
    let param_types = info.func_param_types(func_type);

    // Compute param_kinds from slot types for the WASM ext-bridge.
    // GcRef slots → Bytes (string/[]byte that must be dereferenced); everything else → Value.
    let param_kinds: Vec<ExtSlotKind> = param_types
        .iter()
        .flat_map(|&t| info.type_slot_types(t))
        .map(|st| if st == SlotType::GcRef { ExtSlotKind::Bytes } else { ExtSlotKind::Value })
        .collect();

    // Get return slot count from the function's result type
    let sig = info.as_signature(func_type);
    let ret_slots = info.type_slot_count(sig.results()) as u16;
    let extern_id = ctx.get_or_register_extern_with_slots(extern_name, ret_slots, param_kinds);

    // Use compile_method_args for proper type conversion (e.g., boxing to `any`)
    let total_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
    let args_start = func.alloc_slots(&vec![SlotType::Value; total_slots.max(1) as usize]);
    compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;

    func.emit_with_flags(Opcode::CallExtern, total_slots as u8, dst, extern_id as u16, args_start);
    Ok(())
}

// =============================================================================
// Argument Compilation Helpers
// =============================================================================

/// Compile arguments with parameter types for automatic interface conversion.
/// Used by method calls and defer with known param types.
/// Handles multi-value function calls: f(g()) where g() returns multiple values.
pub fn compile_args_with_types(
    args: &[Expr],
    param_types: &[TypeKey],
    args_start: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let arg_info = info.get_call_arg_info(args, param_types);
    
    if let Some(_) = arg_info.tuple_expand {
        // Multi-value expansion: compile tuple once, then convert each element
        let tuple = super::CompiledTuple::compile(&args[0], ctx, func, info)?;
        
        let mut offset = 0u16;
        let mut elem_idx = 0usize;
        tuple.for_each_element(info, |elem_slot, elem_type| {
            let pt = param_types[elem_idx];
            let pt_slots = info.type_slot_count(pt);
            // Note: emit_assign is fallible, but for_each_element uses FnMut
            // In practice this won't fail for valid code that passed type checking
            let _ = crate::assign::emit_assign(
                args_start + offset,
                crate::assign::AssignSource::Slot { slot: elem_slot, type_key: elem_type },
                pt,
                ctx, func, info
            );
            offset += pt_slots;
            elem_idx += 1;
        });
        Ok(offset)
    } else {
        // Normal case: one arg per param
        let mut offset = 0u16;
        for (i, arg) in args.iter().enumerate() {
            if let Some(&pt) = param_types.get(i) {
                crate::assign::emit_assign(args_start + offset, crate::assign::AssignSource::Expr(arg), pt, ctx, func, info)?;
                offset += info.type_slot_count(pt);
            } else {
                let slots = info.expr_slots(arg.id);
                compile_expr_to(arg, args_start + offset, ctx, func, info)?;
                offset += slots;
            }
        }
        Ok(offset)
    }
}

/// Get extern name for a package function call
pub fn get_extern_name(
    sel: &vo_syntax::ast::SelectorExpr,
    info: &TypeInfoWrapper,
) -> Result<String, CodegenError> {
    if let ExprKind::Ident(pkg_ident) = &sel.expr.kind {
        // Use package name (not path) for extern name to match extension registration
        let pkg_name = info.package_name(pkg_ident)
            .ok_or_else(|| CodegenError::Internal("cannot resolve package".to_string()))?;
        let func_name = info.project.interner.resolve(sel.sel.symbol)
            .ok_or_else(|| CodegenError::Internal("cannot resolve function name".to_string()))?;
        Ok(abi_lookup_name(&pkg_name, func_name))
    } else {
        Err(CodegenError::Internal("expected package.func".to_string()))
    }
}

/// Calculate number of fixed (non-variadic) parameters.
#[inline]
fn num_fixed_params(param_types: &[TypeKey], is_variadic: bool) -> usize {
    if is_variadic && !param_types.is_empty() {
        param_types.len() - 1
    } else {
        param_types.len()
    }
}

/// Compile method arguments with variadic packing and interface conversion.
/// Returns the total slots used for arguments.
pub fn compile_method_args(
    call: &vo_syntax::ast::CallExpr,
    param_types: &[TypeKey],
    is_variadic: bool,
    args_start: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    compile_method_args_for_args(&call.args, call.spread, param_types, is_variadic, args_start, ctx, func, info)?;
    Ok(())
}

pub(crate) fn compile_method_args_for_args(
    args: &[Expr],
    spread: bool,
    param_types: &[TypeKey],
    is_variadic: bool,
    args_start: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    // Tuple expansion for non-variadic calls: f(g()) where g() returns multiple values
    let arg_info = info.get_call_arg_info(args, param_types);
    if arg_info.tuple_expand.is_some() {
        return compile_args_with_types(args, param_types, args_start, ctx, func, info);
    }
    
    if is_variadic && !spread {
        let n_fixed = num_fixed_params(param_types, is_variadic);
        
        // Emit fixed arguments
        let fixed_args: Vec<_> = args.iter().take(n_fixed).cloned().collect();
        let mut offset = compile_args_with_types(&fixed_args, &param_types[..n_fixed], args_start, ctx, func, info)?;
        
        // Pack variadic arguments into slice (handles tuple expansion internally)
        let variadic_args: Vec<_> = args.iter().skip(n_fixed).collect();
        let elem_type = info.slice_elem_type(param_types.last().copied().unwrap());
        let slice_reg = pack_variadic_args(&variadic_args, elem_type, ctx, func, info)?;
        func.emit_copy(args_start + offset, slice_reg, 1);
        offset += 1;
        Ok(offset)
    } else {
        compile_args_with_types(args, param_types, args_start, ctx, func, info)
    }
}

/// Calculate arg slots for method call.
pub fn calc_method_arg_slots(
    call: &vo_syntax::ast::CallExpr,
    param_types: &[TypeKey],
    is_variadic: bool,
    info: &TypeInfoWrapper,
) -> u16 {
    calc_method_arg_slots_for_args(&call.args, call.spread, param_types, is_variadic, info)
}

pub(crate) fn calc_method_arg_slots_for_args(
    args: &[Expr],
    spread: bool,
    param_types: &[TypeKey],
    is_variadic: bool,
    info: &TypeInfoWrapper,
) -> u16 {
    let arg_info = info.get_call_arg_info(args, param_types);
    if arg_info.tuple_expand.is_some() {
        return param_types.iter().map(|&t| info.type_slot_count(t)).sum();
    }
    
    if is_variadic && !spread {
        let n_fixed = num_fixed_params(param_types, is_variadic);
        let fixed_slots: u16 = param_types.iter().take(n_fixed).map(|&t| info.type_slot_count(t)).sum();
        fixed_slots + 1
    } else {
        param_types.iter().map(|&t| info.type_slot_count(t)).sum()
    }
}

/// Pack variadic arguments into a slice.
/// For `f(a, b, c)` where f is variadic, this creates `[]T{a, b, c}` and returns its register.
/// Handles tuple expansion: `f(g())` where g() returns multiple values expands to multiple elements.
/// `variadic_args` are the arguments that should be packed (starting from first variadic arg).
/// `elem_type` is the element type of the variadic slice.
/// Returns the register containing the slice (1 slot).
fn pack_variadic_args(
    variadic_args: &[&vo_syntax::ast::Expr],
    elem_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let elem_slots = info.type_slot_count(elem_type);
    let elem_bytes = (elem_slots as usize) * 8;
    let elem_slot_types = info.type_slot_types(elem_type);
    let elem_vk = info.type_value_kind(elem_type);
    
    // Calculate total element count (expanding tuples)
    let total_elems: usize = variadic_args.iter()
        .map(|arg| {
            let arg_type = info.expr_type(arg.id);
            if info.is_tuple(arg_type) { info.tuple_len(arg_type) } else { 1 }
        })
        .sum();
    
    // Get element meta
    let elem_meta_idx = ctx.get_or_create_value_meta(elem_type, info);
    let meta_reg = func.alloc_slots(&[SlotType::Value]);
    func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);
    
    // Create slice
    let dst = func.alloc_slots(&[SlotType::GcRef]);
    let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
    let num_regs = if flags == 0 { 3 } else { 2 };
    let len_cap_reg = func.alloc_slots(&vec![SlotType::Value; num_regs]);
    let (b, c) = crate::type_info::encode_i32(total_elems as i32);
    func.emit_op(Opcode::LoadInt, len_cap_reg, b, c);      // len
    func.emit_op(Opcode::LoadInt, len_cap_reg + 1, b, c);  // cap = len
    if flags == 0 {
        let eb_idx = ctx.const_int(elem_bytes as i64);
        func.emit_op(Opcode::LoadConst, len_cap_reg + 2, eb_idx, 0);
    }
    func.emit_with_flags(Opcode::SliceNew, flags, dst, meta_reg, len_cap_reg);
    
    // Helper to set one slice element
    let mut slice_idx = 0usize;
    let mut set_elem = |val_reg: u16, func: &mut FuncBuilder, ctx: &mut CodegenContext| {
        let idx_reg = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(Opcode::LoadInt, idx_reg, slice_idx as u16, 0);
        func.emit_slice_set(dst, idx_reg, val_reg, elem_bytes, elem_vk, ctx);
        slice_idx += 1;
    };
    
    // Set each element (expanding tuples as needed)
    for elem in variadic_args.iter() {
        let arg_type = info.expr_type(elem.id);
        
        if info.is_tuple(arg_type) {
            // Tuple expansion: compile once, set each element
            let tuple = super::CompiledTuple::compile(elem, ctx, func, info)?;
            tuple.for_each_element(info, |src_slot, src_type| {
                let val_reg = if info.is_interface(elem_type) {
                    let iface_reg = func.alloc_slots(&elem_slot_types);
                    let _ = crate::assign::emit_assign(iface_reg, crate::assign::AssignSource::Slot { slot: src_slot, type_key: src_type }, elem_type, ctx, func, info);
                    iface_reg
                } else {
                    src_slot
                };
                set_elem(val_reg, func, ctx);
            });
        } else {
            let val_reg = if info.is_interface(elem_type) {
                let iface_reg = func.alloc_slots(&elem_slot_types);
                crate::assign::emit_assign(iface_reg, crate::assign::AssignSource::Expr(elem), elem_type, ctx, func, info)?;
                iface_reg
            } else {
                compile_expr(elem, ctx, func, info)?
            };
            set_elem(val_reg, func, ctx);
        }
    }
    
    Ok(dst)
}
