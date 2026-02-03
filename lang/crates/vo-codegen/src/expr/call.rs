//! Function and method call compilation.

use vo_analysis::objects::TypeKey;
use vo_runtime::SlotType;
use vo_syntax::ast::{Expr, ExprKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

use super::{compile_expr, compile_expr_to};

/// Allocate a call buffer with proper slot types for return values.
/// 
/// Return values are written to the beginning of the buffer after the call,
/// so the first `ret_slots` slots must have correct types for GC tracking.
/// The remaining slots (for arguments) are typed as Value.
fn alloc_call_buffer(func: &mut FuncBuilder, buffer_size: u16, ret_slots: u16, ret_slot_types: &[SlotType]) -> u16 {
    if ret_slots == 0 || buffer_size == 0 {
        return func.alloc_slots(&vec![SlotType::Value; buffer_size.max(1) as usize]);
    }
    
    // Build combined slot types: ret_slot_types for return value, Value for the rest
    let mut types = ret_slot_types.to_vec();
    for _ in ret_slots..buffer_size {
        types.push(SlotType::Value);
    }
    func.alloc_slots(&types)
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
    // Check if method call (selector expression)
    if let ExprKind::Selector(sel) = &call.func.kind {
        return compile_method_call(expr, call, sel, dst, ctx, func, info);
    }
    
    // Check if builtin or type conversion
    if let ExprKind::Ident(ident) = &call.func.kind {
        // Use analysis phase info for builtin detection - correctly handles variable shadowing
        if let Some(builtin_id) = info.expr_builtin(call.func.id) {
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
    
    // Get return slot count for this call
    let ret_slots = info.type_slot_count(info.expr_type(expr.id));
    
    // Get function type and parameter types for interface conversion
    let func_type = info.expr_type(call.func.id);
    let param_types = info.func_param_types(func_type);
    let is_variadic = info.is_variadic(func_type);
    
    // Compute total arg slots using calc_method_arg_slots (handles variadic packing)
    let total_arg_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
    
    // Check if calling a closure (local variable with Signature type)
    if let ExprKind::Ident(ident) = &call.func.kind {
        let obj_key = info.get_use(ident);
        
        // Check if it's a closure (local, capture, or global variable)
        let is_closure = func.lookup_local(ident.symbol).is_some() 
            || func.lookup_capture(ident.symbol).is_some()
            || ctx.get_global_index(obj_key).is_some();
        
        if is_closure {
            let closure_reg = compile_expr(&call.func, ctx, func, info)?;
            let args_start = func.alloc_slots(&vec![SlotType::Value; total_arg_slots.max(ret_slots) as usize]);
            compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;
            
            let c = crate::type_info::encode_call_args(total_arg_slots as u16, ret_slots as u16);
            func.emit_op(Opcode::CallClosure, closure_reg, args_start, c);
            
            if ret_slots > 0 && dst != args_start {
                func.emit_copy(dst, args_start, ret_slots);
            }
            return Ok(());
        }
        
        // Function call - check if it's a Vo function (has body) or extern (no body)
        let obj = &info.project.tc_objs.lobjs[obj_key];
        
        if obj.entity_type().func_has_body() {
            // Vo function - use Call instruction
            let func_idx = ctx.get_func_by_objkey(obj_key)
                .ok_or_else(|| CodegenError::Internal(format!("function not registered: {:?}", ident.symbol)))?;
            
            let need_slots = total_arg_slots.max(ret_slots);
            let args_start = if ret_slots > 0 && ret_slots >= total_arg_slots {
                dst
            } else {
                func.alloc_slots(&vec![SlotType::Value; need_slots as usize])
            };
            
            compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;
            
            let c = crate::type_info::encode_call_args(total_arg_slots as u16, ret_slots as u16);
            let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_idx);
            func.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);
            
            if ret_slots > 0 && dst != args_start {
                func.emit_copy(dst, args_start, ret_slots);
            }
            return Ok(());
        } else {
            // Extern function (no body) - use CallExtern instruction
            let func_name = info.project.interner.resolve(ident.symbol)
                .ok_or_else(|| CodegenError::Internal("cannot resolve function name".to_string()))?;
            let pkg_key = obj.pkg();
            let pkg_name = pkg_key
                .map(|pk| {
                    let pkg = &info.project.tc_objs.pkgs[pk];
                    // Use full path with / replaced by _, but remove .. and . components
                    normalize_pkg_path(pkg.path())
                })
                .unwrap_or_else(|| "main".to_string());
            let extern_name = format!("{}_{}", pkg_name, func_name);
            return compile_extern_call(call, &extern_name, dst, ctx, func, info);
        }
    }
    
    // Non-ident function call (e.g., expression returning a closure)
    let closure_reg = compile_expr(&call.func, ctx, func, info)?;
    compile_closure_call_from_reg(expr, call, closure_reg, dst, ctx, func, info)
}

/// Compile closure call when closure is already in a register.
/// Used for func field calls like h.logic(args) and IIFE calls.
pub fn compile_closure_call_from_reg(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    closure_reg: u16,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let ret_slots = info.type_slot_count(info.expr_type(expr.id)) as u16;
    
    // Get function type from the closure expression to handle variadic properly
    let func_type = info.expr_type(call.func.id);
    let param_types = info.func_param_types(func_type);
    let is_variadic = info.is_variadic(func_type);
    
    // Calculate arg slots with variadic packing
    let total_arg_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
    
    let args_start = func.alloc_slots(&vec![SlotType::Value; total_arg_slots.max(ret_slots).max(1) as usize]);
    compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;
    
    let c = crate::type_info::encode_call_args(total_arg_slots, ret_slots);
    func.emit_op(Opcode::CallClosure, closure_reg, args_start, c);
    
    if ret_slots > 0 && dst != args_start {
        func.emit_copy(dst, args_start, ret_slots);
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
                let ret_slots = info.type_slot_count(info.expr_type(expr.id));
                let func_type = info.expr_type(call.func.id);
                let param_types = info.func_param_types(func_type);
                let is_variadic = info.is_variadic(func_type);
                
                // Compute total arg slots using PARAMETER types (handles interface conversion)
                let total_arg_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
                let args_start = func.alloc_slots(&vec![SlotType::Value; total_arg_slots.max(ret_slots).max(1) as usize]);
                
                // Compile arguments with interface conversion
                compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;
                
                let c = crate::type_info::encode_call_args(total_arg_slots, ret_slots);
                let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_idx);
                func.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);
                
                if ret_slots > 0 && dst != args_start {
                    func.emit_copy(dst, args_start, ret_slots);
                }
                return Ok(());
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
    let selection = info.get_selection(call.func.id);
    if let Some(sel_info) = selection {
        match sel_info.kind() {
            vo_analysis::selection::SelectionKind::FieldVal |
            vo_analysis::selection::SelectionKind::MethodExpr => {
                // FieldVal: struct field of function type
                // MethodExpr: T.Method(recv, args...) or (*T).Method(recv, args...)
                // Both compile to closure call
                let field_type = info.expr_type(call.func.id);
                if info.is_func_type(field_type) {
                    let closure_reg = compile_expr(&call.func, ctx, func, info)?;
                    return compile_closure_call_from_reg(expr, call, closure_reg, dst, ctx, func, info);
                }
            }
            vo_analysis::selection::SelectionKind::MethodVal => {
                // Method value is handled below via resolve_method_call
            }
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
    
    // Dispatch based on call type
    compile_method_call_dispatch(expr, call, sel, &call_info, dst, ctx, func, info)
}

/// Emit interface method call (common for Interface and EmbeddedInterface dispatch).
fn emit_interface_call(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
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
    let arg_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
    let ret_type = info.expr_type(expr.id);
    let ret_slots = info.type_slot_count(ret_type);
    let ret_slot_types = info.type_slot_types(ret_type);
    let buffer_size = arg_slots.max(ret_slots).max(1);
    let args_start = alloc_call_buffer(func, buffer_size, ret_slots, &ret_slot_types);
    
    compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;
    
    let c = crate::type_info::encode_call_args(arg_slots, ret_slots);
    func.emit_with_flags(Opcode::CallIface, method_idx as u8, iface_slot, args_start, c);
    
    if ret_slots > 0 && dst != args_start {
        func.emit_copy(dst, args_start, ret_slots);
    }
    Ok(())
}

/// Dispatch method call based on MethodCallInfo.
fn compile_method_call_dispatch(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    sel: &vo_syntax::ast::SelectorExpr,
    call_info: &crate::embed::MethodCallInfo,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use crate::embed::MethodDispatch;
    
    let method_name = info.project.interner.resolve(sel.sel.symbol).unwrap_or("?");
    let recv_type = info.expr_type(sel.expr.id);
    
    match &call_info.dispatch {
        MethodDispatch::Interface { method_idx } => {
            // Interface dispatch - interface is the receiver directly
            let iface_slot = compile_expr(&sel.expr, ctx, func, info)?;
            emit_interface_call(expr, call, recv_type, *method_idx, iface_slot, method_name, dst, ctx, func, info)
        }
        MethodDispatch::EmbeddedInterface { iface_type, method_idx } => {
            // Embedded interface dispatch - extract interface first
            let recv_is_ptr = info.is_pointer(recv_type);
            let recv_reg = compile_expr(&sel.expr, ctx, func, info)?;
            let iface_slot = func.alloc_interface();
            let start = crate::embed::TraverseStart::new(recv_reg, recv_is_ptr);
            call_info.emit_target(func, start, iface_slot);
            
            emit_interface_call(expr, call, *iface_type, *method_idx, iface_slot, method_name, dst, ctx, func, info)
        }
        MethodDispatch::Static { func_id, expects_ptr_recv } => {
            // Static call
            let base_type = if call_info.recv_is_pointer {
                info.pointer_base(recv_type)
            } else {
                recv_type
            };
            let actual_recv_type = call_info.actual_recv_type(base_type);
            
            // Get method signature
            let method_type = info.expr_type(call.func.id);
            let is_variadic = info.is_variadic(method_type);
            let param_types = info.func_param_types(method_type);
            
            // Calculate slots
            let recv_slots = if *expects_ptr_recv { 1 } else { info.type_slot_count(actual_recv_type) };
            let arg_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
            let total_slots = recv_slots + arg_slots;
            let ret_type = info.expr_type(expr.id);
            let ret_slots = info.type_slot_count(ret_type);
            // Return values are written to beginning of buffer, use proper types for GC tracking
            let ret_slot_types = info.type_slot_types(ret_type);
            let buffer_size = total_slots.max(ret_slots);
            let args_start = alloc_call_buffer(func, buffer_size, ret_slots, &ret_slot_types);
            
            // Emit receiver
            let recv_storage = if let ExprKind::Ident(ident) = &sel.expr.kind {
                func.lookup_local(ident.symbol).map(|local| local.storage)
            } else {
                None
            };
            emit_receiver(
                &sel.expr, args_start, recv_type, recv_storage,
                call_info, actual_recv_type, ctx, func, info
            )?;
            
            // Compile arguments
            compile_method_args(call, &param_types, is_variadic, args_start + recv_slots, ctx, func, info)?;
            
            // Call
            let c = crate::type_info::encode_call_args(total_slots, ret_slots);
            let (func_id_low, func_id_high) = crate::type_info::encode_func_id(*func_id);
            func.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);
            
            if ret_slots > 0 && dst != args_start {
                func.emit_copy(dst, args_start, ret_slots);
            }
            Ok(())
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
    let func_type = info.expr_type(call.func.id);
    let is_variadic = info.is_variadic(func_type);
    let param_types = info.func_param_types(func_type);
    
    // Get return slot count from the function's result type
    let sig = info.as_signature(func_type);
    let ret_slots = info.type_slot_count(sig.results()) as u16;
    let extern_id = ctx.get_or_register_extern_with_ret_slots(extern_name, ret_slots);
    
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
    args: &[vo_syntax::ast::Expr],
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
        Ok(format!("{}_{}", pkg_name, func_name))
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
) -> Result<u16, CodegenError> {
    // Tuple expansion for non-variadic calls: f(g()) where g() returns multiple values
    let arg_info = info.get_call_arg_info(&call.args, param_types);
    if arg_info.tuple_expand.is_some() {
        return compile_args_with_types(&call.args, param_types, args_start, ctx, func, info);
    }
    
    if is_variadic && !call.spread {
        let n_fixed = num_fixed_params(param_types, is_variadic);
        
        // Emit fixed arguments
        let fixed_args: Vec<_> = call.args.iter().take(n_fixed).cloned().collect();
        let mut offset = compile_args_with_types(&fixed_args, &param_types[..n_fixed], args_start, ctx, func, info)?;
        
        // Pack variadic arguments into slice (handles tuple expansion internally)
        let variadic_args: Vec<_> = call.args.iter().skip(n_fixed).collect();
        let elem_type = info.slice_elem_type(param_types.last().copied().unwrap());
        let slice_reg = pack_variadic_args(&variadic_args, elem_type, ctx, func, info)?;
        func.emit_copy(args_start + offset, slice_reg, 1);
        offset += 1;
        Ok(offset)
    } else {
        compile_args_with_types(&call.args, param_types, args_start, ctx, func, info)
    }
}

/// Calculate arg slots for method call.
pub fn calc_method_arg_slots(
    call: &vo_syntax::ast::CallExpr,
    param_types: &[TypeKey],
    is_variadic: bool,
    info: &TypeInfoWrapper,
) -> u16 {
    let arg_info = info.get_call_arg_info(&call.args, param_types);
    if arg_info.tuple_expand.is_some() {
        return param_types.iter().map(|&t| info.type_slot_count(t)).sum();
    }
    
    if is_variadic && !call.spread {
        let n_fixed = num_fixed_params(param_types, is_variadic);
        let fixed_slots: u16 = param_types.iter().take(n_fixed).map(|&t| info.type_slot_count(t)).sum();
        fixed_slots + 1  // +1 for packed slice
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

/// Normalize a package path for extern name generation.
/// Removes `.` and `..` components and replaces `/` with `_`.
/// E.g., "../../libs/vox" -> "libs_vox", "encoding/json" -> "encoding_json"
pub fn normalize_pkg_path(path: &str) -> String {
    path.split('/')
        .filter(|s| !s.is_empty() && *s != "." && *s != "..")
        .collect::<Vec<_>>()
        .join("_")
}
