//! Function and method call compilation.

use vo_analysis::objects::TypeKey;
use vo_runtime::SlotType;
use vo_syntax::ast::{Expr, ExprKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

use super::{compile_expr, compile_expr_to, get_gcref_slot};

/// Allocate a call buffer with proper slot types for return values.
/// 
/// Return values are written to the beginning of the buffer after the call,
/// so the first `ret_slots` slots must have correct types for GC tracking.
/// The remaining slots (for arguments) are typed as Value.
fn alloc_call_buffer(func: &mut FuncBuilder, buffer_size: u16, ret_slots: u16, ret_slot_types: &[SlotType]) -> u16 {
    if ret_slots == 0 || buffer_size == 0 {
        return func.alloc_temp(buffer_size.max(1));
    }
    
    // Build combined slot types: ret_slot_types for return value, Value for the rest
    let mut types = ret_slot_types.to_vec();
    for _ in ret_slots..buffer_size {
        types.push(SlotType::Value);
    }
    func.alloc_temp_typed(&types)
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
        let name = info.project.interner.resolve(ident.symbol);
        if let Some(name) = name {
            if super::builtin::is_builtin(name) {
                return super::builtin::compile_builtin_call(expr, name, call, dst, ctx, func, info);
            }
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
        // First check if it's a known function
        if let Some(func_idx) = ctx.get_function_index(ident.symbol) {
            // Optimization: use dst directly as args_start if it has enough space
            // This avoids a Copy after the call
            let need_slots = total_arg_slots.max(ret_slots);
            let args_start = if ret_slots > 0 && ret_slots >= total_arg_slots {
                // dst has enough space, use it directly
                dst
            } else {
                // Need more space for args than ret, allocate separately
                func.alloc_temp(need_slots)
            };
            
            // Compile args with variadic packing support
            compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;
            
            // Call: a=func_id, b=args_start, c=(arg_slots<<8|ret_slots), flags=func_id_high
            let c = crate::type_info::encode_call_args(total_arg_slots as u16, ret_slots as u16);
            let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_idx);
            func.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);
            
            // Copy result to dst if not already there
            if ret_slots > 0 && dst != args_start {
                func.emit_copy(dst, args_start, ret_slots);
            }
            return Ok(());
        }
        
        // Check if it's a local variable (could be a closure)
        if func.lookup_local(ident.symbol).is_some() || func.lookup_capture(ident.symbol).is_some() {
            // Closure call - compile closure expression first
            let closure_reg = compile_expr(&call.func, ctx, func, info)?;
            
            // Compile arguments - allocate max(arg_slots, ret_slots) for return values
            let args_start = func.alloc_temp(total_arg_slots.max(ret_slots));
            compile_args_with_types(&call.args, &[], args_start, ctx, func, info)?;
            
            // CallClosure: a=closure, b=args_start, c=(arg_slots<<8|ret_slots)
            let c = crate::type_info::encode_call_args(total_arg_slots as u16, ret_slots as u16);
            func.emit_op(Opcode::CallClosure, closure_reg, args_start, c);
            
            // Copy result to dst if needed
            if ret_slots > 0 && dst != args_start {
                func.emit_copy(dst, args_start, ret_slots);
            }
            
            return Ok(());
        }
        
        // Extern function (no body, e.g., bytes.Index called from bytes.Contains)
        if !ctx.is_vo_function(ident.symbol) {
            let func_name = info.project.interner.resolve(ident.symbol)
                .ok_or_else(|| CodegenError::Internal("cannot resolve function name".to_string()))?;
            let obj_key = info.get_use(ident);
            let pkg_key = info.project.tc_objs.lobjs[obj_key].pkg();
            let pkg_path = pkg_key
                .map(|pk| info.project.tc_objs.pkgs[pk].path().to_string())
                .unwrap_or_else(|| "main".to_string());
            let extern_name = format!("{}_{}", pkg_path.replace("/", "_"), func_name);
            return compile_extern_call(call, &extern_name, dst, ctx, func, info);
        }
        
        // Unknown function - error
        let func_name = info.project.interner.resolve(ident.symbol).unwrap_or("?");
        return Err(CodegenError::Internal(format!("unknown function: {}", func_name)));
    }
    
    // Non-ident function call (e.g., expression returning a closure)
    let closure_reg = compile_expr(&call.func, ctx, func, info)?;
    compile_closure_call_from_reg(expr, call, closure_reg, dst, ctx, func, info)
}

/// Compile closure call when closure is already in a register.
/// Used for func field calls like h.logic(args).
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
    let total_arg_slots: u16 = call.args.iter().map(|a| info.expr_slots(a.id)).sum();
    
    let args_start = func.alloc_temp(total_arg_slots.max(ret_slots).max(1));
    compile_args_with_types(&call.args, &[], args_start, ctx, func, info)?;
    
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
/// Cases:
/// - `expects_ptr_recv=true`: pass GcRef (variable must be heap-allocated)
/// - `expects_ptr_recv=false`: pass value (copy from stack or dereference from heap)
/// - `embed_is_pointer=true`: embedded field is a pointer, need extra dereference
pub fn emit_receiver(
    sel_expr: &Expr,
    args_start: u16,
    recv_type: TypeKey,
    recv_storage: Option<StorageKind>,
    expects_ptr_recv: bool,
    actual_recv_type: TypeKey,
    embed_offset: u16,
    embed_is_pointer: bool,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    if expects_ptr_recv {
        // Method expects *T: pass GcRef directly
        if embed_is_pointer && embed_offset == 0 {
            // Pointer embedding: the embedded field IS the pointer we need
            let recv_reg = compile_expr(sel_expr, ctx, func, info)?;
            func.emit_copy(args_start, recv_reg, 1);
        } else if let Some(gcref_slot) = recv_storage.as_ref().and_then(get_gcref_slot) {
            func.emit_copy(args_start, gcref_slot, 1);
        } else {
            let recv_reg = compile_expr(sel_expr, ctx, func, info)?;
            func.emit_copy(args_start, recv_reg, 1);
        }
    } else {
        // Method expects T: pass value
        let recv_is_ptr = info.is_pointer(recv_type);
        let value_slots = info.type_slot_count(actual_recv_type);
        
        if embed_is_pointer {
            // Pointer embedding: first get the embedded pointer, then dereference it
            let recv_reg = compile_expr(sel_expr, ctx, func, info)?;
            if recv_is_ptr {
                // recv is *OuterStruct, need to read embedded *T from it, then dereference
                // Step 1: Read the embedded *T pointer from OuterStruct
                let temp_ptr = func.alloc_temp(1);
                func.emit_with_flags(Opcode::PtrGetN, 1, temp_ptr, recv_reg, embed_offset);
                // Step 2: Dereference the embedded *T to get the value
                func.emit_with_flags(Opcode::PtrGetN, value_slots as u8, args_start, temp_ptr, 0);
            } else {
                // recv is OuterStruct on stack, embedded *T is at recv_reg + embed_offset
                let ptr_reg = recv_reg + embed_offset;  // Slot containing the embedded *T pointer
                func.emit_with_flags(Opcode::PtrGetN, value_slots as u8, args_start, ptr_reg, 0);
            }
        } else {
            match recv_storage {
                Some(StorageKind::HeapBoxed { gcref_slot, .. }) => {
                    // Heap boxed: directly read from GcRef slot
                    func.emit_with_flags(Opcode::PtrGetN, value_slots as u8, args_start, gcref_slot, embed_offset);
                }
                Some(StorageKind::HeapArray { gcref_slot, .. }) => {
                    // Heap array: directly read from GcRef slot
                    func.emit_with_flags(Opcode::PtrGetN, value_slots as u8, args_start, gcref_slot, embed_offset);
                }
                _ if recv_is_ptr => {
                    // Pointer receiver: compile_expr gives us the pointer, then dereference
                    let recv_reg = compile_expr(sel_expr, ctx, func, info)?;
                    func.emit_with_flags(Opcode::PtrGetN, value_slots as u8, args_start, recv_reg, embed_offset);
                }
                _ => {
                    // Stack variable or other: compile_expr gives us the value, just copy
                    let recv_reg = compile_expr(sel_expr, ctx, func, info)?;
                    func.emit_copy(args_start, recv_reg + embed_offset, value_slots);
                }
            }
        }
    }
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
            if ctx.is_vo_function(sel.sel.symbol) {
                // Vo function - use normal Call with proper interface conversion
                let func_idx = ctx.get_function_index(sel.sel.symbol).unwrap();
                let ret_slots = info.type_slot_count(info.expr_type(expr.id));
                let func_type = info.expr_type(call.func.id);
                let param_types = info.func_param_types(func_type);
                let is_variadic = info.is_variadic(func_type);
                
                // Compute total arg slots using PARAMETER types (handles interface conversion)
                let total_arg_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
                let args_start = func.alloc_temp(total_arg_slots.max(ret_slots).max(1));
                
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
    
    // Check if this is a func field call (e.g., h.logic(args) where logic is a func field)
    let selection = info.get_selection(call.func.id);
    if let Some(sel_info) = selection {
        if *sel_info.kind() == vo_analysis::selection::SelectionKind::FieldVal {
            // This is a field access, not a method call
            // Check if the field type is a function
            let field_type = info.expr_type(call.func.id);
            if info.is_func_type(field_type) {
                // Compile as: get field value (closure), then call it
                let closure_reg = compile_expr(&call.func, ctx, func, info)?;
                return compile_closure_call_from_reg(expr, call, closure_reg, dst, ctx, func, info);
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
            // Interface dispatch
            let recv_reg = compile_expr(&sel.expr, ctx, func, info)?;
            let (param_types, is_variadic) = info.get_interface_method_signature(recv_type, method_name);
            let arg_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
            let ret_type = info.expr_type(expr.id);
            let ret_slots = info.type_slot_count(ret_type);
            // Return values are written to beginning of buffer, use proper types for GC tracking
            let ret_slot_types = info.type_slot_types(ret_type);
            let buffer_size = arg_slots.max(ret_slots).max(1);
            let args_start = alloc_call_buffer(func, buffer_size, ret_slots, &ret_slot_types);
            
            compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;
            
            let c = crate::type_info::encode_call_args(arg_slots, ret_slots);
            func.emit_with_flags(Opcode::CallIface, *method_idx as u8, recv_reg, args_start, c);
            
            if ret_slots > 0 && dst != args_start {
                func.emit_copy(dst, args_start, ret_slots);
            }
            Ok(())
        }
        MethodDispatch::EmbeddedInterface { embed_offset, iface_type, method_idx } => {
            // Embedded interface dispatch
            let recv_reg = compile_expr(&sel.expr, ctx, func, info)?;
            let iface_slot = func.alloc_interface();  // Interface is 2 slots
            func.emit_ptr_get(iface_slot, recv_reg, *embed_offset, 2);
            
            let (param_types, is_variadic) = info.get_interface_method_signature(*iface_type, method_name);
            let arg_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
            let ret_type = info.expr_type(expr.id);
            let ret_slots = info.type_slot_count(ret_type);
            // Return values are written to beginning of buffer, use proper types for GC tracking
            let ret_slot_types = info.type_slot_types(ret_type);
            let buffer_size = arg_slots.max(ret_slots).max(1);
            let args_start = alloc_call_buffer(func, buffer_size, ret_slots, &ret_slot_types);
            
            compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;
            
            let c = crate::type_info::encode_call_args(arg_slots, ret_slots);
            func.emit_with_flags(Opcode::CallIface, *method_idx as u8, iface_slot, args_start, c);
            
            if ret_slots > 0 && dst != args_start {
                func.emit_copy(dst, args_start, ret_slots);
            }
            Ok(())
        }
        MethodDispatch::Static { func_id, expects_ptr_recv } => {
            // Static call - use call_info directly
            let base_type = if call_info.recv_is_pointer {
                info.pointer_base(recv_type)
            } else {
                recv_type
            };
            
            // Determine actual receiver type from embed path
            let actual_recv_type = if call_info.embed_path.steps.is_empty() {
                base_type
            } else {
                call_info.embed_path.final_type
            };
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
            let embed_offset = call_info.embed_path.total_offset;
            let embed_is_pointer = call_info.embed_path.steps.iter().any(|s| s.is_pointer);
            emit_receiver(
                &sel.expr, args_start, recv_type, recv_storage,
                *expects_ptr_recv, actual_recv_type, embed_offset, embed_is_pointer,
                ctx, func, info
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
    let extern_id = ctx.get_or_register_extern(extern_name);
    let func_type = info.expr_type(call.func.id);
    let is_variadic = info.is_variadic(func_type);
    let param_types = info.func_param_types(func_type);
    
    // Use compile_method_args for proper type conversion (e.g., boxing to `any`)
    let total_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
    let args_start = func.alloc_temp(total_slots.max(1));
    compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;
    
    func.emit_with_flags(Opcode::CallExtern, total_slots as u8, dst, extern_id as u16, args_start);
    Ok(())
}

// =============================================================================
// Argument Compilation Helpers
// =============================================================================

/// Compile arguments without type conversion, return (args_start, total_slots).
/// Used by non-variadic calls and defer.
pub fn compile_args_simple(
    args: &[vo_syntax::ast::Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(u16, u16), CodegenError> {
    let total_slots: u16 = args.iter().map(|arg| info.expr_slots(arg.id)).sum();
    let args_start = func.alloc_args(total_slots);
    let mut offset = 0u16;
    for arg in args {
        let slots = info.expr_slots(arg.id);
        compile_expr_to(arg, args_start + offset, ctx, func, info)?;
        offset += slots;
    }
    Ok((args_start, total_slots))
}

/// Compile arguments with parameter types for automatic interface conversion.
/// Used by method calls and defer with known param types.
pub fn compile_args_with_types(
    args: &[vo_syntax::ast::Expr],
    param_types: &[TypeKey],
    args_start: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let mut offset = 0u16;
    for (i, arg) in args.iter().enumerate() {
        if let Some(&pt) = param_types.get(i) {
            crate::stmt::compile_value_to(arg, args_start + offset, pt, ctx, func, info)?;
            offset += info.type_slot_count(pt);
        } else {
            let slots = info.expr_slots(arg.id);
            compile_expr_to(arg, args_start + offset, ctx, func, info)?;
            offset += slots;
        }
    }
    Ok(offset)
}

/// Get extern name for a package function call
fn get_extern_name(
    sel: &vo_syntax::ast::SelectorExpr,
    info: &TypeInfoWrapper,
) -> Result<String, CodegenError> {
    if let ExprKind::Ident(pkg_ident) = &sel.expr.kind {
        let pkg_path = info.package_path(pkg_ident)
            .ok_or_else(|| CodegenError::Internal("cannot resolve package".to_string()))?;
        let func_name = info.project.interner.resolve(sel.sel.symbol)
            .ok_or_else(|| CodegenError::Internal("cannot resolve function name".to_string()))?;
        Ok(format!("{}_{}", pkg_path.replace("/", "_"), func_name))
    } else {
        Err(CodegenError::Internal("expected package.func".to_string()))
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
    let num_fixed_params = if is_variadic && !param_types.is_empty() {
        param_types.len() - 1
    } else {
        param_types.len()
    };
    
    if is_variadic && !call.spread {
        // Emit fixed arguments with automatic interface conversion
        let fixed_params = &param_types[..num_fixed_params];
        let fixed_args: Vec<_> = call.args.iter().take(num_fixed_params).cloned().collect();
        let mut offset = compile_args_with_types(&fixed_args, fixed_params, args_start, ctx, func, info)?;
        
        // Pack variadic arguments into slice
        let variadic_args: Vec<&vo_syntax::ast::Expr> = call.args.iter().skip(num_fixed_params).collect();
        let slice_type = param_types.last().copied().unwrap();
        let elem_type = info.slice_elem_type(slice_type);
        let slice_reg = pack_variadic_args(&variadic_args, elem_type, ctx, func, info)?;
        func.emit_copy(args_start + offset, slice_reg, 1);
        offset += 1;
        Ok(offset)
    } else {
        // Non-variadic or spread: emit all arguments with automatic interface conversion
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
    let num_fixed_params = if is_variadic && !param_types.is_empty() {
        param_types.len() - 1
    } else {
        param_types.len()
    };
    
    if is_variadic && !call.spread {
        let fixed_slots: u16 = param_types.iter().take(num_fixed_params)
            .map(|&t| info.type_slot_count(t)).sum();
        fixed_slots + 1  // +1 for the packed slice
    } else {
        param_types.iter().map(|&t| info.type_slot_count(t)).sum()
    }
}

/// Pack variadic arguments into a slice.
/// For `f(a, b, c)` where f is variadic, this creates `[]T{a, b, c}` and returns its register.
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
    let len = variadic_args.len();
    let elem_slots = info.type_slot_count(elem_type);
    let elem_bytes = (elem_slots as usize) * 8;
    let elem_slot_types = info.type_slot_types(elem_type);
    let elem_vk = info.type_value_kind(elem_type);
    
    // Get element meta
    let elem_meta_idx = ctx.get_or_create_value_meta_with_kind(Some(elem_type), elem_slots, &elem_slot_types, Some(elem_vk));
    let meta_reg = func.alloc_temp(1);
    func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);
    
    // Create slice
    let dst = func.alloc_temp(1);
    let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
    let num_regs = if flags == 0 { 3 } else { 2 };
    let len_cap_reg = func.alloc_temp(num_regs);
    let (b, c) = crate::type_info::encode_i32(len as i32);
    func.emit_op(Opcode::LoadInt, len_cap_reg, b, c);      // len
    func.emit_op(Opcode::LoadInt, len_cap_reg + 1, b, c);  // cap = len
    if flags == 0 {
        let eb_idx = ctx.const_int(elem_bytes as i64);
        func.emit_op(Opcode::LoadConst, len_cap_reg + 2, eb_idx, 0);
    }
    func.emit_with_flags(Opcode::SliceNew, flags, dst, meta_reg, len_cap_reg);
    
    // Set each element
    for (i, elem) in variadic_args.iter().enumerate() {
        let val_reg = if info.is_interface(elem_type) {
            let iface_reg = func.alloc_temp(elem_slots);
            crate::stmt::compile_iface_assign(iface_reg, elem, elem_type, ctx, func, info)?;
            iface_reg
        } else {
            compile_expr(elem, ctx, func, info)?
        };
        let idx_reg = func.alloc_temp(1);
        func.emit_op(Opcode::LoadInt, idx_reg, i as u16, 0);
        func.emit_slice_set(dst, idx_reg, val_reg, elem_bytes, elem_vk, ctx);
    }
    
    Ok(dst)
}
