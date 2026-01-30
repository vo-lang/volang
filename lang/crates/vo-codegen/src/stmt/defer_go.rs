//! Defer and go statement compilation.

use vo_vm::instruction::Opcode;

use vo_analysis::objects::TypeKey;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

/// Call signature info extracted from a call expression
struct CallSigInfo {
    param_types: Vec<TypeKey>,
    is_variadic: bool,
}

impl CallSigInfo {
    fn from_call(call_expr: &vo_syntax::ast::CallExpr, info: &TypeInfoWrapper) -> Self {
        let func_type = info.expr_type(call_expr.func.id);
        Self {
            param_types: info.func_param_types(func_type),
            is_variadic: info.is_variadic(func_type),
        }
    }
    
    fn calc_arg_slots(&self, call_expr: &vo_syntax::ast::CallExpr, info: &TypeInfoWrapper) -> u16 {
        crate::expr::call::calc_method_arg_slots(call_expr, &self.param_types, self.is_variadic, info)
    }
    
    fn compile_args(
        &self,
        call_expr: &vo_syntax::ast::CallExpr,
        args_start: u16,
        ctx: &mut CodegenContext,
        func: &mut FuncBuilder,
        info: &TypeInfoWrapper,
    ) -> Result<(), CodegenError> {
        crate::expr::call::compile_method_args(
            call_expr, &self.param_types, self.is_variadic, args_start, ctx, func, info
        )?;
        Ok(())
    }
}

/// Compile defer statement
/// DeferPush instruction format:
/// - a: func_id (flags bit 0 = 0) or closure_reg (flags bit 0 = 1)
/// - b: arg_start
/// - c: arg_slots
/// - flags bit 0: is_closure
pub(crate) fn compile_defer(
    call: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    compile_defer_impl(call, ctx, func, info, false)
}

/// Compile errdefer statement
pub(crate) fn compile_errdefer(
    call: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    compile_defer_impl(call, ctx, func, info, true)
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
    
    // Check for package-qualified function call (e.g., fmt.Println, bytes.Contains)
    // Must check before treating as method call
    if let ExprKind::Selector(sel) = &call_expr.func.kind {
        if let ExprKind::Ident(pkg_ident) = &sel.expr.kind {
            if info.package_path(pkg_ident).is_some() {
                // Package function call - compile as extern or Vo function
                return compile_defer_pkg_func_call(call_expr, sel, opcode, ctx, func, info);
            }
        }
        // Method call (e.g., res.close())
        return compile_defer_method_call(call_expr, sel, opcode, ctx, func, info);
    }
    
    let sig = CallSigInfo::from_call(call_expr, info);
    
    // Regular function call
    if let ExprKind::Ident(ident) = &call_expr.func.kind {
        let obj_key = info.get_use(ident);
        if let Some(func_idx) = ctx.get_func_by_objkey(obj_key) {
            let (args_start, total_arg_slots) = compile_call_args(call_expr, &sig, ctx, func, info)?;
            emit_defer_func(opcode, func_idx, args_start, total_arg_slots, func);
            return Ok(());
        }
    }
    
    // Closure call (local variable or generic expression)
    let closure_reg = crate::expr::compile_expr(&call_expr.func, ctx, func, info)?;
    let (args_start, total_arg_slots) = compile_call_args(call_expr, &sig, ctx, func, info)?;
    emit_defer_closure(opcode, closure_reg, args_start, total_arg_slots, func);
    Ok(())
}

/// Compile call arguments for defer/go, returns (args_start, total_arg_slots)
fn compile_call_args(
    call_expr: &vo_syntax::ast::CallExpr,
    sig: &CallSigInfo,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(u16, u16), CodegenError> {
    let total_arg_slots = sig.calc_arg_slots(call_expr, info);
    let args_start = func.alloc_args(total_arg_slots);
    sig.compile_args(call_expr, args_start, ctx, func, info)?;
    Ok((args_start, total_arg_slots))
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

/// Compile defer for package-qualified function call (e.g., defer fmt.Println(...), defer bytes.Contains(...))
fn compile_defer_pkg_func_call(
    call_expr: &vo_syntax::ast::CallExpr,
    sel: &vo_syntax::ast::SelectorExpr,
    opcode: Opcode,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let obj_key = info.get_use(&sel.sel);
    let obj = &info.project.tc_objs.lobjs[obj_key];
    
    if obj.entity_type().func_has_body() {
        // Vo function - use DeferPush with func_id
        let func_idx = ctx.get_func_by_objkey(obj_key)
            .ok_or_else(|| CodegenError::Internal(format!("pkg func not registered: {:?}", sel.sel.symbol)))?;
        let sig = CallSigInfo::from_call(call_expr, info);
        let (args_start, total_arg_slots) = compile_call_args(call_expr, &sig, ctx, func, info)?;
        emit_defer_func(opcode, func_idx, args_start, total_arg_slots, func);
        return Ok(());
    }
    
    // Extern function (builtin like fmt.Println)
    // For extern functions, we need to generate a wrapper closure that calls the extern
    // and defer that wrapper
    let extern_name = crate::expr::call::get_extern_name(sel, info)?;
    
    // Get return slot count from function signature
    let func_type = info.expr_type(call_expr.func.id);
    let sig = info.as_signature(func_type);
    let ret_slots = info.type_slot_count(sig.results()) as u16;
    
    let wrapper_id = crate::wrapper::generate_defer_extern_wrapper(ctx, &extern_name, call_expr.args.len(), ret_slots);
    
    // Compile args as interface values for print/println/assert style functions
    let any_type = info.any_type();
    let total_arg_slots = call_expr.args.len() as u16 * 2; // each arg is interface (2 slots)
    let args_start = func.alloc_args(total_arg_slots);
    
    for (i, arg) in call_expr.args.iter().enumerate() {
        let dst = args_start + (i as u16 * 2);
        crate::assign::emit_assign(dst, crate::assign::AssignSource::Expr(arg), any_type, ctx, func, info)?;
    }
    
    emit_defer_func(opcode, wrapper_id, args_start, total_arg_slots, func);
    Ok(())
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
    use crate::embed::MethodDispatch;
    
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
    
    match &call_info.dispatch {
        MethodDispatch::Static { func_id, expects_ptr_recv } => {
            // Static dispatch - compile receiver and args, emit DeferPush
            let base_type = if call_info.recv_is_pointer { info.pointer_base(recv_type) } else { recv_type };
            let actual_recv_type = call_info.actual_recv_type(base_type);
            let recv_storage = match &sel.expr.kind {
                ExprKind::Ident(ident) => func.lookup_local(ident.symbol).map(|l| l.storage),
                _ => None,
            };
            
            let sig = CallSigInfo::from_call(call_expr, info);
            let recv_slots = if *expects_ptr_recv { 1 } else { info.type_slot_count(actual_recv_type) };
            let other_arg_slots = sig.calc_arg_slots(call_expr, info);
            let total_arg_slots = recv_slots + other_arg_slots;
            let args_start = func.alloc_args(total_arg_slots);
            
            crate::expr::emit_receiver(
                &sel.expr, args_start, recv_type, recv_storage,
                &call_info, actual_recv_type, ctx, func, info
            )?;
            sig.compile_args(call_expr, args_start + recv_slots, ctx, func, info)?;
            
            emit_defer_func(opcode, *func_id, args_start, total_arg_slots, func);
        }
        MethodDispatch::Interface { method_idx } => {
            // Direct interface dispatch - generate wrapper
            compile_defer_iface_call(
                call_expr, sel, opcode, recv_type, *method_idx, method_name,
                &call_info.embed_path.steps, false, ctx, func, info
            )?;
        }
        MethodDispatch::EmbeddedInterface { method_idx, .. } => {
            // Embedded interface dispatch - extract interface first
            compile_defer_iface_call(
                call_expr, sel, opcode, recv_type, *method_idx, method_name,
                &call_info.embed_path.steps, true, ctx, func, info
            )?;
        }
    }
    Ok(())
}

/// Helper for defer on interface method call.
/// Generates a wrapper function and emits defer with interface value + args.
fn compile_defer_iface_call(
    call_expr: &vo_syntax::ast::CallExpr,
    sel: &vo_syntax::ast::SelectorExpr,
    opcode: Opcode,
    recv_type: vo_analysis::objects::TypeKey,
    method_idx: u32,
    method_name: &str,
    embed_steps: &[crate::embed::EmbedStep],
    is_embedded: bool,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let sig = CallSigInfo::from_call(call_expr, info);
    let arg_slots = sig.calc_arg_slots(call_expr, info);
    
    let wrapper_id = crate::wrapper::generate_defer_iface_wrapper(
        ctx, method_name, method_idx as usize, arg_slots, 0
    );
    
    let total_arg_slots = 2 + arg_slots;
    let args_start = func.alloc_args(total_arg_slots);
    
    // Compile interface receiver
    let iface_reg = crate::expr::compile_expr(&sel.expr, ctx, func, info)?;
    if is_embedded {
        let recv_is_ptr = info.is_pointer(recv_type);
        let start = crate::embed::TraverseStart::new(iface_reg, recv_is_ptr);
        crate::embed::emit_embed_path_traversal(func, start, embed_steps, false, 2, args_start);
    } else {
        func.emit_copy(args_start, iface_reg, 2);
    }
    
    // Compile other args
    sig.compile_args(call_expr, args_start + 2, ctx, func, info)?;
    
    emit_defer_func(opcode, wrapper_id, args_start, total_arg_slots, func);
    Ok(())
}

// === Go statement ===

#[inline]
fn emit_go_func(func_idx: u32, args_start: u16, arg_slots: u16, func: &mut FuncBuilder) {
    let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_idx);
    func.emit_with_flags(Opcode::GoStart, func_id_high << 1, func_id_low, args_start, arg_slots);
}

#[inline]
fn emit_go_closure(closure_reg: u16, args_start: u16, arg_slots: u16, func: &mut FuncBuilder) {
    func.emit_with_flags(Opcode::GoStart, 1, closure_reg, args_start, arg_slots);
}

/// Compile go statement
/// GoStart: a=func_id/closure, b=args_start, c=arg_slots, flags bit0=is_closure
/// GoIsland: a=island, b=closure, c=args_start, flags=arg_slots
pub(crate) fn compile_go(
    target_island: Option<&vo_syntax::ast::Expr>,
    call: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::ExprKind;
    
    let ExprKind::Call(call_expr) = &call.kind else {
        return Err(CodegenError::UnsupportedStmt("go requires a call expression".to_string()));
    };
    
    // go @(island) - cross-island goroutine
    if let Some(island_expr) = target_island {
        // Compile island expression
        let island_reg = crate::expr::compile_expr(island_expr, ctx, func, info)?;
        
        // The call must be a closure literal for go @(island)
        // go @(i) func(args...) { ... }(values...)
        let closure_reg = crate::expr::compile_expr(&call_expr.func, ctx, func, info)?;
        
        // Compile call arguments
        let sig = CallSigInfo::from_call(call_expr, info);
        let (args_start, total_arg_slots) = compile_call_args(call_expr, &sig, ctx, func, info)?;
        
        // GoIsland: a=island, b=closure, c=args_start, flags=arg_slots (max 255)
        assert!(total_arg_slots <= 255, "go @(island) call has too many argument slots (max 255)");
        func.emit_with_flags(Opcode::GoIsland, total_arg_slots as u8, island_reg, closure_reg, args_start);
        return Ok(());
    }
    
    // Regular go (same island)
    let sig = CallSigInfo::from_call(call_expr, info);
    
    // Regular function call
    if let ExprKind::Ident(ident) = &call_expr.func.kind {
        let obj_key = info.get_use(ident);
        if let Some(func_idx) = ctx.get_func_by_objkey(obj_key) {
            let (args_start, total_arg_slots) = compile_call_args(call_expr, &sig, ctx, func, info)?;
            emit_go_func(func_idx, args_start, total_arg_slots, func);
            return Ok(());
        }
    }
    
    // Closure call (local variable or generic expression)
    let closure_reg = crate::expr::compile_expr(&call_expr.func, ctx, func, info)?;
    let (args_start, total_arg_slots) = compile_call_args(call_expr, &sig, ctx, func, info)?;
    emit_go_closure(closure_reg, args_start, total_arg_slots, func);
    Ok(())
}
