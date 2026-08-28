//! Function and method call compilation.
#![allow(clippy::too_many_arguments)]

use vo_analysis::objects::{ObjKey, TypeKey};
use vo_analysis::selection::{Selection, SelectionKind};
use vo_common::abi::try_abi_lookup_name;
use vo_common::symbol::Symbol;
use vo_runtime::bytecode::ReturnShape;
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;
use vo_syntax::ast::{Expr, ExprKind};

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{ElemLayoutSpec, ExprSource, FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

use super::{compile_expr, compile_expr_to, preserves_preexisting_storage};

fn arguments_preserve_preexisting_storage(args: &[Expr]) -> bool {
    args.iter().all(preserves_preexisting_storage)
}

/// Preserve a function value before argument evaluation. A one-slot local
/// function variable is otherwise returned by `compile_expr` as its storage
/// slot, allowing an argument side effect to change which closure gets called.
pub(crate) fn snapshot_closure_value(src: u16, func: &mut FuncBuilder) -> u16 {
    let snapshot = func.alloc_slots(&[SlotType::GcBase]);
    func.emit_copy(snapshot, src, 1);
    snapshot
}

/// Compute slot types for the arg region of a call buffer, matching `calc_method_arg_slots`.
/// For variadic (non-spread) calls the packed slice contributes one exact object base.
pub(crate) fn calc_arg_slot_types(
    call: &vo_syntax::ast::CallExpr,
    param_types: &[TypeKey],
    is_variadic: bool,
    info: &TypeInfoWrapper,
) -> Vec<SlotType> {
    calc_arg_slot_types_for_args(&call.args, call.spread, param_types, is_variadic, info)
}

/// Compute slot types for the arg region of a call buffer, matching `calc_method_arg_slots`.
/// For variadic (non-spread) calls the packed slice contributes one exact object base.
pub(crate) fn calc_arg_slot_types_for_args(
    args: &[Expr],
    spread: bool,
    param_types: &[TypeKey],
    is_variadic: bool,
    info: &TypeInfoWrapper,
) -> Vec<SlotType> {
    let arg_info = info.get_call_arg_info(args, param_types);
    if arg_info.tuple_expand.is_some() {
        return param_types
            .iter()
            .flat_map(|&t| info.type_slot_types(t))
            .collect();
    }
    if is_variadic && !spread {
        let n_fixed = num_fixed_params(param_types, is_variadic);
        let mut types: Vec<SlotType> = param_types
            .iter()
            .take(n_fixed)
            .flat_map(|&t| info.type_slot_types(t))
            .collect();
        types.push(SlotType::GcBase);
        types
    } else {
        param_types
            .iter()
            .flat_map(|&t| info.type_slot_types(t))
            .collect()
    }
}

fn slot_types_for_type_keys(type_keys: &[TypeKey], info: &TypeInfoWrapper) -> Vec<SlotType> {
    type_keys
        .iter()
        .flat_map(|&type_key| info.type_slot_types(type_key))
        .collect()
}

fn func_result_slot_types(func_type: TypeKey, info: &TypeInfoWrapper) -> Vec<SlotType> {
    slot_types_for_type_keys(&info.func_result_types(func_type), info)
}

fn return_interface_metas_for_type_keys(
    type_keys: &[TypeKey],
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Vec<Option<u32>> {
    let mut metas = Vec::new();
    for &type_key in type_keys {
        let slot_types = info.type_slot_types(type_key);
        if info.is_interface(type_key) {
            let iface_meta_id = info.get_or_create_interface_meta_id(type_key, ctx);
            metas.push(Some(iface_meta_id));
            metas.extend((1..slot_types.len()).map(|_| None));
        } else {
            metas.extend((0..slot_types.len()).map(|_| None));
        }
    }
    metas
}

pub(crate) fn return_shape_for_type_keys(
    type_keys: &[TypeKey],
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<ReturnShape, CodegenError> {
    let slot_types = slot_types_for_type_keys(type_keys, info);
    let interface_metas = return_interface_metas_for_type_keys(type_keys, ctx, info);
    ReturnShape::try_with_slot_types_and_interface_metas(slot_types, interface_metas)
        .map_err(CodegenError::Internal)
}

pub(crate) fn get_extern_name_for_obj(
    obj_key: ObjKey,
    func_symbol: Symbol,
    info: &TypeInfoWrapper,
) -> Result<String, CodegenError> {
    let obj = &info.project.tc_objs.lobjs[obj_key];
    let func_name = info
        .project
        .interner
        .resolve(func_symbol)
        .ok_or_else(|| CodegenError::Internal("cannot resolve function name".to_string()))?;
    let pkg_name = obj
        .pkg()
        .map(|pkg_key| info.project.tc_objs.pkgs[pkg_key].abi_path().to_string())
        .unwrap_or_else(|| "main".to_string());
    encode_declared_extern_name(&pkg_name, func_name)
}

fn encode_declared_extern_name(
    package_path: &str,
    function_name: &str,
) -> Result<String, CodegenError> {
    try_abi_lookup_name(package_path, function_name).map_err(|error| {
        CodegenError::TargetLimit(format!(
            "extern {package_path}.{function_name} has an invalid ABI identity: {error}"
        ))
    })
}

#[derive(Clone, Copy)]
enum ResultPlacement {
    Fixed(u16),
    Natural,
}

impl ResultPlacement {
    fn materialized_slot(
        self,
        expr: &Expr,
        ctx: &CodegenContext,
        func: &mut FuncBuilder,
        info: &TypeInfoWrapper,
    ) -> Result<u16, CodegenError> {
        match self {
            Self::Fixed(slot) => Ok(slot),
            Self::Natural => {
                let slot_types = super::expr_runtime_slot_types(expr, ctx, func, info)?;
                Ok(func.alloc_slots(&slot_types))
            }
        }
    }

    fn finish_abi_call(
        self,
        expr: &Expr,
        ret_start: u16,
        actual_ret_slots: u16,
        func: &mut FuncBuilder,
        info: &TypeInfoWrapper,
    ) -> u16 {
        let Self::Fixed(dst) = self else {
            return ret_start;
        };
        copy_call_result(expr, dst, ret_start, actual_ret_slots, func, info);
        dst
    }
}

fn copy_call_result(
    expr: &Expr,
    dst: u16,
    ret_start: u16,
    actual_ret_slots: u16,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) {
    let used_ret_slots = info.type_slot_count(info.expr_type(expr.id));
    if used_ret_slots > 0 && actual_ret_slots > 0 && dst != ret_start {
        debug_assert_eq!(
            used_ret_slots, actual_ret_slots,
            "call expression result shape must match callee result shape when used"
        );
        func.emit_copy(dst, ret_start, used_ret_slots);
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
            info.project.tc_objs.lobjs[obj_key]
                .entity_type()
                .is_type_name()
        }
        ExprKind::Selector(sel) => {
            if let ExprKind::Ident(pkg_ident) = &strip_paren_expr(&sel.expr).kind {
                if info.package_path(pkg_ident).is_some() {
                    let obj_key = info.get_use(&sel.sel);
                    return info.project.tc_objs.lobjs[obj_key]
                        .entity_type()
                        .is_type_name();
                }
            }
            false
        }
        _ => false,
    }
}

fn explicit_interface_conversion_source<'a>(
    expr: &'a Expr,
    info: &TypeInfoWrapper,
) -> Option<&'a Expr> {
    let expr = strip_paren_expr(expr);
    if !info.is_interface(info.expr_type(expr.id)) {
        return None;
    }

    let source = match &expr.kind {
        ExprKind::Call(call)
            if !call.spread && call.args.len() == 1 && is_type_name_expr(&call.func, info) =>
        {
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
    let method_name = info
        .project
        .interner
        .resolve(method_sym)
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
    )
    .ok_or_else(|| {
        CodegenError::Internal(format!(
            "monomorphic interface target not found: type_key={:?}.{}",
            recv_type, method_name,
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
    result: ResultPlacement,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let func_type = info.expr_type(callee_expr.id);
    emit_direct_func_call_with_type(expr, call, func_type, func_idx, result, ctx, func, info)
}

fn emit_direct_func_call_with_type(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    func_type: TypeKey,
    func_idx: u32,
    result: ResultPlacement,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let ret_slot_types = func_result_slot_types(func_type, info);
    let ret_slots = ctx.slot_count_u16_or_record(ret_slot_types.len());
    let param_types = info.func_param_types(func_type);
    let is_variadic = info.is_variadic(func_type);
    let total_arg_slots_usize = calc_method_arg_slots(call, &param_types, is_variadic, info);
    let total_arg_slots = ctx.slot_count_u16_or_record(total_arg_slots_usize);
    let arg_slot_types = calc_arg_slot_types(call, &param_types, is_variadic, info);
    let args_start = func.alloc_call_buffer(&arg_slot_types, &ret_slot_types);

    compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;

    func.emit_static_call(func_idx, args_start);

    let ret_start = args_start + total_arg_slots;
    Ok(result.finish_abi_call(expr, ret_start, ret_slots, func, info))
}

fn compile_framework_entry_intrinsic(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    package_path: &str,
    function_name: &str,
    result: ResultPlacement,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<Option<u16>, CodegenError> {
    let (prefix, suffix, value_argument, argument_count) = match (package_path, function_name) {
        ("github.com/vo-lang/voplay", "Run") => ("__VoplayRun", "GeneratedGame", 0, 1),
        ("github.com/vo-lang/voplay", "Install") => ("__VoplayInstall", "GeneratedGame", 1, 3),
        _ => return Ok(None),
    };
    if call.args.len() != argument_count {
        return Err(CodegenError::Internal(format!(
            "{package_path}.{function_name} intrinsic requires {argument_count} arguments"
        )));
    }
    let concrete = info
        .named_type_name(info.expr_type(call.args[value_argument].id))
        .ok_or_else(|| {
            CodegenError::TargetLimit(format!(
                "{package_path}.{function_name} requires a generated typed entry adapter"
            ))
        })?;
    let entry_name = concrete.strip_suffix(suffix).ok_or_else(|| {
        CodegenError::TargetLimit(format!(
            "{package_path}.{function_name} argument {concrete} has no generated entry descriptor"
        ))
    })?;
    let helper_name = format!("{prefix}{entry_name}");
    let helper = info
        .lookup_current_package_object(&helper_name)
        .ok_or_else(|| {
            CodegenError::TargetLimit(format!(
                "{package_path}.{function_name} requires generated helper {helper_name}; run `vo generate`"
            ))
        })?;
    if !info.func_has_body(helper) {
        return Err(CodegenError::Internal(format!(
            "generated entry helper {helper_name} has no body"
        )));
    }
    let helper_index = ctx.get_func_by_objkey(helper).ok_or_else(|| {
        CodegenError::Internal(format!(
            "generated entry helper {helper_name} is not registered"
        ))
    })?;
    let helper_type = info.obj_type(helper, "generated entry helper must have a type");
    let slot = emit_direct_func_call_with_type(
        expr,
        call,
        helper_type,
        helper_index,
        result,
        ctx,
        func,
        info,
    )?;
    Ok(Some(slot))
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
    compile_call_with_placement(expr, call, ResultPlacement::Fixed(dst), ctx, func, info)
        .map(|_| ())
}

/// Compile a call whose result has no pre-existing destination. Ordinary Vo
/// calls retain the return region of their ABI buffer as the expression value,
/// avoiding a second frame slot and copy.
pub fn compile_call_natural(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    compile_call_with_placement(expr, call, ResultPlacement::Natural, ctx, func, info)
}

fn compile_call_with_placement(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    result: ResultPlacement,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let previous_span = func.replace_active_call_span(Some(expr.span));
    let compiled = compile_call_inner(expr, call, result, ctx, func, info);
    func.replace_active_call_span(previous_span);
    compiled
}

fn compile_call_inner(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    result: ResultPlacement,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    if let Some(spec) = ctx.scoped_call(info.package_key(), expr.id).cloned() {
        return compile_scoped_call(expr, result, &spec, ctx, func, info);
    }
    compile_call_inner_unscoped(expr, call, result, ctx, func, info)
}

fn compile_call_inner_unscoped(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    result: ResultPlacement,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let callee_expr = strip_paren_expr(&call.func);

    // Check if method call (selector expression)
    if let ExprKind::Selector(sel) = &callee_expr.kind {
        if let Some(selection) = info.get_selection(callee_expr.id) {
            if matches!(selection.kind(), SelectionKind::MethodExpr) {
                return compile_method_expr_call(
                    expr, call, sel, selection, result, ctx, func, info,
                );
            }
        }
        return compile_method_call(expr, call, callee_expr, sel, result, ctx, func, info);
    }

    // Check if builtin or type conversion
    if let ExprKind::Ident(ident) = &callee_expr.kind {
        // Use analysis phase info for builtin detection - correctly handles variable shadowing
        if let Some(builtin_id) = info.expr_builtin(callee_expr.id) {
            let dst = result.materialized_slot(expr, ctx, func, info)?;
            super::builtin::compile_builtin_call_by_id(
                expr, builtin_id, call, dst, ctx, func, info,
            )?;
            return Ok(dst);
        }

        // Check if this is a type conversion (ident refers to a type, not a function)
        // Type conversions look like function calls: T(x)
        {
            let obj_key = info.get_use(ident);
            let obj = &info.project.tc_objs.lobjs[obj_key];
            if obj.entity_type().is_type_name() {
                // This is a type conversion
                if call.args.len() == 1 {
                    let dst = result.materialized_slot(expr, ctx, func, info)?;
                    super::conversion::compile_type_conversion(
                        &call.args[0],
                        dst,
                        expr,
                        ctx,
                        func,
                        info,
                    )?;
                    return Ok(dst);
                } else if call.args.is_empty() {
                    // Zero value - already handled by default initialization
                    return result.materialized_slot(expr, ctx, func, info);
                }
            }
        }
    }

    if let ExprKind::FuncLit(func_lit) = &callee_expr.kind {
        if info.closure_captures(callee_expr.id).is_empty() {
            let (func_id, captures) =
                super::literal::lower_func_lit(callee_expr, func_lit, ctx, info)?;
            if !captures.is_empty() {
                panic!("zero-capture func literal lowering returned captures");
            }
            return emit_direct_func_call(
                expr,
                call,
                callee_expr,
                func_id,
                result,
                ctx,
                func,
                info,
            );
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
            return compile_closure_call(expr, call, callee_expr, result, ctx, func, info);
        }

        // Function call - check if it's a Vo function (has body) or extern (no body)
        let obj = &info.project.tc_objs.lobjs[obj_key];

        if obj.entity_type().func_has_body() {
            let func_idx = ctx.get_func_by_objkey(obj_key).ok_or_else(|| {
                CodegenError::Internal(format!("function not registered: {:?}", ident.symbol))
            })?;
            return emit_direct_func_call(
                expr,
                call,
                callee_expr,
                func_idx,
                result,
                ctx,
                func,
                info,
            );
        } else {
            // Extern function (no body) - use CallExtern instruction
            let extern_name = get_extern_name_for_obj(obj_key, ident.symbol, info)?;
            let dst = result.materialized_slot(expr, ctx, func, info)?;
            compile_extern_call(call, &extern_name, dst, ctx, func, info)?;
            return Ok(dst);
        }
    }

    // Non-ident function call (e.g., expression returning a closure)
    compile_closure_call(expr, call, callee_expr, result, ctx, func, info)
}

fn compile_scoped_call(
    expression: &Expr,
    result: ResultPlacement,
    spec: &crate::ScopedCallSpec,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let target = strip_paren_expr(&spec.target);
    let ExprKind::Call(target_call) = &target.kind else {
        return Err(CodegenError::Internal(
            "scoped adapter target must be a call expression".to_string(),
        ));
    };

    let scope_args = func.alloc_slots(&[
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcBase,
    ]);
    let identity = ctx.const_string(&spec.identity);
    func.emit_op(Opcode::StrNew, scope_args, identity, 0);
    let call_site = ctx.const_int(spec.call_site as i64);
    func.emit_op(Opcode::LoadConst, scope_args + 1, call_site, 0);
    func.emit_op(
        Opcode::LoadInt,
        scope_args + 2,
        u16::from(spec.key.is_some()),
        0,
    );
    if let Some(key) = &spec.key {
        compile_expr_to(key, scope_args + 3, ctx, func, info)?;
    } else {
        let empty = ctx.const_string("");
        func.emit_op(Opcode::StrNew, scope_args + 3, empty, 0);
    }
    let enter_types = [
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcBase,
    ];
    let enter = ctx.get_or_register_declared_extern_with_return_shape(
        &spec.enter_extern,
        ReturnShape::slots(0),
        crate::context::ext_slot_kinds_for_slot_types(&enter_types),
    );
    func.emit_call_extern(0, enter, scope_args, enter_types.len(), &[]);

    let target_result = compile_call_inner_unscoped(
        target,
        target_call,
        ResultPlacement::Natural,
        ctx,
        func,
        info,
    )?;

    let exit = ctx.get_or_register_declared_extern_with_return_shape(
        &spec.exit_extern,
        ReturnShape::slots(0),
        Vec::new(),
    );
    func.emit_call_extern(0, exit, 0, 0, &[]);

    if spec.key.is_none() {
        return Ok(result.finish_abi_call(expression, target_result, 1, func, info));
    }
    let key_extern = spec.key_extern.as_ref().ok_or_else(|| {
        CodegenError::Internal("keyed scoped adapter call has no key extern".to_string())
    })?;
    let key_args = func.alloc_call_buffer(&[SlotType::Value, SlotType::GcBase], &[SlotType::Value]);
    func.emit_copy(key_args, target_result, 1);
    func.emit_copy(key_args + 1, scope_args + 3, 1);
    let key = ctx.get_or_register_declared_extern_with_return_shape(
        key_extern,
        ReturnShape::slots(1),
        crate::context::ext_slot_kinds_for_slot_types(&[SlotType::Value, SlotType::GcBase]),
    );
    func.emit_call_extern(key_args + 2, key, key_args, 2, &[SlotType::Value]);
    Ok(result.finish_abi_call(expression, key_args + 2, 1, func, info))
}

fn compile_method_expr_call(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    sel: &vo_syntax::ast::SelectorExpr,
    selection: &vo_analysis::selection::Selection,
    result: ResultPlacement,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let recv_type = selection.recv().ok_or_else(|| {
        CodegenError::Internal("method expression has no receiver type".to_string())
    })?;
    let recv_arg = call.args.first().ok_or_else(|| {
        CodegenError::Internal("method expression call missing receiver argument".to_string())
    })?;
    let forwarded_args = &call.args[1..];
    let method_name = info
        .project
        .interner
        .resolve(sel.sel.symbol)
        .ok_or_else(|| CodegenError::Internal("cannot resolve method name".to_string()))?;
    if info.is_interface(recv_type) {
        if let Some(target) = resolve_monomorphic_iface_target(recv_arg, sel.sel.symbol, ctx, info)?
        {
            let method_type = info.project.tc_objs.lobjs[target.method_obj]
                .typ()
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
                result,
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
    )
    .ok_or_else(|| {
        CodegenError::Internal(format!(
            "method not found: type_key={:?}.{}",
            recv_type, method_name
        ))
    })?;

    let method_type = info.project.tc_objs.lobjs[selection.obj()]
        .typ()
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
        result,
        ctx,
        func,
        info,
    )
}

/// Evaluate a dynamic callee before its arguments and use the call buffer's
/// mandatory hidden receiver slot as that snapshot. Frame materialization
/// overwrites the slot only after dispatch has consumed the closure value.
fn compile_closure_call(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    callee_expr: &Expr,
    result: ResultPlacement,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    // Get function type from the closure expression to handle variadic properly
    let func_type = info.expr_type(callee_expr.id);
    let ret_slot_types = func_result_slot_types(func_type, info);
    let ret_slots = ctx.slot_count_u16_or_record(ret_slot_types.len());
    let param_types = info.func_param_types(func_type);
    let is_variadic = info.is_variadic(func_type);

    // Calculate arg slots with variadic packing
    let total_arg_slots_usize = calc_method_arg_slots(call, &param_types, is_variadic, info);
    let total_arg_slots = ctx.slot_count_u16_or_record(total_arg_slots_usize);

    let direct_closure = if arguments_preserve_preexisting_storage(&call.args) {
        match super::get_expr_source(callee_expr, ctx, func, info) {
            ExprSource::Location(StorageKind::Reference { slot })
            | ExprSource::Location(StorageKind::StackValue { slot, slots: 1 }) => Some(slot),
            _ => None,
        }
    } else {
        None
    };
    let hidden_slot_type = if direct_closure.is_some() {
        SlotType::Value
    } else {
        SlotType::GcBase
    };
    let arg_slot_types = calc_arg_slot_types(call, &param_types, is_variadic, info);
    let args_start =
        func.alloc_dynamic_call_buffer(&[hidden_slot_type], &arg_slot_types, &ret_slot_types);
    let closure_reg = if let Some(slot) = direct_closure {
        slot
    } else {
        let snapshot = args_start
            .checked_sub(1)
            .expect("closure call buffer must reserve one hidden receiver slot");
        compile_expr_to(callee_expr, snapshot, ctx, func, info)?;
        snapshot
    };
    compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;

    func.emit_call_closure(closure_reg, args_start, &arg_slot_types, &ret_slot_types);

    let ret_start = args_start + total_arg_slots;
    Ok(result.finish_abi_call(expr, ret_start, ret_slots, func, info))
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

    // A named array with a value-receiver method may live in canonical
    // global/escaped/captured storage. Method frames always use the flattened
    // value ABI, so cross that representation boundary explicitly while the
    // receiver is evaluated (before any call argument).
    if !expects_ptr_recv && info.is_array(actual_recv_type) {
        if !embed_path.steps.is_empty() {
            return Err(CodegenError::Internal(
                "array value receiver unexpectedly has an embedding path".to_string(),
            ));
        }
        return crate::array_value::prepare_expr(sel_expr, actual_recv_type, ctx, func, info)?
            .emit_to_flat(args_start, actual_recv_type, ctx, func, info);
    }

    // Special case: expression needing pointer with no embedding path - use compile_expr_to_ptr
    // This handles auto-addressing (escaping stack values to heap when pointer needed)
    // Only applies when there's no embed path to traverse (no pointer steps, zero offset)
    if recv_storage.is_none() && expects_ptr_recv && embed_path.steps.is_empty() {
        return super::compile_expr_to_ptr(sel_expr, args_start, ctx, func, info);
    }

    // Determine initial register, pointer state, and base offset from storage
    let start = match recv_storage {
        Some(StorageKind::HeapBoxed {
            gcref_slot,
            stores_pointer,
            ..
        }) => {
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
    crate::embed::emit_embed_path_traversal(
        func,
        start,
        &embed_path.steps,
        expects_ptr_recv,
        value_slots,
        args_start,
    );

    Ok(())
}

fn compile_method_call(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    callee_expr: &Expr,
    sel: &vo_syntax::ast::SelectorExpr,
    result: ResultPlacement,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    // 1. Check for package function call or type conversion (e.g., bytes.Contains, json.Number)
    if let ExprKind::Ident(pkg_ident) = &sel.expr.kind {
        // Check if it's a package reference
        if let Some(package_path) = info.package_path(pkg_ident) {
            // Check if sel.sel refers to a type (type conversion: pkg.Type(x))
            let obj_key = info.get_use(&sel.sel);
            let obj = &info.project.tc_objs.lobjs[obj_key];
            if obj.entity_type().is_type_name() {
                // This is a type conversion: pkg.Type(x)
                if call.args.len() == 1 {
                    let dst = result.materialized_slot(expr, ctx, func, info)?;
                    super::conversion::compile_type_conversion(
                        &call.args[0],
                        dst,
                        expr,
                        ctx,
                        func,
                        info,
                    )?;
                    return Ok(dst);
                } else if call.args.is_empty() {
                    // Zero value - already handled by default initialization
                    return result.materialized_slot(expr, ctx, func, info);
                }
            }

            // A package variable whose value has function type is a dynamic
            // callee. Evaluate and snapshot its closure value before arguments,
            // exactly like a local or struct-field function value.
            if obj.entity_type().is_var() && info.is_func_type(info.expr_type(callee_expr.id)) {
                return compile_closure_call(expr, call, callee_expr, result, ctx, func, info);
            }

            let function_name = info
                .project
                .interner
                .resolve(sel.sel.symbol)
                .ok_or_else(|| {
                    CodegenError::Internal("cannot resolve function name".to_string())
                })?;
            if let Some(slot) = compile_framework_entry_intrinsic(
                expr,
                call,
                &package_path,
                function_name,
                result,
                ctx,
                func,
                info,
            )? {
                return Ok(slot);
            }

            // Check if it's a Vo function (has body) or extern (no body)
            if obj.entity_type().func_has_body() {
                // Vo function - use normal Call with proper interface conversion
                // Use ObjKey to avoid cross-package Symbol collision
                let func_idx = ctx.get_func_by_objkey(obj_key).ok_or_else(|| {
                    CodegenError::Internal(format!("pkg func not registered: {:?}", sel.sel.symbol))
                })?;
                return emit_direct_func_call(
                    expr,
                    call,
                    callee_expr,
                    func_idx,
                    result,
                    ctx,
                    func,
                    info,
                );
            }
            // Extern function - use CallExtern
            let extern_name = get_extern_name(sel, info)?;
            let dst = result.materialized_slot(expr, ctx, func, info)?;
            compile_extern_call(call, &extern_name, dst, ctx, func, info)?;
            return Ok(dst);
        }
    }

    let recv_type = info.expr_type(sel.expr.id);

    let method_name = info
        .project
        .interner
        .resolve(sel.sel.symbol)
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
                    return compile_closure_call(expr, call, callee_expr, result, ctx, func, info);
                }
            }
            SelectionKind::MethodVal => {
                // Method value is handled below via resolve_method_call
            }
        }
    }

    if info.is_interface(recv_type) {
        if let Some(target) =
            resolve_monomorphic_iface_target(&sel.expr, sel.sel.symbol, ctx, info)?
        {
            let method_type = info.project.tc_objs.lobjs[target.method_obj]
                .typ()
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
                result,
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
    )
    .ok_or_else(|| {
        CodegenError::Internal(format!(
            "method not found: type_key={:?}.{}",
            recv_type, method_name
        ))
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
        result,
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
    result: ResultPlacement,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let base_type = if call_info.recv_is_pointer {
        info.pointer_base(recv_type)
    } else {
        recv_type
    };
    let actual_recv_type = call_info.actual_recv_type(base_type);
    let is_variadic = info.is_variadic(method_type);
    let param_types = info.func_param_types(method_type);
    let recv_slots_usize = if expects_ptr_recv {
        1
    } else {
        usize::from(info.type_slot_count(actual_recv_type))
    };
    let recv_slots = ctx.slot_count_u16_or_record(recv_slots_usize);
    let arg_slots_usize =
        calc_method_arg_slots_for_args(args, spread, &param_types, is_variadic, info);
    let total_slots_usize = recv_slots_usize + arg_slots_usize;
    let total_slots = ctx.slot_count_u16_or_record(total_slots_usize);
    let ret_slot_types = func_result_slot_types(method_type, info);
    let ret_slots = ctx.slot_count_u16_or_record(ret_slot_types.len());
    let recv_slot_types: Vec<SlotType> = if expects_ptr_recv {
        vec![SlotType::GcRef]
    } else {
        info.type_slot_types(actual_recv_type)
    };
    let arg_slot_types_only =
        calc_arg_slot_types_for_args(args, spread, &param_types, is_variadic, info);
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
    compile_method_args_for_args(
        args,
        spread,
        &param_types,
        is_variadic,
        args_start + recv_slots,
        ctx,
        func,
        info,
    )?;

    func.emit_static_call(func_id, args_start);

    let ret_start = args_start + total_slots;
    Ok(result.finish_abi_call(expr, ret_start, ret_slots, func, info))
}

fn emit_interface_call_with_args(
    expr: &Expr,
    args: &[Expr],
    spread: bool,
    iface_type: vo_analysis::objects::TypeKey,
    method_idx: u32,
    iface_snapshot: u16,
    method_name: &str,
    result: ResultPlacement,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let (param_types, is_variadic) = info.get_interface_method_signature(iface_type, method_name);
    let arg_slots_usize =
        calc_method_arg_slots_for_args(args, spread, &param_types, is_variadic, info);
    let arg_slots = ctx.slot_count_u16_or_record(arg_slots_usize);
    let ret_slot_types = slot_types_for_type_keys(
        &info.get_interface_method_result_types(iface_type, method_name),
        info,
    );
    let ret_slots = ctx.slot_count_u16_or_record(ret_slot_types.len());
    let arg_slot_types =
        calc_arg_slot_types_for_args(args, spread, &param_types, is_variadic, info);
    let args_start =
        func.alloc_dynamic_call_buffer(&[SlotType::Value], &arg_slot_types, &ret_slot_types);

    compile_method_args_for_args(
        args,
        spread,
        &param_types,
        is_variadic,
        args_start,
        ctx,
        func,
        info,
    )?;

    let iface_meta_id = ctx.get_or_create_interface_meta_id(
        iface_type,
        &info.project.tc_objs,
        &info.project.interner,
    );
    let method_idx = ctx.call_iface_method_index_or_record(method_idx as usize);
    func.emit_call_iface(
        iface_meta_id,
        method_idx,
        iface_snapshot,
        args_start,
        &arg_slot_types,
        &ret_slot_types,
    );

    let ret_start = args_start + arg_slots;
    Ok(result.finish_abi_call(expr, ret_start, ret_slots, func, info))
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
    result: ResultPlacement,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    use crate::embed::MethodDispatch;

    match &call_info.dispatch {
        MethodDispatch::Interface { method_idx } => {
            // An inert argument list cannot change a direct interface
            // location, so dispatch may consume it in place. Every other case
            // retains the authoritative pre-argument snapshot required by the
            // language's call evaluation order.
            let direct_iface = if arguments_preserve_preexisting_storage(args) {
                match super::get_expr_source(recv_expr, ctx, func, info) {
                    ExprSource::Location(StorageKind::StackValue { slot, slots: 2 }) => Some(slot),
                    _ => None,
                }
            } else {
                None
            };
            let iface_snapshot = if let Some(slot) = direct_iface {
                slot
            } else {
                let snapshot = func.alloc_interface();
                compile_expr_to(recv_expr, snapshot, ctx, func, info)?;
                snapshot
            };
            emit_interface_call_with_args(
                expr,
                args,
                spread,
                recv_type,
                *method_idx,
                iface_snapshot,
                method_name,
                result,
                ctx,
                func,
                info,
            )
        }
        MethodDispatch::EmbeddedInterface {
            iface_type,
            method_idx,
        } => {
            // Embedded interface dispatch - extract interface first
            let recv_is_ptr = info.is_pointer(recv_type);
            let recv_reg = compile_expr(recv_expr, ctx, func, info)?;
            // Embedded traversal produces the same authoritative snapshot
            // directly, so every interface dispatch observes one uniform
            // receiver-evaluation contract.
            let iface_snapshot = func.alloc_interface();
            let start = crate::embed::TraverseStart::new(recv_reg, recv_is_ptr);
            call_info.emit_target(func, start, iface_snapshot);

            emit_interface_call_with_args(
                expr,
                args,
                spread,
                *iface_type,
                *method_idx,
                iface_snapshot,
                method_name,
                result,
                ctx,
                func,
                info,
            )
        }
        MethodDispatch::Static {
            func_id,
            expects_ptr_recv,
        } => {
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
                result,
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
    let func_type = info.expr_type(call.func.id);
    let is_variadic = info.is_variadic(func_type);
    let param_types = info.func_param_types(func_type);
    let total_slots_usize = calc_method_arg_slots(call, &param_types, is_variadic, info);
    let _total_slots = ctx.slot_count_u16_or_record(total_slots_usize);
    let mut arg_slot_types = calc_arg_slot_types(call, &param_types, is_variadic, info);
    let param_kinds = crate::context::ext_slot_kinds_for_slot_types(&arg_slot_types);

    // Get return slot count from the function's result type
    let result_types = info.func_result_types(func_type);
    let returns = return_shape_for_type_keys(&result_types, ctx, info)?;
    let ret_slot_types = returns.slot_types.clone();
    let extern_id =
        ctx.get_or_register_declared_extern_with_return_shape(extern_name, returns, param_kinds);

    // Use compile_method_args for proper type conversion (e.g., boxing to `any`),
    // and allocate the call buffer with the same slot layout that callees,
    // extern bridges, GC, and JIT verification will observe.
    if arg_slot_types.is_empty() {
        arg_slot_types.push(SlotType::Value);
    }
    let args_start = func.alloc_slots(&arg_slot_types);
    compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;

    func.emit_call_extern(
        dst,
        extern_id,
        args_start,
        total_slots_usize,
        &ret_slot_types,
    );
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

    if arg_info.tuple_expand.is_some() {
        // Multi-value expansion: compile tuple once, then convert each element
        let tuple = super::CompiledTuple::compile(&args[0], ctx, func, info)?;

        let mut offset = 0u16;
        let mut elem_idx = 0usize;
        tuple.for_each_element_result(info, |elem_slot, elem_type| {
            let pt = param_types[elem_idx];
            let pt_slots = info.type_slot_count(pt);
            crate::assign::emit_assign(
                args_start + offset,
                crate::assign::AssignSource::Slot {
                    slot: elem_slot,
                    type_key: elem_type,
                },
                pt,
                ctx,
                func,
                info,
            )?;
            offset += pt_slots;
            elem_idx += 1;
            Ok::<(), CodegenError>(())
        })?;
        Ok(offset)
    } else {
        // Normal case: one arg per param
        let mut offset = 0u16;
        for (i, arg) in args.iter().enumerate() {
            if let Some(&pt) = param_types.get(i) {
                crate::assign::emit_assign(
                    args_start + offset,
                    crate::assign::AssignSource::Expr(arg),
                    pt,
                    ctx,
                    func,
                    info,
                )?;
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

/// Get the canonical extern name for a package function call.
pub fn get_extern_name(
    sel: &vo_syntax::ast::SelectorExpr,
    info: &TypeInfoWrapper,
) -> Result<String, CodegenError> {
    if let ExprKind::Ident(pkg_ident) = &sel.expr.kind {
        let package_path = info
            .package_abi_path(pkg_ident)
            .ok_or_else(|| CodegenError::Internal("cannot resolve package".to_string()))?;
        let func_name = info
            .project
            .interner
            .resolve(sel.sel.symbol)
            .ok_or_else(|| CodegenError::Internal("cannot resolve function name".to_string()))?;
        encode_declared_extern_name(&package_path, func_name)
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
    compile_method_args_for_args(
        &call.args,
        call.spread,
        param_types,
        is_variadic,
        args_start,
        ctx,
        func,
        info,
    )?;
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
        let mut offset = compile_args_with_types(
            &fixed_args,
            &param_types[..n_fixed],
            args_start,
            ctx,
            func,
            info,
        )?;

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
) -> usize {
    calc_method_arg_slots_for_args(&call.args, call.spread, param_types, is_variadic, info)
}

pub(crate) fn calc_method_arg_slots_for_args(
    args: &[Expr],
    spread: bool,
    param_types: &[TypeKey],
    is_variadic: bool,
    info: &TypeInfoWrapper,
) -> usize {
    let arg_info = info.get_call_arg_info(args, param_types);
    if arg_info.tuple_expand.is_some() {
        return param_types
            .iter()
            .map(|&t| usize::from(info.type_slot_count(t)))
            .sum();
    }

    if is_variadic && !spread {
        let n_fixed = num_fixed_params(param_types, is_variadic);
        let fixed_slots: usize = param_types
            .iter()
            .take(n_fixed)
            .map(|&t| usize::from(info.type_slot_count(t)))
            .sum();
        fixed_slots + 1
    } else {
        param_types
            .iter()
            .map(|&t| usize::from(info.type_slot_count(t)))
            .sum()
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
    let elem_bytes = vo_analysis::check::type_info::elem_bytes_for_heap(elem_type, info.tc_objs());
    let elem_slot_types = info.type_slot_types(elem_type);
    let elem_vk = info.type_value_kind(elem_type);

    // Calculate total element count (expanding tuples)
    let total_elems = variadic_args.iter().try_fold(0usize, |total, arg| {
        let arg_type = info.expr_type(arg.id);
        let expanded = if info.is_tuple(arg_type) {
            info.tuple_len(arg_type)
        } else {
            1
        };
        total.checked_add(expanded).ok_or_else(|| {
            CodegenError::Internal("variadic argument element count overflow".to_string())
        })
    })?;
    let total_elems_i64 = i64::try_from(total_elems).map_err(|_| {
        CodegenError::Internal(format!(
            "variadic argument element count exceeds i64::MAX: {total_elems}"
        ))
    })?;

    // Get element meta
    let elem_meta_idx = ctx.get_or_create_value_meta(elem_type, info);
    let meta_reg = func.alloc_slots(&[SlotType::Value]);
    func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);

    // Create slice
    let dst = func.alloc_slots(&[SlotType::GcBase]);
    let len_cap_reg = func.alloc_slots(&[SlotType::Value; 2]);
    let total_elems_idx = ctx.const_int(total_elems_i64);
    func.emit_op(Opcode::LoadConst, len_cap_reg, total_elems_idx, 0); // len
    func.emit_op(Opcode::LoadConst, len_cap_reg + 1, total_elems_idx, 0); // cap = len
    func.emit_slice_new(
        dst,
        meta_reg,
        len_cap_reg,
        ElemLayoutSpec::new(elem_bytes, elem_vk, &elem_slot_types),
    );

    // Helper to set one slice element
    let mut slice_idx = 0usize;
    let mut set_elem = |val_reg: u16,
                        func: &mut FuncBuilder,
                        ctx: &mut CodegenContext|
     -> Result<(), CodegenError> {
        let idx_reg = func.alloc_slots(&[SlotType::Value]);
        let index = i64::try_from(slice_idx).map_err(|_| {
            CodegenError::Internal(format!(
                "variadic argument index exceeds i64::MAX: {slice_idx}"
            ))
        })?;
        let index_const = ctx.const_int(index);
        func.emit_op(Opcode::LoadConst, idx_reg, index_const, 0);
        func.emit_slice_set(
            dst,
            idx_reg,
            val_reg,
            ElemLayoutSpec::new(elem_bytes, elem_vk, &elem_slot_types),
        );
        slice_idx = slice_idx.checked_add(1).ok_or_else(|| {
            CodegenError::Internal("variadic argument index overflow".to_string())
        })?;
        Ok(())
    };

    // Set each element (expanding tuples as needed)
    for elem in variadic_args.iter() {
        let arg_type = info.expr_type(elem.id);

        if info.is_tuple(arg_type) {
            // Tuple expansion: compile once, set each element
            let tuple = super::CompiledTuple::compile(elem, ctx, func, info)?;
            tuple.for_each_element_result(info, |src_slot, src_type| {
                let val_reg = func.alloc_slots(&elem_slot_types);
                crate::assign::emit_assign(
                    val_reg,
                    crate::assign::AssignSource::Slot {
                        slot: src_slot,
                        type_key: src_type,
                    },
                    elem_type,
                    ctx,
                    func,
                    info,
                )?;
                set_elem(val_reg, func, ctx)?;
                Ok::<(), CodegenError>(())
            })?;
        } else {
            let val_reg = func.alloc_slots(&elem_slot_types);
            crate::assign::emit_assign(
                val_reg,
                crate::assign::AssignSource::Expr(elem),
                elem_type,
                ctx,
                func,
                info,
            )?;
            set_elem(val_reg, func, ctx)?;
        }
    }

    Ok(dst)
}
