//! Switch and type switch statement compilation.

use vo_analysis::objects::TypeKey;
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;
use vo_syntax::ast::{BinaryOp, StmtKind};

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::stmt::var_def::LocalDefiner;
use crate::type_info::TypeInfoWrapper;

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

/// Returns the concrete type only for a clause containing exactly one,
/// non-nil type. Mixed clauses keep the switched expression's interface type.
fn get_single_concrete_type(
    types: &[Option<vo_syntax::ast::TypeExpr>],
) -> Option<&vo_syntax::ast::TypeExpr> {
    if types.len() != 1 {
        return None;
    }
    types[0].as_ref()
}

/// Emit the nil-interface predicate used by every `nil` entry in a type case.
/// A typed nil still has a non-Void dynamic kind and therefore does not match.
fn emit_nil_interface_test(iface_slot: u16, func: &mut FuncBuilder) -> u16 {
    let ok_slot = func.alloc_slots(&[SlotType::Value]);
    let mask_slot = func.alloc_slots(&[SlotType::Value]);
    let kind_slot = func.alloc_slots(&[SlotType::Value]);

    // The dynamic ValueKind occupies the low byte of interface slot zero;
    // Void denotes a nil interface.
    func.emit_op(Opcode::LoadInt, mask_slot, 0xFF, 0);
    func.emit_op(Opcode::And, kind_slot, iface_slot, mask_slot);
    func.emit_op(Opcode::LoadInt, ok_slot, 0, 0);
    func.emit_op(Opcode::EqI, ok_slot, kind_slot, ok_slot);
    ok_slot
}

/// Emit binding for type switch case variable.
/// - `single_type`: Some(type_key) for single-type case (extract via IfaceAssert)
/// - `single_type`: None for multi-type case (keep interface type)
fn emit_type_switch_binding(
    name: vo_common::Symbol,
    single_type: Option<TypeKey>,
    case_span: vo_common::span::Span,
    iface_slot: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    if info.project.interner.resolve(name) == Some("_") {
        return Ok(());
    }

    let case_var_obj = info.get_implicit(case_span);
    // The checker creates a distinct implicit object for every clause. Its
    // type is concrete for a single-type clause and remains the switched
    // expression's exact interface type for nil, default, and multi-type
    // clauses. Reading that object avoids widening a non-empty interface to
    // `any` during code generation.
    let type_key = case_var_obj
        .map(|obj| info.obj_type(obj, "type-switch case variable must have type"))
        .unwrap_or_else(|| single_type.unwrap_or_else(|| info.any_type()));
    let slots = info.type_slot_count(type_key);
    let slot_types = info.type_slot_types(type_key);

    // Extract to a representation-neutral flattened temporary first. The
    // ordinary local-definition path below then selects StackArray, HeapArray,
    // HeapBoxed, or StackValue from the checked type and escape facts. Keeping
    // this decision centralized is essential for concrete array cases: a
    // captured or address-taken array must use the canonical ArrayHeader-backed
    // representation rather than a generic pointer box.
    let value_slot = func.alloc_slots(&slot_types);

    if single_type.is_some() {
        let (assert_kind, target_id) = compute_iface_assert_params(type_key, ctx, info);
        func.emit_iface_assert(
            assert_kind,
            false,
            value_slot,
            iface_slot,
            target_id,
            &slot_types,
        );
    } else {
        func.emit_copy(value_slot, iface_slot, slots);
    }

    let escapes = case_var_obj.is_some_and(|obj| info.is_escaped(obj));
    let mut definer = LocalDefiner::new(ctx, func, info);
    definer.define_local_from_slot(name, type_key, escapes, value_slot, case_var_obj)?;
    Ok(())
}

/// Compile type switch statement
/// Uses IfaceAssert instruction for type checking and value extraction.
pub(crate) fn compile_type_switch(
    type_switch: &vo_syntax::ast::TypeSwitchStmt,
    label: Option<vo_common::Symbol>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Enter scope for init variable shadowing (Go semantics: type switch x := v.(type) creates new scope)
    func.enter_scope();

    // Init statement
    if let Some(init) = &type_switch.init {
        super::compile_stmt(init, ctx, func, info)?;
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
    let iface_slot = func.alloc_slots(&[SlotType::Interface0, SlotType::Interface1]);
    func.emit_copy(iface_slot, expr_reg, 2);

    // Enter breakable context for break support
    func.enter_breakable(label);

    // Collect case jumps and info for variable binding
    let mut case_jumps: Vec<(usize, usize)> = Vec::new(); // (jump_pc, case_idx)
    let mut end_jumps: Vec<usize> = Vec::new();
    let mut default_case_idx: Option<usize> = None;

    // Generate type checks for each case using IfaceAssert
    for (case_idx, case) in type_switch.cases.iter().enumerate() {
        if case.types.is_empty() {
            // Default case (no types specified)
            default_case_idx = Some(case_idx);
        } else {
            // Preserve source order within a type list. `nil` is a regular
            // entry and may be mixed with concrete or interface types.
            for case_type in &case.types {
                match case_type {
                    None => {
                        let ok_slot = emit_nil_interface_test(iface_slot, func);
                        case_jumps.push((func.emit_jump(Opcode::JumpIf, ok_slot), case_idx));
                    }
                    Some(type_expr) => {
                        let type_key = info.type_expr_type(type_expr.id);

                        let (assert_kind, target_id) =
                            compute_iface_assert_params(type_key, ctx, info);

                        // Allocate temp for IfaceAssert result (value + ok)
                        let target_slots = info.type_slot_count(type_key);
                        let result_slots: u16 = if assert_kind == 1 { 2 } else { target_slots };
                        // result + ok bool
                        let mut assert_result_types = info.type_slot_types(type_key);
                        assert_result_types.push(SlotType::Value); // ok bool
                        let result_reg = func.alloc_slots(&assert_result_types); // +1 for ok bool
                        let ok_slot = result_reg + result_slots;

                        let result_layout = info.type_slot_types(type_key);
                        func.emit_iface_assert(
                            assert_kind,
                            true,
                            result_reg,
                            iface_slot,
                            target_id,
                            &result_layout,
                        );

                        // Jump to case body if ok is true
                        case_jumps.push((func.emit_jump(Opcode::JumpIf, ok_slot), case_idx));
                    }
                }
            }
        }
    }

    // Jump to default or end if no case matched
    let no_match_target = default_case_idx.unwrap_or(usize::MAX);
    let no_match_jump_pc = func.emit_jump(Opcode::Jump, 0);

    // Compile case bodies
    let mut case_body_starts: Vec<usize> = Vec::new();
    for case in type_switch.cases.iter() {
        case_body_starts.push(func.current_pc());

        // Enter scope for case-local variables (Go semantics: each case has its own scope)
        func.enter_scope();

        // If assign variable is specified, bind it to the asserted value
        // For default case (types.is_empty()), single_type is None -> interface type
        if let Some(assign_name) = &type_switch.assign {
            let single_type =
                get_single_concrete_type(&case.types).map(|te| info.type_expr_type(te.id));
            emit_type_switch_binding(
                assign_name.symbol,
                single_type,
                case.span,
                iface_slot,
                ctx,
                func,
                info,
            )?;
        }

        // Compile case body
        for stmt in &case.body {
            super::compile_stmt(stmt, ctx, func, info)?;
        }

        // Exit case scope
        func.exit_scope();

        // Jump to end
        end_jumps.push(func.emit_jump(Opcode::Jump, 0));
    }

    let end_pc = func.current_pc();

    // Patch case jumps
    for (jump_pc, case_idx) in &case_jumps {
        if *case_idx < case_body_starts.len() {
            func.patch_jump(*jump_pc, case_body_starts[*case_idx]);
        }
    }

    // Patch no match jump
    let no_match_pc = if no_match_target < case_body_starts.len() {
        case_body_starts[no_match_target]
    } else {
        end_pc
    };
    func.patch_jump(no_match_jump_pc, no_match_pc);

    // Exit breakable context and get break patches
    let break_patches = func.exit_breakable();

    // Patch end jumps (implicit break at end of case) and explicit breaks
    for jump_pc in end_jumps.into_iter().chain(break_patches) {
        func.patch_jump(jump_pc, end_pc);
    }

    // Exit outer scope (restore any shadowed variables from init)
    func.exit_scope();

    Ok(())
}

/// Emit comparison for switch case: tag vs case_val.
/// Returns the slot containing the comparison result (bool).
fn emit_switch_case_comparison(
    tag: u16,
    tag_type: TypeKey,
    case_expr: &vo_syntax::ast::Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    // Convert every case expression into the tag's detached runtime layout.
    // This covers untyped constants, interface conversions, canonical arrays,
    // and named values through the same assignment boundary.
    let case_layout = info
        .try_type_slot_types(tag_type)
        .map_err(CodegenError::Internal)?;
    let case_val = func.alloc_slots(&case_layout);
    crate::assign::emit_assign(
        case_val,
        crate::assign::AssignSource::Expr(case_expr),
        tag_type,
        ctx,
        func,
        info,
    )?;
    let cmp_result = func.alloc_slots(&[SlotType::Value]);

    if info.is_interface(tag_type) {
        func.emit_op(Opcode::IfaceEq, cmp_result, tag, case_val);
    } else if info.is_array(tag_type) || info.is_struct(tag_type) {
        let value_kinds = info
            .try_type_slot_value_kinds(tag_type)
            .map_err(CodegenError::Internal)?;
        crate::expr::comparison::compile_slot_comparison(
            &BinaryOp::Eq,
            tag,
            case_val,
            &case_layout,
            &value_kinds,
            cmp_result,
            func,
        )?;
    } else if info.is_string(tag_type) {
        func.emit_op(Opcode::StrEq, cmp_result, tag, case_val);
    } else if info.is_float(tag_type) {
        if info.is_float32(tag_type) {
            let tag_wide = func.alloc_slots(&[SlotType::Float]);
            let case_wide = func.alloc_slots(&[SlotType::Float]);
            func.emit_op(Opcode::ConvF32F64, tag_wide, tag, 0);
            func.emit_op(Opcode::ConvF32F64, case_wide, case_val, 0);
            func.emit_op(Opcode::EqF, cmp_result, tag_wide, case_wide);
        } else {
            func.emit_op(Opcode::EqF, cmp_result, tag, case_val);
        }
    } else {
        func.emit_op(Opcode::EqI, cmp_result, tag, case_val);
    }

    Ok(cmp_result)
}

/// Compile switch statement with optional label (for break support).
pub(crate) fn compile_switch(
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
        super::compile_stmt(init, ctx, func, info)?
    }

    // Evaluate and detach the tag exactly once before any case expression.
    let tag_type = switch_stmt.tag.as_ref().map(|tag| info.expr_type(tag.id));
    let tag_reg = if let (Some(tag), Some(tag_type)) = (&switch_stmt.tag, tag_type) {
        let tag_layout = info.type_slot_types(tag_type);
        let snapshot = func.alloc_slots(&tag_layout);
        crate::assign::emit_assign(
            snapshot,
            crate::assign::AssignSource::Expr(tag),
            tag_type,
            ctx,
            func,
            info,
        )?;
        Some(snapshot)
    } else {
        None
    };

    // Enter breakable context for break support
    func.enter_breakable(label);

    // Collect case jumps and body positions
    // case_jumps[i] -> case_body_idx[i]: maps each conditional jump to its target case
    let mut case_jumps: Vec<(usize, usize)> = Vec::new(); // (jump_pc, case_idx)
    let mut end_jumps: Vec<usize> = Vec::new();
    let mut default_case_idx: Option<usize> = None;

    // Generate comparison and conditional jumps for each case
    // NOTE: Default case jump is emitted AFTER all other cases are checked,
    // regardless of its position in the source code.
    for (case_idx, case) in switch_stmt.cases.iter().enumerate() {
        if case.exprs.is_empty() {
            default_case_idx = Some(case_idx);
        } else {
            for case_expr in &case.exprs {
                let cmp_result = if let (Some(tag), Some(tt)) = (tag_reg, tag_type) {
                    emit_switch_case_comparison(tag, tt, case_expr, ctx, func, info)?
                } else {
                    // Tagless switch: case expression is the condition itself
                    crate::expr::compile_expr(case_expr, ctx, func, info)?
                };

                case_jumps.push((func.emit_jump(Opcode::JumpIf, cmp_result), case_idx));
            }
        }
    }

    // After all case conditions checked: jump to default or end
    let fallthrough_jump = func.emit_jump(Opcode::Jump, 0);

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
                super::compile_stmt(stmt, ctx, func, info)?;
            }
        }

        // Exit case scope
        func.exit_scope();

        // Jump to end unless case ends with fallthrough
        if !has_fallthrough {
            end_jumps.push(func.emit_jump(Opcode::Jump, 0));
        }
    }

    // Exit breakable context and get break patches
    let break_patches = func.exit_breakable();

    let end_pc = func.current_pc();

    // Patch case condition jumps
    for (jump_pc, case_idx) in case_jumps {
        func.patch_jump(jump_pc, case_body_starts[case_idx]);
    }

    // Patch fallthrough jump: to default case if exists, otherwise to end
    let fallthrough_target = default_case_idx
        .map(|idx| case_body_starts[idx])
        .unwrap_or(end_pc);
    func.patch_jump(fallthrough_jump, fallthrough_target);

    // Patch end jumps (implicit break at end of case) and explicit breaks
    for jump_pc in end_jumps.into_iter().chain(break_patches) {
        func.patch_jump(jump_pc, end_pc);
    }

    // Exit scope (restore any shadowed variables from init)
    func.exit_scope();

    Ok(())
}
