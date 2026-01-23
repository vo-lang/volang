//! Switch and type switch statement compilation.

use vo_analysis::objects::TypeKey;
use vo_runtime::SlotType;
use vo_syntax::ast::StmtKind;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
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

/// Returns the single concrete type if exactly one exists, None otherwise.
fn get_single_concrete_type(types: &[Option<vo_syntax::ast::TypeExpr>]) -> Option<&vo_syntax::ast::TypeExpr> {
    let mut iter = types.iter().filter_map(|t| t.as_ref());
    let first = iter.next()?;
    if iter.next().is_some() {
        None // More than one concrete type
    } else {
        Some(first)
    }
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
) {
    let type_key = single_type.unwrap_or_else(|| info.any_type());
    let slots = info.type_slot_count(type_key);
    let slot_types = info.type_slot_types(type_key);
    
    let case_var_obj = info.get_implicit(case_span);
    let needs_boxing = case_var_obj
        .map(|obj| info.needs_boxing(obj, type_key))
        .unwrap_or(false);
    
    // Emit value extraction to temp or directly to var slot
    let value_slot = if needs_boxing {
        func.alloc_temp_typed(&slot_types)
    } else {
        func.define_local_stack(name, slots, &slot_types)
    };
    
    if single_type.is_some() {
        let (assert_kind, target_id) = compute_iface_assert_params(type_key, ctx, info);
        let flags = assert_kind | ((slots as u8) << 3);
        func.emit_with_flags(Opcode::IfaceAssert, flags, value_slot, iface_slot, target_id as u16);
    } else {
        func.emit_copy(value_slot, iface_slot, slots);
    }
    
    // Box if captured by closure
    if needs_boxing {
        let stores_pointer = info.is_pointer(type_key);
        let gcref_slot = func.define_local_heap_boxed(name, slots, stores_pointer);
        let meta_idx = ctx.get_or_create_value_meta(type_key, info);
        let meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        func.emit_with_flags(Opcode::PtrNew, slots as u8, gcref_slot, meta_reg, 0);
        func.emit_ptr_set(gcref_slot, 0, value_slot, slots);
    }
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
    let iface_slot = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
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
            // Check if this is a nil-only case (all types are None)
            let is_nil_only_case = case.types.iter().all(|t| t.is_none());
            
            if is_nil_only_case {
                // case nil: check if interface is nil (value_kind == Void)
                // nil interface has slot0 with value_kind = 0 (Void) in low 8 bits
                let ok_slot = func.alloc_temp_typed(&[SlotType::Value]);
                let mask_slot = func.alloc_temp_typed(&[SlotType::Value]);
                let vk_slot = func.alloc_temp_typed(&[SlotType::Value]);
                
                // Load mask 0xFF to extract value_kind
                func.emit_op(Opcode::LoadInt, mask_slot, 0xFF, 0);
                // Extract value_kind: vk = slot0 & 0xFF
                func.emit_op(Opcode::And, vk_slot, iface_slot, mask_slot);
                // Check if value_kind == 0 (Void means nil)
                func.emit_op(Opcode::LoadInt, ok_slot, 0, 0);
                func.emit_op(Opcode::EqI, ok_slot, vk_slot, ok_slot);
                
                // Jump to case body if ok is true (interface is nil)
                case_jumps.push((func.emit_jump(Opcode::JumpIf, ok_slot), case_idx));
            } else {
                // Type case - check each type
                for type_opt in &case.types {
                    if let Some(type_expr) = type_opt {
                        let type_key = info.type_expr_type(type_expr.id);
                        
                        let (assert_kind, target_id) = compute_iface_assert_params(type_key, ctx, info);
                        
                        // Allocate temp for IfaceAssert result (value + ok)
                        let target_slots = info.type_slot_count(type_key) as u8;
                        let result_slots: u16 = if assert_kind == 1 { 2 } else { target_slots as u16 };
                        // result + ok bool
                        let mut assert_result_types = info.type_slot_types(type_key);
                        assert_result_types.push(SlotType::Value); // ok bool
                        let result_reg = func.alloc_temp_typed(&assert_result_types); // +1 for ok bool
                        let ok_slot = result_reg + result_slots;
                        
                        // IfaceAssert: a=dst, b=src_iface, c=target_id
                        // flags = assert_kind | (has_ok << 2) | (target_slots << 3)
                        let flags = assert_kind | (1 << 2) | ((target_slots) << 3);
                        func.emit_with_flags(Opcode::IfaceAssert, flags, result_reg, iface_slot, target_id as u16);
                        
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
            let single_type = get_single_concrete_type(&case.types)
                .map(|te| info.type_expr_type(te.id));
            emit_type_switch_binding(
                assign_name.symbol, single_type, case.span, iface_slot,
                ctx, func, info,
            );
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
    let case_val = crate::expr::compile_expr(case_expr, ctx, func, info)?;
    let cmp_result = func.alloc_temp_typed(&[SlotType::Value]);
    
    if info.is_interface(tag_type) {
        // Interface comparison: box case value to interface and use IfaceEq
        let case_type = info.expr_type(case_expr.id);
        let case_iface = if info.is_interface(case_type) {
            case_val
        } else {
            // Box concrete case value to interface for comparison
            let iface_slot = func.alloc_interfaces(1);
            crate::assign::emit_iface_assign_from_concrete(
                iface_slot, case_val, case_type, info.any_type(), ctx, func, info
            )?;
            iface_slot
        };
        func.emit_op(Opcode::IfaceEq, cmp_result, tag, case_iface);
    } else if info.is_string(tag_type) {
        func.emit_op(Opcode::StrEq, cmp_result, tag, case_val);
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
    
    // Compile tag expression (if present)
    let tag_reg = if let Some(tag) = &switch_stmt.tag {
        Some(crate::expr::compile_expr(tag, ctx, func, info)?)
    } else {
        None
    };
    
    // Enter breakable context for break support
    func.enter_breakable(label);
    
    // Collect case jumps and body positions
    // case_jumps[i] -> case_body_idx[i]: maps each conditional jump to its target case
    let mut case_jumps: Vec<(usize, usize)> = Vec::new();  // (jump_pc, case_idx)
    let mut end_jumps: Vec<usize> = Vec::new();
    let mut default_case_idx: Option<usize> = None;
    
    // Get tag type for comparison dispatch
    let tag_type = switch_stmt.tag.as_ref().map(|t| info.expr_type(t.id));
    
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
