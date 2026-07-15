//! Select statement compilation.

use vo_analysis::objects::TypeKey;
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{FuncBuilder, StorageKind};
use crate::type_info::{encode_i32, TypeInfoWrapper};

/// Info for recv case variable binding
struct RecvCaseInfo {
    dst_reg: u16,
    elem_slots: u16,
    has_ok: bool,
    queue_type: TypeKey,
}

enum SelectCasePlan {
    Default,
    Send {
        queue_reg: u16,
        val_reg: u16,
        source_index: u16,
        elem_layout: Vec<SlotType>,
    },
    Recv {
        dst_reg: u16,
        queue_reg: u16,
        source_index: u16,
        has_ok: bool,
        elem_layout: Vec<SlotType>,
    },
}

/// Compile select statement
pub(crate) fn compile_select(
    select_stmt: &vo_syntax::ast::SelectStmt,
    label: Option<vo_common::Symbol>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::CommClause;

    let case_count = select_stmt.cases.len();
    let comm_case_count = select_stmt
        .cases
        .iter()
        .filter(|case| case.comm.is_some())
        .count();
    let comm_case_count = u16::try_from(comm_case_count).map_err(|_| {
        CodegenError::Internal(format!(
            "select communication case count exceeds u16 operand width: {comm_case_count}"
        ))
    })?;
    let has_default = select_stmt.cases.iter().any(|c| c.comm.is_none());

    // Phase 1: Evaluate all case operands in source order before the select
    // state exists. Calls inside operands may execute nested selects on the
    // same fiber, so SelectBegin must not be emitted until operand evaluation
    // has completed.
    let mut case_plans: Vec<SelectCasePlan> = Vec::with_capacity(case_count);
    let mut recv_infos: Vec<Option<RecvCaseInfo>> = Vec::with_capacity(case_count);

    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        let source_index = u16::try_from(case_idx).map_err(|_| {
            CodegenError::Internal(format!(
                "select source case index exceeds u16 operand width: {case_idx}"
            ))
        })?;
        match &case.comm {
            None => {
                case_plans.push(SelectCasePlan::Default);
                recv_infos.push(None);
            }
            Some(CommClause::Send(send)) => {
                let queue_expr_reg = crate::expr::compile_expr(&send.chan, ctx, func, info)?;
                let queue_reg = func.alloc_slots(&[SlotType::GcRef]);
                func.emit_copy(queue_reg, queue_expr_reg, 1);
                let queue_type = info.expr_type(send.chan.id);
                let elem_type = info.queue_elem_type(queue_type);
                let elem_layout = info.type_slot_types(elem_type);
                let val_reg = func.alloc_slots(&elem_layout);
                crate::assign::emit_assign(
                    val_reg,
                    crate::assign::AssignSource::Expr(&send.value),
                    elem_type,
                    ctx,
                    func,
                    info,
                )?;
                case_plans.push(SelectCasePlan::Send {
                    queue_reg,
                    val_reg,
                    source_index,
                    elem_layout,
                });
                recv_infos.push(None);
            }
            Some(CommClause::Recv(recv)) => {
                let queue_expr_reg = crate::expr::compile_expr(&recv.expr, ctx, func, info)?;
                let queue_reg = func.alloc_slots(&[SlotType::GcRef]);
                func.emit_copy(queue_reg, queue_expr_reg, 1);
                let queue_type = info.expr_type(recv.expr.id);
                let elem_slots = info.queue_elem_slots(queue_type);
                let has_ok = recv.lhs.len() > 1;

                // Allocate destination with correct slot types for GC scanning
                let elem_type = info.queue_elem_type(queue_type);
                let elem_layout = info.type_slot_types(elem_type);
                let mut recv_types = elem_layout.clone();
                if has_ok {
                    recv_types.push(SlotType::Value); // ok bool
                }
                let dst_reg = func.alloc_slots(&recv_types);

                debug_assert_eq!(elem_layout.len(), elem_slots as usize);
                case_plans.push(SelectCasePlan::Recv {
                    dst_reg,
                    queue_reg,
                    source_index,
                    has_ok,
                    elem_layout,
                });
                recv_infos.push(Some(RecvCaseInfo {
                    dst_reg,
                    elem_slots,
                    has_ok,
                    queue_type,
                }));
            }
        }
    }

    // SelectBegin: a=communication case count, flags=has_default
    func.emit_with_flags(
        Opcode::SelectBegin,
        has_default as u8,
        comm_case_count,
        0,
        0,
    );

    // Enter breakable context for break support
    func.enter_breakable(label);

    // Phase 2: Register select cases from the already-evaluated operands.
    for plan in &case_plans {
        match plan {
            SelectCasePlan::Default => {}
            SelectCasePlan::Send {
                queue_reg,
                val_reg,
                source_index,
                elem_layout,
            } => func.emit_select_send(*queue_reg, *val_reg, *source_index, elem_layout),
            SelectCasePlan::Recv {
                dst_reg,
                queue_reg,
                source_index,
                has_ok,
                elem_layout,
            } => func.emit_select_recv(*dst_reg, *queue_reg, *source_index, *has_ok, elem_layout),
        }
    }

    // SelectExec: returns chosen case index (-1 for default)
    let result_reg = func.alloc_slots(&[SlotType::Value]);
    func.emit_op(Opcode::SelectExec, result_reg, 0, 0);

    // Phase 3: Generate dispatch jumps
    let mut case_jumps: Vec<usize> = Vec::with_capacity(case_count);
    let cmp_tmp = func.alloc_slots(&[SlotType::Value]);

    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        let target_idx = if case.comm.is_none() {
            -1i32
        } else {
            case_idx as i32
        };
        let (b, c) = encode_i32(target_idx);
        func.emit_op(Opcode::LoadInt, cmp_tmp, b, c);
        func.emit_op(Opcode::EqI, cmp_tmp, result_reg, cmp_tmp);
        case_jumps.push(func.emit_jump(Opcode::JumpIf, cmp_tmp));
    }

    let fallthrough_jump = func.emit_jump(Opcode::Jump, 0);

    // Phase 4: Compile case bodies
    let mut end_jumps: Vec<usize> = Vec::with_capacity(case_count);

    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        func.enter_scope();
        func.patch_jump(case_jumps[case_idx], func.current_pc());

        // Bind recv variables
        if let Some(vo_syntax::ast::CommClause::Recv(recv)) = &case.comm {
            if let Some(ref recv_info) = recv_infos[case_idx] {
                bind_recv_variables(ctx, func, info, recv, recv_info)?;
            }
        }

        for stmt in &case.body {
            super::compile_stmt(stmt, ctx, func, info)?;
        }

        func.exit_scope();
        end_jumps.push(func.emit_jump(Opcode::Jump, 0));
    }

    // Exit breakable context and get break patches
    let break_patches = func.exit_breakable();

    // Patch all end jumps (implicit break at end of case) and explicit breaks
    let end_pc = func.current_pc();
    func.patch_jump(fallthrough_jump, end_pc);
    for jump_pc in end_jumps.into_iter().chain(break_patches) {
        func.patch_jump(jump_pc, end_pc);
    }

    Ok(())
}

fn store_recv_ident(
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
    ident: &vo_syntax::ast::Ident,
    src_slot: u16,
    src_type: TypeKey,
) -> Result<(), CodegenError> {
    if info.project.interner.resolve(ident.symbol) == Some("_") {
        return Ok(());
    }
    let lhs_type = info.ident_type(ident).ok_or_else(|| {
        CodegenError::Internal(format!("recv lhs has no type: {:?}", ident.symbol))
    })?;

    let lv = if let Some(local) = func.lookup_local(ident.symbol) {
        crate::lvalue::LValue::Variable(local.storage)
    } else {
        let obj_key = info.get_use(ident);
        if let Some(global_idx) = ctx.get_global_index(obj_key) {
            crate::lvalue::LValue::Variable(StorageKind::package_global(global_idx, lhs_type, info))
        } else if let Some(capture) = func.lookup_capture(ident.symbol) {
            crate::lvalue::LValue::Capture {
                capture_index: capture.index,
                value_slots: info.type_slot_count(lhs_type),
            }
        } else {
            let ident_name = info.project.interner.resolve(ident.symbol).unwrap_or("?");
            let obj = &info.project.tc_objs.lobjs[obj_key];
            return Err(CodegenError::VariableNotFound(format!(
                "select recv ident name='{}' symbol={:?} obj_key={:?} entity={:?}",
                ident_name,
                ident.symbol,
                obj_key,
                obj.entity_type(),
            )));
        }
    };

    crate::assign::emit_assign_to_lvalue(
        &lv,
        crate::assign::AssignSource::Slot {
            slot: src_slot,
            type_key: src_type,
        },
        lhs_type,
        ctx,
        func,
        info,
    )
}

/// Bind variables for a recv case (either define new or assign to existing)
fn bind_recv_variables(
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
    recv: &vo_syntax::ast::RecvStmt,
    recv_info: &RecvCaseInfo,
) -> Result<(), CodegenError> {
    if recv.lhs.is_empty() {
        return Ok(());
    }

    let elem_type = info.queue_elem_type(recv_info.queue_type);

    // First variable: received value
    let first = &recv.lhs[0];
    if recv.define {
        define_recv_ident(ctx, func, info, first, recv_info.dst_reg, elem_type)?;
    } else {
        store_recv_ident(ctx, func, info, first, recv_info.dst_reg, elem_type)?;
    }

    // Second variable: ok bool (if present)
    if recv_info.has_ok && recv.lhs.len() > 1 {
        let second = &recv.lhs[1];
        let ok_reg = recv_info.dst_reg + recv_info.elem_slots;
        if recv.define {
            define_recv_ident(ctx, func, info, second, ok_reg, info.bool_type())?;
        } else {
            store_recv_ident(ctx, func, info, second, ok_reg, info.bool_type())?;
        }
    }
    Ok(())
}

fn define_recv_ident(
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
    ident: &vo_syntax::ast::Ident,
    src_slot: u16,
    fallback_type: TypeKey,
) -> Result<(), CodegenError> {
    if info.project.interner.resolve(ident.symbol) == Some("_") {
        return Ok(());
    }

    let obj_key = info.get_def(ident);
    let type_key = info.project.tc_objs.lobjs[obj_key]
        .typ()
        .unwrap_or(fallback_type);
    let escapes = info.is_escaped(obj_key);
    let mut definer = super::var_def::LocalDefiner::new(ctx, func, info);
    definer.define_local_from_slot(ident.symbol, type_key, escapes, src_slot, Some(obj_key))?;
    Ok(())
}
