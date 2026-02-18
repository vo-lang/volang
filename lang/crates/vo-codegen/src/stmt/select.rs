//! Select statement compilation.

use vo_analysis::objects::TypeKey;
use vo_runtime::SlotType;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{FuncBuilder, StorageKind};
use crate::type_info::{encode_i32, TypeInfoWrapper};

/// Info for recv case variable binding
struct RecvCaseInfo {
    dst_reg: u16,
    elem_slots: u16,
    has_ok: bool,
    chan_type: TypeKey,
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
    let has_default = select_stmt.cases.iter().any(|c| c.comm.is_none());
    
    // SelectBegin: a=case_count, flags=has_default
    func.emit_with_flags(Opcode::SelectBegin, has_default as u8, case_count as u16, 0, 0);
    
    // Enter breakable context for break support
    func.enter_breakable(label);
    
    // Phase 1: Emit SelectSend/SelectRecv for each case
    let mut recv_infos: Vec<Option<RecvCaseInfo>> = Vec::with_capacity(case_count);
    
    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        match &case.comm {
            None => {
                recv_infos.push(None);
            }
            Some(CommClause::Send(send)) => {
                let chan_reg = crate::expr::compile_expr(&send.chan, ctx, func, info)?;
                let val_reg = crate::expr::compile_expr(&send.value, ctx, func, info)?;
                let chan_type = info.expr_type(send.chan.id);
                let elem_slots = info.chan_elem_slots(chan_type) as u8;
                func.emit_with_flags(Opcode::SelectSend, elem_slots, chan_reg, val_reg, case_idx as u16);
                recv_infos.push(None);
            }
            Some(CommClause::Recv(recv)) => {
                let chan_reg = crate::expr::compile_expr(&recv.expr, ctx, func, info)?;
                let chan_type = info.expr_type(recv.expr.id);
                let elem_slots = info.chan_elem_slots(chan_type);
                let has_ok = recv.lhs.len() > 1;
                
                // Allocate destination: elem slots + optional ok bool
                let total_slots = elem_slots + if has_ok { 1 } else { 0 };
                let dst_reg = func.alloc_slots(&vec![SlotType::Value; total_slots as usize]);
                
                let flags = ((elem_slots as u8) << 1) | (has_ok as u8);
                func.emit_with_flags(Opcode::SelectRecv, flags, dst_reg, chan_reg, case_idx as u16);
                recv_infos.push(Some(RecvCaseInfo { dst_reg, elem_slots, has_ok, chan_type }));
            }
        }
    }
    
    // SelectExec: returns chosen case index (-1 for default)
    let result_reg = func.alloc_slots(&[SlotType::Value]);
    func.emit_op(Opcode::SelectExec, result_reg, 0, 0);
    
    // Phase 2: Generate dispatch jumps
    let mut case_jumps: Vec<usize> = Vec::with_capacity(case_count);
    let cmp_tmp = func.alloc_slots(&[SlotType::Value]);
    
    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
        let target_idx = if case.comm.is_none() { -1i32 } else { case_idx as i32 };
        let (b, c) = encode_i32(target_idx);
        func.emit_op(Opcode::LoadInt, cmp_tmp, b, c);
        func.emit_op(Opcode::EqI, cmp_tmp, result_reg, cmp_tmp);
        case_jumps.push(func.emit_jump(Opcode::JumpIf, cmp_tmp));
    }
    
    let fallthrough_jump = func.emit_jump(Opcode::Jump, 0);
    
    // Phase 3: Compile case bodies
    let mut end_jumps: Vec<usize> = Vec::with_capacity(case_count);
    
    for (case_idx, case) in select_stmt.cases.iter().enumerate() {
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
    let lhs_type = info
        .ident_type(ident)
        .ok_or_else(|| CodegenError::Internal(format!("recv lhs has no type: {:?}", ident.symbol)))?;

    if let Some(local) = func.lookup_local(ident.symbol) {
        return crate::assign::emit_store_to_storage(
            local.storage,
            src_slot,
            src_type,
            lhs_type,
            ctx,
            func,
            info,
        );
    }

    let obj_key = info.get_use(ident);
    if let Some(global_idx) = ctx.get_global_index(obj_key) {
        let slots = if info.is_array(lhs_type) {
            1
        } else {
            info.type_slot_count(lhs_type)
        };
        let storage = StorageKind::Global {
            index: global_idx as u16,
            slots,
        };
        return crate::assign::emit_store_to_storage(
            storage,
            src_slot,
            src_type,
            lhs_type,
            ctx,
            func,
            info,
        );
    }

    if let Some(capture) = func.lookup_capture(ident.symbol) {
        let capture_index = capture.index;
        let slot_types = info.type_slot_types(lhs_type);
        let converted = func.alloc_slots(&slot_types);
        crate::assign::emit_assign(
            converted,
            crate::assign::AssignSource::Slot {
                slot: src_slot,
                type_key: src_type,
            },
            lhs_type,
            ctx,
            func,
            info,
        )?;

        let gcref_slot = func.alloc_slots(&[SlotType::GcRef]);
        func.emit_op(Opcode::ClosureGet, gcref_slot, capture_index, 0);
        func.emit_ptr_set_with_slot_types(gcref_slot, 0, converted, &slot_types);
        return Ok(());
    }

    Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)))
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
    
    let elem_type = info.chan_elem_type(recv_info.chan_type);
    
    // First variable: received value
    let first = &recv.lhs[0];
    if recv.define {
        func.define_local_at(first.symbol, recv_info.dst_reg, recv_info.elem_slots);
    } else {
        store_recv_ident(ctx, func, info, first, recv_info.dst_reg, elem_type)?;
    }
    
    // Second variable: ok bool (if present)
    if recv_info.has_ok && recv.lhs.len() > 1 {
        let second = &recv.lhs[1];
        let ok_reg = recv_info.dst_reg + recv_info.elem_slots;
        if recv.define {
            func.define_local_at(second.symbol, ok_reg, 1);
        } else {
            store_recv_ident(ctx, func, info, second, ok_reg, info.bool_type())?;
        }
    }
    Ok(())
}
