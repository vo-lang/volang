//! Balanced decisions for side-effect-free integer case lists.
//!
//! The switch tag is already snapshotted. Every key must be a checked constant
//! before ordering is changed; dynamic cases keep their original evaluation
//! order. Small leaves use linear equality tests to bound code size.

use vo_common_core::instruction::Opcode;
use vo_common_core::SlotType;
use vo_syntax::ast::SwitchStmt;

use crate::context::CodegenContext;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

pub(super) fn keys(
    statement: &SwitchStmt,
    unsigned: bool,
    info: &TypeInfoWrapper,
) -> Option<Vec<(i64, usize)>> {
    let mut keys = Vec::new();
    for (case, clause) in statement.cases.iter().enumerate() {
        for expression in &clause.exprs {
            keys.push((info.try_const_int(expression)?, case));
        }
    }
    if keys.len() < 8 {
        return None;
    }
    if unsigned {
        keys.sort_unstable_by_key(|&(key, _)| key as u64);
    } else {
        keys.sort_unstable_by_key(|&(key, _)| key);
    }
    Some(keys)
}

pub(super) fn emit(
    keys: &[(i64, usize)],
    tag: u16,
    unsigned: bool,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    cases: &mut Vec<(usize, usize)>,
    no_match: &mut Vec<usize>,
) {
    if keys.len() <= 4 {
        for &(key, case) in keys {
            func.begin_temp_region();
            let constant = func.alloc_slots(&[SlotType::Value]);
            let equal = func.alloc_slots(&[SlotType::Value]);
            func.emit_int(constant, key, ctx);
            func.emit_op(Opcode::EqI, equal, tag, constant);
            cases.push((func.emit_jump(Opcode::JumpIf, equal), case));
            func.end_temp_region();
        }
        no_match.push(func.emit_jump(Opcode::Jump, 0));
        return;
    }
    // Both children have at most half the keys: recursion depth is logarithmic
    // even for a generated switch approaching the bytecode size limit.
    let middle = keys.len() / 2;
    let (key, case) = keys[middle];
    func.begin_temp_region();
    let constant = func.alloc_slots(&[SlotType::Value]);
    let result = func.alloc_slots(&[SlotType::Value]);
    func.emit_int(constant, key, ctx);
    func.emit_op(Opcode::EqI, result, tag, constant);
    cases.push((func.emit_jump(Opcode::JumpIf, result), case));
    func.emit_op(
        if unsigned { Opcode::LtU } else { Opcode::LtI },
        result,
        tag,
        constant,
    );
    let lower = func.emit_jump(Opcode::JumpIf, result);
    func.end_temp_region();
    emit(
        &keys[middle + 1..],
        tag,
        unsigned,
        ctx,
        func,
        cases,
        no_match,
    );
    func.patch_jump(lower, func.current_pc());
    emit(&keys[..middle], tag, unsigned, ctx, func, cases, no_match);
}
