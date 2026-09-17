//! Boolean conditions lowered directly to control flow, without temporary
//! boolean joins. The explicit work stack also handles deeply nested input.

use vo_common_core::instruction::Opcode;
use vo_syntax::ast::{BinaryOp, Expr, ExprKind, UnaryOp};

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

#[derive(Clone, Copy)]
enum Target {
    Fallthrough,
    Label(usize),
}

enum Work<'a> {
    Expression(&'a Expr, Target, Target),
    Bind(usize),
}

/// Return every branch that must be patched to the requested outcome. The
/// other outcome falls through. Internal short-circuit labels bind here.
pub(crate) fn compile_jump(
    expression: &Expr,
    when_true: bool,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<Vec<usize>, CodegenError> {
    let targets = if when_true {
        (Target::Label(0), Target::Fallthrough)
    } else {
        (Target::Fallthrough, Target::Label(0))
    };
    let mut labels = vec![Vec::new()];
    let mut work = vec![Work::Expression(expression, targets.0, targets.1)];
    while let Some(task) = work.pop() {
        let (expression, mut yes, mut no) = match task {
            Work::Expression(expression, yes, no) => (expression, yes, no),
            Work::Bind(label) => {
                for pc in labels[label].drain(..) {
                    func.patch_jump(pc, func.current_pc());
                }
                continue;
            }
        };
        match &expression.kind {
            ExprKind::Paren(inner) => {
                work.push(Work::Expression(inner, yes, no));
                continue;
            }
            ExprKind::Unary(unary) if unary.op == UnaryOp::Not => {
                work.push(Work::Expression(&unary.operand, no, yes));
                continue;
            }
            ExprKind::Binary(binary) if matches!(binary.op, BinaryOp::LogAnd | BinaryOp::LogOr) => {
                let conjunction = binary.op == BinaryOp::LogAnd;
                let skipped = if conjunction { no } else { yes };
                let skipped = match skipped {
                    Target::Label(_) => skipped,
                    Target::Fallthrough => {
                        let label = labels.len();
                        labels.push(Vec::new());
                        work.push(Work::Bind(label));
                        Target::Label(label)
                    }
                };
                work.push(Work::Expression(&binary.right, yes, no));
                work.push(if conjunction {
                    Work::Expression(&binary.left, Target::Fallthrough, skipped)
                } else {
                    Work::Expression(&binary.left, skipped, Target::Fallthrough)
                });
                continue;
            }
            _ => {}
        }
        // Each predicate is consumed by its branch before either successor
        // executes, so its temporary storage can be reused by later leaves.
        func.begin_temp_region();
        let operand = if let ExprKind::Binary(binary) = &expression.kind {
            if let Some((operand, equal)) = super::match_nil_comparison(binary, info) {
                if equal {
                    core::mem::swap(&mut yes, &mut no);
                }
                operand
            } else {
                expression
            }
        } else {
            expression
        };
        let result = super::compile_expr(operand, ctx, func, info);
        let value = match result {
            Ok(value) => value,
            Err(error) => {
                func.end_temp_region();
                return Err(error);
            }
        };
        match (yes, no) {
            (Target::Label(label), Target::Fallthrough) => {
                labels[label].push(func.emit_jump(Opcode::JumpIf, value));
            }
            (Target::Fallthrough, Target::Label(label)) => {
                labels[label].push(func.emit_jump(Opcode::JumpIfNot, value));
            }
            (Target::Label(yes), Target::Label(no)) => {
                labels[yes].push(func.emit_jump(Opcode::JumpIf, value));
                labels[no].push(func.emit_jump(Opcode::Jump, 0));
            }
            (Target::Fallthrough, Target::Fallthrough) => {}
        }
        func.end_temp_region();
    }
    Ok(core::mem::take(&mut labels[0]))
}
