//! Function call checking (stub implementation).

#![allow(dead_code)]

use gox_common::vfs::FileSystem;

use crate::objects::TypeKey;
use crate::operand::{Operand, OperandMode};

use super::checker::Checker;

/// Result kind for expression evaluation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprKind {
    Statement,
    Conversion,
    Expression,
}

impl<F: FileSystem> Checker<F> {
    /// Type-checks a function call (stub).
    pub fn call_expr(&mut self, x: &mut Operand, _func_type: TypeKey, _args: &[TypeKey]) -> ExprKind {
        if x.invalid() {
            return ExprKind::Statement;
        }
        
        match &x.mode {
            OperandMode::TypeExpr => {
                x.mode = OperandMode::Value;
                ExprKind::Conversion
            }
            OperandMode::Builtin(_) => {
                ExprKind::Statement
            }
            _ => {
                x.mode = OperandMode::Value;
                ExprKind::Statement
            }
        }
    }

    /// Checks arguments against signature (stub).
    pub fn arguments(&mut self, _x: &mut Operand, _sig: TypeKey, _args: &[TypeKey]) {
        // TODO: Implement argument checking
    }

    /// Type conversion (stub).
    pub fn conversion(&mut self, x: &mut Operand, target: TypeKey) {
        if x.invalid() {
            return;
        }
        x.mode = OperandMode::Value;
        x.typ = Some(target);
    }
}
