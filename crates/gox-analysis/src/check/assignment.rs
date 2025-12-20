//! Assignment checking (stub implementation).

#![allow(dead_code)]

use gox_common::span::Span;
use gox_common::vfs::FileSystem;

use crate::objects::{ObjKey, TypeKey};
use crate::operand::{Operand, OperandMode};

use super::checker::Checker;

impl<F: FileSystem> Checker<F> {
    /// Checks assignment compatibility (stub).
    pub fn assignment(&mut self, x: &mut Operand, _t: Option<TypeKey>, _note: &str) {
        if x.invalid() {
            return;
        }
        // TODO: Implement full assignment checking
    }

    /// Initializes a constant (stub).
    pub fn init_const(&mut self, _lhs: ObjKey, x: &mut Operand) {
        if x.invalid() {
            return;
        }
        // TODO: Implement constant initialization
    }

    /// Initializes a variable (stub).
    pub fn init_var(&mut self, _lhs: ObjKey, x: &mut Operand, _msg: &str) -> Option<TypeKey> {
        if x.invalid() {
            return None;
        }
        // TODO: Implement variable initialization
        x.typ
    }

    /// Ensures x is a single value.
    pub fn single_value(&mut self, x: &mut Operand) {
        if let OperandMode::CommaOk = x.mode {
            x.mode = OperandMode::Value;
        }
    }
}
