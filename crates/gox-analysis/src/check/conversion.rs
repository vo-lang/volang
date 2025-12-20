//! Type conversion checking (stub implementation).

#![allow(dead_code)]

use gox_common::vfs::FileSystem;

use crate::objects::TypeKey;
use crate::operand::{Operand, OperandMode};

use super::checker::Checker;

impl<F: FileSystem> Checker<F> {
    /// Performs type conversion (stub).
    pub fn convert(&mut self, x: &mut Operand, t: TypeKey) {
        if x.invalid() {
            return;
        }
        x.mode = OperandMode::Value;
        x.typ = Some(t);
    }

    /// Checks if x is convertible to type t (stub).
    pub fn convertible_to_type(&self, _x: &Operand, _t: TypeKey) -> bool {
        // TODO: Implement full conversion checking
        true
    }
}
