//! Built-in function checking (stub implementation).

#![allow(dead_code)]

use gox_common::vfs::FileSystem;

use crate::obj::Builtin;
use crate::objects::TypeKey;
use crate::operand::{Operand, OperandMode};

use super::checker::Checker;

impl<F: FileSystem> Checker<F> {
    /// Type-checks a built-in function call (stub).
    pub fn builtin(&mut self, x: &mut Operand, id: Builtin, _args: &[TypeKey]) -> bool {
        // TODO: Implement full builtin checking
        match id {
            Builtin::Len | Builtin::Cap => {
                x.mode = OperandMode::Value;
                x.typ = self.universe().lookup_type(crate::typ::BasicType::Int);
                true
            }
            Builtin::Make | Builtin::New => {
                x.mode = OperandMode::Value;
                true
            }
            Builtin::Append | Builtin::Copy => {
                x.mode = OperandMode::Value;
                true
            }
            Builtin::Delete | Builtin::Close | Builtin::Panic | Builtin::Print | Builtin::Println | Builtin::Assert => {
                x.mode = OperandMode::NoValue;
                x.typ = None;
                true
            }
            Builtin::Recover => {
                x.mode = OperandMode::Value;
                true
            }
        }
    }
}
