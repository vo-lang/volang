//! Operand representation during type checking.
//!
//! An Operand represents an intermediate value during type checking.

#![allow(dead_code)]

use crate::check::Checker;
use crate::constant;
use crate::obj::{Builtin, ConstValue, Pos};
use crate::objects::{TCObjects, TypeKey};
use crate::typ::{self, BasicType};
use crate::universe::Universe;
use gox_common::vfs::FileSystem;
use gox_common_core::ExprId;
use std::fmt::{self, Display, Write};

/// An OperandMode specifies the (addressing) mode of an operand.
#[derive(Clone, Debug, PartialEq)]
pub enum OperandMode {
    /// Operand is invalid.
    Invalid,
    /// Operand represents no value (result of a function call w/o result).
    NoValue,
    /// Operand is a built-in function.
    Builtin(Builtin),
    /// Operand is a type.
    TypeExpr,
    /// Operand is a constant; the operand's typ is a Basic type.
    Constant(ConstValue),
    /// Operand is an addressable variable.
    Variable,
    /// Operand is a map index expression.
    MapIndex,
    /// Operand is a computed value.
    Value,
    /// Like value, but operand may be used in a comma,ok expression.
    CommaOk,
}

impl OperandMode {
    pub fn constant_val(&self) -> Option<&ConstValue> {
        match self {
            OperandMode::Constant(v) => Some(v),
            _ => None,
        }
    }

    pub fn builtin_id(&self) -> Option<Builtin> {
        match self {
            OperandMode::Builtin(id) => Some(*id),
            _ => None,
        }
    }

    pub fn is_value(&self) -> bool {
        matches!(
            self,
            OperandMode::Constant(_)
                | OperandMode::Variable
                | OperandMode::MapIndex
                | OperandMode::Value
                | OperandMode::CommaOk
        )
    }
}

impl fmt::Display for OperandMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            OperandMode::Invalid => "invalid operand",
            OperandMode::NoValue => "no value",
            OperandMode::Builtin(_) => "built-in",
            OperandMode::TypeExpr => "type",
            OperandMode::Constant(_) => "constant",
            OperandMode::Variable => "variable",
            OperandMode::MapIndex => "map index expression",
            OperandMode::Value => "value",
            OperandMode::CommaOk => "comma, ok expression",
        })
    }
}

/// An Operand represents an intermediate value during type checking.
/// Operands have an (addressing) mode, the expression evaluating to
/// the operand, the operand's type, a value for constants, and an id
/// for built-in functions.
#[derive(Clone, Debug)]
pub struct Operand {
    pub mode: OperandMode,
    pub expr_id: Option<ExprId>,
    pub typ: Option<TypeKey>,
}

impl Default for Operand {
    fn default() -> Self {
        Self::new()
    }
}

impl Operand {
    pub fn new() -> Operand {
        Operand {
            mode: OperandMode::Invalid,
            expr_id: None,
            typ: None,
        }
    }

    pub fn with_mode(mode: OperandMode, typ: Option<TypeKey>) -> Operand {
        Operand { mode, expr_id: None, typ }
    }

    pub fn with_expr(mode: OperandMode, expr_id: ExprId, typ: Option<TypeKey>) -> Operand {
        Operand { mode, expr_id: Some(expr_id), typ }
    }

    pub fn invalid(&self) -> bool {
        self.mode == OperandMode::Invalid
    }

    pub fn pos(&self) -> Pos {
        // TODO: Get position from expr_id when AST integration is complete
        0
    }

    pub fn is_nil(&self, objs: &TCObjects) -> bool {
        match self.mode {
            OperandMode::Value => {
                if let Some(t) = self.typ {
                    let typ = &objs.types[t];
                    if let Some(b) = typ.try_as_basic() {
                        return b.typ() == BasicType::UntypedNil;
                    }
                }
                false
            }
            _ => false,
        }
    }

    /// Sets the operand to a constant from a literal value.
    pub fn set_const(&mut self, val: ConstValue, basic_type: BasicType, univ: &Universe) {
        self.mode = OperandMode::Constant(val);
        self.typ = Some(univ.types()[&basic_type]);
    }

    /// Formats the operand for display.
    pub fn fmt(&self, f: &mut fmt::Formatter<'_>, objs: &TCObjects) -> fmt::Result {
        let mut has_expr = true;

        // <expr> (
        if self.expr_id.is_some() {
            write!(f, "expr#{:?}", self.expr_id.unwrap())?;
        } else {
            match &self.mode {
                OperandMode::Builtin(bi) => {
                    f.write_str(bi.name())?;
                }
                OperandMode::TypeExpr => {
                    typ::fmt_type(self.typ, f, objs)?;
                }
                OperandMode::Constant(val) => {
                    write!(f, "{:?}", val)?;
                }
                _ => has_expr = false,
            }
        }
        if has_expr {
            f.write_str(" (")?;
        }

        // <untyped kind>
        let has_type = match self.mode {
            OperandMode::Invalid
            | OperandMode::NoValue
            | OperandMode::Builtin(_)
            | OperandMode::TypeExpr => false,
            _ => {
                if let Some(t) = self.typ {
                    let tval = &objs.types[t];
                    if tval.is_untyped(objs) {
                        if let Some(b) = tval.try_as_basic() {
                            f.write_str(b.name())?;
                            f.write_char(' ')?;
                        }
                        false
                    } else {
                        true
                    }
                } else {
                    false
                }
            }
        };

        // <mode>
        self.mode.fmt(f)?;

        // <val>
        if let OperandMode::Constant(val) = &self.mode {
            if self.expr_id.is_some() {
                f.write_char(' ')?;
                write!(f, "{:?}", val)?;
            }
        }

        // <typ>
        if has_type {
            f.write_str(" of type ")?;
            typ::fmt_type(self.typ, f, objs)?;
        }

        // )
        if has_expr {
            f.write_char(')')?;
        }
        Ok(())
    }

    /// Returns true if the operand is assignable to a variable of type t.
    /// If reason is provided and assignability fails, it will be set to the failure reason.
    pub fn assignable_to<F: FileSystem>(
        &self,
        t: TypeKey,
        reason: Option<&mut String>,
        checker: &mut Checker<F>,
    ) -> bool {
        let objs = &checker.tc_objs;
        let u = checker.universe();
        
        if self.invalid() || t == u.types()[&BasicType::Invalid] {
            return true; // avoid spurious errors
        }

        let self_typ = match self.typ {
            Some(t) => t,
            None => return false,
        };

        // Identical types are always assignable
        if typ::identical(self_typ, t, objs) {
            return true;
        }

        let t_left = &objs.types[t];
        let t_right = &objs.types[self_typ];
        let ut_key_left = typ::underlying_type(t, objs);
        let ut_key_right = typ::underlying_type(self_typ, objs);
        let ut_left = &objs.types[ut_key_left];
        let ut_right = &objs.types[ut_key_right];

        // Untyped values
        if ut_right.is_untyped(objs) {
            match ut_left {
                typ::Type::Basic(detail) => {
                    if self.is_nil(objs) {
                        return false; // UnsafePointer not in GoX
                    }
                    if let OperandMode::Constant(val) = &self.mode {
                        return val.representable(detail, None);
                    }
                    if detail.typ() == BasicType::Bool {
                        if let Some(b) = ut_right.try_as_basic() {
                            return b.typ() == BasicType::UntypedBool;
                        }
                    }
                }
                typ::Type::Interface(detail) => return self.is_nil(objs) || detail.is_empty(),
                typ::Type::Pointer(_)
                | typ::Type::Signature(_)
                | typ::Type::Slice(_)
                | typ::Type::Map(_)
                | typ::Type::Chan(_) => return self.is_nil(objs),
                _ => {}
            }
        }

        // Typed values: identical underlying types, at least one not named
        if typ::identical(ut_key_right, ut_key_left, objs)
            && (!t_right.is_named() || !t_left.is_named())
        {
            return true;
        }

        // Interface implementation check
        if ut_left.try_as_interface().is_some() {
            if let Some((m, wrong_type)) = crate::lookup::missing_method(self_typ, ut_key_left, true, checker) {
                if let Some(re) = reason {
                    let msg = if wrong_type {
                        "wrong type for method"
                    } else {
                        "missing method"
                    };
                    *re = format!("{} {}", msg, checker.tc_objs.lobjs[m].name());
                }
                return false;
            }
            return true;
        }

        // Bidirectional channel assignability
        if let Some(cr) = ut_right.try_as_chan() {
            if cr.dir() == typ::ChanDir::SendRecv {
                if let Some(cl) = ut_left.try_as_chan() {
                    if typ::identical(cr.elem(), cl.elem(), objs) {
                        return !t_right.is_named() || !t_left.is_named();
                    }
                }
            }
        }

        false
    }
}
