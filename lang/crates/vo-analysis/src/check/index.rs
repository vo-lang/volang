//! Integer indices, slice bounds and indexed literal elements.

use super::checker::Checker;
use super::errors::TypeError;
use super::MAX_LANGUAGE_LEN;
use crate::objects::TypeKey;
use crate::operand::{Operand, OperandMode};
use crate::typ::{self, BasicType};
use vo_common::Span;
use vo_syntax::ast::Expr;

impl Checker {
    /// Checks an index expression for validity.
    /// max is the upper bound for index (exclusive: index must be < max).
    /// Returns the value of the index when it's a constant, returns None if it's not.
    #[allow(clippy::result_unit_err)]
    pub fn index(&mut self, index: &Expr, max: Option<u64>) -> Result<Option<u64>, ()> {
        self.check_int_index(index, max, false)
    }

    /// Checks a slice bound expression for validity.
    /// max is the upper bound for the slice bound (inclusive: bound must be <= max).
    /// Returns the value when it's a constant, returns None if it's not.
    #[allow(clippy::result_unit_err)]
    pub fn slice_bound(&mut self, bound: &Expr, max: Option<u64>) -> Result<Option<u64>, ()> {
        self.check_int_index(bound, max, true)
    }

    /// Common implementation for index/slice_bound checking.
    /// inclusive: if true, allows value == max (for slice bounds); if false, requires value < max (for array index).
    fn check_int_index(
        &mut self,
        index: &Expr,
        max: Option<u64>,
        inclusive: bool,
    ) -> Result<Option<u64>, ()> {
        let x = &mut Operand::new();
        self.expr(x, index);
        self.check_int_index_operand(x, index.span, max, inclusive)
    }

    fn check_int_index_operand(
        &mut self,
        x: &mut Operand,
        span: Span,
        max: Option<u64>,
        inclusive: bool,
    ) -> Result<Option<u64>, ()> {
        if x.invalid() {
            return Err(());
        }

        // An untyped constant must be representable as Int
        self.convert_untyped(x, self.basic_type(BasicType::Int));
        if x.invalid() {
            return Err(());
        }

        // The index must be of integer type
        if !typ::is_integer(x.typ.unwrap(), self.objs()) {
            self.invalid_arg(span, "index must be integer");
            return Err(());
        }

        // A constant index i must be in bounds
        if let OperandMode::Constant(v) = &x.mode {
            if v.sign() < 0 {
                self.invalid_arg(span, "index must not be negative");
                return Err(());
            }
            let (i, valid) = v.to_int().int_as_u64();
            let out_of_bounds = if inclusive {
                max.is_some_and(|m| i > m) // slice bound: i <= max
            } else {
                max.is_some_and(|m| i >= m) // array index: i < max
            };
            if !valid || out_of_bounds {
                self.invalid_arg(span, "index out of bounds");
                return Err(());
            }
            return Ok(Some(i));
        }

        Ok(None)
    }

    /// Checks the elements of an array or slice composite literal against the
    /// literal's element type, and the element indices against the literal length
    /// if known. It returns the length of the literal (maximum index value + 1).
    pub(super) fn indexed_elems(
        &mut self,
        elems: &[vo_syntax::ast::CompositeLitElem],
        t: TypeKey,
        length: Option<u64>,
    ) -> u64 {
        use std::collections::HashSet;
        let mut visited: HashSet<u64> = HashSet::new();
        let mut index: u64 = 0;
        let mut max: u64 = 0;

        for elem in elems {
            let (valid_index, eval) = if let Some(ref key) = elem.key {
                let result = self.index(key, length);
                let kv_index = match result {
                    Ok(Some(index)) => Some(index),
                    Ok(None) => {
                        self.error_code_msg(
                            TypeError::InvalidOp,
                            key.span,
                            "index must be integer constant",
                        );
                        None
                    }
                    Err(()) => None,
                };
                (kv_index, &elem.value)
            } else if length.is_some_and(|l| index >= l) {
                self.error_code_msg(
                    TypeError::InvalidOp,
                    elem.value.span,
                    format!("index {} is out of bounds (>= {})", index, length.unwrap()),
                );
                (None, &elem.value)
            } else {
                (Some(index), &elem.value)
            };

            if let Some(i) = valid_index {
                if visited.contains(&i) {
                    self.error_code_msg(
                        TypeError::DuplicateCase,
                        elem.value.span,
                        format!("duplicate index {} in array or slice literal", i),
                    );
                }
                visited.insert(i);

                if i >= MAX_LANGUAGE_LEN {
                    self.error_code_msg(
                        TypeError::ArrayLenTooLarge,
                        elem.span,
                        format!(
                            "array length inferred from index {i} exceeds MaxInt ({MAX_LANGUAGE_LEN})"
                        ),
                    );
                } else {
                    index = i + 1;
                    if index > max {
                        max = index;
                    }
                }
            }

            // Check element against composite literal element type
            let x = &mut Operand::new();
            self.raw_expr(x, eval, Some(t));
            self.assignment(x, Some(t), "array or slice literal");
        }
        max
    }
}
