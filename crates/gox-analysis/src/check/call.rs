//! Function call checking.
//!
//! This module implements type checking for function calls and type conversions.
//!
//! Adapted from goscript with GoX-specific modifications.

#![allow(dead_code)]

use gox_common::span::Span;
use gox_syntax::ast::CallExpr;

use crate::objects::TypeKey;
use crate::operand::{Operand, OperandMode};
use crate::typ;
use crate::universe::ExprKind;

use super::checker::{Checker, FilesContext};
use super::util::{UnpackResult, UnpackedResultLeftovers};

impl Checker {
    /// Type-checks a call expression.
    /// Returns the expression kind (Statement, Conversion, or Expression).
    pub fn call(
        &mut self,
        x: &mut Operand,
        call: &CallExpr,
        call_span: Span,
    ) -> ExprKind {
        // Evaluate the function expression
        self.raw_expr(x, &call.func, None);

        match &x.mode {
            OperandMode::Invalid => {
                self.use_exprs(&call.args);
                ExprKind::Statement
            }

            OperandMode::TypeExpr => {
                // Type conversion: T(x)
                let t = x.typ.unwrap();
                x.mode = OperandMode::Invalid;

                match call.args.len() {
                    0 => {
                        self.error(call_span, "missing argument in conversion".to_string());
                    }
                    1 => {
                        self.expr(x, &call.args[0]);
                        if !x.invalid() {
                            self.conversion(x, t);
                        }
                    }
                    _ => {
                        self.use_exprs(&call.args);
                        self.error(call.args.last().unwrap().span, "too many arguments in conversion".to_string());
                    }
                }
                ExprKind::Conversion
            }

            OperandMode::Builtin(id) => {
                let id = *id;
                if !self.builtin(x, call, call_span, id) {
                    x.mode = OperandMode::Invalid;
                }
                // A non-constant result implies a function call
                self.octx.has_call_or_recv = match &x.mode {
                    OperandMode::Invalid | OperandMode::Constant(_) => false,
                    _ => true,
                };
                self.universe().builtins()[&id].kind
            }

            _ => {
                // Function/method call
                let func_type = x.typ.unwrap_or(self.invalid_type());
                let sig_key = typ::underlying_type(func_type, self.objs());

                if let Some(sig) = self.otype(sig_key).try_as_signature() {
                    let sig_results = sig.results();
                    let variadic = sig.variadic();
                    let pcount = sig.params_count(self.objs());

                    // Unpack arguments (handles multi-value returns)
                    let result = self.unpack(&call.args, pcount, false, variadic);
                    match result {
                        UnpackResult::Error => x.mode = OperandMode::Invalid,
                        _ => {
                            let (count, _) = result.rhs_count();
                            let re = UnpackedResultLeftovers::new(&result, None);
                            self.arguments(x, call, call_span, sig_key, &re, count);
                        }
                    }

                    // Determine result type
                    let results_tuple = self.otype(sig_results).try_as_tuple().unwrap();
                    match results_tuple.vars().len() {
                        0 => x.mode = OperandMode::NoValue,
                        1 => {
                            x.mode = OperandMode::Value;
                            x.typ = self.lobj(results_tuple.vars()[0]).typ();
                        }
                        _ => {
                            x.mode = OperandMode::Value;
                            x.typ = Some(sig_results);
                        }
                    }
                    self.octx.has_call_or_recv = true;
                } else {
                    self.error(call_span, "cannot call non-function".to_string());
                    x.mode = OperandMode::Invalid;
                }
                ExprKind::Statement
            }
        }
    }

    /// Checks argument passing for the call with the given signature.
    pub fn arguments(
        &mut self,
        x: &mut Operand,
        call: &CallExpr,
        call_span: Span,
        sig: TypeKey,
        re: &UnpackedResultLeftovers,
        n: usize,
    ) {
        let sig_val = self.otype(sig).try_as_signature().unwrap();
        let variadic = sig_val.variadic();
        let params = self.otype(sig_val.params()).try_as_tuple().unwrap();
        let params_len = params.vars().len();

        // Check ellipsis usage
        if call.spread {
            if !variadic {
                self.error(call_span, "cannot use ... in call to non-variadic function".to_string());
                re.use_all(self);
                return;
            }
            if call.args.len() == 1 && n > 1 {
                self.error(call_span, format!("cannot use ... with {}-valued expression", n));
                re.use_all(self);
                return;
            }
        }

        // Evaluate arguments
        for i in 0..n {
            re.get(self, x, i);
            if !x.invalid() {
                let ellipsis = if i == n - 1 { call.spread } else { false };
                self.argument(sig, i, x, ellipsis, "argument");
            }
        }

        // Check argument count
        // A variadic function accepts an "empty" last argument: count one extra
        let count = if variadic { n + 1 } else { n };
        if count < params_len {
            self.error(call_span, "too few arguments in call".to_string());
        }
    }

    /// Checks passing of argument x to the i'th parameter of the given signature.
    /// If ellipsis is true, the argument is followed by ... at that position in the call.
    fn argument(
        &mut self,
        sig: TypeKey,
        i: usize,
        x: &mut Operand,
        ellipsis: bool,
        note: &str,
    ) {
        self.single_value(x);
        if x.invalid() {
            return;
        }

        let sig_val = self.otype(sig).try_as_signature().unwrap();
        let params = self.otype(sig_val.params()).try_as_tuple().unwrap();
        let n = params.vars().len();

        let mut ty = if i < n {
            self.lobj(params.vars()[i]).typ().unwrap()
        } else if sig_val.variadic() {
            self.lobj(params.vars()[n - 1]).typ().unwrap()
        } else {
            self.error(Span::default(), "too many arguments".to_string());
            return;
        };

        if ellipsis {
            // Argument is of the form x... and x is single-valued
            if i != n - 1 {
                self.error(Span::default(), "can only use ... with matching parameter".to_string());
                return;
            }
            // Check that x is assignable to the slice type
            let xtype = x.typ.unwrap();
            if self.otype(xtype).underlying_val(self.objs()).try_as_slice().is_none()
                && xtype != self.basic_type(typ::BasicType::UntypedNil)
            {
                self.error(Span::default(), "cannot use value as variadic argument".to_string());
                return;
            }
        } else if sig_val.variadic() && i >= n - 1 {
            // Non-spread argument to variadic - get element type
            if let Some(slice) = self.otype(ty).try_as_slice() {
                ty = slice.elem();
            }
        }

        self.assignment(x, Some(ty), note);
    }
}
