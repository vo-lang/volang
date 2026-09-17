//! Function call checking.
//!
//! This module implements type checking for function calls and type conversions.
//!
//! Adapted from goscript with Vo-specific modifications.

use vo_syntax::ast::{Expr, ExprKind as AstExprKind};

use crate::objects::TypeKey;
use crate::operand::{Operand, OperandMode};
use crate::typ;
use crate::universe::ExprKind;

use super::checker::Checker;
use super::errors::TypeError;
use super::type_info::{CallArgument, CallInfo, CallKind};
use super::util::{UnpackResult, UnpackedResultLeftovers};

impl Checker {
    /// Type-checks a call expression.
    /// Returns the expression kind (Statement, Conversion, or Expression).
    pub(crate) fn call(&mut self, x: &mut Operand, e: &Expr) -> ExprKind {
        let AstExprKind::Call(call) = &e.kind else {
            unreachable!()
        };
        let call_span = e.span;

        // Evaluate the function expression
        self.expr_or_type(x, &call.func);

        let kind = match x.mode {
            OperandMode::TypeExpr => Some(CallKind::Conversion {
                target: x.typ.unwrap(),
            }),
            OperandMode::Builtin(id) => Some(CallKind::Builtin(id)),
            OperandMode::Invalid => None,
            _ => x.typ.map(|t| CallKind::Function {
                signature: typ::underlying_type(t, self.objs()),
            }),
        };
        let expression_kind = match &x.mode {
            OperandMode::Invalid => {
                self.use_exprs(&call.args);
                ExprKind::Statement
            }

            OperandMode::TypeExpr => {
                // Type conversion: T(x)
                let t = x.typ.unwrap();
                x.mode = OperandMode::Invalid;

                if call.spread {
                    self.invalid_op(call_span, "invalid use of ... in conversion");
                    self.use_exprs(&call.args);
                    return ExprKind::Conversion;
                }
                match call.args.len() {
                    0 => {
                        self.error_code(TypeError::MissingConversionArg, call_span);
                    }
                    1 => {
                        self.expr(x, &call.args[0]);
                        if !x.invalid() {
                            self.conversion(x, t);
                        }
                    }
                    _ => {
                        self.use_exprs(&call.args);
                        self.error_code(
                            TypeError::TooManyConversionArgs,
                            call.args.last().unwrap().span,
                        );
                    }
                }
                ExprKind::Conversion
            }

            OperandMode::Builtin(id) => {
                let id = *id;
                if !self.builtin(x, e, id) {
                    x.mode = OperandMode::Invalid;
                }
                // A non-constant result implies a function call
                self.octx.has_call_or_recv |=
                    !matches!(&x.mode, OperandMode::Invalid | OperandMode::Constant(_));
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
                        UnpackResult::Error => {
                            x.mode = OperandMode::Invalid;
                            return ExprKind::Statement;
                        }
                        _ => {
                            let (count, _) = result.rhs_count();
                            let re = UnpackedResultLeftovers::new(&result, None);
                            if !self.arguments(x, e, sig_key, &re, count) {
                                x.mode = OperandMode::Invalid;
                                return ExprKind::Statement;
                            }
                        }
                    }

                    // Determine result type
                    let results_tuple = self.otype(sig_results).try_as_tuple().unwrap();
                    match results_tuple.vars().len() {
                        0 => {
                            x.mode = OperandMode::NoValue;
                            x.typ = Some(sig_results);
                        }
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
                    self.error_code(TypeError::CannotCall, call_span);
                    x.mode = OperandMode::Invalid;
                }
                ExprKind::Statement
            }
        };
        if !x.invalid() {
            if let Some(kind) = kind {
                self.record_resolved_call(e, call, kind);
            }
        }
        expression_kind
    }

    fn record_resolved_call(&mut self, e: &Expr, call: &vo_syntax::ast::CallExpr, kind: CallKind) {
        let signature = match kind {
            CallKind::Function { signature } => Some(signature),
            CallKind::Builtin(_) => self.result.types.get(&call.func.id).map(|tv| tv.typ),
            CallKind::Conversion { .. } => None,
        };
        let signature = signature.and_then(|key| self.otype(key).try_as_signature());
        let (params, variadic) = signature.map_or((Vec::new(), false), |sig| {
            (
                self.otype(sig.params())
                    .try_as_tuple()
                    .unwrap()
                    .vars()
                    .iter()
                    .map(|&obj| self.lobj(obj).typ().unwrap())
                    .collect::<Vec<_>>(),
                sig.variadic(),
            )
        });
        let skip_type = matches!(
            kind,
            CallKind::Builtin(crate::obj::Builtin::Make | crate::obj::Builtin::New)
        );
        let mut arguments = Vec::new();
        for (source_index, arg) in call.args.iter().enumerate().skip(usize::from(skip_type)) {
            // Constants may remain in the untyped worklist until package checking
            // finishes. They are still real arguments and must retain a binding.
            let source_type = self
                .result
                .types
                .get(&arg.id)
                .map(|tv| tv.typ)
                .or_else(|| self.untyped.get(&arg.id).and_then(|info| info.typ))
                .expect("checked call argument must have a type");
            let types: Vec<_> = if call.args.len() == 1 && !call.spread {
                self.otype(source_type)
                    .try_as_tuple()
                    .map(|tuple| {
                        tuple
                            .vars()
                            .iter()
                            .enumerate()
                            .map(|(index, &obj)| (Some(index), self.lobj(obj).typ().unwrap()))
                            .collect()
                    })
                    .unwrap_or_else(|| vec![(None, source_type)])
            } else {
                vec![(None, source_type)]
            };
            for (tuple_index, source_type) in types {
                let index = arguments.len() + usize::from(skip_type);
                let mut parameter_type = match kind {
                    CallKind::Conversion { target } => target,
                    _ => params
                        .get(index)
                        .or_else(|| variadic.then(|| params.last()).flatten())
                        .copied()
                        .expect("checked argument must have a corresponding parameter"),
                };
                if variadic && !call.spread && index >= params.len().saturating_sub(1) {
                    if let Some(slice) = self.otype(parameter_type).try_as_slice() {
                        parameter_type = slice.elem();
                    }
                }
                // append's string spread has a distinct source ABI.
                if matches!(kind, CallKind::Builtin(crate::obj::Builtin::Append))
                    && call.spread
                    && typ::is_string(source_type, self.objs())
                {
                    parameter_type = source_type;
                }
                arguments.push(CallArgument {
                    source_index,
                    tuple_index,
                    source_type,
                    parameter_type,
                });
            }
        }
        self.result.calls_by_callee.insert(call.func.id, e.id);
        self.result.calls.insert(
            e.id,
            CallInfo {
                kind,
                arguments,
                spread: call.spread,
            },
        );
    }

    /// Checks argument passing for the call with the given signature.
    pub(crate) fn arguments(
        &mut self,
        x: &mut Operand,
        e: &Expr,
        sig: TypeKey,
        re: &UnpackedResultLeftovers,
        n: usize,
    ) -> bool {
        let AstExprKind::Call(call) = &e.kind else {
            unreachable!()
        };
        let call_span = e.span;
        let sig_val = self.otype(sig).try_as_signature().unwrap();
        let variadic = sig_val.variadic();
        let params = self.otype(sig_val.params()).try_as_tuple().unwrap();
        let params_len = params.vars().len();

        // Check ellipsis usage
        if call.spread {
            if !variadic {
                self.error_code(TypeError::SpreadNonVariadic, call_span);
                re.use_all(self);
                return false;
            }
            if call.args.len() == 1 && n > 1 {
                self.error_code_msg(
                    TypeError::SpreadMultiValue,
                    call_span,
                    format!("cannot use ... with {}-valued expression", n),
                );
                re.use_all(self);
                return false;
            }
        }

        // Evaluate arguments
        let mut valid = true;
        for i in 0..n {
            re.get(self, x, i);
            if !x.invalid() {
                let ellipsis = if i == n - 1 { call.spread } else { false };
                valid &= self.argument(sig, i, x, ellipsis, "argument");
            } else {
                valid = false;
            }
        }

        // Check argument count
        // A variadic function accepts an "empty" last argument: count one extra
        let count = if variadic { n + 1 } else { n };
        if count < params_len {
            self.error_code(TypeError::TooFewArgs, call_span);
            valid = false;
        }
        valid
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
    ) -> bool {
        self.single_value(x);
        if x.invalid() {
            return false;
        }

        let sig_val = self.otype(sig).try_as_signature().unwrap();
        let params = self.otype(sig_val.params()).try_as_tuple().unwrap();
        let n = params.vars().len();

        let mut ty = if i < n {
            self.lobj(params.vars()[i]).typ().unwrap()
        } else if sig_val.variadic() {
            self.lobj(params.vars()[n - 1]).typ().unwrap()
        } else {
            self.error_code(TypeError::TooManyArgs, x.pos());
            return false;
        };

        if ellipsis {
            // Argument is of the form x... and x is single-valued
            if i != n - 1 {
                self.error_code(TypeError::SpreadMismatch, x.pos());
                return false;
            }
            // Check that x is assignable to the slice type
            let xtype = x.typ.unwrap();
            if self
                .otype(xtype)
                .underlying_val(self.objs())
                .try_as_slice()
                .is_none()
                && xtype != self.basic_type(typ::BasicType::UntypedNil)
            {
                self.error_code(TypeError::InvalidVariadicArg, x.pos());
                return false;
            }
        } else if sig_val.variadic() && i >= n - 1 {
            // Non-spread argument to variadic - get element type
            if let Some(slice) = self.otype(ty).try_as_slice() {
                ty = slice.elem();
            }
        }

        self.assignment(x, Some(ty), note);
        !x.invalid()
    }
}
