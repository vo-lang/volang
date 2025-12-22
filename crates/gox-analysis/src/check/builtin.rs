//! Built-in function checking.
//!
//! This module implements type checking for Go's built-in functions.
//! Adapted from goscript with GoX-specific modifications:
//! - No complex types (complex64/128, real, imag)
//! - No unsafe package (Alignof, Offsetof, Sizeof)


use std::cmp::Ordering;

use gox_common::span::Span;
use gox_syntax::ast::{CallExpr, Expr};

use crate::constant::Value;
use crate::obj::Builtin;
use crate::objects::{ObjKey, TCObjects, TypeKey};
use crate::operand::{Operand, OperandMode};
use crate::typ::{self, BasicInfo, BasicType, Type};

use super::checker::Checker;
use super::errors::TypeError;

impl Checker {
    /// Type-checks a call to the built-in function specified by id.
    /// Returns true if the call is valid, with *x holding the result.
    /// x.expr_id is not set. If the call is invalid, returns false and *x is undefined.
    pub fn builtin(
        &mut self,
        x: &mut Operand,
        call: &CallExpr,
        call_span: Span,
        id: Builtin,
    ) -> bool {
        let binfo = self.universe().builtins()[&id];

        // append is the only built-in that permits the use of ... for the last argument
        if call.spread && id != Builtin::Append {
            self.invalid_op(
                call_span,
                &format!("invalid use of ... with built-in {}", binfo.name),
            );
            self.use_exprs(&call.args);
            return false;
        }

        // For len(x) and cap(x) we need to know if x contains any function calls or
        // receive operations.
        let hcor_backup = if id == Builtin::Len || id == Builtin::Cap {
            Some(self.octx.has_call_or_recv)
        } else {
            None
        };
        self.octx.has_call_or_recv = false;

        let nargs = call.args.len();

        // Check argument count for special cases
        match id {
            Builtin::Make | Builtin::New => {
                let ord = nargs.cmp(&binfo.arg_count);
                let ord = if binfo.variadic && ord == Ordering::Greater {
                    Ordering::Equal
                } else {
                    ord
                };
                if ord != Ordering::Equal {
                    self.report_arg_mismatch(call_span, binfo.name, binfo.arg_count, nargs);
                    return false;
                }
            }
            _ => {
                // For other builtins, unpack evaluates first argument
                if nargs > 0 {
                    self.multi_expr(x, &call.args[0]);
                    if x.invalid() {
                        return false;
                    }
                }
                // Check arg count
                let expected = binfo.arg_count;
                if !binfo.variadic && nargs != expected {
                    self.report_arg_mismatch(call_span, binfo.name, expected, nargs);
                    return false;
                }
                if binfo.variadic && nargs < expected {
                    self.report_arg_mismatch(call_span, binfo.name, expected, nargs);
                    return false;
                }
            }
        }

        // Record builtin type signature helper
        let record_sig = |checker: &mut Checker, res: Option<TypeKey>, args: &[TypeKey], variadic: bool| {
            let sig = make_sig(&mut checker.tc_objs, res, args, variadic);
            checker.result.record_builtin_type(&OperandMode::Builtin(id), &call.func, sig);
        };

        let result = match id {
            Builtin::Append => {
                let ok = self.builtin_append(x, call, call_span);
                if ok {
                    // append(s S, x ...T) S
                    let slice = x.typ.unwrap();
                    if let Some(detail) = self.otype(typ::underlying_type(slice, self.objs())).try_as_slice() {
                        let tslice = self.new_t_slice(detail.elem());
                        record_sig(self, Some(slice), &[slice, tslice], true);
                    }
                }
                ok
            }
            Builtin::Cap | Builtin::Len => {
                let arg_type = x.typ.unwrap_or(self.invalid_type());
                let result = self.builtin_len_cap(x, id, hcor_backup.unwrap());
                self.octx.has_call_or_recv = hcor_backup.unwrap();
                if result {
                    // len(x) int / cap(x) int - only record for non-constant results
                    if !matches!(x.mode, OperandMode::Constant(_)) {
                        let ty = typ::underlying_type(arg_type, self.objs());
                        let ty = implicit_array_deref(ty, self.objs());
                        record_sig(self, x.typ, &[ty], false);
                    }
                }
                result
            }
            Builtin::Close => {
                let arg_type = x.typ.unwrap_or(self.invalid_type());
                let ok = self.builtin_close(x);
                if ok {
                    // close(c)
                    let tkey = typ::underlying_type(arg_type, self.objs());
                    record_sig(self, None, &[tkey], false);
                }
                ok
            }
            Builtin::Copy => {
                let dst_type = x.typ.unwrap_or(self.invalid_type());
                let ok = self.builtin_copy(x, call, call_span);
                if ok {
                    // copy(dst, src []T) int
                    let mut y = Operand::new();
                    self.multi_expr(&mut y, &call.args[1]);
                    let src_type = y.typ.unwrap_or(self.invalid_type());
                    record_sig(self, Some(self.basic_type(BasicType::Int)), &[dst_type, src_type], false);
                }
                ok
            }
            Builtin::Delete => {
                let map_type = x.typ.unwrap_or(self.invalid_type());
                let ok = self.builtin_delete(x, call, call_span);
                if ok {
                    // delete(m, k)
                    if let Some(detail) = self.otype(map_type).underlying_val(self.objs()).try_as_map() {
                        record_sig(self, None, &[map_type, detail.key()], false);
                    }
                }
                ok
            }
            Builtin::Make => {
                let ok = self.builtin_make(x, call, call_span);
                if ok {
                    // make(T, n) or make(T, n, m)
                    let arg0t = x.typ.unwrap();
                    let int_type = self.basic_type(BasicType::Int);
                    // Record signature based on actual argument count
                    let size_args = nargs - 1; // excluding type argument
                    let args: Vec<TypeKey> = std::iter::once(arg0t)
                        .chain(std::iter::repeat(int_type).take(size_args))
                        .collect();
                    record_sig(self, x.typ, &args, false);
                }
                ok
            }
            Builtin::New => {
                let ok = self.builtin_new(x, call, call_span);
                if ok {
                    // new(T) *T
                    let arg0t = self.type_expr_from_expr(&call.args[0]);
                    record_sig(self, x.typ, &[arg0t], false);
                }
                ok
            }
            Builtin::Panic => {
                let ok = self.builtin_panic(x, call, call_span);
                if ok {
                    // panic(x interface{})
                    let iempty = self.new_t_empty_interface();
                    record_sig(self, None, &[iempty], false);
                }
                ok
            }
            Builtin::Print | Builtin::Println => {
                let ok = self.builtin_print(x, call, call_span, id);
                if ok {
                    // print(x, y, ...) / println(x, y, ...)
                    // Collect all argument types
                    let mut params = vec![];
                    for i in 0..nargs {
                        let mut arg = Operand::new();
                        self.multi_expr(&mut arg, &call.args[i]);
                        if let Some(t) = arg.typ {
                            params.push(t);
                        }
                    }
                    // note: not variadic
                    record_sig(self, None, &params, false);
                }
                ok
            }
            Builtin::Recover => {
                let ok = self.builtin_recover(x);
                if ok {
                    // recover() interface{}
                    record_sig(self, x.typ, &[], false);
                }
                ok
            }
            Builtin::Assert => {
                // assert(pred) - GoX extension
                // Only record signature when argument is not constant
                let arg_type = x.typ.unwrap_or(self.invalid_type());
                let is_constant = matches!(x.mode, OperandMode::Constant(_));
                let ok = self.builtin_assert(x, call_span);
                if ok && !is_constant {
                    // Only record when the argument is not constant
                    record_sig(self, None, &[arg_type], false);
                }
                ok
            }
        };
        result
    }

    /// append(s S, x ...T) S
    fn builtin_append(
        &mut self,
        x: &mut Operand,
        call: &CallExpr,
        call_span: Span,
    ) -> bool {
        let slice = match x.typ {
            Some(t) => t,
            None => return false,
        };

        // Get element type
        let telem = match self.otype(typ::underlying_type(slice, self.objs())) {
            Type::Slice(detail) => detail.elem(),
            _ => {
                self.error_code(TypeError::AppendNotSlice, call_span);
                return false;
            }
        };

        let nargs = call.args.len();

        // Special case: append([]byte, string...)
        if nargs == 2 && call.spread {
            let slice_of_bytes = self.universe().slice_of_bytes();
            let mut reason = String::new();
            if self.assignable_to(x, slice_of_bytes, &mut reason) {
                let mut y = Operand::new();
                self.multi_expr(&mut y, &call.args[1]);
                if y.invalid() {
                    return false;
                }
                if let Some(yt) = y.typ {
                    if typ::is_string(yt, self.objs()) {
                        x.mode = OperandMode::Value;
                        x.typ = Some(slice);
                        return true;
                    }
                }
            }
        }

        // General case: check remaining arguments
        for i in 1..nargs {
            let mut arg = Operand::new();
            self.multi_expr(&mut arg, &call.args[i]);
            if arg.invalid() {
                return false;
            }

            // For variadic, each arg must be assignable to element type
            // (or if spread, the arg must be a slice of element type)
            if call.spread && i == nargs - 1 {
                // Last arg with spread must be a slice
                let _arg_type = arg.typ.unwrap_or(self.invalid_type());
                let tslice = self.new_t_slice(telem);
                let mut reason = String::new();
                if !self.assignable_to(&arg, tslice, &mut reason) {
                    self.error_code(TypeError::AppendInvalidArg, call.args[i].span);
                    return false;
                }
            } else {
                let mut reason = String::new();
                if !self.assignable_to(&arg, telem, &mut reason) {
                    self.error_code(TypeError::AppendInvalidArg, call.args[i].span);
                    return false;
                }
            }
        }

        x.mode = OperandMode::Value;
        x.typ = Some(slice);
        true
    }

    /// cap(x) int / len(x) int
    fn builtin_len_cap(
        &mut self,
        x: &mut Operand,
        id: Builtin,
        has_call_or_recv: bool,
    ) -> bool {
        let ty = typ::underlying_type(x.typ.unwrap(), self.objs());
        let ty = implicit_array_deref(ty, self.objs());

        let mode = match &self.otype(ty) {
            Type::Basic(detail) => {
                if detail.info() == BasicInfo::IsString {
                    if let OperandMode::Constant(v) = &x.mode {
                        OperandMode::Constant(crate::constant::make_uint64(v.str_as_string().len() as u64))
                    } else {
                        OperandMode::Value
                    }
                } else {
                    OperandMode::Invalid
                }
            }
            Type::Array(detail) => {
                if has_call_or_recv {
                    OperandMode::Value
                } else {
                    // spec: "The expressions len(s) and cap(s) are constants
                    // if the type of s is an array or pointer to an array and
                    // the expression s does not contain channel receives or
                    // function calls"
                    OperandMode::Constant(if let Some(len) = detail.len() {
                        crate::constant::make_uint64(len)
                    } else {
                        Value::Unknown
                    })
                }
            }
            Type::Slice(_) | Type::Chan(_) => OperandMode::Value,
            Type::Map(_) => {
                if id == Builtin::Len {
                    OperandMode::Value
                } else {
                    OperandMode::Invalid
                }
            }
            _ => OperandMode::Invalid,
        };

        let invalid_type = self.invalid_type();
        if mode == OperandMode::Invalid && ty != invalid_type {
            let name = if id == Builtin::Len { "len" } else { "cap" };
            self.error_code_msg(TypeError::InvalidLenCapArg, Span::default(), format!("invalid argument for {}", name));
            return false;
        }

        x.mode = mode;
        x.typ = Some(self.basic_type(BasicType::Int));
        true
    }

    /// close(c)
    fn builtin_close(&mut self, x: &mut Operand) -> bool {
        let tkey = typ::underlying_type(x.typ.unwrap(), self.objs());
        match &self.otype(tkey) {
            Type::Chan(detail) => {
                if detail.dir() == typ::ChanDir::RecvOnly {
                    self.error_code(TypeError::CloseRecvOnly, Span::default());
                    return false;
                }
                x.mode = OperandMode::NoValue;
                true
            }
            _ => {
                self.error_code(TypeError::CloseNotChan, Span::default());
                false
            }
        }
    }

    /// copy(dst, src []T) int
    fn builtin_copy(
        &mut self,
        x: &mut Operand,
        call: &CallExpr,
        call_span: Span,
    ) -> bool {
        // dst element type
        let dst = match self.otype(x.typ.unwrap()).underlying_val(self.objs()) {
            Type::Slice(detail) => Some(detail.elem()),
            _ => None,
        };

        // Evaluate src
        let mut y = Operand::new();
        self.multi_expr(&mut y, &call.args[1]);
        if y.invalid() {
            return false;
        }

        // src element type
        let src = match self.otype(y.typ.unwrap()).underlying_val(self.objs()) {
            Type::Basic(detail) if detail.info() == BasicInfo::IsString => {
                Some(self.universe().byte())
            }
            Type::Slice(detail) => Some(detail.elem()),
            _ => None,
        };

        if dst.is_none() || src.is_none() {
            self.error_code(TypeError::CopyNotSlice, call_span);
            return false;
        }

        if !typ::identical_o(dst, src, self.objs()) {
            self.error_code(TypeError::CopyTypeMismatch, call_span);
            return false;
        }

        x.mode = OperandMode::Value;
        x.typ = Some(self.basic_type(BasicType::Int));
        true
    }

    /// delete(m, k)
    fn builtin_delete(
        &mut self,
        x: &mut Operand,
        call: &CallExpr,
        call_span: Span,
    ) -> bool {
        let mtype = x.typ.unwrap();
        match self.otype(mtype).underlying_val(self.objs()) {
            Type::Map(detail) => {
                let key = detail.key();
                
                // Evaluate key argument
                let mut k = Operand::new();
                self.multi_expr(&mut k, &call.args[1]);
                if k.invalid() {
                    return false;
                }
                
                let mut reason = String::new();
                if !self.assignable_to(&k, key, &mut reason) {
                    self.error_code(TypeError::DeleteKeyMismatch, call.args[1].span);
                    return false;
                }
                
                x.mode = OperandMode::NoValue;
                true
            }
            _ => {
                self.error_code(TypeError::DeleteNotMap, call_span);
                false
            }
        }
    }

    /// make(T, n) or make(T, n, m)
    fn builtin_make(
        &mut self,
        x: &mut Operand,
        call: &CallExpr,
        call_span: Span,
    ) -> bool {
        let nargs = call.args.len();
        
        // First argument is a type
        let arg0t = self.type_expr_from_expr(&call.args[0]);
        let invalid_type = self.invalid_type();
        if arg0t == invalid_type {
            return false;
        }

        let min = match self.otype(arg0t).underlying_val(self.objs()) {
            Type::Slice(_) => 2,
            Type::Map(_) | Type::Chan(_) => 1,
            _ => {
                self.error_code(TypeError::MakeInvalidType, call.args[0].span);
                return false;
            }
        };

        if nargs < min || min + 1 < nargs {
            self.error_code_msg(TypeError::MakeArgCount, call_span, format!("make expects {} or {} arguments; found {}", min, min + 1, nargs));
            return false;
        }

        // Validate size arguments
        for i in 1..nargs {
            if let Err(_) = self.index(&call.args[i], None) {
                // Error already reported by index
            }
        }

        // Check length <= capacity
        if nargs == 3 {
            let mut len_op = Operand::new();
            let mut cap_op = Operand::new();
            self.expr(&mut len_op, &call.args[1]);
            self.expr(&mut cap_op, &call.args[2]);
            if let (OperandMode::Constant(len_val), OperandMode::Constant(cap_val)) = (&len_op.mode, &cap_op.mode) {
                let (len, len_exact) = len_val.int_as_i64();
                let (cap, cap_exact) = cap_val.int_as_i64();
                if len_exact && cap_exact && len > cap {
                    self.error_code_msg(TypeError::MakeLenGtCap, call_span, format!("length ({}) larger than capacity ({})", len, cap));
                }
            }
        }

        x.mode = OperandMode::Value;
        x.typ = Some(arg0t);
        true
    }

    /// new(T) *T
    fn builtin_new(
        &mut self,
        x: &mut Operand,
        call: &CallExpr,
        _call_span: Span,
    ) -> bool {
        let arg0t = self.type_expr_from_expr(&call.args[0]);
        let invalid_type = self.invalid_type();
        if arg0t == invalid_type {
            return false;
        }

        x.mode = OperandMode::Value;
        x.typ = Some(self.new_t_pointer(arg0t));
        true
    }

    /// panic(x)
    fn builtin_panic(
        &mut self,
        x: &mut Operand,
        _call: &CallExpr,
        _call_span: Span,
    ) -> bool {
        // Record panic call if inside a function with result parameters
        if let Some(sig) = self.octx.sig {
            if let Some(sig_detail) = self.otype(sig).try_as_signature() {
                if sig_detail.results_count(self.objs()) > 0 {
                    if self.octx.panics.is_none() {
                        self.octx.panics = Some(std::collections::HashSet::new());
                    }
                    // Record call expression id
                }
            }
        }

        // Argument must be assignable to interface{}
        let iempty = self.new_t_empty_interface();
        self.assignment(x, Some(iempty), "argument to panic");
        if x.invalid() {
            return false;
        }

        x.mode = OperandMode::NoValue;
        true
    }

    /// print(x...) / println(x...)
    fn builtin_print(
        &mut self,
        x: &mut Operand,
        call: &CallExpr,
        _call_span: Span,
        id: Builtin,
    ) -> bool {
        let name = if id == Builtin::Print { "print" } else { "println" };
        let nargs = call.args.len();

        for i in 0..nargs {
            if i > 0 {
                self.multi_expr(x, &call.args[i]);
            }
            let msg = format!("argument to {}", name);
            self.assignment(x, None, &msg);
            if x.invalid() {
                return false;
            }
        }

        x.mode = OperandMode::NoValue;
        true
    }

    /// recover() interface{}
    fn builtin_recover(&mut self, x: &mut Operand) -> bool {
        x.mode = OperandMode::Value;
        x.typ = Some(self.new_t_empty_interface());
        true
    }

    /// assert(pred bool) - GoX extension
    fn builtin_assert(&mut self, x: &mut Operand, call_span: Span) -> bool {
        if !typ::is_boolean(x.typ.unwrap_or(self.invalid_type()), self.objs()) {
            self.error_code(TypeError::AssertNotBool, call_span);
            return false;
        }

        // If argument is a constant false, report error
        if let OperandMode::Constant(Value::Bool(false)) = &x.mode {
            self.error_code(TypeError::AssertFailed, call_span);
            // Safe to continue - compile-time assertion failure
        }

        x.mode = OperandMode::NoValue;
        true
    }

    /// Reports argument count mismatch.
    fn report_arg_mismatch(&self, call_span: Span, name: &str, expected: usize, got: usize) {
        let msg = if got < expected {
            "not enough"
        } else {
            "too many"
        };
        self.error_code_msg(TypeError::BuiltinArgCount, call_span, format!("{} arguments for {} (expected {}, found {})", msg, name, expected, got));
    }

    // index function moved to expr.rs

    /// Type-checks a type expression from an Expr AST node.
    /// Used for builtin calls like make(T, ...) where T is passed as Expr.
    /// Aligned with goscript's type_internal for Expr.
    fn type_expr_from_expr(&mut self, e: &Expr) -> TypeKey {
        use gox_syntax::ast::ExprKind;
        
        match &e.kind {
            // Type wrapped as expression (e.g., chan int, []int, map[K]V)
            ExprKind::TypeAsExpr(ty) => self.type_expr(ty),
            // Parenthesized expression: (T)
            ExprKind::Paren(inner) => self.type_expr_from_expr(inner),
            // Identifier could be a type name
            ExprKind::Ident(ident) => {
                let mut x = Operand::new();
                self.ident(&mut x, ident, None, true); // want_type = true
                if let OperandMode::TypeExpr = x.mode {
                    x.typ.unwrap_or(self.invalid_type())
                } else {
                    self.error_code(TypeError::ExpectedType, e.span);
                    self.invalid_type()
                }
            }
            // Selector could be qualified type (pkg.Type)
            ExprKind::Selector(sel) => {
                let mut x = Operand::new();
                self.selector(&mut x, sel);
                if let OperandMode::TypeExpr = x.mode {
                    x.typ.unwrap_or(self.invalid_type())
                } else {
                    self.error_code(TypeError::ExpectedType, e.span);
                    self.invalid_type()
                }
            }
            _ => {
                self.error_code(TypeError::ExpectedType, e.span);
                self.invalid_type()
            }
        }
    }
}

/// make_sig creates a signature type for the given argument and result types.
fn make_sig(
    objs: &mut TCObjects,
    res: Option<TypeKey>,
    args: &[TypeKey],
    variadic: bool,
) -> TypeKey {
    let list: Vec<ObjKey> = args
        .iter()
        .map(|&x| {
            let ty = Some(typ::untyped_default_type(x, objs));
            objs.new_var(0, None, String::new(), ty)
        })
        .collect();
    let params = objs.new_t_tuple(list);
    let rlist = res.map_or(vec![], |x| {
        vec![objs.new_var(0, None, String::new(), Some(x))]
    });
    let results = objs.new_t_tuple(rlist);
    objs.new_t_signature(None, None, params, results, variadic)
}

/// implicit_array_deref returns A if typ is of the form *A and A is an array;
/// otherwise it returns typ.
fn implicit_array_deref(t: TypeKey, objs: &TCObjects) -> TypeKey {
    let ty = &objs.types[t];
    if let Some(detail) = ty.try_as_pointer() {
        let base = typ::underlying_type(detail.base(), objs);
        if objs.types[base].try_as_array().is_some() {
            return base;
        }
    }
    t
}
