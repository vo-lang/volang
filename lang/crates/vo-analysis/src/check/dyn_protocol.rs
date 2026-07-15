use vo_common::Span;

use crate::objects::TypeKey;
use crate::typ::{self, BasicType};

use super::checker::Checker;
use super::errors::TypeError;

pub(crate) const DYN_ATTR: &str = "DynAttr";
pub(crate) const DYN_SET_ATTR: &str = "DynSetAttr";
pub(crate) const DYN_INDEX: &str = "DynIndex";
pub(crate) const DYN_SET_INDEX: &str = "DynSetIndex";
pub(crate) const DYN_CALL: &str = "DynCall";

fn expected_signature(name: &str) -> Option<&'static str> {
    match name {
        DYN_ATTR => Some("DynAttr(name string) (any, error)"),
        DYN_SET_ATTR => Some("DynSetAttr(name string, value any) error"),
        DYN_INDEX => Some("DynIndex(key any) (any, error)"),
        DYN_SET_INDEX => Some("DynSetIndex(key any, value any) error"),
        DYN_CALL => Some("DynCall(args ...any) (any, error)"),
        _ => None,
    }
}

impl Checker {
    pub(crate) fn validate_reserved_dyn_protocol_method(
        &mut self,
        name: &str,
        signature: TypeKey,
        span: Span,
    ) -> bool {
        let Some(expected) = expected_signature(name) else {
            return true;
        };
        if self.dyn_protocol_signature_matches(name, signature) {
            return true;
        }
        self.error_code_msg(
            TypeError::InvalidOp,
            span,
            format!("reserved dynamic protocol method must have signature {expected}"),
        );
        false
    }

    pub(crate) fn resolve_dyn_protocol_method(
        &mut self,
        base_type: TypeKey,
        method_name: &str,
        span: Span,
    ) -> Result<(), ()> {
        let result = crate::lookup::lookup_field_or_method(
            base_type,
            false,
            Some(self.pkg),
            method_name,
            self.objs(),
        );

        match result {
            crate::lookup::LookupResult::Entry(method, _, _) => {
                let signature = self.lobj(method).typ().unwrap_or(self.invalid_type());
                if self.dyn_protocol_signature_matches(method_name, signature) {
                    Ok(())
                } else {
                    let expected = expected_signature(method_name).unwrap_or(method_name);
                    self.error_code_msg(
                        TypeError::InvalidOp,
                        span,
                        format!("dynamic access requires exact protocol signature {expected}"),
                    );
                    Err(())
                }
            }
            crate::lookup::LookupResult::NotFound
            | crate::lookup::LookupResult::BadMethodReceiver => {
                let type_name = crate::display::type_string(base_type, self.objs());
                let expected = expected_signature(method_name).unwrap_or(method_name);
                self.error_code_msg(
                    TypeError::InvalidOp,
                    span,
                    format!(
                        "dynamic access on concrete type {type_name} requires protocol {expected}; use any(value) for explicit reflection"
                    ),
                );
                Err(())
            }
            crate::lookup::LookupResult::Ambiguous(_) => {
                self.error_code_msg(
                    TypeError::InvalidOp,
                    span,
                    format!("ambiguous dynamic protocol method {method_name}"),
                );
                Err(())
            }
        }
    }

    fn dyn_protocol_signature_matches(&self, name: &str, signature: TypeKey) -> bool {
        let Some(sig) = self.otype(signature).try_as_signature() else {
            return false;
        };
        let variadic = sig.variadic();
        let param_tuple = sig.params();
        let result_tuple = sig.results();
        let params: Vec<_> = self
            .otype(param_tuple)
            .try_as_tuple()
            .map(|tuple| {
                tuple
                    .vars()
                    .iter()
                    .filter_map(|var| self.lobj(*var).typ())
                    .collect()
            })
            .unwrap_or_default();
        let results: Vec<_> = self
            .otype(result_tuple)
            .try_as_tuple()
            .map(|tuple| {
                tuple
                    .vars()
                    .iter()
                    .filter_map(|var| self.lobj(*var).typ())
                    .collect()
            })
            .unwrap_or_default();

        let string_type = self.basic_type(BasicType::Str);
        let any_type = self.universe().any_type();
        let error_type = self.universe().error_type();
        let is_any =
            |typ: TypeKey, checker: &Checker| typ::identical(typ, any_type, checker.objs());
        let is_error =
            |typ: TypeKey, checker: &Checker| typ::identical(typ, error_type, checker.objs());
        let returns_any_error =
            results.len() == 2 && is_any(results[0], self) && is_error(results[1], self);

        match name {
            DYN_ATTR => {
                !variadic
                    && params.len() == 1
                    && typ::identical(params[0], string_type, self.objs())
                    && returns_any_error
            }
            DYN_SET_ATTR => {
                !variadic
                    && params.len() == 2
                    && typ::identical(params[0], string_type, self.objs())
                    && is_any(params[1], self)
                    && results.len() == 1
                    && is_error(results[0], self)
            }
            DYN_INDEX => {
                !variadic && params.len() == 1 && is_any(params[0], self) && returns_any_error
            }
            DYN_SET_INDEX => {
                !variadic
                    && params.len() == 2
                    && is_any(params[0], self)
                    && is_any(params[1], self)
                    && results.len() == 1
                    && is_error(results[0], self)
            }
            DYN_CALL => {
                variadic
                    && params.len() == 1
                    && self
                        .otype(params[0])
                        .try_as_slice()
                        .is_some_and(|slice| is_any(slice.elem(), self))
                    && returns_any_error
            }
            _ => true,
        }
    }
}
