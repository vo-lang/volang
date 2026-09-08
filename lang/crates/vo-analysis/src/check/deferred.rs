//! Deferred semantic checks and the state each check requires.

use super::checker::Checker;
use super::errors::TypeError;
use crate::constant::Value;
use crate::objects::{DeclInfoKey, ScopeKey, TypeKey};
use crate::typ::{self, Type};
use vo_common::Span;
use vo_syntax::ast::{Block, InterfaceElem};

pub(super) enum DelayedAction {
    FunctionBody {
        decl: Option<DeclInfoKey>,
        sig: TypeKey,
        body: Block,
        iota: Option<Value>,
    },
    MapKey {
        key: TypeKey,
        key_span: Span,
    },
    PointerBase {
        base_type: TypeKey,
        base_span: Span,
    },
    InterfaceEmbeds {
        embedded_scope: ScopeKey,
        embedded_elems: Vec<InterfaceElem>,
        itype: TypeKey,
    },
    InterfaceReceiver {
        itype: TypeKey,
        recv_type: TypeKey,
    },
}

impl DelayedAction {
    pub(super) fn run(self, checker: &mut Checker) {
        match self {
            Self::FunctionBody {
                decl,
                sig,
                body,
                iota,
            } => checker.func_body(decl, sig, &body, iota),
            Self::MapKey { key, key_span } => {
                if !crate::typ::comparable(key, &checker.tc_objs) {
                    checker.error_code_msg(TypeError::InvalidOp, key_span, "invalid map key type");
                }
            }
            Self::PointerBase {
                base_type,
                base_span,
            } => {
                let invalid_type = checker.invalid_type();
                if base_type == invalid_type {
                    return;
                }
                let underlying = typ::underlying_type(base_type, checker.objs());
                if checker.otype(underlying).try_as_struct().is_none() {
                    checker.error_code_msg(
                        TypeError::PointerToNonStruct,
                        base_span,
                        format!(
                            "invalid pointer type *{} (base must be struct)",
                            checker.type_str(base_type)
                        ),
                    );
                }
            }
            Self::InterfaceEmbeds {
                embedded_scope,
                embedded_elems,
                itype,
            } => {
                let embeds = embedded_elems
                    .iter()
                    .filter_map(|elem| checker.resolve_embedded_interface(embedded_scope, elem))
                    .collect();

                if let Type::Interface(iface_detail) = &mut checker.tc_objs.types[itype] {
                    *iface_detail.embeddeds_mut() = embeds;
                }
            }
            Self::InterfaceReceiver { itype, recv_type } => {
                if let Some(iface_detail) = checker.tc_objs.types[itype].try_as_interface() {
                    for &m in iface_detail.methods().iter() {
                        let t = checker.tc_objs.lobjs[m].typ().unwrap();
                        if let Type::Signature(sig) = &checker.tc_objs.types[t] {
                            if let Some(recv_var) = sig.recv() {
                                checker.tc_objs.lobjs[*recv_var].set_type(Some(recv_type));
                            }
                        }
                    }
                }
            }
        }
    }
}
