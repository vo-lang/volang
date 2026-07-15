//! Sendability checking for cross-island communication.
//!
//! This module determines whether a type can be safely sent across island boundaries.
//! Sendable values are deep-copied when sent through a port capability or transferred
//! by `go @(island)`.

use crate::objects::{TCObjects, TypeKey};
use crate::typ::{try_deep_underlying_type, Type};
use std::collections::HashSet;

/// Result of sendability check.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sendability {
    /// Type is statically sendable (all contents known to be sendable at compile time).
    Static,
    /// Type is not sendable.
    NotSendable(String),
}

impl Sendability {
    #[inline]
    pub fn is_sendable(&self) -> bool {
        !matches!(self, Sendability::NotSendable(_))
    }
}

/// Check if a type is sendable across island boundaries.
///
/// Sendable types (deep-copied on send):
/// - Scalars: bool, integers, floats, rune
/// - string
/// - []T, [N]T where T is sendable
/// - struct where all fields are sendable
/// - *T where T is sendable (pointed object is deep-copied)
/// - map[K]V where K and V are sendable
/// - port<- T where T is sendable - transferable send capability
///
/// Not sendable:
/// - any/interface{} - transfer shapes must remain statically known
/// - island - represents VM instance
/// - func/closures - may capture island-local state
pub fn check_sendable(type_key: TypeKey, objs: &TCObjects) -> Sendability {
    let mut visited = HashSet::new();
    check_sendable_impl(type_key, objs, &mut visited)
}

fn check_sendable_impl(
    type_key: TypeKey,
    objs: &TCObjects,
    visited: &mut HashSet<TypeKey>,
) -> Sendability {
    struct ContextFrame {
        prefix: String,
        parent: Option<usize>,
    }

    fn push_context(
        contexts: &mut Vec<ContextFrame>,
        parent: Option<usize>,
        prefix: String,
    ) -> usize {
        let index = contexts.len();
        contexts.push(ContextFrame { prefix, parent });
        index
    }

    fn contextualize(
        mut reason: String,
        mut context: Option<usize>,
        contexts: &[ContextFrame],
    ) -> String {
        while let Some(index) = context {
            let frame = &contexts[index];
            reason = format!("{}{}", frame.prefix, reason);
            context = frame.parent;
        }
        reason
    }

    let mut contexts = Vec::new();
    let mut tasks = vec![(type_key, None)];
    while let Some((type_key, context)) = tasks.pop() {
        if !visited.insert(type_key) {
            continue;
        }

        let Some(underlying_key) = try_deep_underlying_type(type_key, objs) else {
            return Sendability::NotSendable(contextualize(
                "cyclic named type".into(),
                context,
                &contexts,
            ));
        };
        match &objs.types[underlying_key] {
            Type::Basic(_) => {}
            Type::Array(arr) => tasks.push((arr.elem(), context)),
            Type::Slice(slice) => tasks.push((slice.elem(), context)),
            Type::Struct(s) => {
                for &field_key in s.fields().iter().rev() {
                    let field_obj = &objs.lobjs[field_key];
                    if let Some(field_type) = field_obj.typ() {
                        let child_context = push_context(
                            &mut contexts,
                            context,
                            format!("field '{}': ", field_obj.name()),
                        );
                        tasks.push((field_type, Some(child_context)));
                    }
                }
            }
            Type::Pointer(ptr) => tasks.push((ptr.base(), context)),
            Type::Map(map) => {
                let value_context = push_context(&mut contexts, context, "map value: ".to_string());
                tasks.push((map.elem(), Some(value_context)));
                let key_context = push_context(&mut contexts, context, "map key: ".to_string());
                tasks.push((map.key(), Some(key_context)));
            }
            Type::Chan(_) => {
                return Sendability::NotSendable(contextualize(
                    "chan is island-local".into(),
                    context,
                    &contexts,
                ));
            }
            Type::Port(port) => {
                if port.dir() != crate::typ::ChanDir::SendOnly {
                    return Sendability::NotSendable(contextualize(
                        "only send-only port capabilities may cross islands".into(),
                        context,
                        &contexts,
                    ));
                }
                let child_context =
                    push_context(&mut contexts, context, "port element: ".to_string());
                tasks.push((port.elem(), Some(child_context)));
            }
            Type::Island => {
                return Sendability::NotSendable(contextualize(
                    "island represents a VM instance".into(),
                    context,
                    &contexts,
                ));
            }
            Type::Interface(_) => {
                return Sendability::NotSendable(contextualize(
                    "interface values cannot cross island boundaries".into(),
                    context,
                    &contexts,
                ));
            }
            Type::Signature(_) => {
                return Sendability::NotSendable(contextualize(
                    "func may capture island-local state".into(),
                    context,
                    &contexts,
                ));
            }
            Type::Tuple(_) => {
                return Sendability::NotSendable(contextualize(
                    "tuple is not sendable".into(),
                    context,
                    &contexts,
                ));
            }
            Type::Named(_) => {
                return Sendability::NotSendable(contextualize(
                    "named type has no underlying type".into(),
                    context,
                    &contexts,
                ));
            }
        }
    }
    Sendability::Static
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::typ::{BasicType, ChanDir};
    use vo_common::span::Span;

    const DEEP_TYPE_NESTING: usize = 8_192;

    #[test]
    fn only_send_port_capabilities_are_sendable() {
        let mut objs = TCObjects::new();
        let int_type = objs
            .universe()
            .lookup_type(BasicType::Int)
            .expect("predeclared int type");
        let owner = objs.new_t_port(ChanDir::SendRecv, int_type);
        let send = objs.new_t_port(ChanDir::SendOnly, int_type);
        let receive = objs.new_t_port(ChanDir::RecvOnly, int_type);

        assert!(matches!(check_sendable(send, &objs), Sendability::Static));
        for local_capability in [owner, receive] {
            assert!(matches!(
                check_sendable(local_capability, &objs),
                Sendability::NotSendable(reason)
                    if reason == "only send-only port capabilities may cross islands"
            ));
        }

        let local_channel = objs.new_t_chan(ChanDir::SendRecv, int_type);
        let unsafe_send_port = objs.new_t_port(ChanDir::SendOnly, local_channel);
        assert!(matches!(
            check_sendable(unsafe_send_port, &objs),
            Sendability::NotSendable(reason) if reason == "port element: chan is island-local"
        ));
    }

    #[test]
    fn deep_sendable_types_do_not_use_the_host_call_stack() {
        let mut objs = TCObjects::new();
        let int_type = objs
            .universe()
            .lookup_type(BasicType::Int)
            .expect("predeclared int type");
        let mut nested = int_type;
        for _ in 0..DEEP_TYPE_NESTING {
            nested = objs.new_t_array(nested, Some(1));
        }
        assert_eq!(check_sendable(nested, &objs), Sendability::Static);

        let mut named = int_type;
        for _ in 0..DEEP_TYPE_NESTING {
            named = objs.new_t_named(None, Some(named), Vec::new());
        }
        assert_eq!(check_sendable(named, &objs), Sendability::Static);
    }

    #[test]
    fn iterative_sendability_preserves_error_context_and_priority() {
        let mut objs = TCObjects::new();
        let int_type = objs
            .universe()
            .lookup_type(BasicType::Int)
            .expect("predeclared int type");
        let local_channel = objs.new_t_chan(ChanDir::SendRecv, int_type);
        let bad_map_value = objs.new_t_map(int_type, local_channel);
        let send_port = objs.new_t_port(ChanDir::SendOnly, bad_map_value);
        let field = objs.new_field(
            Span::default(),
            None,
            "payload".to_string(),
            Some(send_port),
            false,
        );
        let container = objs.new_t_struct(vec![field], None);

        assert_eq!(
            check_sendable(container, &objs),
            Sendability::NotSendable(
                "field 'payload': port element: map value: chan is island-local".into()
            )
        );

        let island = objs.new_t_island();
        let bad_map_key = objs.new_t_map(local_channel, island);
        assert_eq!(
            check_sendable(bad_map_key, &objs),
            Sendability::NotSendable("map key: chan is island-local".into())
        );
    }

    #[test]
    fn cyclic_named_metadata_is_rejected_without_looping() {
        let mut objs = TCObjects::new();
        let first = objs.new_t_named(None, None, Vec::new());
        let second = objs.new_t_named(None, Some(first), Vec::new());
        objs.types[first]
            .try_as_named_mut()
            .expect("named type")
            .set_underlying(second);

        assert_eq!(
            check_sendable(first, &objs),
            Sendability::NotSendable("cyclic named type".into())
        );
    }
}
