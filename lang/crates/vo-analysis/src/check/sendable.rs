//! Sendability checking for cross-island communication.
//!
//! This module determines whether a type can be safely sent across island boundaries.
//! Sendable types are deep-copied when sent via `port[T]` or captured by `go(island)`.

use crate::objects::{TCObjects, TypeKey};
use crate::typ::{deep_underlying_type, Type};
use std::collections::HashSet;

/// Result of sendability check.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sendability {
    /// Type is statically sendable (all contents known to be sendable at compile time).
    Static,
    /// Type contains `any`/interface{} - allowed at compile time, verified at runtime.
    /// Runtime will panic if actual value contains chan/func/etc.
    RuntimeCheck,
    /// Type is not sendable.
    NotSendable(String),
}

impl Sendability {
    #[inline]
    pub fn is_sendable(&self) -> bool {
        !matches!(self, Sendability::NotSendable(_))
    }

    #[inline]
    pub fn needs_runtime_check(&self) -> bool {
        matches!(self, Sendability::RuntimeCheck)
    }

    fn merge(self, other: Sendability) -> Sendability {
        match (&self, &other) {
            (Sendability::NotSendable(_), _) => self,
            (_, Sendability::NotSendable(_)) => other,
            (Sendability::RuntimeCheck, _) | (_, Sendability::RuntimeCheck) => Sendability::RuntimeCheck,
            _ => Sendability::Static,
        }
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
///
/// Runtime-checked (needs_runtime_check = true):
/// - any/interface{} - allowed at compile time, verified at runtime
///
/// Not sendable:
/// - chan[T] - bound to island scheduler
/// - port[T] - bound to island scheduler  
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
    // Prevent infinite recursion on recursive types
    if visited.contains(&type_key) {
        return Sendability::Static;
    }
    visited.insert(type_key);

    // Get the underlying type (follow Named types)
    let underlying_key = deep_underlying_type(type_key, objs);
    let typ = &objs.types[underlying_key];

    match typ {
        Type::Basic(_) => Sendability::Static,

        Type::Array(arr) => check_sendable_impl(arr.elem(), objs, visited),

        Type::Slice(slice) => check_sendable_impl(slice.elem(), objs, visited),

        Type::Struct(s) => {
            let mut combined = Sendability::Static;
            for &field_key in s.fields() {
                let field_obj = &objs.lobjs[field_key];
                if let Some(field_type) = field_obj.typ() {
                    let result = check_sendable_impl(field_type, objs, visited);
                    if let Sendability::NotSendable(reason) = &result {
                        return Sendability::NotSendable(format!(
                            "field '{}': {}", field_obj.name(), reason
                        ));
                    }
                    combined = combined.merge(result);
                }
            }
            combined
        }

        Type::Pointer(ptr) => check_sendable_impl(ptr.base(), objs, visited),

        Type::Map(m) => {
            let key_result = check_sendable_impl(m.key(), objs, visited);
            if let Sendability::NotSendable(reason) = &key_result {
                return Sendability::NotSendable(format!("map key: {}", reason));
            }
            let elem_result = check_sendable_impl(m.elem(), objs, visited);
            if let Sendability::NotSendable(reason) = &elem_result {
                return Sendability::NotSendable(format!("map value: {}", reason));
            }
            key_result.merge(elem_result)
        }

        Type::Chan(_) => Sendability::NotSendable("chan is bound to island scheduler".into()),
        Type::Port(_) => Sendability::NotSendable("port is bound to island scheduler".into()),
        Type::Island => Sendability::NotSendable("island represents a VM instance".into()),
        Type::Interface(_) => Sendability::RuntimeCheck,
        Type::Signature(_) => Sendability::NotSendable("func may capture island-local state".into()),
        Type::Tuple(_) => Sendability::NotSendable("tuple is not sendable".into()),
        Type::Named(_) => unreachable!("Named type should have been resolved"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sendability_merge() {
        assert_eq!(Sendability::Static.merge(Sendability::Static), Sendability::Static);
        assert_eq!(Sendability::Static.merge(Sendability::RuntimeCheck), Sendability::RuntimeCheck);
        assert!(matches!(
            Sendability::Static.merge(Sendability::NotSendable("x".into())),
            Sendability::NotSendable(_)
        ));
    }
}
