//! Enumerate possible names, then use the canonical selector lookup to resolve
//! ambiguity, pointer receivers, promotion and package visibility.
use super::index::Member;
use crate::{
    lookup::{lookup_field_or_method, LookupResult},
    objects::{ObjKey, PackageKey, TCObjects},
    operand::OperandMode,
    typ::Type,
};
use std::collections::{BTreeSet, HashSet};

pub(super) fn candidates(member: &Member, package: PackageKey, objects: &TCObjects) -> Vec<ObjKey> {
    if let Some(object) = member.receiver_object {
        let receiver = &objects.lobjs[object];
        if receiver.entity_type().is_pkg_name() {
            return objects.scopes[*objects.pkgs[receiver.pkg_name_imported()].scope()]
                .objects()
                .filter(|key| {
                    objects.lobjs[*key].exported()
                        && (!member.types_only || objects.lobjs[*key].entity_type().is_type_name())
                })
                .collect();
        }
    }
    let Some(receiver) = &member.receiver_type else {
        return Vec::new();
    };
    if matches!(receiver.mode, OperandMode::Invalid | OperandMode::NoValue) {
        return Vec::new();
    }
    let mut pending = vec![receiver.typ];
    let mut visited = HashSet::new();
    let mut names = BTreeSet::new();
    while let Some(key) = pending.pop() {
        if !visited.insert(key) {
            continue;
        }
        match &objects.types[key] {
            Type::Named(detail) => {
                names.extend(
                    detail
                        .methods()
                        .iter()
                        .map(|key| objects.lobjs[*key].name()),
                );
                pending.push(detail.underlying());
            }
            Type::Pointer(detail) => pending.push(detail.base()),
            Type::Struct(detail) => {
                for key in detail.fields() {
                    let field = &objects.lobjs[*key];
                    names.insert(field.name());
                    if field.var_embedded() {
                        if let Some(typ) = field.typ() {
                            pending.push(typ);
                        }
                    }
                }
            }
            Type::Interface(detail) => {
                names.extend(
                    detail
                        .methods()
                        .iter()
                        .map(|key| objects.lobjs[*key].name()),
                );
                pending.extend(detail.embeddeds());
            }
            _ => {}
        }
    }
    names
        .into_iter()
        .filter_map(|name| {
            match lookup_field_or_method(
                receiver.typ,
                receiver.mode == OperandMode::Variable,
                Some(package),
                name,
                objects,
            ) {
                LookupResult::Entry(key, _, _) => {
                    let object = &objects.lobjs[key];
                    if receiver.mode == OperandMode::TypeExpr && !object.entity_type().is_func() {
                        return None;
                    }
                    (object.exported() || object.pkg() == Some(package)).then_some(key)
                }
                _ => None,
            }
        })
        .collect()
}
