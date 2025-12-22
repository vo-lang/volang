//! Field and method lookup.
//!
//! This module provides functions for looking up fields and methods
//! in types, including embedded fields.


use crate::check::Checker;
use crate::obj::LangObj;
use crate::objects::{ObjKey, PackageKey, TCObjects, TypeKey};
use crate::selection::{Selection, SelectionKind};
use crate::typ::{self, Type};
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Write};

macro_rules! lookup_on_found {
    ($indices:ident, $i:ident, $target:expr, $et:ident, $indirect:ident, $found:expr) => {
        $indices = concat_vec($et.indices.clone(), $i);
        if $target.is_some() || $et.multiples {
            return LookupResult::Ambiguous($indices.unwrap());
        }
        *$target = Some($found);
        $indirect = $et.indirect;
    };
}

/// The result of lookup_field_or_method.
#[derive(Debug, PartialEq)]
pub enum LookupResult {
    /// Valid entry: object key, index path, and whether indirect.
    Entry(ObjKey, Vec<usize>, bool),
    /// The index sequence points to an ambiguous entry.
    Ambiguous(Vec<usize>),
    /// A method with pointer receiver was found but receiver was not addressable.
    BadMethodReceiver,
    /// Nothing found.
    NotFound,
}

/// MethodSet represents the method set of a type.
pub struct MethodSet {
    list: Vec<Selection>,
}

impl MethodSet {
    /// Creates a new method set for the given type.
    pub fn new(t: &TypeKey, objs: &mut TCObjects) -> MethodSet {
        let mut mset_base: HashMap<String, MethodCollision> = HashMap::new();
        let (tkey, is_ptr) = try_deref(*t, objs);
        
        // *typ where typ is an interface has no methods
        if is_ptr && objs.types[tkey].try_as_interface().is_some() {
            return MethodSet { list: vec![] };
        }

        let mut current = vec![EmbeddedType::new(tkey, None, is_ptr, false)];
        let mut seen: Option<HashSet<TypeKey>> = None;
        
        while !current.is_empty() {
            let mut next = vec![];
            let mut fset: HashMap<String, FieldCollision> = HashMap::new();
            let mut mset: HashMap<String, MethodCollision> = HashMap::new();
            
            for et in current.iter() {
                let mut tobj = &objs.types[et.typ];
                if let typ::Type::Named(detail) = tobj {
                    if seen.is_none() {
                        seen = Some(HashSet::new());
                    }
                    let seen_mut = seen.as_mut().unwrap();
                    if seen_mut.contains(&et.typ) {
                        continue;
                    }
                    seen_mut.insert(et.typ);
                    add_to_method_set(
                        &mut mset,
                        detail.methods(),
                        et.indices.as_ref().unwrap_or(&vec![]),
                        et.indirect,
                        et.multiples,
                        objs,
                    );
                    tobj = &objs.types[detail.underlying()];
                }
                match tobj {
                    typ::Type::Struct(detail) => {
                        for (i, f) in detail.fields().iter().enumerate() {
                            let fobj = &objs.lobjs[*f];
                            add_to_field_set(&mut fset, f, et.multiples, objs);
                            if fobj.var_embedded() {
                                let (tkey, is_ptr) = try_deref(fobj.typ().unwrap(), objs);
                                next.push(EmbeddedType::new(
                                    tkey,
                                    concat_vec(et.indices.clone(), i),
                                    et.indirect || is_ptr,
                                    et.multiples,
                                ))
                            }
                        }
                    }
                    typ::Type::Interface(detail) => {
                        let all_methods = detail.all_methods();
                        if let Some(ref all) = *all_methods {
                            add_to_method_set(
                                &mut mset,
                                all,
                                et.indices.as_ref().unwrap_or(&vec![]),
                                true,
                                et.multiples,
                                objs,
                            );
                        }
                    }
                    _ => {}
                }
            }
            
            for (k, m) in mset.iter() {
                if !mset_base.contains_key(k) {
                    mset_base.insert(
                        k.clone(),
                        if fset.contains_key(k) {
                            MethodCollision::Collision
                        } else {
                            m.clone()
                        },
                    );
                }
            }
            
            for (k, f) in fset.iter() {
                if *f == FieldCollision::Collision && !mset_base.contains_key(k) {
                    mset_base.insert(k.clone(), MethodCollision::Collision);
                }
            }
            
            current = consolidate_multiples(next, objs);
        }
        
        let mut list: Vec<Selection> = mset_base
            .into_iter()
            .filter_map(|(_, m)| match m {
                MethodCollision::Method(sel) => Some(sel),
                MethodCollision::Collision => None,
            })
            .collect();
        list.sort_by(|a, b| a.id().cmp(b.id()));
        MethodSet { list }
    }

    pub fn list(&self) -> &[Selection] {
        &self.list
    }

    pub fn lookup(&self, pkgkey: &PackageKey, name: &str, objs: &TCObjects) -> Option<&Selection> {
        if self.list.is_empty() {
            return None;
        }
        let pkg = &objs.pkgs[*pkgkey];
        let id = crate::obj::get_id(Some(pkg), name).to_string();
        self.list
            .binary_search_by_key(&id.as_str(), |x| x.id())
            .ok()
            .map(|i| &self.list[i])
    }

    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }

    pub fn fmt(&self, f: &mut fmt::Formatter<'_>, objs: &TCObjects) -> fmt::Result {
        f.write_str("MethodSet {")?;
        for sel in self.list.iter() {
            sel.fmt(f, objs)?;
        }
        f.write_char('}')
    }
}

/// Looks up a field or method with given package and name in type T.
pub fn lookup_field_or_method(
    tkey: TypeKey,
    addressable: bool,
    pkg: Option<PackageKey>,
    name: &str,
    objs: &TCObjects,
) -> LookupResult {
    if let Some(named) = objs.types[tkey].try_as_named() {
        let pkey = named.underlying();
        if objs.types[pkey].try_as_pointer().is_some() {
            let re = lookup_field_or_method_impl(pkey, false, pkg, name, objs);
            if let LookupResult::Entry(okey, _, _) = &re {
                if objs.lobjs[*okey].entity_type().is_func() {
                    return LookupResult::NotFound;
                }
            }
            return re;
        }
    }
    lookup_field_or_method_impl(tkey, addressable, pkg, name, objs)
}

fn lookup_field_or_method_impl(
    tkey: TypeKey,
    addressable: bool,
    pkg: Option<PackageKey>,
    name: &str,
    objs: &TCObjects,
) -> LookupResult {
    if name == "_" {
        return LookupResult::NotFound;
    }
    let (tkey, is_ptr) = try_deref(tkey, objs);
    if is_ptr && typ::is_interface(tkey, objs) {
        return LookupResult::NotFound;
    }
    
    let mut current = vec![EmbeddedType::new(tkey, None, is_ptr, false)];
    let mut indices = None;
    let mut target = None;
    let mut indirect = false;
    let mut seen: Option<HashSet<TypeKey>> = None;
    
    while !current.is_empty() {
        let mut next = vec![];
        for et in current.iter() {
            let mut tobj = &objs.types[et.typ];
            if let typ::Type::Named(detail) = tobj {
                if seen.is_none() {
                    seen = Some(HashSet::new());
                }
                let seen_mut = seen.as_mut().unwrap();
                if seen_mut.contains(&et.typ) {
                    continue;
                }
                seen_mut.insert(et.typ);
                if let Some((i, &okey)) = lookup_method(detail.methods(), pkg, name, objs) {
                    lookup_on_found!(indices, i, &mut target, et, indirect, okey);
                    continue;
                }
                tobj = &objs.types[detail.underlying()];
            }
            match tobj {
                typ::Type::Struct(detail) => {
                    for (i, &f) in detail.fields().iter().enumerate() {
                        let fobj = &objs.lobjs[f];
                        if fobj.same_id(pkg, name, objs) {
                            lookup_on_found!(indices, i, &mut target, et, indirect, f);
                            continue;
                        }
                        if target.is_none() && fobj.var_embedded() {
                            let (tkey, is_ptr) = try_deref(fobj.typ().unwrap(), objs);
                            match &objs.types[tkey] {
                                typ::Type::Named(_)
                                | typ::Type::Struct(_)
                                | typ::Type::Interface(_) => next.push(EmbeddedType::new(
                                    tkey,
                                    concat_vec(et.indices.clone(), i),
                                    et.indirect || is_ptr,
                                    et.multiples,
                                )),
                                _ => {}
                            }
                        }
                    }
                }
                typ::Type::Interface(detail) => {
                    // Ensure interface is complete (collects embedded methods)
                    detail.complete(objs);
                    let all_methods = detail.all_methods();
                    if let Some(ref all) = *all_methods {
                        if let Some((i, &okey)) = lookup_method(all, pkg, name, objs) {
                            lookup_on_found!(indices, i, &mut target, et, indirect, okey);
                        }
                    }
                }
                _ => {}
            }
        }
        if let Some(okey) = target {
            let lobj = &objs.lobjs[okey];
            if lobj.entity_type().is_func() && ptr_recv(lobj, objs) && !indirect && !addressable {
                return LookupResult::BadMethodReceiver;
            }
            return LookupResult::Entry(okey, indices.unwrap(), indirect);
        }
        current = consolidate_multiples(next, objs);
    }
    LookupResult::NotFound
}

/// Try to dereference a pointer type, returns (base_type, was_pointer).
pub fn try_deref(t: TypeKey, objs: &TCObjects) -> (TypeKey, bool) {
    match &objs.types[t] {
        Type::Pointer(detail) => (detail.base(), true),
        _ => (t, false),
    }
}

/// Dereference if pointer to struct, otherwise return original type.
pub fn deref_struct_ptr(t: TypeKey, objs: &TCObjects) -> TypeKey {
    let ut = typ::underlying_type(t, objs);
    match &objs.types[ut] {
        Type::Pointer(detail) => {
            let but = typ::underlying_type(detail.base(), objs);
            match &objs.types[but] {
                Type::Struct(_) => but,
                _ => t,
            }
        }
        _ => t,
    }
}

/// Returns the index of the field with matching package and name.
pub fn field_index(
    fields: &[ObjKey],
    pkg: Option<PackageKey>,
    name: &str,
    objs: &TCObjects,
) -> Option<usize> {
    if name != "_" {
        fields
            .iter()
            .enumerate()
            .find(|(_, x)| objs.lobjs[**x].same_id(pkg, name, objs))
            .map(|(i, _)| i)
    } else {
        None
    }
}

/// Returns the index and key of the method with matching package and name.
pub fn lookup_method<'a>(
    methods: &'a [ObjKey],
    pkg: Option<PackageKey>,
    name: &str,
    objs: &TCObjects,
) -> Option<(usize, &'a ObjKey)> {
    if name != "_" {
        methods
            .iter()
            .enumerate()
            .find(|(_, x)| objs.lobjs[**x].same_id(pkg, name, objs))
    } else {
        None
    }
}

/// ptr_recv returns true if the receiver is of the form *T.
fn ptr_recv(lo: &LangObj, objs: &TCObjects) -> bool {
    if lo.typ().is_none() {
        return false;
    }
    if let Some(sig) = objs.types[lo.typ().unwrap()].try_as_signature() {
        if let Some(re) = sig.recv() {
            let t = objs.lobjs[*re].typ().unwrap();
            let (_, is_ptr) = try_deref(t, objs);
            return is_ptr;
        }
    }
    lo.entity_type().func_has_ptr_recv()
}

fn concat_vec(list: Option<Vec<usize>>, i: usize) -> Option<Vec<usize>> {
    match list {
        None => Some(vec![i]),
        Some(mut v) => {
            v.push(i);
            Some(v)
        }
    }
}

#[derive(Debug)]
struct EmbeddedType {
    typ: TypeKey,
    indices: Option<Vec<usize>>,
    indirect: bool,
    multiples: bool,
}

impl EmbeddedType {
    fn new(typ: TypeKey, indices: Option<Vec<usize>>, indirect: bool, multiples: bool) -> Self {
        EmbeddedType { typ, indices, indirect, multiples }
    }
}

fn consolidate_multiples(list: Vec<EmbeddedType>, objs: &TCObjects) -> Vec<EmbeddedType> {
    let mut result = Vec::with_capacity(list.len());
    if list.is_empty() {
        return result;
    }
    let lookup = |map: &HashMap<TypeKey, usize>, typ: TypeKey| {
        if let Some(i) = map.get(&typ) {
            Some(*i)
        } else {
            map.iter()
                .find(|(k, _)| typ::identical(**k, typ, objs))
                .map(|(_, i)| *i)
        }
    };
    let mut map = HashMap::new();
    for et in list.into_iter() {
        if let Some(i) = lookup(&map, et.typ) {
            result[i].multiples = true;
        } else {
            map.insert(et.typ, result.len());
            result.push(et);
        }
    }
    result
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum FieldCollision {
    Var(ObjKey),
    Collision,
}

fn add_to_field_set(
    set: &mut HashMap<String, FieldCollision>,
    f: &ObjKey,
    multiples: bool,
    objs: &TCObjects,
) {
    let key = objs.lobjs[*f].id(objs);
    if !multiples {
        if set.insert(key.to_string(), FieldCollision::Var(*f)).is_none() {
            return;
        }
    }
    set.insert(key.to_string(), FieldCollision::Collision);
}

#[derive(Clone, Debug)]
enum MethodCollision {
    Method(Selection),
    Collision,
}

fn add_to_method_set(
    set: &mut HashMap<String, MethodCollision>,
    list: &[ObjKey],
    indices: &[usize],
    indirect: bool,
    multiples: bool,
    objs: &TCObjects,
) {
    for (i, okey) in list.iter().enumerate() {
        let mobj = &objs.lobjs[*okey];
        let key = mobj.id(objs).to_string();
        if !multiples {
            if !set.contains_key(&key) && (indirect || !ptr_recv(mobj, objs)) {
                set.insert(
                    key,
                    MethodCollision::Method(Selection::new(
                        SelectionKind::MethodVal,
                        None,
                        *okey,
                        concat_vec(Some(indices.to_vec()), i).unwrap(),
                        indirect,
                        objs,
                    )),
                );
                continue;
            }
        }
        set.insert(key, MethodCollision::Collision);
    }
}

/// missing_method returns None if 't' implements 'intf', otherwise it
/// returns a missing method required by intf and whether it is missing or
/// just has the wrong type.
///
/// For non-interface types 't', or if static is set, 't' implements
/// 'intf' if all methods of 'intf' are present in 't'. Otherwise ('t'
/// is an interface and static is not set), missing_method only checks
/// that methods of 'intf' which are also present in 't' have matching
/// types (e.g., for a type assertion x.(T) where x is of interface type 't').
pub fn missing_method(
    t: TypeKey,
    intf: TypeKey,
    static_: bool,
    checker: &mut Checker,
) -> Option<(ObjKey, bool)> {
    // First, check if interface is empty
    {
        let ival = checker.tc_objs.types[intf].try_as_interface().unwrap();
        if ival.is_empty() {
            return None;
        }
    }

    // Check if t is an interface type
    let is_t_interface = {
        let tval = checker.tc_objs.types[t].underlying_val(&checker.tc_objs);
        tval.try_as_interface().is_some()
    };

    if is_t_interface {
        // t is an interface - compare method sets directly
        let t_methods: Vec<ObjKey> = {
            let tval = checker.tc_objs.types[t].underlying_val(&checker.tc_objs);
            let detail = tval.try_as_interface().unwrap();
            detail.all_methods().as_ref().map(|v| v.clone()).unwrap_or_default()
        };
        let i_methods: Vec<ObjKey> = {
            let ival = checker.tc_objs.types[intf].try_as_interface().unwrap();
            ival.all_methods().as_ref().map(|v| v.clone()).unwrap_or_default()
        };

        for fkey in i_methods {
            let (pkg, name, x_type) = {
                let fval = &checker.tc_objs.lobjs[fkey];
                (fval.pkg(), fval.name().to_string(), fval.typ())
            };
            if let Some((_i, &f)) = lookup_method(&t_methods, pkg, &name, &checker.tc_objs) {
                let y_type = checker.tc_objs.lobjs[f].typ();
                if !typ::identical_o(x_type, y_type, &checker.tc_objs) {
                    return Some((fkey, true)); // wrong type
                }
            } else if static_ {
                return Some((fkey, false)); // missing
            }
        }
        return None;
    }

    // A concrete type implements 'intf' if it implements all methods of 'intf'.
    let all_methods: Vec<ObjKey> = {
        let ival = checker.tc_objs.types[intf].try_as_interface().unwrap();
        ival.all_methods().as_ref().map(|v| v.clone()).unwrap_or_default()
    };

    for fkey in all_methods {
        let (pkg, name, x_type) = {
            let fval = &checker.tc_objs.lobjs[fkey];
            (fval.pkg(), fval.name().to_string(), fval.typ())
        };
        match lookup_field_or_method(t, false, pkg, &name, &checker.tc_objs) {
            LookupResult::Entry(okey, _, _) => {
                if !checker.tc_objs.lobjs[okey].entity_type().is_func() {
                    return Some((fkey, false)); // not a method
                }
                // methods may not have a fully set up signature yet
                checker.obj_decl(okey, None);
                let y_type = checker.tc_objs.lobjs[okey].typ();
                if !typ::identical_o(x_type, y_type, &checker.tc_objs) {
                    return Some((fkey, true)); // wrong type
                }
            }
            _ => return Some((fkey, false)), // missing
        }
    }
    None
}

/// assertable_to reports whether a value of type iface can be asserted to have type t.
/// It returns None as affirmative answer. See docs for missing_method for more info.
pub fn assertable_to(
    iface: TypeKey,
    t: TypeKey,
    checker: &mut Checker,
) -> Option<(ObjKey, bool)> {
    let strict = true;
    if !strict && checker.tc_objs.types[t].is_interface(&checker.tc_objs) {
        return None;
    }
    missing_method(t, iface, false, checker)
}
