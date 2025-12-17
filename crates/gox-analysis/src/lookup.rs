//! Field and method lookup for GoX types.
//!
//! This module implements the complete lookup algorithm for fields and methods,
//! including:
//! - Field lookup in structs with embedded field traversal
//! - Method lookup with proper method set computation
//! - Ambiguity detection for multiple embedded fields with same name
//! - Index sequences for runtime field/method access
//! - Type assertion checking (assertable_to)
//!
//! Based on Go's types/lookup.go implementation.

use crate::types::{
    FuncType, InterfaceType, Method, MethodSet, NamedTypeInfo, Type,
};
use gox_common::Symbol;
use std::collections::{HashMap, HashSet};

/// Result of looking up a field or method.
#[derive(Debug, Clone)]
pub enum LookupResult {
    /// Found a field.
    /// Contains: field type, index path to field, whether access was indirect (through embedded)
    Field(Type, Vec<usize>, bool),

    /// Found a method.
    /// Contains: method signature, index path, whether access was indirect
    Method(FuncType, Vec<usize>, bool),

    /// Ambiguous: multiple fields/methods with the same name at the same depth.
    /// Contains: the index path where ambiguity was detected
    Ambiguous(Vec<usize>),

    /// Nothing found.
    NotFound,
}

/// Tracks an embedded type during breadth-first search.
#[derive(Debug, Clone)]
struct EmbeddedType {
    /// The type being searched.
    ty: Type,
    /// Index path to reach this embedded type from the root.
    indices: Vec<usize>,
    /// Whether we went through an indirect (embedded) access.
    indirect: bool,
    /// Whether this type appeared multiple times at this depth.
    multiples: bool,
}

impl EmbeddedType {
    fn new(ty: Type, indices: Vec<usize>, indirect: bool, multiples: bool) -> Self {
        Self { ty, indices, indirect, multiples }
    }
}

/// Represents either a found method or a collision during method set computation.
#[derive(Debug, Clone)]
enum MethodOrCollision {
    Method(FuncType),
    Collision,
}

/// Complete lookup implementation for fields and methods.
pub struct Lookup<'a> {
    named_types: &'a [NamedTypeInfo],
}

impl<'a> Lookup<'a> {
    /// Creates a new lookup context.
    pub fn new(named_types: &'a [NamedTypeInfo]) -> Self {
        Self { named_types }
    }

    /// Gets the underlying type for a type, resolving Named types.
    pub fn underlying(&self, ty: &Type) -> Type {
        match ty {
            Type::Named(id) => {
                let idx = id.0 as usize;
                if idx < self.named_types.len() {
                    self.named_types[idx].underlying.clone()
                } else {
                    Type::Invalid
                }
            }
            _ => ty.clone(),
        }
    }

    /// Looks up a field or method by name in a type.
    ///
    /// Returns:
    /// - `LookupResult::Field` if a field was found
    /// - `LookupResult::Method` if a method was found
    /// - `LookupResult::Ambiguous` if multiple fields/methods with same name exist at same depth
    /// - `LookupResult::NotFound` if nothing was found
    pub fn lookup_field_or_method(&self, ty: &Type, name: Symbol) -> LookupResult {
        // Start with the type as single entry at shallowest depth
        let mut current = vec![EmbeddedType::new(ty.clone(), vec![], false, false)];

        // Track found target
        let mut target: Option<LookupResult> = None;

        // Track seen named types to avoid infinite recursion
        let mut seen: HashSet<u32> = HashSet::new();

        while !current.is_empty() {
            // Embedded types found at current depth
            let mut next: Vec<EmbeddedType> = vec![];

            for et in &current {
                let mut search_ty = et.ty.clone();

                // Handle Named types
                if let Type::Named(id) = &et.ty {
                    let idx = id.0 as usize;
                    if seen.contains(&(idx as u32)) {
                        // Already seen at shallower depth, skip
                        continue;
                    }
                    seen.insert(idx as u32);

                    if idx < self.named_types.len() {
                        let info = &self.named_types[idx];
                        

                        // Look for method on this named type
                        if let Some((i, method)) = info.methods.iter().enumerate()
                            .find(|(_, m)| m.name == name)
                        {
                            let mut indices = et.indices.clone();
                            indices.push(i);

                            if et.multiples || target.is_some() {
                                return LookupResult::Ambiguous(indices);
                            }
                            target = Some(LookupResult::Method(
                                method.sig.clone(),
                                indices,
                                et.indirect,
                            ));
                            continue; // Can't have matching field
                        }

                        // Continue with underlying type
                        search_ty = info.underlying.clone();
                    }
                }

                // For pointer types, dereference to get the underlying struct
                let deref_ty = match &search_ty {
                    Type::Pointer(inner) => inner.as_ref(),
                    other => other,
                };
                
                // Search in the underlying type
                match deref_ty {
                    Type::Struct(s) => {
                        for (i, field) in s.fields.iter().enumerate() {
                            // Check if field name matches
                            if field.name == Some(name) {
                                let mut indices = et.indices.clone();
                                indices.push(i);

                                if et.multiples || target.is_some() {
                                    return LookupResult::Ambiguous(indices);
                                }
                                target = Some(LookupResult::Field(
                                    field.ty.clone(),
                                    indices,
                                    et.indirect,
                                ));
                                continue;
                            }

                            // Collect embedded fields for next depth search
                            // Only if we haven't found a target yet
                            if target.is_none() && field.embedded {
                                let embedded_ty = &field.ty;
                                // Only traverse Named, Struct, Pointer, Interface types
                                match embedded_ty {
                                    Type::Named(_) | Type::Struct(_) | Type::Pointer(_) | Type::Interface(_) => {
                                        let mut indices = et.indices.clone();
                                        indices.push(i);
                                        next.push(EmbeddedType::new(
                                            embedded_ty.clone(),
                                            indices,
                                            true, // indirect through embedded
                                            et.multiples,
                                        ));
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                    Type::Interface(iface) => {
                        // Get full method set including embedded interfaces
                        let full_methods = self.interface_method_set(iface);
                        
                        // Look for method in interface
                        if let Some(method) = full_methods.get(name) {
                            let indices = et.indices.clone();

                            if et.multiples || target.is_some() {
                                return LookupResult::Ambiguous(indices);
                            }
                            target = Some(LookupResult::Method(
                                method.sig.clone(),
                                indices,
                                et.indirect,
                            ));
                        }
                    }
                    _ => {}
                }
            }

            // If we found a target, return it
            if let Some(result) = target {
                return result;
            }

            // Consolidate multiples for next depth
            current = consolidate_multiples(next);
        }

        LookupResult::NotFound
    }

    /// Computes the complete method set for a type.
    ///
    /// This includes:
    /// - Methods declared directly on the type (for named types)
    /// - Methods from embedded fields (promoted methods)
    /// - Methods from embedded interfaces
    ///
    /// Handles ambiguity by excluding methods that appear multiple times at the same depth.
    pub fn method_set(&self, ty: &Type) -> MethodSet {
        let mut result: HashMap<Symbol, MethodOrCollision> = HashMap::new();

        // Start with type at depth 0
        let mut current = vec![EmbeddedType::new(ty.clone(), vec![], false, false)];
        let mut seen: HashSet<u32> = HashSet::new();

        while !current.is_empty() {
            let mut next: Vec<EmbeddedType> = vec![];
            let mut depth_methods: HashMap<Symbol, MethodOrCollision> = HashMap::new();
            let mut depth_fields: HashMap<Symbol, bool> = HashMap::new(); // name -> multiples

            for et in &current {
                let mut search_ty = et.ty.clone();

                // Handle Named types
                if let Type::Named(id) = &et.ty {
                    let idx = id.0 as usize;
                    if seen.contains(&(idx as u32)) {
                        continue;
                    }
                    seen.insert(idx as u32);

                    if idx < self.named_types.len() {
                        let info = &self.named_types[idx];

                        // Add methods from this named type
                        for method in &info.methods {
                            add_to_method_set(
                                &mut depth_methods,
                                method,
                                et.multiples,
                            );
                        }

                        search_ty = info.underlying.clone();
                    }
                }

                // For pointer types, dereference to get the underlying struct
                let deref_ty = match &search_ty {
                    Type::Pointer(inner) => inner.as_ref(),
                    other => other,
                };
                
                match deref_ty {
                    Type::Struct(s) => {
                        for (i, field) in s.fields.iter().enumerate() {
                            // Track field names for collision with methods
                            if let Some(name) = field.name {
                                let entry = depth_fields.entry(name).or_insert(false);
                                if et.multiples {
                                    *entry = true;
                                }
                            }

                            // Collect embedded fields for next depth
                            if field.embedded {
                                match &field.ty {
                                    Type::Named(_) | Type::Struct(_) | Type::Pointer(_) | Type::Interface(_) => {
                                        let mut indices = et.indices.clone();
                                        indices.push(i);
                                        next.push(EmbeddedType::new(
                                            field.ty.clone(),
                                            indices,
                                            true,
                                            et.multiples,
                                        ));
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                    Type::Interface(iface) => {
                        for method in &iface.methods {
                            add_to_method_set(
                                &mut depth_methods,
                                method,
                                et.multiples,
                            );
                        }
                    }
                    _ => {}
                }
            }

            // Merge depth_methods into result, but only if not already present
            // (shallower depth shadows deeper)
            for (name, entry) in depth_methods {
                result.entry(name).or_insert_with(|| {
                    // Check for field collision
                    if depth_fields.contains_key(&name) {
                        MethodOrCollision::Collision
                    } else {
                        entry
                    }
                });
            }

            // Mark field collisions
            for (name, is_multiple) in &depth_fields {
                if *is_multiple && !result.contains_key(name) {
                    result.insert(*name, MethodOrCollision::Collision);
                }
            }

            current = consolidate_multiples(next);
        }

        // Convert to MethodSet, excluding collisions
        let methods: Vec<Method> = result
            .into_iter()
            .filter_map(|(name, entry)| match entry {
                MethodOrCollision::Method(sig) => Some(Method { name, sig }),
                MethodOrCollision::Collision => None,
            })
            .collect();

        MethodSet::from_methods(methods)
    }

    /// Checks if a value of interface type `iface` can be asserted to have type `target`.
    ///
    /// Returns `None` if the assertion is valid.
    /// Returns `Some((method_name, wrong_type))` if invalid:
    /// - `wrong_type = false`: method is missing
    /// - `wrong_type = true`: method exists but has wrong signature
    pub fn assertable_to(
        &self,
        iface: &InterfaceType,
        target: &Type,
    ) -> Option<(Symbol, bool)> {
        // If target is an interface, no static check needed
        // (dynamic check at runtime)
        if matches!(self.underlying(target), Type::Interface(_)) {
            return None;
        }

        // Check if target implements all methods of iface
        self.missing_method(target, iface)
    }

    /// Checks if type `ty` implements interface `iface`.
    ///
    /// Returns `None` if `ty` implements `iface`.
    /// Returns `Some((method_name, wrong_type))` if not:
    /// - `wrong_type = false`: method is missing
    /// - `wrong_type = true`: method exists but has wrong signature
    pub fn missing_method(
        &self,
        ty: &Type,
        iface: &InterfaceType,
    ) -> Option<(Symbol, bool)> {
        if iface.methods.is_empty() && iface.embeds.is_empty() {
            return None; // Empty interface is always satisfied
        }

        // Get all methods required by the interface
        let iface_methods = self.interface_method_set(iface);

        // Check if ty is also an interface
        if let Type::Interface(ty_iface) = self.underlying(ty) {
            // Interface to interface: check method compatibility
            let ty_methods = self.interface_method_set(&ty_iface);
            for req in &iface_methods.methods {
                if let Some(found) = ty_methods.get(req.name) {
                    if found.sig != req.sig {
                        return Some((req.name, true)); // Wrong signature
                    }
                }
                // Note: for interface-to-interface, missing methods are OK
                // (dynamic check at runtime)
            }
            return None;
        }

        // Concrete type: must have all methods
        let ty_methods = self.method_set(ty);
        for req in &iface_methods.methods {
            match ty_methods.get(req.name) {
                Some(found) if found.sig == req.sig => {
                    // OK
                }
                Some(_) => {
                    return Some((req.name, true)); // Wrong signature
                }
                None => {
                    return Some((req.name, false)); // Missing
                }
            }
        }

        None
    }

    /// Computes the full method set for an interface, expanding embedded interfaces.
    pub fn interface_method_set(&self, iface: &InterfaceType) -> MethodSet {
        let mut methods = iface.methods.clone();

        // Expand embedded interfaces
        for embed_name in &iface.embeds {
            if let Some(info) = self.named_types.iter().find(|i| i.name == *embed_name) {
                if let Type::Interface(embedded_iface) = &info.underlying {
                    let embedded_set = self.interface_method_set(embedded_iface);
                    for m in embedded_set.methods {
                        if !methods.iter().any(|existing| existing.name == m.name) {
                            methods.push(m);
                        }
                    }
                }
            }
        }

        MethodSet::from_methods(methods)
    }
}

/// Adds a method to the method set, handling collisions.
fn add_to_method_set(
    set: &mut HashMap<Symbol, MethodOrCollision>,
    method: &Method,
    multiples: bool,
) {
    if multiples {
        set.insert(method.name, MethodOrCollision::Collision);
        return;
    }

    match set.entry(method.name) {
        std::collections::hash_map::Entry::Occupied(mut e) => {
            e.insert(MethodOrCollision::Collision);
        }
        std::collections::hash_map::Entry::Vacant(e) => {
            e.insert(MethodOrCollision::Method(method.sig.clone()));
        }
    }
}

/// Consolidates multiple embedded types with the same underlying type.
fn consolidate_multiples(list: Vec<EmbeddedType>) -> Vec<EmbeddedType> {
    if list.is_empty() {
        return vec![];
    }

    let mut result: Vec<EmbeddedType> = Vec::with_capacity(list.len());
    let mut seen: HashMap<u32, usize> = HashMap::new(); // type_id -> index in result

    for et in list {
        // Get a unique ID for the type
        let type_id = match &et.ty {
            Type::Named(id) => id.0,
            // For non-named types, use a hash or just add them
            _ => {
                result.push(et);
                continue;
            }
        };

        if let Some(&idx) = seen.get(&type_id) {
            // Mark as multiples
            result[idx].multiples = true;
        } else {
            seen.insert(type_id, result.len());
            result.push(et);
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{BasicType, Field, StructType, NamedTypeId};

    fn make_symbol(id: u32) -> Symbol {
        unsafe { std::mem::transmute(id) }
    }

    #[test]
    fn test_lookup_direct_field() {
        let named_types = vec![];
        let lookup = Lookup::new(&named_types);

        let struct_ty = Type::Struct(StructType {
            fields: vec![
                Field {
                    name: Some(make_symbol(1)), // "x"
                    ty: Type::Basic(BasicType::Int),
                    embedded: false,
                    tag: None,
                },
            ],
        });

        match lookup.lookup_field_or_method(&struct_ty, make_symbol(1)) {
            LookupResult::Field(ty, indices, indirect) => {
                assert_eq!(ty, Type::Basic(BasicType::Int));
                assert_eq!(indices, vec![0]);
                assert!(!indirect);
            }
            _ => panic!("Expected Field result"),
        }
    }

    #[test]
    fn test_lookup_not_found() {
        let named_types = vec![];
        let lookup = Lookup::new(&named_types);

        let struct_ty = Type::Struct(StructType {
            fields: vec![],
        });

        match lookup.lookup_field_or_method(&struct_ty, make_symbol(1)) {
            LookupResult::NotFound => {}
            _ => panic!("Expected NotFound"),
        }
    }

    #[test]
    fn test_method_set_from_named_type() {
        let method = Method {
            name: make_symbol(1),
            sig: FuncType {
                params: vec![],
                results: vec![Type::Basic(BasicType::Int)],
                variadic: false,
            },
        };

        let named_types = vec![NamedTypeInfo {
            name: make_symbol(100),
            underlying: Type::Struct(StructType { fields: vec![] }),
            methods: vec![method.clone()],
        }];

        let lookup = Lookup::new(&named_types);
        let ty = Type::Named(NamedTypeId(0));

        let method_set = lookup.method_set(&ty);
        assert_eq!(method_set.len(), 1);
        assert!(method_set.get(make_symbol(1)).is_some());
    }
}
