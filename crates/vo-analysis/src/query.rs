//! Type query interface for code generation.
//!
//! This module provides a clean API for querying type information,
//! hiding the internal complexity of TCObjects and keys.

use crate::obj::{Builtin, ConstValue, EntityType};
use crate::objects::{ObjKey, TCObjects, TypeKey};
use crate::typ::{
    ArrayDetail, BasicType, ChanDetail, MapDetail, NamedDetail,
    SignatureDetail, SliceDetail, StructDetail, Type,
};
use vo_common::symbol::Symbol;
use vo_common::SymbolInterner;
use vo_common_core::{SlotType, ValueKind};

/// Unified type information query interface for code generation.
///
/// This provides a clean API that hides the internal TCObjects/key complexity.
pub struct TypeQuery<'a> {
    pub(crate) objs: &'a TCObjects,
    pub(crate) interner: &'a SymbolInterner,
    pub(crate) pkg_scope: Option<crate::objects::ScopeKey>,
}

impl<'a> TypeQuery<'a> {
    /// Creates a new TypeQuery.
    pub fn new(
        objs: &'a TCObjects,
        interner: &'a SymbolInterner,
        pkg_scope: Option<crate::objects::ScopeKey>,
    ) -> Self {
        Self { objs, interner, pkg_scope }
    }

    // =========================================================================
    // Symbol resolution
    // =========================================================================

    /// Resolves a Symbol to its string representation.
    pub fn symbol_str(&self, sym: Symbol) -> &str {
        self.interner.resolve(sym).unwrap_or("")
    }

    /// Looks up a name in the package scope.
    pub fn lookup(&self, name: &str) -> Option<EntityRef<'a>> {
        let scope_key = self.pkg_scope?;
        let obj_key = self.objs.scopes[scope_key].lookup(name)?;
        Some(self.entity_ref(obj_key))
    }

    /// Looks up a Symbol in the package scope.
    pub fn lookup_symbol(&self, sym: Symbol) -> Option<EntityRef<'a>> {
        let name = self.symbol_str(sym);
        self.lookup(name)
    }

    /// Looks up a type name Symbol and returns its TypeKey.
    /// Searches package scope first, then universe scope (for basic types).
    pub fn lookup_type_key(&self, sym: Symbol) -> Option<TypeKey> {
        let name = self.symbol_str(sym);
        
        // Try package scope first
        if let Some(scope_key) = self.pkg_scope {
            if let Some(obj_key) = self.objs.scopes[scope_key].lookup(name) {
                let obj = &self.objs.lobjs[obj_key];
                if obj.entity_type().is_type_name() {
                    return obj.typ();
                }
            }
        }
        
        // Try universe scope for basic types
        let universe = self.objs.universe();
        let obj_key = self.objs.scopes[universe.scope()].lookup(name)?;
        let obj = &self.objs.lobjs[obj_key];
        if obj.entity_type().is_type_name() {
            obj.typ()
        } else {
            None
        }
    }

    /// Checks if a symbol refers to a builtin function.
    pub fn is_builtin(&self, sym: Symbol) -> Option<Builtin> {
        let name = self.symbol_str(sym);
        // Check universe scope for builtins
        let universe = self.objs.universe();
        let obj_key = self.objs.scopes[universe.scope()].lookup(name)?;
        let obj = &self.objs.lobjs[obj_key];
        match obj.entity_type() {
            EntityType::Builtin(b) => Some(*b),
            _ => None,
        }
    }

    // =========================================================================
    // Type resolution
    // =========================================================================

    /// Gets the type for a TypeKey.
    pub fn get_type(&self, key: TypeKey) -> &'a Type {
        &self.objs.types[key]
    }

    /// Gets the type for an ObjKey (the object's type).
    pub fn get_obj_type(&self, key: ObjKey) -> Option<&'a Type> {
        let type_key = self.objs.lobjs[key].typ()?;
        Some(&self.objs.types[type_key])
    }

    /// Iterates over all types.
    pub fn iter_types(&self) -> impl Iterator<Item = (TypeKey, &'a Type)> {
        self.objs.types.iter()
    }

    /// Gets the underlying type for a named type.
    pub fn underlying(&self, key: TypeKey) -> &'a Type {
        self.objs.types[key].underlying_val(self.objs)
    }

    /// Gets type details for slice types.
    pub fn slice_elem(&self, slice: &SliceDetail) -> &'a Type {
        &self.objs.types[slice.elem()]
    }

    /// Gets type details for array types.
    pub fn array_elem(&self, arr: &ArrayDetail) -> &'a Type {
        &self.objs.types[arr.elem()]
    }

    /// Gets type details for map types.
    pub fn map_key(&self, map: &MapDetail) -> &'a Type {
        &self.objs.types[map.key()]
    }

    /// Gets type details for map types.
    pub fn map_elem(&self, map: &MapDetail) -> &'a Type {
        &self.objs.types[map.elem()]
    }

    /// Gets type details for chan types.
    pub fn chan_elem(&self, chan: &ChanDetail) -> &'a Type {
        &self.objs.types[chan.elem()]
    }

    /// Gets type details for pointer types.
    pub fn pointer_base(&self, ptr: &crate::typ::PointerDetail) -> &'a Type {
        &self.objs.types[ptr.base()]
    }

    /// Gets the underlying type for a named type.
    pub fn named_underlying(&self, named: &NamedDetail) -> Option<&'a Type> {
        named.try_underlying().map(|k| &self.objs.types[k])
    }

    /// Gets the name of a named type.
    pub fn named_name(&self, named: &NamedDetail) -> Option<&'a str> {
        named.obj().map(|k| self.objs.lobjs[k].name())
    }

    // =========================================================================
    // Struct field access
    // =========================================================================

    /// Gets the fields of a struct type.
    pub fn struct_fields(&self, s: &'a StructDetail) -> Vec<FieldInfo<'a>> {
        s.fields()
            .iter()
            .enumerate()
            .map(|(i, &okey)| {
                let obj = &self.objs.lobjs[okey];
                let type_key = obj.typ();
                FieldInfo {
                    name: obj.name(),
                    typ: type_key.map(|t| &self.objs.types[t]),
                    type_key,
                    tag: s.tag(i).map(|s| s.as_str()),
                    embedded: obj.var_embedded(),
                    index: i,
                }
            })
            .collect()
    }

    /// Looks up a field by name in a struct.
    pub fn struct_field_index(&self, s: &StructDetail, name: Symbol) -> Option<usize> {
        let name_str = self.symbol_str(name);
        s.fields().iter().enumerate().find_map(|(i, &okey)| {
            let obj = &self.objs.lobjs[okey];
            if obj.name() == name_str {
                Some(i)
            } else {
                None
            }
        })
    }

    // =========================================================================
    // Signature access
    // =========================================================================

    /// Gets parameter types for a signature.
    pub fn signature_params(&self, sig: &SignatureDetail) -> Vec<&'a Type> {
        let tuple = self.objs.types[sig.params()].try_as_tuple().unwrap();
        tuple
            .vars()
            .iter()
            .filter_map(|&okey| {
                self.objs.lobjs[okey].typ().map(|t| &self.objs.types[t])
            })
            .collect()
    }

    /// Gets result types for a signature.
    pub fn signature_results(&self, sig: &SignatureDetail) -> Vec<&'a Type> {
        let tuple = self.objs.types[sig.results()].try_as_tuple().unwrap();
        tuple
            .vars()
            .iter()
            .filter_map(|&okey| {
                self.objs.lobjs[okey].typ().map(|t| &self.objs.types[t])
            })
            .collect()
    }

    // =========================================================================
    // Type properties for codegen
    // =========================================================================

    /// Computes the ValueKind for a type.
    pub fn value_kind(&self, ty: &Type) -> ValueKind {
        match ty {
            Type::Basic(b) => basic_to_value_kind(b.typ()),
            Type::Slice(_) => ValueKind::Slice,
            Type::Map(_) => ValueKind::Map,
            Type::Array(_) => ValueKind::Array,
            Type::Chan(_) => ValueKind::Channel,
            Type::Signature(_) => ValueKind::Closure,
            Type::Pointer(_) => ValueKind::Pointer,
            Type::Struct(_) => ValueKind::Struct,
            Type::Interface(_) => ValueKind::Interface,
            Type::Named(n) => {
                let u = n.try_underlying().expect("Named type must have underlying in codegen");
                self.value_kind(&self.objs.types[u])
            }
            Type::Tuple(_) => ValueKind::Nil,
        }
    }

    /// Computes the number of slots a type occupies.
    pub fn type_slots(&self, ty: &Type) -> u16 {
        match ty {
            Type::Basic(_) => 1,
            Type::Slice(_) | Type::Map(_) | Type::Chan(_) | Type::Signature(_) | Type::Pointer(_) => 1,
            Type::Array(arr) => {
                let len = arr.len().unwrap_or(0) as u16;
                len * self.type_slots(&self.objs.types[arr.elem()])
            }
            // Struct is stored as GcRef (1 slot) in VM
            Type::Struct(_) => 1,
            Type::Interface(_) => 2,
            Type::Named(n) => {
                let u = n.try_underlying().expect("Named type must have underlying in codegen");
                self.type_slots(&self.objs.types[u])
            }
            Type::Tuple(_) => 1,
        }
    }

    /// Computes the SlotType list for GC scanning.
    pub fn type_slot_types(&self, ty: &Type) -> Vec<SlotType> {
        match ty {
            Type::Basic(b) if b.typ() == BasicType::Str => vec![SlotType::GcRef],
            Type::Basic(_) => vec![SlotType::Value],
            Type::Slice(_) | Type::Map(_) | Type::Chan(_) | Type::Signature(_) | Type::Pointer(_) => {
                vec![SlotType::GcRef]
            }
            Type::Array(arr) => {
                let elem = self.type_slot_types(&self.objs.types[arr.elem()]);
                let mut result = Vec::new();
                for _ in 0..arr.len().unwrap_or(0) {
                    result.extend(elem.iter().copied());
                }
                result
            }
            // Struct is stored as GcRef (1 slot) in VM, not expanded fields
            Type::Struct(_) => vec![SlotType::GcRef],
            Type::Interface(_) => vec![SlotType::Interface0, SlotType::Interface1],
            Type::Named(n) => {
                let u = n.try_underlying().expect("Named type must have underlying in codegen");
                self.type_slot_types(&self.objs.types[u])
            }
            Type::Tuple(_) => vec![SlotType::Value],
        }
    }

    /// Returns true if the type is a reference type (pointer, slice, map, etc.).
    pub fn is_ref_type(&self, ty: &Type) -> bool {
        match ty {
            Type::Basic(b) => b.typ() == BasicType::Str,
            Type::Slice(_) | Type::Map(_) | Type::Chan(_) | Type::Signature(_) | Type::Pointer(_) => true,
            Type::Array(_) | Type::Struct(_) | Type::Tuple(_) => false,
            Type::Interface(_) => true,
            Type::Named(n) => {
                let u = n.try_underlying().expect("Named type must have underlying in codegen");
                self.is_ref_type(&self.objs.types[u])
            }
        }
    }

    /// Returns true if the type is an interface type.
    pub fn is_interface(&self, ty: &Type) -> bool {
        match ty {
            Type::Interface(_) => true,
            Type::Named(n) => {
                let u = n.try_underlying().expect("Named type must have underlying in codegen");
                matches!(&self.objs.types[u], Type::Interface(_))
            }
            _ => false,
        }
    }

    /// Get the underlying interface detail from a TypeKey (handles Named types).
    pub fn get_interface_detail_by_key(&self, type_key: TypeKey) -> Option<&'a crate::typ::InterfaceDetail> {
        let ty = &self.objs.types[type_key];
        match ty {
            Type::Interface(i) => Some(i),
            Type::Named(n) => {
                let u = n.try_underlying()?;
                self.objs.types[u].try_as_interface()
            }
            _ => None,
        }
    }

    /// Find the index of a method in an interface's method list by ObjKey.
    pub fn interface_method_index(&self, iface: &crate::typ::InterfaceDetail, method_obj: ObjKey) -> Option<usize> {
        let all_methods = iface.all_methods();
        let methods = all_methods.as_ref()?;
        methods.iter().position(|&m| m == method_obj)
    }

    /// Get the name of an object by its ObjKey.
    pub fn obj_name(&self, okey: ObjKey) -> &str {
        self.objs.lobjs[okey].name()
    }

    /// Get the all_methods list from an interface type.
    /// Returns method ObjKeys in order (for dispatch table indexing).
    pub fn interface_all_methods(&self, iface_detail: &crate::typ::InterfaceDetail) -> Vec<ObjKey> {
        iface_detail.all_methods()
            .as_ref()
            .map(|v| v.clone())
            .unwrap_or_default()
    }

    /// Lookup a symbol and return its ObjKey.
    pub fn lookup_symbol_objkey(&self, sym: vo_common::Symbol) -> Option<ObjKey> {
        let name = self.symbol_str(sym);
        let scope_key = self.pkg_scope?;
        self.objs.scopes[scope_key].lookup(name)
    }

    /// Lookup a method in a concrete type by name.
    /// Returns the ObjKey of the method if found.
    pub fn lookup_concrete_method(&self, concrete_type_key: TypeKey, method_name: &str) -> Option<ObjKey> {
        use crate::lookup::{lookup_field_or_method, LookupResult};
        // For method lookup, we pass None as package to match any package (works for exported methods)
        match lookup_field_or_method(concrete_type_key, true, None, method_name, self.objs) {
            LookupResult::Entry(okey, _, _) => {
                if self.objs.lobjs[okey].entity_type().is_func() {
                    Some(okey)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    // =========================================================================
    // Internal helpers
    // =========================================================================

    fn entity_ref(&self, okey: ObjKey) -> EntityRef<'a> {
        let obj = &self.objs.lobjs[okey];
        let typ = obj.typ().map(|t| &self.objs.types[t]);
        
        match obj.entity_type() {
            EntityType::Var(_) => EntityRef::Var {
                name: obj.name(),
                typ,
            },
            EntityType::Func { .. } => EntityRef::Func {
                name: obj.name(),
                sig: typ.and_then(|t| t.try_as_signature()),
            },
            EntityType::TypeName => EntityRef::Type {
                name: obj.name(),
                underlying: typ.map(|t| t.underlying_val(self.objs)),
            },
            EntityType::Const { val } => EntityRef::Const {
                name: obj.name(),
                typ,
                value: val,
            },
            EntityType::Builtin(b) => EntityRef::Builtin(*b),
            EntityType::PkgName { .. } => EntityRef::Package { name: obj.name() },
            EntityType::Nil => EntityRef::Nil,
            EntityType::Label { .. } => EntityRef::Label { name: obj.name() },
        }
    }
}

/// A simplified reference to a language entity.
#[derive(Debug)]
pub enum EntityRef<'a> {
    Var {
        name: &'a str,
        typ: Option<&'a Type>,
    },
    Func {
        name: &'a str,
        sig: Option<&'a SignatureDetail>,
    },
    Type {
        name: &'a str,
        underlying: Option<&'a Type>,
    },
    Const {
        name: &'a str,
        typ: Option<&'a Type>,
        value: &'a ConstValue,
    },
    Builtin(Builtin),
    Package {
        name: &'a str,
    },
    Nil,
    Label {
        name: &'a str,
    },
}

/// Information about a struct field.
#[derive(Debug)]
pub struct FieldInfo<'a> {
    pub name: &'a str,
    pub typ: Option<&'a Type>,
    pub type_key: Option<TypeKey>,
    pub tag: Option<&'a str>,
    pub embedded: bool,
    pub index: usize,
}

// Helper function
fn basic_to_value_kind(b: BasicType) -> ValueKind {
    match b {
        BasicType::Bool => ValueKind::Bool,
        BasicType::Int => ValueKind::Int,
        BasicType::Int8 => ValueKind::Int8,
        BasicType::Int16 => ValueKind::Int16,
        BasicType::Int32 | BasicType::Rune => ValueKind::Int32,
        BasicType::Int64 => ValueKind::Int64,
        BasicType::Uint => ValueKind::Uint,
        BasicType::Uint8 | BasicType::Byte => ValueKind::Uint8,
        BasicType::Uint16 => ValueKind::Uint16,
        BasicType::Uint32 => ValueKind::Uint32,
        BasicType::Uint64 => ValueKind::Uint64,
        BasicType::Uintptr => ValueKind::Uint,
        BasicType::Float32 => ValueKind::Float32,
        BasicType::Float64 => ValueKind::Float64,
        BasicType::Str => ValueKind::String,
        BasicType::UntypedNil => ValueKind::Nil,
        BasicType::UntypedBool => ValueKind::Bool,
        BasicType::UntypedInt | BasicType::UntypedRune => ValueKind::Int,
        BasicType::UntypedFloat => ValueKind::Float64,
        BasicType::UntypedString => ValueKind::String,
        _ => panic!("unexpected BasicType in value_kind: {:?}", b),
    }
}
