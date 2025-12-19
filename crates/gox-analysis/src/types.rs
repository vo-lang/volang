//! Type representations for the GoX type system.
//!
//! This module defines the internal representation of all GoX types used
//! throughout the type checking pipeline.
//!
//! # Type Hierarchy
//!
//! ```text
//! Type
//! ├── Basic (bool, int, uint, float64, string, etc.)
//! ├── Named (user-defined types with methods)
//! ├── Array ([N]T - fixed size)
//! ├── Slice ([]T - dynamic size)
//! ├── Map (map[K]V)
//! ├── Struct (struct { fields })
//! ├── Obx (object { fields } - GoX extension)
//! ├── Func (func(params) results)
//! ├── Interface (interface { methods })
//! ├── Chan (chan T, <-chan T, chan<- T)
//! ├── Tuple (multiple return values)
//! ├── Untyped (untyped constants)
//! ├── Nil (nil literal)
//! └── Invalid (error placeholder)
//! ```
//!
//! # Key Types
//!
//! - [`Type`]: The main enum representing all possible types
//! - [`BasicType`]: Primitive types like `int`, `bool`, `string`
//! - [`NamedTypeId`]: Reference to a user-defined named type
//! - [`NamedTypeInfo`]: Full information about a named type including methods
//! - [`FuncType`]: Function signature with params, results, and variadic flag
//! - [`InterfaceType`]: Interface with methods and embedded interfaces
//! - [`MethodSet`]: Collection of methods for interface satisfaction checking
//! - [`TypeRegistry`]: Lookup table for named type information
//!
//! # Untyped Constants
//!
//! GoX supports untyped constants that can be implicitly converted to compatible
//! types. The [`UntypedKind`] enum tracks the kind of untyped constant:
//! - `UntypedBool` → can become `bool`
//! - `UntypedInt` → can become any integer or float type
//! - `UntypedFloat` → can become any float type
//! - `UntypedString` → can become `string`
//! - `UntypedNil` → can become any slice, map, channel, or interface

use gox_common::Symbol;

/// Represents all types in GoX.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Basic types: int, int8, ..., float64, bool, string
    Basic(BasicType),

    /// Named type: `type MyInt int`
    Named(NamedTypeId),

    /// Array type: `[N]T`
    Array(ArrayType),

    /// Slice type: `[]T`
    Slice(SliceType),

    /// Map type: `map[K]V`
    Map(MapType),

    /// Channel type: `chan T`, `chan<- T`, `<-chan T`
    Chan(ChanType),

    /// Function type: `func(...) ...`
    Func(FuncType),

    /// Struct type: `struct { ... }`
    Struct(StructType),

    /// Pointer type: `*T` (only valid for struct types)
    Pointer(Box<Type>),

    /// Interface type: `interface { ... }`
    Interface(InterfaceType),

    /// Tuple type (for multi-value returns)
    Tuple(Vec<Type>),

    /// Untyped constant types
    Untyped(UntypedKind),

    /// Nil type (for nil literal before context resolution)
    Nil,

    /// Invalid type (for error recovery)
    Invalid,
}

/// Basic (primitive) types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BasicType {
    Bool,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
    String,
}

impl BasicType {
    /// Returns true if this is a numeric type.
    pub fn is_numeric(&self) -> bool {
        !matches!(self, BasicType::Bool | BasicType::String)
    }

    /// Returns true if this is an integer type.
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            BasicType::Int
                | BasicType::Int8
                | BasicType::Int16
                | BasicType::Int32
                | BasicType::Int64
                | BasicType::Uint
                | BasicType::Uint8
                | BasicType::Uint16
                | BasicType::Uint32
                | BasicType::Uint64
        )
    }

    /// Returns true if this is a signed integer type.
    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            BasicType::Int
                | BasicType::Int8
                | BasicType::Int16
                | BasicType::Int32
                | BasicType::Int64
        )
    }

    /// Returns true if this is an unsigned integer type.
    pub fn is_unsigned(&self) -> bool {
        matches!(
            self,
            BasicType::Uint
                | BasicType::Uint8
                | BasicType::Uint16
                | BasicType::Uint32
                | BasicType::Uint64
        )
    }

    /// Returns true if this is a floating-point type.
    pub fn is_float(&self) -> bool {
        matches!(self, BasicType::Float32 | BasicType::Float64)
    }

    /// Returns the name of this basic type.
    pub fn name(&self) -> &'static str {
        match self {
            BasicType::Bool => "bool",
            BasicType::Int => "int",
            BasicType::Int8 => "int8",
            BasicType::Int16 => "int16",
            BasicType::Int32 => "int32",
            BasicType::Int64 => "int64",
            BasicType::Uint => "uint",
            BasicType::Uint8 => "uint8",
            BasicType::Uint16 => "uint16",
            BasicType::Uint32 => "uint32",
            BasicType::Uint64 => "uint64",
            BasicType::Float32 => "float32",
            BasicType::Float64 => "float64",
            BasicType::String => "string",
        }
    }
}

/// Untyped constant kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UntypedKind {
    Bool,
    Int,
    Rune,
    Float,
    String,
}

impl UntypedKind {
    /// Returns the default type for this untyped kind.
    pub fn default_type(&self) -> BasicType {
        match self {
            UntypedKind::Bool => BasicType::Bool,
            UntypedKind::Int => BasicType::Int,
            UntypedKind::Rune => BasicType::Int32, // rune = int32
            UntypedKind::Float => BasicType::Float64,
            UntypedKind::String => BasicType::String,
        }
    }

    /// Returns the precedence for kind promotion.
    /// Higher precedence wins in mixed operations.
    pub fn precedence(&self) -> u8 {
        match self {
            UntypedKind::Bool => 0,
            UntypedKind::String => 0,
            UntypedKind::Int => 1,
            UntypedKind::Rune => 2,
            UntypedKind::Float => 3,
        }
    }
}

/// ID for a named type in the type registry.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NamedTypeId(pub u32);

/// Array type: `[N]T`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayType {
    pub len: u64,
    pub elem: Box<Type>,
}

/// Slice type: `[]T`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SliceType {
    pub elem: Box<Type>,
}

/// Map type: `map[K]V`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MapType {
    pub key: Box<Type>,
    pub value: Box<Type>,
}

/// Channel type: `chan T`, `chan<- T`, `<-chan T`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ChanType {
    pub dir: ChanDir,
    pub elem: Box<Type>,
}

/// Channel direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChanDir {
    /// Bidirectional: `chan T`
    Both,
    /// Send-only: `chan<- T`
    SendOnly,
    /// Receive-only: `<-chan T`
    RecvOnly,
}

/// Function type: `func(T) R`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub params: Vec<Type>,
    pub results: Vec<Type>,
    pub variadic: bool,
}

/// Struct or object type: `struct { ... }` or `object { ... }`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
    pub fields: Vec<Field>,
}

/// A struct/object field.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    /// Field name (None for embedded fields).
    pub name: Option<Symbol>,
    /// Field type.
    pub ty: Type,
    /// True if this is an embedded field.
    pub embedded: bool,
    /// Optional struct tag.
    pub tag: Option<String>,
}

/// Interface type: `interface { ... }`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InterfaceType {
    /// Methods in this interface.
    pub methods: Vec<Method>,
    /// Embedded interface names (resolved in Phase 2).
    pub embeds: Vec<Symbol>,
}

/// A method signature.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Method {
    pub name: Symbol,
    pub sig: FuncType,
    /// True if this method has a pointer receiver (*T), false for value receiver (T).
    /// This affects method set computation: pointer receiver methods only belong to *T's method set.
    pub is_pointer_receiver: bool,
}

impl Type {
    /// Returns the inner type if this is a pointer, otherwise returns self.
    /// This is useful for auto-dereferencing pointer types in field/method access.
    pub fn deref_if_pointer(&self) -> &Type {
        match self {
            Type::Pointer(inner) => inner.as_ref(),
            other => other,
        }
    }

    /// Returns true if this is a value type (copied on assignment).
    pub fn is_value_type(&self) -> bool {
        match self {
            Type::Basic(_) => true,
            Type::Array(_) => true,
            Type::Struct(_) => true,
            Type::Untyped(_) => true,
            // Named types inherit from underlying
            Type::Named(_) => false, // Need to look up in registry
            _ => false,
        }
    }

    /// Returns true if this is an object type (reference semantics, nil zero value).
    pub fn is_object_type(&self) -> bool {
        match self {
            Type::Slice(_) => true,
            Type::Map(_) => true,
            Type::Chan(_) => true,
            Type::Func(_) => true,
            Type::Pointer(_) => true,
            Type::Interface(_) => true,
            Type::Nil => true,
            // Named types inherit from underlying
            Type::Named(_) => false, // Need to look up in registry
            _ => false,
        }
    }

    /// Returns true if this type is comparable with == and !=.
    pub fn is_comparable(&self) -> bool {
        match self {
            Type::Basic(_) => true,
            Type::Array(a) => a.elem.is_comparable(),
            Type::Struct(s) => s.fields.iter().all(|f| f.ty.is_comparable()),
            Type::Pointer(_) => true, // reference comparison
            Type::Interface(_) => true,
            Type::Untyped(_) => true,
            // Slice, Map, Func, Chan are only comparable to nil
            Type::Slice(_) | Type::Map(_) | Type::Func(_) | Type::Chan(_) => false,
            Type::Named(_) => false, // Need to look up in registry
            Type::Tuple(_) => false,
            Type::Nil => true,
            Type::Invalid => false,
        }
    }

    /// Returns true if this type supports ordering (<, <=, >, >=).
    pub fn is_ordered(&self) -> bool {
        match self {
            Type::Basic(b) => matches!(
                b,
                BasicType::Int
                    | BasicType::Int8
                    | BasicType::Int16
                    | BasicType::Int32
                    | BasicType::Int64
                    | BasicType::Uint
                    | BasicType::Uint8
                    | BasicType::Uint16
                    | BasicType::Uint32
                    | BasicType::Uint64
                    | BasicType::Float32
                    | BasicType::Float64
                    | BasicType::String
            ),
            Type::Untyped(k) => matches!(
                k,
                UntypedKind::Int | UntypedKind::Rune | UntypedKind::Float | UntypedKind::String
            ),
            Type::Named(_) => false, // Need to look up in registry
            _ => false,
        }
    }

    /// Returns true if this is an untyped type.
    pub fn is_untyped(&self) -> bool {
        matches!(self, Type::Untyped(_))
    }

    /// Returns true if this is the invalid type.
    pub fn is_invalid(&self) -> bool {
        matches!(self, Type::Invalid)
    }

    /// Returns true if this is the nil type.
    pub fn is_nil(&self) -> bool {
        matches!(self, Type::Nil)
    }
}

/// Information about a named type.
#[derive(Debug, Clone)]
pub struct NamedTypeInfo {
    /// The type's name.
    pub name: Symbol,
    /// The underlying type.
    pub underlying: Type,
    /// Methods declared on this type.
    pub methods: Vec<Method>,
}

/// A computed method set for interface implementation checking.
#[derive(Debug, Clone, Default)]
pub struct MethodSet {
    /// Methods in the set, sorted by name for efficient comparison.
    pub methods: Vec<Method>,
}

impl MethodSet {
    /// Creates a new empty method set.
    pub fn new() -> Self {
        Self { methods: Vec::new() }
    }

    /// Creates a method set from a list of methods.
    pub fn from_methods(mut methods: Vec<Method>) -> Self {
        // Sort by name for consistent comparison
        methods.sort_by(|a, b| a.name.as_u32().cmp(&b.name.as_u32()));
        Self { methods }
    }

    /// Adds a method to the set.
    /// Returns false if a method with the same name already exists.
    pub fn add(&mut self, method: Method) -> bool {
        if self.methods.iter().any(|m| m.name == method.name) {
            return false;
        }
        self.methods.push(method);
        // Keep sorted
        self.methods.sort_by(|a, b| a.name.as_u32().cmp(&b.name.as_u32()));
        true
    }

    /// Merges another method set into this one.
    /// Returns the names of conflicting methods (same name, different signature).
    pub fn merge(&mut self, other: &MethodSet) -> Vec<Symbol> {
        let mut conflicts = Vec::new();
        for method in &other.methods {
            if let Some(existing) = self.methods.iter().find(|m| m.name == method.name) {
                // Check if signatures match
                if existing.sig != method.sig {
                    conflicts.push(method.name);
                }
                // Skip duplicate (same name, same sig is OK)
            } else {
                self.methods.push(method.clone());
            }
        }
        // Re-sort after merge
        self.methods.sort_by(|a, b| a.name.as_u32().cmp(&b.name.as_u32()));
        conflicts
    }

    /// Checks if this method set contains all methods from the required set.
    /// Returns the missing method names if not.
    pub fn implements(&self, required: &MethodSet) -> Result<(), Vec<Symbol>> {
        let mut missing = Vec::new();
        for req_method in &required.methods {
            let found = self.methods.iter().find(|m| m.name == req_method.name);
            match found {
                Some(m) if m.sig == req_method.sig => {
                    // OK - method exists with matching signature
                }
                Some(_) => {
                    // Method exists but signature doesn't match
                    missing.push(req_method.name);
                }
                None => {
                    // Method doesn't exist
                    missing.push(req_method.name);
                }
            }
        }
        if missing.is_empty() {
            Ok(())
        } else {
            Err(missing)
        }
    }

    /// Returns true if this method set is empty.
    pub fn is_empty(&self) -> bool {
        self.methods.is_empty()
    }

    /// Returns the number of methods in the set.
    pub fn len(&self) -> usize {
        self.methods.len()
    }

    /// Gets a method by name.
    pub fn get(&self, name: Symbol) -> Option<&Method> {
        self.methods.iter().find(|m| m.name == name)
    }
}

impl InterfaceType {
    /// Creates a method set from this interface's direct methods.
    /// Note: This does not expand embedded interfaces - use TypeRegistry for that.
    pub fn direct_method_set(&self) -> MethodSet {
        MethodSet::from_methods(self.methods.clone())
    }
}

/// A registry for looking up type information.
/// Used for computing method sets and checking interface implementation.
pub struct TypeRegistry<'a> {
    /// Named type information from Phase 2.
    pub named_types: &'a [NamedTypeInfo],
}

impl<'a> TypeRegistry<'a> {
    /// Creates a new type registry.
    pub fn new(named_types: &'a [NamedTypeInfo]) -> Self {
        Self { named_types }
    }

    /// Gets the underlying type for a named type.
    pub fn underlying(&self, id: NamedTypeId) -> Option<&Type> {
        self.named_types.get(id.0 as usize).map(|info| &info.underlying)
    }

    /// Gets the full method set for a type, including inherited methods.
    pub fn method_set(&self, ty: &Type) -> MethodSet {
        match ty {
            Type::Named(id) => {
                let idx = id.0 as usize;
                if idx < self.named_types.len() {
                    let info = &self.named_types[idx];
                    // Start with the type's own methods
                    let mut set = MethodSet::from_methods(info.methods.clone());
                    // Add methods from underlying type
                    match &info.underlying {
                        Type::Interface(iface) => {
                            let iface_set = self.interface_method_set(iface);
                            set.merge(&iface_set);
                        }
                        Type::Struct(s) => {
                            // Add methods from embedded fields
                            let struct_set = self.struct_method_set(s);
                            set.merge(&struct_set);
                        }
                        Type::Pointer(inner) => {
                            // Pointer to struct - get methods from pointed struct
                            if let Type::Struct(s) = inner.as_ref() {
                                let struct_set = self.struct_method_set(s);
                                set.merge(&struct_set);
                            }
                        }
                        _ => {}
                    }
                    set
                } else {
                    MethodSet::new()
                }
            }
            Type::Interface(iface) => self.interface_method_set(iface),
            Type::Pointer(inner) => {
                // Pointer types - get methods from pointed type
                match inner.as_ref() {
                    Type::Struct(s) => self.struct_method_set(s),
                    Type::Named(id) => {
                        // *Named - get methods from the named type
                        // This includes pointer receiver methods
                        let idx = id.0 as usize;
                        if idx < self.named_types.len() {
                            let info = &self.named_types[idx];
                            let mut set = MethodSet::from_methods(info.methods.clone());
                            // Also get embedded methods from underlying struct
                            if let Type::Struct(s) = &info.underlying {
                                let struct_set = self.struct_method_set(s);
                                set.merge(&struct_set);
                            }
                            set
                        } else {
                            MethodSet::new()
                        }
                    }
                    _ => MethodSet::new(),
                }
            }
            Type::Struct(s) => self.struct_method_set(s),
            _ => MethodSet::new(),
        }
    }

    /// Computes the full method set for an interface, expanding embedded interfaces.
    pub fn interface_method_set(&self, iface: &InterfaceType) -> MethodSet {
        let mut set = iface.direct_method_set();

        // Expand embedded interfaces
        for embed_name in &iface.embeds {
            // Find the embedded interface type
            if let Some(info) = self.named_types.iter().find(|i| i.name == *embed_name) {
                if let Type::Interface(embedded_iface) = &info.underlying {
                    let embedded_set = self.interface_method_set(embedded_iface);
                    set.merge(&embedded_set);
                }
            }
        }

        set
    }

    /// Computes method set from struct's embedded fields.
    fn struct_method_set(&self, s: &StructType) -> MethodSet {
        let mut set = MethodSet::new();

        for field in &s.fields {
            if field.embedded {
                // Get methods from embedded type
                let embedded_set = self.method_set(&field.ty);
                set.merge(&embedded_set);
            }
        }

        set
    }

    /// Checks if a type implements an interface.
    /// Returns Ok(()) if it does, or Err with missing method names.
    pub fn implements_interface(&self, ty: &Type, iface: &InterfaceType) -> Result<(), Vec<Symbol>> {
        let type_methods = self.method_set(ty);
        let iface_methods = self.interface_method_set(iface);
        type_methods.implements(&iface_methods)
    }

    /// Checks if a type can be assigned to an interface type.
    pub fn assignable_to_interface(&self, ty: &Type, iface_ty: &Type) -> Result<(), Vec<Symbol>> {
        match iface_ty {
            Type::Interface(iface) => self.implements_interface(ty, iface),
            Type::Named(id) => {
                if let Some(info) = self.named_types.get(id.0 as usize) {
                    if let Type::Interface(iface) = &info.underlying {
                        return self.implements_interface(ty, iface);
                    }
                }
                Err(vec![]) // Not an interface type
            }
            _ => Err(vec![]), // Not an interface type
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_type_properties() {
        assert!(BasicType::Int.is_numeric());
        assert!(BasicType::Int.is_integer());
        assert!(BasicType::Int.is_signed());
        assert!(!BasicType::Int.is_unsigned());
        assert!(!BasicType::Int.is_float());

        assert!(BasicType::Uint8.is_unsigned());
        assert!(!BasicType::Uint8.is_signed());

        assert!(BasicType::Float64.is_float());
        assert!(BasicType::Float64.is_numeric());
        assert!(!BasicType::Float64.is_integer());

        assert!(!BasicType::Bool.is_numeric());
        assert!(!BasicType::String.is_numeric());
    }

    #[test]
    fn test_type_categories() {
        let int_type = Type::Basic(BasicType::Int);
        assert!(int_type.is_comparable());
        assert!(int_type.is_ordered());

        let slice_type = Type::Slice(SliceType {
            elem: Box::new(Type::Basic(BasicType::Int)),
        });
        assert!(!slice_type.is_comparable());
        assert!(!slice_type.is_ordered());
        assert!(slice_type.is_object_type());

        let struct_type = Type::Struct(StructType { fields: vec![] });
        assert!(struct_type.is_comparable());
        assert!(struct_type.is_value_type());
    }

    #[test]
    fn test_untyped_kind_precedence() {
        assert!(UntypedKind::Float.precedence() > UntypedKind::Rune.precedence());
        assert!(UntypedKind::Rune.precedence() > UntypedKind::Int.precedence());
    }

    fn make_symbol(id: u32) -> Symbol {
        // Create distinct symbols for testing by using raw transmute
        // This is safe in tests since we just need distinct values
        unsafe { std::mem::transmute(id) }
    }

    fn make_method(name_id: u32, param_count: usize, result_count: usize) -> Method {
        Method {
            name: make_symbol(name_id),
            sig: FuncType {
                params: vec![Type::Basic(BasicType::Int); param_count],
                results: vec![Type::Basic(BasicType::Int); result_count],
                variadic: false,
            },
            is_pointer_receiver: false,
        }
    }

    #[test]
    fn test_method_set_add() {
        let mut set = MethodSet::new();
        let m1 = make_method(1, 0, 0);
        let m2 = make_method(2, 1, 1);

        assert!(set.add(m1.clone()));
        assert!(!set.add(m1.clone())); // Duplicate name
        assert!(set.add(m2));
        assert_eq!(set.len(), 2);
    }

    #[test]
    fn test_method_set_implements() {
        let m1 = make_method(1, 0, 1);
        let m2 = make_method(2, 1, 0);

        let required = MethodSet::from_methods(vec![m1.clone()]);
        let full = MethodSet::from_methods(vec![m1.clone(), m2]);
        let empty = MethodSet::new();

        assert!(full.implements(&required).is_ok());
        assert!(full.implements(&empty).is_ok());
        assert!(empty.implements(&required).is_err());
    }

    #[test]
    fn test_method_set_merge() {
        let m1 = make_method(1, 0, 0);
        let m2 = make_method(2, 0, 0);

        let mut set1 = MethodSet::from_methods(vec![m1.clone()]);
        let set2 = MethodSet::from_methods(vec![m2]);

        let conflicts = set1.merge(&set2);
        assert!(conflicts.is_empty());
        assert_eq!(set1.len(), 2);
    }

    #[test]
    fn test_method_set_merge_conflict() {
        // Same name, different signature = conflict
        let m1 = make_method(1, 0, 0); // () -> ()
        let m2 = make_method(1, 1, 0); // (int) -> ()

        let mut set1 = MethodSet::from_methods(vec![m1]);
        let set2 = MethodSet::from_methods(vec![m2]);

        let conflicts = set1.merge(&set2);
        assert_eq!(conflicts.len(), 1);
    }

    #[test]
    fn test_method_set_implements_wrong_sig() {
        let m1 = make_method(1, 0, 0); // () -> ()
        let m2 = make_method(1, 1, 0); // (int) -> () - same name, different sig

        let required = MethodSet::from_methods(vec![m1]);
        let provided = MethodSet::from_methods(vec![m2]);

        // Should fail because signature doesn't match
        assert!(provided.implements(&required).is_err());
    }
}

