//! Type representations for the GoX type system.
//!
//! This module defines the internal representation of all GoX types.

use gox_common::Symbol;

/// Represents all types in GoX.
#[derive(Debug, Clone, PartialEq, Eq)]
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

    /// Object type: `object { ... }` (named Obx in Rust code)
    Obx(StructType),

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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayType {
    pub len: u64,
    pub elem: Box<Type>,
}

/// Slice type: `[]T`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SliceType {
    pub elem: Box<Type>,
}

/// Map type: `map[K]V`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapType {
    pub key: Box<Type>,
    pub value: Box<Type>,
}

/// Channel type: `chan T`, `chan<- T`, `<-chan T`
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncType {
    pub params: Vec<Type>,
    pub results: Vec<Type>,
    pub variadic: bool,
}

/// Struct or object type: `struct { ... }` or `object { ... }`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType {
    pub fields: Vec<Field>,
}

/// A struct/object field.
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterfaceType {
    /// Methods in this interface.
    pub methods: Vec<Method>,
    /// Embedded interface names (resolved in Phase 2).
    pub embeds: Vec<Symbol>,
}

/// A method signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Method {
    pub name: Symbol,
    pub sig: FuncType,
}

impl Type {
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
            Type::Obx(_) => true,
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
            Type::Obx(_) => true, // reference comparison
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
}
