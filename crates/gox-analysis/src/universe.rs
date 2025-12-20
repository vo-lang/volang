//! Universe scope and predefined types/functions.
//!
//! This module sets up the universe scope with all predefined types,
//! constants, and built-in functions.

#![allow(dead_code)]

use crate::obj::{Builtin, ConstValue, LangObj};
use crate::objects::{ObjKey, PackageKey, ScopeKey, TCObjects, TypeKey};
use crate::package::Package;
use crate::scope::Scope;
use crate::typ::{
    BasicDetail, BasicInfo, BasicType, InterfaceDetail, SignatureDetail, SliceDetail,
    TupleDetail, Type,
};
use std::collections::HashMap;

/// ExprKind describes the kind of an expression.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ExprKind {
    Conversion,
    Expression,
    Statement,
}

/// Information about a built-in function.
#[derive(Copy, Clone, Debug)]
pub struct BuiltinInfo {
    pub name: &'static str,
    pub arg_count: usize,
    pub variadic: bool,
    pub kind: ExprKind,
}

/// Universe sets up the universe scope with all predefined types and functions.
#[derive(Debug)]
pub struct Universe {
    scope: ScopeKey,
    unsafe_pkg: PackageKey,
    iota: ObjKey,
    byte: TypeKey,
    rune: TypeKey,
    slice_of_bytes: TypeKey,
    no_value_tuple: TypeKey,
    error_type: TypeKey,
    /// indir is a sentinel type name that is pushed onto the object path
    /// to indicate an "indirection" in the dependency from one type name
    /// to the next. For instance, for "type p *p" the object path contains
    /// p followed by indir, indicating that there's an indirection *p.
    /// Indirections are used to break type cycles.
    indir: ObjKey,
    /// guard_sig is an empty signature type used to guard against func cycles.
    guard_sig: TypeKey,
    types: HashMap<BasicType, TypeKey>,
    builtins: HashMap<Builtin, BuiltinInfo>,
}

impl Universe {
    /// Creates a new universe with all predefined types and functions.
    pub fn new(objs: &mut TCObjects) -> Universe {
        // Create universe scope and unsafe package
        let (universe_scope, unsafe_pkg) = Self::create_universe_scope(objs);

        // Create all basic types
        let types = Self::create_basic_types(objs);

        // Define basic type names in universe scope
        Self::define_basic_types(&types, universe_scope, objs);

        // Create alias types (byte, rune)
        let alias_types = Self::create_alias_types(objs);
        Self::define_basic_types(&alias_types, universe_scope, objs);

        // Get byte and rune type keys
        let byte = alias_types[&BasicType::Byte];
        let rune = alias_types[&BasicType::Rune];

        // Create error type
        let error_type = Self::create_error_type(&types, universe_scope, objs);

        // Define constants (true, false, iota)
        let iota = Self::define_constants(&types, universe_scope, objs);

        // Define nil
        Self::define_nil(&types, universe_scope, objs);

        // Create builtin function info
        let builtins = Self::create_builtins();

        // Define builtin functions
        let invalid_type = types[&BasicType::Invalid];
        Self::define_builtins(&builtins, invalid_type, universe_scope, objs);

        // Create helper types
        let slice_of_bytes = objs.types.insert(Type::Slice(SliceDetail::new(byte)));
        let no_value_tuple = objs.types.insert(Type::Tuple(TupleDetail::new(vec![])));
        
        // Create indir sentinel for type cycle detection
        let indir = objs.lobjs.insert(LangObj::new_type_name(0, None, "*".to_string(), None));
        
        // Create guard_sig for func cycle detection
        let guard_sig = objs.types.insert(Type::Signature(SignatureDetail::new(
            None, None, no_value_tuple, no_value_tuple, false,
        )));

        Universe {
            scope: universe_scope,
            unsafe_pkg,
            iota,
            byte,
            rune,
            slice_of_bytes,
            no_value_tuple,
            error_type,
            indir,
            guard_sig,
            types,
            builtins,
        }
    }

    pub fn scope(&self) -> ScopeKey {
        self.scope
    }

    pub fn unsafe_pkg(&self) -> PackageKey {
        self.unsafe_pkg
    }

    pub fn iota(&self) -> ObjKey {
        self.iota
    }

    pub fn byte(&self) -> TypeKey {
        self.byte
    }

    pub fn rune(&self) -> TypeKey {
        self.rune
    }

    pub fn slice_of_bytes(&self) -> TypeKey {
        self.slice_of_bytes
    }

    pub fn no_value_tuple(&self) -> TypeKey {
        self.no_value_tuple
    }

    pub fn error_type(&self) -> TypeKey {
        self.error_type
    }

    pub fn indir(&self) -> ObjKey {
        self.indir
    }

    pub fn guard_sig(&self) -> TypeKey {
        self.guard_sig
    }

    pub fn types(&self) -> &HashMap<BasicType, TypeKey> {
        &self.types
    }

    pub fn builtins(&self) -> &HashMap<Builtin, BuiltinInfo> {
        &self.builtins
    }

    pub fn lookup_type(&self, basic: BasicType) -> Option<TypeKey> {
        self.types.get(&basic).copied()
    }

    fn create_universe_scope(objs: &mut TCObjects) -> (ScopeKey, PackageKey) {
        let universe_scope = objs.scopes.insert(Scope::new(None, 0, 0, "universe"));
        let unsafe_scope = objs
            .scopes
            .insert(Scope::new(Some(universe_scope), 0, 0, "package unsafe"));
        let mut pkg = Package::new("unsafe".to_string(), Some("unsafe".to_string()), unsafe_scope);
        pkg.mark_complete();
        let unsafe_pkg = objs.pkgs.insert(pkg);
        (universe_scope, unsafe_pkg)
    }

    fn create_basic_types(objs: &mut TCObjects) -> HashMap<BasicType, TypeKey> {
        let basics = vec![
            (BasicType::Invalid, BasicInfo::IsInvalid, "invalid type"),
            (BasicType::Bool, BasicInfo::IsBoolean, "bool"),
            (BasicType::Int, BasicInfo::IsInteger, "int"),
            (BasicType::Int8, BasicInfo::IsInteger, "int8"),
            (BasicType::Int16, BasicInfo::IsInteger, "int16"),
            (BasicType::Int32, BasicInfo::IsInteger, "int32"),
            (BasicType::Int64, BasicInfo::IsInteger, "int64"),
            (BasicType::Uint, BasicInfo::IsInteger, "uint"),
            (BasicType::Uint8, BasicInfo::IsInteger, "uint8"),
            (BasicType::Uint16, BasicInfo::IsInteger, "uint16"),
            (BasicType::Uint32, BasicInfo::IsInteger, "uint32"),
            (BasicType::Uint64, BasicInfo::IsInteger, "uint64"),
            (BasicType::Uintptr, BasicInfo::IsInteger, "uintptr"),
            (BasicType::Float32, BasicInfo::IsFloat, "float32"),
            (BasicType::Float64, BasicInfo::IsFloat, "float64"),
            (BasicType::Str, BasicInfo::IsString, "string"),
            (BasicType::UntypedBool, BasicInfo::IsBoolean, "untyped bool"),
            (BasicType::UntypedInt, BasicInfo::IsInteger, "untyped int"),
            (BasicType::UntypedRune, BasicInfo::IsInteger, "untyped rune"),
            (BasicType::UntypedFloat, BasicInfo::IsFloat, "untyped float"),
            (
                BasicType::UntypedString,
                BasicInfo::IsString,
                "untyped string",
            ),
            (BasicType::UntypedNil, BasicInfo::IsInvalid, "untyped nil"),
        ];

        basics
            .into_iter()
            .map(|(typ, info, name)| {
                let key = objs
                    .types
                    .insert(Type::Basic(BasicDetail::new(typ, info, name)));
                (typ, key)
            })
            .collect()
    }

    fn create_alias_types(objs: &mut TCObjects) -> HashMap<BasicType, TypeKey> {
        vec![
            (BasicType::Byte, BasicInfo::IsInteger, "byte"),
            (BasicType::Rune, BasicInfo::IsInteger, "rune"),
        ]
        .into_iter()
        .map(|(typ, info, name)| {
            let key = objs
                .types
                .insert(Type::Basic(BasicDetail::new(typ, info, name)));
            (typ, key)
        })
        .collect()
    }

    fn define_basic_types(
        types: &HashMap<BasicType, TypeKey>,
        universe_scope: ScopeKey,
        objs: &mut TCObjects,
    ) {
        for (_, &type_key) in types {
            let name = objs.types[type_key]
                .try_as_basic()
                .map(|b| b.name().to_string())
                .unwrap_or_default();

            // Skip types with spaces in name (internal types like "untyped bool")
            if name.contains(' ') {
                continue;
            }

            let obj = LangObj::new_type_name(0, None, name.clone(), Some(type_key));
            let obj_key = objs.lobjs.insert(obj);
            Scope::insert(universe_scope, obj_key, objs);
        }
    }

    fn create_error_type(
        types: &HashMap<BasicType, TypeKey>,
        universe_scope: ScopeKey,
        objs: &mut TCObjects,
    ) -> TypeKey {
        // Create: type error interface { Error() string }
        let string_type = types[&BasicType::Str];

        // Create result variable for Error() method
        let result_var = objs
            .lobjs
            .insert(LangObj::new_var(0, None, "".to_string(), Some(string_type)));

        // Create empty params tuple and results tuple with string
        let params = objs.types.insert(Type::Tuple(TupleDetail::new(vec![])));
        let results = objs
            .types
            .insert(Type::Tuple(TupleDetail::new(vec![result_var])));

        // Create signature for Error() method
        let sig = objs.types.insert(Type::Signature(SignatureDetail::new(
            None, None, params, results, false,
        )));

        // Create Error method
        let error_method = objs
            .lobjs
            .insert(LangObj::new_func(0, None, "Error".to_string(), Some(sig)));

        // Create interface with Error method
        let iface = InterfaceDetail::new(vec![error_method], vec![]);

        // Create underlying interface type
        let underlying = objs.types.insert(Type::Interface(iface));

        // Create named type "error"
        let error_type = objs.types.insert(Type::Named(crate::typ::NamedDetail::new(
            None,
            Some(underlying),
            vec![],
        )));

        // Create type name object
        let type_name = objs.lobjs.insert(LangObj::new_type_name(
            0,
            None,
            "error".to_string(),
            Some(error_type),
        ));

        // Insert into universe scope
        Scope::insert(universe_scope, type_name, objs);

        error_type
    }

    fn define_constants(
        types: &HashMap<BasicType, TypeKey>,
        universe_scope: ScopeKey,
        objs: &mut TCObjects,
    ) -> ObjKey {
        let mut iota_key = ObjKey::default();

        let consts = vec![
            (
                "true",
                BasicType::UntypedBool,
                ConstValue::Bool(true),
                false,
            ),
            (
                "false",
                BasicType::UntypedBool,
                ConstValue::Bool(false),
                false,
            ),
            ("iota", BasicType::UntypedInt, ConstValue::with_i64(0), true),
        ];

        for (name, typ, val, is_iota) in consts {
            let type_key = types[&typ];
            let obj = LangObj::new_const(0, None, name.to_string(), Some(type_key), val);
            let obj_key = objs.lobjs.insert(obj);
            Scope::insert(universe_scope, obj_key, objs);
            if is_iota {
                iota_key = obj_key;
            }
        }

        iota_key
    }

    fn define_nil(
        types: &HashMap<BasicType, TypeKey>,
        universe_scope: ScopeKey,
        objs: &mut TCObjects,
    ) {
        let nil_type = types[&BasicType::UntypedNil];
        let nil = LangObj::new_nil(nil_type);
        let nil_key = objs.lobjs.insert(nil);
        Scope::insert(universe_scope, nil_key, objs);
    }

    fn create_builtins() -> HashMap<Builtin, BuiltinInfo> {
        vec![
            (Builtin::Append, "append", 1, true, ExprKind::Expression),
            (Builtin::Cap, "cap", 1, false, ExprKind::Expression),
            (Builtin::Close, "close", 1, false, ExprKind::Statement),
            (Builtin::Copy, "copy", 2, false, ExprKind::Statement),
            (Builtin::Delete, "delete", 2, false, ExprKind::Statement),
            (Builtin::Len, "len", 1, false, ExprKind::Expression),
            (Builtin::Make, "make", 1, true, ExprKind::Expression),
            (Builtin::New, "new", 1, false, ExprKind::Expression),
            (Builtin::Panic, "panic", 1, false, ExprKind::Statement),
            (Builtin::Print, "print", 0, true, ExprKind::Statement),
            (Builtin::Println, "println", 0, true, ExprKind::Statement),
            (Builtin::Recover, "recover", 0, false, ExprKind::Statement),
            (Builtin::Assert, "assert", 1, false, ExprKind::Statement),
        ]
        .into_iter()
        .map(|(builtin, name, arg_count, variadic, kind)| {
            (
                builtin,
                BuiltinInfo {
                    name,
                    arg_count,
                    variadic,
                    kind,
                },
            )
        })
        .collect()
    }

    fn define_builtins(
        builtins: &HashMap<Builtin, BuiltinInfo>,
        invalid_type: TypeKey,
        universe_scope: ScopeKey,
        objs: &mut TCObjects,
    ) {
        for (&builtin, info) in builtins {
            let obj = LangObj::new_builtin(builtin, invalid_type);
            let obj_key = objs.lobjs.insert(obj);
            Scope::insert(universe_scope, obj_key, objs);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_universe_creation() {
        let mut objs = TCObjects::new();
        let universe = Universe::new(&mut objs);

        // Check that basic types are created
        assert!(universe.lookup_type(BasicType::Int).is_some());
        // BasicType::Str is the string type (not BasicType::String)
        assert!(universe.lookup_type(BasicType::Str).is_some());
        assert!(universe.lookup_type(BasicType::Bool).is_some());

        // Check that builtins are created
        assert!(universe.builtins().contains_key(&Builtin::Len));
        assert!(universe.builtins().contains_key(&Builtin::Append));
    }
}
