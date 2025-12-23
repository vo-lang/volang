//! Central container for all type checking objects.
//!
//! This module defines the key types and the central `TCObjects` container
//! that holds all objects created during type checking.

use crate::arena::Arena;
use crate::check::resolver::DeclInfo;
use crate::obj::LangObj;
use crate::package::Package;
use crate::scope::Scope;
use crate::typ::Type;
use crate::universe::Universe;
use std::borrow::Cow;

// Define all key types used in type checking
crate::define_key! {
    /// Key for language objects (variables, functions, types, etc.)
    pub struct ObjKey;
    
    /// Key for types
    pub struct TypeKey;
    
    /// Key for scopes
    pub struct ScopeKey;
    
    /// Key for packages
    pub struct PackageKey;
    
    /// Key for declaration info
    pub struct DeclInfoKey;
}

/// Type aliases for arena containers
pub type LangObjs = Arena<ObjKey, LangObj>;
pub type Types = Arena<TypeKey, Type>;
pub type Scopes = Arena<ScopeKey, Scope>;
pub type Packages = Arena<PackageKey, Package>;
pub type Decls = Arena<DeclInfoKey, DeclInfo>;

/// Type alias for the qualifier function used in formatting.
pub type FmtQualifier = Box<dyn Fn(&Package) -> Cow<'_, str>>;

fn default_fmt_qualifier(p: &Package) -> Cow<'_, str> {
    Cow::Owned(p.path().to_string())
}

/// Central container for all type checking objects.
///
/// This is the main data structure that holds all objects created during
/// type checking. Objects are allocated in arenas and referenced by typed keys.
pub struct TCObjects {
    /// All language objects (variables, functions, types, constants, etc.)
    pub lobjs: LangObjs,
    
    /// All types
    pub types: Types,
    
    /// All scopes
    pub scopes: Scopes,
    
    /// All packages
    pub pkgs: Packages,
    
    /// Declaration info for package-level objects
    pub decls: Decls,
    
    /// The universe with predefined types and functions
    pub universe: Option<Universe>,
    
    /// Qualifier function for formatting output
    pub fmt_qualifier: FmtQualifier,
}

impl Default for TCObjects {
    fn default() -> Self {
        Self::new()
    }
}

impl TCObjects {
    /// Creates a new empty TCObjects container.
    pub fn new() -> Self {
        let mut objs = Self {
            lobjs: Arena::new(),
            types: Arena::new(),
            scopes: Arena::new(),
            pkgs: Arena::new(),
            decls: Arena::new(),
            universe: None,
            fmt_qualifier: Box::new(default_fmt_qualifier),
        };
        objs.universe = Some(Universe::new(&mut objs));
        objs
    }
    
    /// Returns a reference to the universe.
    pub fn universe(&self) -> &Universe {
        self.universe.as_ref().expect("universe not initialized")
    }

    /// Creates a new TCObjects with pre-allocated capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        let mut objs = Self {
            lobjs: Arena::with_capacity(capacity),
            types: Arena::with_capacity(capacity),
            scopes: Arena::with_capacity(capacity / 4),
            pkgs: Arena::with_capacity(16),
            decls: Arena::with_capacity(capacity / 2),
            universe: None,
            fmt_qualifier: Box::new(default_fmt_qualifier),
        };
        objs.universe = Some(Universe::new(&mut objs));
        objs
    }

    // ----------------------------------------------------------------------------
    // Factory methods for creating objects

    pub fn new_scope(
        &mut self,
        parent: Option<ScopeKey>,
        pos: crate::obj::Pos,
        end: crate::obj::Pos,
        comment: &str,
        is_func: bool,
    ) -> ScopeKey {
        let scope = if is_func {
            Scope::new_func(parent, pos, end)
        } else {
            Scope::new(parent, pos, end, comment)
        };
        let skey = self.scopes.insert(scope);
        if let Some(p) = parent {
            // Don't add children to Universe scope
            if self.universe.is_none() || p != self.universe().scope() {
                self.scopes[p].add_child(skey);
            }
        }
        skey
    }

    pub fn new_package(&mut self, path: String) -> PackageKey {
        let universe_scope = self.universe.as_ref().map(|u| u.scope());
        let skey = self.new_scope(universe_scope, 0, 0, &format!("package {}", path), false);
        let pkg = Package::new(path, None, skey);
        self.pkgs.insert(pkg)
    }
    
    /// Find an existing package by its path.
    pub fn find_package_by_path(&self, path: &str) -> Option<PackageKey> {
        for (key, pkg) in self.pkgs.iter() {
            if pkg.path() == path {
                return Some(key);
            }
        }
        None
    }

    pub fn new_pkg_name(
        &mut self,
        pos: crate::obj::Pos,
        pkg: Option<PackageKey>,
        name: String,
        imported: PackageKey,
    ) -> ObjKey {
        let lobj = crate::obj::LangObj::new_pkg_name(pos, pkg, name, imported, self.universe());
        self.lobjs.insert(lobj)
    }

    pub fn new_const(
        &mut self,
        pos: crate::obj::Pos,
        pkg: Option<PackageKey>,
        name: String,
        typ: Option<TypeKey>,
        val: crate::obj::ConstValue,
    ) -> ObjKey {
        let lobj = crate::obj::LangObj::new_const(pos, pkg, name, typ, val);
        self.lobjs.insert(lobj)
    }

    pub fn new_type_name(
        &mut self,
        pos: crate::obj::Pos,
        pkg: Option<PackageKey>,
        name: String,
        typ: Option<TypeKey>,
    ) -> ObjKey {
        let lobj = crate::obj::LangObj::new_type_name(pos, pkg, name, typ);
        self.lobjs.insert(lobj)
    }

    pub fn new_var(
        &mut self,
        pos: crate::obj::Pos,
        pkg: Option<PackageKey>,
        name: String,
        typ: Option<TypeKey>,
    ) -> ObjKey {
        let lobj = crate::obj::LangObj::new_var(pos, pkg, name, typ);
        self.lobjs.insert(lobj)
    }

    pub fn new_param_var(
        &mut self,
        pos: crate::obj::Pos,
        pkg: Option<PackageKey>,
        name: String,
        typ: Option<TypeKey>,
    ) -> ObjKey {
        let lobj = crate::obj::LangObj::new_param(pos, pkg, name, typ);
        self.lobjs.insert(lobj)
    }

    pub fn new_field(
        &mut self,
        pos: crate::obj::Pos,
        pkg: Option<PackageKey>,
        name: String,
        typ: Option<TypeKey>,
        embedded: bool,
    ) -> ObjKey {
        let lobj = crate::obj::LangObj::new_field(pos, pkg, name, typ, embedded);
        self.lobjs.insert(lobj)
    }

    pub fn new_func(
        &mut self,
        pos: crate::obj::Pos,
        pkg: Option<PackageKey>,
        name: String,
        typ: Option<TypeKey>,
    ) -> ObjKey {
        let lobj = crate::obj::LangObj::new_func(pos, pkg, name, typ);
        self.lobjs.insert(lobj)
    }

    pub fn new_label(
        &mut self,
        pos: crate::obj::Pos,
        pkg: Option<PackageKey>,
        name: String,
    ) -> ObjKey {
        let lobj = crate::obj::LangObj::new_label(pos, pkg, name, self.universe());
        self.lobjs.insert(lobj)
    }

    // ----------------------------------------------------------------------------
    // Factory methods for creating types

    pub fn new_t_basic(
        &mut self,
        typ: crate::typ::BasicType,
        info: crate::typ::BasicInfo,
        name: &'static str,
    ) -> TypeKey {
        self.types.insert(Type::Basic(crate::typ::BasicDetail::new(typ, info, name)))
    }

    pub fn new_t_array(&mut self, elem: TypeKey, len: Option<u64>) -> TypeKey {
        self.types.insert(Type::Array(crate::typ::ArrayDetail::new(elem, len)))
    }

    pub fn new_t_slice(&mut self, elem: TypeKey) -> TypeKey {
        self.types.insert(Type::Slice(crate::typ::SliceDetail::new(elem)))
    }

    pub fn new_t_struct(
        &mut self,
        fields: Vec<ObjKey>,
        tags: Option<Vec<Option<String>>>,
    ) -> TypeKey {
        self.types.insert(Type::Struct(crate::typ::StructDetail::new(fields, tags)))
    }

    pub fn new_t_pointer(&mut self, base: TypeKey) -> TypeKey {
        self.types.insert(Type::Pointer(crate::typ::PointerDetail::new(base)))
    }

    pub fn new_t_tuple(&mut self, vars: Vec<ObjKey>) -> TypeKey {
        self.types.insert(Type::Tuple(crate::typ::TupleDetail::new(vars)))
    }

    pub fn new_t_signature(
        &mut self,
        scope: Option<ScopeKey>,
        recv: Option<ObjKey>,
        params: TypeKey,
        results: TypeKey,
        variadic: bool,
    ) -> TypeKey {
        self.types.insert(Type::Signature(crate::typ::SignatureDetail::new(
            scope, recv, params, results, variadic,
        )))
    }

    pub fn new_t_interface(&mut self, methods: Vec<ObjKey>, embeddeds: Vec<TypeKey>) -> TypeKey {
        self.types.insert(Type::Interface(crate::typ::InterfaceDetail::new(methods, embeddeds)))
    }

    pub fn new_t_empty_interface(&mut self) -> TypeKey {
        self.types.insert(Type::Interface(crate::typ::InterfaceDetail::new_empty()))
    }

    pub fn new_t_map(&mut self, key: TypeKey, elem: TypeKey) -> TypeKey {
        self.types.insert(Type::Map(crate::typ::MapDetail::new(key, elem)))
    }

    pub fn new_t_chan(&mut self, dir: crate::typ::ChanDir, elem: TypeKey) -> TypeKey {
        self.types.insert(Type::Chan(crate::typ::ChanDetail::new(dir, elem)))
    }

    pub fn new_t_named(
        &mut self,
        obj: Option<ObjKey>,
        underlying: Option<TypeKey>,
        methods: Vec<ObjKey>,
    ) -> TypeKey {
        self.types.insert(Type::Named(crate::typ::NamedDetail::new(obj, underlying, methods)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tcobjects_creation() {
        let tc = TCObjects::new();
        // TCObjects::new() creates a Universe which populates the arenas
        assert!(tc.universe.is_some());
        // Arenas should have universe objects (not empty)
        assert!(!tc.lobjs.is_empty());
        assert!(!tc.types.is_empty());
        assert!(!tc.scopes.is_empty());
    }

    #[test]
    fn test_key_types() {
        use crate::arena::ArenaKey;
        
        let obj_key = ObjKey::from_usize(42);
        assert_eq!(obj_key.as_usize(), 42);
        
        let type_key = TypeKey::null();
        assert!(type_key.is_null());
    }
}
