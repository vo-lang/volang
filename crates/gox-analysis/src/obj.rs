//! Language objects representing named entities.
//!
//! A LangObj describes a named language entity such as a package,
//! constant, type, variable, function (incl. methods), or label.

#![allow(dead_code)]

pub use crate::constant::Value as ConstValue;
use crate::objects::{ObjKey, PackageKey, ScopeKey, TCObjects, TypeKey};
use crate::typ::{self, BasicType};
use crate::universe::Universe;
use crate::package::Package;
use std::borrow::Cow;

/// Returns a unique identifier for a (package, name) pair.
pub fn get_id<'a>(pkg: Option<&'a Package>, name: &'a str) -> Cow<'a, str> {
    if is_exported(name) {
        Cow::Borrowed(name)
    } else {
        let path = pkg.map(|p| p.path()).unwrap_or("_");
        Cow::Owned(format!("{}.{}", path, name))
    }
}

/// Returns true if name is exported (starts with uppercase).
pub fn is_exported(name: &str) -> bool {
    name.chars().next().map_or(false, |c| c.is_uppercase())
}

/// Properties for variable entities.
#[derive(Clone, Debug, PartialEq)]
pub struct VarProperty {
    pub embedded: bool,
    pub is_field: bool,
    pub used: bool,
}

impl VarProperty {
    pub fn new(embedded: bool, is_field: bool, used: bool) -> VarProperty {
        VarProperty {
            embedded,
            is_field,
            used,
        }
    }
}

/// Built-in function kinds.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Builtin {
    // Predeclared
    Append,
    Cap,
    Close,
    Copy,
    Delete,
    Len,
    Make,
    New,
    Panic,
    Print,
    Println,
    Recover,
    // GoX extensions
    Assert,
}

impl Builtin {
    pub fn name(&self) -> &'static str {
        match self {
            Builtin::Append => "append",
            Builtin::Cap => "cap",
            Builtin::Close => "close",
            Builtin::Copy => "copy",
            Builtin::Delete => "delete",
            Builtin::Len => "len",
            Builtin::Make => "make",
            Builtin::New => "new",
            Builtin::Panic => "panic",
            Builtin::Print => "print",
            Builtin::Println => "println",
            Builtin::Recover => "recover",
            Builtin::Assert => "assert",
        }
    }
}

/// EntityType defines the types of LangObj entities.
#[derive(Clone, Debug, PartialEq)]
pub enum EntityType {
    /// An imported package.
    PkgName {
        imported: PackageKey,
        used: bool,
    },
    /// A declared constant.
    Const {
        val: ConstValue,
    },
    /// A name for a (defined or alias) type.
    TypeName,
    /// A declared variable (including function parameters, results, and struct fields).
    Var(VarProperty),
    /// A declared function, concrete method, or abstract method.
    Func {
        has_ptr_recv: bool,
    },
    /// A declared label.
    Label {
        used: bool,
    },
    /// A built-in function.
    Builtin(Builtin),
    /// The predeclared value nil.
    Nil,
}

impl EntityType {
    pub fn is_pkg_name(&self) -> bool {
        matches!(self, EntityType::PkgName { .. })
    }

    pub fn is_const(&self) -> bool {
        matches!(self, EntityType::Const { .. })
    }

    pub fn is_type_name(&self) -> bool {
        matches!(self, EntityType::TypeName)
    }

    pub fn is_var(&self) -> bool {
        matches!(self, EntityType::Var(_))
    }

    pub fn is_func(&self) -> bool {
        matches!(self, EntityType::Func { .. })
    }

    pub fn is_label(&self) -> bool {
        matches!(self, EntityType::Label { .. })
    }

    pub fn is_builtin(&self) -> bool {
        matches!(self, EntityType::Builtin(_))
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, EntityType::Nil)
    }

    pub fn is_dependency(&self) -> bool {
        matches!(
            self,
            EntityType::Const { .. } | EntityType::Var(_) | EntityType::Func { .. }
        )
    }

    pub fn func_has_ptr_recv(&self) -> bool {
        match self {
            EntityType::Func { has_ptr_recv } => *has_ptr_recv,
            _ => false,
        }
    }

    pub fn func_set_has_ptr_recv(&mut self, has: bool) {
        if let EntityType::Func { has_ptr_recv } = self {
            *has_ptr_recv = has;
        }
    }

    pub fn var_property(&self) -> &VarProperty {
        match self {
            EntityType::Var(prop) => prop,
            _ => panic!("not a var"),
        }
    }

    pub fn var_property_mut(&mut self) -> &mut VarProperty {
        match self {
            EntityType::Var(prop) => prop,
            _ => panic!("not a var"),
        }
    }
}


/// Object color for dependency ordering.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum ObjColor {
    White,
    Black,
    Gray(usize),
}

/// Position type (will be replaced with proper span later).
pub type Pos = usize;

/// A LangObj describes a named language entity.
#[derive(Clone, Debug)]
pub struct LangObj {
    entity_type: EntityType,
    parent: Option<ScopeKey>,
    pos: Pos,
    pkg: Option<PackageKey>,
    name: String,
    typ: Option<TypeKey>,
    order: u32,
    color: ObjColor,
    scope_pos: Pos,
}

impl LangObj {
    fn new(
        entity_type: EntityType,
        pos: Pos,
        pkg: Option<PackageKey>,
        name: String,
        typ: Option<TypeKey>,
    ) -> LangObj {
        let color = if typ.is_some() {
            ObjColor::Black
        } else {
            ObjColor::White
        };
        LangObj {
            entity_type,
            parent: None,
            pos,
            pkg,
            name,
            typ,
            order: 0,
            color,
            scope_pos: 0,
        }
    }

    pub fn new_pkg_name(
        pos: Pos,
        pkg: Option<PackageKey>,
        name: String,
        imported: PackageKey,
        univ: &Universe,
    ) -> LangObj {
        let t = univ.types()[&BasicType::Invalid];
        LangObj::new(
            EntityType::PkgName {
                imported,
                used: false,
            },
            pos,
            pkg,
            name,
            Some(t),
        )
    }

    pub fn new_const(
        pos: Pos,
        pkg: Option<PackageKey>,
        name: String,
        typ: Option<TypeKey>,
        val: ConstValue,
    ) -> LangObj {
        LangObj::new(EntityType::Const { val }, pos, pkg, name, typ)
    }

    pub fn new_type_name(
        pos: Pos,
        pkg: Option<PackageKey>,
        name: String,
        typ: Option<TypeKey>,
    ) -> LangObj {
        LangObj::new(EntityType::TypeName, pos, pkg, name, typ)
    }

    pub fn new_var(
        pos: Pos,
        pkg: Option<PackageKey>,
        name: String,
        typ: Option<TypeKey>,
    ) -> LangObj {
        LangObj::new(
            EntityType::Var(VarProperty::new(false, false, false)),
            pos,
            pkg,
            name,
            typ,
        )
    }

    pub fn new_param(
        pos: Pos,
        pkg: Option<PackageKey>,
        name: String,
        typ: Option<TypeKey>,
    ) -> LangObj {
        LangObj::new(
            EntityType::Var(VarProperty::new(false, false, true)),
            pos,
            pkg,
            name,
            typ,
        )
    }

    pub fn new_field(
        pos: Pos,
        pkg: Option<PackageKey>,
        name: String,
        typ: Option<TypeKey>,
        embedded: bool,
    ) -> LangObj {
        LangObj::new(
            EntityType::Var(VarProperty::new(embedded, true, false)),
            pos,
            pkg,
            name,
            typ,
        )
    }

    pub fn new_func(
        pos: Pos,
        pkg: Option<PackageKey>,
        name: String,
        typ: Option<TypeKey>,
    ) -> LangObj {
        LangObj::new(
            EntityType::Func { has_ptr_recv: false },
            pos,
            pkg,
            name,
            typ,
        )
    }

    pub fn new_label(pos: Pos, pkg: Option<PackageKey>, name: String, univ: &Universe) -> LangObj {
        let t = univ.types()[&BasicType::Invalid];
        LangObj::new(EntityType::Label { used: false }, pos, pkg, name, Some(t))
    }

    pub fn new_builtin(builtin: Builtin, typ: TypeKey) -> LangObj {
        LangObj::new(
            EntityType::Builtin(builtin),
            0,
            None,
            builtin.name().to_string(),
            Some(typ),
        )
    }

    pub fn new_nil(typ: TypeKey) -> LangObj {
        LangObj::new(EntityType::Nil, 0, None, "nil".to_string(), Some(typ))
    }

    // Getters
    pub fn entity_type(&self) -> &EntityType {
        &self.entity_type
    }

    pub fn entity_type_mut(&mut self) -> &mut EntityType {
        &mut self.entity_type
    }

    pub fn parent(&self) -> Option<ScopeKey> {
        self.parent
    }

    pub fn pos(&self) -> Pos {
        self.pos
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn typ(&self) -> Option<TypeKey> {
        self.typ
    }

    pub fn pkg(&self) -> Option<PackageKey> {
        self.pkg
    }

    pub fn order(&self) -> u32 {
        self.order
    }

    pub(crate) fn color(&self) -> ObjColor {
        self.color
    }

    pub fn scope_pos(&self) -> Pos {
        self.scope_pos
    }

    // Setters
    pub(crate) fn set_type(&mut self, typ: Option<TypeKey>) {
        self.typ = typ;
    }

    pub(crate) fn set_pkg(&mut self, pkg: Option<PackageKey>) {
        self.pkg = pkg;
    }

    pub(crate) fn set_parent(&mut self, parent: Option<ScopeKey>) {
        self.parent = parent;
    }

    pub fn set_order(&mut self, order: u32) {
        self.order = order;
    }

    pub(crate) fn set_color(&mut self, color: ObjColor) {
        self.color = color;
    }

    pub(crate) fn set_scope_pos(&mut self, pos: Pos) {
        self.scope_pos = pos;
    }

    /// Returns true if the name is exported (starts with uppercase).
    pub fn exported(&self) -> bool {
        self.name
            .chars()
            .next()
            .map_or(false, |c| c.is_uppercase())
    }

    /// Returns the unique identifier for this object.
    pub fn id(&self, objs: &TCObjects) -> Cow<'_, str> {
        if self.exported() {
            Cow::Borrowed(&self.name)
        } else {
            let path = self
                .pkg
                .map(|pk| objs.pkgs[pk].path())
                .unwrap_or("_");
            Cow::Owned(format!("{}.{}", path, self.name))
        }
    }

    /// Returns true if this object has the same identity as (pkg, name).
    pub fn same_id(&self, pkg: Option<PackageKey>, name: &str, objs: &TCObjects) -> bool {
        if name != self.name {
            return false;
        }
        if self.exported() {
            return true;
        }
        match (pkg, self.pkg) {
            (Some(a), Some(b)) => objs.pkgs[a].path() == objs.pkgs[b].path(),
            (None, None) => true,
            _ => false,
        }
    }

    // Entity-specific accessors
    pub fn pkg_name_imported(&self) -> PackageKey {
        match &self.entity_type {
            EntityType::PkgName { imported, .. } => *imported,
            _ => panic!("not a pkg name"),
        }
    }

    pub fn const_val(&self) -> &ConstValue {
        match &self.entity_type {
            EntityType::Const { val } => val,
            _ => panic!("not a const"),
        }
    }

    pub(crate) fn set_const_val(&mut self, v: ConstValue) {
        if let EntityType::Const { val } = &mut self.entity_type {
            *val = v;
        }
    }

    pub fn var_embedded(&self) -> bool {
        match &self.entity_type {
            EntityType::Var(prop) => prop.embedded,
            _ => false,
        }
    }

    pub fn var_is_field(&self) -> bool {
        match &self.entity_type {
            EntityType::Var(prop) => prop.is_field,
            _ => false,
        }
    }

    pub fn var_used(&self) -> Option<bool> {
        match &self.entity_type {
            EntityType::Var(prop) => Some(prop.used),
            _ => None,
        }
    }

    pub fn set_var_used(&mut self, used: bool) {
        if let EntityType::Var(prop) = &mut self.entity_type {
            prop.used = used;
        }
    }

    pub fn func_fmt_name(&self, f: &mut std::fmt::Formatter<'_>, objs: &TCObjects) -> std::fmt::Result {
        match &self.entity_type {
            EntityType::Func { .. } => fmt_func_name(self, f, objs),
            _ => panic!("not a func"),
        }
    }
}

// ----------------------------------------------------------------------------
// ObjSet

use std::collections::HashMap;

/// An ObjSet is a set of objects identified by their unique id.
pub struct ObjSet(HashMap<String, ObjKey>);

impl ObjSet {
    pub fn new() -> ObjSet {
        ObjSet(HashMap::new())
    }

    /// Inserts an object into the set. Returns the existing object if already present.
    pub fn insert(&mut self, okey: ObjKey, objs: &TCObjects) -> Option<ObjKey> {
        let obj = &objs.lobjs[okey];
        let id = obj.id(objs).to_string();
        if let Some(&existing) = self.0.get(&id) {
            Some(existing)
        } else {
            self.0.insert(id, okey);
            None
        }
    }
}

// ----------------------------------------------------------------------------
// Formatting

use std::fmt::{self, Write};

pub fn fmt_obj(okey: ObjKey, f: &mut fmt::Formatter<'_>, objs: &TCObjects) -> fmt::Result {
    let obj = &objs.lobjs[okey];
    match obj.entity_type() {
        EntityType::PkgName { imported, .. } => {
            write!(f, "package {}", obj.name())?;
            let path = objs.pkgs[*imported].path();
            if path != obj.name() {
                write!(f, " ('{}')", path)?;
            }
        }
        EntityType::Const { .. } => {
            f.write_str("const")?;
            fmt_obj_name(okey, f, objs)?;
            fmt_obj_type(okey, f, objs)?;
        }
        EntityType::TypeName => {
            f.write_str("type")?;
            fmt_obj_name(okey, f, objs)?;
            fmt_obj_type(okey, f, objs)?;
        }
        EntityType::Var(prop) => {
            f.write_str(if prop.is_field { "field" } else { "var" })?;
            fmt_obj_name(okey, f, objs)?;
            fmt_obj_type(okey, f, objs)?;
        }
        EntityType::Func { .. } => {
            f.write_str("func ")?;
            fmt_func_name(obj, f, objs)?;
            if let Some(t) = obj.typ() {
                typ::fmt_signature(t, f, objs)?;
            }
        }
        EntityType::Label { .. } => {
            f.write_str("label")?;
            fmt_obj_name(okey, f, objs)?;
        }
        EntityType::Builtin(_) => {
            f.write_str("builtin")?;
            fmt_obj_name(okey, f, objs)?;
        }
        EntityType::Nil => f.write_str("nil")?,
    }
    Ok(())
}

fn fmt_obj_name(okey: ObjKey, f: &mut fmt::Formatter<'_>, objs: &TCObjects) -> fmt::Result {
    f.write_char(' ')?;
    let obj = &objs.lobjs[okey];
    if let Some(p) = obj.pkg {
        let pkg_val = &objs.pkgs[p];
        if let Some(k) = objs.scopes[*pkg_val.scope()].lookup(obj.name()) {
            if k == okey {
                pkg_val.fmt_with_qualifier(f, Some(&*objs.fmt_qualifier))?;
            }
        }
    }
    f.write_str(obj.name())
}

fn fmt_obj_type(okey: ObjKey, f: &mut fmt::Formatter<'_>, objs: &TCObjects) -> fmt::Result {
    let obj = &objs.lobjs[okey];
    if obj.typ().is_none() {
        return Ok(());
    }
    let mut obj_typ = obj.typ().unwrap();
    if obj.entity_type().is_type_name() {
        let typ_val = &objs.types[obj.typ().unwrap()];
        if typ_val.try_as_basic().is_some() {
            return Ok(());
        }
        if type_name_is_alias(okey, objs) {
            f.write_str(" =")?;
        } else {
            obj_typ = typ::underlying_type(obj_typ, objs);
        }
    }
    f.write_char(' ')?;
    typ::fmt_type(Some(obj_typ), f, objs)
}

fn fmt_func_name(func: &LangObj, f: &mut fmt::Formatter<'_>, objs: &TCObjects) -> fmt::Result {
    if let Some(t) = func.typ() {
        let sig = objs.types[t].try_as_signature().unwrap();
        if let Some(r) = sig.recv() {
            f.write_char('(')?;
            typ::fmt_type(objs.lobjs[*r].typ(), f, objs)?;
            f.write_str(").")?;
        } else if let Some(p) = func.pkg() {
            objs.pkgs[p].fmt_with_qualifier(f, Some(&*objs.fmt_qualifier))?;
        }
    }
    f.write_str(func.name())
}

pub fn type_name_is_alias(okey: ObjKey, objs: &TCObjects) -> bool {
    let obj = &objs.lobjs[okey];
    match obj.typ() {
        Some(t) => {
            let typ = &objs.types[t];
            match typ {
                typ::Type::Basic(detail) => {
                    let univ = objs.universe();
                    // Any user-defined type name for a basic type is an alias for a
                    // basic type (because basic types are pre-declared in the Universe
                    // scope, outside any package scope), and so is any type name with
                    // a different name than the name of the basic type it refers to.
                    // Additionally, we need to look for "byte" and "rune" because they
                    // are aliases but have the same names (for better error messages).
                    obj.pkg().is_some()
                        || detail.name() != obj.name()
                        || t == univ.byte()
                        || t == univ.rune()
                }
                typ::Type::Named(detail) => *detail.obj() != Some(okey),
                _ => true,
            }
        }
        None => false,
    }
}
