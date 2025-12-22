//! Scope management for type checking.
//!
//! A Scope maintains the set of named language entities declared in the scope
//! and a link to the immediately surrounding (outer) scope.


use crate::obj::Pos;
use crate::objects::{ObjKey, ScopeKey, TCObjects};
use std::collections::HashMap;

/// A Scope maintains the set of named language entities declared in the scope.
#[derive(Debug)]
pub struct Scope {
    parent: Option<ScopeKey>,
    children: Vec<ScopeKey>,
    elems: HashMap<String, ObjKey>,
    pos: Pos,
    end: Pos,
    comment: String,
    is_func: bool,
}

impl Scope {
    /// Creates a new empty scope.
    pub fn new(parent: Option<ScopeKey>, pos: Pos, end: Pos, comment: &str) -> Scope {
        Scope {
            parent,
            children: Vec::new(),
            elems: HashMap::new(),
            pos,
            end,
            comment: comment.to_string(),
            is_func: false,
        }
    }

    /// Creates a new function scope.
    pub fn new_func(parent: Option<ScopeKey>, pos: Pos, end: Pos) -> Scope {
        let mut scope = Scope::new(parent, pos, end, "function");
        scope.is_func = true;
        scope
    }

    pub fn parent(&self) -> Option<ScopeKey> {
        self.parent
    }

    pub fn children(&self) -> &[ScopeKey] {
        &self.children
    }

    pub fn pos(&self) -> Pos {
        self.pos
    }

    pub fn set_pos(&mut self, pos: Pos) {
        self.pos = pos;
    }

    pub fn end(&self) -> Pos {
        self.end
    }

    pub fn comment(&self) -> &str {
        &self.comment
    }

    pub fn is_func(&self) -> bool {
        self.is_func
    }

    pub fn len(&self) -> usize {
        self.elems.len()
    }

    pub fn is_empty(&self) -> bool {
        self.elems.is_empty()
    }

    /// Adds a child scope.
    pub fn add_child(&mut self, child: ScopeKey) {
        self.children.push(child);
    }

    /// Looks up a name in this scope only (not parent scopes).
    pub fn lookup(&self, name: &str) -> Option<ObjKey> {
        self.elems.get(name).copied()
    }

    /// Looks up a name in this scope and all parent scopes.
    pub fn lookup_parent(&self, name: &str, objs: &TCObjects) -> Option<(ScopeKey, ObjKey)> {
        // First check this scope
        if let Some(_obj) = self.elems.get(name) {
            // We need the scope key for this scope, but we don't have it here
            // This is a limitation - caller should use lookup_parent_with_key
            return None;
        }
        // Check parent scopes
        if let Some(parent_key) = self.parent {
            let parent = &objs.scopes[parent_key];
            if let Some(obj) = parent.lookup(name) {
                return Some((parent_key, obj));
            }
            return parent.lookup_parent(name, objs);
        }
        None
    }

    /// Returns an iterator over all names in the scope.
    pub fn names(&self) -> impl Iterator<Item = &String> {
        self.elems.keys()
    }

    /// Returns an iterator over all objects in the scope.
    pub fn objects(&self) -> impl Iterator<Item = ObjKey> + '_ {
        self.elems.values().copied()
    }

    /// Returns a reference to the elements map.
    pub fn elems(&self) -> &std::collections::HashMap<String, ObjKey> {
        &self.elems
    }

    /// Sets the end position.
    pub fn set_end(&mut self, end: Pos) {
        self.end = end;
    }

    /// Returns true if pos is within the scope [pos, end].
    pub fn contains(&self, pos: Pos) -> bool {
        self.pos <= pos && pos <= self.end
    }

    /// insert attempts to insert an object into scope.
    /// If the scope already contains an object with the same name,
    /// insert leaves the scope unchanged and returns the existing object.
    /// Otherwise it inserts the object, sets the object's parent scope
    /// if not already set, and returns None.
    pub fn insert(self_key: ScopeKey, okey: ObjKey, objs: &mut TCObjects) -> Option<ObjKey> {
        let lang_obj = &objs.lobjs[okey];
        let name = lang_obj.name().to_string();
        
        let scope = &objs.scopes[self_key];
        if let Some(&existing) = scope.elems.get(&name) {
            return Some(existing);
        }
        
        // Insert into scope
        objs.scopes[self_key].elems.insert(name, okey);
        
        // Set parent if not already set
        if objs.lobjs[okey].parent().is_none() {
            objs.lobjs[okey].set_parent(Some(self_key));
        }
        
        None
    }
}

/// Looks up a name starting from a given scope, traversing parent scopes.
pub fn lookup_parent(
    start: ScopeKey,
    name: &str,
    objs: &TCObjects,
) -> Option<(ScopeKey, ObjKey)> {
    let mut current = Some(start);
    while let Some(scope_key) = current {
        let scope = &objs.scopes[scope_key];
        if let Some(obj) = scope.lookup(name) {
            return Some((scope_key, obj));
        }
        current = scope.parent();
    }
    None
}

/// Looks up a name starting from a given scope, with position-aware visibility.
pub fn lookup_parent_at(
    start: ScopeKey,
    name: &str,
    pos: Pos,
    objs: &TCObjects,
) -> Option<(ScopeKey, ObjKey)> {
    let mut current = Some(start);
    while let Some(scope_key) = current {
        let scope = &objs.scopes[scope_key];
        if let Some(obj_key) = scope.lookup(name) {
            let obj = &objs.lobjs[obj_key];
            // Check if the object is visible at this position
            if obj.scope_pos() <= pos {
                return Some((scope_key, obj_key));
            }
        }
        current = scope.parent();
    }
    None
}

// ----------------------------------------------------------------------------
// Formatting

use std::fmt;

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_scope(self, f, 0)
    }
}

fn fmt_scope(scope: &Scope, f: &mut fmt::Formatter<'_>, n: usize) -> fmt::Result {
    let ind = ".  ";
    let indn = ind.repeat(n);
    writeln!(f, "{}{} scope", indn, scope.comment)?;
    let indn1 = ind.repeat(n + 1);
    let mut names: Vec<_> = scope.elems.keys().collect();
    names.sort();
    for name in names {
        writeln!(f, "{}{}", indn1, name)?;
    }
    Ok(())
}

/// Formats the scope including its children recursively.
pub fn fmt_scope_full(
    skey: ScopeKey,
    f: &mut fmt::Formatter<'_>,
    n: usize,
    objs: &TCObjects,
) -> fmt::Result {
    let ind = ".  ";
    let indn = ind.repeat(n);
    let scope = &objs.scopes[skey];
    writeln!(f, "{}{} scope {:?}", indn, scope.comment, skey)?;
    fmt_scope(scope, f, n)?;
    for child in scope.children.iter() {
        fmt_scope_full(*child, f, n + 1, objs)?;
    }
    Ok(())
}
