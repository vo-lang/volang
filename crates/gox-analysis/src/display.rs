//! Formatting and display utilities.
//!
//! This module provides display/formatting traits for types, objects,
//! and other type-checker constructs.


use crate::obj::{fmt_obj, Pos};
use crate::objects::{ObjKey, ScopeKey, TCObjects, TypeKey};
use crate::scope::fmt_scope_full;
use crate::selection::Selection;
use crate::typ::fmt_type;
use std::fmt;

/// Formats a type as a string.
pub fn type_string(t: TypeKey, objs: &TCObjects) -> String {
    format!("{}", Displayer::new(&t, objs))
}

/// Formats an object as a string.
pub fn obj_string(o: ObjKey, objs: &TCObjects) -> String {
    format!("{}", Displayer::new(&o, objs))
}

/// A trait for types that can be formatted with TCObjects context.
pub trait Display {
    fn format(&self, f: &mut fmt::Formatter<'_>, objs: &TCObjects) -> fmt::Result;

    fn position(&self, _objs: &TCObjects) -> Pos {
        0
    }
}

/// A wrapper that provides Display implementation with TCObjects context.
pub struct Displayer<'a, T: Display> {
    obj: &'a T,
    objs: &'a TCObjects,
}

impl<'a, T: Display> Displayer<'a, T> {
    pub fn new(obj: &'a T, objs: &'a TCObjects) -> Self {
        Displayer { obj, objs }
    }

    pub fn pos(&self) -> Pos {
        self.obj.position(self.objs)
    }
}

impl<'a, T: Display> fmt::Display for Displayer<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.obj.format(f, self.objs)
    }
}

impl Display for ObjKey {
    fn format(&self, f: &mut fmt::Formatter<'_>, objs: &TCObjects) -> fmt::Result {
        fmt_obj(*self, f, objs)
    }

    fn position(&self, objs: &TCObjects) -> Pos {
        objs.lobjs[*self].pos()
    }
}

impl Display for TypeKey {
    fn format(&self, f: &mut fmt::Formatter<'_>, objs: &TCObjects) -> fmt::Result {
        fmt_type(Some(*self), f, objs)
    }
}

impl Display for ScopeKey {
    fn format(&self, f: &mut fmt::Formatter<'_>, objs: &TCObjects) -> fmt::Result {
        fmt_scope_full(*self, f, 0, objs)
    }
}

impl Display for Selection {
    fn format(&self, f: &mut fmt::Formatter<'_>, objs: &TCObjects) -> fmt::Result {
        self.fmt(f, objs)
    }
}

