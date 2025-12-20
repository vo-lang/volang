//! Package representation for type checking.

#![allow(dead_code)]

use crate::objects::{PackageKey, ScopeKey};
use std::borrow::Cow;
use std::fmt;

/// A Package describes a Go package.
#[derive(Debug)]
pub struct Package {
    path: String,
    name: Option<String>,
    scope: ScopeKey,
    complete: bool,
    imports: Vec<PackageKey>,
    fake: bool,
}

impl Package {
    pub fn new(path: String, name: Option<String>, scope: ScopeKey) -> Package {
        Package {
            path,
            name,
            scope,
            complete: false,
            imports: Vec::new(),
            fake: false,
        }
    }

    pub fn path(&self) -> &str {
        &self.path
    }

    pub fn name(&self) -> &Option<String> {
        &self.name
    }

    pub fn set_name(&mut self, name: String) {
        self.name = Some(name);
    }

    pub fn scope(&self) -> &ScopeKey {
        &self.scope
    }

    pub fn complete(&self) -> bool {
        self.complete
    }

    pub fn mark_complete(&mut self) {
        self.complete = true;
    }

    pub fn fake(&self) -> bool {
        self.fake
    }

    pub fn mark_fake_with_name(&mut self, name: String) {
        self.fake = true;
        self.name = Some(name);
    }

    pub fn imports(&self) -> &Vec<PackageKey> {
        &self.imports
    }

    pub fn imports_mut(&mut self) -> &mut Vec<PackageKey> {
        &mut self.imports
    }

    pub fn add_import(&mut self, pkey: PackageKey) {
        self.imports.push(pkey);
    }

    pub fn set_imports(&mut self, pkgs: Vec<PackageKey>) {
        self.imports = pkgs;
    }

    pub fn fmt_with_qualifier(
        &self,
        f: &mut fmt::Formatter<'_>,
        qf: Option<&dyn Fn(&Package) -> Cow<str>>,
    ) -> fmt::Result {
        if let Some(qualifier) = qf {
            write!(f, "{}.", qualifier(self))
        } else {
            Ok(())
        }
    }
}

impl fmt::Display for Package {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.name {
            None => write!(f, "uninitialized package, path: {}", &self.path),
            Some(name) => write!(f, "package {} ({})", name, &self.path),
        }
    }
}
