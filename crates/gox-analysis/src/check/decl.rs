//! Declaration type checking.
//!
//! This module type-checks top-level declarations including:
//! - Variable declarations (var)
//! - Constant declarations (const)
//! - Type declarations (type)
//! - Function declarations (func)
//! - Interface declarations (interface)

#![allow(dead_code)]

use gox_common::vfs::FileSystem;
use gox_syntax::ast::{Decl, File, FuncDecl};

use crate::obj::{EntityType, LangObj};
use crate::objects::{ObjKey, ScopeKey};
use crate::scope::Scope;

use super::checker::Checker;

impl<F: FileSystem> Checker<F> {
    /// Type-checks an object's declaration to ensure its type is fully set up.
    /// This is called before comparing method signatures to ensure they are complete.
    pub fn obj_decl(&mut self, okey: ObjKey) {
        let lobj = &self.tc_objs.lobjs[okey];
        // If the object already has a type, it's fully checked
        if lobj.typ().is_some() {
            return;
        }
        // TODO: Full implementation requires DeclInfo and color-based cycle detection.
        // For now, this is a no-op - methods should have their types set during
        // the normal type-checking pass before assignable_to is called.
    }

    /// Declares an object in a scope.
    /// Returns error if name already exists (except for blank identifier "_").
    pub fn declare(&mut self, scope_key: ScopeKey, obj_key: ObjKey) {
        let name = self.tc_objs.lobjs[obj_key].name().to_string();
        
        // Blank identifier doesn't introduce a binding
        if name == "_" {
            return;
        }
        
        // Try to insert into scope using static method
        if let Some(existing) = Scope::insert(scope_key, obj_key, &mut self.tc_objs) {
            // Name already declared - report error
            let _ = existing;
            self.error(
                gox_common::span::Span::default(),
                format!("{} redeclared in this block", name),
            );
        }
    }

    /// Type-checks a source file.
    pub fn check_file(&mut self, file: &File) {
        // First pass: collect all declarations
        for decl in &file.decls {
            self.collect_decl(decl);
        }

        // Second pass: type-check all declarations
        for decl in &file.decls {
            self.check_decl(decl);
        }
    }

    /// Collects a declaration (first pass - creates objects).
    fn collect_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Var(var) => self.collect_var_decl(var),
            Decl::Const(cons) => self.collect_const_decl(cons),
            Decl::Type(ty) => self.collect_type_decl(ty),
            Decl::Func(func) => self.collect_func_decl(func),
            Decl::Interface(iface) => self.collect_interface_decl(iface),
        }
    }

    /// Type-checks a declaration (second pass).
    fn check_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Var(var) => self.check_var_decl(var),
            Decl::Const(cons) => self.check_const_decl(cons),
            Decl::Type(ty) => self.check_type_decl(ty),
            Decl::Func(func) => self.check_func_decl(func),
            Decl::Interface(iface) => self.check_interface_decl(iface),
        }
    }

    // ========== Collection (first pass) ==========

    fn collect_var_decl(&mut self, _var: &gox_syntax::ast::VarDecl) {
        // TODO: Create variable objects
    }

    fn collect_const_decl(&mut self, _cons: &gox_syntax::ast::ConstDecl) {
        // TODO: Create constant objects
    }

    fn collect_type_decl(&mut self, _ty: &gox_syntax::ast::TypeDecl) {
        // TODO: Create type objects
    }

    fn collect_func_decl(&mut self, _func: &FuncDecl) {
        // TODO: Create function objects
    }

    fn collect_interface_decl(&mut self, _iface: &gox_syntax::ast::InterfaceDecl) {
        // TODO: Create interface objects
    }

    // ========== Type checking (second pass) ==========

    fn check_var_decl(&mut self, var: &gox_syntax::ast::VarDecl) {
        for spec in &var.specs {
            if let Some(ty) = &spec.ty {
                let _typ = self.resolve_type(ty);
            }
            for val in &spec.values {
                self.check_expr(val);
            }
        }
    }

    fn check_const_decl(&mut self, cons: &gox_syntax::ast::ConstDecl) {
        for spec in &cons.specs {
            if let Some(ty) = &spec.ty {
                let _typ = self.resolve_type(ty);
            }
            for val in &spec.values {
                self.check_expr(val);
            }
        }
    }

    fn check_type_decl(&mut self, ty: &gox_syntax::ast::TypeDecl) {
        let _typ = self.resolve_type(&ty.ty);
    }

    fn check_func_decl(&mut self, func: &FuncDecl) {
        // Check receiver type
        if let Some(recv) = &func.receiver {
            // TODO: Resolve receiver type
            let _ = recv;
        }

        // Check parameter types
        for param in &func.sig.params {
            let _typ = self.resolve_type(&param.ty);
        }

        // Check result types
        for result in &func.sig.results {
            let _typ = self.resolve_type(&result.ty);
        }

        // Check function body if present
        if let Some(body) = &func.body {
            self.check_block(body);
        }
    }

    fn check_interface_decl(&mut self, iface: &gox_syntax::ast::InterfaceDecl) {
        for elem in &iface.elems {
            match elem {
                gox_syntax::ast::InterfaceElem::Method(method) => {
                    for param in &method.sig.params {
                        let _typ = self.resolve_type(&param.ty);
                    }
                    for result in &method.sig.results {
                        let _typ = self.resolve_type(&result.ty);
                    }
                }
                gox_syntax::ast::InterfaceElem::Embedded(_) => {
                    // TODO: Resolve embedded interface
                }
            }
        }
    }
}
