use super::EditorPackage;
use crate::check::{TypeAndValue, TypeInfo};
use crate::objects::{ObjKey, TCObjects};
use std::collections::HashMap;
use vo_common::span::{BytePos, Span};
use vo_syntax::ast::{self, Expr, ExprKind, Ident, TypeExpr, TypeExprKind, Visitor};

pub(super) struct Member {
    pub span: Span,
    pub receiver_object: Option<ObjKey>,
    pub receiver_type: Option<TypeAndValue>,
    pub types_only: bool,
}

#[derive(Default)]
pub(super) struct Index {
    pub definitions: HashMap<ObjKey, Span>,
    occurrences: Vec<(Span, ObjKey)>,
    members: Vec<Member>,
}

impl Index {
    pub fn build(packages: &[EditorPackage], objects: &TCObjects) -> Self {
        let mut index = Self::default();
        for package in packages {
            let Some(facts) = &package.facts else {
                continue;
            };
            // Unaliased imports have an implicit package-name binding whose
            // declaration is the import, without an identifier definition node.
            for (span, object) in &facts.implicits {
                if objects.lobjs[*object].entity_type().is_pkg_name() {
                    index.definitions.insert(*object, *span);
                }
            }
            let mut collector = Collector {
                facts,
                index: &mut index,
            };
            for file in &package.files {
                collector.visit_file(file);
            }
        }
        index
            .occurrences
            .sort_by_key(|(span, _)| (span.start, span.end));
        index
            .members
            .sort_by_key(|member| (member.span.start, member.span.end));
        index
    }

    pub fn object_at(&self, point: BytePos) -> Option<ObjKey> {
        // Prefer the token starting at the cursor, then allow its end position.
        self.occurrences
            .iter()
            .find(|(span, _)| span.start <= point && point < span.end)
            .or_else(|| {
                self.occurrences
                    .iter()
                    .find(|(span, _)| span.start < point && span.end == point)
            })
            .map(|(_, object)| *object)
    }

    pub fn member_at(&self, point: BytePos) -> Option<&Member> {
        self.members
            .iter()
            .find(|member| member.span.start <= point && point <= member.span.end)
    }
}

struct Collector<'a> {
    facts: &'a TypeInfo,
    index: &'a mut Index,
}
impl Visitor for Collector<'_> {
    fn visit_ident(&mut self, ident: &Ident) {
        if ident.span.is_empty() || ident.span.is_dummy() {
            return;
        }
        if let Some(object) = self.facts.get_def(ident) {
            self.index.definitions.insert(object, ident.span);
            if self.facts.get_use(ident).is_none() {
                self.index.occurrences.push((ident.span, object));
            }
        }
        if let Some(object) = self.facts.get_use(ident) {
            self.index.occurrences.push((ident.span, object));
        }
    }
    fn visit_expr(&mut self, expr: &Expr) {
        if let ExprKind::Selector(selector) = &expr.kind {
            if let Some(selection) = self.facts.selections.get(&expr.id) {
                self.index
                    .occurrences
                    .push((selector.sel.span, selection.obj()));
            }
            let receiver_object = if let ExprKind::Ident(ident) = &selector.expr.kind {
                self.facts.get_use(ident)
            } else {
                None
            };
            self.index.members.push(Member {
                span: selector.sel.span,
                receiver_object,
                receiver_type: self.facts.types.get(&selector.expr.id).cloned(),
                types_only: false,
            });
        }
        ast::walk_expr(self, expr);
    }

    fn visit_type_expr(&mut self, expr: &TypeExpr) {
        if let TypeExprKind::Selector(selector) = &expr.kind {
            self.index.members.push(Member {
                span: selector.sel.span,
                receiver_object: self.facts.get_use(&selector.pkg),
                receiver_type: None,
                types_only: true,
            });
        }
        ast::walk_type_expr(self, expr);
    }
}
