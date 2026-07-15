//! Label checking.
//!
//! This module validates label declarations and usages for goto, break,
//! and continue statements.

use std::collections::HashMap;

use vo_common::span::Span;
use vo_syntax::ast::Ident;
use vo_syntax::ast::{Block, Stmt, StmtKind};

use super::checker::Checker;
use super::errors::TypeError;

/// A label block for tracking label scopes.
#[derive(Debug)]
struct LabelBlock {
    /// Parent block for nested scopes.
    parent: Option<Box<LabelBlock>>,
    /// Labels declared in this block.
    labels: HashMap<String, LabelInfo>,
}

/// Information about a declared label.
#[derive(Debug, Clone)]
struct LabelInfo {
    /// The span of the label declaration.
    span: Span,
    /// Whether the label has been used.
    used: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LabelTarget {
    Loop,
    Breakable,
    Other,
}

#[derive(Debug)]
struct ActiveLabel {
    name: String,
    target: LabelTarget,
}

impl LabelBlock {
    fn new(parent: Option<Box<LabelBlock>>) -> Self {
        LabelBlock {
            parent,
            labels: HashMap::new(),
        }
    }

    /// Declares a new label in this block.
    fn declare(&mut self, name: String, span: Span) -> bool {
        use std::collections::hash_map::Entry;
        if let Entry::Vacant(e) = self.labels.entry(name) {
            e.insert(LabelInfo { span, used: false });
            true
        } else {
            false
        }
    }

    /// Marks a label as used.
    fn mark_used(&mut self, name: &str) -> bool {
        if let Some(info) = self.labels.get_mut(name) {
            info.used = true;
            true
        } else if let Some(ref mut parent) = self.parent {
            parent.mark_used(name)
        } else {
            false
        }
    }

    fn contains(&self, name: &str) -> bool {
        self.labels.contains_key(name)
            || self
                .parent
                .as_ref()
                .is_some_and(|parent| parent.contains(name))
    }
}

impl Checker {
    /// Checks labels in a function body.
    pub(crate) fn check_labels(&mut self, body: &Block) {
        let mut block = LabelBlock::new(None);
        self.collect_labels(&body.stmts, &mut block);
        self.check_label_usages(&body.stmts, &mut block, &mut Vec::new());
        self.report_unused_labels(&block);
    }

    /// Collects all label declarations in a block.
    fn collect_labels(&mut self, stmts: &[Stmt], block: &mut LabelBlock) {
        for stmt in stmts {
            self.collect_labels_in_stmt(stmt, block);
        }
    }

    /// Collects labels in a single statement.
    fn collect_labels_in_stmt(&mut self, stmt: &Stmt, block: &mut LabelBlock) {
        match &stmt.kind {
            StmtKind::Labeled(labeled) => {
                let name = self.ident_name(&labeled.label);
                if !block.declare(name.clone(), labeled.label.span) {
                    self.error_code_msg(
                        TypeError::Redeclared,
                        labeled.label.span,
                        format!("label {} already declared", name),
                    );
                }
                self.collect_labels_in_stmt(&labeled.stmt, block);
            }
            StmtKind::Block(b) => self.collect_labels(&b.stmts, block),
            StmtKind::If(if_stmt) => {
                self.collect_labels(&if_stmt.then.stmts, block);
                if let Some(else_) = &if_stmt.else_ {
                    self.collect_labels_in_stmt(else_, block);
                }
            }
            StmtKind::For(for_stmt) => {
                self.collect_labels(&for_stmt.body.stmts, block);
            }
            StmtKind::Switch(sw) => {
                for case in &sw.cases {
                    self.collect_labels(&case.body, block);
                }
            }
            StmtKind::TypeSwitch(ts) => {
                for case in &ts.cases {
                    self.collect_labels(&case.body, block);
                }
            }
            StmtKind::Select(sel) => {
                for case in &sel.cases {
                    self.collect_labels(&case.body, block);
                }
            }
            _ => {}
        }
    }

    /// Checks label usages (goto, break, continue with labels).
    fn check_label_usages(
        &mut self,
        stmts: &[Stmt],
        block: &mut LabelBlock,
        active: &mut Vec<ActiveLabel>,
    ) {
        for stmt in stmts {
            self.check_label_usage_in_stmt(stmt, block, active);
        }
    }

    /// Checks label usage in a single statement.
    fn check_label_usage_in_stmt(
        &mut self,
        stmt: &Stmt,
        block: &mut LabelBlock,
        active: &mut Vec<ActiveLabel>,
    ) {
        match &stmt.kind {
            StmtKind::Goto(goto) => {
                let name = self.ident_name(&goto.label);
                if block.mark_used(&name) {
                    // Label found and marked as used
                } else {
                    self.error_code_msg(
                        TypeError::LabelNotDeclared,
                        goto.label.span,
                        format!("label {} not declared", name),
                    );
                }
            }
            StmtKind::Break(brk) => {
                if let Some(label) = &brk.label {
                    let name = self.ident_name(label);
                    if !block.contains(&name) {
                        self.error_code_msg(
                            TypeError::LabelNotDeclared,
                            label.span,
                            format!("label {} not declared", name),
                        );
                    } else {
                        block.mark_used(&name);
                        let target = active
                            .iter()
                            .rev()
                            .find(|candidate| candidate.name == name)
                            .map(|candidate| candidate.target);
                        if !matches!(target, Some(LabelTarget::Loop | LabelTarget::Breakable)) {
                            self.error_code_msg(
                                TypeError::InvalidBreak,
                                label.span,
                                format!(
                                    "break label {} must refer to an enclosing for, switch, or select statement",
                                    name
                                ),
                            );
                        }
                    }
                }
            }
            StmtKind::Continue(cont) => {
                if let Some(label) = &cont.label {
                    let name = self.ident_name(label);
                    if !block.contains(&name) {
                        self.error_code_msg(
                            TypeError::LabelNotDeclared,
                            label.span,
                            format!("label {} not declared", name),
                        );
                    } else {
                        block.mark_used(&name);
                        let target = active
                            .iter()
                            .rev()
                            .find(|candidate| candidate.name == name)
                            .map(|candidate| candidate.target);
                        if target != Some(LabelTarget::Loop) {
                            self.error_code_msg(
                                TypeError::InvalidContinue,
                                label.span,
                                format!(
                                    "continue label {} must refer to an enclosing for statement",
                                    name
                                ),
                            );
                        }
                    }
                }
            }
            StmtKind::Labeled(labeled) => {
                active.push(ActiveLabel {
                    name: self.ident_name(&labeled.label),
                    target: Self::label_target(&labeled.stmt),
                });
                self.check_label_usage_in_stmt(&labeled.stmt, block, active);
                active.pop();
            }
            StmtKind::Block(b) => {
                self.check_label_usages(&b.stmts, block, active);
            }
            StmtKind::If(if_stmt) => {
                self.check_label_usages(&if_stmt.then.stmts, block, active);
                if let Some(else_) = &if_stmt.else_ {
                    self.check_label_usage_in_stmt(else_, block, active);
                }
            }
            StmtKind::For(for_stmt) => {
                self.check_label_usages(&for_stmt.body.stmts, block, active);
            }
            StmtKind::Switch(sw) => {
                for case in &sw.cases {
                    self.check_label_usages(&case.body, block, active);
                }
            }
            StmtKind::TypeSwitch(ts) => {
                for case in &ts.cases {
                    self.check_label_usages(&case.body, block, active);
                }
            }
            StmtKind::Select(sel) => {
                for case in &sel.cases {
                    self.check_label_usages(&case.body, block, active);
                }
            }
            _ => {}
        }
    }

    fn label_target(stmt: &Stmt) -> LabelTarget {
        match &stmt.kind {
            StmtKind::For(_) => LabelTarget::Loop,
            StmtKind::Switch(_) | StmtKind::TypeSwitch(_) | StmtKind::Select(_) => {
                LabelTarget::Breakable
            }
            _ => LabelTarget::Other,
        }
    }

    /// Reports unused labels.
    fn report_unused_labels(&mut self, block: &LabelBlock) {
        let mut labels: Vec<_> = block.labels.iter().collect();
        labels.sort_unstable_by(|(left_name, left_info), (right_name, right_info)| {
            left_info
                .span
                .start
                .cmp(&right_info.span.start)
                .then_with(|| left_info.span.end.cmp(&right_info.span.end))
                .then_with(|| left_name.cmp(right_name))
        });
        for (name, info) in labels {
            if !info.used {
                self.emit(
                    TypeError::UnusedLabel.at_with_message(
                        info.span,
                        format!("label {} declared but not used", name),
                    ),
                );
            }
        }
    }

    /// Gets the name of an identifier.
    fn ident_name(&self, ident: &Ident) -> String {
        self.resolve_symbol(ident.symbol).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arena::ArenaKey;
    use crate::importer::NullImporter;
    use crate::objects::PackageKey;
    use std::path::PathBuf;
    use vo_syntax::parser;

    #[test]
    fn unused_label_diagnostics_follow_declaration_spans() {
        let source = r#"
package main
func run() {
Zulu:  _ = 1
Alpha: _ = 2
Beta:  _ = 3
}
"#;
        for _ in 0..16 {
            let (file, parse_diagnostics, interner) = parser::parse(source, 0);
            assert!(
                !parse_diagnostics.has_errors(),
                "parse diagnostics: {parse_diagnostics:?}"
            );
            let mut checker = Checker::new_with_trace(PackageKey::null(), interner, false);
            let package = checker
                .tc_objs
                .new_package("main".to_string(), "main".to_string());
            checker.pkg = package;
            let mut importer = NullImporter::new(PathBuf::from("."));
            assert!(checker
                .check_with_importer(std::slice::from_ref(&file), &mut importer)
                .is_ok());

            let diagnostics = checker.diagnostics.borrow();
            let unused: Vec<_> = diagnostics
                .iter()
                .filter(|diagnostic| diagnostic.code == Some(TypeError::UnusedLabel.code()))
                .collect();
            assert_eq!(
                unused
                    .iter()
                    .map(|diagnostic| diagnostic.message.as_str())
                    .collect::<Vec<_>>(),
                [
                    "label Zulu declared but not used",
                    "label Alpha declared but not used",
                    "label Beta declared but not used",
                ]
            );
            assert!(unused
                .windows(2)
                .all(|pair| pair[0].labels[0].span.start < pair[1].labels[0].span.start));
        }
    }
}
