//! Label checking.
//!
//! This module validates label declarations and usages for goto, break,
//! and continue statements.

#![allow(dead_code)]

use std::collections::HashMap;

use gox_common::span::Span;
use gox_common::symbol::Ident;
use gox_common::vfs::FileSystem;
use gox_syntax::ast::{Block, Stmt, StmtKind};

use super::checker::Checker;

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

impl LabelBlock {
    fn new(parent: Option<Box<LabelBlock>>) -> Self {
        LabelBlock {
            parent,
            labels: HashMap::new(),
        }
    }

    /// Declares a new label in this block.
    fn declare(&mut self, name: String, span: Span) -> bool {
        if self.labels.contains_key(&name) {
            false // Already declared
        } else {
            self.labels.insert(name, LabelInfo { span, used: false });
            true
        }
    }

    /// Looks up a label in this block and parent blocks.
    fn lookup(&self, name: &str) -> Option<&LabelInfo> {
        if let Some(info) = self.labels.get(name) {
            Some(info)
        } else if let Some(ref parent) = self.parent {
            parent.lookup(name)
        } else {
            None
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
}

impl<F: FileSystem> Checker<F> {
    /// Checks labels in a function body.
    pub fn check_labels(&mut self, body: &Block) {
        let mut block = LabelBlock::new(None);
        self.collect_labels(&body.stmts, &mut block);
        self.check_label_usages(&body.stmts, &mut block);
        self.report_unused_labels(&block);
    }

    /// Collects all label declarations in a block.
    fn collect_labels(&mut self, stmts: &[Stmt], block: &mut LabelBlock) {
        for stmt in stmts {
            if let StmtKind::Labeled(labeled) = &stmt.kind {
                let name = self.ident_name(&labeled.label);
                if !block.declare(name.clone(), labeled.label.span) {
                    self.error(
                        labeled.label.span,
                        format!("label {} already declared", name),
                    );
                }
                // Recursively collect in the labeled statement
                self.collect_labels_in_stmt(&labeled.stmt, block);
            } else {
                self.collect_labels_in_stmt(stmt, block);
            }
        }
    }

    /// Collects labels in a single statement.
    fn collect_labels_in_stmt(&mut self, stmt: &Stmt, block: &mut LabelBlock) {
        match &stmt.kind {
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
    fn check_label_usages(&mut self, stmts: &[Stmt], block: &mut LabelBlock) {
        for stmt in stmts {
            self.check_label_usage_in_stmt(stmt, block);
        }
    }

    /// Checks label usage in a single statement.
    fn check_label_usage_in_stmt(&mut self, stmt: &Stmt, block: &mut LabelBlock) {
        match &stmt.kind {
            StmtKind::Goto(goto) => {
                let name = self.ident_name(&goto.label);
                if block.mark_used(&name) {
                    // Label found and marked as used
                } else {
                    self.error(goto.label.span, format!("label {} not declared", name));
                }
            }
            StmtKind::Break(brk) => {
                if let Some(label) = &brk.label {
                    let name = self.ident_name(label);
                    if !block.mark_used(&name) {
                        self.error(label.span, format!("label {} not declared", name));
                    }
                }
            }
            StmtKind::Continue(cont) => {
                if let Some(label) = &cont.label {
                    let name = self.ident_name(label);
                    if !block.mark_used(&name) {
                        self.error(label.span, format!("label {} not declared", name));
                    }
                }
            }
            StmtKind::Labeled(labeled) => {
                self.check_label_usage_in_stmt(&labeled.stmt, block);
            }
            StmtKind::Block(b) => {
                self.check_label_usages(&b.stmts, block);
            }
            StmtKind::If(if_stmt) => {
                self.check_label_usages(&if_stmt.then.stmts, block);
                if let Some(else_) = &if_stmt.else_ {
                    self.check_label_usage_in_stmt(else_, block);
                }
            }
            StmtKind::For(for_stmt) => {
                self.check_label_usages(&for_stmt.body.stmts, block);
            }
            StmtKind::Switch(sw) => {
                for case in &sw.cases {
                    self.check_label_usages(&case.body, block);
                }
            }
            StmtKind::TypeSwitch(ts) => {
                for case in &ts.cases {
                    self.check_label_usages(&case.body, block);
                }
            }
            StmtKind::Select(sel) => {
                for case in &sel.cases {
                    self.check_label_usages(&case.body, block);
                }
            }
            _ => {}
        }
    }

    /// Reports unused labels.
    fn report_unused_labels(&mut self, block: &LabelBlock) {
        for (name, info) in &block.labels {
            if !info.used {
                self.soft_error(info.span, format!("label {} declared but not used", name));
            }
        }
    }

    /// Gets the name of an identifier.
    fn ident_name(&self, ident: &Ident) -> String {
        self.resolve_symbol(ident.symbol).to_string()
    }
}
