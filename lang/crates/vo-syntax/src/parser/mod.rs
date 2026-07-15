#![allow(clippy::result_unit_err)]
//! Parser for the Vo programming language.
//!
//! This module provides a recursive descent parser that converts a token stream
//! into an Abstract Syntax Tree (AST). Expression parsing uses Pratt parsing
//! for correct precedence handling.
//!
//! With global position space, the parser accepts a `base` offset. All span
//! positions in the resulting AST are global (base + local offset).

mod decl;
mod expr;
mod stmt;
mod types;

use std::collections::HashMap;

use crate::ast::InlineModMetadata;
use crate::ast::{ExprId, Ident, IdentId, TypeExprId};
use crate::inline_mod::{parse_leading_inline_mod, InlineModParseOutput};
use vo_common::diagnostics::DiagnosticSink;
use vo_common::span::{BytePos, Span};
use vo_common::symbol::SymbolInterner;

use crate::ast::*;
use crate::errors::{SyntaxDiagnosticSink, SyntaxError};
use crate::lexer::{source_position_error, Lexer};
use crate::token::{Token, TokenKind};

/// Parse result type.
pub type ParseResult<T> = Result<T, ()>;

/// Shared recursion budget for recursive-descent parser entry points.
///
/// Pratt-expression frames are comparatively large, so this stays below the
/// point where a default Rust test thread can exhaust its native stack.
pub(crate) const MAX_PARSER_RECURSION_DEPTH: usize = 128;

/// Maximum number of binary operators on any one AST root-to-leaf path.
///
/// Pratt parsing builds such a chain iteratively, so charging every operator
/// against the recursive-descent budget rejects ordinary generated source long
/// before the parser stack is at risk.  512 covers the largest generated source
/// in the current integration corpus (263 operators), while bounding recursive AST
/// visitors, type checking, code generation, destruction, and register growth.
/// Counting an entire path also prevents precedence changes or postfix wrappers
/// from joining several individually valid chains into an unbounded tree.
pub const MAX_BINARY_EXPRESSION_PATH: usize = 512;

/// ID counters state for multi-file parsing.
#[derive(Debug, Clone, Default)]
pub struct IdState {
    pub expr_id: u32,
    pub type_expr_id: u32,
    pub ident_id: u32,
}

/// The parser for Vo source code.
pub struct Parser<'a> {
    /// Base offset for global positions.
    base: u32,
    /// The source text.
    source: &'a str,
    inline_mod: Option<InlineModMetadata>,
    /// The current token.
    current: Token,
    /// The peek token (one ahead).
    peek: Token,
    /// The lexer.
    lexer: Lexer<'a>,
    /// Symbol interner for identifiers.
    interner: SymbolInterner,
    /// Diagnostics sink.
    diagnostics: SyntaxDiagnosticSink,
    /// Whether composite literals are allowed (disabled in if/for/switch conditions).
    allow_composite_lit: bool,
    /// Current shared recursion depth across expressions, types, and statements.
    recursion_depth: usize,
    /// Set while a fatal depth or expression-resource error propagates through recovery.
    fatal_structure_limit_exceeded: bool,
    /// Structural depth of expressions already built by this parser.
    expr_depths: HashMap<ExprId, usize>,
    /// Binary-node count on the deepest path below each expression.
    expr_binary_depths: HashMap<ExprId, usize>,
    /// Structural depth of type expressions already built by this parser.
    type_expr_depths: HashMap<TypeExprId, usize>,
    /// Binary-node count on the deepest path below each type expression.
    type_expr_binary_depths: HashMap<TypeExprId, usize>,
    /// Next ID counters for expressions, type expressions, and identifiers.
    next_ids: IdState,
}

impl<'a> Parser<'a> {
    /// Creates a new parser from source code with a base offset.
    ///
    /// The base offset should come from `SourceMap::file_base()` to ensure
    /// all positions are globally unique.
    pub fn new(source: &'a str, base: u32) -> Self {
        let (inline_mod_output, diagnostics) = if source_position_error(source, base).is_none() {
            parse_leading_inline_mod(source, base)
        } else {
            (InlineModParseOutput::default(), DiagnosticSink::new())
        };
        let mut lexer = Lexer::new(source, base);
        let current = lexer.next_token();
        let peek = lexer.next_token();

        Self {
            base,
            source,
            inline_mod: inline_mod_output.inline_mod,
            current,
            peek,
            lexer,
            interner: SymbolInterner::new(),
            diagnostics: diagnostics.into(),
            allow_composite_lit: true,
            recursion_depth: 0,
            fatal_structure_limit_exceeded: false,
            expr_depths: HashMap::new(),
            expr_binary_depths: HashMap::new(),
            type_expr_depths: HashMap::new(),
            type_expr_binary_depths: HashMap::new(),
            next_ids: IdState::default(),
        }
    }

    /// Creates a new parser with a shared symbol interner.
    pub fn with_interner(source: &'a str, base: u32, interner: SymbolInterner) -> Self {
        Self::with_interner_and_ids(source, base, interner, IdState::default())
    }

    /// Creates a new parser with shared interner and ID state (for multi-file packages).
    pub fn with_interner_and_ids(
        source: &'a str,
        base: u32,
        interner: SymbolInterner,
        ids: IdState,
    ) -> Self {
        let (inline_mod_output, diagnostics) = if source_position_error(source, base).is_none() {
            parse_leading_inline_mod(source, base)
        } else {
            (InlineModParseOutput::default(), DiagnosticSink::new())
        };
        let mut lexer = Lexer::new(source, base);
        let current = lexer.next_token();
        let peek = lexer.next_token();

        Self {
            base,
            source,
            inline_mod: inline_mod_output.inline_mod,
            current,
            peek,
            lexer,
            interner,
            diagnostics: diagnostics.into(),
            allow_composite_lit: true,
            recursion_depth: 0,
            fatal_structure_limit_exceeded: false,
            expr_depths: HashMap::new(),
            expr_binary_depths: HashMap::new(),
            type_expr_depths: HashMap::new(),
            type_expr_binary_depths: HashMap::new(),
            next_ids: ids,
        }
    }

    /// Runs one recursive parser step and restores the shared budget on every
    /// normal and error return path.
    pub(crate) fn with_recursion_budget<T>(
        &mut self,
        parse: impl FnOnce(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<T> {
        if self.recursion_depth == 0 {
            self.fatal_structure_limit_exceeded = false;
        }
        if self.recursion_depth >= MAX_PARSER_RECURSION_DEPTH {
            self.fatal_structure_limit_exceeded = true;
            self.diagnostics
                .emit(SyntaxError::NestingTooDeep.at_with_message(
                    self.current.span,
                    format!(
                        "syntax nesting exceeds the maximum parser depth of {}",
                        MAX_PARSER_RECURSION_DEPTH
                    ),
                ));
            return Err(());
        }

        self.recursion_depth += 1;
        let result = parse(self);
        self.recursion_depth -= 1;
        result
    }

    /// Runs a parser operation with an explicit composite-literal policy and
    /// restores the previous policy on both success and ordinary parse errors.
    ///
    /// Condition grammars temporarily disable composite literals to resolve
    /// the `T{...}`/statement-block ambiguity. That state must never leak into
    /// parser recovery after a malformed condition.
    pub(crate) fn with_composite_literals<T>(
        &mut self,
        allowed: bool,
        parse: impl FnOnce(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<T> {
        let saved = self.allow_composite_lit;
        self.allow_composite_lit = allowed;
        let result = parse(self);
        self.allow_composite_lit = saved;
        result
    }

    /// Returns the current ID state.
    pub fn id_state(&self) -> IdState {
        self.next_ids.clone()
    }

    /// Returns the diagnostics collected during parsing.
    pub fn diagnostics(&self) -> &DiagnosticSink {
        &self.diagnostics
    }

    /// Takes the diagnostics, leaving an empty sink.
    pub fn take_diagnostics(&mut self) -> DiagnosticSink {
        let mut lexer_diagnostics = self.lexer.take_diagnostics();
        let mut parser_diagnostics = self.diagnostics.take();
        let mut diagnostics = SyntaxDiagnosticSink::default();
        for diagnostic in lexer_diagnostics
            .take()
            .into_iter()
            .chain(parser_diagnostics.take())
        {
            diagnostics.emit(diagnostic);
        }
        diagnostics.into_inner()
    }

    /// Returns the symbol interner.
    pub fn interner(&self) -> &SymbolInterner {
        &self.interner
    }

    /// Takes the symbol interner.
    pub fn take_interner(self) -> SymbolInterner {
        self.interner
    }

    /// Allocates a new ExprId.
    pub(crate) fn alloc_expr_id(&mut self) -> ExprId {
        let id = ExprId(self.next_ids.expr_id);
        self.next_ids.expr_id += 1;
        id
    }

    /// Creates an Expr with an auto-allocated ID after checking the depth of
    /// the structure assembled by iterative Pratt/postfix parsing.
    pub(crate) fn make_expr(&mut self, kind: ExprKind, span: Span) -> ParseResult<Expr> {
        let binary_depth = self.expr_kind_binary_depth(&kind);
        if binary_depth > MAX_BINARY_EXPRESSION_PATH {
            self.fatal_structure_limit_exceeded = true;
            self.diagnostics
                .emit(SyntaxError::ExpressionTooComplex.at_with_message(
                    span,
                    format!(
                        "binary expression path exceeds the resource limit of {} operators",
                        MAX_BINARY_EXPRESSION_PATH
                    ),
                ));
            return Err(());
        }

        let depth = self.expr_kind_child_depth(&kind).saturating_add(1);
        if depth > MAX_PARSER_RECURSION_DEPTH {
            self.fatal_structure_limit_exceeded = true;
            self.diagnostics
                .emit(SyntaxError::NestingTooDeep.at_with_message(
                    span,
                    format!(
                        "expression structure exceeds the maximum parser depth of {}",
                        MAX_PARSER_RECURSION_DEPTH
                    ),
                ));
            return Err(());
        }
        let id = self.alloc_expr_id();
        self.expr_depths.insert(id, depth);
        self.expr_binary_depths.insert(id, binary_depth);
        Ok(Expr { id, kind, span })
    }

    /// Allocates a new TypeExprId.
    pub(crate) fn alloc_type_expr_id(&mut self) -> TypeExprId {
        let id = TypeExprId(self.next_ids.type_expr_id);
        self.next_ids.type_expr_id += 1;
        id
    }

    /// Creates a TypeExpr with an auto-allocated ID and a bounded structure.
    pub(crate) fn make_type_expr(
        &mut self,
        kind: TypeExprKind,
        span: Span,
    ) -> ParseResult<TypeExpr> {
        let binary_depth = self.type_expr_kind_binary_depth(&kind);
        if binary_depth > MAX_BINARY_EXPRESSION_PATH {
            self.fatal_structure_limit_exceeded = true;
            self.diagnostics
                .emit(SyntaxError::ExpressionTooComplex.at_with_message(
                    span,
                    format!(
                        "binary expression path exceeds the resource limit of {} operators",
                        MAX_BINARY_EXPRESSION_PATH
                    ),
                ));
            return Err(());
        }
        let depth = self.type_expr_kind_child_depth(&kind).saturating_add(1);
        if depth > MAX_PARSER_RECURSION_DEPTH {
            self.fatal_structure_limit_exceeded = true;
            self.diagnostics
                .emit(SyntaxError::NestingTooDeep.at_with_message(
                    span,
                    format!(
                        "type structure exceeds the maximum parser depth of {}",
                        MAX_PARSER_RECURSION_DEPTH
                    ),
                ));
            return Err(());
        }
        let id = self.alloc_type_expr_id();
        self.type_expr_depths.insert(id, depth);
        self.type_expr_binary_depths.insert(id, binary_depth);
        Ok(TypeExpr { id, kind, span })
    }

    fn expr_depth(&self, expr: &Expr) -> usize {
        self.expr_depths.get(&expr.id).copied().unwrap_or(1)
    }

    fn expr_binary_depth(&self, expr: &Expr) -> usize {
        self.expr_binary_depths.get(&expr.id).copied().unwrap_or(0)
    }

    fn type_expr_depth(&self, typ: &TypeExpr) -> usize {
        self.type_expr_depths.get(&typ.id).copied().unwrap_or(1)
    }

    fn type_expr_binary_depth(&self, typ: &TypeExpr) -> usize {
        self.type_expr_binary_depths
            .get(&typ.id)
            .copied()
            .unwrap_or(0)
    }

    fn func_sig_child_depth(&self, sig: &FuncSig) -> usize {
        sig.params
            .iter()
            .map(|param| self.type_expr_depth(&param.ty))
            .chain(
                sig.results
                    .iter()
                    .map(|result| self.type_expr_depth(&result.ty)),
            )
            .max()
            .unwrap_or(0)
    }

    fn func_sig_binary_depth(&self, sig: &FuncSig) -> usize {
        sig.params
            .iter()
            .map(|param| self.type_expr_binary_depth(&param.ty))
            .chain(
                sig.results
                    .iter()
                    .map(|result| self.type_expr_binary_depth(&result.ty)),
            )
            .max()
            .unwrap_or(0)
    }

    fn expr_kind_binary_depth(&self, kind: &ExprKind) -> usize {
        match kind {
            ExprKind::Ident(_)
            | ExprKind::IntLit(_)
            | ExprKind::FloatLit(_)
            | ExprKind::RuneLit(_)
            | ExprKind::StringLit(_)
            | ExprKind::Ellipsis => 0,
            ExprKind::Binary(expr) => self
                .expr_binary_depth(&expr.left)
                .max(self.expr_binary_depth(&expr.right))
                .saturating_add(1),
            ExprKind::Unary(expr) => self.expr_binary_depth(&expr.operand),
            ExprKind::Call(expr) => core::iter::once(self.expr_binary_depth(&expr.func))
                .chain(expr.args.iter().map(|arg| self.expr_binary_depth(arg)))
                .max()
                .unwrap_or(0),
            ExprKind::Index(expr) => self
                .expr_binary_depth(&expr.expr)
                .max(self.expr_binary_depth(&expr.index)),
            ExprKind::Slice(expr) => core::iter::once(self.expr_binary_depth(&expr.expr))
                .chain(
                    [&expr.low, &expr.high, &expr.max]
                        .into_iter()
                        .filter_map(|bound| bound.as_ref())
                        .map(|bound| self.expr_binary_depth(bound)),
                )
                .max()
                .unwrap_or(0),
            ExprKind::Selector(expr) => self.expr_binary_depth(&expr.expr),
            ExprKind::TypeAssert(expr) => self.expr_binary_depth(&expr.expr).max(
                expr.ty
                    .as_ref()
                    .map(|typ| self.type_expr_binary_depth(typ))
                    .unwrap_or(0),
            ),
            ExprKind::CompositeLit(literal) => {
                let mut depth = literal
                    .ty
                    .as_ref()
                    .map(|typ| self.type_expr_binary_depth(typ))
                    .unwrap_or(0);
                for elem in &literal.elems {
                    if let Some(CompositeLitKey::Expr(key)) = &elem.key {
                        depth = depth.max(self.expr_binary_depth(key));
                    }
                    depth = depth.max(self.expr_binary_depth(&elem.value));
                }
                depth
            }
            ExprKind::FuncLit(literal) => self
                .func_sig_binary_depth(&literal.sig)
                .max(self.block_binary_depth(&literal.body)),
            ExprKind::Conversion(expr) => self
                .type_expr_binary_depth(&expr.ty)
                .max(self.expr_binary_depth(&expr.expr)),
            ExprKind::Receive(expr) | ExprKind::Paren(expr) | ExprKind::TryUnwrap(expr) => {
                self.expr_binary_depth(expr)
            }
            ExprKind::TypeAsExpr(typ) => self.type_expr_binary_depth(typ),
            ExprKind::DynAccess(expr) => {
                let op_depth = match &expr.op {
                    DynAccessOp::Field(_) => 0,
                    DynAccessOp::Index(index) => self.expr_binary_depth(index),
                    DynAccessOp::Call { args, .. } | DynAccessOp::MethodCall { args, .. } => args
                        .iter()
                        .map(|arg| self.expr_binary_depth(arg))
                        .max()
                        .unwrap_or(0),
                };
                self.expr_binary_depth(&expr.base).max(op_depth)
            }
        }
    }

    fn expr_kind_child_depth(&self, kind: &ExprKind) -> usize {
        match kind {
            ExprKind::Ident(_)
            | ExprKind::IntLit(_)
            | ExprKind::FloatLit(_)
            | ExprKind::RuneLit(_)
            | ExprKind::StringLit(_)
            | ExprKind::Ellipsis => 0,
            ExprKind::Binary(expr) => {
                let left_depth = self.expr_depth(&expr.left);
                let right_depth = self.expr_depth(&expr.right);
                if matches!(expr.left.kind, ExprKind::Binary(_)) {
                    // The direct left spine was assembled iteratively by the
                    // Pratt loop.  Preserve the depth of that spine and charge
                    // only recursive structure hanging from its next RHS.
                    left_depth.saturating_sub(1).max(right_depth)
                } else {
                    left_depth.max(right_depth)
                }
            }
            ExprKind::Unary(expr) => self.expr_depth(&expr.operand),
            ExprKind::Call(expr) => core::iter::once(self.expr_depth(&expr.func))
                .chain(expr.args.iter().map(|arg| self.expr_depth(arg)))
                .max()
                .unwrap_or(0),
            ExprKind::Index(expr) => self
                .expr_depth(&expr.expr)
                .max(self.expr_depth(&expr.index)),
            ExprKind::Slice(expr) => core::iter::once(self.expr_depth(&expr.expr))
                .chain(
                    [&expr.low, &expr.high, &expr.max]
                        .into_iter()
                        .filter_map(|bound| bound.as_ref())
                        .map(|bound| self.expr_depth(bound)),
                )
                .max()
                .unwrap_or(0),
            ExprKind::Selector(expr) => self.expr_depth(&expr.expr),
            ExprKind::TypeAssert(expr) => self.expr_depth(&expr.expr).max(
                expr.ty
                    .as_ref()
                    .map(|typ| self.type_expr_depth(typ))
                    .unwrap_or(0),
            ),
            ExprKind::CompositeLit(literal) => {
                let mut depth = literal
                    .ty
                    .as_ref()
                    .map(|typ| self.type_expr_depth(typ))
                    .unwrap_or(0);
                for elem in &literal.elems {
                    if let Some(CompositeLitKey::Expr(key)) = &elem.key {
                        depth = depth.max(self.expr_depth(key));
                    }
                    depth = depth.max(self.expr_depth(&elem.value));
                }
                depth
            }
            ExprKind::FuncLit(literal) => self
                .func_sig_child_depth(&literal.sig)
                .max(self.block_child_depth(&literal.body)),
            ExprKind::Conversion(expr) => self
                .type_expr_depth(&expr.ty)
                .max(self.expr_depth(&expr.expr)),
            ExprKind::Receive(expr) | ExprKind::Paren(expr) | ExprKind::TryUnwrap(expr) => {
                self.expr_depth(expr)
            }
            ExprKind::TypeAsExpr(typ) => self.type_expr_depth(typ),
            ExprKind::DynAccess(expr) => {
                let op_depth = match &expr.op {
                    DynAccessOp::Field(_) => 0,
                    DynAccessOp::Index(index) => self.expr_depth(index),
                    DynAccessOp::Call { args, .. } | DynAccessOp::MethodCall { args, .. } => args
                        .iter()
                        .map(|arg| self.expr_depth(arg))
                        .max()
                        .unwrap_or(0),
                };
                self.expr_depth(&expr.base).max(op_depth)
            }
        }
    }

    fn type_expr_kind_child_depth(&self, kind: &TypeExprKind) -> usize {
        match kind {
            TypeExprKind::Ident(_) | TypeExprKind::Selector(_) | TypeExprKind::Island => 0,
            TypeExprKind::Array(array) => self
                .expr_depth(&array.len)
                .max(self.type_expr_depth(&array.elem)),
            TypeExprKind::Slice(elem) | TypeExprKind::Pointer(elem) => self.type_expr_depth(elem),
            TypeExprKind::Map(map) => self
                .type_expr_depth(&map.key)
                .max(self.type_expr_depth(&map.value)),
            TypeExprKind::Chan(chan) => self.type_expr_depth(&chan.elem),
            TypeExprKind::Port(port) => self.type_expr_depth(&port.elem),
            TypeExprKind::Func(func) => func
                .params
                .iter()
                .chain(&func.results)
                .map(|param| self.type_expr_depth(&param.ty))
                .max()
                .unwrap_or(0),
            TypeExprKind::Struct(structure) => structure
                .fields
                .iter()
                .map(|field| self.type_expr_depth(&field.ty))
                .max()
                .unwrap_or(0),
            TypeExprKind::Interface(interface) => interface
                .elems
                .iter()
                .filter_map(|elem| match elem {
                    InterfaceElem::Method(method) => Some(self.func_sig_child_depth(&method.sig)),
                    InterfaceElem::Embedded(_) | InterfaceElem::EmbeddedQualified { .. } => None,
                })
                .max()
                .unwrap_or(0),
        }
    }

    fn type_expr_kind_binary_depth(&self, kind: &TypeExprKind) -> usize {
        match kind {
            TypeExprKind::Ident(_) | TypeExprKind::Selector(_) | TypeExprKind::Island => 0,
            TypeExprKind::Array(array) => self
                .expr_binary_depth(&array.len)
                .max(self.type_expr_binary_depth(&array.elem)),
            TypeExprKind::Slice(elem) | TypeExprKind::Pointer(elem) => {
                self.type_expr_binary_depth(elem)
            }
            TypeExprKind::Map(map) => self
                .type_expr_binary_depth(&map.key)
                .max(self.type_expr_binary_depth(&map.value)),
            TypeExprKind::Chan(chan) => self.type_expr_binary_depth(&chan.elem),
            TypeExprKind::Port(port) => self.type_expr_binary_depth(&port.elem),
            TypeExprKind::Func(func) => func
                .params
                .iter()
                .chain(&func.results)
                .map(|param| self.type_expr_binary_depth(&param.ty))
                .max()
                .unwrap_or(0),
            TypeExprKind::Struct(structure) => structure
                .fields
                .iter()
                .map(|field| self.type_expr_binary_depth(&field.ty))
                .max()
                .unwrap_or(0),
            TypeExprKind::Interface(interface) => interface
                .elems
                .iter()
                .filter_map(|elem| match elem {
                    InterfaceElem::Method(method) => Some(self.func_sig_binary_depth(&method.sig)),
                    InterfaceElem::Embedded(_) | InterfaceElem::EmbeddedQualified { .. } => None,
                })
                .max()
                .unwrap_or(0),
        }
    }

    /// Finds the deepest binary-expression path reachable through a function
    /// body without using the host stack.
    fn block_binary_depth(&self, root: &Block) -> usize {
        enum Task<'b> {
            Block(&'b Block),
            Stmt(&'b Stmt),
        }

        let mut max_depth = 0;
        let mut tasks = vec![Task::Block(root)];
        while let Some(task) = tasks.pop() {
            match task {
                Task::Block(block) => {
                    tasks.extend(block.stmts.iter().map(Task::Stmt));
                }
                Task::Stmt(stmt) => {
                    macro_rules! include_expr {
                        ($expr:expr) => {
                            max_depth = max_depth.max(self.expr_binary_depth($expr))
                        };
                    }
                    macro_rules! include_type {
                        ($typ:expr) => {
                            max_depth = max_depth.max(self.type_expr_binary_depth($typ))
                        };
                    }
                    match &stmt.kind {
                        StmtKind::Empty
                        | StmtKind::Break(_)
                        | StmtKind::Continue(_)
                        | StmtKind::Goto(_)
                        | StmtKind::Fallthrough => {}
                        StmtKind::Block(block) => tasks.push(Task::Block(block)),
                        StmtKind::Var(decl) => {
                            for spec in &decl.specs {
                                if let Some(typ) = &spec.ty {
                                    include_type!(typ);
                                }
                                for value in &spec.values {
                                    include_expr!(value);
                                }
                            }
                        }
                        StmtKind::Const(decl) => {
                            for spec in &decl.specs {
                                if let Some(typ) = &spec.ty {
                                    include_type!(typ);
                                }
                                for value in &spec.values {
                                    include_expr!(value);
                                }
                            }
                        }
                        StmtKind::Type(decl) => include_type!(&decl.ty),
                        StmtKind::ShortVar(decl) => {
                            for value in &decl.values {
                                include_expr!(value);
                            }
                        }
                        StmtKind::Expr(expr) => include_expr!(expr),
                        StmtKind::Assign(assign) => {
                            for expr in assign.lhs.iter().chain(&assign.rhs) {
                                include_expr!(expr);
                            }
                        }
                        StmtKind::IncDec(stmt) => include_expr!(&stmt.expr),
                        StmtKind::Return(stmt) => {
                            for value in &stmt.values {
                                include_expr!(value);
                            }
                        }
                        StmtKind::If(stmt) => {
                            include_expr!(&stmt.cond);
                            if let Some(init) = &stmt.init {
                                tasks.push(Task::Stmt(init));
                            }
                            tasks.push(Task::Block(&stmt.then));
                            if let Some(otherwise) = &stmt.else_ {
                                tasks.push(Task::Stmt(otherwise));
                            }
                        }
                        StmtKind::For(stmt) => {
                            match &stmt.clause {
                                ForClause::Cond(cond) => {
                                    if let Some(cond) = cond {
                                        include_expr!(cond);
                                    }
                                }
                                ForClause::Three { init, cond, post } => {
                                    if let Some(init) = init {
                                        tasks.push(Task::Stmt(init));
                                    }
                                    if let Some(cond) = cond {
                                        include_expr!(cond);
                                    }
                                    if let Some(post) = post {
                                        tasks.push(Task::Stmt(post));
                                    }
                                }
                                ForClause::Range {
                                    key, value, expr, ..
                                } => {
                                    if let Some(key) = key {
                                        include_expr!(key);
                                    }
                                    if let Some(value) = value {
                                        include_expr!(value);
                                    }
                                    include_expr!(expr);
                                }
                            }
                            tasks.push(Task::Block(&stmt.body));
                        }
                        StmtKind::Switch(stmt) => {
                            if let Some(init) = &stmt.init {
                                tasks.push(Task::Stmt(init));
                            }
                            if let Some(tag) = &stmt.tag {
                                include_expr!(tag);
                            }
                            for case in &stmt.cases {
                                for expr in &case.exprs {
                                    include_expr!(expr);
                                }
                                tasks.extend(case.body.iter().map(Task::Stmt));
                            }
                        }
                        StmtKind::TypeSwitch(stmt) => {
                            if let Some(init) = &stmt.init {
                                tasks.push(Task::Stmt(init));
                            }
                            include_expr!(&stmt.expr);
                            for case in &stmt.cases {
                                for typ in case.types.iter().flatten() {
                                    include_type!(typ);
                                }
                                tasks.extend(case.body.iter().map(Task::Stmt));
                            }
                        }
                        StmtKind::Select(stmt) => {
                            for case in &stmt.cases {
                                if let Some(comm) = &case.comm {
                                    match comm {
                                        CommClause::Send(send) => {
                                            include_expr!(&send.chan);
                                            include_expr!(&send.value);
                                        }
                                        CommClause::Recv(recv) => include_expr!(&recv.expr),
                                    }
                                }
                                tasks.extend(case.body.iter().map(Task::Stmt));
                            }
                        }
                        StmtKind::Go(stmt) => {
                            if let Some(target) = &stmt.target_island {
                                include_expr!(target);
                            }
                            include_expr!(&stmt.call);
                        }
                        StmtKind::Defer(stmt) => include_expr!(&stmt.call),
                        StmtKind::ErrDefer(stmt) => include_expr!(&stmt.call),
                        StmtKind::Fail(stmt) => include_expr!(&stmt.error),
                        StmtKind::Send(stmt) => {
                            include_expr!(&stmt.chan);
                            include_expr!(&stmt.value);
                        }
                        StmtKind::Labeled(stmt) => tasks.push(Task::Stmt(&stmt.stmt)),
                    }
                }
            }
        }
        max_depth
    }

    /// Computes statement/block depth iteratively so a function literal cannot
    /// hide a deep body below an otherwise shallow expression node.
    fn block_child_depth(&self, root: &Block) -> usize {
        enum Task<'b> {
            Block(&'b Block, usize),
            Stmt(&'b Stmt, usize),
        }

        let mut max_depth = 0;
        let mut tasks = vec![Task::Block(root, 0)];
        while let Some(task) = tasks.pop() {
            match task {
                Task::Block(block, parent_depth) => {
                    let depth = parent_depth.saturating_add(1);
                    max_depth = max_depth.max(depth);
                    tasks.extend(block.stmts.iter().map(|stmt| Task::Stmt(stmt, depth)));
                }
                Task::Stmt(stmt, parent_depth) => {
                    let depth = parent_depth.saturating_add(1);
                    max_depth = max_depth.max(depth);
                    macro_rules! include_expr {
                        ($expr:expr) => {
                            max_depth = max_depth.max(
                                depth
                                    .saturating_add(1)
                                    .saturating_add(self.expr_depth($expr)),
                            )
                        };
                    }
                    macro_rules! include_type {
                        ($typ:expr) => {
                            max_depth = max_depth.max(
                                depth
                                    .saturating_add(1)
                                    .saturating_add(self.type_expr_depth($typ)),
                            )
                        };
                    }
                    match &stmt.kind {
                        StmtKind::Empty
                        | StmtKind::Break(_)
                        | StmtKind::Continue(_)
                        | StmtKind::Goto(_)
                        | StmtKind::Fallthrough => {}
                        StmtKind::Block(block) => tasks.push(Task::Block(block, depth)),
                        StmtKind::Var(decl) => {
                            for spec in &decl.specs {
                                if let Some(typ) = &spec.ty {
                                    include_type!(typ);
                                }
                                for value in &spec.values {
                                    include_expr!(value);
                                }
                            }
                        }
                        StmtKind::Const(decl) => {
                            for spec in &decl.specs {
                                if let Some(typ) = &spec.ty {
                                    include_type!(typ);
                                }
                                for value in &spec.values {
                                    include_expr!(value);
                                }
                            }
                        }
                        StmtKind::Type(decl) => include_type!(&decl.ty),
                        StmtKind::ShortVar(decl) => {
                            for value in &decl.values {
                                include_expr!(value);
                            }
                        }
                        StmtKind::Expr(expr) => include_expr!(expr),
                        StmtKind::Assign(assign) => {
                            for expr in assign.lhs.iter().chain(&assign.rhs) {
                                include_expr!(expr);
                            }
                        }
                        StmtKind::IncDec(stmt) => include_expr!(&stmt.expr),
                        StmtKind::Return(stmt) => {
                            for value in &stmt.values {
                                include_expr!(value);
                            }
                        }
                        StmtKind::If(stmt) => {
                            include_expr!(&stmt.cond);
                            if let Some(init) = &stmt.init {
                                tasks.push(Task::Stmt(init, depth));
                            }
                            tasks.push(Task::Block(&stmt.then, depth));
                            if let Some(otherwise) = &stmt.else_ {
                                tasks.push(Task::Stmt(otherwise, depth));
                            }
                        }
                        StmtKind::For(stmt) => {
                            match &stmt.clause {
                                ForClause::Cond(cond) => {
                                    if let Some(cond) = cond {
                                        include_expr!(cond);
                                    }
                                }
                                ForClause::Three { init, cond, post } => {
                                    if let Some(init) = init {
                                        tasks.push(Task::Stmt(init, depth));
                                    }
                                    if let Some(cond) = cond {
                                        include_expr!(cond);
                                    }
                                    if let Some(post) = post {
                                        tasks.push(Task::Stmt(post, depth));
                                    }
                                }
                                ForClause::Range {
                                    key, value, expr, ..
                                } => {
                                    if let Some(key) = key {
                                        include_expr!(key);
                                    }
                                    if let Some(value) = value {
                                        include_expr!(value);
                                    }
                                    include_expr!(expr);
                                }
                            }
                            tasks.push(Task::Block(&stmt.body, depth));
                        }
                        StmtKind::Switch(stmt) => {
                            if let Some(init) = &stmt.init {
                                tasks.push(Task::Stmt(init, depth));
                            }
                            if let Some(tag) = &stmt.tag {
                                include_expr!(tag);
                            }
                            for case in &stmt.cases {
                                for expr in &case.exprs {
                                    include_expr!(expr);
                                }
                                tasks.extend(
                                    case.body
                                        .iter()
                                        .map(|stmt| Task::Stmt(stmt, depth.saturating_add(1))),
                                );
                            }
                        }
                        StmtKind::TypeSwitch(stmt) => {
                            if let Some(init) = &stmt.init {
                                tasks.push(Task::Stmt(init, depth));
                            }
                            include_expr!(&stmt.expr);
                            for case in &stmt.cases {
                                for typ in case.types.iter().flatten() {
                                    include_type!(typ);
                                }
                                tasks.extend(
                                    case.body
                                        .iter()
                                        .map(|stmt| Task::Stmt(stmt, depth.saturating_add(1))),
                                );
                            }
                        }
                        StmtKind::Select(stmt) => {
                            for case in &stmt.cases {
                                if let Some(comm) = &case.comm {
                                    match comm {
                                        CommClause::Send(send) => {
                                            include_expr!(&send.chan);
                                            include_expr!(&send.value);
                                        }
                                        CommClause::Recv(recv) => include_expr!(&recv.expr),
                                    }
                                }
                                tasks.extend(
                                    case.body
                                        .iter()
                                        .map(|stmt| Task::Stmt(stmt, depth.saturating_add(1))),
                                );
                            }
                        }
                        StmtKind::Go(stmt) => {
                            if let Some(target) = &stmt.target_island {
                                include_expr!(target);
                            }
                            include_expr!(&stmt.call);
                        }
                        StmtKind::Defer(stmt) => include_expr!(&stmt.call),
                        StmtKind::ErrDefer(stmt) => include_expr!(&stmt.call),
                        StmtKind::Fail(stmt) => include_expr!(&stmt.error),
                        StmtKind::Send(stmt) => {
                            include_expr!(&stmt.chan);
                            include_expr!(&stmt.value);
                        }
                        StmtKind::Labeled(stmt) => tasks.push(Task::Stmt(&stmt.stmt, depth)),
                    }
                }
            }
        }
        max_depth
    }

    /// Allocates a new IdentId.
    fn alloc_ident_id(&mut self) -> IdentId {
        let id = IdentId(self.next_ids.ident_id);
        self.next_ids.ident_id += 1;
        id
    }

    /// Parses a complete source file.
    pub fn parse_file(&mut self) -> ParseResult<File> {
        let start = self.current.span.start;

        // Parse package clause
        let package = if self.at(TokenKind::Package) {
            self.advance();
            let name = self.parse_ident()?;
            self.expect_semi();
            Some(name)
        } else {
            None
        };

        // Parse imports
        let mut imports = Vec::new();
        while self.at(TokenKind::Import) {
            let parsed = self.parse_import_or_group()?;
            imports.extend(parsed);
        }

        // Parse top-level declarations
        let mut decls = Vec::new();
        while !self.at_eof() {
            if self.eat(TokenKind::Semicolon) {
                continue;
            }
            match self.parse_top_level_decl() {
                Ok(decl) => decls.push(decl),
                Err(()) => {
                    self.synchronize_to_decl();
                    self.fatal_structure_limit_exceeded = false;
                }
            }
        }

        let end = self.current.span.end;
        Ok(File {
            package,
            inline_mod: self.inline_mod.clone(),
            imports,
            decls,
            span: Span::new(start, end),
        })
    }

    /// Parse import or grouped imports: `import "path"` or `import ( ... )`
    fn parse_import_or_group(&mut self) -> ParseResult<Vec<ImportDecl>> {
        let start = self.current.span.start;
        self.expect(TokenKind::Import)?;

        // Check for grouped imports: import ( ... )
        if self.eat(TokenKind::LParen) {
            let mut imports = Vec::new();
            while !self.at(TokenKind::RParen) && !self.at_eof() {
                imports.push(self.parse_import_spec(start, false)?);
            }
            self.expect(TokenKind::RParen)?;
            self.expect_semi();
            Ok(imports)
        } else {
            // Single import - requires semicolon
            let import = self.parse_import_spec(start, true)?;
            Ok(vec![import])
        }
    }

    /// Parse a single import spec (path with optional alias)
    /// If require_semi is true, expect a semicolon after the import path
    fn parse_import_spec(&mut self, start: BytePos, require_semi: bool) -> ParseResult<ImportDecl> {
        // Check for optional alias (identifier before path)
        // Supported forms:
        //   import "path"      - standard import
        //   import name "path" - standard import with alias
        //   import . "path"         - dot import
        //   import _ "path"         - blank import
        let alias = if self.at(TokenKind::Ident)
            && (self.peek_is(TokenKind::StringLit) || self.peek_is(TokenKind::RawStringLit))
        {
            Some(self.parse_ident()?)
        } else if self.at(TokenKind::Dot)
            && (self.peek_is(TokenKind::StringLit) || self.peek_is(TokenKind::RawStringLit))
        {
            // Dot import: import . "path"
            let dot_span = self.current.span;
            self.advance();
            let dot_sym = self.interner.intern(".");
            Some(Ident {
                id: self.alloc_ident_id(),
                symbol: dot_sym,
                span: dot_span,
            })
        } else {
            None
        };

        if self.eat(TokenKind::At) {
            self.error(
                "`import @\"...\"` is no longer supported; use the canonical import path directly",
            );
        }

        let path = self.parse_string_lit()?;

        // Handle semicolon based on context
        if require_semi {
            self.expect_semi();
        } else {
            // In grouped imports, semicolons are inserted by lexer or optional
            self.eat(TokenKind::Semicolon);
        }

        Ok(ImportDecl {
            path,
            alias,
            span: Span::new(start, self.current.span.start),
        })
    }

    // =========================================================================
    // Token manipulation
    // =========================================================================

    fn advance(&mut self) -> Token {
        std::mem::replace(
            &mut self.current,
            std::mem::replace(&mut self.peek, self.lexer.next_token()),
        )
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.current.kind == kind
    }

    fn at_any(&self, kinds: &[TokenKind]) -> bool {
        kinds.contains(&self.current.kind)
    }

    fn peek_is(&self, kind: TokenKind) -> bool {
        self.peek.kind == kind
    }

    fn at_eof(&self) -> bool {
        self.current.kind == TokenKind::Eof
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) -> ParseResult<Token> {
        if self.at(kind) {
            Ok(self.advance())
        } else {
            self.error_expected(kind.as_str());
            Err(())
        }
    }

    fn expect_semi(&mut self) {
        if !self.eat(TokenKind::Semicolon) && !self.at(TokenKind::RBrace) && !self.at_eof() {
            self.error_expected("';'");
        }
    }

    /// Converts a global span to a local range for indexing into source.
    fn span_to_local_range(&self, span: Span) -> std::ops::Range<usize> {
        let start = (span.start.0 - self.base) as usize;
        let end = (span.end.0 - self.base) as usize;
        start..end
    }

    fn span_text(&self, span: Span) -> &str {
        &self.source[self.span_to_local_range(span)]
    }

    fn make_ident(&mut self, token: &Token) -> Ident {
        let text = &self.source[self.span_to_local_range(token.span)];
        let symbol = self.interner.intern(text);
        Ident {
            id: self.alloc_ident_id(),
            symbol,
            span: token.span,
        }
    }

    // =========================================================================
    // Error handling
    // =========================================================================

    fn error(&mut self, message: impl Into<String>) {
        let span = self.current.span;
        self.diagnostics
            .emit(SyntaxError::UnexpectedToken.at_with_message(span, message));
    }

    fn error_at(&mut self, span: Span, message: impl Into<String>) {
        self.diagnostics
            .emit(SyntaxError::UnexpectedToken.at_with_message(span, message));
    }

    fn error_expected(&mut self, expected: &str) {
        self.error(format!(
            "expected {}, found {}",
            expected,
            self.current.kind.as_str()
        ));
    }

    fn synchronize_to_decl(&mut self) {
        while !self.at_eof() {
            if self.at_any(&[
                TokenKind::Var,
                TokenKind::Const,
                TokenKind::Type,
                TokenKind::Func,
            ]) {
                return;
            }
            self.advance();
        }
    }

    fn synchronize_to_stmt(&mut self) {
        while !self.at_eof() {
            if self.at_any(&[
                TokenKind::Var,
                TokenKind::Const,
                TokenKind::If,
                TokenKind::For,
                TokenKind::Switch,
                TokenKind::Select,
                TokenKind::Return,
                TokenKind::Break,
                TokenKind::Continue,
                TokenKind::Goto,
                TokenKind::Go,
                TokenKind::Defer,
                TokenKind::LBrace,
                TokenKind::RBrace,
                TokenKind::Semicolon,
            ]) {
                return;
            }
            self.advance();
        }
    }

    // =========================================================================
    // Helper parsers
    // =========================================================================

    pub(crate) fn parse_ident(&mut self) -> ParseResult<Ident> {
        if self.at(TokenKind::Ident) {
            let token = self.advance();
            Ok(self.make_ident(&token))
        } else if self.current.kind.is_keyword() {
            // Give a more helpful error when a keyword is used as an identifier
            let keyword = self.current.kind.as_str();
            let span = self.current.span;
            self.diagnostics
                .emit(crate::errors::SyntaxError::KeywordAsIdent.at_with_message(
                    span,
                    format!("cannot use keyword '{}' as identifier", keyword),
                ));
            Err(())
        } else {
            self.error_expected("identifier");
            Err(())
        }
    }

    pub(crate) fn parse_ident_list(&mut self) -> ParseResult<Vec<Ident>> {
        let mut idents = vec![self.parse_ident()?];
        while self.eat(TokenKind::Comma) {
            idents.push(self.parse_ident()?);
        }
        Ok(idents)
    }

    pub(crate) fn parse_string_lit(&mut self) -> ParseResult<StringLit> {
        if self.at(TokenKind::StringLit) {
            let token = self.advance();
            let text = &self.source[self.span_to_local_range(token.span)];
            let raw = self.interner.intern(text);
            // Parse the string value (strip quotes and process escapes)
            let value = parse_string_value(text);
            Ok(StringLit {
                raw,
                value,
                is_raw: false,
            })
        } else if self.at(TokenKind::RawStringLit) {
            let token = self.advance();
            let text = &self.source[self.span_to_local_range(token.span)];
            let raw = self.interner.intern(text);
            // Raw strings: just strip the backticks
            let value = text[1..text.len() - 1].to_string();
            Ok(StringLit {
                raw,
                value,
                is_raw: true,
            })
        } else {
            self.error_expected("string literal");
            Err(())
        }
    }

    /// Parse a rune literal and return the character value.
    pub(crate) fn parse_rune_value(&self, text: &str) -> char {
        parse_rune_value(text)
    }

    /// Parses a block: `{ stmt* }`
    pub(crate) fn parse_block(&mut self) -> ParseResult<Block> {
        let start = self.current.span.start;
        self.expect(TokenKind::LBrace)?;

        let mut stmts = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at_eof() {
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(()) if self.fatal_structure_limit_exceeded => return Err(()),
                Err(()) => self.synchronize_to_stmt(),
            }
        }

        let end_token = self.expect(TokenKind::RBrace)?;
        Ok(Block {
            stmts,
            span: Span::new(start, end_token.span.end),
        })
    }
}

/// Parse a string literal value, processing escape sequences.
/// The input includes the surrounding quotes.
fn parse_string_value(text: &str) -> String {
    let inner = &text[1..text.len() - 1]; // strip quotes
    let mut result = String::with_capacity(inner.len());
    let mut chars = inner.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(escaped) = parse_escape_char(&mut chars) {
                result.push(escaped);
            }
        } else {
            result.push(c);
        }
    }

    result
}

/// Parse a rune literal value.
/// The input includes the surrounding single quotes.
fn parse_rune_value(text: &str) -> char {
    let inner = &text[1..text.len() - 1]; // strip quotes
    let mut chars = inner.chars().peekable();

    if let Some(c) = chars.next() {
        if c == '\\' {
            parse_escape_char(&mut chars).unwrap_or('\0')
        } else {
            c
        }
    } else {
        '\0'
    }
}

/// Parse a single escape sequence and return the resulting character.
fn parse_escape_char(chars: &mut std::iter::Peekable<std::str::Chars>) -> Option<char> {
    match chars.next()? {
        'a' => Some('\x07'), // alert/bell
        'b' => Some('\x08'), // backspace
        'f' => Some('\x0C'), // form feed
        'n' => Some('\n'),
        'r' => Some('\r'),
        't' => Some('\t'),
        'v' => Some('\x0B'), // vertical tab
        '\\' => Some('\\'),
        '\'' => Some('\''),
        '"' => Some('"'),
        'x' => {
            // \xNN - 2 hex digits
            let mut val = 0u32;
            for _ in 0..2 {
                if let Some(c) = chars.next() {
                    val = val * 16 + c.to_digit(16)?;
                }
            }
            char::from_u32(val)
        }
        'u' => {
            // \uNNNN - 4 hex digits
            let mut val = 0u32;
            for _ in 0..4 {
                if let Some(c) = chars.next() {
                    val = val * 16 + c.to_digit(16)?;
                }
            }
            char::from_u32(val)
        }
        'U' => {
            // \UNNNNNNNN - 8 hex digits
            let mut val = 0u32;
            for _ in 0..8 {
                if let Some(c) = chars.next() {
                    val = val * 16 + c.to_digit(16)?;
                }
            }
            char::from_u32(val)
        }
        c @ '0'..='7' => {
            // Octal escape \NNN - 3 octal digits
            let mut val = c.to_digit(8)?;
            for _ in 0..2 {
                if let Some(&next) = chars.peek() {
                    if let Some(d) = next.to_digit(8) {
                        chars.next();
                        val = val * 8 + d;
                    } else {
                        break;
                    }
                }
            }
            char::from_u32(val)
        }
        _ => None, // Unknown escape
    }
}

/// Parses source code and returns the AST.
///
/// # Arguments
/// * `source` - The source code text
/// * `base` - Base offset for global positions (from SourceMap::file_base)
pub fn parse(source: &str, base: u32) -> (File, DiagnosticSink, SymbolInterner) {
    let mut parser = Parser::new(source, base);
    let file = parser.parse_file().unwrap_or_else(|()| File {
        package: None,
        inline_mod: parser.inline_mod.clone(),
        imports: Vec::new(),
        decls: Vec::new(),
        span: Span::dummy(),
    });
    let diagnostics = parser.take_diagnostics();
    let interner = parser.take_interner();
    (file, diagnostics, interner)
}

/// Parses source code with a shared interner (for multi-file packages).
pub fn parse_with_interner(
    source: &str,
    base: u32,
    interner: SymbolInterner,
) -> (File, DiagnosticSink, SymbolInterner) {
    let mut parser = Parser::with_interner(source, base, interner);
    let file = parser.parse_file().unwrap_or_else(|()| File {
        package: None,
        inline_mod: parser.inline_mod.clone(),
        imports: Vec::new(),
        decls: Vec::new(),
        span: Span::dummy(),
    });
    let diagnostics = parser.take_diagnostics();
    let interner = parser.take_interner();
    (file, diagnostics, interner)
}

/// Parses source code with shared interner and ID state (for same-package multi-file).
pub fn parse_with_state(
    source: &str,
    base: u32,
    interner: SymbolInterner,
    ids: IdState,
) -> (File, DiagnosticSink, SymbolInterner, IdState) {
    let mut parser = Parser::with_interner_and_ids(source, base, interner, ids);
    let file = parser.parse_file().unwrap_or_else(|()| File {
        package: None,
        inline_mod: parser.inline_mod.clone(),
        imports: Vec::new(),
        decls: Vec::new(),
        span: Span::dummy(),
    });
    let diagnostics = parser.take_diagnostics();
    let ids = parser.id_state();
    let interner = parser.take_interner();
    (file, diagnostics, interner, ids)
}

#[cfg(test)]
mod tests;
