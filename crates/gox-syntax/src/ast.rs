//! Abstract Syntax Tree definitions for GoX.
//!
//! This module defines the complete AST for the GoX programming language,
//! covering all declarations, statements, expressions, and types.

use gox_common::span::Span;
use gox_common::symbol::{Ident, Symbol};
use gox_common_core::ExprId;

/// A source file.
#[derive(Debug, Clone)]
pub struct File {
    /// The package declaration, if present.
    pub package: Option<Ident>,
    /// Import declarations.
    pub imports: Vec<ImportDecl>,
    /// Top-level declarations.
    pub decls: Vec<Decl>,
    /// The span of the entire file.
    pub span: Span,
}

/// The kind of import (local/stdlib vs external).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImportKind {
    /// Standard library or local package: `import "fmt"`, `import "utils"`
    Standard,
    /// External dependency with @ marker: `import @"gin"`
    External,
}

/// An import declaration.
#[derive(Debug, Clone)]
pub struct ImportDecl {
    /// The kind of import (standard/local vs external).
    pub kind: ImportKind,
    /// The import path (string literal).
    pub path: StringLit,
    /// Optional alias for the import (e.g., `import m "math"`).
    pub alias: Option<Ident>,
    /// The span of the import declaration.
    pub span: Span,
}

/// A top-level declaration.
#[derive(Debug, Clone)]
pub enum Decl {
    /// Variable declaration: `var x int = 1`
    Var(VarDecl),
    /// Constant declaration: `const x = 1`
    Const(ConstDecl),
    /// Type declaration: `type T struct { ... }`
    Type(TypeDecl),
    /// Function declaration: `func f() { ... }`
    Func(FuncDecl),
    /// Interface declaration: `interface I { ... }`
    Interface(InterfaceDecl),
}

impl Decl {
    /// Returns the span of this declaration.
    pub fn span(&self) -> Span {
        match self {
            Decl::Var(d) => d.span,
            Decl::Const(d) => d.span,
            Decl::Type(d) => d.span,
            Decl::Func(d) => d.span,
            Decl::Interface(d) => d.span,
        }
    }
}

/// A variable declaration.
#[derive(Debug, Clone)]
pub struct VarDecl {
    /// The variable specifications.
    pub specs: Vec<VarSpec>,
    /// The span of the declaration.
    pub span: Span,
}

/// A single variable specification within a var declaration.
#[derive(Debug, Clone)]
pub struct VarSpec {
    /// The variable names.
    pub names: Vec<Ident>,
    /// The type, if specified.
    pub ty: Option<TypeExpr>,
    /// The initializer expressions, if any.
    pub values: Vec<Expr>,
    /// The span of this spec.
    pub span: Span,
}

/// A constant declaration.
#[derive(Debug, Clone)]
pub struct ConstDecl {
    /// The constant specifications.
    pub specs: Vec<ConstSpec>,
    /// The span of the declaration.
    pub span: Span,
}

/// A single constant specification within a const declaration.
#[derive(Debug, Clone)]
pub struct ConstSpec {
    /// The constant names.
    pub names: Vec<Ident>,
    /// The type, if specified.
    pub ty: Option<TypeExpr>,
    /// The initializer expressions (may be empty for iota continuation).
    pub values: Vec<Expr>,
    /// The span of this spec.
    pub span: Span,
}

/// A type declaration.
#[derive(Debug, Clone)]
pub struct TypeDecl {
    /// The type name.
    pub name: Ident,
    /// The underlying type.
    pub ty: TypeExpr,
    /// The span of the declaration.
    pub span: Span,
}

/// A function declaration.
#[derive(Debug, Clone)]
pub struct FuncDecl {
    /// The receiver, if this is a method.
    pub receiver: Option<Receiver>,
    /// The function name.
    pub name: Ident,
    /// The function signature.
    pub sig: FuncSig,
    /// The function body. None means extern function (implemented outside GoX).
    pub body: Option<Block>,
    /// The span of the declaration.
    pub span: Span,
}

impl FuncDecl {
    /// Returns true if this is an extern function (no body, implemented outside GoX).
    pub fn is_extern(&self) -> bool {
        self.body.is_none()
    }
}

/// A method receiver.
#[derive(Debug, Clone)]
pub struct Receiver {
    /// The receiver variable name.
    pub name: Ident,
    /// The receiver type name.
    pub ty: Ident,
    /// Whether this is a pointer receiver (*T).
    pub is_pointer: bool,
    /// The span of the receiver.
    pub span: Span,
}

/// A function signature.
#[derive(Debug, Clone)]
pub struct FuncSig {
    /// The parameters.
    pub params: Vec<Param>,
    /// The result parameters (may have names for named returns).
    pub results: Vec<ResultParam>,
    /// Whether the last parameter is variadic.
    pub variadic: bool,
    /// The span of the signature.
    pub span: Span,
}

/// A function result parameter (may be named or unnamed).
#[derive(Debug, Clone)]
pub struct ResultParam {
    /// The result name (None for unnamed results).
    pub name: Option<Ident>,
    /// The result type.
    pub ty: TypeExpr,
    /// The span of this result parameter.
    pub span: Span,
}

/// A function parameter.
#[derive(Debug, Clone)]
pub struct Param {
    /// The parameter names (may share a type).
    pub names: Vec<Ident>,
    /// The parameter type.
    pub ty: TypeExpr,
    /// The span of the parameter.
    pub span: Span,
}

/// An interface declaration.
#[derive(Debug, Clone)]
pub struct InterfaceDecl {
    /// The interface name.
    pub name: Ident,
    /// The interface elements (methods and embedded interfaces).
    pub elems: Vec<InterfaceElem>,
    /// The span of the declaration.
    pub span: Span,
}

/// An interface element.
#[derive(Debug, Clone)]
pub enum InterfaceElem {
    /// A method specification.
    Method(MethodSpec),
    /// An embedded interface.
    Embedded(Ident),
}

/// A method specification in an interface.
#[derive(Debug, Clone)]
pub struct MethodSpec {
    /// The method name.
    pub name: Ident,
    /// The method signature.
    pub sig: FuncSig,
    /// The span of the method spec.
    pub span: Span,
}

// =============================================================================
// Type Expressions
// =============================================================================

/// A type expression.
#[derive(Debug, Clone)]
pub struct TypeExpr {
    /// The kind of type.
    pub kind: TypeExprKind,
    /// The span of the type expression.
    pub span: Span,
}

/// The kind of a type expression.
#[derive(Debug, Clone)]
pub enum TypeExprKind {
    /// A named type: `int`, `MyType`
    Ident(Ident),
    /// A qualified type: `pkg.Type`
    Selector(Box<SelectorTypeExpr>),
    /// An array type: `[N]T`
    Array(Box<ArrayType>),
    /// A slice type: `[]T`
    Slice(Box<TypeExpr>),
    /// A map type: `map[K]V`
    Map(Box<MapType>),
    /// A channel type: `chan T`, `chan<- T`, `<-chan T`
    Chan(Box<ChanType>),
    /// A function type: `func(T) R`
    Func(Box<FuncType>),
    /// A struct type: `struct { ... }`
    Struct(Box<StructType>),
    /// A pointer type: `*T` (only valid for struct types)
    Pointer(Box<TypeExpr>),
    /// An interface type: `interface { ... }`
    Interface(Box<InterfaceType>),
}

/// A qualified type expression: `pkg.Type`
#[derive(Debug, Clone)]
pub struct SelectorTypeExpr {
    /// The package identifier.
    pub pkg: Ident,
    /// The type name.
    pub sel: Ident,
}

/// An array type.
#[derive(Debug, Clone)]
pub struct ArrayType {
    /// The array length expression.
    pub len: Expr,
    /// The element type.
    pub elem: TypeExpr,
}

/// A map type.
#[derive(Debug, Clone)]
pub struct MapType {
    /// The key type.
    pub key: TypeExpr,
    /// The value type.
    pub value: TypeExpr,
}

/// A channel type.
#[derive(Debug, Clone)]
pub struct ChanType {
    /// The channel direction.
    pub dir: ChanDir,
    /// The element type.
    pub elem: TypeExpr,
}

/// Channel direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChanDir {
    /// Bidirectional: `chan T`
    Both,
    /// Send-only: `chan<- T`
    Send,
    /// Receive-only: `<-chan T`
    Recv,
}

/// A function type.
#[derive(Debug, Clone)]
pub struct FuncType {
    /// The parameter types.
    pub params: Vec<TypeExpr>,
    /// The result types.
    pub results: Vec<TypeExpr>,
}

/// A struct or object type.
#[derive(Debug, Clone)]
pub struct StructType {
    /// The fields.
    pub fields: Vec<Field>,
}

/// A struct/object field.
#[derive(Debug, Clone)]
pub struct Field {
    /// The field names (may be empty for embedded fields).
    pub names: Vec<Ident>,
    /// The field type.
    pub ty: TypeExpr,
    /// The field tag, if any.
    pub tag: Option<StringLit>,
    /// The span of the field.
    pub span: Span,
}

/// An interface type (inline).
#[derive(Debug, Clone)]
pub struct InterfaceType {
    /// The interface elements.
    pub elems: Vec<InterfaceElem>,
}

// =============================================================================
// Statements
// =============================================================================

/// A statement.
#[derive(Debug, Clone)]
pub struct Stmt {
    /// The kind of statement.
    pub kind: StmtKind,
    /// The span of the statement.
    pub span: Span,
}

/// The kind of a statement.
#[derive(Debug, Clone)]
pub enum StmtKind {
    /// An empty statement.
    Empty,
    /// A block statement.
    Block(Block),
    /// A variable declaration statement.
    Var(VarDecl),
    /// A constant declaration statement.
    Const(ConstDecl),
    /// A type declaration statement.
    Type(TypeDecl),
    /// A short variable declaration: `x := expr`
    ShortVar(ShortVarDecl),
    /// An expression statement.
    Expr(Expr),
    /// An assignment statement.
    Assign(AssignStmt),
    /// An increment/decrement statement.
    IncDec(IncDecStmt),
    /// A return statement.
    Return(ReturnStmt),
    /// An if statement.
    If(IfStmt),
    /// A for statement.
    For(ForStmt),
    /// A switch statement.
    Switch(SwitchStmt),
    /// A type switch statement.
    TypeSwitch(TypeSwitchStmt),
    /// A select statement.
    Select(SelectStmt),
    /// A go statement.
    Go(GoStmt),
    /// A defer statement.
    Defer(DeferStmt),
    /// An errdefer statement (runs only on error return).
    ErrDefer(ErrDeferStmt),
    /// A fail statement (returns error from fallible function).
    Fail(FailStmt),
    /// A send statement.
    Send(SendStmt),
    /// A break statement.
    Break(BreakStmt),
    /// A continue statement.
    Continue(ContinueStmt),
    /// A goto statement.
    Goto(GotoStmt),
    /// A fallthrough statement.
    Fallthrough,
    /// A labeled statement.
    Labeled(LabeledStmt),
}

/// A block of statements.
#[derive(Debug, Clone)]
pub struct Block {
    /// The statements in the block.
    pub stmts: Vec<Stmt>,
    /// The span of the block.
    pub span: Span,
}

/// A short variable declaration.
#[derive(Debug, Clone)]
pub struct ShortVarDecl {
    /// The variable names.
    pub names: Vec<Ident>,
    /// The initializer expressions.
    pub values: Vec<Expr>,
}

/// An assignment statement.
#[derive(Debug, Clone)]
pub struct AssignStmt {
    /// The left-hand side expressions.
    pub lhs: Vec<Expr>,
    /// The assignment operator.
    pub op: AssignOp,
    /// The right-hand side expressions.
    pub rhs: Vec<Expr>,
}

/// Assignment operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    /// `=`
    Assign,
    /// `+=`
    Add,
    /// `-=`
    Sub,
    /// `*=`
    Mul,
    /// `/=`
    Div,
    /// `%=`
    Rem,
    /// `<<=`
    Shl,
    /// `>>=`
    Shr,
    /// `&=`
    And,
    /// `|=`
    Or,
    /// `^=`
    Xor,
    /// `&^=`
    AndNot,
}

/// An increment/decrement statement.
#[derive(Debug, Clone)]
pub struct IncDecStmt {
    /// The expression to increment/decrement.
    pub expr: Expr,
    /// Whether this is increment (true) or decrement (false).
    pub is_inc: bool,
}

/// A return statement.
#[derive(Debug, Clone)]
pub struct ReturnStmt {
    /// The return values.
    pub values: Vec<Expr>,
}

/// An if statement.
#[derive(Debug, Clone)]
pub struct IfStmt {
    /// The init statement, if any.
    pub init: Option<Box<Stmt>>,
    /// The condition.
    pub cond: Expr,
    /// The then block.
    pub then: Block,
    /// The else branch (if or block), if any.
    pub else_: Option<Box<Stmt>>,
}

/// A for statement.
#[derive(Debug, Clone)]
pub struct ForStmt {
    /// The for clause.
    pub clause: ForClause,
    /// The body.
    pub body: Block,
}

/// A for clause.
#[derive(Debug, Clone)]
pub enum ForClause {
    /// A condition-only for: `for cond { }`
    Cond(Option<Expr>),
    /// A three-clause for: `for init; cond; post { }`
    Three {
        init: Option<Box<Stmt>>,
        cond: Option<Expr>,
        post: Option<Box<Stmt>>,
    },
    /// A range for: `for k, v := range expr { }`
    Range {
        key: Option<Ident>,
        value: Option<Ident>,
        define: bool,
        expr: Expr,
    },
}

/// A switch statement.
#[derive(Debug, Clone)]
pub struct SwitchStmt {
    /// The init statement, if any.
    pub init: Option<Box<Stmt>>,
    /// The tag expression, if any.
    pub tag: Option<Expr>,
    /// The case clauses.
    pub cases: Vec<CaseClause>,
}

/// A case clause in a switch statement.
#[derive(Debug, Clone)]
pub struct CaseClause {
    /// The case expressions (empty for default).
    pub exprs: Vec<Expr>,
    /// The statements in this case.
    pub body: Vec<Stmt>,
    /// The span of the case clause.
    pub span: Span,
}

/// A type switch statement.
#[derive(Debug, Clone)]
pub struct TypeSwitchStmt {
    /// The init statement, if any.
    pub init: Option<Box<Stmt>>,
    /// The variable to bind the asserted type to, if any.
    pub assign: Option<Ident>,
    /// The expression being type-switched.
    pub expr: Expr,
    /// The type case clauses.
    pub cases: Vec<TypeCaseClause>,
}

/// A type case clause.
#[derive(Debug, Clone)]
pub struct TypeCaseClause {
    /// The types (empty for default, may contain nil).
    pub types: Vec<Option<TypeExpr>>,
    /// The statements in this case.
    pub body: Vec<Stmt>,
    /// The span of the case clause.
    pub span: Span,
}

/// A select statement.
#[derive(Debug, Clone)]
pub struct SelectStmt {
    /// The select cases.
    pub cases: Vec<SelectCase>,
}

/// A select case.
#[derive(Debug, Clone)]
pub struct SelectCase {
    /// The communication clause (None for default).
    pub comm: Option<CommClause>,
    /// The statements in this case.
    pub body: Vec<Stmt>,
    /// The span of the case.
    pub span: Span,
}

/// A communication clause in a select case.
#[derive(Debug, Clone)]
pub enum CommClause {
    /// A send statement.
    Send(SendStmt),
    /// A receive statement.
    Recv(RecvStmt),
}

/// A receive statement in a select case.
#[derive(Debug, Clone)]
pub struct RecvStmt {
    /// The variables to assign to, if any.
    pub lhs: Vec<Ident>,
    /// Whether this is a short variable declaration.
    pub define: bool,
    /// The receive expression.
    pub expr: Expr,
}

/// A go statement.
#[derive(Debug, Clone)]
pub struct GoStmt {
    /// The call expression.
    pub call: Expr,
}

/// A defer statement.
#[derive(Debug, Clone)]
pub struct DeferStmt {
    /// The call expression.
    pub call: Expr,
}

/// An errdefer statement (runs only on error return).
#[derive(Debug, Clone)]
pub struct ErrDeferStmt {
    /// The call expression to execute on error.
    pub call: Expr,
}

/// A fail statement (returns error from fallible function).
#[derive(Debug, Clone)]
pub struct FailStmt {
    /// The error expression to return.
    pub error: Expr,
}

/// A send statement.
#[derive(Debug, Clone)]
pub struct SendStmt {
    /// The channel expression.
    pub chan: Expr,
    /// The value to send.
    pub value: Expr,
}

/// A break statement.
#[derive(Debug, Clone)]
pub struct BreakStmt {
    /// The label, if any.
    pub label: Option<Ident>,
}

/// A continue statement.
#[derive(Debug, Clone)]
pub struct ContinueStmt {
    /// The label, if any.
    pub label: Option<Ident>,
}

/// A goto statement.
#[derive(Debug, Clone)]
pub struct GotoStmt {
    /// The target label.
    pub label: Ident,
}

/// A labeled statement.
#[derive(Debug, Clone)]
pub struct LabeledStmt {
    /// The label.
    pub label: Ident,
    /// The labeled statement.
    pub stmt: Box<Stmt>,
}

// =============================================================================
// Expressions
// =============================================================================

/// An expression.
#[derive(Debug, Clone)]
pub struct Expr {
    /// Unique ID for this expression (for type lookup).
    pub id: ExprId,
    /// The kind of expression.
    pub kind: ExprKind,
    /// The span of the expression.
    pub span: Span,
}

/// The kind of an expression.
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// An identifier.
    Ident(Ident),
    /// An integer literal.
    IntLit(IntLit),
    /// A float literal.
    FloatLit(FloatLit),
    /// A rune literal.
    RuneLit(RuneLit),
    /// A string literal.
    StringLit(StringLit),
    /// A binary expression.
    Binary(Box<BinaryExpr>),
    /// A unary expression.
    Unary(Box<UnaryExpr>),
    /// A call expression.
    Call(Box<CallExpr>),
    /// An index expression.
    Index(Box<IndexExpr>),
    /// A slice expression.
    Slice(Box<SliceExpr>),
    /// A selector expression.
    Selector(Box<SelectorExpr>),
    /// A type assertion.
    TypeAssert(Box<TypeAssertExpr>),
    /// A composite literal.
    CompositeLit(Box<CompositeLit>),
    /// A function literal.
    FuncLit(Box<FuncLit>),
    /// A type conversion.
    Conversion(Box<ConversionExpr>),
    /// A receive expression.
    Receive(Box<Expr>),
    /// A parenthesized expression.
    Paren(Box<Expr>),
    /// A type used as an expression (for make/new first argument).
    TypeAsExpr(Box<TypeExpr>),
    /// A try-unwrap expression (error propagation with ?).
    TryUnwrap(Box<Expr>),
}

/// An integer literal.
#[derive(Debug, Clone)]
pub struct IntLit {
    /// The raw text of the literal.
    pub raw: Symbol,
}

/// A float literal.
#[derive(Debug, Clone)]
pub struct FloatLit {
    /// The raw text of the literal.
    pub raw: Symbol,
}

/// A rune literal.
#[derive(Debug, Clone)]
pub struct RuneLit {
    /// The raw text of the literal (including quotes).
    pub raw: Symbol,
}

/// A string literal.
#[derive(Debug, Clone)]
pub struct StringLit {
    /// The raw text of the literal (including quotes).
    pub raw: Symbol,
    /// Whether this is a raw string literal.
    pub is_raw: bool,
}

/// A binary expression.
#[derive(Debug, Clone)]
pub struct BinaryExpr {
    /// The left operand.
    pub left: Expr,
    /// The operator.
    pub op: BinaryOp,
    /// The right operand.
    pub right: Expr,
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    Rem,    // %

    // Bitwise
    And,    // &
    Or,     // |
    Xor,    // ^
    AndNot, // &^
    Shl,    // <<
    Shr,    // >>

    // Comparison
    Eq,     // ==
    NotEq,  // !=
    Lt,     // <
    LtEq,   // <=
    Gt,     // >
    GtEq,   // >=

    // Logical
    LogAnd, // &&
    LogOr,  // ||
}

/// A unary expression.
#[derive(Debug, Clone)]
pub struct UnaryExpr {
    /// The operator.
    pub op: UnaryOp,
    /// The operand.
    pub operand: Expr,
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Pos,    // +
    Neg,    // -
    Not,    // !
    BitNot, // ^
    Addr,   // & (address-of)
    Deref,  // * (dereference)
}

/// A call expression.
#[derive(Debug, Clone)]
pub struct CallExpr {
    /// The function being called.
    pub func: Expr,
    /// The arguments.
    pub args: Vec<Expr>,
    /// Whether the last argument is spread with `...`.
    pub spread: bool,
}

/// An index expression.
#[derive(Debug, Clone)]
pub struct IndexExpr {
    /// The expression being indexed.
    pub expr: Expr,
    /// The index.
    pub index: Expr,
}

/// A slice expression.
#[derive(Debug, Clone)]
pub struct SliceExpr {
    /// The expression being sliced.
    pub expr: Expr,
    /// The low bound, if any.
    pub low: Option<Expr>,
    /// The high bound, if any.
    pub high: Option<Expr>,
}

/// A selector expression.
#[derive(Debug, Clone)]
pub struct SelectorExpr {
    /// The expression being selected from.
    pub expr: Expr,
    /// The selected field or method.
    pub sel: Ident,
}

/// A type assertion expression.
#[derive(Debug, Clone)]
pub struct TypeAssertExpr {
    /// The expression being asserted.
    pub expr: Expr,
    /// The asserted type (None for type switch `x.(type)`).
    pub ty: Option<TypeExpr>,
}

/// A composite literal.
#[derive(Debug, Clone)]
pub struct CompositeLit {
    /// The type of the literal.
    pub ty: TypeExpr,
    /// The elements.
    pub elems: Vec<CompositeLitElem>,
}

/// An element in a composite literal.
#[derive(Debug, Clone)]
pub struct CompositeLitElem {
    /// The key, if any (field name or index).
    pub key: Option<CompositeLitKey>,
    /// The value.
    pub value: Expr,
    /// The span of this element.
    pub span: Span,
}

/// A key in a composite literal element.
#[derive(Debug, Clone)]
pub enum CompositeLitKey {
    /// A field name.
    Ident(Ident),
    /// An index expression.
    Expr(Expr),
}

/// A function literal.
#[derive(Debug, Clone)]
pub struct FuncLit {
    /// The function signature.
    pub sig: FuncSig,
    /// The function body.
    pub body: Block,
}

/// A type conversion expression.
#[derive(Debug, Clone)]
pub struct ConversionExpr {
    /// The target type.
    pub ty: TypeExpr,
    /// The expression being converted.
    pub expr: Expr,
}

// =============================================================================
// Visitor trait
// =============================================================================

/// A visitor for traversing the AST.
pub trait Visitor: Sized {
    fn visit_file(&mut self, file: &File) {
        walk_file(self, file);
    }

    fn visit_decl(&mut self, decl: &Decl) {
        walk_decl(self, decl);
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }

    fn visit_type_expr(&mut self, ty: &TypeExpr) {
        walk_type_expr(self, ty);
    }

    fn visit_ident(&mut self, _ident: &Ident) {}
}

/// Walk a file.
pub fn walk_file<V: Visitor>(visitor: &mut V, file: &File) {
    if let Some(pkg) = &file.package {
        visitor.visit_ident(pkg);
    }
    for decl in &file.decls {
        visitor.visit_decl(decl);
    }
}

/// Walk a declaration.
pub fn walk_decl<V: Visitor>(visitor: &mut V, decl: &Decl) {
    match decl {
        Decl::Var(d) => {
            for spec in &d.specs {
                for name in &spec.names {
                    visitor.visit_ident(name);
                }
                if let Some(ty) = &spec.ty {
                    visitor.visit_type_expr(ty);
                }
                for value in &spec.values {
                    visitor.visit_expr(value);
                }
            }
        }
        Decl::Const(d) => {
            for spec in &d.specs {
                for name in &spec.names {
                    visitor.visit_ident(name);
                }
                if let Some(ty) = &spec.ty {
                    visitor.visit_type_expr(ty);
                }
                for value in &spec.values {
                    visitor.visit_expr(value);
                }
            }
        }
        Decl::Type(d) => {
            visitor.visit_ident(&d.name);
            visitor.visit_type_expr(&d.ty);
        }
        Decl::Func(d) => {
            visitor.visit_ident(&d.name);
            if let Some(body) = &d.body {
                for stmt in &body.stmts {
                    visitor.visit_stmt(stmt);
                }
            }
        }
        Decl::Interface(d) => {
            visitor.visit_ident(&d.name);
        }
    }
}

/// Walk a statement.
pub fn walk_stmt<V: Visitor>(visitor: &mut V, stmt: &Stmt) {
    match &stmt.kind {
        StmtKind::Empty => {}
        StmtKind::Block(block) => {
            for s in &block.stmts {
                visitor.visit_stmt(s);
            }
        }
        StmtKind::Var(d) => visitor.visit_decl(&Decl::Var(d.clone())),
        StmtKind::Const(d) => visitor.visit_decl(&Decl::Const(d.clone())),
        StmtKind::Type(d) => visitor.visit_decl(&Decl::Type(d.clone())),
        StmtKind::ShortVar(d) => {
            for name in &d.names {
                visitor.visit_ident(name);
            }
            for value in &d.values {
                visitor.visit_expr(value);
            }
        }
        StmtKind::Expr(e) => visitor.visit_expr(e),
        StmtKind::Assign(a) => {
            for e in &a.lhs {
                visitor.visit_expr(e);
            }
            for e in &a.rhs {
                visitor.visit_expr(e);
            }
        }
        StmtKind::IncDec(i) => visitor.visit_expr(&i.expr),
        StmtKind::Return(r) => {
            for e in &r.values {
                visitor.visit_expr(e);
            }
        }
        StmtKind::If(i) => {
            if let Some(init) = &i.init {
                visitor.visit_stmt(init);
            }
            visitor.visit_expr(&i.cond);
            for s in &i.then.stmts {
                visitor.visit_stmt(s);
            }
            if let Some(else_) = &i.else_ {
                visitor.visit_stmt(else_);
            }
        }
        StmtKind::For(f) => {
            match &f.clause {
                ForClause::Cond(Some(e)) => visitor.visit_expr(e),
                ForClause::Three { init, cond, post } => {
                    if let Some(init) = init {
                        visitor.visit_stmt(init);
                    }
                    if let Some(cond) = cond {
                        visitor.visit_expr(cond);
                    }
                    if let Some(post) = post {
                        visitor.visit_stmt(post);
                    }
                }
                ForClause::Range { expr, .. } => visitor.visit_expr(expr),
                _ => {}
            }
            for s in &f.body.stmts {
                visitor.visit_stmt(s);
            }
        }
        StmtKind::Switch(s) => {
            if let Some(init) = &s.init {
                visitor.visit_stmt(init);
            }
            if let Some(tag) = &s.tag {
                visitor.visit_expr(tag);
            }
            for case in &s.cases {
                for e in &case.exprs {
                    visitor.visit_expr(e);
                }
                for s in &case.body {
                    visitor.visit_stmt(s);
                }
            }
        }
        StmtKind::TypeSwitch(s) => {
            if let Some(init) = &s.init {
                visitor.visit_stmt(init);
            }
            visitor.visit_expr(&s.expr);
            for case in &s.cases {
                for ty in &case.types {
                    if let Some(ty) = ty {
                        visitor.visit_type_expr(ty);
                    }
                }
                for s in &case.body {
                    visitor.visit_stmt(s);
                }
            }
        }
        StmtKind::Select(s) => {
            for case in &s.cases {
                for stmt in &case.body {
                    visitor.visit_stmt(stmt);
                }
            }
        }
        StmtKind::Go(g) => visitor.visit_expr(&g.call),
        StmtKind::Defer(d) => visitor.visit_expr(&d.call),
        StmtKind::ErrDefer(d) => visitor.visit_expr(&d.call),
        StmtKind::Fail(f) => visitor.visit_expr(&f.error),
        StmtKind::Send(s) => {
            visitor.visit_expr(&s.chan);
            visitor.visit_expr(&s.value);
        }
        StmtKind::Break(_) | StmtKind::Continue(_) | StmtKind::Goto(_) | StmtKind::Fallthrough => {}
        StmtKind::Labeled(l) => {
            visitor.visit_ident(&l.label);
            visitor.visit_stmt(&l.stmt);
        }
    }
}

/// Walk an expression.
pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr) {
    match &expr.kind {
        ExprKind::Ident(i) => visitor.visit_ident(i),
        ExprKind::IntLit(_) | ExprKind::FloatLit(_) | ExprKind::RuneLit(_) | ExprKind::StringLit(_) => {}
        ExprKind::Binary(b) => {
            visitor.visit_expr(&b.left);
            visitor.visit_expr(&b.right);
        }
        ExprKind::Unary(u) => visitor.visit_expr(&u.operand),
        ExprKind::Call(c) => {
            visitor.visit_expr(&c.func);
            for arg in &c.args {
                visitor.visit_expr(arg);
            }
        }
        ExprKind::Index(i) => {
            visitor.visit_expr(&i.expr);
            visitor.visit_expr(&i.index);
        }
        ExprKind::Slice(s) => {
            visitor.visit_expr(&s.expr);
            if let Some(low) = &s.low {
                visitor.visit_expr(low);
            }
            if let Some(high) = &s.high {
                visitor.visit_expr(high);
            }
        }
        ExprKind::Selector(s) => {
            visitor.visit_expr(&s.expr);
            visitor.visit_ident(&s.sel);
        }
        ExprKind::TypeAssert(t) => {
            visitor.visit_expr(&t.expr);
            if let Some(ty) = &t.ty {
                visitor.visit_type_expr(ty);
            }
        }
        ExprKind::CompositeLit(c) => {
            visitor.visit_type_expr(&c.ty);
            for elem in &c.elems {
                visitor.visit_expr(&elem.value);
            }
        }
        ExprKind::FuncLit(f) => {
            for stmt in &f.body.stmts {
                visitor.visit_stmt(stmt);
            }
        }
        ExprKind::Conversion(c) => {
            visitor.visit_type_expr(&c.ty);
            visitor.visit_expr(&c.expr);
        }
        ExprKind::Receive(e) => visitor.visit_expr(e),
        ExprKind::Paren(e) => visitor.visit_expr(e),
        ExprKind::TypeAsExpr(t) => visitor.visit_type_expr(t),
        ExprKind::TryUnwrap(e) => visitor.visit_expr(e),
    }
}

/// Walk a type expression.
pub fn walk_type_expr<V: Visitor>(visitor: &mut V, ty: &TypeExpr) {
    match &ty.kind {
        TypeExprKind::Ident(i) => visitor.visit_ident(i),
        TypeExprKind::Selector(s) => {
            visitor.visit_ident(&s.pkg);
            visitor.visit_ident(&s.sel);
        }
        TypeExprKind::Array(a) => {
            visitor.visit_expr(&a.len);
            visitor.visit_type_expr(&a.elem);
        }
        TypeExprKind::Slice(s) => visitor.visit_type_expr(s),
        TypeExprKind::Map(m) => {
            visitor.visit_type_expr(&m.key);
            visitor.visit_type_expr(&m.value);
        }
        TypeExprKind::Chan(c) => visitor.visit_type_expr(&c.elem),
        TypeExprKind::Func(f) => {
            for p in &f.params {
                visitor.visit_type_expr(p);
            }
            for r in &f.results {
                visitor.visit_type_expr(r);
            }
        }
        TypeExprKind::Struct(s) => {
            for field in &s.fields {
                visitor.visit_type_expr(&field.ty);
            }
        }
        TypeExprKind::Pointer(inner) => {
            visitor.visit_type_expr(inner);
        }
        TypeExprKind::Interface(i) => {
            for elem in &i.elems {
                match elem {
                    InterfaceElem::Method(m) => {
                        visitor.visit_ident(&m.name);
                    }
                    InterfaceElem::Embedded(e) => {
                        visitor.visit_ident(e);
                    }
                }
            }
        }
    }
}
