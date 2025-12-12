//! Abstract Syntax Tree types for GoX.
//!
//! Based on GoX Language Specification v2.2.
//!
//! The AST is organized into:
//! - **File structure**: Package, imports, top-level declarations
//! - **Declarations**: var, const, type, func, interface, implements
//! - **Types**: Named, array, slice, map, func, struct
//! - **Statements**: Block, control flow, assignments
//! - **Expressions**: Literals, binary, unary, calls, composite literals

use gox_common::Span;

// ═══════════════════════════════════════════════════════════════════════════
// File Structure (§4)
// ═══════════════════════════════════════════════════════════════════════════

/// A complete GoX source file.
#[derive(Debug, Clone)]
pub struct File {
    pub package: Option<PackageClause>,
    pub imports: Vec<ImportDecl>,
    pub decls: Vec<TopDecl>,
    pub span: Span,
}

/// Package clause: `package name;`
#[derive(Debug, Clone)]
pub struct PackageClause {
    pub name: Ident,
    pub span: Span,
}

/// Import declaration: `import "path";`
#[derive(Debug, Clone)]
pub struct ImportDecl {
    pub path: String,
    pub span: Span,
}

// ═══════════════════════════════════════════════════════════════════════════
// Top-Level Declarations (§4.2, §5)
// ═══════════════════════════════════════════════════════════════════════════

/// Top-level declarations.
#[derive(Debug, Clone)]
pub enum TopDecl {
    Var(VarDecl),
    Const(ConstDecl),
    Type(TypeDecl),
    Interface(InterfaceDecl),
    Implements(ImplementsDecl),
    Func(FuncDecl),
}

impl TopDecl {
    pub fn span(&self) -> Span {
        match self {
            TopDecl::Var(d) => d.span,
            TopDecl::Const(d) => d.span,
            TopDecl::Type(d) => d.span,
            TopDecl::Interface(d) => d.span,
            TopDecl::Implements(d) => d.span,
            TopDecl::Func(d) => d.span,
        }
    }
}

/// Variable declaration: `var x int = 1;` (§5.1)
#[derive(Debug, Clone)]
pub struct VarDecl {
    pub specs: Vec<VarSpec>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct VarSpec {
    pub name: Ident,
    pub ty: Option<Type>,
    pub value: Option<Expr>,
    pub span: Span,
}

/// Constant declaration: `const PI = 3.14;` (§5.2)
#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub specs: Vec<ConstSpec>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ConstSpec {
    pub name: Ident,
    pub ty: Option<Type>,
    pub value: Expr,
    pub span: Span,
}

/// Type declaration: `type User struct { ... };` (§5.4)
#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: Ident,
    pub ty: Type,
    pub span: Span,
}

/// Interface declaration (§7.1)
#[derive(Debug, Clone)]
pub struct InterfaceDecl {
    pub name: Ident,
    pub elements: Vec<InterfaceElem>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum InterfaceElem {
    Method(MethodSpec),
    Embedded(Ident),
}

#[derive(Debug, Clone)]
pub struct MethodSpec {
    pub name: Ident,
    pub params: Vec<Param>,
    pub result: Option<ResultType>,
    pub span: Span,
}

/// Implements declaration: `implements User : Reader, Writer;` (§7.5)
#[derive(Debug, Clone)]
pub struct ImplementsDecl {
    pub type_name: Ident,
    pub interfaces: Vec<Ident>,
    pub span: Span,
}

/// Function declaration (§7.4)
#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub receiver: Option<Receiver>,
    pub name: Ident,
    pub params: Vec<Param>,
    pub result: Option<ResultType>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Receiver {
    pub name: Ident,
    pub ty: Ident, // Must be a named type (§7.4)
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Ident,
    pub ty: Type,
    pub span: Span,
}

/// Result type: single type or tuple of types
#[derive(Debug, Clone)]
pub enum ResultType {
    Single(Type),
    Tuple(Vec<Type>, Span),
}

impl ResultType {
    pub fn span(&self) -> Span {
        match self {
            ResultType::Single(t) => t.span(),
            ResultType::Tuple(_, s) => *s,
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Types (§6)
// ═══════════════════════════════════════════════════════════════════════════

/// Type representations.
#[derive(Debug, Clone)]
pub enum Type {
    /// Named type (e.g., `int`, `User`)
    Named(Ident),
    /// Array type: `[N]T`
    Array(Box<ArrayType>),
    /// Slice type: `[]T`
    Slice(Box<SliceType>),
    /// Map type: `map[K]V`
    Map(Box<MapType>),
    /// Function type: `func(T) R`
    Func(Box<FuncType>),
    /// Inline struct type
    Struct(Box<StructType>),
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Named(id) => id.span,
            Type::Array(t) => t.span,
            Type::Slice(t) => t.span,
            Type::Map(t) => t.span,
            Type::Func(t) => t.span,
            Type::Struct(t) => t.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrayType {
    pub len: i64,
    pub elem: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SliceType {
    pub elem: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MapType {
    pub key: Type,
    pub value: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FuncType {
    pub params: Vec<Type>,
    pub result: Option<Box<ResultType>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub fields: Vec<FieldDecl>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldDecl {
    pub name: Ident,
    pub ty: Type,
    pub tag: Option<String>,
    pub span: Span,
}

// ═══════════════════════════════════════════════════════════════════════════
// Statements (§8)
// ═══════════════════════════════════════════════════════════════════════════

/// Statements.
#[derive(Debug, Clone)]
pub enum Stmt {
    /// Block statement
    Block(Block),
    /// Variable declaration
    Var(VarDecl),
    /// Constant declaration
    Const(ConstDecl),
    /// Short variable declaration: `x := 1`
    ShortVar(ShortVarDecl),
    /// Assignment: `x = 1` or `x += 1`
    Assign(Assignment),
    /// Expression statement
    Expr(ExprStmt),
    /// Return statement
    Return(ReturnStmt),
    /// If statement
    If(Box<IfStmt>),
    /// For statement
    For(Box<ForStmt>),
    /// Switch statement
    Switch(Box<SwitchStmt>),
    /// Break statement
    Break(Span),
    /// Continue statement
    Continue(Span),
    /// Empty statement (`;`)
    Empty(Span),
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Block(b) => b.span,
            Stmt::Var(d) => d.span,
            Stmt::Const(d) => d.span,
            Stmt::ShortVar(d) => d.span,
            Stmt::Assign(a) => a.span,
            Stmt::Expr(e) => e.span,
            Stmt::Return(r) => r.span,
            Stmt::If(i) => i.span,
            Stmt::For(f) => f.span,
            Stmt::Switch(s) => s.span,
            Stmt::Break(s) => *s,
            Stmt::Continue(s) => *s,
            Stmt::Empty(s) => *s,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

/// Short variable declaration: `x := expr` (§5.3)
#[derive(Debug, Clone)]
pub struct ShortVarDecl {
    pub names: Vec<Ident>,
    pub values: Vec<Expr>,
    pub span: Span,
}

/// Assignment statement (§8.3)
#[derive(Debug, Clone)]
pub struct Assignment {
    pub left: Vec<Expr>,
    pub op: AssignOp,
    pub right: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    Assign,        // =
    PlusAssign,    // +=
    MinusAssign,   // -=
    StarAssign,    // *=
    SlashAssign,   // /=
    PercentAssign, // %=
}

/// Expression statement
#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
    pub span: Span,
}

/// Return statement (§8.4)
#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub values: Vec<Expr>,
    pub span: Span,
}

/// If statement (§8.5)
#[derive(Debug, Clone)]
pub struct IfStmt {
    pub cond: Expr,
    pub then_block: Block,
    pub else_clause: Option<ElseClause>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ElseClause {
    Block(Block),
    If(Box<IfStmt>),
}

/// For statement (§8.6)
#[derive(Debug, Clone)]
pub struct ForStmt {
    pub init: Option<Box<Stmt>>,
    pub cond: Option<Expr>,
    pub post: Option<Box<Stmt>>,
    pub body: Block,
    pub span: Span,
}

/// Switch statement (§8.7)
#[derive(Debug, Clone)]
pub struct SwitchStmt {
    pub expr: Expr,
    pub cases: Vec<CaseClause>,
    pub default: Option<DefaultClause>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CaseClause {
    pub exprs: Vec<Expr>,
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct DefaultClause {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

// ═══════════════════════════════════════════════════════════════════════════
// Expressions (§9)
// ═══════════════════════════════════════════════════════════════════════════

/// Expressions.
#[derive(Debug, Clone)]
pub enum Expr {
    /// Identifier
    Ident(Ident),
    /// Literal value
    Literal(Literal),
    /// Binary expression: `a + b`
    Binary(Box<BinaryExpr>),
    /// Unary expression: `-x`, `!x`
    Unary(Box<UnaryExpr>),
    /// Function call: `f(x, y)`
    Call(Box<CallExpr>),
    /// Index expression: `a[i]`
    Index(Box<IndexExpr>),
    /// Selector expression: `a.b`
    Selector(Box<SelectorExpr>),
    /// Composite literal: `T{...}`
    CompositeLit(Box<CompositeLit>),
    /// Grouped expression: `(expr)`
    Grouped(Box<Expr>, Span),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Ident(id) => id.span,
            Expr::Literal(lit) => lit.span(),
            Expr::Binary(b) => b.span,
            Expr::Unary(u) => u.span,
            Expr::Call(c) => c.span,
            Expr::Index(i) => i.span,
            Expr::Selector(s) => s.span,
            Expr::CompositeLit(c) => c.span,
            Expr::Grouped(_, s) => *s,
        }
    }
}

/// Identifier
#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl Ident {
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }
}

/// Literal values
#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64, Span),
    Float(f64, Span),
    String(String, Span),
    Bool(bool, Span),
    Nil(Span),
}

impl Literal {
    pub fn span(&self) -> Span {
        match self {
            Literal::Int(_, s) => *s,
            Literal::Float(_, s) => *s,
            Literal::String(_, s) => *s,
            Literal::Bool(_, s) => *s,
            Literal::Nil(s) => *s,
        }
    }
}

/// Binary expression
#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Expr,
    pub op: BinaryOp,
    pub right: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
    // Comparison
    Eq,    // ==
    NotEq, // !=
    Lt,    // <
    LtEq,  // <=
    Gt,    // >
    GtEq,  // >=
    // Logical
    And, // &&
    Or,  // ||
}

/// Unary expression
#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg, // -
    Not, // !
    Pos, // + (rarely used)
}

/// Function call expression
#[derive(Debug, Clone)]
pub struct CallExpr {
    pub func: Expr,
    pub args: Vec<Expr>,
    pub span: Span,
}

/// Index expression: `a[i]`
#[derive(Debug, Clone)]
pub struct IndexExpr {
    pub expr: Expr,
    pub index: Expr,
    pub span: Span,
}

/// Selector expression: `a.b`
#[derive(Debug, Clone)]
pub struct SelectorExpr {
    pub expr: Expr,
    pub field: Ident,
    pub span: Span,
}

/// Composite literal (§9.4)
#[derive(Debug, Clone)]
pub struct CompositeLit {
    pub ty: Type,
    pub elements: Vec<Element>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Element {
    pub key: Option<ElementKey>,
    pub value: Expr,
    pub span: Span,
}

/// Element key in composite literal
#[derive(Debug, Clone)]
pub enum ElementKey {
    /// Field name (for structs)
    Name(Ident),
    /// Expression (for maps, arrays)
    Expr(Expr),
}
