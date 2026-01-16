//! Detra Abstract Syntax Tree definitions.

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub types: Vec<TypeDecl>,
    pub state: StateDecl,
    pub actions: Vec<ActionDecl>,
    pub rules: Vec<RuleDecl>,
    pub views: Vec<ViewDecl>,
    pub commands: Vec<CommandDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDecl {
    pub name: String,
    pub fields: Vec<FieldDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDecl {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    Int,
    Float,
    String,
    Array(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Named(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StateDecl {
    pub name: String,
    pub fields: Vec<StateField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StateField {
    pub modifier: FieldModifier,
    pub name: String,
    pub ty: Type,
    pub default: Option<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FieldModifier {
    None,
    Const,
    External,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActionDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub body: Vec<ActionStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    pub default: Option<Literal>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ActionStmt {
    Require(Expr),
    Set { path: Path, value: Expr },
    Emit { command: String, args: Vec<Arg> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuleDecl {
    pub name: String,
    pub body: Vec<RuleStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuleStmt {
    Derive { path: Path, value: Expr },
    Check { expr: Expr, message: Option<String> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ViewDecl {
    pub name: String,
    pub body: Node,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CommandDecl {
    pub name: String,
    pub params: Vec<Param>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub kind: String,
    pub props: Vec<Prop>,
    pub children: Vec<ViewChild>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prop {
    pub name: String,
    pub value: PropValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropValue {
    Expr(Expr),
    ActionRef(ActionRef),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActionRef {
    pub name: String,
    pub args: Vec<Arg>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ViewChild {
    Node(Node),
    If(ViewIf),
    For(Comprehension),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ViewIf {
    pub cond: Expr,
    pub then_body: Vec<ViewChild>,
    pub else_body: Option<Vec<ViewChild>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Comprehension {
    pub binding: Binding,
    pub source: Expr,
    pub filters: Vec<Expr>,
    pub sorts: Vec<SortClause>,
    pub body: Node,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    Single(String),
    Pair(String, String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SortClause {
    pub expr: Expr,
    pub desc: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path {
    pub segments: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Path(Path),
    EventVar(String),
    Binary { op: BinOp, left: Box<Expr>, right: Box<Expr> },
    Unary { op: UnaryOp, expr: Box<Expr> },
    Call { func: String, args: Vec<Expr> },
    Index { expr: Box<Expr>, index: Box<Expr> },
    Field { expr: Box<Expr>, field: String },
    ArrayLit(Vec<Expr>),
    MapLit(Vec<(Expr, Expr)>),
    StructLit { ty: String, fields: Vec<(String, Expr)> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
}
