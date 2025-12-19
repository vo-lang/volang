//! Code generation errors.

use gox_common::Span;

#[derive(Debug)]
pub struct CodegenError {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ErrorKind {
    UndefinedSymbol(String),
    TypeMismatch { expected: String, got: String },
    NotCallable,
    InvalidAssignTarget,
    InvalidBreak,
    InvalidContinue,
    TooManyLocals,
    TooManyConstants,
    TooManyFunctions,
    InternalError(String),
}

impl CodegenError {
    pub fn new(kind: ErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn undefined(name: &str, span: Span) -> Self {
        Self::new(ErrorKind::UndefinedSymbol(name.to_string()), span)
    }

    pub fn internal(msg: &str, span: Span) -> Self {
        Self::new(ErrorKind::InternalError(msg.to_string()), span)
    }
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ErrorKind::UndefinedSymbol(name) => write!(f, "undefined: {}", name),
            ErrorKind::TypeMismatch { expected, got } => {
                write!(f, "type mismatch: expected {}, got {}", expected, got)
            }
            ErrorKind::NotCallable => write!(f, "expression is not callable"),
            ErrorKind::InvalidAssignTarget => write!(f, "invalid assignment target"),
            ErrorKind::InvalidBreak => write!(f, "break outside loop"),
            ErrorKind::InvalidContinue => write!(f, "continue outside loop"),
            ErrorKind::TooManyLocals => write!(f, "too many local variables"),
            ErrorKind::TooManyConstants => write!(f, "too many constants"),
            ErrorKind::TooManyFunctions => write!(f, "too many functions"),
            ErrorKind::InternalError(msg) => write!(f, "internal error: {}", msg),
        }
    }
}

impl std::error::Error for CodegenError {}

pub type Result<T> = std::result::Result<T, CodegenError>;
