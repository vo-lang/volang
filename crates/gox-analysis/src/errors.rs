//! Centralized diagnostic definitions for the GoX type checker.
//!
//! All type checker error codes are defined here for easy reference and testing.
//!
//! With global position space, all error reporting methods use `Span` directly
//! without requiring a separate `FileId`.

use gox_common::{Diagnostic, Label, Span};

/// Type checker error codes (2xxx range).
///
/// Error code ranges:
/// - 2000-2099: Collection phase errors
/// - 2100-2199: Resolution phase errors
/// - 2200-2299: General type errors
/// - 2300-2399: Operator errors
/// - 2400-2499: Builtin function errors
/// - 2500-2599: Index/slice errors
/// - 2600-2699: Assignment errors
/// - 2700-2799: Control flow errors
/// - 2800-2899: Channel errors
/// - 2900-2999: Composite literal / interface errors
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum TypeError {
    // === Collection Phase (2000-2099) ===
    /// Identifier redeclared in the same block.
    Redeclared = 2000,

    // === Resolution Phase (2100-2199) ===
    /// Undefined identifier.
    Undefined = 2100,
    /// Identifier is not a type.
    NotAType = 2101,
    /// Invalid recursive type definition.
    InvalidRecursiveType = 2102,
    /// Invalid map key type (not comparable).
    InvalidMapKey = 2103,
    /// Undefined receiver type for method.
    UndefinedReceiverType = 2104,
    /// Array length must be non-negative.
    NegativeArrayLength = 2105,
    /// Array length must be a constant integer.
    NonConstantArrayLength = 2106,

    // === General Type Errors (2200-2299) ===
    /// Identifier is a type, not a value.
    TypeNotValue = 2200,
    /// Cannot call non-function.
    NotCallable = 2201,
    /// Wrong number of arguments in function call.
    WrongArgCount = 2202,
    /// Argument type mismatch.
    ArgTypeMismatch = 2203,
    /// Cannot use type as return value.
    ReturnTypeMismatch = 2204,
    /// Wrong number of return values.
    WrongReturnCount = 2205,
    /// Bare return in function without named returns.
    BareReturnNotAllowed = 2206,
    /// Cannot convert type.
    CannotConvert = 2207,

    // === Operator Errors (2300-2399) ===
    /// Operator requires numeric operands.
    NumericOperandRequired = 2300,
    /// Operator requires integer operands.
    IntegerOperandRequired = 2301,
    /// Operator requires boolean operands.
    BooleanOperandRequired = 2302,
    /// Operator requires ordered operands.
    OrderedOperandRequired = 2303,
    /// Cannot compare these types.
    NotComparable = 2304,
    /// Shift count must be unsigned integer.
    ShiftCountUnsigned = 2305,
    /// Shift operand must be integer.
    ShiftOperandInteger = 2306,
    /// Unary - requires numeric operand.
    UnaryNegNumeric = 2310,
    /// Unary ! requires boolean operand.
    UnaryNotBoolean = 2311,
    /// Unary ^ requires integer operand.
    UnaryBitNotInteger = 2312,
    /// Unary + requires numeric operand.
    UnaryPosNumeric = 2313,

    // === Builtin Function Errors (2400-2499) ===
    /// len requires exactly one argument.
    LenArgCount = 2400,
    /// len argument must be string, array, slice, map, or channel.
    LenArgType = 2401,
    /// cap requires exactly one argument.
    CapArgCount = 2402,
    /// cap argument must be array, slice, or channel.
    CapArgType = 2403,
    /// make requires 1-3 arguments.
    MakeArgCount = 2404,
    /// make first argument must be slice, map, or channel type.
    MakeArgType = 2405,
    /// append requires at least 2 arguments.
    AppendArgCount = 2406,
    /// append first argument must be slice.
    AppendArgType = 2407,
    /// copy requires exactly 2 arguments.
    CopyArgCount = 2408,
    /// copy arguments must be slices.
    CopyArgType = 2409,
    /// delete requires exactly 2 arguments.
    DeleteArgCount = 2410,
    /// delete first argument must be map.
    DeleteArgType = 2411,
    /// close requires exactly one argument.
    CloseArgCount = 2412,
    /// close argument must be channel.
    CloseArgType = 2413,
    /// panic requires exactly one argument.
    PanicArgCount = 2414,
    /// recover requires no arguments.
    RecoverArgCount = 2415,
    /// new requires exactly one argument.
    NewArgCount = 2416,
    /// new argument must be a type.
    NewArgType = 2417,
    /// print/println argument count.
    PrintArgCount = 2418,

    // === Index/Slice Errors (2500-2599) ===
    /// Cannot index this type.
    NotIndexable = 2500,
    /// Index must be integer.
    IndexNotInteger = 2501,
    /// Cannot slice this type.
    NotSliceable = 2502,
    /// Slice index must be integer.
    SliceIndexNotInteger = 2503,
    /// No field or method with this name.
    NoFieldOrMethod = 2510,
    /// Cannot select from this type.
    CannotSelect = 2511,

    // === Assignment Errors (2600-2699) ===
    /// Cannot assign to expression.
    NotAssignable = 2600,
    /// Incompatible types in assignment.
    AssignTypeMismatch = 2601,
    /// Wrong number of values in assignment.
    AssignCountMismatch = 2602,
    /// Incompatible type in variable initialization.
    VarInitTypeMismatch = 2603,
    /// No new variables in short declaration.
    NoNewVarsInShortDecl = 2604,

    // === Control Flow Errors (2700-2799) ===
    /// Non-boolean condition.
    NonBoolCondition = 2700,
    /// Range over non-iterable type.
    NotIterable = 2701,
    /// Type assertion on non-interface.
    TypeAssertNonInterface = 2702,
    /// Impossible type assertion.
    ImpossibleTypeAssertion = 2703,

    // === Channel Errors (2800-2899) ===
    /// Cannot send on receive-only channel.
    SendOnReceiveOnly = 2800,
    /// Cannot receive from send-only channel.
    ReceiveFromSendOnly = 2801,
    /// Send value type mismatch.
    SendTypeMismatch = 2802,
    /// Cannot receive from non-channel.
    ReceiveNonChannel = 2803,

    // === Composite Literal / Interface Errors (2900-2999) ===
    /// Unknown field in struct literal.
    UnknownField = 2900,
    /// Duplicate field in struct literal.
    DuplicateField = 2901,
    /// Mixed field styles in struct literal.
    MixedFieldStyles = 2902,
    /// Field type mismatch in struct literal.
    FieldTypeMismatch = 2903,
    /// Element type mismatch in composite literal.
    ElementTypeMismatch = 2904,
    /// Key type mismatch in map literal.
    KeyTypeMismatch = 2905,
    /// Invalid composite literal type.
    InvalidCompositeLitType = 2906,
    /// Type does not implement interface.
    NotImplemented = 2950,
    /// Missing method for interface.
    MissingMethod = 2951,
}

impl TypeError {
    /// Returns the numeric error code.
    pub fn code(self) -> u16 {
        self as u16
    }

    /// Returns the error message.
    pub fn message(self) -> &'static str {
        match self {
            // Collection Phase
            TypeError::Redeclared => "redeclared in this block",

            // Resolution Phase
            TypeError::Undefined => "undefined",
            TypeError::NotAType => "is not a type",
            TypeError::InvalidRecursiveType => "invalid recursive type",
            TypeError::InvalidMapKey => "invalid map key type: must be comparable",
            TypeError::UndefinedReceiverType => "undefined receiver type",
            TypeError::NegativeArrayLength => "array length must be non-negative",
            TypeError::NonConstantArrayLength => "array length must be a constant integer",

            // General Type Errors
            TypeError::TypeNotValue => "is a type, not a value",
            TypeError::NotCallable => "cannot call non-function",
            TypeError::WrongArgCount => "wrong number of arguments",
            TypeError::ArgTypeMismatch => "argument type mismatch",
            TypeError::ReturnTypeMismatch => "cannot use type as return value",
            TypeError::CannotConvert => "cannot convert",
            TypeError::WrongReturnCount => "wrong number of return values",
            TypeError::BareReturnNotAllowed => "bare return in function without named returns",

            // Operator Errors
            TypeError::NumericOperandRequired => "operator requires numeric operands",
            TypeError::IntegerOperandRequired => "operator requires integer operands",
            TypeError::BooleanOperandRequired => "operator requires boolean operands",
            TypeError::OrderedOperandRequired => "operator requires ordered operands",
            TypeError::NotComparable => "cannot compare these types",
            TypeError::ShiftCountUnsigned => "shift count must be unsigned integer",
            TypeError::ShiftOperandInteger => "shift operand must be integer",
            TypeError::UnaryNegNumeric => "unary - requires numeric operand",
            TypeError::UnaryNotBoolean => "unary ! requires boolean operand",
            TypeError::UnaryBitNotInteger => "unary ^ requires integer operand",
            TypeError::UnaryPosNumeric => "unary + requires numeric operand",

            // Builtin Function Errors
            TypeError::LenArgCount => "len requires exactly one argument",
            TypeError::LenArgType => "argument to len must be string, array, slice, map, or channel",
            TypeError::CapArgCount => "cap requires exactly one argument",
            TypeError::CapArgType => "argument to cap must be array, slice, or channel",
            TypeError::MakeArgCount => "make requires 1-3 arguments",
            TypeError::MakeArgType => "first argument to make must be slice, map, or channel type",
            TypeError::AppendArgCount => "append requires at least 2 arguments",
            TypeError::AppendArgType => "first argument to append must be slice",
            TypeError::CopyArgCount => "copy requires exactly 2 arguments",
            TypeError::CopyArgType => "arguments to copy must be slices",
            TypeError::DeleteArgCount => "delete requires exactly 2 arguments",
            TypeError::DeleteArgType => "first argument to delete must be map",
            TypeError::CloseArgCount => "close requires exactly one argument",
            TypeError::CloseArgType => "argument to close must be channel",
            TypeError::PanicArgCount => "panic requires exactly one argument",
            TypeError::RecoverArgCount => "recover takes no arguments",
            TypeError::NewArgCount => "new requires exactly one argument",
            TypeError::NewArgType => "argument to new must be a type",
            TypeError::PrintArgCount => "print/println requires at least one argument",

            // Index/Slice Errors
            TypeError::NotIndexable => "cannot index this type",
            TypeError::IndexNotInteger => "index must be integer",
            TypeError::NotSliceable => "cannot slice this type",
            TypeError::SliceIndexNotInteger => "slice index must be integer",
            TypeError::NoFieldOrMethod => "no field or method",
            TypeError::CannotSelect => "cannot select from this type",

            // Assignment Errors
            TypeError::NotAssignable => "cannot assign to expression",
            TypeError::AssignTypeMismatch => "incompatible types in assignment",
            TypeError::AssignCountMismatch => "wrong number of values in assignment",
            TypeError::VarInitTypeMismatch => "incompatible type in variable initialization",
            TypeError::NoNewVarsInShortDecl => "no new variables on left side of :=",

            // Control Flow Errors
            TypeError::NonBoolCondition => "non-boolean condition",
            TypeError::NotIterable => "cannot range over this type",
            TypeError::TypeAssertNonInterface => "type assertion on non-interface type",
            TypeError::ImpossibleTypeAssertion => "impossible type assertion",

            // Channel Errors
            TypeError::SendOnReceiveOnly => "cannot send on receive-only channel",
            TypeError::ReceiveFromSendOnly => "cannot receive from send-only channel",
            TypeError::SendTypeMismatch => "send value type mismatch",
            TypeError::ReceiveNonChannel => "cannot receive from non-channel",

            // Composite Literal / Interface Errors
            TypeError::UnknownField => "unknown field in struct literal",
            TypeError::DuplicateField => "duplicate field in struct literal",
            TypeError::MixedFieldStyles => "cannot mix field:value and value styles",
            TypeError::FieldTypeMismatch => "field type mismatch",
            TypeError::ElementTypeMismatch => "element type mismatch",
            TypeError::KeyTypeMismatch => "key type mismatch",
            TypeError::InvalidCompositeLitType => "invalid type for composite literal",
            TypeError::NotImplemented => "type does not implement interface",
            TypeError::MissingMethod => "missing method",
        }
    }

    /// Creates a diagnostic with this error code (no location).
    pub fn diagnostic(self) -> Diagnostic {
        Diagnostic::error(self.message()).with_code(self.code())
    }

    /// Creates a diagnostic with this error code and a custom message.
    pub fn with_message(self, message: impl Into<String>) -> Diagnostic {
        Diagnostic::error(message).with_code(self.code())
    }

    /// Creates a diagnostic with this error code and a span label.
    /// Uses global position - no FileId needed.
    pub fn at(self, span: impl Into<Span>) -> Diagnostic {
        Diagnostic::error(self.message())
            .with_code(self.code())
            .with_label(Label::primary(span))
    }

    /// Creates a diagnostic with this error code, custom message, and span.
    pub fn at_with_message(self, span: impl Into<Span>, message: impl Into<String>) -> Diagnostic {
        Diagnostic::error(message)
            .with_code(self.code())
            .with_label(Label::primary(span))
    }

    /// Creates a formatted message with a name (for errors that reference identifiers).
    pub fn with_name(self, name: &str) -> String {
        match self {
            TypeError::Redeclared => format!("{} redeclared in this block", name),
            TypeError::Undefined => format!("undefined: {}", name),
            TypeError::NotAType => format!("{} is not a type", name),
            TypeError::InvalidRecursiveType => format!("invalid recursive type {}", name),
            TypeError::UndefinedReceiverType => format!("undefined receiver type: {}", name),
            TypeError::TypeNotValue => format!("{} is a type, not a value", name),
            TypeError::NoFieldOrMethod => format!("no field or method {}", name),
            TypeError::UnknownField => format!("unknown field {} in struct literal", name),
            TypeError::DuplicateField => format!("duplicate field {} in struct literal", name),
            TypeError::MissingMethod => format!("missing method {}", name),
            _ => self.message().to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_codes() {
        assert_eq!(TypeError::Redeclared.code(), 2000);
        assert_eq!(TypeError::Undefined.code(), 2100);
        assert_eq!(TypeError::TypeNotValue.code(), 2200);
        assert_eq!(TypeError::NumericOperandRequired.code(), 2300);
        assert_eq!(TypeError::LenArgCount.code(), 2400);
        assert_eq!(TypeError::NotIndexable.code(), 2500);
        assert_eq!(TypeError::NotAssignable.code(), 2600);
        assert_eq!(TypeError::NonBoolCondition.code(), 2700);
        assert_eq!(TypeError::SendOnReceiveOnly.code(), 2800);
        assert_eq!(TypeError::UnknownField.code(), 2900);
        assert_eq!(TypeError::NotImplemented.code(), 2950);
    }

    #[test]
    fn test_diagnostic_creation() {
        let diag = TypeError::Undefined.diagnostic();
        assert_eq!(diag.code, Some(2100));
        assert_eq!(diag.message, "undefined");
    }

    #[test]
    fn test_with_name() {
        assert_eq!(TypeError::Undefined.with_name("foo"), "undefined: foo");
        assert_eq!(TypeError::Redeclared.with_name("x"), "x redeclared in this block");
    }

    #[test]
    fn test_at_with_span() {
        let diag = TypeError::Undefined.at(10u32..20u32);
        assert_eq!(diag.code, Some(2100));
        assert_eq!(diag.labels.len(), 1);
        assert_eq!(diag.labels[0].span.start.0, 10);
    }
}
