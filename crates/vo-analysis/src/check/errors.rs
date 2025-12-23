//! Centralized diagnostic definitions for the Vo type checker.
//!
//! All type checker error codes are defined here for easy reference and testing.
//!
//! Error code ranges:
//! - 1xxx: SyntaxError (lexer/parser) - defined in vo-syntax
//! - 2xxx: TypeError (type checker) - defined here

use vo_common::diagnostics::{Diagnostic, Label};
use vo_common::span::Span;

/// Type checker error codes (2xxx range).
///
/// Error code ranges:
/// - 2000-2099: Type/Assignment errors
/// - 2100-2199: Invalid operation errors
/// - 2200-2299: Declaration/Scope errors
/// - 2300-2399: Function call errors
/// - 2400-2499: Type expression errors
/// - 2500-2599: Statement errors
/// - 2600-2699: Builtin function errors
/// - 2700-2799: Import/Package errors
/// - 2800-2899: Label errors
/// - 2900-2999: Warnings (soft errors)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum TypeError {
    // === Type/Assignment (2000-2099) ===
    /// Type mismatch in assignment or expression.
    TypeMismatch = 2000,
    /// Cannot assign to expression.
    CannotAssign = 2001,
    /// Use of untyped nil.
    UseOfUntypedNil = 2002,
    /// Value is not constant.
    NotConstant = 2003,
    /// Cannot assign to struct field in map.
    CannotAssignMapField = 2004,
    /// Non-name on left side of :=.
    NonNameInShortDecl = 2005,
    /// Assignment mismatch (different number of values).
    AssignmentMismatch = 2006,

    // === Invalid Operation (2100-2199) ===
    /// Invalid operation (generic).
    InvalidOp = 2100,
    /// Cannot index expression.
    CannotIndex = 2101,
    /// Cannot slice expression.
    CannotSlice = 2102,
    /// Cannot slice array (value not addressable).
    CannotSliceArray = 2103,
    /// Division by zero.
    DivisionByZero = 2104,
    /// Shifted operand must be integer.
    ShiftOperandNotInteger = 2105,
    /// Shift count must be unsigned integer.
    ShiftCountNotUnsigned = 2106,
    /// Shift count must not be negative.
    ShiftCountNegative = 2107,
    /// Invalid shift count.
    InvalidShiftCount = 2108,
    /// Cannot take address of expression.
    CannotTakeAddress = 2109,
    /// Cannot dereference non-pointer.
    CannotDereference = 2110,
    /// Type assertion requires interface type.
    TypeAssertNotInterface = 2111,
    /// Use of .(type) outside type switch.
    TypeSwitchOutsideSwitch = 2112,
    /// Invalid composite literal type.
    InvalidCompositeLitType = 2113,
    /// Operator not defined for operand.
    OperatorNotDefined = 2114,

    // === Declaration/Scope (2200-2299) ===
    /// Undeclared name.
    Undeclared = 2200,
    /// Redeclared in this block.
    Redeclared = 2201,
    /// Illegal cycle in declaration.
    IllegalCycle = 2202,
    /// Initialization cycle.
    InitCycle = 2203,
    /// Cannot use _ as value or type.
    BlankAsValue = 2204,
    /// Cannot use iota outside constant declaration.
    IotaOutsideConst = 2205,
    /// Use of package not in selector.
    PackageNotInSelector = 2206,
    /// Invalid constant type.
    InvalidConstType = 2207,
    /// Other declaration (for cycle reporting).
    OtherDeclaration = 2208,
    /// Refers to (for cycle reporting).
    RefersTo = 2209,

    // === Function Call (2300-2399) ===
    /// Cannot call non-function.
    CannotCall = 2300,
    /// Too few arguments in call.
    TooFewArgs = 2301,
    /// Too many arguments in call.
    TooManyArgs = 2302,
    /// Missing argument in conversion.
    MissingConversionArg = 2303,
    /// Too many arguments in conversion.
    TooManyConversionArgs = 2304,
    /// Cannot use ... in call to non-variadic function.
    SpreadNonVariadic = 2305,
    /// Cannot use ... with multi-valued expression.
    SpreadMultiValue = 2306,
    /// Can only use ... with matching parameter.
    SpreadMismatch = 2307,
    /// Cannot use value as variadic argument.
    InvalidVariadicArg = 2308,

    // === Type Expression (2400-2499) ===
    /// Not a type.
    NotAType = 2400,
    /// Expected type.
    ExpectedType = 2401,
    /// Used as type (but is not).
    UsedAsType = 2402,
    /// Array length must be constant.
    ArrayLenNotConstant = 2403,
    /// Array length must be non-negative integer.
    ArrayLenNotInteger = 2404,
    /// Method is missing receiver.
    MissingReceiver = 2405,
    /// Invalid receiver.
    InvalidReceiver = 2406,
    /// Embedded field type cannot be a pointer.
    EmbeddedPointer = 2407,
    /// Embedded field type cannot be pointer to interface.
    EmbeddedPointerInterface = 2408,
    /// Field redeclared.
    FieldRedeclared = 2409,

    // === Statement (2500-2599) ===
    /// Missing return.
    MissingReturn = 2500,
    /// No result values expected.
    UnexpectedReturn = 2501,
    /// Duplicate case in switch.
    DuplicateCase = 2502,
    /// Multiple defaults in switch.
    MultipleDefaults = 2503,
    /// Break not in for/switch/select.
    InvalidBreak = 2504,
    /// Continue not in for.
    InvalidContinue = 2505,
    /// Fallthrough out of place.
    InvalidFallthrough = 2506,
    /// Non-boolean condition.
    NonBoolCondition = 2507,
    /// Cannot send to receive-only channel.
    SendToRecvOnly = 2508,
    /// Cannot send to non-chan type.
    SendToNonChan = 2509,
    /// Cannot receive from send-only channel.
    RecvFromSendOnly = 2510,
    /// Cannot receive from non-chan type.
    RecvFromNonChan = 2511,
    /// Non-numeric operand for inc/dec.
    NonNumericIncDec = 2512,
    /// Missing lhs in assignment.
    MissingLhs = 2513,
    /// Compound assignment requires single-valued expressions.
    CompoundAssignMultiValue = 2514,
    /// Result parameter not in scope at return.
    ResultNotInScope = 2515,
    /// Cannot type switch on non-interface.
    TypeSwitchNonInterface = 2516,
    /// Builtin must be called.
    BuiltinMustBeCalled = 2517,
    /// Type is not an expression.
    TypeNotExpression = 2518,
    /// Previous case (for duplicate case reporting).
    PreviousCase = 2519,
    /// errdefer in function without error return.
    ErrDeferNoErrorReturn = 2520,

    // === Builtin Function (2600-2699) ===
    /// First argument to append must be slice.
    AppendNotSlice = 2600,
    /// Cannot use value in argument to append.
    AppendInvalidArg = 2601,
    /// Invalid argument for len/cap.
    InvalidLenCapArg = 2602,
    /// Cannot close receive-only channel.
    CloseRecvOnly = 2603,
    /// Argument to close must be channel.
    CloseNotChan = 2604,
    /// Copy expects slice arguments.
    CopyNotSlice = 2605,
    /// Arguments to copy have different element types.
    CopyTypeMismatch = 2606,
    /// First argument to delete must be map.
    DeleteNotMap = 2607,
    /// Key not assignable to map key type.
    DeleteKeyMismatch = 2608,
    /// Cannot make; type must be slice, map, or channel.
    MakeInvalidType = 2609,
    /// Make expects N arguments.
    MakeArgCount = 2610,
    /// Length larger than capacity.
    MakeLenGtCap = 2611,
    /// Argument to assert is not boolean.
    AssertNotBool = 2612,
    /// Assertion failed (compile-time).
    AssertFailed = 2613,
    /// Wrong number of arguments for builtin.
    BuiltinArgCount = 2614,

    // === Import/Package (2700-2799) ===
    /// Invalid import path.
    InvalidImportPath = 2700,
    /// Import cycle not allowed.
    ImportCycle = 2701,
    /// Cannot declare init - must be func.
    CannotDeclareInit = 2702,
    /// Cannot declare main - must be func.
    CannotDeclareMain = 2703,
    /// Invalid package name.
    InvalidPackageName = 2704,
    /// Package name mismatch.
    PackageNameMismatch = 2705,
    /// Missing function body.
    MissingFuncBody = 2706,
    /// Func init must have no arguments and no return values.
    InvalidInitSignature = 2707,
    /// Field and method with same name.
    FieldMethodConflict = 2708,
    /// Method already declared.
    MethodRedeclared = 2709,
    /// Unexpected function declaration in statement.
    UnexpectedFuncDecl = 2710,

    // === Label (2800-2899) ===
    /// Label not declared.
    LabelNotDeclared = 2800,
    /// Inner declaration shadows outer.
    LabelShadowed = 2801,

    // === Init Expression (2850-2899) ===
    /// Extra init expr.
    ExtraInitExpr = 2850,
    /// Missing init expr.
    MissingInitExpr = 2851,
    /// Missing type or init expr.
    MissingTypeOrInit = 2852,
    /// Missing value in const declaration.
    MissingConstValue = 2853,

    // === Warnings (2900-2999) ===
    /// Imported but not used.
    UnusedImport = 2900,
    /// Declared but not used.
    UnusedVar = 2901,
    /// No new variables on left side of :=.
    NoNewVars = 2902,
    /// Label declared but not used.
    UnusedLabel = 2903,
}

impl TypeError {
    /// Returns the numeric error code.
    #[inline]
    pub(crate) fn code(self) -> u16 {
        self as u16
    }

    /// Returns whether this is a warning (soft error).
    #[inline]
    pub(crate) fn is_warning(self) -> bool {
        self.code() >= 2900
    }

    /// Returns the default error message.
    pub(crate) fn message(self) -> &'static str {
        match self {
            // Type/Assignment
            TypeError::TypeMismatch => "type mismatch",
            TypeError::CannotAssign => "cannot assign to expression",
            TypeError::UseOfUntypedNil => "use of untyped nil",
            TypeError::NotConstant => "value is not constant",
            TypeError::CannotAssignMapField => "cannot assign to struct field in map",
            TypeError::NonNameInShortDecl => "non-name on left side of :=",
            TypeError::AssignmentMismatch => "assignment mismatch",

            // Invalid Operation
            TypeError::InvalidOp => "invalid operation",
            TypeError::CannotIndex => "cannot index expression",
            TypeError::CannotSlice => "cannot slice expression",
            TypeError::CannotSliceArray => "cannot slice array (value not addressable)",
            TypeError::DivisionByZero => "division by zero",
            TypeError::ShiftOperandNotInteger => "shifted operand must be integer",
            TypeError::ShiftCountNotUnsigned => "shift count must be unsigned integer",
            TypeError::ShiftCountNegative => "shift count must not be negative",
            TypeError::InvalidShiftCount => "invalid shift count",
            TypeError::CannotTakeAddress => "cannot take address of expression",
            TypeError::CannotDereference => "cannot dereference non-pointer",
            TypeError::TypeAssertNotInterface => "type assertion requires interface type",
            TypeError::TypeSwitchOutsideSwitch => "use of .(type) outside type switch",
            TypeError::InvalidCompositeLitType => "invalid composite literal type",
            TypeError::OperatorNotDefined => "operator not defined for operand",

            // Declaration/Scope
            TypeError::Undeclared => "undeclared name",
            TypeError::Redeclared => "redeclared in this block",
            TypeError::IllegalCycle => "illegal cycle in declaration",
            TypeError::InitCycle => "initialization cycle",
            TypeError::BlankAsValue => "cannot use _ as value or type",
            TypeError::IotaOutsideConst => "cannot use iota outside constant declaration",
            TypeError::PackageNotInSelector => "use of package not in selector",
            TypeError::InvalidConstType => "invalid constant type",
            TypeError::OtherDeclaration => "other declaration",
            TypeError::RefersTo => "refers to",

            // Function Call
            TypeError::CannotCall => "cannot call non-function",
            TypeError::TooFewArgs => "too few arguments in call",
            TypeError::TooManyArgs => "too many arguments",
            TypeError::MissingConversionArg => "missing argument in conversion",
            TypeError::TooManyConversionArgs => "too many arguments in conversion",
            TypeError::SpreadNonVariadic => "cannot use ... in call to non-variadic function",
            TypeError::SpreadMultiValue => "cannot use ... with multi-valued expression",
            TypeError::SpreadMismatch => "can only use ... with matching parameter",
            TypeError::InvalidVariadicArg => "cannot use value as variadic argument",

            // Type Expression
            TypeError::NotAType => "not a type",
            TypeError::ExpectedType => "expected type",
            TypeError::UsedAsType => "used as type",
            TypeError::ArrayLenNotConstant => "array length must be constant",
            TypeError::ArrayLenNotInteger => "array length must be a non-negative integer",
            TypeError::MissingReceiver => "method is missing receiver",
            TypeError::InvalidReceiver => "invalid receiver",
            TypeError::EmbeddedPointer => "embedded field type cannot be a pointer",
            TypeError::EmbeddedPointerInterface => "embedded field type cannot be a pointer to an interface",
            TypeError::FieldRedeclared => "field redeclared",

            // Statement
            TypeError::MissingReturn => "missing return",
            TypeError::UnexpectedReturn => "no result values expected",
            TypeError::DuplicateCase => "duplicate case",
            TypeError::MultipleDefaults => "multiple defaults",
            TypeError::InvalidBreak => "break not in for, switch, or select statement",
            TypeError::InvalidContinue => "continue not in for statement",
            TypeError::InvalidFallthrough => "fallthrough statement out of place",
            TypeError::NonBoolCondition => "non-boolean condition",
            TypeError::SendToRecvOnly => "cannot send to receive-only channel",
            TypeError::SendToNonChan => "cannot send to non-chan type",
            TypeError::RecvFromSendOnly => "cannot receive from send-only channel",
            TypeError::RecvFromNonChan => "cannot receive from non-chan type",
            TypeError::NonNumericIncDec => "non-numeric operand for inc/dec",
            TypeError::MissingLhs => "missing lhs in assignment",
            TypeError::CompoundAssignMultiValue => "assignment operation requires single-valued expressions",
            TypeError::ResultNotInScope => "result parameter not in scope at return",
            TypeError::TypeSwitchNonInterface => "cannot type switch on non-interface type",
            TypeError::BuiltinMustBeCalled => "builtin must be called",
            TypeError::TypeNotExpression => "type is not an expression",
            TypeError::PreviousCase => "previous case",

            // Builtin Function
            TypeError::AppendNotSlice => "first argument to append must be a slice",
            TypeError::AppendInvalidArg => "cannot use value in argument to append",
            TypeError::InvalidLenCapArg => "invalid argument for len/cap",
            TypeError::CloseRecvOnly => "cannot close receive-only channel",
            TypeError::CloseNotChan => "argument to close must be a channel",
            TypeError::CopyNotSlice => "copy expects slice arguments",
            TypeError::CopyTypeMismatch => "arguments to copy have different element types",
            TypeError::DeleteNotMap => "first argument to delete must be a map",
            TypeError::DeleteKeyMismatch => "key is not assignable to map key type",
            TypeError::MakeInvalidType => "cannot make; type must be slice, map, or channel",
            TypeError::MakeArgCount => "make expects wrong number of arguments",
            TypeError::MakeLenGtCap => "length larger than capacity",
            TypeError::AssertNotBool => "argument to assert is not a boolean",
            TypeError::AssertFailed => "assertion failed",
            TypeError::BuiltinArgCount => "wrong number of arguments for builtin",

            // Import/Package
            TypeError::InvalidImportPath => "invalid import path",
            TypeError::ImportCycle => "import cycle not allowed",
            TypeError::CannotDeclareInit => "cannot declare init - must be func",
            TypeError::CannotDeclareMain => "cannot declare main - must be func",
            TypeError::InvalidPackageName => "invalid package name",
            TypeError::PackageNameMismatch => "package name mismatch",
            TypeError::MissingFuncBody => "missing function body",
            TypeError::InvalidInitSignature => "func init must have no arguments and no return values",
            TypeError::FieldMethodConflict => "field and method with the same name",
            TypeError::MethodRedeclared => "method already declared",
            TypeError::UnexpectedFuncDecl => "unexpected function declaration in statement",

            // Label
            TypeError::LabelNotDeclared => "label not declared",
            TypeError::LabelShadowed => "inner declaration shadows outer",

            // Init Expression
            TypeError::ExtraInitExpr => "extra init expr",
            TypeError::MissingInitExpr => "missing init expr",
            TypeError::MissingTypeOrInit => "missing type or init expr",
            TypeError::MissingConstValue => "missing value in const declaration",

            // Warnings
            TypeError::UnusedImport => "imported but not used",
            TypeError::UnusedVar => "declared but not used",
            TypeError::NoNewVars => "no new variables on left side of :=",
            TypeError::UnusedLabel => "label declared but not used",
            
            // Errdefer
            TypeError::ErrDeferNoErrorReturn => "errdefer requires function with error return value",
        }
    }

    /// Creates a diagnostic with this error code (no location).
    pub(crate) fn diagnostic(self) -> Diagnostic {
        if self.is_warning() {
            Diagnostic::warning(self.message()).with_code(self.code())
        } else {
            Diagnostic::error(self.message()).with_code(self.code())
        }
    }

    /// Creates a diagnostic with this error code and a custom message.
    pub(crate) fn with_message(self, message: impl Into<String>) -> Diagnostic {
        if self.is_warning() {
            Diagnostic::warning(message).with_code(self.code())
        } else {
            Diagnostic::error(message).with_code(self.code())
        }
    }

    /// Creates a diagnostic with this error code and a span label.
    pub(crate) fn at(self, span: impl Into<Span>) -> Diagnostic {
        self.diagnostic().with_label(Label::primary(span))
    }

    /// Creates a diagnostic with this error code, custom message, and span.
    pub(crate) fn at_with_message(self, span: impl Into<Span>, message: impl Into<String>) -> Diagnostic {
        self.with_message(message).with_label(Label::primary(span))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_code_ranges() {
        // Type/Assignment: 2000-2099
        assert_eq!(TypeError::TypeMismatch.code(), 2000);
        assert_eq!(TypeError::AssignmentMismatch.code(), 2006);

        // Invalid Operation: 2100-2199
        assert_eq!(TypeError::InvalidOp.code(), 2100);
        assert_eq!(TypeError::OperatorNotDefined.code(), 2114);

        // Declaration/Scope: 2200-2299
        assert_eq!(TypeError::Undeclared.code(), 2200);
        assert_eq!(TypeError::RefersTo.code(), 2209);

        // Function Call: 2300-2399
        assert_eq!(TypeError::CannotCall.code(), 2300);
        assert_eq!(TypeError::InvalidVariadicArg.code(), 2308);

        // Type Expression: 2400-2499
        assert_eq!(TypeError::NotAType.code(), 2400);
        assert_eq!(TypeError::FieldRedeclared.code(), 2409);

        // Statement: 2500-2599
        assert_eq!(TypeError::MissingReturn.code(), 2500);
        assert_eq!(TypeError::PreviousCase.code(), 2519);

        // Builtin Function: 2600-2699
        assert_eq!(TypeError::AppendNotSlice.code(), 2600);
        assert_eq!(TypeError::BuiltinArgCount.code(), 2614);

        // Import/Package: 2700-2799
        assert_eq!(TypeError::InvalidImportPath.code(), 2700);
        assert_eq!(TypeError::UnexpectedFuncDecl.code(), 2710);

        // Label: 2800-2899
        assert_eq!(TypeError::LabelNotDeclared.code(), 2800);

        // Init Expression: 2850-2899
        assert_eq!(TypeError::ExtraInitExpr.code(), 2850);
        assert_eq!(TypeError::MissingConstValue.code(), 2853);

        // Warnings: 2900-2999
        assert_eq!(TypeError::UnusedImport.code(), 2900);
        assert_eq!(TypeError::UnusedLabel.code(), 2903);
    }

    #[test]
    fn test_is_warning() {
        // Errors should not be warnings
        assert!(!TypeError::TypeMismatch.is_warning());
        assert!(!TypeError::Undeclared.is_warning());
        assert!(!TypeError::MissingReturn.is_warning());

        // Warnings should be warnings
        assert!(TypeError::UnusedImport.is_warning());
        assert!(TypeError::UnusedVar.is_warning());
        assert!(TypeError::NoNewVars.is_warning());
        assert!(TypeError::UnusedLabel.is_warning());
    }

    #[test]
    fn test_diagnostic_creation() {
        let diag = TypeError::TypeMismatch.diagnostic();
        assert_eq!(diag.code, Some(2000));
        assert_eq!(diag.message, "type mismatch");
        assert!(diag.is_error());

        let diag = TypeError::UnusedImport.diagnostic();
        assert_eq!(diag.code, Some(2900));
        assert_eq!(diag.message, "imported but not used");
        assert!(diag.is_warning());
    }

    #[test]
    fn test_with_message() {
        let diag = TypeError::Undeclared.with_message("undeclared name: foo");
        assert_eq!(diag.code, Some(2200));
        assert_eq!(diag.message, "undeclared name: foo");
        assert!(diag.is_error());
    }

    #[test]
    fn test_at_with_span() {
        let diag = TypeError::TypeMismatch.at(10u32..20u32);
        assert_eq!(diag.code, Some(2000));
        assert_eq!(diag.labels.len(), 1);
        assert_eq!(diag.labels[0].span.start.0, 10);
        assert_eq!(diag.labels[0].span.end.0, 20);
    }

    #[test]
    fn test_at_with_message_and_span() {
        let diag = TypeError::Redeclared.at_with_message(5u32..15u32, "foo redeclared in this block");
        assert_eq!(diag.code, Some(2201));
        assert_eq!(diag.message, "foo redeclared in this block");
        assert_eq!(diag.labels.len(), 1);
    }

    #[test]
    fn test_all_errors_have_messages() {
        // Test that all error variants have non-empty messages
        let errors = [
            TypeError::TypeMismatch,
            TypeError::CannotAssign,
            TypeError::UseOfUntypedNil,
            TypeError::NotConstant,
            TypeError::CannotAssignMapField,
            TypeError::NonNameInShortDecl,
            TypeError::AssignmentMismatch,
            TypeError::InvalidOp,
            TypeError::CannotIndex,
            TypeError::CannotSlice,
            TypeError::CannotSliceArray,
            TypeError::DivisionByZero,
            TypeError::ShiftOperandNotInteger,
            TypeError::ShiftCountNotUnsigned,
            TypeError::ShiftCountNegative,
            TypeError::InvalidShiftCount,
            TypeError::CannotTakeAddress,
            TypeError::CannotDereference,
            TypeError::TypeAssertNotInterface,
            TypeError::TypeSwitchOutsideSwitch,
            TypeError::InvalidCompositeLitType,
            TypeError::OperatorNotDefined,
            TypeError::Undeclared,
            TypeError::Redeclared,
            TypeError::IllegalCycle,
            TypeError::InitCycle,
            TypeError::BlankAsValue,
            TypeError::IotaOutsideConst,
            TypeError::PackageNotInSelector,
            TypeError::InvalidConstType,
            TypeError::OtherDeclaration,
            TypeError::RefersTo,
            TypeError::CannotCall,
            TypeError::TooFewArgs,
            TypeError::TooManyArgs,
            TypeError::MissingConversionArg,
            TypeError::TooManyConversionArgs,
            TypeError::SpreadNonVariadic,
            TypeError::SpreadMultiValue,
            TypeError::SpreadMismatch,
            TypeError::InvalidVariadicArg,
            TypeError::NotAType,
            TypeError::ExpectedType,
            TypeError::UsedAsType,
            TypeError::ArrayLenNotConstant,
            TypeError::ArrayLenNotInteger,
            TypeError::MissingReceiver,
            TypeError::InvalidReceiver,
            TypeError::EmbeddedPointer,
            TypeError::EmbeddedPointerInterface,
            TypeError::FieldRedeclared,
            TypeError::MissingReturn,
            TypeError::UnexpectedReturn,
            TypeError::DuplicateCase,
            TypeError::MultipleDefaults,
            TypeError::InvalidBreak,
            TypeError::InvalidContinue,
            TypeError::InvalidFallthrough,
            TypeError::NonBoolCondition,
            TypeError::SendToRecvOnly,
            TypeError::SendToNonChan,
            TypeError::RecvFromSendOnly,
            TypeError::RecvFromNonChan,
            TypeError::NonNumericIncDec,
            TypeError::MissingLhs,
            TypeError::CompoundAssignMultiValue,
            TypeError::ResultNotInScope,
            TypeError::TypeSwitchNonInterface,
            TypeError::BuiltinMustBeCalled,
            TypeError::TypeNotExpression,
            TypeError::PreviousCase,
            TypeError::AppendNotSlice,
            TypeError::AppendInvalidArg,
            TypeError::InvalidLenCapArg,
            TypeError::CloseRecvOnly,
            TypeError::CloseNotChan,
            TypeError::CopyNotSlice,
            TypeError::CopyTypeMismatch,
            TypeError::DeleteNotMap,
            TypeError::DeleteKeyMismatch,
            TypeError::MakeInvalidType,
            TypeError::MakeArgCount,
            TypeError::MakeLenGtCap,
            TypeError::AssertNotBool,
            TypeError::AssertFailed,
            TypeError::BuiltinArgCount,
            TypeError::InvalidImportPath,
            TypeError::ImportCycle,
            TypeError::CannotDeclareInit,
            TypeError::CannotDeclareMain,
            TypeError::InvalidPackageName,
            TypeError::PackageNameMismatch,
            TypeError::MissingFuncBody,
            TypeError::InvalidInitSignature,
            TypeError::FieldMethodConflict,
            TypeError::MethodRedeclared,
            TypeError::UnexpectedFuncDecl,
            TypeError::LabelNotDeclared,
            TypeError::LabelShadowed,
            TypeError::ExtraInitExpr,
            TypeError::MissingInitExpr,
            TypeError::MissingTypeOrInit,
            TypeError::MissingConstValue,
            TypeError::UnusedImport,
            TypeError::UnusedVar,
            TypeError::NoNewVars,
            TypeError::UnusedLabel,
        ];

        for err in errors {
            let msg = err.message();
            assert!(!msg.is_empty(), "Error {:?} has empty message", err);
        }
    }

    #[test]
    fn test_warning_creates_warning_diagnostic() {
        let diag = TypeError::UnusedVar.at(0u32..5u32);
        assert!(diag.is_warning());
        assert!(!diag.is_error());

        let diag = TypeError::NoNewVars.with_message("no new variables on left side of :=");
        assert!(diag.is_warning());
    }

    #[test]
    fn test_error_creates_error_diagnostic() {
        let diag = TypeError::MissingReturn.at(0u32..5u32);
        assert!(diag.is_error());
        assert!(!diag.is_warning());
    }
}
