//! Symbol interning for efficient identifier handling.
//!
//! This module provides string interning to reduce memory usage and enable
//! fast comparison of identifiers throughout the compiler.

use std::fmt;
use std::hash::{Hash, Hasher};

use string_interner::{backend::StringBackend, DefaultSymbol, StringInterner};

use crate::span::Span;

/// An interned string symbol.
///
/// Symbols are cheap to copy and compare (just a u32 comparison).
/// To get the actual string, you need access to the `SymbolInterner`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(Option<DefaultSymbol>);

impl Symbol {
    /// Creates a symbol from a raw u32 value.
    /// 
    /// # Safety
    /// The caller must ensure the value corresponds to a valid interned symbol.
    #[inline]
    #[allow(dead_code)]
    pub(crate) fn from_raw(raw: u32) -> Self {
        use string_interner::Symbol as _;
        Self(DefaultSymbol::try_from_usize(raw as usize))
    }

    /// Returns the raw u32 value of this symbol.
    #[inline]
    pub fn as_u32(self) -> u32 {
        use string_interner::Symbol as _;
        self.0.map(|s| s.to_usize() as u32).unwrap_or(u32::MAX)
    }

    /// A dummy symbol for uninitialized or placeholder values.
    pub const DUMMY: Symbol = Symbol(None);

    /// Returns true if this is a dummy symbol.
    #[inline]
    pub const fn is_dummy(self) -> bool {
        self.0.is_none()
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_dummy() {
            write!(f, "Symbol(DUMMY)")
        } else {
            write!(f, "Symbol({})", self.as_u32())
        }
    }
}

/// A string interner for symbols.
///
/// This is the central registry for all interned strings. It ensures that
/// each unique string is stored only once, and provides fast lookup.
#[derive(Clone)]
pub struct SymbolInterner {
    interner: StringInterner<StringBackend<DefaultSymbol>>,
}

impl SymbolInterner {
    /// Creates a new empty interner.
    pub fn new() -> Self {
        Self {
            interner: StringInterner::new(),
        }
    }

    /// Creates a new interner with pre-allocated capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            interner: StringInterner::with_capacity(capacity),
        }
    }

    /// Interns a string, returning its symbol.
    ///
    /// If the string was already interned, returns the existing symbol.
    #[inline]
    pub fn intern(&mut self, string: &str) -> Symbol {
        Symbol(Some(self.interner.get_or_intern(string)))
    }

    /// Interns a static string, returning its symbol.
    ///
    /// This is more efficient for string literals.
    #[inline]
    pub fn intern_static(&mut self, string: &'static str) -> Symbol {
        Symbol(Some(self.interner.get_or_intern_static(string)))
    }

    /// Looks up a string without interning it.
    ///
    /// Returns `Some(symbol)` if the string was already interned.
    #[inline]
    pub fn get(&self, string: &str) -> Option<Symbol> {
        self.interner.get(string).map(|s| Symbol(Some(s)))
    }

    /// Resolves a symbol to its string.
    ///
    /// Returns `None` if the symbol is invalid or dummy.
    #[inline]
    pub fn resolve(&self, symbol: Symbol) -> Option<&str> {
        symbol.0.and_then(|s| self.interner.resolve(s))
    }

    /// Returns the number of interned strings.
    #[inline]
    pub fn len(&self) -> usize {
        self.interner.len()
    }

    /// Returns true if no strings have been interned.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.interner.is_empty()
    }
}

impl Default for SymbolInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Debug for SymbolInterner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SymbolInterner")
            .field("len", &self.interner.len())
            .finish()
    }
}

/// An identifier with its symbol and source span.
#[derive(Clone, Copy)]
pub struct Ident {
    /// The interned symbol for this identifier.
    pub symbol: Symbol,
    /// The source span where this identifier appears.
    pub span: Span,
}

impl Ident {
    /// Creates a new identifier.
    #[inline]
    pub const fn new(symbol: Symbol, span: Span) -> Self {
        Self { symbol, span }
    }

    /// Creates a dummy identifier.
    #[inline]
    pub const fn dummy() -> Self {
        Self {
            symbol: Symbol::DUMMY,
            span: Span::dummy(),
        }
    }

    /// Returns true if this is a dummy identifier.
    #[inline]
    pub const fn is_dummy(&self) -> bool {
        self.symbol.is_dummy()
    }

    /// Resolves this identifier to its string using the given interner.
    #[inline]
    pub fn as_str<'a>(&self, interner: &'a SymbolInterner) -> Option<&'a str> {
        interner.resolve(self.symbol)
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.symbol == other.symbol
    }
}

impl Eq for Ident {}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.symbol.hash(state);
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Ident({:?} @ {:?})", self.symbol, self.span)
    }
}

/// Pre-defined symbols for keywords and common identifiers.
///
/// These are interned at startup for fast comparison.
pub mod kw {
    macro_rules! define_keywords {
        ($($name:ident => $string:literal),* $(,)?) => {
            $(
                pub const $name: &str = $string;
            )*

            /// All keyword strings.
            pub const ALL: &[&str] = &[$($string),*];
        };
    }

    define_keywords! {
        BREAK => "break",
        CASE => "case",
        CHAN => "chan",
        CONST => "const",
        CONTINUE => "continue",
        DEFAULT => "default",
        DEFER => "defer",
        ELSE => "else",
        FALLTHROUGH => "fallthrough",
        FOR => "for",
        FUNC => "func",
        GO => "go",
        GOTO => "goto",
        IF => "if",
        IMPORT => "import",
        INTERFACE => "interface",
        MAP => "map",
        OBJECT => "object",
        PACKAGE => "package",
        RANGE => "range",
        RETURN => "return",
        SELECT => "select",
        STRUCT => "struct",
        SWITCH => "switch",
        TYPE => "type",
        VAR => "var",
    }
}

/// Pre-defined symbols for built-in types.
pub mod builtin_types {
    macro_rules! define_builtin_types {
        ($($name:ident => $string:literal),* $(,)?) => {
            $(
                pub const $name: &str = $string;
            )*

            /// All built-in type strings.
            pub const ALL: &[&str] = &[$($string),*];
        };
    }

    define_builtin_types! {
        BOOL => "bool",
        STRING => "string",
        INT => "int",
        INT8 => "int8",
        INT16 => "int16",
        INT32 => "int32",
        INT64 => "int64",
        UINT => "uint",
        UINT8 => "uint8",
        UINT16 => "uint16",
        UINT32 => "uint32",
        UINT64 => "uint64",
        FLOAT32 => "float32",
        FLOAT64 => "float64",
        BYTE => "byte",
        RUNE => "rune",
    }
}

/// Pre-defined symbols for built-in functions.
pub mod builtin_funcs {
    macro_rules! define_builtin_funcs {
        ($($name:ident => $string:literal),* $(,)?) => {
            $(
                pub const $name: &str = $string;
            )*

            /// All built-in function strings.
            pub const ALL: &[&str] = &[$($string),*];
        };
    }

    define_builtin_funcs! {
        LEN => "len",
        CAP => "cap",
        APPEND => "append",
        COPY => "copy",
        DELETE => "delete",
        MAKE => "make",
        CLOSE => "close",
        PANIC => "panic",
        RECOVER => "recover",
        PRINT => "print",
        PRINTLN => "println",
    }
}

/// Pre-defined symbols for built-in constants.
pub mod builtin_consts {
    macro_rules! define_builtin_consts {
        ($($name:ident => $string:literal),* $(,)?) => {
            $(
                pub const $name: &str = $string;
            )*

            /// All built-in constant strings.
            pub const ALL: &[&str] = &[$($string),*];
        };
    }

    define_builtin_consts! {
        TRUE => "true",
        FALSE => "false",
        IOTA => "iota",
        NIL => "nil",
    }
}

/// The blank identifier.
pub const BLANK: &str = "_";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_basic() {
        let sym = Symbol::from_raw(42);
        assert_eq!(sym.as_u32(), 42);
        assert!(!sym.is_dummy());

        assert!(Symbol::DUMMY.is_dummy());
    }

    #[test]
    fn test_interner_basic() {
        let mut interner = SymbolInterner::new();
        
        let sym1 = interner.intern("hello");
        let sym2 = interner.intern("world");
        let sym3 = interner.intern("hello");
        
        assert_eq!(sym1, sym3);
        assert_ne!(sym1, sym2);
        
        assert_eq!(interner.resolve(sym1), Some("hello"));
        assert_eq!(interner.resolve(sym2), Some("world"));
    }

    #[test]
    fn test_interner_get() {
        let mut interner = SymbolInterner::new();
        
        assert!(interner.get("hello").is_none());
        
        let sym = interner.intern("hello");
        assert_eq!(interner.get("hello"), Some(sym));
        assert!(interner.get("world").is_none());
    }

    #[test]
    fn test_interner_static() {
        let mut interner = SymbolInterner::new();
        
        let sym1 = interner.intern_static("static");
        let sym2 = interner.intern("static");
        
        assert_eq!(sym1, sym2);
    }

    #[test]
    fn test_interner_len() {
        let mut interner = SymbolInterner::new();
        
        assert!(interner.is_empty());
        assert_eq!(interner.len(), 0);
        
        interner.intern("a");
        interner.intern("b");
        interner.intern("a"); // duplicate
        
        assert!(!interner.is_empty());
        assert_eq!(interner.len(), 2);
    }

    #[test]
    fn test_interner_resolve_dummy() {
        let interner = SymbolInterner::new();
        assert!(interner.resolve(Symbol::DUMMY).is_none());
    }

    #[test]
    fn test_ident_basic() {
        let ident = Ident::new(Symbol::from_raw(1), Span::from_u32(0, 5));
        assert!(!ident.is_dummy());
        
        let dummy = Ident::dummy();
        assert!(dummy.is_dummy());
    }

    #[test]
    fn test_ident_equality() {
        let ident1 = Ident::new(Symbol::from_raw(1), Span::from_u32(0, 5));
        let ident2 = Ident::new(Symbol::from_raw(1), Span::from_u32(10, 15));
        let ident3 = Ident::new(Symbol::from_raw(2), Span::from_u32(0, 5));
        
        // Same symbol, different spans - should be equal
        assert_eq!(ident1, ident2);
        // Different symbols - should not be equal
        assert_ne!(ident1, ident3);
    }

    #[test]
    fn test_ident_as_str() {
        let mut interner = SymbolInterner::new();
        let sym = interner.intern("myident");
        let ident = Ident::new(sym, Span::from_u32(0, 7));
        
        assert_eq!(ident.as_str(&interner), Some("myident"));
    }

    #[test]
    fn test_keywords() {
        assert_eq!(kw::FUNC, "func");
        assert_eq!(kw::STRUCT, "struct");
        assert!(kw::ALL.contains(&"func"));
        assert!(kw::ALL.contains(&"struct"));
    }

    #[test]
    fn test_builtin_types() {
        assert_eq!(builtin_types::INT, "int");
        assert_eq!(builtin_types::STRING, "string");
        assert!(builtin_types::ALL.contains(&"int"));
    }

    #[test]
    fn test_builtin_funcs() {
        assert_eq!(builtin_funcs::LEN, "len");
        assert_eq!(builtin_funcs::MAKE, "make");
        assert!(builtin_funcs::ALL.contains(&"len"));
    }

    #[test]
    fn test_builtin_consts() {
        assert_eq!(builtin_consts::TRUE, "true");
        assert_eq!(builtin_consts::NIL, "nil");
        assert!(builtin_consts::ALL.contains(&"true"));
    }
}
