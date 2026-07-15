# Vo Language Specification

This document defines the syntax and semantics of the **Vo** programming language.

Vo is a statically typed language with Go-shaped syntax. Go source compatibility
is not a language goal; Vo has distinct pointer, module, error, dynamic-access,
and island-concurrency semantics.

---

## 1. Design Philosophy

Go's appeal is **low ceremony**: it writes like a scripting language but ships like a compiled one. Vo doubles down on this strength—adding flexible execution modes (VM/JIT/AOT) and error handling sugar, while keeping the language simple. Target niche: where you might reach for Go, Python, or Lua.

### 1.1 Goals

- **Scripting-language ergonomics**: Minimal boilerplate, quick to write, easy to read
- **Flexible execution**: Run like a script (VM) or deploy like Go (JIT/AOT)—single binary, no dependencies
- Static typing with local inference—types help, but don't get in the way
- Simple memory model: **value types** (copied) vs **reference types** (heap-allocated)

### 1.2 Differences from Go

- **No generics**: Use `interface{}` or code generation
- **No pointer arithmetic**: Pointers only for struct types (`*MyStruct`)
- **Error handling sugar**: `?` operator, `fail`, and `errdefer` for cleaner error handling

### 1.3 Error Handling Sugar

Vo provides simplified error handling with `?`, `fail`, and `errdefer`.

#### 1.3.1 The `?` operator

**Syntax**: `expr?`

**Type constraints**:
- `expr` must have type `(T1, T2, ..., error)` (tuple with error as last element) or `error`

**Usage constraint**:
- `?` can only be used in a function whose final result is identical to the predeclared `error` type
- A type alias of `error` is identical; a defined type that merely implements `Error() string` does not qualify
- Using `?` in any other function is a compile error

**Result type**:
- `(T1, T2, ..., error)?` → `(T1, T2, ...)` (tuple without error)
  - Special case: `(T, error)?` → `T` (single value)
- `error?` → no value (only valid where a value is not required)

**Runtime semantics**:
```
1. Evaluate expr to get (values..., err)
2. If err != nil:
   a. Preserve the current values of named non-error result variables; unnamed non-error results use their zero values
   b. Set the error return value to err
   c. Begin return unwinding; ordinary `defer` calls and eligible `errdefer`
      calls execute together in reverse registration order
   d. Return from function
3. Otherwise: result is values... (without error)
```

**Side effects**:
- May cause early function return (when error != nil)
- A propagated error makes registered `errdefer` calls eligible; ordinary
  `defer` calls remain eligible on every return path
- Named non-error result variables retain their current values and remain
  available for modification by deferred calls; unnamed non-error results are
  initialized to zero before unwinding

**Examples**:
```vo
func readConfig() (Config, error) {
    data := readFile("config.json")?  // propagates error if any
    return parseConfig(data)
}

func process() error {
    err?  // propagates error if err != nil
    return nil
}
```

#### 1.3.2 `fail`

`fail e` aborts the current function and returns the error `e`.
It is only valid when the function's final result is identical to the
predeclared `error` type.

**Semantics**:
1. Evaluate `e` once and convert it to the predeclared `error` type
2. Preserve the current values of named non-error result variables; use zero
   values for unnamed non-error results
3. Set the error return value to the saved error
4. Begin return unwinding; ordinary `defer` calls and eligible `errdefer`
   calls execute together in reverse registration order
5. Return from function

`expr?` when error != nil is equivalent to `fail err`.

#### 1.3.3 `errdefer`

`errdefer call` registers a deferred call that is eligible only while the
function is on an error path. Its function value, receiver, and arguments are
evaluated when the statement executes, following the same capture rules as
`defer`.

An error path begins when:

- the function returns a non-nil final `error` result, whether through an
  explicit `return`, `fail`, or `?`; or
- the function begins panic unwinding.

On an error return, eligible `errdefer` calls and ordinary `defer` calls form
one LIFO sequence ordered by registration time. On a successful return,
`errdefer` calls are discarded and ordinary `defer` calls still run. During
panic unwinding, both kinds initially run in that same LIFO order. If a
deferred call recovers the panic, remaining `errdefer` calls are discarded and
remaining ordinary `defer` calls continue.
Return-path eligibility is determined once, after result expressions are
committed and before any deferred call runs. A later ordinary defer changing a
named error result does not add or remove errdefers. Errdefers discarded at the
start of a successful return also remain discarded if an ordinary defer then
panics.
It is only valid when the function's final result is identical to the
predeclared `error` type.

#### 1.3.4 The `?` operator with Dynamic Access

When `?` is used with dynamic access in an assignment context, LHS types drive the expected return types and runtime type checking is performed.

**Syntax**: `lhs = dyn_expr?` or `lhs1, lhs2, ... = dyn_expr?`

**Semantics** (for `x = a~>field?` where `x` has type `T`):
```
1. Execute dynamic operation: (__any, __err1) = a~>field
2. If __err1 != nil: fail __err1
3. Type assert __any to T: (__typed, __ok) = __any.(T)
4. If !__ok: fail an error matching dyn.ErrTypeMismatch under errors.Is
5. Assign: x = __typed
```

**Key points**:
- Two error check points: dynamic operation error, then type assertion error
- Type mismatch produces an error matching `dyn.ErrTypeMismatch`; it does not
  panic
- Both errors trigger `errdefer` via `fail`

**Examples**:
```vo
func getInt(obj any) (int, error) {
    var x int
    x = obj~>value?  // LHS type (int) drives expected type
    return x, nil
}

func getPair(obj any) (int, string, error) {
    var a int
    var b string
    a, b = obj~>Pair()?  // multiple return values
    return a, b, nil
}
```

### 1.4 Frontend Resource Limits

The reference compiler applies deterministic limits before retaining
source-derived structures:

- one UTF-8 source file is at most 16 MiB;
- recursive syntax nesting is at most 128 parser frames;
- one expression path contains at most 512 binary operators;
- one file retains at most 256 concrete syntax diagnostics, followed by
  `E1109` when later syntax diagnostics are suppressed;
- one package retains at most 256 concrete type-check diagnostics, followed by
  `E2999` when later type diagnostics are suppressed.

Crossing a structural limit is a compile-time error. Diagnostic suppression
does not make an invalid file or package successful.

---

## 2. Memory Model

### 2.1 Value Types vs Reference Types

Vo distinguishes two categories of types:

| Category | Types | Assignment | Zero Value |
|----------|-------|------------|------------|
| **Value** | `int`, `int8`, ..., `string`, `byte`, `rune`, `[N]T`, `struct` | Copies data | Type-specific |
| **Reference** | `*StructType`, `interface`, `[]T`, `map[K]V`, `chan T`, `port T`, `island`, `func(...)` | Copies reference or capability handle | `nil` |

**Value type zero values**:
- `int`, `int8`, ..., `uint64` → `0`
- `float32`, `float64` → `0.0`
- `bool` → `false`
- `string` → `""`
- `byte` → `0`
- `rune` → `0` (the null character)
- `[N]T` → each element is zero value of `T`
- `struct` → each field is zero value of its type

**Reference type zero values**: Always `nil`.

### 2.2 Named Type Inheritance

When declaring `type T U`:
- `T` inherits the **category** (value or reference) of `U`
- `T` inherits the **zero value** of `U`
- `T` inherits the **comparability** of `U`

```vo
type MyInt int       // value type, zero = 0, comparable
type Users []User    // reference type, zero = nil
type Handler func()  // reference type, zero = nil
```

### 2.3 Pointers to Structs (`*T`)

For any struct type `T`, the type `*T` is a **pointer type** with reference
semantics. Pointer types are restricted to struct bases (`*int` is invalid),
and the language has no pointer arithmetic or pointer-to-integer conversion.

**Operators**:

| Operator | Syntax | Meaning |
|----------|--------|---------|
| Address-of | `&x` | Returns a pointer to an addressable struct operand; a struct composite literal is also permitted |
| Dereference | `*p` | Accesses the struct that `p` points to (alias, not copy) |

**Example**:

```vo
type Point struct { x, y int }

var p Point = Point{1, 2}
var ref *Point = &p         // ref points to p

ref.x = 10                  // modify through pointer
println(p.x)                // 10 (p was modified)

*ref = Point{3, 4}          // assign through pointer
println(p.x)                // 3

var ref2 *Point = &p
println(ref == ref2)        // true (same address)
```

**Nil pointers**:

```vo
var r *Point                // r == nil
r.x = 1                     // RUNTIME ERROR: nil pointer dereference
r = &Point{}                // r points to new zero-valued Point
r = new(Point)              // equivalent to above
```

**Pointer comparison**:
- `p == q`: true if both point to same struct (or both nil)
- `p == nil`: true if p is nil

### 2.4 The `nil` Literal

`nil` represents the absence of a value for reference types.

**Static Rules**:
- `nil` may be assigned to any reference type
- `nil` cannot be assigned to value types (compile error)
- `var x = nil` is invalid: type cannot be inferred
- `x := nil` is invalid: type cannot be inferred (see §5.4)

**Runtime Rules**:
- Field access or explicit dereference through a nil pointer raises a recoverable
  runtime panic
- Calling a pointer-receiver method on a nil `*T` passes nil to the method and
  is valid until the method dereferences it; implicitly calling a value-receiver
  method through a nil `*T` raises a recoverable runtime panic
- Calling a nil function or a method through a nil interface raises a
  recoverable runtime panic
- Index access on a nil slice raises a recoverable runtime panic
- Index access on `nil` map is allowed (returns zero value; the `ok` result is `false`)
- Assignment to a nil map (`m[k] = v`) raises a recoverable runtime panic

### 2.5 Comparability Rules

Types are classified as **comparable** or **non-comparable**:

| Type | Comparable | Comparison Semantics |
|------|------------|---------------------|
| `int`, `int8`, ..., `uint64`, `float32`, `float64` | ✅ | Value equality |
| `bool` | ✅ | Value equality |
| `string` | ✅ | Content equality |
| `[N]T` (if `T` comparable) | ✅ | Element-wise equality |
| Named value type | ✅ (inherits) | Per underlying type |
| `struct` | ✅ if all fields comparable | Field-wise equality |
| `*StructType` | ✅ | Pointer identity (`==`/`!=`), or `nil` |
| `interface` | ✅ | Dynamic value equality (requires underlying comparable) or `nil` |
| `chan T` | ✅ | Channel identity, or `nil` |
| `port T` and directional ports | ✅ | Capability identity, or `nil` |
| `island` | ✅ | Island-handle identity, or `nil` |
| `[]T` | ❌ | Only `== nil` / `!= nil` |
| `map[K]V` | ❌ | Only `== nil` / `!= nil` |
| `func(...)` | ❌ | Only `== nil` / `!= nil` |

**Rules**:
- `==` and `!=` require both operands to be comparable, OR one operand to be `nil` and the other a reference type
- `<`, `<=`, `>`, `>=` are only valid for numeric types (`int`, `int8`, ..., `float64`) and `string`
- Floating-point equality follows IEEE-754: positive and negative zero compare
  equal, while every NaN compares unequal, including to itself.
- String equality compares exact bytes; string ordering is lexicographic by
  unsigned byte value and does not require valid UTF-8.
- Array elements are compared in increasing index order. Struct fields are
  compared in declaration order. Both stop at the first unequal component.
  Representation identity is not an equality shortcut: a composite value that
  contains NaN still compares unequal to itself.
- Two non-nil interface values compare equal only when their dynamic types are
  identical and their dynamic values compare equal. Reaching a dynamic value
  whose type is not comparable raises a runtime error. Normal short-circuiting
  of an enclosing array or struct comparison can make a later interface
  component unreachable.

```vo
1 == 2              // OK: int comparable
"a" < "b"           // OK: string ordered
p == nil            // OK: pointer vs nil
p == q              // OK: pointer comparison
s == nil            // OK: slice vs nil
s == t              // ERROR: slices not comparable
```

### 2.6 Parameter Passing

All parameters are passed by value. For reference types, the "value" is a reference, so mutations inside a function affect the caller's data.

---

## 3. Lexical Structure

### 3.1 Identifiers

```ebnf
Ident                  ::= ( "_" | UnicodeLetter ) { "_" | UnicodeAlphanumeric } ;
UnicodeLetter          ::= /* a Unicode 16.0 code point for which Alphabetic is true */ ;
UnicodeAlphanumeric    ::= /* a Unicode 16.0 code point for which Alnum is true */ ;
```

All Unicode properties in this section use Unicode 16.0 data. `Alnum` is the
union of the `Alphabetic` property and General Category `Decimal_Number`; other
number categories, including superscript digits, do not continue identifiers.
Identifiers are case-sensitive and are compared as their exact Unicode code
point sequence. The implementation does not normalize identifiers.
An identifier is exported when its first code point has the Unicode 16.0
`Uppercase` property. Exported package declarations, struct fields, and methods
are visible to importing packages; all other names are package-private.

### 3.2 Keywords

The following are reserved keywords:

```
 break     case      chan      const     continue
 default   defer     else      fallthrough
 for       func      go        goto      if
 import    interface island    map       package
 port      range     return    select    struct
 switch    type      var       fail      errdefer
```

> **Note**: `panic` and `recover` are **built-in functions**, not keywords. See §10.

### 3.3 Predefined Identifiers

The following are predeclared but can be shadowed:

```
// Types
bool  string
int  int8  int16  int32  int64
uint  uint8  uint16  uint32  uint64
float32  float64
byte  rune  // aliases for uint8 and int32
any        // alias for interface{}
error      // interface { Error() string }

// Constants
true  false  iota

// Zero value
nil

// Blank identifier
_

// Functions (compiler built-ins)
len  cap  append  copy  delete  make  new  close  panic  recover  print  println  assert
```

### 3.4 Dynamic Access (`~>`)

Vo supports opt-in dynamic operations via the `~>` operator.

Dynamic access does not introduce a new runtime representation. It is implemented by compile-time desugaring of a small whitelist of `~>` operations into runtime helper calls.

Dynamic operations return `(any, error)` and are intended to be used with postfix `?` (see §1.3).

The left operand of `~>` may have static type `any/interface`, `(any, error)`,
or a concrete type implementing the exact reserved protocol for the requested
operation. A concrete value without that protocol must be converted explicitly,
as in `any(value)~>Field`, to request reflection. If the left operand is
`(any, error)`, `~>` short-circuits when the error is non-nil.

The following forms are supported:

```vo
var a any = 123
v := a~>field?
v2 := a~>["key"]?
v3 := a~>(1, 2, 3)?
v4 := a~>user~>name?
```

Dynamic writes use `dyn.SetAttr(a, "field", x)` and
`dyn.SetIndex(a, key, x)`. Both return `error`, which the caller must inspect or
propagate. Dynamic assignment targets are rejected at compile time.

Type assertion uses standard interface semantics (not a dynamic operation):
- `a.(T)` — panic on failure
- `v, ok := a.(T)` — ok=false on failure, no panic

The `dyn` package also exposes explicit APIs such as `dyn.GetAttr`,
`dyn.GetIndex`, `dyn.SetAttr`, and `dyn.SetIndex`.

### 3.5 Operators and Punctuation

```
+    -    *    /    %
&    |    ^    &^              // bitwise: AND, OR, XOR, AND NOT (bit clear)
<<   >>
==   !=   <    <=   >    >=
&&   ||   !
 <-                              // channel send/receive
 ++   --                        // increment/decrement (statements only)
 =    :=   +=   -=   *=   /=   %=   <<=  >>=  &=  |=  ^=  &^=
 ?    ~>   @
 (    )    [    ]    {    }
 ,    :    ;    .    ...
```

### 3.6 Literals

```ebnf
Digit    ::= "0".."9" ;
HexDigit ::= "0".."9" | "A".."F" | "a".."f" ;
OctDigit ::= "0".."7" ;
BinDigit ::= "0" | "1" ;
DecDigits ::= Digit ( "_"? Digit )* ;

IntLit ::= DecLit | HexLit | OctLit | LegacyOctLit | BinLit ;
DecLit ::= "0" | ("1".."9") ( "_"? Digit )* ;
HexLit ::= "0" ("x"|"X") HexDigit ( "_"? HexDigit )* ;
OctLit ::= "0" ("o"|"O") OctDigit ( "_"? OctDigit )* ;
LegacyOctLit ::= "0" ( "_"? OctDigit )+ ;
BinLit ::= "0" ("b"|"B") BinDigit ( "_"? BinDigit )* ;

FloatLit    ::= DecFloatLit | HexFloatLit ;
DecFloatLit ::= ( DecDigits "." DecDigits? | "." DecDigits ) ( ("e"|"E") ("+"|"-")? DecDigits )?
             | DecDigits ("e"|"E") ("+"|"-")? DecDigits ;

HexDigits    ::= HexDigit ( "_"? HexDigit )* ;
HexMantissa  ::= HexDigits "." HexDigits?
              | "." HexDigits
              | HexDigits ;
HexExponent  ::= ("p"|"P") ("+"|"-")? DecDigits ;
HexFloatLit  ::= "0" ("x"|"X") HexMantissa HexExponent ;

RuneChar ::= /* any Unicode scalar value except newline, "'", and "\\" */ ;
StringChar ::= /* any Unicode scalar value except newline, '"', and "\\" */ ;
EscapeSeq ::= "\\" ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | "\\" | "'" | '"' ) ;
ByteEscape ::= "\\" OctDigit OctDigit OctDigit | "\\x" HexDigit HexDigit ;
UnicodeEscape ::= "\\u" HexDigit HexDigit HexDigit HexDigit
                | "\\U" HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit ;

RuneLit ::= "'" ( RuneChar | EscapeSeq | ByteEscape | UnicodeEscape ) "'" ;
StringLit ::= InterpretedStringLit | RawStringLit ;
InterpretedStringLit ::= '"' ( StringChar | EscapeSeq | ByteEscape | UnicodeEscape )* '"' ;
RawStringLit ::= "`" ( /* any char except "`" */ )* "`" ;
```

Within a numeric digit sequence, `_` is permitted only between two digits. It
cannot follow a base prefix, appear twice consecutively, touch a decimal point
or exponent marker, or terminate the literal.

An integer with a leading zero and no explicit base prefix uses legacy octal,
so `0644` and `0_644` are octal while `08` is invalid. Hexadecimal
floating-point literals start with `0x`/`0X` and must use a binary exponent
introduced by `p`/`P`.

```vo
0x1p-2      // 0.25
0x1.2p3     // 9.0
0X.8p0      // 0.5
0x1.p0      // 1.0
```

`rune` literals follow Go's Unicode semantics: a rune literal denotes a Unicode code point value. The allowed escape forms are aligned with Go:

- `\\a \\b \\f \\n \\r \\t \\v \\\\ \\' \\"`
- Octal byte escape: `\\` followed by exactly three octal digits (value 0..255)
- Hex byte escape: `\\x` followed by exactly two hex digits (value 0..255)
- Unicode escapes: `\\u` (4 hex digits), `\\U` (8 hex digits), must be valid Unicode code points (no surrogates, max `0x10FFFF`)

Raw string literals use backquotes and may contain any characters except a backquote. Backslashes have no special meaning and escapes are not processed. Newlines are permitted.

A `string` stores an immutable sequence of bytes. String literals and
rune-to-string conversions produce UTF-8, while `string([]byte{...})` preserves
the bytes exactly, including invalid UTF-8. Length and indexing operate on
bytes. A native extern parameter declared as Rust `&str` adds a UTF-8 contract;
passing an arbitrary-byte string that violates it produces a structured extern
contract error and never constructs an invalid Rust string.

### 3.7 Semicolons

Semicolons terminate statements and declarations. The lexer automatically inserts a semicolon after a line's final token if that token is:
- An identifier or basic literal
- One of the keywords `break`, `continue`, `fail`, `fallthrough`, `island`, `return`
- One of the operators `++`, `--`
- The postfix error-propagation operator `?`
- A closing delimiter: `)`, `]`, `}`

A block comment containing a newline participates in this rule as if the
newline appeared directly in the token stream. A single-line block comment does
not create a line boundary.

### 3.8 Comments

```
// Single-line comment
/* Multi-line
   comment */
```

Block comments may nest. The first unmatched `*/` closes the outermost block;
reaching end of file while any block comment remains open is a syntax error.

---

## 4. Program Structure

### 4.1 Source Files

```ebnf
File ::= PackageClause? ImportDecl* TopDecl* ;

PackageClause ::= "package" Ident ";" ;
ImportDecl    ::= "import" ( ImportSpec ";" | "(" ( ImportSpec ";" )* ")" ";" ) ;
ImportSpec    ::= ( Ident | "." )? StringLit ;
```

When a package contains multiple source files, their presentation order is the
case-sensitive lexical order of normalized package-relative paths. Path
separators are normalized to `/`. Declaration order, independent variable
initialization order, and `init` function order use this presentation order.
Filesystem enumeration order never affects program behavior.

### 4.2 Top-Level Declarations

```ebnf
TopDecl ::= VarDecl
          | ConstDecl
          | TypeDecl
          | FuncDecl ;
```

### 4.3 Package Initialization

Package initialization follows these rules:

1. Imported packages are initialized before the package that imports them.
2. Within one package, package variables are initialized in dependency order.
   Variables that are otherwise independent retain source presentation order.
   Dependencies are lexical and transitive: a reference to a package variable,
   function, or statically selected method contributes a dependency, and the
   referenced function or method body is scanned in turn. Taking a function or
   method value counts even when the initializer does not call it. Function
   literal bodies contained in an initializer are scanned by the same rule.
   A dynamically dispatched interface call contributes the dependency visible
   in its initializer expression; its runtime-selected concrete method does not
   add a compile-time dependency.
3. Each package-level `VarSpec` is one dependency and initialization unit. Its
   dependency set is the union of dependencies from every RHS expression, so
   no name in the spec can initialize ahead of a dependency needed by another
   name. All RHS expressions are evaluated from left to right and converted to
   their destination types before any LHS variable in the spec is updated. A
   single tuple-producing RHS is evaluated once. The saved values are then
   committed to non-blank LHS variables from left to right; if RHS evaluation
   or conversion panics, every LHS in that spec retains its previous zero
   value.
4. After all package variables are initialized, every body-bearing
   `func init()` in that package runs in source presentation order.
5. The main package is initialized after all of its dependencies; `main()` runs
   after the main package's `init` functions complete.

Imported packages that are simultaneously eligible are visited deterministically
from imports in source presentation order. Repeated imports do not initialize a
package again within the same island.

### 4.4 Initialization and Entry Functions

An initialization declaration has the exact form `func init() { ... }`: it has
no receiver, parameters, or results, and it must have a body. A package may
declare multiple initialization functions. Their names are not introduced into
package scope, so an `init` function cannot be called or used as a value.

An executable package is named `main` and declares exactly one body-bearing
`func main` with no receiver and no parameters. The entry function may declare
results; the runtime discards every result after the call completes. A method
named `main`, or a function named `main` in another package, is an ordinary
declaration and never becomes the program entry point.

---

## 5. Declarations

### 5.1 Variables

```ebnf
VarDecl     ::= "var" ( VarSpec ";" | "(" VarSpecList ")" ";" ) ;
VarSpecList ::= ( VarSpec ";" )* ;
VarSpec     ::= IdentList Type? ( "=" ExprList )? ;
```

**Grouped declarations** use parentheses:
```vo
var (
    x int
    y = 42
    a, b = 1, 2
)
```

**Static Rules**:
- If `Type` is omitted, `Expr` is required and type is inferred
- If `Expr` is omitted, variable is initialized to zero value (which is `nil` for reference types)
- If `Expr` is `nil`, `Type` is required

For a local `VarSpec`, all initializer expressions are evaluated from left to
right and converted to their destination types before any name declared by
that spec enters scope or receives a value. A single tuple-producing
initializer is evaluated once and may initialize the matching number of names.
The non-blank names then enter scope together and receive the saved values; a
blank name still causes its initializer or tuple component to be evaluated and
converted, then discards it. Within a grouped declaration, successive
`VarSpec`s follow this rule one spec at a time, so a later spec can use names
from an earlier spec.

```vo
var x int           // x = 0
var y = 42          // y inferred as int
var u User          // u is a zero-valued struct
var p UserRef       // p == nil
var h Handler = nil // OK: type is explicit
var z = nil         // ERROR: cannot infer type
```

### 5.2 Constants

```ebnf
ConstDecl     ::= "const" ( ConstSpec ";" | "(" ConstSpecList ")" ";" ) ;
ConstSpecList ::= ( ConstSpec ";" )* ;
ConstSpec     ::= IdentList Type? ( "=" ExprList )? ;
```

Constants require initializers (except when using `iota` continuation). `nil` is not a valid constant value.

**Grouped declarations** use parentheses:
```vo
const (
    Pi = 3.14159
    E  = 2.71828
)
```

**iota**: A predeclared identifier representing successive untyped integer constants. It resets to 0 at each `const` block and increments by 1 for each ConstSpec.

```vo
const (
    Sunday = iota   // 0
    Monday          // 1 (implicit = iota)
    Tuesday         // 2
)

const (
    _  = iota             // 0, ignored
    KB = 1 << (10 * iota) // 1 << 10 = 1024
    MB                    // 1 << 20
    GB                    // 1 << 30
)
```

```vo
const Pi = 3.14159
const MaxSize int = 1024
const Empty = nil   // ERROR: nil cannot be const
```

### 5.3 Numeric Constants

Vo follows Go's numeric constant model (with complex numbers removed), which provides arbitrary-precision arithmetic at compile time and flexible type conversion.

#### 5.3.1 Typed vs Untyped Constants

Constants can be **typed** or **untyped**:

| Kind | Example | Description |
|------|---------|-------------|
| Untyped integer | `42`, `0xFF`, `-14` | No fixed type, default to `int` |
| Untyped float | `3.14`, `1e10`, `2.0` | No fixed type, default to `float64` |
| Untyped rune | `'a'`, `'世'`, `'\n'` | No fixed type, default to `rune` |
| Untyped string | `"hello"` | No fixed type, default to `string` |
| Untyped bool | `true`, `false` | No fixed type, default to `bool` |
| Typed | `int(42)`, `const x int = 1` | Has explicit type |

**Key insight**: Untyped constants live in an "ideal" numeric space and can be freely mixed in expressions. They only acquire a concrete type when assigned to a variable or used in a context requiring a specific type.

```vo
const a = 2 + 3.0       // a == 5.0 (untyped floating-point)
const b = 15 / 4        // b == 3 (untyped integer, integer division)
const c = 15 / 4.0      // c == 3.75 (untyped floating-point)
const d = 'a' + 1       // d == 'b' (untyped rune)
```

#### 5.3.2 Default Types

When an untyped constant is used where a typed value is required (e.g., variable declaration without explicit type), it converts to its **default type**:

| Constant Kind | Default Type |
|---------------|--------------|
| Integer | `int` |
| Floating-point | `float64` |
| Rune | `rune` |
| String | `string` |
| Boolean | `bool` |

```vo
x := 42       // x is int (default type of untyped integer)
y := 3.14     // y is float64 (default type of untyped floating-point)
z := 'a'      // z is rune (default type of untyped rune)
s := "hi"     // s is string
```

#### 5.3.3 Implicit Conversion of Untyped Constants

An untyped constant can be implicitly converted to any compatible type:

```vo
var i int = 42        // OK: untyped 42 → int
var f float64 = 42    // OK: untyped 42 → float64
var b byte = 42       // OK: untyped 42 → byte (if in range)

type MyInt int
var m MyInt = 42      // OK: untyped 42 → MyInt

const Pi = 3.14159
var f64 float64 = Pi  // OK: untyped floating-point → float64
```

**Typed constants** cannot be implicitly converted:

```vo
const TypedInt int = 42
type MyInt int
var m MyInt = TypedInt   // ERROR: int is not MyInt
var m MyInt = 42         // OK: untyped constant
```

#### 5.3.4 Exact Precision and Implementation Budgets

The language model treats numeric constants as mathematical values with
arbitrary precision. Constant evaluation never wraps, saturates, or substitutes
an IEEE infinity or signed zero for an exact untyped value:

```vo
const Huge = 1 << 100           // OK: very large untyped integer
const Four = Huge >> 98         // Four == 4
const Result = Huge / 1e97      // OK: computed at compile time

var x int = Huge                // ERROR: Huge overflows int
var y int = Four                // OK: Four fits in int
```

**Implementation requirement**: Compilers must support at least 256-bit integer
constants and a 256-bit mantissa for floating-point constants. An
implementation may impose larger deterministic resource limits. Crossing such
a limit is a compile-time error; evaluation must not continue with an
approximation.

The current compiler applies these per-package deterministic limits:

- each exact integer, or each numerator/denominator of an exact rational, is at
  most 65,536 bits;
- a constant shift count is at most 65,535, allowing `1 << 65535` to produce
  the largest power of two that fits the single-value numeric limit;
- each folded string constant is at most 1 MiB of UTF-8 bytes;
- all constant-fold input and output payload charged while checking one package
  is at most 64 MiB.

One operation may use a bounded temporary up to approximately twice the numeric
single-value limit so exact cancellation and rational reduction can complete;
its retained result must still fit the 65,536-bit limit. Literal parsing,
arithmetic, shifts, string concatenation, and reuse of named constants all
participate in the same accounting. Exceeding a limit produces the stable
constant-resource diagnostic `E2115` and rejects the package.

Untyped numeric constants are mathematical values, so constant zero has no
sign. Converting `-0.0` directly to a floating-point type therefore produces
positive zero. Negating a typed floating-point zero at run time, or using
`math.Copysign`, produces IEEE negative zero.

#### 5.3.5 Representability

A constant `x` is **representable** by a value of type `T` if:

1. `x` is in the set of values determined by `T`, OR
2. `T` is a floating-point type and `x` can be rounded to `T`'s precision without overflow

```vo
// Representable:
var a byte = 97       // 97 is in [0, 255]
var b float64 = 42.0  // 42.0 is representable as float64
var c int = 1e3       // 1000 is an integer

// Not representable:
var d byte = 256      // ERROR: 256 > 255
var e byte = -1       // ERROR: -1 < 0
var f int = 1.5       // ERROR: 1.5 is not an integer
var g int = 1e100     // ERROR: too large for int
```

#### 5.3.6 Kind Promotion in Expressions

When untyped constants of different kinds are combined in an expression, the result takes the kind that appears later in this precedence list:

**integer < rune < floating-point**

```vo
const a = 1 + 2       // integer + integer → integer (3)
const b = 1 + 2.0     // integer + float → float (3.0)
const c = 'a' + 1     // rune + integer → rune ('b')
const d = 'a' + 1.0   // rune + float → float (98.0)
```

#### 5.3.7 Constant Expressions

Constant expressions are evaluated at compile time with exact precision:

```vo
const (
    Pi    = 3.14159265358979323846264338327950288419716939937510582097494459
    Tau   = 2 * Pi          // Full precision maintained
    Theta = Pi / 2
)

var f float64 = Pi          // Rounded to float64 precision
```

**Division by zero** in constant expressions is a compile-time error:

```vo
const bad = 1 / 0           // ERROR: division by zero
const alsobad = 1.0 / 0.0   // ERROR: division by zero
```

#### 5.3.8 Shift Expressions

Shift expressions use `<<` and `>>`.

**Static Rules**:
- The left operand must be of integer type or an untyped integer/rune constant.
- The right operand must be of integer type or an untyped integer constant.
- If the left operand is an untyped constant, the result is an untyped constant.
- If the left operand is a typed integer value, the result has the same type as the left operand.


### 5.4 Short Variable Declaration

```ebnf
ShortVarDecl ::= IdentList ":=" ExprList ";" ;
IdentList    ::= Ident ( "," Ident )* ;
ExprList     ::= Expr ( "," Expr )* ;
```

**Static Rules**:
- Only valid inside blocks (not at package level)
- Declares new variables and may reassign variables already declared in the
  same scope. At least one non-blank name on the left-hand side must be new;
  every reused non-blank name must already denote a variable in that same
  scope. A name declared only in an outer scope is new in the current scope.
- Number of identifiers must equal number of expressions
- Type of each variable = static type of corresponding expression
- If expression is `nil` with no inferable context type → compile error

All RHS expressions are evaluated from left to right and saved before any new
variable is initialized or any reused variable is assigned. A single
tuple-producing RHS is evaluated once.

```vo
x := 10
x, y := 20, "hi"  // OK: x reassigned, y newly declared
x := nil          // ERROR: cannot infer type from nil
```

### 5.5 Type Declarations

```ebnf
TypeDecl ::= "type" Ident ( "=" )? Type ";" ;
```

A declaration without `=` creates a distinct named type. A declaration with `=` creates an alias whose identity and method set are exactly those of its target. Methods can only be declared on a distinct named type defined in the current package; an alias cannot be a method receiver.

The new named type inherits the category (value/reference), zero value, and comparability from the underlying type (see §2.2).

```vo
type User struct {
    name string
    age  int `json:"age"`
}
```

> **Note**: Struct field tags (`` `...` ``) are metadata strings for field-level
> configuration (e.g., JSON key names). Standard-library and dynamic-access
> consumers interpret Go-style `key:"value"` entries with U+0020 SPACE as the
> only entry separator. A malformed entry stops interpretation of the remaining
> tag; Unicode whitespace is never silently treated as a separator.

---

## 6. Types

### 6.1 Type Grammar

```ebnf
Type ::= TypeName
       | PointerType
       | ArrayType
       | SliceType
       | MapType
       | ChanType
       | PortType
       | IslandType
       | FuncType
       | StructType
       | InterfaceType ;

PointerType ::= "*" Type ;  // Only valid when Type is a struct type
PortType    ::= ( "port" | "port" "<-" | "<-" "port" ) Type ;
IslandType  ::= "island" ;
```

```ebnf
InterfaceType ::= "interface" "{" InterfaceElem* "}" ;
InterfaceElem ::= MethodSpec | EmbeddedIface ;
MethodSpec    ::= Ident "(" ParamList? ")" ResultType? ";" ;  // ParamList defined in §6.7
EmbeddedIface ::= TypeName ";" ;
TypeName      ::= Ident | Ident "." Ident ;  // simple or qualified
```

### 6.2 Built-in Types

| Type | Category | Description |
|------|----------|-------------|
| `bool` | Value | Boolean (`true`/`false`) |
| `int` | Value | Signed 64-bit integer on every backend |
| `int8`, `int16`, `int32`, `int64` | Value | Fixed-width signed integers |
| `uint` | Value | Unsigned 64-bit integer on every backend |
| `uint8`, `uint16`, `uint32`, `uint64` | Value | Fixed-width unsigned integers |
| `float32`, `float64` | Value | IEEE-754 floating point |
| `string` | Value | Immutable byte string; UTF-8 text is conventional, not required |
| `byte` | Value | Alias for `uint8` |
| `rune` | Value | Alias for `int32` (Unicode code point) |

`int` and `uint` have identical width and overflow behavior in native VM, JIT,
WASM, and embedded runtimes. Vo does not predeclare `uintptr`; restricted
pointers cannot be converted to integers.

### 6.3 Arrays

```ebnf
ArrayType ::= "[" ( Expr | "..." ) "]" Type ;
```

Arrays are value types with fixed length. An explicit length expression must be
a non-negative integer constant representable as `int`. The `[...]T` form is
valid only as the type of an array composite literal. Its length is one more
than the largest element index after implicit indexes are assigned
consecutively; an empty literal has length zero. The inferred length must also
be representable as `int`.

Because `int` is fixed at 64 bits, the language limit is `MaxInt = 2^63 - 1` on
every backend. A backend may impose the additional requirement that a
heap-backed array's logical length fit its target address width. The compiler
reports that target limit before code generation; it must not truncate the
length or abort the compiler. Zero-byte element types waive neither limit
because `len` returns `int` and runtime array/slice headers use target-width
indexes.

```vo
var a [4]int  // [0, 0, 0, 0]
```

### 6.4 Slices

```ebnf
SliceType ::= "[" "]" Type ;
```

Slices are reference types referencing a dynamic sequence.

```vo
var s []int         // s == nil
s = []int{1, 2, 3}  // s != nil
```

### 6.5 Maps

```ebnf
MapType ::= "map" "[" Type "]" Type ;
```

Maps are reference types providing key-value storage.

**Key type restriction**: The key type must be **comparable**:

| Valid Keys | Invalid Keys |
|------------|--------------|
| `int`, `int8`, ..., `uint64` | `[]T` (slice) |
| `float32`, `float64` | `map[K]V` |
| `bool`, `string`, `byte`, `rune` | `func(...)` |
| `[N]T` (array of comparable) | |
| `struct` (if comparable) | |
| `*StructType` | |
| `chan T`, `port T`, `island` | |
| `interface` | |

```vo
var m map[string]int      // m == nil
m = map[string]int{}      // m != nil, empty map
m["key"] = 42

var bad map[[]int]int     // ERROR: slices are not comparable
```

Map key matching uses exactly the key type's `==` semantics. Equal keys must
therefore share a hash, while a hash collision alone never makes two keys
equal. In particular, positive and negative floating-point zero name the same
entry. A NaN is a valid floating-point key, yet it cannot match any key,
including the value used to insert it; repeated NaN assignments may create
distinct entries. For an interface key, each map operation checks the dynamic
key value. An unhashable dynamic value raises a runtime error; insertion and
deletion do not partially mutate the map before that error.

### 6.6 Channels

```ebnf
ChanType ::= ( "chan" | "chan" "<-" | "<-" "chan" ) Type ;
```

Channels are reference types used for communication between goroutines. Zero value is `nil`.

**Channel directions**:
- `chan T` — bidirectional (can send and receive)
- `chan<- T` — send-only
- `<-chan T` — receive-only

```vo
var ch chan int           // ch == nil
ch = make(chan int)       // unbuffered channel
ch = make(chan int, 10)   // buffered channel with capacity 10

// Directional channels (typically used in function parameters)
func producer(out chan<- int) {
    out <- 42             // can only send
}

func consumer(in <-chan int) {
    x := <-in             // can only receive
}
```

**Operations**:
- `ch <- value` — send value to channel (blocks if unbuffered and no receiver)
- `value := <-ch` — receive from channel (blocks if no value available)
- `value, ok := <-ch` — receive with closed check (`ok` is `false` if channel closed and empty)

```vo
ch <- 42           // send
x := <-ch          // receive
close(ch)          // close channel (no more sends allowed)
```

#### 6.6.1 Islands and Ports

`island` is a nilable handle to an independent VM instance with its own heap,
globals, initialization state, scheduler, and stacks. `make(island)` creates an
island. Island handles cannot cross an island boundary.

A `port` is a nilable cross-island message endpoint:

- `port T` owns the endpoint and may send, receive, and close it on its home island;
- `port<- T` is a send-only capability and is the only port capability that may cross islands;
- `<-port T` is a receive-only local view and cannot cross islands.

Messages must have a statically sendable concrete type. Interfaces, functions,
channels, islands, bidirectional/receive ports, and any compound type containing
one of them are not sendable. Transfer reconstructs an independent object graph
in the destination island. See [`channel.md`](./channel.md) for the complete
capability, deep-copy, close, and scheduler contract.

### 6.7 Functions

```ebnf
FuncType   ::= "func" "(" ParamList? ")" ResultType? ;
ParamList  ::= ParamDecl ( "," ParamDecl )* ;
ParamDecl  ::= IdentList? Type | Ident? "..." Type ;  // names are optional
ResultType    ::= Type | "(" ParamDecl ( "," ParamDecl )* ")" ;
```

Function types are reference types. Zero value is `nil`.

**Parameter naming**: Function types can include parameter names for documentation, but the names are not part of the type identity. The following are equivalent types:

```vo
func(int, int) int
func(x int, y int) int
func(a, b int) int
```

**Examples**:

```vo
var f func(int, int) int               // f == nil
var g func(x int, y int) int           // with named params (equivalent to above)
var h func(callback func(int) bool)    // function taking function
```

### 6.8 Variadic Functions

```ebnf
VariadicParam ::= Ident? "..." Type ;
```

A function declaration or function type may have a variadic final parameter.
Function results cannot be variadic. The caller may pass zero or more arguments
of the variadic element type.

```vo
func sum(nums ...int) int {
    total := 0
    for i := 0; i < len(nums); i += 1 {
        total += nums[i]
    }
    return total
}

sum(1, 2, 3)        // nums = []int{1, 2, 3}
sum()               // nums = []int{}

sum(s...)           // spread slice as arguments
```

### 6.9 Structs

```ebnf
StructType ::= "struct" "{" FieldDecl* "}" ;
FieldDecl  ::= ( IdentList Type | Type ) Tag? ";" ;
Tag        ::= StringLit ;
```

Structs are **value types**. Assignment copies the struct value. The zero value of a struct is a struct value where each field is the zero value of its type.

**Anonymous fields** (embedding): A field can be a named type or a pointer to a
named struct type. Its unqualified type name becomes the field name. The same
field name cannot be declared twice, including once explicitly and once by
embedding.

```vo
type User struct {
    id   int    "json:\"id\""
    name string "json:\"name\""
}

type Employee struct {
    User           // anonymous/embedded field
    department string
}

// Access embedded fields directly:
e := Employee{}
e.name = "Alice"   // accesses User.name
```

> **Note**: Tags use double-quoted strings with escaped inner quotes.

---

## 7. Interfaces and Methods

### 7.1 Interface Declarations

Interfaces are declared using the `type` keyword, following Go syntax:

```ebnf
TypeDecl      ::= "type" Ident ( "=" )? Type ";" ;
InterfaceType ::= "interface" "{" InterfaceElem* "}" ;
InterfaceElem ::= MethodSpec | EmbeddedIface ;
MethodSpec    ::= Ident "(" ParamList? ")" ResultType? ";" ;
EmbeddedIface ::= TypeName ";" ;
TypeName      ::= Ident | Ident "." Ident ;  // simple or qualified
```

```vo
type Reader interface {
    Read(buf []byte) int
}

type ReadWriter interface {
    Reader  // embedding
    Write(buf []byte) int
}
```

> **Note**: Embedded interfaces can be simple names (`Reader`) or qualified names (`io.Reader`).

### 7.2 Interface Method Sets

The **method set** of an interface is computed as follows:

1. Start with directly declared methods
2. For each embedded interface `I`, recursively compute its method set
3. Union all method sets
4. If two methods have the same name:
   - If signatures are identical → collapse to one method
   - If signatures differ → compile error

```vo
type A interface { Foo() int }
type B interface {
    Foo() int
    Bar()
}
type C interface {
    A
    B
}  // method set = {Foo() int, Bar()}

type D interface { Foo() string }
type E interface {
    A
    D
}  // ERROR: Foo has conflicting signatures
```

### 7.3 Type Method Sets

The **method set** of a type determines which methods can be called on values of that type, and which interfaces the type implements.

| Type | Method Set |
|------|------------|
| Distinct named type `T` | All methods with receiver `T` |
| Pointer type `*T` (where `T` has struct underlying type) | All methods with receiver `T` or `*T` |

```vo
type Point struct { x, y int }

func (p Point) Get() int { return p.x }    // receiver = Point
func (p *Point) Set(x int) { p.x = x }     // receiver = *Point

// Method set of Point = {Get}
// Method set of *Point = {Get, Set}
```

**Interface implementation**: A type `T` implements interface `I` if and only if the method set of `T` is a superset of the method set of `I`.

Method identity includes the declaring package for an unexported method name.
Consequently, an interface with an unexported method can be implemented only by
a type whose matching method was declared in that interface's package; a method
with the same spelling from another package is distinct. Exported method names
have the same identity across packages.

```vo
type Getter interface { Get() int }
type Setter interface { Set(int) }

var p Point
var g Getter = p      // OK: Point has Get()
var s Setter = p      // ERROR: Point doesn't have Set()
var s2 Setter = &p    // OK: *Point has Set()
```

A **distinct named type** whose underlying type is neither pointer nor interface
can have value-receiver methods. Aliases, built-in types, and anonymous types
cannot declare methods.

```vo
type MyInt int
func (m MyInt) Double() int { return int(m) * 2 }  // OK

type MySlice []int
func (s MySlice) Sum() int { ... }  // OK

func (i int) Foo() {}      // ERROR: cannot define on built-in type
func (a []int) Bar() {}    // ERROR: cannot define on anonymous type
```

### 7.4 Method Declarations

```ebnf
FuncDecl ::= "func" Ident "(" ParamList? ")" ResultType? ( Block | ";" )
           | "func" Receiver Ident "(" ParamList? ")" ResultType? Block ;
Receiver ::= "(" Ident "*"? Ident ")" ;
```

A package function declaration may omit the body. Such a declaration provides
the signature for a function implemented outside Vo (extern function). An
extern function is a first-class function value with the same static function
type as a function implemented in Vo: it can be assigned, passed, returned,
stored in composites, deferred, or started with `go`. Calling it still requires
a matching runtime/native registration for the active target. Method
declarations always require a body; the native ABI has no implicit extern
method convention.

The logical extern identity is `(canonical package import path, exact function
identifier)`. Its runtime wire name is the versioned length-delimited encoding
defined by the native FFI specification. Package path separators, punctuation,
case, and the boundary between package and function remain significant.

```vo
func Sqrt(x float64) float64
```

The receiver consists of a name and a **distinct named type defined in the current package**. Aliases, built-in types, anonymous types, and named types whose underlying type is a pointer or interface cannot be receivers. A value receiver may use any other valid named type. The pointer-receiver form `*T` is available only when `T` has a struct underlying type.

**Method name uniqueness**: Methods belong to the type, not to the receiver form. For a given type `T`, method names must be unique across both value receiver (`T`) and pointer receiver (`*T`) declarations.

```vo
type Point struct { x, y int }

func (p Point) Foo() {}    // OK
func (p *Point) Foo() {}   // ERROR: method Foo already declared for Point

func (p Point) Get() int { return p.x }      // OK: value receiver
func (p *Point) Set(x int) { p.x = x }       // OK: different method name
```

```vo
func (u User) Name() string {
    return u.name
}

func (u User) SetName(name string) {
    u.name = name  // NOTE: User is a struct, so this modifies a copy
}

func (a [4]int) Sum() int { ... }  // ERROR: receiver must be named type
```

**Runtime**: Calling a pointer-receiver method with a `nil` `*T` is allowed; the
method receives `nil` and may handle it explicitly. Dereferencing that receiver,
or implicitly invoking a value-receiver method through a `nil` `*T`, causes a
runtime panic.

### 7.5 Method Calls

When calling a method, the compiler automatically inserts address-of or dereference operations as needed:

| Expression | Method Receiver | Transformation |
|------------|-----------------|----------------|
| `v.m()` | `T` | Direct call |
| `v.m()` | `*T` (where `v` is addressable) | `(&v).m()` (auto address-of) |
| `p.m()` | `*T` | Direct call |
| `p.m()` | `T` | `(*p).m()` (auto dereference) |

```vo
type Point struct { x int }

func (p Point) Get() int { return p.x }
func (p *Point) Set(x int) { p.x = x }

var v Point
var p *Point = &v

v.Get()     // direct: receiver is Point
v.Set(10)   // transformed to (&v).Set(10)
p.Get()     // transformed to (*p).Get()
p.Set(20)   // direct: receiver is *Point
```

**Addressability requirement**: The auto address-of transformation `v.m()` → `(&v).m()` requires `v` to be addressable. Non-addressable values (e.g., function return values, map index expressions) cannot call pointer-receiver methods directly.

```vo
func getPoint() Point { return Point{} }

getPoint().Get()    // OK: value receiver
getPoint().Set(10)  // ERROR: cannot take address of getPoint()
```

### 7.6 Method Values and Method Expressions

A selector on a value can produce a **method value**. Evaluating `x.M` evaluates
and saves `x` once, without calling `M`; invoking the resulting function later
supplies that saved receiver. A value receiver is copied when the method value
is formed. A pointer receiver saves the pointer, so later mutations through the
same pointer remain visible. Any ordinary arguments are evaluated only when the
method value is called.

```vo
p := Point{x: 1}
get := p.Get
p.x = 2
println(get()) // 1: the value receiver was copied

set := p.Set   // implicit &p is saved
set(3)
println(p.x)   // 3
```

A selector on a type produces a **method expression**. `T.M` has the same
parameters and results as method `M`, with an explicit first parameter of type
`T`; `(*T).M` uses `*T` as that first parameter. No receiver is captured when
the expression is formed. The selected method must belong to the stated type's
method set, and promoted methods follow the ordinary embedding rules.

```vo
getFn := Point.Get       // func(Point) int
setFn := (*Point).Set    // func(*Point, int)
println(getFn(p))
setFn(&p, 4)
```

For an interface type `I`, `I.M` takes an `I` as its first parameter and
dispatches using that argument's dynamic type when called. Calling it with a
nil interface follows the ordinary nil-interface panic rule.

---

## 8. Statements

### 8.1 Statement Grammar

```ebnf
Stmt ::= Block
       | VarDecl
       | ConstDecl
       | TypeDecl
       | ShortVarDecl
       | Assignment
       | ExprStmt
       | ReturnStmt
       | IfStmt
       | ForStmt
       | SwitchStmt
       | TypeSwitchStmt
       | SelectStmt
       | GoStmt
       | DeferStmt
       | ErrDeferStmt
       | FailStmt
       | SendStmt
       | BreakStmt
       | ContinueStmt
       | FallthroughStmt
       | LabeledStmt
       | IncDecStmt
       | EmptyStmt ;

EmptyStmt  ::= ";" ;
ExprStmt   ::= Expr ";" ;
IncDecStmt ::= Expr ( "++" | "--" ) ";" ;
ErrDeferStmt ::= "errdefer" Expr ";" ;
FailStmt     ::= "fail" Expr ";" ;

SimpleStmt ::= Expr
             | ExprList AssignOp ExprList
             | IdentList ":=" ExprList
             | Expr ( "++" | "--" )
             | Expr "<-" Expr ;
```

`SimpleStmt` is the unterminated form used before the separating semicolon in
an `if`, `for`, or `switch` header. The corresponding standalone statement
productions include their terminating semicolon.

An expression statement is valid only when its expression, after removing
parentheses, is a function, method, or compiler-builtin call; a receive
expression; or a postfix `?` propagation expression. Any produced values are
discarded. Other expressions, including literals, identifiers, arithmetic,
composite literals, indexing, and conversions, are compile-time errors in
statement position.

The expression in a `defer`, `errdefer`, or `go` statement must be a function
or method call. The optional island target on `go` does not change that
requirement. Its callee must be a declared function, method, function literal,
or function value. Compiler built-ins such as `panic`, `recover`, `assert`,
`println`, `close`, and `append` are not first-class function values and cannot
be invoked directly by these statements. A function literal wrapper gives the
builtin an explicit ordinary call site when delayed or asynchronous behavior is
needed.

### 8.2 Blocks

```ebnf
Block ::= "{" Stmt* "}" ;
```

Blocks introduce lexical scope.

### 8.3 Assignments

```ebnf
Assignment ::= ExprList AssignOp ExprList ";" ;
AssignOp   ::= "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | "&=" | "|=" | "^=" | "&^=" ;
ExprList   ::= Expr ( "," Expr )* ;
```

**Evaluation and commit order**:

1. Every non-blank LHS location is prepared from left to right. Selector bases,
   pointer operands, collection descriptors or places, and index or map-key
   expressions needed to identify those locations are evaluated and frozen
   exactly once. No target is mutated in this phase. For a plain assignment,
   bounds and dereference checks needed to reach the final storage are deferred
   to that target's commit.
2. RHS expressions are evaluated from left to right and their values are saved.
   A single tuple-producing RHS is evaluated once before its elements are
   distributed.
3. Saved values are assigned to the frozen LHS locations from left to right.
   Each target performs its bounds, nil-dereference, and map-key validity checks
   as its commit begins. A successful earlier commit remains visible if a later
   target panics.

The same three phases apply to a single plain assignment. The blank identifier
has no location to resolve, although its corresponding RHS is still evaluated.
Consequently `a, b = b, a` swaps the values, and a later LHS cannot change the
index or map key saved for an earlier or later target.

For a compound assignment such as `a[f()] += g()`, the LHS location is resolved
once and its old value is loaded before the RHS is evaluated. The operation is
then computed from that saved old value and the RHS result, and the final value
is committed to the saved location. Thus `f()` runs once and precedes `g()`.
Every bounds or dereference check needed to load that old value also precedes
`g()`. An increment or decrement statement follows the same single-resolution,
single-load rule and then stores the incremented or decremented value.

For example, `a[f()][g()] = r()` evaluates `f`, then `g`, then `r`; only during
the commit does it check the saved outer and inner indexes and store. In
`a[f()][g()] += r()`, it evaluates `f`, checks the outer index, evaluates `g`,
checks the inner index and loads the old element, then evaluates `r`.

```vo
x = 10
a, b = b, a  // swap: evaluates b, a first, then assigns
count += 1
```

### 8.4 Return

```ebnf
ReturnStmt ::= "return" ExprList? ";" ;
```

**Named Return Values**: Result parameters can be named. Named results are local variables initialized to zero values at function start. A bare `return` returns the current values of named result variables.

For an explicit return list, every expression is evaluated from left to right
and converted to its result type before any named result variable is updated.
A single tuple-producing expression is evaluated once and its components are
converted and saved. The saved values are then committed to the result slots
left to right, after which eligible deferred calls run. A panic during return
expression evaluation or conversion occurs before that commit; named results
therefore retain the values they had at the point of the panic. A bare return
skips expression evaluation and begins deferred-call unwinding with the current
named results.

```vo
func sum(a, b int) (result int) {
    result = a + b
    return              // returns result (bare return)
}

func divmod(a, b int) (quot int, rem int) {
    quot = a / b
    rem = a % b
    return              // returns quot, rem
}
```

**Static Rules**:

| Function Returns | `return` Form | Validity |
|------------------|---------------|----------|
| Nothing (no ResultType) | `return` | ✅ Valid |
| Nothing | `return expr` | ❌ Error |
| Unnamed single type `T` | `return` | ❌ Error |
| Unnamed single type `T` | `return expr` | ✅ Required, expr must be assignable to `T` |
| **Named** single type `(name T)` | `return` | ✅ Returns named result |
| Unnamed multiple types | `return` | ❌ Error |
| Unnamed multiple types | `return e1, e2, ...` | ✅ Count and types must match |
| **Named** multiple types | `return` | ✅ Returns named results |

```vo
func f() { return }              // OK
func g() int { return 42 }       // OK
func h() (int, string) { return 1, "x" }  // OK
func i() int { return }          // ERROR: unnamed result
func j() { return 1 }            // ERROR: no result type
func k() (x int) { return }      // OK: named result
```

### 8.5 If

```ebnf
IfStmt ::= "if" ( SimpleStmt ";" )? Expr Block ( "else" ( IfStmt | Block ) )? ;
```

Optional init statement before condition. Condition must be type `bool`.

```vo
if x > 0 {
    // ...
} else if x < 0 {
    // ...
} else {
    // ...
}

// With init statement:
if x := compute(); x > 0 {
    println(x)
}

if err := doSomething(); err != nil {
    return err
}
```

### 8.6 For

```ebnf
ForStmt        ::= "for" ForClause? Block ;
ForClause      ::= Expr | ForThreeClause | ForRangeClause ;
ForThreeClause ::= SimpleStmt? ";" Expr? ";" SimpleStmt? ;
ForRangeClause ::= ( ExprList ( ":=" | "=" ) )? "range" Expr ;
```

**Parsing Note**: The parser distinguishes forms by the presence of `;` or `range` keyword.

In a three-part loop, the init statement executes once. Before each iteration,
the condition is evaluated and must have type `bool`; an omitted condition is
`true`. The post statement executes after the body and after an unlabeled
`continue`, then control returns to the condition. Variables declared by a
short declaration in the init statement have a fresh instance for each
iteration. The first instance receives the init value; each later instance is
initialized from the preceding iteration's value before the post statement is
applied. Closures and addresses captured in different iterations therefore do
not share that loop variable.

```vo
for x < 10 { ... }                    // while-style
for i := 0; i < 10; i += 1 { ... }    // C-style
for ; ; { ... }                       // infinite
for i, v := range slice { ... }       // range over slice
for k, v := range m { ... }           // range over map
for i := range slice { ... }          // range with index only
for range ch { ... }                  // range with no variables
```

**Range Semantics**:

- A range clause accepts at most two iteration variables and uses only `=` or
  `:=`.
- The range expression is evaluated exactly once before the first iteration.
  Rebinding the source variable in the loop body does not change the saved
  iteration source.
- Ranging over an array saves an array value snapshot. Ranging over a slice,
  string, map, channel, or receive-capable port saves its descriptor,
  reference, or capability value. A saved slice or map still refers to its
  underlying storage, so mutations through another alias follow that
  collection's ordinary mutation rules.
- For slices and arrays, `i` is the index (`int`) and `v` is the element value.
- For maps, `k` is the key and `v` is the value. Iteration order is unspecified.
- For strings, `i` is the byte index and `v` is the decoded rune (`int32`).
  UTF-8 is decoded as in `[]rune(s)`: an ill-formed encoding consumes one byte
  at its start and yields `U+FFFD`.
- For channels and receive-capable ports, `v` is each received value until the
  queue is closed and drained; this range accepts a single iteration variable.
  Ranging over a nil queue blocks forever.
- An integer range produces the values from zero through `n - 1` and accepts a
  single iteration variable. A typed integer bound preserves its integer type.
  An untyped bound defaults the new variable to `int` with `:=`; with `=`, the
  iteration value is assigned using the predeclared target's type. A zero or
  negative bound produces no iterations.
- In the assignment form (`=`), the iteration values are first saved in
  temporaries. All left-hand target operands, including address and index
  expressions, are then evaluated left-to-right for that iteration before any
  target is assigned. The assignments are committed left-to-right, matching
  ordinary parallel assignment.
- In the declaration form (`:=`), each iteration creates fresh variable
  instances before assigning that iteration's values. Closures and addresses
  captured in different iterations therefore refer to distinct variables. The
  assignment form (`=`) reuses its predeclared targets.

### 8.7 Switch

```ebnf
SwitchStmt   ::= "switch" ( SimpleStmt ";" )? Expr? "{" SwitchClause* "}" ;
SwitchClause ::= "case" ExprList ":" Stmt* | "default" ":" Stmt* ;
```

The optional init statement executes first. A present tag expression is then
evaluated exactly once and its value is saved for the lifetime of the switch.
Case expressions are considered in source order and are evaluated only until a
match is found. Each case expression is converted or checked against the tag
type using the ordinary assignment rules, then compared with the saved tag
using that type's complete `==` semantics from §2.5. This includes floating-point
value equality, recursive array and struct equality, and interface dynamic-type
and dynamic-value equality (with the usual comparability requirement on the
dynamic value).

If no tag expression is present, the switch behaves as though its saved tag
were `true`, so case expressions are boolean conditions evaluated in source
order. Cases implicitly break unless `fallthrough` is used.
A switch may contain at most one `default` clause, and that clause may appear
at any position among the cases. The default clause is selected only after all
case expressions fail to match; its source position does not stop evaluation of
later case expressions. Each clause is an independent lexical scope.

```vo
switch x {
case 1:
    println("one")
case 2, 3:
    println("two or three")
default:
    println("other")
}

// With init statement:
switch x := getValue(); x {
case 1:
    println("one")
}

// Without tag (boolean cases):
switch {
case x > 0:
    println("positive")
case x < 0:
    println("negative")
default:
    println("zero")
}
```

**Type Rule**: If any case expression is `nil`, the switch expression must have a reference type.

```vo
switch handler {
case nil:
    // handler is nil
default:
    handler.Serve()
}
```

### 8.8 Select

```ebnf
SelectStmt   ::= "select" "{" SelectCase* "}" ;
SelectCase   ::= ( "case" ( SelectSend | RecvStmt ) | "default" ) ":" Stmt* ;
SelectSend   ::= Expr "<-" Expr ;
RecvStmt     ::= ( IdentList ( ":=" | "=" ) )? "<-" Expr ;
```

`select` waits on local channel operations and receive-capable local ports. A
port send is invalid in a select case. On entry, every communication operand is
evaluated exactly once in source order: for a send case this includes the queue
expression followed by the value to send; for a receive case it includes the
queue expression. Receive targets are assigned only after their case is
selected. Nil queues disable their cases.

If one or more cases are ready, one ready case is chosen uniformly at random
and its communication completes before its body begins. If no case is ready
and a `default` exists, the default executes immediately. Otherwise the
goroutine blocks until a case can complete. A select with no communication
cases and no default blocks forever.

A receive case accepts at most two targets: the received value and the optional
success flag. Its assignment operator must be `=` or `:=`.

Each select case is an independent lexical scope. Variables declared by a
receive clause with `:=` enter scope after that clause and remain visible only
through the statements of that case. They are not visible in sibling cases, in
the default case, or after the `select`; separate cases may therefore reuse the
same receive-variable names. A select may contain at most one `default` case.

```vo
select {
case msg := <-ch1:
    println("received", msg)
case ch2 <- value:
    println("sent")
default:
    println("no communication ready")
}
```

### 8.9 Go

```ebnf
GoStmt ::= "go" ( "@" "(" Expr ")" )? Expr ";" ;
```

Without a target, `go` starts a goroutine on the current island. The expression
must be a function or method call. Its function value, receiver, and arguments
are evaluated and saved synchronously in the launching goroutine from left to
right; the new goroutine invokes the saved call. A panic during operand
evaluation therefore occurs in the launcher, while a panic from the invocation
occurs in the new goroutine.

Program completion waits for every goroutine started on the main island to
finish. Returning from `main` does not cancel those goroutines. Long-lived work
therefore needs an explicit stop protocol; standard-library APIs that start
repeating work return a handle that can stop it.

```vo
go handleRequest(conn)
go func() {
    // anonymous function
}()
```

The `go @(isl)` form transfers a call to another island. The target, callee,
receiver, arguments, and closure captures are evaluated on the source island;
every transferred value must satisfy the sendability rules in §6.6.1. Package
initialization completes on the destination island before the call begins.

```vo
worker := make(island)
result := make(port int, 1)

go @(worker) func(out port<- int) {
    out <- 42
}(result)

assert(<-result == 42)
```

### 8.10 Defer

```ebnf
DeferStmt ::= "defer" Expr ";" ;
```

Defers execution of a function or method call until the surrounding function
returns. The function value or receiver and then the arguments are evaluated
immediately from left to right and saved, while the invocation itself is
deferred. A nil saved function value therefore panics during deferred
invocation, not during registration. Ordinary `defer` and eligible `errdefer`
calls share one reverse-registration-order sequence; see §1.3.3.

```vo
func readFile(path string) {
    f := open(path)
    defer close(f)       // called when function returns
    // ... use f ...
}
```

### 8.11 Send

```ebnf
SendStmt ::= Expr "<-" Expr ";" ;
```

Sends a value to a channel or a port with send capability. Port sends additionally
require the value to satisfy the cross-island sendability contract.

```vo
ch <- 42
```

### 8.12 Labels and `goto`

```ebnf
LabeledStmt ::= Ident ":" Stmt ;
```

Labels are scoped to the function body and may be used by labeled `break` and
`continue` forms.

The token `goto` is reserved, but Vo has no `goto` statement. A parsed
`goto label` form is always a compile-time error; labeled `break` and
`continue` provide structured control transfer.

```vo
func example() {
outer:
    for item := range items {
        if item.Done {
            break outer
        }
    }
}
```

### 8.13 Fallthrough

```ebnf
FallthroughStmt ::= "fallthrough" ";" ;
```

In an expression `switch` case, `fallthrough` transfers control to the first
statement of the next clause without evaluating that clause's expressions. It
must be the final non-empty statement directly in the clause body. It is
invalid in the final clause, in a type switch, or inside a nested block of a
clause.

```vo
switch x {
case 1:
    println("one")
    fallthrough
case 2:
    println("one or two")
}
```

### 8.14 Break and Continue

```ebnf
BreakStmt    ::= "break" Ident? ";" ;
ContinueStmt ::= "continue" Ident? ";" ;
```

`break` exits innermost `for`, `switch`, or `select`. `continue` advances to next `for` iteration. Optional label targets a specific enclosing statement.

```vo
outer:
for i := 0; i < 10; i += 1 {
    for j := 0; j < 10; j += 1 {
        if condition {
            break outer   // breaks outer loop
        }
    }
}
```

---

## 9. Expressions

### 9.1 Expression Grammar

```ebnf
Expr      ::= OrExpr ;
OrExpr    ::= AndExpr ( "||" AndExpr )* ;
AndExpr   ::= EqExpr ( "&&" EqExpr )* ;
EqExpr    ::= RelExpr ( ( "==" | "!=" ) RelExpr )* ;
RelExpr   ::= AddExpr ( ( "<" | "<=" | ">" | ">=" ) AddExpr )* ;
AddExpr   ::= MulExpr ( ( "+" | "-" | "|" | "^" ) MulExpr )* ;
MulExpr   ::= UnaryExpr ( ( "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" ) UnaryExpr )* ;
UnaryExpr ::= ( "+" | "-" | "!" | "^" | "&" | "*" | "<-" ) UnaryExpr | Primary ;
```

Operators on the same grammar row have equal precedence and associate left to
right. Unary and postfix operators bind more tightly than every binary
operator. In particular, shifts share the multiplication-precedence row, so
`1 << 2 + 1` means `(1 << 2) + 1`.

#### 9.1.1 Runtime numeric operations

Except for `&&` and `||`, a binary expression evaluates and saves its left
operand before evaluating its right operand. `&&` evaluates the right operand
only when the saved left operand is true; `||` evaluates it only when the saved
left operand is false.

Runtime integer addition, subtraction, multiplication, negation, and bitwise
operations reduce their result modulo two to the width of the expression's
integer type. Signed overflow therefore wraps using two's-complement
representation and does not panic. Signed division truncates toward zero;
unsigned division uses the ordinary non-negative quotient. Remainder is
consistent with that quotient. Integer division or remainder by zero raises a
recoverable runtime panic. The minimum signed value divided by `-1` wraps back
to the minimum value under the same fixed-width rule.

A signed shift count that is negative raises a recoverable runtime panic. An
unsigned shift count is never interpreted as negative. If the count is at
least the width of the left operand's type, left shift and unsigned right shift
produce zero; signed right shift produces zero for a non-negative left operand
and `-1` for a negative left operand. Smaller left shifts discard bits beyond
the left operand's width, unsigned right shift fills with zero, and signed
right shift fills with the sign bit.

Runtime floating-point operations follow IEEE-754. Each `float32` operation is
rounded to `float32` before its result is observed; `float64` operations produce
`float64`. Floating-point division by zero produces the applicable infinity or
NaN and does not raise the integer division-by-zero panic.

### 9.2 Primary Expressions

```ebnf
Primary ::= Operand Postfix* ;
Postfix ::= Selector | Index | SliceExpr | Call | TypeAssertion | "?" | DynamicAccess ;
Operand ::= Ident | Literal | "(" Expr ")" | CompositeLit | Conversion | FuncLit ;
Literal ::= IntLit | FloatLit | RuneLit | StringLit ;
FuncLit ::= "func" "(" ParamList? ")" ResultType? Block ;
```

For an ordinary call, the function value or method receiver is evaluated
first. Arguments are then evaluated from left to right and converted to their
parameter types before invocation. A single tuple-valued argument expression
is evaluated once before its components are distributed. These operand side
effects occur even if invoking the saved function value subsequently panics.

### 9.3 Selectors, Indexing, and Slicing

```ebnf
Selector    ::= "." Ident ;
Index       ::= "[" Expr "]" ;
SliceExpr   ::= "[" Expr? ":" Expr? ( ":" Expr )? "]" ;
Call        ::= "(" ( ExprList "..."? ","? )? ")" ;
DynamicAccess ::= "~>" ( Ident Call? | "[" Expr "]" | Call ) ;
```

A package-qualified selector `pkg.Name` may refer only to an exported package
declaration. When `Name` is a variable, the selector is addressable: reads and
assignments access that island's copy of the package global. Package constants,
functions, and types cannot appear as assignment targets. Unexported package
members are inaccessible outside their declaring package.

An index expression evaluates and freezes its container first, then evaluates
its index exactly once, then checks the index and loads the element. Nested
indexing repeats that sequence one level at a time. Consequently
`a[f()][g()]` evaluates `a`, then `f`, checks and loads the outer element, and
only then evaluates `g`; an outer bounds panic prevents `g` from running.

Array, slice, and string indexes must satisfy `0 <= i < len(container)`. String
indexing returns the byte at that index as `uint8`. A map index evaluates its
map before its key, performs one lookup, and returns the element zero value when
the key is absent; the comma-ok form also returns `false`. An unhashable dynamic
value used through an interface map key raises a recoverable runtime panic.

**Slice Expressions**: Create a sub-slice from an array or slice.
- `a[low:high]` — elements from `low` to `high-1` (inclusive)
- `a[:high]` — elements from start to `high-1`
- `a[low:]` — elements from `low` to end
- `a[:]` — copy of entire slice
- `a[low:high:max]` — slice with explicit capacity (three-index slice)

**Three-Index Slice**: The form `a[low:high:max]` creates a slice with `len = high - low` and `cap = max - low`. This controls the capacity of the resulting slice, preventing it from accessing elements beyond `max` of the original array/slice.

The container descriptor or addressable array place is evaluated and frozen
first. Explicit `low`, `high`, and `max` expressions are then evaluated once in
that order, after which the complete bounds relation is checked. Omitted `low`
defaults to zero, omitted `high` to `len(a)`, and the implicit maximum to
`cap(a)`. A two-index slice requires `0 <= low <= high <= cap(a)` for an array
or slice, except that a string uses `len(a)` as its upper limit. A three-index
slice requires `0 <= low <= high <= max <= cap(a)`.

Slicing an array requires an addressable array operand and aliases that exact
storage. The rule applies equally to an array variable and to any addressable
array subobject, including a struct field, a nested array element, an array
element reached through a slice, and an array field reached through a struct
pointer. Indexing, assigning, or slicing such a subobject never substitutes a
temporary array copy. Slicing a slice preserves the ordinary shared backing
storage. A nil slice may be sliced with bounds valid for length and capacity
zero; the result remains nil.

```vo
a := [5]int{1, 2, 3, 4, 5}
s := a[1:3:4]       // len=2, cap=3 (elements 2,3; can grow to include 4)
s = a[1:3]          // len=2, cap=4 (can grow to include 4,5)
```

> **Note**: Three-index slice is not allowed on strings.

```vo
user.name       // field access (runtime error if user is nil)
arr[i]          // array/slice index
m["key"]        // map access
f(x, y)         // function call
s[1:3]          // slice expression
s[1:3:5]        // three-index slice with capacity
s[:len(s)-1]    // slice from start
```

### 9.4 Composite Literals

```ebnf
CompositeLit ::= Type "{" ElementList? "}" ;
ElementList  ::= Element ( "," Element )* ;
Element      ::= ( Key ":" )? Value ;
Key          ::= Ident | Expr ;
Value        ::= Expr | "{" ElementList? "}" ;
```

**Semantics**:
- Element expressions are evaluated in source order. For each map element, the
  key is evaluated and converted before its value; insertion completes before
  evaluation of the next element. Equal non-constant map keys are therefore
  permitted and a later element replaces the earlier value.
- For `struct`, elements are either all keyed or all positional. A keyed element
  names an existing field exactly once. A literal outside the field's declaring
  package cannot name or positionally initialize an unexported field.
- For `map`, every element has a key expression assignable to the map key type.
  Constant keys in one literal must be distinct after conversion to that type.
- For `array`/`slice`, `Key` is an optional non-negative constant integer index;
  an index cannot occur more than once and must be in bounds when the length is
  fixed.

**Nested literal type elision**: For nested composite literals, the inner type can be omitted when it can be inferred from the outer type:

```vo
u := User{name: "Alice", age: 30}     // struct
a := [3]int{1, 2, 3}                  // array
s := []int{10, 20}                    // slice
m := map[string]int{"a": 1, "b": 2}   // map with keyed elements

// Nested with type elision:
matrix := [][]int{{1, 2}, {3, 4}}     // inner []int elided
points := []Point{{1, 2}, {3, 4}}     // inner Point elided
```

### 9.5 Type Conversions

```ebnf
Conversion ::= Type "(" Expr ")" ;
```

The conversion evaluates its expression once and produces a value of the
destination type. A conversion is permitted in the following cases:

- the value is assignable to the destination type;
- source and destination have identical underlying types after struct tags are
  ignored;
- both types are unnamed pointer types whose base types have identical
  underlying types after struct tags are ignored;
- both types are integer or floating-point types;
- an integer, `[]byte`, or `[]rune` is converted to `string`; or
- a `string` is converted to `[]byte` or `[]rune`.

Conversions that cross an ordinary assignment boundary preserve the type's
value or reference semantics. In particular, converting between fixed-array
types copies the array value.

#### 9.5.1 Constants

Numeric constant conversions are checked using §5.3.5 and fail at compile time
when the constant is not representable by the destination type. They do not
use the runtime saturation rules below. Converting an integer constant to
`string` always succeeds and produces the same Unicode result as a runtime
integer-to-string conversion.

#### 9.5.2 Runtime numeric conversions

- **Integer to integer**: retain the low-order bits up to the destination
  width, then interpret those bits using the destination's signedness. The
  stored result is sign-extended for a signed destination and zero-extended for
  an unsigned destination. This rule covers widening, narrowing, and a change
  of signedness.
- **Integer to floating point**: interpret the source using its declared
  signedness, then round to the nearest destination value using IEEE-754
  round-to-nearest, ties-to-even.
- **`float32` to `float64`**: preserve the value exactly. **`float64` to
  `float32`**: round using IEEE-754 round-to-nearest, ties-to-even; finite
  overflow produces the correspondingly signed infinity. NaN remains NaN.
- **Floating point to integer**: first truncate the finite value toward zero,
  then clamp directly to the final destination type's range. NaN converts to
  zero. Negative values converted to an unsigned type clamp to zero. Positive
  and negative infinity, and finite overflow, clamp to the applicable maximum
  or minimum.

The final-width clamp is significant for narrow targets: for example, a large
`float64` converted to `uint8` produces `255`, never a wrapped low byte.

#### 9.5.3 String and rune conversions

- Converting an integer to `string` treats its numeric value as a Unicode code
  point. A valid Unicode scalar value is encoded as UTF-8. A negative value, a
  surrogate, or a value greater than `U+10FFFF` produces the single replacement
  character `U+FFFD`.
- `string(bytes)` copies the elements of a `[]byte` verbatim, including invalid
  UTF-8. Later mutation of the slice cannot change the string.
- `[]byte(s)` returns a new, independently mutable slice containing the exact
  bytes of `s`.
- `string(runes)` encodes each element of a `[]rune` as UTF-8, substituting
  `U+FFFD` for every invalid Unicode scalar value.
- `[]rune(s)` decodes UTF-8 into a new, independently mutable slice. Each byte
  that cannot begin a valid encoding consumes one byte and produces `U+FFFD`.

Converting an empty string to `[]byte` or `[]rune` produces a non-nil empty
slice. Converting a nil or empty byte/rune slice to `string` produces the empty
string.

```vo
f := 1e100
i := int(f)                      // maximum int value: runtime saturation
negative := -3.5
b := uint8(negative)             // 0
s := string(65)                  // "A"
replacement := string(-1)        // "�" (U+FFFD)
raw := string([]byte{0xff, 0x41}) // exact bytes; UTF-8 validity is not required
```

### 9.6 Type Assertions

```ebnf
TypeAssertion ::= Expr "." "(" Type ")" ;
```

A type assertion extracts the concrete value from an interface value.

```vo
var i interface{} = "hello"

s := i.(string)       // panics if i is not a string
s, ok := i.(string)   // ok is false if i is not a string, no panic
```

**Rules**:
- The expression must be of interface type
- If the assertion fails and no `ok` variable is used, a runtime panic occurs
- With the `ok` form, the zero value is returned and `ok` is `false`

### 9.7 Receive Expression

```ebnf
RecvExpr ::= "<-" Expr ;
```

Receives a value from a channel. Can be used in expressions or statements.

```vo
x := <-ch            // receive value
x, ok := <-ch        // receive with closed check
```

---

## 10. Built-in Functions

The following functions are **compiler built-ins**. Their signatures use meta-notation and do not imply language-level generics or variadic support.

| Function | Behavior |
|----------|----------|
| `len(s)` | Returns length of a string, array, slice, map, channel, or receive-capable port |
| `cap(s)` | Returns capacity of an array, slice, channel, or receive-capable port |
| `append(s, elems...)` | Returns new slice with elements appended |
| `copy(dst, src)` | Copies elements from a source slice to a destination slice; `string` is also accepted as the source for `[]byte`; returns the count copied |
| `delete(m, key)` | Deletes the element with the specified key from a map |
| `make(T, size?, cap?)` | Allocates and initializes a slice, map, channel, port, or island |
| `new(T)` | Allocates a zero-valued struct and returns `*T` |
| `close(ch)` | Closes a send-capable channel or an owner port |
| `panic(v)` | Stops normal execution and begins panicking |
| `recover()` | Captures a panic value during deferred function execution |
| `print(args...)` | Debug output (no newline) |
| `println(args...)` | Debug output (with newline) |
| `assert(cond, args...)` | Panics with a diagnostic string if condition is false |

Builtin arguments are evaluated from left to right exactly once, converted as
required, and saved before the builtin mutates any argument-visible storage.
The array `len`/`cap` constant exception below is the only deviation.

- `len` counts bytes in a string, elements in an array or slice, entries in a
  map, and currently buffered values in a channel or receive-capable port.
  `cap` returns an array's length, a slice's backing capacity, or a channel or
  receive-capable port's configured capacity. Nil slices, maps, channels, and
  receive-capable ports report zero where the operation is defined.
- For an array argument, `len` and `cap` are constants. The array expression is
  not evaluated when it contains no function call or receive operation. If it
  contains either, the expression is evaluated for those side effects before
  the same fixed length is returned.
- `append` returns a slice with the same defined slice type as its first
  argument. It reuses the backing array when capacity permits and otherwise
  allocates a new backing array. A nil slice accepts appends normally. Spread
  form requires a matching slice, with the additional form
  `append(bytes, text...)` for a `[]byte` destination and string source. The
  exact source bytes are appended. Self-append and other overlapping sources
  behave as though the source elements were saved before destination writes.
- `copy` returns `min(len(dst), len(src))` and supports overlapping source and
  destination ranges as though the copied elements were first saved. A string
  source may be copied into `[]byte`, using its exact bytes.
- `delete` is a no-op for a nil map or absent key. An interface key whose
  dynamic value is unhashable raises a recoverable runtime panic before the map
  changes.
- `close` follows the channel and port close contract in
  [`channel.md`](./channel.md): closing nil or an already closed queue panics;
  only a send-capable channel or an owner port may be closed.

**Usage Examples**:
```vo
s := make([]int, 10)      // slice of length 10
m := make(map[string]int) // empty map
s = append(s, 42)         // returns new slice
n := len(s)               // length
```

### 10.1 Panic and Recover

`panic(v)` evaluates `v`, records it as the current panic value, stops normal
execution of the calling function, and unwinds its stack. Language runtime
faults such as nil dereference, bounds failure, invalid interface comparison,
and integer division by zero begin the same recoverable unwinding process with
a diagnostic panic value. Deferred calls execute in reverse registration order
during unwinding.

`recover()` stops the current panic only when it is called directly by a
deferred function that is currently running because of that panic. A call from
a helper invoked by the deferred function is not direct. Outside that context,
or after the panic has already been recovered, `recover()` returns nil and has
no effect. A successful call returns the value supplied to `panic`; therefore
recovering `panic(nil)` succeeds even though the returned interface value is
nil. After recovery, the panicking function completes its return, remaining
ordinary defers continue in reverse registration order, and remaining
`errdefer` calls are discarded. Its named results retain their current values
and may still be changed by remaining ordinary defers; unnamed results use
their zero values. A new panic raised by a deferred call becomes the current
panic and continues unwinding.

```vo
func safeCall(f func()) (err string) {
    defer func() {
        if r := recover(); r != nil {
            err = "caught panic"
        }
    }()
    f()
    return ""
}

func dangerous() {
    panic("something went wrong")
}

result := safeCall(dangerous)  // result = "caught panic"
```

### 10.2 Make

`make` creates and initializes slices, maps, channels, ports, and islands:

```vo
s := make([]int, 10)         // slice with length 10, capacity 10
s := make([]int, 10, 20)     // slice with length 10, capacity 20
m := make(map[string]int)    // empty map
ch := make(chan int)         // unbuffered channel
ch := make(chan int, 10)     // buffered channel with capacity 10
messages := make(port string, 10) // positive capacity is required
worker := make(island)

p := new(Point)              // equivalent to &Point{}
p.x = 10
```

Slice length is required and capacity defaults to the length. Map and channel
capacity is optional. Port capacity is required and must be positive. `island`
takes no size argument. Runtime-negative sizes, `len > cap`, dimensions that do
not fit the target address width, and allocation overflow raise a runtime
panic; equivalent invalid constants are compile-time errors.

### 10.3 Assert

`assert` checks a condition and is active in both debug and release builds.

**Syntax**:
```ebnf
AssertCall ::= "assert" "(" Expr ( "," Expr )* ")" ;
```

**Parameters**:
- First argument: boolean condition (required)
- Subsequent arguments: optional diagnostic values formatted in the same
  space-separated form as `println`

**Behavior**:
- All arguments are evaluated before the assertion is checked.
- A condition that is the constant `false` is a compile-time error.
- If a runtime condition is `true`, execution continues with no further
  effect.
- If a runtime condition is `false`, `assert` panics with the string
  `"assertion failed"`, followed by `": "` and the formatted diagnostic
  values when any were supplied.
- The panic follows ordinary panic semantics: deferred calls run and `recover`
  may capture the diagnostic string. If it remains unrecovered, the command
  host reports it with source-location context.

**Examples**:
```vo
x := 10
assert(x > 0)                              // passes silently

y := -1
assert(y >= 0, "y=", y, "must be >= 0")
// runtime panic value: "assertion failed: y= -1 must be >= 0"

// Multiple diagnostic values
name := "test"
count := 0
assert(count > 0, "count=", count, "name=", name)
// runtime panic value: "assertion failed: count= 0 name= test"
```

**Notes**:
- `assert` is always active; there is no compile-time flag to disable it

---

## 11. Type Switches

```ebnf
TypeSwitchStmt  ::= "switch" ( SimpleStmt ";" )? TypeSwitchGuard "{" TypeCaseClause* "}" ;
TypeSwitchGuard ::= ( Ident ":=" )? Expr "." "(" "type" ")" ;
TypeCaseClause  ::= "case" TypeCaseList ":" Stmt* | "default" ":" Stmt* ;
TypeCaseList    ::= TypeCaseItem ( "," TypeCaseItem )* ;
TypeCaseItem    ::= Type | "nil" ;
```

A type switch evaluates its guard expression exactly once and compares the
dynamic type of the resulting interface value against its cases in source
order. The first matching case runs. A `case nil` matches a nil interface;
`default` runs only when no listed case matches. Cases do not fall through.

```vo
func describe(i interface{}) {
    switch v := i.(type) {
    case int:
        println("int:", v)
    case string:
        println("string:", v)
    case nil:
        println("nil")
    default:
        println("unknown type")
    }
}
```

**Rules**:
- The expression must be of interface type
- Each clause is an independent lexical scope
- With `switch v := x.(type)`, `v` exists only inside a clause. In a clause
  listing exactly one non-nil type, `v` has that type; in a multi-type,
  `case nil`, or `default` clause, it has the guard expression's interface type
- Multiple types can be listed: `case int, float64:`
- The same type and `nil` may each appear at most once across all cases
- At most one `default` clause is permitted; it may appear at any position

---

## 12. Example Program

```vo
package main

import "fmt"

type Error struct {
    msg string "json:\"message\""
}

type Logger interface {
    Log(msg string)
}

type ConsoleLogger struct {
    prefix string
}

func (l ConsoleLogger) Log(msg string) {
    fmt.Println(l.prefix + ": " + msg)
}

func main() int {
    var logger Logger  // logger == nil

    if logger == nil {
        logger = ConsoleLogger{prefix: "[APP]"}
    }

    logger.Log("Hello, Vo!")

    numbers := []int{1, 2, 3}
    numbers = append(numbers, 4)

    for i := 0; i < len(numbers); i += 1 {
        println(numbers[i])
    }

    return 0
}
```
