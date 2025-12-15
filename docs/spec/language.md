# GoX Language Specification

This document defines the syntax and semantics of the **GoX** programming language.

GoX is a statically typed, Go-like language.

## Differences from Go

GoX aims to match the Go language specification as closely as possible, except for the following intentional differences:

- **No generics**: GoX does not support type parameters.
- **No pointers**: GoX does not support pointer types or pointer operators (no `*T`, `&x`, `*x`).
- **No complex numbers**: GoX does not support `complex*` types, imaginary literals, or the `complex/real/imag` built-ins.
- **Explicit reference types**: GoX introduces an `object` composite type with reference semantics and `nil` as the zero value. `object` and `struct` are parallel composite types; a `struct` never "becomes" an `object`.
- **No method expressions/values**: Go method expressions (`T.M`) and method values (`x.M` used as a function value) are not supported. Use `func` literals to create closures instead (see §9.3).

---

## 1. Design Philosophy

### 1.1 Goals

- Familiar syntax for Go programmers
- Static, strong typing with local type inference
- Simple memory model: **object types** (heap-allocated, reference semantics) vs **value types** (copied on assignment)
- Multiple backend targets (LLVM, WASM, VM)

### 1.2 Non-Goals

The following are explicitly out of scope:

- Generics
- Explicit pointer types (`*T`), address-of (`&x`)

---

## 2. Memory Model

### 2.1 Value Types vs Object Types

GoX distinguishes two categories of types:

| Category | Types | Assignment | Zero Value |
|----------|-------|------------|------------|
| **Value** | `int`, `int8`, `int16`, `int32`, `int64`, `uint`, `uint8`, `uint16`, `uint32`, `uint64`, `float32`, `float64`, `bool`, `string`, `byte`, `rune`, `[N]T`, `struct` | Copies data | Type-specific |
| **Object** | `object { ... }`, `interface`, `[]T`, `map[K]V`, `chan T`, `func(...)` | Copies reference | `nil` |

**Value type zero values**:
- `int`, `int8`, ..., `uint64` → `0`
- `float32`, `float64` → `0.0`
- `bool` → `false`
- `string` → `""`
- `byte` → `0`
- `rune` → `0` (the null character)
- `[N]T` → each element is zero value of `T`
- `struct` → each field is zero value of its type

**Object type zero values**: Always `nil`.

### 2.2 Named Type Inheritance

When declaring `type T U`:
- `T` inherits the **category** (value or object) of `U`
- `T` inherits the **zero value** of `U`
- `T` inherits the **comparability** of `U`

```gox
type MyInt int       // value type, zero = 0, comparable
type Users []User    // object type, zero = nil
type Handler func()  // object type, zero = nil
```

### 2.3 Object Reference Semantics

GoX provides an `object` composite type, parallel to `struct`.

`struct` and `object` share similar surface syntax (fields, composite literals, selection, methods), but they are different types with different runtime behavior. In particular, a `struct` value is never an `object` value.

**Consequences**:

1. Variables of object type initialize to `nil`
2. Composite literals of object type (e.g., `T{...}` where `T` is an object type) allocate a new object
3. `make(T)` allocates a new object when `T` is an object type
4. Assignment of object values copies the reference: after `q := p`, both `q` and `p` refer to the same object
5. Mutations through one reference are visible through the other
6. Field access (`p.name`) or method calls (`p.Method()`) on `nil` are **runtime errors**

```gox
type User struct {
    name string
    age  int
}

type UserRef object {
    name string
    age  int
}

var u User            // zero-valued struct

var p UserRef         // p == nil
p.name = "x"          // RUNTIME ERROR: nil dereference

p = UserRef{}         // p != nil, new object created
p.name = "Alice"      // OK

r := make(UserRef)    // r != nil, new object created
r.name = "Carol"

q := p                // q and p refer to same object
q.name = "Bob"        // p.name is now also "Bob"
```

### 2.4 The `nil` Literal

`nil` represents the absence of an object for object types.

**Static Rules**:
- `nil` may be assigned to any object type
- `nil` cannot be assigned to value types (compile error)
- `var x = nil` is invalid: type cannot be inferred
- `x := nil` is invalid: type cannot be inferred (see §5.4)

**Runtime Rules**:
- Field access on `nil` → runtime error
- Method call on `nil` → runtime error
- Index access on `nil` slice → runtime error
- Index access on `nil` map is allowed (returns zero value; the `ok` result is `false`)
- Assignment to a `nil` map (`m[k] = v`) → runtime error

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
| `object` | ✅ | Reference identity (`==`/`!=`), or `nil` |
| `interface` | ✅ | Dynamic value equality (requires underlying comparable) or `nil` |
| `[]T` | ❌ | Only `== nil` / `!= nil` |
| `map[K]V` | ❌ | Only `== nil` / `!= nil` |
| `func(...)` | ❌ | Only `== nil` / `!= nil` |

**Rules**:
- `==` and `!=` require both operands to be comparable, OR one operand to be `nil` and the other an object type
- `<`, `<=`, `>`, `>=` are only valid for numeric types (`int`, `int8`, ..., `float64`) and `string`

```gox
1 == 2              // OK: int comparable
"a" < "b"           // OK: string ordered
p == nil            // OK: object vs nil
p == q              // OK: object reference comparison
s == nil            // OK: slice vs nil
s == t              // ERROR: slices not comparable
```

### 2.6 Parameter Passing

All parameters are passed by value. For object types, the "value" is a reference, so mutations inside a function affect the caller's object.

---

## 3. Lexical Structure

### 3.1 Identifiers

```ebnf
Ident  ::= Letter { Letter | Digit | "_" } ;
Letter ::= "A".."Z" | "a".."z" ;
Digit  ::= "0".."9" ;
```

Identifiers are case-sensitive.

### 3.2 Keywords

The following are reserved keywords:

```
 break     case      chan      const     continue
 default   defer     else      fallthrough
 for       func      go        goto      if
 import    interface map       object
 package   range     return    select    struct
 switch    type      var
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

// Constants
true  false  iota

// Zero value
nil

// Blank identifier
_

// Functions (compiler built-ins)
len  cap  append  copy  delete  make  close  panic  recover  print  println  assert
```

**Blank identifier `_`**: Used to discard values or declare unused variables. It can appear on the left side of assignments and short variable declarations.

```gox
_, err := doSomething()   // discard first return value
for _, v := range slice { // discard index
    println(v)
}
```

### 3.4 Operators and Punctuation

```
+    -    *    /    %
&    |    ^    &^              // bitwise: AND, OR, XOR, AND NOT (bit clear)
<<   >>
==   !=   <    <=   >    >=
&&   ||   !
<-                              // channel send/receive
++   --                        // increment/decrement (statements only)
=    :=   +=   -=   *=   /=   %=   <<=  >>=  &=  |=  ^=  &^=
(    )    [    ]    {    }
,    :    ;    .    ...
```

### 3.5 Literals

```ebnf
HexDigit ::= "0".."9" | "A".."F" | "a".."f" ;
OctDigit ::= "0".."7" ;
BinDigit ::= "0" | "1" ;

IntLit ::= DecLit | HexLit | OctLit | BinLit ;
DecLit ::= "0" | ("1".."9") ( "_"? Digit )* ;
HexLit ::= "0" ("x"|"X") HexDigit ( "_"? HexDigit )* ;
OctLit ::= "0" ("o"|"O") OctDigit ( "_"? OctDigit )* ;
BinLit ::= "0" ("b"|"B") BinDigit ( "_"? BinDigit )* ;

FloatLit    ::= DecFloatLit | HexFloatLit ;
DecFloatLit ::= ( Digit+ "." Digit* | "." Digit+ ) ( ("e"|"E") ("+"|"-")? Digit+ )?
             | Digit+ ("e"|"E") ("+"|"-")? Digit+ ;

HexDigits    ::= HexDigit ( "_"? HexDigit )* ;
HexMantissa  ::= HexDigits "." HexDigits?
              | "." HexDigits
              | HexDigits ;
HexExponent  ::= ("p"|"P") ("+"|"-")? Digit+ ;
HexFloatLit  ::= "0" ("x"|"X") HexMantissa HexExponent ;

UnicodeChar ::= /* any Unicode code point except newline */ ;
EscapeSeq ::= "\\" ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | "\\" | "'" | '"' ) ;
ByteEscape ::= "\\" OctDigit OctDigit OctDigit | "\\x" HexDigit HexDigit ;
UnicodeEscape ::= "\\u" HexDigit HexDigit HexDigit HexDigit
                | "\\U" HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit ;

RuneLit ::= "'" ( UnicodeChar | EscapeSeq | ByteEscape | UnicodeEscape ) "'" ;
StringLit ::= InterpretedStringLit | RawStringLit ;
InterpretedStringLit ::= '"' ( UnicodeChar | EscapeSeq | ByteEscape | UnicodeEscape )* '"' ;
RawStringLit ::= "`" ( /* any char except "`" */ )* "`" ;
```

Hexadecimal floating-point literals follow Go's syntax: they start with `0x`/`0X` and must use a binary exponent introduced by `p`/`P`.

```gox
0x1p-2      // 0.25
0x1.2p3     // 9.0
0X.8p0      // 0.5
0x1.p0      // 1.0
```

`rune` literals follow Go's Unicode semantics: a rune literal denotes a Unicode code point value. The allowed escape forms are aligned with Go:

- `\\a \\b \\f \\n \\r \\t \\v \\\\ \\'` (and `\\"` only inside string literals)
- Octal byte escape: `\\` followed by exactly three octal digits (value 0..255)
- Hex byte escape: `\\x` followed by exactly two hex digits (value 0..255)
- Unicode escapes: `\\u` (4 hex digits), `\\U` (8 hex digits), must be valid Unicode code points (no surrogates, max `0x10FFFF`)

Escape sequences: `\n`, `\t`, `\\`, `\"`.

Raw string literals use backquotes and may contain any characters except a backquote. Backslashes have no special meaning and escapes are not processed. Newlines are permitted.

### 3.6 Semicolons

Semicolons terminate statements and declarations. The lexer automatically inserts a semicolon after a line's final token if that token is:
- An identifier or basic literal
- One of the keywords `break`, `continue`, `fallthrough`, `return`
- One of the operators `++`, `--`
- A closing delimiter: `)`, `]`, `}`

### 3.7 Comments

```
// Single-line comment
/* Multi-line
   comment */
```

---

## 4. Program Structure

### 4.1 Source Files

```ebnf
File ::= PackageClause? ImportDecl* TopDecl* ;

PackageClause ::= "package" Ident ";" ;
ImportDecl    ::= "import" StringLit ";" ;
```

### 4.2 Top-Level Declarations

```ebnf
TopDecl ::= VarDecl
          | ConstDecl
          | TypeDecl
          | InterfaceDecl
          | FuncDecl ;
```

---

## 5. Declarations

### 5.1 Variables

```ebnf
VarDecl     ::= "var" ( VarSpec ";" | "(" VarSpecList ")" ";" ) ;
VarSpecList ::= ( VarSpec ";" )* ;
VarSpec     ::= IdentList Type? ( "=" ExprList )? ;
```

**Grouped declarations** use parentheses:
```gox
var (
    x int
    y = 42
    a, b = 1, 2
)
```

**Static Rules**:
- If `Type` is omitted, `Expr` is required and type is inferred
- If `Expr` is omitted, variable is initialized to zero value (which is `nil` for object types)
- If `Expr` is `nil`, `Type` is required

```gox
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
```gox
const (
    Pi = 3.14159
    E  = 2.71828
)
```

**iota**: A predeclared identifier representing successive untyped integer constants. It resets to 0 at each `const` block and increments by 1 for each ConstSpec.

```gox
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

```gox
const Pi = 3.14159
const MaxSize int = 1024
const Empty = nil   // ERROR: nil cannot be const
```

### 5.3 Numeric Constants

GoX follows Go's numeric constant model (with complex numbers removed), which provides arbitrary-precision arithmetic at compile time and flexible type conversion.

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

```gox
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

```gox
x := 42       // x is int (default type of untyped integer)
y := 3.14     // y is float64 (default type of untyped floating-point)
z := 'a'      // z is rune (default type of untyped rune)
s := "hi"     // s is string
```

#### 5.3.3 Implicit Conversion of Untyped Constants

An untyped constant can be implicitly converted to any compatible type:

```gox
var i int = 42        // OK: untyped 42 → int
var f float64 = 42    // OK: untyped 42 → float64
var b byte = 42       // OK: untyped 42 → byte (if in range)

type MyInt int
var m MyInt = 42      // OK: untyped 42 → MyInt

const Pi = 3.14159
var f64 float64 = Pi  // OK: untyped floating-point → float64
```

**Typed constants** cannot be implicitly converted:

```gox
const TypedInt int = 42
type MyInt int
var m MyInt = TypedInt   // ERROR: int is not MyInt
var m MyInt = 42         // OK: untyped constant
```

#### 5.3.4 Arbitrary Precision

Numeric constants are represented with arbitrary precision at compile time. They do not overflow during constant evaluation:

```gox
const Huge = 1 << 100           // OK: very large untyped integer
const Four = Huge >> 98         // Four == 4
const Result = Huge / 1e97      // OK: computed at compile time

var x int = Huge                // ERROR: Huge overflows int
var y int = Four                // OK: Four fits in int
```

**Implementation requirement**: Compilers must support at least 256-bit integer constants and 256-bit mantissa for floating-point constants.

#### 5.3.5 Representability

A constant `x` is **representable** by a value of type `T` if:

1. `x` is in the set of values determined by `T`, OR
2. `T` is a floating-point type and `x` can be rounded to `T`'s precision without overflow

```gox
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

```gox
const a = 1 + 2       // integer + integer → integer (3)
const b = 1 + 2.0     // integer + float → float (3.0)
const c = 'a' + 1     // rune + integer → rune ('b')
const d = 'a' + 1.0   // rune + float → float (98.0)
```

#### 5.3.7 Constant Expressions

Constant expressions are evaluated at compile time with exact precision:

```gox
const (
    Pi    = 3.14159265358979323846264338327950288419716939937510582097494459
    Tau   = 2 * Pi          // Full precision maintained
    Theta = Pi / 2
)

var f float64 = Pi          // Rounded to float64 precision
```

**Division by zero** in constant expressions is a compile-time error:

```gox
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
- Declares new variables and may reassign existing variables in the same scope, following Go's rules: at least one variable on the left-hand side must be newly declared, and all non-blank variables must already be declared in the same scope.
- Number of identifiers must equal number of expressions
- Type of each variable = static type of corresponding expression
- If expression is `nil` with no inferable context type → compile error

```gox
x := 10
x, y := 20, "hi"  // OK: x reassigned, y newly declared
x := nil          // ERROR: cannot infer type from nil
```

### 5.5 Type Declarations

```ebnf
TypeDecl ::= "type" Ident Type ";" ;
```

The new type inherits the category (value/object), zero value, and comparability from the underlying type (see §2.2).

```gox
type User struct {
    name string
    age  int "json:\"name\""
}
```

> **Note**: Struct field tags are metadata strings. Their semantics are defined by tooling/standard library (e.g., JSON serialization). The compiler preserves them in the AST but does not interpret them.

---

## 6. Types

### 6.1 Type Grammar

```ebnf
Type ::= Ident
       | ObjectType
       | ArrayType
       | SliceType
       | MapType
       | ChanType
       | FuncType
       | StructType
       | InterfaceType ;
```

```ebnf
InterfaceType ::= "interface" "{" InterfaceElem* "}" ;
InterfaceElem ::= MethodSpec | EmbeddedIface ;
MethodSpec    ::= Ident "(" ParamList? ")" ResultType? ";" ;  // ParamList defined in §7.1
EmbeddedIface ::= Ident ";" ;
```

### 6.2 Built-in Types

| Type | Category | Description |
|------|----------|-------------|
| `bool` | Value | Boolean (`true`/`false`) |
| `int`, `int8`, `int16`, `int32`, `int64` | Value | Signed integers |
| `uint`, `uint8`, `uint16`, `uint32`, `uint64` | Value | Unsigned integers |
| `float32`, `float64` | Value | IEEE-754 floating point |
| `string` | Value | Immutable UTF-8 string |
| `byte` | Value | Alias for `uint8` |
| `rune` | Value | Alias for `int32` (Unicode code point) |

### 6.3 Arrays

```ebnf
ArrayType ::= "[" IntLit "]" Type ;
```

Arrays are value types with fixed length.

```gox
var a [4]int  // [0, 0, 0, 0]
```

### 6.4 Slices

```ebnf
SliceType ::= "[" "]" Type ;
```

Slices are object types referencing a dynamic sequence.

```gox
var s []int         // s == nil
s = []int{1, 2, 3}  // s != nil
```

### 6.5 Maps

```ebnf
MapType ::= "map" "[" Type "]" Type ;
```

Maps are object types providing key-value storage.

**Key type restriction**: The key type must be **comparable**:

| Valid Keys | Invalid Keys |
|------------|--------------|
| `int`, `int8`, ..., `uint64` | `[]T` (slice) |
| `float32`, `float64` | `map[K]V` |
| `bool`, `string`, `byte`, `rune` | `func(...)` |
| `[N]T` (array of comparable) | |
| `struct` (if comparable) | |
| `object` | |
| `interface` | |

```gox
var m map[string]int      // m == nil
m = map[string]int{}      // m != nil, empty map
m["key"] = 42

var bad map[[]int]int     // ERROR: slices are not comparable
```

### 6.6 Channels

```ebnf
ChanType ::= ( "chan" | "chan" "<-" | "<-" "chan" ) Type ;
```

Channels are object types used for communication between goroutines. Zero value is `nil`.

**Channel directions**:
- `chan T` — bidirectional (can send and receive)
- `chan<- T` — send-only
- `<-chan T` — receive-only

```gox
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

```gox
ch <- 42           // send
x := <-ch          // receive
close(ch)          // close channel (no more sends allowed)
```

### 6.7 Functions

```ebnf
FuncType      ::= "func" "(" ParamTypeList? ")" ResultType? ;
ParamTypeList ::= Type ( "," Type )* ;
ResultType    ::= Type | "(" Type ( "," Type )* ")" ;
```

Function types are object types. Zero value is `nil`.

```gox
var f func(int) int  // f == nil
```

### 6.8 Variadic Functions

```ebnf
VariadicParam ::= Ident "..." Type ;
```

A function may have a variadic final parameter. The caller may pass zero or more arguments of that type.

```gox
func sum(nums ...int) int {
    total := 0
    for i := 0; i < len(nums); i += 1 {
        total += nums[i]
    }
    return total
}

sum(1, 2, 3)        // nums = []int{1, 2, 3}
sum()               // nums = []int{}

s := []int{1, 2, 3}
sum(s...)           // spread slice as arguments
```

### 6.9 Structs

```ebnf
StructType ::= "struct" "{" FieldDecl* "}" ;
FieldDecl  ::= ( IdentList Type | Type ) Tag? ";" ;
Tag        ::= StringLit ;
```

Structs are **value types**. Assignment copies the struct value. The zero value of a struct is a struct value where each field is the zero value of its type.

**Anonymous fields** (embedding): A field can be just a type name, creating an embedded field. The type must be a named type.

```gox
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

### 6.10 Objects

```ebnf
ObjectType ::= "object" "{" FieldDecl* "}" ;
```

Objects are **object types** with reference semantics. The zero value is `nil`. Assignment copies the reference, not the data.

`object` and `struct` share similar surface syntax (fields, composite literals, selection, methods), but they are different types with different runtime behavior.

```gox
type UserRef object {
    id   int
    name string
}

var p UserRef         // p == nil
p = UserRef{}         // p != nil, new object allocated
p.name = "Alice"

r := make(UserRef)    // r != nil, new object allocated
r.name = "Carol"

q := p                // q and p refer to same object
q.name = "Bob"        // p.name is now also "Bob"
```

**Anonymous fields** (embedding) work the same as in structs.

---

## 7. Interfaces and Methods

### 7.1 Interface Declarations

```ebnf
InterfaceDecl ::= "interface" Ident "{" InterfaceElem* "}" ";" ;
InterfaceElem ::= MethodSpec | EmbeddedIface ;
MethodSpec    ::= Ident "(" ParamList? ")" ResultType? ";" ;
EmbeddedIface ::= Ident ";" ;
ParamList     ::= Param ( "," Param )* ;
Param         ::= IdentList Type ;   // Type sharing: x, y int
```

```gox
interface Reader {
    Read(buf []byte) int
}

interface ReadWriter {
    Reader  // embedding
    Write(buf []byte) int
}
```

> **Scope Limitation**: Embedded interfaces must be unqualified names from the current package.

### 7.2 Interface Method Sets

The **method set** of an interface is computed as follows:

1. Start with directly declared methods
2. For each embedded interface `I`, recursively compute its method set
3. Union all method sets
4. If two methods have the same name:
   - If signatures are identical → collapse to one method
   - If signatures differ → compile error

```gox
interface A { Foo() int }
interface B {
    Foo() int
    Bar()
}
interface C {
    A
    B
}  // method set = {Foo() int, Bar()}

interface D { Foo() string }
interface E {
    A
    D
}  // ERROR: Foo has conflicting signatures
```

### 7.3 Type Method Sets

The **method set** of a named type `T` is the set of all methods declared with receiver type `T`.

```gox
type User struct { ... }

func (u User) Name() string { ... }
func (u User) SetName(n string) { ... }

// Method set of User = {Name() string, SetName(string)}
```

Only **named types** can have methods. You cannot define methods on built-in types, arrays, slices, maps, or function types directly.

### 7.4 Method Declarations

```ebnf
FuncDecl ::= "func" Receiver? Ident "(" ParamList? ")" ResultType? Block ;
Receiver ::= "(" Ident Ident ")" ;
```

The receiver consists of a name and a **named type**. Anonymous types (arrays, slices, maps, func) are not allowed as receivers.

```gox
func (u User) Name() string {
    return u.name
}

func (u User) SetName(name string) {
    u.name = name  // NOTE: User is a struct, so this modifies a copy
}

func (a [4]int) Sum() int { ... }  // ERROR: receiver must be named type
```

**Runtime**: Calling a method on a `nil` receiver is a runtime error.

---

## 8. Statements

### 8.1 Statement Grammar

```ebnf
Stmt ::= Block
       | VarDecl
       | ConstDecl
       | ShortVarDecl
       | Assignment
       | ExprStmt
       | ReturnStmt
       | IfStmt
       | ForStmt
       | SwitchStmt
       | SelectStmt
       | GoStmt
       | DeferStmt
       | SendStmt
       | BreakStmt
       | ContinueStmt
       | GotoStmt
       | FallthroughStmt
       | LabeledStmt
       | IncDecStmt
       | EmptyStmt ;

EmptyStmt  ::= ";" ;
ExprStmt   ::= Expr ";" ;
IncDecStmt ::= Expr ( "++" | "--" ) ";" ;
```

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

**Evaluation Order** (for multi-assignment `a, b = x, y`):
1. All RHS expressions are evaluated left-to-right
2. Results are stored in temporaries
3. All LHS locations are assigned left-to-right from temporaries

This guarantees that `a, b = b, a` swaps the values (or references, for object types).

```gox
x = 10
a, b = b, a  // swap: evaluates b, a first, then assigns
count += 1
```

### 8.4 Return

```ebnf
ReturnStmt ::= "return" ExprList? ";" ;
```

**Static Rules**:

| Function Returns | `return` Form | Validity |
|------------------|---------------|----------|
| Nothing (no ResultType) | `return` | ✅ Required |
| Nothing | `return expr` | ❌ Error |
| Single type `T` | `return` | ❌ Error |
| Single type `T` | `return expr` | ✅ Required, expr must be assignable to `T` |
| Multiple types `(T1, T2, ...)` | `return` | ❌ Error |
| Multiple types | `return e1, e2, ...` | ✅ Count and types must match |

```gox
func f() { return }              // OK
func g() int { return 42 }       // OK
func h() (int, string) { return 1, "x" }  // OK
func i() int { return }          // ERROR
func j() { return 1 }            // ERROR
```

### 8.5 If

```ebnf
IfStmt     ::= "if" ( SimpleStmt ";" )? Expr Block ( "else" ( IfStmt | Block ) )? ;
SimpleStmt ::= ExprStmt | Assignment | ShortVarDecl | IncDecStmt | SendStmt ;
```

Optional init statement before condition. Condition must be type `bool`.

```gox
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
ForStmt        ::= "for" ForClause Block ;
ForClause      ::= Expr | ForThreeClause | ForRangeClause ;
ForThreeClause ::= SimpleStmt? ";" Expr? ";" SimpleStmt? ;
ForRangeClause ::= ( IdentList ( ":=" | "=" ) )? "range" Expr ;
```

**Parsing Note**: The parser distinguishes forms by the presence of `;` or `range` keyword.

```gox
for x < 10 { ... }                    // while-style
for i := 0; i < 10; i += 1 { ... }    // C-style
for ; ; { ... }                       // infinite
for i, v := range slice { ... }       // range over slice
for k, v := range m { ... }           // range over map
for i := range slice { ... }          // range with index only
for range ch { ... }                  // range with no variables
```

**Range Semantics**:
- For slices/arrays: `i` is index (int), `v` is element value
- For maps: `k` is key, `v` is value
- For strings: `i` is byte index, `v` is rune (`int32`)
- For channels: `v` is received value (single variable only)

### 8.7 Switch

```ebnf
SwitchStmt    ::= "switch" ( SimpleStmt ";" )? Expr? "{" CaseClause* DefaultClause? "}" ;
CaseClause    ::= "case" ExprList ":" Stmt* ;
DefaultClause ::= "default" ":" Stmt* ;
```

Optional init statement and optional tag expression. If no tag expression, cases are evaluated as boolean conditions.

Cases implicitly break unless `fallthrough` is used.

```gox
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

**Type Rule**: If any case expression is `nil`, the switch expression must have an object type.

```gox
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
SelectCase   ::= ( "case" ( SendStmt | RecvStmt ) | "default" ) ":" Stmt* ;
RecvStmt     ::= ( IdentList ( ":=" | "=" ) )? "<-" Expr ;
```

`select` waits on multiple channel operations. One ready case is chosen at random. If no case is ready and there's a `default`, it executes immediately.

```gox
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
GoStmt ::= "go" Expr ";" ;
```

Starts a new goroutine executing the function call. The expression must be a function or method call.

```gox
go handleRequest(conn)
go func() {
    // anonymous function
}()
```

### 8.10 Defer

```ebnf
DeferStmt ::= "defer" Expr ";" ;
```

Defers execution of a function call until the surrounding function returns. Arguments are evaluated immediately, but the call is deferred. Multiple defers execute in LIFO order.

```gox
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

Sends a value to a channel. The first expression must be a channel type, the second is the value to send.

```gox
ch <- 42
```

### 8.12 Goto and Labels

```ebnf
GotoStmt    ::= "goto" Ident ";" ;
LabeledStmt ::= Ident ":" Stmt ;
```

`goto` transfers control to the labeled statement. Labels are scoped to the function body.

```gox
func example() {
    if condition {
        goto cleanup
    }
    // ... normal code ...
cleanup:
    // cleanup code
}
```

### 8.13 Fallthrough

```ebnf
FallthroughStmt ::= "fallthrough" ";" ;
```

In a `switch` case, `fallthrough` transfers control to the first statement of the next case. It must be the last statement in a case.

```gox
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

```gox
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
RelExpr   ::= ShiftExpr ( ( "<" | "<=" | ">" | ">=" ) ShiftExpr )* ;
ShiftExpr ::= AddExpr ( ( "<<" | ">>" ) AddExpr )* ;
AddExpr   ::= MulExpr ( ( "+" | "-" | "|" | "^" ) MulExpr )* ;
MulExpr   ::= UnaryExpr ( ( "*" | "/" | "%" | "&" | "&^" ) UnaryExpr )* ;
UnaryExpr ::= ( "+" | "-" | "!" | "^" ) UnaryExpr | Primary ;
```

### 9.2 Primary Expressions

```ebnf
Primary ::= Operand ( Selector | Index | SliceExpr | Call | TypeAssertion )* ;
Operand ::= Ident | Literal | "(" Expr ")" | CompositeLit | Conversion | FuncLit ;
Literal ::= IntLit | FloatLit | RuneLit | StringLit ;
FuncLit ::= "func" "(" ParamList? ")" ResultType? Block ;
```

### 9.3 Selectors, Indexing, and Slicing

```ebnf
Selector    ::= "." Ident ;
Index       ::= "[" Expr "]" ;
SliceExpr   ::= "[" Expr? ":" Expr? "]" ;
Call        ::= "(" ( Expr ( "," Expr )* "..."? )? ")" ;
```

> **Note**: GoX does not support Go's method expressions (`T.M`) or method values (`x.M` used as a function value). To pass a method as a value, use a `func` literal:

```gox
// Instead of: f := x.M
f := func(a int) int { return x.M(a) }
```

**Slice Expressions**: Create a sub-slice from an array or slice.
- `a[low:high]` — elements from `low` to `high-1` (inclusive)
- `a[:high]` — elements from start to `high-1`
- `a[low:]` — elements from `low` to end
- `a[:]` — copy of entire slice

```gox
user.name       // field access (runtime error if user is nil)
arr[i]          // array/slice index
m["key"]        // map access
f(x, y)         // function call
s[1:3]          // slice expression
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
- For `struct`: `Key` must be a field name (`Ident`)
- For `map`: `Key` is an expression of the key type
- For `array`/`slice`: `Key` is an optional integer index

**Nested literal type elision**: For nested composite literals, the inner type can be omitted when it can be inferred from the outer type:

```gox
u := User{name: "Alice", age: 30}     // struct
a := [3]int{1, 2, 3}                  // array
s := []int{10, 20}                    // slice
m := map[string]int{"a": 1, "b": 2}   // map (key is Expr)

// Nested with type elision:
matrix := [][]int{{1, 2}, {3, 4}}     // inner []int elided
points := []Point{{1, 2}, {3, 4}}     // inner Point elided
```

### 9.5 Type Conversions

```ebnf
Conversion ::= Type "(" Expr ")" ;
```

```gox
i := int(f)
s := string(65)  // implementation-defined
```

### 9.6 Type Assertions

```ebnf
TypeAssertion ::= Expr "." "(" Type ")" ;
```

A type assertion extracts the concrete value from an interface value.

```gox
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

```gox
x := <-ch            // receive value
x, ok := <-ch        // receive with closed check
```

---

## 10. Built-in Functions

The following functions are **compiler built-ins**. Their signatures use meta-notation and do not imply language-level generics or variadic support.

| Function | Behavior |
|----------|----------|
| `len(s)` | Returns length of slice, map, string, array, or channel |
| `cap(s)` | Returns capacity of slice or channel |
| `append(s, elems...)` | Returns new slice with elements appended |
| `copy(dst, src)` | Copies elements from src slice to dst slice, returns count copied |
| `delete(m, key)` | Deletes the element with the specified key from a map |
| `make(T, size?, cap?)` | Allocates and initializes slice, map, channel, or object |
| `close(ch)` | Closes a channel (no more sends allowed) |
| `panic(v)` | Stops normal execution and begins panicking |
| `recover()` | Captures a panic value during deferred function execution |
| `print(args...)` | Debug output (no newline) |
| `println(args...)` | Debug output (with newline) |
| `assert(cond, args...)` | Terminates program if condition is false |

**Usage Examples**:
```gox
s := make([]int, 10)      // slice of length 10
m := make(map[string]int) // empty map
s = append(s, 42)         // returns new slice
n := len(s)               // length
```

### 10.1 Panic and Recover

`panic` stops normal execution and begins unwinding the stack. Deferred functions still execute. `recover` can capture the panic value if called within a deferred function.

```gox
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

`make` creates and initializes slices, maps, channels, and objects:

```gox
s := make([]int, 10)         // slice with length 10, capacity 10
s := make([]int, 10, 20)     // slice with length 10, capacity 20
m := make(map[string]int)    // empty map
ch := make(chan int)         // unbuffered channel
ch := make(chan int, 10)     // buffered channel with capacity 10

type UserRef object {
    name string
}
p := make(UserRef)           // new object allocated
p.name = "Alice"
```

### 10.3 Assert

`assert` checks a condition and terminates the program if it is false. It is active in both debug and release builds.

**Syntax**:
```ebnf
AssertCall ::= "assert" "(" Expr ( "," Expr )* ")" ;
```

**Parameters**:
- First argument: boolean condition (required)
- Subsequent arguments: diagnostic values printed on failure (optional, same behavior as `println`)

**Behavior**:
- If condition is `true`: no effect, execution continues
- If condition is `false`: prints error message to stderr and terminates the program

**Output Format**:
```
assertion failed: <arg2><arg3>...
  at <file>:<line>
```

Without additional arguments:
```
assertion failed
  at main.gox:42
```

**Examples**:
```gox
x := 10
assert(x > 0)                              // passes silently

y := -1
assert(y >= 0, "y=", y, " must be >= 0")   // fails, prints:
// assertion failed: y=-1 must be >= 0
//   at main.gox:5

// Multiple diagnostic values
name := "test"
count := 0
assert(count > 0, "count=", count, " name=", name)
// assertion failed: count=0 name=test
//   at main.gox:10
```

**Notes**:
- Unlike `panic`, `assert` does not unwind the stack or run deferred functions
- The diagnostic arguments follow the same formatting rules as `println`
- `assert` is always active; there is no compile-time flag to disable it

---

## 11. Type Switches

```ebnf
TypeSwitchStmt ::= "switch" ( Ident ":=" )? Expr "." "(" "type" ")" "{" TypeCaseClause* "}" ;
TypeCaseClause ::= "case" TypeList ":" Stmt* | "default" ":" Stmt* ;
TypeList       ::= Type ( "," Type )* ;
```

A type switch compares the dynamic type of an interface value against multiple types.

```gox
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
- `v` is bound to the asserted type in each case
- Multiple types can be listed: `case int, float64:`
- `default` handles unmatched types

---

## 12. Example Program

```gox
package main

import "std/io"

type Error struct {
    msg string "json:\"message\""
}

interface Logger {
    Log(msg string)
}

type ConsoleLogger struct {
    prefix string
}

func (l ConsoleLogger) Log(msg string) {
    io.Println(l.prefix + ": " + msg)
}

func main() int {
    var logger Logger  // logger == nil

    if logger == nil {
        logger = ConsoleLogger{prefix: "[APP]"}
    }

    logger.Log("Hello, GoX!")

    numbers := []int{1, 2, 3}
    numbers = append(numbers, 4)

    for i := 0; i < len(numbers); i += 1 {
        println(numbers[i])
    }

    return 0
}
```
