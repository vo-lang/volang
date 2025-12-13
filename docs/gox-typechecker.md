# GoX Type Checker Design

This document describes the design of the GoX type checker, including core challenges, solutions, implementation phases, and testing strategies.

---

## 1. Design Overview

### 1.1 Type Checker Responsibilities

The Type Checker sits in the compiler frontend, receiving the AST from the Parser and producing a type-annotated AST along with diagnostics:

```
AST (untyped) → Type Checker → Typed AST + Diagnostics
```

Main responsibilities:
- **Type Inference**: Infer types for variables and expressions
- **Type Checking**: Validate type legality of operations
- **Symbol Resolution**: Bind identifiers to their declarations
- **Constant Evaluation**: Compute constant expressions at compile time

### 1.2 GoX Type System Characteristics

GoX is a statically typed language with a type system based on Go but simplified:

| Feature | GoX | Go |
|---------|-----|-----|
| Generics | ❌ | ✅ |
| Pointers | ❌ | ✅ |
| Complex Numbers | ❌ | ✅ |
| Object Type | ✅ (reference semantics) | ❌ |

Key concepts:
- **Value Types**: `bool`, `int*`, `float*`, `string`, `array`, `struct` — copied on assignment
- **Object Types**: `slice`, `map`, `chan`, `func`, `object`, `interface` — reference copied on assignment, zero value is `nil`

---

## 2. Core Challenges and Solutions

### 2.1 Challenge 1: Forward References

**Problem**: GoX allows using types before they are declared:

```gox
var x MyInt      // MyInt declared below
type MyInt int
```

**Solution**: Multi-pass scanning architecture

1. **First pass**: Collect all declaration names, build symbol table skeleton
2. **Second pass**: Resolve type definitions, fill in type information
3. **Third pass**: Check function bodies

This ensures all type information is available when checking function bodies.

### 2.2 Challenge 2: Type Cycle Detection

**Problem**: Type definitions may form illegal cycles:

```gox
type A B
type B A  // illegal cycle

type Node struct {
    next Node  // illegal: struct cannot directly contain itself
}

type Node object {
    next Node  // legal: object is a reference type
}
```

**Solution**:

- Maintain a "currently resolving" set during type resolution
- Encountering a type name being resolved → report cycle error
- Exception: Indirect references through `object`, `slice`, `map`, `interface` are legal

Rule: If the path from type A back to itself consists entirely of value types (struct, array), it's an illegal cycle.

### 2.3 Challenge 3: Untyped Constants

**Problem**: GoX constants have complex type inference rules:

```gox
const x = 42        // untyped int
const y = 3.14      // untyped float
const z = x + y     // untyped float (promotion)

var a int = x       // OK: 42 is representable as int
var b byte = 256    // ERROR: 256 exceeds byte range
```

**Solution**:

1. **Arbitrary precision representation**: Use `BigInt` / `BigRational` to store constant values
2. **Kind system**: Distinguish five kinds: `int`, `rune`, `float`, `bool`, `string`
3. **Promotion rules**: `int < rune < float`, mixed operations take the higher kind
4. **Representability check**: Verify value fits in target type range on assignment

### 2.4 Challenge 4: Interface Implementation Checking

**Problem**: Need to determine if a type implements an interface:

```gox
interface Reader {
    Read(buf []byte) int
}

type File object { ... }
func (f File) Read(buf []byte) int { ... }

var r Reader = File{}  // Does File implement Reader?
```

**Solution**:

1. **Method Set Computation**:
   - Collect all methods of a type (including promoted methods from embedded types)
   - For interfaces, expand all embedded interfaces

2. **Implementation Check**:
   - For each method in the interface, check if the type has a method with the same name and signature
   - Signature comparison includes parameter types, return types, and variadic flag

3. **Embedding Conflict Detection**:
   - When multiple embedded interfaces have methods with the same name, signatures must match

### 2.5 Challenge 5: Named Type Semantics

**Problem**: Named types create new type identities:

```gox
type MyInt int

var a int = 42
var b MyInt = 42    // OK: constant works
var c MyInt = a     // ERROR: int ≠ MyInt
```

**Solution**:

Type equality rules:
- **Identical**: Exactly the same type (including name)
- **Underlying**: The base type after stripping all named type wrappers

Assignability rules:
1. Types are identical → OK
2. Underlying types are identical, and at least one is not a named type → OK
3. Target is an interface, source implements that interface → OK
4. Source is an untyped constant representable as target type → OK
5. Source is `nil`, target is an object type → OK

### 2.6 Challenge 6: Built-in Function Special Handling

**Problem**: Built-in functions have special type rules that cannot be expressed with regular function signatures:

```gox
len(slice)      // returns int
len(map)        // returns int
len(string)     // returns int
make([]int, 10) // returns []int
make(chan T)    // returns chan T
append(s, x)    // returns same type as s
```

**Solution**:

Write dedicated type-checking logic for each built-in function:
- `len`/`cap`: Check argument is slice/array/map/chan/string
- `make`: First argument is a type, returns that type
- `append`: First argument is a slice, returns the same slice type
- `delete`: First argument is a map

---

## 3. Architecture Design

### 3.1 Core Data Structures

**Type Representation**:

```
Type
├── Basic (int, float64, bool, string, ...)
├── Named (name + underlying)
├── Array (length + element)
├── Slice (element)
├── Map (key + value)
├── Chan (direction + element)
├── Func (params + results + variadic)
├── Struct (fields)
├── Object (fields)
├── Interface (methods)
├── Tuple (multi-value returns)
└── Invalid (error recovery)
```

**Symbol Table**:

```
Scope
├── parent: Option<Scope>
└── symbols: Map<Name, Entity>

Entity
├── Var { type, constant_value? }
├── Type { type }
├── Func { signature }
├── Builtin { kind }
└── Label
```

**Constant Values**:

```
Constant
├── Bool(bool)
├── Int(BigInt)
├── Float(BigRational)
├── Rune(char)
├── String(String)
└── Nil
```

### 3.2 Scope Hierarchy

```
Universe Scope (predefined: int, string, true, false, nil, len, make, ...)
    └── Package Scope (top-level declarations)
            └── File Scope (imports)
                    └── Function Scope (parameters)
                            └── Block Scope (local variables)
```

### 3.3 Error Recovery Strategy

When type checking encounters an error:
1. Report diagnostic information
2. Return `Type::Invalid`
3. Skip subsequent checks when encountering `Invalid` to avoid cascading errors

---

## 4. Implementation Phases

### Phase 1: Type Collection

**Goal**: Build symbol table skeleton, support forward references

**Input**: AST

**Output**:
- Package scope contains all top-level declarations
- Type names registered (but underlying not yet resolved)
- Constant values evaluated (including iota)

**Key Tasks**:
1. Traverse all top-level declarations
2. Register var/const/type/func/interface names
3. Detect duplicate declarations
4. Handle iota in const blocks

**Testing Strategy**:
- Verify symbol table contains all declarations
- Verify duplicate declarations produce errors
- Verify iota values increment correctly
- Verify forward references don't error (at this phase)

---

### Phase 2: Type Resolution

**Goal**: Resolve all type expressions, detect type errors

**Input**: Phase 1 symbol table + AST

**Output**:
- All Named types have their underlying types filled in
- Interface method sets computed
- Type cycles detected

**Key Tasks**:
1. Resolve type expressions (recursively handle nested types)
2. Detect type cycles
3. Expand interface embeddings, merge method sets
4. Validate map key comparability
5. Validate array lengths are constants

**Testing Strategy**:
- Verify type resolution is correct (named → underlying)
- Verify illegal cycles produce errors
- Verify legal cycles (through object/slice) don't error
- Verify interface method sets are correct
- Verify interface embedding conflicts produce errors
- Verify map key constraints

---

### Phase 3: Body Checking

**Goal**: Complete expression and statement type checking

**Input**: Phase 2 complete type information + AST

**Output**:
- All expressions annotated with types
- All type errors reported

**Key Tasks**:

**3a. Expression Checking**:
- Literal type inference
- Identifier resolution
- Binary/unary operator type rules
- Function call argument matching
- Index/slice expressions
- Selectors (field/method access)
- Type assertions
- Composite literals

**3b. Statement Checking**:
- Assignment statements (type compatibility)
- Short variable declarations (at least one new variable)
- Return statements (match function signature)
- if/for/switch condition types
- Range iteration variable types
- Channel operation direction checks

**3c. Built-in Functions**:
- len/cap argument types
- make arguments and return type
- append type propagation
- delete/copy/close argument checks

**Testing Strategy**:
- Verify type inference for various expressions
- Verify type mismatch errors
- Verify named type assignment rules
- Verify interface implementation checking
- Verify nil assignment rules
- Verify return statement matching
- Verify built-in function argument checking
- Verify channel direction constraints

---

## 5. Testing Strategy Overview

### 5.1 Unit Tests

Each phase has independent unit tests:

| Phase | Testing Focus |
|-------|---------------|
| Phase 1 | Symbol collection, duplicate detection, iota |
| Phase 2 | Type resolution, cycle detection, interface |
| Phase 3 | Expression types, statement checking, built-in functions |

### 5.2 Integration Tests

Use the existing `gox-tests` framework, adding type checker output verification:

```
=== SOURCE ===
package main
var x int = "hello"

=== TYPECHECK ===
ERROR: cannot use string as int (line 2)
```

### 5.3 Test Coverage Points

**Phase 1**:
- Collection of various declarations
- Duplicate declaration errors
- iota behavior across different const blocks
- Method declarations associated with types

**Phase 2**:
- Basic type and composite type resolution
- Direct cycles vs indirect cycles
- Interface embedding and conflicts
- Type constraints (map key)

**Phase 3**:
- Type rules for all operators
- The 5 assignability rules
- Constant representability
- Type requirements for control flow statements
- Special rules for built-in functions

---

## 6. Integration with Existing Code

### 6.1 New Crate

```
crates/
├── gox-types/          # Type system core
│   ├── src/
│   │   ├── lib.rs
│   │   ├── types.rs    # Type definitions
│   │   ├── scope.rs    # Symbol table
│   │   ├── constant.rs # Constant representation
│   │   ├── checker.rs  # Main entry point
│   │   ├── collect.rs  # Phase 1
│   │   ├── resolve.rs  # Phase 2
│   │   └── check.rs    # Phase 3
│   └── Cargo.toml
```

### 6.2 Dependencies

```
gox-types
├── gox-syntax (AST, Span)
└── gox-common (Symbol, Diagnostics)
```

### 6.3 API Design

```rust
pub fn check(file: &ast::File, diag: &mut DiagnosticSink) -> TypedFile;
```

The returned `TypedFile` contains:
- Original AST
- Type mapping for each expression
- Binding information for each identifier

---

## 7. Milestones

| Milestone | Content | Expected Deliverable |
|-----------|---------|---------------------|
| M1 | Phase 1 complete | Symbol table construction, iota handling |
| M2 | Phase 2 complete | Type resolution, cycle detection |
| M3 | Phase 3 basic | Basic expression and statement checking |
| M4 | Phase 3 complete | Built-in functions, full test coverage |
| M5 | Integration | Integration with gox-tests |

Each milestone should have corresponding tests passing to ensure correctness of incremental development.
