<!--
Generated from lang/docs/spec/dynamic.md
Generator: node scripts/ci/docs_sync.mjs
Source-Digest: sha256:b772f19f8b6df75774bfdc2c7d4e713fbcf2091d6b4dd2b510af2892f92abe37
Generated-At: 2026-01-20T20:35:05+08:00
-->
# Dynamic Access Specification

## Overview

Vo supports opt-in dynamic operations on values whose structure is unknown at compile time.

Dynamic access is enabled by the `~>` operator. The operator does not introduce
a new runtime representation. An interface base already carries its dynamic
type and value; a concrete protocol base dispatches statically. Reflection on
another concrete value requires an explicit conversion to `any`.

Dynamic operations return `(any, error)` and are intended to compose with Vo's existing error handling (`?`, `fail`, `errdefer`). There is no error-carrying payload.

### Supported Base Types

The `~>` operator can be used on:

| Type | Allowed | Reason |
|------|---------|--------|
| `any` / `interface` | ✅ | Dynamic dispatch |
| `(any, error)` tuple | ✅ | Chained access with short-circuit |
| Named concrete types | ✅ with protocol | Must implement the exact reserved protocol for the operation |
| Pointers | ✅ with protocol | Must implement the exact reserved protocol for the operation |
| Structs / maps / slices / arrays / strings | ✅ after `any(v)` | Reflection is explicit through an interface value |
| `int`, `bool`, `float` | ❌ | No methods, no fields, no indexing |

Concrete reflection requires an explicit conversion such as `any(value)~>Field`.
Reflection exposes exported struct fields and fields carrying a `dyn:"name"` tag.
An unexported field without that tag remains inaccessible; a protocol method can
provide controlled access when needed.

## Syntax

### Dynamic Access Operator (`~>`)

```go
// Field access
a~>field           // → (any, error)
a~>field?          // → any (propagate error)

// Chaining
a~>b~>c             // → (any, error) (implicit short-circuiting)
a~>b~>c?            // → any

// Indexing
a~>[key]           // → (any, error)
a~>[key]?          // → any (propagate error)

// Call
a~>(args...)       // → (any, error)
a~>(args...)?      // → any (propagate error)

// Method call (syntax sugar)
a~>method(args...) // → (any, error)
a~>method(args...)?// → any (propagate error)

// Type assertion (same as interface)
v := a.(T)         // → T (panic on failure)
v, ok := a.(T)     // → (T, bool) (ok=false on failure, no panic)
```

**Note**: Optional-chaining style operators like `?.` and `?[]` are not part of Vo syntax. Use postfix `?` on the result of each dynamic operation.

**Note**: The left operand may be `any/interface`, `(any, error)`, or a
concrete value implementing the exact reserved protocol for that operation.
Other concrete values require an explicit `any(value)` conversion for
reflection. An `(any, error)` base short-circuits when its error is non-nil.

### Dynamic Operation Whitelist

Dynamic access does not participate in normal static member lookup or overload resolution. Only a small whitelist of operations is supported when using `~>`:

- Field access: `a~>field` (returns `(any, error)`)
- Indexing: `a~>[key]` (returns `(any, error)`)
- Call: `a~>(args...)` (returns `(any, error)`)
- Method call (syntax sugar): `a~>method(args...)` (returns `(any, error)`)
- Error propagation: apply postfix `?` to unwrap and propagate the `error`

Dynamic writes use `dyn.SetAttr` and `dyn.SetIndex`. A dynamic assignment target
is a compile-time error, which keeps every write error visible in source.

All other dynamic operations are compile errors.

## Protocol Methods

Vo defines five **language-level protocol methods** that customize dynamic access behavior. These are reserved method names with fixed signatures.

### Protocol Definition

| Method | Signature | Triggered By |
|--------|-----------|--------------|
| `DynAttr` | `(name string) (any, error)` | `a~>field` |
| `DynSetAttr` | `(name string, value any) error` | `dyn.SetAttr(a, name, v)` or direct call |
| `DynIndex` | `(key any) (any, error)` | `a~>[key]` |
| `DynSetIndex` | `(key any, value any) error` | `dyn.SetIndex(a, key, v)` or direct call |
| `DynCall` | `(args ...any) (any, error)` | `a~>(args)` |

### Semantics

1. **Fixed protocol typing**: A concrete `~>` base must implement the exact protocol for its operation. Interface bases are checked at runtime.

2. **Concrete dispatch**: A concrete protocol base dispatches only to the
   statically validated reserved method. Reflection requires an explicit
   conversion to `any`.

3. **Interface dispatch**: For an interface base, the runtime checks the
   dynamic concrete value for the corresponding protocol. It falls back to
   reflection only when that protocol is absent.

4. **Inheritance via Embedding**: Protocol methods can be inherited through struct embedding.

5. **Signature Enforcement**: If a type defines a method with a protocol name (e.g., `DynAttr`) but the signature does not match, it is a **compile-time error**.

### Example: Custom Dynamic Object

```go
type Config struct {
    data map[string]any
}

func (c *Config) DynAttr(name string) (any, error) {
    v, ok := c.data[name]
    if !ok {
        return nil, dyn.ErrBadField
    }
    return v, nil
}

func (c *Config) DynSetAttr(name string, value any) error {
    c.data[name] = value
    return nil
}

func main() error {
    c := &Config{data: map[string]any{}}
    dyn.SetAttr(c, "name", "hello")? // calls DynSetAttr("name", "hello")
    v := c~>name?         // calls DynAttr("name") → "hello"
    return nil
}
```

### Example: Embedding dyn.MapObject

```go
import "dyn"

type Config struct {
    dyn.MapObject  // embed: inherits DynAttr, DynSetAttr, DynIndex, DynSetIndex
}

func main() error {
    c := Config{dyn.MapObject{}}
    dyn.SetAttr(c, "name", "hello")? // calls inherited DynSetAttr
    v := c~>name?         // calls inherited DynAttr
    return nil
}
```

### Signature Error Example

```go
type Bad struct{}

// Compile error: DynAttr has wrong signature
// Expected: (string) (any, error)
// Got: (int) string
func (b Bad) DynAttr(x int) string { return "" }
```

### Dispatch Priority

For a concrete base, `a~>field` calls its validated `DynAttr` method.

For an interface base, dispatch proceeds in this order:

1. **Protocol method**: call `DynAttr(string) (any, error)` when implemented
2. **Map with string key**: access a `map[string]V` key
3. **Visible struct field**: access an exported field or its `dyn` tag name
4. **Method as closure**: return an exported method when no field matches
5. **Error**: return `ErrBadField`

## Error Mechanism

Package `dyn` exports sentinel errors. Dynamic operations may add operation
context, while `errors.Is` identifies the stable category. A protocol method's
non-nil error is propagated unchanged.

### Error Generation

| Operation | Failure Condition | Stable `errors.Is` Category |
|-----------|-------------------|-----------------------------|
| Any operation | nil base | `dyn.ErrNilBase` |
| `a~>field` | field or method does not exist | `dyn.ErrBadField` |
| `a~>field` | value does not support field access | `dyn.ErrTypeMismatch` |
| `dyn.SetAttr(a, name, x)` | field does not exist | `dyn.ErrBadField` |
| `dyn.SetAttr(a, name, x)` | base is not mutable or value is not assignable | `dyn.ErrTypeMismatch` |
| `a~>[key]` / `dyn.SetIndex` | key has an invalid type | `dyn.ErrBadIndex` |
| `a~>[key]` | map key is absent | `dyn.ErrBadField` |
| `a~>[key]` / `dyn.SetIndex` | index is out of bounds | `dyn.ErrOutOfBounds` |
| `a~>[key]` / `dyn.SetIndex` | value does not support indexing or mutation | `dyn.ErrTypeMismatch` |
| `a~>method(args...)` | method does not exist | `dyn.ErrBadField` |
| Dynamic call | parameter or result signature mismatch | `dyn.ErrSigMismatch` |
| `a~>(args...)` | value is not callable | `dyn.ErrBadCall` |

## Runtime Representation

### Representation

Dynamic access operates on ordinary `any`/`interface` values.

There is no error-carrying payload. Errors are reported via the explicit `error` return value of dynamic operations.

## LHS Type-Driven Assignment

When an LHS variable has a known static type, that type becomes the expected
dynamic result type. The runtime checks ordinary assignment compatibility; it
does not perform numeric coercion or other unrelated explicit conversions.

### Rule

| LHS Declaration | Result Type | Check |
|-----------------|-------------|------------|
| `v, err := ...` | `any` | None (default) |
| `var v int; v, err = ...` | `int` | Runtime assignment/type assertion |
| `var v MyStruct; v, err = ...` | `MyStruct` | Runtime assignment/type assertion |

### Applies To

- **Field access**: `v, err := obj~>field`
- **Index access**: `v, err := obj~>[key]`
- **Method call**: `v, err := obj~>method(args...)`
- **Direct call**: `v, err := obj~>(args...)`

### Examples

```go
var obj interface{} = map[string]int{"count": 42}

// LHS is any (short var decl)
v, err := obj~>["count"]          // v is any

// LHS has known type
var count int
count, err = obj~>["count"]       // count is int (auto conversion)

// Multi-return with known types
type Data struct { Name string; Age int }
var obj2 interface{} = &Data{Name: "Alice", Age: 30}

var name string
var age int
name, err = obj2~>Name            // auto conversion to string
age, err = obj2~>Age              // auto conversion to int
```

### Error on Type Mismatch

If the runtime value is not assignable to the expected LHS type, the operation
returns an error matching `dyn.ErrTypeMismatch`.

```go
var count int
count, err = obj~>["name"]        // err is TypeError (string -> int fails)
```

## Runtime Signature Checking

Dynamic calls perform **runtime signature verification** before invoking the target closure. The expected signature is derived from the LHS types at compile time.

### Checks Performed

1. **Return value count**: The closure's return count must match the number of LHS variables (minus the error variable).
2. **Parameter count**: The closure's parameter count must match the number of arguments provided.
3. **Parameter types**: Each argument's runtime type (rttid) must be compatible with the closure's expected parameter type.
4. **Return types**: If LHS has known types, the closure's return types must be compatible.

### Examples

```go
type Calc struct { val int }
func (c *Calc) Add(x int) int { return c.val + x }
func (c *Calc) Pair() (int, string) { return c.val, "ok" }

var obj interface{} = &Calc{val: 42}

// OK: signature matches, LHS is any
r1, err := obj~>Add(10)           // r1 is any(52)

// OK: signature matches, LHS has known type
var result int
result, err = obj~>Add(10)        // result is 52 (int)

// Error: return count mismatch (expects 1, got 2)
r1, err := obj~>Pair()            // err is SignatureError

// OK: return count matches
r1, r2, err := obj~>Pair()        // r1 is any(42), r2 is any("ok")

// OK: with known LHS types
var n int
var s string
n, s, err = obj~>Pair()           // n is 42, s is "ok"

// Error: parameter count mismatch
r1, err := obj~>Add()             // err is SignatureError (expects 1 arg)
r1, err := obj~>Add(1, 2)         // err is SignatureError (expects 1 arg)

// Error: parameter type mismatch
r1, err := obj~>Add("wrong")      // err is SignatureError (expects int)
```

### Implementation

The expected signature is encoded as an `rttid` (runtime type ID) at compile time, including LHS types when known. At runtime, the `dyn_call_check` function compares the closure's signature rttid against the expected signature rttid. If they don't match, a `SignatureError` is returned without invoking the closure.

## Implementation Details

### Type Checker Changes

```rust
// Dynamic access operator
// If the base has static type any/interface, dynamic access returns (any, error).
if base_type.is_any_or_interface() {
    return Type::Tuple(Type::Any, Type::Error);
}

// If the base has static type (any, error), dynamic access also returns (any, error)
// and short-circuits on the error component.
if base_type.is_tuple_any_error() {
    return Type::Tuple(Type::Any, Type::Error);
}
```

### Codegen Changes

```rust
// Dynamic access operations lower to runtime helper calls that return (value, error).
// Postfix `?` is regular Vo error-propagation sugar.
a~>field           → dyn_get_attr(a, "field")                 // (any, error)
a~>field?          → dyn_get_attr(a, "field")?                // any
a~>[key]           → dyn_get_index(a, any(key))                // (any, error)
a~>[key]?          → dyn_get_index(a, any(key))?               // any
a~>(args...)       → dyn_call(a, any(args...))                 // (any, error)
a~>(args...)?      → dyn_call(a, any(args...))?                // any

// Method call sugar
a~>method(args...) → dyn_call_method(a, "method", any(args...))  // (any, error)
```

### Chained Access Codegen (Short-circuiting)

When the left operand of `~>` has type `(any, error)`, the generated code must short-circuit:

```rust
// a~>b~>c where a has type `any`
// Desugars to:
{
    let (v1, e1) = dyn_get_attr(a, "b")
    if e1 != nil {
        (nil, e1)  // short-circuit
    } else {
        dyn_get_attr(v1, "c")
    }
}

// Equivalently, using a helper:
dyn_chain(dyn_get_attr(a, "b"), |v| dyn_get_attr(v, "c"))

// where dyn_chain is:
fn dyn_chain(base: (any, error), op: fn(any) -> (any, error)) -> (any, error) {
    let (v, e) = base
    if e != nil {
        return (nil, e)
    }
    op(v)
}
```

### Difference: `a~>b~>c` vs `a~>b?~>c`

| Expression | On `a~>b` failure | Result type |
|------------|-------------------|-------------|
| `a~>b~>c` | Short-circuit, return `(nil, err)` | `(any, error)` |
| `a~>b?~>c` | `?` triggers `fail`, function exits | `(any, error)` |

The first form collects errors; the second form fails early.

### nil Interface

If the base value is a nil interface, dynamic access returns `TypeError("cannot access on nil")`.

```go
var a any = nil
v, err := a~>field   // err is TypeError
```

### Compiler Lowering Sketch

The following pseudocode names describe internal lowering operations. They are
not source-level declarations and programs cannot import or call them directly.

```rust
fn dyn_get_attr(d: any, name: &str) -> (any, error) {
    match get_field(d, name) {
        Ok(v) => (v, nil),
        Err(e) => (any(nil), e),
    }
}

fn dyn_get_index(d: any, key: any) -> (any, error) {
    match get_index(d, key) {
        Ok(v) => (v, nil),
        Err(e) => (any(nil), e),
    }
}

// dyn_assert not needed - uses standard interface type assertion

fn dyn_set_attr(d: any, name: &str, val: any) -> error {
    set_field(d, name, val)
}

fn dyn_set_index(d: any, key: any, val: any) -> error {
    set_index(d, key, val)
}

fn dyn_call_method(d: any, method: &str, args: any, expected_sig: rttid) -> (any, error) {
    match get_method(d, method) {
        Ok(m) => dyn_call(m, args, expected_sig),
        Err(e) => (any(nil), e),
    }
}
```

### User-facing Helper APIs

Package `dyn` exposes the complete explicit helper surface. These functions are
useful when callers need to handle a dynamic-operation error directly:

- `dyn.GetAttr(base any, name string) (any, error)`
- `dyn.GetIndex(base any, key any) (any, error)`
- `dyn.SetAttr(base any, name string, value any) error`
- `dyn.SetIndex(base any, key any, value any) error`

Dynamic calls remain language operations written with `~>`; the lowering ABI,
signature metadata, and runtime entry points are compiler implementation details.

## Examples

### Basic Usage

```go
func processJSON(data any) (string, error) {
    a := data
     
    // Chained operations, each step is checked
    userName := (a~>response~>data~>user~>name?).(string)
    userAge := (a~>response~>data~>user~>age?).(int)
    greeting := (a~>config~>greeting?).(string)

    // Dynamic writes expose their errors.
    dyn.SetAttr(a, "last_user", userName)?
    dyn.SetIndex(a, "last_age", userAge)?
    
    // Complex expression
    message := greeting + " " + userName
    
    return message, nil
}
```

### Dynamic set with explicit error handling

```go
func updateCount(data any) error {
    a := data

    err := dyn.SetAttr(a, "count", 1)
    err?

    err = dyn.SetIndex(a, "count", 2)
    err?

    return nil
}
```

### Writing through a dynamically obtained value

```go
func setNested(a any, v any) error {
    child := a~>a?
    dyn.SetAttr(child, "b", v)?
    return nil
}
```

The intermediate value must be a pointer or reference type when the mutation
needs to affect its original owner.

### Safe Access with explicit error

```go
func safeGet(data any, field string) (any, error) {
	v, err := dyn.GetAttr(data, field)
    if err != nil {
        return nil, err
    }
    return v, nil
}
```

## Design Summary

| Aspect | Design |
|--------|--------|
| Entry Point | `~>` operator on supported types (see Supported Base Types) |
| Protocol Methods | `DynAttr`, `DynSetAttr`, `DynIndex`, `DynSetIndex`, `DynCall` |
| Dispatch | Protocol-first, then reflection fallback |
| Result Type | Dynamic operations return `(any, error)` |
| Error Handling | No error-carrying payload; check per-step via `?` |
| Runtime Layout | Base boxed to `any`, uses ordinary interface values |
| Semantics | Whitelist + compile-time desugaring to helper calls |
| Signature Check | Protocol method names require matching signatures (compile error otherwise) |
