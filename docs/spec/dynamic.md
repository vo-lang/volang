# Dynamic Access Specification

## Overview

Vo supports opt-in dynamic (duck-typing) operations on values whose structure is unknown at compile time.

Dynamic access is enabled by the `~>` operator. The `~>` operator does not introduce a new runtime representation: the base value is boxed to `any` if not already an interface.

Dynamic operations return `(any, error)` and are intended to compose with Vo's existing error handling (`?`, `fail`, `errdefer`). There is no error-carrying payload.

### Supported Base Types

The `~>` operator can be used on:

| Type | Allowed | Reason |
|------|---------|--------|
| `any` / `interface` | ✅ | Dynamic dispatch |
| `(any, error)` tuple | ✅ | Chained access with short-circuit |
| Named types | ✅ | May have protocol methods |
| Pointers | ✅ | Base type may have methods |
| Structs | ✅ | Reflection field access |
| Maps | ✅ | Reflection key access |
| Slices / Arrays | ✅ | Reflection index access |
| `string` | ✅ | Index access to characters |
| `int`, `bool`, `float` | ❌ | No methods, no fields, no indexing |

## Syntax

### Dynamic Access Operator (`~>`)

```go
// Field access
a~>field           // → (any, error)
a~>field?          // → any (propagate error)

// Chaining
a~>b~>c             // → (any, error) (implicit short-circuiting)
a~>b~>c?            // → any

// Field assignment
a~>field = value   // statement form: panic-on-error
a~>b~>c = value    // chained assignment target

// Indexing
a~>[key]           // → (any, error)
a~>[key]?          // → any (propagate error)

// Index assignment
a~>[key] = value   // statement form: panic-on-error

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

**Note**: The left operand of `~>` may have static type `any/interface` or `(any, error)`. If the left operand is `(any, error)`, `~>` implicitly short-circuits: if `error != nil`, the result is `(nil, error)`; otherwise the operation continues on the `any` value.

### Dynamic Operation Whitelist

Dynamic access does not participate in normal static member lookup or overload resolution. Only a small whitelist of operations is supported when using `~>`:

- Field access: `a~>field` (returns `(any, error)`)
- Field assignment: `a~>field = x` (statement; panic-on-error)
- Indexing: `a~>[key]` (returns `(any, error)`)
- Index assignment: `a~>[key] = x` (statement; panic-on-error)
- Call: `a~>(args...)` (returns `(any, error)`)
- Method call (syntax sugar): `a~>method(args...)` (returns `(any, error)`)
- Error propagation: apply postfix `?` to unwrap and propagate the `error`

All other dynamic operations are compile errors.

## Protocol Methods

Vo defines five **language-level protocol methods** that customize dynamic access behavior. These are reserved method names with fixed signatures.

### Protocol Definition

| Method | Signature | Triggered By |
|--------|-----------|--------------|
| `DynAttr` | `(name string) (any, error)` | `a~>field` |
| `DynSetAttr` | `(name string, value any) error` | `a~>field = v` |
| `DynIndex` | `(key any) (any, error)` | `a~>[key]` |
| `DynSetIndex` | `(key any, value any) error` | `a~>[key] = v` |
| `DynCall` | `(args ...any) (any, error)` | `a~>(args)` |

### Semantics

1. **Duck Typing**: Any type with a matching method name and signature participates in the protocol. No interface implementation is required.

2. **Protocol-First**: When `~>` is used, the runtime first checks for the corresponding protocol method. If found, it is called. If not found, the runtime falls back to reflection-based access (struct fields, map keys, etc.).

3. **Inheritance via Embedding**: Protocol methods can be inherited through struct embedding.

4. **Signature Enforcement**: If a type defines a method with a protocol name (e.g., `DynAttr`) but the signature does not match, it is a **compile-time error**.

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

func main() {
    c := &Config{data: map[string]any{}}
    c~>name = "hello"     // calls DynSetAttr("name", "hello")
    v := c~>name?         // calls DynAttr("name") → "hello"
}
```

### Example: Embedding dyn.MapObject

```go
import "dyn"

type Config struct {
    dyn.MapObject  // embed: inherits DynAttr, DynSetAttr, DynIndex, DynSetIndex
}

func main() {
    c := Config{dyn.MapObject{}}
    c~>name = "hello"     // calls inherited DynSetAttr
    v := c~>name?         // calls inherited DynAttr
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

When `a~>field` is evaluated:

1. **Protocol method**: If type has `DynAttr(string) (any, error)` → call it
2. **Map with string key**: If type is `map[string]V` → access key
3. **Struct field**: If type is struct/pointer-to-struct → access field by name
4. **Method as closure**: If field not found but method exists → return method as closure
5. **Error**: Return `ErrBadField`

## Error Mechanism

### Error Generation

| Operation | Failure Condition | Error Type |
|-----------|-------------------|------------|
| `a~>field` | field doesn't exist | AttributeError |
| `a~>field` | value is not struct/map | TypeError |
| `a~>field = x` | field doesn't exist or is not assignable | AttributeError |
| `a~>field = x` | value is not mutable (not map and not pointer-to-struct) | TypeError |
| `a~>[key]` | value is not indexable | TypeError |
| `a~>[key] = x` | value is not mutable indexable (not map/slice/array) | TypeError |
| `a~>method(args...)` | method doesn't exist | AttributeError |
| `a~>method(args...)` | signature mismatch (return count, param count, param type) | SignatureError |
| `a~>(args...)` | value is not callable | CallError |
| `a~>(args...)` | signature mismatch | SignatureError |

## Runtime Representation

### Representation

Dynamic access operates on ordinary `any`/`interface` values.

There is no error-carrying payload. Errors are reported via the explicit `error` return value of dynamic operations.

## LHS Type-Driven Conversion

When the LHS variable has a **known static type**, dynamic operations automatically convert the result to that type. This rule applies uniformly to all dynamic access scenarios.

### Rule

| LHS Declaration | Result Type | Conversion |
|-----------------|-------------|------------|
| `v, err := ...` | `any` | None (default) |
| `var v int; v, err = ...` | `int` | Auto type assertion |
| `var v MyStruct; v, err = ...` | `MyStruct` | Auto type assertion |

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

If the runtime value cannot be converted to the expected LHS type, a `TypeError` is returned.

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
a~>field = x       → dyn_set_attr(a, "field", x)               // statement (panic-on-error)
a~>[key]           → dyn_get_index(a, any(key))                // (any, error)
a~>[key]?          → dyn_get_index(a, any(key))?               // any
a~>[key] = x       → dyn_set_index(a, any(key), x)              // statement (panic-on-error)
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

### Runtime Builtins

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

fn dyn_call(callee: any, args: any, expected_sig: rttid) -> (any, error) {
    // Runtime signature check before call:
    // 1. Return value count must match LHS count
    // 2. Parameter count must match args count
    // 3. Parameter types must be compatible (rttid comparison)
    //
    // If signature mismatch, return SignatureError without calling.
    // If callee returns multiple values, the result is []any.
    // If callee returns a single value, the result is that value.
    // If callee returns nothing, the result is nil.
    if !check_signature(callee, expected_sig) {
        return (any(nil), SignatureError)
    }
    match call(callee, args) {
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

The runtime helpers used for lowering are also exposed as user-facing APIs (stdlib or compiler built-ins). They are intended for advanced usage, library authors, and cases where the caller needs to explicitly handle errors.

- `dyn_get_attr(base any, name string) -> (any, error)`
- `dyn_set_attr(base any, name string, value any) -> error`
- `dyn_get_index(base any, key any) -> (any, error)`
- `dyn_set_index(base any, key any, value any) -> error`
- `dyn_call(callee any, args []any, expected_sig rttid) -> (any, error)`
- `dyn_call_method(base any, method string, args []any, expected_sig rttid) -> (any, error)`

## Examples

### Basic Usage

```go
func processJSON(data any) (string, error) {
    a := data
     
    // Chained operations, each step is checked
    userName := (a~>response~>data~>user~>name?).(string)
    userAge := (a~>response~>data~>user~>age?).(int)
    greeting := (a~>config~>greeting?).(string)

    // Dynamic set (single-step). Statement form is panic-on-error.
    a~>last_user = userName
    a~>["last_age"] = userAge
    
    // Complex expression
    message := greeting + " " + userName
    
    return message, nil
}
```

### Dynamic set with explicit error handling

```go
func updateCount(data any) error {
    a := data

    // Dynamic set is statement-only.
    // Use the helper APIs when explicit error handling is needed.
    err := dyn_set_attr(a, "count", 1)
    err?

    err = dyn_set_index(a, "count", 2)
    err?

    return nil
}
```

### Chained assignment

```go
func chainedSet(a any, v any) error {
    // Chained assignment is supported.
    // Its meaning is: evaluate `a~>a` (short-circuit on error), then set `b` on the result.
    a~>a~>b = v
    return nil
}
```

**Warning**: Chained assignment does not guarantee "path write-back" if intermediate results are value copies. For example, if `a~>a` returns a struct by value, setting `b` on it will not modify the original. To ensure mutation, the intermediate value must be a pointer or a reference type (map, slice).

### Safe Access with explicit error

```go
func safeGet(data any, field string) (any, error) {
    v, err := dyn_get_attr(data, field)
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
