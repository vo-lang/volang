# Dynamic Access (`~>`)

The `~>` operator provides safe, concise access to dynamically typed values — perfect for JSON, `any` values, and duck-typing.

## Basic Usage

```vo
// Access a field on an any value
value, err := data~>name

// With error propagation
var name string
name = data~>name?    // access + auto-cast to string, or return error
```

## Chained Access

Chain `~>` for nested access:

```vo
var email string
email = data~>users~>[0]~>email?
```

This safely navigates nested structures. Each step returns `(any, error)`, and `?` propagates failures.

## Index Access

Use `~>[index]` for array/slice access and `~>[key]` for map access:

```vo
// Array index
first := data~>[0]?

// Map key
value := data~>[key]?
```

## Method Calls

Call methods on dynamically typed values:

```vo
result := data~>String()?
```

## Type Casting

When assigning the result of `~>` to a typed variable, Vo automatically attempts a type assertion:

```vo
var count int
count = data~>total?    // auto-casts any to int, errors if wrong type

var items []string
items = data~>tags?     // auto-casts to []string
```

## Error Handling

Without `?`, you get the raw `(any, error)` tuple:

```vo
value, err := data~>field
if err != nil {
    // handle missing field, wrong type, etc.
}
```

## When to Use

- **JSON parsing** — Navigate parsed JSON without defining struct types
- **Plugin systems** — Call methods on interface{} values
- **Configuration** — Access nested config maps
- **Prototyping** — Quick access without full type definitions
