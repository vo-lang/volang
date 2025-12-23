# Error Handling Specification

Version: 1.0  
Status: Draft

This document specifies the error-handling model for Vo, designed to be fully compatible with Go programmers while providing concise syntax sugar for common patterns.

---

## 1. Goals

1. **Go compatibility**: Preserve `(T, error)` return convention.
2. **Concise propagation**: Reduce boilerplate for error checking.
3. **Explicit failures**: No hidden control flow or exceptions.
4. **Gradual adoption**: Old and new styles can coexist.

**Non-goals**:
- Stack-unwinding exceptions.
- New error-union types like `T!`.

---

## 2. Core Features

The language adds three features to Go's error handling:

| Feature | Purpose |
|---------|---------|
| `fail` | Simplified error return |
| `?` | Unwrap-or-propagate operator |
| `errdefer` | Cleanup on failure only |

---

## 3. Return Types

Error-returning functions use Go's standard convention:

```go
func readFile(path string) ([]byte, error)
func parse(data []byte) (*Config, error)
func save() error
```

No new types are introduced. The last return value being `error` marks a function as fallible.

---

## 4. The `fail` Statement

### 4.1 Syntax

```go
fail expr
```

where `expr` has type `error`.

### 4.2 Semantics

`fail e` is equivalent to `return <zero-values>, e`.

### 4.3 Constraints

- `fail` is only permitted in functions whose last return type is `error`.
- `fail nil` is a compile-time error.

### 4.4 Examples

```go
// Traditional Go
func parseInt(s string) (int, error) {
    if s == "" {
        return 0, ErrEmpty
    }
    return 123, nil
}

// With fail
func parseInt(s string) (int, error) {
    if s == "" {
        fail ErrEmpty
    }
    return 123, nil
}
```

For multiple return values:

```go
func divide(a, b int) (int, int, error) {
    if b == 0 {
        fail ErrDivByZero  // equivalent to: return 0, 0, ErrDivByZero
    }
    return a / b, a % b, nil
}
```

---

## 5. The `?` Operator

### 5.1 Syntax

```go
expr?
```

### 5.2 Typing Rules

If `expr` is a function call returning `(T1, T2, ..., Tn, error)`:
- `expr?` has type `(T1, T2, ..., Tn)` (or just `T1` if n=1).
- If n=0 (function returns only `error`), `expr?` has no value (statement only).

### 5.3 Constraints

- `?` is only permitted in functions whose last return type is `error`.
- `?` can only be applied to expressions whose last value is `error`.

### 5.4 Semantics

```go
x := f()?
```

is equivalent to:

```go
x, err := f()
if err != nil {
    fail err
}
```

For multiple values:

```go
a, b := g()?
```

is equivalent to:

```go
a, b, err := g()
if err != nil {
    fail err
}
```

For error-only returns:

```go
h()?
```

is equivalent to:

```go
if err := h(); err != nil {
    fail err
}
```

### 5.5 Examples

```go
// Traditional Go
func load(path string) (*Config, error) {
    data, err := readFile(path)
    if err != nil {
        return nil, err
    }
    cfg, err := parse(data)
    if err != nil {
        return nil, err
    }
    return cfg, nil
}

// With ?
func load(path string) (*Config, error) {
    data := readFile(path)?
    cfg := parse(data)?
    return cfg, nil
}
```

---

## 6. The `errdefer` Statement

### 6.1 Syntax

```go
errdefer statement
```

### 6.2 Semantics

`errdefer` schedules a statement to execute only if the function returns with a non-nil error.

### 6.3 Constraints

- `errdefer` is only permitted in functions whose last return type is `error`.
- `errdefer` statements execute in LIFO order (like `defer`).

### 6.4 Examples

```go
func doTransaction() error {
    tx := beginTx()?
    errdefer tx.Rollback()  // only runs if function fails
    
    updateA(tx)?
    updateB(tx)?
    tx.Commit()?
    return nil
}
```

Compare with traditional Go:

```go
func doTransaction() error {
    tx, err := beginTx()
    if err != nil {
        return err
    }
    
    if err := updateA(tx); err != nil {
        tx.Rollback()
        return err
    }
    if err := updateB(tx); err != nil {
        tx.Rollback()
        return err
    }
    if err := tx.Commit(); err != nil {
        tx.Rollback()
        return err
    }
    return nil
}
```

### 6.5 Desugaring

`errdefer S` is lowered to:

```go
defer func() {
    if __returnedError != nil {
        S
    }
}()
```

The exact mechanism is implementation-defined.

---

## 7. Mandatory Error Handling (Recommended)

### 7.1 Rule

A function call returning `(..., error)` should not have its error silently ignored.

Valid handling:
- Propagate with `?`
- Check with `if err != nil`
- Explicitly ignore with `_ = f()` or `_, _ = g()`

### 7.2 Enforcement

This rule may be enforced as:
- **Compiler warning** (default)
- **Compiler error** (strict mode)
- **Linter rule** (external tool)

---

## 8. Error Wrapping

### 8.1 Standard Pattern

Use `fmt.Errorf` with `%w` for wrapping:

```go
func load(path string) (*Config, error) {
    data, err := readFile(path)
    if err != nil {
        return nil, fmt.Errorf("read config: %w", err)
    }
    // ...
}
```

### 8.2 With `?` (Future Extension)

A future version may support inline wrapping:

```go
data := readFile(path)? wrap "read config"
```

This is not part of v1.0.

---

## 9. Interoperability

### 9.1 Mixing Styles

Old and new styles can coexist in the same codebase:

```go
func example() error {
    // Traditional
    x, err := oldFunc()
    if err != nil {
        return fmt.Errorf("old failed: %w", err)
    }
    
    // New style
    y := newFunc()?
    
    return nil
}
```

### 9.2 Calling Go Code

All Go functions returning `(..., error)` work seamlessly with `?`.

---

## 10. Summary

| Feature | Syntax | Equivalent Go |
|---------|--------|---------------|
| Error return | `fail e` | `return ..., e` |
| Propagate | `x := f()?` | `x, err := f(); if err != nil { return ..., err }` |
| Cleanup on fail | `errdefer S` | Manual cleanup in each error branch |

---

## 11. Examples

### 11.1 File Processing

```go
func processFile(path string) error {
    f := os.Open(path)?
    defer f.Close()
    
    data := io.ReadAll(f)?
    result := transform(data)?
    
    out := os.Create(path + ".out")?
    errdefer out.Close()
    errdefer os.Remove(path + ".out")
    
    out.Write(result)?
    return nil
}
```

### 11.2 HTTP Handler

```go
func handleRequest(w http.ResponseWriter, r *http.Request) error {
    body := io.ReadAll(r.Body)?
    req := parseRequest(body)?
    
    result := processRequest(req)?
    
    w.Write(result)?
    return nil
}
```

### 11.3 Database Transaction

```go
func transferFunds(from, to int, amount int) error {
    tx := db.Begin()?
    errdefer tx.Rollback()
    
    debit(tx, from, amount)?
    credit(tx, to, amount)?
    logTransfer(tx, from, to, amount)?
    
    tx.Commit()?
    return nil
}
```
