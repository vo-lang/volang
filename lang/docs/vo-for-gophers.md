# Vo for Go Programmers

> Most Go code runs on Vo unchanged. This doc covers only what's different.

## TL;DR

| Go | Vo |
|----|-----|
| `if err != nil { return err }` | `expr?` |
| `defer cleanup()` (always) | `errdefer cleanup()` (error path only) |
| `generics` | ❌ Use `interface{}` |
| `*int`, `*string` | ❌ Only `*StructType` |
| reflection | `~>` dynamic access |

---

## Error Handling (Vo's killer feature)

### `?` Operator

Propagates error, returns zero values + error:

```vo
// Go
data, err := readFile(path)
if err != nil {
    return nil, err
}

// Vo
data := readFile(path)?
```

Works on `(T, error)` or just `error`:

```vo
func process() error {
    validate()?          // error? → propagate
    data := fetch()?     // (T, error)? → T
    save(data)?
    return nil
}
```

### `fail`

Early return with error:

```vo
if config.Version < 1 {
    fail errors.New("invalid version")
}
```

### `errdefer`

Runs **only on error path** (when `?` or `fail` triggers):

```vo
func createUser(name string) (*User, error) {
    user := allocUser()?
    errdefer deleteUser(user)   // cleanup if later steps fail
    
    validateName(name)?
    user.Name = name
    saveUser(user)?
    return user, nil
}
```

Regular `defer` still works as in Go.

---

## No Generics

Use `interface{}` (or `any`):

```vo
func Map(slice []any, f func(any) any) []any {
    result := make([]any, len(slice))
    for i, v := range slice {
        result[i] = f(v)
    }
    return result
}
```

---

## Pointers: Struct Only

```vo
// ✅ OK
var p *User = &user
var q *Point = new(Point)

// ❌ Compile error
var x *int        // no pointer to primitives
var s *[]string   // no pointer to slices
```

**Why?** Simplifies memory model. Use struct wrappers if needed:

```vo
type IntBox struct { val int }
var p *IntBox = &IntBox{val: 42}
```

---

## Dynamic Access (`~>`)

Duck-typing for `any`/`interface{}` values:

```vo
func process(data any) (string, error) {
    // Field access - returns (any, error)
    name, err := data~>user~>name
    
    // With ? - two-step unwrap:
    //   1. propagate error if any
    //   2. auto type-assert to LHS type
    var userName string
    userName = data~>user~>name?   // any→string, fails if wrong type
    
    // Index
    first := data~>items~>[0]?
    
    // Method call
    result := data~>Process(arg)?
    
    return userName, nil
}
```

All `~>` ops return `(any, error)`. With `?`:
- Error propagates via `fail`
- Result auto-converts to LHS type (type mismatch → error)

### Dynamic Set

```vo
data~>count = 42           // fail-on-error (implicit ?)
data~>users~>[0] = newUser

// Explicit error handling: use dyn package API
err := dyn.SetAttr(data, "count", 42)
if err != nil {
    // handle error
}
```

### dyn Package API

```vo
import "dyn"

// Get operations - return (any, error)
val, err := dyn.GetAttr(base, "field")
val, err := dyn.GetIndex(base, key)

// Set operations - return error
err := dyn.SetAttr(base, "field", value)
err := dyn.SetIndex(base, key, value)

// Error values for errors.Is()
dyn.ErrNilBase      // base is nil
dyn.ErrBadField     // field not found
dyn.ErrBadIndex     // invalid index type
dyn.ErrOutOfBounds  // index out of bounds
dyn.ErrTypeMismatch // type mismatch
```

---

## What's Identical to Go

- All basic types (`int`, `string`, `bool`, `float64`, etc.)
- Slices, maps, channels, structs, interfaces
- Methods and receivers (`T` and `*T`)
- `defer`, `go`, `select`, `switch`, `for range`
- `panic`/`recover`
- `make`, `append`, `len`, `cap`, `copy`, `delete`, `close`
- Implicit interface implementation
- Package system, imports
- Goroutines and channels
- Value vs reference semantics

---

## Quick Examples

### Error handling chain

```vo
func loadConfig(path string) (Config, error) {
    data := readFile(path)?
    errdefer log("failed to load config")
    
    config := parseJSON(data)?
    validateConfig(config)?
    return config, nil
}
```

### Dynamic JSON-like access

```vo
func getUsername(resp any) (string, error) {
    return resp~>data~>user~>name?.(string), nil
}
```

### Struct with methods

```vo
type Counter struct {
    value int
}

func (c *Counter) Inc() {
    c.value += 1
}

func (c Counter) Get() int {
    return c.value
}
```

---

## Don'ts

| Don't | Do |
|-------|-----|
| `*int` | `struct { val int }` then `*MyStruct` |
| `func[T any](...)` | `func(... any)` |
| ignore errors | use `?` or explicit check |

---

## Running

```bash
# VM (interpreter)
vo run program.vo

# JIT (faster)
vo run --mode=jit program.vo
```
