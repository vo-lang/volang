# Vo for Go Programmers

> Most Go code runs on Vo unchanged. This doc covers only what's different.

**If you know Go, you already know 95% of Vo. Just remember these 4 changes:**

1. **Error Handling** — Use `?` instead of `if err != nil`. Use `errdefer` for error-only cleanup.
2. **No Generics** — Use `any` (interface{}) and type assertions.
3. **Restricted Pointers** — Only structs can be pointers (`*User`). No `*int` or `*string`.
4. **Dynamic Access** — Use `~>` operator for duck-typing (JSON, maps, untyped data).

---

## Error Handling

Think of `?` as `if err != nil { return err }` but inline. It unwraps the value or immediately returns the error.

- **`expr?`** — Propagates error, evaluates to value on success.
- **`errdefer`** — Cleanup that runs **only on error return** (like rollback). Standard `defer` runs always.
- **`fail err`** — Explicitly return an error.

```vo
user := allocUser()?        // Returns err if alloc fails
errdefer deleteUser(user)   // Cleanup only if later steps fail
save(user)?                 // If this fails, deleteUser runs
```

---

## Dynamic Access (`~>`)

Don't use reflection or verbose type assertions for `any` (interface{}) or JSON. **Use the `~>` operator.**

- `v, err := data~>field` returns `(any, error)`.
- `v := data~>field?` propagates error AND **automatically casts** result to your variable's type.

It works for struct fields, map keys (`data~>[key]`), and method calls (`data~>Method()`).

```vo
// data is interface{} (e.g. parsed JSON)
var name string
name = data~>users~>[0]~>name?  // Access path, auto-cast to string, or return err
```

---

## Restricted Pointers

Only struct types can be used as pointers. This simplifies the memory model:

```vo
// OK
user := &User{name: "Alice"}

// Not allowed in Vo (allowed in Go)
// x := new(int)
// p := &someInt
```

---

## Everything Else

Everything else works like Go:

- `func`, `type`, `struct`, `interface`
- `for`, `if`, `switch`, `select`
- `go`, `chan`, `defer`
- `import`, `package` (Vo uses modules)
- Slices, maps, strings, basic types
- Methods with pointer receivers
