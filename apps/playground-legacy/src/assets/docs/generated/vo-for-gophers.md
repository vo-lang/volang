<!--
Generated from lang/docs/vo-for-gophers.md
Generator: node scripts/ci/docs_sync.mjs
Source-Digest: sha256:838f1c1bbfa7b2f20a4a5c422927c1158a4e2b3d93f4082bcd26adcc090f45fa
Generated-At: 2026-01-20T20:35:05+08:00
-->
# Vo for Go Programmers

> Vo uses Go-shaped syntax, so declarations and control flow will look
> familiar. Existing Go source should be treated as a port: Vo deliberately
> has different type, module, error, pointer, dynamic-access, and concurrency
> semantics.

If you know Go, start with these four visible differences and consult the
language specification when porting code:

1. **Error Handling**: Use `?` instead of `if err != nil`. Use `errdefer` for error-only cleanup.
2. **No Generics**: Use `any` (interface{}) and type assertions.
3. **Restricted Pointers**: Only structs can be pointers (`*User`). No `*int` or `*string`.
4. **Dynamic Access**: Use `~>` operator for duck-typing (JSON, maps, untyped data).

---

## Error Handling

Think of `?` as `if err != nil { return err }` but inline. It unwraps the value or immediately returns the error.
- **`expr?`**: Propagates error, evaluates to value on success.
- **`errdefer`**: Cleanup that runs **only on error return** (like rollback). Standard `defer` runs always.
- **`fail err`**: Explicitly return an error.

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
