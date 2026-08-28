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

## Familiar surface

Packages, imports, short declarations, structs, methods, interfaces, slices,
maps, `if`, `for`, `range`, `switch`, `defer`, goroutines, channels, and
`select` intentionally look familiar:

```vo
package main

import "fmt"

type Counter struct {
    Value int
}

func (counter *Counter) Add(delta int) {
    counter.Value += delta
}

func main() {
    counter := &Counter{}
    for _, value := range []int{1, 2, 3} {
        counter.Add(value)
    }
    fmt.Println(counter.Value)
}
```

Treat this familiarity as a reading advantage. Type identity, pointer
eligibility, module selection, runtime scheduling, host effects, and release
artifacts follow Volang's specifications.

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

An enclosing function using `?`, `fail`, or `errdefer` must have the
predeclared `error` type as its final result. On propagation, ordinary defers
and eligible error defers share one reverse registration order.

```vo
func replace(path string, data []byte) error {
    temporary := createTemporary(path)?
    errdefer os.Remove(temporary)

    file := os.Create(temporary)?
    defer file.Close()
    file.Write(data)?
    activate(temporary, path)?
    return nil
}
```

Use returned errors for operational failures. Reserve panic/recover for
invariants and deliberate unwind boundaries.

---

## Pointers and value categories

Volang permits pointers only to struct base types. `*int`, pointer arithmetic,
and arbitrary address manipulation are outside the language.

```vo
type User struct {
    Name string
}

func (user *User) Rename(name string) {
    user.Name = name
}
```

Struct values copy on assignment. Struct pointers, slices, maps, interfaces,
functions, channels, ports, and islands carry reference or capability
semantics. Consult the language and runtime-memory specifications before
porting code whose correctness depends on aliasing or object lifetime.

---

## Interfaces and generics

Interfaces are satisfied structurally through method sets. Volang currently
has no generic declarations or instantiations. Prefer a small behavior
interface when callers need abstraction, and a concrete typed operation when
they need performance and static result types.

Open data can use `any`, type switches, or the dynamic protocol. Avoid replacing
every generic Go helper with `any`; collection-specific functions are often
clearer and preserve checking.

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

Dynamic operations return stable error categories for unsupported operations,
missing members, bad indexes, and type mismatches. They remain bounded protocol
dispatch rather than unrestricted reflection.

---

## Modules and imports

Volang does not consume `go.mod` or the Go module cache. A project owns TOML
intent in `vo.mod` and one exact authenticated graph in `vo.lock`:

```toml
format = 1
module = "example.com/acme/product"
version = "0.1.0"
vo = "0.1.4"

[dependencies]
"github.com/acme/records" = "^1.2.0"
```

Imports contain the selected ModuleId without a version. Run `vo mod sync` to
select, `vo mod fetch` to materialize selected bytes, and `vo mod verify` to
check the frozen graph. A compiler invocation never changes dependency intent.

Workspaces explicitly select local module origins in the lock. Nested
workspaces do not merge.

---

## Goroutines, channels, and islands

Goroutines and channels coordinate inside one island. An island owns a VM,
heap, scheduler, collector, and host-capability boundary. Channels and heap
references stay inside that boundary.

Use typed ports for cross-island communication. Native hosts may place islands
on different threads; browser hosts may place them in separate Wasm instances
or workers. Messages are encoded and validated, so serialization,
backpressure, cancellation, and failure are visible design decisions.

Go code that relies on preemptive scheduling, shared pointers across all
goroutines, or implicit host-wide blocking behavior needs an explicit Volang
concurrency design. Pass contexts, own goroutine lifetimes, and reject stale
interactive completions with a generation check.

---

## Development and release modes

Go normally compiles before execution. Volang supports several modes over the
same verified semantics:

- VM for short startup and embedding;
- Cranelift JIT for longer native development sessions;
- Native AOT for standalone executables or objects;
- Core Wasm AOT for browser and sandboxed Wasm hosts.

Use `vo check`, `vo test --mode=vm`, and `vo test --mode=jit` during a port.
Publish with `vo build` or the official UI release commands after backend
parity tests pass.

## Porting checklist

1. Create a canonical `vo.mod` identity and select dependencies explicitly.
2. Replace generic APIs with concrete functions, small interfaces, or governed
   dynamic values.
3. Replace unsupported pointer shapes and audit value-copy assumptions.
4. Convert repetitive propagation to `?` and rollback cleanup to `errdefer`.
5. Separate island-local channels from cross-island port messages.
6. Replace direct platform authority with standard-library or host capability
   contracts that work on the target backends.
7. Run formatting, checking, VM tests, JIT tests, and the intended AOT release
   target before comparing performance.
