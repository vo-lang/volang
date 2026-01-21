# Vo Programming Language

> **Go-like Syntax. Rust Power. Vibe Ready.**

The scripting language for the Rust world. Statically typed, low ceremony, and built for the AI coding era. **Most Go programs run with minimal changes.**

🎮 **[Try it in the Playground](https://oxfeeefeee.github.io/volang/)**

## Why Vo?

Go's appeal is **low ceremony**: it writes like a scripting language but ships like a compiled one. Vo doubles down on this strength—adding flexible execution modes (VM/JIT/AOT) and error handling sugar, while keeping the language simple. Target niche: where you might reach for Go, Python, or Lua.

### If you know Go, you already know 95% of Vo

Just remember these 4 differences:

1. **Error Handling**: Use `?` instead of `if err != nil`. Use `errdefer` for error-only cleanup.
2. **No Generics**: Use `any` (interface{}) and type assertions.
3. **Restricted Pointers**: Only structs can be pointers (`*User`). No `*int` or `*string`.
4. **Dynamic Access**: Use `~>` operator for duck-typing (JSON, maps, untyped data).

## Execution Backends

| Backend | Status | Notes |
|--------|--------|------|
| VM | ✅ Functional | Bytecode interpreter |
| JIT | ✅ Functional | Cranelift-based JIT |
| WASM | ✅ Functional | Runs in browser playground |
| AOT | 📋 Planned | Not implemented yet |

## Performance (Table 1, reference only)

*Note: results are from an informal / non-strict benchmarking environment. Numbers are not authoritative.*

Relative time ranking (lower is faster, `1.0x` = fastest):

| Rank | Language | Relative |
|------|----------|----------|
| 1 | C | 1.80x |
| 2 | Go | 2.01x |
| 3 | LuaJIT | 2.95x |
| 4 | Java | 5.39x |
| 5 | Vo-JIT | 5.69x |
| 6 | Lua | 39.52x |
| 7 | Vo-VM | 40.48x |
| 8 | Ruby | 119.05x |
| 9 | Python | 132.59x |

## Quick Examples

### Error Handling

Use `?` to propagate errors, `errdefer` for error-only cleanup:

```vo
func readConfig(path string) (Config, error) {
    file := open(path)?           // propagate error with ?
    errdefer file.Close()         // cleanup only if later steps fail
    
    data := readAll(file)?
    config := parse(data)?
    
    if config.Version < 1 {
        fail errors.New("invalid version")
    }
    return config, nil
}
```

### Dynamic Access (`~>`)

Duck-typing for `any`/interface values, perfect for JSON:

```vo
func getName(data any) (string, error) {
    var name string
    name = data~>users~>[0]~>name?  // access path, auto-cast to string
    return name, nil
}
```

### Familiar Go Syntax

```vo
type User struct {
    name string
    age  int
}

func (u *User) Greet() string {
    return "Hello, " + u.name
}

func main() {
    user := User{name: "Alice", age: 30}
    println(user.Greet())
    
    for i, v := range []int{1, 2, 3} {
        println(i, v)
    }
}
```

## Development

```bash
./d.py test          # All tests (VM + JIT)
./d.py test vm       # VM only
./d.py test jit      # JIT only
./d.py bench         # Benchmarks
./d.py loc           # Code statistics
```

## License

MIT License - see [LICENSE](LICENSE) for details.
