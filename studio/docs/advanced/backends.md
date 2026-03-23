# Execution Backends

Vo supports multiple execution backends, each suited for different use cases.

## VM (Bytecode Interpreter)

The default backend. Vo source is compiled to bytecode and interpreted by a register-based virtual machine.

- **When to use**: Development, scripting, embedding
- **Startup**: Fast (no compilation overhead)
- **Throughput**: Moderate

```bash
vo run main.vo              # uses VM by default
vo run main.vo --mode=vm    # explicit
```

## JIT (Just-In-Time Compilation)

Uses Cranelift to compile bytecode to native machine code at runtime.

- **When to use**: Performance-sensitive workloads, long-running programs
- **Startup**: Slightly slower (compilation step)
- **Throughput**: Near-native speed

```bash
vo run main.vo --mode=jit
```

## WASM (WebAssembly)

The Vo compiler and VM are compiled to WebAssembly, enabling Vo programs to run in the browser.

- **When to use**: Web playground, browser-based tools
- **How**: Used automatically by Vo Studio web and the Playground

## Performance Comparison

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

*Note: results are from an informal benchmarking environment. Numbers are not authoritative.*
