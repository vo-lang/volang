# Embedding Vo in Rust

Vo is designed to be embedded in Rust applications. The compiler and VM are pure Rust libraries — add a scripting layer to your project without shipping a separate runtime.

## Crate Overview

| Crate | Purpose |
|-------|---------|
| `vo-engine` | High-level compile + run API (recommended starting point) |
| `vo-vm` | Virtual machine, bytecode loader, `Vm` type |
| `vo-runtime` | GC, FFI types, extern registry, output sinks |
| `vo-common-core` | Shared bytecode `Module` type (for `no_std` targets) |

For most use cases, depend on `vo-engine` alone — it re-exports the types you need.

## Quick Start

### 1. Add the dependency

```toml
[dependencies]
vo-engine = { git = "https://github.com/vo-lang/volang", path = "lang/crates/vo-engine" }
```

### 2. Compile and run

```rust
use vo_engine::{compile_string, run, RunMode};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let compiled = compile_string(r#"
        func main() {
            println("Hello from Vo!")
        }
    "#)?;

    run(compiled, RunMode::Vm, vec![])?;
    Ok(())
}
```

## Compilation API

`vo-engine` provides several compilation entry points:

```rust
// Compile a source string (uses a temp directory internally)
let output = vo_engine::compile_string(code)?;

// Compile a source string rooted at a specific directory
let output = vo_engine::compile_source_at(code, Path::new("/my/project"))?;

// Compile a file or directory on disk
let output = vo_engine::compile(path)?;

// Compile with automatic dependency download (interactive/Studio use)
let output = vo_engine::compile_with_auto_install(path)?;
```

All return a `CompileOutput` containing the bytecode `Module`, source root, extension manifests, and locked dependency metadata.

## Execution API

### Run to completion

```rust
use vo_engine::{run, run_with_output, RunMode};

// Run with stdout output
run(compiled, RunMode::Vm, vec![])?;

// Run with JIT (Cranelift)
run(compiled, RunMode::Jit, vec![])?;
```

### Capture output

```rust
use std::sync::Arc;
use vo_engine::{run_with_output, RunMode, CaptureSink};

let sink = CaptureSink::new();
run_with_output(compiled, RunMode::Vm, vec![], sink.clone())?;

let output: String = sink.take();
println!("Vo printed: {}", output);
```

### Interrupt a running program

```rust
use std::sync::{Arc, atomic::AtomicBool};
use vo_engine::run_with_output_interruptible;

let flag = Arc::new(AtomicBool::new(false));
let flag_clone = flag.clone();

// Set flag from another thread to stop execution
std::thread::spawn(move || {
    std::thread::sleep(std::time::Duration::from_secs(5));
    flag_clone.store(true, std::sync::atomic::Ordering::Relaxed);
});

run_with_output_interruptible(compiled, RunMode::Vm, vec![], sink, Some(flag))?;
```

## Host Functions (Extern FFI)

The most powerful embedding feature: expose Rust functions to Vo code.

### Vo side

Declare extern functions as bodyless function signatures:

```vo
// myapp/main.vo

// Extern functions — implemented in Rust
func getConfig(key string) string
func setMetric(name string, value float64)

func main() {
    host := getConfig("hostname")
    println("Running on:", host)
    setMetric("startup_time", 0.42)
}
```

### Rust side

Implement the extern functions using `ExternCallContext`:

```rust
use vo_runtime::ffi::{ExternCallContext, ExternResult, ExternRegistry};
use vo_vm::bytecode::ExternDef;

fn get_config(ctx: &mut ExternCallContext) -> ExternResult {
    let key = ctx.arg_str(0);              // read string argument
    let value = match key {
        "hostname" => "prod-server-01",
        _ => "unknown",
    };
    ctx.ret_str(0, value);                 // return string to Vo
    ExternResult::Ok
}

fn set_metric(ctx: &mut ExternCallContext) -> ExternResult {
    let name = ctx.arg_str(0);
    let value = ctx.arg_f64(1);            // read float64 argument
    println!("[metric] {} = {}", name, value);
    ExternResult::Ok
}

fn register_externs(registry: &mut ExternRegistry, defs: &[ExternDef]) {
    for (id, def) in defs.iter().enumerate() {
        match def.name.as_str() {
            "getConfig" => registry.register(id as u32, get_config),
            "setMetric" => registry.register(id as u32, set_metric),
            _ => {}
        }
    }
}
```

### Wire it together

Use the lower-level `Vm` API to register your externs before running:

```rust
use vo_engine::compile_string;
use vo_vm::vm::{SchedulingOutcome, Vm};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let compiled = compile_string(SOURCE)?;

    let mut vm = Vm::new();
    let module = compiled.module;

    // Register stdlib + your externs
    vo_stdlib::register_externs(&mut vm.state.extern_registry, &module.externs);
    register_externs(&mut vm.state.extern_registry, &module.externs);

    vm.load(module);

    let outcome = vm.run().map_err(|e| format!("{:?}", e))?;
    assert_eq!(outcome, SchedulingOutcome::Done);
    Ok(())
}
```

## ExternCallContext Reference

Common methods for reading arguments and writing return values:

| Method | Description |
|--------|-------------|
| `arg_i64(n)` | Read argument `n` as `i64` |
| `arg_u64(n)` | Read argument `n` as `u64` |
| `arg_f64(n)` | Read argument `n` as `f64` |
| `arg_bool(n)` | Read argument `n` as `bool` |
| `arg_str(n)` | Read argument `n` as `&str` (zero-copy) |
| `arg_bytes(n)` | Read argument `n` as `&[u8]` (zero-copy) |
| `arg_ref(n)` | Read argument `n` as `GcRef` |
| `ret_i64(n, v)` | Write `i64` to return slot `n` |
| `ret_f64(n, v)` | Write `f64` to return slot `n` |
| `ret_bool(n, v)` | Write `bool` to return slot `n` |
| `ret_str(n, s)` | Allocate and return a string |
| `ret_bytes(n, b)` | Allocate and return a byte slice |
| `ret_nil_error(n)` | Write a nil error (success) |
| `ret_error_msg(n, s)` | Write an error with message |

Argument indices are 0-based and correspond to the Vo function signature order. Multi-slot types like `any` and `error` occupy 2 consecutive slots.

## no_std Embedding

For constrained environments (embedded, WASM guests), Vo supports `no_std` operation. Depend on `vo-vm` and `vo-runtime` with `default-features = false`:

```toml
[dependencies]
vo-vm = { git = "...", default-features = false }
vo-runtime = { git = "...", default-features = false }
vo-common-core = { git = "..." }
```

In `no_std` mode, you compile Vo to bytecode ahead of time (using the full `vo-engine` on your host), then load and run the pre-compiled `.vob` bytecode on the target:

```rust
use vo_vm::vm::{Vm, SchedulingOutcome};
use vo_common_core::bytecode::Module;

let bytecode: &[u8] = include_bytes!("../app.vob");
let module = Module::deserialize(bytecode).expect("invalid bytecode");

let mut vm = Vm::new();
vm.load(module);

match vm.run() {
    Ok(SchedulingOutcome::Done) => { /* success */ }
    Ok(SchedulingOutcome::Blocked) => panic!("deadlock"),
    Err(e) => panic!("runtime error: {:?}", e),
}
```

## Native Extensions

For larger integrations, Vo supports native extension modules distributed as shared libraries (`.dylib` / `.so` / `.dll`) or `.wasm` files. Extensions use `vo.ext.toml` and a `rust/` subdirectory with a standard Cargo project. See [Module System](./modules.md) for details.
