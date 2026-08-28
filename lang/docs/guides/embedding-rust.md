# Embedding Volang in Rust

The Rust crates expose the Volang compiler, verified module image, VM/JIT
execution, output sinks, interruption, extern functions, and lower-level
runtime contracts. Start with `vo-engine` unless the host needs direct VM
control.

## Crate roles

| Crate | Role |
| --- | --- |
| `vo-engine` | high-level compile and execute workflows |
| `vo-vm` | verified bytecode loading, scheduler, VM, and JIT integration |
| `vo-runtime` | GC, values, extern registry, output, and host runtime contracts |
| `vo-common-core` | shared `no_std` bytecode and metadata types |
| `vo-aot-runtime` | packaged runtime support for native AOT products |

Use workspace path dependencies while developing inside this repository. A
consumer should pin a released version or exact Git revision and preserve one
toolchain/runtime ABI family.

## Compile source

```rust
use std::path::Path;

fn compile() -> anyhow::Result<()> {
    let output = vo_engine::compile(Path::new("examples/basics/hello.vo"))?;
    println!("{} functions", output.module.functions.len());
    Ok(())
}
```

The high-level compile APIs accept a file or project and return an immutable,
verified loaded module plus source, extension, and locked dependency facts.
Project compilation does not solve or rewrite dependency intent.

When a selected dependency is absent, the explicitly named auto-install path
may authenticate and materialize bytes already pinned by the lock. Product
code should run module selection and fetch as a separate owner-visible step.

## Run and capture output

```rust
use vo_engine::{run_with_output, CaptureSink, RunMode};

fn execute(compiled: vo_engine::CompileOutput) -> anyhow::Result<String> {
    let sink = CaptureSink::new();
    run_with_output(compiled, RunMode::Vm, vec![], sink.clone())?;
    Ok(sink.take())
}
```

Choose `RunMode::Jit` only in hosts built with compatible JIT support. Output
sinks should enforce application-specific size and lifetime bounds.

## Interrupt work

Long-running guest execution should receive an owned interruption signal. The
host sets it during cancellation or shutdown; the runtime observes it at safe
boundaries and returns a controlled execution result.

Do not detach an unbounded VM thread after the caller has abandoned its result.
Keep the compiled snapshot, output sink, interrupt handle, and worker lifetime
under one session owner.

## Extern capabilities

Guest source declares an extern signature without a body:

```vo
func readSetting(name string) (string, error)

func main() {
    value := readSetting("theme")?
    println(value)
}
```

The host registers an implementation for the verified extern definition. Its
callback reads typed argument slots and writes every declared return slot,
including a nil or non-nil error result.

Extern code must validate argument counts and types, bound payloads, avoid
retaining borrowed guest memory, and expose the narrowest capability needed.
Filesystem, process, network, credential, and UI authority should remain in
the host and travel through versioned request/response contracts.

## Direct VM control

Use `vo-vm` when the host needs custom scheduling, extern registration, module
lifecycle, or incremental polling. Load only a verified module and register
providers before execution begins. The VM owns its heap and scheduler state;
follow the documented thread and re-entry restrictions.

## `no_std` and Wasm hosts

The core bytecode and runtime layers support constrained builds. Compile source
on a full host, serialize a verified `.vob`, and load it into a compatible
runtime. Core Wasm AOT hosts instead instantiate the lowered image and provide
the versioned AOT imports.

The language source must not assume that every native standard-library effect
exists. Unsupported capabilities return defined errors, and the embedder
chooses which providers to make available.

## Versioning and safety

- Pin compiler, bytecode, runtime, and extension ABI versions together.
- Treat guest source, bytecode, Wasm, messages, and extern payloads as
  untrusted input at host boundaries.
- Apply memory, instruction, output, message, and wall-clock limits appropriate
  to the product.
- Test malformed modules and extern failures as well as successful execution.
- Preserve backend parity by using shared verified facts instead of host-side
  semantic shortcuts.

The native FFI specification contains the normative slot, ownership, loading,
and validation rules.
