# Execution backends

Volang compiles every source project through the same frontend and verified
bytecode representation. Development and release backends consume that shared
semantic input with different startup, throughput, host, and packaging goals.

## Backend matrix

| Backend | Command | Primary role | Output |
| --- | --- | --- | --- |
| VM | `vo run app.vo --mode=vm` | shortest development startup, embedding | in-process execution |
| JIT | `vo run app.vo --mode=jit` | long native development sessions | in-process native code |
| Native AOT | `vo build .` | native release | executable or object |
| Core Wasm AOT | `vo build . --kind=wasm` | Web and sandboxed release | `.wasm` image |
| Bytecode | `vo build . --kind=bytecode` | storage and VM embedding | `.vob` module |

## VM

The VM interprets fixed-width register bytecode. It owns goroutine scheduling,
channels, defer/unwind state, precise stack roots, and incremental or
generational GC modes. Startup is short and behavior is observable, making it
the default for scripts, tests, interactive development, and small embedded
workloads.

Use memory flags on `vo run` to set reserve, limit, growth, and GC policy for a
bounded host. A build or host must reject invalid bytecode before execution.

## JIT

The Cranelift JIT starts from VM-owned state and compiles supported hot
functions or loops. On-stack replacement can enter compiled loop code while a
function is active. Compiled code uses the same runtime helpers, safepoints,
stack-map facts, error unwinding, and GC object model as the VM.

Use JIT for native sessions long enough to amortize compilation. Test both VM
and JIT when code exercises concurrency, FFI, panic/recover, GC-sensitive
objects, or hot loops.

## Native AOT

Native AOT lowers every verified function through the shared Cranelift path and
links the packaged runtime. It carries versioned runtime metadata for roots,
deoptimization, unwinding, and extern imports.

```sh
vo build . -o app
vo build . --kind=object --target=aarch64-unknown-linux-gnu -o app.o
```

The target runtime archive and extension archives must match the target and ABI
declared by the artifact. Release verification should exercise the final linked
binary on every supported operating system and architecture.

## Core Wasm AOT

Core Wasm AOT contains executable lowered Volang functions and no bytecode
interpreter. The image imports the versioned Volang host ABI for memory,
scheduling, output, and declared capabilities.

```sh
vo build . --kind=wasm -o app.wasm
```

Browser UI builds package this image with the official DOM/WebGPU adapter and
application assets. Node or embedded hosts can implement the same ABI without
adopting the UI layer.

## Semantic parity

Backend parity means equal observable language behavior for accepted programs:
results, errors, panic/defer ordering, channel behavior, memory safety, and host
capability validation. Timing, scheduling interleaving, compilation latency,
binary size, and peak throughput may differ.

The benchmark harness validates output equality before reporting local timing.
Performance numbers are machine-dependent evidence and are not permanent
language claims.

## Choosing a workflow

- Use VM for short feedback loops and deterministic debugging.
- Add JIT runs for long-lived native behavior and hot-path validation.
- Publish Native AOT for standalone desktop/server executables.
- Publish Core Wasm AOT for browser and sandboxed WebAssembly hosts.
- Keep bytecode only when a VM embedder is part of the product architecture.

The detailed AOT ABI, cache, target, extension, and release contract lives in
`docs/aot.md`. Runtime internals live in `lang/docs/spec` and `lang/docs/dev`.
