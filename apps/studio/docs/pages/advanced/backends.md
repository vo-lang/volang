# Execution Backends

Vo programs go through a two-stage pipeline: **compile** then **execute**. The Vo
compiler (`vo-codegen`) always produces the same bytecode module (`.vob`); what
differs is how that bytecode is executed. Three backends are available, each
targeting a different environment.

---

## VM (Bytecode Interpreter)

The default backend. Vo source is compiled to bytecode and interpreted by a
register-based virtual machine implemented in Rust.

```bash
vo run main.vo              # uses VM by default
vo run main.vo --mode=vm    # explicit
```

### Architecture

- **8-byte fixed instructions** — every instruction is exactly 8 bytes
  (`op:8 | flags:8 | a:16 | b:16 | c:16`), giving simple decoding and
  cache-friendly dispatch.
- **Register-based** — each register is an 8-byte stack slot addressed by
  16-bit index (`slots[bp + N]`). Multi-slot values (structs, interfaces)
  occupy consecutive registers.
- **Fiber-based cooperative scheduling** — goroutines are implemented as
  fibers. Fibers yield on channel operations, `yield`, or `runtime.Gosched()`.
  A scheduler round-robins ready fibers with a fixed time-slice.
- **Island concurrency** — independent island VMs can run on separate threads
  (native) or as separate WASM instances (browser). Cross-island communication
  uses ports; channels are island-local.
- **Incremental GC** — non-moving tri-color mark-sweep collector with precise
  slot scanning. Collection is incremental across scheduler boundaries with
  pause=200, step multiplier=100, and an 8 KB base step size by default.

### When to Use

- **Development** — fast startup, no compilation delay.
- **Scripting and embedding** — smallest footprint, `no_std` compatible core.
- **Short-lived programs** — startup cost dominates, JIT warm-up would be wasted.

---

## JIT (Just-In-Time Compilation)

A **mixed-mode** backend that starts in the VM interpreter and selectively
compiles hot code to native machine code via [Cranelift](https://cranelift.dev).
Requires the `jit` cargo feature on `vo-vm`.

```bash
vo run main.vo --mode=jit
```

### How It Works

1. **Profiling** — the JIT manager instruments every function call and loop
   back-edge. Counters are checked against configurable thresholds.
2. **Full-function compilation** — when a function's call count reaches
   `call_threshold` (default **100**), the entire function is compiled to
   native code by Cranelift at `opt_level = speed`.
3. **Loop OSR (On-Stack Replacement)** — when a loop back-edge count reaches
   `loop_threshold` (default **50**), the hot loop is compiled independently
   and execution transfers mid-function from the interpreter into JIT code.
4. **Direct JIT-to-JIT calls** — the JIT manager maintains a dispatch table
   so compiled functions can call each other directly without returning to
   the interpreter.
5. **VM-managed runtime paths** — functions outside the current strict-JIT
   support set remain interpreter-owned before JIT entry. Invalid strict-JIT metadata,
   helper ABI drift, or runtime-path contract violations fail fast instead of
   silently continuing under JIT.

### When to Use

- **Long-running or compute-heavy programs** — the warm-up cost is amortized
  across many iterations.
- **Benchmarks and numerical workloads** — useful for exercising hot
  full-function compilation, loop OSR, and VM-managed runtime paths.

### Tuning

| Environment Variable | Default | Description |
|---|---|---|
| `VO_JIT_CALL_THRESHOLD` | 100 | Call count before full-function compilation |
| `VO_JIT_LOOP_THRESHOLD` | 50 | Back-edge count before loop OSR |
| `VO_JIT_DEBUG` | *(unset)* | If set, print Cranelift IR for every compiled unit |

The language-test targets are configured in `eng/tests.toml`: `jit` sets
`VO_JIT_CALL_THRESHOLD=1`, `osr` sets `VO_JIT_CALL_THRESHOLD=1000` and
`VO_JIT_LOOP_THRESHOLD=1`, and `gc-jit` sets `VO_GC_STRESS=1`,
`VO_GC_VERIFY=1`, and `VO_JIT_CALL_THRESHOLD=1`.

---

## WASM (WebAssembly)

The Vo runtime (`vo-runtime`, `vo-vm`) is compiled to `wasm32-unknown-unknown`
in **`no_std`** mode. The resulting WASM module runs Vo bytecode inside the
browser — there is no native JIT in this path; execution is always the VM
interpreter.

### Architecture

```
┌──────────────────────────────────────────────────────┐
│  Browser / WebView                                   │
│  ┌──────────────┐   ┌────────────────────────────┐   │
│  │  JS Host     │◄──│  vo-web  (wasm-bindgen)    │   │
│  │  (Studio /   │──►│  ┌────────────────────────┐│   │
│  │   Playground)│   │  │ vo-vm (no_std, no JIT) ││   │
│  └──────────────┘   │  │ vo-runtime (no_std)    ││   │
│                     │  │ vo-stdlib              ││   │
│                     │  └────────────────────────┘│   │
│                     └────────────────────────────┘   │
│  ┌──────────────────────────────────────────────┐    │
│  │  Extension WASM modules (vogui, voplay, …)   │    │
│  │  Loaded via voSetupExtModule / voCallExt      │    │
│  └──────────────────────────────────────────────┘    │
└──────────────────────────────────────────────────────┘
```

- **Two feature tiers** — `vo-web` has a `compiler` feature (default on) that
  bundles the full compiler chain (`vo-syntax` → `vo-analysis` → `vo-codegen`).
  Without it, only pre-compiled bytecode execution is available, producing a
  much smaller WASM binary.
- **Extension ABI** — third-party Vo extension modules (e.g. `vogui`,
  `voplay`) ship as standalone `.wasm` binaries loaded at runtime. They
  communicate with the host VM through a tagged binary protocol
  (input: value/bytes slots; output: self-describing tags `0xE0`–`0xE4`).
- **Island transport** — cross-island commands are encoded as transport frames
  and routed between WASM VM instances via the JS host, mirroring the native
  thread-based island model.

### Where It Is Used

- **Vo Studio** (web mode) — compiles and runs Vo programs in-browser.
- **Vo Playground** — lightweight browser REPL.
- **Render islands** — Studio's GPU preview panel runs a secondary WASM VM
  for the render worker.

---

## Backend Selection Guide

| Criteria | VM | JIT | WASM |
|---|---|---|---|
| **Environment** | Native (CLI, embed) | Native (CLI) | Browser / WebView |
| **Startup** | Fastest | Moderate (warm-up) | Moderate (WASM load + compile) |
| **Peak throughput** | ~35× slower than C | ~4× slower than C | Same as VM (no JIT in browser) |
| **Binary size** | Small | +Cranelift deps | Depends on feature tier |
| **`no_std` support** | Yes | No (requires `std`) | Yes (core runtime) |
| **Debugging** | Full stack traces | Full + Cranelift IR | Browser console traces |

---

## Performance Comparison

The benchmark suite currently has 17 manifest entries under `benchmarks/`.
`./d.py bench all` runs each manifest-listed benchmark through `vo-dev` and
`hyperfine` with a default of one warmup and three measured runs
(`--warmup N` / `--runs N` can override them). The runner writes transient JSON,
Markdown, and `summary.json` files under `target/bench/results/`, places native
build artifacts under `target/bench/artifacts/`, and uses `target/bench/go-cache/`
as the repo-local Go cache. Those generated files are cleaned by
`vo-dev clean bench` and are not repository facts.

Use `./d.py bench score` after a local run to print the relative-time summary
for the current machine, compiler, and toolchain state.
