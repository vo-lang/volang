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
- **Incremental GC** — tri-color mark-sweep collector with Lua-style
  generational aging (young → survival → old → touched). Collection is
  incremental: marking is interruptible across scheduling steps; only the
  final atomic-mark and sweep-start phases are stop-the-world. Default
  parameters: pause=200 (trigger at 2× estimated live), step=8 KB.

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
5. **Graceful fallback** — functions with unsupported features are marked
   `Unsupported` and permanently remain in the interpreter. The rest of the
   program still benefits from JIT.

### When to Use

- **Long-running or compute-heavy programs** — the warm-up cost is amortized
  across many iterations.
- **Benchmarks and numerical workloads** — JIT-compiled tight loops approach
  native C/Go speed on many benchmarks.

### Tuning

| Environment Variable | Default | Description |
|---|---|---|
| `VO_JIT_CALL_THRESHOLD` | 100 | Call count before full-function compilation |
| `VO_JIT_LOOP_THRESHOLD` | 50 | Back-edge count before loop OSR |
| `VO_JIT_DEBUG` | *(unset)* | If set, print Cranelift IR for every compiled unit |

The test harness `./d.py test jit <file>` sets thresholds to 0 so JIT
triggers immediately for deterministic testing.

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

Geometric-mean relative time across 12 benchmarks (lower is faster, `1.0×` =
fastest in each benchmark). Measured on Apple M1, single-core, `--release`.

| Rank | Language | Geometric Mean |
|------|----------|---------------|
| 1 | C | 1.11× |
| 2 | Go | 1.80× |
| 3 | LuaJIT | 2.37× |
| 4 | Java | 3.59× |
| 5 | **Vo-JIT** | **4.25×** |
| 6 | Node | 5.46× |
| 7 | Lua | 24.48× |
| 8 | **Vo-VM** | **34.96×** |

### Per-Benchmark Breakdown

Each cell is the relative time vs. the fastest language in that benchmark
(`1.00` = winner). `–` means not applicable.

| Benchmark | C | Go | LuaJIT | Java | Vo-JIT | Node | Lua | Vo-VM |
|---|---|---|---|---|---|---|---|---|
| binary-trees | 1.27 | 1.29 | 3.04 | **1.00** | 6.45 | 1.29 | 8.14 | 14.69 |
| call-dispatch | – | **1.00** | 1.90 | 2.55 | 5.07 | 3.51 | 25.72 | 59.61 |
| fannkuch | **1.00** | 1.01 | 1.39 | 3.22 | 2.41 | 3.55 | 11.43 | 20.88 |
| fibonacci | **1.00** | 1.14 | 1.74 | 2.48 | 4.86 | 4.82 | 18.03 | 51.23 |
| matrix2 | 1.30 | 1.69 | **1.00** | 4.60 | 3.32 | 6.13 | 59.97 | 44.80 |
| nbody | **1.00** | 1.05 | 1.39 | 4.72 | 5.79 | 4.71 | 37.55 | 40.33 |
| quicksort | 1.02 | **1.00** | 1.99 | 2.92 | 3.39 | 6.24 | 14.58 | 27.89 |
| recursive-tree | **1.00** | 5.51 | 4.89 | 2.70 | 6.26 | 5.77 | 26.54 | 28.93 |
| sieve | **1.00** | 1.53 | 4.17 | 6.00 | 2.46 | 8.58 | 14.02 | 30.51 |
| spectral-norm | 1.59 | 3.07 | **1.00** | 5.53 | 6.78 | 8.26 | 48.01 | 63.28 |
| sum-array | **1.00** | 2.07 | 4.31 | 4.21 | 2.56 | 7.74 | 15.26 | 20.73 |
| task-queue | **1.00** | 1.18 | 1.62 | 3.21 | 1.67 | 4.87 | 14.57 | 16.66 |

### Observations

- **Vo-JIT** is competitive with Go and Java on tight integer/array loops
  (fannkuch 2.41×, sieve 2.46×, task-queue 1.67×, sum-array 2.56×).
- **GC-heavy** benchmarks (binary-trees, recursive-tree) show a wider gap due
  to allocation pressure on the incremental collector.
- **call-dispatch** stresses virtual/interface dispatch; the 5× JIT overhead
  reflects the current inline-cache cost vs. Go's devirtualized static dispatch.
- **Vo-VM** is roughly **8×** slower than Vo-JIT on average, consistent with
  the interpreted-vs-compiled gap seen in other language pairs (e.g. Lua vs
  LuaJIT ≈ 10×).

*Benchmarks use `./d.py bench all`. Results are informal and
hardware-dependent; not authoritative.*
