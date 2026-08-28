# WebAssembly AOT benchmark report

Date: 2026-08-21 (CST)
Host: macOS 26.5.2 (25F84), arm64
Run ID: `1787279263-100588000-38624`

This report measures generated Core Wasm after the complete bytecode lowering,
typed-call specialization, precise shadow-root architecture, bounded recursion
transition, and recyclable stack-chunk optimization. The Wasm image contains
no Volang bytecode interpreter. The complete machine-readable result is
`target/bench/runs/1787279263-100588000-38624/results/summary.json`.

## Method

- Suite: all 21 benchmark manifests.
- Sampling: two warmups followed by five measured runs per command.
- Timing tool: hyperfine 1.20.0.
- Volang build profile: `release-native`; AOT caches disabled.
- Wasm timing: each sample starts Node 24.16.0, validates and compiles the
  image, instantiates the ABI v5 production host, runs `vo_start`, and exits.
- Native AOT timing: the already-built executable is timed; compilation and
  artifact size are recorded separately.
- Correctness: VM stdout is authoritative for the Volang backends. VM, JIT,
  Native AOT, and Wasm AOT matched on all 21 cases.
- Cross-language ranking: the 11 cases available to every ranked language,
  normalized to VM = 1.000; lower is faster.

Toolchain versions were Rust 1.94.0, Go 1.26.1, Node 24.16.0, Lua 5.5.0,
LuaJIT 2.1.1772619647, Python 3.14.3, Ruby 2.6.10, Java 25.0.2, Apple Clang
17.0.0, wasm-pack 0.14.0, and npm 11.13.0.

## Volang backend result

Across all 21 paired cases, Wasm AOT / VM has a geometric-mean ratio of
**0.6630×**: the cold-process Wasm command has 33.7% lower aggregate relative
time, with 13 wins and 8 losses. Wasm / Native AOT is **3.2299×** and Wasm /
JIT is **2.3109×**. These ratios include Node startup, Wasm validation,
compilation, instantiation, and host construction in every sample.

| Benchmark | VM ms | Wasm AOT ms | Native AOT ms | JIT ms | Node ms | Go ms | C ms |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| binary-trees | 1454.2 | 1768.0 ± 25.7 | 445.7 | 443.6 | 190.9 | 199.9 | 191.1 |
| call-dispatch | 2059.5 | 1447.8 ± 180.3 | 57.1 | 83.0 | 129.3 | 28.4 | — |
| channel-block-wake | 1919.2 | 227.6 ± 32.5 | 1911.5 | 2100.6 | — | — | — |
| fannkuch | 728.1 | 495.3 ± 56.6 | 51.4 | 80.2 | 178.6 | 19.5 | 18.8 |
| fibonacci | 1756.4 | 702.0 ± 128.7 | 82.5 | 112.9 | 231.6 | 39.8 | 36.7 |
| jit-call | 113.7 | 156.6 ± 10.9 | 13.1 | 37.4 | — | — | — |
| jit-copy | 72.1 | 136.0 ± 9.6 | 63.5 | 78.9 | — | — | — |
| jit-loop | 710.6 | 161.7 ± 8.8 | 26.5 | 46.0 | — | — | — |
| jit-map | 154.9 | 166.1 ± 6.2 | 82.8 | 108.0 | — | — | — |
| jit-slice | 98.7 | 150.1 ± 4.5 | 37.3 | 61.5 | — | — | — |
| matrix2 | 657.4 | 171.8 ± 12.2 | 45.4 | 67.8 | 125.2 | 26.1 | 13.5 |
| nbody | 467.4 | 251.0 ± 6.6 | 29.6 | 57.7 | 113.8 | 16.7 | 15.5 |
| quicksort | 497.3 | 301.5 ± 12.6 | 80.5 | 112.8 | 165.5 | 28.1 | 20.4 |
| recursive-tree | 791.0 | 2436.6 ± 32.3 | 209.6 | 252.4 | 215.1 | 180.9 | 40.1 |
| scheduler-spawn-peak | 173.2 | 193.9 ± 10.3 | 162.4 | 175.0 | — | — | — |
| scheduler-spawn-recycle | 189.3 | 156.0 ± 14.2 | 196.6 | 207.4 | — | — | — |
| select-block-wake | 1202.5 | 198.1 ± 13.7 | 1277.2 | 1335.1 | — | — | — |
| sieve | 402.4 | 341.2 ± 30.1 | 69.1 | 97.8 | 153.0 | 26.1 | 10.1 |
| spectral-norm | 974.3 | 201.3 ± 7.4 | 78.4 | 98.4 | 149.1 | 46.5 | 20.7 |
| sum-array | 237.8 | 239.8 ± 12.0 | 69.5 | 93.4 | 139.7 | 29.6 | 14.4 |
| task-queue | 184.8 | 176.5 ± 13.3 | 39.8 | 72.0 | 120.7 | 22.2 | 16.3 |

Values are arithmetic means. Wasm cells also show one standard deviation.
Per-case JSON files retain median, minimum, maximum, user and system time,
every sample, memory use, and exit code.

## Cross-language common-scope ranking

| Rank | Runtime | Score vs VM | Common cases |
| ---: | --- | ---: | ---: |
| 1 | C | 0.0386 | 11 |
| 2 | Go | 0.0638 | 11 |
| 3 | LuaJIT | 0.0910 | 11 |
| 4 | Volang Native AOT | 0.1291 | 11 |
| 5 | Volang JIT | 0.1825 | 11 |
| 6 | Java | 0.2469 | 11 |
| 7 | Node | 0.2619 | 11 |
| 8 | Volang Wasm AOT on Node | 0.6810 | 11 |
| 9 | Volang VM | 1.0000 | 11 |
| 10 | Lua | 1.0024 | 11 |
| 11 | Python | 2.7401 | 11 |
| 12 | Ruby | 2.9403 | 11 |

On each language's available paired cases, Wasm AOT / Node is **2.9367×**
(12), Wasm AOT / Go is **12.1678×** (12), and Wasm AOT / C is **17.6266×**
(11). The common ranking's 0.6810 score differs from the all-21 Wasm/VM ratio
because scheduler and backend-specific cases have no cross-language
implementations.

Hyperfine recorded 10 warnings for outliers, first-run cache effects, or
commands near its shell-timer resolution. The 50 cross-language textual
mismatches are expected output-format differences and remain separate from the
exact 21/21 Volang backend equality gate. No samples or cases were discarded.

## Artifact cost

| Artifact | Mean compile time | Range | Mean size | Range | Total size |
| --- | ---: | ---: | ---: | ---: | ---: |
| Core Wasm | 45.88 ms | 37.51–52.74 ms | 77,194 B | 53,987–148,553 B | 1,621,078 B |
| Native executable | 460.92 ms | 300.20–2241.84 ms | 26,832,609 B | 26,809,664–26,876,096 B | 563,484,800 B |

Core Wasm compilation totals 0.963 seconds for all 21 programs. The mean Wasm
image is 0.288% of the mean statically linked native executable. The native
maximum includes the first cold link in the suite; all values remain in the
machine-readable record.

## Current optimizer and root architecture

The backend selects execution strategy from shared bytecode effects, verified
slot layouts, call-graph fixed points, and closed-world dynamic target sets:

- safe-point-free functions use typed multi-value Wasm calls and keep scalar
  arguments, results, and promoted slots in locals;
- non-suspending allocation functions use per-fiber precise shadow-root
  records, with lazy 4 KiB base chunks and recyclable 64 KiB overflow chunks;
- suspending, deferred, unwind-sensitive, and resumable calls use durable
  linear-memory frames and explicit resume states.

Every directly compiled function also has a durable body. Rooted recursion
switches at an exact call boundary when its native-Wasm segment budget is
exhausted, so allocation, host effects, panic state, fuel, return values, and
GC roots are never replayed. Pure scalar or read-only recursive components may
use retry only after a closed-world proof excludes observable effects.

Reused durable allocations are zeroed by the frame allocator. Shadow-root
chunks request uninitialized storage and clear each exact root record before
publishing it to the collector. This removes redundant whole-chunk clears
without weakening zero-value, unwind, or precise-GC invariants. The compiler
also publishes bytecode PCs only at observable effect boundaries and emits a
standard deterministic Wasm `name` section for engine profiling.

These policies contain no benchmark-name or source-spelling checks.

## Interpretation

The strongest Wasm results exercise scheduler operations or kernels that map
well to Core Wasm: channel block/wake is 8.43× faster than VM, select block/wake
is 6.07× faster, spectral norm is 4.84× faster, the loop microbenchmark is
4.40× faster, and matrix multiplication is 3.83× faster. Short cases such as
`jit-call`, `jit-copy`, and `jit-slice` expose the fixed cost of starting Node,
validating the image, compiling Wasm, building the host, and instantiating
memory on every sample.

`recursive-tree` remains the clear steady-state outlier: Wasm is 3.08× VM,
11.62× Native AOT, and 11.33× Node. The benchmark combines recursive calls,
managed allocations, object traversal, and precise roots. Wasm therefore pays
for a GC-visible root record at each allocating call and retains a checked
transition to the durable stack for deep recursion. The competing programs use
different object representations and can keep more state in engine-native
frames. Removing those checks would invalidate GC or stack-overflow semantics.
The next sound improvements require broader escape-aware aggregate
representation, interprocedural allocation specialization, or a more compact
structured rooted-call ABI; they are architectural compiler work rather than
an overlooked local peephole.

Against the directly comparable pre-optimization baseline
`1787164633-687320000-14224`, the all-21 Wasm/VM ratio moved from **1.0119×**
to **0.6630×** (34.5% lower), and the common-scope score moved from **1.2938×**
to **0.6810×** (47.4% lower). These aggregates describe this suite and host;
they do not claim universal workload speedups.

## Readiness evidence

- 21/21 benchmark cases produce identical output across VM, JIT, Native AOT,
  and Wasm AOT.
- The independent `wasm-aot` language target passes 1116/1116 cases.
- Dedicated Wasm AOT unit tests pass 13/13, including deterministic code,
  standard names, typed recursion, precise roots, exact recursion transition,
  dynamic dispatch, bounded initial memory, and uninitialized chunk safety.
- The complete Rust workspace test command passes with zero failures; strict
  full-workspace Clippy and Rust formatting pass.
- Language manifest lint passes; Vo formatting passes for all 1365 governed
  files, with the 11 parser-negative fixtures skipped by design.
- The Web build succeeds; VFS tests pass 14/14 and AOT host tests pass 8/8.
- ABI v5, extern table v3, runtime metadata v1, debug metadata v2, and import
  namespace `volang:runtime/v3` are validated before instantiation. Unknown
  shapes, versions, targets, layouts, and handlers fail closed.
- Component Model output remains gated on a canonical typed capability ABI for
  transferred Volang values.

Repository-wide `vo-dev lint all` still stops at the separately documented
Studio/Vogui governance gate: Studio intentionally has no `vogui-protocol` Git
revision until the external Vogui rewrite has a legal commit identity. Filling
that field with an invented revision would violate the repository contract and
is outside the AOT delivery.

The evidence establishes complete Core Wasm execution for accepted programs,
a production-oriented ABI and host, and an optimizer/root architecture whose
remaining large gap is explicit rather than hidden behind semantic fallback.
