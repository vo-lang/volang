# Volang AOT compilation

Volang provides Native AOT deployment from verified bytecode. Web applications
ship bytecode with the Wasm VM. The former Core Wasm AOT backend and JavaScript
host were removed on 2026-09-16; rebuild Web applications with `vo ui build`.
For VM embedding, use `vo build --kind=bytecode --target=wasm32-unknown-unknown`.

Native targets:

| Target | `vo build --kind` | Output | Runtime contract |
| --- | --- | --- | --- |
| 64-bit little-endian native host | `bin` | linked executable | packaged `libvo_aot_runtime.a` or UI-aware `libvo_ui_aot_runtime_native.a` |
| 64-bit little-endian native target | `object` | ELF, Mach-O, or COFF object | matching static runtime at link time |

All paths consume the serialized, verified Volang module and run target
verification before lowering. Target spellings must be canonical. Native
32-bit, native big-endian, WASI Preview 1, and WASI Component AOT requests are
rejected at the target boundary.

## Commands

Build a native executable for the current host:

```sh
vo build ./cmd/server -o server
```

The official release archive places `vo`, `libvo_aot_runtime.a`, and
`libvo_ui_aot_runtime_native.a` together.
A custom runtime can be selected explicitly:

```sh
vo build ./cmd/server --runtime=/opt/volang/libvo_aot_runtime.a -o server
```

On Windows/MSVC the matching archives are `vo_aot_runtime.lib` and
`vo_ui_aot_runtime_native.lib`; `vo build` drives `link.exe` and emits an
`.exe`. `VO_AOT_LINKER` may select another link-compatible MSVC driver. As on
Unix, executable linking requires the platform linker and SDK to be installed;
`--kind=object` remains available when a downstream build owns final linking.

Custom static runtimes can pass their additional system libraries using repeated
`--link-arg=ARG` options, preserving each argument and its order. For example,
`--link-arg=-framework --link-arg=WebKit` supplies a macOS framework. The desktop
SDK records the exact requirements reported by `rustc --print native-static-libs`
and routes them through this same linker. On Windows, `--windows-gui` chooses
the GUI subsystem and the CRT startup for the generated `main` function. The
default executable remains a console application.

The linker selects the platform's UI runtime archive automatically when the
verified module mounts `github.com/vo-lang/ui`; ordinary command-line programs
keep the smaller core runtime. Packaged toolchains install both archives. A
custom UI runtime can be selected with `VO_UI_AOT_RUNTIME_LIB` or
`--runtime=PATH`; `VO_AOT_RUNTIME_LIB` continues to select the core runtime and
acts as the explicit fallback for UI builds.

Programs importing the `toolchain` package require a compiler host. Their
generated native entry calls the versioned runtime symbol
`vo_aot_initialize_toolchain_host_v1` before starting the program. The default
core runtime omits that capability, so linking such a program fails with the
missing symbol instead of producing an executable whose compiler calls fail
at runtime. `--kind=object` preserves this dependency for downstream linking.

An embedding that needs dynamic compilation can build `vo-aot-runtime` with
`--features toolchain-host` and select that archive through `--runtime=PATH`.
The CLI links the system libraries needed by this host on macOS and Windows.
`vo-dev test run --targets vm,native-aot-host --tags compiler-host` checks the
three dynamic compilation cases against the opt-in runtime and compares them
with VM execution. The core and compiler-host runtime variants run separately;
combining them in one test invocation is rejected to preserve their contracts.
Nightly runs both contracts on Linux, macOS, and Windows.
This opt-in archive includes the compiler and installs its host before entry.
Custom runtimes can implement the same C ABI initializer `int(void)`: install
a `ToolchainHost`, return zero on success, and return a nonzero process exit
code on failure. Initialization failure prevents the Vo program from running.
The ordinary release archive retains the smaller runtime without this feature.

Emit a relocatable cross-target object:

```sh
vo build ./cmd/server \
  --target=aarch64-unknown-linux-gnu \
  --kind=object \
  -o server.o
```

The CLI links executables only when the requested target equals the host.
Cross-target builds stop at an object so the target toolchain can supply its
linker, system libraries, and matching Volang runtime.

## Benchmark contract

`./d.py bench` checks output equality before reporting results for VM, JIT,
OSR, Native AOT, no_std VM and Wasm VM where supported. Native AOT measurements
execute a cache-disabled linked image. Wasm VM measurements include Node startup,
Wasm runtime initialization, source compilation and guest execution; they do not
isolate VM dispatch time. Build and execution measurements have separate labels.
Each run retains bytecode, native executables, identities and diagnostic counters.
Historical reports retain measurements for the removed Web AOT backend.

## AOT cache

Native AOT objects use a content-addressed cache at
`$HOME/.vo/aot/v1`. Set `VO_AOT_CACHE` to a non-empty absolute path to relocate
it. The key covers:

- compiler version and source-derived compiler build identity;
- canonical target;
- artifact kind and Native debug-IR option;
- Native AOT and extension ABI versions;
- the complete serialized verified module.

Entries carry their key, exact length, and payload SHA-256. Reads are bounded,
symbolic-link entries are rejected, and writes use a private temporary file,
`fsync`, and atomic replacement. Missing or corrupt entries are rebuilt. Cache
I/O failures produce a warning and leave the build usable.

## Extensions and host capabilities

Native AOT authenticates extension contracts during compilation and requires
matching providers in the final executable. Build each provider as a Rust
`staticlib`, register its linkme owner once, and pass every archive to the
linker:

```rust
#[cfg(feature = "dynamic-extension")]
vo_ext::export_extensions!();

#[cfg(feature = "aot-static-extension")]
vo_ext::register_static_extension!();
```

```sh
vo build ./app \
  --link-extension=/absolute/path/libimage_extension.a \
  --link-extension=/absolute/path/libdatabase_extension.a \
  -o app
```

The CLI canonicalizes and deduplicates archives, then force-loads them so their
provider tables survive archive dead-code elimination.

## Compatibility and release contract

Native metadata has its own ABI version. Decoders
reject unknown versions, duplicate or missing sections, non-zero reserved
fields, truncated inputs, trailing manifest bytes, invalid tables, target
mismatches, and values above declared limits.

Official release provenance records the CLI and static Native AOT runtime
path, size, and SHA-256. Release CI links and runs a Native AOT program before
packaging. The Web language lane executes bytecode through the Wasm VM.

Native AOT ABI version 3 adds a separately relocated static continuation table,
ordered resume PCs, and precise stack maps for each recovery body. The recovery
ABI carries its PC in argument lane zero and loads canonical frame slots; it
never participates in ordinary call dispatch or deopt fallback. Execution-budget
charges use the same region analysis as normal entry. This restores native
execution after scheduler and GC transitions without a runtime compiler.

Recovery optimization models every advertised PC as an independent CFG entry.
Its live-in values are unknown canonical-frame values, including unknown root
provenance unless the static slot type itself guarantees an exact base.
Constant propagation, value numbering, and range checks honor all external
edges. Entry trampolines load the live values and complete root projection for
their selected PC; object virtualization remains disabled for recovery bodies.
Additional analysis shares the per-function analysis budget, and exhaustion
retains the baseline recovery body. These decisions are made while building the
native image and do not require runtime compilation.

ABI version 4 changes the generated small-object allocation contract: a region
is guarded by exact physical size and every consumed object publishes its own
ValueMeta before its allocation bitmap bit. This permits equal-size objects of
different types to share admission. Code and runtime using the previous guard
and header contract must be rebuilt together; their unchanged field offsets do
not establish ABI compatibility.

ABI version 5 adopts the compact three-slot Rust string descriptor and its
exact-base array owner edge. Native images and their runtime must be rebuilt
together. The bytecode wire representation of string constants is unchanged;
the extension ABI fingerprint separately identifies the descriptor layout.

ABI version 6 makes the first five native argument words authoritative in
machine lanes. Callers populate the wide tail of the callee's shadow window;
Selected recursive call paths retain their existing leading-word mirror.
Ordinary callees initialize alias-backed leading parameters
before guest safepoints and publish the leading words on the cold tier-up path
that can reject entry before SSA initialization. VM and prepared-call entries
derive their lanes from their initialized frame; continuation entries retain
their independent frame-import contract. The signature width is unchanged,
but older callees rely on caller-populated leading slots, so images and runtime
must be rebuilt together.

ABI version 7 adopts the seven-slot canonical array slice and nine-slot
extended slice descriptors. All hot access fields remain in the common prefix;
element metadata and layout occupy separate 32-bit halves of one slot. Generated
native code reads the new layout and stride offsets. Images, runtimes and native
extensions must be rebuilt together; the extension fingerprint includes both
shapes and their tags. Bytecode retains its existing format.

ABI version 8 adopts one control word per Rust runtime Map bucket. Two high
bits distinguish empty, tombstone, occupied and forwarded buckets; the lower
62 bits retain the hash or forwarding index. Checked backing extents preserve
all usable probe/index bits. Key equality and generation-based iterator
identity are unchanged. The native extension fingerprint includes the Map
header, iterator, backing geometry, control encoding and key hash scheme; rebuild images,
runtimes and extensions together.

ABI version 9 embeds VOB22 modules with direct binary32 arithmetic and
comparison opcodes. Existing opcode numbers are preserved; new binary32
opcodes follow `ForLoop`. Each operation rounds at its declared width and
stores the low 32 result bits with a cleared high half. Rebuild native images
and runtimes together.

The metadata also includes the four-entry dynamic-call cache, the
optimizing-entry profile marker, the native stack-byte guard, and distinct
prepared-call continuation/frame fields. Producers, runtime metadata decoding, and the
native artifact cache key share `vo_jit::NATIVE_AOT_ABI_VERSION`; older native
images must be rebuilt. Literal bytes are read from the immutable module constant table through
`vo_str_new_const`; non-empty runtime strings still allocate Island-local
backing and descriptors under the managed admission contract.


Native ABI version 10 adds a verified zero/one hidden-argument offset to prepared
calls and dynamic-call cache entries. Captureless function values place user
arguments at slot zero; closure/receiver calls retain their hidden slot. The
register lanes and shadow frame use the same verified layout. Old native images
must be rebuilt with the matching runtime even though both C structs retain
their previous sizes. VOB22 remains unchanged.

Native ABI version 11 carries an optional module-local instruction source for
runtime traps separately from the physical recovery PC. Shared cold trap blocks
forward that source; generated frame recovery continues to use the calling
instruction. The VM validates the source against the loaded module, and every
new trap or explicit panic clears the previous source. Images and runtime
archives must be rebuilt together. This does not change VOB22
or the separate extension-provider ABI.


Native ABI version 12 accounts for the bounded detached small-block metadata
cache in SpanHeap. Its fixed directory changes generated-code-visible collector
field offsets. Rebuild native images and static runtime archives together;
images from the earlier layout must be rejected. The cache retains host metadata
only, with at most one instance per size class, and immediately returns empty
managed blocks for reuse. VOB22 remains unchanged. The extension
provider ABI remains independent: extensions use owner dispatch and their own
private Gc proxy, without accessing the host collector's Rust layout.

Native ABI version 14 embeds VOB24 compact source coordinates and logical source metadata. Images and static
runtime archives must be rebuilt together so the compiler-free decoder consumes
the same format as the producer. Compile cache schema 19 prevents reuse of the
earlier debug representation. This step leaves generated-code context offsets
and extension-provider ABI10 unchanged.

Native runtime failures retain an instruction source plus an optional physical
inline-callsite source. Both anchors resolve through the common bounded source
DAG. Panic save/recover/reuse moves the two anchors as one state. Engine errors
own their resolved names and locations independently of the executable module;
the compiler-free static runtime formats the chain while its module is alive.
This changes Rust-owned diagnostic state without adding a generated-code context
field or changing Native ABI14. Suspension PCs and precise root maps retain their
separate contracts.


Native ABI version 15 expands the shared dynamic-call cache to four stable
entries and one replaceable victim entry. Generated VM/JIT/OSR/Native code and
the static runtime agree on the 200-byte site stride; each entry keeps the
existing 40-byte identity, prepared-call shape, and dispatch generation guard.
Replacing the victim invalidates its previous native pointer and shape before
publishing a new target. Extension ABI10, VOB24, the bytecode compilation cache
schema are unchanged. Older Native images fail version
validation before execution.
