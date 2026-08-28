# Tooling, resilience, and ecosystem

E7 turns the framework contracts into a workflow that can be created,
inspected, measured, tested, packaged and diagnosed from the Volang toolchain.
All application behavior and test fixtures remain `.vo` source.

## Project creation and authoring

`vo ui new` provides four maintained starters:

```sh
vo ui new hello
vo ui new operations --template=dashboard
vo ui new player --template=media
vo ui new studio --template=studio
```

Every starter is parsed by the CLI test suite and installed from the bundled,
authenticated official UI module. `ui/editors/vscode` contributes `.vo` syntax,
configuration and snippets with no executable editor-host or npm dependency.

Official UIKit implementation source can be inspected or exported without a
package-manager script:

```sh
vo ui source --list
vo ui source kit/icons -o icons.vo
```

File export refuses overwrites and writes a sibling provenance receipt with
the module version, canonical source path and SHA-256 digest. `kit/tokens`
exports deterministic `VUT1` and CSS design-token representations, while
`kit/icons` supplies bounded renderer-neutral symbols.

`ui/module-profiles.toml` publishes additive `minimal`, `application`, `web`,
`studio` and `full` product profiles. Capability declarations map imports to
core, graphics, media, editor, platform and testing sets. Release manifests
carry the selected profile through the existing Volang module capability
schema; the resolver validates requirements, conflicts, targets and exact lock
selection.

## Inspection, profiling and size

```sh
vo ui inspect .
vo ui inspect . --format=json
vo ui inspect . --target=web
vo ui inspect . --target=native
vo ui inspect . --runtime --mode=jit --viewport=1440x900@2
vo ui test . --mode=jit --profile
```

The versioned `volang.ui.inspection.v1` report contains the component identity,
execution path, node/slot/update counts, state/binding/handler counts, bytecode
size, selected target artifact size, function and extern counts, canonical
linked package graph, linked UI packages and authority-bearing packages. Web
inspection passes through the same authority verifier as release builds, so a
server package cannot be hidden from analysis.

`--runtime` explicitly executes the mounted application in the selected VM or
JIT development backend. The same versioned report then includes settled
viewport and revision, node and listener counts, layout boxes, scroll
containers, paint commands, semantic nodes, resource/media/graphics usage,
mount-to-settle time, reactive work, fiber storage, and live goroutines grouped
by runnable, running, blocked, host-wait, and I/O-wait state. Runtime execution
stays opt-in because application startup can invoke declared host authorities.

`vo ui test --profile` reports changed state writes, root evaluations, direct
turns, scheduled bindings, evaluator calls, submitted slots, revisions,
mutations and no-op updates after the authored semantic interaction sequence.
The optimized benchmark adds frame, completion-to-commit and keyed-component
p50/p95/p99 evidence.

## Diagnosis

```sh
vo ui doctor
vo ui doctor . --format=json
```

The doctor checks the host target, bundled official module, Web AOT runtime,
module cache and optional application compile/mount contract. Its JSON schema
is `volang.ui.doctor.v1`, making the same checks usable by IDEs and CI.

When a build fails, follow the boundary named by the diagnostic:

| Boundary | First check |
| --- | --- |
| module or lock | run `vo mod verify`, then `vo ui doctor` |
| Web runtime | build `lang/crates/vo-web` or set `VO_UI_WEB_RUNTIME` |
| client authority | inspect linked authority packages with `--target=web` |
| component update | run the interaction with `vo ui test --profile` |
| renderer/protocol | run the matching headless, Web or desktop conformance crate |
| packaged desktop | inspect the package receipt, target and signing policy |
| stale asynchronous result | verify context cancellation and generation/version checks |

## Testing and failure injection

`ui/testing` supplies a deterministic clock, bounded recorder,
cancellation-aware eventual assertions and named fault countdowns. The same
application service can therefore receive `permission-denied`, `device-loss`,
`oom`, `network-reset` or `late-completion` faults without target branches.

`ui/observability` supplies bounded structured events, metrics, spans, sensitive
attribute redaction and callback recovery. Ring buffers account dropped records
explicitly. A recovered callback produces a degraded result and an error
record; protocol corruption and runtime-integrity failures continue to fail at
their lower trust boundary.

`ui/media.CaptureSession` applies the same generation discipline to permission,
start, stop and close operations. Late permission or stream completions cannot
revive a closed session. The Web host projects portable media state into real
browser audio/video elements; invalid scalar values, unsafe sources and
oversized graphics programs fail before DOM mutation.

## Evidence and platform matrix

`ui/quality-matrix.toml` is the executable suite catalog. Pull requests run
renderer, protocol, browser, package, accessibility and language checks.
Linux, macOS and Windows run native host, system, AccessKit, WGPU, shell,
VM/JIT window, Native AOT, packaging and optimized performance jobs. Release
automation builds a fresh starter and verifies its Web and desktop artifacts
from the candidate toolchain.
