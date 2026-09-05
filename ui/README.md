# Volang UI

Volang UI is the independent, renderer-neutral application framework for
Volang. Applications use typed `.vo` source across Web and desktop, develop on
the VM or JIT, and release through Core Wasm AOT or Native AOT. Application
projects carry no npm dependency graph or JavaScript component runtime.

## Current status

The Volang UI source tree declares a complete 1.0 product contract.
All E0-E8 delivery increments, 57 required capabilities, five showcases, and
twelve product gates are complete. Publication still requires the tagged
candidate and protected-main CI identity defined by
`product-certification.toml`. The M0-M7 foundation remains certified and frozen
as an end-to-end baseline. The completed system includes:

- renderer-neutral views, properties, events, generational identities, and
  atomic mutation batches;
- fine-grained reactivity, direct update sites, a keyed generic reconciler,
  hot-reload state migration, and bounded goroutine completion turns;
- headless, DOM, and native retained renderers;
- shared layout, paint, text, IME, focus, accessibility, and system-service
  contracts;
- browser VM development, Core Wasm AOT release, native VM/JIT development,
  and Native AOT release paths;
- real-browser, real-window, protocol, accessibility, golden, differential,
  and performance evidence;
- `vo ui new`, `dev`, `run`, `build`, `test`, `inspect`, `doctor`, and `source`
  workflows.

Run the governance check with:

```sh
cargo run -q -p vo-dev --locked -- ui-certify --check
```

The output reports `declaration-valid`, the stable capability count, and the
completed E8 delivery state. CI reports `product-certified` only after
`vo-dev ui-certify --evidence target/ci/certification.json` verifies the full
Web and three-platform evidence set for the same immutable source commit.

## Product certification

The current source-of-truth files are:

- [`product-roadmap.toml`](product-roadmap.toml): product domains, showcases,
  and 1.0 gates;
- [`capabilities.toml`](capabilities.toml): capability ownership, dependencies,
  target support, maturity, API stability, acceptance, and evidence;
- [`delivery.toml`](delivery.toml): E0-E8, the active increment, work streams,
  and six permanent contract probes;
- [`roadmap.toml`](roadmap.toml): frozen M0-M7 foundation record;
- [`certification.toml`](certification.toml): executable foundation gates.

E0-E8 are complete. The product includes Component Model V2, structured
state and goroutine ownership, a comfortable cross-platform API, the official
UIKit, typed application/data patterns, semantic SSR and activation, server
authority isolation, PWA deployment, native window lifecycle, standalone
desktop packaging, authenticated update rollback, and independently linkable
graphics, media, document, editor and workspace packs, deterministic testing,
observability, inspection, project diagnosis, source export, templates and a
zero-runtime editor extension. E8 binds this evidence to the stable 1.0
compatibility, documentation and release contract.

The E1 implementation mounts keyed local and imported components from
VUB1, gives equal component types independent scalar state and handler
closures, preserves nodes across incremental VM/JIT updates, migrates state
through transactional reload, and lowers the same evaluator entries to Native
and Core Wasm AOT. The permanent authored probe certifies move, removal,
reinsertion, replacement, stale-handle rejection, real-browser execution, and
packaged Native AOT interaction. Its optimized workload keeps 256 keyed,
stateful instances stable under an 8 ms p95 update budget.

## Ownership

Top-level `ui/` owns framework contracts and remains independent from `lang/`.
Renderer-neutral crates preserve `no_std + alloc` where declared. Compiler,
VM, AOT, Web, native, text, accessibility, GPU, window, and system adapters
depend inward through versioned contracts.

Ordinary applications will normally use:

```text
ui
ui/kit
```

Advanced capabilities are opt-in and independently linked:

```text
ui/resource   ui/navigation   ui/forms      ui/commands
ui/web        ui/desktop      ui/document   ui/editor
ui/graphics   ui/assets       ui/animation  ui/chart
ui/media      ui/language     ui/workspace  ui/system
ui/testing    ui/observability ui/kit/icons
```

The advanced package map is part of the 1.0 architecture. Packages link only
when used and retain the ownership recorded in `capabilities.toml`.

## Existing baseline workflows

Create and develop an application:

```sh
vo ui new hello-ui
cd hello-ui
vo ui dev --open
```

Run a native development window:

```sh
vo ui run . --mode=vm
vo ui run . --mode=jit
```

Inspect and test the selected component path:

```sh
vo ui inspect
vo ui doctor
vo ui test --mode=vm
vo ui test --mode=jit
vo ui source --list
```

Build a static Core Wasm AOT application:

```sh
vo ui build -o dist
```

Examples under [`examples`](examples) remain regression and migration fixtures.
New applications should begin from the maintained dashboard, media, or Studio
templates.

## Architecture and policy

- [`docs/architecture.md`](docs/architecture.md) defines the target architecture
  and the certified invariants it preserves.
- [`docs/component-model-v2.md`](docs/component-model-v2.md) is the E1
  implementation brief and exit contract.
- [`docs/cross-platform-foundation.md`](docs/cross-platform-foundation.md)
  documents the E2 environment, callback, task, stream, effect, measurement,
  focus, portal, and command contracts.
- [`docs/application-data-platform.md`](docs/application-data-platform.md)
  documents the E4 task, resource, navigation, form, command and data packages.
- [`docs/web-desktop-products.md`](docs/web-desktop-products.md) documents E5
  Web SSR/deployment and native desktop packaging/lifecycle/update contracts.
- [`docs/advanced-capability-packs.md`](docs/advanced-capability-packs.md)
  documents E6 graphics, assets, animation, media, document, editor, language,
  workspace and structured-goroutine contracts.
- [`docs/tooling-resilience-ecosystem.md`](docs/tooling-resilience-ecosystem.md)
  documents E7 templates, inspection, diagnosis, deterministic tests,
  observability, source export and the real-platform quality matrix.
- [`docs/getting-started.md`](docs/getting-started.md) is the shortest path from
  installation to VM/JIT development and AOT release.
- [`docs/authoring-guide.md`](docs/authoring-guide.md) covers components, state,
  goroutines, UIKit, resources, and application composition.
- [`docs/testing-troubleshooting.md`](docs/testing-troubleshooting.md) covers
  semantic tests, browser/native evidence, profiling, diagnosis, and recovery.
- [`docs/accessibility-localization.md`](docs/accessibility-localization.md),
  [`docs/security.md`](docs/security.md), and
  [`docs/compatibility-migration.md`](docs/compatibility-migration.md) define
  the corresponding 1.0 product contracts.
- [`docs/contributing-support.md`](docs/contributing-support.md) defines package
  contribution, maintenance, disclosure, and support expectations.
- [`docs/release-notes-1.0.md`](docs/release-notes-1.0.md) summarizes the 1.0
  product boundary and migration path.
- [`docs/package-authoring.md`](docs/package-authoring.md) defines portable
  package boundaries, capability profiles, provenance and compatibility.
- [`docs/roadmap.md`](docs/roadmap.md) explains capability maturity, E0-E8, and
  the contract probes.
- [`docs/platform-capabilities.md`](docs/platform-capabilities.md) records the
  current platform-service baseline.
- [`docs/migration-from-js.md`](docs/migration-from-js.md) maps the existing
  JavaScript ecosystem responsibilities onto the Volang toolchain.
- [`docs/release-policy.md`](docs/release-policy.md) defines stability,
  compatibility, certification, and publication authority.
