# Changelog

All notable user-facing changes are recorded here. Volang follows semantic
versioning for published toolchain releases.

## Unreleased

### Added

- Replacement preview UI framework with explicit components and scopes, reactive
  state, forms, routing, SSR, data resources, accessible HTML controls and optional
  editor, plot and Canvas integrations. Portable project tools include creation,
  development reload, browser tests and native language-service authoring.
- Rewritten Studio Gallery, Docs and Playground with isolated workers, source
  diagnostics, saved drafts and a read-only legacy project export path.
- Standalone desktop packaging for VM, JIT and Native AOT through a matching SDK.
  Application identifiers preserve isolated browser storage across updates and
  relocation; HTTP(S) links open in the system browser. Desktop SDK/application
  manifests use v2 and persistent macOS applications require macOS 14 or newer.
  Export important drafts from earlier desktop previews before upgrading; their
  previous storage remains untouched.
- Project-aware preview UI diagnostics, including JSON output and desktop SDK
  checks. `vo check --read-only` validates existing sources without generators,
  downloads or compilation cache writes.
- Source-bound CI plans, task evidence, full-plan certification, and exact
  promotion verification.
- Windows Native AOT release packaging and a shared platform-neutral Web
  runtime build.
- Compiler caching, impact-aware pull-request lanes, weighted language-test
  sharding, and certified Nightly execution.
- Repository security, contribution, ownership, and governance policy.

### Changed

- Preview UI `check` validates every declared source entry and host imports
  without executing prerendering or producing a distribution. Use `build`
  explicitly before `preview`.
- Pages deployment promotes the Studio candidate produced and tested by main
  CI, removing a second application/runtime build.
- UI declaration validation and product certification now have distinct
  machine-readable statuses.
- Web runtime package metadata follows the workspace version.
- Web builds use Wasm VM bytecode (`.vob`); runtime and application loading
  start concurrently. Native JIT and Native AOT remain available.
- UI topology preflight avoids copying unchanged sibling lists and handles
  structural sibling updates with batch-local linked order. Widget providers
  can declare native commit dependencies; popovers retain resize observations.

### Removed

- Core Wasm AOT compiler, JavaScript runtime, support package, and build/test
  targets. `--kind=wasm` reports the bytecode migration command.
- Obsolete Vogui rewrite governance, fixtures, scripts, and design documents.

### Fixed

- Delayed controlled popover/dialog close notifications no longer overwrite
  a newer keyboard action. Native dismissal remains observable.
- Wasm test runner treats `os.Exit(0)` as success and reports nonzero exit codes.
- Windows symbolic-link support in the standard library.
- macOS explicit-workspace case-alias detection and a scheduler-sensitive
  motion conformance assertion.
- Known vulnerable `rustls-webpki` and `tar` lockfile versions.
