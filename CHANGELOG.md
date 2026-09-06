# Changelog

All notable user-facing changes are recorded here. Volang follows semantic
versioning for published toolchain releases.

## Unreleased

### Added

- Source-bound CI plans, task evidence, full-plan certification, and exact
  promotion verification.
- Windows Native AOT release packaging and a shared platform-neutral Web
  runtime build.
- Compiler caching, impact-aware pull-request lanes, weighted language-test
  sharding, and certified Nightly execution.
- Repository security, contribution, ownership, and governance policy.

### Changed

- Pages deployment promotes the Studio candidate produced and tested by main
  CI, removing a second application/runtime build.
- UI declaration validation and product certification now have distinct
  machine-readable statuses.
- Web runtime package metadata follows the workspace version.
- Core Wasm AOT materialized calls share one frame allocator and recycler,
  reducing the full Studio image from 41,000,583 to 32,606,429 bytes while
  preserving resumable goroutine and GC-visible frame semantics.

### Removed

- Obsolete Vogui rewrite governance, fixtures, scripts, and design documents.

### Fixed

- Duplicate release arguments in Wasm AOT test execution.
- Windows symbolic-link support in the standard library.
- macOS explicit-workspace case-alias detection and a scheduler-sensitive
  motion conformance assertion.
- Known vulnerable `rustls-webpki` and `tar` lockfile versions.
