# Contributing to Volang

Volang accepts focused changes that preserve the language, module, runtime, UI,
and release contracts documented in this repository. Open an issue for a large
semantic or compatibility change before investing in implementation.

## Development setup

Install the versions pinned by `rust-toolchain.toml` and `eng/toolchains.toml`.
Run repository commands from the root with `VOWORK=off` unless the task is
explicitly testing workspace overrides.

For a focused change, run the nearest crate or language-manifest tests first.
Before review, run:

```sh
cargo fmt --all -- --check
cargo run -q -p vo-dev --locked -- lint all
cargo test --locked -p vo-dev
```

Use `docs/ci.md` to reproduce the wider language, Web, UI, Nightly, and release
lanes. Generated files must be updated through their owning generator and the
worktree must remain clean after verification.

## Change expectations

- Add regression coverage for defects and executable acceptance coverage for
  features.
- Keep compiler/runtime/UI layers within the ownership boundaries enforced by
  repository lint.
- Preserve VM, JIT, Native AOT, Wasm VM, and Core Wasm AOT parity wherever the
  affected capability declares those targets.
- Document public behavior, compatibility, security, accessibility, and
  performance effects.
- Avoid unrelated formatting or generated-file churn.

All contributions are licensed under the repository's MIT license.
