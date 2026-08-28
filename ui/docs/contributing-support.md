# Contributing, maintenance, and support

Changes should preserve the renderer-neutral boundary and prove behavior at the
lowest useful layer. A public feature normally includes typed `.vo` API,
bounded failure behavior, VM/JIT/AOT coverage, Web/native projection where
declared, accessibility semantics, documentation and capability evidence.

Before review:

```sh
cargo run -q -p vo-dev --locked -- ui-certify --check
cargo test --locked -p vo ui_
npm --prefix lang/crates/vo-web run test:ui
```

Run the focused quality-matrix suites affected by the change. Public API
changes update compatibility notes. Capability maturity advances only when the
declared target evidence exists; product gates complete only with executable
commands and repository evidence.

Maintainers own security response, dependency provenance, platform support,
deprecation windows, flaky-test quarantine, benchmark regressions and release
receipts. Unsupported targets return typed results and remain listed explicitly.

For support, provide a minimal reproduction and the diagnostics described in
[testing and troubleshooting](testing-troubleshooting.md). Feature proposals
should name the user outcome, authority boundary, target matrix, performance
budget, accessibility behavior and migration impact.
