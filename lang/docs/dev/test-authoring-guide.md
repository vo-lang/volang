# Volang Test Authoring Guide

Status: source-backed language-test workflow guide. Executable policy lives in
`eng/tests.toml`, `tests/lang/manifest.toml`, `cmd/vo-dev`, and `cmd/vo-test`.

## Language Cases

Add language regression files under `tests/lang/cases/`, projects under
`tests/lang/projects/`, and archives under `tests/lang/archives/`. Every
manifest entry in `tests/lang/manifest.toml` must declare:

```toml
[[case]]
id = "slice-append-regression"
kind = "file"
path = "cases/runtime/slice/slice_append_regression.vo"
matrix = "default"
tags = ["slice", "runtime", "regression"]
owner = "runtime"
expect = "pass"
```

Use a named matrix from `eng/tests.toml`; do not add explicit `targets`.
Common matrices are `default`, `native`, `gc`, `compile`, `default-gc`,
`gc-wasm`, `default-no-osr`, and `default-gc-jit-only`.

Compile-fail cases use the compile matrix and must explain why the failure is
expected:

```toml
[[case]]
id = "typechecker.pointer-must-be-struct"
kind = "file"
path = "cases/typechecker/pointer_must_be_struct.vo"
matrix = "compile"
tags = ["typechecker", "compile-fail"]
owner = "typechecker"
reason = "pointer types must point to struct types"
expect = { fail = ["invalid pointer type"] }
```

For a diagnostic that exists only in the WebAssembly compiler, use
`matrix = "wasm-only"` with the same `compile-fail` tag and expectation form.
Expected-failure matrices may contain only `compile` or `wasm` targets.

Skips and target timeouts require a reason:

```toml
skip = ["nostd", "wasm"]
timeout = { jit = 120 }
reason = "requires native time externs; JIT path is intentionally slower"
```

GC and JIT contract cases should be selected by metadata, not filename
heuristics:

```toml
matrix = "gc"
tags = ["gc", "slice", "contract", "regression"]
owner = "runtime-gc"
```

## Commands

Run these before opening a PR that changes test policy or cases:

```sh
cargo run -q -p vo-dev -- test lint --suite lang --strict
cargo run -q -p vo-dev -- test fmt --suite lang
cargo run -q -p vo-dev -- test coverage --suite lang
cargo run -q -p vo-dev -- test explain --suite lang --case <case-id>
```

The formatting gate checks the standard library, every manifest-owned source
or project, repository examples and benchmarks, and the Studio example
catalog. Parser-negative manifest cases are the only files
allowed to remain unformattable.
