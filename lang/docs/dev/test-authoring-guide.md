# Volang Test Authoring Guide

Status: source-backed workflow guide. Executable policy lives in `eng/*.toml`,
`tests/lang/manifest.toml`, `cmd/vo-dev`, `cmd/vo-test`, and declared task
commands.

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

## Task And Contract Tests

Add repository validation tasks in `eng/tasks.toml`. Every task needs `tools`,
`inputs`, `outputs`, `tier`, `tags`, and `owner`. Contract and stress tasks
must carry matching `contract` or `stress` tags.

```toml
[[task]]
name = "cargo-test-module"
title = "cargo module contract tests"
command = ["cargo", "test", "-p", "vo-module"]
tools = ["rust"]
inputs = ["lang/crates/vo-module/**", "Cargo.toml", "**/Cargo.toml", "Cargo.lock"]
outputs = []
tier = "contract"
tags = ["module", "crate-unit", "contract"]
owner = "module"
```

Public tasks must be reachable from a top-level group such as `pr`, `full`,
`test`, `contract`, `stress`, `site`, `release-verify`, or
`legacy-excluded`. Keep group arrays and `[[group]]` metadata in sync.

## Commands

Run these before opening a PR that changes test policy or cases:

```sh
cargo run -q -p vo-dev -- test lint --suite lang --strict
cargo run -q -p vo-dev -- test coverage --suite lang
cargo run -q -p vo-dev -- lint tasks --strict
cargo run -q -p vo-dev -- task coverage
cargo run -q -p vo-dev -- test explain --suite lang --case <case-id>
cargo run -q -p vo-dev -- task plan pr --changed --explain
```
