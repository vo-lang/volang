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
`test`, `contract`, `vm-production`, `stress`, `site`, `release-verify`, or
`legacy-excluded`. Keep group arrays and `[[group]]` metadata in sync.

Ephemeral `target/...` outputs are cleared before a task runs by default. A
producer that builds into a unique same-parent staging directory, verifies the
complete generation, and replaces the published directory with rollback may
declare `output_policy = "transactional"`. This policy only suppresses runner
pre-cleaning for declared `target/...` outputs; checked-in outputs are never
pre-cleaned. Transactional producers must include a failure-injection contract
test proving that preflight/build failure retains the previous payload and
manifest and that success replaces the full directory generation.

The portable protocol uses two same-parent directory renames: published output
to a unique backup, then verified staging to the published path. It guarantees
rollback for failures observed by the producer. It is not a single filesystem
exchange operation; a producer must detect the restart-visible crash window and
restore the newest complete backup whenever the published path is missing.

## Commands

Run these before opening a PR that changes test policy or cases:

```sh
cargo run -q -p vo-dev -- test lint --suite lang --strict
cargo run -q -p vo-dev -- test fmt --suite lang
cargo run -q -p vo-dev -- test coverage --suite lang
cargo run -q -p vo-dev -- lint tasks --strict
cargo run -q -p vo-dev -- task coverage
cargo run -q -p vo-dev -- test explain --suite lang --case <case-id>
cargo run -q -p vo-dev -- task plan pr --changed --explain
```

The formatting gate checks the standard library, every manifest-owned source
or project, repository examples and benchmarks, and the Studio and legacy
playground example catalogs. Parser-negative manifest cases are the only files
allowed to remain unformattable.
