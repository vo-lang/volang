# vo-test

`vo-test` is the native language test execution engine. It is not the public
entry point for selecting tests.

Current test selection lives in:

```text
tests/lang/manifest.toml
eng/tests.toml
cmd/vo-dev/src/test_config.rs
cmd/vo-dev/src/test_manifest.rs
cmd/vo-dev/src/test_system.rs
```

Use `vo-dev` or the `d.py` compatibility wrapper for normal workflows:

```sh
cargo run -q -p vo-dev -- test lint --suite lang
cargo run -q -p vo-dev -- test run --suite lang --targets vm,jit
cargo run -q -p vo-dev -- test run --suite lang --targets vm,jit --path tests/lang/cases/foo.vo
./d.py test both tests/lang/cases/foo.vo
```

## Execution Interface

`vo-dev` generates a structured plan and invokes:

```sh
vo-test run-plan <plan.json> [--jobs N] [--format text|json] [--verbose]
```

When `--jobs` is omitted, `vo-test` caps the worker count at 8 even on larger
machines. This keeps JIT/runtime scheduler regressions from being hidden behind
host-specific oversubscription. Text output emits a progress line every 30
seconds and a complete failure summary at the end.

Jobs whose backend is `jit` must enter JIT-compiled function or loop code at
least once. A run that completes entirely through the interpreter is reported as
a failed JIT job, even if the program output is otherwise correct.

The plan schema is `volang.test-plan.v1`. A job contains:

- `id`: stable job id.
- `case_id`: manifest case id.
- `kind`: `file`, `project`, or `zip`.
- `path`: repo-relative path to compile.
- `target`: logical target from `eng/tests.toml`.
- `backend`: execution backend, such as `vm`, `jit`, `compile`, or `vo-embed`.
- `env`: per-target environment.
- `timeout_sec`: job timeout, resolved from the target default plus any
  target-specific `timeout = { ... }` manifest override.
- `expect`: pass/fail expectation and diagnostic patterns.

`--format json` emits one `volang.test-result.v1` object with aggregate counts
and per-job status records.

## Unsupported Interface

Direct mode selection such as `vo-test both`, `vo-test vm`, or `vo-test jit`
is intentionally unsupported. New test discovery, target expansion, skips,
expected failures, and CI integration belong in `vo-dev` and
`tests/lang/manifest.toml`.
