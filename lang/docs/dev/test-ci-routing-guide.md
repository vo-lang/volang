# Volang Test CI Routing Guide

Status: operational guide for the `vo-dev` task planner. Routing policy is
declared in `eng/tasks.toml`, `eng/ci.toml`, and task inputs.

## How Selection Works

`vo-dev task plan` starts from a selector:

- `pr`, `full`, `test`, `contract`, `vm-production`, `stress`, `site`,
  `release-verify`, and `qualification` select declared task groups.
- `task:<name>` selects one task and its dependencies.
- `smart` or `--changed` selects tasks from changed paths, known prefixes,
  task inputs, dependency expansion, downstream dependents, and fallback policy.

Use explain mode to inspect the reason for each selected task:

```sh
cargo run -q -p vo-dev -- task plan pr --changed --explain
cargo run -q -p vo-dev -- task plan pr --changed --format json --explain
```

Each selected task should report at least one reason, such as a matched
`eng/ci.toml` known prefix, a matched task input pattern, dependency expansion,
downstream dependent expansion, or unknown-path fallback.

## High-Risk Routing

Known prefixes keep contract-sensitive paths from relying on broad fallback:

- `lang/crates/vo-runtime/**` selects runtime and GC contracts.
- `lang/crates/vo-vm/**` selects VM, runtime, and GC contracts.
- `lang/crates/vo-jit/**` selects JIT, OSR, and GC/JIT contracts.
- `lang/crates/vo-analysis/**` selects typechecker contracts.
- `lang/crates/vo-codegen/**` selects codegen and compile contracts.
- `lang/crates/vo-module/**` selects module contracts.
- `lang/crates/vo-web/**` selects WASM and app-visible checks.
- `apps/studio/**` selects Studio, docs, quickplay, and static smoke checks.

If a path appears under `unknown_files` in JSON explain output, add a precise
task input or `known_prefix` rule before relying on fallback.

## Final Source-State Evidence

Query the current final selector fact source with:

```sh
cargo run -q -p vo-dev -- task final-selectors --format json
```

For final signoff, finish source-changing generators and normally track every
new deliverable path first. Run each returned selector with `task run` from one
frozen source state, synchronize the readiness document to the shared evidence
state, and run `vo-dev lint all` afterward. The exact ordered acceptance
sequence lives in `test-system-completion-plan.md`.

## Language Case Selection

Language selection is explainable separately from task selection:

```sh
cargo run -q -p vo-dev -- test plan --suite lang --tags gc --explain
cargo run -q -p vo-dev -- test explain --suite lang --case gc-closure-capture
```

The output names the matrix, owner, tags, target selection, skip reasons,
timeouts, and expectation reasons. JSON output is available for automation:

```sh
cargo run -q -p vo-dev -- test plan --suite lang --matrix gc --format json --explain
cargo run -q -p vo-dev -- test explain --suite lang --case gc-closure-capture --format json
```

## Debug Checklist

1. Run `task plan ... --explain`.
2. Check `unknown_files`; it should be empty for known repository surfaces.
3. Inspect the selected task reasons.
4. For language changes, run `test explain` on the relevant case id.
5. Update `eng/ci.toml`, task inputs, or manifest metadata if selection is too
   broad, too narrow, or unexplained.
