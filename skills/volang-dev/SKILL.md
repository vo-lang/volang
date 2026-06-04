---
name: volang-dev
description: Repo-specific maintenance guide for Volang. Use when Codex needs to modify, debug, review, or explain the Volang compiler frontend, type checker, codegen, bytecode, VM, JIT, GC, module system, stdlib, native FFI, CLI, engineering tasks, tests, WASM, Studio, quickplay, docs, release tooling, or any source in this repository.
---

# Volang Dev

Use this skill as the source-backed maintainer workflow for Volang. Start from
current source, route the task to the owning subsystem, choose focused
validation, and treat stale docs as an expected maintenance hazard.

## Core Protocol

1. Check the worktree and identify the affected paths.
2. Load [references/repo-map.md](references/repo-map.md) for routing when the
   owner is unclear.
3. Load [references/subsystem-playbooks.md](references/subsystem-playbooks.md)
   for common change recipes, and [references/risk-ledger.md](references/risk-ledger.md)
   when the task touches docs, implementation status, generated artifacts,
   modules, FFI, JIT, GC, Studio, or CI policy.
4. Inspect the current source around the relevant entry points.
5. Make the smallest change that matches local patterns.
6. Add or update tests when behavior changes.
7. Run the narrowest validation from [references/verification.md](references/verification.md),
   then widen only when the change crosses subsystem boundaries.
8. Re-read changed source and relevant caveats before finalizing.

## Source Truth Rules

- Prefer current Rust, Vo, Svelte, and TypeScript source over specs, README
  claims, generated mirrors, and dated design notes.
- Treat this skill and its references as the compact maintainer guide.
- Treat `eng/*.toml`, `cmd/vo-dev`, and `tests/lang/manifest.toml` as the
  authority for local and CI workflow behavior.
- Treat `lang/docs/spec/*.md` as canonical intended behavior for user-facing
  contracts, but verify source and tests before explaining current shipped
  implementation status.
- Treat `lang/docs/dev-notes/` and `lang/docs/outdated/` as historical context.
- Treat generated Playground docs as mirrors. Source docs live in
  `lang/docs/spec/` and `lang/docs/vo-for-gophers.md`.
- Do not copy old FFI examples using `#[vo_extern]`; current macros are
  `#[vo_fn]` and `#[vostd_fn]`.
- Do not hard-code test, CI, task, tool, artifact, or release policy in shell
  snippets or workflow YAML. Update `eng/*.toml` and `cmd/vo-dev` instead.
- Do not edit tracked generated artifacts by hand unless the task is explicitly
  about artifact policy. Use declared generators and provenance checks.
- Do not assume `vo test` and `./d.py test` are the same. Repo regression tests
  are manifest-driven through `vo-dev`.

## Reference Selection

- Repo routing and ownership: [references/repo-map.md](references/repo-map.md)
- Validation and command choice: [references/verification.md](references/verification.md)
- Common maintenance recipes: [references/subsystem-playbooks.md](references/subsystem-playbooks.md)
- Volatile caveats and stale-doc checks: [references/risk-ledger.md](references/risk-ledger.md)

Read only the references needed for the task. The repo already has broad
background in this skill; use references for decisions, caveats, and commands.

## Routing Heuristics

- Syntax changes usually cross `vo-syntax`, `vo-analysis`, `vo-codegen`, tests,
  and docs.
- Type rule changes usually cross checker diagnostics, `TypeInfo` consumers,
  selections, escape/sendability post-passes, codegen, and manifest cases.
- Codegen-only changes still often cross slot metadata, wrappers, extern return
  slot accounting, runtime metadata, debug info, VM/JIT behavior, and manifest
  cases.
- Opcode or bytecode changes usually cross `vo-common-core`, codegen, VM exec,
  bytecode text/serialization, function metadata, slot metadata, JIT or
  fallback behavior, runtime/JIT ABI helpers, and tests.
- JIT opcode behavior changes must route through the semantic row, metadata
  requirement, verifier domain, capability/runtime path policy, lowering owner,
  and contract graph together. Do not add parallel opcode-family match tables.
- GC-sensitive changes must audit slot metadata, VM root scanning, interface
  slots, JIT spill/materialization, defer/panic paths, and scheduler
  boundaries.
- Module behavior changes usually cross `vo-module`, `vo-engine`, `cmd/vo`,
  `vo-web`, Studio preparation, specs, and tests.
- Stdlib API changes usually need both `lang/stdlib` facade code and
  `vo-stdlib` extern registration.
- Studio GUI changes often touch `apps/studio/src`, `apps/studio/wasm`,
  `vo-web`, `vo-app-runtime`, renderer bridge ordering, and quickplay paths.
- Engineering workflow changes belong in `eng/*.toml` plus `cmd/vo-dev`; avoid
  duplicating policy in `d.py`, shell scripts, or GitHub YAML.
- Docs and generated docs changes usually cross `lang/docs/spec`,
  `scripts/ci/docs_sync.mjs`, generated Playground docs, Studio docs, and
  docs linting.
- Examples and benchmarks have manifests and lints; do not treat them as loose
  scratch files.

## Validation Defaults

Use `./d.py` from the repo root when possible. It delegates to `vo-dev` and
keeps command behavior centralized.

Common focused checks:

```sh
./d.py test both tests/lang/cases/foo.vo
./d.py test jit tests/lang/cases/foo.vo
./d.py test osr tests/lang/cases/foo.vo
./d.py test wasm tests/lang/cases/foo.vo
cargo run -q -p vo-dev -- test lint --suite lang
cargo run -q -p vo-dev -- task plan pr --changed
cargo run -q -p vo-dev -- verify plan pr
cargo run -q -p vo-dev -- lint all
cargo check --workspace --all-targets --exclude vo-playground
cargo check -p vo-web --target wasm32-unknown-unknown
./d.py ci task studio-build
./d.py ci task docs-lint
```

Use `VOWORK=off` for hermetic language-test expectations unless the task is
specifically about workspace overrides. Manifest-driven native/WASM test
targets in `eng/tests.toml` already set this.

## High-Risk Areas

- `vo.work` can redirect first-party dependencies to sibling repos.
- Frozen build commands should not re-solve dependencies or rewrite `vo.mod` /
  `vo.lock`; explicit `vo mod` lifecycle commands own graph mutation. They may
  still download already-locked cache artifacts or native real-path inline deps.
- Single-file inline module support differs between native and web paths when
  external `require` entries are involved.
- Native FFI docs may lag source macro names. Check `vo-ext` and
  `vo-ffi-macro` before writing examples.
- JIT status is nuanced. Verify current `vo-engine::run`, `vo-vm` dispatch,
  `vm/jit/*`, `jit_mgr`, `vo-jit`, `lang/docs/dev/jit-fact-source.md`, and
  language tests before claiming support or diagnosing failures.
- `vo-jit/src/semantics` is the compact opcode fact source. Keep capability,
  metadata requirements, register effects, runtime dependencies, verifier
  requirements, lowering owner, and fail-fast policy derived from that source.
- Use "VM call materialization" or "side exit" for intentional runtime paths;
  avoid reviving broad "fallback" wording for strict JIT failures.
- GC is non-moving incremental tri-color mark/sweep with precise slot scanning.
  Do not describe it as conservative or moving.
- `RenderBuffer` keeps only the latest render frame.
- Studio and Playground runtime paths are not identical.
- Bare `vo-web` single-file compile paths differ from Studio `prepareEntry`
  dependency preparation.
- Checked-in quickplay artifacts are governed by `eng/artifacts.toml` and
  provenance validation.
- Language regression coverage is large and manifest-driven; add both the `.vo`
  case and manifest metadata.

## Finalization Checklist

- Changed files match the subsystem that owns the behavior.
- Tests or manifest entries cover the new behavior or regression.
- Docs were updated only where they are source of truth for the audience.
- Generated artifacts were updated only through declared generators.
- Examples, benchmarks, and checked-in artifacts still satisfy their manifests
  and lints.
- Validation commands were run or clearly reported as not run.
- Any source-vs-doc disagreement was resolved in favor of source or explicitly
  called out for human follow-up.
