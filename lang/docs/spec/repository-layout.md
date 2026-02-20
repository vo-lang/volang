# Vo Repository Layout Specification

Version: 1.0  
Status: Draft

## 1. Scope

This document defines canonical GitHub repository layout, release tagging, and CI conventions for Vo modules.

This specification is complementary to `module.md`.

## 2. Single-Module Repository (Recommended)

A single published module should use:

```text
repo/
├── vo.mod
├── vo.sum
├── README.md
├── LICENSE
├── .gitignore
├── .github/
│   └── workflows/
│       ├── ci.yml
│       └── release.yml
├── cmd/
│   └── <app>/
│       └── main.vo
├── internal/
│   └── ...
├── pkg/
│   └── ...
├── examples/
│   └── ...
└── tests/
    └── ...
```

Rules:

- `vo.mod` and `vo.sum` must be committed.
- `.vodeps/`, `.vo-cache/`, build artifacts, and Rust `target/` outputs must not be committed.
- Public reusable packages should be placed under `pkg/`.
- Non-public implementation packages should be placed under `internal/`.
- Executable entry points should be placed under `cmd/<name>/`.

## 3. Native Extension Module Layout

A module that ships native extension code should use:

```text
repo/
├── vo.mod
├── vo.sum
├── vo.ext.toml
├── <vo packages>
├── rust/
│   ├── Cargo.toml
│   └── src/
│       └── lib.rs
└── examples/
```

Rules:

- `vo.ext.toml` must be in module root.
- Native dynamic library outputs (`*.so`, `*.dylib`, `*.dll`) are build products and must not be committed.
- Extension source and Vo declarations must stay versioned together in the same release tag.

## 4. Multi-Module Monorepo (Allowed)

A monorepo may host multiple modules:

```text
repo/
├── modules/
│   ├── core/
│   │   ├── vo.mod
│   │   └── ...
│   └── sdk/
│       ├── vo.mod
│       └── ...
└── .github/workflows/
```

Rules:

- Each module must have its own `vo.mod` and `vo.sum`.
- CI must validate each module independently.
- Shared tooling/scripts may live in repo root.

## 5. GitHub Release and Tagging Policy

- Every published module version must map to an immutable Git tag.
- Tags should follow `vX.Y.Z` for single-module repos.
- For monorepos, tags should include module name, e.g. `core/v1.2.3`.
- A release tag must contain `vo.mod`, source code, and extension metadata (if any).

## 6. CI Baseline

Minimum CI checks per module:

1. `./d.py check <module-root>`
2. `./d.py test both --release <module-or-tests>`

Additional required checks when `vo.ext.toml` is present:

3. `./d.py check --target=wasm <module-root>`

   This check verifies one of two outcomes:
   - **Pass**: all `extern func` in the module have `//go:build wasm` file counterparts with Vo implementations.
   - **Fail**: WASM build fails with a missing-implementation error. This is also acceptable — it means the module intentionally does not support WASM, and the CI result documents that fact.

   Either outcome is deterministic. The point is that CI runs the check so the result is explicit and not discovered by a downstream user.

Additional recommended checks:

- `vo.sum` checksum consistency (no modified dependencies without corresponding sum update)
- no forbidden committed directories (`.vodeps`, `.vo-cache`, `target/`)
- If `vo.ext.toml` is present: at least one native test that invokes an extern function to verify the dylib loads correctly

## 7. Publishing Checklist

Before tagging a release:

1. `vo.mod` and `vo.sum` are up-to-date and committed.
2. README states which targets are supported (native / wasm), verified by CI results, not by declaration.
3. If `vo.ext.toml` exists: confirm `native.path` resolves correctly on each target OS; build the dylib in CI before running tests.
4. All CI jobs pass on the release commit.
