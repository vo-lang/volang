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
├── vo.lock
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

- `vo.mod` and `vo.lock` must be committed.
- `.vodeps/`, `.vo-cache/`, build artifacts, and Rust `target/` outputs must not be committed.
- Public reusable packages should be placed under `pkg/`.
- Non-public implementation packages should be placed under `internal/`.
- Executable entry points should be placed under `cmd/<name>/`.

## 3. Native Extension Module Layout

A module that ships native extension code should use:

```text
repo/
├── vo.mod
├── vo.lock
├── <vo packages>
├── rust/
│   ├── Cargo.toml
│   └── src/
│       └── lib.rs
└── examples/
```

Rules:

- Extension metadata must be declared in the module root `vo.mod`.
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

- Each module must have its own `vo.mod` and `vo.lock`.
- CI must validate each module independently.
- Shared tooling/scripts may live in repo root.

## 5. GitHub Release and Tagging Policy

- Every published module version must map to an immutable Git tag.
- Tags should follow `vX.Y.Z` for single-module repos.
- For monorepos, tags should include module name, e.g. `core/v1.2.3`.
- A release must publish a machine-readable `vo.release.json` asset.
- A release must publish a canonical source-package asset for the module version.
- If `vo.mod` declares target-specific artifacts, every required artifact implied by that declared target-support contract must be published as a release asset and listed in `vo.release.json`.

## 6. CI Baseline

Minimum CI checks per module:

1. `./d.py check <module-root>`
2. `./d.py test both --release <module-or-tests>`

Additional required checks when extension metadata is present in `vo.mod`:

3. If `[extension.wasm]` is declared: `./d.py check --target=wasm <module-root>`.

   A declared WASM target is an explicit support claim. This check is expected to pass.

4. If `[[extension.native.targets]]` entries are declared: CI should build or otherwise verify the native extension artifact for each declared target before release staging.

Additional recommended checks:

- `vo.lock` integrity consistency (no dependency or artifact drift without corresponding lock update)
- no forbidden committed directories (`.vodeps`, `.vo-cache`, `target/`)
- If `vo.mod` declares native targets: at least one native test on each covered host platform should invoke an extern function to verify the shared library loads correctly

## 7. Publishing Checklist

Before tagging a release:

1. `vo.mod` and `vo.lock` are up-to-date and committed.
2. If `vo.mod` declares extension metadata: its declared target-support set is authoritative for published extension support. README text may summarize support, but must not contradict the manifest.
3. If `vo.mod` declares extension artifacts: confirm every declared `[[extension.native.targets]]` and `[extension.wasm]` artifact is built or staged with the exact asset names declared in the manifest and recorded in `vo.release.json`.
4. Extension metadata must use the canonical schema from `spec/native-ffi.md` and `spec/module.md`.
5. All CI jobs pass on the release commit.
