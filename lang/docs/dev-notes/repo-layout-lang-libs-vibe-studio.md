# Repo Layout: lang / libs / vibe-studio (Single Cargo Workspace)

## Goals

- Keep the **language core** isolated and authoritative.
- Provide a place for **non-stdlib libraries** (third-party wrappers and first-party libs that should not live in `stdlib`).
- Provide a top-level **IDE/product** crate named `vibe-studio`.
- Use a **single Cargo workspace** for simplicity (tooling/IDE/CI), while enforcing a strict dependency direction by policy.

## Top-level Structure

```
repo-root/
  lang/                      # Language implementation (the only source of truth)
    crates/                  # vo-* crates: compiler/vm/jit/runtime/cli/...
    stdlib/                  # Language standard library only
    test_data/               # Language integration tests
    docs/                    # specs + dev notes
    d.py                     # Language engineering entrypoint (test/bench/loc)
    benchmark/               # Language benchmarks (optional but recommended)

  libs/                      # Non-stdlib libraries
    <lib-name>/
      vo/                    # Vo package surface
        vo.mod
        *.vo
      rust/                  # Rust glue/runtime crates used by the Vo package
        <crate-name>/
          Cargo.toml
          src/

  vibe-studio/               # IDE / product crate (Rust)
    Cargo.toml
    src/

  Cargo.toml                 # Single workspace root
  Cargo.lock
  README.md
  LICENSE
```

### Directory Responsibilities

- `lang/`
  - Everything required to build and validate the Vo language implementation.
  - Must not depend on `libs/` or `vibe-studio/`.

- `libs/`
  - A collection of libraries that are not part of the language standard library.
  - Each library is a **capability bundle**: a Vo package (`vo/`) plus optional Rust support crates (`rust/`).

- `vibe-studio/`
  - The top-level product crate.
  - May depend on `lang/` crates and `libs/**/rust/*` crates.
  - Uses `libs/**/vo/*` at runtime (load/resolve/import), not as a Rust dependency.

## Cargo Workspace (Single Workspace)

The repository root is the only Cargo workspace. It includes:

- `lang/crates/*`
- `libs/**/rust/*`
- `vibe-studio`

Example (root `Cargo.toml`):

```toml
[workspace]
resolver = "2"
members = [
  "lang/crates/*",
  "libs/**/rust/*",
  "vibe-studio",
]
```

### Dependency Direction Policy

Hard rule:

- `lang/crates/*` MUST NOT add Cargo dependencies on:
  - `libs/**/rust/*`
  - `vibe-studio`

Allowed:

- `libs/**/rust/*` -> `lang/crates/*`
- `vibe-studio` -> `lang/crates/*` and `libs/**/rust/*`

This is enforced by review/CI scripts (to be added later if needed).

## Vo Module / Package Conventions (libs)

- Each library under `libs/<lib-name>/vo/` is an independent Vo module with its own `vo.mod`.
- `stdlib` is not copied into `libs`; libs import std via the normal resolver.
- Example library naming:
  - `libs/ui-egui/vo` provides the Vo-facing UI API.
  - `libs/ui-egui/rust/vo-egui-runtime` provides the Rust runtime for desktop/web rendering.

## Egui Wrapper Placement

- Vo package:
  - `libs/ui-egui/vo` (module + public API)
- Rust runtime:
  - `libs/ui-egui/rust/vo-egui-runtime` (egui + wgpu + platform runners + extern surface)
- Product integration:
  - `vibe-studio` consumes the above.

## Migration Checklist

1. Create directories:
   - `lang/`, `libs/`, `vibe-studio/`
2. Move language assets into `lang/`:
   - `crates/` -> `lang/crates/`
   - `stdlib/` -> `lang/stdlib/`
   - `test_data/` -> `lang/test_data/`
   - `docs/` -> `lang/docs/`
   - `d.py` -> `lang/d.py`
   - `benchmark/` -> `lang/benchmark/` (recommended)
3. Update root `Cargo.toml` workspace members to the new paths.
4. Update `lang/d.py` paths (`PROJECT_ROOT`, `TEST_DIR`, `CRATES_DIR`, etc.).
5. Add library skeleton:
   - `libs/ui-egui/vo` with `vo.mod`
   - `libs/ui-egui/rust/vo-egui-runtime` (Cargo crate)
6. Add `vibe-studio` crate skeleton.
7. Verify language toolchain:
   - `./lang/d.py test`

