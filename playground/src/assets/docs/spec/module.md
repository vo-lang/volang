# Vo Module Specification

Version: 1.0  
Status: Redirect

This playground copy does not maintain an independent module-system specification.

The authoritative specification lives at `lang/docs/spec/module.md`.

## Current Module System Summary

The landed Module System 1.0 architecture is defined by the following rules:

- Canonical full-path imports are the only non-stdlib import identity.
- `vo.mod` records direct dependency intent and the root toolchain constraint.
- `vo.lock` records the exact resolved graph, immutable release identity, and verified digests.
- `vo.work` is local-only workspace state and does not change published module identity.
- GitHub Releases, `vo.release.json`, canonical source packages, and optional target artifacts define the registry protocol.
- `vo build`, `vo check`, `vo test`, and `vo run` are frozen consumers of already-resolved state and do not silently access the network.

## Removed Legacy Concepts

The current system does not use:

- alias-based external dependency identity
- alias-based `require` declarations in `vo.mod`
- `.vodeps` as the registry or build contract
- `vo.sum` as the build contract
- published `replace` directives in `vo.mod`
- WASM-only `files(...)` publication semantics

Read `lang/docs/spec/module.md` for the full normative behavior.
