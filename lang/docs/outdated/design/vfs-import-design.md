# Archived: Pre-1.0 VFS Import Design

Status: Obsolete

This document predates the Module System 1.0 cutover and is no longer authoritative.

Do not use this file to implement, review, or validate the current module system.

## Authoritative References

- `lang/docs/spec/module.md`
- `lang/docs/spec/repository-layout.md`
- `lang/docs/dev-notes/2026-03-17-module-system-1.0-landing-plan.md`

## Why This File Was Archived

The old design described assumptions that are no longer part of the landed architecture, including:

- alias-based external dependency identity (`@"alias"` imports)
- alias-based `require` declarations in `vo.mod` (`require <alias> <module-path> <version>`)
- repository-relative package semantics as module identity
- `.vodeps` as the build or registry contract

The current module system uses canonical full-path imports, `vo.mod` + `vo.lock` as the authoritative manifest pair, and GitHub Releases with `vo.release.json` as the registry protocol.
