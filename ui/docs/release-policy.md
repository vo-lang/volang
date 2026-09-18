# UI release policy

The component API is preview. Source and internal protocol changes must update
all first-party applications, examples, generated codecs, documentation and
packaged toolchains in the same change. Deliver one coherent implementation;
removed APIs and products do not receive compatibility adapters.

A distributable toolchain contains matching compiler, UI sources, browser VM,
host modules and build tools. Desktop packages additionally require a matching
SDK. Verify package integrity, runtime identity and platform prerequisites before
execution. Build commands preserve module lockfiles.

`vo-dev ui-certify --check` validates acceptance declarations.
`vo-dev ui-certify --evidence <bundle>` verifies complete CI-bound evidence.
Certification requires Rust quality, Web acceptance and Linux, macOS and Windows
desktop acceptance at the same source commit. Local checks alone cannot establish
cross-platform certification. Public releases also require the repository's
release identity, protected-branch evidence and archive verification.

Website publication validates the exact staged static artifact. A minimal silent
cache-retirement worker may discard obsolete asset caches; user projects and
Playground drafts must never be deleted by that operation.
