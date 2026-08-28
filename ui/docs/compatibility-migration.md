# Compatibility and migration

The 1.0 contract covers stable `.vo` APIs, package paths, documented behavior,
module metadata and stable wire formats. Patch releases preserve these
contracts. Additive minor releases may introduce new packages and capabilities.
Stable removals require deprecation, a named replacement and the compatibility
window in the release policy.

## Upgrade procedure

1. update the Volang toolchain and UI module together;
2. run `vo mod update`, inspect the lock diff, then `vo mod verify`;
3. run `vo ui doctor` and both VM/JIT semantic tests;
4. inspect Web authority and target sizes;
5. build Web and desktop release artifacts;
6. run the product-specific browser/native and accessibility suites.

Customized UIKit source keeps a `.provenance.toml` receipt. Compare its module
version and SHA-256 with `vo ui source` from the new toolchain, then merge the
upstream change explicitly. Export never overwrites the customized file.

Experimental and preview APIs retain their stability label in
`ui/capabilities.toml`. A release that changes one includes a migration note.
Internal protocols can evolve with the toolchain as an atomic release set;
independently consumed stable protocols require negotiation and downgrade rules.

For applications arriving from JavaScript, follow
[migration from JavaScript](migration-from-js.md). Existing obsolete Vogui and
Studio frontends have no compatibility role in the new framework.
