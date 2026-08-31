# Volang UI 1.0 release notes

Volang UI 1.0 establishes a pure-Volang Web and desktop application stack with
VM/JIT development and Wasm/Native AOT release.

The GA contract completes E0-E8, 57 stable required capabilities, 63 governed
UIKit components with no open baseline gap, five showcases, and twelve product
gates. Release provenance binds the governance digest and
`product-certified` status to the tagged candidate commit.

Highlights include separately compiled keyed components, structured state and
goroutines, an official accessible UIKit, navigation/forms/commands/resources,
server rendering and selective activation, native windows and packaging,
graphics/media/document/editor/workspace packs, deterministic testing,
observability, inspection, diagnosis, templates and provenance-preserving
source extension.

Applications carry no npm dependency graph or JavaScript component runtime.
The small browser adapter projects versioned mutation and system protocols to
the DOM and Web APIs. Desktop hosts project the same logical tree to retained
layout/paint, WGPU, native text, AccessKit and system services.

The obsolete Vogui and JavaScript Studio implementation has been removed. New
Studio work begins from `vo ui new --template=studio` and the official editor,
language and workspace packages.

See [getting started](getting-started.md), the [authoring guide](authoring-guide.md),
[compatibility and migration](compatibility-migration.md), and the
[release policy](release-policy.md).
