# Volang UI delivery program

Volang UI has two deliberately separate records:

- `ui/roadmap.toml` is the frozen M0-M7 foundation record. It proves the
  existing renderer, protocol, VM/JIT, AOT, Web, desktop, accessibility,
  scheduler, tooling, and performance baseline.
- The completed 1.0 GA record is governed by `ui/product-roadmap.toml`,
  `ui/capabilities.toml`, and `ui/delivery.toml`.

Foundation certification remains useful evidence. Product maturity was earned
through explicit target, conformance, dogfood, hardening, and stability
evidence.

## Sources of truth

`product-roadmap.toml` defines the product boundary, required domains,
showcases, and release gates. It contains outcomes rather than implementation
checklists.

`capabilities.toml` owns every 1.0 capability. Each entry declares:

- owning domain and packages;
- delivery increment;
- maturity and API stability;
- required and optional execution targets;
- capability dependencies;
- acceptance and existing evidence.

`delivery.toml` owns E0-E8, the active increment, cross-cutting work streams,
and the six permanent contract probes. Capability dependencies may point only
to the same or an earlier delivery increment.

`certification.toml` owns the certified foundation gates.
`product-certification.toml` binds all product gates and showcases to one
candidate identity and immutable release evidence. `vo-dev ui-certify --check`
validates the complete contract.

## Maturity

Capabilities progress through one ordered scale:

1. `specified`: target behavior, ownership, dependencies, and acceptance exist.
2. `implemented`: the owning implementation and focused tests exist.
3. `conformant`: declared execution targets agree on the portable contract.
4. `dogfooded`: a required contract probe or showcase depends on the capability.
5. `hardened`: budgets, failures, accessibility, security, and platform suites pass.
6. `stable`: public compatibility and release evidence are ready for 1.0.

Public API stability is tracked independently as `internal`, `experimental`,
`preview`, `stable`, or `deprecated`. Internal protocol maturity therefore does
not freeze an application-facing API prematurely.

## Delivery increments

### E0 — Executable contracts and governed preparation

Freeze foundation history, validate the capability graph, and establish the
six contract probes. E0 is complete.

### E1 — Component Model V2

Replace the single-root optimization boundary with separately compiled
component artifacts and a persistent component instance graph. Instances own
state, lifecycle, effects, tasks, handlers, environment, reload schema, and
error isolation. Imported components participate in direct updates through
canonical package identity. The implementation contract lives in
`component-model-v2.md`. E1 is complete.

### E2 — Comfortable cross-platform foundation

Deliver inherited environment, typed callbacks, structured tasks/effects,
typed style, adaptive measurement, focus, overlays, semantics, localization,
and one renderer-neutral interaction contract. Web, desktop, and headless run
the same portable behavior with declared target variations. E2 is complete.

### E3 — UIKit Wave 1 and component gallery

Build headless behavior primitives and Volang visual recipes, then ship the
first governed content, form, feedback, overlay, and navigation controls. The
component gallery begins here and remains a permanent interaction laboratory.
E3 is complete.

### E4 — Application and data platform

Complete navigation, forms, commands, query/mutation/stream data flow,
persistence, virtual collections, tables, trees, grids, and application
patterns. The data application becomes permanent Web and desktop dogfood. E4
is complete.

### E5 — Complete Web and desktop products

Web adds server authority, SSG, streaming SSR, selective activation, actions,
caching, metadata, PWA policy, security, and deployment adapters. Desktop adds
multi-window lifecycle, shell services, packaging, signing policy, updates,
and rollback across macOS, Windows, and Linux. E5 is complete.

### E6 — Advanced official capability packs

Ship independently linkable graphics, asset, animation, chart, media, capture,
rich text, document, code editor, and workspace packages. The media application
and pure-Volang Studio workbench grow on public APIs. E6 is complete.

### E7 — Tooling, resilience, and ecosystem

Complete language tooling, preview, inspection, profiling, size analysis,
failure recovery, observability, security, module metadata, source extension,
authoring kits, documentation, and real-platform evidence. E7 is complete.

### E8 — Volang UI 1.0

Every required capability is stable, every showcase is complete, every product
gate has an executable contract, and the source product status is
`product-certified`. E8 is complete. Publication still requires the tagged
candidate, protected-main CI, and immutable release receipts defined by the
release policy.

## E0 contract probes

The probes start as executable specifications and become permanent regression
applications as their dependencies land:

1. `nested-component-list` covers nested instances, keys, package linking,
   structured state, disposal, and reload.
2. `asynchronous-search` covers scoped goroutines, debounce/latest policies,
   cancellation, stale-result isolation, and bounded commits.
3. `modal-validation-form` covers typed callbacks, forms, focus, overlays, IME,
   APG behavior, validation, and restoration.
4. `virtual-data-grid` covers large collections, selection, sorting, commands,
   semantics, memory, and frame budgets.
5. `ssr-activation-route` covers server/shared/client authority, useful streamed
   HTML, typed routes, and component activation.
6. `multi-window-editor` covers multiple roots, shared documents, independent
   renderer/focus/task scopes, commands, workspace layout, and the editor.

## Execution rules

- Ordinary application and component source stays in `.vo`; no template file
  language or application JavaScript graph is introduced.
- The public framework remains under top-level `ui/`. Language, VM, AOT, Web,
  and native packages depend inward through versioned adapters.
- Renderer-neutral core preserves `no_std + alloc`, generational identities,
  atomic commits, deterministic order, bounded queues, and one UI writer per
  root.
- A capability cannot advance past `implemented` without repository evidence.
- A capability cannot advance past `conformant` without every required target.
- A product gate cannot complete from prose; it needs executable commands and
  immutable evidence tied to the candidate commit.
