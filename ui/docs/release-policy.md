# Volang UI release and compatibility policy

Volang UI ships with the Volang toolchain. The pure-Volang modules, compiler
adapter, VM/JIT providers, native runtime, browser runtime, official packages,
templates, and CLI form one authenticated release set. Build outputs link only
the capabilities an application uses.

## Foundation and product status

`ui/roadmap.toml` is the frozen M0-M7 foundation record. A publishable
1.0 release must pass `vo-dev ui-certify --check`, every declared product gate,
and every required CI job from one tagged candidate commit.

The Volang UI 1.0 product status is derived from:

- `ui/product-roadmap.toml` for required domains, showcases, and gates;
- `ui/capabilities.toml` for capability maturity, API stability, target
  support, dependencies, ownership, and evidence;
- `ui/delivery.toml` for E0-E8 and permanent contract probes;
- `ui/certification.toml` and CI for executable release evidence.

Product 1.0 requires every required capability to reach `stable`, E8 to reach
`complete`, every required showcase to complete, and every product gate to pass
from the same candidate commit.

## Capability and API stability

Capability maturity progresses through `specified`, `implemented`,
`conformant`, `dogfooded`, `hardened`, and `stable`. Advancement beyond
specification requires repository evidence. Conformance requires every declared
target; hardening requires applicable performance, failure, accessibility,
security, and real-platform evidence.

Application-facing APIs use an independent stability label:

- `internal` has no source compatibility promise;
- `experimental` may change as contract probes expose design problems;
- `preview` carries migration notes and cannot change silently;
- `stable` follows semantic compatibility and deprecation policy;
- `deprecated` remains available with an automated or documented replacement.

Internal component plans and artifacts may evolve behind the stable public
source API. A format becomes a public binary compatibility promise only after
its release policy explicitly marks it stable.

## Semantic versions

- Patch releases preserve every stable source, package, wire, artifact, and
  behavior contract while fixing defects.
- Minor releases add compatible capabilities and may revise experimental APIs
  with diagnostics and migration notes.
- Major releases may remove completed deprecations or revise stable contracts.
- Stable public deprecations remain for at least one minor release and identify
  their replacement in diagnostics and release notes.

The module solver and `vo.lock` authenticate exact source and artifact bytes.
Builds do not substitute another official UI package after lock validation.

## Versioned internal formats

| Identity | Current purpose | 1.0 status |
| --- | --- | --- |
| VUP1 | compiler-neutral single-component plan | internal |
| VUA1 | deployable single-component artifact | internal |
| VUB1 | bounded multi-component bundle and import requirements | internal |
| VUI1 | atomic renderer mutation frame | internal, certified |
| VUE1 | renderer-to-Volang event frame | internal, certified |
| VUS1 | platform-service request and response | internal, certified |
| VAX1 | accessibility conformance snapshot | internal evidence |
| VPX1 | logical paint conformance snapshot | internal evidence |
| VWX1 | browser mapping conformance snapshot | internal evidence |

Every binary decoder rejects unknown magic, trailing data, invalid identities,
oversized collections, and incomplete frames. A new stable binary contract must
declare compatibility, negotiation, bounds, ownership, downgrade, and migration
rules explicitly.

## Package distribution

Official UI packages use the existing Volang module registry, lockfile, cache,
and release pipeline. UI-specific metadata declares platform, backend,
capability, ABI, accessibility, provenance, license, maintenance, and support.
Untrusted package installation scripts are outside the UI package model.

Source packages compile with the application. A precompiled component package
must carry an authenticated component bundle, canonical package
identity, required runtime capabilities, and a compatible stable component ABI.
The generic source path remains available when a precompiled optimization is
unavailable.

## Publication authority

The release workflow rebuilds all required targets, verifies VM/JIT startup,
links Native AOT applications, builds Web AOT and server artifacts, creates a
fresh starter, executes contract probes and showcases, checks receipts and
provenance, and publishes only through an approved protected environment.

Release receipts bind the CLI, language runtime, UI runtime, official modules,
browser adapter, native libraries, templates, and target artifacts by size and
SHA-256. Publication is serialized and retries remain idempotent.
