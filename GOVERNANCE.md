# Volang Governance

## Maintainers

Repository maintainers own technical direction, review, release authority,
security response, and project policy. CODEOWNERS identifies the current review
owner for each major area. Ownership can be expanded through sustained,
constructive contributions and maintainer consensus.

## Decisions

Routine changes use pull-request review and the required `CI / required` check.
Changes to language semantics, stable APIs, artifact formats, trust boundaries,
or repository ownership require a design record in the relevant documentation
area and explicit maintainer approval.

When consensus is unavailable, maintainers document the alternatives, user
impact, compatibility cost, and selected direction before merging. Security
response may proceed privately and be documented after coordinated disclosure.

## Protected operations

The `main` branch, release tags, GitHub Pages deployment, and GitHub Release
publication are protected operations. Repository settings must require reviewed
pull requests, current required checks, conversation resolution, linear history,
and blocked force pushes and deletions. Release publication additionally uses
the protected `release` environment, immutable releases, and build-provenance
attestation.

CI produces an immutable plan from `eng/ci.toml`. Jobs emit source-bound task
receipts, the required job certifies complete coverage, and deployment promotes
the exact tested bytes. Workflow success without that certification bundle does
not grant product or release status.
