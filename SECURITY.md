# Security Policy

## Supported versions

Security fixes are applied to the latest released Volang version and the
current `main` branch. Pre-release snapshots receive fixes through the next
published snapshot.

## Reporting a vulnerability

Use GitHub's private **Report a vulnerability** form on the repository Security
tab. Include the affected commit or version, target platform, impact, a minimal
reproduction, and any suggested mitigation. Please keep exploit details out of
public issues until maintainers coordinate disclosure.

Maintainers will acknowledge a report within three business days, assess its
severity, and provide an update at least every seven days while remediation is
in progress. A coordinated advisory will credit reporters who want attribution.

## Security boundaries

Volang treats source, bytecode, Core Wasm images, extension artifacts, module
metadata, browser messages, UI artifacts, archives, and package registries as
untrusted inputs. Reports involving verifier bypasses, sandbox or capability
escapes, memory safety, package authenticity, release provenance, denial of
service, or cross-session data exposure are in scope.
