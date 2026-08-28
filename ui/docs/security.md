# Security model

Volang UI follows least authority. Portable application packages receive no
ambient browser, filesystem, process, network, clipboard, window or media
authority. Typed platform hosts expose a requested capability with permission,
support, quota, context cancellation and lifetime.

## Trust boundaries

- module source and artifacts are digest checked through `vo.lock`;
- compiler package provenance is embedded in a versioned sidecar;
- Web AOT rejects server-authority packages before emission;
- binary UI, event, system, accessibility, paint and browser frames validate
  magic, version, bounds, identities, numbers and trailing data;
- paths, URLs, origins, headers, asset programs, state payloads and queues are
  bounded before host mutation;
- desktop packages bind runtime, application, target and update receipts;
- release tags must match a successful protected-main CI commit and pass an
  independently reviewed release environment.

Application code should mark secret-bearing telemetry attributes sensitive,
keep tokens out of view state and URLs, request media or file permissions only
after a user action, and close scoped resources during component disposal.
Recovered callback panics enter a documented degraded state; integrity,
provenance and protocol failures remain fatal at their trust boundary.

Report suspected vulnerabilities privately through the repository security
contact configured for the release. Include the affected version, target,
minimal reproduction and impact. Do not place credentials or personal data in
an issue or diagnostic bundle.
