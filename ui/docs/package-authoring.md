# UI package authoring

An official or third-party UI package is ordinary Volang source with an
explicit module capability contract. It does not run installation scripts.

## Authoring checklist

1. Put renderer-neutral public behavior in `.vo` packages.
2. Keep host access behind a small context-aware interface and declare its
   authority, effects, lifetime, quota and platform support.
3. Bound every decoded collection, byte payload, queue and recursive shape.
4. Publish goroutine results through a generation or immutable input version.
5. Preserve semantic output when a visual renderer is unavailable.
6. Add VM/JIT, Core Wasm AOT and Native AOT evidence for declared targets.
7. Declare module capabilities and profiles; request only the set the package
   imports.
8. Record license, maintenance, security contact, provenance and compatibility
   in release metadata.

To start from an official recipe, list the maintained source surfaces and
export one into an empty destination:

```sh
vo ui source --list
vo ui source kit/components -o components.vo
```

The command refuses to replace existing files and emits
`components.vo.provenance.toml`. Keep that receipt so upgrade tooling can
compare the original module version and digest with later releases.

## Dependency profiles

`ui/module-profiles.toml` is the governed source catalog for the upcoming
`minimal`, `application`, `web`, `studio`, and `full` release profiles. The
bundled source module currently links only imported packages. A registry
release may advertise a profile after its exact source and target artifacts
have been materialized and signed for that capability set.

The lockfile records any resolved capability set and exact source or artifact
provenance. The compiler records canonical linked packages in
`volang.compile.packages`; inspectors and target verifiers consume that same
sidecar.

## Compatibility

Stable packages follow semantic source compatibility. Versioned wire formats
retain magic, version, bounds and unknown-version rejection. Experimental
packages include migration notes when their contract changes. Deprecations name
a replacement and remain through the documented compatibility window.
