# Desktop application distribution

This crate composes the independent `vo-ui-webview`, `vo-ui-native`, VM and Native
AOT providers into standalone application entries. It owns resource receipts,
application layout, common/target bytecode admission and process arguments.
Component state, rendering, IPC and platform scheduling remain with their owners.

The default feature set validates resource manifests without window libraries.
`runner` enables the portable bytecode launcher, `jit` adds native compilation,
and `aot` exports `vo_aot_start` for a statically linked application. The Native
AOT dependency graph excludes the compiler and previous UI kernel. Cargo builds
are serialized by the desktop SDK producer; applications consume prebuilt SDK
artifacts and never build these dependencies themselves.

Resource paths resolve relative to the executable, with macOS application bundle
layout handled explicitly. The loader bounds and hashes files before window
creation; the bytecode runner then performs common and host target verification.
The static image loader retains its existing Native AOT admission checks.
Ordinary window closure succeeds, application exit codes propagate, and failures
return nonzero. `--check`, `--diagnostics` and `--exit-on-failure` support independent
bundle checks and automated system-window acceptance.

See [desktop project tools](../../../../ui/next/desktop.md) and the contract driver
`eng/ui-next/desktop-package-contracts.mjs`. Resource/argument unit tests and real
WebView receipts cover different boundaries; neither grants physical input,
paint, IME or assistive technology certification.
