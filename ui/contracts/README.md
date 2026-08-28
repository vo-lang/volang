# Volang UI contract applications

This directory is reserved for the permanent executable product probes declared
in `ui/delivery.toml`:

- `nested-component-list`
- `asynchronous-search`
- `modal-validation-form`
- `virtual-data-grid`
- `ssr-activation-route`
- `multi-window-editor`

Each probe will contain ordinary `.vo` application source, a locked module,
headless fixtures, target interaction scripts, performance inputs, failure
cases, and expected semantic evidence. The delivery manifest owns identity,
capability coverage, target coverage, and acceptance; probe directories own
executable implementation only.

Probe code must use public or explicitly experimental application APIs. It may
not reach into renderer, VM, compiler, DOM, native-window, or test-only private
state. Tests drive the same event, service, component, and renderer contracts as
production hosts.
