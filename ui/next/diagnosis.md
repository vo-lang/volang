# Check and diagnose a project

Run these commands inside a project containing `ui-next.json`:

```sh
vo ui doctor
vo ui check
vo ui build
```

All three accept a project directory or `--project <directory>`. Existing projects
without `ui-next.json` retain the previous `doctor` command. An invalid new-project
manifest produces a new-project diagnostic and never selects the previous runtime.

`doctor` checks the selected compiler, packaged installation inventory, project
configuration, vendored framework wire version, document markers, Web VM files
and browser test runner. It reports all independent failures with repair guidance.
It does not install tools, rewrite files, compile sources or execute application
code. Browser engines remain a separate installation: `vo ui browsers install`.

```sh
vo ui doctor --target desktop
vo ui doctor --json
```

The desktop target checks the declared entry, document and native SDK inventory.
Linker availability is exercised by `vo ui package`; system WebView and actual
window behavior are exercised by `vo ui run`. Successful diagnosis only covers
the listed configuration and installation checks. The JSON report has schema
`volang.ui-project-diagnosis.v1`, explicit check statuses and a `passed` flag;
a failed check also sets a nonzero process exit status.

`check` validates the production/development document, optional packs, static
page paths, formatting and types for every declared source entry, including
server and explicit desktop entries. It validates host imports by bundling in
memory. It does not run prerender code, create a distribution, or replace an
existing build. It uses the compiler's `--read-only` mode: dependencies and any
generated source must already be available, and checks do not write compilation
caches or invoke generators. Successful compiler diagnostics remain visible.

Use `build` to execute prerendering and create a deployable distribution, then
`preview` to view it. Use `test` for source checks, production build and actual
browser interactions. Builds publish only after all required stages succeed;
a failed build preserves the previous complete output.
