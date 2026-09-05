# Browser contracts

`eng/run-browser-smoke.mjs` runs one declared scenario against a built Volang
artifact. `scenario.spec.ts` owns fixture cleanup, browser diagnostics and the
structured result; `page-contract.mjs` owns Playwright input and waiting;
`scenarios/` owns product assertions. `coverage.json` maps all eight former CDP
scenarios and every original checkpoint to their replacements, with added
regressions listed separately. The additional Studio canary covers a fresh local
project through starter, edit, VM run, preview interaction, save, reload, reopen and refresh the reopened workspace.
The startup regression delays the first project activation through the test
host-service wrapper, checks the loading state, then releases activation and
requires the first starter click to open its editor.
The Nightly lifecycle journey repeats three independent run/cancel cycles,
requires each dedicated run worker to close, recovers from a compiler error,
then reloads, edits, previews and saves while offline before recovering online.
It uses the real service worker and checks persisted source after reconnecting.

Install with `npm ci` in this directory, then
`PLAYWRIGHT_BROWSERS_PATH=../../target/playwright-browsers npm run install:chromium`.
CI uses `.github/actions/setup-browser` to install the same lockfile and browser
revision, including Linux libraries. No system Chrome discovery is used.

From the repository root, after building the gallery:

```sh
node eng/run-browser-smoke.mjs --static-root target/ui-component-gallery-aot \
  --uikit-gallery-smoke --output target/ci/results/ui/gallery.json
```

Each invocation has an isolated `target/ci/browser/<scenario>/<attempt>` directory.
Failures retain the first-attempt trace, screenshot, console/network diagnostics,
HTML report, JSON report, and domain result. Retries are disabled. Blob reports
are enabled when `PLAYWRIGHT_BLOB_OUTPUT_FILE` is supplied for a sharded run.
The compatibility entry point invalidates an old result before launching, and a
browser startup failure cannot reuse a previous success.

The `--studio-aot-smoke --base-url` option exercises a complete Studio test
instance. It includes account and sharing contracts; deploy canaries must use a
separate isolated, account-free journey:

```sh
node eng/run-browser-smoke.mjs --base-url https://example.com/studio/ \
  --studio-canary-smoke --output target/ci/results/site-canary.json
```

The canary can also use `--static-root` to check the final candidate before
promotion. Playwright supplies an empty browser context; all project changes
stay in its local store.

To check reporting, pass a JSON map from all `coverage.json` flags to built
artifact directories (`ui-conformance` maps to `lang/crates/vo-web`):

```sh
node eng/browser/check-diagnostics.mjs /path/to/artifacts.json
```

This injects one explicit assertion failure after each page becomes interactive
and requires a failed process, failed domain report, screenshot and trace. It
produces its own proof report and never changes product assertions or retry
policy. Normal scenario runs preserve the complete product journey.
