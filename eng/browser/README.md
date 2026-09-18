# Browser tests

This directory provides the pinned Playwright dependency used by `eng/ui-next`.
The UI and Studio acceptance drivers live in that directory and run against
built applications with explicit ownership of servers, pages and workers.

`eng/run-browser-smoke.mjs` is a generic probe for independent project fixtures.
It waits for a named browser global to report `{complete:true, passed:true}` and
preserves the result, screenshot and trace through Playwright.

```sh
node eng/run-browser-smoke.mjs --project path/to/project --global __browserSmoke
```

The probe also supports `--static-root` or `--base-url` with `--global`. Use the
UI acceptance drivers for framework and Studio behavior.
