# Testing applications

New starters include a small browser test and fixtures. From an application,
use the matching [UI toolchain](toolchain.md):

```sh
vo ui test --project .
```

The command checks Vo types/format, builds production Wasm VM
artifacts, starts an owned static or request-time server and runs `tests/browser/*.test.mjs`
through the standard Playwright test runner. Defaults are Chromium, Firefox
and WebKit, each with Wasm VM, using one worker and fresh test contexts.
`UI_NEXT_BROWSER=chromium` selects one engine for a focused run. The package
includes the test runner; install its engines with `vo ui browsers install`
or provide a matching `PLAYWRIGHT_BROWSERS_PATH`. The test command does not
install dependencies. Checkout developers can also run
`node eng/ui-next/cli.mjs test --project /path/to/application`.

```js
import { test, expect } from './fixtures.mjs';

test('a name and a little interaction', async ({ page, appURL }) => {
  await page.goto(appURL);
  await page.getByRole('textbox', { name: 'What should we call you?' }).fill('Ada');
  await expect(page.getByRole('heading', {
    name: 'Make something good, Ada.',
  })).toBeVisible();
  await page.getByRole('button', { name: 'Make it happen' }).click();
  await expect(page.getByRole('status')).toHaveText('1 little steps');
});
```

`page` and `expect` are ordinary Playwright APIs. `appURL` points to the owned
production server with the selected backend query parameter. `backend` is
`vm`. Standard locator assertions wait for observable results; a click
returning does not guarantee that asynchronous guest rendering has committed.
The starter delays application loading and edits server HTML before activation.
Its input handler belongs to the Field wrapper; activation must retain the same
input node and deliver the early value to that wrapper on the Web VM.
Tests use labels, roles and visible output and do not read framework node IDs,
private application globals or internal readiness handles.

Use browser routing and clock APIs for browser-owned I/O and time. Application
services and pure Vo state logic can be tested independently through their own
public interfaces. Guest clocks, real input methods and assistive technologies
need their corresponding runtime/device checks; this browser entry does not
substitute for those contracts or provide a separate headless DOM model.

Uncaught page errors fail the test automatically. Assertions and runner errors
make the command fail, and focused `test.only` declarations are rejected.
Every executed run gets its own `target/ui-next/browser-tests/run-*` directory
containing the production build manifest, runner configuration and JSON report.
Failed tests retain screenshots and Playwright traces. The command prints the
artifact location, including after failure; later runs preserve earlier results.
The temporary server closes when testing finishes or is cancelled.

The generated `fixtures.mjs` loads the experimental fixture module selected by
the current runner. The private `VO_UI_TESTING_MODULE` value is set by the test
command; applications do not configure it. Moving the project or tools does not
require rewriting a source path. The application keeps its framework snapshot in `vendor/ui`.
Test fixtures, the browser runner and test cases are excluded from production
web assets. Release publication and product certification remain separate from
the portable toolchain preview.
