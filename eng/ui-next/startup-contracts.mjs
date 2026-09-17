import assert from 'node:assert/strict';

/** Exercise the real guest while its artifact or runtime is still unavailable. */
export async function checkStartup(page, url) {
  await page.route('**/__startup-contracts', route => route.fulfill({contentType: 'text/html', body: '<!doctype html><body></body>'}));
  await page.goto(url + '/__startup-contracts');
  await page.evaluate(async () => {
    window.startupMount = (await import('/host/ui_next/mount.js')).mountUi;
    window.startupHtml = await (await fetch('/artifacts/interaction.ssr.html')).text();
    window.startupRoot = document.body.appendChild(document.createElement('div'));
    window.startupFailures = [];
  });
  const cases = [];
  for (const backend of ['vm']) {
    for (const outcome of ['success', 'retry', 'cancel', 'runtime-load', 'runtime-init']) {
      let requested;
      const artifactRequest = new Promise(resolve => { requested = resolve; });
      const path = '**/__startup-artifact';
      await page.route(path, requested);
      try {
        await page.evaluate(({backend, outcome}) => {
          window.startupRoot.innerHTML = window.startupHtml;
          window.startupSnapshot = window.startupRoot.innerHTML;
          window.startupCounter = window.startupRoot.querySelector('[data-counter=Alpha]');
          window.startupOptions = {backend, artifact: '/__startup-artifact', hydrate: true,
            loadVm: () => import('/wasm/vo_web.js')};
          window.startupRuntimeReached = false;
          if (outcome.startsWith('runtime-')) {
            const pending = new Promise(resolve => {window.releaseStartupRuntime = resolve;});
            window.startupOptions.loadVm = async () => {
              if (outcome === 'runtime-load') {window.startupRuntimeReached = true; await pending;}
              const wasm = await import('/wasm/vo_web.js');
              if (outcome === 'runtime-load') return wasm;
              return {...wasm, default: async () => {window.startupRuntimeReached = true; await pending; return wasm.default();}};
            };
          }
          window.startupApp = window.startupMount(window.startupRoot, window.startupOptions);
          window.startupApp.done.catch(error => window.startupFailures.push(String(error)));
        }, {backend, outcome});
        const request = await artifactRequest;
        const releaseArtifact = async () => request.fulfill({response: await page.request.get(url + `/artifacts/interaction.vob`)});
        if (outcome.startsWith('runtime-')) {
          // Runtime loading and initialization must begin while the artifact
          // response is still held, so network delays do not serialize startup.
          await page.waitForFunction(() => window.startupRuntimeReached);
          await releaseArtifact();
        }
        assert.equal(await page.evaluate(async () => {
          const duplicate = window.startupMount(window.startupRoot, window.startupOptions);
          if (await duplicate.ready) throw new Error('Duplicate root became ready');
          try {await duplicate.done; return false;}
          catch (error) {return error.message.includes('already has a mounted application');}
          finally {duplicate.close();}
        }), true, 'A second application acquired the pending root');
        const root = page.locator('body > div');
        await root.locator('[data-counter=Alpha]').click({clickCount: 2});
        await root.locator('#name').fill('Before ready 中文 😀');
        await root.locator('#controlled-select').selectOption('b');
        // Observe native cancellation without navigating away on the old host.
        const native = await page.evaluate(() => {
          const dispatch = (target, event) => {
            let prevented;
            target.addEventListener(event.type, event => {prevented = event.defaultPrevented; event.preventDefault();}, {once: true});
            target.dispatchEvent(event);
            return prevented;
          };
          const form = window.startupRoot.querySelector('form');
          const submit = new Event('submit', {bubbles: true, cancelable: true});
          const link = window.startupRoot.querySelector('[data-prevented-link]');
          const click = new MouseEvent('click', {bubbles: true, cancelable: true});
          return {submit: dispatch(form, submit), link: dispatch(link, click)};
        });
        if (outcome === 'cancel') {
          await page.evaluate(async () => {
            window.startupApp.close(); window.startupApp.close();
            if (await window.startupApp.ready) throw new Error('Cancelled startup became ready');
          });
          await request.abort();
          await page.evaluate(() => window.startupApp.done);
          assert.equal(await root.evaluate(element => element.childNodes.length), 0);
          // Late disposal of the cancelled application must leave its successor.
          await page.evaluate(() => {
            window.cancelledStartup = window.startupApp;
            window.startupApp = window.startupMount(window.startupRoot, {...window.startupOptions,
              artifact: `/artifacts/interaction.vob`, hydrate: false});
          });
        } else if (outcome === 'retry') {
          await request.fulfill({status: 503, body: 'Try again'});
          assert.equal(await page.evaluate(() => window.startupApp.ready), false);
          await page.waitForFunction(() => window.startupFailures.length === 1);
          assert.match((await page.evaluate(() => window.startupFailures))[0], /503/);
          assert.equal(await root.innerHTML(), await page.evaluate(() => window.startupSnapshot), 'Failed startup changed SSR markers or reset defaults');
          assert.equal(await root.locator('#name').inputValue(), 'Before ready 中文 😀');
          assert.equal(await root.locator('#controlled-select').inputValue(), 'b');
          assert.equal(await page.evaluate(() => {
            const event = new Event('submit', {bubbles: true, cancelable: true});
            const form = window.startupRoot.querySelector('form');
            let prevented;
            form.addEventListener('submit', event => {prevented = event.defaultPrevented; event.preventDefault();}, {once: true});
            form.dispatchEvent(event);
            return prevented;
          }), false, 'Failed startup retained event listeners');
          await page.evaluate(() => {
            window.cancelledStartup = window.startupApp;
            window.startupApp = window.startupMount(window.startupRoot, {...window.startupOptions,
              artifact: `/artifacts/interaction.vob`});
          });
        } else {
          if (outcome.startsWith('runtime-')) await page.evaluate(() => window.releaseStartupRuntime());
          else await releaseArtifact();
        }
        const ready = await page.evaluate(() => window.startupApp.ready);
        if (!ready) {
          const error = await page.evaluate(async () => {try {await window.startupApp.done; return 'Guest exited';} catch (error) {return String(error);}});
          assert.fail(`${backend} ${outcome} did not become ready: ${error}`);
        }
        if (outcome === 'success' || outcome.startsWith('runtime-')) {
          assert.deepEqual(native, {submit: true, link: true}, 'Startup listeners must apply native event options');
          await page.waitForFunction(() => window.startupCounter.textContent === 'Alpha: 2');
          await page.waitForFunction(() => window.startupRoot.querySelector('[data-submitted]').textContent === 'Before ready 中文 😀');
          await page.waitForFunction(() => window.startupRoot.querySelector('[data-event-log]').textContent === 'link handled');
        } else {
          await page.evaluate(() => window.cancelledStartup.close());
          assert.equal(await root.locator('[data-counter=Alpha]').textContent(), 'Alpha: 0', 'Cancelled startup actions entered the replacement');
          await root.locator('[data-counter=Alpha]').click();
          await page.waitForFunction(() => window.startupRoot.querySelector('[data-counter=Alpha]').textContent === 'Alpha: 1');
        }
        if (outcome !== 'cancel') {
          assert.equal(await page.evaluate(() => window.startupRoot.querySelector('[data-counter=Alpha]') === window.startupCounter), true);
          await page.waitForFunction(() => window.startupRoot.querySelector('[data-greeting]').textContent === 'Hello, Before ready 中文 😀');
          assert.equal(await root.locator('#controlled-select').inputValue(), 'b');
          await root.locator('#controlled-choice-form button[type=submit]').click();
          await page.waitForFunction(() => window.startupRoot.querySelector('[data-choice-saved]').textContent === 'false/a/b');
        }
        await page.evaluate(async () => {window.startupApp.close(); await window.startupApp.done; window.startupFailures = [];});
        cases.push({backend, outcome, passed: true});
      } finally {
        await page.unroute(path);
      }
    }
  }
  const reset = await page.evaluate(async () => {
    const {DomRenderer} = await import('/host/ui_next/renderer.js');
    const {WIRE_VERSION} = await import('/host/ui_next/generated/protocol.js');
    const root = window.startupRoot;
    const html = '<form data-vo-id="1"><output data-vo-id="2"><!--vo:t:3-->initial<!--vo:/t:3--></output></form>';
    const mutation = (op, id, fields = {}) => ({op, id, parent: 0, before: 0, name: '', value: '', ...fields});
    const batch = {version: WIRE_VERSION, revision: 1, inputSequence: 0, commands: null, mutations: [
      mutation('create', 1, {name: 'form'}), mutation('insert', 1),
      mutation('create', 2, {name: 'output'}), mutation('insert', 2, {parent: 1}),
      mutation('create', 3, {name: '#text', value: 'initial'}), mutation('insert', 3, {parent: 2}),
    ]};
    for (const retry of [false, true]) {
      root.innerHTML = html;
      const text = root.querySelector('output').childNodes[1];
      let renderer = new DomRenderer(root, () => {}, true);
      try {
        // Both resets precede the first commit and must retain the first nodes.
        root.querySelector('form').reset(); root.querySelector('form').reset();
        if (retry) {
          const invalid = structuredClone(batch); invalid.mutations[4].value = 'mismatch';
          let rejected = false;
          try {renderer.applyBatch(invalid);} catch {rejected = true;}
          if (!rejected) throw new Error('Invalid startup batch was accepted');
          renderer.close(false);
          if (root.innerHTML !== html) throw new Error('Failed startup lost reset output identities');
          renderer = new DomRenderer(root, () => {}, true);
        }
        renderer.applyBatch(batch);
        const output = root.querySelector('output');
        if (output.childNodes.length !== 1 || output.firstChild !== text) throw new Error('Startup reset replaced text or restored retired markers');
      } finally {renderer.close();}
    }
    return {commit: true, failedCommitRetry: true};
  });
  return {passed: true, cases, reset};
}
