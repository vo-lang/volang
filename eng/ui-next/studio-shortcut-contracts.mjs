import {sourceEditor} from './editor-controls.mjs';
import {waitStudioDraft} from './studio-draft-contracts.mjs';
import assert from 'node:assert/strict';

export const shortcutContracts = ['editor-native-enter', 'editor-exact-control-meta-shortcut',
  'editor-composition-bypass', 'editor-repeat-guard', 'editor-busy-guard', 'editor-latest-source-burst',
  'editor-hydrated-node-preserved', 'ui-preview-run-shortcut'];

export async function checkStudioShortcuts(browser, url) {
  const reports = [];
  for (const backend of ['vm']) for (const mode of ['client', 'hydrate']) {
    const page = await browser.newPage(), errors = [];
    page.on('pageerror', error => errors.push(error.message));
    let release = () => {};
    if (mode === 'hydrate') {
      const gate = new Promise(resolve => { release = resolve; });
      await page.route('**/artifacts/studio.*', async route => { await gate; await route.continue(); });
    }
    try {
      await page.goto(`${url}/studio/playground?backend=${backend}${mode === 'hydrate' ? '&ssr' : ''}`, { waitUntil: 'commit' });
      const editor = sourceEditor(page);
      if (mode === 'hydrate') {
        await editor.fill('before boot');
        await editor.press('End');
        await editor.press('Enter');
        assert.equal(await editor.inputValue(), 'before boot\n');
        await editor.evaluate(element => { window.earlyShortcutEditor = element; });
        release();
      }
      await page.waitForFunction(() => window.__studioNext?.ready || window.__studioNext?.error);
      assert.equal(await page.evaluate(() => window.__studioNext.error), null);
      if (mode === 'hydrate') {
        assert.equal(await editor.evaluate(element => element === window.earlyShortcutEditor), true);
        assert.equal(await editor.inputValue(), 'before boot\n');
      }
      assert.equal(await editor.getAttribute('aria-keyshortcuts'), 'Control+Enter Meta+Enter');
      await editor.fill('ordinary input');
      await editor.press('End');
      await editor.press('Enter');
      assert.equal(await editor.inputValue(), 'ordinary input\n');
      const results = await editor.evaluate(element => {
        const press = options => {
          const event = new KeyboardEvent('keydown', { key: 'Enter', bubbles: true, cancelable: true, ...options });
          element.dispatchEvent(event);
          return event.defaultPrevented;
        };
        const bypass = [{}, { shiftKey: true }, { ctrlKey: true, altKey: true },
          { ctrlKey: true, shiftKey: true }, { ctrlKey: true, isComposing: true }].map(press);
        element.dispatchEvent(new CompositionEvent('compositionstart', { bubbles: true }));
        bypass.push(press({ ctrlKey: true }));
        element.dispatchEvent(new CompositionEvent('compositionend', { bubbles: true }));
        const repeated = press({ ctrlKey: true, repeat: true });
        // This later input/save acknowledges the preceding ignored key event.
        element.value = 'repeat was ignored';
        element.dispatchEvent(new InputEvent('input', { bubbles: true }));
        return { bypass, repeated };
      });
      assert.deepEqual(results, { bypass: Array(6).fill(false), repeated: true });
      await waitStudioDraft(page,'repeat was ignored');
      assert.equal(await page.evaluate(() => window.__studioNext.workers.started), 0);

      const source = 'package main\nfunc main() { println("Shortcut 中文") }\n';
      const prevented = await editor.evaluate((element, source) => {
        element.value = source;
        element.dispatchEvent(new InputEvent('input', { bubbles: true }));
        // Both keys precede rendering; the second must see live busy state.
        return [false, false, true].map(repeat => {
          const event = new KeyboardEvent('keydown', { key: 'Enter', ctrlKey: true, repeat, bubbles: true, cancelable: true });
          element.dispatchEvent(event);
          return event.defaultPrevented;
        });
      }, source);
      assert.deepEqual(prevented, [true, true, true]);
      const completed = count => page.waitForFunction(count => {
        const state = window.__studioNext;
        return document.querySelector('[data-output]')?.textContent === 'Shortcut 中文\n' &&
          state.workers.started === count && state.workers.stopped === count;
      }, count, { timeout: 35000 });
      await completed(1);
      assert.equal(await editor.inputValue(), source);
      await editor.press('Meta+Enter');
      await completed(2);
      assert.equal(await editor.inputValue(), source);
      await editor.press('Control+Enter');
      await completed(3);
      assert.equal(await editor.inputValue(), source);

      await page.getByRole('link', { name: 'Try UI components →', exact: true }).click();
      const previewEditor = sourceEditor(page, 'ui-playground-source');
      await page.locator('#ui-playground-source').waitFor();
      const previewSource = (await previewEditor.inputValue()).replace('Make something good.', 'Started with a shortcut');
      await previewEditor.fill(previewSource);
      await previewEditor.press('Control+Enter');
      const frame = page.frameLocator('iframe');
      await frame.getByRole('heading', { name: 'Started with a shortcut', exact: true }).waitFor({ timeout: 35000 });
      await frame.getByRole('button', { name: 'One more idea', exact: true }).click();
      await page.waitForFunction(() => document.querySelector('iframe')?.contentDocument.querySelector('output')?.textContent === '1 little ideas');
      assert.equal(await previewEditor.inputValue(), previewSource);
      await page.getByRole('button', { name: 'Stop preview', exact: true }).click();
      await page.waitForFunction(() => window.__studioNext.workers.started === window.__studioNext.workers.stopped);
      assert.equal(await page.evaluate(() => window.__studioNext.error), null);
      assert.deepEqual(errors, []);
      reports.push({ backend, mode: `${mode}-shortcuts`, passed: true, contracts: shortcutContracts });
    } finally { release(); await page.close(); }
  }
  return reports;
}
