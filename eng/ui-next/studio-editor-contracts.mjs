import assert from 'node:assert/strict';
import {sourceEditor} from './editor-controls.mjs';
import {waitStudioDraft} from './studio-draft-contracts.mjs';

export async function checkStudioEditor(browser, url) {
  const reports = [];
  for (const backend of ['vm']) for (const mode of ['client', 'delayed', 'failure', 'close']) {
    const page = await browser.newPage(), errors = [], requests = [];
    const restoredDraft = mode === 'client' || mode === 'failure';
    const restored = 'package main\r\nfunc main() {\r  unused := "中文🙂"\r\n  missingName()\r}\r\n';
    const normalized = restored.replace(/\r\n?/g, '\n');
    if (restoredDraft) await page.addInitScript(source => {
      localStorage.setItem('volang.studio.next.draft.v1', source);
      localStorage.setItem('volang.studio.next.ui-draft.v1', source);
    }, restored);
    const checkRestored = async (id, key, action) => {
      const control = sourceEditor(page, id);
      await page.waitForFunction(({id, source}) => document.getElementById(id)?.value === source, {id, source:normalized});
      await waitStudioDraft(page,normalized,key);
      await page.getByRole('button', {name:action, exact:true}).click();
      const diagnostic = page.locator('[data-source-diagnostic]').filter({hasText:'missingName'});
      await diagnostic.waitFor({timeout:35000}); await diagnostic.click();
      await page.waitForFunction(({id, start}) => {
        const input = document.getElementById(id);
        return input.selectionStart === start && input.value.slice(input.selectionStart, input.selectionEnd) === 'missingName';
      }, {id, start:normalized.indexOf('missingName')});
      assert.equal(await control.inputValue(), normalized, 'restored draft and displayed source diverged');
      assert.equal(await control.surface.evaluate(input => document.activeElement === input), true);
    };
    page.on('pageerror', error => errors.push(error.message));
    page.on('request', request => requests.push(request.url()));
    let release = () => {};
    if (mode !== 'client') {
      const gate = new Promise(resolve => {release = resolve;});
      await page.route(/\/editor-library(?:-[a-z0-9]+)?\.js(?:\?.*)?$/i, async route => {
        try {
          if (mode === 'failure') return await route.fulfill({status:503, body:'Editor temporarily unavailable'});
          await gate;
          const response = await route.fetch();
          await route.fulfill({response, body:(await response.text()) + '\n;globalThis.editorLibraryEvaluated = true;'});
        } catch (error) {if (!page.isClosed() && !route.request().failure()) throw error;}
      });
    }
    try {
      await page.goto(`${url}/studio/gallery?backend=${backend}`);
      await page.waitForFunction(() => window.__studioNext?.ready || window.__studioNext?.error);
      assert.equal(await page.evaluate(() => window.__studioNext.error), null);
      assert(!requests.some(url => url.includes('editor-library')), 'Gallery loaded the optional editor');
      await page.getByRole('link', {name:'Playground', exact:true}).click();
      const editor = sourceEditor(page);
      await editor.waitFor();
      if (mode === 'client') {
        await page.locator('.cm-content').waitFor();
        for (const dark of [false, true]) {
          await page.locator('#studio-theme').setChecked(dark);
          await page.waitForFunction(dark => document.querySelector('.studio').dataset.theme === (dark ? 'dark' : 'light'), dark);
          const contrast = await page.evaluate(() => {
            const luminance = color => color.match(/[\d.]+/g).slice(0, 3).map(Number).map(value => {
              value /= 255;
              return value <= 0.04045 ? value / 12.92 : ((value + 0.055) / 1.055) ** 2.4;
            }).reduce((sum, value, index) => sum + value * [0.2126, 0.7152, 0.0722][index], 0);
            const background = luminance(getComputedStyle(document.querySelector('.cm-editor')).backgroundColor);
            const tokens = [...document.querySelectorAll('.cm-content span')].filter(node => node.textContent.trim());
            return {count:tokens.length, minimum:Math.min(...tokens.map(node => {
              const ink = luminance(getComputedStyle(node).color);
              return (Math.max(ink, background) + 0.05) / (Math.min(ink, background) + 0.05);
            }))};
          });
          assert(contrast.count > 0 && contrast.minimum >= 4.5, `unreadable source tokens in ${dark ? 'dark' : 'light'} appearance: ${JSON.stringify(contrast)}`);
        }
      }
      if (restoredDraft) await checkRestored('playground-source', 'volang.studio.next.draft.v1', 'Run code');
      const source = 'package main\nfunc main() { println("Editor 中文") }\n';
      await editor.fill(source);
      await waitStudioDraft(page,source);
      if (mode === 'delayed') {
        await editor.input.evaluate(input => input.setSelectionRange(2, 8, 'backward'));
        release();
        await page.waitForFunction(() => window.editorLibraryEvaluated);
        assert.equal(await page.locator('.cm-editor').count(), 0, 'late library interrupted native editing');
        await page.getByLabel('Choose an example', {exact:true}).focus();
        await page.locator('.cm-content').waitFor();
        assert.equal(await editor.inputValue(), source);
        assert.equal(await editor.input.evaluate(input => input.selectionDirection), 'backward');
        assert.equal(await page.locator('.cm-content').getAttribute('aria-keyshortcuts'), 'Control+Enter Meta+Enter Control+Space F12');
      } else if (mode === 'close') {
        await page.evaluate(async () => {window.__studioNext.close(); await window.__studioNext.done;});
        release();
        await page.waitForFunction(() => window.editorLibraryEvaluated);
        assert.equal(await page.locator('#root').textContent(), '');
        assert.equal(await page.locator('.cm-editor').count(), 0);
      }
      if (mode !== 'close') {
        await editor.press('Control+Enter');
        await page.waitForFunction(() => document.querySelector('[data-output]')?.textContent === 'Editor 中文\n', null, {timeout:35000});
        assert.equal(await editor.inputValue(), source);
        if (mode === 'failure') {
          assert.equal(await page.locator('.cm-editor').count(), 0);
          assert.equal(await editor.input.getAttribute('aria-hidden'), null);
          assert.equal(await editor.input.getAttribute('aria-keyshortcuts'),'Control+Enter Meta+Enter');
        }
        const invalid = 'package main\nfunc main() { unused := "中文🙂"; missingName() }\n';
        await editor.fill(invalid);
        await page.getByRole('button',{name:'Run code',exact:true}).click();
        const diagnostic = page.locator('[data-source-diagnostic]').filter({hasText:'missingName'});
        await diagnostic.waitFor();
        assert.equal(await page.getByText('Full compiler message',{exact:true}).count(),1);
        assert.equal(await page.locator('[data-output]').isVisible(),false,'compiler details obscure the primary problem list');
        await diagnostic.click();
        await page.waitForFunction(() => {
          const input=document.getElementById('playground-source');
          return input.value.slice(input.selectionStart,input.selectionEnd)==='missingName';
        });
        const selection = () => editor.input.evaluate(input => ({start:input.selectionStart,end:input.selectionEnd,
          selected:input.value.slice(input.selectionStart,input.selectionEnd)}));
        assert.deepEqual(await selection(),{start:invalid.indexOf('missingName'),end:invalid.indexOf('missingName')+11,selected:'missingName'});
        assert.equal(await editor.surface.evaluate(input=>document.activeElement===input),true,'source jump did not focus the visible editor');
        await page.waitForFunction(() => document.getElementById('playground-source').getAttribute('aria-hidden')!=='true'
          || getSelection().toString()==='missingName');
        await page.locator('[data-source-diagnostic]').filter({hasText:'unused'}).click();
        await page.waitForFunction(() => {
          const input=document.getElementById('playground-source');
          return input.value.slice(input.selectionStart,input.selectionEnd)==='unused';
        });
        assert.equal((await selection()).selected,'unused','warning location reused another diagnostic range');
        await page.waitForFunction(() => document.getElementById('playground-source').getAttribute('aria-hidden')!=='true'
          || getSelection().toString()==='unused');
        if (backend==='vm'&&mode==='client') await page.screenshot({path:`target/ui-next/studio-diagnostics-${browser.browserType().name()}.png`,fullPage:true});
        await editor.fill(source);
        await page.locator('[data-diagnostics-stale]').waitFor();
        assert.equal(await page.locator('[data-source-diagnostic]').count(),0,'edited draft kept old source actions');
        await page.getByRole('button',{name:'Run code',exact:true}).click();
        await page.waitForFunction(() => document.querySelector('[data-output]')?.textContent === 'Editor 中文\n', null, {timeout:35000});
        assert.equal(await page.locator('[aria-label="Source diagnostics"], [data-diagnostics-stale]').count(),0,'repaired source retained old diagnostics');

        if (mode==='client'||mode==='failure') {
          const warningSource = 'package main\nfunc main() { unused := "中文🙂"; println("With warnings") }\n';
          for (const fails of [false,true]) {
            await editor.fill(fails ? warningSource.replace('println("With warnings")', 'panic("Warning runtime failure")') : warningSource);
            await page.getByRole('button',{name:'Run code',exact:true}).click();
            const warning = page.locator('[data-source-diagnostic]').filter({hasText:'unused'});
            await warning.waitFor();
            await page.waitForFunction(fails => document.querySelector('[data-output]')?.textContent.includes(fails ? 'Warning runtime failure' : 'With warnings'), fails);
            assert.equal(await page.locator('[data-output]').isVisible(),true,'warnings hid program output');
            assert.equal(await page.getByText('Full compiler message',{exact:true}).count(),0);
            await warning.click();
            await page.waitForFunction(()=>{const input=document.getElementById('playground-source');return input.value.slice(input.selectionStart,input.selectionEnd)==='unused';});
          }
          await page.getByRole('link',{name:'Try UI components →',exact:true}).click();
          const previewEditor=sourceEditor(page,'ui-playground-source');await previewEditor.waitFor();
          await checkRestored('ui-playground-source', 'volang.studio.next.ui-draft.v1', 'Run preview');
          const original='package main\nimport ui "github.com/vo-lang/ui/next"\nimport "github.com/vo-lang/ui/next/host"\nfunc main() { host.Run(ui.Element("h1", ui.Text("Restored editor"))) }\n';
          await previewEditor.fill(invalid);
          await page.getByRole('button',{name:'Run preview',exact:true}).click();
          await diagnostic.waitFor({timeout:35000});await diagnostic.click();
          await page.waitForFunction(() => {
            const input=document.getElementById('ui-playground-source');
            return input.value.slice(input.selectionStart,input.selectionEnd)==='missingName';
          });
          assert.equal(await previewEditor.input.evaluate(input=>input.value.slice(input.selectionStart,input.selectionEnd)),'missingName','UI preview lost its compiler location');
          await previewEditor.fill(original);
          await page.locator('[data-diagnostics-stale]').waitFor();
          await page.getByRole('button',{name:'Run preview',exact:true}).click();
          await page.waitForFunction(()=>document.querySelector('[data-preview-status]')?.textContent.includes('Your preview is ready'),null,{timeout:35000});
          assert.equal(await page.locator('[aria-label="Source diagnostics"], [data-diagnostics-stale]').count(),0,
            await page.locator('[aria-label="Source diagnostics"], [data-diagnostics-stale]').allTextContents());
          await page.getByRole('button',{name:'Stop preview',exact:true}).click();
          for (const fails of [false,true]) {
            const warningSource = fails
              ? 'package main\nfunc main() { unused := "中文🙂"; panic("Warning preview failure") }\n'
              : original.replace('host.Run(', 'unused := "中文🙂"; host.Run(');
            await previewEditor.fill(warningSource);
            await page.getByRole('button',{name:'Run preview',exact:true}).click();
            const warning = page.locator('[data-source-diagnostic]').filter({hasText:'unused'});
            await warning.waitFor({timeout:35000});
            await page.waitForFunction(fails=>document.querySelector('[data-preview-status]')?.textContent.includes(fails?'Warning preview failure':'Your preview is ready'),fails,{timeout:35000});
            assert.equal(await page.locator('[data-preview-status]').isVisible(),true,'warnings hid preview status');
            await warning.click();
            await page.waitForFunction(()=>{const input=document.getElementById('ui-playground-source');return input.value.slice(input.selectionStart,input.selectionEnd)==='unused';});
            if (!fails) await page.getByRole('button',{name:'Stop preview',exact:true}).click();
          }
        }
        await page.evaluate(async () => {window.__studioNext.close(); await window.__studioNext.done;});
      }
      assert.equal(await page.evaluate(() => window.__studioNext.error), null);
      assert.deepEqual(errors, []);
      reports.push({backend, mode, passed:true,diagnostics:mode!=='close',previewDiagnostics:restoredDraft,restoredDraft,warnings:restoredDraft});
    } finally {release(); await page.close();}
  }
  return reports;
}
