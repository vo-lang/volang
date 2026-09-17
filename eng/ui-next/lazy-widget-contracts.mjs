import assert from 'node:assert/strict';

export async function checkLazyWidgetApplications(browser, url) {
  const reports = [];
  for (const backend of ['vm']) {
    for (const mode of ['client-adopt', 'ssr-remove', 'client-failure', 'client-close']) {
      const page = await browser.newPage(), errors = [];
      page.on('pageerror', error => errors.push(error.message));
      let release;
      const gate = new Promise(resolve => {release = resolve;});
      const started = page.waitForRequest(request => request.url().endsWith('/vendor/uplot/uPlot.esm.js'));
      void started.catch(() => {});
      let imports = 0;
      await page.route('**/vendor/uplot/uPlot.esm.js', async route => {
        imports++; await gate;
        try {
          if (mode === 'client-failure') await route.fulfill({status:503, contentType:'text/javascript', body:'temporarily unavailable'});
          else {
            const response = await route.fetch();
            await route.fulfill({response, body:(await response.text()) + '\n;globalThis.__lazyPlotModuleEvaluated = true;\n'});
          }
        } catch (error) {if (!route.request().failure()) throw error;}
      });
      try {
        await page.goto(`${url}/?example=workbench&backend=${backend}${mode === 'ssr-remove' ? '&ssr' : ''}`, {waitUntil:'commit'});
        if (mode === 'ssr-remove') {
          await page.locator('#profile-name').fill('Before optional code');
          await page.evaluate(() => {window.lazyEarlyInput = document.querySelector('#profile-name');});
        }
        await started;
        await page.waitForFunction(() => window.__uiNext?.ready || window.__uiNext?.error);
        assert.equal(await page.evaluate(() => window.__uiNext.error), null);
        assert.equal(await page.locator('[data-chart] canvas').count(), 0);
        assert.equal(await page.evaluate(() => window.__uiNext.widgets.mounts), 0);
        if (mode === 'client-close') {
          await page.evaluate(async () => {window.__uiNext.close(); await window.__uiNext.done;});
          release();
          await page.waitForFunction(() => window.__lazyPlotModuleEvaluated);
          assert.equal(await page.evaluate(() => window.__uiNext.widgets.mounts), 0);
          assert.equal(await page.locator('#root').textContent(), '');
        } else {
          // The unrelated application can validate and save while the chart is held.
          await page.locator('#profile-name').fill('Ahead of the chart');
          await page.getByRole('button', {name:'Save preferences', exact:true}).click();
          await page.waitForFunction(() => document.querySelector('[data-saved]').textContent.startsWith('Ahead of the chart'));
          await page.evaluate(() => {
            document.querySelector('[data-chart-update]').click();
            document.querySelector('[data-chart-update]').click();
          });
          if (mode === 'ssr-remove') {
            assert.equal(await page.evaluate(() => window.lazyEarlyInput === document.querySelector('#profile-name')), true);
            await page.locator('[data-chart-toggle]').click();
            await page.waitForFunction(() => !document.querySelector('[data-chart]'));
          }
          release();
          if (mode === 'client-failure') {
            await page.waitForFunction(() => document.querySelector('[data-chart-error]').textContent.length > 0);
            assert.equal(await page.evaluate(() => window.__uiNext.error), null);
            assert.equal(await page.locator('[data-chart] canvas').count(), 0);
            await page.locator('#team-filter').fill('grace');
            await page.waitForFunction(() => document.querySelector('[data-catalog="team"] [data-count]').textContent === '1 person');
          } else {
            if (mode === 'ssr-remove') {
              await page.waitForFunction(() => window.__lazyPlotModuleEvaluated);
              assert.equal(await page.evaluate(() => window.__uiNext.widgets.mounts), 0, 'removed widget mounted when its import finished');
              await page.locator('[data-chart-toggle]').click();
            }
            await page.waitForFunction(() => document.querySelector('[data-chart] canvas'));
            const stats = await page.evaluate(() => window.__uiNext.widgets);
            assert.equal(stats.mounts, 1);
            assert.equal(stats.updates, 0, 'pending updates were replayed after mounting');
          }
          assert.equal(await page.locator('#profile-name').inputValue(), 'Ahead of the chart');
          await page.evaluate(async () => {window.__uiNext.close(); await window.__uiNext.done;});
          const stats = await page.evaluate(() => window.__uiNext.widgets);
          assert.equal(stats.mounts, stats.disposals);
        }
        assert.equal(imports, 1, 'native module load was duplicated');
        assert.deepEqual(errors, []);
        reports.push({backend, mode, passed:true, imports});
        console.log(`${backend}: lazy widget ${mode} passed`);
      } finally {release(); await page.close();}
    }
  }
  return reports;
}
