import assert from 'node:assert/strict';

export async function checkInspectionPanel(page, url) {
  await page.route('**/__inspection-panel', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><title>Development panel</title>' }));
  await page.goto(url + '/__inspection-panel');
  await page.evaluate(async () => {
    const { mountUi } = await import('/host/ui_next/development-mount.js');
    const roots = [];
    for (let index = 0; index < 2; index++) {
      const container = document.createElement('div'); container.id = `application-${index}`; document.body.append(container);
      const app = mountUi(container, { artifact: '/artifacts/inspection.vob', loadVm: () => import('/wasm/vo_web.js') });
      roots.push(app);
      if (!await app.ready) throw new Error('Development root did not become ready.');
    }
    window.inspectedApplications = roots;
  });
  const panel = page.locator('[data-ui-inspector]');
  assert.equal(await panel.count(), 1);
  assert.equal(await panel.getByLabel('Application').locator('option').count(), 2);
  await panel.getByText('Inspect components', { exact: true }).click();
  await panel.getByText('inspection.Counter · First', { exact: true }).click();
  await page.locator('#application-0').getByRole('button', { name: 'Add to First', exact: true }).click();
  await page.waitForFunction(() => document.querySelector('#application-0 [data-counter=First]').textContent === '2');
  await panel.getByRole('button', { name: 'Capture snapshot' }).click();
  await page.waitForFunction(() => {
    const rows = document.querySelector('[data-ui-inspector]').shadowRoot.querySelectorAll('tbody tr');
    return [...rows].some(row => row.firstElementChild.textContent === 'count' && row.lastElementChild.textContent === '1');
  });
  await page.locator('#application-0').getByRole('button', { name: 'Start waiting', exact: true }).click();
  await page.waitForFunction(() => document.querySelector('#application-0 [data-request-status]').textContent === 'Waiting');
  await panel.getByRole('button', { name: 'Capture snapshot' }).click();
  await page.waitForFunction(() => {
    const tasks = document.querySelector('[data-ui-inspector]').shadowRoot.querySelector('[data-inspection-tasks]');
    return tasks?.textContent.includes('web.delay') && tasks.textContent.includes('waiting');
  });
  await panel.getByLabel('Application').selectOption({ label: 'application-1' });
  await page.waitForFunction(() => {
    const rows = document.querySelector('[data-ui-inspector]').shadowRoot.querySelectorAll('tbody tr');
    return rows.length > 0 && [...rows].filter(row => row.firstElementChild.textContent === 'count').every(row => row.lastElementChild.textContent === '0');
  });
  assert.equal(await panel.locator('[data-task]').count(), 0, 'tasks leaked between inspected roots');
  await page.evaluate(async () => { window.inspectedApplications[0].close(); await window.inspectedApplications[0].done; });
  assert.equal(await panel.getByLabel('Application').locator('option').count(), 1);
  await page.setViewportSize({ width: 390, height: 844 });
  assert(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
  await page.evaluate(async () => { window.inspectedApplications[1].close(); await window.inspectedApplications[1].done; });
  assert.equal(await panel.count(), 0, 'last root left the panel registered');
  await page.evaluate(async () => {
    const { mountUi } = await import('/host/ui_next/development-mount.js');
    const root = document.body.appendChild(document.createElement('div'));
    const app = mountUi(root, { backend: 'vm', artifact: '/artifacts/inspection.vob', loadVm: () => import('/wasm/vo_web.js') });
    app.close(); app.close(); await app.done;
  });
  assert.equal(await panel.count(), 0, 'closing before startup attached a late panel');
  return { passed: true, contracts: ['one-panel-per-document', 'vm-root-selection', 'independent-state', 'independent-task-history', 'on-demand-capture', 'remaining-root-survives', 'narrow-layout', 'last-root-panel-removal', 'closed-startup-suppression'] };
}
