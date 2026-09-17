import assert from 'node:assert/strict';

export async function checkInspection(browser, url) {
  const reports = [];
  for (const backend of ['vm']) {
    const page = await browser.newPage(), errors = [];
    page.on('pageerror', error => errors.push(String(error)));
    try {
      await page.goto(`${url}/?example=inspection&backend=${backend}`);
      try { await page.waitForFunction(() => window.__inspection?.snapshot || window.__inspection?.error); }
      catch (error) {
        throw new Error(`${backend}: ${error.message}; ${JSON.stringify(await page.evaluate(() => ({ state: window.__inspection, status: document.getElementById('inspection-status')?.textContent, text: document.body.textContent }))) }; page errors: ${errors.join('; ')}`);
      }
      assert.equal(await page.evaluate(() => window.__inspection.error), null);
      const first = await page.evaluate(() => window.__inspection.snapshot);
      assert.equal(first.root.components.length, 3, 'inspection must show only application components');
      assert.deepEqual(first.root.components.map(component => component.frame.name), ['inspection.App', 'inspection.Counter', 'inspection.Counter']);
      assert.equal(first.root.components[0].parent, 0, 'inspection inserted a synthetic parent component');
      const initialFirst = first.root.components.find(component => component.frame.key === 'First');
      const initialSecond = first.root.components.find(component => component.frame.key === 'Second');
      assert.equal(initialFirst.states.find(state => state.identity.key === 'count').value, '0');
      assert.equal(initialFirst.dependencies[0].key, 'doubled');
      assert.deepEqual(first.props.find(record => record.component === initialFirst.id).values, [
        {name:'name',kind:'string',value:'First',truncated:false},
        {name:'step',kind:'int',value:'1',truncated:false},
      ]);
      await page.getByRole('button', { name: 'Add to First', exact: true }).click();
      await page.waitForFunction(() => document.querySelector('[data-counter=First]').textContent === '2');
      const updated = await page.evaluate(() => window.__inspection.capture());
      const nextFirst = updated.root.components.find(component => component.id === initialFirst.id);
      const nextSecond = updated.root.components.find(component => component.id === initialSecond.id);
      assert.equal(nextFirst.renders, initialFirst.renders + 1);
      assert.equal(nextSecond.renders, initialSecond.renders);
      assert.equal(nextFirst.states.find(state => state.identity.key === 'count').value, '1');
      const render = updated.renders.findLast(record => record.component.id === initialFirst.id);
      assert.deepEqual(render.causes, [{ kind: 'state', state: { component: initialFirst.id, key: 'doubled', truncated: false } }]);
      assert(render.nanoseconds >= 0);
      await page.locator(`details[data-component="${initialFirst.id}"] summary`).click();
      const again = await page.evaluate(() => window.__inspection.capture());
      assert.equal(again.root.components.find(component => component.id === initialFirst.id).renders, nextFirst.renders, 'inspection triggered a component render');
      assert.equal(await page.locator(`details[data-component="${initialFirst.id}"]`).getAttribute('open'), '');
      await page.getByRole('button', { name: 'Toggle second counter', exact: true }).click();
      await page.locator('[data-counter=Second]').waitFor({ state: 'detached' });
      const removed = await page.evaluate(() => window.__inspection.capture());
      assert.equal(removed.root.components.some(component => component.id === initialSecond.id), false);
      const parent = removed.renders.findLast(record => record.component.id === initialFirst.id);
      assert.equal(parent.causes[0].kind, 'parent');
      await page.getByRole('button', { name: 'Increase counter step', exact: true }).click();
      await page.waitForFunction(() => document.querySelector('[data-counter-step]').textContent === 'Counter step: 2');
      const changedProps = await page.evaluate(() => window.__inspection.capture());
      assert.equal(changedProps.props.find(record => record.component === initialFirst.id).values.find(value => value.name === 'step').value, '2');
      assert.equal(changedProps.props.some(record => record.component === initialSecond.id), false, 'disposed props survived their owner');
      assert.equal(await page.locator(`details[data-component="${initialFirst.id}"] [data-prop=step] td`).last().textContent(), '2');
      assert.equal(await page.locator('[data-counter=First]').textContent(), '2', 'observing props changed counter state');
      await checkTasks(page, first.root.components[0].id);
      await page.setViewportSize({ width: 390, height: 844 });
      assert(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), 'inspector overflows a narrow page');
      await page.screenshot({ path: `target/ui-next/inspection-${browser.browserType().name()}-${backend}.png`, fullPage: true });
      await page.evaluate(async () => { window.__inspection.close(); await window.__inspection.done; });
      assert.deepEqual(errors, []);
      reports.push({ backend, passed: true, contracts: ['component-state-dependency-snapshot', 'local-update-cause',
        'unrelated-component-stable', 'on-demand-without-reactive-reads', 'keyed-component-disposal',
        'parent-render-cause', 'expansion-preserved', 'declared-props-update-and-disposal', 'request-completion-timeout-cancel',
        'subscription-progress-cancel', 'task-owner-and-timing', 'inspection-transport-excluded',
        'narrow-layout', 'owned-teardown'] });
    } finally { await page.close(); }
  }
  return reports;
}

async function checkTasks(page, owner) {
  const capture = () => page.evaluate(() => window.__inspection.capture());
  const requestStatus = text => page.waitForFunction(text =>
    document.querySelector('[data-request-status]').textContent === text, text);
  const latest = snapshot => snapshot.tasks.at(-1);
  await page.getByRole('button', { name: 'Start waiting', exact: true }).click();
  await requestStatus('Waiting');
  const waiting = latest(await capture());
  assert.equal(waiting.service, 'web.delay');
  assert.equal(waiting.state, 'waiting');
  assert.equal(waiting.task.component, owner);
  assert.equal(waiting.task.frame.name, 'inspection.App');
  assert.equal(waiting.observedStart, true);
  assert(waiting.nanoseconds >= 0);
  assert.equal(waiting.updates, 0);
  assert.match(await page.locator(`[data-task="${waiting.task.id}"]`).textContent(), /web.delay.*waiting.*ms/);
  await page.getByRole('button', { name: 'Cancel request', exact: true }).click();
  await requestStatus('Cancelled');
  assert.equal(latest(await capture()).state, 'cancelled');
  await page.getByRole('button', { name: 'Complete a request', exact: true }).click();
  await requestStatus('Completed');
  const completed = latest(await capture());
  assert.equal(completed.state, 'completed');
  assert.equal(completed.updates, 1);
  assert.equal(completed.error, '');
  await page.getByRole('button', { name: 'Try a timeout', exact: true }).click();
  await requestStatus('Request timed out after 100 ms');
  const timedOut = latest(await capture());
  assert.equal(timedOut.state, 'failed');
  assert.equal(timedOut.timeoutMilliseconds, 100);
  assert.equal(timedOut.error, 'Request timed out after 100 ms');
  assert.equal(timedOut.updates, 1);
  assert.match(await page.locator(`[data-task="${timedOut.task.id}"]`).textContent(), /Request timed out after 100 ms/);
  await page.getByRole('button', { name: 'Watch page activity', exact: true }).click();
  await page.waitForFunction(() => /^Page (active|inactive)$/.test(document.querySelector('[data-watch-status]').textContent));
  const subscription = latest(await capture());
  assert.equal(subscription.service, 'web.page-active');
  assert.equal(subscription.task.subscription, true);
  assert.equal(subscription.state, 'waiting');
  assert(subscription.updates >= 1);
  await page.getByRole('button', { name: 'Cancel subscription', exact: true }).click();
  await page.waitForFunction(() => document.querySelector('[data-watch-status]').textContent === 'Subscription cancelled');
  const final = await capture();
  const cancelled = latest(final);
  assert.equal(cancelled.task.id, subscription.task.id);
  assert.equal(cancelled.state, 'cancelled');
  assert(cancelled.updates >= subscription.updates);
  assert.equal(final.tasks.length, 4, 'inspection transport leaked into the task history');
  const stable = await capture();
  assert.deepEqual(stable.tasks, final.tasks, 'capturing changed completed task history');
}
