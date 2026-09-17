import assert from 'node:assert/strict';
import { test } from 'node:test';
import { createReloadServices } from '../../lang/crates/vo-web/dist/ui_next/reload.js';

test('reload captures correlate, stay bounded and report independent resets', async () => {
  const reports = [], reload = createReloadServices(report => reports.push(report));
  const controller = new AbortController(), requests = [];
  reload.services.watches['ui.reload.request']('', controller.signal, id => requests.push(id));
  const capture = reload.capture();
  assert.equal(reload.capture(), capture);
  assert.deepEqual(requests, ['1']);
  const state = JSON.stringify({ version: 1, states: [{ path: 'root', key: 'count', kind: 'int', value: '9223372036854775806' }] });
  const publish = reload.services.tasks['ui.reload.publish'];
  await publish(JSON.stringify({ version: 1, request: '1', state }), controller.signal);
  assert.equal(await capture, state);
  await reload.services.tasks['ui.reload.report'](JSON.stringify({ restored: 1, reset: 2, messages: ['Changed schema.'] }), controller.signal);
  assert.deepEqual(reports, [{ restored: 1, reset: 2, messages: ['Changed schema.'] }]);
  await assert.rejects(reload.services.tasks['ui.reload.report']('{"restored":-1,"reset":0,"messages":null}', controller.signal), /Invalid reload report/);
  const next = reload.capture(), failed = assert.rejects(next, /Invalid reload state/);
  await assert.rejects(publish(JSON.stringify({ version: 1, request: '2', state: '[]' }), controller.signal), /Invalid reload state/);
  await failed;
  const closing = reload.capture(), closed = assert.rejects(closing, /root has closed/);
  controller.abort();
  await closed;
  reload.close();
});

test('reload rejects oversized captures without settling a later request', async () => {
  const reload = createReloadServices(() => {}), controller = new AbortController();
  reload.services.watches['ui.reload.request']('', controller.signal, () => {});
  const capture = reload.capture(), failure = assert.rejects(capture, /Invalid reload state/);
  await assert.rejects(reload.services.tasks['ui.reload.publish'](JSON.stringify({ version: 1, request: '1',
    state: 'x'.repeat(4 * 1024 * 1024 + 1) }), controller.signal), /Invalid reload state/);
  await failure;
  const next = reload.capture();
  let settled = false;
  void next.then(() => { settled = true; }, () => { settled = true; });
  await reload.services.tasks['ui.reload.publish'](JSON.stringify({ version: 1, request: '1', state: '{}' }), controller.signal);
  assert.equal(settled, false);
  const closed = assert.rejects(next, /Reload is closed/);
  reload.close(); await closed; controller.abort();
});
