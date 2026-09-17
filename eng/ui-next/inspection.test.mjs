import assert from 'node:assert/strict';
import { test } from 'node:test';
import { createInspectionServices } from '../../lang/crates/vo-web/dist/ui_next/inspection.js';

const snapshot = { root: { revision: 1, closed: false, truncated: false, components: [] }, renders: [], truncated: false };
test('inspection requests coalesce, correlate and retire with their root', async () => {
  const inspector = createInspectionServices(), controller = new AbortController(), requests = [];
  await assert.rejects(inspector.snapshot(), /not connected/);
  inspector.services.watches['ui.inspect.request']('', controller.signal, id => requests.push(id));
  const first = inspector.snapshot();
  assert.equal(inspector.snapshot(), first);
  assert.deepEqual(requests, ['1']);
  const publish = inspector.services.tasks['ui.inspect.publish'];
  await publish(JSON.stringify({ version: 1, request: '1', snapshot: snapshot }), controller.signal);
  assert.deepEqual(await first, snapshot);
  const second = inspector.snapshot();
  let settled = false;
  void second.then(() => { settled = true; }, () => { settled = true; });
  await publish(JSON.stringify({ version: 1, request: '1', snapshot: snapshot }), controller.signal);
  assert.equal(settled, false, 'a previous response settled a new capture');
  const aborted = assert.rejects(second, /root has closed/);
  controller.abort();
  await aborted;
  inspector.close(); inspector.close();
  await assert.rejects(inspector.snapshot(), /not connected/);
});

test('inspection deadlines and malformed responses stay local', async t => {
  let expire;
  t.mock.method(globalThis, 'setTimeout', callback => { expire = callback; return 1; });
  t.mock.method(globalThis, 'clearTimeout', () => {});
  const inspector = createInspectionServices(), controller = new AbortController();
  inspector.services.watches['ui.inspect.request']('', controller.signal, () => {});
  const first = inspector.snapshot(), failed = assert.rejects(first, /five seconds/);
  expire();
  await failed;
  const second = inspector.snapshot(), malformed = assert.rejects(second, /Invalid inspection/);
  await assert.rejects(inspector.services.tasks['ui.inspect.publish']('{"version":2}', controller.signal), /Invalid inspection/);
  await malformed;
  const third = inspector.snapshot(), badTree = assert.rejects(third, /Invalid inspection snapshot/);
  await assert.rejects(inspector.services.tasks['ui.inspect.publish'](JSON.stringify({
    version: 1, request: '3', snapshot: { ...snapshot, root: { ...snapshot.root, components: [{}] } },
  }), controller.signal), /Invalid inspection snapshot/);
  await badTree;
  const fourth = inspector.snapshot(), captureError = assert.rejects(fourth, /transport budget/);
  await assert.rejects(inspector.services.tasks['ui.inspect.publish'](JSON.stringify({
    version: 1, request: '4', error: 'The inspection snapshot could not fit its transport budget.',
  }), controller.signal), /transport budget/);
  await captureError;
  const fifth = inspector.snapshot(), closed = assert.rejects(fifth, /Inspection is closed/);
  inspector.close();
  await closed;
  controller.abort();
});

test('task inspection accepts bounded lifecycle records and rejects malformed additions', async () => {
  const inspector = createInspectionServices(), controller = new AbortController(), requests = [];
  inspector.services.watches['ui.inspect.request']('', controller.signal, id => requests.push(id));
  const publish = inspector.services.tasks['ui.inspect.publish'];
  const task = {task:{id:1,component:0,frame:{name:'',key:'',file:'',line:0},subscription:false,truncated:false},
    service:'web.delay',timeoutMilliseconds:500,state:'completed',updates:1,error:'',nanoseconds:10,
    observedStart:true,truncated:false};
  try {
    const pending=inspector.snapshot();
    await publish(JSON.stringify({version:1,request:requests.at(-1),snapshot:{...snapshot,tasks:[task]}}),controller.signal);
    assert.deepEqual((await pending).tasks,[task]);
    for (const tasks of [[{...task,state:'executing'}],[{...task,nanoseconds:Number.MAX_SAFE_INTEGER+1}],
      [{...task,updates:-1}],[{...task,error:'x'.repeat(257)}],[{...task,timeoutMilliseconds:2147483648}],
      [{...task,task:{...task.task,id:0}}],Array(577).fill(task),{}]) {
      const pending=inspector.snapshot(),rejected=assert.rejects(pending,/Invalid inspection snapshot/);
      await assert.rejects(publish(JSON.stringify({version:1,request:requests.at(-1),snapshot:{...snapshot,tasks}}),controller.signal),/Invalid inspection snapshot/);
      await rejected;
    }
  } finally {inspector.close();controller.abort();}
});

test('declared props preserve exact scalar previews and bound component and total records', async () => {
  const inspector = createInspectionServices(), controller = new AbortController(), requests = [];
  inspector.services.watches['ui.inspect.request']('', controller.signal, id => requests.push(id));
  const publish = inspector.services.tasks['ui.inspect.publish'];
  const value = {name:'count',kind:'uint64',value:'18446744073709551615',truncated:false};
  const record = {component:1,values:[value]};
  try {
    const pending = inspector.snapshot();
    await publish(JSON.stringify({version:1,request:requests.at(-1),snapshot:{...snapshot,props:[record]}}),controller.signal);
    assert.deepEqual((await pending).props,[record]);
    for (const props of [[{...record,component:0}],[{...record,values:null}],
      [{...record,values:[{...value,value:0}]}],[{...record,values:[{...value,name:''}]}],
      [{...record,values:[{...value,value:'x'.repeat(257)}]}],[{...record,values:Array(65).fill(value)}],
      Array(513).fill(record),Array.from({length:33},(_,index)=>({component:index+1,values:Array(64).fill(value)})),{}]) {
      const pending = inspector.snapshot(), rejected = assert.rejects(pending,/Invalid inspection snapshot/);
      await assert.rejects(publish(JSON.stringify({version:1,request:requests.at(-1),snapshot:{...snapshot,props}}),controller.signal),/Invalid inspection snapshot/);
      await rejected;
    }
  } finally {inspector.close();controller.abort();}
});
