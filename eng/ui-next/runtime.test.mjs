import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import test from 'node:test';
import { resolve } from 'node:path';
import init, { run } from '../../target/ui-next/wasm-runtime/vo_web.js';
import { InputQueue } from '../../lang/crates/vo-web/dist/ui_next/renderer.js';
import { TaskHost } from '../../lang/crates/vo-web/dist/ui_next/tasks.js';
import { pageWatches } from '../../lang/crates/vo-web/dist/ui_next/page.js';
import { MAX_QUEUED_BYTES, MAX_QUEUED_EVENTS, MAX_FRAME_BYTES } from '../../lang/crates/vo-web/dist/ui_next/generated/protocol.js';
import { decodeInputBatch, encodeEvent, encodeInputBatch, packInputBatch } from '../../lang/crates/vo-web/dist/ui_next/generated/codec.js';
import { root } from './server.mjs';

const eventOptions = { capture: false, altKey: false, ctrlKey: false, metaKey: false, shiftKey: false, repeat: false, isComposing: false, button: 0, pointerType: '', pointer: null, selectedValues: null };

test('page activity publishes native state changes and releases its document listeners', () => {
  const document = new EventTarget(), window = new EventTarget(), controller = new AbortController();
  let focused = true;
  Object.assign(document, { defaultView: window, visibilityState: 'visible', hasFocus: () => focused });
  const values = [], provider = pageWatches(document)['web.page-active'];
  provider('', controller.signal, value => values.push(value));
  window.dispatchEvent(new Event('focus'));
  assert.deepEqual(values, ['true']);
  focused = false;
  window.dispatchEvent(new Event('blur'));
  document.visibilityState = 'hidden';
  document.dispatchEvent(new Event('visibilitychange'));
  focused = true;
  window.dispatchEvent(new Event('focus'));
  assert.deepEqual(values, ['true', 'false'], 'a hidden page became active or repeated an unchanged notification');
  document.visibilityState = 'visible';
  document.dispatchEvent(new Event('visibilitychange'));
  assert.deepEqual(values, ['true', 'false', 'true']);
  controller.abort();
  focused = false;
  window.dispatchEvent(new Event('blur'));
  document.dispatchEvent(new Event('visibilitychange'));
  assert.deepEqual(values, ['true', 'false', 'true'], 'disposed page observer retained event listeners');
  assert.throws(() => provider('unexpected', new AbortController().signal, () => {}), /request data/);
});

test('generated guest UI contracts execute on Wasm VM', async () => {
  await init({ module_or_path: await readFile(resolve(root, 'target/ui-next/wasm-runtime/vo_web_bg.wasm')) });
  const vm = run(await readFile(resolve(root, 'target/ui-next/runtime.vob')));
  try {
    assert.equal(vm.status, 'ok', vm.stderr);
    assert.equal(vm.stdout, 'ui-next runtime contracts: ok\n');
  } finally { vm.free(); }
});

test('input queue batches native turns in order and isolates roots', async () => {
  const first = new InputQueue(), second = new InputQueue();
  const waiting = first.next();
  const event = sequence => ({ ...eventOptions, target: 1, kind: 'click', value: '', key: '', error: '', checked: false, sequence });
  first.push(event(1)); first.push(event(2)); first.push(event(3));
  const batch = decodeInputBatch(await waiting);
  assert.deepEqual(batch.events.map(event => event.sequence), [1, 2, 3]);
  first.close();
  second.push(event(1));
  assert.equal(decodeInputBatch(await second.next()).events.length, 1);
  second.close();
});

test('closing wakes the guest once and rejects concurrent consumers', async () => {
  const queue = new InputQueue();
  const waiting = queue.next();
  await assert.rejects(queue.next(), /multiple input consumers/);
  queue.close(); queue.close();
  assert.equal((await waiting).length, 0);
  assert.equal((await queue.next()).length, 0);
});

test('large input completions split into bounded FIFO frames and own their values', async t => {
  const queue = new InputQueue();
  t.after(() => queue.close());
  const event = { ...eventOptions, target: -1, kind: 'task', value: '界'.repeat(3 * 1024 * 1024), key: '', error: '', checked: false, sequence: 1, selectedValues: ['original'] };
  const first = { ...event, selectedValues: [...event.selectedValues] };
  queue.push(event);
  event.sequence = 2; event.value = 'x'.repeat(9 * 1024 * 1024); event.selectedValues[0] = 'second';
  const second = { ...event, selectedValues: [...event.selectedValues] };
  queue.push(event);
  event.value = 'mutated'; event.selectedValues[0] = 'mutated';
  queue.push({ ...event, sequence: 3 });
  assert.deepEqual(decodeInputBatch(await queue.next()).events, [first]);
  assert.deepEqual(decodeInputBatch(await queue.next()).events, [second, { ...event, sequence: 3 }]);
  // Draining also releases admission bytes, even while this root remains open.
  for (let index = 0; index < 9; index++) {
    queue.push(second);
    assert.equal(decodeInputBatch(await queue.next()).events[0].value.length, second.value.length);
  }
});


test('input frame boundary accounts for its array header and matches the canonical codec', async t => {
  const queue = new InputQueue();
  t.after(() => queue.close());
  const event = { ...eventOptions, target: 1, kind: 'input', value: '', key: '', error: '', checked: false, sequence: 1 };
  const emptySize = encodeInputBatch({ events: [event] }).length;
  event.value = 'a'.repeat(MAX_FRAME_BYTES - emptySize);
  const packed = packInputBatch([encodeEvent(event)]);
  assert.equal(packed.length, MAX_FRAME_BYTES);
  assert.deepEqual(packed, encodeInputBatch({ events: [event] }));
  queue.push(event);
  assert.equal((await queue.next()).length, MAX_FRAME_BYTES);
  const rejected = assert.rejects(queue.next(), /frame exceeds limit/);
  queue.push({ ...event, value: event.value + 'b' });
  await rejected;
});

test('queued input bytes are bounded independently of the event count', async t => {
  const queue = new InputQueue();
  t.after(() => queue.close());
  const event = { ...eventOptions, target: -1, kind: 'task', value: 'x'.repeat(9 * 1024 * 1024), key: '', error: '', checked: false, sequence: 1 };
  const count = Math.floor(MAX_QUEUED_BYTES / encodeEvent(event).length);
  assert(count < MAX_QUEUED_EVENTS);
  const rejected = assert.rejects(queue.next(), /queue exhausted/);
  for (let index = 0; index <= count; index++) queue.push(event);
  await rejected;
  await assert.rejects(queue.next(), /queue exhausted/);
});

test('native dispatch microtasks cannot publish a partial event turn', async t => {
  const queue = new InputQueue();
  t.after(() => queue.close());
  const event = sequence => ({ ...eventOptions, target: 1, kind: 'reset', value: '', key: '', error: '', checked: false, sequence });
  queue.push(event(1));
  let delivered = false;
  const waiting = queue.next().then(bytes => { delivered = true; return bytes; });
  await Promise.resolve();
  await Promise.resolve();
  assert.equal(delivered, false, 'guest ran before the native default action');
  queue.push(event(2));
  assert.deepEqual(decodeInputBatch(await waiting).events.map(event => event.sequence), [1, 2]);
});

test('input overload wakes the consumer with a stable failure', async () => {
  const queue = new InputQueue();
  const waiting = queue.next();
  const rejected = assert.rejects(waiting, /queue exhausted/);
  for (let sequence = 1; sequence <= MAX_QUEUED_EVENTS + 1; sequence++) {
    queue.push({ ...eventOptions, target: 1, kind: 'input', value: '', key: '', error: '', checked: false, sequence });
  }
  await rejected;
  await assert.rejects(queue.next(), /queue exhausted/);
  queue.close();
});

test('oversized native input rejects without leaving the guest suspended', async () => {
  const queue = new InputQueue();
  const rejected = assert.rejects(queue.next(), /frame exceeds limit/);
  queue.push({ ...eventOptions, target: 1, kind: 'input', value: 'x'.repeat(MAX_FRAME_BYTES), key: '', error: '', checked: false, sequence: 1 });
  await rejected;
  queue.close();
});

test('host cancellation drops late provider results even when abort is ignored', async () => {
  const pending = new Map(), delivered = [];
  const host = new TaskHost((...args) => delivered.push(args), {
    controlled: (value, signal) => new Promise(resolve => pending.set(value, { resolve, signal })),
  });
  const start = (id, value) => ({ op: 'start', id, name: 'controlled', value, timeoutMilliseconds: 0 });
  const cancel = id => ({ op: 'cancel', id, name: '', value: '', timeoutMilliseconds: 0 });
  const apply = commands => { host.prepare(commands); host.apply(commands); };
  apply([start(1, 'A')]);
  await new Promise(resolve => setImmediate(resolve));
  assert.throws(() => host.prepare([cancel(1), { ...start(2, 'invalid'), name: 'unknown' }]), /unknown UI task service "unknown"/);
  assert.equal(pending.get('A').signal.aborted, false, 'preflight changed an active task');
  apply([cancel(1), start(2, 'B')]);
  await new Promise(resolve => setImmediate(resolve));
  assert.equal(pending.get('A').signal.aborted, true);
  pending.get('B').resolve('new B');
  pending.get('A').resolve('old A');
  await new Promise(resolve => setImmediate(resolve));
  assert.deepEqual(delivered, [[2, 'new B', '']]);
  apply([start(3, 'C')]);
  await new Promise(resolve => setImmediate(resolve));
  host.close(); host.close();
  assert.equal(pending.get('C').signal.aborted, true);
  pending.get('C').resolve('after close');
  await new Promise(resolve => setImmediate(resolve));
  assert.equal(delivered.length, 1);
});

test('synchronous provider errors become one local result', async () => {
  const delivered = [];
  const host = new TaskHost((...args) => delivered.push(args), { failing() { throw new Error('provider failed'); } });
  const commands = [{ op: 'start', id: 1, name: 'failing', value: '', timeoutMilliseconds: 0 }];
  host.prepare(commands); host.apply(commands);
  await new Promise(resolve => setImmediate(resolve));
  assert.deepEqual(delivered, [[1, '', 'provider failed']]);
  assert.throws(() => host.prepare(commands), /stale/);
  host.close();
});

test('subscriptions retain delivery until cancellation and release partial installations', async () => {
  const sources = new Map(), delivered = [];
  const host = new TaskHost((...args) => delivered.push(args), {}, {
    source(value, signal, emit) { sources.set(value, { signal, emit }); emit('initial'); },
    broken(value, signal) { sources.set(value, { signal }); throw new Error('install failed'); },
  });
  const watch = (id, name, value) => ({ op: 'watch', id, name, value, timeoutMilliseconds: 0 });
  const apply = commands => { host.prepare(commands); host.apply(commands); };
  apply([watch(1, 'source', 'first')]);
  await new Promise(setImmediate);
  sources.get('first').emit('second');
  assert.deepEqual(delivered, [[1, 'initial', ''], [1, 'second', '']]);
  apply([{ op: 'cancel', id: 1, name: '', value: '', timeoutMilliseconds: 0 }, watch(2, 'source', 'replacement')]);
  await new Promise(setImmediate);
  assert.equal(sources.get('first').signal.aborted, true);
  sources.get('first').emit('late');
  assert.equal(delivered.length, 3);
  apply([watch(3, 'broken', 'partial')]);
  await new Promise(setImmediate);
  assert.equal(sources.get('partial').signal.aborted, true);
  assert.deepEqual(delivered.at(-1), [3, '', 'install failed']);
  apply([watch(4, 'source', 'terminal')]);
  await new Promise(setImmediate);
  sources.get('terminal').emit('', 'connection closed');
  assert.equal(sources.get('terminal').signal.aborted, true);
  sources.get('terminal').emit('late after error');
  assert.deepEqual(delivered.at(-1), [4, '', 'connection closed']);
  host.close();
  assert.equal(sources.get('replacement').signal.aborted, true);
  sources.get('replacement').emit('after close');
  assert.equal(delivered.length, 6);
});

test('request deadlines abort once; completion, cancellation and close retire timers', async t => {
  t.mock.timers.enable({ apis: ['setTimeout'] });
  const pending = new Map(), delivered = [];
  const host = new TaskHost((...args) => delivered.push(args), {
    controlled: (value, signal) => new Promise(resolve => pending.set(value, { resolve, signal })),
  });
  const start = (id, value, timeoutMilliseconds = 100) => ({ op: 'start', id, name: 'controlled', value, timeoutMilliseconds });
  const apply = commands => { host.prepare(commands); host.apply(commands); };
  for (const duration of [-1, NaN, 0.5, 2_147_483_648]) assert.throws(() => host.prepare([start(1, 'invalid', duration)]), /invalid/);
  apply([start(1, 'slow')]);
  await new Promise(setImmediate);
  t.mock.timers.tick(99);
  assert.deepEqual(delivered, []);
  t.mock.timers.tick(1);
  assert.deepEqual(delivered, [[1, '', 'Request timed out after 100 ms']]);
  assert.equal(pending.get('slow').signal.aborted, true);
  pending.get('slow').resolve('ignored late success');
  await new Promise(setImmediate);
  assert.equal(delivered.length, 1);
  apply([start(2, 'fast')]);
  await new Promise(setImmediate);
  pending.get('fast').resolve('success');
  await new Promise(setImmediate);
  t.mock.timers.tick(1000);
  assert.deepEqual(delivered[1], [2, 'success', '']);
  assert.equal(pending.get('fast').signal.aborted, false, 'completed operation had a live deadline');
  apply([start(3, 'cancelled')]);
  await new Promise(setImmediate);
  apply([{ op: 'cancel', id: 3, name: '', value: '', timeoutMilliseconds: 0 }]);
  assert.equal(pending.get('cancelled').signal.aborted, true);
  apply([start(4, 'closed')]);
  await new Promise(setImmediate);
  host.close();
  t.mock.timers.tick(1000);
  assert.equal(pending.get('closed').signal.aborted, true);
  assert.equal(delivered.length, 2);
  assert.throws(() => host.prepare([start(5, 'after close')]), /closed/);
});

test('latest pointer delivery coalesces only explicitly opted-in consecutive samples', async t => {
  const queue=new InputQueue();t.after(()=>queue.close());
  const point=sequence=>({...eventOptions,target:1,kind:'pointermove',value:'',key:'',error:'',checked:false,sequence,
    pointerType:'mouse',pointer:{id:1,clientX:sequence,clientY:0,buttons:1,pressure:0.5,isPrimary:true}});
  for(let sequence=1;sequence<=1000;sequence++)queue.push(point(sequence),true);
  assert.deepEqual(decodeInputBatch(await queue.next()).events.map(event=>event.sequence),[1000]);
  queue.push(point(1001),true);
  queue.push({...point(1002),kind:'click',pointer:null});
  queue.push(point(1003),true);queue.push(point(1004),true);
  queue.push({...point(1005),capture:true},true);
  queue.push(point(1006),true);
  queue.push({...point(1007),pointer:{...point(1007).pointer,id:2}},true);
  queue.push(point(1008));queue.push(point(1009));
  assert.deepEqual(decodeInputBatch(await queue.next()).events.map(event=>event.sequence),[1001,1002,1004,1005,1006,1007,1008,1009]);
});

test('FIFO backlog yields between bounded input turns without dropping events', async t => {
  const queue=new InputQueue();t.after(()=>queue.close());
  for(let sequence=1;sequence<=1000;sequence++)queue.push({...eventOptions,target:1,kind:'click',value:'',key:'',error:'',checked:false,sequence});
  const sequences=[];let batches=0;
  while(sequences.length<1000){
    const pending=queue.next();let completed=false;pending.then(()=>{completed=true;});
    await Promise.resolve();assert.equal(completed,false,'backlog did not yield to a host task');
    const events=decodeInputBatch(await pending).events;
    assert(events.length<=128);sequences.push(...events.map(event=>event.sequence));batches++;
  }
  assert.equal(batches,8);
  assert.deepEqual(sequences,Array.from({length:1000},(_,i)=>i+1));
});
