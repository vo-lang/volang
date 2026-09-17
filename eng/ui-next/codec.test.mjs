import assert from 'node:assert/strict';
import test from 'node:test';
import { encodeEvent, decodeEvent, encodeBatch, decodeBatch, encodeCommitAck, decodeCommitAck,
  encodeInputBatch, decodeInputBatch, encodeMutation, decodeMutation, encodeBootstrap, decodeBootstrap, encodePointerData, decodePointerData } from '../../lang/crates/vo-web/dist/ui_next/generated/codec.js';
import { MAX_FRAME_BYTES, MAX_QUEUED_EVENTS, MAX_SELECTED_VALUES, WIRE_VERSION } from '../../lang/crates/vo-web/dist/ui_next/generated/protocol.js';

const event = { target: 1, kind: '', value: '', key: '', checked: false, sequence: 0, error: '',
  capture: false, altKey: false, ctrlKey: false, metaKey: false, shiftKey: false, repeat: false, isComposing: false, button: 0, pointerType: '', pointer: null, selectedValues: null };
const mutation = { op: 'text', id: 1, parent: 0, before: 0, name: '', value: '' };

test('pointer snapshots preserve finite floats and validate optional record framing', () => {
  const pointer = { id: 42, clientX: -12.75, clientY: 19.125, buttons: 3, pressure: 0.625, isPrimary: true };
  const value = { ...event, kind: 'pointermove', pointer };
  assert.deepEqual(decodeEvent(encodeEvent(value)), value);
  assert.equal(encodeEvent(value).length - encodeEvent({ ...value, pointer: null }).length, 41);
  const absent = encodeEvent(event); absent[absent.length - 1] = 2;
  assert.throws(() => decodeEvent(absent), /boolean/);
  const encoded = encodeEvent(value);
  for (let index = 0; index < encoded.length; index++) assert.throws(() => decodeEvent(encoded.subarray(0,index)));
  for (const clientX of [0, -0, Number.MIN_VALUE, Number.MAX_VALUE, -Number.MAX_VALUE]) {
    assert(Object.is(decodePointerData(encodePointerData({ ...pointer, clientX })).clientX, clientX));
  }
  for (const clientX of [NaN, Infinity, -Infinity, null, undefined, '1']) {
    assert.throws(() => encodePointerData({ ...pointer, clientX }), /float/);
  }
  const bytes = encodePointerData(pointer);
  assert.deepEqual([...bytes.subarray(13,21)], [0,0,0,0,0,128,41,192], 'independent -12.75 IEEE754 little-endian fixture');
  for (const value of [NaN, Infinity, -Infinity]) {
    new DataView(bytes.buffer).setFloat64(13,value,true);
    assert.throws(() => decodePointerData(bytes), /float/);
  }
});

test('binary wire preserves Unicode, integers and explicit empty/nil collections', () => {
  const value = { ...event, target: -Number.MAX_SAFE_INTEGER, sequence: Number.MAX_SAFE_INTEGER,
    value: '\ufeff中文\u0000\n😀', checked: true, capture: true, ctrlKey: true, shiftKey: true, repeat: true, isComposing: true, button: 2, pointerType: 'pen' };
  assert.deepEqual(decodeEvent(encodeEvent(value)), value);
  for (const selectedValues of [null, [], ['', '中文', 'same', 'same']]) {
    assert.deepEqual(decodeEvent(encodeEvent({ ...value, selectedValues })), { ...value, selectedValues });
  }
  for (const events of [null, [], [value]]) assert.deepEqual(decodeInputBatch(encodeInputBatch({ events })), { events });
  const batch = { version: WIRE_VERSION, revision: 2, inputSequence: 1,
    mutations: [mutation, {...mutation,op:'property',name:'indeterminate',value:'true'}], commands: [{ op: 'start', id: 2, name: 'http', value: 'example', timeoutMilliseconds: 1500 }] };
  assert.deepEqual(decodeBatch(encodeBatch(batch)), batch);
  assert.equal(encodeMutation(batch.mutations[1])[5],17,'native property must append without renumbering existing operations');
  const scroll = {...mutation,op:'scrollPosition',name:'',value:'[-12.5,345.75]'};
  assert.equal(encodeMutation(scroll)[5],18,'scroll position must append without renumbering existing operations');
  assert.deepEqual(decodeMutation(encodeMutation(scroll)),scroll);
  const textSelection = {...mutation,op:'textSelection',value:JSON.stringify({source:'中文🙂',selection:{start:0,end:4,direction:'backward'}})};
  assert.equal(encodeMutation(textSelection)[5],19,'text selection must append without renumbering existing operations');
  assert.deepEqual(decodeMutation(encodeMutation(textSelection)),textSelection);
  const surrounded = new Uint8Array(encodeEvent(value).length + 6);
  surrounded.set(encodeEvent(value), 3);
  assert.deepEqual(decodeEvent(surrounded.subarray(3, -3)), value, 'byteOffset must be respected');
  // Same hand-authored fixture is consumed by the Vo contracts on all backends.
  const golden = Uint8Array.of(86, 85, 73, 25, 6, 123, 0, 0, 0, 0, 0, 0, 0);
  assert.deepEqual(encodeCommitAck({ revision: 123 }), golden);
  assert.deepEqual(decodeCommitAck(golden), { revision: 123 });
  for (const [revision, bytes] of [
    [5124095576030430, [222, 188, 154, 120, 86, 52, 18, 0]],
    [-5124095576030430, [34, 67, 101, 135, 169, 203, 237, 255]],
  ]) {
    const frame = Uint8Array.of(86, 85, 73, 25, 6, ...bytes);
    assert.deepEqual(encodeCommitAck({ revision }), frame);
    assert.deepEqual(decodeCommitAck(frame), { revision });
  }
  const initial = { data: '/studio/docs?topic=state&locale=中文' };
  assert.deepEqual(decodeBootstrap(encodeBootstrap(initial)), initial);
  assert.throws(() => decodeBootstrap(encodeCommitAck({ revision: 1 })), /header/);
});

test('binary wire rejects malformed frames before exposing a value', () => {
  const encoded = encodeEvent(event);
  for (let end = 0; end < encoded.length; end++) assert.throws(() => decodeEvent(encoded.subarray(0, end)));
  assert.throws(() => decodeEvent(Uint8Array.from([...encoded, 0])), /trailing/);
  assert.throws(() => decodeBatch(encoded), /header/);
  const old = encoded.slice(); old[3]--;
  assert.throws(() => decodeEvent(old), /header/);
  const bool = encoded.slice(); bool[25] = 2;
  assert.throws(() => decodeEvent(bool), /boolean/);
  const unsafe = encoded.slice(); unsafe.fill(255, 5, 13); unsafe[12] = 127;
  assert.throws(() => decodeEvent(unsafe), /integer/);
  const operation = encodeMutation(mutation); operation[5] = 0;
  assert.throws(() => decodeMutation(operation), /operation/);
  const count = encodeBatch({ version: WIRE_VERSION, revision: 1, inputSequence: 0, mutations: [], commands: null });
  new DataView(count.buffer).setUint32(29, 200_001, true);
  assert.throws(() => decodeBatch(count), /array count/);
  const string = encoded.slice(); string.fill(255, 13, 17);
  assert.throws(() => decodeEvent(string), /truncated/);
});

test('binary writers enforce number, type, count and byte limits', () => {
  for (const target of [NaN, Infinity, 1.5, Number.MAX_SAFE_INTEGER + 1]) {
    assert.throws(() => encodeEvent({ ...event, target }), /integer/);
  }
  assert.throws(() => encodeEvent({ ...event, checked: 1 }), /boolean/);
  assert.throws(() => encodeEvent({ ...event, value: null }), /string/);
  assert.throws(() => encodeEvent({ ...event, selectedValues: Array(MAX_SELECTED_VALUES + 1).fill('') }), /array count/);
  assert.throws(() => encodeEvent({ ...event, selectedValues: [null] }), /string/);
  assert.throws(() => encodeMutation({ ...mutation, op: 'unknown' }), /operation/);
  assert.throws(() => encodeInputBatch({ events: Array(MAX_QUEUED_EVENTS + 1).fill(event) }), /array count/);
  const budget = MAX_FRAME_BYTES - encodeEvent(event).length;
  assert.equal(encodeEvent({ ...event, value: 'x'.repeat(budget) }).length, MAX_FRAME_BYTES);
  assert.throws(() => encodeEvent({ ...event, value: 'x'.repeat(budget + 1) }), /exceeds limit/);
  assert.throws(() => encodeEvent({ ...event, value: '中'.repeat(Math.floor(budget / 3) + 1) }), /exceeds limit/);
});
