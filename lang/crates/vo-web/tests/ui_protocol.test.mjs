import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import test from 'node:test';

import {
  AotUiHost,
  createUiWebImports,
  decodeUiEvent,
  decodeUiMutationBatch,
  decodeUiSystemRequest,
  decodeUiSystemResponse,
  encodeUiEvent,
  encodeUiSystemRequest,
  encodeUiSystemResponse,
  UiBrowserSystemHost,
  UiDomAdapter,
  UiVmDomSession,
} from '../dist/index.js';

class Bytes {
  values = [];

  u8(value) {
    this.values.push(value & 0xff);
  }

  u16(value) {
    this.u8(value);
    this.u8(value >>> 8);
  }

  u32(value) {
    this.u16(value);
    this.u16(value >>> 16);
  }

  u64(value) {
    let remaining = BigInt.asUintN(64, value);
    for (let index = 0; index < 8; index += 1) {
      this.u8(Number(remaining & 0xffn));
      remaining >>= 8n;
    }
  }

  f32(value) {
    const bytes = new Uint8Array(4);
    new DataView(bytes.buffer).setFloat32(0, value, true);
    this.raw(bytes);
  }

  identity(index, generation) {
    this.u32(index);
    this.u32(generation);
  }

  raw(bytes) {
    this.values.push(...bytes);
  }

  text(value) {
    const bytes = new TextEncoder().encode(value);
    this.u32(bytes.byteLength);
    this.raw(bytes);
  }

  finish() {
    return Uint8Array.from(this.values);
  }
}

function mutationFrame() {
  const bytes = new Bytes();
  bytes.raw(new TextEncoder().encode('VUI1'));
  bytes.u64(9n);
  bytes.u64(1n);
  bytes.u32(7);

  bytes.u8(1);
  bytes.identity(1, 1);
  bytes.u8(1);
  bytes.u16(4);

  bytes.u8(1);
  bytes.identity(2, 1);
  bytes.u8(2);

  bytes.u8(2);
  bytes.identity(2, 1);
  bytes.text('你好');

  bytes.u8(3);
  bytes.identity(1, 1);
  bytes.u32(8);
  bytes.u8(6);
  bytes.u8(1);
  bytes.f32(12.5);

  bytes.u8(5);
  bytes.identity(1, 1);
  bytes.u16(1);
  bytes.identity(7, 3);
  bytes.u8(5);

  bytes.u8(7);
  bytes.identity(0, 1);
  bytes.identity(1, 1);
  bytes.u8(0);

  bytes.u8(7);
  bytes.identity(1, 1);
  bytes.identity(2, 1);
  bytes.u8(0);
  return bytes.finish();
}

function primitiveFrame(primitive) {
  const bytes = new Bytes();
  bytes.raw(new TextEncoder().encode('VUI1'));
  bytes.u64(1n);
  bytes.u64(1n);
  bytes.u32(1);
  bytes.u8(1);
  bytes.identity(1, 1);
  bytes.u8(1);
  bytes.u16(primitive);
  return bytes.finish();
}

function systemReadClipboardFrame(requestId = 7n, format = 1) {
  const bytes = new Bytes();
  bytes.raw(new TextEncoder().encode('VUS1'));
  bytes.u8(1);
  bytes.u8(1);
  bytes.u16(0);
  bytes.u64(requestId);
  bytes.u8(format);
  return bytes.finish();
}

function systemWriteTextFrame(requestId = 8n, value = 'copied') {
  const bytes = new Bytes();
  bytes.raw(new TextEncoder().encode('VUS1'));
  bytes.u8(1);
  bytes.u8(2);
  bytes.u16(0);
  bytes.u64(requestId);
  bytes.u8(1);
  bytes.text(value);
  return bytes.finish();
}

test('VUS1 browser decoder and encoder preserve independent system frames', () => {
  assert.deepEqual(decodeUiSystemRequest(systemReadClipboardFrame()), {
    requestId: 7n,
    type: 'read-clipboard',
    format: 1,
  });
  assert.deepEqual(decodeUiSystemRequest(systemWriteTextFrame()), {
    requestId: 8n,
    type: 'write-clipboard',
    content: { type: 'text', text: 'copied' },
  });
  const response = encodeUiSystemResponse(7n, {
    type: 'clipboard',
    content: { type: 'text', text: 'browser value' },
  });
  assert.equal(new TextDecoder().decode(response.subarray(0, 4)), 'VUS1');
  const view = new DataView(response.buffer, response.byteOffset, response.byteLength);
  assert.equal(view.getUint8(4), 2);
  assert.equal(view.getUint8(5), 2);
  assert.equal(view.getBigUint64(8, true), 7n);
  assert.throws(
    () => decodeUiSystemRequest(Uint8Array.from([...systemReadClipboardFrame(), 0])),
    /trailing bytes/,
  );
  const drag = {
    requestId: 9n,
    type: 'begin-file-drag',
    mode: 2,
    paths: ['/tmp/alpha.vo', '/tmp/beta.vo'],
    preview: '/tmp/preview.png',
  };
  assert.deepEqual(decodeUiSystemRequest(encodeUiSystemRequest(drag)), drag);
  const invocation = {
    requestId: 12n,
    type: 'invoke-host',
    service: 'volang.studio.host.v1',
    operation: 'projects.list',
    payload: Uint8Array.of(0, 1, 255),
  };
  assert.deepEqual(decodeUiSystemRequest(encodeUiSystemRequest(invocation)), invocation);
  assert.deepEqual(decodeUiSystemResponse(encodeUiSystemResponse(12n, {
    type: 'host-payload', payload: Uint8Array.of(7, 8),
  })), { requestId: 12n, response: { type: 'host-payload', payload: Uint8Array.of(7, 8) } });
});

test('VUI1 decoder follows the Rust wire layout exactly', () => {
  const batch = decodeUiMutationBatch(mutationFrame());
  assert.equal(batch.sessionEpoch, 9n);
  assert.equal(batch.revision, 1n);
  assert.deepEqual(batch.mutations, [
    { type: 'create-element', id: { index: 1, generation: 1 }, primitive: 4 },
    { type: 'create-text', id: { index: 2, generation: 1 } },
    { type: 'set-text', id: { index: 2, generation: 1 }, text: '你好' },
    {
      type: 'set-property',
      id: { index: 1, generation: 1 },
      property: 8,
      value: { type: 'length', value: { unit: 'px', value: 12.5 } },
    },
    {
      type: 'listen',
      id: { index: 1, generation: 1 },
      listener: {
        event: 1,
        handler: { index: 7, generation: 3 },
        capture: true,
        passive: false,
        once: true,
      },
    },
    {
      type: 'insert-before',
      parent: { index: 0, generation: 1 },
      child: { index: 1, generation: 1 },
    },
    {
      type: 'insert-before',
      parent: { index: 1, generation: 1 },
      child: { index: 2, generation: 1 },
    },
  ]);
});

test('VUI1 decoder rejects every truncation and trailing data', () => {
  const frame = mutationFrame();
  for (let length = 0; length < frame.byteLength; length += 1) {
    assert.throws(() => decodeUiMutationBatch(frame.subarray(0, length)), /Volang UI|invalid/);
  }
  const trailing = new Uint8Array(frame.byteLength + 1);
  trailing.set(frame);
  assert.throws(() => decodeUiMutationBatch(trailing), /trailing bytes/);
});

test('VUI1 decoder accepts text areas and rejects future primitives', () => {
  assert.equal(decodeUiMutationBatch(primitiveFrame(16)).mutations[0].primitive, 16);
  assert.throws(() => decodeUiMutationBatch(primitiveFrame(17)), /primitive 17/);
});

test('VUE1 encoder preserves signed scalar bits and identities', () => {
  const frame = encodeUiEvent({
    sessionEpoch: 0x0102_0304_0506_0708n,
    handler: { index: 7, generation: 3 },
    event: 12,
    target: { index: 9, generation: 4 },
    sequence: 11n,
    payload: { type: 'scalar', value: -2n },
  });
  assert.equal(new TextDecoder().decode(frame.subarray(0, 4)), 'VUE1');
  const view = new DataView(frame.buffer, frame.byteOffset, frame.byteLength);
  assert.equal(view.getBigUint64(4, true), 0x0102_0304_0506_0708n);
  assert.equal(view.getUint32(12, true), 7);
  assert.equal(view.getUint32(16, true), 3);
  assert.equal(view.getUint16(20, true), 12);
  assert.equal(view.getUint32(22, true), 9);
  assert.equal(view.getUint32(26, true), 4);
  assert.equal(view.getBigUint64(30, true), 11n);
  assert.equal(view.getUint8(38), 3);
  assert.equal(view.getBigInt64(39, true), -2n);
  assert.equal(frame.byteLength, 47);
});

test('VUE1 encoder carries typed key and pointer payloads', () => {
  const base = {
    sessionEpoch: 5n,
    handler: { index: 2, generation: 1 },
    event: 7,
    target: { index: 3, generation: 1 },
    sequence: 6n,
  };
  const key = encodeUiEvent({
    ...base,
    payload: {
      type: 'key',
      key: 'A',
      code: 'KeyA',
      modifiers: { shift: true, control: false, alt: true, meta: false },
      repeat: true,
      composing: true,
    },
  });
  const keyView = new DataView(key.buffer, key.byteOffset, key.byteLength);
  assert.equal(keyView.getUint8(38), 5);
  assert.equal(keyView.getUint32(39, true), 1);
  assert.equal(new TextDecoder().decode(key.subarray(43, 44)), 'A');
  assert.equal(keyView.getUint32(44, true), 4);
  assert.equal(new TextDecoder().decode(key.subarray(48, 52)), 'KeyA');
  assert.equal(keyView.getUint8(52), 0b11_0101);

  const pointer = encodeUiEvent({
    ...base,
    event: 10,
    payload: {
      type: 'pointer',
      x: 12.25,
      y: -8.5,
      button: -1,
      buttons: 5,
      pointerId: 99n,
      kind: 'pen',
      modifiers: { shift: false, control: true, alt: false, meta: true },
    },
  });
  const pointerView = new DataView(pointer.buffer, pointer.byteOffset, pointer.byteLength);
  assert.equal(pointerView.getUint8(38), 6);
  assert.equal(pointerView.getFloat64(39, true), 12.25);
  assert.equal(pointerView.getFloat64(47, true), -8.5);
  assert.equal(pointerView.getInt16(55, true), -1);
  assert.equal(pointerView.getUint16(57, true), 5);
  assert.equal(pointerView.getBigInt64(59, true), 99n);
  assert.equal(pointerView.getUint8(67), 2);
  assert.equal(pointerView.getUint8(68), 0b1010);
  assert.equal(pointer.byteLength, 69);

  const scroll = encodeUiEvent({
    ...base,
    event: 16,
    payload: {
      type: 'scroll',
      x: 1.5,
      y: 2.5,
      deltaX: -3,
      deltaY: 4,
      unit: 'line',
      modifiers: { shift: true, control: false, alt: false, meta: false },
    },
  });
  const scrollView = new DataView(scroll.buffer, scroll.byteOffset, scroll.byteLength);
  assert.equal(scrollView.getUint8(38), 7);
  assert.equal(scrollView.getFloat64(39, true), 1.5);
  assert.equal(scrollView.getFloat64(47, true), 2.5);
  assert.equal(scrollView.getFloat64(55, true), -3);
  assert.equal(scrollView.getFloat64(63, true), 4);
  assert.equal(scrollView.getUint8(71), 1);
  assert.equal(scrollView.getUint8(72), 1);
  assert.equal(scroll.byteLength, 73);

  const composition = encodeUiEvent({
    ...base,
    event: 14,
    payload: {
      type: 'composition',
      value: '拼音',
      selectionStartUtf16: 1,
      selectionLengthUtf16: 2,
    },
  });
  const compositionView = new DataView(
    composition.buffer,
    composition.byteOffset,
    composition.byteLength,
  );
  assert.equal(compositionView.getUint8(38), 8);
  assert.equal(compositionView.getUint32(39, true), 6);
  assert.equal(new TextDecoder().decode(composition.subarray(43, 49)), '拼音');
  assert.equal(compositionView.getUint32(49, true), 1);
  assert.equal(compositionView.getUint32(53, true), 2);
  assert.equal(composition.byteLength, 57);
});

test('VUE1 decoder round-trips every typed event payload', () => {
  const base = {
    sessionEpoch: 17n,
    handler: { index: 8, generation: 3 },
    event: 14,
    target: { index: 9, generation: 4 },
    sequence: 22n,
  };
  const payloads = [
    { type: 'none' },
    { type: 'text', value: 'Ada' },
    { type: 'toggle', value: true },
    { type: 'scalar', value: -42n },
    { type: 'bytes', value: Uint8Array.of(0, 1, 255) },
    {
      type: 'key', key: 'A', code: 'KeyA',
      modifiers: { shift: true, control: false, alt: true, meta: false },
      repeat: true, composing: false,
    },
    {
      type: 'pointer', x: 1.25, y: -2.5, button: -1, buttons: 3, pointerId: 7n,
      kind: 'pen', modifiers: { shift: false, control: true, alt: false, meta: true },
    },
    {
      type: 'scroll', x: 3, y: 4, deltaX: -5, deltaY: 6, unit: 'page',
      modifiers: { shift: true, control: true, alt: false, meta: false },
    },
    { type: 'composition', value: '拼音', selectionStartUtf16: 1, selectionLengthUtf16: 2 },
    { type: 'text-input', value: 'a🙂b', selectionStartUtf16: 3, selectionLengthUtf16: 0 },
  ];
  for (const payload of payloads) {
    const envelope = { ...base, payload };
    assert.deepEqual(decodeUiEvent(encodeUiEvent(envelope)), envelope);
  }
});

test('VUE1 encoder rejects out-of-range and non-finite host values', () => {
  const base = {
    sessionEpoch: 1n,
    handler: { index: 1, generation: 1 },
    event: 9,
    target: { index: 2, generation: 1 },
    sequence: 1n,
  };
  assert.throws(() => encodeUiEvent({
    ...base,
    payload: {
      type: 'pointer',
      x: Number.NaN,
      y: 0,
      button: 0,
      buttons: 0,
      pointerId: 1n,
      kind: 'mouse',
      modifiers: { shift: false, control: false, alt: false, meta: false },
    },
  }), /finite/);
  assert.throws(() => encodeUiEvent({
    ...base,
    payload: { type: 'scalar', value: 0x8000_0000_0000_0000n },
  }), /i64/);
});

class FakeStyle {
  setProperty(name, value) {
    this[name] = String(value);
  }

  removeProperty(name) {
    const key = name.startsWith('--')
      ? name
      : name.replace(/-([a-z])/g, (_, character) => character.toUpperCase());
    this[key] = '';
  }
}

class FakeNode {
  constructor(ownerDocument, nodeType) {
    this.ownerDocument = ownerDocument;
    this.nodeType = nodeType;
    this.nodeValue = null;
    this.parentNode = null;
    this.childNodes = [];
  }

  insertBefore(child, before) {
    if (this.ownerDocument.failNextInsert) {
      this.ownerDocument.failNextInsert = false;
      throw new Error('injected DOM insert failure');
    }
    const active = this.ownerDocument.activeElement;
    if (
      child.parentNode !== null
      && active !== null
      && (child === active || (typeof child.contains === 'function' && child.contains(active)))
    ) {
      // Moving an attached focused subtree mirrors browser focus loss. This
      // makes no-op DOM reordering observable in the protocol regression suite.
      this.ownerDocument.activeElement = null;
    }
    if (child.parentNode) child.parentNode.removeChild(child);
    const index = before === null ? this.childNodes.length : this.childNodes.indexOf(before);
    if (index < 0) throw new Error('missing before node');
    this.childNodes.splice(index, 0, child);
    child.parentNode = this;
    return child;
  }

  appendChild(child) {
    return this.insertBefore(child, null);
  }

  removeChild(child) {
    const index = this.childNodes.indexOf(child);
    if (index < 0) throw new Error('missing child');
    this.childNodes.splice(index, 1);
    child.parentNode = null;
    return child;
  }

  replaceChildren(...children) {
    for (const child of this.childNodes) child.parentNode = null;
    this.childNodes = [];
    for (const child of children) this.appendChild(child);
  }

  get nextSibling() {
    if (!this.parentNode) return null;
    const index = this.parentNode.childNodes.indexOf(this);
    return this.parentNode.childNodes[index + 1] ?? null;
  }

  remove() {
    this.parentNode?.removeChild(this);
  }
}

class FakeText extends FakeNode {
  constructor(ownerDocument, value) {
    super(ownerDocument, 3);
    this.nodeValue = value;
  }
}

class FakeComment extends FakeNode {
  constructor(ownerDocument, value) {
    super(ownerDocument, 8);
    this.data = value;
  }
}

class FakeElement extends FakeNode {
  constructor(ownerDocument, tagName) {
    super(ownerDocument, 1);
    this.tagName = tagName.toUpperCase();
    this.style = new FakeStyle();
    this.attributes = new Map();
    this.listeners = new Map();
    this.scrollLeft = 0;
    this.scrollTop = 0;
    this.value = '';
    this.checked = false;
    this.disabled = false;
    this.required = false;
    this.placeholder = '';
    this.selectionStart = 0;
    this.selectionEnd = 0;
    this.type = '';
    this.capturedPointers = [];
  }

  setAttribute(name, value) {
    this.attributes.set(name, String(value));
  }

  removeAttribute(name) {
    this.attributes.delete(name);
  }

  hasAttribute(name) {
    return this.attributes.has(name);
  }

  getAttribute(name) {
    return this.attributes.get(name) ?? null;
  }

  querySelectorAll(selector) {
    if (selector !== '[data-volang-node]') throw new Error(`unsupported selector ${selector}`);
    const matches = [];
    const visit = (node) => {
      for (const child of node.childNodes) {
        if (child.nodeType === 1 && child.hasAttribute('data-volang-node')) matches.push(child);
        visit(child);
      }
    };
    visit(this);
    return matches;
  }

  toggleAttribute(name, force) {
    if (force) this.attributes.set(name, '');
    else this.attributes.delete(name);
  }

  setSelectionRange(start, end) {
    this.selectionWrites = (this.selectionWrites ?? 0) + 1;
    this.selectionStart = start;
    this.selectionEnd = end;
  }

  setPointerCapture(pointerId) {
    this.capturedPointers.push(pointerId);
  }

  contains(node) {
    let current = node;
    while (current) {
      if (current === this) return true;
      current = current.parentNode;
    }
    return false;
  }

  focus() {
    this.ownerDocument.activeElement = this;
  }

  getBoundingClientRect() {
    return {
      x: 0,
      y: 0,
      width: Number.parseFloat(this.style.width) || 0,
      height: Number.parseFloat(this.style.height) || 0,
    };
  }

  get isConnected() {
    return this.parentNode !== null;
  }

  addEventListener(name, callback) {
    let listeners = this.listeners.get(name);
    if (!listeners) {
      listeners = new Set();
      this.listeners.set(name, listeners);
    }
    listeners.add(callback);
  }

  removeEventListener(name, callback) {
    const listeners = this.listeners.get(name);
    listeners?.delete(callback);
    if (listeners?.size === 0) this.listeners.delete(name);
  }

  dispatch(name, event = {}) {
    for (const listener of this.listeners.get(name) ?? []) {
      listener({ target: this, ...event, currentTarget: this });
    }
  }
}

class FakeDocument {
  failNextInsert = false;
  activeElement = null;
  listeners = new Map();

  constructor(defaultView) {
    this.defaultView = defaultView;
  }

  addEventListener(name, callback) {
    let listeners = this.listeners.get(name);
    if (!listeners) {
      listeners = new Set();
      this.listeners.set(name, listeners);
    }
    listeners.add(callback);
  }

  removeEventListener(name, callback) {
    const listeners = this.listeners.get(name);
    listeners?.delete(callback);
    if (listeners?.size === 0) this.listeners.delete(name);
  }

  dispatch(name, event = {}) {
    for (const listener of this.listeners.get(name) ?? []) {
      listener({ target: this, ...event, currentTarget: this });
    }
  }

  createElement(tagName) {
    return new FakeElement(this, tagName);
  }

  createTextNode(value) {
    return new FakeText(this, value);
  }

  createComment(value) {
    return new FakeComment(this, value);
  }

  createTreeWalker(root, whatToShow) {
    if (whatToShow !== 128) throw new Error('fake tree walker only supports comments');
    const comments = [];
    const visit = (node) => {
      for (const child of node.childNodes) {
        if (child.nodeType === 8) comments.push(child);
        visit(child);
      }
    };
    visit(root);
    let index = 0;
    return { nextNode: () => comments[index++] ?? null };
  }
}

class FakeWindow {
  listeners = new Map();
  backCalls = 0;
  forwardCalls = 0;
  innerWidth = 1024;
  innerHeight = 768;
  devicePixelRatio = 1;

  constructor(value = 'https://example.test/') {
    this.location = new URL(value);
    this.history = {
      pushState: (_state, _unused, url) => { this.location = new URL(String(url)); },
      replaceState: (_state, _unused, url) => { this.location = new URL(String(url)); },
      back: () => { this.backCalls += 1; },
      forward: () => { this.forwardCalls += 1; },
    };
  }

  addEventListener(name, callback) {
    this.listeners.set(name, callback);
  }

  removeEventListener(name, callback) {
    if (this.listeners.get(name) === callback) this.listeners.delete(name);
  }

  dispatchPop(path) {
    this.location = new URL(path, this.location);
    this.listeners.get('popstate')?.();
  }

  dispatchResize(width, height, scaleFactor = 1) {
    this.innerWidth = width;
    this.innerHeight = height;
    this.devicePixelRatio = scaleFactor;
    this.listeners.get('resize')?.();
  }
}

test('browser system host reports native file drag sources as unsupported', async () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const host = new UiBrowserSystemHost(root);
  const frame = encodeUiSystemRequest({
    requestId: 10n,
    type: 'begin-file-drag',
    mode: 1,
    paths: ['/tmp/demo.vo'],
  });
  assert.deepEqual(decodeUiSystemResponse(await host.execute(frame)), {
    requestId: 10n,
    response: {
      type: 'failure',
      kind: 2,
      message: 'native file drag sources are unavailable in browsers',
    },
  });
  host.dispose();
});

test('browser system host delegates versioned application invocations', async () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const host = new UiBrowserSystemHost(root, {
    invokeHost: async (service, operation, payload) => {
      assert.equal(service, 'volang.studio.host.v1');
      assert.equal(operation, 'health');
      return Uint8Array.of(...payload, 9);
    },
  });
  const frame = encodeUiSystemRequest({
    requestId: 11n, type: 'invoke-host', service: 'volang.studio.host.v1',
    operation: 'health', payload: Uint8Array.of(7, 8),
  });
  assert.deepEqual(decodeUiSystemResponse(await host.execute(frame)), {
    requestId: 11n, response: { type: 'host-payload', payload: Uint8Array.of(7, 8, 9) },
  });
  host.dispose();
});

test('Core-Wasm AOT UI host commits a root and returns a guest handler identity', async () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const commits = [];
  const host = new AotUiHost(root, {
    onCommit: (revision, mutationCount) => commits.push({ revision, mutationCount }),
    systemHost: {
      execute: async (frame) => {
        const request = decodeUiSystemRequest(frame);
        assert.equal(request.type, 'read-clipboard');
        return encodeUiSystemResponse(request.requestId, {
          type: 'clipboard', content: { type: 'text', text: 'aot clipboard' },
        });
      },
    },
  });
  const externs = host.externs();
  const memory = new WebAssembly.Memory({ initial: 1 });
  const data = new DataView(memory.buffer);
  let heap = 4096;
  const allocateString = (value) => {
    const encoded = new TextEncoder().encode(value);
    const header = heap;
    const bytes = header + 16;
    heap += 16 + encoded.byteLength;
    data.setBigUint64(header, BigInt(encoded.byteLength), true);
    data.setBigUint64(header + 8, BigInt(bytes), true);
    new Uint8Array(memory.buffer, bytes, encoded.byteLength).set(encoded);
    return BigInt(header);
  };
  const readString = (reference) => {
    if (reference === 0n) return '';
    const header = Number(reference);
    const length = Number(data.getBigUint64(header, true));
    const pointer = Number(data.getBigUint64(header + 8, true));
    return new TextDecoder().decode(new Uint8Array(memory.buffer, pointer, length));
  };
  const allocateViewSlice = (handles) => {
    const header = heap;
    const values = header + 40;
    heap += 40 + handles.length * 8;
    data.setBigUint64(header, BigInt(values), true);
    data.setBigUint64(header + 8, BigInt(handles.length), true);
    data.setBigUint64(header + 16, BigInt(handles.length), true);
    data.setBigUint64(header + 24, 8n, true);
    data.setBigUint64(header + 32, 0n, true);
    handles.forEach((handle, index) => data.setBigUint64(values + index * 8, handle, true));
    return BigInt(header);
  };
  const floatBits = (value) => {
    const bytes = new ArrayBuffer(8);
    const view = new DataView(bytes);
    view.setFloat64(0, value, true);
    return view.getBigUint64(0, true);
  };
  const invoke = (name, args, destination, packageName = 'github.com/vo-lang/ui') => {
    args.forEach((value, index) => data.setBigUint64(index * 8, BigInt.asUintN(64, value), true));
    const call = {
      descriptor: {}, name, externId: 0, memory, frame: 0,
      destination, argumentsStart: 0, argumentSlots: args.length, args: [],
      readSlot: (slot) => data.getBigUint64(slot * 8, true),
      writeSlot: (slot, value) => data.setBigUint64(slot * 8, BigInt.asUintN(64, value), true),
      readFloat64: (slot) => data.getFloat64(slot * 8, true),
      writeFloat64: (slot, value) => data.setFloat64(slot * 8, value, true),
      readString,
      readStringBytes: (reference) => new TextEncoder().encode(readString(reference)),
      readStringSlice: () => [],
      readByteSlice: () => new Uint8Array(),
      allocateString,
      allocateStringBytes: (value) => allocateString(new TextDecoder().decode(value)),
      allocateStringSlice: () => 0n,
      allocateByteSlice: () => 0n,
      clearError: (slot) => {
        data.setBigUint64(slot * 8, 0n, true);
        data.setBigUint64((slot + 1) * 8, 0n, true);
      },
      writeError: (_slot, message) => { throw new Error(message); },
    };
    const key = `vo1:${new TextEncoder().encode(packageName).byteLength}:${packageName}`
      + `:${new TextEncoder().encode(name).byteLength}:${name}`;
    return externs[key].handler(call);
  };

  await invoke('runtimeReadClipboard', [1n], 40, 'github.com/vo-lang/ui/system');
  assert.equal(data.getBigUint64(40 * 8, true), 1n);
  assert.equal(readString(data.getBigUint64(41 * 8, true)), 'aot clipboard');
  assert.equal(data.getBigUint64(46 * 8, true), 1n);
  assert.equal(data.getBigUint64(47 * 8, true), 0n);

  invoke('LocationPath', [], 20);
  assert.equal(readString(data.getBigUint64(20 * 8, true)), '/');
  invoke('Navigate', [allocateString('/settings?tab=profile')], 20);
  invoke('LocationPath', [], 20);
  assert.equal(readString(data.getBigUint64(20 * 8, true)), '/settings?tab=profile');
  assert.throws(() => invoke('Navigate', [allocateString('//example.com')], 20), /invalid/);

  invoke('runtimeBegin', [1n], 20);
  invoke('runtimeButton', [allocateString('Save'), 7n], 20);
  invoke('FlowDirection', [data.getBigUint64(20 * 8, true), 1n], 20);
  invoke('BorderColor', [data.getBigUint64(20 * 8, true), 0xff63_7effn], 20);
  invoke('BorderWidth', [data.getBigUint64(20 * 8, true), floatBits(2)], 20);
  invoke('HoverBackground', [data.getBigUint64(20 * 8, true), 0xff22_3344n], 20);
  invoke('PressedBackground', [data.getBigUint64(20 * 8, true), 0xff33_4455n], 20);
  invoke('FocusRing', [data.getBigUint64(20 * 8, true), 0xff44_5566n], 20);
  invoke('Elevation', [data.getBigUint64(20 * 8, true), 3n], 20);
  const handle = data.getBigUint64(20 * 8, true);
  const waiting = invoke('runtimeCommitAndWait', [handle, 1n], 24);
  assert.equal(commits.length, 1);
  assert.equal(commits[0].revision, 1n);
  assert.ok(commits[0].mutationCount > 0);
  assert.equal(root.childNodes[0].tagName, 'BUTTON');
  assert.equal(root.childNodes[0].dir, 'rtl');
  assert.equal(root.childNodes[0].style.borderColor, '#637effff');
  assert.equal(root.childNodes[0].style.borderWidth, '2px');
  assert.equal(root.childNodes[0].style['--volang-hover-background'], '#223344ff');
  assert.equal(root.childNodes[0].style['--volang-pressed-background'], '#334455ff');
  assert.equal(root.childNodes[0].style['--volang-focus-ring'], '#445566ff');
  assert.match(root.childNodes[0].style.boxShadow, /0 8px 20px/);
  assert.equal(root.childNodes[0].childNodes[0].nodeValue, 'Save');
  root.childNodes[0].dispatch('click');
  assert.equal(await waiting, 0);
  assert.equal(data.getBigUint64(24 * 8, true), 7n);
  assert.equal(data.getBigUint64(25 * 8, true), 1n);

  // Synchronous state helpers may call Invalidate while the guest is already
  // handling this event. The following render owns that state, so it must not
  // leave a second wake that races with the next browser event.
  invoke('Invalidate', [], 20);

  const stableButton = root.childNodes[0];
  invoke('runtimeBegin', [0n], 20);
  invoke('runtimeButton', [allocateString('Saved'), 8n], 20);
  const nextHandle = data.getBigUint64(20 * 8, true);
  const nextWaiting = invoke('runtimeCommitAndWait', [nextHandle, 0n], 24);
  assert.equal(commits.at(-1).revision, 2n);
  assert.ok(commits.at(-1).mutationCount > 0);
  assert.equal(root.childNodes[0], stableButton);
  assert.equal(root.childNodes[0].childNodes[0].nodeValue, 'Saved');
  root.childNodes[0].dispatch('click');
  assert.equal(await nextWaiting, 0);
  assert.equal(data.getBigUint64(24 * 8, true), 8n);
  assert.equal(data.getBigUint64(25 * 8, true), 1n);

  invoke('runtimeBegin', [0n], 20);
  invoke('runtimeButton', [allocateString('Idle'), 9n], 20);
  const idleHandle = data.getBigUint64(20 * 8, true);
  const invalidated = invoke('runtimeCommitAndWait', [idleHandle, 0n], 24);
  invoke('Invalidate', [], 20);
  assert.equal(await invalidated, 0);
  assert.equal(data.getBigUint64(24 * 8, true), 0xffff_ffffn);
  assert.equal(data.getBigUint64(25 * 8, true), 17n);

  const keyedButton = (label, handler, key) => {
    invoke('runtimeButton', [allocateString(label), BigInt(handler)], 20);
    invoke('Key', [data.getBigUint64(20 * 8, true), allocateString(key)], 20);
    return data.getBigUint64(20 * 8, true);
  };
  invoke('runtimeBegin', [0n], 20);
  const firstA = keyedButton('A', 10, 'a');
  const firstB = keyedButton('B', 11, 'b');
  invoke('Row', [allocateViewSlice([firstA, firstB])], 20);
  const keyedWaiting = invoke(
    'runtimeCommitAndWait', [data.getBigUint64(20 * 8, true), 0n], 24,
  );
  const row = root.childNodes[0];
  const stableA = row.childNodes[0];
  const stableB = row.childNodes[1];
  stableA.dispatch('click');
  assert.equal(await keyedWaiting, 0);

  invoke('runtimeBegin', [0n], 20);
  const secondB = keyedButton('B2', 12, 'b');
  const secondA = keyedButton('A2', 13, 'a');
  invoke('Row', [allocateViewSlice([secondB, secondA])], 20);
  const reorderedWaiting = invoke(
    'runtimeCommitAndWait', [data.getBigUint64(20 * 8, true), 0n], 24,
  );
  assert.equal(root.childNodes[0], row);
  assert.equal(row.childNodes[0], stableB);
  assert.equal(row.childNodes[1], stableA);
  assert.equal(stableB.childNodes[0].nodeValue, 'B2');
  assert.equal(stableA.childNodes[0].nodeValue, 'A2');
  stableB.dispatch('click');
  assert.equal(await reorderedWaiting, 0);
  assert.equal(data.getBigUint64(24 * 8, true), 12n);

  invoke('runtimeBegin', [0n], 20);
  const duplicateA = keyedButton('Duplicate A', 14, 'duplicate');
  const duplicateB = keyedButton('Duplicate B', 15, 'duplicate');
  invoke('Row', [allocateViewSlice([duplicateA, duplicateB])], 20);
  assert.throws(
    () => invoke('runtimeCommitAndWait', [data.getBigUint64(20 * 8, true), 0n], 24),
    /duplicate sibling key/,
  );
  assert.equal(root.childNodes[0], row);
  assert.equal(row.childNodes[0], stableB);
  assert.equal(row.childNodes[1], stableA);

  invoke('runtimeBegin', [0n], 20);
  invoke('runtimeButton', [allocateString('Recovered'), 16n], 20);
  const recoveredWaiting = invoke(
    'runtimeCommitAndWait', [data.getBigUint64(20 * 8, true), 0n], 24,
  );
  assert.equal(root.childNodes[0].tagName, 'BUTTON');
  assert.equal(root.childNodes[0].childNodes[0].nodeValue, 'Recovered');
  root.childNodes[0].dispatch('click');
  assert.equal(await recoveredWaiting, 0);
  assert.equal(data.getBigUint64(24 * 8, true), 16n);

  invoke('runtimeBegin', [0n], 20);
  invoke('runtimeTextArea', [allocateString('a🙂b'), allocateString('Source'), 21n], 20);
  const inputWaiting = invoke(
    'runtimeCommitAndWait', [data.getBigUint64(20 * 8, true), 0n], 24,
  );
  const textArea = root.childNodes[0];
  assert.equal(textArea.tagName, 'TEXTAREA');
  textArea.value = 'a🙂Xb';
  textArea.selectionStart = 4;
  textArea.selectionEnd = 4;
  textArea.dispatch('input');
  assert.equal(await inputWaiting, 0);
  assert.equal(data.getBigUint64(24 * 8, true), 21n);
  assert.equal(data.getBigUint64(25 * 8, true), 2n);
  assert.equal(readString(data.getBigUint64(27 * 8, true)), 'a🙂Xb');
  assert.equal(data.getBigUint64(43 * 8, true), 4n);
  assert.equal(data.getBigUint64(44 * 8, true), 0n);

  invoke('runtimeBegin', [0n], 20);
  invoke('runtimeTextArea', [allocateString('a🙂Xb'), allocateString('Source'), 21n], 20);
  invoke('runtimeOnSelectionChange', [data.getBigUint64(20 * 8, true), 22n], 20);
  const selectionWaiting = invoke(
    'runtimeCommitAndWait', [data.getBigUint64(20 * 8, true), 0n], 24,
  );
  const selectedArea = root.childNodes[0];
  selectedArea.selectionStart = 1;
  selectedArea.selectionEnd = 3;
  selectedArea.focus();
  document.dispatch('selectionchange');
  assert.equal(await selectionWaiting, 0);
  assert.equal(data.getBigUint64(24 * 8, true), 22n);
  assert.equal(data.getBigUint64(25 * 8, true), 20n);
  assert.equal(readString(data.getBigUint64(27 * 8, true)), 'a🙂Xb');
  assert.equal(data.getBigUint64(43 * 8, true), 1n);
  assert.equal(data.getBigUint64(44 * 8, true), 2n);

  invoke('runtimeBegin', [0n], 20);
  invoke('runtimeTextInput', [allocateString('documentation'), allocateString('Command'), 23n], 20);
  invoke('runtimeOnKeyDown', [data.getBigUint64(20 * 8, true), 24n], 20);
  const keyWaiting = invoke(
    'runtimeCommitAndWait', [data.getBigUint64(20 * 8, true), 0n], 24,
  );
  root.childNodes[0].dispatch('keydown', {
    key: 'Enter', code: 'Enter', shiftKey: false, ctrlKey: true, altKey: false,
    metaKey: false, repeat: true, isComposing: false,
  });
  assert.equal(await keyWaiting, 0);
  assert.equal(data.getBigUint64(24 * 8, true), 24n);
  assert.equal(data.getBigUint64(25 * 8, true), 7n);
  assert.equal(readString(data.getBigUint64(29 * 8, true)), 'Enter');
  assert.equal(readString(data.getBigUint64(32 * 8, true)), 'Enter');
  assert.equal(data.getBigUint64(33 * 8, true), 2n);
  assert.equal(data.getBigUint64(34 * 8, true), 1n);
  assert.equal(data.getBigUint64(35 * 8, true), 0n);

  const enterComponent = (key) => invoke('runtimeEnterComponent', [
    allocateString('github.com/acme/widgets::Counter'),
    77n,
    1n,
    allocateString(key),
  ], 20);
  const componentState = (key, initial) => {
    enterComponent(key);
    invoke('UseIntState', [BigInt(initial)], 20);
    const handle = data.getBigUint64(20 * 8, true);
    invoke('runtimeExitComponent', [], 20);
    return handle;
  };

  invoke('runtimeBegin', [0n], 20);
  const alphaState = componentState('alpha', 1);
  const betaState = componentState('beta', 2);
  assert.notEqual(alphaState, betaState);
  assert.notEqual(alphaState & (1n << 63n), 0n);
  invoke('IntStateCommitted', [alphaState], 20);
  assert.equal(data.getBigUint64(20 * 8, true), 0n);
  const scopedA = keyedButton('Scoped A', 17, 'alpha');
  const scopedB = keyedButton('Scoped B', 18, 'beta');
  invoke('Row', [allocateViewSlice([scopedA, scopedB])], 20);
  const scopedWaiting = invoke(
    'runtimeCommitAndWait', [data.getBigUint64(20 * 8, true), 0n], 24,
  );
  invoke('IntStateCommitted', [alphaState], 20);
  assert.equal(data.getBigUint64(20 * 8, true), 1n);
  root.childNodes[0].childNodes[0].dispatch('click');
  assert.equal(await scopedWaiting, 0);
  invoke('SetIntState', [alphaState, 11n], 20);

  invoke('runtimeBegin', [0n], 20);
  assert.equal(componentState('beta', 0), betaState);
  assert.equal(componentState('alpha', 0), alphaState);
  invoke('IntStateValue', [alphaState], 20);
  assert.equal(data.getBigUint64(20 * 8, true), 11n);
  invoke('IntStateValue', [betaState], 20);
  assert.equal(data.getBigUint64(20 * 8, true), 2n);
  const reorderedScopedB = keyedButton('Scoped B2', 19, 'beta');
  const reorderedScopedA = keyedButton('Scoped A2', 20, 'alpha');
  invoke('Row', [allocateViewSlice([reorderedScopedB, reorderedScopedA])], 20);
  const reorderedScopedWaiting = invoke(
    'runtimeCommitAndWait', [data.getBigUint64(20 * 8, true), 0n], 24,
  );
  root.childNodes[0].childNodes[0].dispatch('click');
  assert.equal(await reorderedScopedWaiting, 0);

  invoke('runtimeBegin', [0n], 20);
  assert.equal(componentState('alpha', 0), alphaState);
  const alphaOnly = keyedButton('Scoped A3', 21, 'alpha');
  invoke('Row', [allocateViewSlice([alphaOnly])], 20);
  const removalWaiting = invoke(
    'runtimeCommitAndWait', [data.getBigUint64(20 * 8, true), 0n], 24,
  );
  root.childNodes[0].childNodes[0].dispatch('click');
  assert.equal(await removalWaiting, 0);
  assert.throws(() => invoke('IntStateValue', [betaState], 20), /stale/);
  invoke('IntStateAlive', [alphaState], 20);
  assert.equal(data.getBigUint64(20 * 8, true), 1n);
  invoke('IntStateAlive', [betaState], 20);
  assert.equal(data.getBigUint64(20 * 8, true), 0n);
  invoke('IntStateCommitted', [betaState], 20);
  assert.equal(data.getBigUint64(20 * 8, true), 0n);

  invoke('runtimeBegin', [0n], 20);
  assert.equal(componentState('alpha', 0), alphaState);
  const replacementBetaState = componentState('beta', 9);
  assert.notEqual(replacementBetaState, betaState);
  assert.throws(() => invoke('SetIntState', [betaState, 99n], 20), /stale/);
  invoke('IntStateValue', [replacementBetaState], 20);
  assert.equal(data.getBigUint64(20 * 8, true), 9n);
  const reinsertedA = keyedButton('Scoped A4', 22, 'alpha');
  const reinsertedB = keyedButton('Scoped B4', 23, 'beta');
  invoke('Row', [allocateViewSlice([reinsertedA, reinsertedB])], 20);
  const insertionWaiting = invoke(
    'runtimeCommitAndWait', [data.getBigUint64(20 * 8, true), 0n], 24,
  );
  root.childNodes[0].childNodes[0].dispatch('click');
  assert.equal(await insertionWaiting, 0);
});

test('UI VM DOM session applies history commands and rerenders on popstate', () => {
  const window = new FakeWindow('https://example.test/start');
  const document = new FakeDocument(window);
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  adapter.applyBatch({ sessionEpoch: 22n, revision: 1n, mutations: [] });
  const pending = [{
    key: 'replay-gui-event:1:1:1', source: 'replay-gui-event', token: '1',
    delayMs: 0, replay: true,
  }];
  const locations = [];
  const viewports = [];
  const requests = [{ kind: 'push', path: '/settings?tab=profile' }];
  let current = '';
  let invalidation = false;
  let event;
  let scheduledRuns = 0;
  let replayGeneration = 1;
  const island = {
    run: () => 'suspended_for_host_events',
    runScheduled: () => { scheduledRuns += 1; return 'suspended_for_host_events'; },
    takeHostOutput: () => undefined,
    takePendingHostEvents: () => pending,
    takeUiNavigationRequests: () => requests.splice(0),
    setUiLocation: (path, invalidate) => {
      locations.push([path, invalidate]);
      const changed = current !== path;
      current = path;
      invalidation ||= changed && invalidate;
      return changed;
    },
    setUiViewport: (width, height, scaleFactor, invalidate) => {
      viewports.push([width, height, scaleFactor, invalidate]);
      invalidation ||= invalidate;
      return true;
    },
    takeUiInvalidation: () => {
      const value = invalidation;
      invalidation = false;
      return value;
    },
    wakeHostEvent: () => false,
    wakeHostEventWithData: (key, data) => {
      event = decodeUiEvent(data);
      const accepted = key === pending[0]?.key;
      replayGeneration += 1;
      pending.splice(0, 1, {
        key: `replay-gui-event:${replayGeneration}:${replayGeneration}:${replayGeneration}`,
        source: 'replay-gui-event', token: String(replayGeneration),
        delayMs: 0, replay: true,
      });
      return accepted;
    },
  };
  const session = new UiVmDomSession(island, adapter);
  assert.equal(session.start(), 'suspended_for_host_events');
  assert.equal(window.location.pathname, '/settings');
  assert.equal(window.location.search, '?tab=profile');
  assert.deepEqual(locations, [['/start', false]]);
  assert.deepEqual(viewports, [[1024, 768, 1, false]]);

  window.dispatchPop('/overview#recent');
  assert.deepEqual(locations.at(-1), ['/overview#recent', true]);
  assert.equal(event.event, 17);
  assert.equal(event.handler.index, 0xffff_ffff);
  assert.equal(scheduledRuns, 1);
  window.dispatchResize(1280, 720, 2);
  assert.deepEqual(viewports.at(-1), [1280, 720, 2, true]);
  assert.equal(scheduledRuns, 2);
  session.dispose();
  assert.equal(window.listeners.has('popstate'), false);
  assert.equal(window.listeners.has('resize'), false);
});

test('UI VM DOM session provides isolated memory history to embedded documents', () => {
  const window = new FakeWindow('about:srcdoc');
  const document = new FakeDocument(window);
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  adapter.applyBatch({ sessionEpoch: 23n, revision: 1n, mutations: [] });
  const locations = [];
  const requests = [
    { kind: 'push', path: '/first' },
    { kind: 'push', path: '/second' },
    { kind: 'back' },
  ];
  const island = {
    run: () => 'suspended_for_host_events',
    runScheduled: () => 'suspended_for_host_events',
    takeHostOutput: () => undefined,
    takePendingHostEvents: () => [],
    takeUiNavigationRequests: () => requests.splice(0),
    takeUiSystemRequests: () => [],
    setUiLocation: (path, invalidate) => { locations.push([path, invalidate]); return false; },
    setUiViewport: () => false,
    takeUiInvalidation: () => false,
    wakeHostEvent: () => false,
    wakeHostEventWithData: () => false,
  };

  const session = new UiVmDomSession(island, adapter, { initialLocation: '/embedded' });
  assert.equal(session.start(), 'suspended_for_host_events');
  assert.deepEqual(locations, [['/embedded', false], ['/first', true]]);
  assert.equal(window.location.href, 'about:srcdoc');
  assert.equal(window.listeners.has('popstate'), false);
  assert.equal(window.listeners.has('resize'), true);
  session.dispose();
  assert.equal(window.listeners.has('resize'), false);
});

test('UI VM DOM session completes VUS1 requests without consuming the GUI waiter', async () => {
  const window = new FakeWindow();
  const document = new FakeDocument(window);
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  adapter.applyBatch({ sessionEpoch: 41n, revision: 1n, mutations: [] });
  const pending = [
    {
      key: 'replay-gui-event:1:1:1', source: 'replay-gui-event', token: '1',
      delayMs: 0, replay: true,
    },
    {
      key: 'replay-ui-system:7:2:2', source: 'replay-ui-system', token: '7',
      delayMs: 0, replay: true,
    },
  ];
  const requests = [{ requestId: '7', frame: systemReadClipboardFrame() }];
  let scheduled = 0;
  let completed;
  const island = {
    run: () => 'suspended_for_host_events',
    runScheduled: () => { scheduled += 1; return 'suspended_for_host_events'; },
    takeHostOutput: () => undefined,
    takePendingHostEvents: () => pending,
    takeUiSystemRequests: () => requests.splice(0),
    wakeHostEvent: () => false,
    wakeHostEventWithData: (key, frame) => {
      if (key !== 'replay-ui-system:7:2:2') return false;
      completed = frame;
      pending.splice(1, 1);
      return true;
    },
  };
  const systemHost = {
    execute: async (frame) => {
      assert.equal(decodeUiSystemRequest(frame).type, 'read-clipboard');
      return encodeUiSystemResponse(7n, {
        type: 'clipboard', content: { type: 'text', text: 'browser value' },
      });
    },
  };
  const session = new UiVmDomSession(island, adapter, { systemHost });
  assert.equal(session.start(), 'suspended_for_host_events');
  await new Promise((resolve) => setTimeout(resolve, 0));
  assert.equal(scheduled, 1);
  assert.equal(new TextDecoder().decode(completed.subarray(0, 4)), 'VUS1');
  assert.equal(pending.length, 1);
  assert.equal(pending[0].source, 'replay-gui-event');
  session.dispose();
});

test('UI VM DOM session retains invalidation while an application host request owns the turn', async () => {
  const window = new FakeWindow();
  const document = new FakeDocument(window);
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  adapter.applyBatch({ sessionEpoch: 42n, revision: 1n, mutations: [] });
  const pending = [{
    key: 'replay-ui-system:8:2:2', source: 'replay-ui-system', token: '8',
    delayMs: 0, replay: true,
  }];
  const requests = [{ requestId: '8', frame: systemReadClipboardFrame() }];
  let invalidation = true;
  let invalidationReads = 0;
  let scheduled = 0;
  let invalidationEvent;
  const island = {
    run: () => 'suspended_for_host_events',
    runScheduled: () => {
      scheduled += 1;
      if (scheduled === 1) pending.push({
        key: 'replay-gui-event:9:3:3', source: 'replay-gui-event', token: '9',
        delayMs: 0, replay: true,
      });
      return 'suspended_for_host_events';
    },
    takeHostOutput: () => undefined,
    takePendingHostEvents: () => pending,
    takeUiSystemRequests: () => requests.splice(0),
    takeUiInvalidation: () => {
      invalidationReads += 1;
      const current = invalidation;
      invalidation = false;
      return current;
    },
    wakeHostEvent: () => false,
    wakeHostEventWithData: (key, frame) => {
      const index = pending.findIndex((event) => event.key === key);
      if (index < 0) return false;
      pending.splice(index, 1);
      if (key === 'replay-gui-event:9:3:3') invalidationEvent = decodeUiEvent(frame);
      return true;
    },
  };
  const systemHost = {
    execute: async () => encodeUiSystemResponse(8n, {
      type: 'clipboard', content: { type: 'text', text: 'host response' },
    }),
  };
  const session = new UiVmDomSession(island, adapter, { systemHost });
  assert.equal(session.start(), 'suspended_for_host_events');
  assert.equal(invalidationReads, 0);
  await new Promise((resolve) => setTimeout(resolve, 0));
  assert.equal(invalidationReads, 1);
  assert.equal(invalidationEvent.event, 17);
  assert.equal(scheduled, 2);
  session.dispose();
});

test('DOM adapter commits a revision atomically and queues listener events', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  adapter.applyMutationFrame(mutationFrame());

  assert.equal(adapter.currentSessionEpoch, 9n);
  assert.equal(adapter.currentRevision, 1n);
  assert.equal(root.childNodes.length, 1);
  const column = root.childNodes[0];
  assert.equal(column.tagName, 'DIV');
  assert.equal(column.style.display, 'flex');
  assert.equal(column.style.flexDirection, 'column');
  assert.equal(column.style.gap, '12.5px');
  assert.equal(column.childNodes[0].nodeValue, '你好');

  column.dispatch('click');
  const event = adapter.shiftEventFrame();
  assert.ok(event);
  assert.equal(new TextDecoder().decode(event.subarray(0, 4)), 'VUE1');
  const view = new DataView(event.buffer, event.byteOffset, event.byteLength);
  assert.equal(view.getBigUint64(4, true), 9n);
  assert.equal(view.getUint32(12, true), 7);
  assert.equal(view.getUint32(16, true), 3);
  assert.equal(view.getUint16(20, true), 1);
  assert.equal(view.getUint8(38), 0);

  adapter.applyBatch({
    sessionEpoch: 9n,
    revision: 2n,
    mutations: [
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 16,
        value: { type: 'text', value: '42%' },
      },
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 10,
        value: { type: 'color', value: 0xff315efb },
      },
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 24,
        value: { type: 'i64', value: 8n },
      },
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 26,
        value: { type: 'f64', value: 4.5 },
      },
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 27,
        value: { type: 'f64', value: 18 },
      },
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 28,
        value: { type: 'bool', value: true },
      },
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 29,
        value: { type: 'bool', value: true },
      },
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 30,
        value: { type: 'text', value: 'A value is required' },
      },
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 33,
        value: { type: 'text', value: 'header header / sidebar main' },
      },
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 34,
        value: { type: 'text', value: 'main' },
      },
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 55,
        value: { type: 'color', value: 0xff526680 },
      },
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 56,
        value: { type: 'length', value: { unit: 'px', value: 1.5 } },
      },
      { type: 'set-text', id: { index: 2, generation: 1 }, text: 'updated' },
    ],
  });
  assert.equal(column.style.background, '#315efbff');
  assert.equal(column.attributes.get('aria-valuetext'), '42%');
  assert.equal(column.style.borderRadius, '8px');
  assert.equal(column.scrollLeft, 4.5);
  assert.equal(column.scrollTop, 18);
  assert.equal(column.required, true);
  assert.equal(column.attributes.get('aria-invalid'), 'true');
  assert.equal(column.attributes.get('aria-description'), 'A value is required');
  assert.equal(column.style.gridTemplateAreas, '"header header" "sidebar main"');
  assert.equal(column.style.gridArea, 'main');
  assert.equal(column.style.borderColor, '#526680ff');
  assert.equal(column.style.borderWidth, '1.5px');
  assert.equal(column.style.borderStyle, 'solid');
  assert.equal(column.childNodes[0].nodeValue, 'updated');

  assert.throws(() => adapter.applyBatch({
    sessionEpoch: 9n,
    revision: 3n,
    mutations: [{
      type: 'set-property',
      id: { index: 1, generation: 1 },
      property: 33,
      value: { type: 'text', value: 'a a / a b' },
    }],
  }), /rectangles/);
  assert.equal(adapter.currentRevision, 2n);
  assert.equal(root.childNodes[0].style.gridTemplateAreas, '"header header" "sidebar main"');

  assert.throws(() => adapter.applyBatch({
    sessionEpoch: 9n,
    revision: 3n,
    mutations: [{ type: 'set-text', id: { index: 1, generation: 1 }, text: 'invalid' }],
  }), /element/);
  assert.equal(adapter.currentRevision, 2n);
  assert.equal(root.childNodes[0].childNodes[0].nodeValue, 'updated');
});

test('DOM adapter projects and removes portable interaction state colors', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const button = { index: 1, generation: 1 };
  adapter.applyBatch({
    sessionEpoch: 12n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: button, primitive: 9 },
      { type: 'set-property', id: button, property: 69, value: { type: 'color', value: 0xff223344 } },
      { type: 'set-property', id: button, property: 70, value: { type: 'color', value: 0xff334455 } },
      { type: 'set-property', id: button, property: 71, value: { type: 'color', value: 0xff445566 } },
      { type: 'set-property', id: button, property: 72, value: { type: 'i64', value: 3n } },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child: button },
    ],
  });
  const element = root.childNodes[0];
  assert.equal(element.style['--volang-hover-background'], '#223344ff');
  assert.equal(element.style['--volang-pressed-background'], '#334455ff');
  assert.equal(element.style['--volang-focus-ring'], '#445566ff');
  assert.equal(element.hasAttribute('data-volang-hover-background'), true);
  assert.equal(element.hasAttribute('data-volang-pressed-background'), true);
  assert.equal(element.hasAttribute('data-volang-focus-ring'), true);
  assert.match(element.style.boxShadow, /0 8px 20px/);
  assert.equal(element.getAttribute('data-volang-elevation'), '3');

  adapter.applyBatch({
    sessionEpoch: 12n,
    revision: 2n,
    mutations: [
      { type: 'remove-property', id: button, property: 69 },
      { type: 'remove-property', id: button, property: 70 },
      { type: 'remove-property', id: button, property: 71 },
      { type: 'remove-property', id: button, property: 72 },
    ],
  });
  assert.equal(element.style['--volang-hover-background'], '');
  assert.equal(element.style['--volang-pressed-background'], '');
  assert.equal(element.style['--volang-focus-ring'], '');
  assert.equal(element.hasAttribute('data-volang-hover-background'), false);
  assert.equal(element.hasAttribute('data-volang-pressed-background'), false);
  assert.equal(element.hasAttribute('data-volang-focus-ring'), false);
  assert.equal(element.style.boxShadow, '');
  assert.equal(element.hasAttribute('data-volang-elevation'), false);
});

test('DOM adapter projects text area, asset, graphics, and media properties', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const id = (index) => ({ index, generation: 1 });
  const set = (index, code, value) => ({
    type: 'set-property', id: id(index), property: code, value,
  });
  adapter.applyBatch({
    sessionEpoch: 1n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: id(1), primitive: 16 },
      set(1, 16, { type: 'text', value: 'line one\nline two' }),
      set(1, 17, { type: 'text', value: 'Source' }),
      set(1, 47, { type: 'text', value: '/asset.svg' }),
      set(1, 49, { type: 'text', value: 'contain' }),
      set(1, 50, { type: 'f64', value: 0.75 }),
      set(1, 51, { type: 'text', value: 'scale(1)' }),
      set(1, 52, { type: 'text', value: 'VGC1' }),
      set(1, 53, { type: 'text', value: 'VMS1|2' }),
      { type: 'insert-before', parent: id(0), child: id(1) },
    ],
  });
  const area = root.childNodes[0];
  assert.equal(area.tagName, 'TEXTAREA');
  assert.equal(area.value, 'line one\nline two');
  assert.equal(area.getAttribute('src'), '/asset.svg');
  assert.equal(area.style.objectFit, 'contain');
  assert.equal(area.style.opacity, '0.75');
  assert.equal(area.style.transform, 'scale(1)');
  assert.equal(area.getAttribute('data-volang-graphics'), 'VGC1');
  assert.equal(area.getAttribute('data-volang-media-state'), 'VMS1|2');
});

test('DOM slider preserves range properties and emits a controlled numeric value', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const slider = { index: 1, generation: 1 };
  adapter.applyBatch({
    sessionEpoch: 2n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: slider, primitive: 12 },
      { type: 'set-property', id: slider, property: 16, value: { type: 'f64', value: 40 } },
      { type: 'set-property', id: slider, property: 20, value: { type: 'text', value: 'Quality' } },
      { type: 'set-property', id: slider, property: 57, value: { type: 'f64', value: 0 } },
      { type: 'set-property', id: slider, property: 58, value: { type: 'f64', value: 100 } },
      { type: 'set-property', id: slider, property: 59, value: { type: 'f64', value: 5 } },
      { type: 'set-property', id: slider, property: 11, value: { type: 'color', value: 0xff637eff } },
      {
        type: 'listen', id: slider, listener: {
          event: 2, handler: { index: 7, generation: 1 }, capture: false, passive: false, once: false,
        },
      },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child: slider },
    ],
  });
  const input = root.childNodes[0];
  assert.equal(input.type, 'range');
  assert.equal(input.value, '40');
  assert.equal(input.min, '0');
  assert.equal(input.max, '100');
  assert.equal(input.step, '5');
  assert.equal(input.style.accentColor, '#637effff');
  input.value = '75';
  input.dispatch('input');
  const event = decodeUiEvent(adapter.shiftEventFrame());
  assert.equal(event.event, 2);
  assert.equal(event.payload.type, 'text');
  assert.equal(event.payload.value, '75');
});

test('DOM composed spin buttons expose a complete numeric range', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const input = { index: 1, generation: 1 };
  const action = { index: 2, generation: 1 };
  adapter.applyBatch({
    sessionEpoch: 2n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: input, primitive: 10 },
      { type: 'set-property', id: input, property: 16, value: { type: 'text', value: '3' } },
      { type: 'set-property', id: input, property: 19, value: { type: 'text', value: 'spinbutton' } },
      { type: 'set-property', id: input, property: 20, value: { type: 'text', value: 'Replicas' } },
      { type: 'set-property', id: input, property: 57, value: { type: 'f64', value: 1 } },
      { type: 'set-property', id: input, property: 58, value: { type: 'f64', value: 10 } },
      { type: 'set-property', id: input, property: 59, value: { type: 'f64', value: 1 } },
      { type: 'create-element', id: action, primitive: 3 },
      { type: 'set-property', id: action, property: 19, value: { type: 'text', value: 'button' } },
      { type: 'set-property', id: action, property: 18, value: { type: 'bool', value: true } },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child: input },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child: action },
    ],
  });
  const element = root.childNodes[0];
  assert.equal(element.value, '3');
  assert.equal(element.getAttribute('role'), 'spinbutton');
  assert.equal(element.getAttribute('aria-valuenow'), '3');
  assert.equal(element.getAttribute('aria-valuemin'), '1');
  assert.equal(element.getAttribute('aria-valuemax'), '10');
  assert.equal(element.getAttribute('data-volang-step'), '1');
  assert.equal(root.childNodes[1].getAttribute('aria-disabled'), 'true');
  assert.equal(root.childNodes[1].hasAttribute('disabled'), false);
});

test('DOM text input events preserve the UTF-16 selection after editing', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const area = { index: 1, generation: 1 };
  adapter.applyBatch({
    sessionEpoch: 3n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: area, primitive: 16 },
      { type: 'set-property', id: area, property: 16, value: { type: 'text', value: 'a🙂bc' } },
      {
        type: 'listen', id: area, listener: {
          event: 2, handler: { index: 9, generation: 1 }, capture: false, passive: false, once: false,
        },
      },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child: area },
    ],
  });
  const input = root.childNodes[0];
  input.value = 'a🙂Xbc';
  input.selectionStart = 4;
  input.selectionEnd = 4;
  input.dispatch('input');
  const event = decodeUiEvent(adapter.shiftEventFrame());
  assert.equal(event.payload.type, 'text-input');
  assert.equal(event.payload.value, 'a🙂Xbc');
  assert.equal(event.payload.selectionStartUtf16, 4);
  assert.equal(event.payload.selectionLengthUtf16, 0);
});

test('DOM scroll events preserve both viewport axes', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const area = { index: 1, generation: 1 };
  adapter.applyBatch({
    sessionEpoch: 6n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: area, primitive: 16 },
      {
        type: 'listen', id: area, listener: {
          event: 12, handler: { index: 8, generation: 1 }, capture: false, passive: true, once: false,
        },
      },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child: area },
    ],
  });
  const input = root.childNodes[0];
  input.scrollLeft = 41.5;
  input.scrollTop = 72;
  input.dispatch('scroll');
  const event = decodeUiEvent(adapter.shiftEventFrame());
  assert.equal(event.event, 12);
  assert.equal(event.payload.type, 'scroll');
  assert.equal(event.payload.x, 41.5);
  assert.equal(event.payload.y, 72);
  assert.equal(event.payload.deltaX, 0);
  assert.equal(event.payload.deltaY, 0);
});

test('DOM controlled scroll synchronization does not echo renderer writes', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const area = { index: 1, generation: 1 };
  adapter.applyBatch({
    sessionEpoch: 7n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: area, primitive: 7 },
      { type: 'set-property', id: area, property: 26, value: { type: 'f64', value: 12 } },
      { type: 'set-property', id: area, property: 27, value: { type: 'f64', value: 24 } },
      {
        type: 'listen', id: area, listener: {
          event: 12, handler: { index: 8, generation: 1 }, capture: false, passive: true, once: false,
        },
      },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child: area },
    ],
  });
  const viewport = root.childNodes[0];
  viewport.dispatch('scroll');
  assert.equal(adapter.shiftEventFrame(), undefined);
  viewport.dispatch('scroll');
  assert.equal(adapter.shiftEventFrame(), undefined);

  viewport.scrollLeft = 41.5;
  viewport.scrollTop = 72;
  viewport.dispatch('scroll');
  const userEvent = decodeUiEvent(adapter.shiftEventFrame());
  assert.equal(userEvent.payload.type, 'scroll');
  assert.equal(userEvent.payload.x, 41.5);
  assert.equal(userEvent.payload.y, 72);

  adapter.applyBatch({
    sessionEpoch: 7n,
    revision: 2n,
    mutations: [
      { type: 'set-property', id: area, property: 26, value: { type: 'f64', value: 41.5 } },
      { type: 'set-property', id: area, property: 27, value: { type: 'f64', value: 72 } },
    ],
  });
  viewport.dispatch('scroll');
  assert.equal(adapter.shiftEventFrame(), undefined);
  viewport.dispatch('scroll');
  assert.equal(adapter.shiftEventFrame(), undefined);
});

test('DOM text recipes preserve portable font family and source whitespace', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const text = { index: 1, generation: 1 };
  adapter.applyBatch({
    sessionEpoch: 8n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: text, primitive: 15 },
      { type: 'set-property', id: text, property: 62, value: { type: 'text', value: 'monospace' } },
      { type: 'set-property', id: text, property: 63, value: { type: 'text', value: 'pre' } },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child: text },
    ],
  });
  const element = root.childNodes[0];
  assert.equal(element.style.fontFamily, 'monospace');
  assert.equal(element.style.whiteSpace, 'pre');
  adapter.applyBatch({
    sessionEpoch: 8n,
    revision: 2n,
    mutations: [
      { type: 'remove-property', id: text, property: 62 },
      { type: 'remove-property', id: text, property: 63 },
    ],
  });
  assert.equal(element.style.fontFamily, '');
  assert.equal(element.style.whiteSpace, '');
});

test('DOM selection events preserve value and UTF-16 range without an edit', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const area = { index: 1, generation: 1 };
  adapter.applyBatch({
    sessionEpoch: 4n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: area, primitive: 16 },
      { type: 'set-property', id: area, property: 16, value: { type: 'text', value: 'a🙂bc' } },
      { type: 'set-property', id: area, property: 31, value: { type: 'i64', value: 1n } },
      { type: 'set-property', id: area, property: 32, value: { type: 'i64', value: 2n } },
      {
        type: 'listen', id: area, listener: {
          event: 20, handler: { index: 10, generation: 1 }, capture: false, passive: false, once: false,
        },
      },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child: area },
    ],
  });
  const input = root.childNodes[0];
  assert.equal(input.selectionWrites, 1);
  adapter.applyBatch({
    sessionEpoch: 4n,
    revision: 2n,
    mutations: [
      { type: 'set-property', id: area, property: 31, value: { type: 'i64', value: 1n } },
    ],
  });
  assert.equal(input.selectionWrites, 1);
  input.selectionStart = 3;
  input.selectionEnd = 4;
  input.focus();
  document.dispatch('selectionchange');
  const event = decodeUiEvent(adapter.shiftEventFrame());
  assert.equal(event.event, 20);
  assert.equal(event.payload.type, 'text-input');
  assert.equal(event.payload.value, 'a🙂bc');
  assert.equal(event.payload.selectionStartUtf16, 3);
  assert.equal(event.payload.selectionLengthUtf16, 1);
});

test('DOM adapter keeps graphics canvas backing dimensions aligned with logical size', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const id = { index: 1, generation: 1 };
  adapter.applyBatch({
    sessionEpoch: 1n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id, primitive: 13 },
      { type: 'set-property', id, property: 52, value: { type: 'text', value: 'VGC1' } },
      { type: 'set-property', id, property: 1, value: { type: 'f64', value: 24 } },
      { type: 'set-property', id, property: 2, value: { type: 'f64', value: 20 } },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child: id },
    ],
  });
  const canvas = root.childNodes[0];
  assert.equal(canvas.style.width, '24px');
  assert.equal(canvas.style.height, '20px');
  assert.equal(canvas.width, 24);
  assert.equal(canvas.height, 20);
});

test('DOM adapter materializes a replayable browser media host', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const id = { index: 1, generation: 1 };
  const set = (property, value) => ({ type: 'set-property', id, property, value });
  adapter.applyBatch({
    sessionEpoch: 2n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id, primitive: 14 },
      set(53, { type: 'text', value: 'VMS1|2|1500000000|0.75|1.25' }),
      set(49, { type: 'text', value: 'contain' }),
      set(47, { type: 'text', value: '/movie.mp4' }),
      set(54, { type: 'text', value: '/poster.svg' }),
      set(48, { type: 'text', value: 'video' }),
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child: id },
    ],
  });
  const platform = root.childNodes[0];
  const video = platform.childNodes[0];
  assert.equal(platform.getAttribute('data-volang-platform-view'), '');
  assert.equal(video.tagName, 'VIDEO');
  assert.equal(video.getAttribute('data-volang-media-host'), '');
  assert.equal(video.getAttribute('src'), '/movie.mp4');
  assert.equal(video.getAttribute('poster'), '/poster.svg');
  assert.equal(video.controls, true);
  assert.equal(video.preload, 'metadata');
  assert.equal(video.currentTime, 1.5);
  assert.equal(video.volume, 0.75);
  assert.equal(video.playbackRate, 1.25);
  assert.equal(video.style.objectFit, 'contain');

  adapter.applyBatch({
    sessionEpoch: 2n,
    revision: 2n,
    mutations: [{ type: 'remove-property', id, property: 48 }],
  });
  assert.equal(platform.childNodes.length, 0);
});

test('DOM adapter adopts SSR identities and hydrates them without replacing useful HTML', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const serverColumn = document.createElement('div');
  serverColumn.setAttribute('data-volang-node', '1:1');
  serverColumn.setAttribute('data-volang-primitive', 'column');
  const marker = document.createComment('volang-text:2:1');
  const serverText = document.createTextNode('ready before activation');
  serverColumn.appendChild(marker);
  serverColumn.appendChild(serverText);
  root.appendChild(serverColumn);

  const adapter = new UiDomAdapter(root);
  assert.equal(serverColumn.childNodes.length, 1);
  assert.equal(serverColumn.childNodes[0], serverText);

  adapter.applyBatch({
    sessionEpoch: 12n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: { index: 1, generation: 1 }, primitive: 4 },
      { type: 'create-text', id: { index: 2, generation: 1 } },
      { type: 'set-text', id: { index: 2, generation: 1 }, text: 'ready after activation' },
      {
        type: 'listen',
        id: { index: 1, generation: 1 },
        listener: {
          event: 1,
          handler: { index: 9, generation: 1 },
          capture: false,
          passive: false,
          once: false,
        },
      },
      {
        type: 'insert-before',
        parent: { index: 0, generation: 1 },
        child: { index: 1, generation: 1 },
      },
      {
        type: 'insert-before',
        parent: { index: 1, generation: 1 },
        child: { index: 2, generation: 1 },
      },
    ],
  });

  assert.equal(root.childNodes[0], serverColumn);
  assert.equal(serverColumn.childNodes[0], serverText);
  assert.equal(serverText.nodeValue, 'ready after activation');
  serverColumn.dispatch('click');
  assert.ok(adapter.shiftEventFrame());
});

test('DOM adapter rebuilds the complete initial tree when SSR identities have incompatible kinds', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const marker = document.createComment('volang-text:1:1');
  const conflictingText = document.createTextNode('stale server text');
  const extraServerNode = document.createElement('aside');
  extraServerNode.setAttribute('data-volang-node', '99:1');
  extraServerNode.setAttribute('data-volang-primitive', 'box');
  root.appendChild(marker);
  root.appendChild(conflictingText);
  root.appendChild(extraServerNode);

  const adapter = new UiDomAdapter(root);
  adapter.applyBatch({
    sessionEpoch: 13n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: { index: 1, generation: 1 }, primitive: 4 },
      {
        type: 'set-property', id: { index: 1, generation: 1 }, property: 15,
        value: { type: 'text', value: 'center' },
      },
      {
        type: 'insert-before', parent: { index: 0, generation: 1 },
        child: { index: 1, generation: 1 },
      },
    ],
  });

  assert.equal(root.childNodes.length, 1);
  assert.notEqual(root.childNodes[0], conflictingText);
  assert.notEqual(root.childNodes[0], extraServerNode);
  assert.equal(root.childNodes[0].tagName, 'DIV');
  assert.equal(root.childNodes[0].style.justifyContent, 'center');
});

test('DOM adapter hydrates an empty server-rendered text identity', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const serverButton = document.createElement('button');
  serverButton.setAttribute('data-volang-node', '1:1');
  serverButton.setAttribute('data-volang-primitive', 'button');
  serverButton.appendChild(document.createComment('volang-text:2:1'));
  root.appendChild(serverButton);

  const adapter = new UiDomAdapter(root);
  assert.equal(serverButton.childNodes.length, 1);
  assert.equal(serverButton.childNodes[0].nodeType, 3);
  assert.equal(serverButton.childNodes[0].nodeValue, '');

  adapter.applyBatch({
    sessionEpoch: 12n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: { index: 1, generation: 1 }, primitive: 8 },
      { type: 'create-text', id: { index: 2, generation: 1 } },
      { type: 'set-text', id: { index: 2, generation: 1 }, text: '' },
      {
        type: 'insert-before',
        parent: { index: 0, generation: 1 },
        child: { index: 1, generation: 1 },
      },
      {
        type: 'insert-before',
        parent: { index: 1, generation: 1 },
        child: { index: 2, generation: 1 },
      },
    ],
  });
  assert.equal(serverButton.childNodes[0].nodeValue, '');
});

test('UIKit Web accessibility and visual mapping matches its versioned golden', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const id = (index) => ({ index, generation: 1 });
  const property = (index, code, value) => ({
    type: 'set-property', id: id(index), property: code, value,
  });
  adapter.applyBatch({
    sessionEpoch: 51n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: id(1), primitive: 4 },
      { type: 'create-element', id: id(2), primitive: 15 },
      { type: 'create-text', id: id(3) },
      { type: 'create-element', id: id(4), primitive: 10 },
      { type: 'create-element', id: id(5), primitive: 15 },
      { type: 'create-text', id: id(6) },
      { type: 'create-element', id: id(7), primitive: 11 },
      { type: 'create-element', id: id(8), primitive: 5 },
      { type: 'create-element', id: id(9), primitive: 2 },
      { type: 'create-element', id: id(10), primitive: 2 },
      { type: 'create-element', id: id(11), primitive: 9 },
      { type: 'create-text', id: id(12) },
      property(1, 8, { type: 'i64', value: 16n }),
      property(1, 10, { type: 'color', value: 0xfff7f7f8 }),
      property(2, 19, { type: 'text', value: 'heading' }),
      property(2, 11, { type: 'color', value: 0xff16181d }),
      property(2, 12, { type: 'i64', value: 24n }),
      property(2, 13, { type: 'i64', value: 700n }),
      { type: 'set-text', id: id(3), text: 'Account' },
      property(4, 16, { type: 'text', value: 'Ada' }),
      property(4, 17, { type: 'text', value: 'Your name' }),
      property(4, 20, { type: 'text', value: 'Display name' }),
      property(4, 28, { type: 'bool', value: true }),
      property(4, 29, { type: 'bool', value: true }),
      property(4, 30, { type: 'text', value: 'Name is required' }),
      property(4, 2, { type: 'i64', value: 40n }),
      property(4, 10, { type: 'color', value: 0xffffffff }),
      property(4, 24, { type: 'i64', value: 8n }),
      property(5, 19, { type: 'text', value: 'alert' }),
      { type: 'set-text', id: id(6), text: 'Name is required' },
      property(7, 20, { type: 'text', value: 'Email updates' }),
      property(7, 25, { type: 'bool', value: true }),
      property(8, 19, { type: 'text', value: 'progressbar' }),
      property(8, 20, { type: 'text', value: 'Progress' }),
      property(8, 16, { type: 'text', value: '42%' }),
      property(9, 1, { type: 'i64', value: 100n }),
      property(9, 10, { type: 'color', value: 0xff646b76 }),
      property(10, 1, { type: 'i64', value: 42n }),
      property(10, 10, { type: 'color', value: 0xff315efb }),
      property(11, 10, { type: 'color', value: 0xff315efb }),
      property(11, 11, { type: 'color', value: 0xffffffff }),
      { type: 'set-text', id: id(12), text: 'Save' },
      { type: 'insert-before', parent: id(0), child: id(1) },
      { type: 'insert-before', parent: id(1), child: id(2) },
      { type: 'insert-before', parent: id(2), child: id(3) },
      { type: 'insert-before', parent: id(1), child: id(4) },
      { type: 'insert-before', parent: id(1), child: id(5) },
      { type: 'insert-before', parent: id(5), child: id(6) },
      { type: 'insert-before', parent: id(1), child: id(7) },
      { type: 'insert-before', parent: id(1), child: id(8) },
      { type: 'insert-before', parent: id(8), child: id(9) },
      { type: 'insert-before', parent: id(8), child: id(10) },
      { type: 'insert-before', parent: id(1), child: id(11) },
      { type: 'insert-before', parent: id(11), child: id(12) },
    ],
  });
  const [column] = root.childNodes;
  const [heading, input, alert, toggle, progress, button] = column.childNodes;
  const golden = [
    'VWX1 revision=1',
    `column tag=${column.tagName} display=${column.style.display} direction=${column.style.flexDirection} gap=${column.style.gap} background=${column.style.background}`,
    `heading tag=${heading.tagName} role=${heading.getAttribute('role')} text=${heading.childNodes[0].nodeValue} color=${heading.style.color} size=${heading.style.fontSize} weight=${heading.style.fontWeight}`,
    `textbox tag=${input.tagName} type=${input.type} name=${input.getAttribute('aria-label')} value=${input.value} placeholder=${input.placeholder} required=${input.required} invalid=${input.getAttribute('aria-invalid')} description=${input.getAttribute('aria-description')} height=${input.style.height} background=${input.style.background} radius=${input.style.borderRadius}`,
    `alert tag=${alert.tagName} role=${alert.getAttribute('role')} text=${alert.childNodes[0].nodeValue}`,
    `switch tag=${toggle.tagName} type=${toggle.type} name=${toggle.getAttribute('aria-label')} checked=${toggle.checked}`,
    `progress tag=${progress.tagName} role=${progress.getAttribute('role')} name=${progress.getAttribute('aria-label')} value=${progress.getAttribute('aria-valuetext')} track=${progress.childNodes[0].style.width},${progress.childNodes[0].style.background} fill=${progress.childNodes[1].style.width},${progress.childNodes[1].style.background}`,
    `button tag=${button.tagName} text=${button.childNodes[0].nodeValue} background=${button.style.background} color=${button.style.color}`,
    '',
  ].join('\n');
  const expected = readFileSync(
    new URL('../../../../ui/testdata/goldens/uikit.web.txt', import.meta.url),
    'utf8',
  );
  assert.equal(golden, expected);
});

test('DOM adapter discards queued events when a committed listener is replaced', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const id = (index) => ({ index, generation: 1 });
  adapter.applyBatch({
    sessionEpoch: 31n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: id(1), primitive: 2 },
      {
        type: 'listen', id: id(1),
        listener: { event: 1, handler: id(7), capture: false, passive: false, once: false },
      },
      { type: 'insert-before', parent: id(0), child: id(1) },
    ],
  });
  const button = root.childNodes[0];
  button.dispatch('click');
  assert.ok(adapter.nextEventFrameLength() > 0);

  adapter.applyBatch({
    sessionEpoch: 31n,
    revision: 2n,
    mutations: [
      { type: 'unlisten', id: id(1), event: 1, handler: id(7) },
      {
        type: 'listen', id: id(1),
        listener: { event: 1, handler: id(8), capture: false, passive: false, once: false },
      },
    ],
  });
  assert.equal(adapter.nextEventFrameLength(), 0);
  button.dispatch('click');
  assert.deepEqual(decodeUiEvent(adapter.shiftEventFrame()).handler, id(8));
});

test('DOM pointer capture preserves typed cancellation delivery', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const id = (index) => ({ index, generation: 1 });
  adapter.applyBatch({
    sessionEpoch: 3n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: id(1), primitive: 2 },
      { type: 'set-property', id: id(1), property: 38, value: { type: 'bool', value: true } },
      {
        type: 'listen', id: id(1),
        listener: { event: 9, handler: id(7), capture: false, passive: false, once: false },
      },
      {
        type: 'listen', id: id(1),
        listener: { event: 18, handler: id(8), capture: false, passive: false, once: false },
      },
      { type: 'insert-before', parent: id(0), child: id(1) },
    ],
  });
  const target = root.childNodes[0];
  target.dispatch('pointerdown', {
    pointerId: 41, pointerType: 'touch', clientX: 12, clientY: 8,
    button: 0, buttons: 1,
  });
  assert.deepEqual(target.capturedPointers, [41]);
  assert.equal(decodeUiEvent(adapter.shiftEventFrame()).event, 9);
  target.dispatch('pointercancel', {
    pointerId: 41, pointerType: 'touch', clientX: 13, clientY: 9,
    button: 0, buttons: 0,
  });
  const cancelled = decodeUiEvent(adapter.shiftEventFrame());
  assert.equal(cancelled.event, 18);
  assert.equal(cancelled.payload.type, 'pointer');
  assert.equal(cancelled.payload.pointerId, 41n);
});

test('DOM context menu and file drop events preserve typed host data', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const child = { index: 1, generation: 1 };
  const handler = (index) => ({ index, generation: 1 });
  adapter.applyBatch({
    sessionEpoch: 82n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: child, primitive: 2 },
      { type: 'listen', id: child, listener: { event: 21, handler: handler(21), capture: false, passive: false, once: false } },
      { type: 'listen', id: child, listener: { event: 22, handler: handler(22), capture: false, passive: false, once: false } },
      { type: 'listen', id: child, listener: { event: 23, handler: handler(23), capture: false, passive: false, once: false } },
      { type: 'listen', id: child, listener: { event: 24, handler: handler(24), capture: false, passive: false, once: false } },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child },
    ],
  });
  const target = root.childNodes[0];
  let prevented = 0;
  target.dispatch('contextmenu', {
    clientX: 24, clientY: 48, button: 2, buttons: 0,
    preventDefault: () => { prevented += 1; },
  });
  const context = decodeUiEvent(adapter.shiftEventFrame());
  assert.equal(context.event, 21);
  assert.equal(context.payload.type, 'pointer');
  assert.equal(context.payload.button, 2);
  target.dispatch('dragover', { type: 'dragover', preventDefault: () => { prevented += 1; } });
  assert.equal(adapter.shiftEventFrame(), undefined);
  target.dispatch('drop', {
    preventDefault: () => { prevented += 1; },
    dataTransfer: { files: [
      { name: 'alpha.vo', webkitRelativePath: '' },
      { name: 'beta.vo', webkitRelativePath: 'folder/beta.vo' },
    ] },
  });
  const dropped = decodeUiEvent(adapter.shiftEventFrame());
  assert.equal(dropped.event, 22);
  assert.deepEqual(dropped.payload, { type: 'text', value: 'alpha.vo\0folder/beta.vo' });
  target.dispatch('dragenter', { preventDefault: () => { prevented += 1; } });
  assert.equal(decodeUiEvent(adapter.shiftEventFrame()).event, 23);
  target.dispatch('dragleave', { preventDefault: () => { prevented += 1; } });
  assert.equal(decodeUiEvent(adapter.shiftEventFrame()).event, 24);
  assert.equal(prevented, 5);
});

test('DOM hidden property excludes a stable subtree and is reversible', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const child = { index: 1, generation: 1 };
  adapter.applyBatch({
    sessionEpoch: 77n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: child, primitive: 2 },
      { type: 'set-property', id: child, property: 46, value: { type: 'bool', value: true } },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child },
    ],
  });
  const element = root.childNodes[0];
  assert.equal(element.hidden, true);
  assert.equal(element.getAttribute('aria-hidden'), 'true');
  assert.equal(element.style.display, 'none');
  adapter.applyBatch({
    sessionEpoch: 77n,
    revision: 2n,
    mutations: [{ type: 'remove-property', id: child, property: 46 }],
  });
  assert.notEqual(element.hidden, true);
  assert.equal(element.getAttribute('aria-hidden'), null);
  assert.notEqual(element.style.display, 'none');
});

test('DOM accessibility-hidden excludes semantics without changing layout', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const child = { index: 1, generation: 1 };
  adapter.applyBatch({
    sessionEpoch: 78n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: child, primitive: 2 },
      { type: 'set-property', id: child, property: 60, value: { type: 'bool', value: true } },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child },
    ],
  });
  const element = root.childNodes[0];
  assert.notEqual(element.hidden, true);
  assert.equal(element.getAttribute('aria-hidden'), 'true');
  assert.notEqual(element.style.display, 'none');
  adapter.applyBatch({
    sessionEpoch: 78n,
    revision: 2n,
    mutations: [{ type: 'remove-property', id: child, property: 60 }],
  });
  assert.equal(element.getAttribute('aria-hidden'), null);
});

test('DOM focusable turns composed elements into reversible tab stops', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const child = { index: 1, generation: 1 };
  adapter.applyBatch({
    sessionEpoch: 79n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: child, primitive: 3 },
      { type: 'set-property', id: child, property: 61, value: { type: 'bool', value: true } },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child },
    ],
  });
  const element = root.childNodes[0];
  assert.equal(element.getAttribute('tabindex'), '0');

  adapter.applyBatch({
    sessionEpoch: 79n,
    revision: 2n,
    mutations: [
      { type: 'set-property', id: child, property: 41, value: { type: 'i64', value: 1n } },
    ],
  });
  assert.equal(document.activeElement, element);
  assert.equal(element.getAttribute('tabindex'), '0');

  adapter.applyBatch({
    sessionEpoch: 79n,
    revision: 3n,
    mutations: [{ type: 'remove-property', id: child, property: 61 }],
  });
  assert.equal(element.getAttribute('tabindex'), null);
});

test('DOM composite choice relationships are explicit and reversible', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const child = { index: 1, generation: 1 };
  adapter.applyBatch({
    sessionEpoch: 81n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: child, primitive: 10 },
      { type: 'set-property', id: child, property: 64, value: { type: 'text', value: 'package-input' } },
      { type: 'set-property', id: child, property: 65, value: { type: 'text', value: 'package-option-2' } },
      { type: 'set-property', id: child, property: 66, value: { type: 'text', value: 'package-listbox' } },
      { type: 'set-property', id: child, property: 67, value: { type: 'text', value: 'list' } },
      { type: 'set-property', id: child, property: 68, value: { type: 'bool', value: true } },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child },
    ],
  });
  const element = root.childNodes[0];
  assert.equal(element.getAttribute('id'), 'package-input');
  assert.equal(element.getAttribute('aria-activedescendant'), 'package-option-2');
  assert.equal(element.getAttribute('aria-controls'), 'package-listbox');
  assert.equal(element.getAttribute('aria-autocomplete'), 'list');
  assert.equal(element.getAttribute('aria-multiselectable'), 'true');
  adapter.applyBatch({
    sessionEpoch: 81n,
    revision: 2n,
    mutations: [64, 65, 66, 67, 68].map((property) => ({
      type: 'remove-property', id: child, property,
    })),
  });
  assert.equal(element.getAttribute('id'), null);
  assert.equal(element.getAttribute('aria-activedescendant'), null);
  assert.equal(element.getAttribute('aria-controls'), null);
  assert.equal(element.getAttribute('aria-autocomplete'), null);
  assert.equal(element.getAttribute('aria-multiselectable'), null);
});

test('DOM modal scope traps focus, isolates background, and restores focus', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const identity = (index) => ({ index, generation: 1 });
  adapter.applyBatch({
    sessionEpoch: 31n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: identity(1), primitive: 9 },
      { type: 'insert-before', parent: identity(0), child: identity(1) },
    ],
  });
  const background = root.childNodes[0];
  background.focus();

  adapter.applyBatch({
    sessionEpoch: 31n,
    revision: 2n,
    mutations: [
      { type: 'create-element', id: identity(2), primitive: 5 },
      { type: 'set-property', id: identity(2), property: 35, value: { type: 'bool', value: true } },
      { type: 'create-element', id: identity(3), primitive: 9 },
      { type: 'create-element', id: identity(4), primitive: 10 },
      { type: 'insert-before', parent: identity(2), child: identity(3) },
      { type: 'insert-before', parent: identity(2), child: identity(4) },
      { type: 'insert-before', parent: identity(0), child: identity(2) },
    ],
  });
  const modal = root.childNodes[1];
  const first = modal.childNodes[0];
  const second = modal.childNodes[1];
  assert.equal(modal.attributes.get('aria-modal'), '');
  assert.equal(modal.style.display, 'grid');
  assert.equal(first.style.gridArea, '1 / 1');
  assert.equal(second.style.gridArea, '1 / 1');
  assert.equal(document.activeElement, first);

  let prevented = false;
  let stopped = false;
  root.dispatch('keydown', {
    target: first,
    key: 'Tab',
    shiftKey: false,
    preventDefault: () => { prevented = true; },
    stopImmediatePropagation: () => { stopped = true; },
  });
  assert.equal(document.activeElement, second);
  assert.equal(prevented, true);
  assert.equal(stopped, true);
  root.dispatch('keydown', {
    target: second,
    key: 'Tab',
    shiftKey: false,
    preventDefault() {},
    stopImmediatePropagation() {},
  });
  assert.equal(document.activeElement, first);

  prevented = false;
  stopped = false;
  root.dispatch('pointerdown', {
    target: background,
    preventDefault: () => { prevented = true; },
    stopImmediatePropagation: () => { stopped = true; },
  });
  assert.equal(prevented, true);
  assert.equal(stopped, true);

  adapter.applyBatch({
    sessionEpoch: 31n,
    revision: 3n,
    mutations: [{ type: 'remove-property', id: identity(2), property: 35 }],
  });
  assert.equal(document.activeElement, background);

  document.activeElement = document.createElement('body');
  adapter.applyBatch({
    sessionEpoch: 31n,
    revision: 4n,
    mutations: [
      { type: 'set-property', id: identity(2), property: 35, value: { type: 'bool', value: true } },
    ],
  });
  assert.equal(document.activeElement, first);
  adapter.applyBatch({
    sessionEpoch: 31n,
    revision: 5n,
    mutations: [{ type: 'remove-property', id: identity(2), property: 35 }],
  });
  assert.equal(document.activeElement, background);
});

test('DOM modal validation rejects ambiguous scopes atomically', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const identity = (index) => ({ index, generation: 1 });
  assert.throws(() => adapter.applyBatch({
    sessionEpoch: 32n,
    revision: 1n,
    mutations: [1, 2].flatMap((index) => [
      { type: 'create-element', id: identity(index), primitive: 2 },
      { type: 'set-property', id: identity(index), property: 35, value: { type: 'bool', value: true } },
      { type: 'insert-before', parent: identity(0), child: identity(index) },
    ]),
  }), /one active modal/);
  assert.equal(adapter.currentRevision, 0n);
  assert.equal(root.childNodes.length, 0);
});

test('DOM portals preserve logical ancestry and use deterministic overlay layers', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const identity = (index) => ({ index, generation: 1 });
  adapter.applyBatch({
    sessionEpoch: 33n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: identity(1), primitive: 4 },
      { type: 'create-element', id: identity(2), primitive: 2 },
      { type: 'create-element', id: identity(3), primitive: 5 },
      { type: 'create-element', id: identity(6), primitive: 10 },
      { type: 'set-property', id: identity(3), property: 40, value: { type: 'i64', value: 200n } },
      {
        type: 'listen',
        id: identity(1),
        listener: {
          event: 7,
          handler: identity(90),
          capture: true,
          passive: false,
          once: false,
        },
      },
      { type: 'insert-before', parent: identity(1), child: identity(2) },
      { type: 'insert-before', parent: identity(1), child: identity(3) },
      { type: 'insert-before', parent: identity(3), child: identity(6) },
      { type: 'insert-before', parent: identity(0), child: identity(1) },
    ],
  });
  const application = root.childNodes[0];
  const portal = root.childNodes[1];
  assert.equal(application.childNodes.length, 1);
  assert.equal(portal.getAttribute('data-volang-portal'), '200');
  assert.equal(portal.style.position, 'fixed');
  assert.equal(portal.style.inset, '0');
  assert.equal(portal.style.zIndex, '200');
  root.dispatch('keydown', {
    target: portal.childNodes[0], key: 's', code: 'KeyS', ctrlKey: true,
  });
  const logicalEvent = decodeUiEvent(adapter.shiftEventFrame());
  assert.equal(logicalEvent.event, 7);
  assert.equal(logicalEvent.handler.index, 90);
  assert.equal(logicalEvent.target.index, 1);
  assert.equal(logicalEvent.payload.type, 'key');
  assert.equal(logicalEvent.payload.key, 's');
  assert.equal(logicalEvent.payload.modifiers.control, true);

  adapter.applyBatch({
    sessionEpoch: 33n,
    revision: 2n,
    mutations: [
      { type: 'create-element', id: identity(4), primitive: 2 },
      { type: 'set-property', id: identity(4), property: 40, value: { type: 'i64', value: 300n } },
      { type: 'insert-before', parent: identity(1), child: identity(4), before: identity(3) },
    ],
  });
  assert.equal(root.childNodes[1].getAttribute('data-volang-portal'), '200');
  assert.equal(root.childNodes[2].getAttribute('data-volang-portal'), '300');

  adapter.applyBatch({
    sessionEpoch: 33n,
    revision: 3n,
    mutations: [{ type: 'remove', parent: identity(1), child: identity(3) }],
  });
  assert.equal(root.childNodes.length, 2);
  assert.equal(root.childNodes[1].getAttribute('data-volang-portal'), '300');

  assert.throws(() => adapter.applyBatch({
    sessionEpoch: 33n,
    revision: 4n,
    mutations: [
      { type: 'create-element', id: identity(5), primitive: 2 },
      { type: 'set-property', id: identity(5), property: 40, value: { type: 'i64', value: 301n } },
      { type: 'insert-before', parent: identity(4), child: identity(5) },
    ],
  }), /cannot be nested/);
  assert.equal(adapter.currentRevision, 3n);
});

test('DOM modal portals isolate keys from logical command ancestors', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const identity = (index) => ({ index, generation: 1 });
  adapter.applyBatch({
    sessionEpoch: 35n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: identity(1), primitive: 4 },
      { type: 'create-element', id: identity(2), primitive: 5 },
      { type: 'create-element', id: identity(3), primitive: 10 },
      { type: 'set-property', id: identity(2), property: 35, value: { type: 'bool', value: true } },
      { type: 'set-property', id: identity(2), property: 40, value: { type: 'i64', value: 200n } },
      {
        type: 'listen',
        id: identity(1),
        listener: {
          event: 7, handler: identity(90), capture: true, passive: false, once: false,
        },
      },
      {
        type: 'listen',
        id: identity(2),
        listener: {
          event: 7, handler: identity(91), capture: true, passive: false, once: false,
        },
      },
      { type: 'insert-before', parent: identity(1), child: identity(2) },
      { type: 'insert-before', parent: identity(2), child: identity(3) },
      { type: 'insert-before', parent: identity(0), child: identity(1) },
    ],
  });
  const modal = root.childNodes[1];
  const key = { target: modal.childNodes[0], key: 'Escape', code: 'Escape' };
  root.dispatch('keydown', key);
  assert.equal(adapter.shiftEventFrame(), undefined);

  modal.dispatch('keydown', key);
  const event = decodeUiEvent(adapter.shiftEventFrame());
  assert.equal(event.handler.index, 91);
  assert.equal(event.target.index, 2);
  assert.equal(event.payload.key, 'Escape');
});

test('DOM focus requests are post-commit, composable, idempotent, and atomic', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const identity = (index) => ({ index, generation: 1 });
  adapter.applyBatch({
    sessionEpoch: 34n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: identity(1), primitive: 10 },
      { type: 'set-property', id: identity(1), property: 41, value: { type: 'i64', value: 1n } },
      { type: 'insert-before', parent: identity(0), child: identity(1) },
    ],
  });
  const first = root.childNodes[0];
  assert.equal(document.activeElement, first);
  assert.equal(first.getAttribute('data-volang-focus-request'), '1');

  const external = document.createElement('button');
  root.appendChild(external);
  external.focus();
  adapter.applyBatch({ sessionEpoch: 34n, revision: 2n, mutations: [] });
  assert.equal(document.activeElement, external);
  adapter.applyBatch({
    sessionEpoch: 34n,
    revision: 3n,
    mutations: [
      { type: 'set-property', id: identity(1), property: 41, value: { type: 'i64', value: 2n } },
    ],
  });
  assert.equal(document.activeElement, first);

  adapter.applyBatch({
    sessionEpoch: 34n,
    revision: 4n,
    mutations: [
      { type: 'create-element', id: identity(2), primitive: 9 },
      { type: 'set-property', id: identity(2), property: 41, value: { type: 'i64', value: 1n } },
      { type: 'insert-before', parent: identity(0), child: identity(2) },
    ],
  });
  const second = root.childNodes[2];
  assert.equal(document.activeElement, second);
  assert.equal(adapter.currentRevision, 4n);

  assert.throws(() => adapter.applyBatch({
    sessionEpoch: 34n,
    revision: 5n,
    mutations: [
      { type: 'set-property', id: identity(1), property: 41, value: { type: 'i64', value: 3n } },
      { type: 'set-property', id: identity(2), property: 41, value: { type: 'i64', value: 2n } },
    ],
  }), /one changed focus request/);
  assert.equal(adapter.currentRevision, 4n);
});

test('DOM layout observation quantizes sizes and bounds feedback revisions', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  let wakes = 0;
  const adapter = new UiDomAdapter(root, { onEvent: () => { wakes += 1; } });
  const identity = (index) => ({ index, generation: 1 });
  adapter.applyBatch({
    sessionEpoch: 35n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id: identity(1), primitive: 2 },
      { type: 'set-property', id: identity(1), property: 1, value: { type: 'f64', value: 120.004 } },
      { type: 'set-property', id: identity(1), property: 2, value: { type: 'f64', value: 40 } },
      {
        type: 'listen', id: identity(1),
        listener: { event: 19, handler: identity(7), capture: false, passive: false, once: false },
      },
      { type: 'insert-before', parent: identity(0), child: identity(1) },
    ],
  });
  const initial = decodeUiEvent(adapter.shiftEventFrame());
  assert.equal(initial.sessionEpoch, 35n);
  assert.equal(initial.event, 19);
  assert.equal(initial.payload.type, 'scroll');
  assert.equal(initial.payload.x, 120);
  assert.equal(initial.payload.y, 40);
  assert.equal(wakes, 1);

  adapter.applyBatch({ sessionEpoch: 35n, revision: 2n, mutations: [] });
  assert.equal(adapter.shiftEventFrame(), undefined);
  for (let revision = 3; revision <= 10; revision += 1) {
    adapter.applyBatch({
      sessionEpoch: 35n,
      revision: BigInt(revision),
      mutations: [{
        type: 'set-property', id: identity(1), property: 1,
        value: { type: 'f64', value: 120 + revision },
      }],
    });
    assert.equal(decodeUiEvent(adapter.shiftEventFrame()).event, 19);
  }
  assert.throws(() => adapter.applyBatch({
    sessionEpoch: 35n,
    revision: 11n,
    mutations: [{
      type: 'set-property', id: identity(1), property: 1,
      value: { type: 'f64', value: 131 },
    }],
  }), /feedback iteration limit/);
  assert.equal(adapter.currentRevision, 10n);
  assert.equal(root.childNodes[0].style.width, '130px');
});

test('DOM adapter rebuilds the prior tree after a host commit failure', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  adapter.applyMutationFrame(mutationFrame());
  document.failNextInsert = true;

  assert.throws(() => adapter.applyBatch({
    sessionEpoch: 9n,
    revision: 2n,
    mutations: [
      { type: 'create-text', id: { index: 3, generation: 1 } },
      { type: 'set-text', id: { index: 3, generation: 1 }, text: 'uncommitted' },
      {
        type: 'insert-before',
        parent: { index: 0, generation: 1 },
        child: { index: 3, generation: 1 },
      },
    ],
  }), /injected DOM insert failure/);
  assert.equal(adapter.currentRevision, 1n);
  assert.equal(root.childNodes.length, 1);
  assert.equal(root.childNodes[0].childNodes[0].nodeValue, '你好');
});

test('DOM adapter atomically adopts a reloaded initial session', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  adapter.applyMutationFrame(mutationFrame());
  const previousChild = root.childNodes[0];

  const replacement = adapter.replaceWithInitialFrame(mutationFrame());
  assert.equal(replacement.currentRevision, 1n);
  assert.equal(replacement.currentSessionEpoch, 9n);
  assert.equal(root.childNodes.length, 1);
  assert.notEqual(root.childNodes[0], previousChild);
  assert.equal(root.childNodes[0].childNodes[0].nodeValue, '你好');
});

test('UI VM DOM session switches to a reloaded Island revision', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const outputs = [mutationFrame()];
  const island = {
    run: () => 'suspended_for_host_events',
    runScheduled: () => 'suspended_for_host_events',
    reload: (bytecode) => {
      assert.deepEqual([...bytecode], [1, 2, 3]);
      return 'suspended_for_host_events';
    },
    takeHostOutput: () => outputs.shift(),
    takePendingHostEvents: () => [],
    wakeHostEvent: () => false,
    wakeHostEventWithData: () => false,
  };
  const session = new UiVmDomSession(island, new UiDomAdapter(root));

  assert.equal(session.reload(new Uint8Array([1, 2, 3])), 'suspended_for_host_events');
  assert.equal(session.adapter.currentRevision, 1n);
  assert.equal(root.childNodes[0].childNodes[0].nodeValue, '你好');
  session.dispose();
});

test('DOM adapter preserves UTF-16 composition selection state', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  adapter.applyBatch({
    sessionEpoch: 4n,
    revision: 1n,
    mutations: [
      {
        type: 'create-element',
        id: { index: 1, generation: 1 },
        primitive: 10,
      },
      {
        type: 'listen',
        id: { index: 1, generation: 1 },
        listener: {
          event: 14,
          handler: { index: 2, generation: 1 },
          capture: false,
          passive: false,
          once: false,
        },
      },
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 16,
        value: { type: 'text', value: 'A😀BC' },
      },
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 31,
        value: { type: 'i64', value: 1n },
      },
      {
        type: 'set-property',
        id: { index: 1, generation: 1 },
        property: 32,
        value: { type: 'i64', value: 2n },
      },
      {
        type: 'insert-before',
        parent: { index: 0, generation: 1 },
        child: { index: 1, generation: 1 },
      },
    ],
  });
  const input = root.childNodes[0];
  assert.equal(input.selectionStart, 1);
  assert.equal(input.selectionEnd, 3);
  input.dispatch('compositionupdate', { data: '拼音' });

  const frame = adapter.shiftEventFrame();
  assert.ok(frame);
  const view = new DataView(frame.buffer, frame.byteOffset, frame.byteLength);
  assert.equal(view.getUint8(38), 8);
  assert.equal(view.getUint32(49, true), 1);
  assert.equal(view.getUint32(53, true), 2);

  assert.throws(() => adapter.applyBatch({
    sessionEpoch: 4n,
    revision: 2n,
    mutations: [{
      type: 'set-property',
      id: { index: 1, generation: 1 },
      property: 32,
      value: { type: 'i64', value: -1n },
    }],
  }), /non-negative/);
  assert.equal(adapter.currentRevision, 1n);
  assert.equal(root.childNodes[0].selectionStart, 1);
  assert.equal(root.childNodes[0].selectionEnd, 3);
});

test('DOM adapter defers controlled input writes until the commit after composition ends', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const id = { index: 1, generation: 1 };
  adapter.applyBatch({
    sessionEpoch: 5n,
    revision: 1n,
    mutations: [
      { type: 'create-element', id, primitive: 10 },
      {
        type: 'set-property', id, property: 16,
        value: { type: 'text', value: 'initial' },
      },
      {
        type: 'set-property', id, property: 31,
        value: { type: 'i64', value: 0n },
      },
      {
        type: 'set-property', id, property: 32,
        value: { type: 'i64', value: 0n },
      },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child: id },
    ],
  });
  const input = root.childNodes[0];
  input.dispatch('compositionstart', { data: '' });
  input.value = '拼音';
  input.setSelectionRange(2, 2);

  adapter.applyBatch({
    sessionEpoch: 5n,
    revision: 2n,
    mutations: [
      {
        type: 'set-property', id, property: 16,
        value: { type: 'text', value: '应用状态' },
      },
      {
        type: 'set-property', id, property: 31,
        value: { type: 'i64', value: 1n },
      },
      {
        type: 'set-property', id, property: 32,
        value: { type: 'i64', value: 2n },
      },
    ],
  });
  assert.equal(input.value, '拼音');
  assert.equal(input.selectionStart, 2);
  assert.equal(input.selectionEnd, 2);

  input.dispatch('compositionend', { data: '拼音' });
  adapter.applyBatch({ sessionEpoch: 5n, revision: 3n, mutations: [] });
  assert.equal(input.value, '应用状态');
  assert.equal(input.selectionStart, 1);
  assert.equal(input.selectionEnd, 3);
});

test('DOM import bridge bounds memory and preserves a rejected revision', () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const memory = new WebAssembly.Memory({ initial: 1 });
  const imports = createUiWebImports(adapter, () => memory).volang_ui_web_v1;
  const frame = mutationFrame();
  new Uint8Array(memory.buffer, 64, frame.byteLength).set(frame);

  assert.equal(imports.apply_mutation_frame(64, frame.byteLength), 0);
  assert.equal(adapter.currentRevision, 1n);
  assert.equal(imports.apply_mutation_frame(memory.buffer.byteLength - 1, 2), 1);
  assert.match(adapter.lastError?.message ?? '', /memory range/);
  assert.equal(adapter.currentRevision, 1n);
  assert.equal(imports.next_event_frame_len(), 0);
});

test('UI VM DOM session wakes timer goroutines and keeps GUI replay pending', async () => {
  const document = new FakeDocument();
  const root = document.createElement('main');
  const adapter = new UiDomAdapter(root);
  const pending = [
    { key: 'timer:1:1:1', source: 'timer', token: '1', delayMs: 0, replay: false },
    {
      key: 'replay-gui-event:2:2:2',
      source: 'replay-gui-event',
      token: '2',
      delayMs: 0,
      replay: true,
    },
  ];
  const calls = [];
  let scheduledRuns = 0;
  let invalidation = false;
  let invalidationEvent;
  const island = {
    run: () => 'suspended_for_host_events',
    runScheduled: () => {
      scheduledRuns += 1;
      if (scheduledRuns === 1) invalidation = true;
      return 'suspended_for_host_events';
    },
    takeHostOutput: () => undefined,
    takePendingHostEvents: () => pending,
    takeUiInvalidation: () => {
      const current = invalidation;
      invalidation = false;
      return current;
    },
    wakeHostEvent: (key) => {
      calls.push(key);
      pending.splice(pending.findIndex((event) => event.key === key), 1);
      return true;
    },
    wakeHostEventWithData: (key, data) => {
      invalidationEvent = decodeUiEvent(data);
      calls.push(key);
      pending.splice(pending.findIndex((event) => event.key === key), 1);
      pending.push({
        key: 'replay-gui-event:3:3:3', source: 'replay-gui-event', token: '3',
        delayMs: 0, replay: true,
      });
      return true;
    },
  };
  adapter.applyBatch({ sessionEpoch: 12n, revision: 1n, mutations: [] });
  const session = new UiVmDomSession(island, adapter);
  assert.equal(session.start(), 'suspended_for_host_events');
  await new Promise((resolve) => setTimeout(resolve, 10));
  assert.deepEqual(calls, ['timer:1:1:1', 'replay-gui-event:2:2:2']);
  assert.equal(invalidationEvent.event, 17);
  assert.equal(invalidationEvent.handler.index, 0xffff_ffff);
  assert.equal(invalidationEvent.target.index, 0);
  assert.equal(scheduledRuns, 2);
  assert.equal(pending[0].source, 'replay-gui-event');
  session.dispose();
});

test('DOM editable combobox Enter is consumed before host delivery and ordinary typing is preserved', () => {
  for (const [role, key, passive, expectedPrevented] of [
    ['combobox', 'Enter', false, true], ['combobox', 'a', false, false],
    ['combobox', ' ', false, false], ['textbox', 'Enter', false, false],
    ['combobox', 'Enter', true, false],
  ]) {
    const document = new FakeDocument();
    const root = document.createElement('main');
    let prevented = false;
    const adapter = new UiDomAdapter(root, { onEvent() { assert.equal(prevented, expectedPrevented); } });
    const input = { index: 1, generation: 1 };
    adapter.applyBatch({ sessionEpoch: 71n, revision: 1n, mutations: [
      { type: 'create-element', id: input, primitive: 10 },
      { type: 'listen', id: input, listener: { event: 7, handler: { index: 8, generation: 1 }, capture: false, passive, once: false } },
      { type: 'insert-before', parent: { index: 0, generation: 1 }, child: input },
    ] });
    const node = root.childNodes[0]; node.setAttribute('role', role); node.focus();
    node.dispatch('keydown', { key, cancelable: true, preventDefault() { prevented = true; } });
    assert.equal(prevented, expectedPrevented, `${role}, ${key}, passive=${passive}`);
    assert.equal(decodeUiEvent(adapter.shiftEventFrame()).payload.key, key);
    assert.equal(adapter.shiftEventFrame(), undefined);
  }
});
