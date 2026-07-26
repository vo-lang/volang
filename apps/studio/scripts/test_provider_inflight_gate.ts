import assert from 'node:assert/strict';

import { ProviderInflightGate } from '../src/lib/gui/provider_inflight_gate.ts';

const gate = new ProviderInflightGate();
const first = gate.open(1);
const callA = gate.enter(first);
const callB = gate.enter(first);
assert.ok(callA);
assert.ok(callB);

let drained = false;
const draining = gate.beginDrain(first).then(() => {
  drained = true;
});
assert.equal(gate.enter(first), null);
await Promise.resolve();
assert.equal(drained, false);
callA.release();
callA.release();
await Promise.resolve();
assert.equal(drained, false);

const replacement = gate.open(1);
assert.ok(gate.enter(replacement));
callB.release();
await draining;
assert.equal(drained, true);
assert.equal(gate.enter(first), null);

const secondSession = gate.open(2);
assert.ok(gate.enter(secondSession));
await gate.beginDrain(gate.open(3));
assert.ok(gate.enter(secondSession));
assert.throws(() => gate.open(0), /positive safe integer/);

console.log('studio provider in-flight gate: ok');
