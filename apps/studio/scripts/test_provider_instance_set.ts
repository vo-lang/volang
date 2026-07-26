import assert from 'node:assert/strict';

import { ProviderInstanceSet } from '../src/lib/gui/provider_instance_set.ts';

const instances = new ProviderInstanceSet<{ label: string }>();
const first = { label: 'first' };
const second = { label: 'second' };
instances.set(1, first).set(2, second);
assert.equal(instances.size, 2);
assert.equal(instances.get(1), first);
assert.equal(instances.get(2), second);

const replacement = { label: 'replacement' };
instances.set(1, replacement);
assert.equal(instances.delete(1, first), false);
assert.equal(instances.get(1), replacement);
assert.equal(instances.delete(2, second), true);
assert.equal(instances.get(1), replacement);
assert.deepEqual([...instances.keys()], [1]);
assert.throws(() => instances.set(0, first), /positive safe integer/);

const staleLease = instances.begin(3);
const currentLease = instances.begin(3);
assert.equal(instances.install(staleLease, { label: 'stale' }), false);
const leased = { label: 'leased' };
assert.equal(instances.install(currentLease, leased), true);
assert.equal(instances.invalidate(3), leased);
assert.equal(instances.install(currentLease, { label: 'late' }), false);

const pendingLease = instances.begin(4);
assert.deepEqual(instances.trackedSessionIds().sort((left, right) => left - right), [1, 3, 4]);
instances.clear();
assert.equal(instances.install(pendingLease, { label: 'late-after-clear' }), false);
assert.equal(instances.size, 0);

console.log('studio provider instance set: ok');
