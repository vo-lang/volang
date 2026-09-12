import assert from 'node:assert/strict';
import test from 'node:test';
import { AotRunQueue } from '../dist/aot_run_queue.js';

test('ready queue excludes blocked fibers and reuses wait registrations', () => {
  const queue = new AotRunQueue(4096);
  for (let id = 1; id <= 4096; id++) queue.add(id);
  for (let id = 1; id <= 4096; id++) {
    assert.equal(queue.next(), id);
    assert.equal(queue.wait(id, 9000, 1), true);
    queue.finish(id);
  }
  assert.equal(queue.next(), 0);
  for (let id = 1; id <= 4096; id++) {
    queue.notify(9000, 2);
    assert.equal(queue.next(), id);
    assert.equal(queue.next(), 0);
    assert.equal(queue.wait(id, 9000, 1), true);
  }
  assert.equal(queue.stats().waiterCapacity, 4096);
});

test('select wake cancels competing cases and excludes self rendezvous', () => {
  const queue = new AotRunQueue();
  queue.add(1); queue.add(2);
  queue.next(); queue.next();
  queue.wait(1, 100, 0); queue.wait(1, 100, 0); queue.wait(1, 200, 1);
  queue.wait(2, 100, 0);
  queue.notify(100, 1, 1);
  assert.equal(queue.next(), 2);
  assert.equal(queue.stats().activeWaiters, 3);
  queue.notify(200, 2);
  assert.equal(queue.next(), 1);
  assert.equal(queue.stats().activeWaiters, 0);
  queue.notify(100, 3);
  assert.equal(queue.next(), 0);
});

test('close wakeups are bounded and remove fibers independently of ready order', () => {
  const queue = new AotRunQueue();
  for (let id = 1; id <= 500; id++) { queue.add(id); queue.next(); queue.wait(id, 100, id % 2); }
  queue.close(100);
  assert.ok(queue.next() > 0);
  assert.equal(queue.stats().activeWaiters, 436);
  for (let id = 2; id <= 500; id += 2) queue.remove(id);
  assert.equal(queue.previous(499), 497);
  assert.equal(queue.previous(1), 0);
  const seen = new Set();
  for (;;) { const next = queue.next(); if (next === 0) break; if (next > 0) { assert.ok(!seen.has(next)); seen.add(next); } }
  assert.equal(queue.stats().activeWaiters, 0);
  queue.forgetQueue(100);
});

test('host completion cannot wake a reused fiber address', () => {
  const queue = new AotRunQueue();
  queue.add(1); queue.next();
  const wake = queue.parkHost(1);
  assert.equal(queue.next(), -2);
  queue.remove(1);
  queue.add(1); queue.next(); queue.park(1);
  wake();
  assert.equal(queue.next(), 0);
});

test('waiter capacity is explicit, and removal releases its admission', () => {
  const queue = new AotRunQueue(1);
  queue.add(1); queue.next();
  assert.equal(queue.wait(1, 100, 0), true);
  assert.equal(queue.wait(1, 200, 1), false);
  queue.remove(1);
  queue.add(2); queue.next();
  assert.equal(queue.wait(2, 100, 0), true);
});

test('failed-Island cleanup cancels one select registration per work unit', () => {
  const queue = new AotRunQueue();
  queue.add(1); queue.next();
  for (let id = 100; id < 1100; id++) queue.wait(1, id, id % 2);
  for (let remaining = 999; remaining >= 0; remaining--) {
    assert.equal(queue.wakeForCleanup(1), false);
    assert.equal(queue.stats().activeWaiters, remaining);
    assert.equal(queue.next(), 0);
  }
  assert.equal(queue.wakeForCleanup(1), true);
  assert.equal(queue.next(), 1);
});
