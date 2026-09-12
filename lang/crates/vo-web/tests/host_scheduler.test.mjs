import assert from 'node:assert/strict';
import test from 'node:test';
import { yieldHost } from '../dist/host_scheduler.js';

test('concurrent host turns use tasks and release every channel', async () => {
  const previous = globalThis.setTimeout;
  let timers = 0;
  globalThis.setTimeout = (...args) => { timers++; return previous(...args); };
  try {
    let completed = 0;
    const turns = Array.from({ length: 16 }, async () => { await yieldHost(); completed++; });
    await Promise.resolve();
    assert.equal(completed, 0, 'a microtask alone cannot resume guest work');
    await Promise.all(turns);
    assert.equal(completed, 16);
    assert.equal(timers, 0);
  } finally { globalThis.setTimeout = previous; }
});

test('hosts without MessageChannel retain the timer fallback', async () => {
  const previous = globalThis.MessageChannel;
  globalThis.MessageChannel = undefined;
  try { await yieldHost(); } finally { globalThis.MessageChannel = previous; }
  assert.equal(globalThis.MessageChannel, previous);
});
