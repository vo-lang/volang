import assert from 'node:assert/strict';

import { LatestTaskQueue } from '../src/lib/latest_task_queue.ts';

function deferred(): { promise: Promise<void>; resolve: () => void } {
  let resolve = () => {};
  const promise = new Promise<void>((done) => {
    resolve = done;
  });
  return { promise, resolve };
}

const latestWins = new LatestTaskQueue();
const firstGate = deferred();
const firstStarted = deferred();
const events: string[] = [];
const first = latestWins.run(async ({ isLatest }) => {
  events.push('first:start');
  firstStarted.resolve();
  await firstGate.promise;
  events.push(isLatest() ? 'first:commit' : 'first:stale');
});
await firstStarted.promise;
const skipped = latestWins.run(async () => {
  events.push('second:start');
});
const latest = latestWins.run(async ({ isLatest }) => {
  events.push('third:start');
  assert.equal(isLatest(), true);
  events.push('third:commit');
});
firstGate.resolve();
const [firstResult, skippedResult, latestResult] = await Promise.all([first, skipped, latest]);
assert.deepEqual(events, ['first:start', 'first:stale', 'third:start', 'third:commit']);
assert.deepEqual(firstResult, { status: 'cancelled' });
assert.deepEqual(skippedResult, { status: 'cancelled' });
assert.deepEqual(latestResult, { status: 'completed', value: undefined });

const staleErrors = new LatestTaskQueue();
const errorGate = deferred();
const errorStarted = deferred();
const staleFailure = staleErrors.run(async () => {
  errorStarted.resolve();
  await errorGate.promise;
  throw new Error('stale failure');
});
await errorStarted.promise;
const recovery = staleErrors.run(async ({ isLatest }) => {
  assert.equal(isLatest(), true);
});
errorGate.resolve();
assert.deepEqual(await staleFailure, { status: 'cancelled' });
assert.deepEqual(await recovery, { status: 'completed', value: undefined });

const currentErrors = new LatestTaskQueue();
await assert.rejects(
  currentErrors.run(async () => {
    throw new Error('current failure');
  }),
  /current failure/,
);

const invalidated = new LatestTaskQueue();
const invalidatedGate = deferred();
const invalidatedStarted = deferred();
const cancelled = invalidated.run(async () => {
  invalidatedStarted.resolve();
  await invalidatedGate.promise;
});
await invalidatedStarted.promise;
invalidated.invalidate();
invalidatedGate.resolve();
assert.deepEqual(await cancelled, { status: 'cancelled' });

const committedQueue = new LatestTaskQueue();
const committedGate = deferred();
const committedStarted = deferred();
let latestAfterCommit = true;
let recommitAfterInvalidation = true;
const committed = committedQueue.run(async ({ commit, isLatest }) => {
  assert.equal(commit(), true);
  committedStarted.resolve();
  await committedGate.promise;
  latestAfterCommit = isLatest();
  recommitAfterInvalidation = commit();
});
await committedStarted.promise;
committedQueue.invalidate();
committedGate.resolve();
assert.deepEqual(await committed, { status: 'cancelled' });
assert.equal(latestAfterCommit, false);
assert.equal(recommitAfterInvalidation, false);

console.log('studio latest-task queue: ok');
