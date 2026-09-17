export class ServerBusy extends Error {
  constructor() { super('All server rendering slots are occupied.'); }
}

// Admission is FIFO and request-owned. A cancelled waiter never starts a process.
export function createAdmission({concurrency = 4, queued = 16} = {}) {
  if (!Number.isSafeInteger(concurrency) || concurrency < 1 || concurrency > 64 ||
      !Number.isSafeInteger(queued) || queued < 0 || queued > 1024) throw new Error('Invalid server admission bounds.');
  let active = 0;
  const waiting = [];
  function releaseSlot() {
    let released = false;
    return () => {
      if (released) return;
      released = true;
      active--;
      const next = waiting.shift();
      if (next) { next.signal.removeEventListener('abort', next.abort); active++; next.resolve(releaseSlot()); }
    };
  }
  return async signal => {
    signal.throwIfAborted();
    if (active < concurrency) { active++; return releaseSlot(); }
    if (waiting.length >= queued) throw new ServerBusy();
    return new Promise((resolve, reject) => {
      const entry = {signal, resolve, abort() {
        const index = waiting.indexOf(entry);
        if (index !== -1) waiting.splice(index, 1);
        reject(signal.reason);
      }};
      waiting.push(entry);
      signal.addEventListener('abort', entry.abort, {once:true});
    });
  };
}
