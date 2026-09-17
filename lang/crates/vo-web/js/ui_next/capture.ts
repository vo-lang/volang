import type { UiServices } from './host.js';

/** One bounded request/reply channel over the guest's serialized task lane. */
export function createCaptureChannel<T>(options: {
  name: string; label: string; maxBytes: number; decode(envelope: Record<string, unknown>): T;
}) {
  const { name, label, maxBytes } = options, lower = label.toLowerCase();
  let request: ((id: string) => void) | undefined, sequence = 0, closed = false;
  let pending: { id: string; promise: Promise<T>; resolve(value: T): void; reject(error: Error): void } | undefined;
  let timer: ReturnType<typeof setTimeout> | undefined;
  const finish = (value?: T, error?: Error) => {
    clearTimeout(timer); timer = undefined;
    const operation = pending; pending = undefined;
    if (error) operation?.reject(error); else if (value !== undefined) operation?.resolve(value);
  };
  const services: UiServices = {
    watches: {
      [`${name}.request`](_value, signal, emit) {
        if (closed || signal.aborted) throw new Error(`${label} is closed.`);
        if (request) throw new Error(`${label} services already belong to another root.`);
        request = id => emit(id);
        signal.addEventListener('abort', () => {
          request = undefined;
          finish(undefined, new Error(`The ${lower} root has closed.`));
        }, { once: true });
      },
    },
    tasks: {
      async [`${name}.publish`](value) {
        try {
          if (closed) return '';
          if (value.length > maxBytes || new TextEncoder().encode(value).length > maxBytes) throw new Error(`${label} snapshot is too large.`);
          const envelope = JSON.parse(value);
          if (envelope?.version !== 1 || typeof envelope.request !== 'string') throw new Error(`Invalid ${lower} envelope.`);
          if (envelope.request !== pending?.id) return '';
          if (typeof envelope.error === 'string' && envelope.error) throw new Error(envelope.error);
          finish(options.decode(envelope));
          return '';
        } catch (error) {
          finish(undefined, error instanceof Error ? error : new Error(String(error)));
          throw error;
        }
      },
    },
  };
  return {
    services,
    capture(): Promise<T> {
      if (closed || !request) return Promise.reject(new Error(`${label} is not connected to a live root.`));
      if (pending) return pending.promise;
      if (sequence === Number.MAX_SAFE_INTEGER) return Promise.reject(new Error(`${label} request identities exhausted.`));
      let resolve!: (value: T) => void, reject!: (error: Error) => void;
      const promise = new Promise<T>((accept, fail) => { resolve = accept; reject = fail; });
      const id = String(++sequence);
      pending = { id, promise, resolve, reject };
      timer = setTimeout(() => finish(undefined, new Error(`${label} did not respond within five seconds.`)), 5000);
      request(id);
      return promise;
    },
    close(): void {
      if (closed) return;
      closed = true; request = undefined;
      finish(undefined, new Error(`${label} is closed.`));
    },
  };
}
