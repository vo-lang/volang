import { Channel } from '@tauri-apps/api/core';
import type { StreamHandle } from '../types';

export function makeStreamHandleFromProducer<T>(
  start: (emit: (msg: T) => void, onDone: () => void, onError: (err: Error) => void) => void,
): StreamHandle<T> {
  const queue: T[] = [];
  const waiters: Array<{ resolve: (result: IteratorResult<T>) => void; reject: (err: Error) => void }> = [];
  let done = false;
  let error: Error | null = null;

  function enqueue(msg: T): void {
    if (done) return;
    if (waiters.length > 0) {
      waiters.shift()!.resolve({ value: msg, done: false });
    } else {
      queue.push(msg);
    }
  }

  function finish(err?: Error): void {
    if (done) return;
    if (err) error = err;
    done = true;
    while (waiters.length > 0) {
      const waiter = waiters.shift()!;
      if (error) {
        waiter.reject(error);
      } else {
        waiter.resolve({ value: undefined as unknown as T, done: true });
      }
    }
  }

  start(enqueue, () => finish(), (e) => finish(e));

  return {
    [Symbol.asyncIterator]() {
      return {
        next(): Promise<IteratorResult<T>> {
          if (queue.length > 0) {
            return Promise.resolve({ value: queue.shift()!, done: false });
          }
          if (done) {
            if (error) return Promise.reject(error);
            return Promise.resolve({ value: undefined as unknown as T, done: true });
          }
          return new Promise<IteratorResult<T>>((resolve, reject) => {
            waiters.push({ resolve, reject });
          });
        },
        return(): Promise<IteratorResult<T>> {
          finish();
          return Promise.resolve({ value: undefined as unknown as T, done: true });
        },
      };
    },
    cancel() {
      finish();
    },
  };
}

export function makeTauriStreamHandle<T>(
  invokeSetup: (channel: Channel<T>) => Promise<void>,
  isCompletionEvent: (msg: T) => boolean = () => false,
): StreamHandle<T> {
  const channelRefs: Channel<T>[] = [];
  return makeStreamHandleFromProducer<T>((emit, onDone, onError) => {
    const ch = new Channel<T>();
    channelRefs.push(ch);
    ch.onmessage = (msg) => {
      emit(msg);
      if (isCompletionEvent(msg)) {
        channelRefs.length = 0;
        onDone();
      }
    };
    invokeSetup(ch).catch((error) => {
      channelRefs.length = 0;
      onError(error);
    });
  });
}

export function makeResolvedStreamHandle<T>(values: T[]): StreamHandle<T> {
  let index = 0;
  return {
    [Symbol.asyncIterator]() {
      return {
        next(): Promise<IteratorResult<T>> {
          if (index < values.length) {
            return Promise.resolve({ value: values[index++]!, done: false });
          }
          return Promise.resolve({ value: undefined as unknown as T, done: true });
        },
        return(): Promise<IteratorResult<T>> {
          index = values.length;
          return Promise.resolve({ value: undefined as unknown as T, done: true });
        },
      };
    },
    cancel() {
      index = values.length;
    },
  };
}

export function makeErrorStreamHandle<T>(message: string): StreamHandle<T> {
  let consumed = false;
  return {
    [Symbol.asyncIterator]() {
      return {
        next(): Promise<IteratorResult<T>> {
          if (!consumed) {
            consumed = true;
            return Promise.reject(new Error(message));
          }
          return Promise.resolve({ value: undefined as unknown as T, done: true });
        },
      };
    },
    cancel() {
      consumed = true;
    },
  };
}
