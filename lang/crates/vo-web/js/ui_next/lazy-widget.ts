import type { WidgetInstance, WidgetProvider } from './widgets.js';

export type WidgetLoader = (document: Document, signal: AbortSignal) => Promise<WidgetProvider>;
export interface LazyWidgetOptions { timeoutMilliseconds?: number }
type Loaded = { provider: WidgetProvider } | { error: unknown };
type Receive = (result: Loaded) => void;

// A shared import may remain pending after its widgets are removed. Its single
// observer holds only this subscriber set; cancellation releases the root.
const pendingLoads = new WeakMap<Promise<WidgetProvider>, Set<Receive>>();
function observe(promise: Promise<WidgetProvider>, receive: Receive): () => void {
  let subscribers = pendingLoads.get(promise);
  if (!subscribers) {
    subscribers = new Set();
    pendingLoads.set(promise, subscribers);
    const group = subscribers;
    const complete = (result: Loaded): void => {
      pendingLoads.delete(promise);
      const pending = [...group];
      group.clear();
      for (const notify of pending) notify(result);
    };
    void promise.then(provider => complete({ provider }), error => complete({ error }));
  }
  subscribers.add(receive);
  return () => subscribers.delete(receive);
}

/** Load an optional integration on its first mounted use. The loader receives
 * the document and cancellation, while the synchronous provider owns its DOM. */
export function createLazyWidget(load: WidgetLoader, options: LazyWidgetOptions = {}): WidgetProvider {
  const timeout = options.timeoutMilliseconds ?? 15000;
  if (typeof load !== 'function' || !Number.isSafeInteger(timeout) || timeout < 1 || timeout > 60000) {
    throw new Error('lazy widget requires a loader and a deadline between 1 and 60000 milliseconds');
  }
  return ({ element, value, signal, emit, fail }) => {
    signal.throwIfAborted();
    let latest = value, instance: WidgetInstance | undefined, disposed = false;
    let timer: ReturnType<typeof setTimeout> | undefined, unsubscribe: (() => void) | undefined;
    const stopWaiting = (): void => {
      clearTimeout(timer); timer = undefined;
      unsubscribe?.(); unsubscribe = undefined;
    };
    const cancel = (): void => {
      stopWaiting();
      disposed = true;
      signal.removeEventListener('abort', cancel);
      latest = '';
    };
    const dispose = (): void => {
      cancel();
      const current = instance;
      instance = undefined;
      current?.dispose();
    };
    signal.addEventListener('abort', cancel, { once: true });
    try {
      const pending = Promise.resolve(load(element.ownerDocument, signal));
      unsubscribe = observe(pending, result => {
        if (disposed) return;
        stopWaiting();
        try {
          if ('error' in result) throw result.error;
          if (typeof result.provider !== 'function') throw new Error('lazy widget loader did not return a provider');
          const mounted = result.provider({ element, value: latest, signal, emit, fail });
          latest = '';
          if (disposed) mounted.dispose();
          else { instance = mounted; instance.afterCommit?.(); }
        } catch (error) { fail(String((error as Error)?.message ?? error)); }
      });
      signal.throwIfAborted();
      timer = setTimeout(() => fail('widget loading timed out'), timeout);
    } catch (error) { dispose(); throw error; }
    return {
      update(value) {
        if (disposed) return;
        if (instance) instance.update(value);
        else latest = value;
      },
      afterCommit() { if (!disposed) instance?.afterCommit?.(); },
      commitTargets() { return instance ? instance.afterCommit ? instance.commitTargets?.() : [] : undefined; },
      dispose,
    };
  };
}
