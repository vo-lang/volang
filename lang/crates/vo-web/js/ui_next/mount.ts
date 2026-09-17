import { createUiTransport, type UiServices, type UiTransport } from './host.js';
import { closeOnPageExit } from './page.js';
import { initializeUiVm, runVmUi, type UiVmRuntime } from './vm.js';
export type { UiVmRuntime } from './vm.js';
// Optional browser services remain explicit exports so simple entries can omit
// their adapters from the bundle. Factories bind to the actual mounted root.
export { createNavigationServices } from './navigation.js';
export { createLazyWidget } from './lazy-widget.js';
export { createPersistentStorage } from './storage.js';

const mountedRoots = new WeakMap<HTMLElement, UiTransport>();

export interface MountOptions {
  backend?: 'vm';
  artifact: string | URL;
  /** The matching Wasm VM module, loaded alongside the application bytecode. */
  loadVm: () => Promise<UiVmRuntime>;
  hydrate?: boolean;
  /** Factories create adapters bound to each mounted root's actual container. */
  services?: UiServices | ((container: HTMLElement) => UiServices);
}

export interface UiApplication {
  /** True after the initial commit, effects and input subscriptions are ready. */
  ready: Promise<boolean>;
  /** Settles when the guest exits; loading and execution failures reject here. */
  done: Promise<void>;
  /** Cancels loading or closes the live root. Safe before ready and after done. */
  close(): void;
}

/** Own artifact loading, guest execution and cleanup through one application lifetime. */
export function mountUi(container: HTMLElement, options: MountOptions): UiApplication {
  if (options.backend !== undefined && options.backend !== 'vm') throw new Error('Unknown UI backend.');
  const controller = new AbortController();
  const services = typeof options.services === 'function' ? options.services(container) : options.services;
  let closed = false;
  let host: UiTransport | undefined;
  let finishReady: ((interactive: boolean) => void) | undefined;
  const ready = new Promise<boolean>(resolve => { finishReady = resolve; });
  const settleReady = (interactive: boolean): void => { finishReady?.(interactive); finishReady = undefined; };
  const releaseRoot = (): void => {
    if (host && mountedRoots.get(container) === host) mountedRoots.delete(container);
  };
  const close = (): void => {
    if (closed) return;
    closed = true;
    detachExit();
    controller.abort();
    try { host?.close(); }
    finally { releaseRoot(); settleReady(false); }
  };
  const detachExit = closeOnPageExit(container.ownerDocument.defaultView, close);
  const done = (async () => {
    // Own SSR listeners before the first asynchronous load. The same bounded
    // queue then preserves actions in order until the guest can receive them.
    if (mountedRoots.has(container)) throw new Error('UI root already has a mounted application.');
    const transport = host = createUiTransport(container, options.hydrate ?? container.hasChildNodes(), services);
    mountedRoots.set(container, transport);
    void transport.ready.then(settleReady);
    const artifact = (async () => {
      const response = await fetch(options.artifact, { signal: controller.signal });
      if (!response.ok) throw new Error(`Could not load UI artifact (${response.status} ${response.statusText}).`);
      return new Uint8Array(await response.arrayBuffer());
    })();
    const runtime = (async () => {
      const wasm = await options.loadVm();
      // Imports and shared initialization may outlive a cancelled application.
      // They must never initialize a late loader or create an orphan Island.
      if (controller.signal.aborted) return;
      await initializeUiVm(wasm);
      return wasm;
    })();
    // Attach both rejection handlers immediately. A failed load cancels the
    // owned fetch without interrupting another root's shared initialization.
    const [bytes, wasm] = await Promise.all([artifact, runtime]);
    if (closed || !wasm) return;
    const vm = new wasm.VoVmIsland(bytes);
    try {
      await runVmUi(vm, bytes => transport.exchange(bytes));
    } finally {
      try { transport.close(false); }
      finally { vm.free(); }
    }

  })().catch(error => { if (!closed) throw error; }).finally(() => {
    controller.abort();
    detachExit();
    try { host?.close(false); }
    finally { releaseRoot(); settleReady(false); }
  });
  // Callers may await ready before attaching their error UI to done.
  void done.catch(() => {});
  return { ready, done, close };
}
