// Aliased only for configured development entries. Production uses mount.ts.
import { mountUi as mountApplication, type MountOptions, type UiApplication } from './mount.js';
import { createInspectionServices } from './inspection.js';
import { attachInspectionPanel } from './inspection-panel.js';
import { createReloadServices, type ReloadReport } from './reload.js';
import { captureReloadInputs } from './reload-inputs.js';
import { closeOnPageExit } from './page.js';
export type { MountOptions, UiApplication } from './mount.js';
export { createNavigationServices } from './navigation.js';
export { createLazyWidget } from './lazy-widget.js';

export function mountUi(container: HTMLElement, options: MountOptions): UiApplication {
  const window = container.ownerDocument.defaultView!;
  const source = { label: container.id || 'Application' };
  let closed = false, building = false, composing = false, queued: string | undefined;
  let detachExit = () => {};
  let staging: Instance | undefined;
  let finish!: () => void, fail!: (error: unknown) => void;
  const done = new Promise<void>((resolve, reject) => { finish = resolve; fail = reject; });
  void done.catch(() => {});
  const status = (type: 'ready' | 'error' | 'closed', message = '', report?: ReloadReport) => {
    window.dispatchEvent(new CustomEvent('vo-ui-reload-status', { detail: { source, type, message, report } }));
  };
  type Instance = ReturnType<typeof start>;
  const start = (element: HTMLElement, overrides: Partial<MountOptions> = {}, bootstrap = true) => {
    const inspector = createInspectionServices();
    const reload = createReloadServices(report => status('ready', '', report));
    const settings = { ...options, ...overrides };
    const services = typeof settings.services === 'function' ? settings.services(element) : settings.services;
    const initial = services?.initialData;
    const application = mountApplication(element, { ...settings, services: {
      ...services,
      initialData: bootstrap ? async signal => JSON.stringify({ version: 1, state: '', data: typeof initial === 'function' ? await initial(signal) : initial ?? '' }) : initial,
      tasks: { ...services?.tasks, ...inspector.services.tasks, ...reload.services.tasks },
      watches: { ...services?.watches, ...inspector.services.watches, ...reload.services.watches },
    } });
    let detach: (() => void) | undefined, released = false;
    const release = () => {
      if (released) return;
      released = true; detach?.(); detach = undefined; inspector.close(); reload.close();
    };
    const instance = { element, application, reload, release };
    void application.ready.then(ready => {
      if (ready && !released && !closed) detach = attachInspectionPanel(element, inspector);
    });
    void application.done.then(() => {
      release(); if (active === instance) { remove(); finish(); }
    }, error => {
      release(); if (active === instance) { remove(); fail(error); }
    });
    return instance;
  };
  let active = start(container);
  const ready = active.application.ready;
  const refresh = async () => {
    if (building || closed || composing) return;
    building = true;
    try {
      while (queued !== undefined && !closed && !composing) {
        const version = queued; queued = undefined;
        const previous = active;
        let unfreeze: (() => void) | undefined, restoreInputs: ReturnType<typeof captureReloadInputs> | undefined;
        let timer: ReturnType<typeof setTimeout> | undefined, activated = false, bootstrapped = false, deferred = false;
        let scroll: { x: number; y: number } | undefined;
        const artifact = new URL(String(options.artifact), window.location.href);
        artifact.searchParams.set('ui-dev', version);
        const replacement = previous.element.cloneNode(false) as HTMLElement;
        try {
          const services = typeof options.services === 'function' ? options.services(replacement) : options.services;
          const candidate = start(replacement, { artifact, hydrate: false, services: {
            ...services,
            async initialData(signal) {
              signal.throwIfAborted();
              if (closed || active !== previous) throw new Error('Reload was superseded.');
              if (composing) { deferred = true; queued ??= version; throw new Error('Reload waits for composition to finish.'); }
              restoreInputs = captureReloadInputs(previous.element);
              const inert = previous.element.inert;
              previous.element.inert = true;
              unfreeze = () => { previous.element.inert = inert; };
              const state = await previous.reload.capture();
              signal.throwIfAborted();
              bootstrapped = true;
              const initial = services?.initialData;
              const data = typeof initial === 'function' ? await initial(signal) : initial ?? '';
              signal.throwIfAborted();
              return JSON.stringify({ version: 1, state, data });
            },
            activation: {
              before() {
                if (closed || active !== previous || !previous.element.parentNode) throw new Error('Reload target is no longer mounted.');
                if (!bootstrapped) throw new Error('The development entry must call develop.Run to restore state.');
                services?.activation?.before();
                scroll = { x: window.scrollX, y: window.scrollY };
                active = candidate; activated = true;
                previous.release(); previous.application.close();
                previous.element.replaceWith(replacement);
              },
              after(batch) {
                restoreInputs?.(replacement, batch);
                if (scroll) window.scrollTo({ left: scroll.x, top: scroll.y, behavior: 'instant' });
                services?.activation?.after(batch);
              },
            },
          } }, false);
          staging = candidate;
          await Promise.race([
            candidate.application.ready.then(ready => { if (!ready) return candidate.application.done.then(() => { throw new Error('Replacement UI did not become ready.'); }); }),
            candidate.application.done.then(() => { throw new Error('Replacement UI exited during startup.'); }),
            new Promise<never>((_, reject) => { timer = setTimeout(() => reject(new Error('UI reload exceeded fifteen seconds.')), 15000); }),
          ]);
          status('ready');
        } catch (error) {
          if (!activated) { staging?.release(); staging?.application.close(); }
          if (!closed && !deferred) status('error', String(error instanceof Error ? error.message : error));
        } finally {
          clearTimeout(timer); unfreeze?.();
          if (!activated && !closed) restoreInputs?.(previous.element);
          staging = undefined;
        }
      }
    } finally { building = false; }
  };
  const onReload = (event: Event) => {
    if (closed) return;
    event.preventDefault();
    queued = String((event as CustomEvent).detail?.version ?? Date.now());
    void refresh();
  };
  const compositionStart = (event: Event) => { if (event.target instanceof Node && active.element.contains(event.target)) composing = true; };
  const compositionEnd = (event: Event) => {
    if (!(event.target instanceof Node) || !active.element.contains(event.target)) return;
    composing = false; queueMicrotask(() => void refresh());
  };
  const remove = () => {
    detachExit();
    window.removeEventListener('vo-ui-reload', onReload);
    window.removeEventListener('compositionstart', compositionStart, true);
    window.removeEventListener('compositionend', compositionEnd, true);
  };
  window.addEventListener('vo-ui-reload', onReload);
  window.addEventListener('compositionstart', compositionStart, true);
  window.addEventListener('compositionend', compositionEnd, true);
  const close = (): void => {
    if (closed) return;
    closed = true; queued = undefined; remove();
    staging?.release(); staging?.application.close();
    active.release(); active.application.close(); status('closed'); finish();
  };
  detachExit = closeOnPageExit(window, close);
  return {ready, done, close};
}
