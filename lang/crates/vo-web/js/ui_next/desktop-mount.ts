/** Framework-owned system WebView entry. Uses the same DOM/lifecycle host as Web. */
import {createUiTransport} from './host.js';
import {MAX_FRAME_BYTES} from './generated/protocol.js';

interface DesktopWindow extends Window {
  ipc: {postMessage(message: string): void};
  __volangDesktop?: {
    ready: Promise<boolean>;
    receive(id: number, data: string): Promise<void>;
    close(): void;
    dispose(): void;
    failHost(message: string): void;
    fail(message: string): void;
  };
}

import type {MountOptions, UiApplication} from './mount.js';
import type {MediaSources} from './media-sources.js';
export {createDesktopNavigationServices as createNavigationServices} from './navigation.js';
export {createLazyWidget} from './lazy-widget.js';
export {createPersistentStorage} from './storage.js';

/** Native execution uses the same authored boot/services and one document root. */
export function mountUi(container: HTMLElement, options: Pick<MountOptions, 'hydrate' | 'services'> = {}): UiApplication {
  const owner = window as unknown as DesktopWindow;
  const configuration = document.getElementById('volang-desktop-config');
  const root = container;
  if (owner.__volangDesktop) throw new Error('Desktop document already has an application');
  if (!configuration || !root) throw new Error('Missing desktop document bootstrap');
  const {token, media} = JSON.parse(configuration.textContent ?? '{}') as {token: unknown; media?: MediaSources};
  configuration.remove();
  if (typeof token !== 'string' || !/^[a-f0-9]{32}$/.test(token)) throw new Error('Invalid desktop document identity');
  const services = typeof options.services === 'function' ? options.services(root) : options.services;
  const transport = createUiTransport(root, options.hydrate ?? false, services,
    media && Object.keys(media).length ? media : undefined);
  let finish!: () => void;
  let reject!: (error: Error) => void;
  const done = new Promise<void>((resolve, failure) => { finish = resolve; reject = failure; });
  void done.catch(() => {});
  const send = (message: unknown) => owner.ipc.postMessage(JSON.stringify({token, message}));
  let receiving = false;
  let disposed = false;
  let sequence = 0;
  const events = new AbortController();

  function encode(bytes: Uint8Array): string {
    if (bytes.length > MAX_FRAME_BYTES) throw new Error('Desktop response exceeds frame limit');
    const chunks: string[] = [];
    for (let offset = 0; offset < bytes.length; offset += 8192) {
      chunks.push(String.fromCharCode(...bytes.subarray(offset, offset + 8192)));
    }
    return btoa(chunks.join(''));
  }

  function dispose(): void {
    disposed = true;
    events.abort();
    transport.close();
    finish();
  }

  function fail(error: unknown): void {
    if (disposed) return;
    const message = String(error).slice(0, 4000);
    failHost(message);
    send({kind: 'failure', message});
  }

  window.addEventListener('error', event => fail(event.error ?? event.message), {signal: events.signal});
  window.addEventListener('unhandledrejection', event => fail(event.reason), {signal: events.signal});
  // A native session belongs to this document. An ordinary page load cannot
  // reconnect its pending exchange; terminate it while the old IPC is alive.
  window.addEventListener('pagehide', () => fail('The application document closed. Reopen the application to start a new session.'), {signal: events.signal});

  function failHost(message: string): void {
    reject(new Error(message));
    try { dispose(); } finally {
      const panel = document.createElement('section');
      panel.setAttribute('role', 'alert');
      panel.style.cssText = 'margin:40px auto;padding:24px;max-width:800px;font:15px/1.6 system-ui';
      const heading = document.createElement('h1');
      heading.textContent = 'Application stopped';
      const detail = document.createElement('pre');
      detail.style.cssText = 'white-space:pre-wrap;overflow-wrap:anywhere';
      detail.textContent = message;
      panel.append(heading, detail);
      root!.replaceChildren(panel);
    }
  }

  owner.__volangDesktop = Object.freeze({
    ready: transport.ready,
    async receive(id: number, data: string): Promise<void> {
      if (disposed) return;
      try {
        if (receiving || !Number.isSafeInteger(id) || id !== sequence + 1) throw new Error('Unexpected desktop exchange identity');
        if (data.length > Math.ceil(MAX_FRAME_BYTES / 3) * 4) throw new Error('Desktop output exceeds frame limit');
        receiving = true;
        sequence = id;
        const bytes = Uint8Array.from(atob(data), char => char.charCodeAt(0));
        if (bytes.length > MAX_FRAME_BYTES) throw new Error('Desktop output exceeds frame limit');
        const reply = await transport.exchange(bytes);
        if (!disposed) send({kind: 'reply', id, data: encode(reply)});
      } catch (error) {
        fail(error);
      } finally { receiving = false; }
    },
    close() { transport.close(); },
    dispose,
    failHost,
    fail,
  });
  send({kind: 'start'});

  return {ready: transport.ready, done, close: () => transport.close()};
}
