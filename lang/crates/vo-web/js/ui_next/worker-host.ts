import { createUiTransport, type UiServices } from './host.js';
import type { UiApplication } from './mount.js';
import { MAX_FRAME_BYTES } from './generated/protocol.js';
import { closeOnPageExit } from './page.js';

export interface WorkerUiOptions {
  hydrate?: boolean;
  services?: UiServices;
  /** Includes runtime loading and any application-owned compilation. Default 30s. */
  startupTimeoutMilliseconds?: number;
  /** Guest computation between exchanges; idle input waits have no deadline. Default 10s. */
  turnTimeoutMilliseconds?: number;
}

/** Takes ownership of a dedicated Worker. Call before sending its start request.
 * DOM, input and browser services stay here; computation can always be stopped. */
export function createWorkerUi(container: HTMLElement, worker: Worker, options: WorkerUiOptions = {}): UiApplication {
  const startup = options.startupTimeoutMilliseconds ?? 30000, turn = options.turnTimeoutMilliseconds ?? 10000;
  if (![startup, turn].every(value => Number.isSafeInteger(value) && value >= 1 && value <= 120000)) {
    throw new Error('UI worker deadlines must be within 1..120000 milliseconds');
  }
  const transport = createUiTransport(container, options.hydrate ?? false, options.services);
  let closed = false, exchanging = false, interactive = false, sequence = 0;
  let detachExit = () => {};
  let timer: ReturnType<typeof setTimeout>;
  let complete!: () => void, fail!: (error: unknown) => void;
  const done = new Promise<void>((resolve, reject) => { complete = resolve; fail = reject; });
  void done.catch(() => {});
  const finish = (error?: unknown): void => {
    if (closed) return;
    closed = true;
    detachExit();
    clearTimeout(timer);
    worker.removeEventListener('message', receive);
    worker.removeEventListener('error', crashed);
    worker.removeEventListener('messageerror', unreadable);
    worker.terminate();
    try { transport.close(); } catch (cause) { error ??= cause; }
    if (error) fail(error); else complete();
  };
  const deadline = (milliseconds: number): void => {
    clearTimeout(timer);
    timer = setTimeout(() => finish(new Error(`UI worker did not respond within ${milliseconds} ms`)), milliseconds);
  };
  const receive = (event: MessageEvent): void => {
    void (async () => {
      if (closed) return;
      const message = event.data;
      if (message?.kind === 'ui-exit' && typeof message.error === 'string' && message.error.length <= 65536) {
        finish(message.error ? new Error(message.error) : undefined);
        return;
      }
      if (message?.kind !== 'ui-exchange' || exchanging || message.sequence !== sequence + 1
        || !Number.isSafeInteger(message.sequence) || !(message.bytes instanceof Uint8Array)
        || message.bytes.length > MAX_FRAME_BYTES) throw new Error('Invalid or overlapping UI worker exchange');
      exchanging = true;
      sequence = message.sequence;
      if (interactive || message.bytes.length > 0) clearTimeout(timer);
      const bytes = await transport.exchange(message.bytes);
      if (closed) return;
      exchanging = false;
      deadline(turn);
      worker.postMessage({ kind: 'ui-input', sequence, bytes }, [bytes.buffer]);
    })().catch(finish);
  };
  const crashed = (event: ErrorEvent): void => { event.preventDefault(); finish(new Error(event.message || 'UI worker failed')); };
  const unreadable = (): void => finish(new Error('Unreadable UI worker message'));
  detachExit = closeOnPageExit(container.ownerDocument.defaultView, () => finish());
  worker.addEventListener('message', receive);
  worker.addEventListener('error', crashed);
  worker.addEventListener('messageerror', unreadable);
  deadline(startup);
  void transport.ready.then(value => {
    interactive = value;
    if (value && exchanging) clearTimeout(timer);
  });
  return { ready: transport.ready, done, close() { finish(); } };
}
