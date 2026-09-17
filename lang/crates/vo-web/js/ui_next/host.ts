import { DomRenderer, InputQueue, decodeBatch } from './renderer.js';
import { encodeCommitAck, decodeBootstrap, encodeBootstrap } from './generated/codec.js';
import { KIND_BOOTSTRAP } from './generated/protocol.js';
import type { Batch } from './generated/protocol.js';
import { TaskHost } from './tasks.js';
import { pageWatches } from './page.js';
import {FileHost} from './files.js';
import { motionTasks } from './motion.js';
import { runVmUi, type UiVm } from './vm.js';
import type { TaskProviders, WatchProviders } from './tasks.js';
import type { WidgetProviders } from './widgets.js';

// Optional lab counters; applications pay no timing/aggregation cost by default.
export interface UiMetrics { exchanges: number; batches: number; mutations: number; guestBytes: number; inputBytes: number; hostMs: number }
export interface UiServices {
  initialData?: string | ((signal: AbortSignal) => Promise<string>);
  tasks?: TaskProviders; watches?: WatchProviders; widgets?: WidgetProviders; metrics?: UiMetrics;
  /** Optional adapter handoff, after preflight and before initial effects. */
  activation?: { before(): void; after(batch: Batch): void };
}

function acknowledge(revision: number): Uint8Array {
  return encodeCommitAck({ revision });
}

export interface UiTransport {
  readonly ready: Promise<boolean>;
  exchange(bytes: Uint8Array): Promise<Uint8Array>;
  close(remove?: boolean): void;
}

/** One serialized, bounded wire connection to an owned renderer root. */
export function createUiTransport(container: HTMLElement, hydrate = false, services: UiServices = {}): UiTransport {
  return new HostBoundary(container, hydrate, services);
}

class HostBoundary {
  readonly input = new InputQueue();
  readonly renderer: DomRenderer;
  private readonly tasks: TaskHost;
  private readonly files = new FileHost();
  private closed = false;
  private initialized = false;
  private committed = false;
  private bootstrap?: AbortController;
  private finishReady?: (interactive: boolean) => void;
  // True after the first commit and its initial task/effect commands, when the
  // guest first waits for input. Closing before that point resolves false.
  readonly ready = new Promise<boolean>(resolve => { this.finishReady = resolve; });

  constructor(container: HTMLElement, hydrate: boolean, private readonly services: UiServices) {
    this.renderer = new DomRenderer(container, (event, latest) => this.input.push(event, latest), hydrate, {...this.files.widgets, ...services.widgets});
    this.tasks = new TaskHost((id, value, error) => this.renderer.post('@task', id, value, error),
      { ...this.files.tasks, ...motionTasks(container), 'ui.measure': async value => this.renderer.measure(value), ...services.tasks },
      { ...pageWatches(container.ownerDocument), ...services.watches });
  }
  async exchange(bytes: Uint8Array): Promise<Uint8Array> {
    if (this.closed) return new Uint8Array();
    const metrics = this.services.metrics;
    if (metrics) metrics.exchanges++;
    if (bytes[4] === KIND_BOOTSTRAP) {
      if (this.initialized || decodeBootstrap(bytes).data !== '') throw new Error('invalid or repeated UI bootstrap');
      this.initialized = true;
      const initial = this.services.initialData;
      let data = typeof initial === 'string' ? initial : '';
      if (typeof initial === 'function') {
        const controller = this.bootstrap = new AbortController();
        let aborted!: () => void;
        const stopped = new Promise<string>(resolve => { aborted = () => resolve(''); });
        controller.signal.addEventListener('abort', aborted, { once: true });
        try { data = await Promise.race([Promise.resolve().then(() => controller.signal.aborted ? '' : initial(controller.signal)), stopped]); }
        finally { controller.signal.removeEventListener('abort', aborted); this.bootstrap = undefined; }
      }
      return this.closed ? new Uint8Array() : encodeBootstrap({ data });
    }
    if (bytes.length === 0) {
      if (this.committed) { this.finishReady?.(true); this.finishReady = undefined; }
      const response = await this.input.next();
      if (metrics) metrics.inputBytes += response.length;
      return response;
    }
    const started = metrics ? performance.now() : 0;
    const batch = decodeBatch(bytes);
    this.initialized = true;
    this.tasks.prepare(batch.commands);
    const revision = this.renderer.applyBatch(batch, () => {
      if (!this.committed) this.services.activation?.before();
      this.tasks.cancelPending(batch.commands);
    });
    if (!this.committed) this.services.activation?.after(batch);
    this.tasks.apply(batch.commands);
    this.committed = true;
    if (metrics) {
      metrics.batches++;
      metrics.mutations += batch.mutations?.length ?? 0;
      metrics.guestBytes += bytes.length;
      metrics.hostMs += performance.now() - started;
    }
    return acknowledge(revision);
  }
  close(remove = true): void {
    if (this.closed) return;
    this.closed = true;
    this.bootstrap?.abort(); this.bootstrap = undefined;
    this.finishReady?.(false);
    this.finishReady = undefined;
    this.tasks.close();
    this.input.close();
    try { this.renderer.close(remove); }
    finally { this.files.close(); }
  }
}

export function createVmUi(container: HTMLElement, vm: UiVm, hydrate = false, services: UiServices = {}): { done: Promise<void>; ready: Promise<boolean>; close(): void } {
  const host = new HostBoundary(container, hydrate, services);
  const done = (async () => {
    try {
      await runVmUi(vm, bytes => host.exchange(bytes));
    } finally { host.close(false); }
  })();
  return { done, ready: host.ready, close() { host.close(); } };
}
