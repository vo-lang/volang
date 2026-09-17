import { MAX_MUTATIONS } from './generated/protocol.js';
import type { Command } from './generated/protocol.js';
import { fetchHTTP, fetchText } from './http.js';

export type TaskProvider = (value: string, signal: AbortSignal) => Promise<string>;
export type TaskProviders = Readonly<Record<string, TaskProvider>>;
/** Install a source synchronously; release owned resources when signal aborts. */
export type WatchProvider = (value: string, signal: AbortSignal, emit: (value: string, error?: string) => void) => void;
export type WatchProviders = Readonly<Record<string, WatchProvider>>;
interface PendingTask { controller: AbortController; timer?: ReturnType<typeof setTimeout> }

const builtins: TaskProviders = {
  'web.fetch-text': fetchText,
  'web.http': fetchHTTP,
  'web.delay'(value, signal) {
    const request = JSON.parse(value);
    if (!Number.isSafeInteger(request.milliseconds) || request.milliseconds < 0
      || request.milliseconds > 2_147_483_647 || typeof request.value !== 'string') {
      throw new Error('invalid delay request');
    }
    return new Promise((resolve, reject) => {
      const abort = () => { clearTimeout(timer); reject(new Error('cancelled')); };
      const timer = setTimeout(() => { signal.removeEventListener('abort', abort); resolve(request.value); }, request.milliseconds);
      signal.addEventListener('abort', abort, { once: true });
      if (signal.aborted) abort();
    });
  },
};

/** One owner per root; completion never directly invokes a guest closure. */
export class TaskHost {
  private readonly tasks = new Map<number, PendingTask>();
  private readonly providers: TaskProviders;
  private readonly watchers: WatchProviders;
  private highestId = 0;
  private closed = false;

  constructor(private readonly emit: (id: number, value: string, error: string) => void, providers: TaskProviders = {}, watchers: WatchProviders = {}) {
    this.providers = Object.freeze({ ...builtins, ...providers });
    this.watchers = Object.freeze({ ...watchers });
  }

  prepare(commands: Command[] | null | undefined): void {
    if (this.closed) throw new Error('UI task host is closed');
    if (commands == null) return;
    if (!Array.isArray(commands) || commands.length > MAX_MUTATIONS) throw new Error('invalid UI task command count');
    let highest = this.highestId;
    for (const command of commands) {
      if (!command || !Number.isSafeInteger(command.id) || command.id <= 0
        || typeof command.name !== 'string' || typeof command.value !== 'string'
        || !Number.isSafeInteger(command.timeoutMilliseconds) || command.timeoutMilliseconds < 0
        || command.timeoutMilliseconds > 2_147_483_647) throw new Error('invalid UI task command');
      if (command.op === 'start' || command.op === 'watch') {
        const providers = command.op === 'watch' ? this.watchers : this.providers;
        if (!Object.prototype.hasOwnProperty.call(providers, command.name)) throw new Error(`unknown UI task service ${JSON.stringify(command.name.slice(0, 128))}`);
        if (command.id <= highest) throw new Error(`stale UI task id ${command.id}`);
        highest = command.id;
      } else if (command.op !== 'cancel' || command.id > highest) throw new Error('invalid UI task cancellation');
    }
  }

  apply(commands: Command[] | null | undefined): void {
    if (this.closed) throw new Error('UI task host is closed');
    for (const command of commands ?? []) {
      if (command.op === 'cancel') {
        this.cancel(command.id);
        continue;
      }
      this.highestId = command.id;
      const controller = new AbortController();
      const pending: PendingTask = { controller };
      this.tasks.set(command.id, pending);
      const complete = (value: string, error: string): void => {
        if (this.closed || this.tasks.get(command.id) !== pending) return;
        if (command.op !== 'watch' || error !== '') {
          this.tasks.delete(command.id);
          clearTimeout(pending.timer);
        }
        this.emit(command.id, value, error);
        if (command.op === 'watch' && error !== '') controller.abort();
      };
      if (command.timeoutMilliseconds > 0) {
        pending.timer = setTimeout(() => {
          complete('', `Request timed out after ${command.timeoutMilliseconds} ms`);
          controller.abort();
        }, command.timeoutMilliseconds);
      }
      // Enter through a promise so synchronous provider errors use the same path.
      Promise.resolve().then(() => {
        if (controller.signal.aborted) return;
        if (command.op === 'watch') {
          this.watchers[command.name](command.value, controller.signal, (value, error = '') => {
            if (typeof value !== 'string' || typeof error !== 'string') { complete('', 'UI subscription provider returned a non-string result'); return; }
            complete(value, error);
          });
          return;
        }
        return this.providers[command.name](command.value, controller.signal);
      }).then(value => {
        if (command.op === 'watch' || controller.signal.aborted) return;
        if (typeof value !== 'string') throw new Error('UI task provider returned a non-string result');
        complete(value, '');
      }).catch(error => complete('', String(error?.message ?? error)));
    }
  }

  cancelPending(commands: Command[] | null | undefined): void {
    for (const command of commands ?? []) {
      if (command.op !== 'cancel') continue;
      this.cancel(command.id);
    }
  }

  private cancel(id: number): void {
    const pending = this.tasks.get(id);
    if (!pending) return;
    this.tasks.delete(id);
    clearTimeout(pending.timer);
    pending.controller.abort();
  }

  close(): void {
    if (this.closed) return;
    this.closed = true;
    const pending = [...this.tasks.values()];
    this.tasks.clear();
    for (const task of pending) { clearTimeout(task.timer); task.controller.abort(); }
  }
}
