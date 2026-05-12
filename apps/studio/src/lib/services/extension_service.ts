import { writable, type Readable } from 'svelte/store';

import type { Backend } from '../backend/backend';
import type { InstalledModule, InstallEvent, StreamHandle } from '../types';
import { formatError } from '../format_error';

export type InstallStatus = 'idle' | 'fetching' | 'building' | 'done' | 'error';

export interface InstallProgress {
  spec: string;
  status: InstallStatus;
  message: string;
}

export interface ExtensionState {
  installed: InstalledModule[];
  inProgress: InstallProgress[];
  lastError: string | null;
}

export class ExtensionService {
  private readonly stateStore = writable<ExtensionState>({
    installed: [],
    inProgress: [],
    lastError: null,
  });

  constructor(private readonly backend: Backend) {}

  get state(): Readable<ExtensionState> {
    return { subscribe: this.stateStore.subscribe };
  }

  async loadInstalled(): Promise<InstalledModule[]> {
    try {
      const modules = await this.backend.listInstalledModules();
      this.stateStore.update((s) => ({ ...s, installed: modules }));
      return modules;
    } catch (error) {
      this.stateStore.update((s) => ({ ...s, lastError: formatError(error) }));
      return [];
    }
  }

  install(spec: string): StreamHandle<InstallEvent> {
    this.stateStore.update((s) => ({
      ...s,
      inProgress: [...s.inProgress, { spec, status: 'fetching', message: `Installing ${spec}...` }],
    }));

    const stream = this.backend.voGet(spec);
    this.consumeInstallStream(stream, spec);
    return stream;
  }

  private consumeInstallStream(stream: StreamHandle<InstallEvent>, spec: string): void {
    (async () => {
      try {
        for await (const event of stream) {
          if (event.kind === 'fetch') {
            this.updateProgress(spec, 'fetching', event.message);
          } else if (event.kind === 'build') {
            this.updateProgress(spec, 'building', event.line);
          } else if (event.kind === 'done') {
            this.stateStore.update((s) => ({
              ...s,
              inProgress: s.inProgress.filter((p) => p.spec !== spec),
            }));
            await this.loadInstalled();
          } else if (event.kind === 'error') {
            this.updateProgress(spec, 'error', event.message);
          }
        }
      } catch (error) {
        this.updateProgress(spec, 'error', formatError(error));
      }
    })();
  }

  private updateProgress(spec: string, status: InstallStatus, message: string): void {
    this.stateStore.update((s) => ({
      ...s,
      inProgress: s.inProgress.map((p) => p.spec === spec ? { ...p, status, message } : p),
    }));
  }

  async voVersion(): Promise<string> {
    return this.backend.voVersion();
  }

  async voInit(path: string, name?: string): Promise<string> {
    return this.backend.voInit(path, name);
  }
}

