import { get, type Readable } from 'svelte/store';

import type { Backend } from '../backend/backend';
import type { GuiRunOutput, RunEvent, RunOpts, StreamHandle } from '../types';
import { formatError } from '../format_error';
import { consoleClear, consolePush } from '../../stores/console';
import { runtime, IDLE_RUNTIME, type RuntimeState } from '../../stores/runtime';

export type { RuntimeKind, RuntimeStatus, RuntimeState } from '../../stores/runtime';

export class RuntimeService {
  private activeConsoleRunId = 0;
  private nextConsoleRunId = 0;

  constructor(private readonly backend: Backend) {}

  get state(): Readable<RuntimeState> {
    return { subscribe: runtime.subscribe };
  }

  run(target: string, opts?: RunOpts): StreamHandle<RunEvent> {
    const runMode = opts?.mode ?? 'vm';
    const runId = this.beginConsoleRun(target, runMode);
    const stream = this.backend.runVo(target, opts);
    this.consumeRunStream(stream, target, runMode, runId);
    return stream;
  }

  private consumeRunStream(stream: StreamHandle<RunEvent>, target: string, runMode: 'vm' | 'jit', runId: number): void {
    (async () => {
      try {
        for await (const event of stream) {
          if (!this.isConsoleRunActive(runId)) {
            continue;
          }
          if (event.kind === 'stdout') {
            consolePush('stdout', event.text);
            runtime.update((s) => ({ ...s, consoleLines: [...s.consoleLines, event.text] }));
          } else if (event.kind === 'stderr') {
            consolePush('stderr', event.text);
            runtime.update((s) => ({ ...s, consoleLines: [...s.consoleLines, `[err] ${event.text}`] }));
          } else if (event.kind === 'stopped') {
            this.finishConsoleRun(runId, { status: 'ready', isRunning: false });
          } else if (event.kind === 'done') {
            this.finishConsoleRun(runId, { status: 'ready', isRunning: false });
          } else if (event.kind === 'error') {
            consolePush('stderr', event.message);
            this.finishConsoleRun(runId, { status: 'ready', isRunning: false, lastError: event.message });
          }
        }
      } catch (error) {
        if (!this.isConsoleRunActive(runId)) {
          return;
        }
        const message = formatError(error);
        consolePush('stderr', message);
        this.activeConsoleRunId = 0;
        runtime.set({ ...IDLE_RUNTIME, status: 'ready', kind: 'console', target, runMode, lastError: message });
      }
    })();
  }

  async runConsole(target: string, runMode: 'vm' | 'jit'): Promise<string> {
    const runId = this.beginConsoleRun(target, runMode);
    const lines: string[] = [];
    try {
      const stream = this.backend.runVo(target, { mode: runMode });
      for await (const event of stream) {
        if (!this.isConsoleRunActive(runId)) {
          continue;
        }
        if (event.kind === 'stdout') {
          lines.push(event.text);
          consolePush('stdout', event.text);
          runtime.update((s) => ({ ...s, consoleLines: [...s.consoleLines, event.text] }));
        } else if (event.kind === 'stderr') {
          lines.push(event.text);
          consolePush('stderr', event.text);
          runtime.update((s) => ({ ...s, consoleLines: [...s.consoleLines, `[err] ${event.text}`] }));
        } else if (event.kind === 'stopped') {
          this.finishConsoleRun(runId, { status: 'ready', isRunning: false });
          return lines.join('\n');
        } else if (event.kind === 'done') {
          if (event.exitCode !== 0) throw new Error(`Exited with code ${event.exitCode}`);
        } else if (event.kind === 'error') {
          throw new Error(event.message);
        }
      }
      this.finishConsoleRun(runId, { status: 'ready', isRunning: false });
      return lines.join('\n');
    } catch (error) {
      if (!this.isConsoleRunActive(runId)) {
        return lines.join('\n');
      }
      const message = formatError(error);
      if (lines.length === 0) {
        consolePush('stderr', message);
      }
      this.activeConsoleRunId = 0;
      runtime.set({ ...IDLE_RUNTIME, status: 'ready', kind: 'console', target, runMode, lastError: message });
      throw error;
    }
  }

  async runGui(target: string): Promise<GuiRunOutput> {
    runtime.set({ ...IDLE_RUNTIME, status: 'running', kind: 'gui', target, isRunning: true });
    try {
      const output = await this.backend.runGui(target);
      runtime.set({
        ...IDLE_RUNTIME,
        status: 'ready',
        kind: 'gui',
        target,
        isRunning: true,
        guiEntryPath: output.entryPath,
        guiModuleBytes: output.moduleBytes,
        guiRenderBytes: output.renderBytes,
        guiFramework: output.framework,
        guiExternalWidgetHandlerId: output.externalWidgetHandlerId,
      });
      return output;
    } catch (error) {
      const message = formatError(error);
      consolePush('stderr', message);
      runtime.set({ ...IDLE_RUNTIME, status: 'ready', kind: 'gui', target, lastError: message });
      throw error;
    }
  }

  async pollGuiRender(): Promise<Uint8Array> {
    const bytes = await this.backend.pollGuiRender();
    if (bytes.length > 0) {
      runtime.update((s) => ({ ...s, kind: 'gui', status: 'ready', isRunning: true, guiRenderBytes: bytes }));
    }
    return bytes;
  }

  async stopGui(): Promise<void> {
    await this.backend.stopGui();
    runtime.set({ ...IDLE_RUNTIME });
  }

  async stopConsole(): Promise<void> {
    const state = get(runtime);
    if (!state.isRunning || state.kind !== 'console') {
      return;
    }
    await this.backend.stopVoRun();
  }

  async stop(): Promise<void> {
    const state = get(runtime);
    if (!state.isRunning) {
      return;
    }
    if (state.kind === 'gui') {
      await this.stopGui();
      return;
    }
    if (state.kind === 'console') {
      await this.stopConsole();
    }
  }

  planConsoleRun(target: string): void {
    runtime.set({ ...IDLE_RUNTIME, status: 'ready', kind: 'console', target, runMode: 'vm' });
  }

  planGuiRun(target: string): void {
    runtime.set({ ...IDLE_RUNTIME, status: 'ready', kind: 'gui', target });
  }

  clearConsole(): void {
    consoleClear();
    runtime.update((s) => ({ ...s, consoleLines: [] }));
  }

  private beginConsoleRun(target: string, runMode: 'vm' | 'jit'): number {
    const runId = ++this.nextConsoleRunId;
    this.activeConsoleRunId = runId;
    runtime.set({ ...IDLE_RUNTIME, status: 'running', kind: 'console', target, runMode, isRunning: true });
    return runId;
  }

  private isConsoleRunActive(runId: number): boolean {
    return this.activeConsoleRunId === runId;
  }

  private finishConsoleRun(runId: number, patch: Partial<RuntimeState>): void {
    if (!this.isConsoleRunActive(runId)) {
      return;
    }
    this.activeConsoleRunId = 0;
    runtime.update((state) => ({ ...state, ...patch }));
  }
}
