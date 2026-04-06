import { get, type Readable } from 'svelte/store';

import type { ProtocolModule } from '../gui/renderer_bridge';
import type { Backend } from '../backend/backend';
import type { GuiRunOutput, RunEvent, RunOpts, StreamHandle } from '../types';
import { formatError } from '../format_error';
import { consoleClear, consolePush } from '../../stores/console';
import { runtime, IDLE_RUNTIME, IDLE_GUI, type RuntimeState } from '../../stores/runtime';

export type { RuntimeKind, RuntimeStatus, RuntimeState } from '../../stores/runtime';

const GUI_SESSION_SUPERSEDED_MESSAGE = 'GUI session superseded';

export class GuiSessionSupersededError extends Error {
  constructor() {
    super(GUI_SESSION_SUPERSEDED_MESSAGE);
    this.name = 'GuiSessionSupersededError';
  }
}

export function isGuiSessionSupersededError(error: unknown): error is GuiSessionSupersededError {
  return error instanceof GuiSessionSupersededError;
}

export class RuntimeService {
  private activeConsoleRunId = 0;
  private nextConsoleRunId = 0;
  private guiSessionId = 0;
  private guiSessionActive = false;
  private guiOperationChain: Promise<void> = Promise.resolve();

  private protocolModule: ProtocolModule | null = null;

  constructor(private readonly backend: Backend) {}

  setProtocolModule(mod: ProtocolModule | null): void {
    this.protocolModule = mod;
    runtime.update((state) => {
      if (state.kind !== 'gui' || !state.isRunning || !state.gui.renderBytes) {
        return state;
      }
      return {
        ...state,
        gui: {
          ...state.gui,
          externalWidgetHandlerId: mod?.findExternalWidgetHandlerId(state.gui.renderBytes) ?? null,
        },
      };
    });
  }

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
    const sessionId = this.beginGuiSession(target);
    return this.serializeGuiOperation(async () => {
      try {
        this.assertGuiSessionCurrent(sessionId);
        await this.backend.stopGui();
        this.assertGuiSessionCurrent(sessionId);
        const output = await this.backend.runGui(target);
        this.assertGuiSessionCurrent(sessionId);
        const externalWidgetHandlerId = output.externalWidgetHandlerId
          ?? this.protocolModule?.findExternalWidgetHandlerId(output.renderBytes)
          ?? null;
        runtime.set({
          ...IDLE_RUNTIME,
          status: 'ready',
          kind: 'gui',
          target,
          isRunning: true,
          gui: {
            entryPath: output.entryPath,
            moduleBytes: output.moduleBytes,
            renderBytes: output.renderBytes,
            framework: output.framework,
            sessionId,
            externalWidgetHandlerId,
          },
        });
        return {
          ...output,
          externalWidgetHandlerId,
        };
      } catch (error) {
        const sessionError = this.coerceGuiSessionError(error, sessionId);
        if (!isGuiSessionSupersededError(sessionError) && this.isGuiSessionCurrent(sessionId)) {
          this.guiSessionActive = false;
          const message = formatError(sessionError);
          consolePush('stderr', message);
          runtime.set({ ...IDLE_RUNTIME, status: 'ready', kind: 'gui', target, lastError: message });
        }
        throw sessionError;
      }
    });
  }

  async sendGuiEvent(handlerId: number, payload: string): Promise<Uint8Array> {
    const sessionId = this.requireActiveGuiSession();
    return this.serializeGuiOperation(async () => {
      try {
        this.assertGuiSessionCurrent(sessionId);
        const bytes = await this.backend.sendGuiEvent(handlerId, payload);
        this.assertGuiSessionCurrent(sessionId);
        this.applyGuiRender(bytes);
        return bytes;
      } catch (error) {
        throw this.coerceGuiSessionError(error, sessionId);
      }
    });
  }

  async sendGuiEventAsync(handlerId: number, payload: string): Promise<void> {
    if (!this.guiSessionActive) {
      return;
    }
    const sessionId = this.guiSessionId;
    await this.serializeGuiOperation(async () => {
      if (!this.isGuiSessionActiveFor(sessionId)) {
        return;
      }
      try {
        await this.backend.sendGuiEventAsync(handlerId, payload);
        if (!this.isGuiSessionActiveFor(sessionId)) {
          return;
        }
      } catch (error) {
        const sessionError = this.coerceGuiSessionError(error, sessionId);
        if (isGuiSessionSupersededError(sessionError)) {
          return;
        }
        throw sessionError;
      }
    });
  }

  async pushIslandTransport(data: Uint8Array): Promise<void> {
    if (!this.guiSessionActive) {
      return;
    }
    const sessionId = this.guiSessionId;
    await this.serializeGuiOperation(async () => {
      if (!this.isGuiSessionActiveFor(sessionId)) {
        return;
      }
      try {
        await this.backend.pushIslandTransport(data);
        if (!this.isGuiSessionActiveFor(sessionId)) {
          return;
        }
      } catch (error) {
        const sessionError = this.coerceGuiSessionError(error, sessionId);
        if (isGuiSessionSupersededError(sessionError)) {
          return;
        }
        throw sessionError;
      }
    });
  }

  async pollGuiRender(): Promise<Uint8Array> {
    if (!this.guiSessionActive) {
      return new Uint8Array(0);
    }
    const sessionId = this.guiSessionId;
    return this.serializeGuiOperation(async () => {
      if (!this.isGuiSessionActiveFor(sessionId)) {
        return new Uint8Array(0);
      }
      try {
        const bytes = await this.backend.pollGuiRender();
        if (!this.isGuiSessionActiveFor(sessionId)) {
          return new Uint8Array(0);
        }
        this.applyGuiRender(bytes);
        return bytes;
      } catch (error) {
        const sessionError = this.coerceGuiSessionError(error, sessionId);
        if (isGuiSessionSupersededError(sessionError)) {
          return new Uint8Array(0);
        }
        throw sessionError;
      }
    });
  }

  async stopGui(): Promise<void> {
    const sessionId = this.invalidateGuiSession();
    await this.serializeGuiOperation(async () => {
      await this.backend.stopGui();
      if (this.isGuiSessionCurrent(sessionId) && !this.guiSessionActive) {
        runtime.set({ ...IDLE_RUNTIME });
      }
    });
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

  private beginGuiSession(target: string): number {
    const sessionId = this.guiSessionId + 1;
    this.guiSessionId = sessionId;
    this.guiSessionActive = true;
    this.protocolModule = null;
    runtime.set({ ...IDLE_RUNTIME, status: 'running', kind: 'gui', target, isRunning: true, gui: { ...IDLE_GUI, sessionId } });
    return sessionId;
  }

  private invalidateGuiSession(): number {
    const sessionId = this.guiSessionId + 1;
    this.guiSessionId = sessionId;
    this.guiSessionActive = false;
    this.protocolModule = null;
    return sessionId;
  }

  private requireActiveGuiSession(): number {
    if (!this.guiSessionActive) {
      throw new GuiSessionSupersededError();
    }
    return this.guiSessionId;
  }

  private isGuiSessionCurrent(sessionId: number): boolean {
    return this.guiSessionId === sessionId;
  }

  private isGuiSessionActiveFor(sessionId: number): boolean {
    return this.guiSessionActive && this.isGuiSessionCurrent(sessionId);
  }

  private assertGuiSessionCurrent(sessionId: number): void {
    if (!this.isGuiSessionActiveFor(sessionId)) {
      throw new GuiSessionSupersededError();
    }
  }

  private coerceGuiSessionError(error: unknown, sessionId: number): unknown {
    if (isGuiSessionSupersededError(error)) {
      return error;
    }
    if (!this.guiSessionActive || !this.isGuiSessionCurrent(sessionId)) {
      return new GuiSessionSupersededError();
    }
    return error;
  }

  private serializeGuiOperation<T>(run: () => Promise<T>): Promise<T> {
    const next = this.guiOperationChain.then(run, run);
    this.guiOperationChain = next.then(() => undefined, () => undefined);
    return next;
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

  private applyGuiRender(bytes: Uint8Array): void {
    if (bytes.length === 0) {
      return;
    }
    runtime.update((state) => {
      const externalWidgetHandlerId = this.protocolModule?.findExternalWidgetHandlerId(bytes) ?? null;
      return {
        ...state,
        kind: 'gui',
        status: 'ready',
        isRunning: true,
        gui: {
          ...state.gui,
          renderBytes: bytes,
          externalWidgetHandlerId: externalWidgetHandlerId ?? state.gui.externalWidgetHandlerId,
        },
      };
    });
  }
}
