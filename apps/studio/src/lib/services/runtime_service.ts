import { get, type Readable } from 'svelte/store';

import type {
  Backend,
  ResolvedAppSurfaceRoute,
  RuntimeHandle,
} from '../backend/backend';
import type {
  DisplayPulseSubmission,
  DisplayTimingRequest,
  FrameworkLaneBinding,
  GuiRunOutput,
  RunEvent,
  RunOpts,
  StreamHandle,
} from '../types';
import { GuiSessionAuthority, type GuiSessionToken } from '../gui_session';
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

export type GuiPreview = Readonly<{
  session: GuiSessionToken;
  output: GuiRunOutput;
}>;

function nextAnimationFrameMicros(): Promise<bigint> {
  return new Promise((resolve) => {
    requestAnimationFrame((timestampMillis) => {
      resolve(BigInt(Math.max(0, Math.round(timestampMillis * 1000))));
    });
  });
}

export class RuntimeService {
  private activeConsoleRunId = 0;
  private nextConsoleRunId = 0;
  private readonly guiSessions = new GuiSessionAuthority();
  private readonly guiPreviews = new Map<
    number,
    { session: GuiSessionToken; target: string; output: GuiRunOutput }
  >();
  private guiOperationChain: Promise<void> = Promise.resolve();
  private readonly displayTimingMicros = new Map<string, bigint>();

  constructor(private readonly backend: Backend) {
    backend.setGuiGuestExitHandler((session, exitCode) => {
      this.finishGuiGuestExitForToken(session, exitCode);
    });
    backend.setGuiGuestErrorHandler((session, error) => {
      this.finishGuiGuestErrorForToken(session, error);
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
    return (await this.runGuiPreview(target)).output;
  }

  async runGuiPreview(target: string): Promise<GuiPreview> {
    const session = this.beginGuiSession(target);
    return this.serializeGuiOperation(async () => {
      try {
        const output = await this.backend.runGui(target, session);
        this.assertGuiSessionCurrent(session);
        this.guiPreviews.set(session.id, { session, target, output });
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
            gameRenderBytes: null,
            framework: output.framework,
            providerFrameworks: output.providerFrameworks,
            sessionId: session.id,
          },
        });
        return { session, output };
      } catch (error) {
        const sessionError = this.coerceGuiSessionError(error, session);
        this.guiSessions.invalidate(session);
        this.guiPreviews.delete(session.id);
        await this.backend.stopGui(session).catch(() => undefined);
        const selected = this.guiSessions.active;
        const selectedPreview = selected ? this.guiPreviews.get(selected.id) : undefined;
        if (selected && selectedPreview) {
          await this.backend.selectGuiPreview(selected);
          this.publishGuiPreview(selectedPreview);
        } else {
          runtime.set({
            ...IDLE_RUNTIME,
            status: 'ready',
            kind: 'gui',
            target,
            lastError: formatError(sessionError),
          });
        }
        throw sessionError;
      }
    });
  }

  async sendGuiEventFor(
    session: GuiSessionToken,
    handlerId: number,
    payload: string,
  ): Promise<Uint8Array> {
    this.assertGuiSessionCurrent(session);
    return this.serializeGuiOperation(async () => {
      const bytes = await this.backend.sendGuiEvent(handlerId, payload, session);
      this.assertGuiSessionCurrent(session);
      return bytes;
    });
  }

  async pollGuiRenderFor(session: GuiSessionToken): Promise<Uint8Array> {
    this.assertGuiSessionCurrent(session);
    return this.backend.pollGuiRender(session);
  }

  async pollGameRenderFor(session: GuiSessionToken): Promise<Uint8Array> {
    this.assertGuiSessionCurrent(session);
    return this.backend.pollGameRender(session);
  }

  async stopGuiPreview(session: GuiSessionToken): Promise<void> {
    const wasSelected = this.guiSessions.active === session;
    if (!this.guiSessions.invalidate(session)) {
      return;
    }
    this.guiPreviews.delete(session.id);
    this.clearDisplayTimingFor(session);
    await this.serializeGuiOperation(() => this.backend.stopGui(session));
    if (this.guiSessions.size === 0) {
      runtime.set({ ...IDLE_RUNTIME });
      return;
    }
    if (wasSelected) {
      const selected = this.guiSessions.active;
      const preview = selected ? this.guiPreviews.get(selected.id) : undefined;
      if (!selected || !preview) throw new GuiSessionSupersededError();
      await this.backend.selectGuiPreview(selected);
      this.publishGuiPreview(preview);
    } else {
      runtime.update((state) => ({ ...state }));
    }
  }

  listGuiPreviews(): readonly GuiPreview[] {
    return [...this.guiPreviews.values()].map(({ session, output }) => ({ session, output }));
  }

  async selectGuiPreview(session: GuiSessionToken): Promise<GuiRunOutput> {
    return this.serializeGuiOperation(async () => {
      const preview = this.guiPreviews.get(session.id);
      if (!preview || preview.session !== session || !this.guiSessions.isActive(session)) {
        throw new GuiSessionSupersededError();
      }
      this.guiSessions.select(session);
      await this.backend.selectGuiPreview(session);
      this.publishGuiPreview(preview);
      return preview.output;
    });
  }

  private publishGuiPreview(
    preview: { session: GuiSessionToken; target: string; output: GuiRunOutput },
  ): void {
    runtime.set({
      ...IDLE_RUNTIME,
      status: 'ready',
      kind: 'gui',
      target: preview.target,
      isRunning: true,
      gui: {
        entryPath: preview.output.entryPath,
        moduleBytes: preview.output.moduleBytes,
        renderBytes: preview.output.renderBytes,
        gameRenderBytes: null,
        framework: preview.output.framework,
        providerFrameworks: preview.output.providerFrameworks,
        sessionId: preview.session.id,
      },
    });
  }

  async sendGuiEvent(handlerId: number, payload: string): Promise<Uint8Array> {
    const session = this.requireActiveGuiSession();
    return this.serializeGuiOperation(async () => {
      try {
        this.assertGuiSessionCurrent(session);
        const bytes = await this.backend.sendGuiEvent(handlerId, payload, session);
        this.assertGuiSessionCurrent(session);
        this.applyGuiRender(bytes);
        return bytes;
      } catch (error) {
        throw this.coerceGuiSessionError(error, session);
      }
    });
  }

  async sendGuiEventAsync(handlerId: number, payload: string): Promise<void> {
    const session = this.guiSessions.active;
    if (!session) {
      return;
    }
    await this.serializeGuiOperation(async () => {
      if (!this.isGuiSessionActiveFor(session)) {
        return;
      }
      try {
        await this.backend.sendGuiEventAsync(handlerId, payload, session);
        if (!this.isGuiSessionActiveFor(session)) {
          return;
        }
      } catch (error) {
        const sessionError = this.coerceGuiSessionError(error, session);
        if (isGuiSessionSupersededError(sessionError)) {
          return;
        }
        throw sessionError;
      }
    });
  }

  async pushIslandTransport(data: Uint8Array, sessionId?: number): Promise<void> {
    const session = this.requireLiveGuiSession(sessionId);
    await this.serializeGuiOperation(async () => {
      if (!this.isGuiSessionActiveFor(session)) {
        return;
      }
      try {
        await this.backend.pushIslandTransport(data, session);
        if (!this.isGuiSessionActiveFor(session)) {
          return;
        }
      } catch (error) {
        const sessionError = this.coerceGuiSessionError(error, session);
        if (isGuiSessionSupersededError(sessionError)) {
          return;
        }
        throw sessionError;
      }
    });
  }

  async pushAndPollIslandTransport(
    data: Uint8Array,
    sessionId?: number,
  ): Promise<Uint8Array[]> {
    const session = this.requireLiveGuiSession(sessionId);
    return this.serializeGuiOperation(async () => {
      if (!this.isGuiSessionActiveFor(session)) {
        return [];
      }
      try {
        const frames = await this.backend.pushAndPollIslandTransport(data, session);
        if (!this.isGuiSessionActiveFor(session)) {
          return [];
        }
        return frames;
      } catch (error) {
        const sessionError = this.coerceGuiSessionError(error, session);
        if (isGuiSessionSupersededError(sessionError)) {
          return [];
        }
        throw sessionError;
      }
    });
  }

  async pollIslandTransport(sessionId?: number): Promise<Uint8Array> {
    const session = this.requireLiveGuiSession(sessionId);
    return this.serializeGuiOperation(async () => {
      if (!this.isGuiSessionActiveFor(session)) {
        return new Uint8Array(0);
      }
      try {
        const bytes = await this.backend.pollIslandTransport(session);
        if (!this.isGuiSessionActiveFor(session)) {
          return new Uint8Array(0);
        }
        return bytes;
      } catch (error) {
        const sessionError = this.coerceGuiSessionError(error, session);
        if (isGuiSessionSupersededError(sessionError)) {
          return new Uint8Array(0);
        }
        throw sessionError;
      }
    });
  }

  async openFrameworkLane(owner: string, sessionId?: number): Promise<FrameworkLaneBinding> {
    const session = this.requireLiveGuiSession(sessionId);
    return this.serializeGuiOperation(async () => {
      this.assertGuiSessionCurrent(session);
      const binding = await this.backend.openFrameworkLane(owner, session);
      this.assertGuiSessionCurrent(session);
      return binding;
    });
  }

  async completeVoguiTargetCommit(
    accepted: boolean,
    providerError = '',
    sessionId?: number,
  ): Promise<void> {
    const session = this.requireLiveGuiSession(sessionId);
    if (!this.backend.completeVoguiTargetCommit) {
      throw new Error('Active GUI backend cannot complete browser Vogui commits');
    }
    await this.serializeGuiOperation(async () => {
      this.assertGuiSessionCurrent(session);
      await this.backend.completeVoguiTargetCommit!(accepted, providerError, session);
      this.assertGuiSessionCurrent(session);
    });
  }

  async beginFrameworkProvider(moduleKey: string, sessionId?: number): Promise<void> {
    await this.runFrameworkProviderOperation('begin', moduleKey, sessionId);
  }

  async loadFrameworkProvider(moduleKey: string, sessionId?: number): Promise<void> {
    await this.runFrameworkProviderOperation('load', moduleKey, sessionId);
  }

  async unloadFrameworkProvider(moduleKey: string, sessionId?: number): Promise<void> {
    await this.runFrameworkProviderOperation('unload', moduleKey, sessionId);
  }

  async readyFrameworkProvider(moduleKey: string, sessionId?: number): Promise<void> {
    await this.runFrameworkProviderOperation('ready', moduleKey, sessionId);
  }

  async abortFrameworkProvider(moduleKey: string, sessionId?: number): Promise<void> {
    await this.runFrameworkProviderOperation('abort', moduleKey, sessionId);
  }

  async closeFrameworkProvider(moduleKey: string, sessionId?: number): Promise<void> {
    await this.runFrameworkProviderOperation('close', moduleKey, sessionId);
  }

  async runFrameworkProviderOperation(
    operation: 'load' | 'unload' | 'begin' | 'ready' | 'abort' | 'close',
    moduleKey: string,
    sessionId?: number,
  ): Promise<void> {
    const session = this.requireLiveGuiSession(sessionId);
    await this.serializeGuiOperation(async () => {
      this.assertGuiSessionCurrent(session);
      const method = {
        load: this.backend.loadFrameworkProvider,
        unload: this.backend.unloadFrameworkProvider,
        begin: this.backend.beginFrameworkProvider,
        ready: this.backend.readyFrameworkProvider,
        abort: this.backend.abortFrameworkProvider,
        close: this.backend.closeFrameworkProvider,
      }[operation];
      if (method) {
        await method.call(this.backend, moduleKey, session);
      }
      this.assertGuiSessionCurrent(session);
    });
  }

  async pollFrameworkLane(
    binding: FrameworkLaneBinding,
    sessionId?: number,
  ): Promise<Uint8Array> {
    const session = this.requireLiveGuiSession(sessionId);
    return this.serializeGuiOperation(async () => {
      if (!this.isGuiSessionActiveFor(session)) {
        return new Uint8Array(0);
      }
      try {
        const packet = await this.backend.pollFrameworkLane(binding, session);
        if (!this.isGuiSessionActiveFor(session)) {
          return new Uint8Array(0);
        }
        return packet;
      } catch (error) {
        throw this.coerceGuiSessionError(error, session);
      }
    });
  }

  async submitFrameworkLane(
    binding: FrameworkLaneBinding,
    packet: Uint8Array,
    sessionId?: number,
  ): Promise<void> {
    const session = this.requireLiveGuiSession(sessionId);
    await this.serializeGuiOperation(async () => {
      this.assertGuiSessionCurrent(session);
      await this.backend.submitFrameworkLane(binding, packet, session);
      this.assertGuiSessionCurrent(session);
    });
  }

  async submitFrameworkLaneBatch(
    binding: FrameworkLaneBinding,
    packetBatch: Uint8Array,
    sessionId?: number,
  ): Promise<void> {
    const packets: Uint8Array[] = [];
    if (packetBatch.length < 4) {
      throw new Error('framework lane packet batch is truncated');
    }
    const view = new DataView(packetBatch.buffer, packetBatch.byteOffset, packetBatch.byteLength);
    const count = view.getUint32(0, true);
    let cursor = 4;
    for (let index = 0; index < count; index += 1) {
      if (cursor + 4 > packetBatch.length) {
        throw new Error('framework lane packet batch is truncated');
      }
      const length = view.getUint32(cursor, true);
      cursor += 4;
      if (length === 0 || cursor + length > packetBatch.length) {
        throw new Error('framework lane packet batch is malformed');
      }
      packets.push(packetBatch.slice(cursor, cursor + length));
      cursor += length;
    }
    if (count === 0 || count > 4096 || cursor !== packetBatch.length) {
      throw new Error('framework lane packet batch is malformed');
    }
    const session = this.requireLiveGuiSession(sessionId);
    await this.serializeGuiOperation(async () => {
      this.assertGuiSessionCurrent(session);
      await this.backend.submitFrameworkLaneBatch(binding, packets, session);
      this.assertGuiSessionCurrent(session);
    });
  }

  async pollDisplayTimingRequest(): Promise<DisplayTimingRequest | null> {
    const session = this.guiSessions.active;
    if (!session) {
      return null;
    }
    return this.pollDisplayTimingRequestFor(session);
  }

  async pollDisplayTimingRequestFor(
    session: GuiSessionToken,
  ): Promise<DisplayTimingRequest | null> {
    return this.serializeGuiOperation(async () => {
      if (!this.isGuiSessionActiveFor(session)) {
        return null;
      }
      try {
        const request = await this.backend.pollDisplayTimingRequest(session);
        if (!this.isGuiSessionActiveFor(session)) {
          return null;
        }
        return request;
      } catch (error) {
        throw this.coerceGuiSessionError(error, session);
      }
    });
  }

  async submitDisplayPulse(
    request: DisplayTimingRequest,
    observedMicros: string,
    intervalMicros: string,
  ): Promise<DisplayPulseSubmission> {
    const session = this.requireActiveGuiSession();
    return this.submitDisplayPulseFor(session, request, observedMicros, intervalMicros);
  }

  async submitDisplayPulseFor(
    session: GuiSessionToken,
    request: DisplayTimingRequest,
    observedMicros: string,
    intervalMicros: string,
  ): Promise<DisplayPulseSubmission> {
    return this.serializeGuiOperation(async () => {
      this.assertGuiSessionCurrent(session);
      const submission = await this.backend.submitDisplayPulse(
        request,
        observedMicros,
        intervalMicros,
        session,
      );
      this.assertGuiSessionCurrent(session);
      return submission;
    });
  }

  async serviceDisplayTiming(): Promise<number> {
    const state = get(runtime);
    if (state.kind !== 'gui' || !state.isRunning) return 0;
    const session = this.guiSessions.active;
    if (!session) return 0;
    return this.serviceDisplayTimingFor(session);
  }

  async serviceDisplayTimingFor(session: GuiSessionToken): Promise<number> {
    this.assertGuiSessionCurrent(session);
    const requests: DisplayTimingRequest[] = [];
    for (let index = 0; index < 64; index++) {
      const request = await this.pollDisplayTimingRequestFor(session);
      if (request === null) {
        break;
      }
      requests.push(request);
    }
    if (requests.length === 0) {
      return 0;
    }
    const observedMicros = await nextAnimationFrameMicros();
    let emittedDomains = 0;
    for (const request of requests) {
      const key = `${session.id}:${request.view.index}:${request.view.generation}`;
      const previous = this.displayTimingMicros.get(key);
      const intervalMicros = previous === undefined
        ? 16_667n
        : observedMicros > previous
          ? observedMicros - previous
          : 1n;
      this.displayTimingMicros.set(key, observedMicros);
      const submission = await this.submitDisplayPulseFor(
        session,
        request,
        observedMicros.toString(),
        intervalMicros.toString(),
      );
      emittedDomains += submission.emittedDomains;
    }
    return emittedDomains;
  }

  async pollGuiRender(): Promise<Uint8Array> {
    const state = get(runtime);
    if (state.kind !== 'gui' || !state.isRunning) return new Uint8Array(0);
    const session = this.guiSessions.active;
    if (!session) {
      return new Uint8Array(0);
    }
    return this.serializeGuiOperation(async () => {
      if (!this.isGuiSessionActiveFor(session)) {
        return new Uint8Array(0);
      }
      try {
        const bytes = await this.backend.pollGuiRender(session);
        if (!this.isGuiSessionActiveFor(session)) {
          return new Uint8Array(0);
        }
        this.applyGuiRender(bytes);
        return bytes;
      } catch (error) {
        const sessionError = this.coerceGuiSessionError(error, session);
        if (isGuiSessionSupersededError(sessionError)) {
          return new Uint8Array(0);
        }
        throw sessionError;
      }
    });
  }

  async pollGameRender(): Promise<Uint8Array> {
    const state = get(runtime);
    if (state.kind !== 'gui' || !state.isRunning) return new Uint8Array(0);
    const session = this.guiSessions.active;
    if (!session) {
      return new Uint8Array(0);
    }
    return this.serializeGuiOperation(async () => {
      if (!this.isGuiSessionActiveFor(session)) {
        return new Uint8Array(0);
      }
      const bytes = await this.backend.pollGameRender(session);
      if (bytes.length > 0 && this.isGuiSessionActiveFor(session)) {
        runtime.update((state) => ({
          ...state,
          gui: {
            ...state.gui,
            gameRenderBytes: bytes,
          },
        }));
      }
      return bytes;
    });
  }

  isGuiSessionSelected(sessionId: number): boolean {
    const state = get(runtime);
    return state.kind === 'gui'
      && state.isRunning
      && state.gui.sessionId === sessionId
      && this.guiSessions.active?.id === sessionId;
  }

  async serviceDisplayTimingForSession(
    sessionId: number,
    onlyWhenInactive = false,
  ): Promise<number> {
    const session = this.requireLiveGuiSession(sessionId);
    if (onlyWhenInactive && this.isGuiSessionSelected(sessionId)) return 0;
    return this.serviceDisplayTimingFor(session);
  }

  async pollGameRenderForSession(
    sessionId: number,
    onlyWhenInactive = false,
  ): Promise<Uint8Array> {
    const session = this.requireLiveGuiSession(sessionId);
    return this.serializeGuiOperation(async () => {
      if (!this.isGuiSessionActiveFor(session)) return new Uint8Array(0);
      if (onlyWhenInactive && this.guiSessions.active === session) {
        return new Uint8Array(0);
      }
      return this.backend.pollGameRender(session);
    });
  }

  async submitGameRenderResult(result: Uint8Array, sessionId?: number): Promise<void> {
    const session = this.requireLiveGuiSession(sessionId);
    if (!this.backend.submitGameRenderResult) {
      throw new Error('active Studio backend cannot accept game render results');
    }
    await this.backend.submitGameRenderResult(result, session);
    this.assertGuiSessionCurrent(session);
  }

  async resolveAppSurfaceRoute(
    surface: RuntimeHandle,
    sessionId?: number,
  ): Promise<ResolvedAppSurfaceRoute> {
    const session = this.requireLiveGuiSession(sessionId);
    if (!this.backend.resolveAppSurfaceRoute) {
      throw new Error('active Studio backend cannot resolve App Surface routes');
    }
    const route = await this.backend.resolveAppSurfaceRoute(surface, session);
    this.assertGuiSessionCurrent(session);
    return route;
  }

  async restartComposedWebview(sessionId?: number): Promise<bigint> {
    const session = this.requireLiveGuiSession(sessionId);
    if (!this.backend.restartComposedWebview) {
      throw new Error('active Studio backend cannot restart its composed WebView');
    }
    const epoch = await this.backend.restartComposedWebview(session);
    this.assertGuiSessionCurrent(session);
    return epoch;
  }

  async registerAppSurfaceShortcuts(
    surface: RuntimeHandle,
    registrations: readonly Readonly<{
      classMask: bigint;
      scope: 'view' | 'window' | 'session';
      priority: number;
    }>[],
    sessionId?: number,
  ): Promise<bigint> {
    const session = this.requireLiveGuiSession(sessionId);
    if (!this.backend.registerAppSurfaceShortcuts) {
      throw new Error('active Studio backend cannot register App Surface shortcuts');
    }
    const revision = await this.backend.registerAppSurfaceShortcuts(
      surface,
      registrations,
      session,
    );
    this.assertGuiSessionCurrent(session);
    return revision;
  }

  async stopGui(): Promise<void> {
    const sessions = this.guiSessions.sessions();
    for (const session of sessions) {
      this.guiSessions.invalidate(session);
      this.guiPreviews.delete(session.id);
      this.clearDisplayTimingFor(session);
    }
    await this.serializeGuiOperation(async () => {
      for (const session of sessions) {
        await this.backend.stopGui(session);
      }
    });
    runtime.set({ ...IDLE_RUNTIME });
  }

  /** Commit a terminal status reported by either the logic VM or a render VM. */
  finishGuiGuestExit(sessionId: number, exitCode: number): void {
    const preview = this.guiPreviews.get(sessionId);
    if (!preview || !this.guiSessions.isActive(preview.session)) {
      return;
    }
    this.finishGuiGuestExitForToken(preview.session, exitCode);
  }

  private finishGuiGuestExitForToken(session: GuiSessionToken, exitCode: number): void {
    this.finishGuiGuestTerminal(
      session,
      `GUI guest exited with status ${exitCode}`,
      exitCode === 0 ? null : `GUI guest exited with status ${exitCode}`,
      exitCode === 0 ? 'system' : 'stderr',
    );
  }

  private finishGuiGuestErrorForToken(session: GuiSessionToken, error: Error): void {
    const message = formatError(error);
    this.finishGuiGuestTerminal(session, message, message, 'stderr');
  }

  private finishGuiGuestTerminal(
    session: GuiSessionToken,
    message: string,
    lastError: string | null,
    consoleKind: 'system' | 'stderr',
  ): void {
    if (!this.isGuiSessionActiveFor(session)) {
      return;
    }

    const exitedPreview = this.guiPreviews.get(session.id);
    const wasSelected = this.guiSessions.active === session;
    const target = exitedPreview?.target ?? get(runtime).target;
    this.invalidateGuiSession(session);
    this.guiPreviews.delete(session.id);
    this.clearDisplayTimingFor(session);
    consolePush(consoleKind, message);
    const selected = this.guiSessions.active;
    const selectedPreview = selected ? this.guiPreviews.get(selected.id) : undefined;
    if (wasSelected && selectedPreview) {
      this.publishGuiPreview(selectedPreview);
    } else if (selectedPreview) {
      runtime.update((state) => ({ ...state }));
    } else {
      runtime.set({
        ...IDLE_RUNTIME,
        status: 'ready',
        kind: 'gui',
        target,
        lastError,
      });
    }

    // Serialize teardown behind any transport operation that observed the
    // exit. A newly requested GUI run is queued after this teardown.
    void this.serializeGuiOperation(async () => {
      await this.backend.stopGui(session);
      if (wasSelected && selected && selectedPreview) {
        await this.backend.selectGuiPreview(selected);
      }
    }).catch((error) => {
      console.error('[RuntimeService] failed to clean up exited GUI guest:', error);
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
    if (!state.isRunning && this.guiSessions.size === 0) {
      return;
    }
    if (this.guiSessions.size > 0) {
      await this.stopGui();
    }
    if (state.kind === 'console' && state.isRunning) {
      await this.backend.stopVoRun();
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

  private clearDisplayTimingFor(session: GuiSessionToken): void {
    const prefix = `${session.id}:`;
    for (const key of this.displayTimingMicros.keys()) {
      if (key.startsWith(prefix)) {
        this.displayTimingMicros.delete(key);
      }
    }
  }

  private beginGuiSession(target: string): GuiSessionToken {
    const session = this.guiSessions.begin();
    runtime.set({
      ...IDLE_RUNTIME,
      status: 'running',
      kind: 'gui',
      target,
      isRunning: true,
      gui: { ...IDLE_GUI, sessionId: session.id },
    });
    return session;
  }

  private invalidateGuiSession(expected?: GuiSessionToken): GuiSessionToken | null {
    const invalidated = this.guiSessions.invalidate(expected);
    return invalidated;
  }

  private requireActiveGuiSession(): GuiSessionToken {
    const session = this.guiSessions.active;
    if (!session) {
      throw new GuiSessionSupersededError();
    }
    return session;
  }

  private requireLiveGuiSession(sessionId?: number): GuiSessionToken {
    if (sessionId === undefined) return this.requireActiveGuiSession();
    const preview = this.guiPreviews.get(sessionId);
    if (
      preview === undefined
      || preview.session.id !== sessionId
      || !this.guiSessions.isActive(preview.session)
    ) {
      throw new GuiSessionSupersededError();
    }
    return preview.session;
  }

  private isGuiSessionCurrent(session: GuiSessionToken): boolean {
    return this.guiSessions.isActive(session);
  }

  private isGuiSessionActiveFor(session: GuiSessionToken): boolean {
    return this.guiSessions.isActive(session);
  }

  private assertGuiSessionCurrent(session: GuiSessionToken): void {
    if (!this.isGuiSessionActiveFor(session)) {
      throw new GuiSessionSupersededError();
    }
  }

  private coerceGuiSessionError(error: unknown, session: GuiSessionToken): unknown {
    if (isGuiSessionSupersededError(error)) {
      return error;
    }
    if (!this.isGuiSessionCurrent(session)) {
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
      return {
        ...state,
        kind: 'gui',
        status: 'ready',
        isRunning: true,
        gui: {
          ...state.gui,
          renderBytes: bytes,
        },
      };
    });
  }
}
