import type {
  Backend,
  FileDialogFilter,
  ResolvedAppSurfaceRoute,
  RuntimeHandle,
} from './backend';
import type {
  BootstrapContext,
  DiscoveredProject,
  DisplayPulseSubmission,
  DisplayTimingRequest,
  FsEntry,
  FsStat,
  GitOp,
  GitResult,
  GuiRunOutput,
  HttpOpts,
  HttpResult,
  FrameworkContract,
  FrameworkLaneBinding,
  LaunchSpec,
  ProcEvent,
  PreparedSession,
  RunEvent,
  RunOpts,
  SessionInfo,
  StreamHandle,
} from '../types';

import { invoke as tauriInvoke, listen as tauriListen } from '../tauri';
import { consolePush } from '../../stores/console';
import { formatDurationMs, pushUiConsole, renderStudioLogRecord, type StudioLogRecord } from './gui_console';
import { makeTauriStreamHandle } from './stream_handle';
import { GuiSessionBinding, type GuiSessionToken } from '../gui_session';
import { decodeGuiPlatformRequest } from '../gui/platform_request';
import {
  executeGuiPlatformRequest,
  releaseBrowserPlatformSession,
  type GuiPlatformHostAdapter,
} from '../gui/platform_host';
import { VoguiSubscriptionHost, type VoguiHostSubscription } from '../gui/subscription_host';
import {
  decodeWebviewBridgeFrame,
  encodeWebviewBridgeFrame,
  type WebviewBridgeFrame,
  type WebviewBridgeLane,
  type WebviewBridgeOwner,
} from '../gui/webview_transport';

type StudioLogEvent = { sessionId: number; record: StudioLogRecord };
type GuiGuestExitEvent = { sessionId: number; exitCode: number };
type GuiFatalErrorEvent = { sessionId: number; message: string };

type StudioSessionHandle = Readonly<{
  index: number;
  generation: number;
}>;

type NativeGuiRunResult = {
  previewHandle: StudioSessionHandle;
  sessionEpoch: string;
  bridgeEpoch: string;
  renderBytes: number[];
  moduleBytes: number[];
  entryPath: string;
  framework: FrameworkContract | null;
  providerFrameworks: FrameworkContract[];
  vfsSnapshot: {
    rootPath: string;
    files: Array<{ path: string; bytes: number[] }>;
  };
};

function displayPath(path: string): string {
  const normalized = path.trim().replace(/\\/g, '/');
  return normalized || path;
}

function waitForNextUiFrame(): Promise<void> {
  return new Promise((resolve) => {
    let settled = false;
    const finish = () => {
      if (settled) {
        return;
      }
      settled = true;
      resolve();
    };
    if (typeof window !== 'undefined' && typeof window.requestAnimationFrame === 'function') {
      window.requestAnimationFrame(() => finish());
      setTimeout(finish, 50);
      return;
    }
    setTimeout(finish, 0);
  });
}

function pushNativeStudioLog(record: StudioLogRecord): void {
  pushUiConsole(renderStudioLogRecord(record, displayPath));
}

export class NativeBackend implements Backend {
  readonly platform = 'native' as const;
  private readonly guiSession = new GuiSessionBinding();
  private readonly previewHandles = new Map<number, StudioSessionHandle>();
  private readonly bridgeEpochs = new Map<number, bigint>();
  private readonly bridgeOwners = new Map<number, WebviewBridgeOwner>();
  private readonly bridgeOutboundSequences = new Map<number, bigint>();
  private guiLogListenerPromise: Promise<void> | null = null;
  private guiExitListenerPromise: Promise<void> | null = null;
  private guiFatalListenerPromise: Promise<void> | null = null;
  private guiGuestExitHandler: ((session: GuiSessionToken, exitCode: number) => void) | null = null;
  private guiGuestErrorHandler: ((session: GuiSessionToken, error: Error) => void) | null = null;
  private readonly guiFatalErrors = new Map<number, Error>();
  private readonly guiGuestExits = new Map<number, number>();
  private readonly guiNavigationUrls = new Map<number, string>();
  private readonly voguiSubscriptions = new Map<number, VoguiSubscriptionHost>();
  private readonly guiPlatformHost: GuiPlatformHostAdapter = {
    openFile: (descriptor) => this.invoke<string | null>('cmd_pick_file', {
      defaultPath: descriptor.defaultPath,
      filters: descriptor.filters,
    }),
    saveFile: (descriptor) => this.invoke<string | null>('cmd_save_file', {
      defaultPath: descriptor.defaultPath,
      filters: descriptor.filters,
    }),
    navigation: (command, argument, request) =>
      this.navigateGuiBrowser(command, argument, request.session),
    readVfs: async (path) => new Uint8Array(
      await this.invoke<ArrayBuffer>('cmd_read_binary', { path }),
    ),
    writeVfs: async (path, data, request) => {
      await this.invoke<void>('cmd_write_binary', {
        path,
        data: [...data],
      });
      const sessionId = this.studioSessionIdForPreview(request.session);
      window.dispatchEvent(new CustomEvent('vogui-resource-change', {
        detail: { sessionId, resource: path, operation: 'write' },
      }));
    },
    statVfs: (path) => this.invoke<FsStat>('cmd_stat_path', { path }),
    listVfs: (path) => this.invoke<FsEntry[]>('cmd_list_dir', { path }),
  };

  setGuiGuestExitHandler(handler: ((session: GuiSessionToken, exitCode: number) => void) | null): void {
    this.guiGuestExitHandler = handler;
  }

  setGuiGuestErrorHandler(handler: ((session: GuiSessionToken, error: Error) => void) | null): void {
    this.guiGuestErrorHandler = handler;
  }

  async selectGuiPreview(session: GuiSessionToken): Promise<void> {
    this.activePreviewHandle(session);
    this.guiSession.select(session);
    for (const [sessionId, host] of this.voguiSubscriptions) {
      host.setInteractive(sessionId === session.id);
    }
  }

  async getBootstrapContext(): Promise<BootstrapContext> {
    return this.invoke<BootstrapContext>('cmd_get_bootstrap_context');
  }

  async openSession(spec: LaunchSpec): Promise<SessionInfo> {
    return this.activateSession(await this.prepareSession(spec));
  }

  async prepareSession(spec: LaunchSpec): Promise<PreparedSession> {
    return this.invoke<PreparedSession>('cmd_prepare_session', { spec });
  }

  async activateSession(candidate: PreparedSession): Promise<SessionInfo> {
    return this.invoke<SessionInfo>('cmd_activate_session', { candidate });
  }

  async restoreSession(previous: SessionInfo): Promise<SessionInfo> {
    return this.invoke<SessionInfo>('cmd_restore_session', { previous });
  }

  async discardPreparedSession(candidate: PreparedSession): Promise<void> {
    await this.invoke<void>('cmd_discard_prepared_session', { candidate });
  }

  async listPreparedSessionDir(candidate: PreparedSession, path: string): Promise<FsEntry[]> {
    return this.invoke<FsEntry[]>('cmd_list_prepared_session_dir', { candidate, path });
  }

  async readPreparedSessionFile(candidate: PreparedSession, path: string): Promise<string> {
    return this.invoke<string>('cmd_read_prepared_session_file', { candidate, path });
  }

  async discoverWorkspaceProjects(): Promise<DiscoveredProject[]> {
    return this.invoke<DiscoveredProject[]>('cmd_discover_workspace_projects');
  }

  async listDir(path: string): Promise<FsEntry[]> {
    return this.invoke<FsEntry[]>('cmd_list_dir', { path });
  }

  async readFile(path: string): Promise<string> {
    return this.invoke<string>('cmd_read_file', { path });
  }

  async writeFile(path: string, content: string): Promise<void> {
    await this.invoke<void>('cmd_write_file', { path, content });
  }

  async mkdir(path: string): Promise<void> {
    await this.invoke<void>('cmd_mkdir', { path });
  }

  async removeEntry(path: string, recursive: boolean): Promise<void> {
    await this.invoke<void>('cmd_remove_entry', { path, recursive });
  }

  async renameEntry(oldPath: string, newPath: string): Promise<void> {
    await this.invoke<void>('cmd_rename_entry', { oldPath, newPath });
  }

  async dumpVo(path: string): Promise<string> {
    return this.invoke<string>('cmd_dump_vo', { path });
  }

  runVo(path: string, opts?: RunOpts): StreamHandle<RunEvent> {
    const runMode = opts?.mode ?? 'vm';
    return makeTauriStreamHandle<RunEvent>((channel) =>
      this.invoke<void>('cmd_run_vo_stream', { path, runMode, onEvent: channel }),
      (event) => event.kind === 'done' || event.kind === 'error' || event.kind === 'stopped',
    );
  }

  async stopVoRun(): Promise<void> {
    await this.invoke<void>('cmd_stop_vo_run');
  }

  async runGui(path: string, session: GuiSessionToken): Promise<GuiRunOutput> {
    const sessionId = session.id;
    let startedPreview: StudioSessionHandle | null = null;
    this.guiSession.activate(session);
    for (const [liveSessionId, host] of this.voguiSubscriptions) {
      host.setInteractive(liveSessionId === sessionId);
    }
    this.guiFatalErrors.delete(sessionId);
    this.guiGuestExits.delete(sessionId);
    const targetLabel = displayPath(path);
    try {
      consolePush('system', `Opening GUI ${targetLabel}`);
      consolePush('system', `Preparing dependencies and compiling GUI ${targetLabel}...`);
      await waitForNextUiFrame();
      await this.ensureGuiLogListener();
      await this.ensureGuiExitListener();
      await this.ensureGuiFatalListener();
      const totalStart = performance.now();

      const result = await this.invoke<NativeGuiRunResult>('cmd_run_gui', { entryPath: path, sessionId });
      startedPreview = result.previewHandle;
      const startupFatal = this.guiFatalErrors.get(sessionId);
      if (startupFatal) {
        throw startupFatal;
      }
      const startupExit = this.guiGuestExits.get(sessionId);
      if (startupExit !== undefined) {
        throw new Error(`GUI guest exited during startup with status ${startupExit}`);
      }

      if (!this.guiSession.isActive(session)) {
        throw new Error('GUI session superseded');
      }
      this.previewHandles.set(sessionId, result.previewHandle);
      await this.invoke<void>('cmd_attach_webview_bridge', {
        previewHandle: result.previewHandle,
        bridgeEpoch: result.bridgeEpoch,
      });
      this.bridgeEpochs.set(sessionId, BigInt(result.bridgeEpoch));
      this.bridgeOwners.set(sessionId, Object.freeze({
        session: result.previewHandle,
        sessionEpoch: BigInt(result.sessionEpoch),
        bridgeEpoch: BigInt(result.bridgeEpoch),
      }));
      this.bridgeOutboundSequences.set(sessionId, 1n);
      await this.drainGuiPlatformRequests();
      const attachedFatal = this.guiFatalErrors.get(sessionId);
      if (attachedFatal) throw attachedFatal;
      const attachedExit = this.guiGuestExits.get(sessionId);
      if (attachedExit !== undefined) {
        throw new Error(`GUI guest exited during startup with status ${attachedExit}`);
      }

      consolePush('success', `Opened GUI ${targetLabel} in ${formatDurationMs(performance.now() - totalStart)}`);
      return {
        renderBytes: new Uint8Array(result.renderBytes),
        moduleBytes: new Uint8Array(result.moduleBytes),
        entryPath: result.entryPath,
        framework: result.framework,
        providerFrameworks: result.providerFrameworks,
        vfsSnapshot: {
          rootPath: result.vfsSnapshot.rootPath,
          files: result.vfsSnapshot.files.map((file) => ({
            path: file.path,
            bytes: new Uint8Array(file.bytes),
          })),
        },
      };
    } catch (error) {
      this.disposeVoguiSubscriptions(session);
      this.bridgeEpochs.delete(sessionId);
      this.bridgeOwners.delete(sessionId);
      this.bridgeOutboundSequences.delete(sessionId);
      this.previewHandles.delete(sessionId);
      this.guiFatalErrors.delete(sessionId);
      this.guiGuestExits.delete(sessionId);
      this.guiSession.clear(session);
      if (startedPreview) {
        await this.invoke<void>('cmd_stop_gui', { previewHandle: startedPreview })
          .catch((cleanupError) => {
            console.error('[studio-native] failed to clean up GUI startup:', cleanupError);
          });
        releaseBrowserPlatformSession(startedPreview);
      }
      throw error;
    }
  }

  async sendGuiEvent(
    handlerId: number,
    payload: string,
    session?: GuiSessionToken,
  ): Promise<Uint8Array> {
    const raw = await this.invoke<ArrayBuffer>('cmd_send_gui_event', {
      previewHandle: this.activePreviewHandle(session),
      handlerId,
      payload,
    });
    await this.drainGuiPlatformRequests(session);
    return new Uint8Array(raw);
  }

  async sendGuiEventAsync(
    handlerId: number,
    payload: string,
    session?: GuiSessionToken,
  ): Promise<void> {
    await this.invoke<void>('cmd_send_gui_event_async', {
      previewHandle: this.activePreviewHandle(session),
      handlerId,
      payload,
    });
    await this.drainGuiPlatformRequests(session);
  }

  async pushAndPollIslandTransport(
    data: Uint8Array,
    session?: GuiSessionToken,
  ): Promise<Uint8Array[]> {
    await this.invoke<void>('cmd_push_island_transport', {
      previewHandle: this.activePreviewHandle(session),
      data: Array.from(data),
    });
    await this.drainGuiPlatformRequests(session);
    return [];
  }

  async pollIslandTransport(_session?: GuiSessionToken): Promise<Uint8Array> {
    return new Uint8Array(0);
  }

  async pollGuiRender(session?: GuiSessionToken): Promise<Uint8Array> {
    await this.drainGuiPlatformRequests(session);
    const raw = await this.invoke<ArrayBuffer>('cmd_poll_gui_render', {
      previewHandle: this.activePreviewHandle(session),
    });
    return new Uint8Array(raw);
  }

  async pollGameRender(session?: GuiSessionToken): Promise<Uint8Array> {
    const raw = await this.invoke<ArrayBuffer>('cmd_poll_game_render', {
      previewHandle: this.activePreviewHandle(session),
    });
    return new Uint8Array(raw);
  }

  async submitGameRenderResult(
    result: Uint8Array,
    session?: GuiSessionToken,
  ): Promise<void> {
    await this.invoke('cmd_submit_game_render_result', {
      previewHandle: this.activePreviewHandle(session),
      result: Array.from(result),
    });
  }

  async resolveAppSurfaceRoute(
    surface: RuntimeHandle,
    session?: GuiSessionToken,
  ): Promise<ResolvedAppSurfaceRoute> {
    const route = await this.invoke<{
      session: RuntimeHandle;
      sessionEpoch: string;
      window: RuntimeHandle;
      view: RuntimeHandle;
      surface: RuntimeHandle;
      kind: ResolvedAppSurfaceRoute['kind'];
      zOrder: number;
      inputPolicy: ResolvedAppSurfaceRoute['inputPolicy'];
    }>('cmd_resolve_platform_surface', {
      previewHandle: this.activePreviewHandle(session),
      surface,
    });
    return Object.freeze({
      ...route,
      sessionEpoch: BigInt(route.sessionEpoch),
    });
  }

  async registerAppSurfaceShortcuts(
    surface: RuntimeHandle,
    registrations: readonly Readonly<{
      classMask: bigint;
      scope: 'view' | 'window' | 'session';
      priority: number;
    }>[],
    session?: GuiSessionToken,
  ): Promise<bigint> {
    const revision = await this.invoke<string>('cmd_register_platform_surface_shortcuts', {
      previewHandle: this.activePreviewHandle(session),
      surface,
      registrations: registrations.map((registration) => ({
        ...registration,
        classMask: registration.classMask.toString(),
      })),
    });
    return BigInt(revision);
  }

  async openFrameworkLane(
    owner: string,
    session?: GuiSessionToken,
  ): Promise<FrameworkLaneBinding> {
    const binding = await this.invoke<Omit<FrameworkLaneBinding, 'selectedExactFingerprint'> & {
      selectedExactFingerprint: number[];
    }>('cmd_open_framework_lane', {
      previewHandle: this.activePreviewHandle(session),
      owner,
    });
    return {
      ...binding,
      selectedExactFingerprint: new Uint8Array(binding.selectedExactFingerprint),
    };
  }

  async beginFrameworkProvider(moduleKey: string, session?: GuiSessionToken): Promise<void> {
    await this.invoke<void>('cmd_begin_framework_provider', {
      previewHandle: this.activePreviewHandle(session),
      moduleKey,
    });
  }

  async loadFrameworkProvider(moduleKey: string, session?: GuiSessionToken): Promise<void> {
    await this.invoke<void>('cmd_load_framework_provider', {
      previewHandle: this.activePreviewHandle(session),
      moduleKey,
    });
  }

  async unloadFrameworkProvider(moduleKey: string, session?: GuiSessionToken): Promise<void> {
    await this.invoke<void>('cmd_unload_framework_provider', {
      previewHandle: this.activePreviewHandle(session),
      moduleKey,
    });
  }

  async readyFrameworkProvider(moduleKey: string, session?: GuiSessionToken): Promise<void> {
    await this.invoke<void>('cmd_ready_framework_provider', {
      previewHandle: this.activePreviewHandle(session),
      moduleKey,
    });
  }

  async abortFrameworkProvider(moduleKey: string, session?: GuiSessionToken): Promise<void> {
    await this.invoke<void>('cmd_abort_framework_provider', {
      previewHandle: this.activePreviewHandle(session),
      moduleKey,
    });
  }

  async closeFrameworkProvider(moduleKey: string, session?: GuiSessionToken): Promise<void> {
    await this.invoke<void>('cmd_close_framework_provider', {
      previewHandle: this.activePreviewHandle(session),
      moduleKey,
    });
  }

  async pollFrameworkLane(
    binding: FrameworkLaneBinding,
    session?: GuiSessionToken,
  ): Promise<Uint8Array> {
    const raw = await this.invoke<ArrayBuffer>('cmd_poll_framework_lane', {
      previewHandle: this.activePreviewHandle(session),
      channelIndex: binding.channel.index,
      channelGeneration: binding.channel.generation,
      channelEpoch: binding.channelEpoch,
    });
    return new Uint8Array(raw);
  }

  async submitFrameworkLane(
    binding: FrameworkLaneBinding,
    packet: Uint8Array,
    session?: GuiSessionToken,
  ): Promise<void> {
    await this.invoke<void>('cmd_submit_framework_lane', {
      previewHandle: this.activePreviewHandle(session),
      channelIndex: binding.channel.index,
      channelGeneration: binding.channel.generation,
      channelEpoch: binding.channelEpoch,
      packet: Array.from(packet),
    });
    await this.drainGuiPlatformRequests(session);
  }

  async submitFrameworkLaneBatch(
    binding: FrameworkLaneBinding,
    packets: readonly Uint8Array[],
    session?: GuiSessionToken,
  ): Promise<void> {
    await this.invoke<void>('cmd_submit_framework_lane_batch', {
      previewHandle: this.activePreviewHandle(session),
      channelIndex: binding.channel.index,
      channelGeneration: binding.channel.generation,
      channelEpoch: binding.channelEpoch,
      packets: packets.map((packet) => Array.from(packet)),
    });
    await this.drainGuiPlatformRequests(session);
  }

  async pollDisplayTimingRequest(
    session?: GuiSessionToken,
  ): Promise<DisplayTimingRequest | null> {
    return this.invoke<DisplayTimingRequest | null>('cmd_poll_display_timing_request', {
      previewHandle: this.activePreviewHandle(session),
    });
  }

  async submitDisplayPulse(
    request: DisplayTimingRequest,
    observedMicros: string,
    intervalMicros: string,
    session?: GuiSessionToken,
  ): Promise<DisplayPulseSubmission> {
    return this.invoke<DisplayPulseSubmission>('cmd_submit_display_pulse', {
      previewHandle: this.activePreviewHandle(session),
      request,
      observedMicros,
      intervalMicros,
    });
  }

  async stopGui(requested?: GuiSessionToken): Promise<void> {
    const session = this.guiSession.clear(requested);
    if (!session) return;
    this.disposeVoguiSubscriptions(session);
    const previewHandle = this.previewHandles.get(session.id);
    this.previewHandles.delete(session.id);
    this.bridgeEpochs.delete(session.id);
    this.bridgeOwners.delete(session.id);
    this.bridgeOutboundSequences.delete(session.id);
    this.guiFatalErrors.delete(session.id);
    this.guiGuestExits.delete(session.id);
    this.guiNavigationUrls.delete(session.id);
    if (previewHandle) {
      try {
        await this.invoke<void>('cmd_stop_gui', { previewHandle });
      } finally {
        releaseBrowserPlatformSession(previewHandle);
      }
    }
  }

  async restartWebviewBridge(
    snapshots: readonly Readonly<{ key: bigint; payload: Uint8Array }>[],
    requested?: GuiSessionToken,
  ): Promise<bigint> {
    const session = requested ?? this.guiSession.active;
    if (!session) throw new Error('No GUI preview is active');
    const previewHandle = this.activePreviewHandle(session);
    const ownedSnapshots = snapshots.map((snapshot) => {
      if (snapshot.key <= 0n) throw new Error('WebView restart snapshot key must be positive');
      return {
        key: snapshot.key.toString(),
        payload: [...snapshot.payload],
      };
    });
    const report = await this.invoke<{
      oldEpoch: string;
      newEpoch: string;
      discardedToWebview: number;
      discardedFromWebview: number;
    }>('cmd_restart_webview_bridge_with_snapshots', {
      previewHandle,
      snapshots: ownedSnapshots,
    });
    const newEpoch = BigInt(report.newEpoch);
    this.bridgeEpochs.set(session.id, newEpoch);
    const currentOwner = this.bridgeOwners.get(session.id);
    if (!currentOwner) throw new Error('WebView bridge owner is unavailable');
    this.bridgeOwners.set(session.id, Object.freeze({
      ...currentOwner,
      bridgeEpoch: newEpoch,
    }));
    this.bridgeOutboundSequences.set(session.id, 1n);
    return newEpoch;
  }

  async restartComposedWebview(session?: GuiSessionToken): Promise<bigint> {
    return this.restartWebviewBridge([], session);
  }

  async pollWebviewBridge(): Promise<Uint8Array> {
    const raw = await this.invoke<ArrayBuffer>('cmd_poll_webview_bridge', {
      previewHandle: this.activePreviewHandle(),
    });
    return new Uint8Array(raw);
  }

  async pollWebviewBridgeFrame(): Promise<WebviewBridgeFrame | null> {
    const session = this.guiSession.active;
    if (!session) throw new Error('No GUI preview is active');
    const encoded = await this.pollWebviewBridge();
    if (encoded.byteLength === 0) return null;
    const owner = this.bridgeOwners.get(session.id);
    if (!owner) throw new Error('WebView bridge owner is unavailable');
    return decodeWebviewBridgeFrame(encoded, owner);
  }

  async submitWebviewBridge(frame: Uint8Array): Promise<void> {
    await this.invoke<void>('cmd_submit_webview_bridge', {
      previewHandle: this.activePreviewHandle(),
      frame: [...frame],
    });
  }

  async submitWebviewBridgePayload(
    lane: WebviewBridgeLane,
    coalesceKey: bigint,
    payload: Uint8Array,
  ): Promise<void> {
    const session = this.guiSession.active;
    if (!session) throw new Error('No GUI preview is active');
    const owner = this.bridgeOwners.get(session.id);
    const sequence = this.bridgeOutboundSequences.get(session.id);
    if (!owner || sequence === undefined) throw new Error('WebView bridge owner is unavailable');
    const encoded = encodeWebviewBridgeFrame({
      ...owner,
      sequence,
      lane,
      coalesceKey,
      payload,
    });
    await this.submitWebviewBridge(encoded);
    this.bridgeOutboundSequences.set(session.id, sequence + 1n);
  }

  private activePreviewHandle(requested?: GuiSessionToken): StudioSessionHandle {
    const session = requested ?? this.guiSession.active;
    if (!session) throw new Error('No GUI preview is active');
    if (!this.guiSession.isActive(session)) throw new Error('GUI preview session is stale');
    const handle = this.previewHandles.get(session.id);
    if (!handle) throw new Error('GUI preview host is unavailable');
    return handle;
  }

  private async drainGuiPlatformRequests(session?: GuiSessionToken): Promise<void> {
    const owner = session ?? this.guiSession.active;
    if (!owner) throw new Error('No GUI preview is active');
    const previewHandle = this.activePreviewHandle(owner);
    const subscriptions = await this.invoke<ArrayBuffer>('cmd_poll_vogui_subscriptions', {
      previewHandle,
    });
    this.voguiSubscriptionsFor(owner).reconcile(new Uint8Array(subscriptions));
    for (let count = 0; count < 128; count += 1) {
      const raw = await this.invoke<ArrayBuffer>('cmd_poll_platform_request', {
        previewHandle,
      });
      const frame = new Uint8Array(raw);
      if (frame.length === 0) return;
      const request = decodeGuiPlatformRequest(frame);
      if (
        request.session.index !== previewHandle.index
        || request.session.generation !== previewHandle.generation
      ) {
        throw new Error('GUI platform request escaped its preview Session');
      }
      const result = await executeGuiPlatformRequest(request, this.guiPlatformHost);
      await this.invoke<void>('cmd_complete_platform_request', {
        previewHandle,
        requestId: request.requestId.toString(),
        outcome: result.outcome,
        payload: Array.from(result.payload),
      });
    }
    throw new Error('GUI platform request drain exceeded its bounded turn limit');
  }

  private async submitVoguiSubscriptionEvent(
    session: GuiSessionToken,
    subscription: VoguiHostSubscription,
    payload: Uint8Array,
  ): Promise<void> {
    await this.invoke<void>('cmd_submit_vogui_subscription_event', {
      previewHandle: this.activePreviewHandle(session),
      caller: [...subscription.caller],
      handleIndex: subscription.handleIndex,
      handleGeneration: subscription.handleGeneration,
      payload: [...payload],
    });
    await this.drainGuiPlatformRequests(session);
  }

  private voguiSubscriptionsFor(session: GuiSessionToken): VoguiSubscriptionHost {
    let host = this.voguiSubscriptions.get(session.id);
    if (!host) {
      host = new VoguiSubscriptionHost(
        session.id,
        (subscription, payload) =>
          this.submitVoguiSubscriptionEvent(session, subscription, payload),
      );
      host.setInteractive(this.guiSession.active === session);
      this.voguiSubscriptions.set(session.id, host);
    }
    return host;
  }

  private disposeVoguiSubscriptions(session: GuiSessionToken): void {
    this.voguiSubscriptions.get(session.id)?.dispose();
    this.voguiSubscriptions.delete(session.id);
  }

  private async navigateGuiBrowser(
    command: string,
    argument: string | null,
    preview: StudioSessionHandle,
  ): Promise<Uint8Array> {
    const sessionId = this.studioSessionIdForPreview(preview);
    const current = this.guiNavigationUrls.get(sessionId) ?? window.location.href;
    const normalized = new URL(argument ?? '', current).href;
    if (command === 'external') {
      window.open(normalized, '_blank', 'noopener,noreferrer');
    } else if (command === 'push' || command === 'replace') {
      this.guiNavigationUrls.set(sessionId, normalized);
      window.dispatchEvent(new CustomEvent('studio-gui-navigation', {
        detail: { sessionId, url: normalized, command },
      }));
    } else {
      throw new Error(`unknown navigation command '${command}'`);
    }
    return new TextEncoder().encode(normalized);
  }

  private studioSessionIdForPreview(preview: StudioSessionHandle): number {
    const sessionId = [...this.previewHandles.entries()].find(([, handle]) => (
      handle.index === preview.index && handle.generation === preview.generation
    ))?.[0];
    if (sessionId === undefined) throw new Error('GUI App Session is stale');
    return sessionId;
  }

  async voInit(path: string, module: string, mainContent: string): Promise<string> {
    return this.invoke<string>('cmd_vo_init', { path, module, mainContent });
  }

  spawnProcess(
    program: string,
    args: string[],
    cwd?: string,
    env?: Record<string, string>,
  ): StreamHandle<ProcEvent> {
    return makeTauriStreamHandle<ProcEvent>((channel) =>
      this.invoke<void>('cmd_spawn_process', { program, args, cwd, env, onEvent: channel }),
      (event) => event.kind === 'done' || event.kind === 'error',
    );
  }

  async httpRequest(method: string, url: string, opts?: HttpOpts): Promise<HttpResult> {
    return this.invoke<HttpResult>('cmd_http_request', { method, url, opts });
  }

  async pickDirectory(defaultPath?: string): Promise<string | null> {
    return this.invoke<string | null>('cmd_pick_directory', { defaultPath: defaultPath ?? null });
  }

  async pickFile(defaultPath?: string, filters?: FileDialogFilter[]): Promise<string | null> {
    return this.invoke<string | null>('cmd_pick_file', {
      defaultPath: defaultPath ?? null,
      filters: filters ?? null,
    });
  }

  async createWorkspaceFiles(files: { path: string; content: string }[]): Promise<void> {
    await this.invoke<void>('cmd_create_workspace_files', { files });
  }

  async createProjectFile(path: string, content: string): Promise<void> {
    await this.invoke<void>('cmd_create_project_file', { path, content });
  }

  async gitExec(op: GitOp): Promise<GitResult> {
    return this.invoke<GitResult>('cmd_git_exec', { op });
  }

  private async invoke<T>(command: string, args?: Record<string, unknown>): Promise<T> {
    return tauriInvoke<T>(command, args);
  }

  private async ensureGuiLogListener(): Promise<void> {
    if (!this.guiLogListenerPromise) {
      this.guiLogListenerPromise = tauriListen<StudioLogEvent>('studio_log', (event) => {
        if (!this.guiSession.isActiveId(event.payload.sessionId)) {
          return;
        }
        pushNativeStudioLog(event.payload.record);
      }).then(() => undefined);
    }
    await this.guiLogListenerPromise;
  }

  private async ensureGuiExitListener(): Promise<void> {
    if (!this.guiExitListenerPromise) {
      this.guiExitListenerPromise = tauriListen<GuiGuestExitEvent>('gui_guest_exit', (event) => {
        const session = this.guiSession.get(event.payload.sessionId);
        if (!session) return;
        const exitCode = event.payload.exitCode;
        this.guiGuestExits.set(session.id, exitCode);
        this.guiGuestExitHandler?.(session, exitCode);
      }).then(() => undefined);
    }
    await this.guiExitListenerPromise;
  }

  private async ensureGuiFatalListener(): Promise<void> {
    if (!this.guiFatalListenerPromise) {
      this.guiFatalListenerPromise = tauriListen<GuiFatalErrorEvent>('gui_fatal_error', (event) => {
        const session = this.guiSession.get(event.payload.sessionId);
        if (!session || this.guiFatalErrors.has(session.id)) return;
        const error = new Error(event.payload.message || 'Native GUI guest failed');
        this.guiFatalErrors.set(session.id, error);
        this.guiGuestErrorHandler?.(session, error);
      }).then(() => undefined);
    }
    await this.guiFatalListenerPromise;
  }
}
