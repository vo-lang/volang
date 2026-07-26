import type {
  GuiPlatformHandle,
  GuiPlatformRequest,
  GuiPlatformRequestScope,
} from './platform_request';

export type GuiPlatformCompletionOutcome =
  | 'completed'
  | 'denied'
  | 'unsupported'
  | 'cancelled'
  | 'timed_out'
  | 'failed';

export interface GuiPlatformExecutionResult {
  outcome: GuiPlatformCompletionOutcome;
  payload: Uint8Array;
}

export type GuiFileDialogDescriptor = Readonly<{
  defaultPath: string | null;
  filters: ReadonlyArray<Readonly<{ name: string; extensions: string[] }>>;
}>;

export interface GuiPlatformHostAdapter {
  openFile?(
    descriptor: GuiFileDialogDescriptor,
    request: GuiPlatformRequest,
  ): Promise<string | null>;
  saveFile?(
    descriptor: GuiFileDialogDescriptor,
    request: GuiPlatformRequest,
  ): Promise<string | null>;
  navigation?(
    command: string,
    argument: string | null,
    request: GuiPlatformRequest,
  ): Promise<Uint8Array>;
  readVfs?(path: string, request: GuiPlatformRequest): Promise<Uint8Array>;
  writeVfs?(path: string, data: Uint8Array, request: GuiPlatformRequest): Promise<void>;
  statVfs?(path: string, request: GuiPlatformRequest): Promise<unknown>;
  listVfs?(path: string, request: GuiPlatformRequest): Promise<unknown>;
  windowCommand?(
    scope: Extract<GuiPlatformRequestScope, { kind: 'window' }>,
    command: string,
    argument: string | null,
  ): Promise<Uint8Array>;
  viewCommand?(
    scope: Extract<GuiPlatformRequestScope, { kind: 'view' }>,
    command: string,
    argument: string | null,
  ): Promise<Uint8Array>;
  activateAudio?(): Promise<void>;
}

export type BrowserPlatformEvent =
  | Readonly<{
    kind: 'window.focus' | 'window.close' | 'window.fullscreen' | 'window.exit-fullscreen';
    session: GuiPlatformHandle;
    window: GuiPlatformHandle;
  }>
  | Readonly<{
    kind: 'window.title';
    session: GuiPlatformHandle;
    window: GuiPlatformHandle;
    title: string;
  }>
  | Readonly<{
    kind: 'view.focus' | 'view.blur';
    session: GuiPlatformHandle;
    window: GuiPlatformHandle;
    view: GuiPlatformHandle;
  }>
  | Readonly<{
    kind: 'view.title';
    session: GuiPlatformHandle;
    window: GuiPlatformHandle;
    view: GuiPlatformHandle;
    title: string;
  }>;

const browserPlatformListeners = new Set<(event: BrowserPlatformEvent) => void>();

export function subscribeBrowserPlatformEvents(
  listener: (event: BrowserPlatformEvent) => void,
): () => void {
  browserPlatformListeners.add(listener);
  return () => browserPlatformListeners.delete(listener);
}

function publishBrowserPlatformEvent(event: BrowserPlatformEvent): void {
  for (const listener of [...browserPlatformListeners]) listener(event);
}

const encoder = new TextEncoder();
const decoder = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });

export async function executeGuiPlatformRequest(
  request: GuiPlatformRequest,
  adapter: GuiPlatformHostAdapter = {},
): Promise<GuiPlatformExecutionResult> {
  try {
    switch (request.kind) {
      case 'clipboard.read':
        if (!navigator.clipboard?.readText) return empty('unsupported');
        return completed(encoder.encode(await navigator.clipboard.readText()));
      case 'clipboard.write':
        if (!navigator.clipboard?.writeText) return empty('unsupported');
        await navigator.clipboard.writeText(decoder.decode(request.payload));
        return empty('completed');
      case 'file.open': {
        if (!adapter.openFile) return empty('unsupported');
        const path = await adapter.openFile(decodeFileDialog(request.payload), request);
        return path === null ? empty('cancelled') : completed(encoder.encode(path));
      }
      case 'file.save': {
        if (!adapter.saveFile) return empty('unsupported');
        const path = await adapter.saveFile(decodeFileDialog(request.payload), request);
        return path === null ? empty('cancelled') : completed(encoder.encode(path));
      }
      case 'navigation': {
        const navigation = decodeCommand(request.payload, 'push');
        if (adapter.navigation) {
          return completed(await adapter.navigation(
            navigation.command,
            navigation.argument,
            request,
          ));
        }
        const normalized = new URL(navigation.argument ?? '', window.location.href).href;
        if (navigation.command === 'replace') {
          window.history.replaceState(null, '', normalized);
        } else if (navigation.command === 'external') {
          window.location.assign(normalized);
        } else {
          window.history.pushState(null, '', normalized);
        }
        window.dispatchEvent(new PopStateEvent('popstate'));
        return completed(encoder.encode(normalized));
      }
      case 'window.command': {
        if (request.scope.kind !== 'window') {
          throw new Error('window command requires Window scope');
        }
        const command = decodeCommand(request.payload);
        if (adapter.windowCommand) {
          return completed(await adapter.windowCommand(
            request.scope,
            command.command,
            command.argument,
          ));
        }
        return completed(await executeBrowserWindowCommand(
          request.session,
          request.scope,
          command.command,
          command.argument,
        ));
      }
      case 'view.command': {
        if (request.scope.kind !== 'view') {
          throw new Error('view command requires View scope');
        }
        const command = decodeCommand(request.payload);
        if (adapter.viewCommand) {
          return completed(await adapter.viewCommand(
            request.scope,
            command.command,
            command.argument,
          ));
        }
        return completed(executeBrowserViewCommand(
          request.session,
          request.scope,
          command.command,
          command.argument,
        ));
      }
      case 'vfs':
        return completed(await executeVfs(request.payload, adapter, request));
      case 'capability':
        return completed(Uint8Array.of(platformCapabilitySupported(
          decoder.decode(request.payload),
          adapter,
        ) ? 1 : 0));
      case 'audio.activation':
        if (adapter.activateAudio) {
          await adapter.activateAudio();
        } else {
          window.dispatchEvent(new CustomEvent('vogui-audio-activation'));
        }
        return empty('completed');
      case 'haptics':
        return executeBrowserHaptics(request.payload);
      default:
        return empty('unsupported');
    }
  } catch (error) {
    return {
      outcome: 'failed',
      payload: encoder.encode(error instanceof Error ? error.message : String(error)),
    };
  }
}

function platformCapabilitySupported(
  capability: string,
  adapter: GuiPlatformHostAdapter,
): boolean {
  switch (capability) {
    case 'clipboard.read':
      return typeof navigator.clipboard?.readText === 'function';
    case 'clipboard.write':
      return typeof navigator.clipboard?.writeText === 'function';
    case 'navigation':
    case 'window.command':
    case 'view.command':
    case 'audio.activation':
    case 'capability':
      return true;
    case 'haptics':
      return browserHasHaptics();
    case 'file.open':
      return adapter.openFile !== undefined;
    case 'file.save':
      return adapter.saveFile !== undefined;
    case 'vfs':
      return adapter.readVfs !== undefined
        && adapter.writeVfs !== undefined
        && adapter.statVfs !== undefined
        && adapter.listVfs !== undefined;
    default:
      return false;
  }
}

type BrowserHapticActuator = {
  playEffect(
    type: string,
    parameters: {
      duration: number;
      startDelay: number;
      strongMagnitude?: number;
      weakMagnitude?: number;
      leftTrigger?: number;
      rightTrigger?: number;
    },
  ): Promise<string>;
};

function browserHasHaptics(): boolean {
  return Array.from(navigator.getGamepads?.() ?? []).some((gamepad) => (
    gamepad !== null
    && typeof (gamepad as Gamepad & { vibrationActuator?: BrowserHapticActuator })
      .vibrationActuator?.playEffect === 'function'
  ));
}

async function executeBrowserHaptics(payload: Uint8Array): Promise<GuiPlatformExecutionResult> {
  if (
    payload.byteLength !== 32
    || payload[0] !== 0x56
    || payload[1] !== 0x48
    || payload[2] !== 0x50
    || payload[3] !== 0x31
  ) {
    throw new Error('invalid VHP1 haptics request');
  }
  const view = new DataView(payload.buffer, payload.byteOffset, payload.byteLength);
  const effect = payload[4];
  const device = view.getBigUint64(8, true);
  const generation = view.getUint32(16, true);
  const duration = view.getUint32(20, true);
  const first = view.getUint16(24, true);
  const second = view.getUint16(26, true);
  if (
    device === 0n
    || generation === 0
    || duration === 0
    || duration > 60_000
    || first > 32_768
    || second > 32_768
    || (effect !== 1 && effect !== 2)
  ) {
    throw new Error('invalid VHP1 haptics values');
  }
  const gamepad = Array.from(navigator.getGamepads?.() ?? []).find((candidate) => (
    candidate !== null && BigInt(candidate.index + 1) === device
  ));
  const actuator = (
    gamepad as (Gamepad & { vibrationActuator?: BrowserHapticActuator }) | undefined
  )?.vibrationActuator;
  if (!actuator?.playEffect) return empty('unsupported');
  const scale = 1 / 32_768;
  const result = await actuator.playEffect(
    effect === 1 ? 'dual-rumble' : 'trigger-rumble',
    effect === 1
      ? {
        duration,
        startDelay: 0,
        strongMagnitude: first * scale,
        weakMagnitude: second * scale,
      }
      : {
        duration,
        startDelay: 0,
        leftTrigger: first * scale,
        rightTrigger: second * scale,
      },
  );
  return result === 'complete' || result === 'preempted'
    ? empty('completed')
    : empty('failed');
}

function decodeFileDialog(payload: Uint8Array): GuiFileDialogDescriptor {
  if (payload.byteLength === 0) return { defaultPath: null, filters: [] };
  const text = decoder.decode(payload);
  if (!text.trimStart().startsWith('{')) return { defaultPath: text || null, filters: [] };
  const value = JSON.parse(text) as {
    defaultPath?: unknown;
    filters?: Array<{ name?: unknown; extensions?: unknown }>;
  };
  const defaultPath = typeof value.defaultPath === 'string' ? value.defaultPath : null;
  const filters = Array.isArray(value.filters)
    ? value.filters.flatMap((filter) => (
      typeof filter.name === 'string'
      && Array.isArray(filter.extensions)
      && filter.extensions.every((extension) => typeof extension === 'string')
        ? [{ name: filter.name, extensions: filter.extensions as string[] }]
        : []
    ))
    : [];
  return { defaultPath, filters };
}

function decodeCommand(
  payload: Uint8Array,
  defaultCommand = '',
): { command: string; argument: string | null } {
  const text = decoder.decode(payload);
  if (!text.trimStart().startsWith('{')) {
    return defaultCommand
      ? { command: defaultCommand, argument: text }
      : { command: text, argument: null };
  }
  const value = JSON.parse(text) as { command?: unknown; argument?: unknown; url?: unknown };
  const command = typeof value.command === 'string' ? value.command : defaultCommand;
  const argument = typeof value.argument === 'string'
    ? value.argument
    : typeof value.url === 'string' ? value.url : null;
  if (command.length === 0) throw new Error('platform command name is empty');
  return { command, argument };
}

type BrowserWindowState = {
  title: string;
  closed: boolean;
};

const browserWindows = new Map<string, BrowserWindowState>();
const browserViews = new Set<string>();
let focusedBrowserWindow: string | null = null;
let focusedBrowserView: string | null = null;

export function releaseBrowserPlatformSession(session: GuiPlatformHandle): void {
  const prefix = `${platformHandleKey(session)}/`;
  for (const key of browserWindows.keys()) {
    if (key.startsWith(prefix)) browserWindows.delete(key);
  }
  for (const key of browserViews) {
    if (key.startsWith(prefix)) browserViews.delete(key);
  }
  if (focusedBrowserWindow?.startsWith(prefix)) focusedBrowserWindow = null;
  if (focusedBrowserView?.startsWith(prefix)) focusedBrowserView = null;
}

function platformHandleKey(handle: GuiPlatformHandle): string {
  return `${handle.index}:${handle.generation}`;
}

function browserWindowKey(
  session: GuiPlatformHandle,
  window: GuiPlatformHandle,
): string {
  return `${platformHandleKey(session)}/${platformHandleKey(window)}`;
}

function browserViewKey(
  session: GuiPlatformHandle,
  window: GuiPlatformHandle,
  view: GuiPlatformHandle,
): string {
  return `${browserWindowKey(session, window)}/${platformHandleKey(view)}`;
}

async function executeBrowserWindowCommand(
  session: GuiPlatformHandle,
  scope: Extract<GuiPlatformRequestScope, { kind: 'window' }>,
  command: string,
  argument: string | null,
): Promise<Uint8Array> {
  const key = browserWindowKey(session, scope.window);
  const state = browserWindows.get(key) ?? { title: '', closed: false };
  if (state.closed && command !== 'focus') {
    throw new Error('browser Window is closed');
  }
  switch (command) {
    case 'focus':
      state.closed = false;
      focusedBrowserWindow = key;
      window.focus();
      publishBrowserPlatformEvent({
        kind: 'window.focus',
        session,
        window: scope.window,
      });
      break;
    case 'close':
      state.closed = true;
      if (focusedBrowserWindow === key && document.fullscreenElement) {
        await document.exitFullscreen();
      }
      if (focusedBrowserWindow === key) focusedBrowserWindow = null;
      if (focusedBrowserView?.startsWith(`${key}/`)) focusedBrowserView = null;
      publishBrowserPlatformEvent({
        kind: 'window.close',
        session,
        window: scope.window,
      });
      break;
    case 'fullscreen':
      focusedBrowserWindow = key;
      await document.documentElement.requestFullscreen();
      publishBrowserPlatformEvent({
        kind: 'window.fullscreen',
        session,
        window: scope.window,
      });
      break;
    case 'exit-fullscreen':
      if (focusedBrowserWindow !== key) {
        throw new Error('browser Window does not own fullscreen presentation');
      }
      if (document.fullscreenElement) await document.exitFullscreen();
      publishBrowserPlatformEvent({
        kind: 'window.exit-fullscreen',
        session,
        window: scope.window,
      });
      break;
    case 'title':
      state.title = argument ?? '';
      if (focusedBrowserWindow === null || focusedBrowserWindow === key) {
        focusedBrowserWindow = key;
        document.title = state.title;
      }
      publishBrowserPlatformEvent({
        kind: 'window.title',
        session,
        window: scope.window,
        title: state.title,
      });
      break;
    default:
      throw new Error(`unknown window command '${command}'`);
  }
  browserWindows.set(key, state);
  return new Uint8Array(0);
}

function executeBrowserViewCommand(
  session: GuiPlatformHandle,
  scope: Extract<GuiPlatformRequestScope, { kind: 'view' }>,
  command: string,
  argument: string | null,
): Uint8Array {
  const windowKey = browserWindowKey(session, scope.window);
  const windowState = browserWindows.get(windowKey) ?? { title: '', closed: false };
  if (windowState.closed) throw new Error('browser View belongs to a closed Window');
  const key = browserViewKey(session, scope.window, scope.view);
  browserViews.add(key);
  switch (command) {
    case 'focus':
      focusedBrowserWindow = windowKey;
      focusedBrowserView = key;
      window.focus();
      publishBrowserPlatformEvent({
        kind: 'view.focus',
        session,
        window: scope.window,
        view: scope.view,
      });
      break;
    case 'blur':
      if (focusedBrowserView === key) {
        focusedBrowserView = null;
        (document.activeElement as HTMLElement | null)?.blur();
      }
      publishBrowserPlatformEvent({
        kind: 'view.blur',
        session,
        window: scope.window,
        view: scope.view,
      });
      break;
    case 'title':
      windowState.title = argument ?? '';
      browserWindows.set(windowKey, windowState);
      if (focusedBrowserView === key) document.title = windowState.title;
      publishBrowserPlatformEvent({
        kind: 'view.title',
        session,
        window: scope.window,
        view: scope.view,
        title: windowState.title,
      });
      break;
    default:
      throw new Error(`unknown view command '${command}'`);
  }
  return new Uint8Array(0);
}

async function executeVfs(
  payload: Uint8Array,
  adapter: GuiPlatformHostAdapter,
  request: GuiPlatformRequest,
): Promise<Uint8Array> {
  if (payload.byteLength < 16
    || payload[0] !== 0x56
    || payload[1] !== 0x46
    || payload[2] !== 0x53
    || payload[3] !== 0x31) {
    throw new Error('invalid VFS1 request');
  }
  const view = new DataView(payload.buffer, payload.byteOffset, payload.byteLength);
  const operation = payload[4];
  if (payload[5] !== 0 || view.getUint16(6, true) !== 0) {
    throw new Error('invalid VFS1 request header');
  }
  const pathLength = view.getUint32(8, true);
  const dataLength = view.getUint32(12, true);
  if (16 + pathLength + dataLength !== payload.byteLength) {
    throw new Error('invalid VFS1 request length');
  }
  const path = decoder.decode(payload.slice(16, 16 + pathLength));
  const data = payload.slice(16 + pathLength);
  switch (operation) {
    case 1:
      if (!adapter.readVfs || dataLength !== 0) throw new Error('VFS read is unavailable');
      return adapter.readVfs(path, request);
    case 2:
      if (!adapter.writeVfs) throw new Error('VFS write is unavailable');
      await adapter.writeVfs(path, data, request);
      return new Uint8Array(0);
    case 3:
      if (!adapter.statVfs || dataLength !== 0) throw new Error('VFS stat is unavailable');
      return encoder.encode(JSON.stringify(await adapter.statVfs(path, request)));
    case 4:
      if (!adapter.listVfs || dataLength !== 0) throw new Error('VFS list is unavailable');
      return encoder.encode(JSON.stringify(await adapter.listVfs(path, request)));
    default:
      throw new Error(`unknown VFS1 operation ${operation}`);
  }
}

function completed(payload: Uint8Array): GuiPlatformExecutionResult {
  return { outcome: 'completed', payload };
}

function empty(outcome: GuiPlatformCompletionOutcome): GuiPlatformExecutionResult {
  return { outcome, payload: new Uint8Array(0) };
}
