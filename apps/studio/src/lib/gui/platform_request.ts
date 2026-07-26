export type GuiPlatformRequestKind =
  | 'clipboard.read'
  | 'clipboard.write'
  | 'file.open'
  | 'file.save'
  | 'navigation'
  | 'window.command'
  | 'view.command'
  | 'vfs'
  | 'capability'
  | 'audio.activation'
  | 'haptics';

export type GuiPlatformHandle = Readonly<{ index: number; generation: number }>;

export type GuiPlatformRequestScope =
  | Readonly<{ kind: 'session' }>
  | Readonly<{ kind: 'window'; window: GuiPlatformHandle }>
  | Readonly<{ kind: 'view'; window: GuiPlatformHandle; view: GuiPlatformHandle }>
  | Readonly<{
      kind: 'surface';
      window: GuiPlatformHandle;
      view: GuiPlatformHandle;
      surface: GuiPlatformHandle;
    }>;

export interface GuiPlatformRequest {
  requestId: bigint;
  sequence: bigint;
  deadlineMillis: bigint;
  session: GuiPlatformHandle;
  sessionEpoch: bigint;
  kind: GuiPlatformRequestKind;
  scope: GuiPlatformRequestScope;
  payload: Uint8Array;
}

export function decodeGuiPlatformRequest(frame: Uint8Array): GuiPlatformRequest {
  const fixedLength = 76;
  if (frame.length < fixedLength) {
    throw new Error('truncated VPR1 platform request');
  }
  if (frame[0] !== 0x56 || frame[1] !== 0x50 || frame[2] !== 0x52 || frame[3] !== 0x31) {
    throw new Error('invalid VPR1 platform request magic');
  }
  const view = new DataView(frame.buffer, frame.byteOffset, frame.byteLength);
  const kindTag = frame[4];
  const kind = [
    '',
    'clipboard.read',
    'clipboard.write',
    'file.open',
    'file.save',
    'navigation',
    'window.command',
    'view.command',
    'vfs',
    'capability',
    'audio.activation',
    'haptics',
  ][kindTag] as GuiPlatformRequestKind | undefined;
  if (!kind) {
    throw new Error(`unknown VPR1 platform request kind ${kindTag}`);
  }
  const scopeTag = frame[5];
  if (scopeTag < 1 || scopeTag > 4 || view.getUint16(6, true) !== 0) {
    throw new Error('invalid VPR1 platform request scope header');
  }
  const requestId = view.getBigUint64(8, true);
  const sequence = view.getBigUint64(16, true);
  const deadlineMillis = view.getBigUint64(24, true);
  const session = decodeHandle(view, 32);
  const sessionEpoch = view.getBigUint64(40, true);
  const window = decodeHandle(view, 48);
  const appView = decodeHandle(view, 56);
  const surface = decodeHandle(view, 64);
  if (
    requestId === 0n
    || sequence === 0n
    || deadlineMillis === 0n
    || !validHandle(session)
    || sessionEpoch === 0n
  ) {
    throw new Error('invalid VPR1 platform request identity');
  }
  const scope = decodeScope(scopeTag, window, appView, surface);
  const payloadLength = view.getUint32(72, true);
  if (fixedLength + payloadLength !== frame.length) {
    throw new Error('invalid VPR1 platform request payload length');
  }
  return {
    requestId,
    sequence,
    deadlineMillis,
    session,
    sessionEpoch,
    kind,
    scope,
    payload: frame.slice(fixedLength),
  };
}

function decodeHandle(view: DataView, offset: number): GuiPlatformHandle {
  return Object.freeze({
    index: view.getUint32(offset, true),
    generation: view.getUint32(offset + 4, true),
  });
}

function validHandle(handle: GuiPlatformHandle): boolean {
  return handle.index !== 0xFFFF_FFFF && handle.generation !== 0;
}

function invalidHandle(handle: GuiPlatformHandle): boolean {
  return handle.index === 0xFFFF_FFFF && handle.generation === 0;
}

function decodeScope(
  tag: number,
  window: GuiPlatformHandle,
  view: GuiPlatformHandle,
  surface: GuiPlatformHandle,
): GuiPlatformRequestScope {
  switch (tag) {
    case 1:
      if (!invalidHandle(window) || !invalidHandle(view) || !invalidHandle(surface)) break;
      return Object.freeze({ kind: 'session' });
    case 2:
      if (!validHandle(window) || !invalidHandle(view) || !invalidHandle(surface)) break;
      return Object.freeze({ kind: 'window', window });
    case 3:
      if (!validHandle(window) || !validHandle(view) || !invalidHandle(surface)) break;
      return Object.freeze({ kind: 'view', window, view });
    case 4:
      if (!validHandle(window) || !validHandle(view) || !validHandle(surface)) break;
      return Object.freeze({ kind: 'surface', window, view, surface });
  }
  throw new Error('invalid VPR1 platform request scope identity');
}
