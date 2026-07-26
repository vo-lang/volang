export type WebviewBridgeLane =
  | 'control'
  | 'completion'
  | 'reliable-input'
  | 'framework'
  | 'presentation'
  | 'diagnostics';

export type WebviewBridgeOwner = Readonly<{
  session: Readonly<{ index: number; generation: number }>;
  sessionEpoch: bigint;
  bridgeEpoch: bigint;
}>;

export type WebviewBridgeFrame = WebviewBridgeOwner & Readonly<{
  sequence: bigint;
  lane: WebviewBridgeLane;
  coalesceKey: bigint;
  payload: Uint8Array;
}>;

const FIXED_BYTES = 52;
const MAX_FRAME_BYTES = 1024 * 1024;
const COMPOSITION_RECOVERY_FIXED_BYTES = 16;

export const WEBVIEW_COMPOSITION_RECOVERY_SNAPSHOT_KEY = 0x5650_5742_5245_4331n;

export type WebviewCompositionRecoverySnapshot = Readonly<{
  uiModel: Uint8Array;
  simulationWorld: Uint8Array;
}>;

export function encodeWebviewCompositionRecoverySnapshot(
  uiModel: Uint8Array,
  simulationWorld: Uint8Array,
): Uint8Array {
  if (
    uiModel.byteLength > 0xffff_ffff
    || simulationWorld.byteLength > 0xffff_ffff
    || COMPOSITION_RECOVERY_FIXED_BYTES + uiModel.byteLength + simulationWorld.byteLength
      > MAX_FRAME_BYTES
  ) {
    throw new Error('WebView composition recovery snapshot exceeds bridge capacity');
  }
  const encoded = new Uint8Array(
    COMPOSITION_RECOVERY_FIXED_BYTES + uiModel.byteLength + simulationWorld.byteLength,
  );
  encoded.set([0x56, 0x57, 0x52, 0x43, 0x31], 0);
  const view = new DataView(encoded.buffer);
  view.setUint32(8, uiModel.byteLength, true);
  view.setUint32(12, simulationWorld.byteLength, true);
  encoded.set(uiModel, COMPOSITION_RECOVERY_FIXED_BYTES);
  encoded.set(simulationWorld, COMPOSITION_RECOVERY_FIXED_BYTES + uiModel.byteLength);
  return encoded;
}

export function decodeWebviewCompositionRecoverySnapshot(
  encoded: Uint8Array,
): WebviewCompositionRecoverySnapshot {
  if (
    encoded.byteLength < COMPOSITION_RECOVERY_FIXED_BYTES
    || encoded[0] !== 0x56
    || encoded[1] !== 0x57
    || encoded[2] !== 0x52
    || encoded[3] !== 0x43
    || encoded[4] !== 0x31
    || encoded[5] !== 0
    || encoded[6] !== 0
    || encoded[7] !== 0
  ) {
    throw new Error('invalid VWRC1 WebView composition recovery snapshot');
  }
  const view = new DataView(encoded.buffer, encoded.byteOffset, encoded.byteLength);
  const uiBytes = view.getUint32(8, true);
  const worldBytes = view.getUint32(12, true);
  if (
    COMPOSITION_RECOVERY_FIXED_BYTES + uiBytes + worldBytes !== encoded.byteLength
  ) {
    throw new Error('invalid VWRC1 WebView composition recovery snapshot length');
  }
  return Object.freeze({
    uiModel: encoded.slice(
      COMPOSITION_RECOVERY_FIXED_BYTES,
      COMPOSITION_RECOVERY_FIXED_BYTES + uiBytes,
    ),
    simulationWorld: encoded.slice(COMPOSITION_RECOVERY_FIXED_BYTES + uiBytes),
  });
}

export function encodeWebviewBridgeFrame(frame: WebviewBridgeFrame): Uint8Array {
  validateFrame(frame);
  const encoded = new Uint8Array(FIXED_BYTES + frame.payload.byteLength);
  encoded.set([0x56, 0x42, 0x54, 0x31], 0);
  encoded[4] = laneTag(frame.lane);
  const view = new DataView(encoded.buffer);
  view.setUint32(8, frame.session.index, true);
  view.setUint32(12, frame.session.generation, true);
  view.setBigUint64(16, frame.sessionEpoch, true);
  view.setBigUint64(24, frame.bridgeEpoch, true);
  view.setBigUint64(32, frame.sequence, true);
  view.setBigUint64(40, frame.coalesceKey, true);
  view.setUint32(48, frame.payload.byteLength, true);
  encoded.set(frame.payload, FIXED_BYTES);
  return encoded;
}

export function decodeWebviewBridgeFrame(
  encoded: Uint8Array,
  expected?: WebviewBridgeOwner,
): WebviewBridgeFrame {
  if (
    encoded.byteLength < FIXED_BYTES
    || encoded[0] !== 0x56
    || encoded[1] !== 0x42
    || encoded[2] !== 0x54
    || encoded[3] !== 0x31
  ) {
    throw new Error('invalid VBT1 WebView bridge frame');
  }
  const view = new DataView(encoded.buffer, encoded.byteOffset, encoded.byteLength);
  const payloadLength = view.getUint32(48, true);
  if (
    payloadLength > MAX_FRAME_BYTES
    || FIXED_BYTES + payloadLength !== encoded.byteLength
  ) {
    throw new Error('invalid VBT1 WebView bridge payload length');
  }
  const frame: WebviewBridgeFrame = Object.freeze({
    session: Object.freeze({
      index: view.getUint32(8, true),
      generation: view.getUint32(12, true),
    }),
    sessionEpoch: view.getBigUint64(16, true),
    bridgeEpoch: view.getBigUint64(24, true),
    sequence: view.getBigUint64(32, true),
    lane: decodeLane(encoded[4]),
    coalesceKey: view.getBigUint64(40, true),
    payload: encoded.slice(FIXED_BYTES),
  });
  validateFrame(frame);
  if (
    expected !== undefined
    && (
      frame.session.index !== expected.session.index
      || frame.session.generation !== expected.session.generation
      || frame.sessionEpoch !== expected.sessionEpoch
      || frame.bridgeEpoch !== expected.bridgeEpoch
    )
  ) {
    throw new Error('stale or foreign VBT1 WebView bridge frame');
  }
  return frame;
}

function validateFrame(frame: WebviewBridgeFrame): void {
  if (
    !Number.isInteger(frame.session.index)
    || frame.session.index < 0
    || !Number.isInteger(frame.session.generation)
    || frame.session.generation < 1
    || frame.sessionEpoch < 1n
    || frame.bridgeEpoch < 1n
    || frame.sequence < 1n
    || frame.coalesceKey < 0n
    || frame.payload.byteLength > MAX_FRAME_BYTES
  ) {
    throw new Error('invalid WebView bridge frame identity or bounds');
  }
  if (frame.coalesceKey !== 0n && frame.lane !== 'presentation' && frame.lane !== 'diagnostics') {
    throw new Error('WebView bridge coalesce key is invalid for lane');
  }
}

function laneTag(lane: WebviewBridgeLane): number {
  switch (lane) {
    case 'control': return 1;
    case 'completion': return 2;
    case 'reliable-input': return 3;
    case 'framework': return 4;
    case 'presentation': return 5;
    case 'diagnostics': return 6;
  }
}

function decodeLane(tag: number): WebviewBridgeLane {
  switch (tag) {
    case 1: return 'control';
    case 2: return 'completion';
    case 3: return 'reliable-input';
    case 4: return 'framework';
    case 5: return 'presentation';
    case 6: return 'diagnostics';
    default: throw new Error(`unknown VBT1 WebView bridge lane ${tag}`);
  }
}
