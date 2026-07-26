import { invoke } from '../tauri';
import {
  decodeWebviewBridgeFrame,
  decodeWebviewCompositionRecoverySnapshot,
  encodeWebviewBridgeFrame,
  encodeWebviewCompositionRecoverySnapshot,
  WEBVIEW_COMPOSITION_RECOVERY_SNAPSHOT_KEY,
  type WebviewBridgeOwner,
} from './webview_transport';

type SmokeOwnerWire = Readonly<{
  sessionIndex: number;
  sessionGeneration: number;
  sessionEpoch: string;
  bridgeEpoch: string;
}>;

type SmokeRestart = Readonly<{
  oldEpoch: string;
  newEpoch: string;
  discardedToWebview: number;
  discardedFromWebview: number;
}>;

const encoder = new TextEncoder();
const decoder = new TextDecoder();

function ownerFromWire(wire: SmokeOwnerWire): WebviewBridgeOwner {
  return Object.freeze({
    session: Object.freeze({
      index: wire.sessionIndex,
      generation: wire.sessionGeneration,
    }),
    sessionEpoch: BigInt(wire.sessionEpoch),
    bridgeEpoch: BigInt(wire.bridgeEpoch),
  });
}

async function poll(owner: WebviewBridgeOwner) {
  const raw = await invoke<ArrayBuffer>('cmd_webview_native_smoke_poll');
  const encoded = new Uint8Array(raw);
  if (encoded.byteLength === 0) throw new Error('native bridge returned no frame');
  return decodeWebviewBridgeFrame(encoded, owner);
}

async function take(owner: WebviewBridgeOwner) {
  const raw = await invoke<ArrayBuffer>('cmd_webview_native_smoke_take');
  const encoded = new Uint8Array(raw);
  if (encoded.byteLength === 0) throw new Error('native bridge retained no WebView frame');
  return decodeWebviewBridgeFrame(encoded, owner);
}

async function finish(report: unknown): Promise<void> {
  await invoke('cmd_webview_native_smoke_finish', { report });
}

export async function runWebviewNativeSmoke(target: HTMLElement): Promise<void> {
  const phase = new URLSearchParams(window.location.search).get('phase');
  target.textContent = `Studio native WebView composition smoke: ${phase ?? 'unknown'}`;
  try {
    if (phase === 'initial') {
      await runInitialPhase();
      return;
    }
    if (phase === 'recover') {
      await runRecoveryPhase();
      return;
    }
    throw new Error(`unknown WebView native smoke phase ${String(phase)}`);
  } catch (error) {
    await finish({
      schemaVersion: 1,
      test: 'studio-webview-native-composition',
      result: 'failed',
      phase,
      error: error instanceof Error ? error.message : String(error),
      userAgent: navigator.userAgent,
      platform: navigator.platform,
    });
  }
}

async function runInitialPhase(): Promise<void> {
  const wire = await invoke<SmokeOwnerWire>('cmd_webview_native_smoke_begin');
  const owner = ownerFromWire(wire);
  const nativeFrame = await poll(owner);
  if (
    nativeFrame.lane !== 'control'
    || decoder.decode(nativeFrame.payload) !== 'native-ready'
  ) {
    throw new Error('native-to-WebView bridge payload mismatch');
  }

  const webFrame = encodeWebviewBridgeFrame({
    ...owner,
    sequence: 1n,
    lane: 'reliable-input',
    coalesceKey: 0n,
    payload: encoder.encode('webview-input'),
  });
  await invoke('cmd_webview_native_smoke_submit', { frame: [...webFrame] });
  const retained = await take(owner);
  if (decoder.decode(retained.payload) !== 'webview-input') {
    throw new Error('WebView-to-native bridge payload mismatch');
  }

  const recovery = encodeWebviewCompositionRecoverySnapshot(
    encoder.encode('ui-model-revision-17'),
    encoder.encode('simulation-world-tick-9001'),
  );
  const restart = await invoke<SmokeRestart>('cmd_webview_native_smoke_restart', {
    snapshots: [{
      key: WEBVIEW_COMPOSITION_RECOVERY_SNAPSHOT_KEY.toString(),
      payload: [...recovery],
    }],
  });
  if (BigInt(restart.newEpoch) !== owner.bridgeEpoch + 1n) {
    throw new Error('WebView bridge epoch did not advance exactly once');
  }
  await invoke('cmd_webview_native_smoke_replace');
}

async function runRecoveryPhase(): Promise<void> {
  const wire = await invoke<SmokeOwnerWire>('cmd_webview_native_smoke_owner');
  const owner = ownerFromWire(wire);
  if (owner.bridgeEpoch < 2n) throw new Error('replacement WebView observed the initial epoch');
  await invoke('cmd_webview_native_smoke_attach', {
    bridgeEpoch: owner.bridgeEpoch.toString(),
  });

  const recoveryFrame = await poll(owner);
  if (
    recoveryFrame.lane !== 'presentation'
    || recoveryFrame.coalesceKey !== WEBVIEW_COMPOSITION_RECOVERY_SNAPSHOT_KEY
  ) {
    throw new Error('replacement WebView did not receive the composition recovery snapshot');
  }
  const recovery = decodeWebviewCompositionRecoverySnapshot(recoveryFrame.payload);
  if (
    decoder.decode(recovery.uiModel) !== 'ui-model-revision-17'
    || decoder.decode(recovery.simulationWorld) !== 'simulation-world-tick-9001'
  ) {
    throw new Error('composition recovery snapshot content mismatch');
  }

  const staleFrame = encodeWebviewBridgeFrame({
    ...owner,
    bridgeEpoch: owner.bridgeEpoch - 1n,
    sequence: 2n,
    lane: 'reliable-input',
    coalesceKey: 0n,
    payload: encoder.encode('stale-input'),
  });
  let staleRejected = false;
  try {
    await invoke('cmd_webview_native_smoke_submit', { frame: [...staleFrame] });
  } catch {
    staleRejected = true;
  }
  if (!staleRejected) throw new Error('replacement WebView accepted a stale bridge epoch');

  const recoveredFrame = encodeWebviewBridgeFrame({
    ...owner,
    sequence: 1n,
    lane: 'completion',
    coalesceKey: 0n,
    payload: encoder.encode('recovery-complete'),
  });
  await invoke('cmd_webview_native_smoke_submit', { frame: [...recoveredFrame] });
  const retained = await take(owner);
  if (decoder.decode(retained.payload) !== 'recovery-complete') {
    throw new Error('replacement WebView completion did not reach native');
  }

  await finish({
    schemaVersion: 1,
    test: 'studio-webview-native-composition',
    result: 'passed',
    topology: 'webview-native-host',
    initialWindowLabel: 'main',
    recoveryWindowLabel: 'webview-native-smoke-recovery',
    nativeToWebview: true,
    webviewToNative: true,
    staleEpochRejected: true,
    uiModelRecovered: true,
    simulationWorldRecovered: true,
    bridgeEpoch: owner.bridgeEpoch.toString(),
    userAgent: navigator.userAgent,
    platform: navigator.platform,
  });
}
