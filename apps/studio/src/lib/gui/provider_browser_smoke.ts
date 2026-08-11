import type { Backend } from '../backend/backend';
import type { RuntimeService } from '../services/runtime_service';
import { withHostBridgeSessionSync } from '../studio_wasm';
import type { FrameworkContract } from '../types';
import {
  deliverRenderBytes,
  isRendererBridgeActive,
  loadHostBridgeModule,
  startRendererBridge,
  stopRendererBridge,
  unloadHostBridgeModule,
  type VfsSnapshot,
} from './renderer_bridge';

type SmokeResult = {
  ok: boolean;
  checks: Record<string, boolean>;
  events: string[];
  error: string | null;
};

const encoder = new TextEncoder();

export async function runProviderBrowserSmoke(target: HTMLElement): Promise<void> {
  target.replaceChildren();
  const heading = document.createElement('h1');
  heading.textContent = 'Studio provider lifecycle smoke';
  const surfaceA = makeSurface('provider-smoke-surface-a');
  const surfaceB = makeSurface('provider-smoke-surface-b');
  const resultNode = document.createElement('pre');
  resultNode.dataset.testid = 'provider-smoke-result';
  resultNode.textContent = 'running';
  target.append(heading, surfaceA, surfaceB, resultNode);

  const events: string[] = [];
  (globalThis as typeof globalThis & { __voProviderSmokeEvents?: string[] }).__voProviderSmokeEvents = events;
  const checks: Record<string, boolean> = {};
  let error: string | null = null;
  try {
    await startRendererBridge('provider-smoke-canvas-a', surfaceA, backendStub(), runtimeStub(), 101, context('/a.js'), snapshot([
      ['/a.js', rendererSource('a')],
    ]));
    await startRendererBridge('provider-smoke-canvas-b', surfaceB, backendStub(), runtimeStub(), 102, context('/b.js'), snapshot([
      ['/b.js', rendererSource('b')],
    ]));
    deliverRenderBytes(101, surfaceA, new Uint8Array([1, 2]));
    deliverRenderBytes(102, surfaceB, new Uint8Array([7, 8]));
    for (
      let attempt = 0;
      attempt < 100 && (surfaceA.textContent !== 'a:1,2' || surfaceB.textContent !== 'b:7,8');
      attempt += 1
    ) {
      await new Promise((resolve) => setTimeout(resolve, 0));
    }
    checks.twoSurfaces = surfaceA.textContent === 'a:1,2' && surfaceB.textContent === 'b:7,8';
    checks.twoProvidersActive = isRendererBridgeActive(101) && isRendererBridgeActive(102);

    const vfs = window as unknown as {
      _vfsWriteFile(path: string, bytes: Uint8Array, mode: number): string | null;
      _vfsReadFile(path: string): [Uint8Array | null, string | null];
    };
    withHostBridgeSessionSync(101, () => {
      if (vfs._vfsWriteFile('/session-state', encoder.encode('alpha'), 0o600)) {
        throw new Error('session A VFS write failed');
      }
    });
    withHostBridgeSessionSync(102, () => {
      if (vfs._vfsWriteFile('/session-state', encoder.encode('beta'), 0o600)) {
        throw new Error('session B VFS write failed');
      }
    });
    const stateA = withHostBridgeSessionSync(101, () => vfs._vfsReadFile('/session-state'));
    const stateB = withHostBridgeSessionSync(102, () => vfs._vfsReadFile('/session-state'));
    checks.sessionVfsPersistsAcrossSwitches = new TextDecoder().decode(stateA[0] ?? undefined) === 'alpha'
      && stateA[1] === null
      && new TextDecoder().decode(stateB[0] ?? undefined) === 'beta'
      && stateB[1] === null;

    const cancelledStartup = new AbortController();
    const pendingStartup = startRendererBridge(
      'provider-smoke-canvas-cancel',
      surfaceA,
      backendStub(),
      runtimeStub(),
      104,
      context('/cancel.js'),
      snapshot([['/cancel.js', hangingRendererSource()]]),
      cancelledStartup.signal,
    );
    for (let attempt = 0; attempt < 100 && !events.includes('cancel:init'); attempt += 1) {
      await new Promise((resolve) => setTimeout(resolve, 0));
    }
    if (!events.includes('cancel:init')) throw new Error('cancel smoke renderer did not begin init');
    cancelledStartup.abort(new Error('injected startup cancellation'));
    let cancellationObserved = false;
    try {
      await pendingStartup;
    } catch (providerError) {
      cancellationObserved = providerError instanceof Error
        && providerError.message === 'injected startup cancellation';
    }
    checks.startupCancellationRollsBack = cancellationObserved
      && events.includes('cancel:destroyWidgets')
      && events.includes('cancel:stop')
      && !isRendererBridgeActive(104);

    await startRendererBridge(
      'provider-smoke-canvas-render-cancel',
      surfaceA,
      backendStub(),
      runtimeStub(),
      105,
      context('/render-cancel.js'),
      snapshot([['/render-cancel.js', hangingRenderSource()]]),
    );
    deliverRenderBytes(105, surfaceA, new Uint8Array([5]));
    for (let attempt = 0; attempt < 100 && !events.includes('render-cancel:render'); attempt += 1) {
      await new Promise((resolve) => setTimeout(resolve, 0));
    }
    if (!events.includes('render-cancel:render')) throw new Error('cancel smoke renderer did not begin render');
    stopRendererBridge(105);
    for (let attempt = 0; attempt < 100 && !events.includes('render-cancel:stop'); attempt += 1) {
      await new Promise((resolve) => setTimeout(resolve, 0));
    }
    checks.activeRenderCancellationDrains = events.includes('render-cancel:destroyWidgets')
      && events.includes('render-cancel:stop')
      && !isRendererBridgeActive(105);

    let injectedFailure = false;
    try {
      await startRendererBridge('provider-smoke-canvas-fault', surfaceA, backendStub(), runtimeStub(), 103, context('/fault.js'), snapshot([
        ['/fault.js', failingRendererSource()],
      ]));
    } catch (providerError) {
      injectedFailure = providerError instanceof Error && providerError.message === 'injected provider init failure';
    }
    await Promise.resolve();
    checks.failureObserved = injectedFailure;
    checks.failureRolledBack = events.includes('fault:destroyWidgets')
      && events.includes('fault:stop')
      && !isRendererBridgeActive(103);
    checks.peersSurviveFailure = isRendererBridgeActive(101) && isRendererBridgeActive(102);

    stopRendererBridge(101);
    await Promise.resolve();
    deliverRenderBytes(102, surfaceB, new Uint8Array([9]));
    for (let attempt = 0; attempt < 100 && surfaceB.textContent !== 'b:9'; attempt += 1) {
      await new Promise((resolve) => setTimeout(resolve, 0));
    }
    checks.singleStopIsolated = !isRendererBridgeActive(101)
      && isRendererBridgeActive(102)
      && surfaceB.textContent === 'b:9';

    const bridgeSnapshotA = snapshot([['/host-a.js', hostBridgeSource(201)]]);
    const bridgeSnapshotB = snapshot([['/host-b.js', hostBridgeSource(202)]]);
    const bridgeA = await loadHostBridgeModule(201, '/host-a.js', '/smoke.vo', bridgeSnapshotA.files);
    const bridgeB = await loadHostBridgeModule(202, '/host-b.js', '/smoke.vo', bridgeSnapshotB.files);
    const bridgeValue = (bridge: typeof bridgeA): number => (
      bridge.buildImports({} as never).provider_smoke?.() as number
    );
    checks.hostBridgeSessionsIsolated = bridgeValue(bridgeA) === 201 && bridgeValue(bridgeB) === 202;
    let hostBridgeFailure = false;
    try {
      const faultSnapshot = snapshot([['/host-fault.js', `throw new Error('injected host bridge evaluation failure');`]]);
      await loadHostBridgeModule(203, '/host-fault.js', '/smoke.vo', faultSnapshot.files);
    } catch (providerError) {
      hostBridgeFailure = providerError instanceof Error
        && providerError.message === 'injected host bridge evaluation failure';
    }
    const recoverySnapshot = snapshot([['/host-recovered.js', hostBridgeSource(203)]]);
    const recoveredBridge = await loadHostBridgeModule(
      203,
      '/host-recovered.js',
      '/smoke.vo',
      recoverySnapshot.files,
    );
    checks.hostBridgeFailureRecovery = hostBridgeFailure && bridgeValue(recoveredBridge) === 203;
    unloadHostBridgeModule(201);
    const cachedPeer = await loadHostBridgeModule(
      202,
      '/host-b.js',
      '/smoke.vo',
      bridgeSnapshotB.files,
    );
    checks.hostBridgeUnloadIsolated = cachedPeer === bridgeB && bridgeValue(cachedPeer) === 202;
  } catch (smokeError) {
    error = smokeError instanceof Error ? smokeError.message : String(smokeError);
  } finally {
    stopRendererBridge(101);
    stopRendererBridge(102);
    stopRendererBridge(103);
    stopRendererBridge(104);
    stopRendererBridge(105);
    unloadHostBridgeModule(201);
    unloadHostBridgeModule(202);
    unloadHostBridgeModule(203);
    await Promise.resolve();
  }
  const ok = error === null && Object.values(checks).length === 12 && Object.values(checks).every(Boolean);
  const result: SmokeResult = { ok, checks, events: [...events], error };
  resultNode.dataset.status = ok ? 'passed' : 'failed';
  resultNode.textContent = JSON.stringify(result, null, 2);
}

function makeSurface(testId: string): HTMLElement {
  const surface = document.createElement('section');
  surface.dataset.testid = testId;
  return surface;
}

function context(rendererPath: string) {
  const framework: FrameworkContract = {
    moduleKey: `github.com/vo-lang/smoke${rendererPath}`,
    name: `smoke:${rendererPath}`,
    entry: null,
    providerRole: 'diagnostics',
    providerRoles: ['diagnostics'],
    capabilities: [],
    roles: ['renderer'],
    jsModules: { renderer: rendererPath },
  };
  return {
    moduleBytes: new Uint8Array([0]),
    entryPath: '/smoke.vo',
    framework,
    providerFrameworks: [],
  };
}

function snapshot(files: Array<[string, string]>): VfsSnapshot {
  return {
    rootPath: '/',
    files: files.map(([path, source]) => ({ path, bytes: encoder.encode(source) })),
  };
}

function rendererSource(label: string): string {
  return `
const events = globalThis.__voProviderSmokeEvents;
export default {
  async init() { events.push('${label}:init'); },
  render(container, bytes) {
    events.push('${label}:render');
    container.textContent = '${label}:' + Array.from(bytes).join(',');
  },
  destroyWidgets() { events.push('${label}:destroyWidgets'); },
  stop() { events.push('${label}:stop'); },
};`;
}

function failingRendererSource(): string {
  return `
const events = globalThis.__voProviderSmokeEvents;
export default {
  async init() { events.push('fault:init'); throw new Error('injected provider init failure'); },
  render() {},
  destroyWidgets() { events.push('fault:destroyWidgets'); },
  stop() { events.push('fault:stop'); },
};`;
}

function hangingRendererSource(): string {
  return `
const events = globalThis.__voProviderSmokeEvents;
export default {
  async init() { events.push('cancel:init'); await new Promise(() => {}); },
  render() {},
  destroyWidgets() { events.push('cancel:destroyWidgets'); },
  stop() { events.push('cancel:stop'); },
};`;
}

function hangingRenderSource(): string {
  return `
const events = globalThis.__voProviderSmokeEvents;
export default {
  async init() { events.push('render-cancel:init'); },
  async render() { events.push('render-cancel:render'); await new Promise(() => {}); },
  destroyWidgets() { events.push('render-cancel:destroyWidgets'); },
  stop() { events.push('render-cancel:stop'); },
};`;
}

function hostBridgeSource(value: number): string {
  return `export function buildImports() { return { provider_smoke() { return ${value}; } }; }`;
}

function backendStub(): Backend {
  return { platform: 'web' } as unknown as Backend;
}

function runtimeStub(): RuntimeService {
  return {
    loadFrameworkProvider: async () => {},
    unloadFrameworkProvider: async () => {},
    beginFrameworkProvider: async () => {},
    readyFrameworkProvider: async () => {},
    abortFrameworkProvider: async () => {},
    closeFrameworkProvider: async () => {},
  } as unknown as RuntimeService;
}
