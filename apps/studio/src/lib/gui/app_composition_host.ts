import {
  subscribeBrowserPlatformEvents,
  type BrowserPlatformEvent,
} from './platform_host';

export type AppHandle = Readonly<{
  index: number;
  generation: number;
}>;

export type AppSurfaceIdentity = Readonly<{
  sessionId: number;
  session: AppHandle;
  sessionEpoch: bigint;
  window: AppHandle;
  view: AppHandle;
  surface: AppHandle;
}>;

export type AppSurfaceKind = 'dom' | 'canvas';
export type AppSurfaceInputPolicy = 'opaque' | 'transparent' | 'passthrough';
export type AppSurfaceHitPolicy = 'opaque' | 'transparent';

export type AppSurfaceHitRegion = Readonly<{
  xMilli: number;
  yMilli: number;
  widthMilli: number;
  heightMilli: number;
  input: AppSurfaceHitPolicy;
}>;

export type AppSystemShortcut = Readonly<{
  classMask: bigint;
  physical: string;
  alt: boolean;
  control: boolean;
  meta: boolean;
  shift: boolean;
  scope: 'view' | 'window' | 'session';
  priority: number;
}>;

export type AppCompositionTraceEntry = Readonly<{
  sequence: bigint;
  timestampMicros: bigint;
  action:
    | 'attach'
    | 'detach'
    | 'hitRegions'
    | 'suspend'
    | 'resume'
    | 'pointerCapture'
    | 'pointerRelease'
    | 'focus'
    | 'shortcut'
    | 'pointer'
    | 'wheel'
    | 'key'
    | 'text'
    | 'composition';
  surface: AppSurfaceIdentity;
  detail: string;
}>;

export type AppSurfaceDescriptor = Readonly<{
  identity: AppSurfaceIdentity;
  kind: AppSurfaceKind;
  layer: number;
  input: AppSurfaceInputPolicy;
  label: string;
}>;

export type AppSurfaceMetrics = Readonly<{
  cssWidth: number;
  cssHeight: number;
  deviceWidth: number;
  deviceHeight: number;
  scaleNumerator: number;
  scaleDenominator: number;
}>;

export type AppSurfaceInputEvent =
  | Readonly<{
    type: 'pointerDown' | 'pointerMove' | 'pointerUp' | 'pointerCancel';
    sequence: bigint;
    timestampMicros: bigint;
    surface: AppSurfaceIdentity;
    pointerId: number;
    pointerType: string;
    xMilli: number;
    yMilli: number;
    localXMilli: number;
    localYMilli: number;
    movementXMilli: number;
    movementYMilli: number;
    button: number;
    buttons: number;
    pressureQ16: number;
    tiltX: number;
    tiltY: number;
    alt: boolean;
    control: boolean;
    meta: boolean;
    shift: boolean;
    synthesized: boolean;
  }>
  | Readonly<{
    type: 'wheel';
    sequence: bigint;
    timestampMicros: bigint;
    surface: AppSurfaceIdentity;
    xMilli: number;
    yMilli: number;
    localXMilli: number;
    localYMilli: number;
    deltaXMilli: number;
    deltaYMilli: number;
    deltaZMilli: number;
    deltaMode: number;
    alt: boolean;
    control: boolean;
    meta: boolean;
    shift: boolean;
  }>
  | Readonly<{
    type: 'keyDown' | 'keyUp';
    sequence: bigint;
    timestampMicros: bigint;
    surface: AppSurfaceIdentity;
    physical: string;
    logical: string;
    repeat: boolean;
    alt: boolean;
    control: boolean;
    meta: boolean;
    shift: boolean;
    synthesized: boolean;
  }>
  | Readonly<{
    type: 'text' | 'compositionStart' | 'compositionUpdate' | 'compositionEnd';
    sequence: bigint;
    timestampMicros: bigint;
    surface: AppSurfaceIdentity;
    text: string;
    inputType: string;
    composing: boolean;
    synthesized: boolean;
  }>
  | Readonly<{
    type: 'focus';
    sequence: bigint;
    timestampMicros: bigint;
    surface: AppSurfaceIdentity;
    focused: boolean;
    synthesized: boolean;
  }>;

export type AppSurfaceInputSink = (event: AppSurfaceInputEvent) => void;

export interface AppSurfaceLease {
  readonly descriptor: AppSurfaceDescriptor;
  readonly element: HTMLDivElement | HTMLCanvasElement;
  metrics(): AppSurfaceMetrics;
  release(): void;
}

type SurfaceRecord = {
  readonly owner: string;
  readonly key: string;
  readonly windowKey: string;
  readonly viewKey: string;
  readonly descriptor: AppSurfaceDescriptor;
  readonly element: HTMLDivElement | HTMLCanvasElement;
  readonly lease: AppSurfaceLease;
};

type ViewHostRecord = {
  readonly key: string;
  readonly windowKey: string;
  readonly element: HTMLDivElement;
  readonly surfaces: Set<string>;
};

type WindowHostRecord = {
  readonly key: string;
  readonly element: HTMLDivElement;
  readonly views: Set<string>;
};

type HitRegionRecord = {
  readonly revision: bigint;
  readonly regions: readonly AppSurfaceHitRegion[];
};

type HitRecord = {
  readonly record: SurfaceRecord;
  readonly input: AppSurfaceHitPolicy;
};

type ShortcutRegistrationRecord = {
  readonly key: string;
  readonly scope: AppSystemShortcut['scope'];
  readonly priority: number;
};

export type AppAssetHandle = Readonly<{ index: number; generation: number }>;

type AssetBufferRecord = {
  readonly owner: string;
  readonly bytes: Uint8Array;
};

const MAX_FRAMEWORK_OWNER_BYTES = 512;
const MAX_SURFACE_LABEL_BYTES = 1024;
const MAX_HIT_REGIONS_PER_SURFACE = 16_384;
const MAX_SYSTEM_SHORTCUTS = 1_024;
const MAX_COMPOSITION_TRACE_ENTRIES = 4_096;
const encoder = new TextEncoder();

export class AppCompositionHost {
  readonly #sessionId: number;
  readonly #maxSurfaces: number;
  readonly #records = new Map<string, SurfaceRecord>();
  readonly #windowHosts = new Map<string, WindowHostRecord>();
  readonly #viewHosts = new Map<string, ViewHostRecord>();
  readonly #keysByOwner = new Map<string, Set<string>>();
  readonly #inputSinks = new Map<string, AppSurfaceInputSink>();
  readonly #pointerCapture = new Map<number, string>();
  readonly #pressedKeys = new Map<string, Set<string>>();
  readonly #hitRegions = new Map<string, HitRegionRecord>();
  readonly #suspended = new Set<string>();
  readonly #lowerSuspensionClaims = new Map<string, Set<string>>();
  readonly #modalOverlays = new Set<string>();
  readonly #shortcutOwners = new Map<string, ShortcutRegistrationRecord[]>();
  readonly #shortcutsBySurface = new Map<string, Set<string>>();
  readonly #trace: AppCompositionTraceEntry[] = [];
  readonly #assetBuffers = new Map<string, AssetBufferRecord>();
  #assetBufferBytes = 0;
  readonly #abort = new AbortController();
  readonly #unsubscribePlatformEvents: () => void;
  #root: HTMLElement;
  #focusedKey: string | null = null;
  #fullscreenWindowKey: string | null = null;
  #nextInputSequence = 1n;
  #closed = false;

  constructor(sessionId: number, root: HTMLElement, maxSurfaces = 256) {
    if (!Number.isSafeInteger(sessionId) || sessionId < 1) {
      throw new Error('composition session ID must be a positive safe integer');
    }
    if (!Number.isSafeInteger(maxSurfaces) || maxSurfaces < 1) {
      throw new Error('composition surface capacity must be positive');
    }
    this.#sessionId = sessionId;
    this.#root = root;
    this.#maxSurfaces = maxSurfaces;
    configureRoot(root);
    this.#installInputCapture();
    this.#unsubscribePlatformEvents = subscribeBrowserPlatformEvents(
      (event) => this.#applyBrowserPlatformEvent(event),
    );
  }

  get sessionId(): number {
    return this.#sessionId;
  }

  attach(owner: string, descriptor: AppSurfaceDescriptor): AppSurfaceLease {
    this.#assertOpen();
    validateOwner(owner);
    validateDescriptor(this.#sessionId, descriptor);
    const key = surfaceKey(descriptor.identity);
    if (this.#records.has(key)) {
      throw new Error('App Surface is already attached');
    }
    if (this.#records.size >= this.#maxSurfaces) {
      throw new Error('App Surface capacity exceeded');
    }
    const element = descriptor.kind === 'canvas'
      ? document.createElement('canvas')
      : document.createElement('div');
    configureSurfaceElement(element, descriptor);
    const windowKey = appWindowKey(descriptor.identity);
    const viewKey = appViewKey(descriptor.identity);
    const viewHost = this.#ensureViewHost(descriptor.identity, windowKey, viewKey);
    const lease: AppSurfaceLease = Object.freeze({
      descriptor,
      element,
      metrics: () => this.#metrics(key),
      release: () => this.#release(owner, key),
    });
    const record: SurfaceRecord = {
      owner,
      key,
      windowKey,
      viewKey,
      descriptor,
      element,
      lease,
    };
    this.#records.set(key, record);
    viewHost.surfaces.add(key);
    let ownerKeys = this.#keysByOwner.get(owner);
    if (!ownerKeys) {
      ownerKeys = new Set();
      this.#keysByOwner.set(owner, ownerKeys);
    }
    ownerKeys.add(key);
    for (const overlayKey of this.#modalOverlays) {
      const overlay = this.#records.get(overlayKey);
      if (
        overlay !== undefined
        && sameHandle(
          overlay.descriptor.identity.window,
          record.descriptor.identity.window,
        )
        && sameHandle(
          overlay.descriptor.identity.view,
          record.descriptor.identity.view,
        )
        && record.descriptor.layer < overlay.descriptor.layer
      ) {
        let claimants = this.#lowerSuspensionClaims.get(record.key);
        if (claimants === undefined) {
          claimants = new Set();
          this.#lowerSuspensionClaims.set(record.key, claimants);
        }
        claimants.add(overlayKey);
        record.element.dataset.appSurfaceSuspended = 'true';
      }
    }
    viewHost.element.appendChild(element);
    this.#sortViewLayers(viewKey);
    this.#recordTrace('attach', record, descriptor.kind);
    return lease;
  }

  lookup(identity: AppSurfaceIdentity): AppSurfaceLease | null {
    this.#assertOpen();
    validateIdentity(this.#sessionId, identity);
    return this.#records.get(surfaceKey(identity))?.lease ?? null;
  }

  layers(): readonly AppSurfaceLease[] {
    this.#assertOpen();
    return Object.freeze(this.#orderedRecords().map((record) => record.lease));
  }

  pointerHitStack(clientX: number, clientY: number): readonly AppSurfaceIdentity[] {
    this.#assertOpen();
    if (!Number.isFinite(clientX) || !Number.isFinite(clientY)) {
      throw new Error('pointer coordinates must be finite');
    }
    return Object.freeze(
      this.#hitRecords(clientX, clientY).map(({ record }) => record.descriptor.identity),
    );
  }

  publishHitRegions(
    owner: string,
    identity: AppSurfaceIdentity,
    revision: bigint,
    regions: readonly AppSurfaceHitRegion[],
  ): void {
    this.#assertOpen();
    const record = this.#ownedRecord(owner, identity);
    if (revision <= 0n || regions.length > MAX_HIT_REGIONS_PER_SURFACE) {
      throw new Error('App Surface hit-region publication is invalid');
    }
    const previous = this.#hitRegions.get(record.key);
    if (previous !== undefined && revision <= previous.revision) {
      throw new Error('App Surface hit-region revision must advance');
    }
    const owned = regions.map((region) => {
      validateHitRegion(region);
      return Object.freeze({ ...region });
    });
    this.#hitRegions.set(record.key, {
      revision,
      regions: Object.freeze(owned),
    });
    this.#recordTrace('hitRegions', record, `${revision}:${owned.length}`);
  }

  setInputSuspended(
    owner: string,
    identity: AppSurfaceIdentity,
    suspended: boolean,
  ): void {
    this.#assertOpen();
    const record = this.#ownedRecord(owner, identity);
    if (suspended === this.#suspended.has(record.key)) return;
    if (suspended) {
      this.#releaseSurfaceInput(record);
      this.#suspended.add(record.key);
      record.element.dataset.appSurfaceSuspended = 'true';
      this.#recordTrace('suspend', record, '');
      this.#restoreViewFocus(
        record.descriptor.identity.window,
        record.descriptor.identity.view,
      );
      return;
    }
    this.#suspended.delete(record.key);
    if (!this.#isSuspended(record.key)) delete record.element.dataset.appSurfaceSuspended;
    this.#recordTrace('resume', record, '');
  }

  setLowerInputSuspended(
    owner: string,
    overlay: AppSurfaceIdentity,
    suspended: boolean,
  ): void {
    this.#assertOpen();
    const record = this.#ownedRecord(owner, overlay);
    if (record.descriptor.input === 'passthrough') {
      throw new Error('passthrough Surface cannot suspend lower input');
    }
    if (suspended === this.#modalOverlays.has(record.key)) return;
    this.#applyLowerSuspension(record, suspended);
  }

  reserveSystemShortcuts(
    owner: string,
    identity: AppSurfaceIdentity,
    shortcuts: readonly AppSystemShortcut[],
  ): void {
    this.#assertOpen();
    const record = this.#ownedRecord(owner, identity);
    if (shortcuts.length > MAX_SYSTEM_SHORTCUTS) {
      throw new Error('App Surface system-shortcut capacity exceeded');
    }
    this.#clearSurfaceShortcuts(record.key);
    const owned = new Set<string>();
    for (const shortcut of shortcuts) {
      validateSystemShortcut(shortcut);
      const signature = shortcutSignature(shortcut);
      const registrations = this.#shortcutOwners.get(signature) ?? [];
      if (registrations.some((registration) => registration.key === record.key)) {
        throw new Error('App system shortcut is duplicated for one Surface');
      }
      registrations.push({
        key: record.key,
        scope: shortcut.scope,
        priority: shortcut.priority,
      });
      this.#shortcutOwners.set(signature, registrations);
      owned.add(signature);
    }
    if (owned.size > 0) this.#shortcutsBySurface.set(record.key, owned);
    this.#recordTrace('shortcut', record, String(owned.size));
  }

  traceSnapshot(): readonly AppCompositionTraceEntry[] {
    this.#assertOpen();
    return Object.freeze(this.#trace.slice());
  }

  subscribeInput(owner: string, sink: AppSurfaceInputSink): () => void {
    this.#assertOpen();
    validateOwner(owner);
    if (this.#inputSinks.has(owner)) {
      throw new Error('framework Surface input sink is already registered');
    }
    this.#inputSinks.set(owner, sink);
    let active = true;
    return () => {
      if (!active) return;
      active = false;
      if (this.#inputSinks.get(owner) === sink) {
        this.#releaseOwnerInput(owner);
        this.#inputSinks.delete(owner);
      }
    };
  }

  capturePointer(owner: string, pointerId: number, identity: AppSurfaceIdentity): void {
    this.#assertOpen();
    validateOwner(owner);
    validatePointerId(pointerId);
    const key = surfaceKey(identity);
    const record = this.#records.get(key);
    if (
      record?.owner !== owner
      || record.descriptor.input === 'passthrough'
      || this.#isSuspended(key)
    ) {
      throw new Error('pointer capture Surface owner mismatch');
    }
    this.#pointerCapture.set(pointerId, key);
    this.#recordTrace('pointerCapture', record, String(pointerId));
    try {
      this.#root.setPointerCapture(pointerId);
    } catch {
      // Browser capture can reject after the native pointer has already ended.
    }
  }

  releasePointer(owner: string, pointerId: number): void {
    this.#assertOpen();
    validateOwner(owner);
    validatePointerId(pointerId);
    const key = this.#pointerCapture.get(pointerId);
    if (key === undefined) return;
    if (this.#records.get(key)?.owner !== owner) {
      throw new Error('pointer capture owner mismatch');
    }
    this.#pointerCapture.delete(pointerId);
    const record = this.#records.get(key);
    if (record !== undefined) this.#recordTrace('pointerRelease', record, String(pointerId));
    if (this.#root.hasPointerCapture(pointerId)) {
      this.#root.releasePointerCapture(pointerId);
    }
  }

  focus(owner: string, identity: AppSurfaceIdentity): void {
    this.#assertOpen();
    validateOwner(owner);
    const key = surfaceKey(identity);
    const record = this.#records.get(key);
    if (
      record?.owner !== owner
      || record.descriptor.input === 'passthrough'
      || this.#isSuspended(key)
    ) {
      throw new Error('focused Surface owner mismatch');
    }
    this.#setFocusedKey(key, false);
  }

  publishAssetBuffer(owner: string, asset: AppAssetHandle, bytes: Uint8Array): void {
    this.#assertOpen();
    validateOwner(owner);
    validateAssetHandle(asset);
    if (bytes.byteLength === 0 || bytes.byteLength > 256 * 1024 * 1024) {
      throw new Error('App asset buffer size is invalid');
    }
    const key = assetBufferKey(asset);
    const previous = this.#assetBuffers.get(key);
    if (previous !== undefined && previous.owner !== owner) {
      throw new Error('App asset buffer owner mismatch');
    }
    const nextBytes = this.#assetBufferBytes
      - (previous?.bytes.byteLength ?? 0)
      + bytes.byteLength;
    if (nextBytes > 512 * 1024 * 1024) {
      throw new Error('App asset buffer budget exceeded');
    }
    this.#assetBuffers.set(key, { owner, bytes: new Uint8Array(bytes) });
    this.#assetBufferBytes = nextBytes;
  }

  readAssetBuffer(asset: AppAssetHandle): ArrayBuffer {
    this.#assertOpen();
    validateAssetHandle(asset);
    const record = this.#assetBuffers.get(assetBufferKey(asset));
    if (record === undefined) throw new Error('App asset buffer is unavailable');
    return record.bytes.slice().buffer;
  }

  releaseAssetBuffer(owner: string, asset: AppAssetHandle): void {
    if (this.#closed) return;
    validateOwner(owner);
    validateAssetHandle(asset);
    const key = assetBufferKey(asset);
    const record = this.#assetBuffers.get(key);
    if (record === undefined) return;
    if (record.owner !== owner) throw new Error('App asset buffer owner mismatch');
    this.#assetBufferBytes -= record.bytes.byteLength;
    this.#assetBuffers.delete(key);
  }

  reparent(root: HTMLElement): void {
    this.#assertOpen();
    if (root === this.#root) return;
    configureRoot(root);
    this.#root = root;
    for (const window of this.#orderedWindowHosts()) {
      root.appendChild(window.element);
    }
    this.#layoutWindowHosts();
  }

  closeOwner(owner: string): void {
    if (this.#closed) return;
    this.#releaseOwnerInput(owner);
    this.#inputSinks.delete(owner);
    const keys = this.#keysByOwner.get(owner);
    if (keys) {
      for (const key of [...keys]) {
        this.#release(owner, key);
      }
    }
    for (const [key, record] of this.#assetBuffers) {
      if (record.owner !== owner) continue;
      this.#assetBufferBytes -= record.bytes.byteLength;
      this.#assetBuffers.delete(key);
    }
  }

  close(): void {
    if (this.#closed) return;
    for (const owner of [...this.#inputSinks.keys()]) this.#releaseOwnerInput(owner);
    this.#abort.abort();
    this.#unsubscribePlatformEvents();
    for (const record of this.#records.values()) {
      record.element.remove();
    }
    for (const window of this.#windowHosts.values()) {
      window.element.remove();
    }
    this.#records.clear();
    this.#viewHosts.clear();
    this.#windowHosts.clear();
    this.#keysByOwner.clear();
    this.#inputSinks.clear();
    this.#pointerCapture.clear();
    this.#pressedKeys.clear();
    this.#hitRegions.clear();
    this.#suspended.clear();
    this.#lowerSuspensionClaims.clear();
    this.#modalOverlays.clear();
    this.#shortcutOwners.clear();
    this.#shortcutsBySurface.clear();
    this.#trace.length = 0;
    this.#assetBuffers.clear();
    this.#assetBufferBytes = 0;
    this.#focusedKey = null;
    this.#fullscreenWindowKey = null;
    this.#closed = true;
  }

  #metrics(key: string): AppSurfaceMetrics {
    this.#assertOpen();
    const record = this.#records.get(key);
    if (!record) throw new Error('App Surface lease is closed');
    const rect = record.element.getBoundingClientRect();
    const ratio = window.devicePixelRatio;
    const scaleNumerator = Number.isFinite(ratio) && ratio > 0
      ? Math.max(1, Math.round(ratio * 1_000_000))
      : 1_000_000;
    const scaleDenominator = 1_000_000;
    return Object.freeze({
      cssWidth: Math.max(0, rect.width),
      cssHeight: Math.max(0, rect.height),
      deviceWidth: Math.max(1, Math.round(rect.width * scaleNumerator / scaleDenominator)),
      deviceHeight: Math.max(1, Math.round(rect.height * scaleNumerator / scaleDenominator)),
      scaleNumerator,
      scaleDenominator,
    });
  }

  #release(owner: string, key: string): void {
    if (this.#closed) return;
    const record = this.#records.get(key);
    if (!record) return;
    if (record.owner !== owner) throw new Error('App Surface owner mismatch');
    const window = record.descriptor.identity.window;
    const view = record.descriptor.identity.view;
    this.#releaseSurfaceInput(record);
    if (this.#modalOverlays.has(key)) this.#applyLowerSuspension(record, false);
    this.#hitRegions.delete(key);
    this.#suspended.delete(key);
    this.#lowerSuspensionClaims.delete(key);
    for (const claimants of this.#lowerSuspensionClaims.values()) claimants.delete(key);
    this.#clearSurfaceShortcuts(key);
    this.#recordTrace('detach', record, '');
    record.element.remove();
    this.#records.delete(key);
    const viewHost = this.#viewHosts.get(record.viewKey);
    viewHost?.surfaces.delete(key);
    if (viewHost?.surfaces.size === 0) {
      viewHost.element.remove();
      this.#viewHosts.delete(record.viewKey);
      const windowHost = this.#windowHosts.get(record.windowKey);
      windowHost?.views.delete(record.viewKey);
      if (windowHost?.views.size === 0) {
        windowHost.element.remove();
        this.#windowHosts.delete(record.windowKey);
        if (this.#fullscreenWindowKey === record.windowKey) {
          this.#fullscreenWindowKey = null;
          this.#restoreWindowVisibility();
        }
      } else if (windowHost) {
        this.#layoutViewHosts(windowHost);
      }
      this.#layoutWindowHosts();
    }
    const ownerKeys = this.#keysByOwner.get(owner);
    ownerKeys?.delete(key);
    if (ownerKeys?.size === 0) this.#keysByOwner.delete(owner);
    this.#restoreViewFocus(window, view);
  }

  #ensureViewHost(
    identity: AppSurfaceIdentity,
    windowKey: string,
    viewKey: string,
  ): ViewHostRecord {
    let windowHost = this.#windowHosts.get(windowKey);
    if (windowHost === undefined) {
      const element = document.createElement('div');
      configureWindowHostElement(element, windowKey);
      element.hidden = this.#fullscreenWindowKey !== null
        && this.#fullscreenWindowKey !== windowKey;
      windowHost = { key: windowKey, element, views: new Set() };
      this.#windowHosts.set(windowKey, windowHost);
      this.#root.appendChild(element);
      this.#layoutWindowHosts();
    }
    let viewHost = this.#viewHosts.get(viewKey);
    if (viewHost === undefined) {
      const element = document.createElement('div');
      configureViewHostElement(element, identity, viewKey);
      viewHost = { key: viewKey, windowKey, element, surfaces: new Set() };
      this.#viewHosts.set(viewKey, viewHost);
      windowHost.views.add(viewKey);
      windowHost.element.appendChild(element);
      this.#layoutViewHosts(windowHost);
    } else if (viewHost.windowKey !== windowKey) {
      throw new Error('App View host changed its Window identity');
    }
    return viewHost;
  }

  #sortViewLayers(viewKey: string): void {
    const viewHost = this.#viewHosts.get(viewKey);
    if (viewHost === undefined) return;
    for (const record of this.#orderedRecords()) {
      if (record.viewKey === viewKey) viewHost.element.appendChild(record.element);
    }
  }

  #orderedWindowHosts(): WindowHostRecord[] {
    return [...this.#windowHosts.values()]
      .sort((left, right) => compareUtf8(left.key, right.key));
  }

  #layoutWindowHosts(): void {
    const windows = this.#orderedWindowHosts();
    const visibleCount = windows.filter((window) => !window.element.hidden).length;
    this.#root.style.gridTemplateColumns =
      `repeat(${Math.max(1, visibleCount)}, minmax(0, 1fr))`;
    for (const window of windows) {
      this.#root.appendChild(window.element);
      window.element.dataset.appWindowMultiple = windows.length > 1 ? 'true' : 'false';
    }
  }

  #layoutViewHosts(window: WindowHostRecord): void {
    const views = [...window.views]
      .map((key) => this.#viewHosts.get(key))
      .filter((view): view is ViewHostRecord => view !== undefined)
      .sort((left, right) => compareUtf8(left.key, right.key));
    window.element.style.gridTemplateColumns =
      `repeat(${Math.max(1, views.length)}, minmax(0, 1fr))`;
    for (const view of views) {
      window.element.appendChild(view.element);
      view.element.dataset.appViewMultiple = views.length > 1 ? 'true' : 'false';
    }
  }

  #applyBrowserPlatformEvent(event: BrowserPlatformEvent): void {
    if (this.#closed) return;
    const records = this.#orderedRecords().filter((record) => (
      sameHandle(record.descriptor.identity.session, event.session)
      && sameHandle(record.descriptor.identity.window, event.window)
    ));
    const first = records[0];
    if (first === undefined) return;
    const windowHost = this.#windowHosts.get(first.windowKey);
    if (windowHost === undefined) return;
    if (event.kind === 'window.close') {
      for (const record of records) this.#releaseSurfaceInput(record);
      windowHost.element.hidden = true;
      windowHost.element.dataset.appWindowClosed = 'true';
      if (this.#fullscreenWindowKey === windowHost.key) {
        this.#fullscreenWindowKey = null;
        this.#restoreWindowVisibility();
      }
      this.#layoutWindowHosts();
      return;
    }
    if (event.kind === 'window.fullscreen') {
      this.#fullscreenWindowKey = windowHost.key;
      for (const candidate of this.#windowHosts.values()) {
        candidate.element.hidden = candidate.key !== windowHost.key;
      }
      delete windowHost.element.dataset.appWindowClosed;
      this.#layoutWindowHosts();
      const focus = records.reverse().find((record) => (
        record.descriptor.input !== 'passthrough' && !this.#isSuspended(record.key)
      ));
      if (focus !== undefined) this.#setFocusedKey(focus.key, true);
      return;
    }
    if (event.kind === 'window.exit-fullscreen') {
      if (this.#fullscreenWindowKey === windowHost.key) {
        this.#fullscreenWindowKey = null;
        this.#restoreWindowVisibility();
        this.#layoutWindowHosts();
      }
      return;
    }
    if (event.kind === 'window.focus') {
      if (this.#fullscreenWindowKey === null || this.#fullscreenWindowKey === windowHost.key) {
        windowHost.element.hidden = false;
      }
      delete windowHost.element.dataset.appWindowClosed;
      this.#layoutWindowHosts();
      const focus = records.reverse().find((record) => (
        record.descriptor.input !== 'passthrough' && !this.#isSuspended(record.key)
      ));
      if (focus !== undefined) this.#setFocusedKey(focus.key, true);
      return;
    }
    if (event.kind === 'window.title') {
      windowHost.element.dataset.appWindowTitle = event.title;
      return;
    }
    if (!('view' in event)) return;
    const viewRecords = records.filter((record) => (
      sameHandle(record.descriptor.identity.view, event.view)
    ));
    const viewHost = this.#viewHosts.get(viewRecords[0]?.viewKey ?? '');
    if (viewHost === undefined) return;
    if (event.kind === 'view.title') {
      viewHost.element.dataset.appViewTitle = event.title;
      return;
    }
    if (event.kind === 'view.blur') {
      delete viewHost.element.dataset.appViewFocused;
      const focused = this.#focusedRecord();
      if (
        focused !== null
        && sameHandle(focused.descriptor.identity.window, event.window)
        && sameHandle(focused.descriptor.identity.view, event.view)
      ) {
        this.#setFocusedKey(null, false);
      }
      return;
    }
    if (this.#fullscreenWindowKey === null || this.#fullscreenWindowKey === windowHost.key) {
      windowHost.element.hidden = false;
    }
    delete windowHost.element.dataset.appWindowClosed;
    viewHost.element.dataset.appViewFocused = 'true';
    for (const key of windowHost.views) {
      const sibling = this.#viewHosts.get(key);
      if (sibling !== undefined && sibling !== viewHost) {
        delete sibling.element.dataset.appViewFocused;
      }
    }
    this.#layoutWindowHosts();
    const focus = viewRecords.reverse().find((record) => (
      record.descriptor.input !== 'passthrough' && !this.#isSuspended(record.key)
    ));
    if (focus !== undefined) this.#setFocusedKey(focus.key, true);
  }

  #restoreWindowVisibility(): void {
    for (const window of this.#windowHosts.values()) {
      window.element.hidden = window.element.dataset.appWindowClosed === 'true';
    }
  }

  #orderedRecords(): SurfaceRecord[] {
    return [...this.#records.values()].sort((left, right) => (
      left.descriptor.layer - right.descriptor.layer
      || compareUtf8(left.key, right.key)
    ));
  }

  #assertOpen(): void {
    if (this.#closed) throw new Error('App composition host is closed');
  }

  #installInputCapture(): void {
    const options = { capture: true, signal: this.#abort.signal };
    for (const type of ['pointerdown', 'pointermove', 'pointerup', 'pointercancel'] as const) {
      this.#root.addEventListener(type, (event) => this.#routePointer(event), options);
    }
    this.#root.addEventListener('wheel', (event) => this.#routeWheel(event), {
      capture: true,
      passive: true,
      signal: this.#abort.signal,
    });
    for (const type of ['keydown', 'keyup'] as const) {
      this.#root.addEventListener(type, (event) => this.#routeKey(event), options);
    }
    this.#root.addEventListener('beforeinput', (event) => this.#routeText(event), options);
    for (const type of ['compositionstart', 'compositionupdate', 'compositionend'] as const) {
      this.#root.addEventListener(type, (event) => this.#routeComposition(event), options);
    }
    this.#root.addEventListener('focusout', (event) => {
      if (
        this.#focusedKey !== null
        && (!(event.relatedTarget instanceof Node) || !this.#root.contains(event.relatedTarget))
      ) {
        this.#setFocusedKey(null, false);
      }
    }, options);
  }

  #routePointer(event: PointerEvent): void {
    const capturedKey = this.#pointerCapture.get(event.pointerId);
    const hits = capturedKey === undefined
      ? this.#hitRecords(event.clientX, event.clientY)
      : [this.#records.get(capturedKey)]
        .filter((record): record is SurfaceRecord => (
          record !== undefined && !this.#isSuspended(record.key)
        ))
        .map((record) => ({ record, input: 'opaque' as const }));
    if (event.type === 'pointerdown') {
      const focus = hits[0]?.record;
      if (focus !== undefined) this.#setFocusedKey(focus.key, false);
    }
    this.#recordArbitration('pointer', hits);
    for (const { record } of hits) {
      const rect = record.element.getBoundingClientRect();
      this.#dispatch(record, Object.freeze({
        type: pointerEventType(event.type),
        sequence: this.#takeInputSequence(),
        timestampMicros: eventTimestampMicros(event),
        surface: record.descriptor.identity,
        pointerId: event.pointerId,
        pointerType: event.pointerType,
        xMilli: milli(event.clientX),
        yMilli: milli(event.clientY),
        localXMilli: milli(event.clientX - rect.left),
        localYMilli: milli(event.clientY - rect.top),
        movementXMilli: milli(event.movementX),
        movementYMilli: milli(event.movementY),
        button: event.button,
        buttons: event.buttons,
        pressureQ16: Math.max(0, Math.min(65_536, Math.round(event.pressure * 65_536))),
        tiltX: event.tiltX,
        tiltY: event.tiltY,
        alt: event.altKey,
        control: event.ctrlKey,
        meta: event.metaKey,
        shift: event.shiftKey,
        synthesized: false,
      }));
    }
    if (event.type === 'pointerup' || event.type === 'pointercancel') {
      this.#pointerCapture.delete(event.pointerId);
    }
  }

  #routeWheel(event: WheelEvent): void {
    const hits = this.#hitRecords(event.clientX, event.clientY);
    this.#recordArbitration('wheel', hits);
    for (const { record } of hits) {
      const rect = record.element.getBoundingClientRect();
      this.#dispatch(record, Object.freeze({
        type: 'wheel',
        sequence: this.#takeInputSequence(),
        timestampMicros: eventTimestampMicros(event),
        surface: record.descriptor.identity,
        xMilli: milli(event.clientX),
        yMilli: milli(event.clientY),
        localXMilli: milli(event.clientX - rect.left),
        localYMilli: milli(event.clientY - rect.top),
        deltaXMilli: milli(event.deltaX),
        deltaYMilli: milli(event.deltaY),
        deltaZMilli: milli(event.deltaZ),
        deltaMode: event.deltaMode,
        alt: event.altKey,
        control: event.ctrlKey,
        meta: event.metaKey,
        shift: event.shiftKey,
      }));
    }
  }

  #routeKey(event: KeyboardEvent): void {
    let records: SurfaceRecord[];
    if (event.type === 'keydown') {
      records = this.#keyboardTargets(event);
      if (records.length === 0) return;
      this.#pressedKeys.set(event.code, new Set(records.map((record) => record.key)));
    } else {
      const keys = this.#pressedKeys.get(event.code);
      this.#pressedKeys.delete(event.code);
      records = keys === undefined
        ? this.#keyboardTargets(event)
        : [...keys]
          .map((key) => this.#records.get(key))
          .filter((record): record is SurfaceRecord => (
            record !== undefined && !this.#isSuspended(record.key)
          ));
    }
    if (records.length > 0) {
      this.#recordTrace(
        'key',
        records[0]!,
        records.map((record) => record.key).join(','),
      );
    }
    for (const record of records) {
      this.#dispatch(record, Object.freeze({
        type: event.type === 'keydown' ? 'keyDown' : 'keyUp',
        sequence: this.#takeInputSequence(),
        timestampMicros: eventTimestampMicros(event),
        surface: record.descriptor.identity,
        physical: event.code,
        logical: event.key,
        repeat: event.repeat,
        alt: event.altKey,
        control: event.ctrlKey,
        meta: event.metaKey,
        shift: event.shiftKey,
        synthesized: false,
      }));
    }
  }

  #routeText(event: InputEvent): void {
    if (event.data === null || event.data.length === 0) return;
    const record = this.#focusedRecord();
    if (record === null) return;
    this.#recordTrace('text', record, event.inputType);
    this.#dispatch(record, Object.freeze({
      type: 'text',
      sequence: this.#takeInputSequence(),
      timestampMicros: eventTimestampMicros(event),
      surface: record.descriptor.identity,
      text: event.data,
      inputType: event.inputType,
      composing: event.isComposing,
      synthesized: false,
    }));
  }

  #routeComposition(event: CompositionEvent): void {
    const record = this.#focusedRecord();
    if (record === null) return;
    this.#recordTrace('composition', record, event.type);
    this.#dispatch(record, Object.freeze({
      type: compositionEventType(event.type),
      sequence: this.#takeInputSequence(),
      timestampMicros: eventTimestampMicros(event),
      surface: record.descriptor.identity,
      text: event.data,
      inputType: '',
      composing: event.type !== 'compositionend',
      synthesized: false,
    }));
  }

  #hitRecords(clientX: number, clientY: number): HitRecord[] {
    const hits: HitRecord[] = [];
    for (const record of this.#orderedRecords().reverse()) {
      if (
        record.descriptor.input === 'passthrough'
        || this.#isSuspended(record.key)
      ) {
        continue;
      }
      const rect = record.element.getBoundingClientRect();
      if (
        clientX < rect.left
        || clientX >= rect.right
        || clientY < rect.top
        || clientY >= rect.bottom
      ) {
        continue;
      }
      const input = this.#surfaceHitPolicy(
        record,
        milli(clientX - rect.left),
        milli(clientY - rect.top),
      );
      if (input === null) continue;
      hits.push({ record, input });
      if (input === 'opaque') break;
    }
    return hits;
  }

  #setFocusedKey(key: string | null, synthesized: boolean): void {
    if (this.#focusedKey === key) return;
    const previous = this.#focusedRecord();
    this.#focusedKey = key;
    if (previous !== null) {
      this.#dispatch(previous, Object.freeze({
        type: 'focus',
        sequence: this.#takeInputSequence(),
        timestampMicros: monotonicMicros(),
        surface: previous.descriptor.identity,
        focused: false,
        synthesized,
      }));
    }
    const next = this.#focusedRecord();
    if (next !== null) {
      this.#dispatch(next, Object.freeze({
        type: 'focus',
        sequence: this.#takeInputSequence(),
        timestampMicros: monotonicMicros(),
        surface: next.descriptor.identity,
        focused: true,
        synthesized,
      }));
      this.#recordTrace('focus', next, synthesized ? 'restored' : 'selected');
    }
  }

  #releaseOwnerInput(owner: string): void {
    for (const [pointerId, key] of [...this.#pointerCapture]) {
      const record = this.#records.get(key);
      if (record?.owner !== owner) continue;
      this.#dispatchPointerCancel(record, pointerId);
      this.#pointerCapture.delete(pointerId);
    }
    for (const [physical, keys] of [...this.#pressedKeys]) {
      for (const key of [...keys]) {
        const record = this.#records.get(key);
        if (record?.owner !== owner) continue;
        this.#dispatchKeyRelease(record, physical);
        keys.delete(key);
      }
      if (keys.size === 0) this.#pressedKeys.delete(physical);
    }
    if (this.#focusedRecord()?.owner === owner) this.#setFocusedKey(null, true);
  }

  #releaseSurfaceInput(record: SurfaceRecord): void {
    for (const [pointerId, key] of [...this.#pointerCapture]) {
      if (key !== record.key) continue;
      this.#dispatchPointerCancel(record, pointerId);
      this.#pointerCapture.delete(pointerId);
    }
    for (const [physical, keys] of [...this.#pressedKeys]) {
      if (!keys.delete(record.key)) continue;
      this.#dispatchKeyRelease(record, physical);
      if (keys.size === 0) this.#pressedKeys.delete(physical);
    }
    if (this.#focusedKey === record.key) this.#setFocusedKey(null, true);
  }

  #dispatchPointerCancel(record: SurfaceRecord, pointerId: number): void {
    this.#dispatch(record, Object.freeze({
      type: 'pointerCancel',
      sequence: this.#takeInputSequence(),
      timestampMicros: monotonicMicros(),
      surface: record.descriptor.identity,
      pointerId,
      pointerType: '',
      xMilli: 0,
      yMilli: 0,
      localXMilli: 0,
      localYMilli: 0,
      movementXMilli: 0,
      movementYMilli: 0,
      button: -1,
      buttons: 0,
      pressureQ16: 0,
      tiltX: 0,
      tiltY: 0,
      alt: false,
      control: false,
      meta: false,
      shift: false,
      synthesized: true,
    }));
  }

  #dispatchKeyRelease(record: SurfaceRecord, physical: string): void {
    this.#dispatch(record, Object.freeze({
      type: 'keyUp',
      sequence: this.#takeInputSequence(),
      timestampMicros: monotonicMicros(),
      surface: record.descriptor.identity,
      physical,
      logical: '',
      repeat: false,
      alt: false,
      control: false,
      meta: false,
      shift: false,
      synthesized: true,
    }));
  }

  #focusedRecord(): SurfaceRecord | null {
    if (this.#focusedKey === null || this.#isSuspended(this.#focusedKey)) return null;
    return this.#records.get(this.#focusedKey) ?? null;
  }

  #surfaceHitPolicy(
    record: SurfaceRecord,
    localXMilli: number,
    localYMilli: number,
  ): AppSurfaceHitPolicy | null {
    if (record.descriptor.input === 'opaque') return 'opaque';
    const publication = this.#hitRegions.get(record.key);
    if (publication === undefined) return 'transparent';
    for (let index = publication.regions.length - 1; index >= 0; index -= 1) {
      const region = publication.regions[index]!;
      if (
        localXMilli >= region.xMilli
        && localXMilli < region.xMilli + region.widthMilli
        && localYMilli >= region.yMilli
        && localYMilli < region.yMilli + region.heightMilli
      ) {
        return region.input;
      }
    }
    return null;
  }

  #keyboardTargets(event: KeyboardEvent): SurfaceRecord[] {
    const records: SurfaceRecord[] = [];
    const focused = this.#focusedRecord();
    if (focused !== null) records.push(focused);
    const reservedKey = this.#resolveSystemShortcut(shortcutSignature({
      physical: event.code,
      alt: event.altKey,
      control: event.ctrlKey,
      meta: event.metaKey,
      shift: event.shiftKey,
    }));
    if (reservedKey !== undefined && reservedKey !== focused?.key) {
      const reserved = this.#records.get(reservedKey);
      if (reserved !== undefined && !this.#isSuspended(reserved.key)) {
        records.push(reserved);
      }
    }
    return records;
  }

  #restoreViewFocus(window: AppHandle, view: AppHandle): void {
    if (this.#focusedRecord() !== null) return;
    const fallback = this.#orderedRecords()
      .reverse()
      .find((record) => (
        sameHandle(record.descriptor.identity.window, window)
        && sameHandle(record.descriptor.identity.view, view)
        && record.descriptor.input !== 'passthrough'
        && !this.#isSuspended(record.key)
      ));
    if (fallback !== undefined) this.#setFocusedKey(fallback.key, true);
  }

  #ownedRecord(owner: string, identity: AppSurfaceIdentity): SurfaceRecord {
    validateOwner(owner);
    validateIdentity(this.#sessionId, identity);
    const record = this.#records.get(surfaceKey(identity));
    if (record?.owner !== owner) throw new Error('App Surface owner mismatch');
    return record;
  }

  #clearSurfaceShortcuts(key: string): void {
    const shortcuts = this.#shortcutsBySurface.get(key);
    if (shortcuts === undefined) return;
    for (const shortcut of shortcuts) {
      const registrations = this.#shortcutOwners
        .get(shortcut)
        ?.filter((registration) => registration.key !== key);
      if (registrations === undefined || registrations.length === 0) {
        this.#shortcutOwners.delete(shortcut);
      } else {
        this.#shortcutOwners.set(shortcut, registrations);
      }
    }
    this.#shortcutsBySurface.delete(key);
  }

  #applyLowerSuspension(overlay: SurfaceRecord, suspended: boolean): void {
    const targets = this.#orderedRecords().filter((candidate) => (
      candidate.key !== overlay.key
      && sameHandle(
        candidate.descriptor.identity.window,
        overlay.descriptor.identity.window,
      )
      && sameHandle(
        candidate.descriptor.identity.view,
        overlay.descriptor.identity.view,
      )
      && candidate.descriptor.layer < overlay.descriptor.layer
    ));
    if (suspended) {
      this.#modalOverlays.add(overlay.key);
      for (const target of targets) {
        let claimants = this.#lowerSuspensionClaims.get(target.key);
        if (claimants === undefined) {
          claimants = new Set();
          this.#lowerSuspensionClaims.set(target.key, claimants);
        }
        const wasSuspended = this.#isSuspended(target.key);
        claimants.add(overlay.key);
        if (!wasSuspended) {
          this.#releaseSurfaceInput(target);
          target.element.dataset.appSurfaceSuspended = 'true';
        }
      }
      this.#recordTrace('suspend', overlay, `lower:${targets.length}`);
      this.#setFocusedKey(overlay.key, true);
      return;
    }
    this.#modalOverlays.delete(overlay.key);
    for (const target of targets) {
      const claimants = this.#lowerSuspensionClaims.get(target.key);
      claimants?.delete(overlay.key);
      if (claimants?.size === 0) this.#lowerSuspensionClaims.delete(target.key);
      if (!this.#isSuspended(target.key)) {
        delete target.element.dataset.appSurfaceSuspended;
      }
    }
    this.#recordTrace('resume', overlay, `lower:${targets.length}`);
  }

  #isSuspended(key: string): boolean {
    return this.#suspended.has(key)
      || (this.#lowerSuspensionClaims.get(key)?.size ?? 0) > 0;
  }

  #resolveSystemShortcut(signature: string): string | undefined {
    const focused = this.#focusedRecord();
    const registrations = this.#shortcutOwners.get(signature);
    if (registrations === undefined) return undefined;
    return registrations
      .map((registration) => ({
        registration,
        record: this.#records.get(registration.key),
      }))
      .filter(({ registration, record }) => (
        record !== undefined
        && !this.#isSuspended(registration.key)
        && shortcutScopeMatches(registration.scope, record, focused)
      ))
      .sort((left, right) => (
        right.registration.priority - left.registration.priority
        || right.record!.descriptor.layer - left.record!.descriptor.layer
        || compareUtf8(left.registration.key, right.registration.key)
      ))[0]?.registration.key;
  }

  #recordTrace(
    action: AppCompositionTraceEntry['action'],
    record: SurfaceRecord,
    detail: string,
  ): void {
    this.#trace.push(Object.freeze({
      sequence: this.#takeInputSequence(),
      timestampMicros: monotonicMicros(),
      action,
      surface: record.descriptor.identity,
      detail,
    }));
    if (this.#trace.length > MAX_COMPOSITION_TRACE_ENTRIES) this.#trace.shift();
  }

  #recordArbitration(
    action: 'pointer' | 'wheel',
    hits: readonly HitRecord[],
  ): void {
    const first = hits[0];
    if (first === undefined) return;
    this.#recordTrace(
      action,
      first.record,
      hits.map(({ record, input }) => `${record.key}:${input}`).join(','),
    );
  }

  #dispatch(record: SurfaceRecord, event: AppSurfaceInputEvent): void {
    this.#inputSinks.get(record.owner)?.(event);
  }

  #takeInputSequence(): bigint {
    const sequence = this.#nextInputSequence;
    this.#nextInputSequence += 1n;
    return sequence;
  }
}

function validateAssetHandle(handle: AppAssetHandle): void {
  if (
    !Number.isInteger(handle.index) || handle.index < 0 || handle.index >= 0xffffffff
    || !Number.isInteger(handle.generation) || handle.generation < 1
    || handle.generation > 0xffffffff
  ) {
    throw new Error('App asset handle is invalid');
  }
}

function assetBufferKey(handle: AppAssetHandle): string {
  return `${handle.index}:${handle.generation}`;
}

function configureRoot(root: HTMLElement): void {
  root.style.position = 'relative';
  root.style.isolation = 'isolate';
  root.style.overflow = 'hidden';
  root.style.display = 'grid';
  root.style.gridTemplateColumns = 'minmax(0, 1fr)';
  root.style.gridTemplateRows = 'minmax(0, 1fr)';
}

function configureWindowHostElement(element: HTMLDivElement, key: string): void {
  element.dataset.appWindow = key;
  element.style.position = 'relative';
  element.style.display = 'grid';
  element.style.gridTemplateColumns = 'minmax(0, 1fr)';
  element.style.gridTemplateRows = 'minmax(0, 1fr)';
  element.style.minWidth = '0';
  element.style.minHeight = '0';
  element.style.overflow = 'hidden';
  element.style.isolation = 'isolate';
}

function configureViewHostElement(
  element: HTMLDivElement,
  identity: AppSurfaceIdentity,
  key: string,
): void {
  element.dataset.appView = key;
  element.dataset.appWindow = appWindowKey(identity);
  element.style.position = 'relative';
  element.style.minWidth = '0';
  element.style.minHeight = '0';
  element.style.overflow = 'hidden';
  element.style.isolation = 'isolate';
}

function configureSurfaceElement(
  element: HTMLDivElement | HTMLCanvasElement,
  descriptor: AppSurfaceDescriptor,
): void {
  element.dataset.appSurface = surfaceKey(descriptor.identity);
  element.dataset.appSurfaceLabel = descriptor.label;
  element.style.position = 'absolute';
  element.style.inset = '0';
  element.style.width = '100%';
  element.style.height = '100%';
  element.style.zIndex = String(descriptor.layer);
  element.style.pointerEvents = descriptor.input === 'passthrough' ? 'none' : 'auto';
  if (descriptor.input === 'transparent') {
    element.dataset.appSurfaceInput = 'transparent';
  }
  if (element instanceof HTMLCanvasElement) {
    element.tabIndex = 0;
    element.style.display = 'block';
  }
}

function validateOwner(owner: string): void {
  if (!owner || encoder.encode(owner).byteLength > MAX_FRAMEWORK_OWNER_BYTES) {
    throw new Error('invalid framework Surface owner');
  }
}

function validateDescriptor(sessionId: number, descriptor: AppSurfaceDescriptor): void {
  validateIdentity(sessionId, descriptor.identity);
  if (descriptor.kind !== 'dom' && descriptor.kind !== 'canvas') {
    throw new Error('invalid App Surface kind');
  }
  if (!Number.isSafeInteger(descriptor.layer) || descriptor.layer < -1_000_000 || descriptor.layer > 1_000_000) {
    throw new Error('invalid App Surface layer');
  }
  if (
    descriptor.input !== 'opaque'
    && descriptor.input !== 'transparent'
    && descriptor.input !== 'passthrough'
  ) {
    throw new Error('invalid App Surface input policy');
  }
  if (!descriptor.label || encoder.encode(descriptor.label).byteLength > MAX_SURFACE_LABEL_BYTES) {
    throw new Error('invalid App Surface label');
  }
}

function validateHitRegion(region: AppSurfaceHitRegion): void {
  if (
    !Number.isSafeInteger(region.xMilli)
    || !Number.isSafeInteger(region.yMilli)
    || !Number.isSafeInteger(region.widthMilli)
    || !Number.isSafeInteger(region.heightMilli)
    || region.widthMilli <= 0
    || region.heightMilli <= 0
    || region.xMilli < -0x8000_0000
    || region.yMilli < -0x8000_0000
    || region.xMilli > 0x7fff_ffff
    || region.yMilli > 0x7fff_ffff
    || region.widthMilli > 0x7fff_ffff
    || region.heightMilli > 0x7fff_ffff
    || region.xMilli + region.widthMilli > 0x7fff_ffff
    || region.yMilli + region.heightMilli > 0x7fff_ffff
    || (region.input !== 'opaque' && region.input !== 'transparent')
  ) {
    throw new Error('App Surface hit region is invalid');
  }
}

function validateSystemShortcut(shortcut: AppSystemShortcut): void {
  if (
    shortcut.classMask <= 0n
    || shortcut.classMask > 0xffff_ffff_ffff_ffffn
    || !shortcut.physical
    || encoder.encode(shortcut.physical).byteLength > 128
    || typeof shortcut.alt !== 'boolean'
    || typeof shortcut.control !== 'boolean'
    || typeof shortcut.meta !== 'boolean'
    || typeof shortcut.shift !== 'boolean'
    || (
      shortcut.scope !== 'view'
      && shortcut.scope !== 'window'
      && shortcut.scope !== 'session'
    )
    || !Number.isSafeInteger(shortcut.priority)
    || shortcut.priority < -0x8000
    || shortcut.priority > 0x7fff
  ) {
    throw new Error('App system shortcut is invalid');
  }
}

function shortcutScopeMatches(
  scope: AppSystemShortcut['scope'],
  target: SurfaceRecord,
  focused: SurfaceRecord | null,
): boolean {
  if (scope === 'session') return true;
  if (focused === null) return false;
  if (scope === 'window') {
    return sameHandle(
      target.descriptor.identity.window,
      focused.descriptor.identity.window,
    );
  }
  return sameHandle(
    target.descriptor.identity.window,
    focused.descriptor.identity.window,
  ) && sameHandle(target.descriptor.identity.view, focused.descriptor.identity.view);
}

function shortcutSignature(
  shortcut: Pick<
    AppSystemShortcut,
    'physical' | 'alt' | 'control' | 'meta' | 'shift'
  >,
): string {
  return [
    shortcut.physical,
    shortcut.alt ? '1' : '0',
    shortcut.control ? '1' : '0',
    shortcut.meta ? '1' : '0',
    shortcut.shift ? '1' : '0',
  ].join(':');
}

function validateIdentity(sessionId: number, identity: AppSurfaceIdentity): void {
  if (identity.sessionId !== sessionId || identity.sessionEpoch <= 0n) {
    throw new Error('App Surface session mismatch');
  }
  validateHandle(identity.session);
  validateHandle(identity.window);
  validateHandle(identity.view);
  validateHandle(identity.surface);
}

function validateHandle(handle: AppHandle): void {
  if (
    !Number.isSafeInteger(handle.index)
    || handle.index < 0
    || handle.index >= 0xffff_ffff
    || !Number.isSafeInteger(handle.generation)
    || handle.generation < 1
    || handle.generation > 0xffff_ffff
  ) {
    throw new Error('invalid App Surface handle');
  }
}

function sameHandle(left: AppHandle, right: AppHandle): boolean {
  return left.index === right.index && left.generation === right.generation;
}

function surfaceKey(identity: AppSurfaceIdentity): string {
  return [
    `${identity.session.index}:${identity.session.generation}@${identity.sessionEpoch}`,
    `${identity.window.index}:${identity.window.generation}`,
    `${identity.view.index}:${identity.view.generation}`,
    `${identity.surface.index}:${identity.surface.generation}`,
  ].join('/');
}

function appWindowKey(identity: AppSurfaceIdentity): string {
  return [
    `${identity.session.index}:${identity.session.generation}@${identity.sessionEpoch}`,
    `${identity.window.index}:${identity.window.generation}`,
  ].join('/');
}

function appViewKey(identity: AppSurfaceIdentity): string {
  return [
    appWindowKey(identity),
    `${identity.view.index}:${identity.view.generation}`,
  ].join('/');
}

function compareUtf8(left: string, right: string): number {
  const leftBytes = encoder.encode(left);
  const rightBytes = encoder.encode(right);
  const common = Math.min(leftBytes.byteLength, rightBytes.byteLength);
  for (let index = 0; index < common; index += 1) {
    const order = leftBytes[index]! - rightBytes[index]!;
    if (order !== 0) return order;
  }
  return leftBytes.byteLength - rightBytes.byteLength;
}

function validatePointerId(pointerId: number): void {
  if (!Number.isSafeInteger(pointerId) || pointerId < 0 || pointerId > 0xffff_ffff) {
    throw new Error('invalid pointer identity');
  }
}

function pointerEventType(type: string): 'pointerDown' | 'pointerMove' | 'pointerUp' | 'pointerCancel' {
  switch (type) {
    case 'pointerdown': return 'pointerDown';
    case 'pointermove': return 'pointerMove';
    case 'pointerup': return 'pointerUp';
    case 'pointercancel': return 'pointerCancel';
    default: throw new Error('invalid pointer event type');
  }
}

function compositionEventType(
  type: string,
): 'compositionStart' | 'compositionUpdate' | 'compositionEnd' {
  switch (type) {
    case 'compositionstart': return 'compositionStart';
    case 'compositionupdate': return 'compositionUpdate';
    case 'compositionend': return 'compositionEnd';
    default: throw new Error('invalid composition event type');
  }
}

function milli(value: number): number {
  if (!Number.isFinite(value)) return 0;
  return Math.max(-0x8000_0000, Math.min(0x7fff_ffff, Math.round(value * 1000)));
}

function eventTimestampMicros(event: Event): bigint {
  return BigInt(Math.max(0, Math.round(event.timeStamp * 1000)));
}

function monotonicMicros(): bigint {
  return BigInt(Math.max(0, Math.round(performance.now() * 1000)));
}
