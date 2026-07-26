const SNAPSHOT_MAGIC = new Uint8Array([0x56, 0x53, 0x42, 0x31]);
const textEncoder = new TextEncoder();
const textDecoder = new TextDecoder('utf-8', { fatal: true });
const HOST_DRIVEN_KINDS = new Set([
  'animation.clock',
  'resize',
  'visibility',
  'route.location',
  'global.shortcut',
  'pointer.stream',
  'file.drop',
  'resource.watch',
  'platform.lifecycle',
]);

export type VoguiHostSubscription = Readonly<{
  caller: Uint8Array;
  handleIndex: number;
  handleGeneration: number;
  kind: string;
  descriptor: Uint8Array;
}>;

type SubscriptionSubmit = (
  subscription: VoguiHostSubscription,
  payload: Uint8Array,
) => void | Promise<void>;

class SnapshotReader {
  readonly #bytes: Uint8Array;
  readonly #view: DataView;
  #offset = 0;

  constructor(bytes: Uint8Array) {
    this.#bytes = bytes;
    this.#view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  }

  take(length: number): Uint8Array {
    const end = this.#offset + length;
    if (!Number.isSafeInteger(length) || length < 0 || end > this.#bytes.byteLength) {
      throw new RangeError('truncated Vogui subscription snapshot');
    }
    const value = this.#bytes.slice(this.#offset, end);
    this.#offset = end;
    return value;
  }

  u16(): number {
    const value = this.#view.getUint16(this.#offset, true);
    this.#offset += 2;
    return value;
  }

  u32(): number {
    const value = this.#view.getUint32(this.#offset, true);
    this.#offset += 4;
    return value;
  }

  done(): boolean {
    return this.#offset === this.#bytes.byteLength;
  }
}

export function decodeVoguiSubscriptionSnapshot(bytes: Uint8Array): VoguiHostSubscription[] {
  const reader = new SnapshotReader(bytes);
  if (!sameBytes(reader.take(4), SNAPSHOT_MAGIC)) {
    throw new Error('invalid Vogui subscription snapshot magic');
  }
  const count = reader.u32();
  if (count > 4096) throw new RangeError('Vogui subscription snapshot capacity exceeded');
  const subscriptions: VoguiHostSubscription[] = [];
  for (let index = 0; index < count; index += 1) {
    const caller = reader.take(32);
    const handleIndex = reader.u32();
    const handleGeneration = reader.u32();
    const kindLength = reader.u16();
    const descriptorLength = reader.u32();
    const kind = textDecoder.decode(reader.take(kindLength));
    const descriptor = reader.take(descriptorLength);
    subscriptions.push({ caller, handleIndex, handleGeneration, kind, descriptor });
  }
  if (!reader.done()) throw new Error('trailing Vogui subscription snapshot bytes');
  return subscriptions;
}

export class VoguiSubscriptionHost {
  readonly #sessionId: number;
  readonly #submit: SubscriptionSubmit;
  #subscriptions = new Map<string, VoguiHostSubscription>();
  #listeners: Array<() => void> = [];
  #animationFrame: number | null = null;
  #pointerFrame: number | null = null;
  #pendingPointer: Uint8Array | null = null;
  #tail = Promise.resolve();
  #interactive = false;
  #routeUrl = typeof window === 'undefined' ? 'about:blank' : window.location.href;

  constructor(sessionId: number, submit: SubscriptionSubmit) {
    if (!Number.isSafeInteger(sessionId) || sessionId < 1) {
      throw new Error('Vogui subscription host session ID must be positive');
    }
    this.#sessionId = sessionId;
    this.#submit = submit;
  }

  setInteractive(interactive: boolean): void {
    if (this.#interactive === interactive) return;
    this.#interactive = interactive;
    if (!interactive) {
      if (this.#pointerFrame !== null) cancelAnimationFrame(this.#pointerFrame);
      this.#pointerFrame = null;
      this.#pendingPointer = null;
    }
  }

  reconcile(frame: Uint8Array): void {
    const previous = this.#subscriptions;
    const next = new Map(
      decodeVoguiSubscriptionSnapshot(frame)
        .filter((subscription) => HOST_DRIVEN_KINDS.has(subscription.kind))
        .map((subscription) => [
          subscriptionIdentity(subscription),
          subscription,
        ]),
    );
    const wasActive = this.#subscriptions.size > 0;
    this.#subscriptions = next;
    if (!wasActive && next.size > 0) this.#installListeners();
    if (wasActive && next.size === 0) this.dispose();
    this.#scheduleAnimation();
    for (const [identity, subscription] of next) {
      if (previous.has(identity)) continue;
      if (subscription.kind === 'resize') this.#emitResize(subscription);
      if (subscription.kind === 'visibility') this.#emitVisibility(subscription);
      if (subscription.kind === 'route.location') this.#emitRoute(subscription);
    }
  }

  dispose(): void {
    for (const remove of this.#listeners.splice(0)) remove();
    if (this.#animationFrame !== null) cancelAnimationFrame(this.#animationFrame);
    if (this.#pointerFrame !== null) cancelAnimationFrame(this.#pointerFrame);
    this.#animationFrame = null;
    this.#pointerFrame = null;
    this.#pendingPointer = null;
    this.#subscriptions.clear();
  }

  #installListeners(): void {
    if (typeof window === 'undefined' || typeof document === 'undefined') return;
    this.#listen(window, 'resize', () => this.#forKind('resize', (item) => this.#emitResize(item)));
    this.#listen(document, 'visibilitychange', () => {
      this.#forKind('visibility', (item) => this.#emitVisibility(item));
      this.#emitText('platform.lifecycle', document.hidden ? 'suspended' : 'resumed');
    });
    this.#listen(window, 'studio-gui-navigation', (event) => {
      const detail = (event as CustomEvent<unknown>).detail;
      if (
        typeof detail !== 'object'
        || detail === null
        || !('sessionId' in detail)
        || detail.sessionId !== this.#sessionId
        || !('url' in detail)
        || typeof detail.url !== 'string'
      ) return;
      this.#routeUrl = detail.url;
      this.#forKind('route.location', (item) => this.#emitRoute(item));
    });
    this.#listen(window, 'keydown', (event) => {
      if (this.#interactive) this.#emitShortcut(event as KeyboardEvent);
    });
    this.#listen(window, 'pointerdown', (event) => {
      if (this.#interactive) this.#emitPointer(event as PointerEvent);
    });
    this.#listen(window, 'pointermove', (event) => {
      if (this.#interactive) this.#queuePointer(event as PointerEvent);
    });
    this.#listen(window, 'pointerup', (event) => {
      if (this.#interactive) this.#emitPointer(event as PointerEvent);
    });
    this.#listen(window, 'pointercancel', (event) => {
      if (this.#interactive) this.#emitPointer(event as PointerEvent);
    });
    this.#listen(window, 'dragover', (event) => {
      if (this.#interactive && this.#hasKind('file.drop')) event.preventDefault();
    });
    this.#listen(window, 'drop', (event) => {
      if (this.#interactive) this.#emitDrop(event as DragEvent);
    });
    this.#listen(window, 'focus', () => {
      if (this.#interactive) this.#emitText('platform.lifecycle', 'activated');
    });
    this.#listen(window, 'blur', () => {
      if (this.#interactive) this.#emitText('platform.lifecycle', 'deactivated');
    });
    this.#listen(window, 'pageshow', () => this.#emitText('platform.lifecycle', 'resumed'));
    this.#listen(window, 'pagehide', () => this.#emitText('platform.lifecycle', 'suspended'));
    this.#listen(window, 'online', () => this.#emitText('platform.lifecycle', 'online'));
    this.#listen(window, 'offline', () => this.#emitText('platform.lifecycle', 'offline'));
    this.#listen(window, 'vogui-resource-change', (event) => {
      const detail = (event as CustomEvent<unknown>).detail;
      if (
        typeof detail === 'object'
        && detail !== null
        && 'sessionId' in detail
        && detail.sessionId !== this.#sessionId
      ) return;
      const payload = detail instanceof Uint8Array ? detail : textEncoder.encode(JSON.stringify(detail ?? null));
      const resource = typeof detail === 'object'
        && detail !== null
        && 'resource' in detail
        && typeof detail.resource === 'string'
        ? detail.resource
        : null;
      this.#forKind('resource.watch', (subscription) => {
        const watched = subscription.descriptor.byteLength === 0
          ? null
          : textDecoder.decode(subscription.descriptor);
        if (watched === null || resource === null || watched === resource) {
          this.#emit(subscription, payload);
        }
      });
    });
  }

  #listen(target: EventTarget, type: string, listener: EventListener): void {
    target.addEventListener(type, listener);
    this.#listeners.push(() => target.removeEventListener(type, listener));
  }

  #forKind(kind: string, action: (subscription: VoguiHostSubscription) => void): void {
    for (const subscription of this.#subscriptions.values()) {
      if (subscription.kind === kind) action(subscription);
    }
  }

  #hasKind(kind: string): boolean {
    return [...this.#subscriptions.values()].some((subscription) => subscription.kind === kind);
  }

  #emit(subscription: VoguiHostSubscription, payload: Uint8Array): void {
    this.#tail = this.#tail
      .then(() => this.#submit(subscription, payload))
      .catch((error) => console.error('[studio-gui] Vogui subscription event failed', error));
  }

  #emitAll(kind: string, payload: Uint8Array): void {
    this.#forKind(kind, (subscription) => this.#emit(subscription, payload));
  }

  #emitText(kind: string, value: string): void {
    this.#emitAll(kind, textEncoder.encode(value));
  }

  #emitResize(subscription: VoguiHostSubscription): void {
    const payload = new Uint8Array(24);
    const view = new DataView(payload.buffer);
    view.setFloat64(0, window.innerWidth, true);
    view.setFloat64(8, window.innerHeight, true);
    view.setFloat64(16, window.devicePixelRatio, true);
    this.#emit(subscription, payload);
  }

  #emitVisibility(subscription: VoguiHostSubscription): void {
    this.#emit(subscription, new Uint8Array([document.hidden ? 2 : 1]));
  }

  #emitRoute(subscription: VoguiHostSubscription): void {
    this.#emit(subscription, textEncoder.encode(this.#routeUrl));
  }

  #emitShortcut(event: KeyboardEvent): void {
    const payload = textEncoder.encode(JSON.stringify({
      key: event.key,
      code: event.code,
      alt: event.altKey,
      control: event.ctrlKey,
      meta: event.metaKey,
      shift: event.shiftKey,
      repeat: event.repeat,
    }));
    const winner = [...this.#subscriptions.values()]
      .filter((subscription) => (
        subscription.kind === 'global.shortcut'
        && matchesShortcut(shortcutDescriptor(subscription.descriptor).accelerator, event)
      ))
      .map((subscription) => ({ subscription, descriptor: shortcutDescriptor(subscription.descriptor) }))
      .sort((left, right) => (
        right.descriptor.priority - left.descriptor.priority
        || shortcutScopeRank(right.descriptor.scope) - shortcutScopeRank(left.descriptor.scope)
        || subscriptionIdentity(left.subscription).localeCompare(subscriptionIdentity(right.subscription))
      ))[0];
    if (winner !== undefined) {
      event.preventDefault();
      this.#emit(winner.subscription, payload);
    }
  }

  #emitPointer(event: PointerEvent): void {
    this.#emitAll('pointer.stream', encodePointer(event));
  }

  #queuePointer(event: PointerEvent): void {
    if (!this.#hasKind('pointer.stream')) return;
    this.#pendingPointer = encodePointer(event);
    if (this.#pointerFrame !== null) return;
    this.#pointerFrame = requestAnimationFrame(() => {
      this.#pointerFrame = null;
      const payload = this.#pendingPointer;
      this.#pendingPointer = null;
      if (payload !== null) this.#emitAll('pointer.stream', payload);
    });
  }

  #emitDrop(event: DragEvent): void {
    if (!this.#hasKind('file.drop')) return;
    event.preventDefault();
    const files = [...(event.dataTransfer?.files ?? [])].map((file) => ({
      name: file.name,
      size: file.size,
      type: file.type,
      lastModified: file.lastModified,
    }));
    this.#emitAll('file.drop', textEncoder.encode(JSON.stringify(files)));
  }

  #scheduleAnimation(): void {
    if (!this.#hasKind('animation.clock')) {
      if (this.#animationFrame !== null) cancelAnimationFrame(this.#animationFrame);
      this.#animationFrame = null;
      return;
    }
    if (this.#animationFrame !== null) return;
    this.#animationFrame = requestAnimationFrame((timestamp) => {
      this.#animationFrame = null;
      const payload = new Uint8Array(8);
      new DataView(payload.buffer).setBigUint64(0, BigInt(Math.max(0, Math.floor(timestamp))), true);
      this.#emitAll('animation.clock', payload);
      this.#scheduleAnimation();
    });
  }
}

function subscriptionIdentity(subscription: VoguiHostSubscription): string {
  return `${hex(subscription.caller)}:${subscription.handleIndex}:${subscription.handleGeneration}`;
}

function hex(bytes: Uint8Array): string {
  return [...bytes].map((value) => value.toString(16).padStart(2, '0')).join('');
}

function sameBytes(left: Uint8Array, right: Uint8Array): boolean {
  return left.byteLength === right.byteLength && left.every((value, index) => value === right[index]);
}

type ShortcutDescriptor = Readonly<{
  accelerator: Uint8Array;
  scope: 'view' | 'window' | 'session';
  priority: number;
  classMask: bigint;
}>;

function shortcutDescriptor(bytes: Uint8Array): ShortcutDescriptor {
  const text = textDecoder.decode(bytes);
  const fields = text.split('|');
  let scope: ShortcutDescriptor['scope'] = 'session';
  let priority = 0;
  let classMask = 1n;
  for (const field of fields.slice(1)) {
    const [key, value] = field.split('=', 2);
    if (key === 'scope' && (value === 'view' || value === 'window' || value === 'session')) {
      scope = value;
    } else if (key === 'priority' && /^-?\d+$/.test(value ?? '')) {
      priority = Math.max(-0x8000, Math.min(0x7fff, Number(value)));
    } else if (key === 'class' && /^\d+$/.test(value ?? '')) {
      const parsed = BigInt(value!);
      if (parsed > 0n && parsed <= 0xffff_ffff_ffff_ffffn) classMask = parsed;
    }
  }
  return {
    accelerator: textEncoder.encode(fields[0] ?? ''),
    scope,
    priority,
    classMask,
  };
}

function shortcutScopeRank(scope: ShortcutDescriptor['scope']): number {
  return scope === 'view' ? 3 : scope === 'window' ? 2 : 1;
}

function matchesShortcut(descriptor: Uint8Array, event: KeyboardEvent): boolean {
  if (descriptor.byteLength === 0) return true;
  const parts = textDecoder.decode(descriptor).toLowerCase().split('+').map((part) => part.trim());
  const key = parts[parts.length - 1];
  if (key !== event.key.toLowerCase() && key !== event.code.toLowerCase()) return false;
  return parts.includes('alt') === event.altKey
    && (parts.includes('ctrl') || parts.includes('control')) === event.ctrlKey
    && (parts.includes('meta') || parts.includes('cmd') || parts.includes('command')) === event.metaKey
    && parts.includes('shift') === event.shiftKey;
}

function encodePointer(event: PointerEvent): Uint8Array {
  return textEncoder.encode(JSON.stringify({
    type: event.type,
    pointerId: event.pointerId,
    pointerType: event.pointerType,
    primary: event.isPrimary,
    x: event.clientX,
    y: event.clientY,
    buttons: event.buttons,
    pressure: event.pressure,
    tiltX: event.tiltX,
    tiltY: event.tiltY,
    twist: event.twist,
  }));
}
