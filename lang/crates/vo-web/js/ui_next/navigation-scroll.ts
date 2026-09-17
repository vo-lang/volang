/** Optional ownership of the window viewport, synchronized with guest commits. */
export const locationChanged = 'volang:locationchange';
const stateKey = '__volangUiNavigation';
const owners = new WeakMap<Window, ScrollNavigation>();
let nextIdentity = 0;
interface Position { x: number; y: number; focus: string }
interface Entry { key: string; position?: Position }
type Intent = 'initial' | 'native' | 'push' | 'replace' | 'pop';

function entry(state: unknown): Entry | undefined {
  if (!state || typeof state !== 'object') return;
  const value = (state as Record<string, unknown>)[stateKey] as Entry | undefined;
  if (value && typeof value.key === 'string') return value;
}
function stateWith(state: unknown, value: Entry): object {
  if (state != null && Object.prototype.toString.call(state) !== '[object Object]') {
    throw new Error('Navigation restoration requires null or object history state.');
  }
  return { ...(state ?? {}), [stateKey]: value };
}
function identity(): string { return `ui-${Date.now().toString(36)}-${++nextIdentity}`; }
function validPosition(value: Position | undefined): value is Position {
  return !!value && Number.isFinite(value.x) && Number.isFinite(value.y) && typeof value.focus === 'string';
}

/** Capture the outgoing viewport before history changes; other state fields survive replace. */
export function navigationState(window: Window, replace: boolean): unknown {
  const owner = owners.get(window);
  if (!owner) return replace ? window.history.state : null;
  owner.capture(true);
  return stateWith(replace ? window.history.state : null, { key: identity() });
}
export function captureNativeNavigation(window: Window, href: string, event: MouseEvent): void { owners.get(window)?.beforeNative(href, event); }

export function watchNavigation(container: HTMLElement, focusID: string, signal: AbortSignal, emit: (value: string) => void): void {
  const window = container.ownerDocument.defaultView!;
  if (owners.has(window)) throw new Error('Only one navigation viewport owner may be active in a window.');
  if (signal.aborted) return;
  const owner = new ScrollNavigation(window, container, focusID, emit);
  owners.set(window, owner);
  const close = (): void => { owner.close(); if (owners.get(window) === owner) owners.delete(window); };
  signal.addEventListener('abort', close, { once: true });
  try { owner.start(); }
  catch (error) { close(); throw error; }
}

export function commitNavigation(container: HTMLElement, value: string, signal: AbortSignal): Promise<string> {
  const request = JSON.parse(value);
  if (typeof request.observer !== 'string' || !Number.isSafeInteger(request.revision) || typeof request.url !== 'string') {
    throw new Error('Invalid navigation commit.');
  }
  // Native history dispatch can run microtasks before finishing its default
  // viewport action. A posted task completes that turn before applying ours.
  return new Promise((resolve, reject) => {
    const channel = new MessageChannel();
    const release = (): void => {
      channel.port1.onmessage = null;
      channel.port1.close(); channel.port2.close(); signal.removeEventListener('abort', cancel);
    };
    const cancel = (): void => { release(); resolve(''); };
    channel.port1.onmessage = () => {
      release();
      try {
        const owner = owners.get(container.ownerDocument.defaultView!);
        if (owner?.container === container) owner.commit(request.observer, request.revision, request.url);
        resolve('');
      } catch (error) { reject(error); }
    };
    signal.addEventListener('abort', cancel, { once: true });
    if (signal.aborted) cancel(); else channel.port2.postMessage(null);
  });
}

class ScrollNavigation {
  private readonly observer = identity();
  private readonly controller = new AbortController();
  private readonly positions = new Map<string, Position>();
  private previousRestoration: ScrollRestoration;
  private url = '';
  private key = '';
  private revision = 0;
  private committed = 0;
  private intent: Intent = 'initial';
  private preserve = false;
  private preserveFocus = false;
  private position: Position = { x: 0, y: 0, focus: '' };
  private nativeCaptured: MouseEvent | undefined;
  private stopResize: (() => void) | undefined;
  private closed = false;

  constructor(private readonly window: Window, readonly container: HTMLElement, private readonly focusID: string,
    private readonly emit: (value: string) => void) {
    this.previousRestoration = window.history.scrollRestoration;
  }
  private current(): string { const { pathname, search, hash } = this.window.location; return pathname + search + hash; }
  private ensureEntry(): Entry {
    const existing = entry(this.window.history.state);
    if (existing) return existing;
    const fresh = { key: identity() };
    this.window.history.replaceState(stateWith(this.window.history.state, fresh), '');
    return fresh;
  }
  start(): void {
    const { window } = this;
    this.key = this.ensureEntry().key;
    this.url = this.current();
    window.history.scrollRestoration = 'manual';
    const options = { signal: this.controller.signal };
    window.addEventListener(locationChanged, event => {
      const detail = (event as CustomEvent<{ replace: boolean; preserveScroll: boolean; preserveFocus: boolean }>).detail;
      this.changed(detail?.replace ? 'replace' : 'push', detail?.preserveScroll ?? false, detail?.preserveFocus ?? false);
    }, options);
    window.addEventListener('popstate', () => this.changed('pop', false), options);
    window.addEventListener('hashchange', () => this.changed('pop', false), options);
    window.addEventListener('scroll', () => { if (!this.nativeCaptured && this.current() === this.url) this.capture(false); }, options);
    this.publish();
    // Subscription setup already follows the initial DOM commit. Preserve that
    // viewport even when the first user action precedes the metadata round trip.
    this.committed = this.revision;
    this.capture(false);
    // A client-rendered document can load its fragment after this first commit.
    // Existing targets retain the browser's initial positioning and user input.
    if (window.location.hash && !this.fragmentTarget()) this.restoreWhenReady(() => this.restoreFragment());
  }
  private publish(): void {
    this.revision++;
    this.emit(JSON.stringify({ observer: this.observer, revision: this.revision, url: this.url }));
  }
  private changed(intent: Intent, preserve: boolean, preserveFocus = false): void {
    const url = this.current();
    const known = entry(this.window.history.state);
    let destination = this.ensureEntry();
    if (url === this.url && destination.key === this.key) return;
    this.stopResize?.();
    const oldURL = new URL(this.url, this.window.location.href);
    const hashOnly = oldURL.pathname + oldURL.search === this.window.location.pathname + this.window.location.search;
    if (intent === 'pop' && (this.nativeCaptured || (!known && hashOnly))) intent = 'native';
    if (intent === 'pop') this.capture(false);
    if (destination.key === this.key) {
      // A new native fragment entry inherits history.state from its predecessor.
      destination = { key: identity() };
      this.window.history.replaceState(stateWith(this.window.history.state, destination), '');
      intent = 'native';
    }
    this.nativeCaptured = undefined;
    const previous = this.positions.get(this.key);
    this.key = destination.key;
    this.url = url;
    this.intent = intent;
    this.preserve = preserve;
    this.preserveFocus = preserveFocus;
    const saved = this.positions.get(this.key) ?? destination.position;
    this.position = intent === 'pop' && validPosition(saved) ? saved
      : preserve && previous ? previous : { x: 0, y: 0, focus: '' };
    this.publish();
  }
  capture(persist: boolean): void {
    if (this.committed !== this.revision || this.stopResize) return;
    const active = this.container.ownerDocument.activeElement;
    const position = { x: this.window.scrollX, y: this.window.scrollY,
      focus: active && this.container.contains(active) ? active.id : '' };
    this.positions.delete(this.key);
    this.positions.set(this.key, position);
    if (this.positions.size > 128) this.positions.delete(this.positions.keys().next().value!);
    if (persist && entry(this.window.history.state)?.key === this.key) {
      this.window.history.replaceState(stateWith(this.window.history.state, { key: this.key, position }), '');
    }
  }
  beforeNative(href: string, event: MouseEvent): void {
    this.capture(true);
    this.nativeCaptured = href !== this.window.location.href ? event : undefined;
    // A later application listener may suppress the native default action.
    setTimeout(() => { if (event.defaultPrevented && this.nativeCaptured === event) this.nativeCaptured = undefined; }, 0);
  }
  commit(observer: string, revision: number, url: string): void {
    if (observer !== this.observer || revision !== this.revision || url !== this.url || this.committed === revision) return;
    this.committed = revision;
    if (this.intent === 'initial' || this.intent === 'native') { this.capture(false); return; }
    const document = this.container.ownerDocument;
    const desiredFocus = this.intent === 'pop' ? this.position.focus || this.focusID : this.focusID;
    let focus = this.preserveFocus && document.activeElement && this.container.contains(document.activeElement)
      ? document.activeElement as HTMLElement : document.getElementById(desiredFocus);
    if (!focus || !this.container.contains(focus)) focus = document.getElementById(this.focusID);
    if (focus && this.container.contains(focus) && document.activeElement !== focus) focus.focus({ preventScroll: true });
    if (focus && document.activeElement !== focus) {
      const fallback = document.getElementById(this.focusID);
      if (fallback && this.container.contains(fallback)) fallback.focus({ preventScroll: true });
    }
    if (!this.preserve && this.intent !== 'pop' && this.window.location.hash) {
      this.restoreWhenReady(() => this.restoreFragment());
      return;
    }
    this.restorePosition(this.position);
  }
  private restorePosition(position: Position): void {
    this.restoreWhenReady(() => {
      this.window.scrollTo({ left: position.x, top: position.y, behavior: 'instant' });
      return Math.abs(this.window.scrollY - position.y) <= 1 && Math.abs(this.window.scrollX - position.x) <= 1;
    });
  }
  private fragmentTarget(): Element | undefined {
    const document = this.container.ownerDocument;
    let fragment = this.window.location.hash.slice(1);
    try { fragment = decodeURIComponent(fragment); } catch { /* Literal fragment fallback. */ }
    const target = document.getElementById(fragment) ?? document.getElementsByName(fragment)[0];
    return target && this.container.contains(target) ? target : undefined;
  }
  private restoreFragment(): boolean {
    const target = this.fragmentTarget();
    if (!target) return false;
    target.scrollIntoView({ behavior: 'instant' });
    return true;
  }
  private restoreWhenReady(apply: () => boolean): void {
    if (apply()) { this.capture(false); return; }
    // A restored route can first commit its loading view. Follow layout growth
    // briefly, with immediate cancellation on navigation, user input or disposal.
    const controller = new AbortController();
    let finished = false;
    const observer = new ResizeObserver(() => { if (!finished && apply()) finish(); });
    const mutations = new MutationObserver(() => { if (!finished && apply()) finish(); });
    const finish = (): void => {
      if (finished) return;
      finished = true;
      observer.disconnect(); mutations.disconnect(); controller.abort(); clearTimeout(timer); this.stopResize = undefined;
      this.capture(false);
    };
    const timer = setTimeout(finish, 3000);
    this.stopResize = finish;
    for (const name of ['wheel', 'touchstart', 'pointerdown', 'keydown']) {
      this.window.addEventListener(name, finish, { signal: controller.signal, capture: true, passive: true });
    }
    observer.observe(this.container);
    observer.observe(this.container.ownerDocument.documentElement);
    mutations.observe(this.container, { childList: true, subtree: true });
  }
  close(): void {
    if (this.closed) return;
    this.closed = true;
    this.stopResize?.();
    this.controller.abort();
    this.window.history.scrollRestoration = this.previousRestoration;
    this.positions.clear();
  }
}
