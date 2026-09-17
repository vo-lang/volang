import { waitForFiniteMotion } from './motion.js';

// The pinned DOM declarations predate the standard source option.
interface AnchoredPopover extends HTMLElement { showPopover(options?: { source: HTMLElement }): void }
interface StyleValue { previous: string; priority: string; applied: string }
interface Binding {
  element: HTMLElement;
  anchor: HTMLElement;
  anchorID: number;
  placement: string;
  styles: Map<string, StyleValue>;
  side: string | null;
}
interface Retirement { binding: Binding; cancel(): void }

/** Native nonmodal layers with scoped positioning and managed range movement. */
export class PopoverHost {
  private readonly active = new Map<number, Binding>();
  private readonly retiring = new Map<number, Retirement>();
  private readonly presented = new Set<number>();
  private readonly opening = new Set<number>();
  private observer: ResizeObserver | undefined;
  private readonly observed = new Set<Element>();
  private listeners: AbortController | undefined;
  private frame: number | undefined;
  constructor(private readonly document: Document) {}

  begin(): void {
    this.presented.clear();
    this.opening.clear();
    for (const [id, binding] of this.active) if (binding.element.matches(':popover-open')) this.presented.add(id);
  }
  set(id: number, element: HTMLElement, anchorID: number, anchor: HTMLElement | undefined, placement: string, open: boolean): void {
    if (!open || !anchor) {
      this.remove(new Set([id]), !!anchor);
      if (element.matches(':popover-open')) element.hidePopover();
      return;
    }
    let binding = this.active.get(id);
    if (!binding) {
      const retirement = this.retiring.get(id);
      if (retirement) {
        this.retiring.delete(id);
        retirement.cancel();
        if (retirement.binding.element === element) binding = retirement.binding;
        else this.restore(retirement.binding);
      }
      binding ??= { element, anchor, anchorID, placement, styles: new Map(), side: element.getAttribute('data-ui-popover-side') };
      this.active.set(id, binding);
      this.opening.add(id);
    }
    binding.anchor = anchor; binding.anchorID = anchorID; binding.placement = placement;
    this.presented.add(id);
  }
  /** True when a newly opened layer deliberately took focus. */
  settle(): boolean {
    let focused = false;
    for (const group of this.layers()) {
      // Visibility reads precede mutations within each independent layer. Nested
      // anchors are measured only after their containing popup has been placed.
      const visible = group.map(([id, binding]) => ({id, binding, visible: this.anchorVisible(binding) && binding.element.isConnected}));
      const positioned: Binding[] = [];
      for (const {id, binding, visible: shown} of visible) {
        if (!shown) { this.remove(new Set([id])); continue; }
        if (this.presented.has(id) && !binding.element.matches(':popover-open')) {
          const before = this.document.activeElement;
          if (this.opening.has(id) && binding.element.popover !== 'hint') binding.anchor.focus({ preventScroll: true });
          (binding.element as AnchoredPopover).showPopover({ source: binding.anchor });
          if (this.opening.has(id) && before !== this.document.activeElement) focused = true;
        }
        if (binding.element.matches(':popover-open')) positioned.push(binding);
      }
      this.position(positioned);
    }
    this.sync();
    return focused;
  }
  private style(binding: Binding, name: string, value: string): void {
    const style = binding.element.style;
    let owned = binding.styles.get(name);
    if (!owned) {
      owned = { previous: style.getPropertyValue(name), priority: style.getPropertyPriority(name), applied: value };
      binding.styles.set(name, owned);
    }
    owned.applied = value;
    if (style.getPropertyValue(name) !== value) style.setProperty(name, value);
  }
  private layers(): [number, Binding][][] {
    const pending = [...this.active];
    const layers: [number, Binding][][] = [];
    while (pending.length) {
      let layer = pending.filter(([, binding]) => !pending.some(([, parent]) => parent !== binding && parent.element.contains(binding.anchor)));
      // Malformed mutual anchors cannot make the host loop indefinitely.
      if (!layer.length) layer = [...pending];
      layers.push(layer);
      const placed = new Set(layer.map(([id]) => id));
      for (let i = pending.length - 1; i >= 0; i--) if (placed.has(pending[i][0])) pending.splice(i, 1);
    }
    return layers;
  }
  private position(bindings: Binding[]): void {
    const window = this.document.defaultView!;
    const viewport = window.visualViewport;
    const left = (viewport?.offsetLeft ?? 0) + 8, top = (viewport?.offsetTop ?? 0) + 8;
    const right = left + (viewport?.width ?? window.innerWidth) - 16;
    const bottom = top + (viewport?.height ?? window.innerHeight) - 16;
    for (const binding of bindings) {
      this.style(binding, 'position', 'fixed');
      this.style(binding, 'right', 'auto');
      this.style(binding, 'bottom', 'auto');
      this.style(binding, 'margin', '0px');
      this.style(binding, '--ui-popover-width', `${Math.max(0, right - left)}px`);
      this.style(binding, '--ui-popover-height', `${Math.max(0, bottom - top)}px`);
    }
    const anchors = bindings.map(binding => ({binding, anchor: binding.anchor.getBoundingClientRect(), rtl: window.getComputedStyle(binding.anchor).direction === 'rtl'}));
    for (const {binding, anchor} of anchors) this.style(binding, '--ui-popover-anchor-width', `${anchor.width}px`);
    const measured = anchors.map(item => ({...item, popup: item.binding.element.getBoundingClientRect()}));
    for (const {binding, anchor, rtl, popup} of measured) {
      let [side, align = 'start'] = binding.placement.split('-');
      if (side === 'bottom' && bottom - anchor.bottom - 8 < popup.height && anchor.top - top > bottom - anchor.bottom) side = 'top';
      else if (side === 'top' && anchor.top - top - 8 < popup.height && bottom - anchor.bottom > anchor.top - top) side = 'bottom';
      else if (side === 'right' && right - anchor.right - 8 < popup.width && anchor.left - left > right - anchor.right) side = 'left';
      else if (side === 'left' && anchor.left - left - 8 < popup.width && right - anchor.right > anchor.left - left) side = 'right';
      let x = (align === 'end') !== rtl ? anchor.right - popup.width : anchor.left;
      let y = side === 'top' ? anchor.top - popup.height - 8 : anchor.bottom + 8;
      if (side === 'left' || side === 'right') {
        x = side === 'left' ? anchor.left - popup.width - 8 : anchor.right + 8;
        y = anchor.top + (anchor.height - popup.height) / 2;
      }
      x = Math.max(left, Math.min(x, right - popup.width));
      y = Math.max(top, Math.min(y, bottom - popup.height));
      this.style(binding, 'left', `${Math.round(x)}px`);
      this.style(binding, 'top', `${Math.round(y)}px`);
      binding.element.setAttribute('data-ui-popover-side', side);
    }
  }
  private readonly schedule = (): void => {
    if (this.frame !== undefined) return;
    this.frame = this.document.defaultView!.requestAnimationFrame(() => {
      this.frame = undefined;
      for (const group of this.layers()) {
        const visible = group.map(([id, binding]) => ({id, binding, visible: this.anchorVisible(binding)}));
        const positioned: Binding[] = [];
        for (const {id, binding, visible: shown} of visible) {
          if (!shown) this.remove(new Set([id]));
          else if (binding.element.matches(':popover-open')) positioned.push(binding);
        }
        this.position(positioned);
      }
    });
  };
  private anchorVisible(binding: Binding): boolean {
    return binding.anchor.isConnected && binding.anchor.getClientRects().length > 0
      && this.document.defaultView!.getComputedStyle(binding.anchor).visibility !== 'hidden';
  }
  private readonly nativeToggle = (event: Event): void => {
    for (const [id, binding] of this.active) {
      // Coalesced notifications from a moved/reopened range observe live state.
      if (event.target === binding.element && !binding.element.matches(':popover-open')) { this.remove(new Set([id]), true); return; }
    }
  };
  private sync(): void {
    if (!this.active.size) {
      this.observer?.disconnect(); this.observer = undefined;
      this.observed.clear();
      this.listeners?.abort(); this.listeners = undefined;
      if (this.frame !== undefined) this.document.defaultView!.cancelAnimationFrame(this.frame);
      this.frame = undefined;
      return;
    }
    if (!this.listeners) {
      this.listeners = new AbortController();
      const options = { signal: this.listeners.signal, passive: true };
      const window = this.document.defaultView!;
      window.addEventListener('resize', this.schedule, options);
      this.document.addEventListener('scroll', this.schedule, { ...options, capture: true });
      this.document.addEventListener('toggle', this.nativeToggle, { ...options, capture: true });
      window.visualViewport?.addEventListener('resize', this.schedule, options);
      window.visualViewport?.addEventListener('scroll', this.schedule, options);
      this.observer = new ResizeObserver(this.schedule);
    }
    const targets = new Set<Element>();
    for (const binding of this.active.values()) { targets.add(binding.anchor); targets.add(binding.element); }
    for (const element of this.observed) if (!targets.has(element)) {
      this.observer!.unobserve(element); this.observed.delete(element);
    }
    for (const element of targets) if (!this.observed.has(element)) {
      this.observer!.observe(element); this.observed.add(element);
    }
  }
  private restore(binding: Binding): void {
    for (const [name, owned] of binding.styles) {
      if (binding.element.style.getPropertyValue(name) !== owned.applied) continue;
      if (owned.previous) binding.element.style.setProperty(name, owned.previous, owned.priority);
      else binding.element.style.removeProperty(name);
    }
    if (binding.side === null) binding.element.removeAttribute('data-ui-popover-side');
    else binding.element.setAttribute('data-ui-popover-side', binding.side);
  }
  private retire(id: number, binding: Binding): void {
    const controller = new AbortController();
    let motion: Promise<void> | undefined;
    try {
      if (binding.element.isConnected && this.anchorVisible(binding)) motion = waitForFiniteMotion(binding.element, controller.signal);
    } catch { /* Excessive or unavailable motion cannot retain positioning. */ }
    if (!motion) { this.restore(binding); return; }
    const window = this.document.defaultView!;
    const timer = window.setTimeout(() => controller.abort(), 5000);
    const retirement = { binding, cancel: () => { window.clearTimeout(timer); controller.abort(); } };
    this.retiring.set(id, retirement);
    const finish = (): void => {
      if (this.retiring.get(id) !== retirement) return;
      this.retiring.delete(id);
      retirement.cancel();
      this.restore(binding);
    };
    void motion.then(finish, finish);
  }
  remove(ids: ReadonlySet<number>, animate = false): void {
    const bindings = new Map([...this.retiring].map(([id, entry]) => [id, entry.binding] as const));
    for (const [id, binding] of this.active) bindings.set(id, binding);
    const ancestors = [...ids].map(id => bindings.get(id)?.element).filter(element => element !== undefined);
    for (const [id, binding] of [...bindings].reverse()) {
      if (!ids.has(id) && !ids.has(binding.anchorID) && !ancestors.some(ancestor => ancestor.contains(binding.element))) continue;
      const retirement = this.retiring.get(id);
      if (retirement) {
        if (animate) continue;
        this.retiring.delete(id);
        retirement.cancel();
      }
      this.active.delete(id); this.presented.delete(id); this.opening.delete(id);
      if (binding.element.matches(':popover-open')) binding.element.hidePopover();
      if (animate) this.retire(id, binding);
      else this.restore(binding);
    }
    this.sync();
  }
  close(): void { this.remove(new Set([...this.active.keys(), ...this.retiring.keys()])); this.presented.clear(); this.opening.clear(); }
}
