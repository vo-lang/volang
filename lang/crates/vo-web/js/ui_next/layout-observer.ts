interface Observation {
  kind: 'size' | 'viewport';
  element: HTMLElement;
  report(value: string): void;
  latest?: string;
  previous?: string;
  active: boolean;
  scroll?: () => void;
}

/** Root-owned observers share one frame and separate all layout reads from delivery. */
export class LayoutObserverHost {
  private readonly sizes = new Map<Element, Observation>();
  private readonly viewports = new Map<Element, Observation>();
  private readonly pending = new Set<Observation>();
  private sizeObserver?: ResizeObserver;
  private viewportObserver?: ResizeObserver;
  private frame?: number;
  private closed = false;
  constructor(private readonly window: Window) {}

  observe(kind: 'size' | 'viewport', element: HTMLElement, report: (value: string) => void): () => void {
    if (this.closed) throw new Error('layout observer host is closed');
    const observations = kind === 'size' ? this.sizes : this.viewports;
    if (observations.has(element)) throw new Error('duplicate layout observation');
    const observation: Observation = {kind, element, report, active: true};
    observations.set(element, observation);
    if (kind === 'size') {
      this.sizeObserver ??= new ResizeObserver(entries => {
        for (const entry of entries) {
          const current = this.sizes.get(entry.target), box = entry.borderBoxSize[0];
          if (!current || !box) continue;
          current.latest = JSON.stringify({inline: box.inlineSize, block: box.blockSize});
          this.schedule(current);
        }
      });
      this.sizeObserver.observe(element, {box: 'border-box'});
    } else {
      this.viewportObserver ??= new ResizeObserver(entries => {
        for (const entry of entries) {
          const current = this.viewports.get(entry.target);
          if (current) this.schedule(current);
        }
      });
      this.viewportObserver.observe(element);
      observation.scroll = () => this.schedule(observation);
      element.addEventListener('scroll', observation.scroll, {passive: true});
      this.schedule(observation);
    }
    return () => this.remove(observation);
  }

  private schedule(observation: Observation): void {
    if (!observation.active || this.closed) return;
    this.pending.add(observation);
    this.frame ??= this.window.requestAnimationFrame(() => this.publish());
  }

  private publish(): void {
    this.frame = undefined;
    const pending = [...this.pending];
    this.pending.clear();
    const deliveries: [Observation, string][] = [];
    for (const current of pending) {
      if (!current.active || !current.element.isConnected) continue;
      const element = current.element;
      const value = current.kind === 'size' ? current.latest : JSON.stringify({
        width: element.clientWidth, height: element.clientHeight, top: element.scrollTop, left: element.scrollLeft,
      });
      if (value !== undefined && value !== current.previous) deliveries.push([current, value]);
    }
    for (const [current, value] of deliveries) {
      if (!current.active || this.closed) continue;
      current.previous = value;
      current.report(value);
    }
  }

  private remove(current: Observation): void {
    if (!current.active) return;
    current.active = false;
    this.pending.delete(current);
    const observations = current.kind === 'size' ? this.sizes : this.viewports;
    observations.delete(current.element);
    const observer = current.kind === 'size' ? this.sizeObserver : this.viewportObserver;
    observer?.unobserve(current.element);
    if (current.scroll) current.element.removeEventListener('scroll', current.scroll);
    if (!observations.size) {
      observer?.disconnect();
      if (current.kind === 'size') this.sizeObserver = undefined;
      else this.viewportObserver = undefined;
    }
    if (!this.pending.size && this.frame !== undefined) {
      this.window.cancelAnimationFrame(this.frame);
      this.frame = undefined;
    }
  }

  close(): void {
    this.closed = true;
    for (const current of [...this.sizes.values(), ...this.viewports.values()]) this.remove(current);
  }
}
