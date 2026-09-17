export interface WidgetInstance {
  update(value: string): void;
  /** Read committed native controls/layout after this root's DOM batch. */
  afterCommit?(): void;
  /** Optional native-control dependencies. Their own and ancestor mutations
   * trigger afterCommit; structural batches always trigger it. Omit for widgets
   * that depend on arbitrary document/layout changes. Empty means value-only. */
  commitTargets?(): readonly Node[] | undefined;
  dispose(): void;
}
export interface WidgetContext {
  element: HTMLElement;
  value: string;
  signal: AbortSignal;
  emit(value: string): void;
  /** End this instance and publish a local failure, including from async work. */
  fail(message: string): void;
}
export type WidgetProvider = (context: WidgetContext) => WidgetInstance;
export type WidgetProviders = Readonly<Record<string, WidgetProvider>>;
interface Binding { name: string; controller: AbortController; instance?: WidgetInstance; targets?: Set<Node> }

export class WidgetHost {
  private readonly bindings = new Map<number, Binding>();
  private readonly providers: WidgetProviders;
  private readonly watchers = new Map<Node, Set<number>>();
  private readonly globalCommits = new Set<number>();
  private readonly pendingCommits = new Set<number>();

  constructor(private readonly emit: (id: number, value: string, error: string) => void, providers: WidgetProviders = {}) {
    this.providers = Object.freeze({ ...providers });
  }
  supports(name: string): boolean { return Object.prototype.hasOwnProperty.call(this.providers, name); }

  apply(id: number, element: HTMLElement, name: string, value: string): void {
    let binding = this.bindings.get(id);
    try {
      if (binding) {
        if (binding.name !== name) throw new Error('widget type changed without a new identity');
        binding.instance!.update(value);
        if (this.bindings.get(id) === binding) this.pendingCommits.add(id);
        return;
      }
      binding = { name, controller: new AbortController() };
      this.bindings.set(id, binding);
      const current = binding;
      const emit = (value: string): void => {
        if (this.bindings.get(id) === current && !current.controller.signal.aborted) this.emit(id, value, '');
      };
      const instance = this.providers[name]({ element, value, signal: binding.controller.signal, emit,
        fail: message => this.fail(id, current, message) });
      // A provider may fail or remove its root during its own installation.
      if (this.bindings.get(id) === current) {
        current.instance = instance;
        this.pendingCommits.add(id);
        this.observe(id, current);
      }
      else instance.dispose();
    } catch (error) {
      if (binding) this.fail(id, binding, String((error as Error)?.message ?? error));
    }
  }

  private fail(id: number, binding: Binding, message: string): void {
    if (this.bindings.get(id) !== binding) return;
    this.remove(id);
    this.emit(id, '', message);
  }

  private unobserve(id: number, binding: Binding): void {
    this.globalCommits.delete(id);
    for (const target of binding.targets ?? []) {
      const ids = this.watchers.get(target)!;
      ids.delete(id);
      if (!ids.size) this.watchers.delete(target);
    }
    binding.targets = undefined;
  }

  private observe(id: number, binding: Binding): void {
    this.unobserve(id, binding);
    if (!binding.instance?.afterCommit) return;
    const targets = binding.instance.commitTargets?.();
    if (this.bindings.get(id) !== binding) return;
    if (targets === undefined) {this.globalCommits.add(id); return;}
    const observed = binding.targets = new Set<Node>();
    for (const target of targets) for (let node: Node | null = target; node; node = node.parentNode) {
      if (observed.has(node)) break;
      observed.add(node);
      let ids = this.watchers.get(node);
      if (!ids) this.watchers.set(node, ids = new Set());
      ids.add(id);
    }
  }

  afterCommit(changed?: ReadonlySet<Node>, structural = false): void {
    const ids = changed === undefined || structural ? new Set(this.bindings.keys()) : new Set(this.globalCommits);
    for (const id of this.pendingCommits) ids.add(id);
    for (const node of changed ?? []) for (const id of this.watchers.get(node) ?? []) ids.add(id);
    this.pendingCommits.clear();
    const selected = [...ids].map(id => [id, this.bindings.get(id)] as const);
    for (const [id, binding] of selected) {
      if (!binding) continue;
      if (this.bindings.get(id) !== binding) continue;
      try {
        binding.instance?.afterCommit?.();
        if (this.bindings.get(id) === binding) this.observe(id, binding);
      }
      catch (error) { this.fail(id, binding, String((error as Error)?.message ?? error)); }
    }
  }

  remove(id: number): void {
    this.removeMany([id]);
  }

  removeMany(ids: number[]): void {
    const removed: [number, Binding][] = [];
    for (const id of ids) {
      const binding = this.bindings.get(id);
      if (binding) {
        removed.push([id, binding]); this.bindings.delete(id);
        this.unobserve(id, binding); this.pendingCommits.delete(id);
      }
    }
    for (const [, binding] of removed) binding.controller.abort();
    for (const [id, binding] of removed) {
      try { binding.instance?.dispose(); }
      catch (error) { this.emit(id, '', `widget cleanup: ${String((error as Error)?.message ?? error)}`); }
    }
  }

  close(): void { this.removeMany([...this.bindings.keys()]); }
}
