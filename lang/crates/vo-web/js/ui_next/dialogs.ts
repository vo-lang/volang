import { containTab } from './focus.js';

/** Native modal ownership. HTML supplies the modal layer and inertness. The
 * framework wraps focus traversal and retains presentation across range moves. */
const scrollLocks = new WeakMap<Document, { owners: Set<DialogHost>; value: string; priority: string }>();

export class DialogHost {
  private readonly active = new Map<number, HTMLDialogElement>();
  private readonly closing = new Set<number>();
  private readonly presented = new Set<number>();
  private readonly opening = new Set<number>();
  private listening = false;
  private readonly keydown = (event: KeyboardEvent): void => {
    const target = event.target as Element | null;
    const dialog = target?.closest?.('dialog:modal');
    if (dialog && [...this.active.values()].includes(dialog as HTMLDialogElement)) containTab(event, dialog as HTMLElement);
  };
  private readonly nativeClose = (event: Event): void => {
    for (const [id, element] of this.active) {
      // Moving a live range can queue a close before the same dialog reopens.
      if (event.target === element && !element.open) { this.remove(new Set([id])); return; }
    }
  };
  constructor(private readonly document: Document) {}

  begin(): void {
    this.presented.clear();
    this.opening.clear();
    for (const [id, element] of this.active) if (element.open) this.presented.add(id);
  }

  set(id: number, element: HTMLDialogElement, open: boolean): void {
    if (open) {
      if (!this.presented.has(id)) this.opening.add(id);
      this.closing.delete(id); this.active.set(id, element); this.presented.add(id);
    }
    else this.closing.add(id);
  }

  /** True when a newly opened modal deliberately changed focus. Retained range
   * moves still allow the renderer to restore the previous control/selection. */
  settle(): boolean {
    let focused = false;
    if (this.closing.size) { this.remove(this.closing); this.closing.clear(); }
    if (!this.active.size) return false;
    for (const [id, element] of this.active) {
      if (this.presented.has(id) && !element.matches(':modal')) {
        const before = this.document.activeElement;
        if (element.open) element.close();
        element.showModal();
        if (this.opening.has(id) && before !== this.document.activeElement) focused = true;
      }
    }
    this.syncScroll();
    return focused;
  }

  remove(ids: ReadonlySet<number>): void {
    if (!ids.size) return;
    const ancestors = [...ids].map(id => this.active.get(id)).filter(element => element !== undefined);
    for (const [id, element] of [...this.active].reverse()) {
      if (!ids.has(id) && !ancestors.some(ancestor => ancestor.contains(element))) continue;
      this.active.delete(id);
      this.presented.delete(id);
      this.opening.delete(id);
      if (element.open) element.close();
    }
    this.syncScroll();
  }

  close(): void { this.remove(new Set(this.active.keys())); this.closing.clear(); this.presented.clear(); this.opening.clear(); }

  private syncScroll(): void {
    const style = this.document.documentElement.style;
    let lock = scrollLocks.get(this.document);
    if (Boolean(this.active.size) !== this.listening) {
      if (this.active.size) {
        this.document.addEventListener('keydown', this.keydown);
        this.document.addEventListener('close', this.nativeClose, true);
      } else {
        this.document.removeEventListener('keydown', this.keydown);
        this.document.removeEventListener('close', this.nativeClose, true);
      }
      this.listening = Boolean(this.active.size);
    }
    if ([...this.active.values()].some(element => element.open)) {
      if (!lock) {
        lock = { owners: new Set(), value: style.getPropertyValue('overflow'), priority: style.getPropertyPriority('overflow') };
        scrollLocks.set(this.document, lock);
        style.setProperty('overflow', 'hidden');
      }
      lock.owners.add(this);
    } else {
      if (!lock) return;
      lock.owners.delete(this);
      if (!lock.owners.size) {
        if (style.getPropertyValue('overflow') === 'hidden' && style.getPropertyPriority('overflow') === '') {
          if (lock.value) style.setProperty('overflow', lock.value, lock.priority); else style.removeProperty('overflow');
        }
        scrollLocks.delete(this.document);
      }
    }
  }
}
