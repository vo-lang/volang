/** Experimental DOM boundary. Component state and reconciliation live in Vo. */
import type { Mutation, Batch, Event as UiEvent } from './generated/protocol.js';
import { decodeBatch } from './generated/codec.js';
import { WidgetHost } from './widgets.js';
import type { WidgetProviders } from './widgets.js';
import { htmlNamespace, booleanAttributes } from './elements.js';
import { layoutObservation, eventBinding, matchesKeyboard, removeListener } from './events.js';
import { nodeAt } from './nodes.js';
import type { NodeEntry as Entry } from './nodes.js';
import { prepareBatch } from './topology.js';
import { adoptHtml } from './hydrate.js';
import { PointerHost, pointerData } from './pointers.js';
import { DialogHost } from './dialogs.js';
import { PopoverHost } from './popovers.js';
import { PortalHost } from './portals.js';
import { LayoutObserverHost } from './layout-observer.js';
import { activeElement, captureContentSelection, composedContains, focusAvailable } from './focus.js';
import { selectedValues, applySelection } from './selection.js';
import {applyTextSelection} from './text-selection.js';
import {mediaSource, type MediaSources} from './media-sources.js';
export type { Mutation, Batch, Event as UiEvent } from './generated/protocol.js';
export { decodeBatch } from './generated/codec.js';
export { InputQueue } from './input.js';

const deferredOperations = new Set<Mutation['op']>(['widget', 'focus', 'scroll', 'scrollPosition', 'textSelection', 'modal', 'popover', 'default', 'selection', 'portal', 'property']);

function pauseMedia(entry: Entry): void {
  if (entry.namespace !== htmlNamespace || (entry.kind !== 'audio' && entry.kind !== 'video')) return;
  const media = entry.start as HTMLMediaElement;
  if (!media.paused) media.pause();
}

export class DomRenderer {
  private readonly nodes = new Map<number, Entry>();
  private readonly ids = new WeakMap<Node, number>();
  private readonly pendingInputs = new Set<number>();
  private readonly unmanagedCompositions = new WeakSet<EventTarget>();
  private readonly resetOutputs = new Map<number, Node[]>();
  private sequence = 0;
  private revision = 0;
  private inputSequence = 0;
  private highestId = 0;
  private hydrating = false;
  private commitAdoption?: () => void;
  private closed = false;
  private readonly widgets: WidgetHost;
  private readonly dialogs: DialogHost;
  private readonly popovers: PopoverHost;
  private readonly layout: LayoutObserverHost;
  private readonly portals = new PortalHost(this.nodes);
  private readonly pointers = new PointerHost();
  private readonly compositionStart: EventListener;
  private readonly compositionEnd: EventListener;
  private readonly formReset: EventListener;
  private readonly unhandledControlEdit: EventListener;

  constructor(private readonly container: HTMLElement, private readonly send: (event: UiEvent, latest?: boolean) => void, hydrate = false, widgets: WidgetProviders = {}, private readonly mediaSources?: MediaSources) {
    if (container.childNodes.length !== 0 && !hydrate) throw new Error('UI root must be empty before mounting');
    this.widgets = new WidgetHost((id, value, error) => this.post('@widget', id, value, error), widgets);
    this.dialogs = new DialogHost(container.ownerDocument);
    this.popovers = new PopoverHost(container.ownerDocument);
    this.layout = new LayoutObserverHost(container.ownerDocument.defaultView!);
    this.nodes.set(0, { start: container, end: container, kind: '#root', namespace: htmlNamespace, parent: null,
      children: [], listeners: new Map(), editedAt: 0, composing: false });
    this.compositionStart = (event) => {
      const entry = this.entryFor(event.target);
      if (entry) entry.composing = true;
      else if (event.target) this.unmanagedCompositions.add(event.target);
    };
    this.compositionEnd = (event) => {
      const entry = this.entryFor(event.target);
      if (event.target) this.unmanagedCompositions.delete(event.target);
      if (!entry) return;
      entry.composing = false;
      this.replayInput(entry, false);
    };
    this.formReset = event => {
      let affected = false;
      const sequence = this.sequence + 1;
      for (const [id, entry] of this.nodes) {
        if (entry.namespace === htmlNamespace && entry.kind === 'output' && (entry.start as HTMLOutputElement).form === event.target) {
          // WebKit's native reset replaces output children, including managed
          // text/range nodes. Retain the original topology until acknowledgement.
          // Consecutive resets must keep the first, still-managed snapshot.
          if (!this.resetOutputs.has(id)) this.resetOutputs.set(id, [...entry.start.childNodes]);
          affected = true;
          continue;
        }
        if (entry.namespace !== htmlNamespace || !['input', 'textarea', 'select'].includes(entry.kind)
          || (entry.start as HTMLInputElement).form !== event.target
          || (entry.desiredValue === undefined && entry.desiredChecked === undefined && entry.desiredIndeterminate === undefined && entry.desiredSelection === undefined)) continue;
        entry.editedAt = sequence;
        this.pendingInputs.add(id);
        affected = true;
      }
      // Reset's native default action runs after its listeners. Queue one input
      // acknowledgement so controlled values settle even without an on-reset
      // handler. Uncontrolled fields retain the browser's reset behavior.
      if (affected) this.post('@reset', 0, '');
    };
    this.unhandledControlEdit = event => {
      const entry = this.entryFor(event.target);
      if (!entry || entry.namespace !== htmlNamespace || !['input', 'textarea', 'select'].includes(entry.kind)) return;
      const control = entry.start as HTMLInputElement;
      const checkedControl = entry.kind === 'input' && ['checkbox', 'radio'].includes(control.type);
      if (checkedControl ? entry.desiredChecked === undefined && entry.desiredIndeterminate === undefined : entry.desiredValue === undefined && entry.desiredSelection === undefined) return;
      if (['input', 'change', 'input:capture', 'change:capture'].some(kind => entry.listeners.has(kind))) return;
      // Controlled values still need an acknowledgement when there is no guest
      // input handler. Default-only fields retain native editing without work.
      const id = this.ids.get(entry.start)!;
      entry.editedAt = this.sequence + 1;
      this.pendingInputs.add(id);
      this.post('@control', id, '');
    };
    container.addEventListener('compositionstart', this.compositionStart, true);
    container.addEventListener('compositionend', this.compositionEnd, true);
    container.ownerDocument.addEventListener('reset', this.formReset, true);
    container.addEventListener('input', this.unhandledControlEdit, true);
    container.addEventListener('change', this.unhandledControlEdit, true);
    if (hydrate) {
      this.hydrating = true;
      try { this.commitAdoption = adoptHtml(container, this.nodes, this.ids, {
        bind: mutation => this.applyMutation(mutation), replay: entry => this.replayInput(entry),
      }); }
      catch (error) { this.close(false); throw error; }
    }
  }


  private entryFor(target: EventTarget | null): Entry | undefined {
    const id = target === null ? undefined : this.ids.get(target as Node);
    return id === undefined ? undefined : this.nodes.get(id);
  }

  /** Replay a native edit through its nearest declared input/change route. */
  private replayInput(entry: Entry, includeChange = true): void {
    for (let current: Node | null = entry.start; current && current !== this.container; current = current.parentNode) {
      const listeners = this.entryFor(current)?.listeners;
      const kind = listeners?.has('input') || listeners?.has('input:capture') ? 'input'
        : includeChange && (listeners?.has('change') || listeners?.has('change:capture')) ? 'change' : undefined;
      if (kind) { entry.start.dispatchEvent(new Event(kind, { bubbles: true })); return; }
    }
  }

  private node(id: number): Entry {
    return nodeAt(this.nodes, id);
  }

  private restoreOutputs(): void {
    for (const [id, children] of this.resetOutputs) {
      const output = this.nodes.get(id)?.start as HTMLOutputElement | undefined;
      if (output && (output.childNodes.length !== children.length || children.some((child, index) => output.childNodes[index] !== child))) {
        output.replaceChildren(...children);
      }
    }
    this.resetOutputs.clear();
  }

  apply(bytes: Uint8Array): number {
    return this.applyBatch(decodeBatch(bytes));
  }

  applyBatch(batch: Batch, beforeMutation?: () => void): number {
    if (this.closed) throw new Error('UI renderer is closed');
    const prepared = prepareBatch(batch, { nodes: this.nodes, revision: this.revision, inputSequence: this.inputSequence,
      sequence: this.sequence, highestId: this.highestId, hydrating: this.hydrating, portals:this.portals.targets }, this.widgets);
    const structural = (batch.mutations ?? []).some(mutation => ['create', 'insert', 'remove', 'portal'].includes(mutation.op));
    // Structural commits already notify every widget and refresh its ancestry.
    // Avoid collecting a redundant node set for large mounts and reorderings.
    const changedNodes = structural ? undefined : new Set<Node>();
    beforeMutation?.();
    this.restoreOutputs();
    this.commitAdoption?.();
    this.commitAdoption = undefined;
    const document = this.container.ownerDocument;
    const active = activeElement(document) as HTMLInputElement | null;
    const restoreFocus = active !== null && composedContains(this.container, active);
    const restoreContentSelection = restoreFocus ? captureContentSelection(active) : undefined;
    const selection = restoreFocus && typeof active.selectionStart === 'number'
      ? { start: active.selectionStart, end: active.selectionEnd, direction: active.selectionDirection, value: active.value } : null;
    this.dialogs.begin();
    this.popovers.begin();
    this.pointers.begin();
    // Cancel all removed widget owners before any external cleanup/DOM removal.
    const removed = [...prepared.shapes].filter(([, shape]) => shape === null).map(([id]) => id);
    this.widgets.removeMany(removed);
    if (removed.length) this.dialogs.remove(new Set(removed));
    if (removed.length) this.popovers.remove(new Set(removed));
    if (removed.length) this.portals.removing(new Set(removed));
    for (const mutation of batch.mutations ?? []) if (!deferredOperations.has(mutation.op)) this.applyMutation(mutation, prepared.shapes.get(mutation.id)?.namespace);
    for (const [id, shape] of prepared.shapes) {
      if (shape === null) {
        const entry = this.node(id);
        // Native removal can defer pausing during a seek. End playback within
        // the disposal commit; keyed moves never enter this branch.
        pauseMedia(entry);
        this.pointers.release(entry.start);
        for (const listener of entry.listeners.values()) removeListener(entry.start, listener);
        this.ids.delete(entry.start);
        this.nodes.delete(id);
        this.pendingInputs.delete(id);
      } else {
        const entry = this.node(id);
        entry.parent = shape.parent;
        entry.children = shape.children;
        if (entry.namespace === htmlNamespace && entry.kind === 'select' && (entry.desiredValue !== undefined || entry.desiredSelection !== undefined)) {
          this.pendingInputs.add(id);
        }
      }
    }
    this.revision = batch.revision;
    this.inputSequence = batch.inputSequence;
    this.highestId = prepared.highest;
    this.hydrating = false;
    this.portals.settle(prepared.portals);
    this.pointers.settle();
    for (const mutation of batch.mutations ?? []) {
      if (mutation.op !== 'default') continue;
      const control = this.node(mutation.id).start as HTMLInputElement | HTMLTextAreaElement;
      if (mutation.name === 'checked') {
        const input = control as HTMLInputElement;
        // Reassigning an unchanged radio default during hydration can change
        // group selection after a user selected another member before boot.
        if (input.defaultChecked !== (mutation.value === 'true')) input.defaultChecked = mutation.value === 'true';
      } else if (control.defaultValue !== mutation.value) control.defaultValue = mutation.value;
    }
    for (const [id, values] of prepared.selections ?? []) {
      this.node(id).desiredSelection = values;
      this.pendingInputs.add(id);
    }
    for (const [id, value] of prepared.indeterminate ?? []) {
      this.node(id).desiredIndeterminate = value;
      this.pendingInputs.add(id);
    }
    for (const id of this.pendingInputs) {
      const entry = this.node(id);
      if (entry.composing || entry.editedAt > batch.inputSequence) continue;
      changedNodes?.add(entry.start);
      if (entry.desiredValue !== undefined) {
        const input = entry.start as HTMLInputElement;
        if (input.value !== entry.desiredValue) input.value = entry.desiredValue;
      }
      if (entry.desiredChecked !== undefined) (entry.start as HTMLInputElement).checked = entry.desiredChecked;
      if (entry.desiredIndeterminate !== undefined) (entry.start as HTMLInputElement).indeterminate = entry.desiredIndeterminate;
      if (entry.desiredSelection !== undefined) applySelection(entry.start as HTMLSelectElement, entry.desiredSelection);
      this.pendingInputs.delete(id);
    }
    for (const mutation of batch.mutations ?? []) {
      if (mutation.op === 'widget' && this.nodes.has(mutation.id)) {
        this.widgets.apply(mutation.id, this.node(mutation.id).start as HTMLElement, mutation.name, mutation.value);
      }
    }
    for (const mutation of batch.mutations ?? []) {
      if (mutation.op === 'modal') {
        const entry = this.node(mutation.id);
        entry.desiredModalOpen = mutation.value === 'true';
        this.dialogs.set(mutation.id, entry.start as HTMLDialogElement, entry.desiredModalOpen);
      }
    }
    const modalFocused = this.dialogs.settle();
    for (const mutation of batch.mutations ?? []) {
      if (mutation.op === 'popover') {
        const entry = this.node(mutation.id);
        entry.desiredPopoverOpen = mutation.value === 'true';
        this.popovers.set(mutation.id, entry.start as HTMLElement,
          mutation.parent, mutation.parent ? this.node(mutation.parent).start as HTMLElement : undefined, mutation.name, entry.desiredPopoverOpen);
      }
    }
    const presentationFocused = this.popovers.settle();
    if (restoreFocus && !modalFocused && !presentationFocused && active.isConnected &&
      (activeElement(document) === active || focusAvailable(active))) {
      restoreContentSelection?.();
      if (activeElement(document) !== active) active.focus({ preventScroll: true });
    }
    // WebKit can retain activeElement across a moved dialog while resetting its
    // text selection. Restore unchanged text independently of the focus change.
    if (selection !== null && active !== null && active.isConnected && activeElement(document) === active && active.value === selection.value
      && (active.selectionStart !== selection.start || active.selectionEnd !== selection.end || active.selectionDirection !== selection.direction)) {
      active.setSelectionRange(selection.start, selection.end, selection.direction ?? undefined);
    }
    for (const operation of prepared.elementOperations ?? []) {
      if (operation.op === 'focus') {
        const element = this.node(operation.id).start as HTMLElement;
        element.focus?.();
      } else if (operation.op === 'textSelection') {
        const entry = this.node(operation.id);
        if (!entry.composing && entry.editedAt <= batch.inputSequence) applyTextSelection(entry.start as HTMLInputElement, operation.request);
      } else if (operation.op === 'scrollPosition') {
        if (operation.position) {
          const [left, top] = operation.position;
          (this.node(operation.id).start as Element).scrollTo({left, top, behavior:'instant'});
        }
      } else {
        const element = this.node(operation.id).start as Element;
        element.scrollIntoView({ block: 'nearest', inline: 'nearest', behavior: 'instant' });
      }
    }
    if (changedNodes) for (const mutation of batch.mutations ?? []) {
      const entry = this.nodes.get(mutation.id);
      if (entry) changedNodes.add(entry.start);
    }
    this.widgets.afterCommit(changedNodes, structural);
    return this.revision;
  }

  private applyMutation(mutation: Mutation, namespace = htmlNamespace): void {
    const document = this.container.ownerDocument;
    if (mutation.op === 'create') {
      if (this.hydrating) return;
      let start: Node;
      let end: Node;
      if (mutation.name === '#fragment') {
        start = document.createComment(`ui:${mutation.id}`);
        end = document.createComment(`/ui:${mutation.id}`);
        const fragment = document.createDocumentFragment();
        fragment.append(start, end);
      } else {
        start = mutation.name === '#text' ? document.createTextNode(mutation.value)
          : document.createElementNS(namespace, mutation.name === '#widget' ? 'div' : mutation.name);
        // Stay inert even when insertion precedes the authored type mutation.
        if (mutation.name === 'script' && namespace === htmlNamespace) (start as Element).setAttribute('type', 'application/json');
        end = start;
      }
      this.nodes.set(mutation.id, { start, end, kind: mutation.name, namespace, parent: null, children: [],
        listeners: new Map(), editedAt: 0, composing: false });
      this.ids.set(start, mutation.id);
      return;
    }
    const entry = this.node(mutation.id);
    if (mutation.op === 'insert' || mutation.op === 'remove') {
      if (this.hydrating && mutation.op === 'insert') return;
      const fragment = document.createDocumentFragment();
      const start = mutation.op === 'insert' ? this.portals.anchor(mutation.id) : entry.start;
      const end = start === entry.start ? entry.end : start;
      let cursor: Node | null = start;
      while (cursor !== null) {
        const next: Node | null = cursor.nextSibling;
        fragment.appendChild(cursor);
        if (cursor === end) break;
        cursor = next;
      }
      if (mutation.op === 'insert') {
        const parent = this.node(mutation.parent);
        const anchor = mutation.before !== 0 ? this.portals.anchor(mutation.before) : parent.kind === '#fragment' ? parent.end : null;
        const destination = parent.kind === '#fragment' ? parent.start.parentNode! : parent.start;
        destination.insertBefore(fragment, anchor);
      }
      return;
    }
    if (mutation.op === 'text') { entry.start.nodeValue = mutation.value; return; }
    if (mutation.op === 'listen') {
      const options = eventBinding(mutation.name, mutation.value);
      const previous = entry.listeners.get(mutation.name);
      if (previous?.binding === options.binding) return;
      if (previous) removeListener(entry.start, previous);
      if (layoutObservation(options.kind)) {
        if (entry.namespace !== htmlNamespace) throw new Error(options.kind + ' observations require an HTML element');
        const dispose = this.layout.observe(options.kind === 'size' ? 'size' : 'viewport', entry.start as HTMLElement, value => this.post(options.kind, mutation.id, value));
        entry.listeners.set(mutation.name, { ...options, invoke: () => {}, dispose });
        return;
      }
      const listener: EventListener = (event) => {
        if (this.closed) return;
        const keyboard = event as KeyboardEvent;
        const origin = this.entryFor(event.target);
        const composing = keyboard.isComposing || origin?.composing ||
          (event.target !== null && this.unmanagedCompositions.has(event.target));
        if (!matchesKeyboard(options, keyboard, composing)) return;
        // A queued close from a moved/reopened dialog must not close its new
        // presentation. The current native state is authoritative for this event.
        if (options.kind === 'close' && entry.kind === 'dialog'
          && ((entry.start as HTMLDialogElement).open || entry.desiredModalOpen === false)) return;
        // Native toggle delivery is deferred. Echoing a committed request can
        // arrive after a new keyboard action and overwrite its guest intent.
        // Controlled popovers only report native changes to the requested state.
        if (options.kind === 'toggle' && entry.desiredPopoverOpen !== undefined
          && (entry.start as HTMLElement).matches(':popover-open') === entry.desiredPopoverOpen) return;
        if (options.preventDefault) event.preventDefault();
        if (options.stopPropagation) event.stopPropagation();
        if (options.capturePointer) this.pointers.capture(entry.start as Element, event);
        const formInput = options.kind === 'input' || options.kind === 'change';
        const target = (formInput ? event.target ?? entry.start : entry.start) as HTMLElement;
        const control = target as HTMLInputElement;
        const sequence = ++this.sequence;
        const edited = formInput ? origin : undefined;
        if (edited) {
          edited.editedAt = sequence;
          this.pendingInputs.add(this.ids.get(edited.start)!);
        }
        let value = control.value ?? '';
        if (options.kind === 'toggle') value = (entry.kind === 'details' ? (entry.start as HTMLDetailsElement).open : control.matches(':popover-open')) ? 'open' : 'closed';
        this.send({ target: mutation.id, kind: options.kind, value,
          key: keyboard.key ?? '', checked: control.checked ?? false, sequence, error: '', capture: options.capture,
          altKey: keyboard.altKey ?? false, ctrlKey: keyboard.ctrlKey ?? false, metaKey: keyboard.metaKey ?? false,
          shiftKey: keyboard.shiftKey ?? false, repeat: keyboard.repeat ?? false,
          isComposing: composing, button: (event as MouseEvent).button ?? 0,
          pointerType: (event as PointerEvent).pointerType ?? '',
          pointer: pointerData(event),
          selectedValues: target.namespaceURI === htmlNamespace && target.localName === 'select' && (target as HTMLSelectElement).multiple
            ? selectedValues(target as HTMLSelectElement) : null }, options.latest);
      };
      entry.listeners.set(mutation.name, { ...options, invoke: listener });
      if (previous?.capturePointer && !options.capturePointer && ![...entry.listeners.values()].some(listener => listener.capturePointer)) this.pointers.release(entry.start);
      entry.start.addEventListener(options.kind, listener, { capture: options.capture, passive: options.passive });
      return;
    }
    if (mutation.op === 'unlisten') {
      const listener = entry.listeners.get(mutation.name);
      if (listener) removeListener(entry.start, listener);
      entry.listeners.delete(mutation.name);
      if (listener?.capturePointer && ![...entry.listeners.values()].some(listener => listener.capturePointer)) this.pointers.release(entry.start);
      return;
    }
    const element = entry.start as HTMLElement;
    const name = entry.namespace === htmlNamespace ? mutation.name.toLowerCase() : mutation.name;
    if (entry.namespace === htmlNamespace && name === 'value' && ['input', 'textarea', 'select'].includes(entry.kind)) {
      entry.desiredValue = mutation.op === 'removeAttr' ? undefined : mutation.value;
      this.pendingInputs.add(mutation.id);
      return;
    }
    if (entry.namespace === htmlNamespace && name === 'checked' && entry.kind === 'input') {
      entry.desiredChecked = mutation.op === 'removeAttr' ? undefined : mutation.value !== 'false';
      this.pendingInputs.add(mutation.id);
      return;
    }
    const mediaMute = entry.namespace === htmlNamespace && name === 'muted' && (entry.kind === 'audio' || entry.kind === 'video');
    const remove = mutation.op === 'removeAttr' || (entry.namespace === htmlNamespace && booleanAttributes.has(name) && mutation.value === 'false');
    // Native mute is separate from its reflected default. Preserve a pre-boot
    // choice when adopting that same default; later declaration changes own it.
    const retainMute = mediaMute && this.hydrating && element.hasAttribute(name) === !remove;
    const value = !remove && this.mediaSources && name === 'src'
      ? mediaSource(mutation.value, entry.kind, name, element.ownerDocument.baseURI, this.mediaSources)
      : mutation.value;
    if (remove) {
      element.removeAttribute(name);
    } else if (!this.hydrating || element.getAttribute(name) !== value) {
      // Replaying an unchanged src reloads native media/frames. Adoption also
      // avoids repeating custom-element callbacks for already-present values.
      element.setAttribute(name, value);
    }
    if (mediaMute && !retainMute) (element as HTMLMediaElement).muted = !remove;
  }

  post(kind: string, target: number, value: string, error = ''): void {
    if (this.closed) return;
    this.send({ kind, target, value, error, key: '', checked: false, sequence: ++this.sequence,
      capture: false, altKey: false, ctrlKey: false, metaKey: false, shiftKey: false, repeat: false, isComposing: false, button: 0, pointerType: '', pointer: null, selectedValues: null });
  }

  /** Read only a node owned by this renderer, after the declaring DOM commit. */
  measure(value: string): string {
    if (!/^(0|[1-9][0-9]*)$/.test(value) || value.length > 16) throw new Error('invalid measurement target');
    const id = Number(value);
    if (!Number.isSafeInteger(id)) throw new Error('invalid measurement target');
    const entry = this.closed || id === 0 ? undefined : this.nodes.get(id);
    const element = entry && entry.start === entry.end && entry.start.nodeType === 1 ? entry.start as Element : undefined;
    if (!element?.isConnected) return JSON.stringify({ found: false, x: 0, y: 0, width: 0, height: 0 });
    const { x, y, width, height } = element.getBoundingClientRect();
    if (![x, y, width, height].every(Number.isFinite) || width < 0 || height < 0) throw new Error('invalid native element geometry');
    return JSON.stringify({ found: true, x, y, width, height });
  }

  close(remove = true): void {
    if (this.closed) { if (remove) this.container.replaceChildren(); return; }
    this.closed = true;
    this.commitAdoption = undefined;
    // A native reset may replace output children while loading. Restore their
    // server identities before releasing a failed, still-uncommitted adoption.
    if (!remove && this.hydrating) this.restoreOutputs();
    this.pointers.close();
    this.layout.close();
    this.popovers.close();
    this.dialogs.close();
    this.widgets.close();
    this.portals.close();
    for (const entry of this.nodes.values()) {
      if (remove) pauseMedia(entry);
      for (const listener of entry.listeners.values()) removeListener(entry.start, listener);
    }
    this.container.removeEventListener('compositionstart', this.compositionStart, true);
    this.container.removeEventListener('compositionend', this.compositionEnd, true);
    this.container.ownerDocument.removeEventListener('reset', this.formReset, true);
    this.container.removeEventListener('input', this.unhandledControlEdit, true);
    this.container.removeEventListener('change', this.unhandledControlEdit, true);
    if (remove) this.container.replaceChildren();
    this.nodes.clear();
    this.pendingInputs.clear();
    this.resetOutputs.clear();
  }
}
