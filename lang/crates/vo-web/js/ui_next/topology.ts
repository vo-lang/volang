import { MAX_MUTATIONS, MAX_PORTALS, WIRE_VERSION } from './generated/protocol.js';
import type { Batch } from './generated/protocol.js';
import { htmlNamespace, svgNamespace, voidElements, elementNamespace, validElement } from './elements.js';
import { eventBinding, layoutObservation } from './events.js';
import { nodeAt } from './nodes.js';
import type { NodeEntry, NodeShape as Shape } from './nodes.js';
import { parseSelection } from './selection.js';
import {parseTextSelection, textSelectionInputTypes} from './text-selection.js';
import type {TextSelectionRequest} from './text-selection.js';
import { textElements, validateNativeText, validateHtmlContent, dataBlockType } from './text.js';

interface CurrentTree {
  nodes: ReadonlyMap<number, NodeEntry>;
  revision: number;
  inputSequence: number;
  sequence: number;
  highestId: number;
  hydrating: boolean;
  portals?: ReadonlyMap<number, number>;
}
type ElementOperation = { op: 'focus' | 'scroll'; id: number }
  | { op: 'textSelection'; id: number; request:TextSelectionRequest }
  | { op: 'scrollPosition'; id: number; position?: [number, number] };
export interface PreparedBatch { elementOperations?: ElementOperation[]; shapes: Map<number, Shape | null>; highest: number; selections?: Map<number, string[] | undefined>; indeterminate?: Map<number, boolean | undefined>; portals?: Map<number, number> }

const attributeName = /^[A-Za-z_:][A-Za-z0-9_:.-]*$/;

/** A batch-local sibling order. Build once per changed parent, then detach and
 * insert by identity. Materialize only the final order, keeping reorder work
 * linear in the original children plus mutations instead of shifting arrays. */
class ChildOrder {
  private readonly links = new Map<number, { previous: number; next: number }>();
  private first = 0;
  private last = 0;

  constructor(children: readonly number[]) {
    for (const id of children) this.insert(id, 0);
  }

  remove(id: number): void {
    const link = this.links.get(id);
    if (!link) throw new Error('missing UI child in parent');
    if (link.previous) this.links.get(link.previous)!.next = link.next;
    else this.first = link.next;
    if (link.next) this.links.get(link.next)!.previous = link.previous;
    else this.last = link.previous;
    this.links.delete(id);
  }

  insert(id: number, before: number): void {
    if (this.links.has(id) || before && !this.links.has(before)) throw new Error('invalid UI child insertion');
    const previous = before ? this.links.get(before)!.previous : this.last;
    this.links.set(id, { previous, next: before });
    if (previous) this.links.get(previous)!.next = id;
    else this.first = id;
    if (before) this.links.get(before)!.previous = id;
    else this.last = id;
  }

  *values(): IterableIterator<number> {
    for (let id = this.first; id; id = this.links.get(id)!.next) yield id;
  }
}

/** Preflight the complete batch without changing the live DOM or registry. */
export function prepareBatch(batch: Batch, state: Readonly<CurrentTree>, widgets: { supports(name: string): boolean }): PreparedBatch {
  const node = (id: number) => nodeAt(state.nodes, id);
  if (batch.version !== WIRE_VERSION || !Number.isSafeInteger(batch.revision)
    || !Number.isSafeInteger(batch.inputSequence) || batch.inputSequence < state.inputSequence
    || batch.inputSequence > state.sequence) throw new Error('invalid UI batch header');
  const mutations = batch.mutations ?? [];
  if (!Array.isArray(mutations) || mutations.length > MAX_MUTATIONS
    || batch.revision !== state.revision + 1) {
    throw new Error('invalid UI batch revision or mutation count');
  }
  const shapes = new Map<number, Shape | null>();
  const childOrders = new Map<number, ChildOrder>();
  const controls = new Map<number, { type: string; value?: string }>();
  const selects = new Map<number, { multiple: boolean; value?: string; controlled: boolean }>();
  let elementOperations: ElementOperation[] | undefined;
  let scriptTypes: Map<number, string> | undefined;
  let selections: Map<number, string[] | undefined> | undefined;
  let indeterminate: Map<number, boolean | undefined> | undefined;
  let portals = state.portals?.size ? new Map(state.portals) : undefined;
  const popoverModes = new Map<number, string | null>();
  let highest = state.highestId;
  const shape = (id: number): Shape => {
    if (shapes.has(id)) {
      const value = shapes.get(id);
      if (!value) throw new Error(`removed UI node ${id}`);
      return value;
    }
    const entry = node(id);
    // Borrow unchanged child arrays. Only structural mutations own a new order;
    // reading an ancestor for namespace checks must not copy all its children.
    const value = { kind: entry.kind, namespace: entry.namespace, parent: entry.parent, children: state.hydrating && id === 0 ? [] : entry.children };
    shapes.set(id, value);
    return value;
  };
  const childOrder = (id: number): ChildOrder => {
    let order = childOrders.get(id);
    if (!order) {
      order = new ChildOrder(shape(id).children);
      childOrders.set(id, order);
    }
    return order;
  };
  const detach = (id: number, value: Shape): void => {
    if (value.parent !== null) childOrder(value.parent).remove(id);
  };
  const remove = (id: number): void => {
    for (const child of childOrders.get(id)?.values() ?? shape(id).children) remove(child);
    shapes.set(id, null);
  };
  const selectState = (id: number) => {
    let control = selects.get(id);
    if (!control) {
      const existing = state.nodes.get(id);
      control = { multiple: (existing?.start as HTMLSelectElement | undefined)?.multiple ?? false,
        value: existing?.desiredValue, controlled: existing?.desiredSelection !== undefined };
      selects.set(id, control);
    }
    return control;
  };
  const inputState = (id: number) => {
    let control = controls.get(id);
    if (!control) {
      const existing = state.nodes.get(id);
      control = {type: existing ? (existing.start as HTMLInputElement).type : 'text', value: existing?.desiredValue};
      controls.set(id, control);
    }
    return control;
  };
  for (const mutation of mutations) {
    if (mutation === null || typeof mutation !== 'object'
      || ![mutation.id, mutation.parent, mutation.before].every((v) => Number.isSafeInteger(v) && v >= 0)
      || mutation.id === 0 || typeof mutation.name !== 'string' || typeof mutation.value !== 'string') {
      throw new Error('invalid UI mutation');
    }
    if (mutation.op === 'create') {
      if (mutation.id <= highest || !(mutation.name === '#text' || mutation.name === '#fragment' || mutation.name === '#widget' || validElement(mutation.name, svgNamespace))) {
        throw new Error('invalid UI node creation');
      }
      highest = mutation.id;
      if (state.hydrating) {
        const entry = node(mutation.id);
        if (entry.kind !== mutation.name || (entry.kind === '#text' && entry.start.nodeValue !== mutation.value)) {
          throw new Error(`UI hydration mismatch at node ${mutation.id}`);
        }
      }
      shapes.set(mutation.id, { kind: mutation.name, namespace: '', parent: null, children: [], text: mutation.name === '#text' ? mutation.value : undefined });
      continue;
    }
    const value = shape(mutation.id);
    switch (mutation.op) {
      case 'insert': {
        const parent = shape(mutation.parent);
        if (parent.kind === '#text' || parent.kind === '#widget') throw new Error('UI element cannot contain children');
        if (mutation.before !== 0 && shape(mutation.before).parent !== mutation.parent) throw new Error('foreign UI insertion anchor');
        if (mutation.before === mutation.id) throw new Error('UI node cannot anchor itself');
        let ancestor: number | null = mutation.parent;
        for (let depth = 0; ancestor !== null; depth++) {
          if (ancestor === mutation.id || depth >= 512) throw new Error('cyclic or excessively deep UI tree');
          ancestor = shape(ancestor).parent;
        }
        detach(mutation.id, value);
        childOrder(mutation.parent).insert(mutation.id, mutation.before);
        value.parent = mutation.parent;
        break;
      }
      case 'remove': detach(mutation.id, value); remove(mutation.id); break;
      case 'text':
        if (value.kind !== '#text') throw new Error('UI text update requires a text node');
        value.text = mutation.value;
        break;
      case 'attr': case 'removeAttr':
        if ((value.kind.startsWith('#') && value.kind !== '#widget') || !attributeName.test(mutation.name) || /^on|^data-vo-/i.test(mutation.name)) {
          throw new Error('invalid UI attribute');
        }
        if (value.kind === 'script') {
          const name = mutation.name.toLowerCase();
          if (name === 'src' && mutation.op === 'attr') throw new Error('JSON data blocks cannot have src');
          if (name === 'type') {
            if (mutation.op === 'removeAttr' || !dataBlockType(mutation.value)) throw new Error('Script requires an inline JSON data block type');
            (scriptTypes ??= new Map()).set(mutation.id, mutation.value);
          }
        }
        // Only HTML consumers use this provisional control metadata. Resolve
        // namespace after topology so attribute ordering cannot bypass checks.
        if (mutation.name.toLowerCase() === 'popover') popoverModes.set(mutation.id, mutation.op === 'attr' ? mutation.value : null);
        if (value.kind === 'input' && ['type', 'value'].includes(mutation.name.toLowerCase())) {
          const control = inputState(mutation.id);
          if (mutation.name.toLowerCase() === 'type') control.type = mutation.op === 'removeAttr' ? 'text' : mutation.value.toLowerCase();
          else control.value = mutation.op === 'removeAttr' ? undefined : mutation.value;
        }
        if (value.kind === 'select' && ['multiple', 'value'].includes(mutation.name.toLowerCase())) {
          const control = selectState(mutation.id);
          if (mutation.name.toLowerCase() === 'multiple') control.multiple = mutation.op === 'attr' && mutation.value !== 'false';
          else control.value = mutation.op === 'attr' ? mutation.value : undefined;
        }
        break;
      case 'listen': case 'unlisten':
        if (value.kind.startsWith('#') && value.kind !== '#widget') throw new Error('unsupported UI event');
        eventBinding(mutation.name, mutation.op === 'listen' ? mutation.value : undefined);
        break;
      case 'widget':
        if (value.kind !== '#widget' || !widgets.supports(mutation.name)) throw new Error('unsupported UI widget');
        break;
      case 'textSelection':
        if (!['input','textarea'].includes(value.kind) || mutation.parent !== 0 || mutation.before !== 0 || mutation.name !== '') throw new Error('Text selection requires a text control');
        if (value.kind === 'input') inputState(mutation.id);
        (elementOperations ??= []).push({op:'textSelection',id:mutation.id,request:parseTextSelection(mutation.value)});
        break;
      case 'scrollPosition': {
        if (value.kind.startsWith('#') && value.kind !== '#widget') throw new Error('Scroll position requires an element');
        if (mutation.parent !== 0 || mutation.before !== 0 || mutation.name !== '' || mutation.value.length > 128) throw new Error('Invalid scroll position');
        let position;
        try { position = JSON.parse(mutation.value); } catch { throw new Error('Invalid scroll position'); }
        if (!Array.isArray(position) || ![2,4].includes(position.length) || !position.every(value => typeof value === 'number' && Number.isFinite(value))) throw new Error('Invalid scroll position');
        const current = state.nodes.get(mutation.id)?.start as Element | undefined;
        // Test against the native position before this batch can clamp it by
        // changing layout. A newer user scroll takes priority over a correction.
        const matches = position.length === 2 || current && current.scrollLeft === position[2] && current.scrollTop === position[3];
        (elementOperations ??= []).push({op:'scrollPosition', id:mutation.id, position:matches ? [position[0],position[1]] : undefined});
        break;
      }
      case 'focus': case 'scroll':
        if (value.kind.startsWith('#') && value.kind !== '#widget') throw new Error('UI element operation requires an element');
        if (mutation.op === 'scroll' && (mutation.parent !== 0 || mutation.before !== 0 || mutation.name !== '' || mutation.value !== '')) throw new Error('Invalid UI scroll operation');
        (elementOperations ??= []).push({op:mutation.op, id:mutation.id});
        break;
      case 'default':
        if (!['input', 'textarea'].includes(value.kind) || mutation.parent !== 0 || mutation.before !== 0
          || !['value', 'checked'].includes(mutation.name)
          || (mutation.name === 'checked' && (value.kind !== 'input' || !['true', 'false'].includes(mutation.value)))) throw new Error('Invalid native default binding');
        break;
      case 'selection':
        if (value.kind !== 'select' || mutation.parent !== 0 || mutation.before !== 0 || mutation.name !== '') throw new Error('Invalid UI selection binding');
        (selections ??= new Map()).set(mutation.id, mutation.value === '' ? undefined : parseSelection(mutation.value));
        selectState(mutation.id).controlled = mutation.value !== '';
        break;
      case 'property':
        if (value.kind !== 'input' || mutation.parent !== 0 || mutation.before !== 0 || mutation.name !== 'indeterminate'
          || !['', 'true', 'false'].includes(mutation.value)) throw new Error('Invalid native property binding');
        (indeterminate ??= new Map()).set(mutation.id, mutation.value === '' ? undefined : mutation.value === 'true');
        inputState(mutation.id);
        break;
      case 'modal':
        if (value.kind !== 'dialog' || !['true', 'false'].includes(mutation.value)) throw new Error('UI modal binding requires a dialog and boolean value');
        if (typeof HTMLDialogElement === 'undefined' || typeof HTMLDialogElement.prototype.showModal !== 'function') throw new Error('This browser does not support native modal dialogs');
        break;
      case 'popover':
        if (value.kind.startsWith('#') || value.kind === 'dialog' || !['true', 'false'].includes(mutation.value)
          || !['bottom-start', 'bottom-end', 'top-start', 'top-end', 'left', 'right'].includes(mutation.name)
          || (mutation.value === 'true' && mutation.parent === 0)) throw new Error('Invalid UI popover binding');
        if (typeof HTMLElement.prototype.showPopover !== 'function') throw new Error('This browser does not support native popovers');
        break;
      case 'portal':
        if (value.kind.startsWith('#') || mutation.before !== 0 || mutation.name !== '' || mutation.value !== '') throw new Error('Invalid UI portal binding');
        (portals ??= new Map()).set(mutation.id, mutation.parent);
        break;
      default: throw new Error(`unknown UI mutation ${mutation.op}`);
    }
  }
  for (const [id, order] of childOrders) {
    const value = shapes.get(id);
    if (value) value.children = [...order.values()];
  }
  const namespaces = new Map<number, string>([[0, htmlNamespace]]);
  const namespaceOf = (id: number): string => {
    const known = namespaces.get(id);
    if (known) return known;
    const value = shape(id);
    if (value.parent === null) throw new Error('detached UI node at commit');
    const parent = shape(value.parent);
    const namespace = elementNamespace(value.kind, parent.kind, namespaceOf(value.parent));
    const existing = state.nodes.get(id);
    if (existing && existing.namespace !== namespace) throw new Error('UI namespace mismatch');
    if (!value.kind.startsWith('#') && !validElement(value.kind, namespace)) throw new Error('invalid UI element name');
    if (value.kind === '#widget' && namespace !== htmlNamespace) throw new Error('DOM widgets require HTML context');
    if (value.kind === 'script' && namespace !== htmlNamespace) throw new Error('JSON data blocks require HTML context');
    namespaces.set(id, namespace);
    value.namespace = namespace;
    return namespace;
  };
  for (const [id, value] of shapes) {
    if (id !== 0 && value !== null && value.parent === null) throw new Error('detached UI node at commit');
    if (value !== null && namespaceOf(id) === htmlNamespace
      && (voidElements.has(value.kind) || value.kind === 'textarea') && value.children.length !== 0) {
      throw new Error('UI element cannot contain children; textarea uses its value binding');
    }
    if (value !== null && value.namespace === htmlNamespace) {
      validateHtmlContent(value.kind, value.children.length);
      if (value.kind === 'script' && !dataBlockType(scriptTypes?.get(id) ?? (state.nodes.get(id)?.start as Element | undefined)?.getAttribute('type'))) throw new Error('Script requires an inline JSON data block type');
    }
    if (value !== null && value.namespace === htmlNamespace && textElements.has(value.kind)) {
      if (value.children.length > 1 || value.children.length === 1 && shape(value.children[0]).kind !== '#text') throw new Error('Native text elements accept at most one direct Text child');
      if (value.children.length) {
        const child = value.children[0];
        validateNativeText(value.kind, shape(child).text ?? state.nodes.get(child)?.start.nodeValue ?? '');
      }
    }
    if (state.hydrating && value !== null) {
      const entry = node(id);
      if (entry.parent !== value.parent || entry.children.length !== value.children.length
        || entry.children.some((child, index) => child !== value.children[index])) {
        throw new Error(`UI hydration topology mismatch at node ${id}`);
      }
    }
  }
  for (const operation of elementOperations ?? []) {
    const target = shape(operation.id);
    if (operation.op === 'textSelection' && (target.namespace !== htmlNamespace || target.kind === 'input'
      && !textSelectionInputTypes.has(controls.get(operation.id)!.type))) throw new Error('Text selection requires an HTML text control');
  }
  for (const mutation of mutations) if (mutation.op === 'listen' && layoutObservation(mutation.name)) {
    if (shape(mutation.id).namespace !== htmlNamespace) throw new Error(mutation.name + ' observations require an HTML element');
  }
  for (const mutation of mutations) if (mutation.op === 'default') {
    const value = shape(mutation.id);
    if (value.namespace !== htmlNamespace) throw new Error('Native defaults require HTML controls');
    if (value.kind === 'input' && mutation.name === 'value' && mutation.value) {
      const type = controls.get(mutation.id)?.type ?? (state.nodes.get(mutation.id)?.start as HTMLInputElement | undefined)?.type ?? 'text';
      if (type === 'file') throw new Error('UI file inputs cannot have a nonempty default value');
    }
  }
  for (const mutation of mutations) if (mutation.op === 'modal') {
    if (shape(mutation.id).namespace !== htmlNamespace) throw new Error('UI modal binding requires an HTML dialog');
  }
  for (const mutation of mutations) if (mutation.op === 'popover') {
    const popup = shape(mutation.id);
    const mode = popoverModes.has(mutation.id) ? popoverModes.get(mutation.id)
      : (state.nodes.get(mutation.id)?.start as HTMLElement | undefined)?.getAttribute('popover');
    if (popup.namespace !== htmlNamespace || (mode !== 'auto' && mode !== 'hint')) throw new Error('UI popovers require native auto or hint presentation in HTML');
    if (mutation.parent !== 0) {
      const anchor = shape(mutation.parent);
      if (anchor.namespace !== htmlNamespace || (anchor.kind.startsWith('#') && anchor.kind !== '#widget')) throw new Error('UI popover anchor must be an HTML element');
      for (let ancestor: number | null = mutation.parent; ancestor !== null; ancestor = shape(ancestor).parent) {
        if (ancestor === mutation.id) throw new Error('UI popover anchor cannot belong to its own content');
      }
    }
  }
  for (const [id, control] of controls) {
    if (shapes.get(id) !== null && namespaces.get(id) === htmlNamespace && control.type === 'file' && control.value) {
      throw new Error('UI file inputs cannot have a nonempty controlled value');
    }
    const mixed = indeterminate?.has(id) ? indeterminate.get(id) : state.nodes.get(id)?.desiredIndeterminate;
    if (shapes.get(id) !== null && namespaces.get(id) === htmlNamespace && mixed !== undefined && control.type !== 'checkbox') throw new Error('Indeterminate requires an HTML checkbox input');
  }
  for (const id of indeterminate?.keys() ?? []) {
    if (shapes.get(id) === null) {indeterminate!.delete(id); continue;}
    if (shape(id).namespace !== htmlNamespace) throw new Error('Native properties require HTML controls');
  }
  for (const [id, control] of selects) {
    if (shapes.get(id) !== null && shape(id).namespace === htmlNamespace && control.controlled && (!control.multiple || control.value !== undefined)) {
      throw new Error('UI selection requires a multiple select without a scalar value binding');
    }
  }
  for (const id of selections?.keys() ?? []) {
    if (shape(id).namespace !== htmlNamespace) throw new Error('UI selection requires an HTML select');
  }
  // Resolve placement against the complete next tree, including targets created
  // later in this batch. Invalid effective parent graphs never reach the DOM.
  for (const mutation of mutations) if (mutation.op === 'portal') {
    shape(mutation.id);
    if (namespaceOf(mutation.id) !== htmlNamespace) throw new Error('UI portals require an HTML element');
    if (mutation.parent !== 0) shape(mutation.parent);
  }
  if (portals) {
    for (const [id, target] of portals) {
      if (target === 0 || shapes.get(id) === null || shapes.get(target) === null) {portals.delete(id); continue;}
      const destination = shape(target);
      if (namespaceOf(id) !== htmlNamespace || namespaceOf(target) !== htmlNamespace || destination.kind.startsWith('#')
        || voidElements.has(destination.kind) || destination.kind === 'textarea' || destination.kind === 'iframe' || textElements.has(destination.kind)) throw new Error('UI portal target must be an HTML child container');
      for (let ancestor: number | null = target; ancestor !== null; ancestor = shape(ancestor).parent) {
        if (ancestor === id) throw new Error('UI portal target cannot belong to its own content');
      }
    }
    if (portals.size > MAX_PORTALS) throw new Error('UI portal count exceeds the root limit');
    for (const id of portals.keys()) {
      let ancestor: number | null = id;
      for (let depth = 0; ancestor !== null; depth++) {
        if (depth >= 512) throw new Error('Cyclic or excessively deep UI portal placement');
        ancestor = portals.get(ancestor) ?? shape(ancestor).parent;
        if (ancestor === id) throw new Error('Cyclic UI portal placement');
      }
    }
  }
  if (state.hydrating && shapes.size !== state.nodes.size) throw new Error('UI hydration node count mismatch');
  return { elementOperations, shapes, highest, selections, indeterminate, portals };
}
