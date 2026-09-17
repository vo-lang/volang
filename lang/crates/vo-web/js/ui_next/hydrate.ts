import type { Mutation } from './generated/protocol.js';
import { htmlNamespace, elementNamespace, initialControlValue, initialControlChecked, initialControlSelection } from './elements.js';
import { eventBinding } from './events.js';
import { nodeAt } from './nodes.js';
import type { NodeEntry as Entry } from './nodes.js';
import { parseSelection, sameSelection, selectedValues } from './selection.js';
import { textElements, dataBlockType, validateHtmlContent } from './text.js';

interface AdoptionEvents {
  bind(mutation: Mutation): void;
  replay(entry: Entry): void;
}

/** Bind server identities now; return destructive cleanup for the first commit. */
export function adoptHtml(container: HTMLElement, nodes: Map<number, Entry>, ids: WeakMap<Node, number>, events: AdoptionEvents): () => void {
  const node = (id: number) => nodeAt(nodes, id);
  const document = container.ownerDocument;
  const checkedDefaults = new Map<HTMLInputElement, boolean>();
  const controlledSelects: HTMLSelectElement[] = [];
  const markers: Node[] = [];
  const bindings: { id: number; events: { name: string; value: string }[]; initialValue?: string; initialChecked?: boolean; initialSelection?: string[] }[] = [];
  const register = (id: number, entry: Entry): void => {
    if (!Number.isSafeInteger(id) || id <= 0 || nodes.has(id)) throw new Error('duplicate or invalid hydration identity');
    nodes.set(id, entry);
    ids.set(entry.start, id);
    node(entry.parent!).children.push(id);
  };
  const scan = (parent: number, first: Node | null, stop: Node | null): void => {
    let cursor = first;
    while (cursor !== stop && cursor !== null) {
      if (cursor.nodeType === 1) {
        const element = cursor as HTMLElement;
        const id = Number(element.getAttribute('data-vo-id'));
        const kind = element.hasAttribute('data-vo-widget') ? '#widget' : element.localName;
        const entry: Entry = { start: element, end: element, kind, namespace: element.namespaceURI ?? htmlNamespace, parent, children: [],
          listeners: new Map(), editedAt: 0, composing: false };
        if (kind === 'script' && (entry.namespace !== htmlNamespace || !dataBlockType(element.getAttribute('type')) || element.hasAttribute('src'))) throw new Error('Script requires an inline HTML JSON data block');
        if (entry.namespace === htmlNamespace && kind !== 'script') validateHtmlContent(kind, element.childNodes.length);
        if (['input', 'textarea', 'select'].includes(kind) && element.hasAttribute('value') && !element.hasAttribute('data-vo-default-value')) {
          entry.desiredValue = element.getAttribute('value')!;
        }
        if (kind === 'textarea' && entry.namespace === htmlNamespace && element.hasAttribute('data-vo-controlled')) {
          entry.desiredValue = (element as HTMLTextAreaElement).defaultValue;
        }
        if (kind === 'input' && element.hasAttribute('data-vo-checked') && ['checkbox', 'radio'].includes((element as HTMLInputElement).type)) {
          entry.desiredChecked = element.hasAttribute('checked');
        }
        if (element.hasAttribute('data-vo-indeterminate')) {
          const value = element.getAttribute('data-vo-indeterminate');
          if (kind !== 'input' || entry.namespace !== htmlNamespace || (element as HTMLInputElement).type !== 'checkbox' || !['true', 'false'].includes(value!)) throw new Error('Invalid native indeterminate hydration marker');
          entry.desiredIndeterminate = value === 'true';
        }
        if (kind === 'select' && entry.namespace === htmlNamespace && element.hasAttribute('data-vo-selected')) {
          if (!(element as HTMLSelectElement).multiple || entry.desiredValue !== undefined) throw new Error('Invalid hydrated UI selection');
          entry.desiredSelection = parseSelection(element.getAttribute('data-vo-selected')!);
        }
        if (kind === 'select' && entry.namespace === htmlNamespace && (entry.desiredValue !== undefined || entry.desiredSelection !== undefined)) {
          controlledSelects.push(element as HTMLSelectElement);
        }
        register(id, entry);
        const events = (element.getAttribute('data-vo-events') ?? '').split(' ').filter(Boolean).map(token => {
          const parts = token.split('=');
          if (parts.length !== 2) throw new Error('invalid hydration event');
          eventBinding(parts[0], parts[1]);
          return { name: parts[0], value: parts[1] };
        });
        const formControl = entry.namespace === htmlNamespace && ['input', 'textarea', 'select'].includes(kind);
        const checkedControl = kind === 'input' && entry.namespace === htmlNamespace && ['checkbox', 'radio'].includes((element as HTMLInputElement).type);
        const multiple = formControl && kind === 'select' && (element as HTMLSelectElement).multiple;
        bindings.push({ id, events, initialValue: formControl && !multiple ? initialControlValue(element) : undefined,
          initialSelection: multiple ? initialControlSelection(element as HTMLSelectElement) : undefined,
          initialChecked: checkedControl ? initialControlChecked(element as HTMLInputElement, checkedDefaults) : undefined });
        if (element.hasAttribute('data-vo-text')) {
          const identity = element.getAttribute('data-vo-text')!;
          if (entry.namespace !== htmlNamespace || !textElements.has(kind) || !/^[1-9][0-9]*$/.test(identity)) throw new Error('Invalid native text hydration marker');
          let text = element.firstChild;
          if (text === null) {text = document.createTextNode(''); element.append(text);}
          if (text.nodeType !== 3 || text.nextSibling !== null) throw new Error('Invalid native text hydration content');
          register(Number(identity), {start: text, end: text, kind: '#text', namespace: htmlNamespace, parent: id, children: [],
            listeners: new Map(), editedAt: 0, composing: false});
        } else if (!(kind === 'textarea' && entry.namespace === htmlNamespace)) scan(id, element.firstChild, null);
        cursor = element.nextSibling;
        continue;
      }
      const marker = cursor.nodeType === 8 ? /^vo:([rt]):([1-9][0-9]*)$/.exec(cursor.nodeValue ?? '') : null;
      if (!marker) throw new Error('unexpected content in hydratable UI root');
      const id = Number(marker[2]);
      const ending = `vo:/${marker[1]}:${id}`;
      let end = cursor.nextSibling;
      while (end !== null && !(end.nodeType === 8 && end.nodeValue === ending)) end = end.nextSibling;
      if (end === null) throw new Error('missing hydration range end');
      if (marker[1] === 'r') {
        register(id, { start: cursor, end, kind: '#fragment', namespace: elementNamespace('#fragment', node(parent).kind, node(parent).namespace), parent, children: [],
          listeners: new Map(), editedAt: 0, composing: false });
        scan(id, cursor.nextSibling, end);
      } else {
        let text = cursor.nextSibling;
        if (text === end) { text = document.createTextNode(''); end.parentNode!.insertBefore(text, end); }
        if (text === null || text.nodeType !== 3 || text.nextSibling !== end) throw new Error('invalid hydration text range');
        register(id, { start: text, end: text, kind: '#text', namespace: elementNamespace('#text', node(parent).kind, node(parent).namespace), parent, children: [],
          listeners: new Map(), editedAt: 0, composing: false });
        markers.push(cursor, end);
      }
      cursor = end.nextSibling;
    }
  };
  scan(0, container.firstChild, null);
  for (const binding of bindings) {
    for (const event of binding.events) events.bind({ op: 'listen', id: binding.id, parent: 0, before: 0, ...event });
    const entry = node(binding.id);
    // Edits made before the host loaded are captured from the existing control.
    const control = entry.start as HTMLInputElement;
    if ((binding.initialValue !== undefined && control.value !== binding.initialValue)
      || (binding.initialSelection !== undefined && !sameSelection(selectedValues(entry.start as HTMLSelectElement), binding.initialSelection))
      || (binding.initialChecked !== undefined && control.checked !== binding.initialChecked)) {
      events.replay(entry);
    }
  }
  return () => {
    // Keep markers and reset defaults intact until preflight succeeds, so loading
    // failures can detach listeners and retry against the same server markup.
    for (const marker of markers) marker.parentNode!.removeChild(marker);
    // SSR selected attributes express the visible controlled value. Preserve
    // current native edits, then let the first guest batch supply authored reset
    // defaults, matching a client mount when control is later released.
    for (const select of controlledSelects) {
      const current = [...select.options].map(option => ({ option, selected: option.selected }));
      // A same-value assignment can leave native selectedness pristine. An actual
      // change marks every option dirty before authored defaults are reapplied.
      for (const { option } of current) { option.selected = !option.selected; option.removeAttribute('selected'); }
      if (select.multiple) for (const { option, selected } of current) option.selected = selected;
      else select.selectedIndex = current.findIndex(value => value.selected);
    }
  };
}
