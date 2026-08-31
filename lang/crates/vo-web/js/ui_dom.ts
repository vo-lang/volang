import {
  decodeUiEvent,
  decodeUiMutationBatch,
  DEFAULT_UI_PROTOCOL_LIMITS,
  encodeUiEvent,
  type UiEventEnvelope,
  type UiEventPayload,
  type UiIdentity,
  type UiListener,
  type UiMutation,
  type UiMutationBatch,
  type UiProtocolLimits,
  type UiValue,
  uiIdentityKey,
} from './ui_protocol.js';
import { UiBrowserSystemHost, type UiSystemHost } from './ui_system.js';

interface NodeRecord {
  readonly id: UiIdentity;
  readonly primitive?: number;
  readonly textNode: boolean;
  text: string;
  properties: Map<number, UiValue>;
  listeners: Map<number, UiListener>;
  parent?: string;
  children: string[];
}

interface BrowserListener {
  readonly listener: UiListener;
  readonly callback: EventListener;
  readonly attachments: ReadonlyArray<{ readonly target: EventTarget; readonly name: string }>;
}

export interface UiDomAdapterOptions {
  readonly rootId?: UiIdentity;
  readonly limits?: UiProtocolLimits;
  readonly onEvent?: () => void;
}

const DEFAULT_ROOT_ID: UiIdentity = Object.freeze({ index: 0, generation: 1 });
const MAX_MEASUREMENTS_PER_COMMIT = 256;
const MAX_MEASUREMENT_FEEDBACK_TURNS = 8;
const MAX_PENDING_MEASUREMENT_EVENTS = 4_096;
const MEASUREMENT_QUANTUM = 64;
const PORTAL_LOGICAL_EVENT_TYPES = Object.freeze([
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 21, 22, 23, 24,
]);

function cloneRecord(record: NodeRecord): NodeRecord {
  return {
    id: record.id,
    primitive: record.primitive,
    textNode: record.textNode,
    text: record.text,
    properties: new Map(record.properties),
    listeners: new Map(record.listeners),
    parent: record.parent,
    children: [...record.children],
  };
}

function cloneNodes(nodes: ReadonlyMap<string, NodeRecord>): Map<string, NodeRecord> {
  return new Map([...nodes].map(([key, record]) => [key, cloneRecord(record)]));
}

function sameIdentity(left: UiIdentity, right: UiIdentity): boolean {
  return left.index === right.index && left.generation === right.generation;
}

function valueBytes(value: UiValue): number {
  switch (value.type) {
    case 'text':
      return new TextEncoder().encode(value.value).byteLength;
    case 'bytes':
      return value.value.byteLength;
    default:
      return 8;
  }
}

function requireNode(nodes: Map<string, NodeRecord>, id: UiIdentity): NodeRecord {
  const node = nodes.get(uiIdentityKey(id));
  if (node === undefined) throw new Error(`missing Volang UI node ${uiIdentityKey(id)}`);
  return node;
}

function isAncestor(nodes: ReadonlyMap<string, NodeRecord>, ancestor: string, child: string): boolean {
  let current = nodes.get(child)?.parent;
  const visited = new Set<string>();
  while (current !== undefined) {
    if (current === ancestor) return true;
    if (visited.has(current)) throw new Error('cycle in Volang UI host tree');
    visited.add(current);
    current = nodes.get(current)?.parent;
  }
  return false;
}

function applyLogicalMutation(
  nodes: Map<string, NodeRecord>,
  mutation: UiMutation,
  rootKey: string,
  limits: UiProtocolLimits,
): void {
  switch (mutation.type) {
    case 'create-element':
    case 'create-text': {
      const key = uiIdentityKey(mutation.id);
      if (key === rootKey) throw new Error('Volang UI root identity cannot be created');
      if (nodes.has(key)) throw new Error(`Volang UI node already exists: ${key}`);
      if (nodes.size >= limits.maxNodes) throw new Error('Volang UI node limit exceeded');
      nodes.set(key, {
        id: mutation.id,
        primitive: mutation.type === 'create-element' ? mutation.primitive : undefined,
        textNode: mutation.type === 'create-text',
        text: '',
        properties: new Map(),
        listeners: new Map(),
        children: [],
      });
      return;
    }
    case 'set-text': {
      const node = requireNode(nodes, mutation.id);
      if (!node.textNode) throw new Error('Volang UI SetText target is an element');
      if (new TextEncoder().encode(mutation.text).byteLength > limits.maxTextBytes) {
        throw new Error('Volang UI text limit exceeded');
      }
      node.text = mutation.text;
      return;
    }
    case 'set-property': {
      const node = requireNode(nodes, mutation.id);
      if (node.textNode) throw new Error('Volang UI property target is a text node');
      if (valueBytes(mutation.value) > limits.maxValueBytes) {
        throw new Error('Volang UI property value limit exceeded');
      }
      if (!node.properties.has(mutation.property)
        && node.properties.size >= limits.maxPropertiesPerNode) {
        throw new Error('Volang UI property count limit exceeded');
      }
      node.properties.set(mutation.property, mutation.value);
      return;
    }
    case 'remove-property': {
      const node = requireNode(nodes, mutation.id);
      if (node.textNode) throw new Error('Volang UI property target is a text node');
      node.properties.delete(mutation.property);
      return;
    }
    case 'listen': {
      const node = requireNode(nodes, mutation.id);
      if (node.textNode) throw new Error('Volang UI listener target is a text node');
      node.listeners.set(mutation.listener.event, mutation.listener);
      return;
    }
    case 'unlisten': {
      const node = requireNode(nodes, mutation.id);
      const current = node.listeners.get(mutation.event);
      if (current === undefined || !sameIdentity(current.handler, mutation.handler)) {
        throw new Error('Volang UI listener identity mismatch');
      }
      node.listeners.delete(mutation.event);
      return;
    }
    case 'insert-before': {
      const parentKey = uiIdentityKey(mutation.parent);
      const childKey = uiIdentityKey(mutation.child);
      if (parentKey === childKey || isAncestor(nodes, childKey, parentKey)) {
        throw new Error('Volang UI insertion would create a cycle');
      }
      const parent = requireNode(nodes, mutation.parent);
      const child = requireNode(nodes, mutation.child);
      if (parent.textNode) throw new Error('Volang UI text nodes cannot contain children');
      if (child.parent !== undefined && child.parent !== parentKey) {
        throw new Error('Volang UI child already belongs to another parent');
      }
      const beforeKey = mutation.before === undefined ? undefined : uiIdentityKey(mutation.before);
      if (beforeKey === childKey
        || (beforeKey !== undefined && !parent.children.includes(beforeKey))) {
        throw new Error('Volang UI before-sibling identity mismatch');
      }
      const existing = parent.children.indexOf(childKey);
      if (existing >= 0) parent.children.splice(existing, 1);
      else if (parent.children.length >= limits.maxChildrenPerNode) {
        throw new Error('Volang UI child count limit exceeded');
      }
      const position = beforeKey === undefined ? parent.children.length : parent.children.indexOf(beforeKey);
      parent.children.splice(position, 0, childKey);
      child.parent = parentKey;
      return;
    }
    case 'remove': {
      const parentKey = uiIdentityKey(mutation.parent);
      const childKey = uiIdentityKey(mutation.child);
      const parent = requireNode(nodes, mutation.parent);
      const child = requireNode(nodes, mutation.child);
      const position = parent.children.indexOf(childKey);
      if (child.parent !== parentKey || position < 0) {
        throw new Error('Volang UI child relationship mismatch');
      }
      parent.children.splice(position, 1);
      child.parent = undefined;
      return;
    }
    case 'delete': {
      const key = uiIdentityKey(mutation.id);
      if (key === rootKey) throw new Error('Volang UI root identity cannot be deleted');
      const node = requireNode(nodes, mutation.id);
      if (node.parent !== undefined || node.children.length !== 0) {
        throw new Error('Volang UI nodes must be detached and empty before deletion');
      }
      nodes.delete(key);
      return;
    }
  }
}

function primitiveTag(primitive: number): { tag: string; inputType?: string } {
  switch (primitive) {
    case 1:
      return { tag: 'span' };
    case 8:
      return { tag: 'img' };
    case 9:
      return { tag: 'button' };
    case 10:
      return { tag: 'input', inputType: 'text' };
    case 11:
      return { tag: 'input', inputType: 'checkbox' };
    case 12:
      return { tag: 'input', inputType: 'range' };
    case 13:
      return { tag: 'canvas' };
    case 15:
      return { tag: 'span' };
    case 16:
      return { tag: 'textarea' };
    default:
      return { tag: 'div' };
  }
}

function serverPrimitive(value: string): number | undefined {
  const primitives: Record<string, number> = {
    root: 0, fragment: 1, box: 2, row: 3, column: 4, stack: 5, grid: 6, scroll: 7,
    image: 8, button: 9, 'text-input': 10, toggle: 11, slider: 12, canvas: 13,
    'platform-view': 14, text: 15, 'text-area': 16,
  };
  return primitives[value];
}

function eventName(event: number): string {
  const names = [
    '', 'click', 'input', 'change', 'submit', 'focus', 'blur', 'keydown', 'keyup',
    'pointerdown', 'pointermove', 'pointerup', 'scroll', 'compositionstart',
    'compositionupdate', 'compositionend', 'wheel', '', 'pointercancel', 'volanglayout', 'selectionchange',
    'contextmenu', 'drop', 'dragenter', 'dragleave',
  ];
  const name = names[event];
  if (name === undefined || name === '') throw new Error(`unsupported Volang UI event ${event}`);
  return name;
}

function eventModifiers(event: Event): {
  shift: boolean;
  control: boolean;
  alt: boolean;
  meta: boolean;
} {
  const modified = event as Event & {
    readonly shiftKey?: boolean;
    readonly ctrlKey?: boolean;
    readonly altKey?: boolean;
    readonly metaKey?: boolean;
  };
  return {
    shift: modified.shiftKey === true,
    control: modified.ctrlKey === true,
    alt: modified.altKey === true,
    meta: modified.metaKey === true,
  };
}

function lengthCss(value: Extract<UiValue, { type: 'length' }>['value']): string {
  switch (value.unit) {
    case 'auto': return 'auto';
    case 'px': return `${value.value}px`;
    case 'percent': return `${value.value}%`;
    case 'vw': return `${value.value}vw`;
    case 'vh': return `${value.value}vh`;
  }
}

function colorCss(value: number): string {
  const alpha = (value >>> 24) & 0xff;
  const red = (value >>> 16) & 0xff;
  const green = (value >>> 8) & 0xff;
  const blue = value & 0xff;
  return `#${[red, green, blue, alpha]
    .map((channel) => channel.toString(16).padStart(2, '0'))
    .join('')}`;
}

function scalarText(value: UiValue): string {
  switch (value.type) {
    case 'bool': return value.value ? 'true' : 'false';
    case 'i64': return value.value.toString();
    case 'f64': return String(value.value);
    case 'text': return value.value;
    case 'color': return colorCss(value.value);
    case 'length': return lengthCss(value.value);
    case 'bytes': return '';
  }
}

function cssLength(value: UiValue): string {
  if (value.type === 'length') return lengthCss(value.value);
  if (value.type === 'i64' || value.type === 'f64') return `${value.value}px`;
  return scalarText(value);
}

function gridAreaRows(value: string): string[][] {
  const rows = value.split('/').map((row) => row.trim().split(/\s+/u).filter(Boolean));
  const columns = rows[0]?.length ?? 0;
  if (rows.length === 0 || columns === 0 || rows.some((row) => row.length !== columns)) {
    throw new Error('Volang UI grid template areas require equally sized non-empty rows');
  }
  for (const row of rows) {
    for (const name of row) {
      if (name !== '.' && !/^[A-Za-z_][A-Za-z0-9_-]*$/u.test(name)) {
        throw new Error('Volang UI grid area names must be ASCII identifiers or dots');
      }
    }
  }
  const bounds = new Map<string, { minRow: number; maxRow: number; minColumn: number; maxColumn: number }>();
  rows.forEach((row, rowIndex) => row.forEach((name, columnIndex) => {
    if (name === '.') return;
    const current = bounds.get(name);
    if (current === undefined) {
      bounds.set(name, {
        minRow: rowIndex, maxRow: rowIndex, minColumn: columnIndex, maxColumn: columnIndex,
      });
    } else {
      current.minRow = Math.min(current.minRow, rowIndex);
      current.maxRow = Math.max(current.maxRow, rowIndex);
      current.minColumn = Math.min(current.minColumn, columnIndex);
      current.maxColumn = Math.max(current.maxColumn, columnIndex);
    }
  }));
  for (const [name, area] of bounds) {
    for (let row = area.minRow; row <= area.maxRow; row += 1) {
      for (let column = area.minColumn; column <= area.maxColumn; column += 1) {
        if (rows[row]?.[column] !== name) {
          throw new Error('Volang UI named grid areas must form rectangles');
        }
      }
    }
  }
  return rows;
}

function gridTemplateAreasCss(value: UiValue): string {
  if (value.type !== 'text') throw new Error('Volang UI grid template areas require text');
  return gridAreaRows(value.value).map((row) => `"${row.join(' ')}"`).join(' ');
}

function gridAreaCss(value: UiValue): string {
  if (value.type !== 'text' || !/^[A-Za-z_][A-Za-z0-9_-]*$/u.test(value.value)) {
    throw new Error('Volang UI grid area requires an ASCII identifier');
  }
  return value.value;
}

function isElement(value: unknown): value is Element {
  return typeof value === 'object'
    && value !== null
    && 'nodeType' in value
    && value.nodeType === 1;
}

function isInput(element: Element): element is HTMLInputElement | HTMLTextAreaElement {
  const tag = element.tagName.toLowerCase();
  return tag === 'input' || tag === 'textarea';
}

function isCheckableInput(element: Element): element is HTMLInputElement {
  return element.tagName.toLowerCase() === 'input';
}

function supportsNativeDisabled(element: Element): boolean {
  return ['button', 'fieldset', 'input', 'optgroup', 'option', 'select', 'textarea']
    .includes(element.tagName.toLowerCase());
}

function textSelectionSignature(element: HTMLInputElement | HTMLTextAreaElement): string {
  const start = element.selectionStart ?? 0;
  const end = element.selectionEnd ?? start;
  return `${element.value}\u0000${start}:${end}`;
}

function booleanValue(value: UiValue, label: string): boolean {
  if (value.type !== 'bool') throw new Error(`Volang UI ${label} requires a boolean`);
  return value.value;
}

function isNaturallyFocusable(element: Element): boolean {
  const tag = element.tagName.toLowerCase();
  return tag === 'button' || tag === 'input' || tag === 'select' || tag === 'textarea';
}

function setProgrammaticFocus(element: Element, enabled: boolean, marker: string): void {
  if (enabled && !isNaturallyFocusable(element) && !element.hasAttribute('tabindex')) {
    element.setAttribute('tabindex', '-1');
    element.setAttribute(marker, '');
  } else if (!enabled && element.hasAttribute(marker)) {
    element.removeAttribute('tabindex');
    element.removeAttribute(marker);
  }
}

function setHidden(element: Element, hidden: boolean): void {
  const html = element as HTMLElement;
  const marker = 'data-volang-hidden-display';
  if (hidden) {
    if (!element.hasAttribute(marker)) element.setAttribute(marker, html.style.display || '');
    html.style.display = 'none';
    html.hidden = true;
    element.setAttribute('aria-hidden', 'true');
    return;
  }
  if (element.hasAttribute(marker)) {
    html.style.display = element.getAttribute(marker) ?? '';
    element.removeAttribute(marker);
  }
  html.hidden = false;
  element.removeAttribute('aria-hidden');
}

function selectionOffset(value: UiValue, label: string): number {
  if (value.type !== 'i64' || value.value < 0n || value.value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(`Volang UI ${label} requires a non-negative safe i64`);
  }
  return Number(value.value);
}

function boundedScalar(value: UiValue, maximumBytes: number, label: string): string {
  const rendered = scalarText(value);
  if (new TextEncoder().encode(rendered).byteLength > maximumBytes) {
    throw new Error(`Volang UI ${label} exceeds its byte limit`);
  }
  return rendered;
}

function canvasColor(value: string): string {
  if (!/^[0-9a-f]{1,8}$/iu.test(value)) throw new Error('Volang UI graphics color is invalid');
  return `#${value.padStart(8, '0')}`;
}

function synchronizeGraphicsCanvasSize(canvas: HTMLCanvasElement): void {
  const logicalWidth = /^([0-9]+(?:\.[0-9]+)?)px$/u.exec(canvas.style.width)?.[1];
  const logicalHeight = /^([0-9]+(?:\.[0-9]+)?)px$/u.exec(canvas.style.height)?.[1];
  if (logicalWidth !== undefined) {
    const width = Math.round(Number(logicalWidth));
    if (canvas.width !== width) canvas.width = width;
  }
  if (logicalHeight !== undefined) {
    const height = Math.round(Number(logicalHeight));
    if (canvas.height !== height) canvas.height = height;
  }
}

function renderGraphicsProgram(element: Element, program: string): void {
  if (element.tagName.toLowerCase() !== 'canvas') return;
  const canvas = element as HTMLCanvasElement;
  synchronizeGraphicsCanvasSize(canvas);
  if (!('getContext' in element)) return;
  const context = canvas.getContext('2d');
  if (context === null) return;
  const lines = program.split('\n');
  if (lines[0] !== 'VGC1' || lines.length > 65_537) throw new Error('Volang UI graphics program is invalid');
  context.clearRect(0, 0, canvas.width, canvas.height);
  for (const line of lines.slice(1)) {
    const fields = line.split('|');
    if (fields.length !== 5) throw new Error('Volang UI graphics command is invalid');
    const kind = Number(fields[0]);
    const values = fields[1] === '' ? [] : fields[1].split(',').map(Number);
    if (!Number.isInteger(kind) || values.some((value) => !Number.isFinite(value) || Math.abs(value) > 1_000_000_000)) {
      throw new Error('Volang UI graphics command contains an invalid number');
    }
    const color = canvasColor(fields[2]);
    const width = Number(fields[3]);
    if (!Number.isFinite(width) || width < 0) throw new Error('Volang UI graphics stroke width is invalid');
    context.fillStyle = color;
    context.strokeStyle = color;
    context.lineWidth = width;
    switch (kind) {
      case 0:
        context.clearRect(0, 0, canvas.width, canvas.height);
        context.fillRect(0, 0, canvas.width, canvas.height);
        break;
      case 1: if (values.length !== 4) throw new Error('invalid fill rectangle'); context.fillRect(values[0], values[1], values[2], values[3]); break;
      case 2: if (values.length !== 4) throw new Error('invalid stroke rectangle'); context.strokeRect(values[0], values[1], values[2], values[3]); break;
      case 3:
        if (values.length !== 4) throw new Error('invalid line');
        context.beginPath(); context.moveTo(values[0], values[1]); context.lineTo(values[2], values[3]); context.stroke();
        break;
      case 4:
        if (values.length !== 3 || values[2] < 0) throw new Error('invalid circle');
        context.beginPath(); context.arc(values[0], values[1], values[2], 0, Math.PI * 2); context.fill();
        break;
      case 5:
        if (values.length !== 3) throw new Error('invalid text');
        context.font = `${values[2]}px system-ui, sans-serif`; context.fillText(fields[4], values[0], values[1]);
        break;
      case 6: {
        context.beginPath(); let offset = 0;
        for (const operation of fields[4]) {
          if (operation === 'M' || operation === 'L') {
            if (offset + 2 > values.length) throw new Error('invalid path');
            if (operation === 'M') context.moveTo(values[offset], values[offset + 1]);
            else context.lineTo(values[offset], values[offset + 1]);
            offset += 2;
          } else if (operation === 'Q') {
            if (offset + 4 > values.length) throw new Error('invalid path');
            context.quadraticCurveTo(values[offset], values[offset + 1], values[offset + 2], values[offset + 3]); offset += 4;
          } else if (operation === 'Z') context.closePath();
          else throw new Error('invalid path operation');
        }
        if (offset !== values.length) throw new Error('invalid path values');
        if (width > 0) context.stroke(); else context.fill();
        break;
      }
      default: throw new Error(`unsupported Volang UI graphics command ${kind}`);
    }
  }
}

function platformMediaChild(element: Element): Element | undefined {
  for (const child of Array.from(element.childNodes)) {
    if (isElement(child) && child.hasAttribute('data-volang-media-host')) return child;
  }
  return undefined;
}

function synchronizePlatformMedia(element: Element): Element | undefined {
  if (!element.hasAttribute('data-volang-platform-view')) return undefined;
  const kind = element.getAttribute('data-volang-content-type');
  let media = platformMediaChild(element);
  if (kind !== 'audio' && kind !== 'video') {
    media?.remove();
    return undefined;
  }
  if (media !== undefined && media.tagName.toLowerCase() !== kind) {
    media.remove(); media = undefined;
  }
  if (media === undefined) {
    media = element.ownerDocument.createElement(kind);
    media.setAttribute('data-volang-media-host', '');
    (media as HTMLMediaElement).controls = true;
    (media as HTMLMediaElement).preload = 'metadata';
    (media as HTMLElement).style.width = '100%';
    (media as HTMLElement).style.height = '100%';
    element.appendChild(media);
  }
  const source = element.getAttribute('src');
  if (source !== null) media.setAttribute('src', source);
  const poster = element.getAttribute('poster');
  if (poster !== null && kind === 'video') media.setAttribute('poster', poster);
  const fit = (element as HTMLElement).style.objectFit;
  if (fit !== '') (media as HTMLElement).style.objectFit = fit;
  return media;
}

function applyMediaState(element: Element, encoded: string): void {
  const media = synchronizePlatformMedia(element);
  if (media === undefined) return;
  const fields = encoded.split('|');
  if (fields.length !== 5 || fields[0] !== 'VMS1' || !/^[0-7]$/u.test(fields[1]) || !/^[0-9]+$/u.test(fields[2])) {
    throw new Error('Volang UI media state is invalid');
  }
  const positionNanos = BigInt(fields[2]);
  const position = Number(positionNanos) / 1_000_000_000;
  const volume = Number(fields[3]);
  const rate = Number(fields[4]);
  if (!Number.isFinite(position) || position < 0 || !Number.isFinite(volume) || volume < 0 || volume > 1
    || !Number.isFinite(rate) || rate < 0.25 || rate > 4) {
    throw new Error('Volang UI media state contains an invalid scalar');
  }
  const host = media as HTMLMediaElement;
  if (Math.abs((host.currentTime ?? 0) - position) > 0.05) host.currentTime = position;
  host.volume = volume; host.playbackRate = rate;
  const state = Number(fields[1]);
  if (state === 3 && typeof host.play === 'function') {
    const started = host.play();
    if (started !== undefined && typeof started.catch === 'function') void started.catch(() => undefined);
  } else if (state >= 4 && typeof host.pause === 'function') host.pause();
}

function elevationShadow(level: number): string {
  switch (level) {
    case 0: return 'none';
    case 1: return '0 1px 2px rgb(15 23 42 / 0.10), 0 1px 4px rgb(15 23 42 / 0.08)';
    case 2: return '0 3px 8px rgb(15 23 42 / 0.12), 0 1px 3px rgb(15 23 42 / 0.10)';
    case 3: return '0 8px 20px rgb(15 23 42 / 0.16), 0 2px 6px rgb(15 23 42 / 0.10)';
    case 4: return '0 14px 34px rgb(15 23 42 / 0.20), 0 4px 10px rgb(15 23 42 / 0.12)';
    case 5: return '0 24px 56px rgb(15 23 42 / 0.24), 0 8px 18px rgb(15 23 42 / 0.14)';
    default: throw new Error('Volang UI elevation level must be between zero and five');
  }
}

function applyProperty(element: Element, property: number, value: UiValue): void {
  const html = element as HTMLElement;
  switch (property) {
    case 1:
      html.style.width = cssLength(value);
      if (element.hasAttribute('data-volang-graphics')) renderGraphicsProgram(element, element.getAttribute('data-volang-graphics') ?? '');
      return;
    case 2:
      html.style.height = cssLength(value);
      if (element.hasAttribute('data-volang-graphics')) renderGraphicsProgram(element, element.getAttribute('data-volang-graphics') ?? '');
      return;
    case 3: html.style.minWidth = cssLength(value); return;
    case 4: html.style.minHeight = cssLength(value); return;
    case 5: html.style.maxWidth = cssLength(value); return;
    case 6: html.style.maxHeight = cssLength(value); return;
    case 7: html.style.flex = scalarText(value); return;
    case 8: html.style.gap = cssLength(value); return;
    case 9: html.style.padding = cssLength(value); return;
    case 10: html.style.background = scalarText(value); return;
    case 11:
      html.style.color = scalarText(value);
      if (isCheckableInput(element)) element.style.accentColor = scalarText(value);
      return;
    case 12: html.style.fontSize = cssLength(value); return;
    case 13: html.style.fontWeight = scalarText(value); return;
    case 14: html.style.alignItems = scalarText(value); return;
    case 15: html.style.justifyContent = scalarText(value); return;
    case 16:
      if (isInput(element)) {
        element.value = scalarText(value);
        if (element.getAttribute('role') === 'spinbutton') {
          element.setAttribute('aria-valuenow', scalarText(value));
        }
      } else element.setAttribute('aria-valuetext', scalarText(value));
      return;
    case 17:
      if (!isInput(element)) throw new Error('Volang UI placeholder requires an input');
      element.placeholder = scalarText(value);
      return;
    case 18:
      if (supportsNativeDisabled(element)) {
        (element as HTMLButtonElement | HTMLInputElement).disabled = booleanValue(value, 'disabled property');
      } else if (booleanValue(value, 'disabled property')) {
        element.setAttribute('aria-disabled', 'true');
      } else {
        element.removeAttribute('aria-disabled');
      }
      return;
    case 19: {
      const role = scalarText(value);
      element.setAttribute('role', role);
      if (role === 'spinbutton' && isInput(element)) {
        element.setAttribute('aria-valuenow', element.value);
      }
      return;
    }
    case 20: element.setAttribute('aria-label', scalarText(value)); return;
    case 21: element.setAttribute('data-testid', scalarText(value)); return;
    case 22: html.style.gridTemplateColumns = scalarText(value); return;
    case 23: html.style.overflow = scalarText(value); return;
    case 24: html.style.borderRadius = cssLength(value); return;
    case 25:
      if (isCheckableInput(element)) element.checked = booleanValue(value, 'checked property');
      else element.setAttribute('aria-checked', scalarText(value));
      return;
    case 26: html.scrollLeft = Number(scalarText(value)); return;
    case 27: html.scrollTop = Number(scalarText(value)); return;
    case 28:
      if ('required' in element) (element as HTMLInputElement).required = value.type === 'bool' && value.value;
      else element.toggleAttribute('aria-required', value.type === 'bool' && value.value);
      return;
    case 29: element.setAttribute('aria-invalid', scalarText(value)); return;
    case 30: element.setAttribute('aria-description', scalarText(value)); return;
    case 31:
      if (!isInput(element)) throw new Error('Volang UI selection start requires an input');
      selectionOffset(value, 'selection start');
      return;
    case 32:
      if (!isInput(element)) throw new Error('Volang UI selection length requires an input');
      selectionOffset(value, 'selection length');
      return;
    case 33: html.style.gridTemplateAreas = gridTemplateAreasCss(value); return;
    case 34: html.style.gridArea = gridAreaCss(value); return;
    case 35: {
      const active = booleanValue(value, 'modal property');
      element.toggleAttribute('aria-modal', active);
      setProgrammaticFocus(element, active, 'data-volang-modal-focus');
      return;
    }
    case 36: {
      const active = booleanValue(value, 'auto-focus property');
      element.toggleAttribute('data-volang-autofocus', active);
      setProgrammaticFocus(element, active, 'data-volang-auto-focusable');
      return;
    }
    case 37: {
      const pointerEvents = scalarText(value);
      if (pointerEvents !== 'auto' && pointerEvents !== 'none') {
        throw new Error('Volang UI pointer events must be auto or none');
      }
      html.style.pointerEvents = pointerEvents;
      return;
    }
    case 38:
      html.toggleAttribute('data-volang-pointer-capture', booleanValue(value, 'pointer capture'));
      return;
    case 39: {
      const direction = scalarText(value);
      if (direction !== '0' && direction !== '1') throw new Error('Volang UI flow direction is invalid');
      html.dir = direction === '1' ? 'rtl' : 'ltr';
      return;
    }
    case 40: {
      if (value.type !== 'i64' || value.value < -1_000_000n || value.value > 1_000_000n) {
        throw new Error('Volang UI portal layer exceeds the portable range');
      }
      element.setAttribute('data-volang-portal', value.value.toString());
      html.style.position = 'fixed';
      html.style.inset = '0';
      html.style.zIndex = String(Number(value.value));
      return;
    }
    case 41: {
      if (value.type !== 'i64' || value.value < 0n) {
        throw new Error('Volang UI focus request token cannot be negative');
      }
      element.setAttribute('data-volang-focus-request', value.value.toString());
      setProgrammaticFocus(element, value.value > 0n, 'data-volang-focus-requestable');
      return;
    }
    case 42: element.setAttribute('aria-selected', scalarText(value)); return;
    case 43: element.setAttribute('aria-expanded', scalarText(value)); return;
    case 44: element.setAttribute('aria-pressed', scalarText(value)); return;
    case 45: {
      const current = scalarText(value);
      if (!['false', 'true', 'page', 'step', 'location', 'date', 'time'].includes(current)) {
        throw new Error('Volang UI current token is invalid');
      }
      element.setAttribute('aria-current', current);
      return;
    }
    case 46: {
      const hidden = booleanValue(value, 'hidden property');
      setHidden(element, hidden);
      return;
    }
    case 47: {
      const source = boundedScalar(value, 4_096, 'source');
      if (/[\u0000-\u001f\u007f]/u.test(source) || /^\s*javascript:/iu.test(source)) {
        throw new Error('Volang UI source is unsafe');
      }
      element.setAttribute('src', source);
      synchronizePlatformMedia(element)?.setAttribute('src', source);
      return;
    }
    case 48: {
      element.setAttribute('data-volang-content-type', boundedScalar(value, 255, 'content type'));
      const media = synchronizePlatformMedia(element);
      const state = element.getAttribute('data-volang-media-state');
      if (media !== undefined && state !== null) applyMediaState(element, state);
      return;
    }
    case 49: {
      const fit = scalarText(value);
      if (!['contain', 'cover', 'fill', 'none', 'scale-down'].includes(fit)) throw new Error('Volang UI fit is invalid');
      html.style.objectFit = fit;
      const media = synchronizePlatformMedia(element); if (media !== undefined) (media as HTMLElement).style.objectFit = fit;
      return;
    }
    case 50: {
      const opacity = Number(scalarText(value));
      if (!Number.isFinite(opacity) || opacity < 0 || opacity > 1) throw new Error('Volang UI opacity is invalid');
      html.style.opacity = String(opacity);
      return;
    }
    case 51: html.style.transform = boundedScalar(value, 512, 'transform'); return;
    case 52: {
      const program = boundedScalar(value, 1_048_576, 'graphics program');
      element.setAttribute('data-volang-graphics', program);
      renderGraphicsProgram(element, program);
      return;
    }
    case 53: {
      const state = boundedScalar(value, 65_536, 'media state');
      element.setAttribute('data-volang-media-state', state); applyMediaState(element, state); return;
    }
    case 54: {
      const poster = boundedScalar(value, 4_096, 'poster'); element.setAttribute('poster', poster);
      const media = synchronizePlatformMedia(element); if (media?.tagName.toLowerCase() === 'video') media.setAttribute('poster', poster);
      return;
    }
    case 55: html.style.borderColor = scalarText(value); html.style.borderStyle = 'solid'; return;
    case 56: html.style.borderWidth = cssLength(value); html.style.borderStyle = 'solid'; return;
    case 57:
      if (isCheckableInput(element) && element.type === 'range') element.min = scalarText(value);
      else element.setAttribute('aria-valuemin', scalarText(value));
      return;
    case 58:
      if (isCheckableInput(element) && element.type === 'range') element.max = scalarText(value);
      else element.setAttribute('aria-valuemax', scalarText(value));
      return;
    case 59:
      if (isCheckableInput(element) && element.type === 'range') element.step = scalarText(value);
      else element.setAttribute('data-volang-step', scalarText(value));
      return;
    case 60:
      element.setAttribute('aria-hidden', booleanValue(value, 'accessibility hidden property') ? 'true' : 'false');
      return;
    case 61:
      element.setAttribute('tabindex', booleanValue(value, 'focusable property') ? '0' : '-1');
      return;
    case 62: {
      const family = scalarText(value);
      if (!['system-ui', 'sans-serif', 'serif', 'monospace'].includes(family)) {
        throw new Error('Volang UI font family is invalid');
      }
      html.style.fontFamily = family;
      return;
    }
    case 63: {
      const policy = scalarText(value);
      if (!['normal', 'pre', 'pre-wrap', 'nowrap', 'break-spaces'].includes(policy)) {
        throw new Error('Volang UI white-space policy is invalid');
      }
      html.style.whiteSpace = policy;
      return;
    }
    case 64: {
      const id = boundedScalar(value, 255, 'element id');
      if (!/^[A-Za-z][A-Za-z0-9_:.-]*$/u.test(id)) throw new Error('Volang UI element id is invalid');
      element.setAttribute('id', id);
      return;
    }
    case 65: {
      const id = boundedScalar(value, 255, 'active descendant');
      if (!/^[A-Za-z][A-Za-z0-9_:.-]*$/u.test(id)) throw new Error('Volang UI active descendant is invalid');
      element.setAttribute('aria-activedescendant', id);
      return;
    }
    case 66: element.setAttribute('aria-controls', boundedScalar(value, 255, 'controls')); return;
    case 67: {
      const token = scalarText(value);
      if (!['none', 'inline', 'list', 'both'].includes(token)) throw new Error('Volang UI autocomplete token is invalid');
      element.setAttribute('aria-autocomplete', token);
      return;
    }
    case 68: element.setAttribute('aria-multiselectable', scalarText(value)); return;
    case 69:
      html.style.setProperty('--volang-hover-background', scalarText(value));
      element.setAttribute('data-volang-hover-background', '');
      return;
    case 70:
      html.style.setProperty('--volang-pressed-background', scalarText(value));
      element.setAttribute('data-volang-pressed-background', '');
      return;
    case 71:
      html.style.setProperty('--volang-focus-ring', scalarText(value));
      element.setAttribute('data-volang-focus-ring', '');
      return;
    case 72: {
      if (value.type !== 'i64' || value.value < 0n || value.value > 5n) {
        throw new Error('Volang UI elevation level must be between zero and five');
      }
      html.style.boxShadow = elevationShadow(Number(value.value));
      element.setAttribute('data-volang-elevation', value.value.toString());
      return;
    }
    default:
      if (property < 1 << 16) throw new Error(`unsupported Volang UI property ${property}`);
      element.setAttribute(`data-volang-${property}`, scalarText(value));
  }
}

function removeProperty(element: Element, property: number): void {
  const html = element as HTMLElement;
  switch (property) {
    case 1: html.style.removeProperty('width'); return;
    case 2: html.style.removeProperty('height'); return;
    case 3: html.style.removeProperty('min-width'); return;
    case 4: html.style.removeProperty('min-height'); return;
    case 5: html.style.removeProperty('max-width'); return;
    case 6: html.style.removeProperty('max-height'); return;
    case 7: html.style.removeProperty('flex'); return;
    case 8: html.style.removeProperty('gap'); return;
    case 9: html.style.removeProperty('padding'); return;
    case 10: html.style.removeProperty('background'); return;
    case 11: html.style.removeProperty('color'); if (isCheckableInput(element)) element.style.removeProperty('accent-color'); return;
    case 12: html.style.removeProperty('font-size'); return;
    case 13: html.style.removeProperty('font-weight'); return;
    case 14: html.style.removeProperty('align-items'); return;
    case 15: html.style.removeProperty('justify-content'); return;
    case 16:
      if (isInput(element)) {
        element.value = '';
        element.removeAttribute('aria-valuenow');
      } else element.removeAttribute('aria-valuetext');
      return;
    case 17: if (isInput(element)) element.placeholder = ''; return;
    case 18:
      if (supportsNativeDisabled(element)) (element as HTMLButtonElement | HTMLInputElement).disabled = false;
      element.removeAttribute('aria-disabled');
      return;
    case 19: element.removeAttribute('role'); element.removeAttribute('aria-valuenow'); return;
    case 20: element.removeAttribute('aria-label'); return;
    case 21: element.removeAttribute('data-testid'); return;
    case 22: html.style.removeProperty('grid-template-columns'); return;
    case 23: html.style.removeProperty('overflow'); return;
    case 24: html.style.removeProperty('border-radius'); return;
    case 25:
      if (isCheckableInput(element)) element.checked = false;
      else element.removeAttribute('aria-checked');
      return;
    case 26: html.scrollLeft = 0; return;
    case 27: html.scrollTop = 0; return;
    case 28:
      if ('required' in element) (element as HTMLInputElement).required = false;
      element.removeAttribute('aria-required');
      return;
    case 29: element.removeAttribute('aria-invalid'); return;
    case 30: element.removeAttribute('aria-description'); return;
    case 31:
    case 32:
      return;
    case 33: html.style.removeProperty('grid-template-areas'); return;
    case 34: html.style.removeProperty('grid-area'); return;
    case 35:
      element.removeAttribute('aria-modal');
      setProgrammaticFocus(element, false, 'data-volang-modal-focus');
      return;
    case 36:
      element.removeAttribute('data-volang-autofocus');
      setProgrammaticFocus(element, false, 'data-volang-auto-focusable');
      return;
    case 37: html.style.removeProperty('pointer-events'); return;
    case 38: element.removeAttribute('data-volang-pointer-capture'); return;
    case 39: html.removeAttribute('dir'); return;
    case 40:
      element.removeAttribute('data-volang-portal');
      html.style.removeProperty('position');
      html.style.removeProperty('inset');
      html.style.removeProperty('z-index');
      return;
    case 41:
      element.removeAttribute('data-volang-focus-request');
      setProgrammaticFocus(element, false, 'data-volang-focus-requestable');
      return;
    case 42: element.removeAttribute('aria-selected'); return;
    case 43: element.removeAttribute('aria-expanded'); return;
    case 44: element.removeAttribute('aria-pressed'); return;
    case 45: element.removeAttribute('aria-current'); return;
    case 46:
      setHidden(element, false);
      return;
    case 47: element.removeAttribute('src'); platformMediaChild(element)?.removeAttribute('src'); return;
    case 48: element.removeAttribute('data-volang-content-type'); synchronizePlatformMedia(element); return;
    case 49: html.style.removeProperty('object-fit'); (platformMediaChild(element) as HTMLElement | undefined)?.style.removeProperty('object-fit'); return;
    case 50: html.style.removeProperty('opacity'); return;
    case 51: html.style.removeProperty('transform'); return;
    case 52: element.removeAttribute('data-volang-graphics'); return;
    case 53: element.removeAttribute('data-volang-media-state'); return;
    case 54: element.removeAttribute('poster'); platformMediaChild(element)?.removeAttribute('poster'); return;
    case 55: html.style.removeProperty('border-color'); return;
    case 56: html.style.removeProperty('border-width'); return;
    case 57:
      if (isCheckableInput(element)) element.removeAttribute('min');
      element.removeAttribute('aria-valuemin'); return;
    case 58:
      if (isCheckableInput(element)) element.removeAttribute('max');
      element.removeAttribute('aria-valuemax'); return;
    case 59:
      if (isCheckableInput(element)) element.removeAttribute('step');
      element.removeAttribute('data-volang-step'); return;
    case 60: element.removeAttribute('aria-hidden'); return;
    case 61: element.removeAttribute('tabindex'); return;
    case 62: html.style.removeProperty('font-family'); return;
    case 63: html.style.removeProperty('white-space'); return;
    case 64: element.removeAttribute('id'); return;
    case 65: element.removeAttribute('aria-activedescendant'); return;
    case 66: element.removeAttribute('aria-controls'); return;
    case 67: element.removeAttribute('aria-autocomplete'); return;
    case 68: element.removeAttribute('aria-multiselectable'); return;
    case 69:
      html.style.removeProperty('--volang-hover-background');
      element.removeAttribute('data-volang-hover-background');
      return;
    case 70:
      html.style.removeProperty('--volang-pressed-background');
      element.removeAttribute('data-volang-pressed-background');
      return;
    case 71:
      html.style.removeProperty('--volang-focus-ring');
      element.removeAttribute('data-volang-focus-ring');
      return;
    case 72:
      html.style.removeProperty('box-shadow');
      element.removeAttribute('data-volang-elevation');
      return;
    default: element.removeAttribute(`data-volang-${property}`);
  }
}

export class UiDomAdapter {
  private readonly rootId: UiIdentity;
  private readonly rootKey: string;
  private readonly limits: UiProtocolLimits;
  private readonly onEvent?: () => void;
  private nodes: Map<string, NodeRecord>;
  private domNodes = new Map<string, Node>();
  private domListeners = new Map<string, Map<number, BrowserListener>>();
  private textSelections = new Map<string, string>();
  private sessionEpoch?: bigint;
  private revision = 0n;
  private eventSequence = 0n;
  private readonly eventFrames: Uint8Array[] = [];
  private readonly composingNodes = new Set<string>();
  private readonly pendingCompositionSync = new Set<string>();
  private readonly pendingControlledScroll = new Map<string, { x: number; y: number }>();
  private activeModalKey?: string;
  private restoreFocus?: Element;
  private restoreFocusKey?: string;
  private lastErrorValue?: Error;
  private lastFocusRequestKey?: string;
  private lastFocusRequestToken?: bigint;
  private measurements = new Map<string, { width: number; height: number }>();
  private measurementFeedbackTurns = 0;
  private readonly portalCaptureListeners = new Map<number, EventListener>();
  private readonly portalBubbleListeners = new Map<number, EventListener>();
  private readonly logicalOnceDelivered = new Set<string>();

  private readonly modalFocusGuard = (event: Event): void => {
    const modal = this.activeModalElement();
    if (modal === undefined || this.eventTargetsModal(event, modal)) return;
    event.preventDefault();
    event.stopImmediatePropagation();
    this.focusModalTarget(this.nodes, this.activeModalKey as string, false);
  };

  private readonly modalPointerGuard = (event: Event): void => {
    const modal = this.activeModalElement();
    if (modal === undefined || this.eventTargetsModal(event, modal)) return;
    event.preventDefault();
    event.stopImmediatePropagation();
  };

  private readonly modalKeyGuard = (event: Event): void => {
    const keyboard = event as KeyboardEvent;
    if (keyboard.key !== 'Tab' || this.activeModalKey === undefined) return;
    const focusable = this.modalFocusableKeys(this.nodes, this.activeModalKey);
    event.preventDefault();
    event.stopImmediatePropagation();
    if (focusable.length === 0) {
      this.focusElement(this.activeModalKey);
      return;
    }
    const activeKey = this.elementKey(this.root.ownerDocument.activeElement);
    const current = activeKey === undefined ? -1 : focusable.indexOf(activeKey);
    const next = keyboard.shiftKey
      ? (current <= 0 ? focusable.length - 1 : current - 1)
      : (current < 0 || current + 1 >= focusable.length ? 0 : current + 1);
    this.focusElement(focusable[next] as string);
  };

  constructor(public root: HTMLElement, options: UiDomAdapterOptions = {}) {
    this.rootId = options.rootId ?? DEFAULT_ROOT_ID;
    this.rootKey = uiIdentityKey(this.rootId);
    this.limits = options.limits ?? DEFAULT_UI_PROTOCOL_LIMITS;
    this.onEvent = options.onEvent;
    this.nodes = new Map([[this.rootKey, {
      id: this.rootId,
      primitive: 0,
      textNode: false,
      text: '',
      properties: new Map(),
      listeners: new Map(),
      children: [],
    }]]);
    this.domNodes.set(this.rootKey, root);
    this.adoptServerRenderedNodes();
    this.bindRoot(root);
  }

  get currentRevision(): bigint {
    return this.revision;
  }

  get currentSessionEpoch(): bigint | undefined {
    return this.sessionEpoch;
  }

  get lastError(): Error | undefined {
    return this.lastErrorValue;
  }

  applyMutationFrame(frame: Uint8Array): void {
    const batch = decodeUiMutationBatch(frame, this.limits);
    this.applyBatch(batch);
  }

  /// Builds a fresh session against a detached root, then swaps the accepted
  /// children into the visible root with one DOM operation. The returned
  /// adapter owns the new session epoch, logical tree, and live listeners.
  replaceWithInitialFrame(frame: Uint8Array): UiDomAdapter {
    const stagingRoot = this.root.ownerDocument.createElement(
      this.root.tagName.toLowerCase(),
    ) as HTMLElement;
    const replacement = new UiDomAdapter(stagingRoot, {
      rootId: this.rootId,
      limits: this.limits,
      onEvent: this.onEvent,
    });
    replacement.applyMutationFrame(frame);
    this.root.replaceChildren(...Array.from(stagingRoot.childNodes));
    replacement.bindRoot(this.root);
    replacement.domNodes.set(this.rootKey, this.root);
    replacement.synchronizeModalFocus(replacement.nodes);
    return replacement;
  }

  applyBatch(batch: UiMutationBatch): void {
    if (this.sessionEpoch === undefined) {
      if (batch.revision !== 1n) throw new Error('first Volang UI revision must be 1');
    } else {
      if (batch.sessionEpoch !== this.sessionEpoch) throw new Error('stale Volang UI session epoch');
      if (batch.revision !== this.revision + 1n) throw new Error('non-consecutive Volang UI revision');
    }
    const staged = cloneNodes(this.nodes);
    for (const mutation of batch.mutations) {
      applyLogicalMutation(staged, mutation, this.rootKey, this.limits);
    }

    const previous = this.nodes;
    let synchronizedCompositions = new Set<string>();
    let measurementEvents = 0;
    try {
      if (this.sessionEpoch === undefined && !this.initialHydrationMatches(batch)) {
        this.discardAdoptedServerTree();
      }
      this.restoreLogicalDom(previous);
      for (const mutation of batch.mutations) this.applyDomMutation(mutation);
      synchronizedCompositions = this.applyPendingCompositionValues(staged);
      this.applyControlledSelections(staged);
      this.synchronizeStackChildren(staged);
      this.synchronizePortals(staged);
      this.synchronizeModalFocus(staged);
      this.synchronizeFocusRequest(staged);
      this.discardStaleQueuedEvents(staged, batch.sessionEpoch);
      measurementEvents = this.synchronizeMeasurements(staged, batch.sessionEpoch);
    } catch (cause) {
      this.nodes = previous;
      try {
        this.rebuild(previous);
        this.synchronizeModalFocus(previous);
      } catch (rollback) {
        const error = new Error('Volang UI DOM commit and rollback failed') as Error & {
          commitCause: unknown;
          rollbackCause: unknown;
        };
        error.commitCause = cause;
        error.rollbackCause = rollback;
        throw error;
      }
      throw cause;
    }
    this.nodes = staged;
    this.sessionEpoch = batch.sessionEpoch;
    this.revision = batch.revision;
    for (const key of synchronizedCompositions) this.pendingCompositionSync.delete(key);
    if (measurementEvents > 0) this.onEvent?.();
  }

  nextEventFrameLength(): number {
    return this.eventFrames[0]?.byteLength ?? 0;
  }

  readEventFrame(destination: Uint8Array): boolean {
    const frame = this.eventFrames[0];
    if (frame === undefined || destination.byteLength !== frame.byteLength) return false;
    destination.set(frame);
    this.eventFrames.shift();
    return true;
  }

  shiftEventFrame(): Uint8Array | undefined {
    return this.eventFrames.shift();
  }

  private discardStaleQueuedEvents(
    nodes: ReadonlyMap<string, NodeRecord>,
    sessionEpoch: bigint,
  ): void {
    const retained = this.eventFrames.filter((frame) => {
      const envelope = decodeUiEvent(frame, this.limits);
      if (envelope.sessionEpoch !== sessionEpoch) return false;
      if (envelope.event === 17) {
        return envelope.handler.index === 0xffff_ffff
          && sameIdentity(envelope.target, this.rootId);
      }
      const listener = nodes.get(uiIdentityKey(envelope.target))
        ?.listeners.get(envelope.event);
      return listener !== undefined && sameIdentity(listener.handler, envelope.handler);
    });
    this.eventFrames.splice(0, this.eventFrames.length, ...retained);
  }

  /// Creates a trusted renderer-neutral wake for state written by a worker
  /// goroutine. The VM provider validates the reserved event/handler/root tuple
  /// before the guest loop starts a new render transaction.
  createInvalidationFrame(): Uint8Array {
    if (this.sessionEpoch === undefined) {
      throw new Error('Volang UI invalidation requires a mounted session');
    }
    this.eventSequence += 1n;
    return encodeUiEvent({
      sessionEpoch: this.sessionEpoch,
      handler: { index: 0xffff_ffff, generation: 1 },
      event: 17,
      target: this.rootId,
      sequence: this.eventSequence,
      payload: { type: 'none' },
    }, this.limits);
  }

  private requireDomNode(id: UiIdentity): Node {
    const node = this.domNodes.get(uiIdentityKey(id));
    if (node === undefined) throw new Error(`missing Volang UI DOM node ${uiIdentityKey(id)}`);
    return node;
  }

  private bindRoot(root: HTMLElement): void {
    if (this.root !== root) {
      this.root.removeEventListener('focusin', this.modalFocusGuard, true);
      this.root.removeEventListener('pointerdown', this.modalPointerGuard, true);
      this.root.removeEventListener('click', this.modalPointerGuard, true);
      this.root.removeEventListener('keydown', this.modalKeyGuard, true);
      for (const [eventType, callback] of this.portalCaptureListeners) {
        this.root.removeEventListener(eventName(eventType), callback, true);
      }
      for (const [eventType, callback] of this.portalBubbleListeners) {
        this.root.removeEventListener(eventName(eventType), callback, false);
      }
      this.root = root;
    }
    root.addEventListener('focusin', this.modalFocusGuard, true);
    root.addEventListener('pointerdown', this.modalPointerGuard, true);
    root.addEventListener('click', this.modalPointerGuard, true);
    root.addEventListener('keydown', this.modalKeyGuard, true);
    for (const eventType of PORTAL_LOGICAL_EVENT_TYPES) {
      let capture = this.portalCaptureListeners.get(eventType);
      if (capture === undefined) {
        capture = (event) => this.forwardPortalEvent(eventType, event, true);
        this.portalCaptureListeners.set(eventType, capture);
      }
      let bubble = this.portalBubbleListeners.get(eventType);
      if (bubble === undefined) {
        bubble = (event) => this.forwardPortalEvent(eventType, event, false);
        this.portalBubbleListeners.set(eventType, bubble);
      }
      root.addEventListener(eventName(eventType), capture, true);
      root.addEventListener(eventName(eventType), bubble, false);
    }
  }

  private eventTargetKey(event: Event): string | undefined {
    let node = event.target as Node | null;
    while (node !== null) {
      if (isElement(node)) {
        const key = node.getAttribute('data-volang-node');
        if (key !== null) return key;
      }
      if (node === this.root) break;
      node = node.parentNode;
    }
    return undefined;
  }

  private forwardPortalEvent(eventType: number, event: Event, capture: boolean): void {
    const modal = this.activeModalElement();
    if (modal !== undefined && this.eventTargetsModal(event, modal)) return;
    const targetKey = this.eventTargetKey(event);
    if (targetKey === undefined) return;
    let current: string | undefined = targetKey;
    let portalKey: string | undefined;
    const visited = new Set<string>();
    while (current !== undefined && current !== this.rootKey) {
      if (visited.has(current)) return;
      visited.add(current);
      const record = this.nodes.get(current);
      if (record === undefined) return;
      if (record.properties.has(40)) portalKey = current;
      current = record.parent;
    }
    if (portalKey === undefined) return;
    const ancestors: NodeRecord[] = [];
    current = this.nodes.get(portalKey)?.parent;
    while (current !== undefined && current !== this.rootKey) {
      const record = this.nodes.get(current);
      if (record === undefined) return;
      ancestors.push(record);
      current = record.parent;
    }
    if (capture) ancestors.reverse();
    for (const record of ancestors) {
      const listener = record.listeners.get(eventType);
      if (listener === undefined || listener.capture !== capture) continue;
      const onceKey = `${uiIdentityKey(record.id)}:${eventType}:${uiIdentityKey(listener.handler)}`;
      if (listener.once && this.logicalOnceDelivered.has(onceKey)) continue;
      this.queueListenerEvent(record.id, listener, event);
      if (listener.once) this.logicalOnceDelivered.add(onceKey);
    }
  }

  private activeModalElement(): Element | undefined {
    if (this.activeModalKey === undefined) return undefined;
    const node = this.domNodes.get(this.activeModalKey);
    return isElement(node) ? node : undefined;
  }

  private eventTargetsModal(event: Event, modal: Element): boolean {
    const target = event.target;
    return target !== null && modal.contains(target as Node);
  }

  private elementKey(value: Element | null | undefined): string | undefined {
    if (!isElement(value)) return undefined;
    const key = value.getAttribute('data-volang-node');
    return key === null ? undefined : key;
  }

  private modalFocusableKeys(nodes: ReadonlyMap<string, NodeRecord>, modalKey: string): string[] {
    const keys: string[] = [];
    for (const [key, record] of nodes) {
      if (key === modalKey || !isAncestor(nodes, modalKey, key)) continue;
      const explicitlyFocusable = record.properties.get(61);
      if (record.textNode || (![9, 10, 11, 12].includes(record.primitive ?? -1)
        && (explicitlyFocusable === undefined || !booleanValue(explicitlyFocusable, 'focusable property')))) continue;
      const disabled = record.properties.get(18);
      if (disabled !== undefined && booleanValue(disabled, 'disabled property')) continue;
      keys.push(key);
    }
    return keys;
  }

  private rootFocusableKeys(nodes: ReadonlyMap<string, NodeRecord>): string[] {
    const keys: string[] = [];
    for (const [key, record] of nodes) {
      const explicitlyFocusable = record.properties.get(61);
      if (record.textNode || (![9, 10, 11, 12].includes(record.primitive ?? -1)
        && (explicitlyFocusable === undefined || !booleanValue(explicitlyFocusable, 'focusable property')))) continue;
      const disabled = record.properties.get(18);
      if (disabled !== undefined && booleanValue(disabled, 'disabled property')) continue;
      keys.push(key);
    }
    return keys;
  }

  private modalState(nodes: ReadonlyMap<string, NodeRecord>): { key: string; preferred: string } | undefined {
    let modalKey: string | undefined;
    for (const [key, record] of nodes) {
      if (key !== this.rootKey && !isAncestor(nodes, this.rootKey, key)) continue;
      const value = record.properties.get(35);
      if (value === undefined || !booleanValue(value, 'modal property')) continue;
      if (modalKey !== undefined) throw new Error('Volang UI allows one active modal focus scope');
      modalKey = key;
    }
    if (modalKey === undefined) return undefined;
    let preferred: string | undefined;
    for (const [key, record] of nodes) {
      if (key !== this.rootKey && !isAncestor(nodes, this.rootKey, key)) continue;
      const value = record.properties.get(36);
      if (value === undefined || !booleanValue(value, 'auto-focus property')) continue;
      if (key !== modalKey && !isAncestor(nodes, modalKey, key)) continue;
      if (preferred !== undefined) throw new Error('Volang UI modal has multiple auto-focus targets');
      preferred = key;
    }
    return {
      key: modalKey,
      preferred: preferred ?? this.modalFocusableKeys(nodes, modalKey)[0] ?? modalKey,
    };
  }

  private focusElement(key: string): void {
    const element = this.domNodes.get(key);
    if (isElement(element)) (element as HTMLElement).focus({ preventScroll: true });
  }

  private focusModalTarget(
    nodes: ReadonlyMap<string, NodeRecord>,
    modalKey: string,
    preferDeclared: boolean,
  ): void {
    const state = this.modalState(nodes);
    if (state === undefined || state.key !== modalKey) return;
    const target = preferDeclared
      ? state.preferred
      : this.modalFocusableKeys(nodes, modalKey)[0] ?? modalKey;
    this.focusElement(target);
  }

  private synchronizeModalFocus(nodes: ReadonlyMap<string, NodeRecord>): void {
    const next = this.modalState(nodes);
    if (next === undefined) {
      if (this.activeModalKey === undefined) return;
      const keyedRestore = this.restoreFocusKey === undefined
        ? undefined
        : this.domNodes.get(this.restoreFocusKey);
      const restore = isElement(keyedRestore) ? keyedRestore : this.restoreFocus;
      this.activeModalKey = undefined;
      this.restoreFocus = undefined;
      this.restoreFocusKey = undefined;
      if (restore?.isConnected) (restore as HTMLElement).focus({ preventScroll: true });
      if (restore === undefined || this.root.ownerDocument.activeElement !== restore) {
        const fallback = this.rootFocusableKeys(nodes)[0];
        if (fallback !== undefined) this.focusElement(fallback);
      }
      return;
    }
    const changed = this.activeModalKey !== next.key;
    if (changed) {
      const active = this.root.ownerDocument.activeElement;
      const activeKey = this.elementKey(active);
      this.restoreFocus = activeKey === undefined || !this.domNodes.has(activeKey)
        ? undefined
        : active as Element;
      this.restoreFocusKey = activeKey;
      this.activeModalKey = next.key;
    }
    const modal = this.activeModalElement();
    const active = this.root.ownerDocument.activeElement;
    if (modal !== undefined && (changed || active === null || !modal.contains(active))) {
      this.focusElement(next.preferred);
    }
  }

  private focusRequestState(
    nodes: ReadonlyMap<string, NodeRecord>,
  ): { key: string; token: bigint } | undefined {
    let request: { key: string; token: bigint } | undefined;
    const modal = this.modalState(nodes);
    for (const [key, record] of nodes) {
      if (key !== this.rootKey && !isAncestor(nodes, this.rootKey, key)) continue;
      const value = record.properties.get(41);
      if (value === undefined) continue;
      if (value.type !== 'i64' || value.value < 0n) {
        throw new Error('Volang UI focus request token cannot be negative');
      }
      if (value.value === 0n) continue;
      const previous = this.nodes.get(key)?.properties.get(41);
      const changed = previous === undefined
        || previous.type !== 'i64'
        || previous.value !== value.value;
      if (!changed) continue;
      if (request !== undefined) throw new Error('Volang UI allows one changed focus request per commit');
      if (modal !== undefined && key !== modal.key && !isAncestor(nodes, modal.key, key)) {
        throw new Error('Volang UI focus request must remain inside the active modal');
      }
      request = { key, token: value.value };
    }
    return request;
  }

  private synchronizeFocusRequest(nodes: ReadonlyMap<string, NodeRecord>): void {
    const request = this.focusRequestState(nodes);
    if (request === undefined) {
      this.lastFocusRequestKey = undefined;
      this.lastFocusRequestToken = undefined;
      return;
    }
    if (this.lastFocusRequestKey === request.key && this.lastFocusRequestToken === request.token) {
      return;
    }
    this.focusElement(request.key);
    this.lastFocusRequestKey = request.key;
    this.lastFocusRequestToken = request.token;
  }

  private synchronizeMeasurements(
    nodes: ReadonlyMap<string, NodeRecord>,
    sessionEpoch: bigint,
  ): number {
    const next = new Map<string, { width: number; height: number }>();
    const changed: Array<{ record: NodeRecord; listener: UiListener; width: number; height: number }> = [];
    for (const [key, record] of nodes) {
      const listener = record.listeners.get(19);
      if (listener === undefined) continue;
      if (key !== this.rootKey && !isAncestor(nodes, this.rootKey, key)) continue;
      const node = this.domNodes.get(key);
      if (!isElement(node)) throw new Error('Volang UI layout observer target must be an element');
      const rectangle = typeof node.getBoundingClientRect === 'function'
        ? node.getBoundingClientRect()
        : undefined;
      const rawWidth = (rectangle?.width ?? Number.parseFloat((node as HTMLElement).style.width)) || 0;
      const rawHeight = (rectangle?.height ?? Number.parseFloat((node as HTMLElement).style.height)) || 0;
      if (!Number.isFinite(rawWidth) || !Number.isFinite(rawHeight) || rawWidth < 0 || rawHeight < 0) {
        throw new Error('Volang UI layout observer produced invalid geometry');
      }
      const width = Math.round(rawWidth * MEASUREMENT_QUANTUM) / MEASUREMENT_QUANTUM;
      const height = Math.round(rawHeight * MEASUREMENT_QUANTUM) / MEASUREMENT_QUANTUM;
      next.set(key, { width, height });
      const previous = this.measurements.get(key);
      if (previous?.width === width && previous.height === height) continue;
      if (changed.length >= MAX_MEASUREMENTS_PER_COMMIT) {
        throw new Error('Volang UI measurement listener limit exceeded');
      }
      changed.push({ record, listener, width, height });
    }
    const turns = changed.length === 0 ? 0 : this.measurementFeedbackTurns + 1;
    if (turns > MAX_MEASUREMENT_FEEDBACK_TURNS) {
      throw new Error('Volang UI measurement feedback iteration limit exceeded');
    }
    if (this.eventFrames.length + changed.length > MAX_PENDING_MEASUREMENT_EVENTS) {
      throw new Error('Volang UI measurement event queue limit exceeded');
    }
    const frames: Uint8Array[] = [];
    for (const item of changed) {
      this.eventSequence += 1n;
      frames.push(encodeUiEvent({
        sessionEpoch,
        handler: item.listener.handler,
        event: 19,
        target: item.record.id,
        sequence: this.eventSequence,
        payload: {
          type: 'scroll',
          x: item.width,
          y: item.height,
          deltaX: 0,
          deltaY: 0,
          unit: 'pixel',
          modifiers: { shift: false, control: false, alt: false, meta: false },
        },
      }, this.limits));
    }
    this.measurements = next;
    this.measurementFeedbackTurns = turns;
    this.eventFrames.push(...frames);
    return frames.length;
  }

  private createDomNode(id: UiIdentity, primitive?: number): Node {
    if (primitive === undefined) return this.root.ownerDocument.createTextNode('');
    const descriptor = primitiveTag(primitive);
    const element = this.root.ownerDocument.createElement(descriptor.tag);
    const key = uiIdentityKey(id);
    element.setAttribute('data-volang-node', key);
    if (descriptor.inputType !== undefined) (element as HTMLInputElement).type = descriptor.inputType;
    this.configureDomElement(element, primitive, key);
    return element;
  }

  private configureDomElement(element: Element, primitive: number, key: string): void {
	const structural = element as HTMLElement;
	// Portable flex and grid children must be allowed to shrink below their
	// content size. Without these defaults a nested Scroll expands the browser
	// document instead of becoming the bounded viewport described by the tree.
	// Explicit MinWidth/MinHeight properties are applied after creation and take
	// precedence over these renderer defaults.
	structural.style.minWidth = '0';
	structural.style.minHeight = '0';
    if (primitive === 10 || primitive === 16) {
      element.addEventListener('compositionstart', () => {
        this.pendingCompositionSync.delete(key);
        this.composingNodes.add(key);
      });
      element.addEventListener('compositionend', () => {
        this.composingNodes.delete(key);
        this.pendingCompositionSync.add(key);
      });
    }
    if (primitive === 1) structural.style.display = 'contents';
    if (primitive === 3 || primitive === 4) {
      const html = element as HTMLElement;
      html.style.display = 'flex';
      html.style.flexDirection = primitive === 3 ? 'row' : 'column';
    } else if (primitive === 5) {
      const html = element as HTMLElement;
      html.style.position = 'relative';
      html.style.display = 'grid';
    } else if (primitive === 6) {
      (element as HTMLElement).style.display = 'grid';
    } else if (primitive === 7) {
      (element as HTMLElement).style.overflow = 'auto';
    } else if (primitive === 14) {
      element.setAttribute('data-volang-platform-view', '');
    }
  }

  private adoptServerRenderedNodes(): void {
    for (const element of Array.from(this.root.querySelectorAll('[data-volang-node]'))) {
      const key = element.getAttribute('data-volang-node');
      if (key === null || key === this.rootKey || this.domNodes.has(key)) {
        throw new Error('server-rendered Volang UI contains a duplicate or invalid node identity');
      }
      const primitiveName = element.getAttribute('data-volang-primitive');
      const primitive = primitiveName === null ? undefined : serverPrimitive(primitiveName);
      if (primitive === undefined) {
        throw new Error('server-rendered Volang UI primitive is invalid');
      }
      this.configureDomElement(element, primitive, key);
      this.domNodes.set(key, element);
    }
    const walker = this.root.ownerDocument.createTreeWalker(this.root, 128);
    const markers: Comment[] = [];
    let current = walker.nextNode();
    while (current !== null) {
      markers.push(current as Comment);
      current = walker.nextNode();
    }
    for (const marker of markers) {
      const prefix = 'volang-text:';
      if (!marker.data.startsWith(prefix)) continue;
      const key = marker.data.slice(prefix.length);
      if (key.length === 0 || this.domNodes.has(key) || marker.parentNode === null) {
        throw new Error('server-rendered Volang UI text marker is invalid');
      }
      let value = marker.nextSibling;
      if (value?.nodeType !== 3) {
        value = this.root.ownerDocument.createTextNode('');
        marker.parentNode.insertBefore(value, marker.nextSibling);
      }
      this.domNodes.set(key, value);
      marker.remove();
    }
  }

  private initialHydrationMatches(batch: UiMutationBatch): boolean {
    const created = new Set<string>();
    for (const mutation of batch.mutations) {
      if (mutation.type !== 'create-element' && mutation.type !== 'create-text') continue;
      const key = uiIdentityKey(mutation.id);
      created.add(key);
      const existing = this.domNodes.get(key);
      if (existing === undefined) continue;
      if (mutation.type === 'create-text') {
        if (existing.nodeType !== 3) return false;
        continue;
      }
      if (!isElement(existing)) return false;
      const primitiveName = existing.getAttribute('data-volang-primitive');
      if (primitiveName === null || serverPrimitive(primitiveName) !== mutation.primitive) return false;
    }
    for (const key of this.domNodes.keys()) {
      if (key !== this.rootKey && !created.has(key)) return false;
    }
    return true;
  }

  private discardAdoptedServerTree(): void {
    for (const key of this.domListeners.keys()) this.clearNodeListeners(key);
    this.root.replaceChildren();
    this.domNodes = new Map([[this.rootKey, this.root]]);
    this.domListeners = new Map();
    this.textSelections = new Map();
    this.composingNodes.clear();
    this.pendingCompositionSync.clear();
    this.pendingControlledScroll.clear();
  }

  private applyDomMutation(mutation: UiMutation): void {
    switch (mutation.type) {
      case 'create-element':
      case 'create-text': {
        const key = uiIdentityKey(mutation.id);
        if (!this.domNodes.has(key)) {
          this.domNodes.set(
            key,
            this.createDomNode(mutation.id, mutation.type === 'create-element' ? mutation.primitive : undefined),
          );
        }
        return;
      }
      case 'set-text':
        this.requireDomNode(mutation.id).nodeValue = mutation.text;
        return;
      case 'set-property': {
        const node = this.requireDomNode(mutation.id);
        const key = uiIdentityKey(mutation.id);
        if (!isElement(node)) {
          throw new Error(
            `Volang UI property ${mutation.property} target ${uiIdentityKey(mutation.id)} is not an element`
            + ` (nodeType=${node.nodeType}, text=${JSON.stringify(node.nodeValue ?? '')})`,
          );
        }
        if (mutation.property === 16 && this.composingNodes.has(key)) return;
        applyProperty(node, mutation.property, mutation.value);
        if (mutation.property === 26 || mutation.property === 27) {
          const scrollNode = node as HTMLElement;
          this.pendingControlledScroll.set(key, {
            x: Number.isFinite(scrollNode.scrollLeft) ? scrollNode.scrollLeft : 0,
            y: Number.isFinite(scrollNode.scrollTop) ? scrollNode.scrollTop : 0,
          });
        }
        return;
      }
      case 'remove-property': {
        const node = this.requireDomNode(mutation.id);
        const key = uiIdentityKey(mutation.id);
        if (!isElement(node)) {
          throw new Error(
            `Volang UI property ${mutation.property} target ${uiIdentityKey(mutation.id)} is not an element`
            + ` (nodeType=${node.nodeType}, text=${JSON.stringify(node.nodeValue ?? '')})`,
          );
        }
        if (mutation.property === 16 && this.composingNodes.has(key)) return;
        removeProperty(node, mutation.property);
        if (mutation.property === 26 || mutation.property === 27) {
          const scrollNode = node as HTMLElement;
          this.pendingControlledScroll.set(key, {
            x: Number.isFinite(scrollNode.scrollLeft) ? scrollNode.scrollLeft : 0,
            y: Number.isFinite(scrollNode.scrollTop) ? scrollNode.scrollTop : 0,
          });
        }
        return;
      }
      case 'listen':
        this.installListener(mutation.id, mutation.listener);
        return;
      case 'unlisten':
        this.removeListener(mutation.id, mutation.event, mutation.handler);
        return;
      case 'insert-before': {
        const parent = this.requireDomNode(mutation.parent);
        const child = this.requireDomNode(mutation.child);
        const before = mutation.before === undefined ? null : this.requireDomNode(mutation.before);
        parent.insertBefore(child, before);
        return;
      }
      case 'remove':
        this.requireDomNode(mutation.parent).removeChild(this.requireDomNode(mutation.child));
        return;
      case 'delete': {
        const key = uiIdentityKey(mutation.id);
        this.clearNodeListeners(key);
        this.composingNodes.delete(key);
        this.pendingCompositionSync.delete(key);
        this.pendingControlledScroll.delete(key);
        this.domNodes.delete(key);
        return;
      }
    }
  }

  private applyPendingCompositionValues(nodes: ReadonlyMap<string, NodeRecord>): Set<string> {
    const synchronized = new Set<string>();
    for (const key of this.pendingCompositionSync) {
      if (this.composingNodes.has(key)) continue;
      const record = nodes.get(key);
      if (record === undefined) {
        synchronized.add(key);
        continue;
      }
      if (record.primitive !== 10 && record.primitive !== 16) {
        throw new Error('Volang UI composition value target must be a text input');
      }
      const node = this.domNodes.get(key);
      if (!isElement(node) || !isInput(node)) {
        throw new Error('Volang UI composition value target is missing');
      }
      const value = record.properties.get(16);
      if (value === undefined) removeProperty(node, 16);
      else applyProperty(node, 16, value);
      synchronized.add(key);
    }
    return synchronized;
  }

  private synchronizeStackChildren(nodes: ReadonlyMap<string, NodeRecord>): void {
    for (const [key, record] of nodes) {
      const node = this.domNodes.get(key);
      if (!isElement(node)) continue;
      const parent = record.parent === undefined ? undefined : nodes.get(record.parent);
      const stacked = parent?.primitive === 5 && !record.properties.has(34);
      if (stacked) {
        (node as HTMLElement).style.gridArea = '1 / 1';
        node.setAttribute('data-volang-stack-child', '');
      } else if (node.hasAttribute('data-volang-stack-child')) {
        if (!record.properties.has(34)) (node as HTMLElement).style.removeProperty('grid-area');
        node.removeAttribute('data-volang-stack-child');
      }
    }
  }

  private restoreLogicalDom(nodes: ReadonlyMap<string, NodeRecord>): void {
    for (const [key, record] of nodes) {
      const parent = this.domNodes.get(key);
      if (parent === undefined) continue;
      let cursor: ChildNode | null = parent.childNodes[0] ?? null;
      for (const child of record.children) {
        const childNode = this.domNodes.get(child);
        if (childNode === undefined) throw new Error(`missing Volang UI logical child ${child}`);
        if (childNode === cursor) {
          cursor = cursor.nextSibling;
          continue;
        }
        parent.insertBefore(childNode, cursor);
        cursor = childNode.nextSibling;
      }
    }
  }

  private synchronizePortals(nodes: ReadonlyMap<string, NodeRecord>): void {
    const portals: Array<{ key: string; layer: bigint; order: number }> = [];
    let order = 0;
    for (const [key, record] of nodes) {
      const value = record.properties.get(40);
      if (value === undefined) {
        order += 1;
        continue;
      }
      if (value.type !== 'i64' || value.value < -1_000_000n || value.value > 1_000_000n) {
        throw new Error('Volang UI portal layer exceeds the portable range');
      }
      let parent = record.parent;
      let connected = key === this.rootKey;
      while (parent !== undefined) {
        if (parent === this.rootKey) connected = true;
        if (nodes.get(parent)?.properties.has(40)) {
          throw new Error('Volang UI portal roots cannot be nested');
        }
        parent = nodes.get(parent)?.parent;
      }
      if (!connected) {
        order += 1;
        continue;
      }
      portals.push({ key, layer: value.value, order });
      order += 1;
    }
    portals.sort((left, right) => left.layer < right.layer
      ? -1
      : left.layer > right.layer ? 1 : left.order - right.order);
    for (const portal of portals) {
      const node = this.domNodes.get(portal.key);
      if (node === undefined) throw new Error(`missing Volang UI portal ${portal.key}`);
      this.root.appendChild(node);
    }
  }

  private applyControlledSelections(nodes: ReadonlyMap<string, NodeRecord>): void {
    for (const [key, record] of nodes) {
      const startValue = record.properties.get(31);
      const lengthValue = record.properties.get(32);
      if (startValue === undefined && lengthValue === undefined) continue;
      if (this.composingNodes.has(key)) continue;
      if (record.primitive !== 10 && record.primitive !== 16) {
        throw new Error('Volang UI controlled selection requires a text input');
      }
      const node = this.domNodes.get(key);
      if (!isElement(node) || !isInput(node)) {
        throw new Error('Volang UI controlled selection target is missing');
      }
      const currentStart = node.selectionStart ?? 0;
      const currentEnd = node.selectionEnd ?? currentStart;
      const start = startValue === undefined
        ? currentStart
        : selectionOffset(startValue, 'selection start');
      const length = lengthValue === undefined
        ? Math.max(0, currentEnd - currentStart)
        : selectionOffset(lengthValue, 'selection length');
      const end = start + length;
      if (!Number.isSafeInteger(end)) {
        throw new Error('Volang UI controlled selection range exceeds safe host indices');
      }
      if (currentStart !== start || currentEnd !== end) node.setSelectionRange(start, end);
      this.textSelections.set(key, textSelectionSignature(node));
    }
  }

  private eventPayload(eventType: number, event: Event, targetOverride?: Element): UiEventPayload {
    const target = targetOverride ?? event.currentTarget;
    if ((eventType === 2 || eventType === 20) && isElement(target) && isInput(target)) {
      if (!isCheckableInput(target) || target.type !== 'range') {
        const start = target.selectionStart ?? 0;
        const end = target.selectionEnd ?? start;
        return {
          type: 'text-input',
          value: target.value,
          selectionStartUtf16: start,
          selectionLengthUtf16: Math.max(0, end - start),
        };
      }
      return { type: 'text', value: target.value };
    }
    if (eventType === 3 && isElement(target) && isInput(target)) {
      return isCheckableInput(target) && target.type === 'checkbox'
        ? { type: 'toggle', value: target.checked }
        : { type: 'text', value: target.value };
    }
    if (eventType === 12 && isElement(target)) {
      return {
        type: 'scroll',
        x: Number.isFinite(target.scrollLeft) ? target.scrollLeft : 0,
        y: Number.isFinite(target.scrollTop) ? target.scrollTop : 0,
        deltaX: 0,
        deltaY: 0,
        unit: 'pixel',
        modifiers: { shift: false, control: false, alt: false, meta: false },
      };
    }
    if (eventType === 7 || eventType === 8) {
      const keyboard = event as KeyboardEvent;
      return {
        type: 'key',
        key: keyboard.key ?? '',
        code: keyboard.code ?? '',
        modifiers: eventModifiers(event),
        repeat: keyboard.repeat === true,
        composing: keyboard.isComposing === true,
      };
    }
    if ((eventType >= 9 && eventType <= 11) || eventType === 18 || eventType === 21) {
      const pointer = event as PointerEvent;
      const pointerType = pointer.pointerType;
      return {
        type: 'pointer',
        x: Number.isFinite(pointer.clientX) ? pointer.clientX : 0,
        y: Number.isFinite(pointer.clientY) ? pointer.clientY : 0,
        button: Number.isInteger(pointer.button) ? pointer.button : 0,
        buttons: Number.isInteger(pointer.buttons) ? pointer.buttons : 0,
        pointerId: BigInt(Number.isSafeInteger(pointer.pointerId) ? pointer.pointerId : 0),
        kind: pointerType === 'mouse' || pointerType === 'pen' || pointerType === 'touch'
          ? pointerType
          : 'unknown',
        modifiers: eventModifiers(event),
      };
    }
    if (eventType >= 13 && eventType <= 15) {
      const target = event.currentTarget;
      const input = isElement(target) && isInput(target) ? target : undefined;
      const start = input?.selectionStart ?? 0;
      const end = input?.selectionEnd ?? start;
      return {
        type: 'composition',
        value: (event as CompositionEvent).data ?? '',
        selectionStartUtf16: start,
        selectionLengthUtf16: Math.max(0, end - start),
      };
    }
    if (eventType === 16) {
      const wheel = event as WheelEvent;
      return {
        type: 'scroll',
        x: Number.isFinite(wheel.clientX) ? wheel.clientX : 0,
        y: Number.isFinite(wheel.clientY) ? wheel.clientY : 0,
        deltaX: Number.isFinite(wheel.deltaX) ? wheel.deltaX : 0,
        deltaY: Number.isFinite(wheel.deltaY) ? wheel.deltaY : 0,
        unit: wheel.deltaMode === 1 ? 'line' : wheel.deltaMode === 2 ? 'page' : 'pixel',
        modifiers: eventModifiers(event),
      };
    }
    if (eventType === 22) {
      const files = Array.from((event as DragEvent).dataTransfer?.files ?? []);
      const paths = files.map((file) => file.webkitRelativePath || file.name);
      return { type: 'text', value: paths.join('\u0000') };
    }
    return { type: 'none' };
  }

  private installListener(id: UiIdentity, listener: UiListener): void {
    const key = uiIdentityKey(id);
    const node = this.requireDomNode(id);
    if (!isElement(node)) throw new Error('Volang UI listener target is not an element');
    this.removeListener(id, listener.event);
    this.logicalOnceDelivered.delete(
      `${key}:${listener.event}:${uiIdentityKey(listener.handler)}`,
    );
    const callback: EventListener = (event) => {
      if (this.sessionEpoch === undefined) return;
      if (listener.event === 22 && event.type === 'dragover') {
        event.preventDefault();
        return;
      }
      if (listener.event >= 21 && listener.event <= 24) event.preventDefault();
      if (listener.event === 12) {
        const expected = this.pendingControlledScroll.get(key);
        const scrollNode = node as HTMLElement;
        if (expected !== undefined
          && Math.abs(scrollNode.scrollLeft - expected.x) < 0.5
          && Math.abs(scrollNode.scrollTop - expected.y) < 0.5) {
          return;
        }
        this.pendingControlledScroll.delete(key);
      }
      if (listener.event === 9
        && node.hasAttribute('data-volang-pointer-capture')
        && typeof node.setPointerCapture === 'function') {
        const pointerId = (event as PointerEvent).pointerId;
        if (Number.isSafeInteger(pointerId)) {
          try {
            node.setPointerCapture(pointerId);
          } catch {
            // Synthetic events and already-cancelled platform sequences may
            // reject capture; their typed event is still delivered.
          }
        }
      }
      if (listener.event === 20 && node.ownerDocument?.activeElement !== node) return;
      if (listener.event === 20 && isInput(node)) {
        const selection = textSelectionSignature(node);
        if (this.textSelections.get(key) === selection) return;
        this.textSelections.set(key, selection);
      } else if (listener.event === 2 && isInput(node)) {
        this.textSelections.set(key, textSelectionSignature(node));
      }
      this.queueListenerEvent(id, listener, event, listener.event === 20 ? node : undefined);
    };
    const attachments = listener.event === 20
      ? [
        { target: node.ownerDocument ?? node, name: 'selectionchange' },
        { target: node, name: 'select' },
        { target: node, name: 'keyup' },
        { target: node, name: 'pointerup' },
      ]
      : listener.event === 22
        ? [
          { target: node as EventTarget, name: 'drop' },
          { target: node as EventTarget, name: 'dragover' },
        ]
        : [{ target: node as EventTarget, name: eventName(listener.event) }];
    for (const attachment of attachments) {
      attachment.target.addEventListener(attachment.name, callback, {
        capture: listener.capture,
        passive: listener.passive,
        once: listener.once,
      });
    }
    let listeners = this.domListeners.get(key);
    if (listeners === undefined) {
      listeners = new Map();
      this.domListeners.set(key, listeners);
    }
    listeners.set(listener.event, { listener, callback, attachments });
  }

  private queueListenerEvent(
    id: UiIdentity,
    listener: UiListener,
    event: Event,
    targetOverride?: Element,
  ): void {
    if (this.sessionEpoch === undefined) return;
    this.eventSequence += 1n;
    const envelope: UiEventEnvelope = {
      sessionEpoch: this.sessionEpoch,
      handler: listener.handler,
      event: listener.event,
      target: id,
      sequence: this.eventSequence,
      payload: this.eventPayload(listener.event, event, targetOverride),
    };
    this.eventFrames.push(encodeUiEvent(envelope, this.limits));
    this.onEvent?.();
  }

  private removeListener(id: UiIdentity, event: number, handler?: UiIdentity): void {
    const key = uiIdentityKey(id);
    const current = this.domListeners.get(key)?.get(event);
    if (current === undefined) return;
    if (handler !== undefined && !sameIdentity(current.listener.handler, handler)) {
      throw new Error('Volang UI browser listener identity mismatch');
    }
    for (const attachment of current.attachments) {
      attachment.target.removeEventListener(attachment.name, current.callback, {
        capture: current.listener.capture,
      });
    }
    this.domListeners.get(key)?.delete(event);
  }

  private clearNodeListeners(key: string): void {
    const listeners = this.domListeners.get(key);
    if (listeners !== undefined) {
      for (const current of listeners.values()) {
        for (const attachment of current.attachments) {
          attachment.target.removeEventListener(attachment.name, current.callback, {
            capture: current.listener.capture,
          });
        }
      }
    }
    this.textSelections.delete(key);
    this.pendingControlledScroll.delete(key);
    this.domListeners.delete(key);
  }

  private rebuild(nodes: Map<string, NodeRecord>): void {
    this.composingNodes.clear();
    this.pendingCompositionSync.clear();
    this.pendingControlledScroll.clear();
    for (const key of this.domListeners.keys()) this.clearNodeListeners(key);
    this.root.replaceChildren();
    this.domNodes = new Map([[this.rootKey, this.root]]);
    this.domListeners = new Map();
    this.textSelections = new Map();
    for (const [key, record] of nodes) {
      if (key !== this.rootKey) {
        this.domNodes.set(key, this.createDomNode(record.id, record.primitive));
      }
    }
    for (const [key, record] of nodes) {
      const node = this.domNodes.get(key);
      if (node === undefined) throw new Error(`cannot rebuild Volang UI node ${key}`);
      if (record.textNode) node.nodeValue = record.text;
      else if (isElement(node)) {
        for (const [property, value] of record.properties) applyProperty(node, property, value);
        for (const listener of record.listeners.values()) this.installListener(record.id, listener);
      }
    }
    for (const [key, record] of nodes) {
      const parent = this.domNodes.get(key);
      if (parent === undefined) continue;
      for (const child of record.children) {
        const childNode = this.domNodes.get(child);
        if (childNode === undefined) throw new Error(`cannot rebuild Volang UI child ${child}`);
        parent.appendChild(childNode);
      }
    }
    this.synchronizeStackChildren(nodes);
    this.synchronizePortals(nodes);
    this.applyControlledSelections(nodes);
    this.synchronizeFocusRequest(nodes);
  }

  recordError(cause: unknown): void {
    this.lastErrorValue = cause instanceof Error ? cause : new Error(String(cause));
  }
}

export interface UiWebImports {
  readonly volang_ui_web_v1: {
    readonly apply_mutation_frame: (pointer: number, length: number) => number;
    readonly next_event_frame_len: () => number;
    readonly read_event_frame: (pointer: number, length: number) => number;
  };
}

export function createUiWebImports(
  adapter: UiDomAdapter,
  memory: () => WebAssembly.Memory,
): UiWebImports {
  function memoryRange(pointer: number, length: number): Uint8Array {
    if (!Number.isSafeInteger(pointer) || !Number.isSafeInteger(length)
      || pointer < 0 || length < 0 || pointer + length > memory().buffer.byteLength) {
      throw new Error('Volang UI Wasm memory range is invalid');
    }
    return new Uint8Array(memory().buffer, pointer, length);
  }
  return {
    volang_ui_web_v1: {
      apply_mutation_frame(pointer, length) {
        try {
          adapter.applyMutationFrame(memoryRange(pointer, length));
          return 0;
        } catch (error) {
          adapter.recordError(error);
          return 1;
        }
      },
      next_event_frame_len() {
        const length = adapter.nextEventFrameLength();
        return length <= 0xffff_fffe ? length : 0xffff_ffff;
      },
      read_event_frame(pointer, length) {
        try {
          return adapter.readEventFrame(memoryRange(pointer, length)) ? 0 : 1;
        } catch (error) {
          adapter.recordError(error);
          return 2;
        }
      },
    },
  };
}

export interface UiVmIsland {
  run(): string;
  runScheduled(): string;
  reload(bytecode: Uint8Array): string;
  takeHostOutput(): Uint8Array | undefined;
  takePendingHostEvents(): ArrayLike<UiVmHostEvent>;
  wakeHostEvent(key: string): boolean;
  wakeHostEventWithData(key: string, data: Uint8Array): boolean;
  takeUiInvalidation?(): boolean;
  setUiLocation?(path: string, invalidate: boolean): boolean;
  setUiViewport?(width: number, height: number, scaleFactor: number, invalidate: boolean): boolean;
  takeUiNavigationRequests?(): ArrayLike<UiVmNavigationRequest>;
  takeUiSystemRequests?(): ArrayLike<UiVmSystemRequest>;
}

export interface UiVmSystemRequest {
  readonly requestId: string;
  readonly frame: Uint8Array;
}

export interface UiVmNavigationRequest {
  readonly kind: 'push' | 'replace' | 'back' | 'forward';
  readonly path?: string;
}

export interface UiVmHostEvent {
  readonly key: string;
  readonly source: string;
  readonly token: string;
  readonly delayMs: number;
  readonly replay: boolean;
}

export interface UiVmDomSessionOptions {
  readonly onError?: (error: Error) => void;
  readonly onPendingHostEvents?: (events: readonly UiVmHostEvent[]) => void;
  readonly systemHost?: UiSystemHost;
  /** Enables an isolated in-memory history for iframe and embedded hosts. */
  readonly initialLocation?: string;
}

export class UiVmDomSession {
  private delivering = false;
  private disposed = false;
  private readonly timers = new Map<string, ReturnType<typeof setTimeout>>();
  private readonly notifiedExternalEvents = new Set<string>();
  private readonly activeSystemRequests = new Set<string>();
  private readonly browserWindow?: Window;
  private readonly virtualHistory?: string[];
  private virtualHistoryIndex = 0;
  private readonly popStateListener: () => void;
  private readonly resizeListener: () => void;
  private readonly systemHost: UiSystemHost;
  private systemEpoch = 1;

  constructor(
    readonly island: UiVmIsland,
    public adapter: UiDomAdapter,
    private readonly options: UiVmDomSessionOptions = {},
  ) {
    this.browserWindow = adapter.root.ownerDocument.defaultView ?? undefined;
    this.systemHost = options.systemHost ?? new UiBrowserSystemHost(adapter.root);
    this.popStateListener = () => this.acceptBrowserLocation();
    this.resizeListener = () => this.acceptBrowserViewport(true);
    if (options.initialLocation !== undefined) {
      validateNavigationPath(options.initialLocation);
      this.virtualHistory = [options.initialLocation];
      this.island.setUiLocation?.(options.initialLocation, false);
    }
    if (this.browserWindow !== undefined) {
      if (this.virtualHistory === undefined) {
        this.island.setUiLocation?.(browserLocation(this.browserWindow), false);
        this.browserWindow.addEventListener('popstate', this.popStateListener);
      }
      this.acceptBrowserViewport(false);
      this.browserWindow.addEventListener('resize', this.resizeListener);
    }
  }

  start(): string {
    if (this.disposed) throw new Error('Volang UI DOM session has been disposed');
    return this.advance(() => this.island.run());
  }

  deliverEvents(): string | undefined {
    if (this.delivering) return undefined;
    this.delivering = true;
    let outcome: string | undefined;
    try {
      while (true) {
        const frame = this.adapter.shiftEventFrame();
        if (frame === undefined) break;
        const pending = Array.from(this.island.takePendingHostEvents())
          .find((event) => event.source === 'replay-gui-event');
        if (pending === undefined) throw new Error('Volang UI event has no waiting UI Island turn');
        if (!this.island.wakeHostEventWithData(pending.key, frame)) {
          throw new Error('Volang UI Island rejected its pending event identity');
        }
        outcome = this.advance(() => this.island.runScheduled());
      }
      return outcome;
    } finally {
      this.delivering = false;
    }
  }

  reload(bytecode: Uint8Array): string {
    if (this.disposed) throw new Error('Volang UI DOM session has been disposed');
    const outcome = this.island.reload(bytecode);
    const frame = this.island.takeHostOutput();
    if (frame === undefined) {
      throw new Error('reloaded Volang UI Island produced no initial mutation frame');
    }
    const replacement = this.adapter.replaceWithInitialFrame(frame);
    if (this.island.takeHostOutput() !== undefined) {
      throw new Error('reloaded Volang UI Island produced multiple initial mutation frames');
    }
    for (const timer of this.timers.values()) clearTimeout(timer);
    this.timers.clear();
    this.notifiedExternalEvents.clear();
    this.activeSystemRequests.clear();
    this.systemEpoch += 1;
    this.systemHost.reset?.();
    this.adapter = replacement;
    this.throwOnTerminalFailure(outcome);
    this.reconcileHostEvents();
    return outcome;
  }

  dispose(): void {
    this.disposed = true;
    this.browserWindow?.removeEventListener('popstate', this.popStateListener);
    this.browserWindow?.removeEventListener('resize', this.resizeListener);
    for (const timer of this.timers.values()) clearTimeout(timer);
    this.timers.clear();
    this.notifiedExternalEvents.clear();
    this.activeSystemRequests.clear();
    this.systemEpoch += 1;
    this.systemHost.dispose?.();
  }

  private advance(run: () => string): string {
    let outcome = run();
    while (true) {
      this.drainMutations();
      this.throwOnTerminalFailure(outcome);
      this.reconcileNavigationRequests();
      this.reconcileSystemRequests();
      this.reconcileHostEvents();
      const pending = Array.from(this.island.takePendingHostEvents())
        .find((event) => event.source === 'replay-gui-event');
      // An invalidation may arrive while the application is suspended on an
      // application-host request. Keep it pending until the root render loop
      // reaches its GUI replay boundary instead of consuming it early.
      if (pending === undefined || this.island.takeUiInvalidation?.() !== true) return outcome;
      if (!this.island.wakeHostEventWithData(pending.key, this.adapter.createInvalidationFrame())) {
        throw new Error('Volang UI Island rejected its invalidation identity');
      }
      outcome = this.island.runScheduled();
    }
  }

  private reconcileNavigationRequests(): void {
    const requests = Array.from(this.island.takeUiNavigationRequests?.() ?? []);
    if (requests.length === 0) return;
    if (this.virtualHistory !== undefined) {
      for (const request of requests) {
        switch (request.kind) {
          case 'push': {
            const path = request.path;
            if (path === undefined) throw new Error('Volang UI navigation request has no path');
            validateNavigationPath(path);
            this.virtualHistory.splice(this.virtualHistoryIndex + 1);
            this.virtualHistory.push(path);
            this.virtualHistoryIndex = this.virtualHistory.length - 1;
            break;
          }
          case 'replace': {
            const path = request.path;
            if (path === undefined) throw new Error('Volang UI navigation request has no path');
            validateNavigationPath(path);
            this.virtualHistory[this.virtualHistoryIndex] = path;
            break;
          }
          case 'back':
            if (this.virtualHistoryIndex > 0) this.virtualHistoryIndex -= 1;
            break;
          case 'forward':
            if (this.virtualHistoryIndex + 1 < this.virtualHistory.length) this.virtualHistoryIndex += 1;
            break;
          default: throw new Error('Volang UI navigation request kind is invalid');
        }
      }
      this.island.setUiLocation?.(this.virtualHistory[this.virtualHistoryIndex] as string, true);
      return;
    }
    const window = this.browserWindow;
    if (window === undefined) return;
    for (const request of requests) {
      switch (request.kind) {
        case 'push':
        case 'replace': {
          const path = request.path;
          if (path === undefined) throw new Error('Volang UI navigation request has no path');
          const url = checkedNavigationUrl(window, path);
          if (request.kind === 'push') window.history.pushState(null, '', url);
          else window.history.replaceState(null, '', url);
          break;
        }
        case 'back': window.history.back(); break;
        case 'forward': window.history.forward(); break;
        default: throw new Error('Volang UI navigation request kind is invalid');
      }
    }
  }

  private reconcileSystemRequests(): void {
    const requests = Array.from(this.island.takeUiSystemRequests?.() ?? []);
    if (requests.length === 0) return;
    const pending = Array.from(this.island.takePendingHostEvents());
    const epoch = this.systemEpoch;
    for (const request of requests) {
      if (this.activeSystemRequests.has(request.requestId)) {
        throw new Error(`duplicate Volang UI system request ${request.requestId}`);
      }
      const wait = pending.find((event) => event.source === 'replay-ui-system'
        && event.token === request.requestId);
      if (wait === undefined) {
        throw new Error(`Volang UI system request ${request.requestId} has no replay waiter`);
      }
      this.activeSystemRequests.add(request.requestId);
      void this.systemHost.execute(request.frame).then((response) => {
        if (this.disposed || epoch !== this.systemEpoch) return;
        const current = Array.from(this.island.takePendingHostEvents())
          .find((event) => event.source === 'replay-ui-system'
            && event.token === request.requestId && event.key === wait.key);
        if (current === undefined) {
          throw new Error(`Volang UI system waiter ${request.requestId} became stale`);
        }
        if (!this.island.wakeHostEventWithData(current.key, response)) {
          throw new Error(`Volang UI Island rejected system response ${request.requestId}`);
        }
        this.activeSystemRequests.delete(request.requestId);
        this.advance(() => this.island.runScheduled());
      }).catch((cause) => {
        this.activeSystemRequests.delete(request.requestId);
        if (this.disposed || epoch !== this.systemEpoch) return;
        this.reportAsyncError(cause);
      });
    }
  }

  private acceptBrowserLocation(): void {
    if (this.disposed || this.browserWindow === undefined || this.virtualHistory !== undefined) return;
    try {
      const changed = this.island.setUiLocation?.(browserLocation(this.browserWindow), true);
      if (changed === true) this.advance(() => 'suspended_for_host_events');
    } catch (cause) {
      const error = cause instanceof Error ? cause : new Error(String(cause));
      this.adapter.recordError(error);
      if (this.options.onError !== undefined) this.options.onError(error);
      else setTimeout(() => { throw error; }, 0);
    }
  }

  private acceptBrowserViewport(invalidate: boolean): void {
    if (this.disposed || this.browserWindow === undefined) return;
    try {
      const changed = this.island.setUiViewport?.(
        this.browserWindow.innerWidth,
        this.browserWindow.innerHeight,
        this.browserWindow.devicePixelRatio,
        invalidate,
      );
      if (changed === true && invalidate) this.advance(() => 'suspended_for_host_events');
    } catch (cause) {
      const error = cause instanceof Error ? cause : new Error(String(cause));
      this.adapter.recordError(error);
      if (this.options.onError !== undefined) this.options.onError(error);
      else setTimeout(() => { throw error; }, 0);
    }
  }

  private reconcileHostEvents(): void {
    if (this.disposed) return;
    const pending = Array.from(this.island.takePendingHostEvents());
    const liveTimers = new Set(
      pending.filter((event) => event.source === 'timer').map((event) => event.key),
    );
    for (const [key, timer] of this.timers) {
      if (!liveTimers.has(key)) {
        clearTimeout(timer);
        this.timers.delete(key);
      }
    }
    for (const event of pending) {
      if (event.source !== 'timer' || this.timers.has(event.key)) continue;
      const timer = setTimeout(() => this.wakeTimer(event), Math.max(0, event.delayMs));
      this.timers.set(event.key, timer);
    }

    const external = pending.filter((event) => event.replay
      && event.source !== 'replay-gui-event'
      && event.source !== 'replay-ui-system');
    const liveExternal = new Set(external.map((event) => event.key));
    for (const key of this.notifiedExternalEvents) {
      if (!liveExternal.has(key)) this.notifiedExternalEvents.delete(key);
    }
    const fresh = external.filter((event) => !this.notifiedExternalEvents.has(event.key));
    for (const event of fresh) this.notifiedExternalEvents.add(event.key);
    if (fresh.length > 0) this.options.onPendingHostEvents?.(fresh);
  }

  private wakeTimer(event: UiVmHostEvent): void {
    this.timers.delete(event.key);
    if (this.disposed || !this.island.wakeHostEvent(event.key)) return;
    try {
      this.advance(() => this.island.runScheduled());
    } catch (cause) {
      const error = cause instanceof Error ? cause : new Error(String(cause));
      this.adapter.recordError(error);
      if (this.options.onError !== undefined) this.options.onError(error);
      else setTimeout(() => { throw error; }, 0);
    }
  }

  private reportAsyncError(cause: unknown): void {
    const error = cause instanceof Error ? cause : new Error(String(cause));
    this.adapter.recordError(error);
    if (this.options.onError !== undefined) this.options.onError(error);
    else setTimeout(() => { throw error; }, 0);
  }

  private drainMutations(): void {
    while (true) {
      const frame = this.island.takeHostOutput();
      if (frame === undefined) return;
      this.adapter.applyMutationFrame(frame);
    }
  }

  private throwOnTerminalFailure(outcome: string): void {
    if (outcome === 'blocked' || outcome === 'panicked' || outcome.startsWith('error')) {
      throw new Error(`Volang UI Island stopped with ${outcome}`);
    }
  }
}

function browserLocation(window: Window): string {
  return `${window.location.pathname}${window.location.search}${window.location.hash}`;
}

function checkedNavigationUrl(window: Window, path: string): URL {
  validateNavigationPath(path);
  const url = new URL(path, window.location.href);
  if (url.origin !== window.location.origin) {
    throw new Error('Volang UI navigation must stay on the current origin');
  }
  return url;
}

function validateNavigationPath(path: string): void {
  if (!path.startsWith('/') || path.startsWith('//') || path.length > 16 * 1024
    || [...path].some((character) => character === '\\' || character.charCodeAt(0) < 0x20)) {
    throw new Error('Volang UI navigation path is invalid');
  }
}

export function connectUiVmToDom(
  island: UiVmIsland,
  root: HTMLElement,
  options: Omit<UiDomAdapterOptions, 'onEvent'> & UiVmDomSessionOptions = {},
): UiVmDomSession {
  let session: UiVmDomSession | undefined;
  const { onError, onPendingHostEvents, systemHost, initialLocation, ...adapterOptions } = options;
  const adapter = new UiDomAdapter(root, {
    ...adapterOptions,
    onEvent: () => session?.deliverEvents(),
  });
  session = new UiVmDomSession(island, adapter, {
    onError, onPendingHostEvents, systemHost, initialLocation,
  });
  return session;
}
