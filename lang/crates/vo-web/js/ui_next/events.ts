import { EVENT_KINDS, EVENT_FLAGS_MASK, EVENT_CAPTURE, EVENT_PREVENT_DEFAULT, EVENT_STOP_PROPAGATION, EVENT_PASSIVE, EVENT_CAPTURE_POINTER, EVENT_LATEST, MAX_EVENT_KEYS, MAX_EVENT_KEY_BYTES, MAX_EVENT_MODIFIERS, KEY_ALT, KEY_CTRL, KEY_META, KEY_SHIFT, KEY_MODIFIERS_MASK } from './generated/protocol.js';

const kinds = new Set<string>(EVENT_KINDS);
export interface EventBinding {
  kind: string;
  capture: boolean;
  preventDefault: boolean;
  stopPropagation: boolean;
  passive: boolean;
  capturePointer: boolean;
  latest: boolean;
  flags: number;
  binding: string;
  keys?: ReadonlySet<string>;
  modifiers?: ReadonlySet<number>;
}
export interface NativeListener extends EventBinding { invoke: EventListener; dispose?: () => void }

export function eventBinding(name: string, value?: string): EventBinding {
  const parts = name.split(':');
  if (parts.length > 2 || !kinds.has(parts[0]) || (parts.length === 2 && parts[1] !== 'capture')) throw new Error('unsupported UI event');
  const capture = parts.length === 2;
  const separator = value?.indexOf('|') ?? -1;
  const rawFlags = separator < 0 ? value : value!.slice(0, separator);
  if (rawFlags !== undefined && !/^(0|[1-9][0-9]*)$/.test(rawFlags)) throw new Error('invalid UI event options');
  const flags = rawFlags === undefined ? (capture ? EVENT_CAPTURE : 0) : Number(rawFlags);
  if (!Number.isSafeInteger(flags) || flags > EVENT_FLAGS_MASK || (flags & ~EVENT_FLAGS_MASK) !== 0 || Boolean(flags & EVENT_CAPTURE) !== capture
    || (flags & EVENT_PASSIVE && flags & EVENT_PREVENT_DEFAULT)) throw new Error('invalid UI event options');
  let keys: ReadonlySet<string> | undefined;
  if ((flags & EVENT_CAPTURE_POINTER) && parts[0] !== 'pointerdown') throw new Error('pointer capture requires a pointerdown listener');
  if ((flags & EVENT_LATEST) && parts[0] !== 'pointermove') throw new Error('latest delivery requires a pointermove listener');
  let modifiers: ReadonlySet<number> | undefined;
  if (layoutObservation(parts[0]) && flags !== 0) throw new Error(parts[0] + ' observations do not support native event options');
  if (separator >= 0) {
    const filter: unknown = JSON.parse(value!.slice(separator + 1));
    if (!['keydown', 'keyup'].includes(parts[0])) throw new Error('keyboard filters require keyboard events');
    let values: unknown = filter;
    if (!Array.isArray(filter)) {
      if (!filter || typeof filter !== 'object' || Object.keys(filter).some(key => !['keys', 'modifiers'].includes(key))) {
        throw new Error('invalid UI keyboard filter');
      }
      const object = filter as { keys?: unknown; modifiers?: unknown };
      if (!Array.isArray(object.modifiers) || object.modifiers.length === 0 || object.modifiers.length > MAX_EVENT_MODIFIERS ||
        object.modifiers.some(mask => !Number.isSafeInteger(mask) || mask < 0 || mask > KEY_MODIFIERS_MASK || (mask & ~KEY_MODIFIERS_MASK) !== 0) ||
        new Set(object.modifiers).size !== object.modifiers.length) throw new Error('invalid UI modifier filter');
      modifiers = new Set(object.modifiers);
      values = object.keys;
    }
    const encoder = new TextEncoder(), decoder = new TextDecoder();
    if (values == null && modifiers) values = [];
    if (!Array.isArray(values) || values.length === 0 && !modifiers || values.length > MAX_EVENT_KEYS
      || values.some(key => {
        if (typeof key !== 'string' || !key.length) return true;
        const encoded = encoder.encode(key);
        return encoded.length > MAX_EVENT_KEY_BYTES || decoder.decode(encoded) !== key;
      })
      || new Set(values).size !== values.length) throw new Error('invalid UI key filter');
    if (values.length) keys = new Set(values);
  }
  return { kind: parts[0], capture, flags, binding: value ?? String(flags), keys, modifiers, preventDefault: Boolean(flags & EVENT_PREVENT_DEFAULT),
    stopPropagation: Boolean(flags & EVENT_STOP_PROPAGATION), passive: Boolean(flags & EVENT_PASSIVE), capturePointer: Boolean(flags & EVENT_CAPTURE_POINTER), latest: Boolean(flags & EVENT_LATEST) };
}

/** Match before native prevention; ordinary typing and composition stay native. */
export function matchesKeyboard(binding: EventBinding, event: KeyboardEvent, composing = false): boolean {
  if (!binding.keys && !binding.modifiers) return true;
  if (event.isComposing || composing) return false;
  if (binding.keys && !binding.keys.has(event.key)) return false;
  const mask = (event.altKey ? KEY_ALT : 0) | (event.ctrlKey ? KEY_CTRL : 0) |
    (event.metaKey ? KEY_META : 0) | (event.shiftKey ? KEY_SHIFT : 0);
  return !binding.modifiers || binding.modifiers.has(mask);
}

export function removeListener(node: Node, listener: NativeListener): void {
  listener.dispose?.();
  node.removeEventListener(listener.kind, listener.invoke, listener.capture);
}

export function layoutObservation(kind: string): kind is 'viewport' | 'size' {
  return kind === 'viewport' || kind === 'size';
}
