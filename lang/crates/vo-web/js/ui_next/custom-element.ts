import type { WidgetProvider } from './widgets.js';

export interface CustomElementOptions {
  /** Register an autonomous custom element in the root's document before mount. */
  tag: string;
  properties?: readonly string[];
  events?: readonly string[];
}

const encoder = new TextEncoder();
const owns = (value: object, name: string): boolean => Object.prototype.hasOwnProperty.call(value, name);
const record = (value: unknown): value is Record<string, unknown> => value !== null && typeof value === 'object' && !Array.isArray(value);
const reservedTags = new Set(['annotation-xml', 'color-profile', 'font-face', 'font-face-src', 'font-face-uri', 'font-face-format', 'font-face-name', 'missing-glyph']);

/** Optional adapter supplied by the package which owns the element definition.
 * JSON crosses the guest boundary; native objects remain inside the widget. */
export function createCustomElementWidget(options: CustomElementOptions): WidgetProvider {
  const { tag } = options;
  const properties = [...(options.properties ?? [])], events = [...(options.events ?? [])];
  if (!/^[a-z][a-z0-9._-]*-[a-z0-9._-]*$/.test(tag) || tag.length > 128 || reservedTags.has(tag)) throw new Error('invalid autonomous custom element name');
  if (properties.length > 64 || new Set(properties).size !== properties.length
    || properties.some(name => typeof name !== 'string' || !/^[A-Za-z_$][\w$]*$/.test(name) || name.length > 128 || ['__proto__', 'prototype', 'constructor'].includes(name))) throw new Error('invalid custom element properties');
  if (events.length > 32 || new Set(events).size !== events.length
    || events.some(name => typeof name !== 'string' || !name || name.length > 128 || /\s/.test(name))) throw new Error('invalid custom element events');
  const allowedProperties = new Set(properties);
  const parse = (value: string) => {
    if (value.length > 65536 || encoder.encode(value).length > 65536) throw new Error('custom element input exceeds 64 KiB');
    const data: unknown = JSON.parse(value);
    if (!record(data) || data.version !== 1 || !record(data.attributes)
      || !(data.properties === null || record(data.properties))) throw new Error('invalid custom element input');
    const attributes = data.attributes as Record<string, string>;
    const values = (data.properties ?? {}) as Record<string, unknown>;
    if (Object.keys(attributes).length > 64 || Object.entries(attributes).some(([name, value]) => typeof value !== 'string'
      || !/^[a-z_:][a-z0-9_:.-]*$/.test(name) || /^on|^data-vo-/.test(name))) throw new Error('invalid custom element attributes');
    if (Object.keys(values).some(name => !allowedProperties.has(name))) throw new Error('undeclared custom element property');
    return { attributes, values };
  };
  return ({ element: container, value, emit, signal }) => {
    const initial = parse(value);
    signal.throwIfAborted();
    const definition = container.ownerDocument.defaultView?.customElements.get(tag);
    if (!definition) throw new Error(`Load the ${tag} definition in this document before mounting it.`);
    const element = container.ownerDocument.createElement(tag);
    if (!(element instanceof definition)) throw new Error(`The ${tag} constructor failed.`);
    const target = element as unknown as Record<string, unknown>;
    let disposed = false, previousAttributes: Record<string, string> = {};
    const previousProperties = new Map<string, string>();
    const publish = (event: Event) => {
      if (disposed) return;
      try {
        const detail = JSON.stringify('detail' in event ? event.detail : null) ?? 'null';
        if (detail.length > 16384 || encoder.encode(detail).length > 16384) throw new Error('custom element event exceeds 16 KiB');
        emit(JSON.stringify({ type: event.type, detail, error: '' }));
      } catch (error) {
        emit(JSON.stringify({ type: event.type, detail: '', error: String((error as Error)?.message ?? error).slice(0, 1024) }));
      }
    };
    const dispose = () => {
      if (disposed) return;
      disposed = true;
      signal.removeEventListener('abort', dispose);
      for (const name of events) element.removeEventListener(name, publish);
      element.remove();
      previousProperties.clear(); previousAttributes = {};
    };
    const apply = ({ attributes, values }: ReturnType<typeof parse>) => {
      for (const name of Object.keys(previousAttributes)) if (!owns(attributes, name)) element.removeAttribute(name);
      for (const [name, value] of Object.entries(attributes)) if (previousAttributes[name] !== value) element.setAttribute(name, value);
      previousAttributes = attributes;
      for (const name of properties) {
        if (owns(values, name)) {
          const encoded = JSON.stringify(values[name]);
          if (!previousProperties.has(name) || previousProperties.get(name) !== encoded) {
            target[name] = values[name];
            previousProperties.set(name, encoded);
          }
        } else if (previousProperties.has(name)) {
          target[name] = undefined;
          previousProperties.delete(name);
        }
      }
    };
    signal.addEventListener('abort', dispose, { once: true });
    for (const name of events) element.addEventListener(name, publish);
    try {
      apply(initial);
      signal.throwIfAborted();
      container.append(element);
    } catch (error) { dispose(); throw error; }
    return { update(value) { if (!disposed) apply(parse(value)); }, dispose };
  };
}
