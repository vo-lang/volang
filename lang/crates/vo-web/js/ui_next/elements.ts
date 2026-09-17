/** Browser element semantics shared by creation, preflight and hydration. */
export const htmlNamespace = 'http://www.w3.org/1999/xhtml';
export const svgNamespace = 'http://www.w3.org/2000/svg';
export const booleanAttributes = new Set(['allowfullscreen', 'async', 'autofocus', 'autoplay', 'checked',
  'controls', 'default', 'defer', 'disabled', 'formnovalidate', 'hidden', 'inert', 'ismap', 'itemscope',
  'loop', 'multiple', 'muted', 'nomodule', 'novalidate', 'open', 'playsinline', 'readonly', 'required',
  'reversed', 'selected']);
export const voidElements = new Set(['area', 'base', 'br', 'col', 'embed', 'hr', 'img', 'input', 'link', 'meta', 'param', 'source', 'track', 'wbr']);

export function elementNamespace(kind: string, parentKind: string, parentNamespace: string): string {
  if (kind === 'svg') return svgNamespace;
  return parentNamespace === svgNamespace && !['foreignObject', 'desc', 'title'].includes(parentKind)
    ? svgNamespace : htmlNamespace;
}

export function validElement(kind: string, namespace: string): boolean {
  return namespace === svgNamespace ? /^[A-Za-z][A-Za-z0-9-]*$/.test(kind) : /^[a-z][a-z0-9-]*$/.test(kind);
}

/** Reconstruct the native initial value without confusing normalization with an
 * early user edit. Resetting an isolated clone handles select defaults, numeric
 * input sanitization and textarea newlines using the browser's own rules. */
function resetControl(element: HTMLElement): HTMLInputElement | HTMLTextAreaElement | HTMLSelectElement {
  const copy = element.cloneNode(true) as HTMLInputElement | HTMLTextAreaElement | HTMLSelectElement;
  copy.removeAttribute('form');
  const form = element.ownerDocument.createElement('form');
  form.append(copy);
  HTMLFormElement.prototype.reset.call(form);
  return copy;
}

export function initialControlValue(element: HTMLElement): string {
  return resetControl(element).value;
}

export function initialControlSelection(element: HTMLSelectElement): string[] {
  return [...(resetControl(element) as HTMLSelectElement).selectedOptions].map(option => option.value);
}

/** Radio defaults resolve across the entire native group. Cache the group once
 * per adoption so mutually checked markup is not mistaken for a pre-boot edit. */
export function initialControlChecked(element: HTMLInputElement, cache: Map<HTMLInputElement, boolean>): boolean {
  if (element.type !== 'radio' || !element.name) return element.defaultChecked;
  const existing = cache.get(element);
  if (existing !== undefined) return existing;
  const form = element.ownerDocument.createElement('form');
  const pairs: [HTMLInputElement, HTMLInputElement][] = [];
  for (const candidate of (element.getRootNode() as ParentNode).querySelectorAll<HTMLInputElement>('input')) {
    if (candidate.type !== 'radio' || candidate.name !== element.name || candidate.form !== element.form) continue;
    const copy = candidate.cloneNode() as HTMLInputElement;
    copy.removeAttribute('form');
    form.append(copy);
    pairs.push([candidate, copy]);
  }
  HTMLFormElement.prototype.reset.call(form);
  for (const [original, copy] of pairs) cache.set(original, copy.checked);
  return cache.get(element) ?? element.defaultChecked;
}
