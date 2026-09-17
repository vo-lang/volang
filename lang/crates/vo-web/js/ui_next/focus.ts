const candidates = 'a[href],area[href],button,input,select,textarea,iframe,object,embed,[tabindex],[contenteditable]';
const focusProjections = new WeakMap<HTMLElement, HTMLElement>();

/** An owned editing surface shares its stable native control's reload identity. */
export function registerFocusProjection(surface: HTMLElement, control: HTMLElement): () => void {
  if (surface.ownerDocument !== control.ownerDocument || surface === control || focusProjections.has(surface)) {
    throw new Error('invalid UI focus projection');
  }
  focusProjections.set(surface, control);
  return () => {if (focusProjections.get(surface) === control) focusProjections.delete(surface);};
}

export function nativeFocusTarget(surface: HTMLElement | null): HTMLElement | null {
  return surface && (focusProjections.get(surface) ?? surface);
}

/** The actual focused control inside accessible open shadow roots. */
export function activeElement(document: Document): HTMLElement | null {
  let active = document.activeElement;
  while (active?.shadowRoot?.activeElement) active = active.shadowRoot.activeElement;
  return active as HTMLElement | null;
}

/** Keep a contenteditable selection when its unchanged DOM range is moved.
 * Restore before focus: editor focus handlers may immediately read selection. */
export function captureContentSelection(element: HTMLElement): (() => void) | undefined {
  if (!element.isContentEditable) return;
  const root = element.getRootNode() as Document | ShadowRoot;
  const selection = 'getSelection' in root ? (root as Document).getSelection() : element.ownerDocument.getSelection();
  if (!selection?.anchorNode || !selection.focusNode ||
    !element.contains(selection.anchorNode) || !element.contains(selection.focusNode)) return;
  const {anchorNode, anchorOffset, focusNode, focusOffset} = selection;
  const endpoint = (node: Node, offset: number) => {
    const text = node.nodeValue, child = node.childNodes[offset], length = node.childNodes.length;
    return () => element.contains(node) && node.nodeValue === text &&
      (text === null ? node.childNodes.length === length && node.childNodes[offset] === child : offset <= text.length);
  };
  const validAnchor = endpoint(anchorNode, anchorOffset), validFocus = endpoint(focusNode, focusOffset);
  return () => {
    if (validAnchor() && validFocus() && (selection.anchorNode !== anchorNode || selection.anchorOffset !== anchorOffset ||
      selection.focusNode !== focusNode || selection.focusOffset !== focusOffset)) {
      selection.setBaseAndExtent(anchorNode, anchorOffset, focusNode, focusOffset);
    }
  };
}

function composedParent(element: Element): Element | null {
  if (element.assignedSlot) return element.assignedSlot;
  if (element.parentElement) return element.parentElement;
  const root = element.getRootNode();
  return 'host' in root ? (root as ShadowRoot).host : null;
}

export function composedContains(root: Element, element: Element): boolean {
  for (let current: Element | null = element; current; current = composedParent(current)) if (current === root) return true;
  return false;
}

function isInactive(element: Element): boolean {
  for (let current: Element | null = element; current; current = composedParent(current)) {
    // Native discrete exits keep layout boxes after presentation has closed.
    if (current.hasAttribute('inert') || (current.hasAttribute('popover') && !current.matches(':popover-open'))) return true;
  }
  return false;
}

interface FocusEntry { element: HTMLElement; index: number; focusable: boolean; children?: HTMLElement[] }

/** Whether a previously focusable control is still available for focus. */
export function focusAvailable(element: HTMLElement): boolean {
  if (element.matches(':disabled') || isInactive(element) || !element.getClientRects().length) return false;
  const visibility = element.ownerDocument.defaultView!.getComputedStyle(element).visibility;
  return visibility !== 'hidden' && visibility !== 'collapse';
}

// Shadow hosts and slots own nested tab-order scopes. Positive tabindex values
// are sorted inside their own scope, before each owner's order is flattened.
function focusScope(children: readonly Element[]): HTMLElement[] {
  const entries: FocusEntry[] = [];
  const visit = (element: HTMLElement): void => {
    if (isInactive(element)) return;
    const index = element.isContentEditable && !element.hasAttribute('tabindex') ? 0 : element.tabIndex;
    const focusable = element.matches(candidates) && index >= 0 && focusAvailable(element);
    const shadow = element.shadowRoot;
    const slot = element.localName === 'slot' && typeof (element as HTMLSlotElement).assignedElements === 'function'
      ? element as HTMLSlotElement : undefined;
    if (shadow || slot) {
      const ownerIndex = element.hasAttribute('tabindex') ? element.tabIndex : 0;
      if (ownerIndex < 0) return;
      const assigned = slot?.assignedElements({ flatten: true });
      const nested = shadow ? [...shadow.children] : assigned?.length ? assigned : [...element.children];
      entries.push({ element, index: ownerIndex, focusable: focusable && !shadow?.delegatesFocus, children: focusScope(nested) });
    } else {
      if (focusable) entries.push({ element, index, focusable: true });
      for (const child of element.children) visit(child as HTMLElement);
    }
  };
  for (const child of children) visit(child as HTMLElement);
  entries.sort((a, b) => a.index === b.index ? 0 : a.index === 0 ? 1 : b.index === 0 ? -1 : a.index - b.index);
  return entries.flatMap(entry => [...(entry.focusable ? [entry.element] : []), ...(entry.children ?? [])]);
}

/** The tab order of visible native controls in one focus scope. */
export function tabOrder(root: HTMLElement): HTMLElement[] {
  const elements = focusScope([...(root.shadowRoot ?? root).children]);
  return elements.filter(element => {
    const radio = element as HTMLInputElement;
    if (element.localName !== 'input' || radio.type !== 'radio' || !radio.name) return true;
    const group = elements.filter(candidate => candidate.localName === 'input' && (candidate as HTMLInputElement).type === 'radio'
      && (candidate as HTMLInputElement).name === radio.name && (candidate as HTMLInputElement).form === radio.form
      && candidate.getRootNode() === element.getRootNode()) as HTMLInputElement[];
    return element === (group.find(candidate => candidate.checked) ?? group[0]);
  });
}

/** Keep traversal inside the modal, including on platforms that skip buttons in
 * the browser's default Tab order. Other control keys keep their native behavior. */
export function containTab(event: KeyboardEvent, root: HTMLElement): void {
  if (event.defaultPrevented || event.key !== 'Tab' || event.ctrlKey || event.altKey || event.metaKey || event.isComposing) return;
  const elements = tabOrder(root);
  const active = activeElement(root.ownerDocument);
  const index = elements.indexOf(active as HTMLElement);
  event.preventDefault();
  const next = index < 0 ? (event.shiftKey ? elements.length - 1 : 0)
    : (index + (event.shiftKey ? -1 : 1) + elements.length) % elements.length;
  (elements[next] ?? root).focus();
}
