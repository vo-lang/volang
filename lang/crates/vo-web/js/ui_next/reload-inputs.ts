import type { Batch } from './generated/protocol.js';
import { activeElement, nativeFocusTarget } from './focus.js';
import { applySelection, selectedValues } from './selection.js';

type Control = HTMLInputElement | HTMLTextAreaElement | HTMLSelectElement;

/** Native edits use unambiguous explicit IDs and matching control types.
 * Controlled values come from the guest's typed state checkpoint. */
export function captureReloadInputs(container: HTMLElement) {
  const active = nativeFocusTarget(activeElement(container.ownerDocument));
  const identified = new Map<string, Element | null>();
  for (const element of container.querySelectorAll('[id]')) identified.set(element.id, identified.has(element.id) ? null : element);
  const focused = active && identified.get(active.id) === active ? active.id : undefined;
  const scrolling = [...identified].flatMap(([id, element]) => element && (element.scrollTop || element.scrollLeft)
    ? [{ id, tag: element.localName, top: element.scrollTop, left: element.scrollLeft }] : []);
  const controls = [...container.querySelectorAll<Control>('input[id], textarea[id], select[id]')]
    .filter(control => identified.get(control.id) === control && !(control.localName === 'input' && control.type === 'file'))
    .map(control => ({
      id: control.id, tag: control.localName, type: control.type, value: control.value,
      checked: control.localName === 'input' ? (control as HTMLInputElement).checked : undefined,
      values: control.localName === 'select' ? selectedValues(control as HTMLSelectElement) : undefined,
      selectedIndex: control.localName === 'select' ? (control as HTMLSelectElement).selectedIndex : -1,
      start: 'selectionStart' in control ? control.selectionStart : null,
      end: 'selectionEnd' in control ? control.selectionEnd : null,
      direction: 'selectionDirection' in control ? control.selectionDirection : null,
      top: control.scrollTop, left: control.scrollLeft,
    }));
  return (next: HTMLElement, batch?: Batch) => {
    const ids = new Map<number, string>(), bound = new Set<number>();
    for (const mutation of batch?.mutations ?? []) {
      if (mutation.op === 'attr' && mutation.name.toLowerCase() === 'id') ids.set(mutation.id, mutation.value);
      if (mutation.op === 'selection' || (mutation.op === 'attr' && ['value', 'checked'].includes(mutation.name.toLowerCase()))) bound.add(mutation.id);
    }
    const controlled = new Set([...bound].map(id => ids.get(id)));
    const elements = new Map<string, HTMLElement | null>();
    for (const element of next.querySelectorAll<HTMLElement>('[id]')) elements.set(element.id, elements.has(element.id) ? null : element);
    for (const saved of controls) {
      const control = elements.get(saved.id) as Control | null | undefined;
      if (!control || control.localName !== saved.tag || control.type !== saved.type) continue;
      if (batch && !controlled.has(saved.id)) {
        if (saved.values) {
          const select = control as HTMLSelectElement;
          if (select.multiple) applySelection(select, saved.values);
          else select.selectedIndex = saved.selectedIndex < 0 ? -1 : select.options[saved.selectedIndex]?.value === saved.value
            ? saved.selectedIndex : [...select.options].findIndex(option => option.value === saved.value);
        }
        else control.value = saved.value;
        if (saved.checked !== undefined) (control as HTMLInputElement).checked = saved.checked;
      }
      if (saved.start !== null && saved.end !== null && control.value === saved.value && 'setSelectionRange' in control) {
        control.setSelectionRange(saved.start, saved.end, saved.direction ?? undefined);
      }
      control.scrollTo({top:saved.top, left:saved.left, behavior:'instant'});
    }
    for (const saved of scrolling) {
      const element = elements.get(saved.id);
      // Source reload restores a snapshot immediately, including on pages that
      // opt into smooth scrolling for ordinary navigation.
      if (element?.localName === saved.tag) element.scrollTo({top:saved.top, left:saved.left, behavior:'instant'});
    }
    if (focused) elements.get(focused)?.focus({ preventScroll: true });
  };
}
