import type { PointerData } from './generated/protocol.js';

/** Snapshot only the browser's PointerEvent interface; ordinary input is null. */
export function pointerData(event: Event): PointerData | null {
  if (!(event instanceof PointerEvent)) return null;
  return { id: event.pointerId, clientX: event.clientX, clientY: event.clientY,
    buttons: event.buttons, pressure: event.pressure, isPrimary: event.isPrimary };
}

interface Capture {
  pointers: Set<number>;
  lost: EventListener;
}

/** Native capture belongs to a mounted element, independent of render revision. */
export class PointerHost {
  private readonly captures = new Map<Element, Capture>();

  capture(element: Element, event: Event): void {
    // A constructed event has no active native pointer to capture.
    if (!(event instanceof PointerEvent) || !event.isTrusted || !event.buttons || !element.isConnected) return;
    if (!this.set(element, event.pointerId)) return;
    if (!element.hasPointerCapture(event.pointerId)) return;
    // A later listener can replace a still-pending capture before a native
    // lostpointercapture event exists. Keep one owner for that pointer.
    for (const [owner, capture] of this.captures) {
      if (owner === element) continue;
      capture.pointers.delete(event.pointerId);
      if (!capture.pointers.size) this.forget(owner);
    }
    let capture = this.captures.get(element);
    if (!capture) {
      const pointers = new Set<number>();
      const lost: EventListener = event => {
        if (event.target !== element || !event.isTrusted) return;
        pointers.delete((event as PointerEvent).pointerId);
        if (!pointers.size) this.forget(element);
      };
      capture = { pointers, lost };
      this.captures.set(element, capture);
      element.addEventListener('lostpointercapture', lost);
    }
    capture.pointers.add(event.pointerId);
  }

  private set(element: Element, id: number): boolean {
    try { element.setPointerCapture(id); return true; }
    catch (error) {
      // Pointer lock or an ended native stream can make a request unavailable.
      // The down event still reaches the guest; gotpointercapture confirms it.
      if (error instanceof DOMException && ['NotFoundError', 'InvalidStateError'].includes(error.name)) return false;
      throw error;
    }
  }

  private forget(element: Element): void {
    const capture = this.captures.get(element);
    if (!capture) return;
    element.removeEventListener('lostpointercapture', capture.lost);
    this.captures.delete(element);
  }

  release(node: Node): void {
    const element = node as Element;
    const capture = this.captures.get(element);
    if (!capture) return;
    this.forget(element);
    for (const id of capture.pointers) {
      if (element.hasPointerCapture(id)) element.releasePointerCapture(id);
    }
  }

  /** Retire captures replaced by another native owner before DOM movement. */
  begin(): void {
    for (const [element, capture] of this.captures) {
      for (const id of capture.pointers) {
        if (!element.hasPointerCapture(id)) capture.pointers.delete(id);
      }
      if (!capture.pointers.size) this.forget(element);
    }
  }

  /** Restore only captures this host still owned before the DOM commit. */
  settle(): void {
    for (const [element, capture] of this.captures) {
      if (!element.isConnected) { this.release(element); continue; }
      for (const id of capture.pointers) {
        if (!element.hasPointerCapture(id) && !this.set(element, id)) capture.pointers.delete(id);
      }
      if (!capture.pointers.size) this.forget(element);
    }
  }

  close(): void {
    for (const element of this.captures.keys()) this.release(element);
  }
}
