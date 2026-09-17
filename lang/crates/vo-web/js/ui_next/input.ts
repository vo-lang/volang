import { MAX_FRAME_BYTES, MAX_QUEUED_BYTES, MAX_QUEUED_EVENTS } from './generated/protocol.js';
import type { Event as UiEvent } from './generated/protocol.js';
import { encodeEvent, EVENT_HEADER_BYTES, INPUT_BATCH_HEADER_BYTES, packInputBatch } from './generated/codec.js';

export const MAX_INPUT_EVENTS_PER_TURN = 128;

/** One bounded FIFO per root. Closing wakes a suspended guest with EOF. */
export class InputQueue {
  private events: Uint8Array[] = [];
  private queuedBytes = 0;
  private tailKey?: string;
  private waiter: { resolve(bytes: Uint8Array): void; reject(error: Error): void } | undefined;
  private closed = false;
  private failure: Error | undefined;
  private scheduled = false;
  private channel: MessageChannel | undefined;
  push(event: UiEvent, latest = false): void {
    if (this.closed || this.failure) return;
    try {
      if (latest && event.kind !== 'pointermove') throw new Error('latest delivery requires a pointermove event');
      const key = latest && event.pointer ? [event.target,event.capture,event.pointer.id,event.pointer.buttons,
        event.pointerType,event.button,event.altKey,event.ctrlKey,event.metaKey,event.shiftKey,event.isComposing].join(':') : undefined;
      const replace = key !== undefined && key === this.tailKey && this.events.length > 0;
      if (!replace && this.events.length >= MAX_QUEUED_EVENTS) throw new Error('UI input queue exhausted');
      const bytes = encodeEvent(event);
      if (bytes.length - EVENT_HEADER_BYTES + INPUT_BATCH_HEADER_BYTES > MAX_FRAME_BYTES) throw new Error('UI frame exceeds limit');
      const retired = replace ? this.events[this.events.length - 1].length : 0;
      if (bytes.length > MAX_QUEUED_BYTES - this.queuedBytes + retired) throw new Error('UI input queue exhausted');
      if (replace) this.events[this.events.length - 1] = bytes;
      else this.events.push(bytes);
      this.queuedBytes += bytes.length - retired;
      this.tailKey = key;
    } catch (error) { this.fail(error instanceof Error ? error : new Error(String(error))); return; }
    this.schedule();
  }
  private schedule(): void {
    // A native event can run microtasks between its listeners and before its
    // default action. A posted task waits for the whole dispatch to finish.
    if (!this.scheduled) {
      this.scheduled = true;
      if (!this.channel) {
        this.channel = new MessageChannel();
        this.channel.port1.onmessage = () => { this.scheduled = false; this.drain(); };
      }
      this.channel.port2.postMessage(null);
    }
  }
  next(): Promise<Uint8Array> {
    if (this.failure) return Promise.reject(this.failure);
    if (this.closed) return Promise.resolve(new Uint8Array());
    if (this.waiter) return Promise.reject(new Error('UI root has multiple input consumers'));
    return new Promise((resolve, reject) => {
      this.waiter = { resolve, reject };
      if (!this.scheduled) this.drain();
    });
  }
  private fail(error: Error): void {
    this.failure = error;
    this.events = [];
    this.tailKey = undefined;
    this.queuedBytes = 0;
    this.releaseChannel();
    const waiter = this.waiter;
    this.waiter = undefined;
    waiter?.reject(error);
  }
  private drain(): void {
    if (!this.waiter || this.events.length === 0) return;
    let size = INPUT_BATCH_HEADER_BYTES, count = 0, retiredBytes = 0;
    for (const event of this.events) {
      const added = event.length - EVENT_HEADER_BYTES;
      if (count >= MAX_INPUT_EVENTS_PER_TURN || added > MAX_FRAME_BYTES - size) break;
      size += added; retiredBytes += event.length; count++;
    }
    let bytes: Uint8Array;
    try { bytes = packInputBatch(this.events.slice(0, count)); }
    catch (error) { this.fail(error instanceof Error ? error : new Error(String(error))); return; }
    const waiter = this.waiter;
    this.waiter = undefined;
    this.events = this.events.slice(count);
    this.queuedBytes -= retiredBytes;
    if (this.events.length) this.schedule();
    else this.tailKey = undefined;
    waiter.resolve(bytes);
  }
  close(): void {
    this.closed = true;
    this.events = [];
    this.tailKey = undefined;
    this.queuedBytes = 0;
    this.releaseChannel();
    const waiter = this.waiter;
    this.waiter = undefined;
    waiter?.resolve(new Uint8Array());
  }
  private releaseChannel(): void {
    this.channel?.port1.close();
    this.channel?.port2.close();
    this.channel = undefined;
    this.scheduled = false;
  }
}
