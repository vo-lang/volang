/** Host-owned scheduling metadata. Guest frames/payloads remain in SpanHeap.
 * A fiber has one ready-queue position and reusable intrusive waiter nodes;
 * waking a select removes every competing registration before publication.
 */
type Direction = 0 | 1;
class WaitList { head?: Waiter; tail?: Waiter; }
class QueueWaits {
  readonly lanes = [new WaitList(), new WaitList()] as const;
  broadcast = false;
  nextBroadcast?: QueueWaits;
}
class Fiber {
  previous?: Fiber;
  next?: Fiber;
  readyPrevious?: Fiber;
  readyNext?: Fiber;
  ready = false;
  parked = false;
  hostWait = false;
  readonly waiters: Waiter[] = [];
  count = 0;
  constructor(readonly pointer: number) {}
}
class Waiter {
  previous?: Waiter;
  next?: Waiter;
  list?: WaitList;
  constructor(readonly fiber: Fiber) {}
}

export class AotRunQueue {
  private readonly fibers = new Map<number, Fiber>();
  private readonly queues = new Map<number, QueueWaits>();
  private tail?: Fiber;
  private readyHead?: Fiber;
  private readyTail?: Fiber;
  private broadcastHead?: QueueWaits;
  private broadcastTail?: QueueWaits;
  private waiterCapacity = 0;
  private activeWaiters = 0;
  private hostWaits = 0;
  private dispatches = 0;
  private waits = 0;
  private wakeups = 0;
  constructor(private readonly maxWaiters = 1_000_000) {
    if (!Number.isSafeInteger(maxWaiters) || maxWaiters < 0) throw new RangeError('invalid scheduler waiter limit');
  }
  stats() {
    return { fibers: this.fibers.size, waiterCapacity: this.waiterCapacity,
      activeWaiters: this.activeWaiters, hostWaits: this.hostWaits,
      dispatches: this.dispatches, waits: this.waits, wakeups: this.wakeups };
  }
  add(pointer: number): void {
    if (this.fibers.has(pointer)) throw new Error('duplicate scheduler fiber');
    const fiber = new Fiber(pointer);
    fiber.previous = this.tail;
    if (this.tail) this.tail.next = fiber;
    this.tail = fiber;
    this.fibers.set(pointer, fiber);
    this.enqueue(fiber);
  }
  previous(pointer: number): number { return this.fibers.get(pointer)?.previous?.pointer ?? 0; }
  remove(pointer: number): void {
    const fiber = this.fibers.get(pointer);
    if (!fiber) return;
    this.clear(fiber); this.unqueue(fiber);
    if (fiber.hostWait) this.hostWaits -= 1;
    if (fiber.previous) fiber.previous.next = fiber.next;
    if (fiber.next) fiber.next.previous = fiber.previous;
    else this.tail = fiber.previous;
    this.waiterCapacity -= fiber.waiters.length;
    this.fibers.delete(pointer);
  }
  park(pointer: number): void {
    const fiber = this.fibers.get(pointer);
    if (fiber) { fiber.parked = true; this.unqueue(fiber); }
  }
  parkHost(pointer: number): () => void {
    const fiber = this.fibers.get(pointer);
    if (fiber && !fiber.hostWait) { fiber.hostWait = true; this.hostWaits += 1; }
    this.park(pointer);
    return () => { if (fiber && this.fibers.get(pointer) === fiber) this.wake(pointer); };
  }
  /** Called after an entire fiber quantum, including materialized call returns. */
  finish(pointer: number): void {
    const fiber = this.fibers.get(pointer);
    if (fiber && !fiber.parked) this.enqueue(fiber);
  }
  wake(pointer: number): void {
    const fiber = this.fibers.get(pointer);
    if (fiber) {
      if (fiber.parked) this.wakeups += 1;
      this.clear(fiber);
      if (fiber.hostWait) { fiber.hostWait = false; this.hostWaits -= 1; }
      fiber.parked = false; this.enqueue(fiber);
    }
  }
  /** Failed-Island cleanup can cancel a large select one registration per GC unit. */
  wakeForCleanup(pointer: number): boolean {
    const fiber = this.fibers.get(pointer);
    if (fiber?.count) { this.clearOne(fiber); return false; }
    this.wake(pointer);
    return true;
  }
  wait(pointer: number, queue: number, direction: number): boolean {
    const fiber = this.fibers.get(pointer);
    if (!fiber || (direction !== 0 && direction !== 1)) throw new Error('invalid scheduler wait');
    this.park(pointer); this.waits += 1;
    if (!queue) return true;
    let waiter = fiber.waiters[fiber.count];
    if (!waiter) {
      if (this.waiterCapacity === this.maxWaiters) return false;
      waiter = new Waiter(fiber); fiber.waiters.push(waiter); this.waiterCapacity += 1;
    }
    let queues = this.queues.get(queue);
    if (!queues) { queues = new QueueWaits(); this.queues.set(queue, queues); }
    const list = queues.lanes[direction as Direction];
    waiter.previous = list.tail; waiter.next = undefined; waiter.list = list;
    if (list.tail) list.tail.next = waiter; else list.head = waiter;
    list.tail = waiter; fiber.count += 1; this.activeWaiters += 1;
    return true;
  }
  /** A state change wakes one contender in each selected direction. */
  notify(queue: number, mask: number, exclude = 0): void {
    const queues = this.queues.get(queue);
    if (!queues) return;
    for (const direction of [0, 1] as const) {
      if (!(mask & (1 << direction))) continue;
      let waiter = queues.lanes[direction].head;
      // A single select can list the same queue more than once. Skip its
      // registrations without allowing a self-rendezvous.
      while (waiter?.fiber.pointer === exclude) waiter = waiter.next;
      if (waiter) this.wake(waiter.fiber.pointer);
    }
  }
  close(queue: number): void {
    const queues = this.queues.get(queue);
    if (!queues || queues.broadcast) return;
    queues.broadcast = true;
    if (this.broadcastTail) this.broadcastTail.nextBroadcast = queues;
    else this.broadcastHead = queues;
    this.broadcastTail = queues;
  }
  forgetQueue(queue: number): void {
    const queues = this.queues.get(queue);
    if (queues?.lanes.some(list => list.head)) throw new Error('queue reclaimed with active waiters');
    this.queues.delete(queue);
  }
  /** Broadcasts advance at most 64 wakeups per scheduler crossing. */
  next(): number {
    for (let work = 0; work < 64 && this.broadcastHead; work += 1) {
      const queues = this.broadcastHead;
      const waiter = queues.lanes[0].head ?? queues.lanes[1].head;
      if (waiter) this.wake(waiter.fiber.pointer);
      else {
        this.broadcastHead = queues.nextBroadcast;
        if (!this.broadcastHead) this.broadcastTail = undefined;
        queues.nextBroadcast = undefined; queues.broadcast = false;
      }
    }
    const fiber = this.readyHead;
    if (!fiber) return this.broadcastHead ? -1 : this.hostWaits ? -2 : 0;
    this.unqueue(fiber); this.dispatches += 1;
    return fiber.pointer;
  }
  private clearOne(fiber: Fiber): void {
    const waiter = fiber.waiters[--fiber.count], list = waiter.list!;
    if (waiter.previous) waiter.previous.next = waiter.next; else list.head = waiter.next;
    if (waiter.next) waiter.next.previous = waiter.previous; else list.tail = waiter.previous;
    waiter.list = undefined; waiter.next = undefined; waiter.previous = undefined;
    this.activeWaiters -= 1;
  }
  private clear(fiber: Fiber): void {
    while (fiber.count) this.clearOne(fiber);
  }
  private enqueue(fiber: Fiber): void {
    if (fiber.ready) return;
    fiber.ready = true; fiber.readyPrevious = this.readyTail;
    if (this.readyTail) this.readyTail.readyNext = fiber; else this.readyHead = fiber;
    this.readyTail = fiber;
  }
  private unqueue(fiber: Fiber): void {
    if (!fiber.ready) return;
    if (fiber.readyPrevious) fiber.readyPrevious.readyNext = fiber.readyNext; else this.readyHead = fiber.readyNext;
    if (fiber.readyNext) fiber.readyNext.readyPrevious = fiber.readyPrevious; else this.readyTail = fiber.readyPrevious;
    fiber.ready = false; fiber.readyPrevious = undefined; fiber.readyNext = undefined;
  }
}
