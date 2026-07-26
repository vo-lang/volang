/** Preview-owned provider instances with exact owner and late-teardown checks. */
export class ProviderInstanceSet<T extends object> {
  readonly #entries = new Map<number, T>();
  readonly #generations = new Map<number, number>();

  get size(): number {
    return this.#entries.size;
  }

  has(sessionId: number): boolean {
    return this.#entries.has(assertSessionId(sessionId));
  }

  get(sessionId: number): T | undefined {
    return this.#entries.get(assertSessionId(sessionId));
  }

  set(sessionId: number, instance: T): this {
    this.#entries.set(assertSessionId(sessionId), instance);
    return this;
  }

  begin(sessionId: number): ProviderInstanceLease {
    const id = assertSessionId(sessionId);
    const generation = nextGeneration(this.#generations.get(id) ?? 0);
    this.#generations.set(id, generation);
    return Object.freeze({ sessionId: id, generation });
  }

  install(lease: ProviderInstanceLease, instance: T): boolean {
    if (this.#generations.get(assertSessionId(lease.sessionId)) !== lease.generation) return false;
    this.#entries.set(lease.sessionId, instance);
    return true;
  }

  invalidate(sessionId: number): T | undefined {
    const id = assertSessionId(sessionId);
    this.#generations.set(id, nextGeneration(this.#generations.get(id) ?? 0));
    const current = this.#entries.get(id);
    this.#entries.delete(id);
    return current;
  }

  /** Removes only the expected instance when supplied, rejecting late teardown. */
  delete(sessionId: number, expected?: T): boolean {
    const id = assertSessionId(sessionId);
    const current = this.#entries.get(id);
    if (!current || (expected !== undefined && current !== expected)) return false;
    return this.#entries.delete(id);
  }

  clear(): void {
    this.#entries.clear();
    for (const [sessionId, generation] of this.#generations) {
      this.#generations.set(sessionId, nextGeneration(generation));
    }
  }

  keys(): IterableIterator<number> {
    return this.#entries.keys();
  }

  /** Includes sessions that only have an in-flight generation lease. */
  trackedSessionIds(): number[] {
    return [...new Set([...this.#generations.keys(), ...this.#entries.keys()])];
  }

  values(): IterableIterator<T> {
    return this.#entries.values();
  }
}

export type ProviderInstanceLease = Readonly<{ sessionId: number; generation: number }>;

function assertSessionId(sessionId: number): number {
  if (!Number.isSafeInteger(sessionId) || sessionId < 1) {
    throw new Error('provider instance session ID must be a positive safe integer');
  }
  return sessionId;
}

function nextGeneration(current: number): number {
  const next = current + 1;
  if (!Number.isSafeInteger(next)) throw new Error('provider instance generation space is exhausted');
  return next;
}
