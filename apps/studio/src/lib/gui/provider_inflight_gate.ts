export type ProviderGateHandle = Readonly<{ sessionId: number; generation: number }>;

export type ProviderCallTicket = {
  readonly handle: ProviderGateHandle;
  release(): void;
};

type GateState = {
  open: boolean;
  inflight: number;
  resolveDrained: (() => void) | null;
  drained: Promise<void>;
};

/** Generation-aware admission and drain for preview-owned provider calls. */
export class ProviderInflightGate {
  readonly #generations = new Map<number, number>();
  readonly #states = new Map<string, GateState>();

  open(sessionId: number): ProviderGateHandle {
    const id = assertSessionId(sessionId);
    const generation = nextGeneration(this.#generations.get(id) ?? 0);
    this.#generations.set(id, generation);
    let resolveDrained: (() => void) | null = null;
    const drained = new Promise<void>((resolve) => {
      resolveDrained = resolve;
    });
    this.#states.set(key(id, generation), {
      open: true,
      inflight: 0,
      resolveDrained,
      drained,
    });
    return Object.freeze({ sessionId: id, generation });
  }

  enter(handle: ProviderGateHandle): ProviderCallTicket | null {
    const state = this.#states.get(handleKey(handle));
    if (!state?.open) return null;
    if (!Number.isSafeInteger(state.inflight + 1)) {
      throw new Error('provider in-flight call count is exhausted');
    }
    state.inflight += 1;
    let released = false;
    return {
      handle,
      release: () => {
        if (released) return;
        released = true;
        this.#release(handle);
      },
    };
  }

  beginDrain(handle: ProviderGateHandle): Promise<void> {
    const state = this.#states.get(handleKey(handle));
    if (!state) return Promise.resolve();
    state.open = false;
    this.#finishIfDrained(handle, state);
    return state.drained;
  }

  #release(handle: ProviderGateHandle): void {
    const state = this.#states.get(handleKey(handle));
    if (!state || state.inflight < 1) return;
    state.inflight -= 1;
    this.#finishIfDrained(handle, state);
  }

  #finishIfDrained(handle: ProviderGateHandle, state: GateState): void {
    if (state.open || state.inflight !== 0 || !state.resolveDrained) return;
    const resolve = state.resolveDrained;
    state.resolveDrained = null;
    this.#states.delete(handleKey(handle));
    resolve();
  }
}

function handleKey(handle: ProviderGateHandle): string {
  return key(assertSessionId(handle.sessionId), handle.generation);
}

function key(sessionId: number, generation: number): string {
  if (!Number.isSafeInteger(generation) || generation < 1) {
    throw new Error('provider gate generation must be a positive safe integer');
  }
  return `${sessionId}:${generation}`;
}

function assertSessionId(sessionId: number): number {
  if (!Number.isSafeInteger(sessionId) || sessionId < 1) {
    throw new Error('provider gate session ID must be a positive safe integer');
  }
  return sessionId;
}

function nextGeneration(current: number): number {
  const next = current + 1;
  if (!Number.isSafeInteger(next)) throw new Error('provider gate generation space is exhausted');
  return next;
}
