export interface LatestTaskContext {
  isLatest(): boolean;
  commit(): boolean;
}

export type LatestTaskResult<T> =
  | { status: 'completed'; value: T }
  | { status: 'cancelled' };

/**
 * Serializes stateful work while letting a newer request supersede queued work.
 * Running work receives an explicit freshness check for every asynchronous
 * boundary after which it may publish state.
 */
export class LatestTaskQueue {
  private generation = 0;
  private tail: Promise<void> = Promise.resolve();

  run<T>(task: (context: LatestTaskContext) => Promise<T>): Promise<LatestTaskResult<T>> {
    const generation = ++this.generation;
    const current = this.tail
      .catch(() => undefined)
      .then(async () => {
        if (generation !== this.generation) {
          return { status: 'cancelled' } as const;
        }
        let committed = false;
        try {
          const value = await task({
            isLatest: () => generation === this.generation,
            commit: () => {
              if (generation !== this.generation) return false;
              if (committed) return true;
              committed = true;
              return true;
            },
          });
          return generation === this.generation
            ? { status: 'completed', value } as const
            : { status: 'cancelled' } as const;
        } catch (error) {
          if (generation === this.generation) throw error;
          return { status: 'cancelled' } as const;
        }
      });
    this.tail = current.then(() => undefined, () => undefined);
    return current;
  }

  invalidate(): void {
    this.generation++;
  }
}
