/** Read-only sorted source index over privately owned numeric records. */
export abstract class AotSourceIndex<Value> implements ReadonlyMap<number, Value> {
  abstract readonly size: number;
  protected abstract keyAt(index: number): number;
  protected abstract valueAt(index: number): Value;
  get [Symbol.toStringTag](): string { return 'Map'; }
  private find(pc: number): number {
    let low = 0;
    let high = this.size;
    while (low < high) {
      const mid = low + ((high - low) >>> 1);
      if (this.keyAt(mid) < pc) low = mid + 1;
      else high = mid;
    }
    return low < this.size && this.keyAt(low) === pc ? low : -1;
  }
  has(pc: number): boolean { return this.find(pc) >= 0; }
  get(pc: number): Value | undefined {
    const index = this.find(pc);
    return index < 0 ? undefined : this.valueAt(index);
  }
  *keys(): IterableIterator<number> {
    for (let index = 0; index < this.size; index++) yield this.keyAt(index);
  }
  *values(): IterableIterator<Value> {
    for (let index = 0; index < this.size; index++) yield this.valueAt(index);
  }
  *entries(): IterableIterator<[number, Value]> {
    for (let index = 0; index < this.size; index++) yield [this.keyAt(index), this.valueAt(index)];
  }
  [Symbol.iterator](): IterableIterator<[number, Value]> { return this.entries(); }
  forEach(callback: (value: Value, key: number, map: ReadonlyMap<number, Value>) => void, thisArg?: unknown): void {
    for (const [key, value] of this) callback.call(thisArg, value, key, this);
  }
}
