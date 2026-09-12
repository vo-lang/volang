/** Compact immutable PC index. Decode location objects only when queried. */
import type { AotDebugLocation } from './aot_metadata.js';
import { AotSourceIndex } from './aot_source_index.js';

export class AotSourceLocations extends AotSourceIndex<AotDebugLocation> {
  /** The parser transfers validated records; no buffer or mutator is exposed. */
  constructor(private readonly words: Uint32Array, private readonly files: readonly string[]) {
    super();
  }
  get size(): number { return this.words.length / 5; }
  protected keyAt(index: number): number { return this.words[index * 5]; }
  protected valueAt(index: number): AotDebugLocation {
    const words = this.words;
    const offset = index * 5;
    return {
      pc: words[offset], file: this.files[words[offset + 1]],
      line: words[offset + 2], col: words[offset + 3], length: words[offset + 4],
    };
  }
}
