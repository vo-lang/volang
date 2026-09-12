/** Conservative store hints for generated code. Precise slot interpretation,
 * allocation identity, generations and ownership remain with the host tracer.
 * The first 32 physical slots use a mask; a byte cutoff covers any later roots.
 */
import type { AotAllocationLayout, AotMemoryMetadata } from './aot_trace.js';

export interface AotBarrierHint { readonly mask: number; readonly end: number; }
function referenced(kind: number): boolean { return kind === 1 || kind === 2 || kind === 4; }
function fixed(slots: readonly number[]): AotBarrierHint {
  let mask = 0, end = 0;
  slots.forEach((kind, index) => {
    if (!referenced(kind)) return;
    if (index < 32) mask |= 1 << index;
    end = (index + 1) * 8;
  });
  return { mask: mask >>> 0, end };
}
function repeated(prefix: readonly number[], roots: boolean): AotBarrierHint {
  if (!roots) return fixed(prefix);
  const first = fixed(prefix).mask;
  return { mask: (first | (0xffff_ffff << prefix.length)) >>> 0, end: 0xffff_ffff };
}
export function allocationBarrierHint(layout: AotAllocationLayout): AotBarrierHint {
  switch (layout.tag) {
    case 2: return fixed(layout.first.slots);
    // Sequence/queue views can point into separate backing. Conservatively
    // include the inline payload when the element layout contains references.
    case 3: return repeated([2, 0, 0, 0], layout.first.roots.length !== 0);
    case 4: return fixed([0, 0, 0, 0, 2]);
    case 5: return repeated([2, 0], layout.entries.roots.length !== 0);
    case 6: {
      const prefix = Array<number>(15).fill(0); prefix[9] = 2;
      return repeated(prefix, layout.first.roots.length !== 0);
    }
    default: return { mask: 0, end: 0 };
  }
}
export function frameBarrierHint(metadata: AotMemoryMetadata, slots: readonly number[], fiber = false): AotBarrierHint {
  const prefix = Array<number>(metadata.frameBytes / 8).fill(0);
  if (!fiber) for (const offset of metadata.frameDefers) prefix[Math.floor(offset / 8)] = 2;
  return fixed([...prefix, ...slots]);
}
export function fiberBarrierHint(metadata: AotMemoryMetadata): AotBarrierHint {
  const slots = Array<number>(metadata.fiberBytes / 8).fill(0);
  slots[metadata.fiberPanic / 8 + 1] = 4;
  slots[metadata.fiberPreviousPanic / 8] = 2;
  return frameBarrierHint(metadata, slots, true);
}
export function publishBarrierHint(view: DataView, header: number, hint: AotBarrierHint): void {
  view.setUint32(header + 8, hint.mask, true);
  view.setUint32(header + 24, hint.end, true);
}
