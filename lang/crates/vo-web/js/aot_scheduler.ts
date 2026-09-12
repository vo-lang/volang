/** A task boundary without the minimum delay/clamping of zero-duration timers.
 * Close both ports on delivery so completed instances retain no event-loop work.
 */
import { yieldHost } from './host_scheduler.js';
export const yieldAotHost: () => Promise<void> = yieldHost;

/** Drive one instance's resumable guest without starving browser events. */
export async function driveAotScheduler(
  step: () => number,
  pending: () => Iterable<Promise<void>>,
): Promise<number> {
  const WOULD_BLOCK = 5;
  const YIELD = 17;
  // Several guest quanta fit in one host turn. This amortizes event-loop scheduling
  // while putting a finite bound on continuously runnable guest work.
  const QUANTA_PER_HOST_TURN = 16;
  let quanta = 0;
  for (;;) {
    const status = step();
    if (status === YIELD) {
      if (++quanta >= QUANTA_PER_HOST_TURN) {
        quanta = 0;
        await yieldAotHost();
      }
      continue;
    }
    if (status === WOULD_BLOCK) {
      const waits = [...pending()];
      if (waits.length !== 0) {
        quanta = 0;
        await Promise.race(waits);
        continue;
      }
    }
    return status;
  }
}
