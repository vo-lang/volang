import type { TaskProviders } from './tasks.js';

// A paused animation may outlive many cancelled requests. Attach once to its
// current finished promise; request cancellation removes its own subscriber.
// The remaining promise callback holds no request or root when the set is empty.
const completions = new WeakMap<Promise<Animation>, Set<() => void>>();

function observe(animation: Animation, complete: () => void): () => void {
  const finished = animation.finished;
  let subscribers = completions.get(finished);
  if (!subscribers) {
    subscribers = new Set();
    completions.set(finished, subscribers);
    const group = subscribers;
    const settle = (): void => {
      const pending = [...group];
      group.clear();
      for (const notify of pending) notify();
    };
    // Rejection means cancellation, which also completes an exit. Unlike native
    // cancel events this works for WebKit animations cancelled before playback.
    void finished.then(settle, settle);
  }
  subscribers.add(complete);
  return () => subscribers.delete(complete);
}

/** The caller owns cancellation/deadlines. Undefined means there is no finite
 * motion to wait for, allowing native presentation cleanup to stay synchronous. */
export function waitForFiniteMotion(element: Element, signal: AbortSignal): Promise<void> | undefined {
  const animations = element.getAnimations().filter(animation => animation.playState !== 'finished'
    && animation.playState !== 'idle' && animation.effect?.getComputedTiming().endTime !== Infinity);
  if (animations.length > 128) throw new Error('element motion exceeds the supported animation count');
  if (!animations.length) return;
  return new Promise((resolve, reject) => {
    let pending = animations.length;
    const subscriptions: (() => void)[] = [];
    const cleanup = (): void => {
      for (const unsubscribe of subscriptions) unsubscribe();
      signal.removeEventListener('abort', abort);
    };
    const settled = (): void => {
      if (--pending === 0) { cleanup(); resolve(); }
    };
    const abort = (): void => { cleanup(); reject(new Error('cancelled')); };
    for (const animation of animations) subscriptions.push(observe(animation, settled));
    signal.addEventListener('abort', abort, { once: true });
    if (signal.aborted) abort();
  });
}

/** Wait on one element inside the declaring root. Children retain their own
 * lifetimes; an infinite spinner cannot hold an exit open. */
export function motionTasks(container: HTMLElement): TaskProviders {
  return {
    async 'web.motion-finished'(id, signal) {
      if (!id || id.length > 1024) throw new Error('motion requires an element ID');
      const element = container.querySelector(`#${CSS.escape(id)}`);
      if (!element) return '';
      await waitForFiniteMotion(element, signal);
      return '';
    },
  };
}
