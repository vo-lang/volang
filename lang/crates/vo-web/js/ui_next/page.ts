import type { WatchProviders } from './tasks.js';

/** A cached document keeps its application. Final disposal releases the hook. */
export function closeOnPageExit(window: Window | null, close: () => void): () => void {
  if (!window) return () => {};
  const hide = (event: PageTransitionEvent): void => { if (!event.persisted) close(); };
  window.addEventListener('pagehide', hide);
  return () => window.removeEventListener('pagehide', hide);
}

/** Optional document lifecycle sources, bound to the root's owning document. */
export function pageWatches(document: Document): WatchProviders {
  return {
    'web.page-active'(value, signal, emit) {
      if (value !== '') throw new Error('page activity does not accept request data');
      const window = document.defaultView;
      if (!window) throw new Error('page activity requires a window');
      let previous = '';
      const update = (): void => {
        const active = String(document.visibilityState === 'visible' && document.hasFocus());
        if (active !== previous) { previous = active; emit(active); }
      };
      document.addEventListener('visibilitychange', update, { signal });
      window.addEventListener('focus', update, { signal });
      window.addEventListener('blur', update, { signal });
      update();
    },
  };
}
