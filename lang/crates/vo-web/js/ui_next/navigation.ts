import type { TaskProviders, WatchProviders } from './tasks.js';
import { captureNativeNavigation, commitNavigation, locationChanged as changed, navigationState, watchNavigation } from './navigation-scroll.js';

/** Opt-in navigation services, scoped to one root and its owning window. */
export function createNavigationServices(container: HTMLElement): { tasks: TaskProviders; watches: WatchProviders } {
  return navigationServices(container, (url, current) => url.origin === current.origin && ['http:', 'https:'].includes(url.protocol));
}

/** System WebViews use a fixed asset authority; custom-scheme origins may be opaque. */
export function createDesktopNavigationServices(container: HTMLElement): { tasks: TaskProviders; watches: WatchProviders } {
  return navigationServices(container, (url, current) =>
    ((current.protocol === 'volang:' && current.hostname === 'localhost') ||
      (current.protocol === 'http:' && current.hostname === 'volang.localhost')) &&
    !current.port && !url.port && !url.username && !url.password &&
    url.protocol === current.protocol && url.hostname === current.hostname);
}

function navigationServices(container: HTMLElement, accepts: (url: URL, current: Location) => boolean): { tasks: TaskProviders; watches: WatchProviders } {
  const window = container.ownerDocument.defaultView;
  if (!window) throw new Error('UI navigation requires a window');
  const current = () => window.location.pathname + window.location.search + window.location.hash;
  const navigate = (href: string, replace: boolean, preserveScroll = false, preserveFocus = false): string => {
    const url = new URL(href, window.location.href);
    if (!accepts(url, window.location)) {
      throw new Error('UI navigation requires a URL within this application');
    }
    if (url.href === window.location.href && !replace) return current();
    const state = navigationState(window, replace);
    if (replace) window.history.replaceState(state, '', url.href);
    else window.history.pushState(state, '', url.href);
    window.dispatchEvent(new CustomEvent(changed, { detail: { replace, preserveScroll, preserveFocus } }));
    return current();
  };
  return {
    tasks: {
      async 'web.document-title'(value) { container.ownerDocument.title = value; return ''; },
      async 'web.navigation-commit'(value, signal) { return commitNavigation(container, value, signal); },
      async 'web.navigate'(value) {
        const request = JSON.parse(value);
        if (typeof request?.href !== 'string') throw new Error('invalid navigation request');
        validateOptions(request);
        return navigate(request.href, request.replace, request.preserveScroll, request.preserveFocus);
      },
      async 'web.query'(value) {
        const request = JSON.parse(value);
        validateOptions(request);
        if (!request.values || typeof request.values !== 'object' || Array.isArray(request.values)) throw new Error('invalid query update');
        const fields = Object.entries(request.values);
        if (fields.length === 0) return current();
        if (fields.length > 128) throw new Error('query update exceeds 128 fields');
        const url = new URL(window.location.href);
        let bytes = 0, count = 0;
        for (const [name, values] of fields) {
          if (values !== null && !Array.isArray(values)) throw new Error('invalid query values');
          bytes += name.length;
          if (bytes > 8192) throw new Error('query update exceeds its URL budget');
          url.searchParams.delete(name);
          for (const item of values ?? []) {
            if (typeof item !== 'string' || ++count > 128) throw new Error('invalid query values');
            bytes += item.length;
            if (bytes > 8192) throw new Error('query update exceeds its URL budget');
            url.searchParams.append(name, item);
          }
          if (bytes > 8192) throw new Error('query update exceeds its URL budget');
        }
        url.searchParams.sort();
        if (new TextEncoder().encode(url.pathname + url.search + url.hash).length > 8192) throw new Error('query update exceeds its URL budget');
        return navigate(url.href, request.replace, request.preserveScroll, request.preserveFocus);
      },
    },
    watches: {
      'web.navigation'(value, signal, emit) { watchNavigation(container, value, signal, emit); },
      'web.location'(_value, signal, emit) {
        let previous = '';
        const update = () => {
          const value = current();
          if (value !== previous) { previous = value; emit(value); }
        };
        for (const event of ['popstate', 'hashchange', changed]) window.addEventListener(event, update, { signal });
        container.addEventListener('click', event => {
          if (event.defaultPrevented || event.button !== 0 || event.altKey || event.ctrlKey || event.metaKey || event.shiftKey) return;
          const target = event.target as Element | null;
          const anchor = target?.closest?.('a[data-ui-route]') as HTMLAnchorElement | null;
          if (!anchor || anchor.namespaceURI !== 'http://www.w3.org/1999/xhtml' || !container.contains(anchor) || anchor.hasAttribute('download') || anchor.relList.contains('external')
            || (anchor.target && anchor.target.toLowerCase() !== '_self')) return;
          let url: URL;
          try { url = new URL(anchor.href, window.location.href); }
          catch { return; }
          if (!accepts(url, window.location)) return;
          // The browser owns same-page anchors, including scrolling, focus and
          // repeated clicks on the current fragment. History observers still
          // receive the resulting hashchange/popstate notifications.
          if (url.pathname === window.location.pathname && url.search === window.location.search && url.href.includes('#')) {
            captureNativeNavigation(window, url.href, event);
            return;
          }
          event.preventDefault();
          navigate(url.href, false);
        }, { signal });
        update();
      },
    },
  };
}

function validateOptions(value: { replace?: unknown; preserveScroll?: unknown; preserveFocus?: unknown } | null): void {
  if (!value || typeof value.replace !== 'boolean' ||
      (value.preserveScroll !== undefined && typeof value.preserveScroll !== 'boolean') ||
      (value.preserveFocus !== undefined && typeof value.preserveFocus !== 'boolean')) throw new Error('invalid navigation request');
}
