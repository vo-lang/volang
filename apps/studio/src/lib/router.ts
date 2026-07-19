import { writable } from 'svelte/store';
import type { AppMode } from '../stores/ide';
import type { LaunchSpec, StudioMode } from './types';

export interface Route {
  mode: AppMode;
  docsPath: string | null;
  exampleId: string | null;
}

export function parseHash(hash: string): Route {
  const raw = hash.replace(/^#\/?/, '');
  const queryOffset = raw.indexOf('?');
  const path = queryOffset >= 0 ? raw.slice(0, queryOffset) : raw;
  const params = new URLSearchParams(queryOffset >= 0 ? raw.slice(queryOffset + 1) : '');
  const exampleId = params.get('example')?.trim() || null;
  if (path.startsWith('docs/')) {
    return { mode: 'docs', docsPath: path.slice('docs/'.length) || null, exampleId: null };
  }
  if (path === 'docs') {
    return { mode: 'docs', docsPath: null, exampleId: null };
  }
  if (path === 'develop' || path === 'dev') {
    return { mode: 'develop', docsPath: null, exampleId };
  }
  if (path === 'runner') {
    return { mode: 'runner', docsPath: null, exampleId: null };
  }
  return { mode: 'manage', docsPath: null, exampleId: null };
}

export function resolveStartupLaunch(options: {
  bootstrapLaunch: LaunchSpec | null;
  bootstrapMode: StudioMode;
  hash: string;
  search: string;
  browserUrlWasSynchronized: boolean;
}): LaunchSpec {
  const params = new URLSearchParams(options.search);
  if (params.has('mode') || params.has('proj')) {
    const rawMode = params.get('mode');
    const mode: StudioMode = rawMode === 'runner' ? 'runner' : 'dev';
    const rawProject = params.get('proj');
    return { proj: rawProject?.trim() || null, mode };
  }
  if (hasExplicitStudioHash(options.hash) || options.browserUrlWasSynchronized) {
    const route = parseHash(options.hash);
    return {
      proj: null,
      mode: route.mode === 'runner' ? 'runner' : 'dev',
    };
  }
  return options.bootstrapLaunch ?? { proj: null, mode: options.bootstrapMode };
}

function hasExplicitStudioHash(hash: string): boolean {
  if (hash === '#' || hash === '#/') return true;
  const route = parseHash(hash);
  return route.mode !== 'manage';
}

const browserWindow = typeof window === 'undefined' ? null : window;
const historyNavigationListeners = new Set<() => void>();
let observedBrowserHref = browserWindow?.location.href ?? '';

function parseBrowserRoute(): Route {
  if (!browserWindow) return parseHash('');
  const hashRoute = parseHash(browserWindow.location.hash);
  if (hashRoute.mode !== 'manage' || browserWindow.location.hash === '#/' || browserWindow.location.hash === '#') {
    return hashRoute;
  }
  const mode = new URLSearchParams(browserWindow.location.search).get('mode');
  if (mode === 'runner') return { mode: 'runner', docsPath: null, exampleId: null };
  if (mode === 'dev' || mode === 'develop') return { mode: 'develop', docsPath: null, exampleId: null };
  return hashRoute;
}

export const route = writable<Route>(parseBrowserRoute());

export function syncRouteFromLocation(): void {
  observedBrowserHref = browserWindow?.location.href ?? '';
  route.set(parseBrowserRoute());
}

function syncRouteFromBrowserHistory(): void {
  if (!browserWindow || browserWindow.location.href === observedBrowserHref) return;
  syncRouteFromLocation();
  for (const listener of historyNavigationListeners) listener();
}

browserWindow?.addEventListener('hashchange', syncRouteFromBrowserHistory);
browserWindow?.addEventListener('popstate', syncRouteFromBrowserHistory);

export function onBrowserHistoryNavigation(listener: () => void): () => void {
  historyNavigationListeners.add(listener);
  return () => historyNavigationListeners.delete(listener);
}

function navigateHash(
  hash: string,
  options: { clearSearch?: boolean; replace?: boolean } = {},
): void {
  if (!browserWindow) return;
  const url = new URL(browserWindow.location.href);
  if (options.clearSearch) {
    url.search = '';
  }
  url.hash = hash;
  if (browserWindow.location.href !== url.toString()) {
    const method = options.replace ? 'replaceState' : 'pushState';
    browserWindow.history[method](null, '', url.toString());
    syncRouteFromLocation();
  }
}

function hasLaunchParams(): boolean {
  if (!browserWindow) return false;
  const params = new URLSearchParams(browserWindow.location.search);
  return params.has('mode') || params.has('proj');
}

// Set the hash without triggering a redundant hashchange
export function setDocsHash(file: string): void {
  const slug = file.replace(/\.md$/, '');
  const currentRoute = browserWindow ? parseHash(browserWindow.location.hash) : parseHash('');
  navigateHash(`#/docs/${slug}`, {
    clearSearch: true,
    replace: currentRoute.mode === 'docs' && currentRoute.docsPath === null,
  });
}

export function setExampleHash(exampleId: string, options: { replace?: boolean } = {}): void {
  const params = new URLSearchParams();
  params.set('example', exampleId);
  const currentRoute = browserWindow ? parseHash(browserWindow.location.hash) : parseHash('');
  navigateHash(`#/develop?${params.toString()}`, {
    clearSearch: true,
    replace: options.replace
      ?? (currentRoute.mode === 'develop' && currentRoute.exampleId === null),
  });
}

export function setModeHash(mode: AppMode): void {
  if (!browserWindow) return;
  if (mode === 'docs') {
    if (!browserWindow.location.hash.startsWith('#/docs')) {
      navigateHash('#/docs', { clearSearch: true });
    } else {
      navigateHash(browserWindow.location.hash, { clearSearch: true, replace: true });
    }
  } else if (mode === 'manage') {
    navigateHash('#/', { clearSearch: true });
  } else if (hasLaunchParams()) {
    syncRouteFromLocation();
  } else if (mode === 'develop' && parseHash(browserWindow.location.hash).mode === 'develop') {
    syncRouteFromLocation();
  } else {
    navigateHash(`#/${mode}`);
  }
}

// Resolve docs path from hash: 'getting-started/introduction' → 'getting-started/introduction.md'
export function resolveDocsFile(path: string): string {
  if (path.endsWith('.md')) return path;
  return `${path}.md`;
}
