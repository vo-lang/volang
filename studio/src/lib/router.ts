import { writable } from 'svelte/store';
import type { AppMode } from '../stores/ide';

export interface Route {
  mode: AppMode;
  docsPath: string | null;
}

function parseHash(hash: string): Route {
  const raw = hash.replace(/^#\/?/, '');
  if (raw.startsWith('docs/')) {
    return { mode: 'docs', docsPath: raw.slice('docs/'.length) || null };
  }
  if (raw === 'docs') {
    return { mode: 'docs', docsPath: null };
  }
  if (raw === 'develop' || raw === 'dev') {
    return { mode: 'develop', docsPath: null };
  }
  if (raw === 'runner') {
    return { mode: 'runner', docsPath: null };
  }
  // Default: no hash or unrecognized → null mode (don't override)
  return { mode: 'manage', docsPath: null };
}

export const route = writable<Route>(parseHash(window.location.hash));

// Listen for hash changes
window.addEventListener('hashchange', () => {
  route.set(parseHash(window.location.hash));
});

function replaceHash(hash: string, options: { clearSearch?: boolean } = {}): void {
  const url = new URL(window.location.href);
  if (options.clearSearch) {
    url.search = '';
  }
  url.hash = hash;
  if (window.location.href !== url.toString()) {
    window.history.replaceState(null, '', url.toString());
    route.set(parseHash(url.hash));
  }
}

// Set the hash without triggering a redundant hashchange
export function setDocsHash(file: string): void {
  // Convert 'getting-started/introduction.md' → 'docs/getting-started/introduction'
  const slug = file.replace(/\.md$/, '');
  replaceHash(`#/docs/${slug}`, { clearSearch: true });
}

export function setModeHash(mode: AppMode): void {
  if (mode === 'docs') {
    // Don't change docs path, just ensure prefix
    if (!window.location.hash.startsWith('#/docs')) {
      replaceHash('#/docs', { clearSearch: true });
    } else {
      replaceHash(window.location.hash, { clearSearch: true });
    }
  } else if (mode === 'manage') {
    replaceHash('#/', { clearSearch: true });
  } else {
    window.location.hash = `#/${mode}`;
  }
}

// Resolve docs path from hash: 'getting-started/introduction' → 'getting-started/introduction.md'
export function resolveDocsFile(path: string): string {
  if (path.endsWith('.md')) return path;
  return `${path}.md`;
}
