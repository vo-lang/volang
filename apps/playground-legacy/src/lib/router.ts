export type Route = 'home' | 'playground' | 'docs';

import { writable, derived } from 'svelte/store';

// Current full hash (without #)
export const currentHash = writable<string>(window.location.hash.slice(1) || 'home');

// Derived top-level route
export const currentRoute = derived(currentHash, ($hash) => {
  const root = $hash.split('/')[0];
  if (root === 'home' || root === 'playground' || root === 'docs') {
    return root as Route;
  }
  return 'home';
});

export function navigate(path: string) {
  window.location.hash = path;
  // hashchange event will update the store
}

// Handle initial hash if empty
if (!window.location.hash) {
  window.location.hash = 'home';
}

window.addEventListener('hashchange', () => {
  currentHash.set(window.location.hash.slice(1));
});
