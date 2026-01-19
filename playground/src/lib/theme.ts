import { writable } from 'svelte/store';

export type Theme = 'light' | 'dark';

// Get initial theme from localStorage or system preference
const getInitialTheme = (): Theme => {
  const saved = localStorage.getItem('theme') as Theme;
  if (saved) return saved;
  // Default to light as requested, but could check system pref:
  // return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
  return 'light';
};

export const theme = writable<Theme>(getInitialTheme());

// Subscribe to store updates to persist theme
theme.subscribe((value) => {
  localStorage.setItem('theme', value);
  document.documentElement.setAttribute('data-theme', value);
});

export function toggleTheme() {
  theme.update((t) => (t === 'light' ? 'dark' : 'light'));
}
