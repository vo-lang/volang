import './app.css';
import 'golden-layout/dist/css/goldenlayout-base.css';
import 'monaco-editor/min/vs/editor/editor.main.css';
import App from './App.svelte';

applyLaunchEnvDefaults();

const target = document.getElementById('app');

if (!target) {
  throw new Error('Missing #app mount point');
}

const StudioApp = App as unknown as new (options: { target: HTMLElement }) => unknown;

new StudioApp({
  target,
});

function applyLaunchEnvDefaults(): void {
  if (window.location.search.length > 1) {
    return;
  }
  const proj = import.meta.env.VITE_STUDIO_PROJ?.trim();
  const mode = import.meta.env.VITE_STUDIO_MODE?.trim();
  if (!proj && !mode) {
    return;
  }
  const url = new URL(window.location.href);
  if (proj) {
    url.searchParams.set('proj', proj);
  }
  if (mode) {
    url.searchParams.set('mode', mode);
  }
  window.history.replaceState(null, '', url.toString());
}
