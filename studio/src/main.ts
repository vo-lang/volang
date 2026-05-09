import './app.css';
import 'golden-layout/dist/css/goldenlayout-base.css';
import 'monaco-editor/min/vs/editor/editor.main.css';
import App from './App.svelte';

if (!applyLaunchEnvDefaults()) {
  postLocalRunnerPerfHeartbeat('main-bootstrap');
  window.setTimeout(() => postLocalRunnerPerfHeartbeat('main-heartbeat'), 500);
  window.setTimeout(() => postLocalRunnerPerfHeartbeat('main-heartbeat'), 2500);

  const target = document.getElementById('app');

  if (!target) {
    throw new Error('Missing #app mount point');
  }

  const StudioApp = App as unknown as new (options: { target: HTMLElement }) => unknown;

  new StudioApp({
    target,
  });
}

function applyLaunchEnvDefaults(): boolean {
  if (window.location.search.length > 1) {
    return false;
  }
  const proj = import.meta.env.VITE_STUDIO_PROJ?.trim();
  const mode = import.meta.env.VITE_STUDIO_MODE?.trim();
  if (!proj && !mode) {
    return false;
  }
  const url = new URL(window.location.href);
  if (proj) {
    url.searchParams.set('proj', proj);
  }
  if (mode) {
    url.searchParams.set('mode', mode);
  }
  window.location.replace(url.toString());
  return true;
}

function postLocalRunnerPerfHeartbeat(kind: string): void {
  if (!isLocalHost(window.location.hostname)) {
    return;
  }
  const payload = {
    schemaVersion: 1,
    source: 'studio-main',
    kind,
    mode: new URLSearchParams(window.location.search).get('voplayPerf') || 'stats',
    metrics: {
      href: window.location.href,
      hash: window.location.hash,
      search: window.location.search,
    },
    nowMs: performance.now(),
    href: window.location.href,
    visibility: document.visibilityState,
    focus: document.hasFocus(),
  };
  try {
    void fetch('/__voplay_perf_report', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify(payload),
      keepalive: true,
    }).catch(() => {});
  } catch {
    // Local diagnostics must not affect app startup.
  }
}

function isLocalHost(hostname: string): boolean {
  return hostname === 'localhost'
    || hostname === '127.0.0.1'
    || hostname === '::1'
    || hostname === '[::1]';
}
