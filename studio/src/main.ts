import './app.css';
import '@xterm/xterm/css/xterm.css';
import App from './App.svelte';

const target = document.getElementById('app');

if (!target) {
  throw new Error('Missing #app mount point');
}

const StudioApp = App as unknown as new (options: { target: HTMLElement }) => unknown;

new StudioApp({
  target,
});
