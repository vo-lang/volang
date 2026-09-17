import { mount, unmount } from 'svelte';
import App from './App.svelte';
const app = mount(App, { target: document.getElementById('root') });
window.__benchmark.close = () => unmount(app);
