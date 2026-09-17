import { createApp } from 'vue';
import App from './App.vue';
const app = createApp(App);
app.mount('#root');
window.__benchmark.close = () => app.unmount();
