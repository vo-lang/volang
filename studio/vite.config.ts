import { defineConfig } from 'vite';
import { resolve } from 'path';
import { svelte, vitePreprocess } from '@sveltejs/vite-plugin-svelte';
import monacoEditorPlugin from 'vite-plugin-monaco-editor';

const base = process.env.VITE_BASE ?? '/';

export default defineConfig({
  base,
  plugins: [
    svelte({ preprocess: vitePreprocess() }),
    (monacoEditorPlugin as any).default({
      languageWorkers: ['editorWorkerService'],
    }),
  ],
  server: {
    port: 5173,
    strictPort: true,
    fs: {
      // Allow importing from libs/ (vogui/js/src) outside the studio root.
      allow: [resolve(__dirname, '..')],
    },
  },
  build: {
    outDir: 'dist',
    target: ['es2020', 'chrome105'],
    rollupOptions: {
      input: {
        main: resolve(__dirname, 'index.html'),
      },
    },
  },
  resolve: {
    alias: {
      '@vogui': resolve(__dirname, '../libs/vogui/js/src'),
      '@vo-web': resolve(__dirname, '../lang/crates/vo-web/js'),
    },
  },
});
