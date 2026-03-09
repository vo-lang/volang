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
      output: {
        manualChunks(id) {
          const monacoPrefix = 'monaco-editor/esm/vs/';
          const monacoIndex = id.indexOf(monacoPrefix);
          if (monacoIndex >= 0) {
            const rel = id.slice(monacoIndex + monacoPrefix.length);
            const parts = rel.split('/');
            const scope = parts[0] || 'root';
            const bucket = parts[1] || 'root';
            const detail = parts[2] || 'root';
            return `monaco-${scope}-${bucket}-${detail}`;
          }
          if (id.includes('@tauri-apps/api')) return 'tauri';
          return undefined;
        },
      },
    },
  },
  resolve: {
    alias: {
      '@examples': resolve(__dirname, '../examples'),
      '@vogui': resolve(__dirname, '../../vogui/js/src'),
      '@vo-web': resolve(__dirname, '../lang/crates/vo-web/js'),
      // Preact aliases for vogui's Radix UI dependencies
      'react': resolve(__dirname, '../../vogui/js/node_modules/preact/compat'),
      'react-dom': resolve(__dirname, '../../vogui/js/node_modules/preact/compat'),
      'react/jsx-runtime': resolve(__dirname, '../../vogui/js/node_modules/preact/jsx-runtime'),
      'preact': resolve(__dirname, '../../vogui/js/node_modules/preact'),
    },
  },
});
