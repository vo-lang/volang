import { defineConfig } from 'vite';
import { resolve } from 'path';

export default defineConfig({
  server: {
    port: 5180,
    strictPort: true,
    fs: {
      // Studio imports source directly from ../libs/vogui/js/src during dev.
      // Allow repo-level file serving instead of restricting to /studio only.
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
  optimizeDeps: {
    include: ['codemirror', '@codemirror/state', '@codemirror/view'],
  },
  resolve: {
    alias: {
      '@vogui': resolve(__dirname, '../libs/vogui/js/src'),
    },
  },
});
