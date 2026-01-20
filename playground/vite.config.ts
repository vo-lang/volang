import { defineConfig } from 'vite';
import { svelte } from '@sveltejs/vite-plugin-svelte';
import path from 'path';

export default defineConfig({
  plugins: [svelte()],
  resolve: {
    alias: {
      '@vo-web': path.resolve(__dirname, '../lang/crates/vo-web/pkg'),
    },
  },
  server: {
    fs: {
      allow: [
        // Allow serving files from project root
        path.resolve(__dirname, '..'),
      ],
    },
  },
  optimizeDeps: {
    exclude: ['monaco-editor'],
  },
  build: {
    target: 'esnext',
  },
});
