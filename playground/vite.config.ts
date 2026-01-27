import { defineConfig } from 'vite';
import { svelte } from '@sveltejs/vite-plugin-svelte';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export default defineConfig({
  plugins: [svelte()],
  resolve: {
    alias: {
      '@vo-playground': path.resolve(__dirname, './rust/pkg'),
      '@vo-web': path.resolve(__dirname, '../lang/crates/vo-web/js'),
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
