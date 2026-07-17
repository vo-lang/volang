import { defineConfig } from 'vite';
import { svelte } from '@sveltejs/vite-plugin-svelte';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(__dirname, '../..');
const voguiRoot = path.join(repoRoot, 'ci_modules/vogui');

export default defineConfig({
  plugins: [svelte()],
  resolve: {
    alias: {
      '@vo-playground': path.resolve(__dirname, 'rust/pkg'),
      '@vo-web': path.join(repoRoot, 'lang/crates/vo-web/js'),
      'react': path.join(voguiRoot, 'js/node_modules/preact/compat'),
      'react-dom': path.join(voguiRoot, 'js/node_modules/preact/compat'),
      'react/jsx-runtime': path.join(voguiRoot, 'js/node_modules/preact/jsx-runtime'),
    },
  },
  server: {
    fs: {
      allow: [
        repoRoot,
      ],
    },
  },
  css: {
    postcss: path.join(voguiRoot, 'js'),
  },
  optimizeDeps: {
    exclude: ['monaco-editor'],
  },
  build: {
    target: 'esnext',
  },
});
