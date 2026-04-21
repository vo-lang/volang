import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { defineConfig } from 'vite';
import { svelte, vitePreprocess } from '@sveltejs/vite-plugin-svelte';

interface ProxyRequest {
  url?: string;
}

interface ProxyResponse {
  writeHead(status: number, headers?: Record<string, string>): void;
  end(body?: string | Uint8Array): void;
}

interface ProxyMiddlewares {
  use(route: string, handler: (req: ProxyRequest, res: ProxyResponse) => void): void;
}

interface ProxyServer {
  middlewares: ProxyMiddlewares;
}

interface UpstreamHeaders {
  get(name: string): string | null;
}

interface UpstreamResponse {
  status: number;
  headers: UpstreamHeaders;
  arrayBuffer(): Promise<ArrayBuffer>;
}

function writeProxyResponseHeaders(res: ProxyResponse, upstream: UpstreamResponse) {
  const headers: Record<string, string> = {
    'content-type': upstream.headers.get('content-type') ?? 'application/octet-stream',
    'access-control-allow-origin': '*',
  };
  const contentLength = upstream.headers.get('content-length');
  if (contentLength !== null) {
    headers['content-length'] = contentLength;
  }
  res.writeHead(upstream.status, headers);
}

function proxyErrorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

async function proxyGhReleaseRequest(req: ProxyRequest, res: ProxyResponse): Promise<void> {
  const target = `https://github.com${req.url ?? ''}`;
  try {
    const upstream = await (globalThis as typeof globalThis & {
      fetch(url: string, init: { redirect: 'follow' }): Promise<UpstreamResponse>;
    }).fetch(target, { redirect: 'follow' });
    writeProxyResponseHeaders(res, upstream);
    const buf = new Uint8Array(await upstream.arrayBuffer());
    res.end(buf);
  } catch (err) {
    res.writeHead(502, { 'content-type': 'text/plain; charset=utf-8' });
    res.end(`gh-release proxy error: ${proxyErrorMessage(err)}`);
  }
}

function installGhReleaseProxy(middlewares: ProxyMiddlewares) {
  middlewares.use('/gh-release/', (req: ProxyRequest, res: ProxyResponse) => {
    void proxyGhReleaseRequest(req, res);
  });
}

function ghReleaseProxy() {
  return {
    name: 'gh-release-proxy',
    configureServer(server: ProxyServer) {
      installGhReleaseProxy(server.middlewares);
    },
    configurePreviewServer(server: ProxyServer) {
      installGhReleaseProxy(server.middlewares);
    },
  };
}

function studioManualChunks(id: string): string | undefined {
  const normalized = id.replace(/\\/g, '/');
  if (!normalized.includes('/node_modules/')) {
    return undefined;
  }
  if (normalized.includes('/monaco-editor/esm/')) {
    return 'vendor-monaco';
  }
  if (normalized.includes('/golden-layout/')) {
    return 'vendor-layout';
  }
  if (normalized.includes('/@xterm/')) {
    return 'vendor-terminal';
  }
  if (normalized.includes('/marked/') || normalized.includes('/highlight.js/')) {
    return 'vendor-docs';
  }
  if (normalized.includes('/@tauri-apps/')) {
    return 'vendor-tauri';
  }
  if (normalized.includes('/svelte/')) {
    return 'vendor-svelte';
  }
  return 'vendor';
}

const buildEnv = (globalThis as typeof globalThis & {
  process?: { env?: Record<string, string | undefined> };
}).process?.env ?? {};

function readStudioWasmBuildId(): string | null {
  try {
    const value = readFileSync(resolve('public/wasm/vo_studio_wasm.build_id'), 'utf8').trim();
    return value.length > 0 ? value : null;
  } catch {
    return null;
  }
}

const studioBuildId = readStudioWasmBuildId()
  || [
    buildEnv.GITHUB_SHA,
    buildEnv.GITHUB_RUN_ID,
    buildEnv.GITHUB_RUN_ATTEMPT,
  ].filter((value): value is string => typeof value === 'string' && value.length > 0).join('-')
  || Date.now().toString(36);

export default defineConfig({
  plugins: [ghReleaseProxy(), svelte({ preprocess: vitePreprocess() })],
  server: {
    port: 5174,
    strictPort: true,
  },
  define: {
    __STUDIO_BUILD_ID__: JSON.stringify(studioBuildId),
  },
  build: {
    outDir: 'dist',
    target: ['es2020', 'chrome105'],
    chunkSizeWarningLimit: 2800,
    rollupOptions: {
      output: {
        manualChunks: studioManualChunks,
      },
    },
  },
  // Allow .wasm files from public/ to be served as assets
  assetsInclude: ['**/*.wasm'],
  optimizeDeps: {
    // studio WASM module is loaded at runtime from public/wasm/, not bundled
    exclude: ['vo_studio_wasm'],
  },
});
