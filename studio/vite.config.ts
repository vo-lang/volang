import { defineConfig } from 'vite';
import { svelte, vitePreprocess } from '@sveltejs/vite-plugin-svelte';

// GitHub release asset downloads redirect to Azure blob storage which lacks
// CORS headers.  This plugin proxies /gh-release/* requests server-side,
// following redirects and returning the final response with permissive CORS.

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
    const upstream = await globalThis.fetch(target, { redirect: 'follow' }) as UpstreamResponse;
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

export default defineConfig({
  plugins: [ghReleaseProxy(), svelte({ preprocess: vitePreprocess() })],
  server: {
    port: 5174,
    strictPort: true,
  },
  build: {
    outDir: 'dist',
    target: ['es2020', 'chrome105'],
  },
  // Allow .wasm files from public/ to be served as assets
  assetsInclude: ['**/*.wasm'],
  optimizeDeps: {
    // studio WASM module is loaded at runtime from public/wasm/, not bundled
    exclude: ['vo_studio_wasm'],
  },
});
