declare module 'vite' {
  export function defineConfig(config: unknown): unknown;
}

declare module '@sveltejs/vite-plugin-svelte' {
  export function svelte(config?: unknown): unknown;
  export function vitePreprocess(config?: unknown): unknown;
}

declare module '@tauri-apps/api/core' {
  export const invoke: <T>(command: string, args?: Record<string, unknown>) => Promise<T>;

  export class Channel<T = unknown> {
    id: number;
    onmessage: (response: T) => void;
    toJSON(): string;
  }
}
