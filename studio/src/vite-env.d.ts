interface ImportMetaEnv {
  readonly VITE_STUDIO_LAUNCH_URL?: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}

declare module '@vo-web/index' {
  export const vfs: any;
  export function initVFS(): Promise<void>;
}
