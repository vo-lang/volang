// Studio examples — loaded at build time via Vite import.meta.glob.
// To add/remove examples, just add/remove .vo files in volang/examples/.

export interface StudioSyncExample {
  path: string;
  content: string;
}

const rawModules: Record<string, string> = import.meta.glob([
  '@examples/**/*.vo',
  '@examples/**/vo.mod',
], {
  query: '?raw',
  import: 'default',
  eager: true,
});

export const STUDIO_SYNC_EXAMPLES: StudioSyncExample[] = Object.entries(rawModules)
  .map(([filePath, content]) => ({
    path: filePath.replace(/^.*\/examples\//, ''),
    content,
  }))
  .sort((a, b) => a.path.localeCompare(b.path));
