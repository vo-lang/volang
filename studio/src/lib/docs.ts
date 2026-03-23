import { Marked } from 'marked';
import hljs from 'highlight.js/lib/core';
import go from 'highlight.js/lib/languages/go';
import bash from 'highlight.js/lib/languages/bash';
import json from 'highlight.js/lib/languages/json';
import toml from 'highlight.js/lib/languages/ini';
import yaml from 'highlight.js/lib/languages/yaml';

// Register languages — Vo uses Go syntax, so alias 'vo' to Go highlighter
hljs.registerLanguage('go', go);
hljs.registerLanguage('vo', go);
hljs.registerLanguage('bash', bash);
hljs.registerLanguage('json', json);
hljs.registerLanguage('toml', toml);
hljs.registerLanguage('yaml', yaml);

// -- Manifest types --

export interface DocPage {
  title: string;
  file: string;
}

export interface DocSection {
  title: string;
  slug: string;
  pages: DocPage[];
}

export interface DocsManifest {
  sections: DocSection[];
}

// -- Build-time bundled content --

const rawFiles: Record<string, string> = import.meta.glob(
  '../../docs/**/*.md',
  { eager: true, query: '?raw', import: 'default' },
) as Record<string, string>;

const manifestGlob: Record<string, DocsManifest> = import.meta.glob(
  '../../docs/_manifest.json',
  { eager: true, import: 'default' },
) as Record<string, DocsManifest>;

// Normalize the glob results: keys are like '../../docs/getting-started/introduction.md'
// We want keys like 'getting-started/introduction.md'
const docsContent: Map<string, string> = new Map();
for (const [rawPath, content] of Object.entries(rawFiles)) {
  const normalized = rawPath.replace(/^.*?docs\//, '');
  docsContent.set(normalized, content);
}

// Extract manifest (glob returns object keyed by path)
let manifest: DocsManifest = { sections: [] };
for (const value of Object.values(manifestGlob)) {
  manifest = value;
  break;
}

// -- Markdown renderer --

const marked = new Marked({
  renderer: {
    code({ text, lang }: { text: string; lang?: string | undefined }): string {
      const language = lang || '';
      if (language && hljs.getLanguage(language)) {
        const highlighted = hljs.highlight(text, { language }).value;
        return `<pre><code class="hljs language-${language}">${highlighted}</code></pre>`;
      }
      const escaped = text
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;');
      return `<pre><code>${escaped}</code></pre>`;
    },
  },
});

// -- Public API --

export function getManifest(): DocsManifest {
  return manifest;
}

export function getDocContent(file: string): string | null {
  return docsContent.get(file) ?? null;
}

export function renderMarkdown(source: string): string {
  return marked.parse(source, { async: false }) as string;
}

export function getFirstPage(): string | null {
  const first = manifest.sections[0]?.pages[0];
  return first?.file ?? null;
}
