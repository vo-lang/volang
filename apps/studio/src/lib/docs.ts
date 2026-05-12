import { Marked } from 'marked';
import hljs from 'highlight.js/lib/core';
import go from 'highlight.js/lib/languages/go';
import bash from 'highlight.js/lib/languages/bash';
import json from 'highlight.js/lib/languages/json';
import toml from 'highlight.js/lib/languages/ini';
import yaml from 'highlight.js/lib/languages/yaml';
import manifestSource from '../../docs/manifest.toml?raw';

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
  '../../docs/pages/**/*.md',
  { eager: true, query: '?raw', import: 'default' },
) as Record<string, string>;

// Normalize the glob results: keys are like '../../docs/pages/getting-started/introduction.md'
// We want keys like 'getting-started/introduction.md'
const docsContent: Map<string, string> = new Map();
for (const [rawPath, content] of Object.entries(rawFiles)) {
  const normalized = rawPath.replace(/^.*?docs\/pages\//, '');
  docsContent.set(normalized, content);
}

const manifest: DocsManifest = parseDocsManifest(manifestSource);

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

function parseDocsManifest(source: string): DocsManifest {
  const sections: DocSection[] = [];
  let currentSection: DocSection | null = null;
  let currentPage: DocPage | null = null;

  for (const rawLine of source.split(/\r?\n/)) {
    const line = rawLine.trim();
    if (!line || line.startsWith('#')) continue;
    if (line === '[[section]]') {
      currentSection = { title: '', slug: '', pages: [] };
      sections.push(currentSection);
      currentPage = null;
      continue;
    }
    if (line === '[[section.page]]') {
      if (!currentSection) {
        throw new Error('apps/studio/docs/manifest.toml declares a page before a section');
      }
      currentPage = { title: '', file: '' };
      currentSection.pages.push(currentPage);
      continue;
    }
    const match = line.match(/^([A-Za-z0-9_.-]+)\s*=\s*"([^"]*)"$/);
    if (!match) continue;
    const [, key, value] = match;
    if (currentPage) {
      if (key === 'title') currentPage.title = value;
      if (key === 'file') currentPage.file = value;
    } else if (currentSection) {
      if (key === 'title') currentSection.title = value;
      if (key === 'slug') currentSection.slug = value;
    }
  }

  return { sections };
}
