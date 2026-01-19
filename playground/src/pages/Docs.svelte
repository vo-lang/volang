<script lang="ts">
  import { marked } from 'marked';
  import { currentHash, navigate } from '../lib/router';

  // Import all markdown files
  const modules = import.meta.glob('../assets/docs/**/*.md', { query: '?raw', import: 'default' });

  // Map simplified IDs to file paths
  const docsMap: Record<string, string> = {
    'vo-for-gophers': '../assets/docs/vo-for-gophers.md',
    'language': '../assets/docs/spec/language.md',
    'dynamic': '../assets/docs/spec/dynamic.md',
    'module': '../assets/docs/spec/module.md',
    'native-ffi': '../assets/docs/spec/native-ffi.md',
    'memory-model': '../assets/docs/spec/memory-model-and-instructions.md',
    'type-attributes': '../assets/docs/spec/type-attributes.md',
    'vm-bytecode': '../assets/docs/spec/vm-bytecode.md',
    'vm-jit': '../assets/docs/spec/vm-jit-unified-design.md',
  };

  const menu = [
    { title: 'Guides', items: [{ id: 'vo-for-gophers', label: 'Vo for Gophers' }] },
    { title: 'Specification', items: [
      { id: 'language', label: 'Language Spec' },
      { id: 'dynamic', label: 'Dynamic Semantics' },
      { id: 'module', label: 'Module System' },
      { id: 'native-ffi', label: 'Native FFI' },
      { id: 'memory-model', label: 'Memory Model' },
      { id: 'type-attributes', label: 'Type Attributes' },
    ]},
    { title: 'Internals', items: [
      { id: 'vm-bytecode', label: 'VM Bytecode' },
      { id: 'vm-jit', label: 'JIT Design' },
    ]},
  ];

  let content = $state('');
  let activeDoc = $state('');

  // Effect to load content when hash changes
  $effect(() => {
    const parts = $currentHash.split('/');
    // Default to vo-for-gophers if just #docs or #docs/
    const docId = (parts.length > 1 && parts[1]) ? parts[1] : 'vo-for-gophers';
    
    if (docId !== activeDoc) {
      activeDoc = docId;
      loadDoc(docId);
    }
  });

  async function loadDoc(id: string) {
    const path = docsMap[id];
    if (!path || !modules[path]) {
      content = '<h1>Document not found</h1>';
      return;
    }

    try {
      const raw = await modules[path]() as string;
      content = marked(raw) as string;
    } catch (e) {
      content = `<h1>Error loading document</h1><pre>${e}</pre>`;
    }
  }
</script>

<div class="docs-container">
  <div class="sidebar">
    {#each menu as section}
      <h3>{section.title}</h3>
      <ul>
        {#each section.items as item}
          <li>
            <button 
              class:active={activeDoc === item.id}
              onclick={() => navigate(`docs/${item.id}`)}
            >
              {item.label}
            </button>
          </li>
        {/each}
      </ul>
    {/each}
  </div>
  <div class="content markdown-body">
    {@html content}
  </div>
</div>

<style>
  .docs-container {
    display: flex;
    max-width: 1400px;
    margin: 0 auto;
    height: calc(100vh - var(--header-height));
  }

  .sidebar {
    width: 280px;
    padding: 30px;
    border-right: 1px solid var(--border);
    background: var(--bg-secondary);
    overflow-y: auto;
    flex-shrink: 0;
  }

  .sidebar h3 {
    font-size: 13px;
    margin: 24px 0 12px;
    color: var(--text-secondary);
    text-transform: uppercase;
    letter-spacing: 1px;
    font-weight: 600;
  }

  .sidebar h3:first-child {
    margin-top: 0;
  }

  .sidebar ul {
    list-style: none;
  }

  .sidebar button {
    display: block;
    width: 100%;
    text-align: left;
    padding: 8px 12px;
    color: var(--text-secondary);
    background: none;
    border: none;
    border-radius: 6px;
    cursor: pointer;
    font-size: 14px;
    transition: all 0.2s;
  }

  .sidebar button:hover {
    background: var(--bg-tertiary);
    color: var(--text-primary);
  }

  .sidebar button.active {
    background: var(--bg-tertiary);
    color: var(--accent);
    font-weight: 500;
  }

  .content {
    flex: 1;
    padding: 40px 60px;
    overflow-y: auto;
    font-size: 16px;
    line-height: 1.6;
    color: var(--text-primary);
    max-width: 900px; /* Limit line length for readability */
  }

  /* Basic Markdown Styles */
  :global(.markdown-body h1), :global(.markdown-body h2), :global(.markdown-body h3) {
    margin-top: 24px;
    margin-bottom: 16px;
    font-weight: 600;
    line-height: 1.25;
    color: var(--text-primary);
  }

  :global(.markdown-body h1) { font-size: 2em; padding-bottom: 0.3em; border-bottom: 1px solid var(--border); }
  :global(.markdown-body h2) { font-size: 1.5em; padding-bottom: 0.3em; border-bottom: 1px solid var(--border); }
  :global(.markdown-body h3) { font-size: 1.25em; }
  :global(.markdown-body h4) { font-size: 1.1em; font-weight: 600; }

  :global(.markdown-body p) { margin-bottom: 16px; }
  
  :global(.markdown-body a) { color: var(--accent); text-decoration: none; }
  :global(.markdown-body a:hover) { text-decoration: underline; }

  :global(.markdown-body code) {
    padding: 0.2em 0.4em;
    margin: 0;
    font-size: 85%;
    background-color: var(--bg-tertiary);
    border-radius: 6px;
    font-family: 'Consolas', monospace;
    color: var(--text-primary);
  }

  :global(.markdown-body pre) {
    padding: 16px;
    overflow: auto;
    font-size: 85%;
    line-height: 1.45;
    background-color: var(--bg-primary);
    border-radius: 6px;
    margin-bottom: 16px;
    border: 1px solid var(--border);
  }

  :global(.markdown-body pre code) {
    background-color: transparent;
    padding: 0;
    color: inherit;
  }

  :global(.markdown-body table) {
    border-collapse: collapse;
    width: 100%;
    margin-bottom: 16px;
  }

  :global(.markdown-body table th), :global(.markdown-body table td) {
    padding: 8px 13px;
    border: 1px solid var(--border);
  }

  :global(.markdown-body table tr:nth-child(2n)) {
    background-color: var(--bg-secondary);
  }

  :global(.markdown-body blockquote) {
    padding: 0 1em;
    color: var(--text-secondary);
    border-left: 0.25em solid var(--accent);
    background: var(--bg-secondary);
    margin: 0 0 16px 0;
  }

  :global(.markdown-body ul), :global(.markdown-body ol) {
    padding-left: 2em;
    margin-bottom: 16px;
  }

  :global(.markdown-body li) {
    margin-bottom: 0.25em;
  }

  @media (max-width: 768px) {
    .docs-container {
      flex-direction: column;
    }
    .sidebar {
      width: 100%;
      height: auto;
      border-right: none;
      border-bottom: 1px solid var(--border);
      padding: 20px;
    }
    .content {
      padding: 20px;
    }
  }
</style>
