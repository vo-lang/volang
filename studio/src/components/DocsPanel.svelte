<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import { getManifest, getDocContent, renderMarkdown, getFirstPage } from '../lib/docs';
  import type { DocSection } from '../lib/docs';
  import { route, setDocsHash, resolveDocsFile } from '../lib/router';

  let manifest = getManifest();
  let activeFile: string = '';
  let renderedHtml = '';
  let expandedSections: Set<string> = new Set(manifest.sections.map(s => s.slug));
  let contentEl: HTMLElement | null = null;
  let mobileNavOpen = false;

  $: activeSection = manifest.sections.find((section) => section.pages.some((page) => page.file === activeFile));
  $: activePage = activeSection?.pages.find((page) => page.file === activeFile);
  $: activePageTitle = activePage?.title ?? 'Documentation';

  // Subscribe to route changes (e.g. browser back/forward, direct URL)
  const unsubRoute = route.subscribe(r => {
    if (r.mode === 'docs' && r.docsPath) {
      const file = resolveDocsFile(r.docsPath);
      if (file !== activeFile && getDocContent(file)) {
        loadPage(file, false);
      }
    }
  });

  onMount(() => {
    // If no route-driven page yet, load first page
    if (!activeFile) {
      const first = getFirstPage();
      if (first) {
        loadPage(first, true);
      }
    }
  });

  onDestroy(() => {
    unsubRoute();
  });

  function loadPage(file: string, updateHash = true): void {
    const source = getDocContent(file);
    if (!source) {
      renderedHtml = `<p class="docs-error">Document not found: ${file}</p>`;
      return;
    }
    activeFile = file;
    renderedHtml = renderMarkdown(source);
    if (updateHash) {
      setDocsHash(file);
    }
    if (contentEl) {
      contentEl.scrollTop = 0;
    }
    mobileNavOpen = false;
  }

  function toggleSection(slug: string): void {
    if (expandedSections.has(slug)) {
      expandedSections.delete(slug);
    } else {
      expandedSections.add(slug);
    }
    expandedSections = expandedSections;
  }

</script>

<svelte:window
  on:keydown={(event) => {
    if (event.key === 'Escape') {
      mobileNavOpen = false;
    }
  }}
/>

<div class="docs-layout">
  <div class="docs-mobile-bar">
    <button
      class="docs-mobile-menu"
      aria-label="Open documentation menu"
      aria-controls="docs-navigation"
      aria-expanded={mobileNavOpen}
      on:click={() => (mobileNavOpen = true)}
    >
      <svg viewBox="0 0 16 16" aria-hidden="true">
        <path d="M3 4h10M3 8h10M3 12h10" />
      </svg>
    </button>
    <div class="docs-mobile-title">
      <span class="docs-mobile-kicker">Documentation</span>
      <span class="docs-mobile-current">{activePageTitle}</span>
    </div>
  </div>

  {#if mobileNavOpen}
    <button
      class="docs-nav-backdrop"
      aria-label="Close documentation menu"
      on:click={() => (mobileNavOpen = false)}
    ></button>
  {/if}

  <nav id="docs-navigation" class="docs-sidebar" class:mobile-open={mobileNavOpen}>
    <div class="docs-sidebar-header">
      <span>Documentation</span>
      <button
        class="docs-sidebar-close"
        aria-label="Close documentation menu"
        on:click={() => (mobileNavOpen = false)}
      >
        <svg viewBox="0 0 16 16" aria-hidden="true">
          <path d="M4 4l8 8M12 4l-8 8" />
        </svg>
      </button>
    </div>
    {#each manifest.sections as section}
      <div class="docs-section">
        <button
          class="docs-section-toggle"
          class:expanded={expandedSections.has(section.slug)}
          on:click={() => toggleSection(section.slug)}
        >
          <span class="docs-section-arrow">{expandedSections.has(section.slug) ? '▾' : '▸'}</span>
          {section.title}
        </button>
        {#if expandedSections.has(section.slug)}
          <div class="docs-section-pages">
            {#each section.pages as page}
              <button
                class="docs-page-link"
                class:active={page.file === activeFile}
                on:click={() => loadPage(page.file, true)}
              >
                {page.title}
              </button>
            {/each}
          </div>
        {/if}
      </div>
    {/each}
  </nav>

  <main class="docs-content" bind:this={contentEl}>
    <article class="docs-article">
      {@html renderedHtml}
    </article>
  </main>
</div>

<style>
  .docs-layout {
    display: flex;
    flex: 1;
    min-height: 0;
    overflow: hidden;
    position: relative;
  }

  .docs-mobile-bar,
  .docs-nav-backdrop,
  .docs-sidebar-close {
    display: none;
  }

  /* -- Sidebar -- */
  .docs-sidebar {
    width: 240px;
    flex-shrink: 0;
    background: #181825;
    border-right: 1px solid #1e1e2e;
    display: flex;
    flex-direction: column;
    overflow-y: auto;
    padding: 0;
  }
  .docs-sidebar-header {
    padding: 16px 16px 12px;
    font-size: 12px;
    font-weight: 700;
    letter-spacing: 0.06em;
    text-transform: uppercase;
    color: #585b70;
  }
  .docs-section {
    margin-bottom: 2px;
  }
  .docs-section-toggle {
    display: flex;
    align-items: center;
    gap: 6px;
    width: 100%;
    border: none;
    background: none;
    color: #a6adc8;
    font: inherit;
    font-size: 13px;
    font-weight: 600;
    padding: 6px 16px;
    cursor: pointer;
    text-align: left;
    transition: color 0.1s;
  }
  .docs-section-toggle:hover {
    color: #cdd6f4;
  }
  .docs-section-arrow {
    font-size: 10px;
    width: 12px;
    flex-shrink: 0;
    text-align: center;
  }
  .docs-section-pages {
    display: flex;
    flex-direction: column;
    padding: 2px 0 6px;
  }
  .docs-page-link {
    display: block;
    width: 100%;
    border: none;
    background: none;
    color: #7f849c;
    font: inherit;
    font-size: 13px;
    padding: 5px 16px 5px 34px;
    cursor: pointer;
    text-align: left;
    transition: color 0.1s, background 0.1s;
    border-left: 2px solid transparent;
  }
  .docs-page-link:hover {
    color: #cdd6f4;
    background: rgba(137, 180, 250, 0.04);
  }
  .docs-page-link.active {
    color: #89b4fa;
    border-left-color: #89b4fa;
    background: rgba(137, 180, 250, 0.08);
  }

  /* -- Content -- */
  .docs-content {
    flex: 1;
    overflow-y: auto;
    padding: 0;
    min-width: 0;
  }
  .docs-article {
    max-width: 760px;
    margin: 0 auto;
    padding: 32px 40px 80px;
    line-height: 1.7;
    color: #cdd6f4;
  }

  /* -- Markdown typography -- */
  .docs-article :global(h1) {
    font-size: 28px;
    font-weight: 800;
    margin: 0 0 16px;
    color: #cdd6f4;
    letter-spacing: 0;
  }
  .docs-article :global(h2) {
    font-size: 20px;
    font-weight: 700;
    margin: 32px 0 12px;
    color: #cdd6f4;
    padding-bottom: 6px;
    border-bottom: 1px solid #313244;
  }
  .docs-article :global(h3) {
    font-size: 16px;
    font-weight: 700;
    margin: 24px 0 8px;
    color: #bac2de;
  }
  .docs-article :global(p) {
    margin: 0 0 14px;
    color: #a6adc8;
  }
  .docs-article :global(a) {
    color: #89b4fa;
    text-decoration: none;
  }
  .docs-article :global(a:hover) {
    text-decoration: underline;
  }
  .docs-article :global(strong) {
    color: #cdd6f4;
    font-weight: 700;
  }
  .docs-article :global(code) {
    font-family: 'SF Mono', 'Fira Code', 'Cascadia Code', monospace;
    font-size: 0.88em;
    background: #1e1e2e;
    padding: 2px 6px;
    border-radius: 4px;
    color: #f5c2e7;
  }
  .docs-article :global(pre) {
    background: #1e1e2e;
    border: 1px solid #313244;
    border-radius: 8px;
    padding: 14px 16px;
    overflow-x: auto;
    margin: 0 0 16px;
    line-height: 1.5;
  }
  .docs-article :global(pre code) {
    background: none;
    padding: 0;
    border-radius: 0;
    font-size: 13px;
    color: #cdd6f4;
  }
  .docs-article :global(blockquote) {
    border-left: 3px solid #89b4fa;
    margin: 0 0 16px;
    padding: 8px 16px;
    color: #a6adc8;
    background: rgba(137, 180, 250, 0.04);
    border-radius: 0 8px 8px 0;
  }
  .docs-article :global(blockquote p) {
    margin: 0;
  }
  .docs-article :global(ul),
  .docs-article :global(ol) {
    margin: 0 0 14px;
    padding-left: 24px;
    color: #a6adc8;
  }
  .docs-article :global(li) {
    margin-bottom: 4px;
  }
  .docs-article :global(table) {
    width: 100%;
    border-collapse: collapse;
    margin: 0 0 16px;
    font-size: 13px;
  }
  .docs-article :global(th) {
    text-align: left;
    padding: 8px 12px;
    border-bottom: 2px solid #313244;
    color: #cdd6f4;
    font-weight: 700;
  }
  .docs-article :global(td) {
    padding: 6px 12px;
    border-bottom: 1px solid #1e1e2e;
    color: #a6adc8;
  }
  .docs-article :global(hr) {
    border: none;
    border-top: 1px solid #313244;
    margin: 24px 0;
  }

  @media (max-width: 700px) {
    .docs-layout {
      flex-direction: column;
      overflow: hidden;
    }

    .docs-mobile-bar {
      display: flex;
      align-items: center;
      gap: 12px;
      min-height: 52px;
      padding: 8px 14px;
      border-bottom: 1px solid #1e1e2e;
      background: #11111b;
      flex: 0 0 auto;
    }

    .docs-mobile-menu,
    .docs-sidebar-close {
      border: 1px solid #313244;
      background: #181825;
      color: #bac2de;
      cursor: pointer;
      font: inherit;
      display: flex;
      align-items: center;
      justify-content: center;
      flex-shrink: 0;
      transition: background 0.1s, border-color 0.1s, color 0.1s;
    }

    .docs-mobile-menu:hover,
    .docs-sidebar-close:hover {
      background: #1e1e2e;
      border-color: #45475a;
      color: #cdd6f4;
    }

    .docs-mobile-menu {
      width: 36px;
      height: 36px;
      border-radius: 8px;
    }

    .docs-mobile-menu svg,
    .docs-sidebar-close svg {
      width: 16px;
      height: 16px;
      stroke: currentColor;
      fill: none;
      stroke-width: 1.8;
      stroke-linecap: round;
      stroke-linejoin: round;
    }

    .docs-mobile-title {
      display: flex;
      flex-direction: column;
      gap: 1px;
      min-width: 0;
    }

    .docs-mobile-kicker {
      color: #585b70;
      font-size: 10px;
      font-weight: 800;
      letter-spacing: 0.08em;
      line-height: 1.2;
      text-transform: uppercase;
    }

    .docs-mobile-current {
      color: #cdd6f4;
      font-size: 14px;
      font-weight: 700;
      line-height: 1.3;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }

    .docs-nav-backdrop {
      display: block;
      position: absolute;
      inset: 0;
      z-index: 10;
      border: none;
      background: rgba(17, 17, 27, 0.68);
      backdrop-filter: blur(2px);
      cursor: default;
    }

    .docs-sidebar {
      position: absolute;
      inset: 0 auto 0 0;
      z-index: 11;
      width: min(320px, calc(100% - 24px));
      max-height: none;
      border-right: 1px solid #313244;
      border-bottom: none;
      box-shadow: 24px 0 48px rgba(0, 0, 0, 0.35);
      transform: translateX(-100%);
      transition: transform 0.16s ease, visibility 0.16s ease;
      visibility: hidden;
      pointer-events: none;
    }

    .docs-sidebar.mobile-open {
      transform: translateX(0);
      visibility: visible;
      pointer-events: auto;
    }

    .docs-sidebar-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 12px;
      position: sticky;
      top: 0;
      z-index: 1;
      background: #181825;
      padding: 12px 12px 8px 16px;
    }

    .docs-sidebar-close {
      width: 32px;
      height: 32px;
      border-radius: 8px;
    }

    .docs-section-toggle {
      padding: 8px 16px;
    }

    .docs-page-link {
      padding: 7px 16px 7px 34px;
    }

    .docs-content {
      width: 100%;
      flex: 1 1 auto;
      min-height: 0;
    }

    .docs-article {
      max-width: none;
      padding: 24px 20px 72px;
    }

    .docs-article :global(h1) {
      font-size: 24px;
    }

    .docs-article :global(h2) {
      font-size: 18px;
      margin-top: 28px;
    }

    .docs-article :global(pre) {
      margin-left: -4px;
      margin-right: -4px;
      border-radius: 6px;
    }

    .docs-article :global(table) {
      display: block;
      overflow-x: auto;
      white-space: nowrap;
    }
  }

  @media (max-width: 430px) {
    .docs-article {
      padding: 22px 16px 64px;
    }

    .docs-article :global(h1) {
      font-size: 22px;
    }
  }

  /* -- highlight.js Catppuccin Mocha tokens -- */
  .docs-article :global(.hljs-keyword) { color: #cba6f7; }
  .docs-article :global(.hljs-built_in) { color: #f9e2af; }
  .docs-article :global(.hljs-type) { color: #f9e2af; }
  .docs-article :global(.hljs-literal) { color: #fab387; }
  .docs-article :global(.hljs-number) { color: #fab387; }
  .docs-article :global(.hljs-string) { color: #a6e3a1; }
  .docs-article :global(.hljs-comment) { color: #585b70; font-style: italic; }
  .docs-article :global(.hljs-function) { color: #89b4fa; }
  .docs-article :global(.hljs-title) { color: #89b4fa; }
  .docs-article :global(.hljs-params) { color: #f5c2e7; }
  .docs-article :global(.hljs-attr) { color: #89dceb; }
  .docs-article :global(.hljs-selector-class) { color: #f9e2af; }
  .docs-article :global(.hljs-symbol) { color: #f2cdcd; }

  :global(.docs-error) {
    color: #f38ba8;
    font-size: 14px;
    padding: 24px;
  }
</style>
