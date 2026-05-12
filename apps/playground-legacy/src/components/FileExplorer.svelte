<script lang="ts">
  import JSZip from 'jszip';
  import { onMount } from 'svelte';

  let { onSelect, selectedFile = $bindable('') }: { onSelect: (code: string, filename: string) => void, selectedFile?: string } = $props();
  
  type Tab = 'examples' | 'tests';
  let activeTab: Tab = $state('examples');
  
  let examples: { name: string; content: string }[] = $state([]);
  let testFiles: { name: string; content: string }[] = $state([]);
  let loadingExamples = $state(true);
  let loadingTests = $state(true);
  let searchQuery = $state('');

  const currentFiles = $derived(activeTab === 'examples' ? examples : testFiles);
  const loading = $derived(activeTab === 'examples' ? loadingExamples : loadingTests);
  
  const filteredFiles = $derived(
    searchQuery 
      ? currentFiles.filter(f => f.name.toLowerCase().includes(searchQuery.toLowerCase()))
      : currentFiles
  );

  // Load examples using import.meta.glob
  const exampleModules = import.meta.glob('../assets/examples/*.vo', { as: 'raw', eager: true });

  onMount(async () => {
    // Load examples from assets
    const loadedExamples: { name: string; content: string }[] = [];
    for (const [path, content] of Object.entries(exampleModules)) {
      const name = path.split('/').pop() || path;
      loadedExamples.push({ name, content: content as string });
    }
    examples = loadedExamples.sort((a, b) => a.name.localeCompare(b.name));
    loadingExamples = false;

    // Load test files from zip
    try {
      const response = await fetch('/test_data.zip');
      const blob = await response.blob();
      const zip = await JSZip.loadAsync(blob);
      
      const loadedFiles: { name: string; content: string }[] = [];
      
      for (const [filename, file] of Object.entries(zip.files)) {
        if (!file.dir && filename.endsWith('.vo')) {
          const content = await file.async('string');
          loadedFiles.push({ name: filename, content });
        }
      }
      
      testFiles = loadedFiles.sort((a, b) => a.name.localeCompare(b.name));
    } catch (e) {
      console.error('Failed to load test_data.zip:', e);
    } finally {
      loadingTests = false;
    }
  });

  function handleFileClick(file: { name: string; content: string }) {
    selectedFile = file.name;
    onSelect(file.content, file.name);
  }
  
  function switchTab(tab: Tab) {
    activeTab = tab;
    searchQuery = '';
    selectedFile = '';
  }
</script>

<div class="file-explorer">
  <div class="tabs">
    <button 
      class="tab" 
      class:active={activeTab === 'examples'}
      onclick={() => switchTab('examples')}
    >
      Examples
      <span class="count">{examples.length}</span>
    </button>
    <button 
      class="tab" 
      class:active={activeTab === 'tests'}
      onclick={() => switchTab('tests')}
    >
      Test Files
      <span class="count">{testFiles.length}</span>
    </button>
  </div>
  
  <div class="search-box">
    <input 
      type="text" 
      placeholder="Search files..."
      bind:value={searchQuery}
    />
  </div>

  <div class="file-list">
    {#if loading}
      <div class="loading">Loading...</div>
    {:else}
      {#each filteredFiles as file}
        <button 
          class="file-item"
          class:selected={selectedFile === file.name}
          onclick={() => handleFileClick(file)}
        >
          <span class="file-icon">📄</span>
          <span class="file-name">{file.name}</span>
        </button>
      {/each}
      
      {#if filteredFiles.length === 0}
        <div class="no-results">No matching files</div>
      {/if}
    {/if}
  </div>
</div>

<style>
  .file-explorer {
    display: flex;
    flex-direction: column;
    height: 100%;
    background: var(--bg-secondary);
    border-right: 1px solid var(--border);
  }

  .tabs {
    display: flex;
    border-bottom: 1px solid var(--border);
    flex-shrink: 0;
  }

  .tab {
    flex: 1;
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 6px;
    padding: 10px 12px;
    background: transparent;
    border: none;
    border-bottom: 2px solid transparent;
    color: var(--text-secondary);
    font-size: 12px;
    font-weight: 500;
    cursor: pointer;
    transition: all 0.2s;
  }

  .tab:hover {
    color: var(--text-primary);
    background: var(--bg-tertiary);
  }

  .tab.active {
    color: var(--accent);
    border-bottom-color: var(--accent);
  }

  .count {
    font-size: 10px;
    padding: 1px 6px;
    background: var(--bg-tertiary);
    border-radius: 10px;
    color: var(--text-tertiary);
  }

  .tab.active .count {
    background: var(--accent-light);
    color: var(--accent);
  }

  .search-box {
    padding: 8px 12px;
    border-bottom: 1px solid var(--border);
    flex-shrink: 0;
  }

  .search-box input {
    width: 100%;
    padding: 8px 12px;
    border: 1px solid var(--border);
    border-radius: 6px;
    background: var(--bg-primary);
    color: var(--text-primary);
    font-size: 13px;
    outline: none;
    transition: border-color 0.2s;
  }

  .search-box input:focus {
    border-color: var(--accent);
  }

  .search-box input::placeholder {
    color: var(--text-tertiary);
  }

  .file-list {
    flex: 1;
    overflow-y: auto;
    padding: 8px 0;
  }

  .file-item {
    display: flex;
    align-items: center;
    gap: 8px;
    width: 100%;
    padding: 6px 16px;
    background: transparent;
    border: none;
    color: var(--text-secondary);
    font-size: 13px;
    text-align: left;
    cursor: pointer;
    transition: all 0.15s;
  }

  .file-item:hover {
    background: var(--bg-tertiary);
    color: var(--text-primary);
  }

  .file-item.selected {
    background: var(--accent-light);
    color: var(--accent);
  }

  .file-icon {
    font-size: 14px;
    opacity: 0.7;
  }

  .file-name {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    font-family: var(--font-mono);
    font-size: 12px;
  }

  .no-results {
    padding: 20px 16px;
    text-align: center;
    color: var(--text-tertiary);
    font-size: 13px;
  }

  .loading {
    padding: 20px 16px;
    text-align: center;
    color: var(--text-secondary);
    font-size: 13px;
  }
</style>
