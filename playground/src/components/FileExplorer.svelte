<script lang="ts">
  import JSZip from 'jszip';
  import { onMount } from 'svelte';

  let { onSelect }: { onSelect: (code: string, filename: string) => void } = $props();
  
  let files: { name: string; content: string }[] = $state([]);
  let loading = $state(true);
  let searchQuery = $state('');
  let selectedFile = $state('');

  const filteredFiles = $derived(
    searchQuery 
      ? files.filter(f => f.name.toLowerCase().includes(searchQuery.toLowerCase()))
      : files
  );

  onMount(async () => {
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
      
      files = loadedFiles.sort((a, b) => a.name.localeCompare(b.name));
    } catch (e) {
      console.error('Failed to load test_data.zip:', e);
    } finally {
      loading = false;
    }
  });

  function handleFileClick(file: { name: string; content: string }) {
    selectedFile = file.name;
    onSelect(file.content, file.name);
  }
</script>

<div class="file-explorer">
  <div class="explorer-header">
    <span class="title">Test Files</span>
    <span class="count">{files.length}</span>
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

  .explorer-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 12px 16px;
    border-bottom: 1px solid var(--border);
    flex-shrink: 0;
  }

  .title {
    font-size: 12px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    color: var(--text-secondary);
  }

  .count {
    font-size: 11px;
    padding: 2px 8px;
    background: var(--bg-tertiary);
    border-radius: 10px;
    color: var(--text-secondary);
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
