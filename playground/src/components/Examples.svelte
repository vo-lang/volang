<script lang="ts">
  import { examples } from '../examples/index.ts';

  let { onSelect }: { onSelect: (example: { name: string; code: string }) => void } = $props();
</script>

<div class="examples">
  <span class="label">Load Example:</span>
  <div class="example-list">
    {#each examples as example}
      <button class="example-pill" onclick={() => onSelect(example)}>
        {example.name}
      </button>
    {/each}
  </div>
</div>

<style>
  .examples {
    display: flex;
    align-items: center;
    gap: 12px;
    overflow-x: auto;
    scrollbar-width: none; /* Hide scrollbar for cleaner look */
  }

  .examples::-webkit-scrollbar {
    display: none;
  }

  .label {
    font-size: 12px;
    font-weight: 600;
    color: var(--text-secondary);
    text-transform: uppercase;
    letter-spacing: 0.5px;
    flex-shrink: 0;
  }

  .example-list {
    display: flex;
    gap: 8px;
    flex-wrap: nowrap;
  }

  .example-pill {
    padding: 4px 12px;
    font-size: 12px;
    background: var(--bg-primary);
    color: var(--text-secondary);
    border: 1px solid var(--border);
    border-radius: 100px;
    cursor: pointer;
    transition: all 0.2s;
    white-space: nowrap;
  }

  .example-pill:hover {
    border-color: var(--accent);
    color: var(--accent);
    background: var(--accent-light);
  }
</style>
