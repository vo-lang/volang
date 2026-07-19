<script lang="ts">
  import {
    STARTER_CATEGORIES,
    STARTER_EXAMPLES,
    type StarterCategoryId,
    type StarterExample,
  } from './content';

  type GalleryFilter = 'all' | StarterCategoryId;

  export let examples: readonly StarterExample[] = STARTER_EXAMPLES;
  export let onOpen: (example: StarterExample) => void = () => {};
  export let openingId: string | null = null;

  let activeFilter: GalleryFilter = 'all';

  $: visibleExamples = activeFilter === 'all'
    ? examples
    : examples.filter((example) => example.category === activeFilter);
  $: openingExample = openingId ? examples.find((example) => example.id === openingId) : null;

  function selectFilter(filter: GalleryFilter): void {
    activeFilter = filter;
  }
</script>

<section class="gallery" aria-labelledby="starter-gallery-title" aria-busy={openingId !== null}>
  <div class="heading-row">
    <div>
      <span class="kicker">Learn by running</span>
      <h2 id="starter-gallery-title">Pick a starting point</h2>
      <p>Open a focused example in Studio, change it, and see the result immediately.</p>
    </div>

    <div class="filters" role="group" aria-label="Filter starter examples">
      <button
        type="button"
        class:active={activeFilter === 'all'}
        aria-pressed={activeFilter === 'all'}
        on:click={() => selectFilter('all')}
      >All</button>
      {#each STARTER_CATEGORIES as category}
        <button
          type="button"
          class:active={activeFilter === category.id}
          aria-pressed={activeFilter === category.id}
          on:click={() => selectFilter(category.id)}
        >{category.label}</button>
      {/each}
    </div>
  </div>

  <p class="sr-only" role="status" aria-live="polite">
    {openingExample
      ? `Opening ${openingExample.title} in Studio`
      : `${visibleExamples.length} starter examples shown`}
  </p>

  <div class="cards">
    {#each visibleExamples as example (example.id)}
      <article class="card {example.accent}">
        <div class="card-top">
          <span class="monogram" aria-hidden="true">{example.monogram}</span>
          <span class="file">{example.file}</span>
        </div>
        <div class="card-copy">
          <h3>{example.title}</h3>
          <p>{example.description}</p>
        </div>
        <div class="card-footer">
          <ul class="tags" aria-label={`${example.title} topics`}>
            {#each example.tags as tag}
              <li>{tag}</li>
            {/each}
          </ul>
          <button
            class="open-button"
            type="button"
            disabled={openingId !== null}
            aria-busy={openingId === example.id}
            aria-label={`Open ${example.title} in Studio`}
            on:click={() => onOpen(example)}
          >
            <span>{openingId === example.id ? 'Opening…' : 'Open'}</span>
            <svg viewBox="0 0 16 16" aria-hidden="true"><path d="M4 8h8M9 5l3 3-3 3" /></svg>
          </button>
        </div>
      </article>
    {/each}
  </div>

  {#if visibleExamples.length === 0}
    <p class="empty">No starter examples match this filter.</p>
  {/if}
</section>

<style>
  .gallery {
    padding: clamp(64px, 8vw, 112px) clamp(20px, 7vw, 104px);
    color: #edf3ff;
    background: #0b101c;
  }

  .heading-row {
    display: flex;
    align-items: flex-end;
    justify-content: space-between;
    gap: 32px;
    max-width: 1440px;
    margin: 0 auto 34px;
  }

  .heading-row > div:first-child {
    max-width: 650px;
  }

  .kicker {
    color: #66daca;
    font-size: 12px;
    font-weight: 820;
    letter-spacing: 0.12em;
    text-transform: uppercase;
  }

  h2 {
    margin: 10px 0 0;
    font-size: clamp(32px, 4vw, 52px);
    font-weight: 810;
    letter-spacing: -0.04em;
    line-height: 1.05;
  }

  .heading-row p {
    max-width: 580px;
    margin: 15px 0 0;
    color: #8997ae;
    font-size: 15px;
    line-height: 1.65;
  }

  .filters {
    display: flex;
    flex-wrap: wrap;
    justify-content: flex-end;
    gap: 7px;
  }

  .filters button {
    min-height: 38px;
    padding: 0 13px;
    border: 1px solid rgba(149, 168, 204, 0.15);
    border-radius: 999px;
    color: #8390a7;
    background: rgba(255, 255, 255, 0.025);
    font: inherit;
    font-size: 12px;
    font-weight: 720;
    cursor: pointer;
    transition: color 150ms ease, border-color 150ms ease, background 150ms ease;
  }

  .filters button:hover,
  .filters button.active {
    border-color: rgba(108, 177, 255, 0.4);
    color: #cfe4ff;
    background: rgba(108, 177, 255, 0.1);
  }

  .filters button:focus-visible,
  .open-button:focus-visible {
    outline: 3px solid rgba(124, 183, 255, 0.4);
    outline-offset: 3px;
  }

  .cards {
    display: grid;
    grid-template-columns: repeat(3, minmax(0, 1fr));
    gap: 14px;
    max-width: 1440px;
    margin: 0 auto;
  }

  .card {
    --accent: #77b9ff;
    --accent-rgb: 119, 185, 255;
    min-height: 252px;
    display: flex;
    flex-direction: column;
    padding: 19px;
    border: 1px solid rgba(151, 170, 205, 0.13);
    border-radius: 18px;
    background:
      radial-gradient(circle at 100% 0, rgba(var(--accent-rgb), 0.09), transparent 42%),
      rgba(255, 255, 255, 0.025);
    transition: transform 170ms ease, border-color 170ms ease, background 170ms ease;
  }

  .card:hover {
    transform: translateY(-3px);
    border-color: rgba(var(--accent-rgb), 0.3);
    background:
      radial-gradient(circle at 100% 0, rgba(var(--accent-rgb), 0.14), transparent 44%),
      rgba(255, 255, 255, 0.038);
  }

  .card.violet {
    --accent: #c1a5ff;
    --accent-rgb: 193, 165, 255;
  }

  .card.mint {
    --accent: #63ddbd;
    --accent-rgb: 99, 221, 189;
  }

  .card.amber {
    --accent: #f0bd71;
    --accent-rgb: 240, 189, 113;
  }

  .card-top {
    display: flex;
    align-items: center;
    gap: 10px;
  }

  .monogram {
    width: 38px;
    height: 38px;
    display: inline-grid;
    place-items: center;
    flex: 0 0 auto;
    border: 1px solid rgba(var(--accent-rgb), 0.3);
    border-radius: 11px;
    color: var(--accent);
    background: rgba(var(--accent-rgb), 0.09);
    font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
    font-size: 11px;
    font-weight: 850;
    letter-spacing: 0.05em;
  }

  .file {
    min-width: 0;
    overflow: hidden;
    color: #8491a8;
    font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
    font-size: 11px;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .card-copy {
    flex: 1;
    padding-top: 20px;
  }

  h3 {
    margin: 0;
    color: #e8f0ff;
    font-size: 18px;
    font-weight: 760;
    letter-spacing: -0.018em;
  }

  .card-copy p {
    margin: 9px 0 0;
    color: #7f8da4;
    font-size: 13px;
    line-height: 1.62;
  }

  .card-footer {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    padding-top: 18px;
    border-top: 1px solid rgba(151, 170, 205, 0.09);
  }

  .tags {
    min-width: 0;
    display: flex;
    flex-wrap: wrap;
    gap: 5px;
    margin: 0;
    padding: 0;
    list-style: none;
  }

  .tags li {
    padding: 3px 7px;
    border-radius: 999px;
    color: #8491a8;
    background: rgba(255, 255, 255, 0.04);
    font-size: 10px;
    font-weight: 700;
  }

  .open-button {
    min-height: 38px;
    display: inline-flex;
    align-items: center;
    gap: 5px;
    flex: 0 0 auto;
    padding: 0 10px;
    border: 1px solid rgba(var(--accent-rgb), 0.24);
    border-radius: 10px;
    color: var(--accent);
    background: rgba(var(--accent-rgb), 0.07);
    font: inherit;
    font-size: 12px;
    font-weight: 780;
    cursor: pointer;
    transition: border-color 150ms ease, background 150ms ease;
  }

  .open-button:hover:not(:disabled) {
    border-color: rgba(var(--accent-rgb), 0.48);
    background: rgba(var(--accent-rgb), 0.13);
  }

  .open-button:disabled {
    cursor: wait;
    opacity: 0.52;
  }

  .open-button svg {
    width: 14px;
    height: 14px;
    fill: none;
    stroke: currentColor;
    stroke-width: 1.7;
    stroke-linecap: round;
    stroke-linejoin: round;
  }

  .empty {
    max-width: 1440px;
    margin: 20px auto 0;
    padding: 32px;
    border: 1px dashed rgba(151, 170, 205, 0.16);
    border-radius: 16px;
    color: #8794aa;
    font-size: 13px;
    text-align: center;
  }

  .sr-only {
    position: absolute;
    width: 1px;
    height: 1px;
    padding: 0;
    margin: -1px;
    overflow: hidden;
    clip: rect(0, 0, 0, 0);
    white-space: nowrap;
    border: 0;
  }

  @media (max-width: 1080px) {
    .heading-row {
      align-items: flex-start;
      flex-direction: column;
    }

    .filters {
      justify-content: flex-start;
    }

    .cards {
      grid-template-columns: repeat(2, minmax(0, 1fr));
    }
  }

  @media (max-width: 660px) {
    .gallery {
      padding: 64px 16px;
    }

    .heading-row {
      margin-bottom: 26px;
    }

    .filters {
      width: calc(100% + 32px);
      margin-inline: -16px;
      padding: 3px 16px 7px;
      flex-wrap: nowrap;
      justify-content: flex-start;
      overflow-x: auto;
      scrollbar-width: none;
    }

    .filters::-webkit-scrollbar {
      display: none;
    }

    .filters button {
      min-height: 42px;
      flex: 0 0 auto;
    }

    .cards {
      grid-template-columns: 1fr;
    }

    .card {
      min-height: 232px;
    }

    .open-button {
      min-height: 42px;
      padding-inline: 13px;
    }
  }

  @media (prefers-reduced-motion: reduce) {
    .card,
    .filters button,
    .open-button {
      transition: none;
    }

    .card:hover {
      transform: none;
    }
  }
</style>
