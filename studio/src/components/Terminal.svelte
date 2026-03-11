<script lang="ts">
  import { afterUpdate, onMount, tick } from 'svelte';
  import { terminal, termPush, termClear } from '../stores/terminal';
  import type { TermLine, TermLineKind } from '../stores/terminal';
  import { executeCommand, displayCwd, getCompletions } from '../lib/terminal_cmd';
  import { bridge } from '../lib/bridge';
  import { ide } from '../stores/ide';
  import { explorer } from '../stores/explorer';

  // mode: 'full' = standalone tab, 'panel' = embedded bottom panel
  export let mode: 'full' | 'panel' = 'full';

  let inputEl: HTMLInputElement;
  let scrollEl: HTMLDivElement;
  let inputValue = '';
  let historyIdx = -1;   // -1 = not browsing history
  let historyDraft = ''; // saved draft while browsing history
  let userScrolledUp = false;
  let showScrollPill = false;

  // Tab completion
  let completionList: string[] = [];
  let completionIdx = -1;
  let completionReplaceFrom = 0;
  let completionPending = false;

  $: lines    = $terminal.lines;
  $: busy     = $terminal.busy;
  $: cwd      = $terminal.cwd;
  $: history  = $terminal.history;
  $: prompt   = displayCwd(cwd, bridge().workspaceRoot) + ' $ ';

  onMount(() => {
    if (mode === 'full') focusInput();
    if ($terminal.lines.length === 0) {
      termPush('system', 'Vo Studio Shell  —  type \'help\' for available commands');
    }
  });

  function focusInput() {
    inputEl?.focus();
  }

  function handleRootClick() {
    const sel = window.getSelection();
    if (sel && sel.toString().length > 0) return;
    resetCompletions();
    focusInput();
  }

  // ── Tab completion ──────────────────────────────────────────────────────────

  function resetCompletions() {
    completionList = [];
    completionIdx  = -1;
    completionReplaceFrom = 0;
    completionPending = false;
  }

  async function handleTabComplete() {
    if (busy) return;
    const shell = bridge().shell;
    shell.cd(cwd);

    if (completionList.length > 0) {
      // Cycle through existing completions
      completionIdx = (completionIdx + 1) % completionList.length;
      inputValue = inputValue.slice(0, completionReplaceFrom) + completionList[completionIdx];
      return;
    }

    completionPending = true;
    try {
      const result = await getCompletions(inputValue, cwd, bridge().workspaceRoot, shell);
      if (result.completions.length === 0) { completionPending = false; return; }

      completionReplaceFrom = result.replaceFrom;

      if (result.completions.length === 1) {
        inputValue = inputValue.slice(0, completionReplaceFrom) + result.completions[0];
        completionPending = false;
        return;
      }

      // Multiple matches: fill longest common prefix, then show dropdown
      const common = longestCommonPrefix(result.completions);
      const partial = inputValue.slice(completionReplaceFrom);
      if (common.length > partial.length) {
        inputValue = inputValue.slice(0, completionReplaceFrom) + common;
      }
      completionList = result.completions;
      completionIdx  = 0;
      inputValue = inputValue.slice(0, completionReplaceFrom) + completionList[0];
    } finally {
      completionPending = false;
    }
  }

  function longestCommonPrefix(strs: string[]): string {
    if (strs.length === 0) return '';
    let prefix = strs[0];
    for (let i = 1; i < strs.length; i++) {
      while (!strs[i].startsWith(prefix)) prefix = prefix.slice(0, -1);
    }
    return prefix;
  }

  // Auto-scroll to bottom when new lines arrive.
  afterUpdate(() => {
    if (!userScrolledUp && scrollEl) {
      scrollEl.scrollTop = scrollEl.scrollHeight;
    }
  });

  function handleScroll() {
    if (!scrollEl) return;
    const { scrollTop, scrollHeight, clientHeight } = scrollEl;
    const atBottom = scrollHeight - scrollTop - clientHeight < 40;
    userScrolledUp = !atBottom;
    showScrollPill = userScrolledUp && lines.length > 0;
  }

  function scrollToBottom() {
    if (!scrollEl) return;
    scrollEl.scrollTop = scrollEl.scrollHeight;
    userScrolledUp = false;
    showScrollPill = false;
  }

  async function submit() {
    if (busy) return;
    const cmd = inputValue.trim();
    inputValue = '';
    historyIdx = -1;
    historyDraft = '';

    termPush('input', cmd);
    userScrolledUp = false;

    const shell = bridge().shell;
    shell.cd(cwd);

    const result = await executeCommand(cmd, shell, bridge().workspaceRoot, () => {});
    if (result.newCwd !== undefined) {
      terminal.update(s => ({ ...s, cwd: result.newCwd! }));
    } else {
      // Keep cwd in sync with shell client
      terminal.update(s => ({ ...s, cwd: shell.cwd }));
    }

    if (result.guiPath) {
      try {
        ide.update(s => ({ ...s, isRunning: true, isGuiApp: false, guestRender: null, runStatus: 'preparing', runDurationMs: null, outputExpanded: true }));
        await bridge().shell.exec({ kind: 'app.prepare', path: result.guiPath });
        ide.update(s => ({ ...s, runStatus: 'compiling' }));
        const res = await bridge().shell.exec({ kind: 'gui.run', path: result.guiPath });
        const bytes = (res as { renderBytes: Uint8Array }).renderBytes;
        ide.update(s => ({ ...s, isRunning: true, isGuiApp: true, guestRender: bytes, runStatus: 'running', runDurationMs: null, outputExpanded: true }));
        explorer.update(e => ({ ...e, appMode: 'develop' }));
      } catch (e) {
        ide.update(s => ({ ...s, isRunning: false, isGuiApp: false, guestRender: null, runStatus: 'error' }));
        termPush('error', `GUI launch failed: ${String(e)}`);
      }
    }

    await tick();
    focusInput();
  }

  function handleKeydown(e: KeyboardEvent) {
    if (e.key === 'Tab') {
      e.preventDefault();
      handleTabComplete();
      return;
    }

    if (e.key === 'Enter') {
      e.preventDefault();
      resetCompletions();
      submit();
      return;
    }

    if (e.key === 'Escape') {
      resetCompletions();
      return;
    }

    // Any printable key or backspace clears completion list
    if (!e.ctrlKey && !e.metaKey && !e.altKey && (e.key.length === 1 || e.key === 'Backspace' || e.key === 'Delete')) {
      resetCompletions();
    }

    if (e.key === 'ArrowUp') {
      e.preventDefault();
      if (history.length === 0) return;
      if (historyIdx === -1) {
        historyDraft = inputValue;
        historyIdx = history.length - 1;
      } else if (historyIdx > 0) {
        historyIdx--;
      }
      inputValue = history[historyIdx] ?? '';
      return;
    }

    if (e.key === 'ArrowDown') {
      e.preventDefault();
      if (historyIdx === -1) return;
      if (historyIdx < history.length - 1) {
        historyIdx++;
        inputValue = history[historyIdx];
      } else {
        historyIdx = -1;
        inputValue = historyDraft;
      }
      return;
    }

    if (e.key === 'c' && e.ctrlKey) {
      e.preventDefault();
      if (busy) {
        termPush('system', '^C');
      }
      inputValue = '';
      historyIdx = -1;
      return;
    }

    if (e.key === 'l' && e.ctrlKey) {
      e.preventDefault();
      termClear();
      return;
    }
  }

  // ANSI escape parser: supports bold, dim, italic, underline, 8 standard colors,
  // bright colors, and compound sequences like \x1b[1;34m.
  function parseLine(text: string): Array<{ text: string; cls: string }> {
    if (!text.includes('\x1b[')) return [{ text, cls: '' }];
    const parts: Array<{ text: string; cls: string }> = [];
    const segments = text.split(/(\x1b\[[0-9;]*m)/);
    const styles = new Set<string>();

    for (const seg of segments) {
      if (seg.startsWith('\x1b[') && seg.endsWith('m')) {
        const codes = seg.slice(2, -1).split(';').map(Number);
        for (const code of codes) {
          if      (code === 0)  { styles.clear(); }
          else if (code === 1)  { styles.add('ansi-bold'); }
          else if (code === 2)  { styles.add('ansi-dim'); }
          else if (code === 3)  { styles.add('ansi-italic'); }
          else if (code === 4)  { styles.add('ansi-underline'); }
          else if (code === 22) { styles.delete('ansi-bold'); styles.delete('ansi-dim'); }
          else if (code === 31) { clearColor(styles); styles.add('ansi-red'); }
          else if (code === 32) { clearColor(styles); styles.add('ansi-green'); }
          else if (code === 33) { clearColor(styles); styles.add('ansi-yellow'); }
          else if (code === 34) { clearColor(styles); styles.add('ansi-blue'); }
          else if (code === 35) { clearColor(styles); styles.add('ansi-magenta'); }
          else if (code === 36) { clearColor(styles); styles.add('ansi-cyan'); }
          else if (code === 37) { clearColor(styles); styles.add('ansi-white'); }
          else if (code === 90) { clearColor(styles); styles.add('ansi-bright-black'); }
          else if (code === 91) { clearColor(styles); styles.add('ansi-bright-red'); }
          else if (code === 92) { clearColor(styles); styles.add('ansi-bright-green'); }
          else if (code === 93) { clearColor(styles); styles.add('ansi-bright-yellow'); }
          else if (code === 94) { clearColor(styles); styles.add('ansi-bright-blue'); }
          else if (code === 95) { clearColor(styles); styles.add('ansi-bright-magenta'); }
          else if (code === 96) { clearColor(styles); styles.add('ansi-bright-cyan'); }
          else if (code === 97) { clearColor(styles); styles.add('ansi-bright-white'); }
        }
      } else if (seg) {
        parts.push({ text: seg, cls: [...styles].join(' ') });
      }
    }
    return parts;
  }

  function clearColor(styles: Set<string>) {
    const ATTRS = new Set(['ansi-bold','ansi-dim','ansi-italic','ansi-underline']);
    for (const s of styles) if (!ATTRS.has(s)) styles.delete(s);
  }

  function lineClass(kind: TermLineKind): string {
    switch (kind) {
      case 'input':      return 'tl-input';
      case 'output':     return 'tl-output';
      case 'error':      return 'tl-error';
      case 'warn':       return 'tl-warn';
      case 'stream-out': return 'tl-stream-out';
      case 'stream-err': return 'tl-stream-err';
      case 'system':     return 'tl-system';
    }
  }
</script>

<!-- svelte-ignore a11y-click-events-have-key-events a11y-no-static-element-interactions -->
<div
  class="terminal-root {mode}"
  on:click={handleRootClick}
>
  <!-- Output area -->
  <div
    class="terminal-output"
    bind:this={scrollEl}
    on:scroll={handleScroll}
  >
    {#each lines as line (line.id)}
      <div class="term-line {lineClass(line.kind)}">
        {#if line.kind === 'input'}
          <span class="prompt-echo">{displayCwd(cwd, bridge().workspaceRoot)} $</span>
          <span class="input-echo">&nbsp;{line.text}</span>
        {:else}
          {#each parseLine(line.text) as seg}
            <span class={seg.cls}>{seg.text}</span>
          {/each}
        {/if}
      </div>
    {/each}

    <!-- Spacer so input line is always visible -->
    <div class="bottom-pad"></div>
  </div>

  {#if showScrollPill}
    <button class="scroll-pill" on:click|stopPropagation={scrollToBottom}>
      ↓ New output
    </button>
  {/if}

  <!-- Tab completion dropdown -->
  {#if completionList.length > 1}
    <!-- svelte-ignore a11y-click-events-have-key-events a11y-no-static-element-interactions -->
    <div class="completion-bar" on:mousedown|preventDefault>
      {#each completionList as item, i}
        <!-- svelte-ignore a11y-click-events-have-key-events a11y-no-static-element-interactions -->
        <span
          class="completion-item"
          class:active={i === completionIdx}
          on:click={() => {
            inputValue = inputValue.slice(0, completionReplaceFrom) + item;
            resetCompletions();
            focusInput();
          }}
        >{item}</span>
      {/each}
    </div>
  {/if}

  <!-- Input line -->
  <div class="input-row" class:busy>
    <span class="prompt">{prompt}</span>
    <input
      bind:this={inputEl}
      bind:value={inputValue}
      class="term-input"
      type="text"
      autocomplete="off"
      autocorrect="off"
      autocapitalize="off"
      spellcheck={false}
      disabled={busy}
      on:keydown={handleKeydown}
    />
    {#if busy}
      <span class="busy-spinner"></span>
    {:else if completionPending}
      <span class="completion-spinner"></span>
    {/if}
  </div>
</div>

<style>
  .terminal-root {
    display: flex;
    flex-direction: column;
    background: #0d0d14;
    font-family: 'JetBrains Mono', 'Fira Code', 'Cascadia Code', ui-monospace, monospace;
    font-size: 13px;
    line-height: 1.55;
    color: #cdd6f4;
    overflow: hidden;
    cursor: text;
  }

  .terminal-root.full {
    flex: 1;
    height: 100%;
    min-height: 0;
  }

  .terminal-root.panel {
    height: 100%;
    min-height: 0;
  }

  /* ── Output ───────────────────────────────────────────────────────────── */

  .terminal-output {
    flex: 1;
    overflow-y: auto;
    padding: 10px 14px 4px;
    min-height: 0;
    scrollbar-width: thin;
    scrollbar-color: #313244 transparent;
  }

  .terminal-output::-webkit-scrollbar { width: 6px; }
  .terminal-output::-webkit-scrollbar-track { background: transparent; }
  .terminal-output::-webkit-scrollbar-thumb { background: #313244; border-radius: 3px; }

  .bottom-pad { height: 6px; }

  /* ── Line types ───────────────────────────────────────────────────────── */

  .term-line {
    white-space: pre-wrap;
    word-break: break-all;
    min-height: 1.55em;
    padding: 0;
  }

  .tl-input       { color: #89dceb; }
  .tl-output      { color: #cdd6f4; }
  .tl-error       { color: #f38ba8; }
  .tl-warn        { color: #f9e2af; }
  .tl-stream-out  { color: #a6e3a1; }
  .tl-stream-err  { color: #fab387; }
  .tl-system      { color: #585b70; font-style: italic; }

  .prompt-echo    { color: #6c7086; user-select: none; }
  .input-echo     { color: #89dceb; }

  /* ANSI attributes */
  :global(.ansi-bold)      { font-weight: 700; }
  :global(.ansi-dim)       { opacity: 0.55; }
  :global(.ansi-italic)    { font-style: italic; }
  :global(.ansi-underline) { text-decoration: underline; }

  /* ANSI standard colors */
  :global(.ansi-red)     { color: #f38ba8; }
  :global(.ansi-green)   { color: #a6e3a1; }
  :global(.ansi-yellow)  { color: #f9e2af; }
  :global(.ansi-blue)    { color: #89b4fa; }
  :global(.ansi-magenta) { color: #cba6f7; }
  :global(.ansi-cyan)    { color: #89dceb; }
  :global(.ansi-white)   { color: #cdd6f4; }
  :global(.ansi-bright-black)   { color: #45475a; }
  :global(.ansi-bright-red)     { color: #f38ba8; }
  :global(.ansi-bright-green)   { color: #a6e3a1; }
  :global(.ansi-bright-yellow)  { color: #f9e2af; }
  :global(.ansi-bright-blue)    { color: #89b4fa; }
  :global(.ansi-bright-magenta) { color: #cba6f7; }
  :global(.ansi-bright-cyan)    { color: #94e2d5; }
  :global(.ansi-bright-white)   { color: #ffffff; }

  /* ── Input row ────────────────────────────────────────────────────────── */

  .input-row {
    display: flex;
    align-items: center;
    padding: 4px 14px 8px;
    gap: 0;
    border-top: 1px solid #1a1a2a;
    flex-shrink: 0;
    background: #0d0d14;
  }

  .input-row.busy .prompt {
    color: #585b70;
  }

  .prompt {
    color: #a6e3a1;
    white-space: nowrap;
    user-select: none;
    flex-shrink: 0;
  }

  .term-input {
    flex: 1;
    background: none;
    border: none;
    outline: none;
    color: #cdd6f4;
    font-family: inherit;
    font-size: inherit;
    line-height: inherit;
    padding: 0;
    caret-color: #89b4fa;
    min-width: 0;
  }

  .term-input:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  .busy-spinner {
    width: 14px;
    height: 14px;
    border: 2px solid #313244;
    border-top-color: #a6e3a1;
    border-radius: 50%;
    animation: spin 0.7s linear infinite;
    flex-shrink: 0;
  }

  .completion-spinner {
    width: 14px;
    height: 14px;
    border: 2px solid #313244;
    border-top-color: #89b4fa;
    border-radius: 50%;
    animation: spin 0.7s linear infinite;
    flex-shrink: 0;
    opacity: 0.6;
  }

  @keyframes spin {
    to { transform: rotate(360deg); }
  }

  /* ── Completion bar ──────────────────────────────────────────────────── */

  .completion-bar {
    display: flex;
    flex-wrap: wrap;
    gap: 2px 4px;
    padding: 6px 14px;
    border-top: 1px solid #1a1a2a;
    background: #0a0a12;
    flex-shrink: 0;
    max-height: 80px;
    overflow-y: auto;
  }

  .completion-item {
    font-size: 12px;
    padding: 2px 8px;
    border-radius: 4px;
    color: #7f849c;
    cursor: pointer;
    white-space: nowrap;
    transition: background 0.1s, color 0.1s;
    border: 1px solid transparent;
  }

  .completion-item:hover,
  .completion-item.active {
    background: #1e1e35;
    color: #89b4fa;
    border-color: #2a2a4a;
  }

  /* ── Scroll pill ────────────────────────────────────────────────────────── */

  .scroll-pill {
    position: absolute;
    bottom: 52px;
    right: 18px;
    background: #313244;
    color: #cdd6f4;
    border: none;
    border-radius: 12px;
    padding: 4px 12px;
    font-size: 11px;
    cursor: pointer;
    z-index: 10;
    pointer-events: all;
  }

  .scroll-pill:hover {
    background: #45475a;
  }
</style>
