<script lang="ts">
  import { onDestroy, onMount } from 'svelte';
  import { Terminal as XTerm } from '@xterm/xterm';
  import { FitAddon } from '@xterm/addon-fit';
  import { WebLinksAddon } from '@xterm/addon-web-links';
  import type { TermLine, TermService, TermState } from '../lib/services/term_service';
  import { formatError } from '../lib/format_error';

  export let term: TermService;
  export let sessionRoot = '';
  export let currentDir = '';
  export let platform: 'native' | 'wasm' = 'native';
  export let afterExecute: (() => void) | undefined = undefined;

  let containerEl: HTMLDivElement;
  let xterm: XTerm | null = null;
  let fitAddon: FitAddon | null = null;
  let inputBuffer = '';
  let historyIndex = -1;
  let historySnapshot: string[] = [];
  let resizeObserver: ResizeObserver | null = null;
  let unsubscribe: (() => void) | null = null;
  let lastLineCount = 0;
  let cwdDraft = '';
  let cwdError = '';
  let termState: TermState = { cwd: '/', history: [], lastCommand: null, lines: [] };

  $: sessionRootLabel = sessionRoot || term?.sessionRoot || '/';
  $: currentDirLabel = currentDir || sessionRootLabel;
  $: pathHint = platform === 'native'
    ? 'TERM CWD controls TERM path resolution and native process cwd.'
    : 'TERM CWD is a browser VFS path inside the current Studio session.';

  // ANSI color helpers
  function esc(code: string): string { return `\x1b[${code}m`; }
  const RESET  = esc('0');
  const BOLD   = esc('1');
  const GREEN  = esc('32');
  const RED    = esc('31');
  const CYAN   = esc('36');
  const DIM    = esc('2');

  function prompt(cwd: string): string {
    return `${DIM}${cwd}${RESET} ${CYAN}$${RESET} `;
  }

  function writePrompt(): void {
    if (!xterm) return;
    const s = term.snapshot();
    xterm.write(prompt(s.cwd));
  }

  function printLines(lines: TermLine[]): void {
    if (!xterm) return;
    // Handle clear: lines array was reset — also clear the visual terminal
    if (lines.length < lastLineCount) {
      lastLineCount = 0;
      xterm.clear();
    }
    if (lines.length <= lastLineCount) return;
    const newLines = lines.slice(lastLineCount);
    for (const line of newLines) {
      // 'input' lines are already echoed by the keyboard handler — skip them
      if (line.kind === 'input') continue;
      const colored =
        line.kind === 'error'  ? `${RED}${line.text}${RESET}` :
        line.kind === 'output' ? line.text :
        `${DIM}${line.text}${RESET}`;
      xterm.writeln('\r' + colored);
    }
    lastLineCount = lines.length;
  }

  async function applyCwd(): Promise<void> {
    cwdError = '';
    try {
      await term.setCwd(cwdDraft);
      termState = term.snapshot();
      cwdDraft = termState.cwd;
    } catch (error) {
      cwdError = formatError(error);
    }
  }

  async function useSessionRoot(): Promise<void> {
    cwdDraft = sessionRootLabel;
    await applyCwd();
  }

  async function useCurrentDir(): Promise<void> {
    cwdDraft = currentDirLabel;
    await applyCwd();
  }

  onMount(() => {
    xterm = new XTerm({
      theme: {
        background:   '#11111b',
        foreground:   '#cdd6f4',
        cursor:       '#f5c2e7',
        selectionBackground: '#313244',
        black:        '#45475a', brightBlack:  '#585b70',
        red:          '#f38ba8', brightRed:    '#f38ba8',
        green:        '#a6e3a1', brightGreen:  '#a6e3a1',
        yellow:       '#f9e2af', brightYellow: '#f9e2af',
        blue:         '#89b4fa', brightBlue:   '#89b4fa',
        magenta:      '#f5c2e7', brightMagenta:'#f5c2e7',
        cyan:         '#89dceb', brightCyan:   '#89dceb',
        white:        '#bac2de', brightWhite:  '#a6adc8',
      },
      fontFamily: "'JetBrains Mono', 'Fira Mono', 'Cascadia Code', monospace",
      fontSize: 13,
      lineHeight: 1.55,
      cursorBlink: true,
      scrollback: 10000,
      convertEol: true,
    });

    fitAddon = new FitAddon();
    xterm.loadAddon(fitAddon);
    xterm.loadAddon(new WebLinksAddon());
    xterm.open(containerEl);
    fitAddon.fit();

    // Welcome message + initial prompt
    xterm.writeln(`${BOLD}${CYAN}Vo Studio TERM${RESET}`);
    xterm.writeln(`${DIM}Type 'help' for available commands.${RESET}`);
    const s0 = term.snapshot();
    termState = s0;
    cwdDraft = s0.cwd;
    xterm.write(prompt(s0.cwd));

    // Subscribe to service state to render new output lines
    unsubscribe = term.state.subscribe((state) => {
      const previousCwd = termState.cwd;
      termState = state;
      if (!cwdError && (cwdDraft === '' || cwdDraft === previousCwd)) {
        cwdDraft = state.cwd;
      }
      printLines(state.lines);
    });

    // Handle keyboard input
    xterm.onKey(({ key, domEvent }) => {
      const code = domEvent.key;

      if (code === 'Enter') {
        const cmd = inputBuffer.trim();
        inputBuffer = '';
        historyIndex = -1;
        xterm!.write('\r\n');
        if (!cmd) {
          writePrompt();
          return;
        }
        lastLineCount = term.snapshot().lines.length;
        void term.execute(cmd).then(() => {
          afterExecute?.();
          // Flush any pending output lines from service state
          const snap = term.snapshot();
          printLines(snap.lines);
          writePrompt();
        });
        return;
      }

      if (code === 'Backspace') {
        if (inputBuffer.length > 0) {
          inputBuffer = inputBuffer.slice(0, -1);
          xterm!.write('\b \b');
        }
        return;
      }

      if (code === 'ArrowUp') {
        const hist = term.snapshot().history;
        historySnapshot = hist;
        if (historyIndex < hist.length - 1) {
          historyIndex++;
          replaceInput(hist[hist.length - 1 - historyIndex] ?? '');
        }
        return;
      }

      if (code === 'ArrowDown') {
        if (historyIndex > 0) {
          historyIndex--;
          const hist = historySnapshot;
          replaceInput(hist[hist.length - 1 - historyIndex] ?? '');
        } else if (historyIndex === 0) {
          historyIndex = -1;
          replaceInput('');
        }
        return;
      }

      if (code === 'Tab') {
        domEvent.preventDefault();
        return;
      }

      // Ctrl+C — clear input line
      if (code === 'c' && domEvent.ctrlKey) {
        xterm!.write('^C\r\n');
        inputBuffer = '';
        historyIndex = -1;
        const s = term.snapshot();
        xterm!.write(prompt(s.cwd));
        return;
      }

      // Ctrl+L — clear screen
      if (code === 'l' && domEvent.ctrlKey) {
        term.clear();
        lastLineCount = 0;
        xterm!.clear();
        const s = term.snapshot();
        xterm!.write(prompt(s.cwd));
        return;
      }

      // Printable characters
      if (key.length === 1 && !domEvent.ctrlKey && !domEvent.altKey && !domEvent.metaKey) {
        inputBuffer += key;
        xterm!.write(key);
      }
    });

    // Fit on resize
    resizeObserver = new ResizeObserver(() => { fitAddon?.fit(); });
    resizeObserver.observe(containerEl);
  });

  function replaceInput(newVal: string): void {
    if (!xterm) return;
    xterm.write('\r\x1b[K');
    const s = term.snapshot();
    xterm.write(prompt(s.cwd) + newVal);
    inputBuffer = newVal;
  }

  onDestroy(() => {
    unsubscribe?.();
    resizeObserver?.disconnect();
    xterm?.dispose();
    xterm = null;
  });
</script>

<div class="term-root">
  <div class="term-header">
    <div class="term-header-main">
      <div class="term-title-group">
        <div class="term-title">TERM</div>
        <div class="term-subtitle">{platform === 'native' ? 'Native session' : 'Browser session'}</div>
      </div>

      <div class="term-paths">
        <div class="path-card">
          <div class="path-label">Session Root</div>
          <div class="path-value" title={sessionRootLabel}>{sessionRootLabel}</div>
        </div>

        <form class="path-card path-card-editable" on:submit|preventDefault={() => void applyCwd()}>
          <div class="path-label">TERM CWD</div>
          <div class="cwd-row">
            <input
              class="cwd-input"
              bind:value={cwdDraft}
              spellcheck="false"
              autocomplete="off"
              autocapitalize="off"
              on:input={() => {
                cwdError = '';
              }}
            />
            <button class="path-btn path-btn-primary" type="submit" disabled={!cwdDraft || cwdDraft === termState.cwd}>Apply</button>
            <button class="path-btn" type="button" on:click={() => void useSessionRoot()} disabled={termState.cwd === sessionRootLabel}>Root</button>
            <button class="path-btn" type="button" on:click={() => void useCurrentDir()} disabled={!currentDirLabel || termState.cwd === currentDirLabel}>Folder</button>
          </div>
        </form>
      </div>
    </div>

    <div class="term-hint">{pathHint}</div>

    {#if cwdError}
      <div class="term-error">{cwdError}</div>
    {/if}
  </div>

  <div class="term-body" bind:this={containerEl}></div>
</div>

<style>
  :global(.xterm) { height: 100%; }
  :global(.xterm-viewport) { overflow-y: auto !important; }

  .term-root {
    display: flex;
    flex-direction: column;
    width: 100%;
    height: 100%;
    overflow: hidden;
    background: #11111b;
    padding: 0;
    box-sizing: border-box;
  }
  .term-header {
    display: flex;
    flex-direction: column;
    gap: 10px;
    padding: 12px;
    border-bottom: 1px solid #1e1e2e;
    background: linear-gradient(180deg, rgba(24, 24, 37, 0.98), rgba(17, 17, 27, 0.98));
    flex-shrink: 0;
  }
  .term-header-main {
    display: flex;
    align-items: flex-start;
    justify-content: space-between;
    gap: 12px;
  }
  .term-title-group {
    display: flex;
    flex-direction: column;
    gap: 4px;
    min-width: 120px;
  }
  .term-title {
    color: #cdd6f4;
    font-size: 12px;
    font-weight: 800;
    letter-spacing: 0.1em;
    text-transform: uppercase;
  }
  .term-subtitle {
    color: #6c7086;
    font-size: 11px;
    line-height: 1.4;
  }
  .term-paths {
    display: grid;
    grid-template-columns: minmax(220px, 1fr) minmax(320px, 1.4fr);
    gap: 10px;
    flex: 1;
    min-width: 0;
  }
  .path-card {
    display: flex;
    flex-direction: column;
    gap: 6px;
    min-width: 0;
    padding: 10px 12px;
    border: 1px solid #313244;
    border-radius: 10px;
    background: rgba(17, 17, 27, 0.76);
  }
  .path-card-editable {
    margin: 0;
  }
  .path-label {
    color: #7f849c;
    font-size: 10px;
    font-weight: 800;
    letter-spacing: 0.08em;
    text-transform: uppercase;
  }
  .path-value {
    color: #bac2de;
    font-size: 12px;
    font-family: 'JetBrains Mono', 'Fira Mono', monospace;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .cwd-row {
    display: flex;
    gap: 8px;
    min-width: 0;
  }
  .cwd-input {
    flex: 1;
    min-width: 0;
    border: 1px solid #313244;
    border-radius: 8px;
    background: #0b0b12;
    color: #cdd6f4;
    font: inherit;
    font-family: 'JetBrains Mono', 'Fira Mono', monospace;
    font-size: 12px;
    padding: 8px 10px;
    outline: none;
  }
  .cwd-input:focus {
    border-color: #89b4fa;
    box-shadow: 0 0 0 1px rgba(137, 180, 250, 0.24);
  }
  .path-btn {
    border: 1px solid #313244;
    border-radius: 8px;
    background: #181825;
    color: #cdd6f4;
    font: inherit;
    font-size: 11px;
    font-weight: 700;
    padding: 0 12px;
    cursor: pointer;
    transition: background 0.12s, border-color 0.12s, color 0.12s, opacity 0.12s;
  }
  .path-btn:hover:not(:disabled) {
    background: #1e1e2e;
    border-color: #45475a;
  }
  .path-btn:disabled {
    opacity: 0.4;
    cursor: not-allowed;
  }
  .path-btn-primary {
    background: rgba(137, 180, 250, 0.12);
    color: #89b4fa;
    border-color: rgba(137, 180, 250, 0.3);
  }
  .path-btn-primary:hover:not(:disabled) {
    background: rgba(137, 180, 250, 0.2);
    border-color: #89b4fa;
  }
  .term-hint {
    color: #6c7086;
    font-size: 11px;
    line-height: 1.5;
  }
  .term-error {
    color: #f38ba8;
    font-size: 11px;
    line-height: 1.5;
  }
  .term-body {
    flex: 1;
    min-height: 0;
    overflow: hidden;
    padding: 4px;
  }
  @media (max-width: 980px) {
    .term-header-main {
      flex-direction: column;
    }
    .term-paths {
      grid-template-columns: 1fr;
      width: 100%;
    }
    .cwd-row {
      flex-wrap: wrap;
    }
    .path-btn {
      height: 34px;
    }
  }
</style>
