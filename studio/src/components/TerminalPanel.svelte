<script lang="ts">
  import { onDestroy, onMount } from 'svelte';
  import { Terminal } from '@xterm/xterm';
  import { FitAddon } from '@xterm/addon-fit';
  import { WebLinksAddon } from '@xterm/addon-web-links';
  import type { TerminalService } from '../lib/services/terminal_service';

  export let terminal: TerminalService;
  export let afterExecute: (() => void) | undefined = undefined;

  let containerEl: HTMLDivElement;
  let xterm: Terminal | null = null;
  let fitAddon: FitAddon | null = null;
  let inputBuffer = '';
  let historyIndex = -1;
  let historySnapshot: string[] = [];
  let resizeObserver: ResizeObserver | null = null;
  let unsubscribe: (() => void) | null = null;
  let lastLineCount = 0;

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
    const s = terminal.snapshot();
    xterm.write('\r\n' + prompt(s.cwd));
  }

  function printLines(lines: import('../lib/services/terminal_service').TerminalLine[]): void {
    if (!xterm || lines.length <= lastLineCount) return;
    const newLines = lines.slice(lastLineCount);
    for (const line of newLines) {
      const colored =
        line.kind === 'input'  ? `${BOLD}${CYAN}${line.text}${RESET}` :
        line.kind === 'error'  ? `${RED}${line.text}${RESET}` :
        line.kind === 'output' ? line.text :
        `${DIM}${line.text}${RESET}`;
      xterm.writeln('\r' + colored);
    }
    lastLineCount = lines.length;
    // Re-draw prompt after output (only if last line was output, not input)
    const last = newLines[newLines.length - 1];
    if (last && last.kind !== 'input') {
      const s = terminal.snapshot();
      xterm.write(prompt(s.cwd));
    }
  }

  onMount(() => {
    xterm = new Terminal({
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
    xterm.writeln(`${BOLD}${CYAN}Vo Studio Terminal${RESET}`);
    xterm.writeln(`${DIM}Type 'help' for available commands.${RESET}`);
    const s0 = terminal.snapshot();
    xterm.write(prompt(s0.cwd));

    // Subscribe to service state to render new output lines
    unsubscribe = terminal.state.subscribe((state) => {
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
        lastLineCount = terminal.snapshot().lines.length;
        void terminal.execute(cmd).then(() => {
          afterExecute?.();
          writePrompt();
          lastLineCount = terminal.snapshot().lines.length;
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
        const hist = terminal.snapshot().history;
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
        const s = terminal.snapshot();
        xterm!.write(prompt(s.cwd));
        return;
      }

      // Ctrl+L — clear screen
      if (code === 'l' && domEvent.ctrlKey) {
        terminal.clear();
        lastLineCount = 0;
        xterm!.clear();
        const s = terminal.snapshot();
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
    const s = terminal.snapshot();
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

<div class="terminal-root" bind:this={containerEl}></div>

<style>
  :global(.xterm) { height: 100%; }
  :global(.xterm-viewport) { overflow-y: auto !important; }

  .terminal-root {
    width: 100%;
    height: 100%;
    overflow: hidden;
    background: #11111b;
    padding: 4px;
    box-sizing: border-box;
  }
</style>
