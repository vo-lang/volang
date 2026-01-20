<script lang="ts">
  import Editor from '../components/Editor.svelte';
  import Output from '../components/Output.svelte';
  import Examples from '../components/Examples.svelte';
  import FileExplorer from '../components/FileExplorer.svelte';
  import GuiPreview from '../components/GuiPreview.svelte';
  import { runCode, type RunStatus } from '../wasm/vo.ts';

  let code = $state(`package main

import "errors"

// Vo: Go-inspired with elegant error handling
// Features: goroutine, channel, ? operator, fail

func main() {
    // Concurrent computation with channels
    ch := make(chan int, 3)
    
    go compute(ch, 10)
    go compute(ch, 20)
    go compute(ch, 30)
    
    sum := 0
    for i := 0; i < 3; i++ {
        sum += <-ch
    }
    println("Sum of squares:", sum)

    // Elegant error handling
    println("\\nError handling demo:")
    err := pipeline()
    if err != nil {
        println("Pipeline:", err)
    }
}

func compute(ch chan int, n int) {
    ch <- n * n  // Send square to channel
}

func pipeline() error {
    step1()?     // ? propagates error automatically
    step2()?
    return nil
}

func step1() error {
    println("  Step 1: OK")
    return nil
}

func step2() error {
    fail errors.New("step2 failed")  // Concise error return
}
`);

  let stdout = $state('');
  let stderr = $state('');
  let status: RunStatus = $state('idle');
  let currentFile = $state('');
  let guiMode = $state(false);
  let nodeTree: any = $state(null);
  let consoleCollapsed = $state(false);

  async function handleRun() {
    status = 'running';
    stdout = '';
    stderr = '';
    nodeTree = null;
    guiMode = false;

    try {
      const result = await runCode(code);
      let output = result.stdout;
      
      // Check for VoGUI output
      if (output.startsWith('__VOGUI__')) {
        guiMode = true;
        const jsonStr = output.slice(9).trim();
        try {
          const parsed = JSON.parse(jsonStr);
          nodeTree = parsed.tree;
          stdout = '';  // Clear stdout for GUI mode
        } catch (parseErr) {
          stderr = 'Failed to parse GUI output: ' + parseErr;
          status = 'error';
          return;
        }
      } else {
        stdout = output;
      }
      
      stderr = result.stderr;
      status = result.status === 'ok' ? 'success' : 'error';
    } catch (e) {
      stderr = e instanceof Error ? e.message : String(e);
      status = 'error';
    }
  }

  function handleReset() {
    stdout = '';
    stderr = '';
    status = 'idle';
    guiMode = false;
    nodeTree = null;
  }

  function handleExampleSelect(example: { code: string }) {
    code = example.code;
    currentFile = '';
    handleReset();
  }

  function handleFileSelect(content: string, filename: string) {
    code = content;
    currentFile = filename;
    handleReset();
  }
</script>

<div class="playground">
  <div class="toolbar">
    <div class="toolbar-left">
      <div class="actions">
        <button class="btn-primary" onclick={handleRun} disabled={status === 'running'}>
          {status === 'running' ? 'Running...' : 'Run'}
        </button>
        <button class="btn-secondary" onclick={handleReset}>
          Reset
        </button>
      </div>
      {#if guiMode}
        <span class="mode-badge gui">GUI Mode</span>
      {/if}
      {#if currentFile}
        <span class="current-file">{currentFile}</span>
      {/if}
    </div>
    <Examples onSelect={handleExampleSelect} />
  </div>

  <div class="main-area">
    <div class="editor-row">
      <div class="sidebar">
        <FileExplorer onSelect={handleFileSelect} />
      </div>
      <div class="editor-panel">
        <Editor bind:value={code} />
      </div>
      {#if guiMode}
        <div class="gui-panel">
          <GuiPreview {nodeTree} />
        </div>
      {/if}
    </div>
    <div class="console-panel" class:collapsed={consoleCollapsed && guiMode}>
      <Output {stdout} {stderr} {status} collapsible={guiMode} bind:collapsed={consoleCollapsed} />
    </div>
  </div>
</div>

<style>
  .playground {
    display: flex;
    flex-direction: column;
    height: calc(100vh - var(--header-height));
  }

  .toolbar {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 20px;
    padding: 10px 20px;
    background: var(--bg-secondary);
    border-bottom: 1px solid var(--border);
    flex-shrink: 0;
  }

  .toolbar-left {
    display: flex;
    align-items: center;
    gap: 16px;
  }

  .actions {
    display: flex;
    gap: 8px;
  }

  .mode-badge {
    font-family: var(--font-mono);
    font-size: 11px;
    padding: 4px 10px;
    border-radius: 4px;
    font-weight: 600;
  }

  .mode-badge.gui {
    background: var(--accent);
    color: white;
  }

  .current-file {
    font-family: var(--font-mono);
    font-size: 12px;
    color: var(--text-secondary);
    padding: 4px 10px;
    background: var(--bg-tertiary);
    border-radius: 4px;
  }

  .main-area {
    display: flex;
    flex-direction: column;
    flex: 1;
    overflow: hidden;
  }

  .editor-row {
    display: flex;
    flex: 1;
    overflow: hidden;
  }

  .sidebar {
    width: 280px;
    min-width: 200px;
    flex-shrink: 0;
    overflow: hidden;
  }

  .editor-panel {
    flex: 1;
    border-right: 1px solid var(--border);
    overflow: hidden;
    min-width: 300px;
  }

  .gui-panel {
    width: 400px;
    min-width: 300px;
    overflow: hidden;
    border-left: 1px solid var(--border);
  }

  .console-panel {
    border-top: 1px solid var(--border);
    height: 200px;
    flex-shrink: 0;
    overflow: hidden;
  }

  .console-panel.collapsed {
    height: 40px;
  }

  @media (max-width: 1200px) {
    .sidebar {
      width: 220px;
    }
    .gui-panel {
      width: 320px;
    }
  }

  @media (max-width: 900px) {
    .editor-row {
      flex-direction: column;
    }
    .sidebar {
      display: none;
    }
    .editor-panel {
      border-right: none;
      border-bottom: 1px solid var(--border);
      flex: 1;
    }
    .gui-panel {
      width: 100%;
      height: 200px;
      border-left: none;
    }
    .toolbar {
      flex-direction: column;
      align-items: flex-start;
      gap: 10px;
    }
  }
</style>
