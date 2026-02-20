<script lang="ts">
  import Editor from '../components/Editor.svelte';
  import Output from '../components/Output.svelte';
  import FileExplorer from '../components/FileExplorer.svelte';
  import GuiPreview from '../components/GuiPreview.svelte';
  import { runCode, runCodeWithModules, initGuiApp, initGuiAppWithModules, handleGuiEvent, setRenderCallback, type RunStatus } from '../wasm/vo.ts';
  import guiTetris from '../assets/examples/gui_tetris.vo?raw';
  import resvgDemo from '../assets/examples/resvg_demo.vo?raw';

  let code = $state(guiTetris);

  let stdout = $state('');
  let stderr = $state('');
  let status: RunStatus = $state('idle');
  let currentFile = $state('gui_tetris.vo');
  let guiMode = $state(false);
  let pngDataUrl = $state<string | null>(null);
  let renderData: { type: 'render' | 'patch'; tree?: any; patches?: any[] } | null = $state(null);
  let consoleCollapsed = $state(false);
  let guiFullscreen = $state(false);
  let showTouchControls = $state(true);

  type Panel = 'gui' | 'editor' | 'console' | 'files';
  let activePanel = $state<Panel>('gui');
  let guiPanelWidth = $state(600);
  let isResizing = $state(false);

  // Register render callback for async updates (timers)
  setRenderCallback((json: string) => {
    try {
      const parsed = JSON.parse(json);
      renderData = { type: 'render', tree: parsed.tree };
    } catch (e) {
      console.error('Failed to parse render JSON from timer:', e);
    }
  });

  async function handleRun() {
    status = 'running';
    stdout = '';
    stderr = '';
    renderData = null;
    guiMode = false;
    pngDataUrl = null;
    consoleCollapsed = false;
    activePanel = 'console';

    // Detect GUI code by checking for import "vogui"
    const isGuiCode = code.includes('import "vogui"');
    // Detect third-party module imports (github.com/...)
    const hasModuleImports = /import\s+"github\.com\//.test(code);

    try {
      if (isGuiCode && hasModuleImports) {
        // GUI app that also imports third-party modules (e.g. resvg demo)
        const result = await initGuiAppWithModules(code);
        if (result.status !== 'ok') {
          stderr = result.error || 'Unknown error';
          status = 'error';
          activePanel = 'console';
          return;
        }
        guiMode = true;
        consoleCollapsed = true;
        if (result.renderJson) {
          const parsed = JSON.parse(result.renderJson);
          renderData = { type: 'render', tree: parsed.tree };
        }
        status = 'success';
        activePanel = 'gui';
      } else if (isGuiCode) {
        // Use initGuiApp for GUI code
        const result = await initGuiApp(code);
        if (result.status !== 'ok') {
          stderr = result.error || 'Unknown error';
          status = 'error';
          activePanel = 'console';
          return;
        }
        guiMode = true;
        consoleCollapsed = true;
        if (result.renderJson) {
          const parsed = JSON.parse(result.renderJson);
          renderData = { type: 'render', tree: parsed.tree };
        }
        status = 'success';
        activePanel = 'gui';
      } else if (hasModuleImports) {
        // Code with third-party module imports: fetch modules from GitHub first
        const result = await runCodeWithModules(code);
        let output = result.stdout;
        if (output.startsWith('__PNG__')) {
          pngDataUrl = 'data:image/png;base64,' + output.slice(7).trim();
          stdout = '';
          activePanel = 'gui';
        } else {
          stdout = output;
          activePanel = 'console';
        }
        stderr = result.stderr;
        status = result.status === 'ok' ? 'success' : 'error';
      } else {
        // Regular code execution
        const result = await runCode(code);
        let output = result.stdout;
        
        // Check for VoGUI output (legacy path)
        if (output.startsWith('__VOGUI__')) {
          guiMode = true;
          const jsonStr = output.slice(9).trim();
          try {
            const parsed = JSON.parse(jsonStr);
            renderData = { type: 'render', tree: parsed.tree };
            stdout = '';
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
        activePanel = 'console';
      }
    } catch (e) {
      stderr = e instanceof Error ? e.message : String(e);
      status = 'error';
      activePanel = 'console';
    }
  }

  function startResize(e: MouseEvent) {
    e.preventDefault();
    isResizing = true;
    const startX = e.clientX;
    const startWidth = guiPanelWidth;
    
    function onMouseMove(e: MouseEvent) {
      const delta = startX - e.clientX;
      guiPanelWidth = Math.max(300, Math.min(1200, startWidth + delta));
    }
    
    function onMouseUp() {
      isResizing = false;
      document.removeEventListener('mousemove', onMouseMove);
      document.removeEventListener('mouseup', onMouseUp);
    }
    
    document.addEventListener('mousemove', onMouseMove);
    document.addEventListener('mouseup', onMouseUp);
  }

  async function onGuiEvent(handlerId: number, payload: string) {
    try {
      const result = await handleGuiEvent(handlerId, payload);
      if (result.status !== 'ok') {
        stderr = result.error;
        return;
      }
      if (result.renderJson) {
        const parsed = JSON.parse(result.renderJson);
        renderData = { type: 'render', tree: parsed.tree };
      }
    } catch (e) {
      stderr = e instanceof Error ? e.message : String(e);
    }
  }

  function handleReset() {
    stdout = '';
    stderr = '';
    status = 'idle';
    guiMode = false;
    pngDataUrl = null;
    renderData = null;
    activePanel = 'editor';
  }

  function handleFileSelect(content: string, filename: string) {
    code = content;
    currentFile = filename;
    handleReset();
    activePanel = 'editor';
  }

  async function sendGuiKey(key: string) {
    await onGuiEvent(-2, JSON.stringify({ key }));
  }

  // Touch control key mappings (gamepad-style)
  const TOUCH_KEYS = {
    up: 'ArrowUp',
    down: 'ArrowDown',
    left: 'ArrowLeft',
    right: 'ArrowRight',
    a: ' ',        // Space - primary action (jump/confirm/drop)
    b: 'Enter',    // Enter - secondary action (start/select)
    pause: 'Escape'
  };
</script>

<div class="playground" data-active={activePanel} data-gui={guiMode ? 'true' : 'false'} data-fullscreen={guiFullscreen ? 'true' : 'false'}>
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
      {:else if pngDataUrl}
        <span class="mode-badge resvg">SVG → PNG</span>
      {/if}
      {#if currentFile}
        <span class="current-file">{currentFile}</span>
      {/if}
    </div>
  </div>

  <div class="mobile-tabs">
    <button class="tab-btn" class:active={activePanel === 'gui'} onclick={() => (activePanel = 'gui')}>
      GUI
    </button>
    <button class="tab-btn" class:active={activePanel === 'editor'} onclick={() => (activePanel = 'editor')}>
      Editor
    </button>
    <button class="tab-btn" class:active={activePanel === 'console'} onclick={() => (activePanel = 'console')}>
      Console
    </button>
    <button class="tab-btn" class:active={activePanel === 'files'} onclick={() => (activePanel = 'files')}>
      Files
    </button>
  </div>

  <div class="main-area">
    <div class="editor-row">
      <div class="sidebar panel panel-files">
        <FileExplorer onSelect={handleFileSelect} bind:selectedFile={currentFile} />
      </div>
      <div class="editor-panel panel panel-editor">
        <Editor bind:value={code} />
      </div>
      <div 
        class="resizer" 
        class:hidden={!guiMode && !pngDataUrl}
        onmousedown={startResize}
      ></div>
      <div class="gui-panel panel panel-gui" class:inactive={!guiMode && !pngDataUrl} style:width="{guiPanelWidth}px">
        {#if pngDataUrl}
          <div class="png-output">
            <img src={pngDataUrl} alt="SVG rendered to PNG" />
            <p class="png-caption">Rendered via <code>github.com/vo-lang/resvg</code></p>
          </div>
        {:else}
          <GuiPreview {renderData} interactive={guiMode} onEvent={guiMode ? onGuiEvent : undefined} />
        {/if}
        {#if guiMode}
          <div class="gui-mobile-toolbar">
            <button 
              class="gui-toggle-btn" 
              class:active={guiFullscreen}
              onclick={() => guiFullscreen = !guiFullscreen}
              title={guiFullscreen ? 'Exit Fullscreen' : 'Fullscreen'}
            >
              {guiFullscreen ? '⛶' : '⛶'}
            </button>
            <button 
              class="gui-toggle-btn" 
              class:active={showTouchControls}
              onclick={() => showTouchControls = !showTouchControls}
              title={showTouchControls ? 'Hide Controls' : 'Show Controls'}
            >
              🎮
            </button>
          </div>
        {/if}
      </div>
    </div>
    <div class="console-panel panel panel-console" class:collapsed={consoleCollapsed && guiMode}>
      <Output {stdout} {stderr} {status} collapsible={guiMode} bind:collapsed={consoleCollapsed} />
    </div>
  </div>

  {#if guiMode && showTouchControls}
    <div class="touch-controls">
      <!-- D-Pad (left side) -->
      <div class="dpad">
        <button class="dpad-btn up" onpointerdown={(e) => { e.preventDefault(); sendGuiKey(TOUCH_KEYS.up); }}>▲</button>
        <button class="dpad-btn left" onpointerdown={(e) => { e.preventDefault(); sendGuiKey(TOUCH_KEYS.left); }}>◀</button>
        <button class="dpad-btn right" onpointerdown={(e) => { e.preventDefault(); sendGuiKey(TOUCH_KEYS.right); }}>▶</button>
        <button class="dpad-btn down" onpointerdown={(e) => { e.preventDefault(); sendGuiKey(TOUCH_KEYS.down); }}>▼</button>
      </div>
      <!-- Action buttons (right side) -->
      <div class="action-btns">
        <button class="action-btn pause" onpointerdown={(e) => { e.preventDefault(); sendGuiKey(TOUCH_KEYS.pause); }}>⏸</button>
        <div class="ab-row">
          <button class="action-btn b" onpointerdown={(e) => { e.preventDefault(); sendGuiKey(TOUCH_KEYS.b); }}>B</button>
          <button class="action-btn a" onpointerdown={(e) => { e.preventDefault(); sendGuiKey(TOUCH_KEYS.a); }}>A</button>
        </div>
      </div>
    </div>
  {/if}
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

  .mode-badge.resvg {
    background: #e94560;
    color: white;
  }

  .png-output {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    height: 100%;
    padding: 24px;
    gap: 16px;
    background: var(--bg-primary);
  }

  .png-output img {
    max-width: 100%;
    max-height: calc(100% - 48px);
    border-radius: 8px;
    box-shadow: 0 4px 24px rgba(0, 0, 0, 0.4);
    image-rendering: pixelated;
  }

  .png-caption {
    font-family: var(--font-mono);
    font-size: 11px;
    color: var(--text-secondary);
    margin: 0;
  }

  .png-caption code {
    color: #e94560;
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
    min-height: 0;
  }

  .editor-row {
    display: flex;
    flex: 1;
    overflow: hidden;
    min-height: 0;
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

  .resizer {
    width: 6px;
    cursor: col-resize;
    background: transparent;
    transition: background 0.15s;
    flex-shrink: 0;
  }

  .resizer:hover,
  .resizer:active {
    background: var(--accent);
  }

  .resizer.hidden {
    display: none;
  }

  .gui-panel {
    min-width: 300px;
    max-width: 1200px;
    overflow: hidden;
    border-left: 1px solid var(--border);
    flex-shrink: 0;
  }

  .gui-panel.inactive {
    display: none;
  }

  .gui-mobile-toolbar {
    display: none;
    position: absolute;
    top: 8px;
    right: 8px;
    gap: 8px;
    z-index: 10;
  }

  .gui-toggle-btn {
    width: 36px;
    height: 36px;
    min-height: unset;
    border-radius: 8px;
    background: var(--bg-secondary);
    border: 1px solid var(--border);
    color: var(--text-secondary);
    font-size: 16px;
    display: flex;
    align-items: center;
    justify-content: center;
    cursor: pointer;
    transition: all 0.15s ease;
  }

  .gui-toggle-btn:hover {
    background: var(--bg-tertiary);
  }

  .gui-toggle-btn.active {
    background: var(--accent);
    color: white;
    border-color: var(--accent);
  }

  .console-panel {
    border-top: 1px solid var(--border);
    height: 200px;
    flex-shrink: 0;
    overflow: hidden;
    transition: height 0.2s ease;
  }

  .console-panel.collapsed {
    height: 40px;
  }

  .mobile-tabs {
    display: none;
    padding: 8px var(--page-gutter);
    gap: 8px;
    background: var(--bg-secondary);
    border-bottom: 1px solid var(--border);
  }

  .tab-btn {
    flex: 1;
    padding: 10px 12px;
    background: var(--bg-tertiary);
    border: 1px solid var(--border);
    border-radius: 10px;
    font-size: 13px;
    font-weight: 600;
    color: var(--text-secondary);
  }

  .tab-btn.active {
    background: var(--accent-light);
    color: var(--accent);
    border-color: var(--accent);
  }

  .touch-controls {
    display: none;
    position: fixed;
    left: 0;
    right: 0;
    bottom: 0;
    padding: 16px var(--page-gutter) 24px;
    justify-content: space-between;
    align-items: flex-end;
    pointer-events: none;
  }

  /* D-Pad (left side) - classic cross layout */
  .dpad {
    display: grid;
    grid-template-areas:
      ".    up   ."
      "left .    right"
      ".    down .";
    grid-template-columns: repeat(3, 48px);
    grid-template-rows: repeat(3, 48px);
    gap: 4px;
    pointer-events: auto;
  }

  .dpad-btn {
    width: 48px;
    height: 48px;
    min-height: unset;
    background: rgba(0, 0, 0, 0.4);
    color: rgba(255, 255, 255, 0.9);
    border: 1px solid rgba(255, 255, 255, 0.2);
    border-radius: 12px;
    font-size: 16px;
    font-weight: 700;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .dpad-btn.up { grid-area: up; }
  .dpad-btn.down { grid-area: down; }
  .dpad-btn.left { grid-area: left; }
  .dpad-btn.right { grid-area: right; }

  .dpad-btn:active {
    background: rgba(255, 255, 255, 0.3);
  }

  /* Action buttons (right side) */
  .action-btns {
    display: flex;
    flex-direction: column;
    align-items: flex-end;
    gap: 12px;
    pointer-events: auto;
  }

  .ab-row {
    display: flex;
    gap: 12px;
  }

  .action-btn {
    width: 56px;
    height: 56px;
    min-height: unset;
    border-radius: 50%;
    font-size: 16px;
    font-weight: 700;
    display: flex;
    align-items: center;
    justify-content: center;
    border: 2px solid rgba(255, 255, 255, 0.3);
  }

  .action-btn.a {
    background: rgba(46, 204, 113, 0.5);
    color: rgba(255, 255, 255, 0.9);
  }

  .action-btn.b {
    background: rgba(231, 76, 60, 0.5);
    color: rgba(255, 255, 255, 0.9);
  }

  .action-btn.pause {
    width: 40px;
    height: 40px;
    border-radius: 50%;
    background: rgba(0, 0, 0, 0.4);
    color: rgba(255, 255, 255, 0.9);
    font-size: 14px;
  }

  .action-btn:active {
    transform: scale(0.92);
    opacity: 0.8;
  }

  @media (max-width: 1200px) {
    .sidebar {
      width: 220px;
    }
    .gui-panel {
      width: 500px;
    }
  }

  @media (max-width: 900px) {

    .editor-row {
      flex-direction: column;
    }

    .sidebar {
      width: 100%;
      min-width: 0;
    }

    .editor-panel {
      border-right: none;
      min-width: 0;
    }

    .gui-panel {
      width: 100%;
      flex: 1;
      min-height: 280px;
      min-width: 0;
      border-left: none;
      border-top: 1px solid var(--border);
    }

    .toolbar {
      flex-direction: column;
      align-items: stretch;
      gap: 12px;
      padding: 12px var(--page-gutter);
    }

    .toolbar-left {
      flex-wrap: wrap;
      gap: 10px;
    }

    .actions {
      width: 100%;
    }

    .actions button {
      flex: 1;
    }

    .mobile-tabs {
      display: flex;
    }

    .panel {
      display: none;
      height: 100%;
    }

    .playground[data-active='files'] .panel-files {
      display: flex;
    }

    .playground[data-active='editor'] .panel-editor {
      display: flex;
    }

    .playground[data-active='gui'] .panel-gui {
      display: flex;
    }

    .playground[data-active='console'] .panel-console {
      display: flex;
    }

    .console-panel {
      height: 100%;
      border-top: none;
    }

    .touch-controls {
      display: flex;
    }

    .gui-panel {
      position: relative;
    }

    .gui-mobile-toolbar {
      display: flex;
    }

    /* Fullscreen mode for GUI panel on mobile */
    .playground[data-fullscreen='true'][data-active='gui'] .toolbar,
    .playground[data-fullscreen='true'][data-active='gui'] .mobile-tabs {
      display: none;
    }

    .playground[data-fullscreen='true'][data-active='gui'] .main-area {
      height: 100%;
    }

    .playground[data-fullscreen='true'][data-active='gui'] .panel-gui {
      position: fixed;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      z-index: 100;
      border: none;
    }

    .playground[data-fullscreen='true'] .gui-mobile-toolbar {
      top: 16px;
      right: 16px;
    }
  }
</style>
