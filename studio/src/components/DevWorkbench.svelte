<script lang="ts">
  import { onDestroy, onMount, tick } from 'svelte';
  import type { ComponentContainer, GoldenLayout as GoldenLayoutApi, LayoutConfig } from 'golden-layout';
  import type { SvelteComponent } from 'svelte';
  import type { ServiceRegistry } from '../lib/services/service_registry';
  import type { FsEntry } from '../lib/types';
  import Toolbar from './Toolbar.svelte';
  import FileTree from './FileTree.svelte';
  import Editor from './Editor.svelte';
  import Console from './Console.svelte';
  import PreviewPanel from './PreviewPanel.svelte';
  import { ide } from '../stores/ide';

  interface FileTreeProps {
    entries: FsEntry[];
    currentDir: string;
    sessionRoot: string;
    onOpenEntry: (entry: FsEntry) => void;
    onGoParent: () => void;
  }

  interface ConsoleProps {
    mode: 'pane';
  }

  type GoldenLayoutModule = typeof import('golden-layout');
  type PaneType = 'explorer' | 'editor' | 'console';
  type PaneComponentProps = FileTreeProps | Record<string, never> | ConsoleProps;
  type PaneComponentInstance = SvelteComponent;
  type PaneComponentConstructor<Props> = new (options: { target: HTMLElement; props: Props }) => PaneComponentInstance;

  interface BoundPane {
    container: ComponentContainer;
    host: HTMLDivElement;
    instance: PaneComponentInstance;
    type: PaneType;
  }

  const FileTreeCtor = FileTree as unknown as PaneComponentConstructor<FileTreeProps>;
  const EditorCtor = Editor as unknown as PaneComponentConstructor<Record<string, never>>;
  const ConsoleCtor = Console as unknown as PaneComponentConstructor<ConsoleProps>;

  export let registry: ServiceRegistry | null = null;
  export let explorerEntries: FsEntry[] = [];
  export let currentDir = '';
  export let sessionRoot = '';
  export let showExplorer = false;
  export let isSingleFileSession = false;
  export let isGuiProject = false;
  export let projectHasGui = false;
  export let previewCollapsed = false;
  export let outputExpanded = false;
  export let previewTitle = '';
  export let onSave: () => void = () => {};
  export let onShare: () => void = () => {};
  export let onRun: () => void = () => {};
  export let onRunFullscreen: () => void = () => {};
  export let onStop: () => void = () => {};
  export let onSetProjectHasGui: (hasGui: boolean) => void = () => {};
  export let onOpenEntry: (entry: FsEntry) => void = () => {};
  export let onGoParent: () => void = () => {};
  export let onExitFullscreen: () => void = () => {};
  export let onTogglePreviewCollapsed: () => void = () => {};

  let workbenchHost: HTMLDivElement | undefined;
  let layout: GoldenLayoutApi | null = null;
  let resizeObserver: ResizeObserver | null = null;
  let layoutShapeKey = '';
  let layoutInitGeneration = 0;
  let goldenLayoutModulePromise: Promise<GoldenLayoutModule> | null = null;
  const boundPanes = new Map<ComponentContainer, BoundPane>();

  async function loadGoldenLayoutModule(): Promise<GoldenLayoutModule> {
    if (!goldenLayoutModulePromise) {
      goldenLayoutModulePromise = import('golden-layout');
    }
    return goldenLayoutModulePromise;
  }

  function createPaneHost(container: ComponentContainer): HTMLDivElement {
    const host = document.createElement('div');
    host.className = 'workbench-pane-host';
    container.element.appendChild(host);
    return host;
  }

  function paneProps(type: PaneType): PaneComponentProps {
    switch (type) {
      case 'explorer':
        return {
          entries: explorerEntries,
          currentDir,
          sessionRoot,
          onOpenEntry,
          onGoParent,
        } satisfies FileTreeProps;
      case 'console':
        return { mode: 'pane' } satisfies ConsoleProps;
      case 'editor':
        return {};
    }
  }

  function paneConstructor(type: PaneType): PaneComponentConstructor<PaneComponentProps> {
    switch (type) {
      case 'explorer':
        return FileTreeCtor as PaneComponentConstructor<PaneComponentProps>;
      case 'console':
        return ConsoleCtor as PaneComponentConstructor<PaneComponentProps>;
      case 'editor':
        return EditorCtor as PaneComponentConstructor<PaneComponentProps>;
    }
  }

  function mountPane(type: PaneType, container: ComponentContainer): void {
    const host = createPaneHost(container);
    const Ctor = paneConstructor(type);
    const instance = new Ctor({
      target: host,
      props: paneProps(type),
    });
    const bound: BoundPane = { container, host, instance, type };
    boundPanes.set(container, bound);
    container.on('beforeComponentRelease', () => {
      boundPanes.delete(container);
      instance.$destroy();
      host.remove();
    });
  }

  function updateBoundPanes(): void {
    for (const bound of boundPanes.values()) {
      bound.instance.$set(paneProps(bound.type));
    }
  }

  function layoutConfig(): LayoutConfig {
    const centerColumn = {
      type: 'column' as const,
      content: [
        {
          type: 'component' as const,
          componentType: 'editor',
          id: 'editor',
          isClosable: false,
          size: '72%',
          minSize: '180px',
        },
        {
          type: 'component' as const,
          componentType: 'console',
          id: 'console',
          isClosable: false,
          size: '28%',
          minSize: '120px',
        },
      ],
    };

    if (showExplorer) {
      return {
        root: {
          type: 'row',
          content: [
            {
              type: 'component',
              componentType: 'explorer',
              id: 'explorer',
              isClosable: false,
              size: '24%',
              minSize: '180px',
            },
            {
              ...centerColumn,
              size: '76%',
            },
          ],
        },
        settings: {
          constrainDragToContainer: true,
          reorderEnabled: false,
        },
        dimensions: {
          borderWidth: 5,
          borderGrabWidth: 10,
          defaultMinItemHeight: '96px',
          defaultMinItemWidth: '120px',
        },
        header: {
          show: false,
          popout: false,
          close: false,
          maximise: false,
          tabDropdown: false,
        },
      };
    }

    return {
      root: centerColumn,
      settings: {
        constrainDragToContainer: true,
        reorderEnabled: false,
      },
      dimensions: {
        borderWidth: 5,
        borderGrabWidth: 10,
        defaultMinItemHeight: '96px',
        defaultMinItemWidth: '120px',
      },
      header: {
        show: false,
        popout: false,
        close: false,
        maximise: false,
        tabDropdown: false,
      },
    };
  }

  function syncLayoutSize(): void {
    if (!layout || !workbenchHost) {
      return;
    }
    layout.setSize(Math.round(workbenchHost.clientWidth), Math.round(workbenchHost.clientHeight));
  }

  function ensureLayoutStructure(): void {
    if (!layout) {
      return;
    }
    const nextShapeKey = showExplorer ? 'module' : 'single-file';
    if (layoutShapeKey === nextShapeKey) {
      return;
    }
    layoutShapeKey = nextShapeKey;
    layout.loadLayout(layoutConfig());
    void tick().then(() => syncLayoutSize());
  }

  async function ensureLayout(): Promise<void> {
    if (!workbenchHost || layout) {
      return;
    }
    const generation = ++layoutInitGeneration;
    const { GoldenLayout } = await loadGoldenLayoutModule();
    if (generation !== layoutInitGeneration || !workbenchHost) {
      return;
    }
    layout = new GoldenLayout(workbenchHost);
    layout.registerComponentFactoryFunction('explorer', (container) => {
      mountPane('explorer', container);
    });
    layout.registerComponentFactoryFunction('editor', (container) => {
      mountPane('editor', container);
    });
    layout.registerComponentFactoryFunction('console', (container) => {
      mountPane('console', container);
    });
    ensureLayoutStructure();
    resizeObserver = new ResizeObserver(() => {
      syncLayoutSize();
    });
    resizeObserver.observe(workbenchHost);
    syncLayoutSize();
  }

  onMount(() => {
    void ensureLayout();
  });

  onDestroy(() => {
    layoutInitGeneration++;
    resizeObserver?.disconnect();
    resizeObserver = null;
    for (const bound of boundPanes.values()) {
      bound.instance.$destroy();
      bound.host.remove();
    }
    boundPanes.clear();
    layout?.destroy();
    layout = null;
  });

  $: if (layout) {
    ensureLayoutStructure();
  }

  $: if (workbenchHost && !layout) {
    void ensureLayout();
  }

  $: updateBoundPanes();
</script>

<div class="dev-workbench" class:single-file={isSingleFileSession}>
  <Toolbar
    onSave={onSave}
    onShare={onShare}
    onRun={onRun}
    onRunFullscreen={onRunFullscreen}
    onStop={onStop}
    onSetProjectHasGui={onSetProjectHasGui}
    projectHasGui={projectHasGui}
  />

  <div class="dev-body">
    <div class="workbench-surface" class:with-preview={isGuiProject}>
      <div class="workbench-host" bind:this={workbenchHost}></div>
    </div>

    {#if isGuiProject && registry}
      <PreviewPanel
        {registry}
        collapsed={previewCollapsed}
        fullscreen={outputExpanded}
        fullscreenTitle={previewTitle}
        showFullscreenAction={true}
        onFullscreenAction={() => ide.update((s) => ({ ...s, outputExpanded: true, previewCollapsed: false }))}
        onExitFullscreenAction={onExitFullscreen}
        onToggleCollapsed={onTogglePreviewCollapsed}
      />
    {/if}
  </div>
</div>

<style>
  .dev-workbench {
    display: flex;
    flex: 1;
    flex-direction: column;
    min-height: 0;
    overflow: hidden;
  }
  .dev-workbench.single-file {
    background: linear-gradient(180deg, rgba(17, 17, 27, 0.98), rgba(24, 24, 37, 0.96));
  }
  .dev-body {
    display: flex;
    flex: 1;
    min-height: 0;
    overflow: hidden;
  }
  .workbench-surface {
    flex: 1;
    min-width: 0;
    min-height: 0;
    overflow: hidden;
  }
  .workbench-surface.with-preview {
    border-right: 1px solid #1e1e2e;
  }
  .workbench-host {
    width: 100%;
    height: 100%;
    min-width: 0;
    min-height: 0;
  }
  :global(.workbench-pane-host) {
    width: 100%;
    height: 100%;
    min-width: 0;
    min-height: 0;
    display: flex;
    overflow: hidden;
  }
  :global(.workbench-pane-host > *) {
    flex: 1;
    min-width: 0;
    min-height: 0;
  }
  :global(.dev-workbench .lm_root) {
    background: #11111b;
  }
  :global(.dev-workbench .lm_splitter) {
    background: #1e1e2e;
  }
  :global(.dev-workbench .lm_splitter:hover),
  :global(.dev-workbench .lm_splitter:active) {
    background: #313244;
  }
  :global(.dev-workbench .lm_content) {
    background: #11111b;
  }
</style>
