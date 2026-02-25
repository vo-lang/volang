/**
 * File Explorer ExternalWidget — VS Code-style sidebar.
 * Real folder browsing via File System Access API, in-memory fallback.
 */
import { registerWidget } from '@vogui/index';
import type { WidgetInstance } from '@vogui/types';

interface FsNode {
  name: string;
  path: string;
  isDir: boolean;
  expanded: boolean;
  children: FsNode[];
  handle?: FileSystemFileHandle | FileSystemDirectoryHandle;
  content?: string;
}
type CreateMode = '' | 'file' | 'dir';

const CSS = `
.fe{display:flex;flex-direction:column;height:100%;background:#1e1e1e;color:#ccc;
  font:13px/1.4 -apple-system,"Segoe UI",system-ui,sans-serif;user-select:none;
  overflow:hidden;border-right:1px solid #3c3c3c}
.fe-hdr{display:flex;align-items:center;gap:2px;padding:0 8px;height:35px;
  flex-shrink:0;border-bottom:1px solid #3c3c3c}
.fe-title{font-size:11px;font-weight:600;letter-spacing:.06em;text-transform:uppercase;
  color:#bbb;flex:1}
.fe-hbtn{display:flex;align-items:center;justify-content:center;width:22px;height:22px;
  border:none;background:transparent;color:#bbb;cursor:pointer;border-radius:4px;
  font-size:14px;padding:0}
.fe-hbtn:hover{background:#2a2d2e;color:#fff}
.fe-sec{display:flex;align-items:center;gap:4px;padding:4px 8px;flex-shrink:0;
  cursor:pointer;font-size:11px;font-weight:600;text-transform:uppercase;
  color:#bbb;letter-spacing:.06em}
.fe-sec:hover{background:#2a2d2e}
.fe-arr{font-size:10px;transition:transform .15s;display:inline-block}
.fe-arr.open{transform:rotate(0deg)}.fe-arr.closed{transform:rotate(-90deg)}
.fe-tree{flex:1;overflow-y:auto;overflow-x:hidden}
.fe-tree::-webkit-scrollbar{width:8px}
.fe-tree::-webkit-scrollbar-thumb{background:#424242;border-radius:4px}
.fe-tree::-webkit-scrollbar-thumb:hover{background:#555}
.fe-item{display:flex;align-items:center;height:22px;padding-right:4px;
  cursor:pointer;position:relative;white-space:nowrap;overflow:hidden}
.fe-item:hover{background:#2a2d2e}
.fe-item.active{background:#094771}
.fe-item.active .fe-lbl{color:#fff}
.fe-darr{width:16px;flex-shrink:0;font-size:10px;color:#888;text-align:center;
  transition:transform .1s}
.fe-darr.open{transform:rotate(90deg)}
.fe-ico{width:18px;flex-shrink:0;font-size:12px;text-align:center}
.fe-lbl{flex:1;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;
  min-width:0;color:#ccc;font-size:13px}
.fe-acts{display:none;align-items:center;gap:1px;margin-left:4px;flex-shrink:0}
.fe-item:hover .fe-acts,.fe-item.active .fe-acts{display:flex}
.fe-abtn{display:flex;align-items:center;justify-content:center;width:20px;height:20px;
  border:none;background:transparent;color:#858585;cursor:pointer;border-radius:3px;
  font-size:13px;padding:0}
.fe-abtn:hover{background:#3c3c3c;color:#ccc}
.fe-abtn.del:hover{color:#f48771}
.fe-ri{flex:1;min-width:0;height:18px;padding:0 4px;background:#3c3c3c;color:#ccc;
  border:1px solid #007acc;border-radius:2px;
  font:13px -apple-system,"Segoe UI",system-ui,sans-serif;outline:none}
.fe-cr{display:flex;align-items:center;height:28px;padding:0 6px;gap:4px;
  background:#252526;border-top:1px solid #3c3c3c;flex-shrink:0}
.fe-ci{flex-shrink:0;color:#858585;font-size:13px;width:18px;text-align:center}
.fe-cinp{flex:1;min-width:0;height:20px;padding:0 6px;background:#3c3c3c;color:#ccc;
  border:1px solid #007acc;border-radius:2px;
  font:13px -apple-system,"Segoe UI",system-ui,sans-serif;outline:none}
.fe-cinp::placeholder{color:#555}
.fe-cok{height:20px;padding:0 8px;background:#0e639c;color:#fff;border:none;
  border-radius:2px;font-size:12px;cursor:pointer}
.fe-cok:hover{background:#1177bb}
.fe-ccancel{height:20px;padding:0 6px;background:transparent;color:#858585;
  border:1px solid #555;border-radius:2px;font-size:12px;cursor:pointer}
.fe-ccancel:hover{background:#3c3c3c;color:#ccc}
`;

const EXT_ICONS: Record<string,string> = {
  vo:'◆',ts:'TS',js:'JS',jsx:'JS',tsx:'TS',json:'{}',md:'≡',
  html:'<>',css:'#',rs:'⚙',toml:'⚙',yaml:'⚙',yml:'⚙',
  sh:'$',py:'Py',go:'Go',txt:'≣',
};

const EXAMPLES: Array<{path:string;content:string}> = [
  {path:'hello.vo',content:`package main\n\nimport "fmt"\n\nfunc main() {\n\tfmt.Println("Hello, Vo!")\n}\n`},
  {path:'counter.vo',content:`package main\n\nimport (\n\t"fmt"\n\t"vogui"\n)\n\ntype Counter struct{ Count int }\n\nfunc main() {\n\tvogui.Run(vogui.App{\n\t\tInit: func() any { return &Counter{} },\n\t\tView: view,\n\t})\n}\n\nfunc view(state any) vogui.Node {\n\ts := state.(*Counter)\n\treturn vogui.Col(\n\t\tvogui.Text("Count: " + fmt.Sprint(s.Count)).Font(24).Bold(),\n\t\tvogui.Row(\n\t\t\tvogui.Button("−", vogui.On(decrement)).W(48),\n\t\t\tvogui.Button("+", vogui.On(increment)).W(48),\n\t\t).Gap(8),\n\t).P(24).Gap(16)\n}\n\nfunc increment(s any) { s.(*Counter).Count++ }\nfunc decrement(s any) { s.(*Counter).Count-- }\n`},
  {path:'fibonacci.vo',content:`package main\n\nimport "fmt"\n\nfunc fib(n int) int {\n\tif n <= 1 { return n }\n\treturn fib(n-1) + fib(n-2)\n}\n\nfunc main() {\n\tfor i := 0; i < 10; i++ {\n\t\tfmt.Printf("fib(%d) = %d\\n", i, fib(i))\n\t}\n}\n`},
];

function icon(name: string, isDir: boolean, expanded?: boolean): string {
  if (isDir) return expanded ? '📂' : '📁';
  const ext = name.split('.').pop()?.toLowerCase() ?? '';
  return EXT_ICONS[ext] ?? '≣';
}

function depth(path: string): number {
  return path.replace(/\/$/, '').split('/').length - 1;
}

function joinPath(parent: string, name: string): string {
  return parent ? parent + name : name;
}

function parentPath(path: string): string {
  return path.replace(/[^/]*\/?$/, '');
}

async function resolveDirHandle(
  root: FileSystemDirectoryHandle,
  relPath: string,
): Promise<FileSystemDirectoryHandle> {
  if (!relPath) return root;
  let cur: FileSystemDirectoryHandle = root;
  for (const part of relPath.replace(/\/$/, '').split('/')) {
    cur = await cur.getDirectoryHandle(part);
  }
  return cur;
}

async function readDirHandle(
  dirH: FileSystemDirectoryHandle,
  prefix: string,
  showHidden = false,
): Promise<FsNode[]> {
  const nodes: FsNode[] = [];
  for await (const [name, handle] of (dirH as any).entries()) {
    if (!showHidden && name.startsWith('.')) continue;
    const path = joinPath(prefix, name) + (handle.kind === 'directory' ? '/' : '');
    nodes.push({ name, path, isDir: handle.kind === 'directory',
      expanded: false, children: [], handle });
  }
  nodes.sort((a, b) => {
    if (a.isDir !== b.isDir) return a.isDir ? -1 : 1;
    return a.name.localeCompare(b.name, undefined, { numeric: true });
  });
  return nodes;
}

function createFileExplorer(
  container: HTMLElement,
  props: any,
  onEvent: (payload: string) => void,
): WidgetInstance {
  if (!document.getElementById('fe-css')) {
    const s = document.createElement('style');
    s.id = 'fe-css'; s.textContent = CSS;
    document.head.appendChild(s);
  }

  let nodes: FsNode[] = EXAMPLES.map(e => ({
    name: e.path, path: e.path, isDir: false,
    expanded: false, children: [], content: e.content,
  }));
  let dirHandle: FileSystemDirectoryHandle | null = null;
  let folderLabel = 'EXAMPLES';
  let sectionOpen = true;
  let activePath: string = props?.activePath ?? '';
  let readOnly: boolean = props?.readOnly ?? false;
  let showHidden: boolean = props?.showHidden ?? false;
  let filter: string = props?.filter ?? '';
  let renameNode: FsNode | null = null;
  let renameVal = '';
  let createMode: CreateMode = '';
  let createParent = '';
  let createVal = '';

  container.style.cssText = 'height:100%;display:flex;flex-direction:column;overflow:hidden';
  const root = el('div', 'fe'); container.appendChild(root);

  // Header
  const hdr = el('div', 'fe-hdr'); root.appendChild(hdr);
  const titleEl = el('span', 'fe-title'); titleEl.textContent = 'Explorer'; hdr.appendChild(titleEl);
  const btnNewFile = hbtn('📄', 'New File', () => startCreate('file', ''));
  const btnNewDir  = hbtn('📁', 'New Folder', () => startCreate('dir', ''));
  hbtn('⬆', 'Open Folder from Disk', openFolder);

  function applyReadOnly() {
    const hide = readOnly ? 'none' : '';
    btnNewFile.style.display = hide;
    btnNewDir.style.display  = hide;
  }
  applyReadOnly();

  function hbtn(ico: string, t: string, fn: ()=>void): HTMLButtonElement {
    const b = el('button','fe-hbtn') as HTMLButtonElement;
    b.title = t; b.textContent = ico; b.onclick = fn; hdr.appendChild(b); return b;
  }

  // Section header
  const sec = el('div', 'fe-sec'); root.appendChild(sec);
  const secArr = el('span', 'fe-arr open'); secArr.textContent = '▾'; sec.appendChild(secArr);
  const secName = el('span'); secName.textContent = folderLabel; sec.appendChild(secName);
  sec.onclick = () => {
    sectionOpen = !sectionOpen;
    secArr.className = 'fe-arr ' + (sectionOpen ? 'open' : 'closed');
    tree.style.display = sectionOpen ? '' : 'none';
  };

  // Tree
  const tree = el('div', 'fe-tree'); root.appendChild(tree);

  // Create row
  const crRow = el('div', 'fe-cr') as HTMLDivElement;
  crRow.style.display = 'none'; root.appendChild(crRow);

  function el(tag: string, cls?: string): HTMLElement {
    const e = document.createElement(tag);
    if (cls) e.className = cls;
    return e;
  }

  function matchesFilter(name: string): boolean {
    if (!filter) return true;
    const pat = filter.replace(/[.+^${}()|[\]\\]/g, '\\$&').replace(/\*/g, '.*').replace(/\?/g, '.');
    return new RegExp('^' + pat + '$', 'i').test(name);
  }

  function render() {
    tree.innerHTML = '';
    renderList(nodes, tree);
    renderCreateRow();
  }

  function renderList(list: FsNode[], parent: HTMLElement) {
    for (const n of list) {
      if (!n.isDir && !matchesFilter(n.name)) continue;
      parent.appendChild(renderItem(n));
      if (n.isDir && n.expanded) renderList(n.children, parent);
    }
  }

  function renderItem(n: FsNode): HTMLElement {
    const d = depth(n.path);
    const isActive = !n.isDir && n.path === activePath;
    const isRen = renameNode === n;

    const row = el('div', 'fe-item' + (isActive ? ' active' : ''));
    // Indent
    const ind = el('div'); ind.style.width = (d * 16) + 'px'; ind.style.flexShrink = '0'; row.appendChild(ind);
    // Arrow
    const arr = el('div', 'fe-darr' + (n.isDir && n.expanded ? ' open' : ''));
    arr.textContent = n.isDir ? '▶' : ''; arr.style.width = '16px';
    arr.style.flexShrink = '0'; arr.style.textAlign = 'center'; row.appendChild(arr);
    // Icon
    const ico = el('div', 'fe-ico');
    ico.textContent = icon(n.name, n.isDir, n.expanded); row.appendChild(ico);

    if (isRen) {
      const inp = el('input', 'fe-ri') as HTMLInputElement;
      inp.value = renameVal;
      inp.oninput = e => { renameVal = (e.target as HTMLInputElement).value; };
      inp.onkeydown = e => {
        if (e.key === 'Enter') { e.preventDefault(); confirmRename(); }
        if (e.key === 'Escape') { e.preventDefault(); cancelEdit(); }
      };
      inp.onblur = () => setTimeout(cancelEdit, 150);
      row.appendChild(inp);
      setTimeout(() => { inp.focus(); inp.select(); }, 0);
    } else {
      const lbl = el('div', 'fe-lbl'); lbl.textContent = n.name; lbl.title = n.path;
      row.appendChild(lbl);

      const acts = el('div', 'fe-acts');
      if (!readOnly) {
        if (n.isDir) {
          abtn(acts, '📄', 'New File', e => { e.stopPropagation(); startCreate('file', n.path); });
          abtn(acts, '📁', 'New Folder', e => { e.stopPropagation(); startCreate('dir', n.path); });
        }
        abtn(acts, '✎', 'Rename', e => { e.stopPropagation(); startRename(n); });
        const db = abtn(acts, '⨯', 'Delete', e => { e.stopPropagation(); deleteNode(n); });
        db.classList.add('del');
      }
      row.appendChild(acts);

      row.onclick = () => n.isDir ? toggleDir(n) : selectFile(n);
    }
    return row;
  }

  function abtn(parent: HTMLElement, ico: string, t: string, fn: (e:MouseEvent)=>void): HTMLButtonElement {
    const b = el('button','fe-abtn') as HTMLButtonElement;
    b.title = t; b.textContent = ico;
    b.addEventListener('click', fn as EventListener);
    parent.appendChild(b); return b;
  }

  function renderCreateRow() {
    if (!createMode) { crRow.style.display = 'none'; return; }
    crRow.style.display = 'flex'; crRow.innerHTML = '';
    const ico = el('span', 'fe-ci'); ico.textContent = createMode === 'dir' ? '📁' : '📄';
    crRow.appendChild(ico);
    const inp = el('input', 'fe-cinp') as HTMLInputElement;
    inp.placeholder = createMode === 'dir' ? 'folder name…' : 'file name…';
    inp.value = createVal;
    inp.oninput = e => { createVal = (e.target as HTMLInputElement).value; };
    inp.onkeydown = e => {
      if (e.key === 'Enter') { e.preventDefault(); confirmCreate(); }
      if (e.key === 'Escape') { e.preventDefault(); cancelEdit(); }
    };
    crRow.appendChild(inp);
    const ok = el('button', 'fe-cok') as HTMLButtonElement;
    ok.textContent = 'Create'; ok.onclick = confirmCreate; crRow.appendChild(ok);
    const cc = el('button', 'fe-ccancel') as HTMLButtonElement;
    cc.textContent = '✕'; cc.onclick = cancelEdit; crRow.appendChild(cc);
    setTimeout(() => inp.focus(), 0);
  }

  // ── Operations ─────────────────────────────────────────────────────────────

  function toggleDir(n: FsNode) {
    n.expanded = !n.expanded;
    if (n.expanded && n.handle && n.children.length === 0) {
      readDirHandle(n.handle as FileSystemDirectoryHandle, n.path)
        .then(ch => { n.children = ch; render(); });
    } else { render(); }
  }

  async function selectFile(n: FsNode) {
    let content = n.content ?? '';
    if (content === undefined && n.handle) {
      const file = await (n.handle as FileSystemFileHandle).getFile();
      content = await file.text();
      n.content = content;
    }
    activePath = n.path;
    render();
    onEvent(JSON.stringify({ type: 'open', path: n.path, content }));
  }

  function startRename(n: FsNode) {
    renameNode = n; renameVal = n.name; createMode = ''; render();
  }

  async function confirmRename() {
    if (!renameNode) return;
    const newName = renameVal.trim();
    if (!newName || newName === renameNode.name) { cancelEdit(); return; }
    const pp = parentPath(renameNode.path);
    const newPath = joinPath(pp, newName) + (renameNode.isDir ? '/' : '');

    if (renameNode.handle && dirHandle && !renameNode.isDir) {
      try {
        const fh = renameNode.handle as FileSystemFileHandle;
        const text = await (await fh.getFile()).text();
        const pd = await resolveDirHandle(dirHandle, pp);
        const nfh = await pd.getFileHandle(newName, { create: true });
        const w = await (nfh as any).createWritable();
        await w.write(text); await w.close();
        await (pd as any).removeEntry(renameNode.name);
        renameNode.handle = nfh; renameNode.content = text;
      } catch(e) { console.error('[FE] rename failed', e); }
    }
    const oldPath = renameNode.path;
    if (activePath === oldPath) activePath = newPath;
    renameNode.name = newName; renameNode.path = newPath;
    renameNode = null; render();
    onEvent(JSON.stringify({ type: 'rename', path: newPath, oldPath }));
  }

  function cancelEdit() { renameNode = null; createMode = ''; createVal = ''; render(); }

  function deleteNode(n: FsNode) {
    const hasKids = n.isDir && n.children.length > 0;
    if (!confirm(`Delete "${n.name}"${hasKids ? ' and all its contents' : ''}?`)) return;
    if (n.handle && dirHandle) {
      const pp = parentPath(n.path);
      resolveDirHandle(dirHandle, pp)
        .then(pd => (pd as any).removeEntry(n.name, { recursive: true }))
        .catch(e => console.error('[FE] delete failed', e));
    }
    const deletedPath = n.path;
    removeFrom(nodes, n);
    if (activePath === deletedPath || (n.isDir && activePath.startsWith(deletedPath))) activePath = '';
    render();
    onEvent(JSON.stringify({ type: 'delete', path: deletedPath }));
  }

  function removeFrom(list: FsNode[], target: FsNode): boolean {
    const i = list.indexOf(target);
    if (i !== -1) { list.splice(i, 1); return true; }
    for (const n of list) if (n.isDir && removeFrom(n.children, target)) return true;
    return false;
  }

  function startCreate(mode: CreateMode, pp: string) {
    createMode = mode; createParent = pp; createVal = ''; renameNode = null;
    if (pp) { const pn = findNode(nodes, pp); if (pn) pn.expanded = true; }
    render();
  }

  async function confirmCreate() {
    const name = createVal.trim();
    if (!name || !createMode) { cancelEdit(); return; }
    const newPath = joinPath(createParent, name) + (createMode === 'dir' ? '/' : '');

    if (dirHandle) {
      try {
        const pd = await resolveDirHandle(dirHandle, createParent);
        if (createMode === 'dir') {
          const dh = await pd.getDirectoryHandle(name, { create: true });
          insertNode({ name, path: newPath, isDir: true, expanded: false, children: [], handle: dh });
        } else {
          const fh = await pd.getFileHandle(name, { create: true });
          const w = await (fh as any).createWritable(); await w.write(''); await w.close();
          const nn: FsNode = { name, path: newPath, isDir: false, expanded: false, children: [], handle: fh, content: '' };
          insertNode(nn); activePath = newPath;
          onEvent(JSON.stringify({ type: 'create', path: newPath, content: '' }));
        }
      } catch(e) { console.error('[FE] create failed', e); }
    } else {
      const nn: FsNode = { name, path: newPath, isDir: createMode === 'dir',
        expanded: false, children: [], content: '' };
      insertNode(nn);
      if (createMode === 'file') {
        activePath = newPath;
        onEvent(JSON.stringify({ type: 'create', path: newPath, content: '' }));
      }
    }
    cancelEdit();
  }

  function insertNode(n: FsNode) {
    if (!createParent) { nodes.push(n); sortList(nodes); return; }
    const p = findNode(nodes, createParent);
    if (p) { p.children.push(n); sortList(p.children); } else { nodes.push(n); sortList(nodes); }
  }

  function sortList(list: FsNode[]) {
    list.sort((a, b) => {
      if (a.isDir !== b.isDir) return a.isDir ? -1 : 1;
      return a.name.localeCompare(b.name, undefined, { numeric: true });
    });
  }

  function findNode(list: FsNode[], path: string): FsNode | null {
    for (const n of list) {
      if (n.path === path) return n;
      if (n.isDir && path.startsWith(n.path)) { const f = findNode(n.children, path); if (f) return f; }
    }
    return null;
  }

  async function openFolder() {
    if (!('showDirectoryPicker' in window)) {
      alert('File System Access API not supported.\nUse Chrome or Edge.'); return;
    }
    try {
      dirHandle = await (window as any).showDirectoryPicker({ mode: 'readwrite' });
      folderLabel = dirHandle!.name.toUpperCase();
      secName.textContent = folderLabel;
      nodes = await readDirHandle(dirHandle!, '', showHidden);
      activePath = '';
      render();
    } catch(e: any) {
      if (e?.name !== 'AbortError') console.error('[FE] openFolder failed', e);
    }
  }

  render();

  // Auto-select first file if no active path
  if (!activePath && nodes.length > 0) {
    const first = nodes.find(n => !n.isDir);
    if (first) selectFile(first);
  }

  return {
    update(newProps: any) {
      let changed = false;
      const ap = newProps?.activePath ?? '';
      if (ap !== activePath) { activePath = ap; changed = true; }
      const ro = newProps?.readOnly ?? false;
      if (ro !== readOnly) { readOnly = ro; applyReadOnly(); changed = true; }
      const sh = newProps?.showHidden ?? false;
      if (sh !== showHidden) { showHidden = sh; changed = true; }
      const fl = newProps?.filter ?? '';
      if (fl !== filter) { filter = fl; changed = true; }
      if (changed) render();
    },
    destroy() { root.remove(); },
  };
}

registerWidget('file-explorer', { create: createFileExplorer });
