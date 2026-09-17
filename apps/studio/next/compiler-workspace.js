import {WIRE_VERSION} from '/host/ui_next/generated/protocol.js';

export function writeCompilerFile(filesystem,path,source) {
  const directory=path.slice(0,path.lastIndexOf('/'));
  const error=filesystem.vfs.mkdirAll(directory,0o755)??filesystem.vfs.writeFile(path,new TextEncoder().encode(source),0o644);
  if(error)throw new Error(error);
}

// Execution and semantic workers consume the same packaged UI source snapshot
// and canonical workspace-lock preparation, in their own memory-only filesystems.
export async function prepareUiCompiler(runtime,filesystem,root) {
  const response=await fetch('/artifacts/playground-ui.json');
  if(!response.ok)throw new Error('The UI examples could not be loaded.');
  const snapshot=await response.json();
  if(snapshot.format!==1||snapshot.wireVersion!==WIRE_VERSION)throw new Error('Please rebuild Studio and reload to use matching UI sources.');
  for(const file of snapshot.files)writeCompilerFile(filesystem,`${root}/${file.path}`,file.text);
  const manifest=path=>snapshot.files.find(file=>file.path===path).text;
  writeCompilerFile(filesystem,`${root}/vo.lock`,runtime.prepareWorkspaceLock(manifest('vo.mod'),[manifest('vendor/ui/vo.mod')]));
}
