import {access,copyFile,mkdir} from 'node:fs/promises';
import {basename,join} from 'node:path';
import {desktopArtifact} from './desktop-sdk-manifest.mjs';

// Cargo reports build-script search paths even for cached dependencies. Keep
// those distributor paths out of the delivered SDK; only copy referenced libs.
export function nativeBuildRequirements(log) {
  const paths=new Set(),diagnostics=[];
  for(const line of log.split('\n')) {
    if(!line.startsWith('{')) {diagnostics.push(line);continue;}
    let message;try {message=JSON.parse(line);}catch{continue;}
    if(message.reason==='build-script-executed') {
      for(const path of message.linked_paths??[]) {
        if(path.startsWith('native='))paths.add(path.slice(7));
        else if(!path.includes('='))paths.add(path);
      }
    }
    if(message.reason==='compiler-message')diagnostics.push(message.message.message,message.message.rendered??'');
  }
  const nativeLink=diagnostics.join('\n').match(/(?:^|\n)(?:note: )?native-static-libs: ([^\n]+)/)?.[1].trim().split(/\s+/);
  if(!nativeLink?.length)throw new Error('Rust did not report native static link requirements.');
  return {nativeLink,searchPaths:[...paths]};
}

export async function bundleNativeLibraries(directory,{nativeLink,searchPaths,platform=process.platform}) {
  if(platform!=='win32')return [];
  const libraries=[];
  for(const name of new Set(nativeLink.filter(value=>/^[^\\/:]+\.lib$/i.test(value)))) {
    let identity;
    for(const path of searchPaths) {
      const source=join(path,name);
      try {await access(source);}catch(error){if(error.code==='ENOENT')continue;throw error;}
      const actual=await desktopArtifact(path,name);
      if(identity) {
        if(actual.sha256!==identity.sha256)throw new Error(`Conflicting native link libraries: ${name}`);
        continue;
      }
      const destination=`native/${basename(name)}`;
      await mkdir(join(directory,'native'),{recursive:true});
      await copyFile(source,join(directory,destination));
      identity=await desktopArtifact(directory,destination);libraries.push(identity);
    }
    // Libraries supplied by the installed Windows SDK retain their standard
    // linker names. Cargo-provided import libraries travel with our runtime.
  }
  return libraries;
}
