import {dirname,join} from 'node:path';

/** Host-native artifacts and linker arguments; compiler owns tool discovery. */
export function desktopRuntimeName(platform=process.platform) {
  if(!['win32','darwin','linux'].includes(platform))throw new Error(`Unsupported desktop platform: ${platform}`);
  return platform==='win32'?'vo_ui_desktop_runtime.lib':'libvo_ui_desktop_runtime.a';
}

export function desktopAotArguments({entry,runtime,output,nativeLink,libraries=[],sdkDirectory,platform=process.platform}) {
  const bundled=new Map(libraries.map(resource=>[resource.path.split('/').at(-1).toLowerCase(),join(sdkDirectory,resource.path)]));
  // MSVC object directives may name the same import libraries transitively.
  const search=platform==='win32'?[...new Set([...bundled.values()].map(dirname))].map(path=>`--link-arg=/LIBPATH:${path}`):[];
  return ['build',entry,`--runtime=${runtime}`,...search,...nativeLink.map(value=>`--link-arg=${bundled.get(value.toLowerCase())??value}`),
    ...(platform==='win32'?['--windows-gui']:[]),'-o',output];
}
