/** Host-native artifacts and linker arguments; compiler owns tool discovery. */
export function desktopRuntimeName(platform=process.platform) {
  if(!['win32','darwin','linux'].includes(platform))throw new Error(`Unsupported desktop platform: ${platform}`);
  return platform==='win32'?'vo_ui_desktop_runtime.lib':'libvo_ui_desktop_runtime.a';
}

export function desktopAotArguments({entry,runtime,output,nativeLink,platform=process.platform}) {
  return ['build',entry,`--runtime=${runtime}`,...nativeLink.map(value=>`--link-arg=${value}`),
    ...(platform==='win32'?['--windows-gui']:[]),'-o',output];
}
