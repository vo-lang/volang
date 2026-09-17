import {chmod,copyFile,mkdir,mkdtemp,readFile,readdir,rm,writeFile} from 'node:fs/promises';
import {basename,dirname,extname,join} from 'node:path';
import {build} from './node_modules/esbuild/lib/main.js';
import {toolchain,compilerPath} from './toolchain.mjs';
import {execute} from './execute.mjs';
import {loadProject} from './project-config.mjs';
import {desktopConfig,desktopHtml} from './desktop-config.mjs';
export {desktopConfig,desktopHtml} from './desktop-config.mjs';
import {resolveEntry} from './project-entries.mjs';
import {projectHost} from './project-features.mjs';
import {desktopArtifact,readDesktopSdk} from './desktop-sdk-manifest.mjs';
import {thirdPartyNotices} from './third-party.mjs';
import {desktopAotArguments} from './desktop-link.mjs';
import {renameDirectory} from './publish-directory.mjs';

const types={'.html':'html','.js':'javascript','.css':'css','.svg':'svg','.png':'png','.jpg':'jpeg','.jpeg':'jpeg','.webp':'webp',
  '.woff2':'woff2','.json':'json','.txt':'text','.ico':'icon','.wav':'wav','.mp3':'mp3','.mp4':'mp4','.webm':'webm','.vob':'binary','.wasm':'wasm'};
const mediaType=path => basename(path)==='LICENSE' ? 'text' : types[extname(path)];

async function list(directory,base='') {
  const result=[];
  for(const entry of await readdir(join(directory,base),{withFileTypes:true})) {
    const path=base ? `${base}/${entry.name}` : entry.name;
    if(entry.isDirectory())result.push(...await list(directory,path));
    else if(entry.isFile())result.push(path);
    else throw new Error(`Desktop resources cannot contain links or special files: ${path}`);
  }
  return result.sort();
}
const xml=value=>value.replace(/[&<>"']/g,char=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&apos;'}[char]));

export async function buildDesktopProject(directory,{backend='aot',signal,sdkDirectory=toolchain.desktop}={}) {
  if(!['vm','jit','aot'].includes(backend))throw new Error('Desktop backend must be vm, jit or aot.');
  if(!sdkDirectory)throw new Error('Native desktop tools are not installed. Install the matching UI toolchain with its desktop SDK.');
  const sdk=await readDesktopSdk(sdkDirectory);
  if(backend==='aot' && !sdk.runtime)throw new Error('This desktop SDK does not provide Native AOT for this platform.');
  const project=await loadProject(directory);
  ({directory}=project);
  const {config,features}=project;
  if(config.wireVersion!==sdk.wireVersion)throw new Error('Desktop SDK and project wire versions differ.');
  const options=desktopConfig(config);
  const entry=await resolveEntry(directory,options.entry,'desktop.entry');
  const html=desktopHtml(project.html,config);
  const outputs=join(directory,'target/ui-desktop');await mkdir(outputs,{recursive:true});
  const stage=await mkdtemp(join(outputs,'staging-')),destination=join(outputs,`dist-${backend}`),backup=stage+'-previous';
  const mac=process.platform==='darwin',app=mac ? join(stage,'Application.app') : stage;
  const resources=mac ? join(app,'Contents/Resources') : join(app,'resources');
  const executable=mac ? join(app,'Contents/MacOS/application') : join(app,process.platform==='win32'?'application.exe':'application');
  let previous=false;
  try {
    await mkdir(resources,{recursive:true});await mkdir(dirname(executable),{recursive:true});
    const source=join(directory,'web');
    for(const path of await list(source)) {
      if(['index.html','boot.js'].includes(path))continue;
      if(['desktop.json','desktop.js','theme.css','app.vob','THIRD_PARTY_NOTICES.txt'].includes(path))throw new Error(`web/${path} is reserved for generated desktop resources.`);
      if(!mediaType(path))throw new Error(`Unsupported desktop resource type: ${path}`);
      await mkdir(dirname(join(resources,path)),{recursive:true});await copyFile(join(source,path),join(resources,path));
    }
    await writeFile(join(resources,'index.html'),html);
    await copyFile(join(directory,'vendor/ui/next/kit/theme.css'),join(resources,'theme.css'));
    const bundled=await build({absWorkingDir:directory,entryPoints:[join(directory,'web/boot.js')],outfile:join(resources,'desktop.js'),
      bundle:true,format:'esm',platform:'browser',target:'es2022',minify:true,metafile:true,
      plugins:[projectHost({features,desktop:true})]});
    const thirdParty=await thirdPartyNotices({inputs:bundled.metafile.inputs,workingDirectory:directory,directory:resources});
    const env={...process.env,VOWORK:join(directory,'vo.work')};
    if(backend==='aot') {
      await execute(compilerPath(),desktopAotArguments({entry,runtime:join(sdkDirectory,sdk.runtime.path),output:executable,nativeLink:sdk.nativeLink,libraries:sdk.libraries,sdkDirectory}),{cwd:directory,env,signal});
    } else {
      await execute(compilerPath(),['emit','bytecode',entry,'-o',join(resources,'app.vob')],{cwd:directory,env,signal});
      await copyFile(join(sdkDirectory,sdk.runner.path),executable);
    }
    await chmod(executable,0o755);
    const describe=async path=>({...await desktopArtifact(resources,path),mediaType:mediaType(path)});
    const paths=await list(resources);
    if(paths.length>1026)throw new Error('Desktop application exceeds its resource count limit.');
    const assets=[];
    for(const path of paths)if(!['index.html','desktop.js','app.vob'].includes(path))assets.push(await describe(path));
    await writeFile(join(resources,'desktop.json'),JSON.stringify({schema:'volang.ui-desktop-application.v2',identifier:options.identifier,title:options.title,width:options.width,height:options.height,backend,
      index:await describe('index.html'),host:await describe('desktop.js'),application:backend==='aot'?null:await describe('app.vob'),assets},null,2)+'\n');
    if(mac) {
      await writeFile(join(app,'Contents/Info.plist'),`<?xml version="1.0" encoding="UTF-8"?>\n<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">\n<plist version="1.0"><dict><key>CFBundleExecutable</key><string>application</string><key>CFBundleName</key><string>${xml(options.title)}</string><key>CFBundleIdentifier</key><string>${xml(options.identifier)}</string><key>CFBundlePackageType</key><string>APPL</string><key>CFBundleVersion</key><string>1</string><key>LSMinimumSystemVersion</key><string>14.0</string><key>NSHighResolutionCapable</key><true/></dict></plist>\n`);
    }
    // Validate the exact standalone resources and bytecode before publication.
    await execute(executable,['--check'],{cwd:stage,signal});
    const artifacts=[];for(const path of await list(stage))artifacts.push(await desktopArtifact(stage,path));
    await writeFile(join(stage,'build-report.json'),JSON.stringify({schema:'volang.ui-desktop-build.v1',backend,platform:process.platform,arch:process.arch,
      profile:sdk.profile,wireVersion:config.wireVersion,entry:options.entry,features,thirdParty,
      executable:executable.slice(stage.length+1).replaceAll('\\','/'),artifacts},null,2)+'\n');
    signal?.throwIfAborted();
    try {await renameDirectory(destination,backup,{signal});previous=true;}catch(error){if(error.code!=='ENOENT')throw error;}
    try {await renameDirectory(stage,destination,{signal});}catch(error){if(previous){await renameDirectory(backup,destination);previous=false;}throw error;}
    if(previous){await rm(backup,{recursive:true,force:true});previous=false;}
    return destination;
  } finally {await rm(stage,{recursive:true,force:true});}
}

export async function runDesktopProject(directory,options={}) {
  const output=await buildDesktopProject(directory,{...options,backend:options.backend??'jit'});
  const report=JSON.parse(await readFile(join(output,'build-report.json'),'utf8'));
  console.log(`Opening ${output}`);
  const log=await execute(join(output,report.executable),[],{cwd:output,signal:options.signal});
  if(log)console.log(log);
}
