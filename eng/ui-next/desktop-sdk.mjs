import {access,copyFile,mkdir,mkdtemp,readFile,rm,writeFile} from 'node:fs/promises';
import {dirname,join,resolve} from 'node:path';
import {pathToFileURL} from 'node:url';
import {toolchain} from './toolchain.mjs';
import {execute} from './project.mjs';
import {desktopArtifact,readDesktopSdk} from './desktop-sdk-manifest.mjs';
import {desktopRuntimeName} from './desktop-link.mjs';
import {publishNewDirectory} from './publish-directory.mjs';
import {nativeBuildRequirements,bundleNativeLibraries} from './desktop-native-libraries.mjs';

// Built once by the distributor. End-user projects need neither Rust nor Cargo.
export async function buildDesktopSdk(directory,{profile='release-native',signal}={}) {
  if (toolchain.kind !== 'checkout' || !['dev','release-native'].includes(profile)) throw new Error('Build a desktop SDK from a checkout with dev or release-native profile.');
  directory=resolve(directory);
  try {await access(directory);throw new Error('Choose a new desktop SDK directory.');} catch(error) {if(error.code!=='ENOENT')throw error;}
  await mkdir(dirname(directory),{recursive:true});
  const stage=await mkdtemp(join(dirname(directory),'.desktop-sdk-'));
  const env={...process.env,VOWORK:'off',CARGO_INCREMENTAL:'0'};
  const cargo=['--locked','--offline','--profile',profile,'-p','vo-ui-desktop-runtime'];
  const output=join(toolchain.root,'target',profile==='dev'?'debug':profile);
  try {
    console.log('Building native desktop runner…');
    await writeFile(join(stage,'runner-build.log'),await execute('cargo',['build',...cargo,'--features','jit','--bin','vo-ui-desktop'],{env,signal}));
    const runner=process.platform==='win32'?'vo-ui-desktop.exe':'vo-ui-desktop';
    await copyFile(join(output,runner),join(stage,runner));
    console.log('Building Native AOT desktop runtime…');
    const messages=join(stage,'cargo-messages.jsonl');
    const log=await execute('cargo',['rustc',...cargo,'--color','never','--message-format=json','--features','aot','--lib','--','--print','native-static-libs'],{env,signal,stdoutFile:messages});
    await writeFile(join(stage,'aot-build.log'),log);
    const requirements=nativeBuildRequirements((await readFile(messages,'utf8'))+'\n'+log),{nativeLink}=requirements;
    const libraries=await bundleNativeLibraries(stage,requirements);
    const tree=await execute('cargo',['tree','--locked','--offline','-p','vo-ui-desktop-runtime','--features','aot','-e','normal'],{env,signal});
    for(const dependency of ['vo-ui-runtime ','vo-ui-vm ','vo-ui-integration ','vo-codegen ','cranelift-codegen ']) {
      if(tree.includes(dependency))throw new Error(`Desktop AOT unexpectedly depends on ${dependency}`);
    }
    await writeFile(join(stage,'aot-dependencies.txt'),tree);
    const name=desktopRuntimeName();
    await copyFile(join(output,name),join(stage,name));
    const runtime=await desktopArtifact(stage,name);
    const wire=JSON.parse(await readFile(join(toolchain.ui,'next/wire.schema.json'),'utf8'));
    await writeFile(join(stage,'desktop-sdk.json'),JSON.stringify({schema:'volang.ui-desktop-sdk.v3',platform:process.platform,arch:process.arch,profile,wireVersion:wire.version,
      runner:await desktopArtifact(stage,runner),runtime,nativeLink,libraries},null,2)+'\n');
    await readDesktopSdk(stage);signal?.throwIfAborted();
    await publishNewDirectory(stage,directory);
    return directory;
  } finally {await rm(stage,{recursive:true,force:true});}
}

if(process.argv[1] && import.meta.url===pathToFileURL(process.argv[1]).href) {
  const [directory,...args]=process.argv.slice(2);
  if(!directory || directory.startsWith('-') || !(args.length===0 || args.length===2 && args[0]==='--profile')) {
    console.error('Usage: node eng/ui-next/desktop-sdk.mjs <new-directory> [--profile dev|release-native]');process.exitCode=1;
  } else {
    const lifetime=new AbortController();for(const signal of ['SIGINT','SIGTERM'])process.once(signal,()=>lifetime.abort());
    await buildDesktopSdk(directory,{profile:args[1],signal:lifetime.signal}).then(value=>console.log(`Desktop SDK: ${value}`)).catch(error=>{console.error(error.message);process.exitCode=1;});
  }
}
