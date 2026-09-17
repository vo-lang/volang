import {cp,mkdir,mkdtemp,readFile,rename,rm,writeFile} from 'node:fs/promises';
import {basename,extname,join,resolve} from 'node:path';
import {pathToFileURL} from 'node:url';
import {build} from './node_modules/esbuild/lib/main.js';
import {root} from './repository-paths.mjs';
import {createProject,execute} from './project.mjs';
import {compilerPath} from './toolchain.mjs';
import {buildDesktopProject} from './project-desktop.mjs';
import {studioHostImports} from './studio-build.mjs';
import {verifyStudioDocuments} from './studio-documents.mjs';
import {buildPlaygroundSources} from './playground-sources.mjs';
import {thirdPartyNotices} from './third-party.mjs';

// Studio remains an ordinary project consumer. This assembler supplies its
// shared application sources, offline documentation and optional compiler workers.
export async function buildDesktopStudio({backend='jit',signal,check=false}={}) {
  await verifyStudioDocuments(root);
  const outputs=resolve(root,'target/ui-next/studio-desktop');await mkdir(outputs,{recursive:true});
  const stage=await mkdtemp(join(outputs,'stage-')),project=join(stage,'project');
  const source=resolve(root,'apps/studio/next');
  const destination=join(outputs,`${check?'check':'dist'}-${backend}`),backup=stage+'-previous';
  let previous=false;
  try {
    await createProject(project,{signal});
    const studio=join(project,'vendor/studio');await mkdir(studio);
    await cp(resolve(root,'apps/studio/vo.mod'),join(studio,'vo.mod'));
    await cp(source,join(studio,'next'),{recursive:true,filter:path=>!['tests'].includes(path.slice(source.length+1).split(/[\\/]/)[0])});
    await writeFile(join(project,'vo.mod'),(await readFile(join(project,'vo.mod'),'utf8'))+'"github.com/vo-lang/studio" = "^0.1.0"\n');
    await writeFile(join(project,'vo.work'),'format = 1\nmembers = [".", "vendor/ui", "vendor/studio"]\n');
    await cp(join(source,'main.vo'),join(project,'main.vo'));
    const env={...process.env,VOWORK:join(project,'vo.work')};
    await execute(compilerPath(),['work','sync',project],{cwd:project,env,signal});
    const config=JSON.parse(await readFile(join(project,'ui-next.json'),'utf8'));
    config.document={title:'Volang Studio',description:'Explore the UI, try the language, and find your next idea.'};
    config.desktop={title:'Volang Studio',identifier:check?'dev.volang.studio.next.contracts':'dev.volang.studio.next',width:1280,height:860};
    delete config.developmentEntry;delete config.prerenderEntry;
    await writeFile(join(project,'ui-next.json'),JSON.stringify(config,null,2)+'\n');
    const web=join(project,'web'),assets=join(web,'studio-assets');await mkdir(assets);
    let html=await readFile(join(source,'index.html'),'utf8');
    html=html.replace('<title>Volang Studio · Make something good</title>','<title><!--ui-next:title--></title>')
      .replace(/<meta name="description" content="[^"]*">/,'<meta name="description" content="<!--ui-next:description-->">')
      .replace('src="/studio-assets/boot.js"','src="<!--ui-next:assets-->assets/app.js"');
    if(check)html=html.replace('</body>','<script src="/check.js" defer></script></body>');
    await writeFile(join(web,'index.html'),html);
    // Keep the assembled Studio entry as a real browser module. Only the native
    // transport bootstrap is bundled by the common project command.
    await writeFile(join(web,'boot.js'),`if(location.pathname === '/index.html') history.replaceState(null,'','/studio/gallery');\nvoid import(new URL('/studio-assets/boot.js',location.href).href);\n`);
    if(check)await cp(resolve(root,'eng/ui-next/studio-desktop-check.js'),join(web,'check.js'));
    for(const name of ['studio.css','preview.html'])await cp(join(source,name),join(assets,name));
    await mkdir(join(web,'ui-kit'));await cp(resolve(root,'ui/next/kit/theme.css'),join(web,'ui-kit/theme.css'));
    await mkdir(join(web,'studio-docs'));
    const docs=JSON.parse(await readFile(join(source,'documentation/index.json'),'utf8'));
    for(const item of [...docs.pages,docs.search])await cp(join(source,'documentation',item.Asset),join(web,'studio-docs',item.Asset));
    await mkdir(join(web,'artifacts'));await cp(await buildPlaygroundSources(),join(web,'artifacts/playground-ui.json'));
    for(const [from,to] of [['wasm-runtime','wasm'],['wasm-compiler','compiler']]) {
      await cp(resolve(root,'target/ui-next',from),join(web,to),{recursive:true,filter:path=>!basename(path).startsWith('.')&&!['.ts','.json','.md'].includes(extname(path))});
    }
    const nativeEntry={name:'studio-native-entry',setup(builder){
      builder.onResolve({filter:/^\/host\/ui_next\/mount\.js$/},()=>({path:resolve(root,'lang/crates/vo-web/js/ui_next/desktop-mount.ts')}));
      builder.onResolve({filter:/^\/host\/ui_next\/navigation\.js$/},()=>({path:resolve(root,'lang/crates/vo-web/js/ui_next/desktop-navigation.ts')}));
    }};
    const boot=join(stage,'native-boot.js');
    await writeFile(boot,`import {mountUi} from '/host/ui_next/mount.js';\nimport {startStudio} from ${JSON.stringify(join(source,'application.js'))};\nawait startStudio(mountUi,{reload:null});\n`);
    const bundled=await build({absWorkingDir:root,entryPoints:{boot,...Object.fromEntries(['runner','preview','ui-runner','language-worker'].map(name=>[name,join(source,name+'.js')]))},
      outdir:assets,chunkNames:'chunks/[name]-[hash]',bundle:true,splitting:true,format:'esm',platform:'browser',target:'es2022',minify:true,metafile:true,
      plugins:[nativeEntry,studioHostImports],external:['/compiler/*','/wasm/*']});
    // Notices are authored resources in the prepared project, so the common
    // packager inventories and serves them with the rest of Studio.
    const thirdParty=await thirdPartyNotices({inputs:bundled.metafile.inputs,workingDirectory:root,directory:stage});
    if(thirdParty.length)await cp(join(stage,'THIRD_PARTY_NOTICES.txt'),join(web,'studio-third-party.txt'));
    const built=await buildDesktopProject(project,{backend,signal});
    await writeFile(join(built,'studio-report.json'),JSON.stringify({schema:'volang.studio-desktop.v1',backend,check,documents:docs.pages.length,
      execution:'native-ui-with-optional-wasm-playground-workers',thirdParty,browserInputs:Object.keys(bundled.metafile.inputs).sort()},null,2)+'\n');
    signal?.throwIfAborted();
    try{await rename(destination,backup);previous=true;}catch(error){if(error.code!=='ENOENT')throw error;}
    try{await rename(built,destination);}catch(error){if(previous){await rename(backup,destination);previous=false;}throw error;}
    if(previous)await rm(backup,{recursive:true,force:true});
    return destination;
  } finally {await rm(stage,{recursive:true,force:true});}
}

if(process.argv[1]&&import.meta.url===pathToFileURL(process.argv[1]).href){
  const args=process.argv.slice(2);let backend='jit',check=false;
  for(let index=0;index<args.length;index++){
    if(args[index]==='--backend'&&['vm','jit','aot'].includes(args[index+1]))backend=args[++index];
    else if(args[index]==='--check'&&!check)check=true;
    else throw new Error('Usage: node eng/ui-next/studio-desktop.mjs [--backend vm|jit|aot] [--check]');
  }
  const lifetime=new AbortController();for(const signal of ['SIGINT','SIGTERM'])process.once(signal,()=>lifetime.abort());
  await buildDesktopStudio({backend,check,signal:lifetime.signal}).then(directory=>console.log(`Studio desktop: ${directory}`)).catch(error=>{console.error(error.message);process.exitCode=1;});
}
