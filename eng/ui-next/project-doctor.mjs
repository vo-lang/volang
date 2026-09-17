import {access,open,readFile,stat} from 'node:fs/promises';
import {join,relative} from 'node:path';
import {toolchain} from './toolchain.mjs';
import {loadProject,prepareProjectWeb} from './project-config.mjs';
import {desktopConfig,desktopHtml} from './desktop-config.mjs';
import {resolveEntry} from './project-entries.mjs';
import {readDesktopSdk} from './desktop-sdk-manifest.mjs';
import {verifyToolchain} from './toolchain-inventory.mjs';
import {execute} from './execute.mjs';

export function doctorArguments(args) {
  if(args[0]!=='--project' || !args[1])throw new Error('doctor requires --project <directory>.');
  const result={directory:args[1],target:'web',json:false},seen=new Set();
  for(let index=2;index<args.length;index++) {
    const option=args[index];
    if(seen.has(option))throw new Error(`Repeated doctor option: ${option}`);
    seen.add(option);
    if(option==='--json')result.json=true;
    else if(option==='--target' && ['web','desktop'].includes(args[index+1]))result.target=args[++index];
    else throw new Error('Usage: ui doctor --project <directory> [--target web|desktop] [--json]');
  }
  return result;
}

export async function diagnoseProject(directory,{target='web',signal,installation=toolchain}={}) {
  if(!['web','desktop'].includes(target))throw new Error('Doctor target must be web or desktop.');
  const checks=[];
  async function check(id,action,repair) {
    signal?.throwIfAborted();
    try {const detail=await action();checks.push({id,status:'passed',detail});return detail;}
    catch(error) {if(signal?.aborted)throw signal.reason;checks.push({id,status:'failed',detail:error.message,repair});}
  }
  await check('node',()=>{
    if(Number(process.versions.node.split('.')[0])<24)throw new Error('Node.js 24 or newer is required.');
    return process.version;
  },'Select Node.js 24 or newer with VO_UI_NODE.');
  if(installation.kind==='packaged')await check('installation',async()=>{
    const report=await verifyToolchain(installation.root,{signal});
    return `${report.files} inventoried files verified for ${report.platform}-${report.arch}.`;
  },'Restore the complete matching UI toolchain; keep its files together when moving it.');
  await check('compiler',async()=>{
    const deadline=AbortSignal.timeout(10_000);
    const version=(await execute(installation.compiler,['--version'],{
      signal:signal ? AbortSignal.any([signal,deadline]) : deadline,
    })).trim();
    if(!/^vo version \d+\.\d+\.\d+\b/.test(version))throw new Error('The selected executable is not a Volang compiler.');
    return version;
  },'Install the matching compiler and retain its executable permission.');
  let project;
  await check('project',async()=>{
    project=await loadProject(directory);
    return `${project.entries.size} Web entry set(s), UI wire v${project.config.wireVersion}.`;
  },'Correct the reported project file. Use a matching toolchain for the project’s wire version.');
  if(project)await check('framework',async()=>{
    const path=join(project.directory,'vendor/ui/next/wire.schema.json');
    const schema=JSON.parse(await readFile(path,'utf8'));
    if(schema.version!==project.config.wireVersion)throw new Error('The vendored UI wire version differs from ui-next.json.');
    await access(join(project.directory,'vendor/ui/next/kit/theme.css'));
    return `Vendored UI wire v${schema.version} matches this project.`;
  },'Restore this project’s matching vendor/ui snapshot. Do not fix a wire mismatch by editing only its version number.');
  if(project)await check('document',async()=>{
    if(target==='web') {
      await prepareProjectWeb(project);await prepareProjectWeb(project,{development:true});
    } else {
      await resolveEntry(project.directory,desktopConfig(project.config).entry,'desktop.entry');
      desktopHtml(project.html,project.config);
    }
    return target==='web' ? 'Production and development HTML configuration is valid.' : 'Desktop entry and document configuration are valid.';
  },'Correct the document markers or entry configuration in ui-next.json and web/index.html.');
  if(target==='web') {
    await check('web-runtime',async()=>{
      const file=await open(join(installation.vm,'vo_web_bg.wasm'),'r');
      try {
        const header=Buffer.alloc(8),result=await file.read(header,0,8,0);
        if(result.bytesRead!==8 || !header.equals(Buffer.from([0,97,115,109,1,0,0,0])))throw new Error('The Web VM is missing its WebAssembly header.');
      } finally {await file.close();}
      if(!(await stat(join(installation.vm,'vo_web.js'))).isFile())throw new Error('The Web VM JavaScript module is unavailable.');
      return 'Web VM module and WebAssembly binary are present.';
    },'Restore the matching Web runtime. Checkout builds use eng/ui-next/build-runtime.mjs.');
    await check('browser-test-runner',async()=>{
      await access(installation.testCLI);await access(installation.testModule);
      return 'Test runner is present. Install matching browser engines with vo ui browsers install.';
    },'Restore the toolchain’s browser test runner. Engine installation is a separate step.');
  } else {
    await check('desktop-sdk',async()=>{
      if(!installation.desktop)throw new Error('This installation has no desktop SDK.');
      const sdk=await readDesktopSdk(installation.desktop);
      if(project && sdk.wireVersion!==project.config.wireVersion)throw new Error('Desktop SDK and project wire versions differ.');
      if(!sdk.runtime)throw new Error('The desktop SDK has no Native AOT runtime.');
      return `${sdk.platform}-${sdk.arch}, ${sdk.profile}: VM/JIT runner and Native AOT runtime verified.`;
    },'Install the matching toolchain with its desktop SDK.');
  }
  const local=path=>path && (relative(project.directory,path).replaceAll('\\','/') || '.');
  return {schema:'volang.ui-project-diagnosis.v1',passed:checks.every(check=>check.status==='passed'),target,
    ...(project ? {project:{directory:project.directory,wireVersion:project.config.wireVersion,features:project.features,
      entries:[...project.entries.values()].map(entry=>Object.fromEntries(Object.entries(entry).map(([key,value])=>[key,key==='id'?value:local(value)]))),
      serverEntry:local(project.serverEntry),desktopEntry:local(project.desktopEntry)}} : {}),
    checks,limits:target==='web'
      ? ['Application types and host imports are checked by vo ui check.','Browser engines and interaction are checked by vo ui test.']
      : ['Native linking is checked by vo ui package.','System WebView availability and window behavior are checked by vo ui run.']};
}

export function formatDoctor(report) {
  return [`UI ${report.target} diagnosis: ${report.passed?'passed':'needs attention'}`,
    ...report.checks.flatMap(check=>[`${check.status==='passed'?'OK':'FAIL'} ${check.id}: ${check.detail}`,...(check.repair?[`  ${check.repair}`]:[])]),
    ...report.limits].join('\n');
}
