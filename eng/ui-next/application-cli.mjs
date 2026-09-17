import {templateNames} from './project-templates.mjs';
import {pathToFileURL} from 'node:url';
import {toolchain} from './toolchain.mjs';

export const applicationUsage = `Usage: ui create <directory> [--template ${templateNames.join('|')}]
       ui <check|build|dev|preview|test> --project <directory>
       ui <run|package> --project <directory> [--backend vm|jit|aot]
       ui doctor --project <directory> [--target web|desktop] [--json]
       ui verify
       ui browsers install [chromium|firefox|webkit ...]
Requires Node.js 24 or newer. Browser engines are installed separately.`;

export async function runApplicationCommand(command,args) {
  const commands = ['create','check','build','dev','preview','test','verify','browsers','run','package','doctor'];
  if (['--help','-h','help'].includes(command) || commands.includes(command) && args.length === 1 && ['--help','-h'].includes(args[0])) {
    console.log(applicationUsage); return;
  }
  const lifetime = new AbortController(), cancel = () => lifetime.abort();
  for (const signal of ['SIGINT','SIGTERM']) process.once(signal,cancel);
  try {
    if (command === 'create' && args[0] && !args[0].startsWith('-') &&
        (args.length === 1 || args.length === 3 && args[1] === '--template')) {
      const {createProject} = await import('./project.mjs');
      console.log(`Created ${await createProject(args[0],{template:args[2] ?? 'default',signal:lifetime.signal})}.`);
    } else if (command === 'verify' && !args.length) {
      if (toolchain.kind !== 'packaged') throw new Error('verify requires a packaged UI toolchain.');
      const {verifyToolchain} = await import('./toolchain-inventory.mjs');
      const report = await verifyToolchain(toolchain.root,{signal:lifetime.signal});
      console.log(`Verified ${report.files} files for ${report.platform}-${report.arch}, UI wire ${report.wireVersion}.`);
    } else if (command === 'browsers' && args[0] === 'install' && args.slice(1).every(name => ['chromium','firefox','webkit'].includes(name))) {
      const {execute} = await import('./project.mjs');
      console.log(await execute(process.execPath,[toolchain.testCLI,...args],{signal:lifetime.signal,
        env:{...process.env,...(process.env.PLAYWRIGHT_BROWSERS_PATH || toolchain.browserCache ? {PLAYWRIGHT_BROWSERS_PATH:process.env.PLAYWRIGHT_BROWSERS_PATH ?? toolchain.browserCache} : {})}}));
    } else if (command==='doctor') {
      const {doctorArguments,diagnoseProject,formatDoctor}=await import('./project-doctor.mjs');
      const options=doctorArguments(args);
      const report=await diagnoseProject(options.directory,{target:options.target,signal:lifetime.signal});
      console.log(options.json ? JSON.stringify(report,null,2) : formatDoctor(report));
      if(!report.passed)process.exitCode=1;
    } else if (['run','package'].includes(command) && args[0] === '--project' && args[1] &&
        (args.length === 2 || args.length === 4 && args[2] === '--backend' && ['vm','jit','aot'].includes(args[3]))) {
      const {buildDesktopProject,runDesktopProject} = await import('./project-desktop.mjs');
      const options = {signal:lifetime.signal,backend:args[3] ?? (command === 'run' ? 'jit' : 'aot')};
      if (command === 'run') await runDesktopProject(args[1],options);
      else console.log(`Packaged ${await buildDesktopProject(args[1],options)}`);
    } else if (['check','build','dev','preview','test'].includes(command) && args.length === 2 && args[0] === '--project') {
      if (command === 'check' || command === 'build') {
        const {checkProject,buildProject} = await import('./project.mjs');
        if (command === 'check') {
          const report=await checkProject(args[1],{signal:lifetime.signal});
          if(report.diagnostics)console.log(report.diagnostics);
          console.log(`Checked ${report.entries} source entries in ${report.directory}.`);
        } else console.log(`Built ${await buildProject(args[1],{signal:lifetime.signal})}`);
      } else if (command === 'test') {
        const {testProject} = await import('./project-testing.mjs');
        const result = await testProject(args[1],{signal:lifetime.signal});
        console.log(`${result.log}\nBrowser test report: ${result.output}/report.json`);
      } else {
        const {developProject,previewProject} = await import('./project-development.mjs');
        const application = await (command === 'dev' ? developProject : previewProject)(args[1],{signal:lifetime.signal});
        try {
          console.log(application.url);
          if (!lifetime.signal.aborted) await new Promise(resolve => lifetime.signal.addEventListener('abort',resolve,{once:true}));
        } finally {await application.close();}
      }
    } else throw new Error(applicationUsage);
  } catch (error) {
    if (!lifetime.signal.aborted) throw error;
    process.exitCode = 130;
  } finally {
    for (const signal of ['SIGINT','SIGTERM']) process.removeListener(signal,cancel);
  }
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  await runApplicationCommand(...[process.argv[2],process.argv.slice(3)]).catch(error => {console.error(error.message); process.exitCode = 1;});
}
