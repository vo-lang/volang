import {applicationUsage,runApplicationCommand} from './application-cli.mjs';

const packageUsage='node eng/ui-next/cli.mjs package <new-directory> [--compiler <executable>] [--desktop <sdk-directory>]';

async function main() {
  const [command,...args] = process.argv.slice(2);
  if (command === '--help' || command === 'help') {
    console.log(`${applicationUsage}\n\nCheckout commands:\n  node eng/ui-next/cli.mjs <check|build|dev|preview>\n  node eng/ui-next/cli.mjs build --studio [--static]\n  ${packageUsage}`);
  } else if (command === 'package' && args[0] !== '--project') {
    if (args.length === 1 && args[0] === '--help') {console.log(packageUsage);return;}
    const options = {};
    if (!args[0] || args[0].startsWith('-') || args.length % 2 !== 1) throw new Error('Usage: ' + packageUsage);
    for (let index = 1; index < args.length; index += 2) {
      const key = {'--compiler':'compiler','--desktop':'desktop'}[args[index]];
      if (!key || options[key] || !args[index+1] || args[index+1].startsWith('-')) throw new Error('Usage: ' + packageUsage);
      options[key] = args[index+1];
    }
    const {buildToolchain} = await import('./toolchain-build.mjs');
    const lifetime = new AbortController();
    for (const signal of ['SIGINT','SIGTERM']) process.once(signal,() => lifetime.abort());
    console.log(`Packaged ${await buildToolchain(args[0],{signal:lifetime.signal,...options})}`);
  } else if (command === 'build' && args[0] === '--studio' && (args.length === 1 || args.length === 2 && args[1] === '--static')) {
    const {buildStudio} = await import('./studio-build.mjs');
    const lifetime = new AbortController();
    for (const signal of ['SIGINT','SIGTERM']) process.once(signal,() => lifetime.abort());
    const source = await buildStudio({signal:lifetime.signal});
    if (args[1] === '--static') {
      const {exportStudio} = await import('./studio-static.mjs');
      console.log(`Built ${await exportStudio({source,signal:lifetime.signal})}`);
    } else console.log(`Built ${source}`);
  } else if (['check','build','dev','preview'].includes(command) && !args.length) {
    const {runStudioCommand} = await import('./studio-cli.mjs');
    await runStudioCommand(command);
  } else await runApplicationCommand(command,args);
}
await main().catch(error => {console.error(error.message); process.exitCode = 1;});
