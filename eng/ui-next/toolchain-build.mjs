import {deliveredExampleFiles} from './project-templates.mjs';
import {constants} from 'node:fs';
import {access,chmod,cp,mkdir,mkdtemp,readFile,realpath,rm,stat,writeFile} from 'node:fs/promises';
import {dirname,join,relative,resolve} from 'node:path';
import {build} from './node_modules/esbuild/lib/main.js';
import {toolchain} from './toolchain.mjs';
import {toolchainSchema} from './toolchain-manifest.mjs';
import {inventory,verifyToolchain} from './toolchain-inventory.mjs';
import {copyToolPackages} from './toolchain-packages.mjs';
import {serverProtocol} from './server-request.mjs';
import {optionalHostEntries} from './project-features.mjs';
import {buildAuthoringExtension} from '../../ui/editors/vscode/build.mjs';
import {publishNewDirectory} from './publish-directory.mjs';

export async function buildToolchain(directory,{signal,compiler=toolchain.compiler,desktop} = {}) {
  if (toolchain.kind !== 'checkout') throw new Error('Build a UI toolchain from the source checkout.');
  signal?.throwIfAborted();
  directory = resolve(directory);
  try {await access(directory); throw new Error('Choose a new directory for the UI toolchain; existing files are preserved.');}
  catch (error) {if (error.code !== 'ENOENT') throw error;}
  compiler=await realpath(compiler);
  if (!(await stat(compiler)).isFile()) throw new Error('Select a compiler executable file.');
  await access(compiler,constants.X_OK);
  signal?.throwIfAborted();
  await mkdir(dirname(directory),{recursive:true});
  const stage = await mkdtemp(join(dirname(directory),'.ui-toolchain-'));
  const root = toolchain.root, sourceTools = join(root,'eng/ui-next'), web = join(root,'lang/crates/vo-web');
  async function copy(source,destination) {
    signal?.throwIfAborted();
    await mkdir(dirname(join(stage,destination)),{recursive:true});
    await cp(source,join(stage,destination),{recursive:true});
  }
  async function copyGraph(inputs,source,destination) {
    for (const input of Object.keys(inputs)) {
      const path = resolve(root,input), local = relative(source,path);
      if (!local || local.startsWith('..') || resolve(source,local) !== path) throw new Error(`Unexpected UI toolchain dependency: ${input}`);
      await copy(path,join(destination,local));
    }
  }
  try {
    const host = await build({absWorkingDir:root,entryPoints:['mount.ts','development-mount.ts','desktop-mount.ts',...optionalHostEntries].map(name => join(toolchain.host,name)),
      outdir:join(stage,'.host-analysis'),bundle:true,format:'esm',splitting:true,platform:'browser',target:'es2020',write:false,metafile:true});
    await copyGraph(host.metafile.inputs,web,'web');
    await copy(join(toolchain.host,'development.js'),'web/js/ui_next/development.js');
    await copy(toolchain.vm,'web/vm');
    const modules = await build({absWorkingDir:root,entryPoints:['application-cli.mjs','testing.mjs'].map(name => join(sourceTools,name)),
      outdir:join(stage,'.tools-analysis'),bundle:true,format:'esm',splitting:true,platform:'node',target:'node24',write:false,metafile:true,packages:'external',
      plugins:[{name:'delivered-dependencies',setup(builder){
        builder.onResolve({filter:/^(\.\/node_modules\/|\.\/toolchain-source\.mjs$)/},args => ({path:args.path,external:true}));
      }}]});
    await copyGraph(modules.metafile.inputs,sourceTools,'tools');
    // Inline the framework's language parser while retaining normal package
    // imports for the optional editor. Project bundling still records licenses.
    await build({entryPoints:[toolchain.editor],outfile:join(stage,'tools/editor-library.mjs'),bundle:true,format:'esm',platform:'browser',target:'es2020',packages:'external'});
    await copy(toolchain.plot,'tools/plot-library.mjs');
    const toolPackage = JSON.parse(await readFile(join(sourceTools,'package.json'),'utf8'));
    const optionalEditor = Object.keys(toolPackage.dependencies).filter(name => name === 'codemirror' || name.startsWith('@codemirror/') || name.startsWith('@lezer/'));
    const packages = await copyToolPackages([['esbuild',sourceTools],['uplot',sourceTools],...optionalEditor.map(name => [name,sourceTools]),['@playwright/test',join(root,'eng/browser')]],join(stage,'tools/node_modules'),{signal});
    await writeFile(join(stage,'tools/package.json'),JSON.stringify({name:'@volang/ui-tools',private:true,type:'module',engines:{node:'>=24'}}) + '\n');
    if (desktop) {
      const {readDesktopSdk} = await import('./desktop-sdk-manifest.mjs');
      const sdk = await readDesktopSdk(desktop);
      const schema = JSON.parse(await readFile(join(toolchain.ui,'next/wire.schema.json'),'utf8'));
      if (sdk.wireVersion !== schema.version) throw new Error('Desktop SDK wire version does not match this toolchain.');
      for (const path of ['desktop-sdk.json',sdk.runner.path,...(sdk.runtime ? [sdk.runtime.path] : [])]) await copy(join(desktop,path),join('desktop',path));
    }
    await copy(compiler,process.platform === 'win32' ? 'bin/vo.exe' : 'bin/vo');
    const authoring = await buildAuthoringExtension(join(stage,'.authoring'),{signal});
    await copy(authoring.vsix,'editors/volang-ui-authoring.vsix');
    await rm(authoring.directory,{recursive:true,force:true});
    await copy(join(toolchain.ui,'vo.mod'),'ui/vo.mod');
    await copy(join(toolchain.ui,'docs/release-policy.md'),'ui/docs/release-policy.md');
    await mkdir(join(stage,'ui/next'),{recursive:true});
    const ui = join(toolchain.ui,'next');
    await cp(ui,join(stage,'ui/next'),{recursive:true,filter:path => !['tests','examples','design.md','performance.md'].includes(relative(ui,path).split(/[\\/]/)[0])});
    for(const path of deliveredExampleFiles) await copy(join(ui,path),join('ui/next',path));
    await copy(toolchain.license,'LICENSE');
    const entry = '#!/usr/bin/env node\nimport {runApplicationCommand} from "./tools/application-cli.mjs";\nawait runApplicationCommand(process.argv[2],process.argv.slice(3)).catch(error => {console.error(error.message); process.exitCode = 1;});\n';
    await writeFile(join(stage,'ui.mjs'),entry);await chmod(join(stage,'ui.mjs'),0o755);
    await writeFile(join(stage,'README.md'),[
      '# Volang UI toolchain preview',
      `This ${process.platform}-${process.arch} package needs Node.js 24 or newer. It includes the matching compiler, experimental UI source, browser runtime and project tools. It does not require a Volang checkout or Cargo.`,
      'Add this package’s bin directory to PATH, then run `vo ui verify` and `vo ui create /path/to/my-app`. Inside the project, run `vo ui dev`; check, build, preview and test use the same shape. Project commands also accept a directory argument or explicit `--project /path/to/my-app`. The `vo ui web` spelling remains available. Direct `node ui.mjs` commands use the explicit `--project` argument. Optional editor and plot libraries are bundled only for projects declaring them.',
      desktop ? 'Native desktop tools are included. Use vo ui run in a project for JIT, or vo ui package for Native AOT (requires the platform C linker). Optional --backend vm|jit|aot selects execution explicitly. Built applications run without Node, Cargo or this toolchain. macOS 14 or newer produces an unsigned Application.app; Linux/Windows produce a portable directory. Keep the generated desktop.identifier stable across application updates to retain browser storage. Signing, notarization and installer publication are separate release steps.' : 'This package contains the Web tools. Desktop run/package require a matching installation with the desktop SDK.',
      'Browser tests need engines installed with `node ui.mjs browsers install`, or an existing matching PLAYWRIGHT_BROWSERS_PATH. Engine downloads are separate from this package. After moving the toolchain, use its new ui.mjs path; generated test fixtures use the active runner. Existing vendor/ui and vo.lock files remain project-owned.',
      'VS Code users can install editors/volang-ui-authoring.vsix and set volang.server.path to this package’s bin/vo (bin/vo.exe on Windows). Completion, definitions and diagnostics use vo lsp --stdio. The extension includes its language client and needs no npm installation.',
      'The complete per-file inventory is tools/toolchain.json. This local preview has not been published or product-certified. Third-party source licenses are retained in tools/node_modules.',
    ].join('\n\n') + '\n');
    const schema = JSON.parse(await readFile(join(toolchain.ui,'next/wire.schema.json'),'utf8'));
    const paths = {compiler:process.platform === 'win32' ? 'bin/vo.exe' : 'bin/vo',ui:'ui',license:'LICENSE',host:'web/js/ui_next',vm:'web/vm',
      plot:'tools/plot-library.mjs',editor:'tools/editor-library.mjs',cli:'ui.mjs',testing:'tools/testing.mjs',
      ...(desktop ? {desktop:'desktop'} : {}),testModule:'tools/node_modules/@playwright/test/index.mjs',testCLI:'tools/node_modules/@playwright/test/cli.js'};
    const artifacts = await inventory(stage,{signal});
    await writeFile(join(stage,'tools/toolchain.json'),JSON.stringify({schema:toolchainSchema,platform:process.platform,arch:process.arch,nodeMajor:24,
      wireVersion:schema.version,serverProtocol,paths,packages,artifacts},null,2) + '\n');
    await verifyToolchain(stage,{signal});
    signal?.throwIfAborted();
    await publishNewDirectory(stage,directory);
    return directory;
  } finally {await rm(stage,{recursive:true,force:true});}
}
