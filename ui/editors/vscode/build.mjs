import {createHash} from 'node:crypto';
import {createRequire} from 'node:module';
import {cp,mkdir,mkdtemp,readFile,readdir,rename,rm,rmdir,stat,writeFile} from 'node:fs/promises';
import {dirname,join,relative,resolve} from 'node:path';
import {fileURLToPath,pathToFileURL} from 'node:url';
import {spawn} from 'node:child_process';
import {build} from 'esbuild';

const directory = dirname(fileURLToPath(import.meta.url));
const root = resolve(directory,'../../..');
const require = createRequire(import.meta.url);
const hash = bytes => createHash('sha256').update(bytes).digest('hex');

async function licenses(inputs) {
  const packages = new Set();
  for (const input of Object.keys(inputs)) {
    const path = resolve(directory,input).replaceAll('\\','/');
    const marker = path.lastIndexOf('/node_modules/');
    if (marker < 0) continue;
    const parts = path.slice(marker + 14).split('/');
    packages.add(path.slice(0,marker + 14) + parts.slice(0,parts[0].startsWith('@') ? 2 : 1).join('/'));
  }
  const entries = [];
  for (const path of [...packages].sort()) {
    const metadata = JSON.parse(await readFile(join(path,'package.json'),'utf8'));
    let license;
    for (const name of ['LICENSE','LICENSE.md','LICENSE.txt','License.txt','LICENSE-MIT','COPYING']) {
      try {license = await readFile(join(path,name),'utf8');break;}
      catch (error) {if (error.code !== 'ENOENT') throw error;}
    }
    if (!license) throw new Error(`Missing bundled dependency license: ${metadata.name}`);
    entries.push({name:metadata.name,version:metadata.version,license:metadata.license,text:license});
  }
  return entries.sort((a,b) => a.name.localeCompare(b.name));
}

export async function buildAuthoringExtension(destination,{signal} = {}) {
  destination = resolve(destination);
  try {await stat(destination);throw new Error('Choose a new extension output directory.');}
  catch (error) {if (error.code !== 'ENOENT') throw error;}
  await mkdir(dirname(destination),{recursive:true});
  const stage = await mkdtemp(join(dirname(destination),'.authoring-'));
  const extension = join(stage,'extension');
  try {
    signal?.throwIfAborted();
    await mkdir(extension);
    const metadata = JSON.parse(await readFile(join(directory,'package.json'),'utf8'));
    delete metadata.dependencies; delete metadata.devDependencies; delete metadata.scripts;
    await writeFile(join(extension,'package.json'),JSON.stringify(metadata,null,2)+'\n');
    for (const name of ['README.md','language-configuration.json','syntaxes','snippets']) {
      await cp(join(directory,name),join(extension,name),{recursive:true});
    }
    await cp(join(root,'LICENSE'),join(extension,'LICENSE'));
    const bundle = await build({absWorkingDir:directory,entryPoints:['extension.cjs'],outfile:join(extension,'extension.cjs'),
      bundle:true,platform:'node',format:'cjs',target:'node18',external:['vscode'],minify:true,metafile:true,legalComments:'linked'});
    const dependencies = await licenses(bundle.metafile.inputs);
    await writeFile(join(extension,'THIRD-PARTY-NOTICES.txt'),dependencies.map(item => `${item.name} ${item.version} (${item.license})\n\n${item.text}`).join('\n\n'));
    signal?.throwIfAborted();
    const vsix = join(stage,'volang-ui-authoring.vsix');
    await new Promise((resolve,reject) => {
      const child = spawn(process.execPath,[require.resolve('@vscode/vsce/vsce'),'package','--no-dependencies','--out',vsix,
        '--baseContentUrl','https://github.com/vo-lang/volang/blob/main/ui/editors/vscode/'],{
        cwd:extension,env:{...process.env,SOURCE_DATE_EPOCH:'315532800'},signal,stdio:['ignore','pipe','pipe'],
      });
      let output = '';
      for (const stream of [child.stdout,child.stderr]) stream.on('data',chunk => {output=(output+chunk).slice(-16384);});
      child.once('error',reject);
      child.once('close',code => code === 0 ? resolve() : reject(new Error(`VS Code package failed (${code}): ${output}`)));
    });
    const bytes = await readFile(vsix);
    const report = {version:1,extensionVersion:metadata.version,sha256:hash(bytes),bytes:bytes.length,
      dependencies:dependencies.map(({text,...entry}) => entry),inputs:[]};
    const inputs = new Set([...Object.keys(bundle.metafile.inputs),'package.json','package-lock.json','README.md','language-configuration.json','build.mjs',relative(directory,join(root,'LICENSE'))]);
    for (const folder of ['syntaxes','snippets']) for (const entry of await readdir(join(directory,folder),{withFileTypes:true})) {
      if (!entry.isFile()) throw new Error(`Unexpected authoring resource: ${folder}/${entry.name}`);
      inputs.add(join(folder,entry.name));
    }
    for (const input of [...inputs].sort()) {
      const path = resolve(directory,input);
      report.inputs.push({path:relative(root,path).replaceAll('\\','/'),sha256:hash(await readFile(path))});
    }
    await writeFile(join(stage,'report.json'),JSON.stringify(report,null,2)+'\n');
    signal?.throwIfAborted();
    await mkdir(destination);
    try {await rename(stage,destination);}
    catch (error) {await rmdir(destination).catch(() => {});throw error;}
    return {...report,directory:destination,vsix:join(destination,'volang-ui-authoring.vsix')};
  } finally {await rm(stage,{recursive:true,force:true});}
}

if (process.argv[1] && pathToFileURL(resolve(process.argv[1])).href === import.meta.url) {
  if (process.argv.length !== 3) throw new Error('Usage: npm run package -- <new-directory>');
  const result = await buildAuthoringExtension(process.argv[2]);
  console.log(JSON.stringify({vsix:result.vsix,bytes:result.bytes,sha256:result.sha256}));
}
