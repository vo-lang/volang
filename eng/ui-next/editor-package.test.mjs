import assert from 'node:assert/strict';
import {createRequire} from 'node:module';
import {mkdtemp,readFile,rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import test from 'node:test';
import {buildAuthoringExtension} from '../../ui/editors/vscode/build.mjs';

const {unzipSync,strFromU8} = createRequire(import.meta.url)('fflate');

test('portable VS Code extension is self-contained and reproducible',async () => {
  const directory = await mkdtemp(join(tmpdir(),'vo-extension-'));
  try {
    const first = await buildAuthoringExtension(join(directory,'first'));
    const second = await buildAuthoringExtension(join(directory,'second'));
    assert.equal(first.sha256,second.sha256);
    const zip = unzipSync(await readFile(first.vsix));
    const manifest = JSON.parse(strFromU8(zip['extension/package.json']));
    assert.equal(manifest.main,'./extension.cjs');
    assert.equal(manifest.dependencies,undefined);
    assert.equal(manifest.devDependencies,undefined);
    assert(manifest.contributes.commands.some(item => item.command === 'volang.restartServer'));
    assert(!Object.keys(zip).some(path => path.includes('node_modules') || path.includes('/test/')));
    assert.match(strFromU8(zip['extension/THIRD-PARTY-NOTICES.txt']),/vscode-languageclient 9\.0\.1/);
    assert.match(strFromU8(zip['extension/LICENSE.txt']),/MIT License/);
    assert(zip['extension/extension.cjs'].length > 100000);
    assert(zip['extension/snippets/web.json']);
    assert(zip['extension/syntaxes/volang.tmLanguage.json']);
    await assert.rejects(buildAuthoringExtension(first.directory),/new extension output/);
    assert.equal((await readFile(first.vsix)).length,first.bytes);
  } finally {await rm(directory,{recursive:true,force:true});}
});
