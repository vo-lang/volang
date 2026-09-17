import assert from 'node:assert/strict';
import test from 'node:test';
import {readFile,stat} from 'node:fs/promises';
import {join} from 'node:path';
import {build} from './node_modules/esbuild/lib/main.js';
import {projectTemplates,templateNames,deliveredExampleFiles} from './project-templates.mjs';
import {projectFeatures} from './project-features.mjs';
import {applicationUsage} from './application-cli.mjs';
import {toolchain} from './toolchain.mjs';

test('the public catalog preserves templates and supplies complete example projects',async()=>{
  for(const name of ['default','pages','fieldnotes','listening','canvas','plot','scroll-position','variable-list']) {
    assert(Object.hasOwn(projectTemplates,name));
    assert(applicationUsage.includes(name));
  }
  const sources=[];
  for(const [name,template]of Object.entries(projectTemplates)) {
    assert.match(name,/^[a-z][a-z-]*$/);
    projectFeatures(template.config);
    if(template.files) {
      assert.equal(template.directory,undefined);
      assert.deepEqual(template.files.map(([,destination])=>destination),['app/app.vo','web/app.css','tests/browser/app.test.mjs']);
      for(const [source]of template.files) {
        assert(source.startsWith('examples/'+name+'/'));
        const path=join(toolchain.ui,'next',source);
        assert((await stat(path)).isFile());
        assert((await readFile(path)).length>0);
        sources.push(source);
      }
      const source=await readFile(join(toolchain.ui,'next',template.files[2][0]),'utf8');
      assert.match(source,/from '\.\/fixtures\.mjs'/);
      assert(!source.includes('/eng/')&&!source.includes('node_modules'));
    } else assert((await stat(join(toolchain.ui,'next',template.directory))).isDirectory());
  }
  assert.deepEqual([...deliveredExampleFiles].sort(),sources.sort());
  assert.equal(new Set(deliveredExampleFiles).size,deliveredExampleFiles.length);
});

test('packaged source analysis inventories the template catalog',async()=>{
  const result=await build({entryPoints:[new URL('project-templates.mjs',import.meta.url).pathname],bundle:true,write:false,
    format:'esm',platform:'node',target:'node24',packages:'external',metafile:true});
  assert(Object.keys(result.metafile.inputs).some(path=>path.endsWith('/project-templates.json')));
  assert.equal(templateNames.length,Object.keys(projectTemplates).length);
});
