import {createRequire} from 'node:module';
import {cp,mkdir,readFile,realpath} from 'node:fs/promises';
import {dirname,join,relative} from 'node:path';

async function installedPackage(name,parent) {
  // Follow Node's package search paths, including nested dependencies. Native
  // binary packages need not expose a JavaScript entry or package.json export.
  for (const modules of createRequire(join(parent,'package.json')).resolve.paths(name) ?? []) {
    try {
      const path = await realpath(join(modules,name));
      const metadata = JSON.parse(await readFile(join(path,'package.json'),'utf8'));
      if (metadata.name === name) return {path,metadata};
    } catch (error) {if (error.code !== 'ENOENT') throw error;}
  }
  throw Object.assign(new Error(`Cannot locate installed dependency ${name}.`),{code:'MODULE_NOT_FOUND'});
}

// Copy the actual locked dependency closure, without an install step or scripts.
// Conflicting versions require explicit packaging work instead of flattening a
// different graph into the delivered toolchain.
export async function copyToolPackages(requests,directory,{signal} = {}) {
  const found = new Map();
  async function include(name,parent,optional = false) {
    signal?.throwIfAborted();
    let entry;
    try {entry = await installedPackage(name,parent);}
    catch (error) {if (optional && error.code === 'MODULE_NOT_FOUND') return; throw error;}
    const {path,metadata} = entry;
    const matches = (values,current) => !values || (values.includes(current) || values.every(value => value.startsWith('!'))) && !values.includes('!' + current);
    if (!matches(metadata.os,process.platform) || !matches(metadata.cpu,process.arch)) {
      if (optional) return;
      throw new Error(`Dependency ${name} does not support this platform.`);
    }
    if (found.has(name)) {
      if (found.get(name).version !== metadata.version) throw new Error(`Conflicting installed versions of ${name}.`);
      return;
    }
    found.set(name,{name,version:metadata.version,license:metadata.license});
    const destination = join(directory,name);
    await mkdir(dirname(destination),{recursive:true});
    await cp(path,destination,{recursive:true,filter:source => !relative(path,source).split(/[\\/]/).includes('node_modules')});
    for (const dependency of Object.keys(metadata.dependencies ?? {})) await include(dependency,path);
    for (const dependency of Object.keys(metadata.optionalDependencies ?? {})) await include(dependency,path,true);
  }
  for (const [name,parent] of requests) await include(name,parent);
  return [...found.values()].sort((a,b) => a.name < b.name ? -1 : a.name > b.name ? 1 : 0);
}
