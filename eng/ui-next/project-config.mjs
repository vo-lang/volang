import {access,readFile,realpath,stat} from 'node:fs/promises';
import {join,resolve} from 'node:path';
import {toolchain} from './toolchain.mjs';
import {projectFeatures} from './project-features.mjs';
import {projectBackend,prepareBackend} from './project-backend.mjs';
import {resolveEntry,resolveProjectEntries} from './project-entries.mjs';
import {documentMetadata,prerenderPages} from './prerender-pages.mjs';
import {prepareHtml} from './prerender.mjs';
import {desktopConfig} from './desktop-config.mjs';

// One validated snapshot per operation: readers must not re-read a manifest
// after validating a different copy during an editor save.
export async function loadProject(directory) {
  directory=await realpath(resolve(directory));
  let config;
  try {config=JSON.parse(await readFile(join(directory,'ui-next.json'),'utf8'));}
  catch(error) {throw new Error(`Cannot read ui-next.json: ${error.message}`,{cause:error});}
  if(!config || typeof config!=='object' || Array.isArray(config))throw new Error('ui-next.json must contain a project configuration object.');
  const features=projectFeatures(config);
  projectBackend(config);
  documentMetadata(config.document);
  const schema=JSON.parse(await readFile(join(toolchain.ui,'next/wire.schema.json'),'utf8'));
  if(config.format!==1 || config.wireVersion!==schema.version) {
    throw new Error(`This application requires a matching experimental UI host (current wire v${schema.version}).`);
  }
  for(const name of ['vo.mod','vo.lock','vo.work','web/index.html','web/boot.js']) {
    if(!(await stat(join(directory,name))).isFile())throw new Error(`${name} must be a regular project file.`);
  }
  const entries=await resolveProjectEntries(directory,config);
  const serverEntry=await resolveEntry(directory,config.serverEntry,'serverEntry');
  const pages=serverEntry ? [] : prerenderPages(config);
  const desktopEntry=config.desktop===undefined ? undefined
    : await resolveEntry(directory,desktopConfig(config).entry,'desktop.entry');
  const html=await readFile(join(directory,'web/index.html'),'utf8');
  return {directory,config,features,entries,serverEntry,pages,desktopEntry,html};
}

export async function readProject(directory) {
  return (await loadProject(directory)).directory;
}

export async function prepareProjectWeb(project,{development=false}={}) {
  const {directory,config,entries,serverEntry,pages,html}=project;
  const inspection=development && !!entries.get('default').developmentEntry;
  const used=new Set(serverEntry ? entries.keys() : pages.map(page=>page.entry ?? 'default'));
  const compiled=[...entries.values()].filter(entry=>used.has(entry.id));
  const prerender=!development && !serverEntry && compiled.some(entry=>entry.prerenderEntry);
  const template=prepareBackend(html,config,{development});
  const composeHtml=prepareHtml(template,prerender || !!serverEntry);
  if(serverEntry)for(const entry of used)composeHtml('',{data:'',assets:'/',title:'',description:'',...config.document,entry});
  for(const page of pages)composeHtml(undefined,page);
  for(const name of ['assets','theme.css','build-report.json','THIRD_PARTY_NOTICES.txt']) {
    await absent(join(directory,'web',name),`web/${name} is reserved for generated output; put application assets under another name.`);
  }
  for(const page of pages)if(page.path!=='/') {
    await absent(join(directory,'web',page.file),`Static page ${page.path} collides with an authored web file.`);
  }
  return {inspection,used,compiled,prerender,template,composeHtml};
}

async function absent(path,message) {
  try {await access(path);}
  catch(error) {if(error.code==='ENOENT')return;throw error;}
  throw new Error(message);
}
