import {prepareHtml} from './prerender.mjs';
import {prepareBackend} from './project-backend.mjs';

const marker='<!--volang-desktop-bootstrap-->';

export function desktopConfig(config) {
  const value=config.desktop ?? {};
  if(!value || typeof value!=='object' || Array.isArray(value) || Object.keys(value).some(key=>!['title','width','height','entry','data','identifier'].includes(key))) {
    throw new Error('desktop accepts title, width, height, entry, data and identifier.');
  }
  const result={title:value.title ?? config.document?.title ?? 'Volang',width:value.width ?? 1080,height:value.height ?? 800,
    entry:value.entry ?? '.',data:value.data ?? '',identifier:value.identifier};
  if((config.serverEntry || config.pageEntries) && value.entry===undefined) {
    throw new Error('Server and multi-page projects must declare desktop.entry explicitly and provide their desktop services.');
  }
  if(result.identifier===undefined)throw new Error('Set a stable desktop.identifier in ui-next.json (for example dev.example.my-app). It owns this application’s persistent browser data.');
  if(typeof result.title!=='string' || !result.title.trim() || Buffer.byteLength(result.title)>1024 || /[\x00-\x1f\x7f]/.test(result.title)
      || ![result.width,result.height].every(n=>Number.isInteger(n)&&n>0&&n<=16384)
      || typeof result.entry!=='string' || !result.entry || result.entry.startsWith('/') || result.entry.includes('\\')
      || typeof result.data!=='string' || !result.data.isWellFormed() || Buffer.byteLength(result.data)>1024*1024
      || typeof result.identifier!=='string' || result.identifier.length>255 || !/^[a-zA-Z][a-zA-Z0-9-]*(\.[a-zA-Z][a-zA-Z0-9-]*)+$/.test(result.identifier)) {
    throw new Error('Invalid desktop window, entry, data or application identifier.');
  }
  return result;
}

export function desktopHtml(template,config) {
  const options=desktopConfig(config);
  const html=prepareHtml(prepareBackend(template,config),false)(undefined,{assets:'/',title:options.title,description:config.document?.description,data:options.data});
  const boot=/<script\s+type="module"\s+src="\/assets\/app\.js"><\/script>/g;
  if([...html.matchAll(boot)].length!==1 || html.includes(marker)) throw new Error('Desktop requires one canonical assets/app.js module script in web/index.html.');
  return html.replace(boot,marker);
}
