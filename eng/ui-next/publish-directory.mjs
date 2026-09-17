import fs from 'node:fs/promises';
import {setTimeout as delay} from 'node:timers/promises';

// Windows can briefly retain a sharing lock after an executable has exited.
// Retry only sharing/access failures, for at most 4.55 seconds. A persistent
// failure retains its original filesystem error; no destination is removed.
export async function renameDirectory(stage,destination,{signal}={}) {
  const waits=[50,100,200,400,800,1000,1000,1000];
  for(let attempt=0;;attempt++) {
    signal?.throwIfAborted();
    try {await fs.rename(stage,destination);return;}
    catch(error) {
      if(process.platform!=='win32'||!['EPERM','EACCES','EBUSY'].includes(error.code)||attempt===waits.length)throw error;
      try {await delay(waits[attempt],undefined,{signal});}
      catch(error) {signal?.throwIfAborted();throw error;}
    }
  }
}

// Publish a complete staged directory without replacing an existing project.
// Windows MoveFileEx refuses an existing directory, including an empty one,
// but Node's rename permits replacing a file. Check existing entries first.
// POSIX rename can replace empty directories, so reserve the name first.
// https://learn.microsoft.com/windows/win32/api/winbase/nf-winbase-movefileexw
export async function publishNewDirectory(stage,destination) {
  if(process.platform==='win32') {
    try {
      await fs.lstat(destination);
      throw new Error(`Destination already exists: ${destination}`);
    } catch(error) {
      if(error.code!=='ENOENT') throw error;
    }
    await fs.rename(stage,destination);
    return;
  }
  await fs.mkdir(destination);
  try {await fs.rename(stage,destination);}
  catch(error) {await fs.rmdir(destination).catch(()=>{});throw error;}
}
