import {lstat,mkdir,rename,rmdir} from 'node:fs/promises';

// Publish a complete staged directory without replacing an existing project.
// Windows MoveFileEx refuses an existing directory, including an empty one,
// but Node's rename permits replacing a file. Check existing entries first.
// POSIX rename can replace empty directories, so reserve the name first.
// https://learn.microsoft.com/windows/win32/api/winbase/nf-winbase-movefileexw
export async function publishNewDirectory(stage,destination) {
  if(process.platform==='win32') {
    try {
      await lstat(destination);
      throw new Error(`Destination already exists: ${destination}`);
    } catch(error) {
      if(error.code!=='ENOENT') throw error;
    }
    await rename(stage,destination);
    return;
  }
  await mkdir(destination);
  try {await rename(stage,destination);}
  catch(error) {await rmdir(destination).catch(()=>{});throw error;}
}
