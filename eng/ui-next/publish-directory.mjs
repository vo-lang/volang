import {mkdir,rename,rmdir} from 'node:fs/promises';

// Publish a complete staged directory without replacing an existing project.
// Windows MoveFileEx refuses an existing directory, including an empty one.
// POSIX rename can replace empty directories, so reserve the name first.
// https://learn.microsoft.com/windows/win32/api/winbase/nf-winbase-movefileexw
export async function publishNewDirectory(stage,destination) {
  if(process.platform==='win32') {
    await rename(stage,destination);
    return;
  }
  await mkdir(destination);
  try {await rename(stage,destination);}
  catch(error) {await rmdir(destination).catch(()=>{});throw error;}
}
