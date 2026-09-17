import {spawn} from 'node:child_process';

// vo-dev owns the complete command group/job and can terminate it forcibly.
// A nested stage must stay in that group. Direct local runs own their stages.
export function spawnContract(executable,args,options) {
  const group=process.platform !== 'win32' && !options.env?.VO_CI_ATTEMPT_DIR;
  const child=spawn(executable,args,{...options,detached:group});
  let closed=false;
  child.once('close',() => {closed=true;});
  return {child,stop(signal) {
    if (!child.pid || closed) return;
    try {group ? process.kill(-child.pid,signal) : child.kill(signal);}
    catch (error) {if (error.code !== 'ESRCH') throw error;}
  }};
}
