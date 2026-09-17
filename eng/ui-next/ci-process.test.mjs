import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {once} from 'node:events';
import test from 'node:test';
import {spawnContract} from './ci-process.mjs';

const live='process.stdout.write("READY\\n");setInterval(() => {},1000);';
const module=new URL('./ci-process.mjs',import.meta.url).href;
const env={...process.env};delete env.VO_CI_ATTEMPT_DIR;
function kill(pid) {try {process.kill(pid,'SIGKILL');} catch (error) {if (error.code !== 'ESRCH') throw error;}}
async function exercise(t,child,stop) {
  let output='',finished=false;
  const closed=new Promise((resolve,reject) => {child.once('error',reject);child.once('close',code => {finished=true;resolve(code);});});
  t.after(() => {
    if (finished || !child.pid) return;
    kill(-child.pid);for (const match of output.matchAll(/PID (\d+)/g)) kill(Number(match[1]));
  });
  let timeout;
  try {
    const ready=new Promise(resolve => child.stdout.on('data',bytes => {output+=bytes;if(output.includes('READY')) resolve();}));
    await Promise.race([ready,new Promise((_,reject) => {timeout=setTimeout(() => reject(new Error('fixture did not start: '+output)),5000);})]);
    clearTimeout(timeout);assert.match(output,/PID \d+/);
    stop();
    // Descendants inherit stdout. close can complete only after the entire
    // group releases that pipe, even when the root is forcibly terminated.
    await Promise.race([closed,new Promise((_,reject) => {timeout=setTimeout(() => reject(new Error('a child escaped command cancellation')),2000);})]);
  } finally {clearTimeout(timeout);}
}

test('a CI stage remains inside the executor group on forced termination',{skip:process.platform === 'win32'},async t => {
  const source=`import {spawnContract} from ${JSON.stringify(module)};
    const stage=spawnContract(process.execPath,['-e',${JSON.stringify(live)}],{env:process.env,stdio:['ignore','inherit','inherit']});
    console.log('PID '+stage.child.pid);setInterval(() => {},1000);`;
  const child=spawn(process.execPath,['--input-type=module','-e',source],{detached:true,env:{...env,VO_CI_ATTEMPT_DIR:'fixture'},stdio:['ignore','pipe','pipe']});
  await exercise(t,child,() => kill(-child.pid));
});

test('a direct local stage cancels its descendants together',{skip:process.platform === 'win32'},async t => {
  const source=`const {spawn}=require('node:child_process');
    const child=spawn(process.execPath,['-e',${JSON.stringify(live)}],{stdio:'inherit'});
    console.log('PID '+child.pid);setInterval(() => {},1000);`;
  const stage=spawnContract(process.execPath,['-e',source],{env,stdio:['ignore','pipe','pipe']});
  await exercise(t,stage.child,() => stage.stop('SIGTERM'));
});

test('stopping a fully closed stage is harmless',{skip:process.platform === 'win32'},async () => {
  const stage=spawnContract(process.execPath,['-e',''],{env,stdio:'ignore'});
  const [code,signal]=await once(stage.child,'close');
  assert.equal(code,0);assert.equal(signal,null);
  stage.stop('SIGTERM');stage.stop('SIGKILL');
});
