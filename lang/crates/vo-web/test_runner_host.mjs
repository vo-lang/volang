import { mkdtemp, writeFile, mkdir, symlink, readFile, stat, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createServer, createConnection } from 'node:net';
import { createSocket } from 'node:dgram';

export const hostPlatform = process.platform === 'darwin' ? 'macos'
  : process.platform === 'win32' ? 'windows' : process.platform;
const capabilities = new Set(['symlink', 'loopback']);

export function validateHostPlan(plan) {
  if (plan.host_platform !== hostPlatform || !['linux', 'macos', 'windows'].includes(hostPlatform)) {
    throw new Error(`test host ${plan.host_platform} differs from current ${hostPlatform}`);
  }
  for (const job of plan.jobs) {
    if (!Array.isArray(job.requires_host) || new Set(job.requires_host).size !== job.requires_host.length
        || job.requires_host.some(name => !capabilities.has(name))) {
      throw new Error(`${job.id} has missing, unknown or duplicate host requirements`);
    }
    if (job.resource_group !== null && (typeof job.resource_group !== 'string'
        || !/^[a-z0-9-]{1,64}$/.test(job.resource_group))) {
      throw new Error(`${job.id} has invalid resource group`);
    }
    if (Object.keys(job.env ?? {}).some(key => key.toUpperCase().startsWith('VO_TEST_HOST_'))) {
      throw new Error(`${job.id} overrides runner-owned host evidence`);
    }
  }
}

function observed(job) {
  return new Set([...job.requires_host, ...(job.tags?.includes('symlink') ? ['symlink'] : [])]);
}

export async function probeHost(plan) {
  validateHostPlan(plan);
  const probes = {};
  for (const name of new Set(plan.jobs.flatMap(job => [...observed(job)]))) {
    try {
      await (name === 'symlink' ? probeSymlink() : probeLoopback());
      probes[name] = { status: 'supported', detail: 'independent host probe completed' };
    } catch (error) {
      probes[name] = { status: (name === 'loopback' || error.capabilityOperation === true)
        && ['EACCES', 'EPERM', 'ENOSYS', 'ENOTSUP'].includes(error.code)
        ? 'unavailable' : 'error', detail: String(error.message ?? error).slice(0, 1024) };
    }
  }
  return probes;
}

export function jobHost(job, probes) {
  const host_capabilities = Object.fromEntries([...observed(job)].map(name => [name, probes[name]]));
  for (const name of observed(job)) {
    const probe = probes[name];
    if (!probe || probe.status === 'error') {
      return { host_capabilities, failure_kind: 'infrastructure', error: `host probe ${name} failed: ${probe?.detail ?? 'missing'}` };
    }
    if (job.requires_host.includes(name) && probe.status !== 'supported') {
      return { host_capabilities, failure_kind: 'portability', error: `required host capability ${name} is unavailable: ${probe.detail}` };
    }
  }
  return { host_capabilities, failure_kind: null, error: '' };
}

async function probeSymlink() {
  const root = await mkdtemp(join(tmpdir(), 'vo-host-capability-'));
  try {
    await writeFile(join(root, 'target'), 'probe');
    await mkdir(join(root, 'directory'));
    try {
      await symlink('target', join(root, 'file-link'), 'file');
      await symlink('directory', join(root, 'directory-link'), 'dir');
    } catch (error) { error.capabilityOperation = true; throw error; }
    if (await readFile(join(root, 'file-link'), 'utf8') !== 'probe'
        || !(await stat(join(root, 'directory-link'))).isDirectory()) {
      throw new Error('symlink probe did not resolve its own targets');
    }
  } finally { await rm(root, { recursive: true, force: true }); }
}

async function probeLoopback() {
  await new Promise((resolve, reject) => {
    const server = createServer();
    let client;
    let peer;
    let finished = false;
    const finish = error => {
      if (finished) return;
      finished = true;
      clearTimeout(timer);
      client?.destroy(); peer?.destroy();
      server.close(() => error ? reject(error) : resolve());
    };
    const timer = setTimeout(() => finish(new Error('loopback TCP probe timed out')), 2000);
    server.on('error', finish);
    server.on('connection', socket => { peer = socket; finish(); });
    server.listen(0, '127.0.0.1', () => {
      client = createConnection(server.address().port, '127.0.0.1');
      client.on('error', finish);
    });
  });
  await new Promise((resolve, reject) => {
    const receiver = createSocket('udp4');
    const sender = createSocket('udp4');
    let finished = false;
    const finish = error => {
      if (finished) return;
      finished = true;
      clearTimeout(timer);
      for (const socket of [receiver, sender]) { try { socket.close(); } catch {} }
      error ? reject(error) : resolve();
    };
    const timer = setTimeout(() => finish(new Error('loopback UDP probe timed out')), 2000);
    receiver.on('error', finish); sender.on('error', finish);
    receiver.on('message', (bytes, address) => finish(bytes.toString() === 'probe'
      && address.address === '127.0.0.1' && address.port === sender.address().port
      ? null : new Error('loopback probe payload or sender differs')));
    receiver.bind(0, '127.0.0.1', () => sender.send('probe', receiver.address().port, '127.0.0.1', error => { if (error) finish(error); }));
  });
}
