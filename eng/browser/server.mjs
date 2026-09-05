import { tmpdir } from 'node:os';
import { spawnSync } from 'node:child_process';
import { mkdirSync } from 'node:fs';
import { readFile, readdir, stat, mkdtemp, rm } from 'node:fs/promises';
import { createServer } from 'node:http';
import { extname, join, resolve, sep, relative } from 'node:path';

export function compileProject(project, output, bundleEntry) {
  if (bundleEntry !== null) {
    mkdirSync(output, { recursive: true });
    const executableName = process.platform === "win32" ? "rolldown.exe" : "rolldown";
    const executable = join(project, "node_modules", ".bin", executableName);
    const result = spawnSync(executable, [
      resolve(bundleEntry),
      "--format",
      "esm",
      "--platform",
      "browser",
      "--file",
      join(output, "browser_smoke.js"),
      "--transform.target",
      "es2022",
    ], {
      cwd: project,
      encoding: "utf8",
      env: process.env,
    });
    if (result.error !== undefined) throw result.error;
    if (result.status !== 0) {
      throw new Error(`browser smoke bundling failed:\n${result.stdout}${result.stderr}`);
    }
    return;
  }
  const executable = [join(project, 'node_modules', 'typescript', 'bin', 'tsc')].find((candidate) => {
    const result = spawnSync(process.execPath, [candidate, "--version"], {
      cwd: project,
      encoding: "utf8",
      env: process.env,
    });
    return result.status === 0;
  });
  if (executable === undefined) {
    throw new Error("TypeScript compiler was not found in the smoke project");
  }
  const result = spawnSync(process.execPath, [executable,
    "--project",
    join(project, "tsconfig.json"),
    "--noEmit",
    "false",
    "--declaration",
    "false",
    "--declarationMap",
    "false",
    "--sourceMap",
    "false",
    "--outDir",
    output,
  ], {
    cwd: project,
    encoding: "utf8",
    env: process.env,
  });
  if (result.error !== undefined) throw result.error;
  if (result.status !== 0) {
    throw new Error(`TypeScript smoke compilation failed:\n${result.stdout}${result.stderr}`);
  }
}

export async function findUniqueFile(root, name) {
  const matches = [];
  async function walk(directory) {
    for (const entry of await readdir(directory, { withFileTypes: true })) {
      const path = join(directory, entry.name);
      if (entry.isDirectory()) await walk(path);
      else if (entry.isFile() && entry.name === name) matches.push(path);
    }
  }
  await walk(root);
  if (matches.length !== 1) {
    throw new Error(`expected one ${name} in compiled output, found ${matches.length}`);
  }
  return matches[0];
}

export async function startServer(root, indexHtml, crossOriginIsolated) {
  const httpServer = createServer(async (request, response) => {
    try {
      const requestUrl = new URL(request.url ?? "/", "http://127.0.0.1");
      if (requestUrl.pathname === "/" || requestUrl.pathname === "/browser-smoke.html") {
        response.writeHead(200, {
          "content-type": "text/html; charset=utf-8",
          "cache-control": "no-store",
          ...(crossOriginIsolated ? {
            "cross-origin-opener-policy": "same-origin",
            "cross-origin-embedder-policy": "require-corp",
          } : {}),
        });
        response.end(indexHtml);
        return;
      }
      let candidate = resolve(root, `.${decodeURIComponent(requestUrl.pathname)}`);
      if (candidate !== root && !candidate.startsWith(`${root}${sep}`)) {
        response.writeHead(403);
        response.end("forbidden");
        return;
      }
      let metadata = await stat(candidate);
      if (metadata.isDirectory()) {
        candidate = join(candidate, "index.html");
        metadata = await stat(candidate);
      }
      if (!metadata.isFile()) throw new Error("not a file");
      response.writeHead(200, {
        "content-type": mimeType(candidate),
        "cache-control": "no-store",
        ...(crossOriginIsolated ? { "cross-origin-resource-policy": "same-origin" } : {}),
      });
      response.end(await readFile(candidate));
    } catch {
      response.writeHead(404);
      response.end("not found");
    }
  });
  await new Promise((done, reject) => {
    httpServer.once("error", reject);
    httpServer.listen(0, "127.0.0.1", done);
  });
  return httpServer;
}

function mimeType(path) {
  switch (extname(path)) {
    case ".js":
      return "text/javascript; charset=utf-8";
    case ".json":
      return "application/json; charset=utf-8";
    case ".html":
      return "text/html; charset=utf-8";
    case ".css":
      return "text/css; charset=utf-8";
    case ".svg":
      return "image/svg+xml";
    case ".webmanifest":
      return "application/manifest+json; charset=utf-8";
    case ".wasm":
      return "application/wasm";
    default:
      return "application/octet-stream";
  }
}

function toUrlPath(path) {
  return path.split(sep).join("/");
}

export async function prepareApplication(options) {
  if (options.baseURL) return { url: options.baseURL, close: async () => {} };
  const temporary = await mkdtemp(join(tmpdir(), 'volang-browser-'));
  let server;
  try {
    const project = options.projectRoot;
    let html = await readFile(join(project, options.html), 'utf8');
    const root = options.staticRoot === null ? join(temporary, 'compiled') : project;
    if (options.staticRoot === null) {
      compileProject(project, root, options.bundleEntry);
      const smoke = await findUniqueFile(root, 'browser_smoke.js');
      html = html.replace(/\/src\/browser_smoke\.ts/g, `/${toUrlPath(relative(root, smoke))}`);
    }
    server = await startServer(root, html, options.staticRoot === null);
    return {
      url: `http://127.0.0.1:${server.address().port}/`,
      close: async () => {
        await new Promise(done => server.close(done));
        await rm(temporary, { recursive: true, force: true });
      },
    };
  } catch (error) {
    if (server) await new Promise(done => server.close(done));
    await rm(temporary, { recursive: true, force: true });
    throw error;
  }
}
