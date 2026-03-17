import type { Backend } from '../backend/backend';
import type {
  GitHubRemoteRef,
  GitHubUser,
  ManifestProject,
  ManagedProject,
  ProjectManifest,
  RemoteMetadata,
} from './types';

const API_ROOT = 'https://api.github.com';
const MANIFEST_FILENAME = '_vo_studio_manifest_.json';
const TOKEN_STORAGE_KEY = 'vo_studio_github_token';
const MANIFEST_GIST_ID_STORAGE_KEY = 'vo_studio_manifest_gist_id';

interface GitHubResponseErrorShape {
  message?: string;
  errors?: Array<{ message?: string }>;
}

export class GitHubRemoteClient {
  constructor(private readonly backend: Backend) {}

  loadStoredToken(): string | null {
    if (typeof localStorage === 'undefined') return null;
    return localStorage.getItem(TOKEN_STORAGE_KEY);
  }

  storeToken(token: string | null): void {
    if (typeof localStorage === 'undefined') return;
    if (token) {
      localStorage.setItem(TOKEN_STORAGE_KEY, token);
      return;
    }
    localStorage.removeItem(TOKEN_STORAGE_KEY);
  }

  async getViewer(token: string): Promise<GitHubUser> {
    const user = await this.requestJson<Record<string, unknown>>(token, 'GET', '/user');
    return {
      login: String(user.login ?? ''),
      avatarUrl: String(user.avatar_url ?? ''),
      htmlUrl: String(user.html_url ?? ''),
      name: user.name == null ? null : String(user.name),
    };
  }

  async loadManifest(token: string): Promise<{ gistId: string; manifest: ProjectManifest }> {
    let gistId = await this.findManifestGistId(token);
    if (!gistId) {
      const gist = await this.createGist(token, MANIFEST_FILENAME, JSON.stringify(emptyManifest(), null, 2), 'Vo Studio remote manifest');
      gistId = gist.id;
      this.cacheManifestGistId(gistId);
      return { gistId, manifest: emptyManifest() };
    }

    const gist = await this.requestJson<{ files?: Record<string, { content?: string }> }>(token, 'GET', `/gists/${gistId}`);
    const manifestFile = gist.files?.[MANIFEST_FILENAME];
    if (!manifestFile?.content) {
      return { gistId, manifest: emptyManifest() };
    }

    try {
      const parsed = JSON.parse(manifestFile.content) as ProjectManifest;
      if (parsed.v !== 1 || !Array.isArray(parsed.projects)) {
        return { gistId, manifest: emptyManifest() };
      }
      return { gistId, manifest: parsed };
    } catch {
      return { gistId, manifest: emptyManifest() };
    }
  }

  async saveManifest(token: string, gistId: string, manifest: ProjectManifest): Promise<void> {
    await this.updateGist(token, gistId, {
      [MANIFEST_FILENAME]: {
        content: JSON.stringify(manifest, null, 2),
      },
    });
    this.cacheManifestGistId(gistId);
  }

  async fetchRemoteMetadata(token: string, manifestProjects: ManifestProject[]): Promise<Map<string, RemoteMetadata>> {
    const entries = await Promise.all(manifestProjects.map(async (project) => {
      try {
        if (project.remote.kind === 'gist' && project.remote.gistId) {
          return [manifestKey(project.name, project.type), await this.fetchGistMetadata(token, project.remote.gistId)] as const;
        }
        if (project.remote.kind === 'repo' && project.remote.owner && project.remote.repo) {
          return [manifestKey(project.name, project.type), await this.fetchRepoMetadata(token, project.remote.owner, project.remote.repo)] as const;
        }
      } catch {
        return null;
      }
      return null;
    }));

    const result = new Map<string, RemoteMetadata>();
    for (const entry of entries) {
      if (!entry) continue;
      result.set(entry[0], entry[1]);
    }
    return result;
  }

  async fetchRemoteContent(token: string, project: ManagedProject): Promise<Record<string, string>> {
    if (!project.remote) return {};
    if (project.remote.kind === 'gist' && project.remote.gistId) {
      return this.fetchGistFiles(token, project.remote.gistId);
    }
    if (project.remote.kind === 'repo' && project.remote.owner && project.remote.repo) {
      return this.pullRepoFiles(token, project.remote.owner, project.remote.repo);
    }
    return {};
  }

  async createGist(token: string, filename: string, content: string, description: string): Promise<{ id: string }> {
    return this.requestJson(token, 'POST', '/gists', {
      description,
      public: false,
      files: {
        [filename]: { content },
      },
    });
  }

  async updateSingleFileGist(token: string, gistId: string, filename: string, content: string): Promise<void> {
    await this.updateGist(token, gistId, {
      [filename]: { content },
    });
  }

  async renameSingleFileGist(token: string, gistId: string, oldFilename: string, newFilename: string, content: string): Promise<void> {
    await this.updateGist(token, gistId, {
      [oldFilename]: {
        filename: newFilename,
        content,
      },
    });
  }

  async deleteRemoteProject(token: string, remote: GitHubRemoteRef): Promise<void> {
    if (remote.kind === 'gist' && remote.gistId) {
      await this.requestJson(token, 'DELETE', `/gists/${remote.gistId}`);
      return;
    }
    if (remote.kind === 'repo' && remote.owner && remote.repo) {
      await this.requestJson(token, 'DELETE', `/repos/${remote.owner}/${remote.repo}`);
      return;
    }
    throw new Error('Invalid remote source');
  }

  async fetchGistFiles(token: string, gistId: string): Promise<Record<string, string>> {
    const gist = await this.requestJson<{ files?: Record<string, { content?: string }> }>(token, 'GET', `/gists/${gistId}`);
    const result: Record<string, string> = {};
    for (const [name, file] of Object.entries(gist.files ?? {})) {
      if (name === MANIFEST_FILENAME) continue;
      if (file.content != null) result[name] = file.content;
    }
    return result;
  }

  async fetchGistMetadata(token: string, gistId: string): Promise<RemoteMetadata> {
    const gist = await this.requestJson<{ updated_at?: string; created_at?: string }>(token, 'GET', `/gists/${gistId}`);
    return {
      updatedAt: gist.updated_at ?? gist.created_at ?? '',
    };
  }

  async createRepo(token: string, name: string, description = ''): Promise<{ owner: { login: string }; name: string }> {
    return this.requestJson(token, 'POST', '/user/repos', {
      name,
      description: description || 'Created by Vo Studio',
      private: false,
      auto_init: true,
    });
  }

  async renameRepo(token: string, owner: string, repo: string, newName: string): Promise<{ owner: { login: string }; name: string }> {
    return this.requestJson(token, 'PATCH', `/repos/${owner}/${repo}`, {
      name: newName,
    });
  }

  async fetchRepoMetadata(token: string, owner: string, repo: string): Promise<RemoteMetadata> {
    const repoInfo = await this.requestJson<Record<string, unknown>>(token, 'GET', `/repos/${owner}/${repo}`);
    return {
      updatedAt: String(repoInfo.pushed_at ?? repoInfo.updated_at ?? ''),
    };
  }

  async pushRepoFiles(token: string, owner: string, repo: string, files: Record<string, string>, message = 'Update via Vo Studio'): Promise<void> {
    const repoPath = `/repos/${owner}/${repo}`;
    let defaultBranch = 'main';
    try {
      const repoInfo = await this.requestJson<Record<string, unknown>>(token, 'GET', repoPath);
      defaultBranch = String(repoInfo.default_branch ?? 'main');
    } catch {
      defaultBranch = 'main';
    }
    const encodedBranch = encodeURIComponent(defaultBranch);
    let parentSha: string | null = null;
    try {
      const ref = await this.requestJson<{ object?: { sha?: string } }>(token, 'GET', `${repoPath}/git/ref/heads/${encodedBranch}`);
      parentSha = ref.object?.sha ?? null;
    } catch {
      parentSha = null;
    }

    const treeItems: Array<{ path: string; mode: string; type: string; sha: string }> = [];
    for (const [path, content] of Object.entries(files)) {
      try {
        const blob = await this.requestJson<{ sha: string }>(token, 'POST', `${repoPath}/git/blobs`, {
          content: encodeBase64Utf8(content),
          encoding: 'base64',
        });
        treeItems.push({ path, mode: '100644', type: 'blob', sha: blob.sha });
      } catch (error) {
        const text = String(error instanceof Error ? error.message : error).toLowerCase();
        if (text.includes('empty')) {
          await this.requestJson(token, 'PUT', `${repoPath}/contents/.gitkeep`, {
            message: 'Initialize repository',
            content: encodeBase64Utf8(''),
          });
          return this.pushRepoFiles(token, owner, repo, files, message);
        }
        throw error;
      }
    }

    const tree = await this.requestJson<{ sha: string }>(token, 'POST', `${repoPath}/git/trees`, {
      tree: treeItems,
    });
    const commitBody: Record<string, unknown> = {
      message,
      tree: tree.sha,
    };
    if (parentSha) commitBody.parents = [parentSha];
    const commit = await this.requestJson<{ sha: string }>(token, 'POST', `${repoPath}/git/commits`, commitBody);

    if (parentSha) {
      await this.requestJson(token, 'PATCH', `${repoPath}/git/refs/heads/${encodedBranch}`, {
        sha: commit.sha,
      });
      return;
    }

    await this.requestJson(token, 'POST', `${repoPath}/git/refs`, {
      ref: `refs/heads/${defaultBranch}`,
      sha: commit.sha,
    });
  }

  async pullRepoFiles(token: string, owner: string, repo: string, ref?: string): Promise<Record<string, string>> {
    const repoPath = `/repos/${owner}/${repo}`;
    let branch = ref ?? 'main';
    if (!ref) {
      try {
        const repoInfo = await this.requestJson<Record<string, unknown>>(token, 'GET', repoPath);
        branch = String(repoInfo.default_branch ?? 'main');
      } catch {
        branch = 'main';
      }
    }

    const tree = await this.requestJson<{ tree?: Array<Record<string, unknown>>; truncated?: boolean }>(
      token,
      'GET',
      `${repoPath}/git/trees/${encodeURIComponent(branch)}?recursive=1`,
    );
    if (tree.truncated) throw new Error('Repository too large to pull');

    const result: Record<string, string> = {};
    for (const item of tree.tree ?? []) {
      if (item.type !== 'blob') continue;
      const size = Number(item.size ?? 0);
      if (size > 1_000_000) continue;
      const path = String(item.path ?? '');
      const sha = String(item.sha ?? '');
      if (!path || !sha) continue;
      const blob = await this.requestJson<{ content?: string }>(token, 'GET', `${repoPath}/git/blobs/${sha}`);
      if (blob.content != null) {
        result[path] = decodeBase64Utf8(blob.content);
      }
    }
    return result;
  }

  private async updateGist(
    token: string,
    gistId: string,
    files: Record<string, { content?: string; filename?: string; }>,
  ): Promise<void> {
    await this.requestJson(token, 'PATCH', `/gists/${gistId}`, { files });
  }

  private async findManifestGistId(token: string): Promise<string | null> {
    const cached = this.readCachedManifestGistId();
    if (cached) {
      try {
        const gist = await this.requestJson<{ files?: Record<string, unknown> }>(token, 'GET', `/gists/${cached}`);
        if (gist.files?.[MANIFEST_FILENAME]) {
          return cached;
        }
      } catch {
        this.cacheManifestGistId(null);
      }
    }

    for (let page = 1; page <= 5; page += 1) {
      const gists = await this.requestJson<Array<{ id: string; files?: Record<string, unknown> }>>(token, 'GET', `/gists?per_page=100&page=${page}`);
      if (gists.length === 0) break;
      for (const gist of gists) {
        if (gist.files?.[MANIFEST_FILENAME]) {
          this.cacheManifestGistId(gist.id);
          return gist.id;
        }
      }
    }

    return null;
  }

  private readCachedManifestGistId(): string | null {
    if (typeof localStorage === 'undefined') return null;
    return localStorage.getItem(MANIFEST_GIST_ID_STORAGE_KEY);
  }

  private cacheManifestGistId(gistId: string | null): void {
    if (typeof localStorage === 'undefined') return;
    if (gistId) {
      localStorage.setItem(MANIFEST_GIST_ID_STORAGE_KEY, gistId);
      return;
    }
    localStorage.removeItem(MANIFEST_GIST_ID_STORAGE_KEY);
  }

  private async requestJson<T>(token: string, method: string, path: string, body?: unknown): Promise<T> {
    const result = await this.backend.httpRequest(method, `${API_ROOT}${path}`, {
      headers: {
        Accept: 'application/vnd.github+json',
        Authorization: `Bearer ${token}`,
        'X-GitHub-Api-Version': '2022-11-28',
        'Content-Type': 'application/json',
      },
      body: body == null ? undefined : JSON.stringify(body),
      timeoutMs: 30_000,
    });

    const isJson = result.body.trim().startsWith('{') || result.body.trim().startsWith('[');
    const payload = isJson ? safeParseJson(result.body) : null;
    if (result.status < 200 || result.status >= 300) {
      const errorPayload = payload as GitHubResponseErrorShape | null;
      const nested = errorPayload?.errors?.map((entry) => entry.message).filter(Boolean).join('; ');
      const message = errorPayload?.message || nested || result.body || `${method} ${path} failed (${result.status})`;
      throw new Error(message);
    }

    if (payload == null) {
      return {} as T;
    }
    return payload as T;
  }
}

export function buildManifest(projects: ManagedProject[]): ProjectManifest {
  return {
    v: 1,
    projects: projects
      .filter((project): project is ManagedProject & { remote: GitHubRemoteRef } => project.remote != null)
      .map((project) => ({
        name: project.name,
        type: project.type,
        remote: project.remote,
        pushedAt: project.pushedAt ?? new Date().toISOString(),
        contentHash: project.syncedHash ?? project.currentRemoteHash ?? undefined,
        hasGui: project.hasGui || undefined,
      })),
  };
}

export function emptyManifest(): ProjectManifest {
  return { v: 1, projects: [] };
}

export function manifestKey(name: string, type: string): string {
  return `${type}:${name}`;
}

function safeParseJson(text: string): unknown {
  try {
    return JSON.parse(text);
  } catch {
    return null;
  }
}

function encodeBase64Utf8(value: string): string {
  const bytes = new TextEncoder().encode(value);
  let binary = '';
  for (const byte of bytes) {
    binary += String.fromCharCode(byte);
  }
  return btoa(binary);
}

function decodeBase64Utf8(value: string): string {
  const normalized = value.replace(/\n/g, '');
  const binary = atob(normalized);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i += 1) {
    bytes[i] = binary.charCodeAt(i);
  }
  return new TextDecoder().decode(bytes);
}
