import { writable } from 'svelte/store';

export interface GitHubUser {
  login: string;
  name: string | null;
  avatar_url: string;
  public_repos: number;
  followers: number;
}

export interface GitHubState {
  token: string | null;
  user: GitHubUser | null;
  isLoading: boolean;
  error: string;
}

const STORAGE_KEY = 'vibe_studio_github_token';

export const github = writable<GitHubState>({
  token: null,
  user: null,
  isLoading: false,
  error: '',
});

export async function githubFetch(
  token: string,
  path: string,
  options: RequestInit = {},
): Promise<any> {
  const res = await fetch(`https://api.github.com${path}`, {
    ...options,
    headers: {
      Accept: 'application/vnd.github+json',
      Authorization: `Bearer ${token}`,
      'X-GitHub-Api-Version': '2022-11-28',
      'Content-Type': 'application/json',
      ...(options.headers ?? {}),
    },
  });
  if (!res.ok) {
    const body = await res.json().catch(() => ({}));
    throw new Error(body.message ?? `GitHub API error ${res.status}`);
  }
  if (res.status === 204) return null;
  return res.json();
}

export async function loginWithToken(token: string): Promise<void> {
  github.update(s => ({ ...s, isLoading: true, error: '' }));
  try {
    const user: GitHubUser = await githubFetch(token, '/user');
    localStorage.setItem(STORAGE_KEY, token);
    github.update(s => ({ ...s, token, user, isLoading: false }));
  } catch (e: any) {
    github.update(s => ({ ...s, isLoading: false, error: String(e) }));
    throw e;
  }
}

export function logout(): void {
  localStorage.removeItem(STORAGE_KEY);
  github.update(() => ({ token: null, user: null, isLoading: false, error: '' }));
}

export function loadSavedToken(): void {
  const saved = localStorage.getItem(STORAGE_KEY);
  if (saved) {
    loginWithToken(saved).catch(() => logout());
  }
}

export async function createGist(
  token: string,
  filename: string,
  content: string,
  description: string = '',
): Promise<{ id: string; html_url: string }> {
  return githubFetch(token, '/gists', {
    method: 'POST',
    body: JSON.stringify({
      description,
      public: false,
      files: { [filename]: { content } },
    }),
  });
}

export async function renameRepo(
  token: string,
  owner: string,
  repo: string,
  newName: string,
): Promise<{ owner: { login: string }; name: string }> {
  return githubFetch(token, `/repos/${owner}/${repo}`, {
    method: 'PATCH',
    body: JSON.stringify({ name: newName }),
  });
}

export async function updateGist(
  token: string,
  gistId: string,
  filename: string,
  content: string,
): Promise<void> {
  await githubFetch(token, `/gists/${gistId}`, {
    method: 'PATCH',
    body: JSON.stringify({ files: { [filename]: { content } } }),
  });
}

export async function renameGistFile(
  token: string,
  gistId: string,
  oldFilename: string,
  newFilename: string,
): Promise<void> {
  if (oldFilename === newFilename) return;
  await githubFetch(token, `/gists/${gistId}`, {
    method: 'PATCH',
    body: JSON.stringify({
      files: {
        [oldFilename]: { filename: newFilename },
      },
    }),
  });
}

export function strToBase64(str: string): string {
  const bytes = new TextEncoder().encode(str);
  let binary = '';
  for (let i = 0; i < bytes.length; i++) binary += String.fromCharCode(bytes[i]);
  return btoa(binary);
}

export function base64ToStr(b64: string): string {
  const clean = b64.replace(/\n/g, '');
  const binary = atob(clean);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
  return new TextDecoder().decode(bytes);
}

// ── Manifest Gist ─────────────────────────────────────────────────────────────

const MANIFEST_FILENAME = '_vibe_studio_.json';
const MANIFEST_GIST_ID_KEY = 'vibe_studio_manifest_gist_id';

export interface RemoteSource {
  kind: 'gist' | 'repo';
  gistId?: string;
  owner?: string;
  repo?: string;
}

export interface ManifestProject {
  name: string;
  type: 'single' | 'multi';
  remote: RemoteSource;
  pushedAt: string;
}

export interface Manifest {
  v: number;
  projects: ManifestProject[];
}

function emptyManifest(): Manifest {
  return { v: 1, projects: [] };
}

export async function findManifestGistId(token: string): Promise<string | null> {
  // Check localStorage first
  const cached = localStorage.getItem(MANIFEST_GIST_ID_KEY);
  if (cached) {
    // Verify it still exists
    try {
      const gist = await githubFetch(token, `/gists/${cached}`);
      if (gist.files && gist.files[MANIFEST_FILENAME]) return cached;
    } catch {
      // Gist was deleted or token changed, fall through to search
    }
    localStorage.removeItem(MANIFEST_GIST_ID_KEY);
  }

  // Search user's gists for the manifest file
  let page = 1;
  while (page <= 5) {
    const gists = await githubFetch(token, `/gists?per_page=100&page=${page}`);
    if (!Array.isArray(gists) || gists.length === 0) break;
    for (const g of gists) {
      if (g.files && g.files[MANIFEST_FILENAME]) {
        localStorage.setItem(MANIFEST_GIST_ID_KEY, g.id);
        return g.id;
      }
    }
    page++;
  }
  return null;
}

export async function loadManifest(token: string): Promise<{ gistId: string; manifest: Manifest }> {
  let gistId = await findManifestGistId(token);

  if (!gistId) {
    // Create manifest gist
    const result = await createGist(token, MANIFEST_FILENAME, JSON.stringify(emptyManifest(), null, 2), 'Vibe Studio project manifest');
    gistId = result.id;
    localStorage.setItem(MANIFEST_GIST_ID_KEY, gistId);
    return { gistId, manifest: emptyManifest() };
  }

  const full = await githubFetch(token, `/gists/${gistId}`);
  const file = full.files[MANIFEST_FILENAME];
  if (!file || !file.content) {
    return { gistId, manifest: emptyManifest() };
  }

  try {
    const manifest: Manifest = JSON.parse(file.content);
    return { gistId, manifest };
  } catch {
    return { gistId, manifest: emptyManifest() };
  }
}

export async function saveManifest(token: string, gistId: string, manifest: Manifest): Promise<void> {
  await updateGist(token, gistId, MANIFEST_FILENAME, JSON.stringify(manifest, null, 2));
}

export async function fetchGistFiles(
  token: string,
  gistId: string,
): Promise<Record<string, string>> {
  const full = await githubFetch(token, `/gists/${gistId}`);
  const result: Record<string, string> = {};
  for (const [name, file] of Object.entries(full.files as Record<string, { content?: string }>)) {
    if (name === MANIFEST_FILENAME) continue;
    if (file.content != null) result[name] = file.content;
  }
  return result;
}

// ── Git Data API (Repo push/pull) ─────────────────────────────────────────────

export async function createRepo(
  token: string,
  name: string,
  description: string = '',
): Promise<{ full_name: string; owner: { login: string }; name: string }> {
  return githubFetch(token, '/user/repos', {
    method: 'POST',
    body: JSON.stringify({
      name,
      description: description || 'Created by Vibe Studio',
      private: false,
      auto_init: false,
    }),
  });
}

export async function gitPushFiles(
  token: string,
  owner: string,
  repo: string,
  files: Record<string, string>,
  message: string = 'Update via Vibe Studio',
): Promise<void> {
  const repoPath = `/repos/${owner}/${repo}`;

  // 1. Get current HEAD ref to find parent commit
  let parentSha: string | null = null;
  let baseTreeSha: string | null = null;
  try {
    const ref = await githubFetch(token, `${repoPath}/git/ref/heads/main`);
    parentSha = ref.object.sha;
    const parentCommit = await githubFetch(token, `${repoPath}/git/commits/${parentSha}`);
    baseTreeSha = parentCommit.tree.sha;
  } catch {
    // New repo with no commits yet — that's fine, parentSha stays null
  }

  // 2. Create blobs for each file
  const treeItems: { path: string; mode: string; type: string; sha: string }[] = [];
  for (const [path, content] of Object.entries(files)) {
    const blob = await githubFetch(token, `${repoPath}/git/blobs`, {
      method: 'POST',
      body: JSON.stringify({ content: strToBase64(content), encoding: 'base64' }),
    });
    treeItems.push({ path, mode: '100644', type: 'blob', sha: blob.sha });
  }

  // 3. Create tree
  const treeBody: any = { tree: treeItems };
  if (baseTreeSha) treeBody.base_tree = baseTreeSha;
  const tree = await githubFetch(token, `${repoPath}/git/trees`, {
    method: 'POST',
    body: JSON.stringify(treeBody),
  });

  // 4. Create commit
  const commitBody: any = { message, tree: tree.sha };
  if (parentSha) commitBody.parents = [parentSha];
  const commit = await githubFetch(token, `${repoPath}/git/commits`, {
    method: 'POST',
    body: JSON.stringify(commitBody),
  });

  // 5. Update ref (or create it)
  if (parentSha) {
    await githubFetch(token, `${repoPath}/git/refs/heads/main`, {
      method: 'PATCH',
      body: JSON.stringify({ sha: commit.sha, force: true }),
    });
  } else {
    await githubFetch(token, `${repoPath}/git/refs`, {
      method: 'POST',
      body: JSON.stringify({ ref: 'refs/heads/main', sha: commit.sha }),
    });
  }
}

export async function gitPullFiles(
  token: string,
  owner: string,
  repo: string,
): Promise<Record<string, string>> {
  const repoPath = `/repos/${owner}/${repo}`;

  // Get tree recursively from default branch
  let branch = 'main';
  try {
    const repoInfo = await githubFetch(token, repoPath);
    branch = repoInfo.default_branch || 'main';
  } catch { /* use main */ }

  const { tree, truncated } = await githubFetch(
    token, `${repoPath}/git/trees/${branch}?recursive=1`,
  );
  if (truncated) throw new Error('Repository too large to pull');

  const result: Record<string, string> = {};
  for (const item of (tree as any[])) {
    if (item.type !== 'blob') continue;
    // Skip binary-looking files (only pull text)
    if (item.size > 1_000_000) continue;
    const blob = await githubFetch(token, `${repoPath}/git/blobs/${item.sha}`);
    result[item.path] = base64ToStr(blob.content);
  }
  return result;
}
