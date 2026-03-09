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

const TOKEN_STORAGE_KEY = 'vibe_studio_github_token';
const LEGACY_TOKEN_SESSION_KEY = 'vibe_studio_github_token_session';

export const github = writable<GitHubState>({
  token: null,
  user: null,
  isLoading: false,
  error: '',
});

export async function githubFetch(
  token: string | null,
  path: string,
  options: RequestInit = {},
): Promise<any> {
  const headers: Record<string, string> = {
    Accept: 'application/vnd.github+json',
    'X-GitHub-Api-Version': '2022-11-28',
    'Content-Type': 'application/json',
    ...(options.headers as Record<string, string> | undefined ?? {}),
  };
  if (token) {
    headers.Authorization = `Bearer ${token}`;
  }
  const res = await fetch(`https://api.github.com${path}`, {
    ...options,
    headers,
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
    localStorage.setItem(TOKEN_STORAGE_KEY, token);
    sessionStorage.removeItem(LEGACY_TOKEN_SESSION_KEY);
    github.update(s => ({ ...s, token, user, isLoading: false }));
  } catch (e: any) {
    github.update(s => ({ ...s, isLoading: false, error: String(e) }));
    throw e;
  }
}

export function logout(): void {
  localStorage.removeItem(TOKEN_STORAGE_KEY);
  sessionStorage.removeItem(LEGACY_TOKEN_SESSION_KEY);
  github.update(() => ({ token: null, user: null, isLoading: false, error: '' }));
}

export function loadSavedToken(): void {
  const savedFromLocal = localStorage.getItem(TOKEN_STORAGE_KEY);
  const savedFromLegacySession = sessionStorage.getItem(LEGACY_TOKEN_SESSION_KEY);
  const saved = savedFromLocal ?? savedFromLegacySession;
  if (!savedFromLocal && savedFromLegacySession) {
    localStorage.setItem(TOKEN_STORAGE_KEY, savedFromLegacySession);
  }
  sessionStorage.removeItem(LEGACY_TOKEN_SESSION_KEY);
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
  contentHash?: string;         // hash of content at last sync
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

// ── Remote metadata helpers ───────────────────────────────────────────────────

export interface RemoteMetadata {
  updatedAt: string;              // ISO timestamp from GitHub API
  files: Record<string, string>;  // filename → content
}

export async function fetchGistMetadata(
  token: string,
  gistId: string,
): Promise<RemoteMetadata> {
  const full = await githubFetch(token, `/gists/${gistId}`);
  const files: Record<string, string> = {};
  for (const [name, file] of Object.entries(full.files as Record<string, { content?: string }>)) {
    if (name === MANIFEST_FILENAME) continue;
    if ((file as any).content != null) files[name] = (file as any).content;
  }
  return {
    updatedAt: full.updated_at ?? full.created_at ?? '',
    files,
  };
}

export async function fetchRepoMetadata(
  token: string,
  owner: string,
  repo: string,
): Promise<RemoteMetadata> {
  const repoPath = `/repos/${owner}/${repo}`;
  const repoInfo = await githubFetch(token, repoPath);
  const updatedAt: string = repoInfo.pushed_at ?? repoInfo.updated_at ?? '';

  let branch = repoInfo.default_branch || 'main';
  let files: Record<string, string> = {};
  try {
    const { tree, truncated } = await githubFetch(
      token, `${repoPath}/git/trees/${branch}?recursive=1`,
    );
    if (!truncated) {
      for (const item of (tree as any[])) {
        if (item.type !== 'blob') continue;
        if (item.size > 1_000_000) continue;
        const blob = await githubFetch(token, `${repoPath}/git/blobs/${item.sha}`);
        files[item.path] = base64ToStr(blob.content);
      }
    }
  } catch {
    // Empty repo or network error — files stays empty
  }
  return { updatedAt, files };
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
      auto_init: true,  // ensures git DB is initialized so Git Data API always works
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

  // 1. Get current HEAD ref (just need parent SHA, not the tree)
  let parentSha: string | null = null;
  try {
    const ref = await githubFetch(token, `${repoPath}/git/ref/heads/main`);
    parentSha = ref.object.sha;
  } catch {
    // No commits on 'main' yet — parentSha stays null
  }

  // 2. Create blobs for each file.
  //    Truly uninitialized repos (auto_init:false, never pushed) return 409 here —
  //    bootstrap via Contents API and retry.
  const treeItems: { path: string; mode: string; type: string; sha: string }[] = [];
  for (const [path, content] of Object.entries(files)) {
    let blob: { sha: string };
    try {
      blob = await githubFetch(token, `${repoPath}/git/blobs`, {
        method: 'POST',
        body: JSON.stringify({ content: strToBase64(content), encoding: 'base64' }),
      });
    } catch (e: any) {
      if ((String(e.message ?? '')).toLowerCase().includes('empty')) {
        // Initialize the git DB via Contents API, then retry the whole push
        await githubFetch(token, `${repoPath}/contents/.gitkeep`, {
          method: 'PUT',
          body: JSON.stringify({ message: 'Initialize repository', content: strToBase64('') }),
        });
        return gitPushFiles(token, owner, repo, files, message);
      }
      throw e;
    }
    treeItems.push({ path, mode: '100644', type: 'blob', sha: blob.sha });
  }

  // 3. Create a fresh tree — no base_tree, so the commit contains exactly our files
  const tree = await githubFetch(token, `${repoPath}/git/trees`, {
    method: 'POST',
    body: JSON.stringify({ tree: treeItems }),
  });

  // 4. Create commit
  const commitBody: any = { message, tree: tree.sha };
  if (parentSha) commitBody.parents = [parentSha];
  const commit = await githubFetch(token, `${repoPath}/git/commits`, {
    method: 'POST',
    body: JSON.stringify(commitBody),
  });

  // 5. Update or create the ref
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
  token: string | null,
  owner: string,
  repo: string,
  ref?: string,
): Promise<Record<string, string>> {
  const repoPath = `/repos/${owner}/${repo}`;

  // Get tree recursively from default branch
  let branch = ref || 'main';
  if (!ref) {
    try {
      const repoInfo = await githubFetch(token, repoPath);
      branch = repoInfo.default_branch || 'main';
    } catch { /* use main */ }
  }

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
