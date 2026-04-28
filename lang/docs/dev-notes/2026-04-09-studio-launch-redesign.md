# Studio Launch Protocol Redesign

## Principle

A project is identified by a URL. One URL, one project.

## Project URL

A project URL is any URL that points to a Vo project:

| URL                                              | Meaning                       |
|--------------------------------------------------|-------------------------------|
| `https://github.com/vo-lang/BlockKart`          | GitHub repo, default branch   |
| `https://github.com/vo-lang/BlockKart/tree/main/sub` | GitHub repo, ref + subdir |
| `/Users/me/code/BlockKart`                      | Local path (native only)      |

That's it. A URL locates a project. Nothing else goes in it.

## Mode

Orthogonal to the project URL. Two modes:

- `dev` — full IDE (default for native)
- `runner` — run-only (default for web)

## Surface Areas

### Web

```
volang.dev/?proj=https://github.com/vo-lang/BlockKart
volang.dev/?proj=https://github.com/vo-lang/BlockKart&mode=dev
```

`proj` is the project URL. `mode` is optional (defaults to `runner` on web).

### CLI

```bash
./d.py studio-native https://github.com/vo-lang/BlockKart
./d.py studio-native /path/to/project
./d.py studio-native --runner https://github.com/vo-lang/BlockKart
./d.py studio https://github.com/vo-lang/BlockKart
```

The positional arg is the project URL. `--runner` overrides mode.

### Share URL

A share URL is just a web URL where the project URL contains a pinned commit:

```
volang.dev/?proj=https://github.com/vo-lang/BlockKart/tree/abc123&mode=runner
```

No separate share format. It's the same URL, just with a commit ref.

## Internal Types

```typescript
interface LaunchSpec {
  proj: string | null;     // project URL (null = workspace)
  mode: StudioMode;        // 'dev' | 'runner'
}
```

Parsed internally after resolution:

```typescript
type ProjectSource =
  | { kind: 'workspace' }
  | { kind: 'path'; path: string }
  | { kind: 'github_repo'; owner: string; repo: string; commit: string | null; subdir: string | null }
```

```typescript
interface BootstrapContext {
  workspaceRoot: string;
  launch: LaunchSpec | null;
  mode: StudioMode;
  platform: 'native' | 'wasm';
}
```

## Environment Variables (native)

- `STUDIO_PROJ` — project URL
- `STUDIO_MODE` — `dev` or `runner`
- `STUDIO_WORKSPACE` — workspace root override
