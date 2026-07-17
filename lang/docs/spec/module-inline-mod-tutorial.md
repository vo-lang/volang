# Inline `vo.mod` Tutorial

An inline module keeps a small dependency-free program in one `.vo` file.
Its metadata uses the same TOML values as `vo.mod`, with a deliberately smaller
schema.

## 1. Choose the right form

| Form | External dependencies | Workspace | Public extension metadata | User-owned lock |
|---|---|---|---|---|
| Ad hoc file | No | No | No | No |
| Inline module | No | No | No | No |
| Project | Yes | Optional | Yes | For registry edges; a complete selected all-local workspace may stay lockless |

Use an ad hoc file for standard-library-only experiments. Use an inline module
when the file needs an explicit local identity or toolchain constraint. Create
a project when the program needs external dependencies, multiple files,
publication, workspaces, or extension metadata.

## 2. Minimal inline module

Create `hello.vo`:

```vo
/*vo:mod
module = "local/hello"
vo = "^0.1.0"
*/

package main

func main() {
    println("hello")
}
```

Then run, check, or build it:

```bash
vo run hello.vo
vo check hello.vo
vo build hello.vo -o hello.vob
```

`local/hello` identifies this ephemeral root. It cannot be published or
imported by another module.

## 3. External imports require a project

Inline metadata does not accept `[dependencies]`, and an inline file cannot
import an external module. Promote the file to a project, declare dependency
intent in `vo.mod`, run `vo mod sync`, and commit the generated `vo.lock`.
This gives every external graph one explicit, reviewable authority.

Dependency-free inline compilation takes the direct single-file path and
creates no generated `vo.mod` or `vo.lock`.

## 4. Placement and schema rules

The opening sentinel is reserved:

```text
/*vo:mod
```

It must appear in the leading comment region before the first source token.
Copyright headers and ordinary comments may precede it. That leading region
may contain only one reserved `/*vo:mod` block; matching text after the first
source token follows ordinary source-comment or string rules. The closing `*/`
ends the TOML body.

Inline metadata accepts exactly:

- `module`, which must be `local/<name>`;
- `vo`.

These project-only sections are rejected:

- `[dependencies]`;
- `[web]`;
- `[extension]` and its child tables;
- `[build]` and its child tables.

Unknown keys, a second reserved block in the leading comment region, and
malformed TOML are hard errors. Any leading `/*vo:...` directive other than
`/*vo:mod` is also an error.

## 5. Isolation

An inline module cannot live inside a directory tree already owned by
`vo.mod`. It never reads `vo.work`, dependency-local locks, or unpublished
repository state.

Inline compilation performs no registry discovery or dependency-cache
mutation.

## 6. Promote to a project

Promote the script when it needs any of these:

- more source files or packages;
- a published `github.com/...` identity;
- a committed dependency lock;
- `vo.work` source substitutions;
- dependencies, `[web]`, `[extension]`, or `[build]` metadata.

Create this layout:

```text
demo/
├── vo.mod
├── vo.lock        # required for registry-backed dependencies
└── main.vo
```

Move the TOML body into `vo.mod`, change `module` to the canonical published
path, remove the comment block, then run:

```bash
vo mod sync
vo mod fetch
vo mod verify
```

`sync` selects and writes the graph, `fetch` fills the authenticated cache, and
`verify` checks the result without changing project or cache state.
A selected `vo.work` may authorize a lockless project only when every reachable
dependency is a local member. Any missing or registry-backed edge requires the
complete v3 lock. Release verification always uses the locked declared graph.

## 7. Common diagnostics

### Inline block inside a project

The file already belongs to a normal module. Remove the block and declare its
dependencies in the project's `vo.mod`.

### Inline root must use `local/<name>`

Only ephemeral roots use inline metadata. Promote the file before assigning a
published path.

### Inline dependencies are unsupported

Create a project, move dependency declarations into `vo.mod`, and commit the
generated `vo.lock`.

See the full [`module protocol`](./module.md) for lock, workspace, release, and
authentication rules.
