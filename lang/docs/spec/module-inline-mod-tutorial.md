# Inline `vo.mod` Tutorial

This document is a practical companion to spec Section 5.6 and Section 10.2.
It explains when to use inline module metadata, how the current CLI behaves, and when you should promote a script into a real project.

## 1. What inline `vo.mod` is for

Use inline `vo.mod` metadata when all of the following are true:

- You want to keep the program in a single `.vo` file.
- The file is not inside a directory tree that already has `vo.mod`.
- You may need external published dependencies.
- You do not want to commit a user-owned `vo.mod` and `vo.lock` yet.

An inline mod turns one source file into a **single-file ephemeral module**.
The module gets a reserved identity in the `local/<name>` namespace and can declare `require` lines directly in the source file.

## 2. Ad hoc vs inline mod vs real project

| Form | External deps | `vo.work` | `replace` | `vo.ext.toml` | Committed `vo.mod` / `vo.lock` |
|---|---|---|---|---|---|
| Ad hoc file | No | No | No | No | No |
| Inline mod file | Yes | No | No | No | No |
| Real project | Yes | Yes, if present | Yes | Yes | Yes |

Use an **ad hoc file** when the program only needs the standard library.
Use an **inline mod file** when you still want a single file but need external modules.
Use a **real project** when the code grows beyond one file or needs normal module-level features.

## 3. The smallest inline-mod script

Create `hello.vo`:

```vo
/*vo:mod
module local/hello
vo ^0.1.0
*/

package main

func main() {
    println("hello from an inline-mod script")
}
```

Run it:

```bash
vo run hello.vo
```

Build bytecode:

```bash
vo build hello.vo -o hello.vob
```

Check it without running:

```bash
vo check hello.vo
```

The `module local/hello` line names the ephemeral module.
It is not a published module path and must not be imported by any other module.

## 4. Adding external dependencies

When you need external modules, add `require` lines inside the block:

```vo
/*vo:mod
module local/demo
vo ^0.1.0
require github.com/example/greeter ^1.2.0
*/

package main

import "github.com/example/greeter"

func main() {
    greeter.Hello()
}
```

Replace `github.com/example/greeter` with a real published module path and a real version constraint.

On the current native CLI, `vo run`, `vo build`, and `vo check` all go through the single-file auto-install path for inline-mod files.
That means the toolchain will:

- Parse the inline block.
- Resolve the declared `require` graph.
- Materialize a toolchain-owned ephemeral `vo.mod` and `vo.lock` in the cache.
- Download any missing locked modules into the shared module cache.
- Compile the source file against that frozen graph.

If resolution fails, the command fails hard.
There is no fallback to repository trees, unpublished snapshots, or local workspace overrides.

## 5. Cache layout

Inline-mod files do not create a user-visible `vo.mod` or `vo.lock` next to the script.
Instead, the toolchain may create a cache-local ephemeral project.

On the current native CLI, the cache is usually rooted at:

```text
$HOME/.vo/mod/
```

The inline-mod cache layout looks like this:

```text
$HOME/.vo/mod/
  ephemeral/
    <sha256-of-canonical-inline-body>/
      vo.mod
      vo.lock
  github.com/<owner>/<repo>/.../<version>/
```

Important details:

- The ephemeral cache key is derived from the canonical inline `module` / `vo` / `require` content.
- Reordering equivalent `require` lines still yields the same canonical ephemeral `vo.mod`.
- Changing the inline dependency set produces a different cache entry.
- Compile-cache invalidation also tracks the resolved dependency state, not only the file bytes.

Treat the cache as toolchain-owned state.
Do not edit the generated files by hand.

## 6. Reserved-prefix contract

The prefix `/*vo:` is reserved at the start of a file.
Only `/*vo:mod` is valid there.

These are all hard errors:

- A leading block comment such as `/*vo:script`.
- A second `/*vo:mod` block in the same file.
- Duplicate `module` directives.
- Duplicate `vo` directives.
- Duplicate `require` entries for the same module path.
- `replace` directives inside the inline block.
- `local/*` paths inside `require`.

If you want an ordinary comment at the top of the file, do not start it with `/*vo:`.

## 7. Restrictions that matter in practice

Inline-mod files are intentionally narrower than real projects.

They must not:

- live inside a directory tree that already has `vo.mod`
- use `vo.work`
- declare `replace`
- coexist with `vo.ext.toml` in the same directory
- define a multi-package module layout

Those restrictions keep single-file execution unambiguous.
The file is either:

- an ad hoc program
- a single-file ephemeral module
- or a normal project file

It is never more than one at the same time.

## 8. When to promote the script into a real project

Promote the script when any of these become true:

- You want more than one package or more than one source file.
- You want a committed `vo.lock` in version control.
- You need `replace` for local development.
- You need `vo.work` workspace overrides.
- You need `vo.ext.toml` for native or WASM extension metadata.
- The file now lives inside an existing module tree.

A minimal project layout looks like this:

```text
demo/
  vo.mod
  vo.lock
  main.vo
```

At that point, move the dependency intent out of the source file and into `vo.mod`.

## 9. Troubleshooting

### `inline '/*vo:mod' block is not allowed in a file inside a project with vo.mod`

The file is already part of a normal project.
Remove the inline block and use the project's `vo.mod` instead.

### `duplicate 'module' directive` or `duplicate 'vo' directive`

The inline block must behave like one canonical `vo.mod` root.
Keep exactly one `module` line and exactly one `vo` line.

### `vo.ext.toml` conflict

A single-file inline module cannot share a directory with `vo.ext.toml`.
If you need extension metadata, promote the script to a real project.

### `local/* paths are not allowed in require`

`local/<name>` is only for naming the ephemeral root itself.
Every `require` must be a canonical external module path.

## 10. Related references

- Spec: [`module.md`](./module.md)
- Go-oriented language overview: [`../vo-for-gophers.md`](../vo-for-gophers.md)
