# Inline `vo.mod` Tutorial

An inline module gives one dependency-free `.vo` file an explicit unpublished
identity and toolchain minimum.

## Minimal file

```vo
/*vo:mod
format = 1
module = "local/hello"
version = "0.1.0"
vo = "0.1.0"
*/

package main

func main() {
    println("hello")
}
```

```bash
vo run hello.vo
vo check hello.vo
vo build hello.vo -o hello.vob
```

The block must appear in the leading comment region. Its schema contains
exactly `format`, `module`, `version`, and `vo`. The ModuleId must use
`local/<name>`.

Inline modules accept no dependencies, `[web]`, `[extension]`, `[build]`, or
unknown keys. They never discover `vo.mod` above the file, `vo.work`, a
registry, or the module cache. External imports are errors.

## Promote to a project

Create a project when the program needs dependencies, multiple files, web or
extension metadata, publication, or workspace participation:

```text
demo/
├── vo.mod
├── vo.lock        # present exactly when dependencies are non-empty
└── main.vo
```

Move the metadata to `vo.mod`, add dependency intent, then select and
materialize the graph:

```bash
vo mod sync
vo mod fetch
vo mod verify
```

Use `vo work sync` when selected dependencies should come from explicit local
workspace members. Every non-empty graph still has one `vo.lock`.

See the full [`module protocol`](./module.md) for identity, selection,
workspace, release, and cache rules.
