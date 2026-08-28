# Getting started with Volang UI

Volang UI ships with the `vo` toolchain. Application code is typed `.vo`
source. Development uses the VM or JIT; Web releases use Core Wasm AOT and
desktop releases use Native AOT.

## Create and run

```sh
vo ui new hello
cd hello
vo ui dev --open
vo ui run . --mode=jit
```

Choose a maintained starting point with `--template=dashboard`,
`--template=media`, or `--template=studio`. The command installs the bundled,
authenticated `github.com/vo-lang/ui` module and writes an exact `vo.lock`.

The starter exposes one ordinary function:

```go
package main

import "github.com/vo-lang/ui"

func App() ui.View {
	return ui.Column(ui.Text("Hello"), ui.Button("Continue", ui.Action(func() {})))
}

func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
```

## Test and diagnose

```sh
vo ui test . --mode=vm
vo ui test . --mode=jit --profile
vo ui inspect . --target=web
vo ui doctor .
```

Tests address controls by accessible name and can drive input, focus, keys,
dragging, waits, snapshots and profiling. Inspector output includes linked UI
packages, authority packages, bytecode/AOT size and component counts.

## Release

```sh
vo ui build . -o dist-web
vo ui package . -o dist-desktop
```

`ui.web.toml` owns routes, document metadata, PWA and security policy.
`ui.desktop.toml` owns application identity, window defaults, signing policy
and update metadata. Release artifacts carry receipts and require no developer
toolchain or network connection at startup.

Read [application architecture](application-data-platform.md),
[Web and desktop products](web-desktop-products.md), and
[testing and troubleshooting](testing-troubleshooting.md) next.
