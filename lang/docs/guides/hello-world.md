# Hello world

This walkthrough starts with one file, promotes it to a module, adds a test,
and produces release artifacts.

## Run one file

Create `hello.vo`:

```vo
func greet(name string) string {
    return "Hello, " + name + "!"
}

func main() {
    println(greet("Volang"))
}
```

Check, format, and run it:

```sh
vo check hello.vo
vo fmt hello.vo
vo run hello.vo
```

The default run mode is the bytecode VM. For a longer native workload, use:

```sh
vo run hello.vo --mode=jit
```

## Create a module project

Create an empty directory and initialize its identity:

```sh
mkdir hello-app
cd hello-app
vo init example.com/acme/hello-app
```

`vo init` writes `vo.mod`; source remains under your control. Create the source
directories and put reusable logic in `greet/greet.vo`:

```sh
mkdir -p greet tests
```

```vo
package greet

func Message(name string) string {
    return "Hello, " + name + "!"
}
```

Add `main.vo`:

```vo
package main

import "example.com/acme/hello-app/greet"

func main() {
    println(greet.Message("module"))
}
```

Run the project directory:

```sh
vo check .
vo run .
```

## Add a test

Create `tests/greeting.vo`:

```vo
package main

import "example.com/acme/hello-app/greet"

func assert(condition bool, message string) {
    if !condition {
        panic(message)
    }
}

func main() {
    assert(greet.Message("Ada") == "Hello, Ada!", "greeting")
}
```

Run the project tests in the VM and JIT:

```sh
vo test --mode=vm
vo test --mode=jit
```

## Build Native AOT

`vo build` produces a host executable by default:

```sh
vo build . -o hello-app
./hello-app
```

Use `--kind=object` for a relocatable object or `--target=TRIPLE` for a
supported cross target. A custom runtime archive may be supplied with
`--runtime=PATH`.

## Build Core Wasm AOT

Create an interpreter-free Core Wasm deployment image with:

```sh
vo build . --kind=wasm -o hello-app.wasm
```

The Wasm image targets the versioned Volang AOT host ABI. A host such as
`vo-web` provides output, memory, scheduling, and declared platform imports.
For browser applications, `vo ui build` assembles the image, host adapter,
assets, manifest, and deployment policy into one directory.

## Continue

Read the language tour for syntax, the modules guide before adding external
dependencies, and the execution guide before selecting production backends.
