# Hello World

## Single File

Create `hello.vo`:

```vo
func main() {
    println("Hello, World!")
}
```

Run it:

```bash
vo run hello.vo
```

## Module Project

Initialize a new project:

```bash
mkdir myapp
cd myapp
vo init github.com/your-name/myapp
```

This creates `vo.mod`. Add `main.vo` in the same directory:

```vo
package main

func main() {
    println("Hello from a module!")
}
```

Edit `main.vo` and run:

```bash
vo run .
```

## Build an Artifact

Compile to bytecode without running:

```bash
vo build .
```

This produces a `.vob` file you can distribute and run later.

## Type-Check Only

Check for errors without running:

```bash
vo check .
```
