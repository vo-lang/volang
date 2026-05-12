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
vo init myapp
cd myapp
```

This creates:

```
myapp/
  main.vo
  vo.mod
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
