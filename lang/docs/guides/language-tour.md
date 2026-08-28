# Language tour

Volang uses a compact Go-shaped grammar while defining its own language
semantics. This tour covers the everyday surface; the language specification
contains the normative details and limits.

## Packages and imports

Multi-file directories declare one package. Executable projects use
`package main` and a bodyless-argument `main` function:

```vo
package main

import (
    "fmt"
    "strings"
)

func main() {
    println(strings.ToUpper(fmt.Sprintf("hello %d", 3)))
}
```

Standard-library imports use short paths. External imports use the stable
ModuleId from `vo.mod`; source imports contain no version.

## Variables and constants

Short declarations infer local types. `var` supplies an explicit type or zero
value. Constants are compile-time values.

```vo
name := "Ada"
count := 3
var total int
var labels []string
const retryLimit = 5
```

Multiple assignment evaluates the right-hand side before committing the left:

```vo
left, right := 1, 2
left, right = right, left
```

## Basic and composite types

The language provides booleans, signed and unsigned integers, floating-point
values, strings, bytes, and runes. Composite values include arrays, slices,
maps, structs, interfaces, functions, channels, ports, and islands.

```vo
numbers := []int{2, 3, 5}
lookup := map[string]int{"two": 2, "three": 3}
fixed := [3]byte{1, 2, 3}
```

Struct values copy on assignment. Pointers are available only for struct base
types. Slices, maps, functions, interfaces, channels, ports, islands, and
struct pointers carry reference or capability semantics.

## Structs, methods, and interfaces

```vo
type User struct {
    Name string
    Age int
}

func (user User) Label() string {
    return user.Name
}

func (user *User) Birthday() {
    user.Age++
}

type Labeler interface {
    Label() string
}
```

Interfaces are satisfied structurally by method sets. A value receiver method
works on values and pointers; a pointer receiver method mutates the pointed-to
struct and belongs to the pointer method set.

## Functions and closures

Functions may return multiple values. Function values and closures can retain
captured state.

```vo
func divide(a float64, b float64) (float64, error) {
    if b == 0 {
        fail errors.New("division by zero")
    }
    return a / b, nil
}

func counter() func() int {
    value := 0
    return func() int {
        value++
        return value
    }
}
```

## Control flow

`if`, `for`, `range`, and `switch` cover conditional and iterative control.

```vo
for index, value := range []string{"a", "b"} {
    if index == 0 {
        println(value)
    }
}

switch value := item.(type) {
case int:
    println("integer", value)
case string:
    println("string", value)
default:
    println("other")
}
```

There is no `while`; use `for condition`. `break` and `continue` affect the
nearest loop, with labels available where the specification permits them.

## Errors and cleanup

Errors are ordinary interface values. Volang adds three concise operations:

```vo
func load(path string) ([]byte, error) {
    file := os.Open(path)?
    defer file.Close()
    data := io.ReadAll(file)?
    if len(data) == 0 {
        fail errors.New("empty input")
    }
    return data, nil
}
```

`expr?` propagates a non-nil final error. `fail` begins an error return.
`errdefer` registers rollback work that participates only in error or panic
unwinding. Ordinary `defer` participates in every return path.

## Goroutines and channels

`go` starts a goroutine in the current island. Channels coordinate goroutines
within that scheduler:

```vo
results := make(chan int, 2)
go func() { results <- 20 }()
go func() { results <- 22 }()
println(<-results + <-results)
```

Use `select` for multiple channel operations and a `context.Context` for
cancellation and deadlines. Use ports to cross island boundaries; channel
values stay island-local.

## Dynamic access

The `~>` operator performs a checked operation over an `any` value or a type
implementing the dynamic protocol:

```vo
func username(value any) (string, error) {
    var name string
    name = value~>profile~>name?
    return name, nil
}
```

Assignment context supplies the expected result type. Missing members,
unsupported operations, and mismatched types return stable dynamic errors.

## Memory and limits

The runtime uses precise metadata and a non-moving collector. Backend and host
boundaries must preserve stack maps and object layout. The compiler also
enforces bounded file size, syntax depth, expression complexity, and diagnostic
counts so malformed input cannot consume unbounded frontend resources.
