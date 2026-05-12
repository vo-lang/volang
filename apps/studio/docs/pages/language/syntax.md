# Syntax Overview

Vo uses Go-compatible syntax with a few extensions. This page covers the basics.

## Variables

```vo
// Short declaration (type inferred)
name := "Alice"
age := 30
scores := []int{90, 85, 92}

// Explicit type
var count int
var items []string
```

## Functions

```vo
func add(a, b int) int {
    return a + b
}

func divide(a, b float64) (float64, error) {
    if b == 0 {
        fail errors.New("division by zero")
    }
    return a / b, nil
}
```

## Types

```vo
type User struct {
    name string
    age  int
}

type Stringer interface {
    String() string
}

func (u *User) String() string {
    return u.name
}
```

## Control Flow

```vo
// For loop
for i := 0; i < 10; i++ {
    println(i)
}

// Range
for i, v := range items {
    println(i, v)
}

// If
if x > 0 {
    println("positive")
} else {
    println("non-positive")
}

// Switch
switch v := value.(type) {
case int:
    println("int:", v)
case string:
    println("string:", v)
}
```

## Concurrency

```vo
ch := make(chan int, 10)

go func() {
    ch <- 42
}()

value := <-ch
println(value) // 42
```

## Imports

```vo
import (
    "fmt"
    "strings"
    "encoding/json"
)
```
