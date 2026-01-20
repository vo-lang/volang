export interface Example {
  name: string;
  code: string;
}

export const examples: Example[] = [
  {
    name: 'Error Handling',
    code: `package main

import "errors"

// Vo's elegant error handling: ? operator and fail statement

func main() {
    result, err := process(10)
    if err != nil {
        println("Error:", err)
    } else {
        println("Result:", result)
    }

    _, err = process(-5)
    if err != nil {
        println("Caught:", err)
    }
}

func process(n int) (int, error) {
    validate(n)?  // ? auto-propagates error
    return n * 2, nil
}

func validate(n int) error {
    if n < 0 {
        fail errors.New("negative not allowed")  // concise error return
    }
    return nil
}
`,
  },
  {
    name: 'Goroutines',
    code: `package main

// Concurrent computation with goroutines and channels

func main() {
    ch := make(chan int, 3)
    
    // Launch concurrent workers
    go square(ch, 3)
    go square(ch, 4)
    go square(ch, 5)
    
    // Collect results
    sum := 0
    for i := 0; i < 3; i++ {
        sum += <-ch
    }
    println("3² + 4² + 5² =", sum)  // 50
}

func square(ch chan int, n int) {
    ch <- n * n
}
`,
  },
  {
    name: 'Errdefer',
    code: `package main

import "errors"

// errdefer: cleanup only on error, not on success

func main() {
    err := openAndProcess(true)
    if err != nil {
        println("Failed:", err)
    }
    
    println("")
    
    err = openAndProcess(false)
    if err != nil {
        println("Failed:", err)
    }
}

func openAndProcess(shouldFail bool) error {
    println("Opening resource...")
    
    // errdefer runs ONLY if function returns error
    errdefer println("Cleanup: rolling back!")
    
    // defer always runs
    defer println("Always: closing resource")
    
    if shouldFail {
        fail errors.New("processing failed")
    }
    
    println("Processing succeeded")
    return nil
}
`,
  },
  {
    name: 'Channels',
    code: `package main

// Channel communication patterns

func main() {
    // Buffered channel
    jobs := make(chan int, 5)
    done := make(chan bool)
    
    // Producer
    go func() {
        for i := 1; i <= 5; i++ {
            jobs <- i
            println("Sent job", i)
        }
        close(jobs)
    }()
    
    // Consumer
    go func() {
        for job := range jobs {
            println("Processing job", job)
        }
        done <- true
    }()
    
    <-done
    println("All done!")
}
`,
  },
  {
    name: 'Closures',
    code: `package main

// Closures capture variables by reference

func main() {
    counter := makeCounter(10)
    
    println(counter())  // 11
    println(counter())  // 12
    println(counter())  // 13
    
    // Each closure has its own state
    another := makeCounter(100)
    println(another())  // 101
    println(counter())  // 14
}

func makeCounter(start int) func() int {
    count := start
    return func() int {
        count++
        return count
    }
}
`,
  },
  {
    name: 'Interfaces',
    code: `package main

// Duck typing with interfaces

type Writer interface {
    Write(data string)
}

type Console struct{}

func (c Console) Write(data string) {
    println("[Console]", data)
}

type Logger struct {
    prefix string
}

func (l Logger) Write(data string) {
    println(l.prefix, data)
}

func output(w Writer, msg string) {
    w.Write(msg)
}

func main() {
    console := Console{}
    logger := Logger{prefix: "[LOG]"}
    
    output(console, "Hello from console")
    output(logger, "Hello from logger")
}
`,
  },
  {
    name: 'Select',
    code: `package main

// Select for multiplexing channels

func main() {
    ch1 := make(chan string)
    ch2 := make(chan string)
    
    go func() { ch1 <- "from ch1" }()
    go func() { ch2 <- "from ch2" }()
    
    for i := 0; i < 2; i++ {
        select {
        case msg := <-ch1:
            println("Received:", msg)
        case msg := <-ch2:
            println("Received:", msg)
        }
    }
    println("Done")
}
`,
  },
  {
    name: 'Defer',
    code: `package main

// Defer executes in LIFO order at function exit

func main() {
    println("Start")
    
    defer func() { println("First defer (runs last)") }()
    defer func() { println("Second defer") }()
    defer func() { println("Third defer (runs first)") }()
    
    println("End of main")
}
`,
  },
  {
    name: 'GUI Demo',
    code: `package main

import "encoding/json"

// VoGUI Demo - Declarative UI in Vo
// This renders a simple counter interface

func main() {
    // Build the UI tree
    tree := Column(
        Text("Hello VoGUI!"),
        Text("Count: 0"),
        Row(
            Button("-", 0),
            Button("+", 1),
        ),
        Text("Click buttons to interact (coming soon)"),
    )
    
    // Output for GUI renderer
    output := map[string]any{
        "tree":     tree,
        "handlers": 2,
    }
    data, _ := json.Marshal(output)
    println("__VOGUI__" + string(data))
}

// UI Component helpers
func Column(children ...any) map[string]any {
    return map[string]any{"Type": "Column", "Children": children}
}

func Row(children ...any) map[string]any {
    return map[string]any{"Type": "Row", "Children": children}
}

func Text(content string) map[string]any {
    return map[string]any{"Type": "Text", "Props": map[string]any{"content": content}}
}

func Button(text string, handler int) map[string]any {
    return map[string]any{"Type": "Button", "Props": map[string]any{"text": text, "onClick": handler}}
}
`,
  },
];

