package main

import "fmt"

func fib(n int) int {
	if n < 2 {
		return n
	}
	return fib(n-1) + fib(n-2)
}

func main() {
	n := 32
	result := fib(n)
	fmt.Printf("fib(%d) = %d\n", n, result)
}
