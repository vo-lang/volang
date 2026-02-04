package main

import "fmt"

func main() {
	n := 10000000
	arr := make([]int, n)
	for i := 0; i < n; i++ {
		arr[i] = i
	}

	sum := 0
	for i := 0; i < n; i++ {
		sum += arr[i]
	}
	fmt.Println(sum)
}
