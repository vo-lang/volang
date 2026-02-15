package main

import "fmt"

func makeAdder(base int) func(int) int {
	return func(x int) int {
		return x + base
	}
}

func main() {
	iterations := 20000000
	f0 := makeAdder(1)
	f1 := makeAdder(3)
	f2 := makeAdder(5)
	f3 := makeAdder(7)
	acc := 0

	for i := 0; i < iterations; i++ {
		x := i & 1023
		slot := i & 3
		if slot == 0 {
			acc += f0(x)
		} else if slot == 1 {
			acc += f1(x)
		} else if slot == 2 {
			acc += f2(x)
		} else {
			acc += f3(x)
		}
	}

	fmt.Println(acc)
}
