package main

import "fmt"

type Adder interface {
	Add(int) int
}

type Add1 struct{}
type Add3 struct{}
type Add5 struct{}
type Add7 struct{}

func (a *Add1) Add(x int) int { return x + 1 }
func (a *Add3) Add(x int) int { return x + 3 }
func (a *Add5) Add(x int) int { return x + 5 }
func (a *Add7) Add(x int) int { return x + 7 }

func main() {
	iterations := 20000000
	var a0 Adder = &Add1{}
	var a1 Adder = &Add3{}
	var a2 Adder = &Add5{}
	var a3 Adder = &Add7{}
	acc := 0

	for i := 0; i < iterations; i++ {
		x := i & 1023
		slot := i & 3
		if slot == 0 {
			acc += a0.Add(x)
		} else if slot == 1 {
			acc += a1.Add(x)
		} else if slot == 2 {
			acc += a2.Add(x)
		} else {
			acc += a3.Add(x)
		}
	}

	fmt.Println(acc)
}
