package main

import "fmt"

func swap(arr []int, i, j int) {
	arr[i], arr[j] = arr[j], arr[i]
}

func partition(arr []int, low, high int) int {
	pivot := arr[high]
	i := low - 1
	for j := low; j < high; j++ {
		if arr[j] <= pivot {
			i++
			swap(arr, i, j)
		}
	}
	swap(arr, i+1, high)
	return i + 1
}

func quicksort(arr []int, low, high int) {
	if low < high {
		pi := partition(arr, low, high)
		quicksort(arr, low, pi-1)
		quicksort(arr, pi+1, high)
	}
}

func isSorted(arr []int) bool {
	for i := 1; i < len(arr); i++ {
		if arr[i-1] > arr[i] {
			return false
		}
	}
	return true
}

func main() {
	size := 3000
	iterations := 20
	checksum := 0

	for iter := 0; iter < iterations; iter++ {
		// Create array with pseudo-random values
		arr := make([]int, size)
		for i := 0; i < size; i++ {
			// Simple LCG: (a * x + c) mod m
			arr[i] = ((i * 1103515245 + 12345 + iter) % 1000000) - 500000
		}

		quicksort(arr, 0, size-1)

		if isSorted(arr) {
			checksum += arr[0] + arr[size-1]
		}
	}

	fmt.Println(checksum)
}
