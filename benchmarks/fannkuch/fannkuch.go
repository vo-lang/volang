package main

import "fmt"

func fannkuch(n int) int {
	perm := make([]int, n)
	perm1 := make([]int, n)
	count := make([]int, n)

	for i := 0; i < n; i++ {
		perm1[i] = i
	}

	maxFlips := 0
	checksum := 0
	sign := 1
	r := n

	for {
		// Generate next permutation
		for r != 1 {
			count[r-1] = r
			r--
		}

		// Copy perm1 to perm
		for i := 0; i < n; i++ {
			perm[i] = perm1[i]
		}

		// Count flips
		flips := 0
		for perm[0] != 0 {
			k := perm[0] + 1
			// Reverse first k elements
			for i, j := 0, k-1; i < j; i, j = i+1, j-1 {
				perm[i], perm[j] = perm[j], perm[i]
			}
			flips++
		}

		if flips > maxFlips {
			maxFlips = flips
		}
		checksum += sign * flips
		sign = -sign

		// Next permutation
		for {
			if r == n {
				fmt.Println(checksum)
				return maxFlips
			}
			// Rotate perm1[0..r]
			p0 := perm1[0]
			for i := 0; i < r; i++ {
				perm1[i] = perm1[i+1]
			}
			perm1[r] = p0

			count[r]--
			if count[r] > 0 {
				break
			}
			r++
		}
	}
}

func main() {
	n := 8
	result := fannkuch(n)
	fmt.Printf("Pfannkuchen(%d) = %d\n", n, result)
}
