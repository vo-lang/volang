package main

import "fmt"

type Node struct {
	left  *Node
	right *Node
}

func make_(depth int) *Node {
	if depth == 0 {
		return &Node{}
	}
	return &Node{
		left:  make_(depth - 1),
		right: make_(depth - 1),
	}
}

func check(n *Node) int {
	if n.left == nil {
		return 1
	}
	return 1 + check(n.left) + check(n.right)
}

func main() {
	minDepth := 4
	maxDepth := 15
	stretchDepth := maxDepth + 1

	stretchTree := make_(stretchDepth)
	fmt.Printf("stretch tree of depth %d check: %d\n", stretchDepth, check(stretchTree))

	longLivedTree := make_(maxDepth)

	for depth := minDepth; depth <= maxDepth; depth += 2 {
		iterations := 1 << (maxDepth - depth + minDepth)
		sum := 0
		for i := 0; i < iterations; i++ {
			tree := make_(depth)
			sum += check(tree)
		}
		fmt.Printf("%d trees of depth %d check: %d\n", iterations, depth, sum)
	}

	fmt.Printf("long lived tree of depth %d check: %d\n", maxDepth, check(longLivedTree))
}
