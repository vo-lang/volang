class Node:
    __slots__ = ['left', 'right']
    def __init__(self, left=None, right=None):
        self.left = left
        self.right = right

def make(depth):
    if depth == 0:
        return Node()
    return Node(make(depth - 1), make(depth - 1))

def check(n):
    if n.left is None:
        return 1
    return 1 + check(n.left) + check(n.right)

def main():
    min_depth = 4
    max_depth = 15
    stretch_depth = max_depth + 1

    stretch_tree = make(stretch_depth)
    print(f"stretch tree of depth {stretch_depth} check: {check(stretch_tree)}")

    long_lived_tree = make(max_depth)

    for depth in range(min_depth, max_depth + 1, 2):
        iterations = 1 << (max_depth - depth + min_depth)
        total = 0
        for _ in range(iterations):
            tree = make(depth)
            total += check(tree)
        print(f"{iterations} trees of depth {depth} check: {total}")

    print(f"long lived tree of depth {max_depth} check: {check(long_lived_tree)}")

if __name__ == "__main__":
    import sys
    sys.setrecursionlimit(100000)
    main()
