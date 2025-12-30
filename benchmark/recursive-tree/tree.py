class Node:
    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None
    
    def insert(self, val):
        if val < self.value:
            if self.left is None:
                self.left = Node(val)
            else:
                self.left.insert(val)
        else:
            if self.right is None:
                self.right = Node(val)
            else:
                self.right.insert(val)
    
    def sum(self):
        total = self.value
        if self.left is not None:
            total += self.left.sum()
        if self.right is not None:
            total += self.right.sum()
        return total
    
    def count(self):
        c = 1
        if self.left is not None:
            c += self.left.count()
        if self.right is not None:
            c += self.right.count()
        return c
    
    def height(self):
        lh = 0
        rh = 0
        if self.left is not None:
            lh = self.left.height()
        if self.right is not None:
            rh = self.right.height()
        return max(lh, rh) + 1

def make_tree(values):
    if len(values) == 0:
        return None
    root = Node(values[0])
    for i in range(1, len(values)):
        root.insert(values[i])
    return root

def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def process_with_closure(arr, multiplier):
    fib10 = fibonacci(10)
    
    def transform(x):
        return x * multiplier + fib10
    
    return [transform(x) for x in arr]

def matrix_multiply(a, b):
    n = len(a)
    result = [[0] * n for _ in range(n)]
    for i in range(n):
        for j in range(n):
            sum_val = 0
            for k in range(n):
                sum_val += a[i][k] * b[k][j]
            result[i][j] = sum_val
    return result

def main():
    iterations = 500
    tree_size = 100
    matrix_size = 20
    
    total_sum = 0
    total_count = 0
    total_height = 0
    
    for iter in range(iterations):
        values = [(i * 7 + iter * 13) % 1000 for i in range(tree_size)]
        
        tree = make_tree(values)
        total_sum += tree.sum()
        total_count += tree.count()
        total_height += tree.height()
        
        processed = process_with_closure(values, iter % 10 + 1)
        for val in processed:
            total_sum += val % 100
        
        if iter % 50 == 0:
            mat_a = [[(i + j + iter) % 10 for j in range(matrix_size)] for i in range(matrix_size)]
            mat_b = [[(i * j + iter) % 10 for j in range(matrix_size)] for i in range(matrix_size)]
            
            mat_c = matrix_multiply(mat_a, mat_b)
            for i in range(matrix_size):
                for j in range(matrix_size):
                    total_sum += mat_c[i][j]
    
    print(total_sum, total_count, total_height)

if __name__ == '__main__':
    main()
