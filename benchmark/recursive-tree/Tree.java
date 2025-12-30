class Node {
    int value;
    Node left;
    Node right;
    
    Node(int value) {
        this.value = value;
        this.left = null;
        this.right = null;
    }
    
    void insert(int val) {
        if (val < this.value) {
            if (this.left == null) {
                this.left = new Node(val);
            } else {
                this.left.insert(val);
            }
        } else {
            if (this.right == null) {
                this.right = new Node(val);
            } else {
                this.right.insert(val);
            }
        }
    }
    
    int sum() {
        int total = this.value;
        if (this.left != null) {
            total += this.left.sum();
        }
        if (this.right != null) {
            total += this.right.sum();
        }
        return total;
    }
    
    int count() {
        int c = 1;
        if (this.left != null) {
            c += this.left.count();
        }
        if (this.right != null) {
            c += this.right.count();
        }
        return c;
    }
    
    int height() {
        int lh = 0;
        int rh = 0;
        if (this.left != null) {
            lh = this.left.height();
        }
        if (this.right != null) {
            rh = this.right.height();
        }
        return Math.max(lh, rh) + 1;
    }
}

public class Tree {
    static Node makeTree(int[] values) {
        if (values.length == 0) {
            return null;
        }
        Node root = new Node(values[0]);
        for (int i = 1; i < values.length; i++) {
            root.insert(values[i]);
        }
        return root;
    }
    
    static int fibonacci(int n) {
        if (n <= 1) {
            return n;
        }
        return fibonacci(n - 1) + fibonacci(n - 2);
    }
    
    static int[] processWithClosure(int[] arr, int multiplier) {
        int[] result = new int[arr.length];
        int fib10 = fibonacci(10);
        
        for (int i = 0; i < arr.length; i++) {
            result[i] = arr[i] * multiplier + fib10;
        }
        
        return result;
    }
    
    static int[][] matrixMultiply(int[][] a, int[][] b) {
        int n = a.length;
        int[][] result = new int[n][n];
        
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                int sum = 0;
                for (int k = 0; k < n; k++) {
                    sum += a[i][k] * b[k][j];
                }
                result[i][j] = sum;
            }
        }
        
        return result;
    }
    
    public static void main(String[] args) {
        int iterations = 500;
        int treeSize = 100;
        int matrixSize = 20;
        
        int totalSum = 0;
        int totalCount = 0;
        int totalHeight = 0;
        
        for (int iter = 0; iter < iterations; iter++) {
            int[] values = new int[treeSize];
            for (int i = 0; i < treeSize; i++) {
                values[i] = (i * 7 + iter * 13) % 1000;
            }
            
            Node tree = makeTree(values);
            totalSum += tree.sum();
            totalCount += tree.count();
            totalHeight += tree.height();
            
            int[] processed = processWithClosure(values, iter % 10 + 1);
            for (int i = 0; i < processed.length; i++) {
                totalSum += processed[i] % 100;
            }
            
            if (iter % 50 == 0) {
                int[][] matA = new int[matrixSize][matrixSize];
                int[][] matB = new int[matrixSize][matrixSize];
                
                for (int i = 0; i < matrixSize; i++) {
                    for (int j = 0; j < matrixSize; j++) {
                        matA[i][j] = (i + j + iter) % 10;
                        matB[i][j] = (i * j + iter) % 10;
                    }
                }
                
                int[][] matC = matrixMultiply(matA, matB);
                for (int i = 0; i < matrixSize; i++) {
                    for (int j = 0; j < matrixSize; j++) {
                        totalSum += matC[i][j];
                    }
                }
            }
        }
        
        System.out.println(totalSum + " " + totalCount + " " + totalHeight);
    }
}
