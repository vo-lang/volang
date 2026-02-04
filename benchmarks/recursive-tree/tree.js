class Node {
    constructor(value) {
        this.value = value;
        this.left = null;
        this.right = null;
    }

    insert(val) {
        if (val < this.value) {
            if (this.left === null) {
                this.left = new Node(val);
            } else {
                this.left.insert(val);
            }
        } else {
            if (this.right === null) {
                this.right = new Node(val);
            } else {
                this.right.insert(val);
            }
        }
    }

    sum() {
        let total = this.value;
        if (this.left !== null) {
            total += this.left.sum();
        }
        if (this.right !== null) {
            total += this.right.sum();
        }
        return total;
    }

    count() {
        let c = 1;
        if (this.left !== null) {
            c += this.left.count();
        }
        if (this.right !== null) {
            c += this.right.count();
        }
        return c;
    }

    height() {
        let lh = 0, rh = 0;
        if (this.left !== null) {
            lh = this.left.height();
        }
        if (this.right !== null) {
            rh = this.right.height();
        }
        return (lh > rh ? lh : rh) + 1;
    }
}

function makeTree(values) {
    if (values.length === 0) {
        return null;
    }
    const root = new Node(values[0]);
    for (let i = 1; i < values.length; i++) {
        root.insert(values[i]);
    }
    return root;
}

function fibonacci(n) {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

function processWithClosure(arr, multiplier) {
    const result = new Array(arr.length);
    const fib10 = fibonacci(10);

    const transform = (x) => x * multiplier + fib10;

    for (let i = 0; i < arr.length; i++) {
        result[i] = transform(arr[i]);
    }

    return result;
}

function matrixMultiply(a, b) {
    const n = a.length;
    const result = new Array(n);
    for (let i = 0; i < n; i++) {
        result[i] = new Array(n);
        for (let j = 0; j < n; j++) {
            let sum = 0;
            for (let k = 0; k < n; k++) {
                sum += a[i][k] * b[k][j];
            }
            result[i][j] = sum;
        }
    }
    return result;
}

const iterations = 3500;
const treeSize = 100;
const matrixSize = 20;

let totalSum = 0;
let totalCount = 0;
let totalHeight = 0;

for (let iter = 0; iter < iterations; iter++) {
    const values = new Array(treeSize);
    for (let i = 0; i < treeSize; i++) {
        values[i] = (i * 7 + iter * 13) % 1000;
    }

    const tree = makeTree(values);
    totalSum += tree.sum();
    totalCount += tree.count();
    totalHeight += tree.height();

    const processed = processWithClosure(values, iter % 10 + 1);
    for (let i = 0; i < processed.length; i++) {
        totalSum += processed[i] % 100;
    }

    if (iter % 50 === 0) {
        const matA = new Array(matrixSize);
        const matB = new Array(matrixSize);
        for (let i = 0; i < matrixSize; i++) {
            matA[i] = new Array(matrixSize);
            matB[i] = new Array(matrixSize);
            for (let j = 0; j < matrixSize; j++) {
                matA[i][j] = (i + j + iter) % 10;
                matB[i][j] = (i * j + iter) % 10;
            }
        }

        const matC = matrixMultiply(matA, matB);
        for (let i = 0; i < matrixSize; i++) {
            for (let j = 0; j < matrixSize; j++) {
                totalSum += matC[i][j];
            }
        }
    }
}

console.log(totalSum, totalCount, totalHeight);
