#include <stdio.h>
#include <stdlib.h>

typedef struct Node {
    int value;
    struct Node *left;
    struct Node *right;
} Node;

Node* createNode(int value) {
    Node *node = (Node*)malloc(sizeof(Node));
    node->value = value;
    node->left = NULL;
    node->right = NULL;
    return node;
}

void insert(Node *n, int val) {
    if (val < n->value) {
        if (n->left == NULL) {
            n->left = createNode(val);
        } else {
            insert(n->left, val);
        }
    } else {
        if (n->right == NULL) {
            n->right = createNode(val);
        } else {
            insert(n->right, val);
        }
    }
}

int sum(Node *n) {
    if (n == NULL) return 0;
    int total = n->value;
    if (n->left != NULL) total += sum(n->left);
    if (n->right != NULL) total += sum(n->right);
    return total;
}

int count(Node *n) {
    if (n == NULL) return 0;
    int c = 1;
    if (n->left != NULL) c += count(n->left);
    if (n->right != NULL) c += count(n->right);
    return c;
}

int height(Node *n) {
    if (n == NULL) return 0;
    int lh = 0, rh = 0;
    if (n->left != NULL) lh = height(n->left);
    if (n->right != NULL) rh = height(n->right);
    return (lh > rh ? lh : rh) + 1;
}

void freeTree(Node *n) {
    if (n == NULL) return;
    freeTree(n->left);
    freeTree(n->right);
    free(n);
}

Node* makeTree(int *values, int size) {
    if (size == 0) return NULL;
    Node *root = createNode(values[0]);
    for (int i = 1; i < size; i++) {
        insert(root, values[i]);
    }
    return root;
}

int fibonacci(int n) {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

void processWithClosure(int *arr, int size, int multiplier, int *result) {
    int fib10 = fibonacci(10);
    for (int i = 0; i < size; i++) {
        result[i] = arr[i] * multiplier + fib10;
    }
}

void matrixMultiply(int **a, int **b, int **result, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            int sum = 0;
            for (int k = 0; k < n; k++) {
                sum += a[i][k] * b[k][j];
            }
            result[i][j] = sum;
        }
    }
}

int main() {
    int iterations = 500;
    int treeSize = 100;
    int matrixSize = 20;
    
    int totalSum = 0;
    int totalCount = 0;
    int totalHeight = 0;
    
    for (int iter = 0; iter < iterations; iter++) {
        int *values = (int*)malloc(treeSize * sizeof(int));
        for (int i = 0; i < treeSize; i++) {
            values[i] = (i * 7 + iter * 13) % 1000;
        }
        
        Node *tree = makeTree(values, treeSize);
        totalSum += sum(tree);
        totalCount += count(tree);
        totalHeight += height(tree);
        
        int *processed = (int*)malloc(treeSize * sizeof(int));
        processWithClosure(values, treeSize, iter % 10 + 1, processed);
        for (int i = 0; i < treeSize; i++) {
            totalSum += processed[i] % 100;
        }
        free(processed);
        
        if (iter % 50 == 0) {
            int **matA = (int**)malloc(matrixSize * sizeof(int*));
            int **matB = (int**)malloc(matrixSize * sizeof(int*));
            int **matC = (int**)malloc(matrixSize * sizeof(int*));
            
            for (int i = 0; i < matrixSize; i++) {
                matA[i] = (int*)malloc(matrixSize * sizeof(int));
                matB[i] = (int*)malloc(matrixSize * sizeof(int));
                matC[i] = (int*)malloc(matrixSize * sizeof(int));
                for (int j = 0; j < matrixSize; j++) {
                    matA[i][j] = (i + j + iter) % 10;
                    matB[i][j] = (i * j + iter) % 10;
                }
            }
            
            matrixMultiply(matA, matB, matC, matrixSize);
            
            for (int i = 0; i < matrixSize; i++) {
                for (int j = 0; j < matrixSize; j++) {
                    totalSum += matC[i][j];
                }
                free(matA[i]);
                free(matB[i]);
                free(matC[i]);
            }
            free(matA);
            free(matB);
            free(matC);
        }
        
        freeTree(tree);
        free(values);
    }
    
    printf("%d %d %d\n", totalSum, totalCount, totalHeight);
    return 0;
}
