#include <stdio.h>
#include <stdlib.h>

typedef struct Node {
    struct Node *left;
    struct Node *right;
} Node;

Node* make_tree(int depth) {
    Node* node = (Node*)malloc(sizeof(Node));
    if (depth == 0) {
        node->left = NULL;
        node->right = NULL;
    } else {
        node->left = make_tree(depth - 1);
        node->right = make_tree(depth - 1);
    }
    return node;
}

int check(Node* n) {
    if (n->left == NULL) {
        return 1;
    }
    return 1 + check(n->left) + check(n->right);
}

void free_tree(Node* n) {
    if (n == NULL) return;
    free_tree(n->left);
    free_tree(n->right);
    free(n);
}

int main() {
    int minDepth = 4;
    int maxDepth = 15;
    int stretchDepth = maxDepth + 1;

    Node* stretchTree = make_tree(stretchDepth);
    printf("stretch tree of depth %d check: %d\n", stretchDepth, check(stretchTree));
    free_tree(stretchTree);

    Node* longLivedTree = make_tree(maxDepth);

    for (int depth = minDepth; depth <= maxDepth; depth += 2) {
        int iterations = 1 << (maxDepth - depth + minDepth);
        int sum = 0;
        for (int i = 0; i < iterations; i++) {
            Node* tree = make_tree(depth);
            sum += check(tree);
            free_tree(tree);
        }
        printf("%d trees of depth %d check: %d\n", iterations, depth, sum);
    }

    printf("long lived tree of depth %d check: %d\n", maxDepth, check(longLivedTree));
    free_tree(longLivedTree);

    return 0;
}
