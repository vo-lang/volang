class Node {
    constructor() {
        this.left = null;
        this.right = null;
    }
}

function make(depth) {
    if (depth === 0) {
        return new Node();
    }
    const node = new Node();
    node.left = make(depth - 1);
    node.right = make(depth - 1);
    return node;
}

function check(n) {
    if (n.left === null) {
        return 1;
    }
    return 1 + check(n.left) + check(n.right);
}

const minDepth = 4;
const maxDepth = 15;
const stretchDepth = maxDepth + 1;

const stretchTree = make(stretchDepth);
console.log(`stretch tree of depth ${stretchDepth} check: ${check(stretchTree)}`);

const longLivedTree = make(maxDepth);

for (let depth = minDepth; depth <= maxDepth; depth += 2) {
    const iterations = 1 << (maxDepth - depth + minDepth);
    let sum = 0;
    for (let i = 0; i < iterations; i++) {
        const tree = make(depth);
        sum += check(tree);
    }
    console.log(`${iterations} trees of depth ${depth} check: ${sum}`);
}

console.log(`long lived tree of depth ${maxDepth} check: ${check(longLivedTree)}`);
