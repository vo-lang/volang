public class Trees {
    static class Node {
        Node left;
        Node right;
    }

    static Node make(int depth) {
        Node n = new Node();
        if (depth > 0) {
            n.left = make(depth - 1);
            n.right = make(depth - 1);
        }
        return n;
    }

    static int check(Node n) {
        if (n.left == null) {
            return 1;
        }
        return 1 + check(n.left) + check(n.right);
    }

    public static void main(String[] args) {
        int minDepth = 4;
        int maxDepth = 10;
        int stretchDepth = maxDepth + 1;

        Node stretchTree = make(stretchDepth);
        System.out.printf("stretch tree of depth %d check: %d%n", stretchDepth, check(stretchTree));

        Node longLivedTree = make(maxDepth);

        for (int depth = minDepth; depth <= maxDepth; depth += 2) {
            int iterations = 1 << (maxDepth - depth + minDepth);
            int sum = 0;
            for (int i = 0; i < iterations; i++) {
                Node tree = make(depth);
                sum += check(tree);
            }
            System.out.printf("%d trees of depth %d check: %d%n", iterations, depth, sum);
        }

        System.out.printf("long lived tree of depth %d check: %d%n", maxDepth, check(longLivedTree));
    }
}
