public class Bench {
    @FunctionalInterface
    interface IntFunc {
        int apply(int x);
    }

    static IntFunc makeAdder(int base) {
        return x -> x + base;
    }

    public static void main(String[] args) {
        int iterations = 20_000_000;
        IntFunc f0 = makeAdder(1);
        IntFunc f1 = makeAdder(3);
        IntFunc f2 = makeAdder(5);
        IntFunc f3 = makeAdder(7);
        long acc = 0;

        for (int i = 0; i < iterations; i++) {
            int x = i & 1023;
            int slot = i & 3;
            if (slot == 0) {
                acc += f0.apply(x);
            } else if (slot == 1) {
                acc += f1.apply(x);
            } else if (slot == 2) {
                acc += f2.apply(x);
            } else {
                acc += f3.apply(x);
            }
        }

        System.out.println(acc);
    }
}
