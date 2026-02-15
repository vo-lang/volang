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
        IntFunc f = makeAdder(7);
        long acc = 0;

        for (int i = 0; i < iterations; i++) {
            acc += f.apply(i & 1023);
        }

        System.out.println(acc);
    }
}
