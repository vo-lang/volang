public class Bench {
    interface Adder {
        int add(int x);
    }

    static class Inc implements Adder {
        private final int base;

        Inc(int base) {
            this.base = base;
        }

        @Override
        public int add(int x) {
            return x + base;
        }
    }

    public static void main(String[] args) {
        int iterations = 20_000_000;
        Adder adder = new Inc(7);
        long acc = 0;

        for (int i = 0; i < iterations; i++) {
            acc += adder.add(i & 1023);
        }

        System.out.println(acc);
    }
}
