public class Bench {
    interface Adder {
        int add(int x);
    }

    static class Add1 implements Adder {
        @Override
        public int add(int x) { return x + 1; }
    }

    static class Add3 implements Adder {
        @Override
        public int add(int x) { return x + 3; }
    }

    static class Add5 implements Adder {
        @Override
        public int add(int x) { return x + 5; }
    }

    static class Add7 implements Adder {
        @Override
        public int add(int x) { return x + 7; }
    }

    public static void main(String[] args) {
        int iterations = 20_000_000;
        Adder a0 = new Add1();
        Adder a1 = new Add3();
        Adder a2 = new Add5();
        Adder a3 = new Add7();
        long acc = 0;

        for (int i = 0; i < iterations; i++) {
            int x = i & 1023;
            int slot = i & 3;
            if (slot == 0) {
                acc += a0.add(x);
            } else if (slot == 1) {
                acc += a1.add(x);
            } else if (slot == 2) {
                acc += a2.add(x);
            } else {
                acc += a3.add(x);
            }
        }

        System.out.println(acc);
    }
}
