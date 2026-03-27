public class Bench {
    @FunctionalInterface
    interface IntFunc {
        int apply(int x);
    }

    interface Adder {
        int add(int x);
    }

    static class Inc implements Adder {
        private final int base;
        Inc(int base) { this.base = base; }
        @Override
        public int add(int x) { return x + base; }
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

    static IntFunc makeAdder(int base) {
        return x -> x + base;
    }

    public static void main(String[] args) {
        int n = 5_000_000;
        long acc = 0;

        // closure mono
        IntFunc f = makeAdder(7);
        for (int i = 0; i < n; i++) {
            acc += f.apply(i & 1023);
        }

        // closure poly4
        IntFunc f0 = makeAdder(1);
        IntFunc f1 = makeAdder(3);
        IntFunc f2 = makeAdder(5);
        IntFunc f3 = makeAdder(7);
        for (int i = 0; i < n; i++) {
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

        // iface mono
        Adder adder = new Inc(7);
        for (int i = 0; i < n; i++) {
            acc += adder.add(i & 1023);
        }

        // iface poly4
        Adder a0 = new Add1();
        Adder a1 = new Add3();
        Adder a2 = new Add5();
        Adder a3 = new Add7();
        for (int i = 0; i < n; i++) {
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
