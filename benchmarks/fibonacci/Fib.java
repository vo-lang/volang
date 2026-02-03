public class Fib {
    static int fib(int n) {
        if (n < 2) {
            return n;
        }
        return fib(n - 1) + fib(n - 2);
    }

    public static void main(String[] args) {
        int n = 35;
        int result = fib(n);
        System.out.printf("fib(%d) = %d%n", n, result);
    }
}
