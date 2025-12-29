#include <stdio.h>

int fib(int n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

int main() {
    int n = 30;
    int result = fib(n);
    printf("fib(%d) = %d\n", n, result);
    return 0;
}
