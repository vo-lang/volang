#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int sieve(int n) {
    bool* isPrime = (bool*)malloc((n + 1) * sizeof(bool));
    for (int i = 0; i <= n; i++) {
        isPrime[i] = true;
    }
    isPrime[0] = false;
    isPrime[1] = false;

    for (int i = 2; i * i <= n; i++) {
        if (isPrime[i]) {
            for (int j = i * i; j <= n; j += i) {
                isPrime[j] = false;
            }
        }
    }

    int count = 0;
    for (int i = 2; i <= n; i++) {
        if (isPrime[i]) {
            count++;
        }
    }

    free(isPrime);
    return count;
}

int main() {
    int n = 6000000;
    int count = sieve(n);
    printf("primes up to %d: %d\n", n, count);
    return 0;
}
