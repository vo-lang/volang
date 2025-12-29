#include <stdio.h>
#include <stdlib.h>

int fannkuch(int n) {
    int *perm = malloc(n * sizeof(int));
    int *perm1 = malloc(n * sizeof(int));
    int *count = malloc(n * sizeof(int));

    for (int i = 0; i < n; i++) {
        perm1[i] = i;
    }

    int maxFlips = 0;
    int checksum = 0;
    int sign = 1;
    int r = n;

    while (1) {
        // Generate next permutation
        while (r != 1) {
            count[r - 1] = r;
            r--;
        }

        // Copy perm1 to perm
        for (int i = 0; i < n; i++) {
            perm[i] = perm1[i];
        }

        // Count flips
        int flips = 0;
        while (perm[0] != 0) {
            int k = perm[0] + 1;
            // Reverse first k elements
            for (int i = 0, j = k - 1; i < j; i++, j--) {
                int tmp = perm[i];
                perm[i] = perm[j];
                perm[j] = tmp;
            }
            flips++;
        }

        if (flips > maxFlips) {
            maxFlips = flips;
        }
        checksum += sign * flips;
        sign = -sign;

        // Next permutation
        while (1) {
            if (r == n) {
                printf("%d\n", checksum);
                free(perm);
                free(perm1);
                free(count);
                return maxFlips;
            }
            // Rotate perm1[0..r]
            int p0 = perm1[0];
            for (int i = 0; i < r; i++) {
                perm1[i] = perm1[i + 1];
            }
            perm1[r] = p0;

            count[r]--;
            if (count[r] > 0) {
                break;
            }
            r++;
        }
    }
}

int main() {
    int n = 9;
    int result = fannkuch(n);
    printf("Pfannkuchen(%d) = %d\n", n, result);
    return 0;
}
