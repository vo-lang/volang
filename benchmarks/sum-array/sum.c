#include <stdio.h>
#include <stdlib.h>

int main() {
    int n = 10000000;
    int *arr = malloc(n * sizeof(int));
    for (int i = 0; i < n; i++) {
        arr[i] = i;
    }

    long sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    printf("%ld\n", sum);
    free(arr);
    return 0;
}
