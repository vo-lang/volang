#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double a(int i, int j) {
    return 1.0 / ((i + j) * (i + j + 1) / 2 + i + 1);
}

void multiply_Av(int n, double *v, double *av) {
    for (int i = 0; i < n; i++) {
        av[i] = 0;
        for (int j = 0; j < n; j++) {
            av[i] += a(i, j) * v[j];
        }
    }
}

void multiply_Atv(int n, double *v, double *atv) {
    for (int i = 0; i < n; i++) {
        atv[i] = 0;
        for (int j = 0; j < n; j++) {
            atv[i] += a(j, i) * v[j];
        }
    }
}

void multiply_AtAv(int n, double *v, double *atav, double *av) {
    multiply_Av(n, v, av);
    multiply_Atv(n, av, atav);
}

double spectral_norm(int n) {
    double *u = malloc(n * sizeof(double));
    double *v = malloc(n * sizeof(double));
    double *av = malloc(n * sizeof(double));

    for (int i = 0; i < n; i++) {
        u[i] = 1;
    }

    for (int i = 0; i < 10; i++) {
        multiply_AtAv(n, u, v, av);
        multiply_AtAv(n, v, u, av);
    }

    double vBv = 0, vv = 0;
    for (int i = 0; i < n; i++) {
        vBv += u[i] * v[i];
        vv += v[i] * v[i];
    }

    free(u);
    free(v);
    free(av);

    return sqrt(vBv / vv);
}

int main() {
    int n = 200;
    double result = spectral_norm(n);
    printf("%.9f\n", result);
    return 0;
}
