public class Spectral {
    static double a(int i, int j) {
        return 1.0 / ((i + j) * (i + j + 1) / 2 + i + 1);
    }

    static void multiplyAv(int n, double[] v, double[] av) {
        for (int i = 0; i < n; i++) {
            av[i] = 0;
            for (int j = 0; j < n; j++) {
                av[i] += a(i, j) * v[j];
            }
        }
    }

    static void multiplyAtv(int n, double[] v, double[] atv) {
        for (int i = 0; i < n; i++) {
            atv[i] = 0;
            for (int j = 0; j < n; j++) {
                atv[i] += a(j, i) * v[j];
            }
        }
    }

    static void multiplyAtAv(int n, double[] v, double[] atav, double[] av) {
        multiplyAv(n, v, av);
        multiplyAtv(n, av, atav);
    }

    static double spectralNorm(int n) {
        double[] u = new double[n];
        double[] v = new double[n];
        double[] av = new double[n];

        for (int i = 0; i < n; i++) {
            u[i] = 1;
        }

        for (int i = 0; i < 10; i++) {
            multiplyAtAv(n, u, v, av);
            multiplyAtAv(n, v, u, av);
        }

        double vBv = 0, vv = 0;
        for (int i = 0; i < n; i++) {
            vBv += u[i] * v[i];
            vv += v[i] * v[i];
        }

        return Math.sqrt(vBv / vv);
    }

    public static void main(String[] args) {
        int n = 200;
        double result = spectralNorm(n);
        System.out.printf("%.9f%n", result);
    }
}
