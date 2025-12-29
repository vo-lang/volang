public class Fannkuch {
    static int fannkuch(int n) {
        int[] perm = new int[n];
        int[] perm1 = new int[n];
        int[] count = new int[n];

        for (int i = 0; i < n; i++) {
            perm1[i] = i;
        }

        int maxFlips = 0;
        int checksum = 0;
        int sign = 1;
        int r = n;

        while (true) {
            // Generate next permutation
            while (r != 1) {
                count[r - 1] = r;
                r--;
            }

            // Copy perm1 to perm
            System.arraycopy(perm1, 0, perm, 0, n);

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
            while (true) {
                if (r == n) {
                    System.out.println(checksum);
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

    public static void main(String[] args) {
        int n = 9;
        int result = fannkuch(n);
        System.out.printf("Pfannkuchen(%d) = %d%n", n, result);
    }
}
