public class Sum {
    public static void main(String[] args) {
        int n = 10000000;
        int[] arr = new int[n];
        for (int i = 0; i < n; i++) {
            arr[i] = i;
        }

        long sum = 0;
        for (int i = 0; i < n; i++) {
            sum += arr[i];
        }
        System.out.println(sum);
    }
}
