public class Sort {
    static void swap(int[] arr, int i, int j) {
        int tmp = arr[i];
        arr[i] = arr[j];
        arr[j] = tmp;
    }

    static int partition(int[] arr, int low, int high) {
        int pivot = arr[high];
        int i = low - 1;
        for (int j = low; j < high; j++) {
            if (arr[j] <= pivot) {
                i++;
                swap(arr, i, j);
            }
        }
        swap(arr, i + 1, high);
        return i + 1;
    }

    static void quicksort(int[] arr, int low, int high) {
        if (low < high) {
            int pi = partition(arr, low, high);
            quicksort(arr, low, pi - 1);
            quicksort(arr, pi + 1, high);
        }
    }

    static boolean isSorted(int[] arr) {
        for (int i = 1; i < arr.length; i++) {
            if (arr[i - 1] > arr[i]) {
                return false;
            }
        }
        return true;
    }

    public static void main(String[] args) {
        int size = 1000;
        int iterations = 10;
        long checksum = 0;

        for (int iter = 0; iter < iterations; iter++) {
            // Create array with pseudo-random values
            int[] arr = new int[size];
            for (int i = 0; i < size; i++) {
                // Simple LCG: (a * x + c) mod m
                arr[i] = (int)(((long)i * 1103515245 + 12345 + iter) % 1000000) - 500000;
            }

            quicksort(arr, 0, size - 1);

            if (isSorted(arr)) {
                checksum += arr[0] + arr[size - 1];
            }
        }

        System.out.println(checksum);
    }
}
