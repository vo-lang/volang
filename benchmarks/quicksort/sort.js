function swap(arr, i, j) {
    const tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
}

function partition(arr, low, high) {
    const pivot = arr[high];
    let i = low - 1;
    for (let j = low; j < high; j++) {
        if (arr[j] <= pivot) {
            i++;
            swap(arr, i, j);
        }
    }
    swap(arr, i + 1, high);
    return i + 1;
}

function quicksort(arr, low, high) {
    if (low < high) {
        const pi = partition(arr, low, high);
        quicksort(arr, low, pi - 1);
        quicksort(arr, pi + 1, high);
    }
}

function isSorted(arr) {
    for (let i = 1; i < arr.length; i++) {
        if (arr[i - 1] > arr[i]) {
            return false;
        }
    }
    return true;
}

const size = 3000;
const iterations = 200;
let checksum = 0;

for (let iter = 0; iter < iterations; iter++) {
    const arr = new Array(size);
    for (let i = 0; i < size; i++) {
        arr[i] = ((i * 1103515245 + 12345 + iter) % 1000000) - 500000;
    }

    quicksort(arr, 0, size - 1);

    if (isSorted(arr)) {
        checksum += arr[0] + arr[size - 1];
    }
}

console.log(checksum);
