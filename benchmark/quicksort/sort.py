def swap(arr, i, j):
    arr[i], arr[j] = arr[j], arr[i]


def partition(arr, low, high):
    pivot = arr[high]
    i = low - 1
    for j in range(low, high):
        if arr[j] <= pivot:
            i += 1
            swap(arr, i, j)
    swap(arr, i + 1, high)
    return i + 1


def quicksort(arr, low, high):
    if low < high:
        pi = partition(arr, low, high)
        quicksort(arr, low, pi - 1)
        quicksort(arr, pi + 1, high)


def is_sorted(arr):
    for i in range(1, len(arr)):
        if arr[i - 1] > arr[i]:
            return False
    return True


def main():
    size = 3000
    iterations = 20
    checksum = 0

    for iter in range(iterations):
        # Create array with pseudo-random values
        arr = [0] * size
        for i in range(size):
            # Simple LCG: (a * x + c) mod m
            arr[i] = ((i * 1103515245 + 12345 + iter) % 1000000) - 500000

        quicksort(arr, 0, size - 1)

        if is_sorted(arr):
            checksum += arr[0] + arr[size - 1]

    print(checksum)


import sys
sys.setrecursionlimit(20000)
main()
