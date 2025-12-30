def swap(arr, i, j)
  arr[i], arr[j] = arr[j], arr[i]
end

def partition(arr, low, high)
  pivot = arr[high]
  i = low - 1
  (low...high).each do |j|
    if arr[j] <= pivot
      i += 1
      swap(arr, i, j)
    end
  end
  swap(arr, i + 1, high)
  i + 1
end

def quicksort(arr, low, high)
  if low < high
    pi = partition(arr, low, high)
    quicksort(arr, low, pi - 1)
    quicksort(arr, pi + 1, high)
  end
end

def is_sorted(arr)
  (1...arr.length).each do |i|
    return false if arr[i - 1] > arr[i]
  end
  true
end

def main
  size = 3000
  iterations = 20
  checksum = 0

  iterations.times do |iter|
    # Create array with pseudo-random values
    arr = Array.new(size) do |i|
      ((i * 1103515245 + 12345 + iter) % 1000000) - 500000
    end

    quicksort(arr, 0, size - 1)

    if is_sorted(arr)
      checksum += arr[0] + arr[size - 1]
    end
  end

  puts checksum
end

main
