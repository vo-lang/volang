local function swap(arr, i, j)
    arr[i], arr[j] = arr[j], arr[i]
end

local function partition(arr, low, high)
    local pivot = arr[high]
    local i = low - 1
    for j = low, high - 1 do
        if arr[j] <= pivot then
            i = i + 1
            swap(arr, i, j)
        end
    end
    swap(arr, i + 1, high)
    return i + 1
end

local function quicksort(arr, low, high)
    if low < high then
        local pi = partition(arr, low, high)
        quicksort(arr, low, pi - 1)
        quicksort(arr, pi + 1, high)
    end
end

local function isSorted(arr, n)
    for i = 2, n do
        if arr[i - 1] > arr[i] then
            return false
        end
    end
    return true
end

local function main()
    local size = 1000
    local iterations = 10
    local checksum = 0

    for iter = 0, iterations - 1 do
        -- Create array with pseudo-random values
        local arr = {}
        for i = 1, size do
            -- Simple LCG: (a * x + c) mod m (using i-1 for 0-based logic)
            arr[i] = (((i - 1) * 1103515245 + 12345 + iter) % 1000000) - 500000
        end

        quicksort(arr, 1, size)

        if isSorted(arr, size) then
            checksum = checksum + arr[1] + arr[size]
        end
    end

    print(checksum)
end

main()
