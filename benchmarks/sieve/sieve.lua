local function sieve(n)
    local isPrime = {}
    for i = 0, n do
        isPrime[i] = true
    end
    isPrime[0] = false
    isPrime[1] = false

    local i = 2
    while i * i <= n do
        if isPrime[i] then
            for j = i * i, n, i do
                isPrime[j] = false
            end
        end
        i = i + 1
    end

    local count = 0
    for i = 2, n do
        if isPrime[i] then
            count = count + 1
        end
    end
    return count
end

local n = 6000000
local count = sieve(n)
print(string.format("primes up to %d: %d", n, count))
