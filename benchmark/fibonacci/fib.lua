local function fib(n)
    if n < 2 then
        return n
    end
    return fib(n - 1) + fib(n - 2)
end

local n = 35
local result = fib(n)
print(string.format("fib(%d) = %d", n, result))
