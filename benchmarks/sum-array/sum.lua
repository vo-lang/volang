local n = 10000000
local arr = {}
for i = 0, n - 1 do
    arr[i] = i
end

local sum = 0
for i = 0, n - 1 do
    sum = sum + arr[i]
end
print(sum)
