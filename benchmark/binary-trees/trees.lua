local function make(depth)
    if depth == 0 then
        return {}
    end
    return {
        left = make(depth - 1),
        right = make(depth - 1)
    }
end

local function check(n)
    if n.left == nil then
        return 1
    end
    return 1 + check(n.left) + check(n.right)
end

local minDepth = 4
local maxDepth = 16
local stretchDepth = maxDepth + 1

local stretchTree = make(stretchDepth)
print(string.format("stretch tree of depth %d check: %d", stretchDepth, check(stretchTree)))

local longLivedTree = make(maxDepth)

for depth = minDepth, maxDepth, 2 do
    local iterations = 2 ^ (maxDepth - depth + minDepth)
    local sum = 0
    for i = 1, iterations do
        local tree = make(depth)
        sum = sum + check(tree)
    end
    print(string.format("%d trees of depth %d check: %d", iterations, depth, sum))
end

print(string.format("long lived tree of depth %d check: %d", maxDepth, check(longLivedTree)))
