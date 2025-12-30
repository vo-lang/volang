local Node = {}
Node.__index = Node

function Node:new(value)
    local node = {
        value = value,
        left = nil,
        right = nil
    }
    setmetatable(node, Node)
    return node
end

function Node:insert(val)
    if val < self.value then
        if self.left == nil then
            self.left = Node:new(val)
        else
            self.left:insert(val)
        end
    else
        if self.right == nil then
            self.right = Node:new(val)
        else
            self.right:insert(val)
        end
    end
end

function Node:sum()
    local total = self.value
    if self.left ~= nil then
        total = total + self.left:sum()
    end
    if self.right ~= nil then
        total = total + self.right:sum()
    end
    return total
end

function Node:count()
    local c = 1
    if self.left ~= nil then
        c = c + self.left:count()
    end
    if self.right ~= nil then
        c = c + self.right:count()
    end
    return c
end

function Node:height()
    local lh = 0
    local rh = 0
    if self.left ~= nil then
        lh = self.left:height()
    end
    if self.right ~= nil then
        rh = self.right:height()
    end
    if lh > rh then
        return lh + 1
    else
        return rh + 1
    end
end

local function makeTree(values)
    if #values == 0 then
        return nil
    end
    local root = Node:new(values[1])
    for i = 2, #values do
        root:insert(values[i])
    end
    return root
end

local function fibonacci(n)
    if n <= 1 then
        return n
    end
    return fibonacci(n - 1) + fibonacci(n - 2)
end

local function processWithClosure(arr, multiplier)
    local result = {}
    local fib10 = fibonacci(10)
    
    local function transform(x)
        return x * multiplier + fib10
    end
    
    for i = 1, #arr do
        result[i] = transform(arr[i])
    end
    
    return result
end

local function matrixMultiply(a, b)
    local n = #a
    local result = {}
    for i = 1, n do
        result[i] = {}
        for j = 1, n do
            local sum = 0
            for k = 1, n do
                sum = sum + a[i][k] * b[k][j]
            end
            result[i][j] = sum
        end
    end
    return result
end

local function main()
    local iterations = 500
    local treeSize = 100
    local matrixSize = 20
    
    local totalSum = 0
    local totalCount = 0
    local totalHeight = 0
    
    for iter = 0, iterations - 1 do
        local values = {}
        for i = 0, treeSize - 1 do
            values[i + 1] = (i * 7 + iter * 13) % 1000
        end
        
        local tree = makeTree(values)
        totalSum = totalSum + tree:sum()
        totalCount = totalCount + tree:count()
        totalHeight = totalHeight + tree:height()
        
        local processed = processWithClosure(values, iter % 10 + 1)
        for i = 1, #processed do
            totalSum = totalSum + (processed[i] % 100)
        end
        
        if iter % 50 == 0 then
            local matA = {}
            local matB = {}
            for i = 1, matrixSize do
                matA[i] = {}
                matB[i] = {}
                for j = 1, matrixSize do
                    matA[i][j] = (i - 1 + j - 1 + iter) % 10
                    matB[i][j] = ((i - 1) * (j - 1) + iter) % 10
                end
            end
            
            local matC = matrixMultiply(matA, matB)
            for i = 1, matrixSize do
                for j = 1, matrixSize do
                    totalSum = totalSum + matC[i][j]
                end
            end
        end
    end
    
    print(totalSum, totalCount, totalHeight)
end

main()
