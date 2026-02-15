local Inc = {}
Inc.__index = Inc

function Inc.new(base)
  return setmetatable({ base = base }, Inc)
end

function Inc:add(x)
  return x + self.base
end

local iterations = 20000000
local adder = Inc.new(7)
local acc = 0

for i = 0, iterations - 1 do
  acc = acc + adder:add(i % 1024)
end

print(acc)
