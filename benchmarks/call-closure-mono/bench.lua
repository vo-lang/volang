local function makeAdder(base)
  return function(x)
    return x + base
  end
end

local iterations = 20000000
local f = makeAdder(7)
local acc = 0

for i = 0, iterations - 1 do
  acc = acc + f(i % 1024)
end

print(acc)
