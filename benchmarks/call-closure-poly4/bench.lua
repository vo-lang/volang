local function makeAdder(base)
  return function(x)
    return x + base
  end
end

local iterations = 20000000
local f0 = makeAdder(1)
local f1 = makeAdder(3)
local f2 = makeAdder(5)
local f3 = makeAdder(7)
local acc = 0

for i = 0, iterations - 1 do
  local x = i % 1024
  local slot = i % 4
  if slot == 0 then
    acc = acc + f0(x)
  elseif slot == 1 then
    acc = acc + f1(x)
  elseif slot == 2 then
    acc = acc + f2(x)
  else
    acc = acc + f3(x)
  end
end

print(acc)
