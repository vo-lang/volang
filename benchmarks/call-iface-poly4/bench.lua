local Add1 = {}
Add1.__index = Add1
function Add1.new() return setmetatable({}, Add1) end
function Add1:add(x) return x + 1 end

local Add3 = {}
Add3.__index = Add3
function Add3.new() return setmetatable({}, Add3) end
function Add3:add(x) return x + 3 end

local Add5 = {}
Add5.__index = Add5
function Add5.new() return setmetatable({}, Add5) end
function Add5:add(x) return x + 5 end

local Add7 = {}
Add7.__index = Add7
function Add7.new() return setmetatable({}, Add7) end
function Add7:add(x) return x + 7 end

local iterations = 20000000
local a0 = Add1.new()
local a1 = Add3.new()
local a2 = Add5.new()
local a3 = Add7.new()
local acc = 0

for i = 0, iterations - 1 do
  local x = i % 1024
  local slot = i % 4
  if slot == 0 then
    acc = acc + a0:add(x)
  elseif slot == 1 then
    acc = acc + a1:add(x)
  elseif slot == 2 then
    acc = acc + a2:add(x)
  else
    acc = acc + a3:add(x)
  end
end

print(acc)
