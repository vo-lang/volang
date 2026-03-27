local function makeAdder(base)
  return function(x)
    return x + base
  end
end

local Inc = {}
Inc.__index = Inc
function Inc.new(base) return setmetatable({ base = base }, Inc) end
function Inc:add(x) return x + self.base end

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

local n = 5000000
local acc = 0

-- closure mono
local f = makeAdder(7)
for i = 0, n - 1 do
  acc = acc + f(i % 1024)
end

-- closure poly4
local f0 = makeAdder(1)
local f1 = makeAdder(3)
local f2 = makeAdder(5)
local f3 = makeAdder(7)
for i = 0, n - 1 do
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

-- iface mono
local adder = Inc.new(7)
for i = 0, n - 1 do
  acc = acc + adder:add(i % 1024)
end

-- iface poly4
local a0 = Add1.new()
local a1 = Add3.new()
local a2 = Add5.new()
local a3 = Add7.new()
for i = 0, n - 1 do
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
