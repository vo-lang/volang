local function a(i, j)
    return 1.0 / ((i + j) * (i + j + 1) / 2 + i + 1)
end

local function multiply_Av(n, v, av)
    for i = 0, n - 1 do
        av[i] = 0
        for j = 0, n - 1 do
            av[i] = av[i] + a(i, j) * v[j]
        end
    end
end

local function multiply_Atv(n, v, atv)
    for i = 0, n - 1 do
        atv[i] = 0
        for j = 0, n - 1 do
            atv[i] = atv[i] + a(j, i) * v[j]
        end
    end
end

local function multiply_AtAv(n, v, atav, av)
    multiply_Av(n, v, av)
    multiply_Atv(n, av, atav)
end

local function spectral_norm(n)
    local u = {}
    local v = {}
    local av = {}

    for i = 0, n - 1 do
        u[i] = 1
        v[i] = 0
        av[i] = 0
    end

    for _ = 1, 10 do
        multiply_AtAv(n, u, v, av)
        multiply_AtAv(n, v, u, av)
    end

    local vBv = 0
    local vv = 0
    for i = 0, n - 1 do
        vBv = vBv + u[i] * v[i]
        vv = vv + v[i] * v[i]
    end

    return math.sqrt(vBv / vv)
end

local n = 500
local result = spectral_norm(n)
print(string.format("%.9f", result))
