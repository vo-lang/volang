local function fannkuch(n)
    local perm = {}
    local perm1 = {}
    local count = {}

    for i = 0, n - 1 do
        perm1[i] = i
    end

    local maxFlips = 0
    local checksum = 0
    local sign = 1
    local r = n

    while true do
        -- Generate next permutation
        while r ~= 1 do
            count[r - 1] = r
            r = r - 1
        end

        -- Copy perm1 to perm
        for i = 0, n - 1 do
            perm[i] = perm1[i]
        end

        -- Count flips
        local flips = 0
        while perm[0] ~= 0 do
            local k = perm[0] + 1
            -- Reverse first k elements
            local i, j = 0, k - 1
            while i < j do
                perm[i], perm[j] = perm[j], perm[i]
                i = i + 1
                j = j - 1
            end
            flips = flips + 1
        end

        if flips > maxFlips then
            maxFlips = flips
        end
        checksum = checksum + sign * flips
        sign = -sign

        -- Next permutation
        while true do
            if r == n then
                print(checksum)
                return maxFlips
            end
            -- Rotate perm1[0..r]
            local p0 = perm1[0]
            for i = 0, r - 1 do
                perm1[i] = perm1[i + 1]
            end
            perm1[r] = p0

            count[r] = count[r] - 1
            if count[r] > 0 then
                break
            end
            r = r + 1
        end
    end
end

local n = 10
local result = fannkuch(n)
print(string.format("Pfannkuchen(%d) = %d", n, result))
