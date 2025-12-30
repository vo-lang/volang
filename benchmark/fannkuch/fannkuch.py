def fannkuch(n):
    perm = [0] * n
    perm1 = list(range(n))
    count = [0] * n

    max_flips = 0
    checksum = 0
    sign = 1
    r = n

    while True:
        # Generate next permutation
        while r != 1:
            count[r - 1] = r
            r -= 1

        # Copy perm1 to perm
        perm[:] = perm1[:]

        # Count flips
        flips = 0
        while perm[0] != 0:
            k = perm[0] + 1
            # Reverse first k elements
            perm[:k] = perm[:k][::-1]
            flips += 1

        if flips > max_flips:
            max_flips = flips
        checksum += sign * flips
        sign = -sign

        # Next permutation
        while True:
            if r == n:
                print(checksum)
                return max_flips
            # Rotate perm1[0..r]
            p0 = perm1[0]
            for i in range(r):
                perm1[i] = perm1[i + 1]
            perm1[r] = p0

            count[r] -= 1
            if count[r] > 0:
                break
            r += 1

if __name__ == "__main__":
    n = 10
    result = fannkuch(n)
    print(f"Pfannkuchen({n}) = {result}")
