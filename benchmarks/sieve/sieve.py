def sieve(n):
    is_prime = [True] * (n + 1)
    is_prime[0] = False
    is_prime[1] = False

    i = 2
    while i * i <= n:
        if is_prime[i]:
            for j in range(i * i, n + 1, i):
                is_prime[j] = False
        i += 1

    count = 0
    for i in range(2, n + 1):
        if is_prime[i]:
            count += 1
    return count

n = 6000000
count = sieve(n)
print(f"primes up to {n}: {count}")
