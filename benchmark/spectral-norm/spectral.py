import math

def a(i, j):
    return 1.0 / ((i + j) * (i + j + 1) // 2 + i + 1)

def multiply_Av(n, v, av):
    for i in range(n):
        av[i] = 0
        for j in range(n):
            av[i] += a(i, j) * v[j]

def multiply_Atv(n, v, atv):
    for i in range(n):
        atv[i] = 0
        for j in range(n):
            atv[i] += a(j, i) * v[j]

def multiply_AtAv(n, v, atav, av):
    multiply_Av(n, v, av)
    multiply_Atv(n, av, atav)

def spectral_norm(n):
    u = [1.0] * n
    v = [0.0] * n
    av = [0.0] * n

    for _ in range(10):
        multiply_AtAv(n, u, v, av)
        multiply_AtAv(n, v, u, av)

    vBv = 0.0
    vv = 0.0
    for i in range(n):
        vBv += u[i] * v[i]
        vv += v[i] * v[i]

    return math.sqrt(vBv / vv)

if __name__ == "__main__":
    n = 200
    result = spectral_norm(n)
    print(f"{result:.9f}")
