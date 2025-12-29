def a(i, j)
  1.0 / ((i + j) * (i + j + 1) / 2 + i + 1)
end

def multiply_Av(n, v, av)
  n.times do |i|
    av[i] = 0
    n.times do |j|
      av[i] += a(i, j) * v[j]
    end
  end
end

def multiply_Atv(n, v, atv)
  n.times do |i|
    atv[i] = 0
    n.times do |j|
      atv[i] += a(j, i) * v[j]
    end
  end
end

def multiply_AtAv(n, v, atav, av)
  multiply_Av(n, v, av)
  multiply_Atv(n, av, atav)
end

def spectral_norm(n)
  u = Array.new(n, 1.0)
  v = Array.new(n, 0.0)
  av = Array.new(n, 0.0)

  10.times do
    multiply_AtAv(n, u, v, av)
    multiply_AtAv(n, v, u, av)
  end

  vBv = 0.0
  vv = 0.0
  n.times do |i|
    vBv += u[i] * v[i]
    vv += v[i] * v[i]
  end

  Math.sqrt(vBv / vv)
end

n = 200
result = spectral_norm(n)
puts format("%.9f", result)
