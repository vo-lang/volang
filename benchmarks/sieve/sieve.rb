def sieve(n)
  is_prime = Array.new(n + 1, true)
  is_prime[0] = false
  is_prime[1] = false

  i = 2
  while i * i <= n
    if is_prime[i]
      j = i * i
      while j <= n
        is_prime[j] = false
        j += i
      end
    end
    i += 1
  end

  count = 0
  (2..n).each do |i|
    count += 1 if is_prime[i]
  end
  count
end

n = 6000000
count = sieve(n)
puts "primes up to #{n}: #{count}"
