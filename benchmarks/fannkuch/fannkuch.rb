def fannkuch(n)
  perm = Array.new(n, 0)
  perm1 = (0...n).to_a
  count = Array.new(n, 0)

  max_flips = 0
  checksum = 0
  sign = 1
  r = n

  loop do
    # Generate next permutation
    while r != 1
      count[r - 1] = r
      r -= 1
    end

    # Copy perm1 to perm
    perm = perm1.dup

    # Count flips
    flips = 0
    while perm[0] != 0
      k = perm[0] + 1
      # Reverse first k elements
      perm[0, k] = perm[0, k].reverse
      flips += 1
    end

    max_flips = flips if flips > max_flips
    checksum += sign * flips
    sign = -sign

    # Next permutation
    loop do
      if r == n
        puts checksum
        return max_flips
      end
      # Rotate perm1[0..r]
      p0 = perm1[0]
      (0...r).each { |i| perm1[i] = perm1[i + 1] }
      perm1[r] = p0

      count[r] -= 1
      break if count[r] > 0
      r += 1
    end
  end
end

n = 10
result = fannkuch(n)
puts "Pfannkuchen(#{n}) = #{result}"
