def fib(n)
  return n if n < 2
  fib(n - 1) + fib(n - 2)
end

n = 35
result = fib(n)
puts "fib(#{n}) = #{result}"
