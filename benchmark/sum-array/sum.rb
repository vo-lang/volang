n = 30000000
arr = (0...n).to_a

sum = 0
n.times do |i|
  sum += arr[i]
end
puts sum
