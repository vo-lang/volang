class Node
  attr_accessor :left, :right

  def initialize(left = nil, right = nil)
    @left = left
    @right = right
  end
end

def make(depth)
  return Node.new if depth == 0
  Node.new(make(depth - 1), make(depth - 1))
end

def check(n)
  return 1 if n.left.nil?
  1 + check(n.left) + check(n.right)
end

def main
  min_depth = 4
  max_depth = 10
  stretch_depth = max_depth + 1

  stretch_tree = make(stretch_depth)
  puts "stretch tree of depth #{stretch_depth} check: #{check(stretch_tree)}"

  long_lived_tree = make(max_depth)

  (min_depth..max_depth).step(2) do |depth|
    iterations = 1 << (max_depth - depth + min_depth)
    total = 0
    iterations.times do
      tree = make(depth)
      total += check(tree)
    end
    puts "#{iterations} trees of depth #{depth} check: #{total}"
  end

  puts "long lived tree of depth #{max_depth} check: #{check(long_lived_tree)}"
end

main
