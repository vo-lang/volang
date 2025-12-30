class Node
  attr_accessor :value, :left, :right
  
  def initialize(value)
    @value = value
    @left = nil
    @right = nil
  end
  
  def insert(val)
    if val < @value
      if @left.nil?
        @left = Node.new(val)
      else
        @left.insert(val)
      end
    else
      if @right.nil?
        @right = Node.new(val)
      else
        @right.insert(val)
      end
    end
  end
  
  def sum
    total = @value
    total += @left.sum unless @left.nil?
    total += @right.sum unless @right.nil?
    total
  end
  
  def count
    c = 1
    c += @left.count unless @left.nil?
    c += @right.count unless @right.nil?
    c
  end
  
  def height
    lh = @left.nil? ? 0 : @left.height
    rh = @right.nil? ? 0 : @right.height
    [lh, rh].max + 1
  end
end

def make_tree(values)
  return nil if values.empty?
  root = Node.new(values[0])
  values[1..-1].each { |val| root.insert(val) }
  root
end

def fibonacci(n)
  return n if n <= 1
  fibonacci(n - 1) + fibonacci(n - 2)
end

def process_with_closure(arr, multiplier)
  fib10 = fibonacci(10)
  
  transform = ->(x) { x * multiplier + fib10 }
  
  arr.map { |x| transform.call(x) }
end

def matrix_multiply(a, b)
  n = a.length
  result = Array.new(n) { Array.new(n, 0) }
  
  n.times do |i|
    n.times do |j|
      sum = 0
      n.times do |k|
        sum += a[i][k] * b[k][j]
      end
      result[i][j] = sum
    end
  end
  
  result
end

def main
  iterations = 500
  tree_size = 100
  matrix_size = 20
  
  total_sum = 0
  total_count = 0
  total_height = 0
  
  iterations.times do |iter|
    values = (0...tree_size).map { |i| (i * 7 + iter * 13) % 1000 }
    
    tree = make_tree(values)
    total_sum += tree.sum
    total_count += tree.count
    total_height += tree.height
    
    processed = process_with_closure(values, iter % 10 + 1)
    processed.each { |val| total_sum += val % 100 }
    
    if iter % 50 == 0
      mat_a = Array.new(matrix_size) { |i| Array.new(matrix_size) { |j| (i + j + iter) % 10 } }
      mat_b = Array.new(matrix_size) { |i| Array.new(matrix_size) { |j| (i * j + iter) % 10 } }
      
      mat_c = matrix_multiply(mat_a, mat_b)
      mat_c.each do |row|
        row.each { |val| total_sum += val }
      end
    end
  end
  
  puts "#{total_sum} #{total_count} #{total_height}"
end

main
