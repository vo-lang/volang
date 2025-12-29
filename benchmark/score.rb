#!/usr/bin/env ruby

require 'json'

# Read all JSON files and calculate average relative scores
def calculate_scores
  results_dir = File.join(__dir__, 'results')
  scores = Hash.new { |h, k| h[k] = [] }
  
  puts "Reading results from: #{results_dir}"
  json_files = Dir.glob(File.join(results_dir, '*.json'))
  puts "Found JSON files: #{json_files.map { |f| File.basename(f) }.join(', ')}"
  
  json_files.each do |file|
    benchmark_name = File.basename(file, '.json')
    puts "\nProcessing: #{benchmark_name}"
    data = JSON.parse(File.read(file))
    
    # Debug: print all commands
    puts "  Commands found: #{data['results'].map { |r| r['command'] }.join(', ')}"
    
    next unless data['results']
    
    # Find the fastest (minimum mean) time to calculate relative scores
    mean_times = {}
    data['results'].each do |result|
      command = result['command']
      mean = result['mean']
      
      # Extract language name from command
      lang = case command
             when 'Vo-VM' then 'Vo-VM'
             when 'Vo-JIT' then 'Vo-JIT'
             when 'Go' then 'Go'
             when 'Lua' then 'Lua'
             when 'LuaJIT' then 'LuaJIT'
             when 'Python' then 'Python'
             when 'Ruby' then 'Ruby'
             when 'Java' then 'Java'
             when 'C' then 'C'
             else 
               puts "  Unmatched command: #{command}"
               next
             end
      
      mean_times[lang] = mean if lang
    end
    
    # Calculate relative scores (1.0 = fastest)
    fastest = mean_times.values.min
    mean_times.each do |lang, mean|
      relative = mean / fastest
      scores[lang] << relative
      puts "  #{lang}: #{relative.round(2)} (mean: #{mean.round(4)}s)"
    end
  end
  
  # Calculate averages
  averages = {}
  scores.each do |lang, values|
    next if values.empty?
    # Filter out nil values
    clean_values = values.compact
    next if clean_values.empty?
    averages[lang] = clean_values.sum.to_f / clean_values.length
  end
  
  # Debug: print what we found
  puts "Found data for #{averages.keys.length} languages: #{averages.keys.sort.join(', ')}" if !averages.empty?
  
  # Sort by score (lower is better)
  sorted = averages.sort_by { |_, score| score }
  
  puts "Language Performance Ranking (lower relative time is better):"
  puts "=" * 60
  sorted.each_with_index do |(lang, score), index|
    puts sprintf("%2d. %-8s: %.2f", index + 1, lang, score.round(2))
  end
  
  puts "\nDetailed relative times by benchmark:"
  puts "=" * 80
  
  # Print detailed table
  benchmarks = []
  Dir.glob(File.join(results_dir, '*.json')).each do |file|
    benchmarks << File.basename(file, '.json')
  end
  benchmarks.sort!
  
  # Header
  print sprintf("%-12s", "Benchmark")
  sorted.each { |lang, _| print sprintf(" %8s", lang) }
  puts
  
  puts "-" * 80
  
  # Data rows
  benchmarks.each do |benchmark|
    data = JSON.parse(File.read(File.join(results_dir, "#{benchmark}.json")))
    next unless data['results']
    
    print sprintf("%-12s", benchmark)
    
    # Find fastest for this benchmark
    mean_times = {}
    data['results'].each do |result|
      command = result['command']
      mean = result['mean']
      
      lang = case command
             when 'Vo-VM' then 'Vo-VM'
             when 'Vo-JIT' then 'Vo-JIT'
             when 'Go' then 'Go'
             when 'Lua' then 'Lua'
             when 'LuaJIT' then 'LuaJIT'
             when 'Python' then 'Python'
             when 'Ruby' then 'Ruby'
             when 'Java' then 'Java'
             when 'C' then 'C'
             else next
             end
      
      mean_times[lang] = mean if lang
    end
    
    fastest = mean_times.values.min
    
    sorted.each do |lang, _|
      if mean_times[lang]
        relative = mean_times[lang] / fastest
        print sprintf(" %8.2f", relative.round(2))
      else
        print sprintf(" %8s", "-")
      end
    end
    puts
  end
  
  puts "\nHow to read this table:"
puts "- Ranking: Lower average relative time is better (1.0x = fastest overall)"
puts "- Detailed table: 1.0 = fastest in that specific benchmark"
puts "- Example: If C shows 1.00 and Go shows 1.45 in sieve, Go is 45% slower than C"
puts "- Example: Ruby's 290.99 in matrix2 means it's ~291x slower than the fastest (C)"
puts "\nNote: Values are relative times (1.0 = fastest in each benchmark)"
end

calculate_scores
