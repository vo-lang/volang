#!/usr/bin/env ruby

require 'json'
require 'fileutils'

class BenchmarkRunner
  def initialize
    @script_dir = __dir__
    @project_root = File.dirname(@script_dir)
    @results_dir = File.join(@script_dir, 'results')
    @run_all_langs = false
    @vo_mode = false
  end

  def run(args)
    parse_args(args)
    
    command = args.shift || 'all'
    
    case command
    when 'help', '--help', '-h'
      show_help
    when 'list'
      list_benchmarks
    when 'all'
      check_deps
      build_vo
      run_all_benchmarks
      calculate_scores
    when 'vo'
      @vo_mode = true
      check_deps
      build_vo
      run_all_benchmarks
      calculate_scores
    when 'score'
      calculate_scores
    else
      if benchmark_exists?(command)
        check_deps
        build_vo
        run_benchmark(command)
      else
        puts "Unknown benchmark: #{command}"
        list_benchmarks
        exit 1
      end
    end
  end

  private

  def parse_args(args)
    args.delete_if do |arg|
      if arg == '--all-langs'
        @run_all_langs = true
        true
      elsif arg == '--vo-mode'
        @vo_mode = true
        true
      else
        false
      end
    end
  end

  def check_deps
    missing = []
    
    missing << 'hyperfine' unless command_exists?('hyperfine')
    missing << 'go' unless command_exists?('go')
    
    unless missing.empty?
      puts "Missing dependencies: #{missing.join(', ')}"
      puts "Install with:"
      puts "  brew install #{missing.join(' ')}"
      exit 1
    end
  end

  def command_exists?(cmd)
    system("command -v #{cmd} > /dev/null 2>&1")
  end

  def build_vo
    puts "Building Vo (release)..."
    system("cargo build --release --bin vo --manifest-path #{@project_root}/Cargo.toml 2>/dev/null")
  end

  def show_help
    puts <<~HELP
      Vo Language Benchmark Suite
      
      Usage: bench.rb [OPTIONS] [COMMAND]
      
      Commands:
        all              Run all benchmarks and generate report (default)
        vo               Run only Vo (VM + JIT) and C benchmarks
        list             List available benchmarks
        score            Analyze existing results without running benchmarks
        <benchmark>      Run a specific benchmark (e.g., fibonacci, quicksort)
        help             Show this help message
      
      Options:
        --all-langs      Include Python and Ruby in benchmarks (disabled by default)
        --vo-mode        Only run Vo and C (same as 'vo' command)
      
      Examples:
        ./bench.rb                    # Run all benchmarks
        ./bench.rb vo                 # Run only Vo and C benchmarks
        ./bench.rb list               # List available benchmarks
        ./bench.rb fibonacci          # Run fibonacci benchmark only
        ./bench.rb --all-langs all    # Run all benchmarks including Python/Ruby
        ./bench.rb --vo-mode all      # Run all benchmarks but only Vo and C
        ./bench.rb score              # Analyze existing results
      
      Languages tested (by default):
        - Vo (VM and JIT modes)
        - Go
        - Lua / LuaJIT
        - Java
        - C
      
      With --all-langs:
        - Python
        - Ruby
      
      Results are saved in: #{@results_dir}
    HELP
  end

  def list_benchmarks
    puts "Available benchmarks:"
    Dir.glob(File.join(@script_dir, '*/')).each do |dir|
      name = File.basename(dir)
      next if name == 'results'
      puts "  - #{name}"
    end
  end

  def benchmark_exists?(name)
    Dir.exist?(File.join(@script_dir, name))
  end

  def run_all_benchmarks
    Dir.glob(File.join(@script_dir, '*/')).each do |dir|
      name = File.basename(dir)
      next if name == 'results'
      run_benchmark(name)
    end
  end

  def run_benchmark(name)
    dir = File.join(@script_dir, name)
    vo_bin = File.join(@project_root, 'target', 'release', 'vo')
    
    puts "\n=== #{name} ===\n"
    
    # Find source files
    vo_file = Dir.glob(File.join(dir, '*.vo')).first
    go_file = Dir.glob(File.join(dir, '*.go')).first
    lua_file = Dir.glob(File.join(dir, '*.lua')).first
    py_file = Dir.glob(File.join(dir, '*.py')).first
    rb_file = Dir.glob(File.join(dir, '*.rb')).first
    java_file = Dir.glob(File.join(dir, '*.java')).first
    c_file = Dir.glob(File.join(dir, '*.c')).first
    
    cmds = []
    names = []
    
    # Vo VM and JIT
    if vo_file
      cmds << "#{vo_bin} run '#{vo_file}'"
      names << 'Vo-VM'
      
      cmds << "#{vo_bin} run --mode=jit '#{vo_file}'"
      names << 'Vo-JIT'
    end
    
    # Go (pre-compile) - skip in vo mode
    if go_file && !@vo_mode
      go_bin = File.join(dir, 'go_bench')
      if system("go build -o #{go_bin} #{go_file} 2>/dev/null")
        cmds << "'#{go_bin}'"
        names << 'Go'
      end
    end
    
    # Lua - skip in vo mode
    if lua_file && command_exists?('lua') && !@vo_mode
      cmds << "lua '#{lua_file}'"
      names << 'Lua'
    end
    
    # LuaJIT - skip in vo mode
    if lua_file && command_exists?('luajit') && !@vo_mode
      cmds << "luajit '#{lua_file}'"
      names << 'LuaJIT'
    end
    
    # Python (only if --all-langs) - skip in vo mode
    if py_file && @run_all_langs && !@vo_mode
      cmds << "python3 '#{py_file}'"
      names << 'Python'
    end
    
    # Ruby (only if --all-langs) - skip in vo mode
    if rb_file && command_exists?('ruby') && @run_all_langs && !@vo_mode
      cmds << "ruby '#{rb_file}'"
      names << 'Ruby'
    end
    
    # Java (compile and run) - skip in vo mode
    if java_file && command_exists?('java') && command_exists?('javac') && !@vo_mode
      java_class = File.basename(java_file, '.java')
      if system("javac -d #{dir} #{java_file} 2>/dev/null")
        cmds << "java -cp '#{dir}' '#{java_class}'"
        names << 'Java'
      end
    end
    
    # C (compile and run)
    if c_file && (command_exists?('cc') || command_exists?('gcc') || command_exists?('clang'))
      c_bin = File.join(dir, 'c_bench')
      compiler = command_exists?('cc') ? 'cc' : (command_exists?('gcc') ? 'gcc' : 'clang')
      if system("#{compiler} -O3 -o #{c_bin} #{c_file} 2>/dev/null")
        cmds << "'#{c_bin}'"
        names << 'C'
      end
    end
    
    return if cmds.empty?
    
    # Build hyperfine command
    FileUtils.mkdir_p(@results_dir)
    export_json = File.join(@results_dir, "#{name}.json")
    export_md = File.join(@results_dir, "#{name}.md")
    
    hf_args = ['hyperfine', '--warmup', '1', '--runs', '3']
    cmds.each_with_index do |cmd, i|
      hf_args << '-n' << names[i] << cmd
    end
    hf_args << '--export-json' << export_json
    hf_args << '--export-markdown' << export_md
    
    system(*hf_args)
  end

  def calculate_scores
    puts "\n=== Calculating Scores ===\n"
    
    scores = Hash.new { |h, k| h[k] = [] }
    
    puts "Reading results from: #{@results_dir}"
    json_files = Dir.glob(File.join(@results_dir, '*.json'))
    
    if json_files.empty?
      puts "No results found. Run benchmarks first."
      return
    end
    
    puts "Found JSON files: #{json_files.map { |f| File.basename(f) }.join(', ')}"
    
    json_files.each do |file|
      benchmark_name = File.basename(file, '.json')
      puts "\nProcessing: #{benchmark_name}"
      data = JSON.parse(File.read(file))
      
      next unless data['results']
      
      # Extract mean times
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
      
      # Calculate relative scores (skip invalid times)
      valid_times = mean_times.reject { |_, mean| mean.nil? || mean.nan? || mean.infinite? || mean == 0 }
      next if valid_times.empty?
      
      # In vo mode, use Vo-VM as baseline; otherwise use fastest
      baseline = if @vo_mode && valid_times['Vo-VM']
                   valid_times['Vo-VM']
                 else
                   valid_times.values.min
                 end
      
      valid_times.each do |lang, mean|
        relative = mean / baseline
        scores[lang] << relative
        puts "  #{lang}: #{relative.round(2)} (mean: #{mean.round(4)}s)"
      end
    end
    
    # Calculate averages
    averages = {}
    scores.each do |lang, values|
      next if values.empty?
      clean_values = values.compact
      next if clean_values.empty?
      averages[lang] = clean_values.sum.to_f / clean_values.length
    end
    
    if averages.empty?
      puts "\nNo valid results to analyze."
      return
    end
    
    # Sort by score (lower is better)
    sorted = averages.sort_by { |_, score| score }
    
    puts "\n" + "=" * 60
    puts "Language Performance Ranking (lower relative time is better):"
    puts "=" * 60
    sorted.each_with_index do |(lang, score), index|
      puts sprintf("%2d. %-8s: %.2f", index + 1, lang, score.round(2))
    end
    
    puts "\n" + "=" * 80
    puts "Detailed relative times by benchmark:"
    puts "=" * 80
    
    # Print detailed table
    benchmarks = json_files.map { |f| File.basename(f, '.json') }.sort
    
    # Header
    print sprintf("%-15s", "Benchmark")
    sorted.each { |lang, _| print sprintf(" %8s", lang) }
    puts
    puts "-" * 80
    
    # Data rows
    benchmarks.each do |benchmark|
      data = JSON.parse(File.read(File.join(@results_dir, "#{benchmark}.json")))
      next unless data['results']
      
      print sprintf("%-15s", benchmark)
      
      # Extract mean times
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
      
      # Skip benchmarks with invalid times
      valid_times = mean_times.reject { |_, mean| mean.nil? || mean.nan? || mean.infinite? || mean == 0 }
      if valid_times.empty?
        sorted.each { |_, _| print sprintf(" %8s", "N/A") }
        puts
        next
      end
      
      # In vo mode, use Vo-VM as baseline; otherwise use fastest
      baseline = if @vo_mode && valid_times['Vo-VM']
                   valid_times['Vo-VM']
                 else
                   valid_times.values.min
                 end
      
      sorted.each do |lang, _|
        if valid_times[lang]
          relative = valid_times[lang] / baseline
          print sprintf(" %8.2f", relative.round(2))
        else
          print sprintf(" %8s", "-")
        end
      end
      puts
    end
    
    puts "\nHow to read this table:"
    if @vo_mode
      puts "- Ranking: Relative to Vo-VM baseline (1.0x = same as Vo-VM)"
      puts "- Detailed table: 1.0 = same speed as Vo-VM, <1.0 = faster, >1.0 = slower"
    else
      puts "- Ranking: Lower average relative time is better (1.0x = fastest overall)"
      puts "- Detailed table: 1.0 = fastest in that specific benchmark"
    end
    puts "- '-' means the language was not tested for that benchmark"
    puts "- 'N/A' means all results were invalid (too fast to measure accurately)"
  end
end

if __FILE__ == $0
  runner = BenchmarkRunner.new
  runner.run(ARGV.dup)
end
