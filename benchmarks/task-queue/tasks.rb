STATUS_PENDING = 0
STATUS_RUNNING = 1
STATUS_COMPLETED = 2
STATUS_FAILED = 3

PRIORITY_LOW = 0
PRIORITY_MEDIUM = 1
PRIORITY_HIGH = 2

class Task
  attr_accessor :id, :priority, :status, :duration, :retries

  def initialize(id, priority, duration)
    @id = id
    @priority = priority
    @status = STATUS_PENDING
    @duration = duration
    @retries = 0
  end
end

class TaskQueue
  attr_accessor :tasks, :completed, :failed, :total_time

  def initialize
    @tasks = []
    @completed = 0
    @failed = 0
    @total_time = 0
  end

  def add_task(id, priority, duration)
    @tasks << Task.new(id, priority, duration)
  end

  def find_highest_priority
    best_idx = -1
    best_priority = -1
    @tasks.each_with_index do |task, i|
      if task.status == STATUS_PENDING
        if task.priority > best_priority
          best_priority = task.priority
          best_idx = i
        end
      end
    end
    best_idx
  end

  def process_task(idx, current_time)
    return false if idx < 0 || idx >= @tasks.length

    task = @tasks[idx]
    task.status = STATUS_RUNNING

    # Simulate processing: tasks with odd duration "fail" on first try
    should_fail = task.duration % 2 == 1 && task.retries == 0

    if should_fail
      task.retries += 1
      task.status = STATUS_PENDING  # Retry
      return false
    end

    # Check timeout (tasks > 100 duration fail after 3 retries)
    if task.duration > 100 && task.retries >= 3
      task.status = STATUS_FAILED
      @failed += 1
      return false
    end

    # Success
    task.status = STATUS_COMPLETED
    @completed += 1
    @total_time += task.duration
    true
  end

  def run_until_empty
    iterations = 0
    loop do
      idx = find_highest_priority
      break if idx < 0
      process_task(idx, iterations)
      iterations += 1
      break if iterations > 1000000  # Safety limit
    end
    iterations
  end

  def get_stats
    [@completed, @failed, @total_time]
  end
end

def main
  total_completed = 0
  total_failed = 0
  total_time = 0

  rounds = 200
  tasks_per_round = 150

  rounds.times do |round|
    q = TaskQueue.new

    tasks_per_round.times do |i|
      priority = i % 3
      duration = (i * 7 + round) % 150
      q.add_task(round * tasks_per_round + i, priority, duration)
    end

    q.run_until_empty

    completed, failed, time = q.get_stats
    total_completed += completed
    total_failed += failed
    total_time += time
  end

  puts total_completed
  puts total_failed
  puts total_time
end

main
