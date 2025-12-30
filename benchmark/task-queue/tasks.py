STATUS_PENDING = 0
STATUS_RUNNING = 1
STATUS_COMPLETED = 2
STATUS_FAILED = 3

PRIORITY_LOW = 0
PRIORITY_MEDIUM = 1
PRIORITY_HIGH = 2

class Task:
    def __init__(self, id, priority, duration):
        self.id = id
        self.priority = priority
        self.status = STATUS_PENDING
        self.duration = duration
        self.retries = 0

class TaskQueue:
    def __init__(self):
        self.tasks = []
        self.completed = 0
        self.failed = 0
        self.total_time = 0
    
    def add_task(self, id, priority, duration):
        self.tasks.append(Task(id, priority, duration))
    
    def find_highest_priority(self):
        best_idx = -1
        best_priority = -1
        for i, task in enumerate(self.tasks):
            if task.status == STATUS_PENDING:
                if task.priority > best_priority:
                    best_priority = task.priority
                    best_idx = i
        return best_idx
    
    def process_task(self, idx, current_time):
        if idx < 0 or idx >= len(self.tasks):
            return False
        
        task = self.tasks[idx]
        task.status = STATUS_RUNNING
        
        # Simulate processing: tasks with odd duration "fail" on first try
        should_fail = task.duration % 2 == 1 and task.retries == 0
        
        if should_fail:
            task.retries += 1
            task.status = STATUS_PENDING  # Retry
            return False
        
        # Check timeout (tasks > 100 duration fail after 3 retries)
        if task.duration > 100 and task.retries >= 3:
            task.status = STATUS_FAILED
            self.failed += 1
            return False
        
        # Success
        task.status = STATUS_COMPLETED
        self.completed += 1
        self.total_time += task.duration
        return True
    
    def run_until_empty(self):
        iterations = 0
        while True:
            idx = self.find_highest_priority()
            if idx < 0:
                break
            self.process_task(idx, iterations)
            iterations += 1
            if iterations > 1000000:
                break  # Safety limit
        return iterations
    
    def get_stats(self):
        return self.completed, self.failed, self.total_time

def main():
    total_completed = 0
    total_failed = 0
    total_time = 0
    
    rounds = 2000
    tasks_per_round = 150
    
    for round in range(rounds):
        q = TaskQueue()
        
        for i in range(tasks_per_round):
            priority = i % 3
            duration = (i * 7 + round) % 150
            q.add_task(round * tasks_per_round + i, priority, duration)
        
        q.run_until_empty()
        
        completed, failed, time = q.get_stats()
        total_completed += completed
        total_failed += failed
        total_time += time
    
    print(total_completed)
    print(total_failed)
    print(total_time)

if __name__ == "__main__":
    main()
