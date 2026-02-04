const StatusPending = 0;
const StatusRunning = 1;
const StatusCompleted = 2;
const StatusFailed = 3;

const PriorityLow = 0;
const PriorityMedium = 1;
const PriorityHigh = 2;

class Task {
    constructor(id, priority, duration) {
        this.id = id;
        this.priority = priority;
        this.status = StatusPending;
        this.duration = duration;
        this.retries = 0;
    }
}

class TaskQueue {
    constructor() {
        this.tasks = [];
        this.completed = 0;
        this.failed = 0;
        this.totalTime = 0;
    }

    addTask(id, priority, duration) {
        this.tasks.push(new Task(id, priority, duration));
    }

    findHighestPriority() {
        let bestIdx = -1;
        let bestPriority = -1;
        for (let i = 0; i < this.tasks.length; i++) {
            if (this.tasks[i].status === StatusPending) {
                if (this.tasks[i].priority > bestPriority) {
                    bestPriority = this.tasks[i].priority;
                    bestIdx = i;
                }
            }
        }
        return bestIdx;
    }

    processTask(idx, currentTime) {
        if (idx < 0 || idx >= this.tasks.length) {
            return false;
        }

        const task = this.tasks[idx];
        task.status = StatusRunning;

        // Simulate processing: tasks with odd duration "fail" on first try
        const shouldFail = task.duration % 2 === 1 && task.retries === 0;

        if (shouldFail) {
            task.retries++;
            task.status = StatusPending; // Retry
            return false;
        }

        // Check timeout (tasks > 100 duration fail after 3 retries)
        if (task.duration > 100 && task.retries >= 3) {
            task.status = StatusFailed;
            this.failed++;
            return false;
        }

        // Success
        task.status = StatusCompleted;
        this.completed++;
        this.totalTime += task.duration;
        return true;
    }

    runUntilEmpty() {
        let iterations = 0;
        while (true) {
            const idx = this.findHighestPriority();
            if (idx < 0) {
                break;
            }
            this.processTask(idx, iterations);
            iterations++;
            if (iterations > 1000000) {
                break; // Safety limit
            }
        }
        return iterations;
    }

    getStats() {
        return [this.completed, this.failed, this.totalTime];
    }
}

let totalCompleted = 0;
let totalFailed = 0;
let totalTime = 0;

const rounds = 200;
const tasksPerRound = 150;

for (let round = 0; round < rounds; round++) {
    const q = new TaskQueue();

    for (let i = 0; i < tasksPerRound; i++) {
        const priority = i % 3;
        const duration = (i * 7 + round) % 150;
        q.addTask(round * tasksPerRound + i, priority, duration);
    }

    q.runUntilEmpty();

    const [completed, failed, time] = q.getStats();
    totalCompleted += completed;
    totalFailed += failed;
    totalTime += time;
}

console.log(totalCompleted);
console.log(totalFailed);
console.log(totalTime);
