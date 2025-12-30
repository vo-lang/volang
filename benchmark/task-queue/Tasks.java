import java.util.ArrayList;

public class Tasks {
    static final int STATUS_PENDING = 0;
    static final int STATUS_RUNNING = 1;
    static final int STATUS_COMPLETED = 2;
    static final int STATUS_FAILED = 3;

    static final int PRIORITY_LOW = 0;
    static final int PRIORITY_MEDIUM = 1;
    static final int PRIORITY_HIGH = 2;

    static class Task {
        int id;
        int priority;
        int status;
        int duration;
        int retries;

        Task(int id, int priority, int duration) {
            this.id = id;
            this.priority = priority;
            this.status = STATUS_PENDING;
            this.duration = duration;
            this.retries = 0;
        }
    }

    static class TaskQueue {
        ArrayList<Task> tasks = new ArrayList<>();
        int completed = 0;
        int failed = 0;
        int totalTime = 0;

        void addTask(int id, int priority, int duration) {
            tasks.add(new Task(id, priority, duration));
        }

        int findHighestPriority() {
            int bestIdx = -1;
            int bestPriority = -1;
            for (int i = 0; i < tasks.size(); i++) {
                Task task = tasks.get(i);
                if (task.status == STATUS_PENDING) {
                    if (task.priority > bestPriority) {
                        bestPriority = task.priority;
                        bestIdx = i;
                    }
                }
            }
            return bestIdx;
        }

        boolean processTask(int idx, int currentTime) {
            if (idx < 0 || idx >= tasks.size()) {
                return false;
            }

            Task task = tasks.get(idx);
            task.status = STATUS_RUNNING;

            // Simulate processing: tasks with odd duration "fail" on first try
            boolean shouldFail = task.duration % 2 == 1 && task.retries == 0;

            if (shouldFail) {
                task.retries++;
                task.status = STATUS_PENDING; // Retry
                return false;
            }

            // Check timeout (tasks > 100 duration fail after 3 retries)
            if (task.duration > 100 && task.retries >= 3) {
                task.status = STATUS_FAILED;
                failed++;
                return false;
            }

            // Success
            task.status = STATUS_COMPLETED;
            completed++;
            totalTime += task.duration;
            return true;
        }

        int runUntilEmpty() {
            int iterations = 0;
            while (true) {
                int idx = findHighestPriority();
                if (idx < 0) {
                    break;
                }
                processTask(idx, iterations);
                iterations++;
                if (iterations > 1000000) {
                    break; // Safety limit
                }
            }
            return iterations;
        }
    }

    public static void main(String[] args) {
        int totalCompleted = 0;
        int totalFailed = 0;
        int totalTime = 0;

        int rounds = 2000;
        int tasksPerRound = 150;

        for (int round = 0; round < rounds; round++) {
            TaskQueue q = new TaskQueue();

            for (int i = 0; i < tasksPerRound; i++) {
                int priority = i % 3;
                int duration = (i * 7 + round) % 150;
                q.addTask(round * tasksPerRound + i, priority, duration);
            }

            q.runUntilEmpty();

            totalCompleted += q.completed;
            totalFailed += q.failed;
            totalTime += q.totalTime;
        }

        System.out.println(totalCompleted);
        System.out.println(totalFailed);
        System.out.println(totalTime);
    }
}
