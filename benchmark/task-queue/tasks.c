#include <stdio.h>
#include <stdlib.h>

#define STATUS_PENDING 0
#define STATUS_RUNNING 1
#define STATUS_COMPLETED 2
#define STATUS_FAILED 3

#define PRIORITY_LOW 0
#define PRIORITY_MEDIUM 1
#define PRIORITY_HIGH 2

typedef struct {
    int id;
    int priority;
    int status;
    int duration;
    int retries;
} Task;

typedef struct {
    Task* tasks;
    int size;
    int capacity;
    int completed;
    int failed;
    int totalTime;
} TaskQueue;

TaskQueue* newTaskQueue() {
    TaskQueue* q = (TaskQueue*)malloc(sizeof(TaskQueue));
    q->capacity = 128;
    q->tasks = (Task*)malloc(q->capacity * sizeof(Task));
    q->size = 0;
    q->completed = 0;
    q->failed = 0;
    q->totalTime = 0;
    return q;
}

void freeTaskQueue(TaskQueue* q) {
    free(q->tasks);
    free(q);
}

void addTask(TaskQueue* q, int id, int priority, int duration) {
    if (q->size >= q->capacity) {
        q->capacity *= 2;
        q->tasks = (Task*)realloc(q->tasks, q->capacity * sizeof(Task));
    }
    Task* t = &q->tasks[q->size++];
    t->id = id;
    t->priority = priority;
    t->status = STATUS_PENDING;
    t->duration = duration;
    t->retries = 0;
}

int findHighestPriority(TaskQueue* q) {
    int bestIdx = -1;
    int bestPriority = -1;
    for (int i = 0; i < q->size; i++) {
        if (q->tasks[i].status == STATUS_PENDING) {
            if (q->tasks[i].priority > bestPriority) {
                bestPriority = q->tasks[i].priority;
                bestIdx = i;
            }
        }
    }
    return bestIdx;
}

int processTask(TaskQueue* q, int idx, int currentTime) {
    if (idx < 0 || idx >= q->size) {
        return 0;
    }

    Task* task = &q->tasks[idx];
    task->status = STATUS_RUNNING;

    // Simulate processing: tasks with odd duration "fail" on first try
    int shouldFail = task->duration % 2 == 1 && task->retries == 0;

    if (shouldFail) {
        task->retries++;
        task->status = STATUS_PENDING; // Retry
        return 0;
    }

    // Check timeout (tasks > 100 duration fail after 3 retries)
    if (task->duration > 100 && task->retries >= 3) {
        task->status = STATUS_FAILED;
        q->failed++;
        return 0;
    }

    // Success
    task->status = STATUS_COMPLETED;
    q->completed++;
    q->totalTime += task->duration;
    return 1;
}

int runUntilEmpty(TaskQueue* q) {
    int iterations = 0;
    while (1) {
        int idx = findHighestPriority(q);
        if (idx < 0) {
            break;
        }
        processTask(q, idx, iterations);
        iterations++;
        if (iterations > 1000000) {
            break; // Safety limit
        }
    }
    return iterations;
}

int main() {
    int totalCompleted = 0;
    int totalFailed = 0;
    int totalTime = 0;

    int rounds = 1000;
    int tasksPerRound = 100;

    for (int round = 0; round < rounds; round++) {
        TaskQueue* q = newTaskQueue();

        for (int i = 0; i < tasksPerRound; i++) {
            int priority = i % 3;
            int duration = (i * 7 + round) % 150;
            addTask(q, round * tasksPerRound + i, priority, duration);
        }

        runUntilEmpty(q);

        totalCompleted += q->completed;
        totalFailed += q->failed;
        totalTime += q->totalTime;

        freeTaskQueue(q);
    }

    printf("%d\n", totalCompleted);
    printf("%d\n", totalFailed);
    printf("%d\n", totalTime);
    return 0;
}
