package main

import "fmt"

const (
	StatusPending   = 0
	StatusRunning   = 1
	StatusCompleted = 2
	StatusFailed    = 3
)

const (
	PriorityLow    = 0
	PriorityMedium = 1
	PriorityHigh   = 2
)

type Task struct {
	id       int
	priority int
	status   int
	duration int
	retries  int
}

type TaskQueue struct {
	tasks     []Task
	completed int
	failed    int
	totalTime int
}

func newTaskQueue() *TaskQueue {
	return &TaskQueue{
		tasks:     make([]Task, 0),
		completed: 0,
		failed:    0,
		totalTime: 0,
	}
}

func (q *TaskQueue) addTask(id, priority, duration int) {
	task := Task{
		id:       id,
		priority: priority,
		status:   StatusPending,
		duration: duration,
		retries:  0,
	}
	q.tasks = append(q.tasks, task)
}

func (q *TaskQueue) findHighestPriority() int {
	bestIdx := -1
	bestPriority := -1
	for i := 0; i < len(q.tasks); i++ {
		if q.tasks[i].status == StatusPending {
			if q.tasks[i].priority > bestPriority {
				bestPriority = q.tasks[i].priority
				bestIdx = i
			}
		}
	}
	return bestIdx
}

func (q *TaskQueue) processTask(idx int, currentTime int) bool {
	if idx < 0 || idx >= len(q.tasks) {
		return false
	}
	
	task := &q.tasks[idx]
	task.status = StatusRunning
	
	// Simulate processing: tasks with odd duration "fail" on first try
	shouldFail := task.duration%2 == 1 && task.retries == 0
	
	if shouldFail {
		task.retries++
		task.status = StatusPending // Retry
		return false
	}
	
	// Check timeout (tasks > 100 duration fail after 3 retries)
	if task.duration > 100 && task.retries >= 3 {
		task.status = StatusFailed
		q.failed++
		return false
	}
	
	// Success
	task.status = StatusCompleted
	q.completed++
	q.totalTime += task.duration
	return true
}

func (q *TaskQueue) runUntilEmpty() int {
	iterations := 0
	for {
		idx := q.findHighestPriority()
		if idx < 0 {
			break
		}
		q.processTask(idx, iterations)
		iterations++
		if iterations > 1000000 {
			break // Safety limit
		}
	}
	return iterations
}

func (q *TaskQueue) getStats() (int, int, int) {
	return q.completed, q.failed, q.totalTime
}

func main() {
	totalCompleted := 0
	totalFailed := 0
	totalTime := 0
	
	// Run multiple rounds
	rounds := 1000
	tasksPerRound := 100
	
	for round := 0; round < rounds; round++ {
		q := newTaskQueue()
		
		// Add tasks with varying priorities and durations
		for i := 0; i < tasksPerRound; i++ {
			priority := i % 3
			duration := (i * 7 + round) % 150
			q.addTask(round*tasksPerRound+i, priority, duration)
		}
		
		q.runUntilEmpty()
		
		completed, failed, time := q.getStats()
		totalCompleted += completed
		totalFailed += failed
		totalTime += time
	}
	
	fmt.Println(totalCompleted)
	fmt.Println(totalFailed)
	fmt.Println(totalTime)
}
