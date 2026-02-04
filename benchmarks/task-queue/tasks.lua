local StatusPending = 0
local StatusRunning = 1
local StatusCompleted = 2
local StatusFailed = 3

local PriorityLow = 0
local PriorityMedium = 1
local PriorityHigh = 2

local function newTask(id, priority, duration)
    return {
        id = id,
        priority = priority,
        status = StatusPending,
        duration = duration,
        retries = 0
    }
end

local function newTaskQueue()
    return {
        tasks = {},
        completed = 0,
        failed = 0,
        totalTime = 0
    }
end

local function addTask(q, id, priority, duration)
    table.insert(q.tasks, newTask(id, priority, duration))
end

local function findHighestPriority(q)
    local bestIdx = -1
    local bestPriority = -1
    for i = 1, #q.tasks do
        if q.tasks[i].status == StatusPending then
            if q.tasks[i].priority > bestPriority then
                bestPriority = q.tasks[i].priority
                bestIdx = i
            end
        end
    end
    return bestIdx
end

local function processTask(q, idx, currentTime)
    if idx < 1 or idx > #q.tasks then
        return false
    end
    
    local task = q.tasks[idx]
    task.status = StatusRunning
    
    -- Simulate processing: tasks with odd duration "fail" on first try
    local shouldFail = task.duration % 2 == 1 and task.retries == 0
    
    if shouldFail then
        task.retries = task.retries + 1
        task.status = StatusPending -- Retry
        return false
    end
    
    -- Check timeout (tasks > 100 duration fail after 3 retries)
    if task.duration > 100 and task.retries >= 3 then
        task.status = StatusFailed
        q.failed = q.failed + 1
        return false
    end
    
    -- Success
    task.status = StatusCompleted
    q.completed = q.completed + 1
    q.totalTime = q.totalTime + task.duration
    return true
end

local function runUntilEmpty(q)
    local iterations = 0
    while true do
        local idx = findHighestPriority(q)
        if idx < 0 then
            break
        end
        processTask(q, idx, iterations)
        iterations = iterations + 1
        if iterations > 1000000 then
            break -- Safety limit
        end
    end
    return iterations
end

-- Main
local totalCompleted = 0
local totalFailed = 0
local totalTime = 0

local rounds = 200
local tasksPerRound = 150

for round = 0, rounds - 1 do
    local q = newTaskQueue()
    
    for i = 0, tasksPerRound - 1 do
        local priority = i % 3
        local duration = (i * 7 + round) % 150
        addTask(q, round * tasksPerRound + i, priority, duration)
    end
    
    runUntilEmpty(q)
    
    totalCompleted = totalCompleted + q.completed
    totalFailed = totalFailed + q.failed
    totalTime = totalTime + q.totalTime
end

print(totalCompleted)
print(totalFailed)
print(totalTime)
