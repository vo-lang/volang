import assert from 'node:assert/strict';
import test from 'node:test';
import { runAot } from '../dist/index.js';
import { AotMemoryRuntime } from '../dist/aot_memory.js';
import { compileAotSource } from '../aot_test_compiler.mjs';

async function execute(source, options = {}) {
  return runAot(compileAotSource(source), { fuel: 50000000n, ...options });
}

const memory_probe = `package main
import "fmt"
type Node struct { Value int; Next *Node }
func main() {
    root := &Node{Value: 7}
    for i := 0; i < 50000; i++ {
        p := &Node{Value: i, Next: root}
        assert(p.Next.Value == 7)
        if i % 1000 == 0 { root.Next = p }
    }
    fmt.Println(root.Value, root.Next.Value)
}
`;
const island_oom = `package main
func main() {
    bad := make(island)
    good := make(island)
    done := make(port int, 1)
    go @(bad) func() {
        values := make([]int, 1000000)
        println("BAD", len(values))
    }()
    go @(good) func(out port<- int) { out <- 7 }(done)
    assert(<-done == 7)
    println("healthy peers completed")
}
`;
const mem_api = `package main
import "runtime/mem"
func main() {
    before := mem.ReadStats()
    assert(mem.GCStep(0))
    after := mem.ReadStats()
    assert(after.WorkUnitsTotal == before.WorkUnitsTotal)
    assert(mem.GCStep(1))
    after = mem.ReadStats()
    assert(after.WorkUnitsTotal <= before.WorkUnitsTotal + 1)
    assert(mem.GCCollect())
    for i := 0; i < 100; i++ { _ = mem.ReadStats() }
    after = mem.ReadStats()
    assert(after.ManagedCommittedBytes >= after.ManagedLiveBytes)
    assert(after.WasmMaximumSet)
    println("memory controls passed")
}
`;
const async_memory = `package main
import "time"
func main() {
    worker :=  make(island)
    result := make(port int, 1)
    go @(worker) func(out port<- int) { time.Sleep(time.Millisecond); out <- 7 }(result)
    for i := 0; i < 100000; i++ { value := make([]int, 16); value[0] = i; assert(value[0] == i) }
    assert(<-result == 7)
    println("async owners passed")
}
`;

test('generated stores preserve old-to-young edges across bounded GC steps', async () => {
  const result = await execute(memory_probe, { memory: { stepUnits: 127, debtBytes: 32768 } });
  assert.equal(result.result.status, 'ok', result.result.stderr);
  assert.equal(result.result.stdout, '7 49000\n');
  assert.ok(result.memoryStats[0].collector.minorCycles > 0);
  assert.ok(result.memoryStats[0].collector.maxStepUnits <= 127);
  assert.ok(result.memoryStats[0].committedBytes < 1048576);
});

test('child OOM remains sticky, clears runtime state, and preserves peers', async () => {
  const result = await execute(island_oom, { memory: { hardLimitBytes: 2097152, stepUnits: 127 } });
  assert.equal(result.result.status, 'ok', result.result.stderr);
  assert.equal(result.result.stdout, 'healthy peers completed\n');
  const failed = result.memoryStats.filter(owner => owner.error);
  assert.equal(failed.length, 1);
  assert.equal(failed[0].error, 'HardLimitExceeded');
  assert.equal(failed[0].objects, 1, 'only stable Island state identity remains');
  assert.equal(failed[0].committedBytes, 65536);
});

test('os.Exit terminates the instance before defers or later fiber effects', async () => {
  for (const code of [0, 7]) {
    const result = await execute(`package main
import "os"
func main() {
    defer func() { println("unexpected defer") }()
    ready := make(chan int)
    release := make(chan int)
    go func() { ready <- 1; <-release; println("unexpected fiber") }()
    <-ready
    println("before exit")
    os.Exit(${code})
    release <- 1
    println("unexpected continuation")
}`);
    assert.equal(result.exitCode, code);
    assert.equal(result.result.status, code === 0 ? 'ok' : 'error');
    assert.equal(result.result.stdout, 'before exit\n');
    assert.equal(result.result.stderr, '');
  }
});

test('runtime/mem accepts zero work without mutation and bounds manual steps', async () => {
  const result = await execute(mem_api, { memory: { automaticGC: false, stepUnits: 127 } });
  assert.equal(result.result.status, 'ok', result.result.stderr);
  assert.equal(result.result.stdout, 'memory controls passed\n');
  assert.ok(result.memoryStats[0].collector.majorCycles >= 1);
  assert.ok(result.memoryStats[0].collector.maxStepUnits <= 127);
});

test('asynchronous host allocations retain their captured Island and expire after replay', async () => {
  let retainedCall;
  const result = await execute(async_memory, { memory: { stepUnits: 127, debtBytes: 32768 }, externs: {
    'vo1:4:time:18:blocking_sleepNano': { supportedEffects: 31n, handler: async call => {
      retainedCall = call;
      const reference = call.allocateString('retained through GC');
      // Even a rootless host temporary survives a collection while the caller waits.
      for (let i = 0; i < 10000; i++) call.allocateString('churn');
      await new Promise(resolve => setTimeout(resolve, 10));
      assert.equal(call.readString(reference), 'retained through GC');
    } },
  } });
  assert.equal(result.result.status, 'ok', result.result.stderr);
  assert.equal(result.result.stdout, 'async owners passed\n');
  assert.ok(retainedCall, 'custom async provider was used');
  assert.throws(() => retainedCall.allocateString('expired'), /expired.*lease/);
  assert.ok(result.memoryStats[1].allocationBytes > 100000);
});


function memoryFixture(policy = {}) {
  const empty = { slots: [], roots: [] };
  const metadata = {
    version: 1, frameDescriptor: 1, islandDescriptor: 2, stackBase: 5 * 65536, stackLimit: 6 * 65536, barrierPages: 64,
    frameBytes: 88, frameFunction: 16, frameParent: 48, frameDefers: [24, 32],
    fiberBytes: 176, fiberNext: 0, fiberFrame: 16, fiberIsland: 112,
    fiberPanicGeneration: 56, fiberPanic: 40, fiberPreviousPanic: 88,
    descriptors: [0, 1, 2, 6].map(tag => ({ tag, stride: 8, first: empty, second: empty, entries: empty })),
    frames: [{ slots: [3, 4, 0], roots: [1] }],
  };
  const memory = new WebAssembly.Memory({ initial: 6, maximum: 64 });
  const runtime = new AotMemoryRuntime(memory, 64, metadata, { automaticGC: false, ...policy });
  runtime.call(-8, 8, 2, 1, 0);
  runtime.call(-10, metadata.stackBase, 0, 0, 0);
  return { runtime, memory, frame: metadata.stackBase + metadata.frameBytes };
}

test('each GC request interrupts a quantum replenished by generated code', () => {
  const { runtime } = memoryFixture();
  const quantum = new WebAssembly.Global({ value: 'i32', mutable: true }, 16);
  runtime.attach({ exports: { vo_execution_quantum: quantum } });
  for (const request of [() => runtime.requestStep(1n), () => runtime.requestCollection()]) {
    for (let turn = 0; turn < 2; turn++) {
      quantum.value = 16;
      assert.equal(request(), true);
      assert.equal(quantum.value, 0);
    }
  }
});

test('freeing a frame from an earlier GC cycle preserves new allocation debt', () => {
  const { runtime } = memoryFixture({ automaticGC: true, debtBytes: 512 });
  const debt = new WebAssembly.Global({ value: 'i32', mutable: true }, 0);
  runtime.attach({ exports: { vo_gc_debt: debt } });
  const frame = runtime.call(-22, 112, 0, 0, 0);
  assert.ok(frame);
  runtime.requestCollection();
  while (runtime.drainStep()) {}
  assert.equal(debt.value, 0);
  assert.ok(runtime.call(-1, 512, 0, 0, 0));
  assert.ok(debt.value > 0);
  runtime.call(-5, frame, 0, 0, 0);
  assert.ok(debt.value > 0, 'old frame retirement cannot cancel pending guest GC');
});

test('asynchronous interface results publish atomically and root staged references', () => {
  const { runtime, memory, frame } = memoryFixture();
  const call = runtime.openCall(frame, 0, 3, Uint8Array.of(3, 4, 0), 28n);
  const pointer = call.run(() => runtime.call(-1, 16, 0, 0, 0));
  call.retain();
  call.run(() => runtime.stageSlot(frame, 1, BigInt(pointer)));
  runtime.requestCollection();
  while (runtime.drainStep()) {}
  assert.ok(runtime.provider.findHeader(pointer));
  assert.equal(new DataView(memory.buffer).getBigUint64(frame + 8, true), 0n);
  call.run(() => runtime.stageSlot(frame, 0, 17n));
  call.release();
  const view = new DataView(memory.buffer);
  assert.equal(view.getBigUint64(frame, true), 17n);
  assert.equal(view.getBigUint64(frame + 8, true), BigInt(pointer));
  assert.throws(() => call.run(() => 0), /expired.*lease/);
});

test('stale queue generations fail before reused endpoint storage is accessed', () => {
  const { runtime } = memoryFixture();
  const pointer = runtime.call(-1, 128, 3, 0, 0);
  const generation = runtime.provider.view().getUint32(pointer - 4, true);
  assert.equal(runtime.call(-11, pointer, generation, 0, 0), pointer);
  runtime.call(-5, pointer, 0, 0, 0);
  const reused = runtime.call(-1, 128, 3, 0, 0);
  assert.equal(reused, pointer);
  const next = runtime.provider.view().getUint32(reused - 4, true);
  assert.notEqual(next, generation);
  assert.throws(() => runtime.call(-11, reused, generation, 0, 0), /InvalidPointer/);
  assert.equal(runtime.call(-1, 8, 0, 0, 0), 0, 'failure remains sticky');
});

test('lease capacity is checked before an async call can retain owner state', () => {
  const { runtime, frame } = memoryFixture({ maxLeases: 0 });
  const call = runtime.openCall(frame, 0, 0, new Uint8Array(), 28n);
  assert.throws(() => call.retain(), /MetadataExhausted/);
  call.release();
  assert.equal(runtime.stats()[0].error, 'MetadataExhausted');
});


test('host admission controls remain Island-local and enforce safe boundaries', () => {
  const { runtime } = memoryFixture();
  const controls = runtime.controls();
  assert.ok(controls.reserve(3 * 65536));
  assert.ok(controls.setGrowthAllowed(false));
  assert.ok(controls.setMode('incremental'));
  controls.setAutomaticGC(false);
  controls.reportExternalBytes(1234);
  const before = runtime.publicStats();
  assert.equal(controls.step(0), 0);
  assert.deepEqual(runtime.publicStats(), before);
  assert.equal(before[7], 1234n);
  assert.equal(before[25], 0n);
  assert.equal(before[29], 1n);
  assert.equal(before[31], 0n);
  assert.equal(controls.setHardLimit(1), false);
  assert.ok(controls.setHardLimit(4 * 65536));
  const pointer = runtime.call(-1, 65536, 0, 0, 0);
  assert.ok(pointer);
  assert.equal(runtime.stats()[0].committedBytes, 4 * 65536);
  assert.equal(controls.step(1), 1);
  assert.equal(controls.setMode('generational'), false, 'active cycle retains its chosen mode');
  assert.throws(() => runtime.step(() => controls.reserve(65536)), /host scheduling boundary/);
});

test('explicit host leases survive calls and release their roots exactly once', () => {
  const { runtime } = memoryFixture({ maxLeases: 1 });
  const pointer = runtime.call(-1, 16, 0, 0, 0);
  const lease = runtime.lease(BigInt(pointer));
  runtime.requestCollection(); while (runtime.drainStep()) {}
  assert.equal(lease.resolve(), BigInt(pointer));
  lease.release(); lease.release();
  assert.throws(() => lease.resolve(), /expired.*lease/);
  runtime.requestCollection(); while (runtime.drainStep()) {}
  assert.equal(runtime.provider.findHeader(pointer), 0);
});

test('cancelled asynchronous returns cannot publish after guest execution ends', () => {
  const { runtime, memory, frame } = memoryFixture();
  const call = runtime.openCall(frame, 0, 3, Uint8Array.of(3, 4, 0), 28n);
  call.retain();
  call.run(() => runtime.stageSlot(frame, 2, 77n));
  call.cancel();
  assert.equal(new DataView(memory.buffer).getBigUint64(frame + 16, true), 0n);
  assert.throws(() => call.run(() => 0), /expired.*lease/);
  assert.equal(runtime.publicStats()[24], 0n);
});

test('generated barriers preserve wide, copied and conditional roots during bounded collection', async () => {
  const source = `package main
import "runtime/mem"
type Wide struct { Padding [40]int; Value any; Next *Wide }
func main() {
    old := &Wide{Value: "old"}
    values := make([]*Wide, 4)
    values[0] = old
    table := make(map[int]*Wide)
    table[0] = old
    assert(mem.GCCollect())
    assert(mem.GCCollect())
    for i := 0; i < 1000; i++ {
        assert(mem.GCStep(1))
        next := &Wide{Value: i, Next: old}
        old.Next = next
        values[1] = next
        copy(values[2:], values[:2])
        table[i % 13] = next
        old.Value = next
        assert(old.Next.Value.(int) == i)
        assert(values[3] == next)
    }
    assert(mem.GCCollect())
    assert(old.Value.(*Wide).Value.(int) == 999)
    assert(values[3].Value.(int) == 999)
    assert(table[999 % 13].Value.(int) == 999)
    println("typed barriers passed")
}
`;
  const result = await execute(source, { memory: { debtBytes: 16384, stepUnits: 31 } });
  assert.equal(result.result.status, 'ok', result.result.stderr);
  assert.equal(result.result.stdout, 'typed barriers passed\n');
  assert.ok(result.memoryStats[0].collector.majorCycles >= 3);
  assert.ok(result.memoryStats[0].collector.maxStepUnits <= 31);
});

test('host scheduling allows event-loop work while the guest remains runnable', async () => {
  const { driveAotScheduler } = await import('../dist/aot_scheduler.js');
  let timerRan = false, steps = 0;
  const timer = new Promise(resolve => setTimeout(() => { timerRan = true; resolve(); }, 0));
  const status = await driveAotScheduler(() => ++steps < 2000 ? 17 : 0, () => []);
  assert.equal(status, 0);
  assert.ok(timerRan, 'a runnable guest must not starve host tasks');
  await timer;
});


test('owner-local frame layouts retain staged conditional results and retire roots', () => {
  const { runtime, memory } = memoryFixture();
  const state = runtime.call(-8, 8, 2, 0, 0);
  assert.ok(state);
  runtime.call(-9, state, 0, 0, 0);
  const raw = runtime.call(-22, 112, 0, 0, 0);
  const frame = raw + runtime.metadata.frameBytes;
  const pointer = runtime.call(-1, 16, 0, 0, 0);
  assert.ok(raw && pointer);
  // Empty explicit types require the registered frame's complete layout.
  const call = runtime.openCall(frame, 0, 2, new Uint8Array(), 28n);
  call.retain();
  call.run(() => {
    assert.equal(runtime.stageSlot(frame, 0, 17n), true);
    assert.equal(runtime.stageSlot(frame, 1, BigInt(pointer)), true);
  });
  runtime.requestCollection(); while (runtime.drainStep()) {}
  assert.ok(runtime.provider.findHeader(pointer), 'staged child pointer remains rooted');
  call.release();
  assert.equal(new DataView(memory.buffer).getBigUint64(frame + 8, true), BigInt(pointer));
  runtime.requestCollection(); while (runtime.drainStep()) {}
  assert.ok(runtime.provider.findHeader(pointer), 'published child frame keeps the pointer');
  runtime.call(-5, raw, 0, 0, 0);
  runtime.requestCollection(); while (runtime.drainStep()) {}
  assert.equal(runtime.provider.findHeader(raw), 0, 'retired frame is no longer a root');
  assert.equal(runtime.provider.findHeader(pointer), 0, 'retired child frame releases its reachable graph');
  runtime.call(-9, 0, 0, 0, 0);
  assert.equal(runtime.stats()[0].error, undefined);
  assert.equal(runtime.stats()[1].error, undefined);
});


test('managed frame registration rejects an extent shorter than its layout', () => {
  const { runtime } = memoryFixture();
  const raw = runtime.call(-1, runtime.metadata.frameBytes, runtime.metadata.frameDescriptor, 0, 0);
  runtime.call(-10, raw, 0, 0, 0);
  assert.equal(runtime.stats()[0].error, 'InvalidPointer');
});

test('frame layout storage is admitted before disabling memory growth', () => {
  const { runtime } = memoryFixture({ initialReserveBytes: 2 * 65536, growthAllowed: false });
  const pointers = Array.from({ length: 64 }, () => runtime.call(-22, 112, 0, 0, 0));
  assert.ok(pointers.every(Boolean));
  runtime.requestCollection(); while (runtime.drainStep()) {}
  for (const pointer of pointers) assert.ok(runtime.provider.findHeader(pointer));
  for (const pointer of pointers) runtime.call(-5, pointer, 0, 0, 0);
  runtime.requestCollection(); while (runtime.drainStep()) {}
  for (const pointer of pointers) assert.equal(runtime.provider.findHeader(pointer), 0);
  assert.equal(runtime.stats()[0].error, undefined);
});


test('an interior address cannot borrow a registered frame layout', () => {
  const { runtime } = memoryFixture();
  const raw = runtime.call(-22, 112, 0, 0, 0);
  const pointer = runtime.call(-1, 16, 0, 0, 0);
  for (const [offset, expected] of [[0, true], [8, false]]) {
    const frame = raw + runtime.metadata.frameBytes + offset;
    const call = runtime.openCall(frame, 0, 2, new Uint8Array(), 28n);
    call.run(() => {
      assert.ok(runtime.stageSlot(frame, 0, 17n));
      assert.ok(runtime.stageSlot(frame, 1, BigInt(pointer)));
    });
    assert.equal(call.roots.has(pointer), expected);
    call.cancel();
  }
});


test('explicit frees reject interior, header, padding and retired addresses', () => {
  for (const bytes of [0, 1, 31, 33, 1024, 32736, 65537, 3 * 65536]) {
    const { runtime } = memoryFixture();
    const pointer = runtime.call(-1, bytes, 0, 0, 0);
    assert.ok(pointer);
    const span = runtime.provider.spanAtHeader(pointer - 32);
    const before = runtime.stats();
    const invalid = [0, 1, pointer - 32, pointer - 1, pointer + 1,
      pointer + Math.max(1, bytes - 1), pointer + bytes + 1, 0xffff_ffff];
    for (const address of invalid) {
      assert.equal(runtime.call(-5, address, 0, 0, 0), 0);
      assert.equal(runtime.provider.findHeader(pointer), pointer - 32);
      assert.deepEqual(runtime.stats(), before, `invalid free ${address}, size ${bytes}`);
    }
    runtime.call(-5, pointer, 0, 0, 0);
    assert.equal(runtime.provider.findHeader(pointer), 0);
    const after = runtime.stats();
    assert.equal(after[0].objects, before[0].objects - 1);
    assert.equal(after[0].liveBytes, before[0].liveBytes - bytes - 32);
    assert.equal(after[0].allocationBytes, before[0].allocationBytes);
    if (span.pages > 1) {
      assert.equal(span.reclaimPage, 1, 'one explicit free reclaims at most one page');
      assert.equal(span.retired, false);
    }
    runtime.call(-5, pointer, 0, 0, 0);
    assert.deepEqual(runtime.stats(), after, 'duplicate free is inert');
  }
});

test('explicit free follows the allocation owner and preserves sticky failures', () => {
  const { runtime } = memoryFixture();
  const rootPointer = runtime.call(-1, 16, 0, 0, 0);
  const childState = runtime.call(-8, 8, 2, 0, 0);
  runtime.call(-9, childState, 0, 0, 0);
  const childPointer = runtime.call(-1, 256, 0, 0, 0);
  const childBefore = runtime.stats()[1];
  runtime.call(-5, rootPointer, 0, 0, 0);
  assert.deepEqual(runtime.stats()[1], childBefore, 'active peer counters are unchanged');
  runtime.call(-9, 0, 0, 0, 0);
  const rootBefore = runtime.stats()[0];
  runtime.call(-5, childPointer, 0, 0, 0);
  assert.deepEqual(runtime.stats()[0], rootBefore, 'address selects the child owner');
  assert.equal(runtime.stats()[1].objects, childBefore.objects - 1);
  assert.equal(runtime.provider.findHeader(childState), childState - 32);
  const pointer = runtime.call(-1, 16, 0, 0, 0);
  runtime.controls().setAllocationAllowed(false);
  assert.equal(runtime.call(-1, 16, 0, 0, 0), 0);
  assert.equal(runtime.stats()[0].error, 'AllocationForbidden');
  runtime.call(-5, pointer, 0, 0, 0);
  assert.equal(runtime.provider.findHeader(pointer), 0);
  assert.equal(runtime.stats()[0].error, 'AllocationForbidden', 'cleanup retains the terminal error');
});


test('transfer staging filters foreign references while ordinary stores reject them', () => {
  for (const transfer of [false, true]) {
    const { runtime, memory, frame } = memoryFixture();
    const child = runtime.call(-8, 8, 2, 0, 0);
    runtime.call(-9, child, 0, 0, 0);
    const foreign = runtime.call(-1, 16, 0, 0, 0);
    runtime.call(-9, 0, 0, 0, 0);
    const view = new DataView(memory.buffer);
    view.setBigUint64(frame, 14n, true);
    view.setBigUint64(frame + 8, BigInt(foreign), true);
    runtime.write(frame + 8, 8, transfer);
    assert.equal(runtime.stats()[0].error, transfer ? undefined : 'InvalidPointer');
    assert.equal(runtime.stats()[1].error, undefined);
    assert.equal(runtime.provider.findHeader(foreign), foreign - 32);
  }
});

test('ordinary reference stores preserve nil and generation validation', () => {
  for (const [pointerKind, generation, expected] of [
    ['nil', 0n, undefined], ['nil', 5n, undefined],
    ['local', 0n, undefined], ['local', 5n, 'InvalidPointer'],
    ['retired', 0n, 'InvalidPointer'],
  ]) {
    const { runtime, memory, frame } = memoryFixture();
    const pointer = pointerKind === 'nil' ? 0 : runtime.call(-1, 16, 0, 0, 0);
    if (pointerKind === 'retired') runtime.call(-5, pointer, 0, 0, 0);
    const view = new DataView(memory.buffer);
    view.setBigUint64(frame, 14n, true);
    view.setBigUint64(frame + 8, (generation << 32n) | BigInt(pointer), true);
    runtime.write(frame + 8, 8);
    assert.equal(runtime.stats()[0].error, expected, `${pointerKind}, generation ${generation}`);
  }
});

test('retaining one host call repeatedly consumes exactly one lease', () => {
  const { runtime, frame } = memoryFixture({ maxLeases: 1 });
  const call = runtime.openCall(frame, 0, 0, new Uint8Array(), 28n);
  for (let i = 0; i < 100; i++) call.retain();
  assert.equal(runtime.publicStats()[24], 1n);
  assert.equal(runtime.stats()[0].error, undefined);
  assert.equal(call.owner.hostCalls.size, 1);
  call.leave();
  assert.equal(runtime.publicStats()[24], 1n, 'async leave preserves the retained call');
  call.release(); call.release();
  assert.equal(runtime.publicStats()[24], 0n);
  assert.equal(call.owner.hostCalls.size, 0);
  const next = runtime.openCall(frame, 0, 0, new Uint8Array(), 28n);
  next.retain(); next.release();
  assert.equal(runtime.publicStats()[24], 0n, 'the full lease quota is reusable');
});

test('closed host calls reject retain before modifying owner quota', () => {
  for (const maxLeases of [0, 1]) {
    const { runtime, frame } = memoryFixture({ maxLeases });
    const call = runtime.openCall(frame, 0, 0, new Uint8Array(), 28n);
    if (maxLeases) call.retain();
    call.release();
    assert.throws(() => call.retain(), /expired Volang host-call lease/);
    assert.equal(runtime.publicStats()[24], 0n);
    assert.equal(call.owner.hostCalls.size, 0);
    assert.equal(runtime.stats()[0].error, undefined, 'expired handles do not poison the Island');
    call.cancel(); call.release();
    assert.equal(runtime.publicStats()[24], 0n);
  }
});
