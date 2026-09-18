// Each run owns one worker. Deadlines belong to phases so a cold compiler load
// cannot consume the execution budget; cancellation always releases the worker.
export function createRunTask(counters, {
  createWorker = () => new Worker('/studio-assets/runner.js', {type:'module', name:'volang-playground'}),
  deadlines = {loading:45000, compiling:15000, running:10000},
} = {}) {
  return (source, signal) => {
    if (source.length > 100000) return Promise.reject(new Error('Please keep this example under 100,000 characters.'));
    if (signal.aborted) return Promise.reject(new Error('Run stopped.'));
    return new Promise((resolve, reject) => {
      const worker = createWorker();
      counters.started++;
      let finished = false, timer, phase = 'loading';
      const finish = (error, result) => {
        if (finished) return;
        finished = true;
        clearTimeout(timer);
        signal.removeEventListener('abort', abort);
        worker.terminate();
        counters.stopped++;
        if (error) reject(error); else resolve(JSON.stringify(result));
      };
      const abort = () => finish(new Error('Run stopped.'));
      const deadline = () => {
        clearTimeout(timer);
        timer = setTimeout(() => finish(new Error({
          loading:'The compiler took too long to load. Check your connection and try again.',
          compiling:'Compilation exceeded its time limit.',
          running:'Execution exceeded 10 seconds. Check for an infinite loop.',
        }[phase])), deadlines[phase]);
      };
      signal.addEventListener('abort', abort, {once:true});
      worker.onmessage = ({data}) => {
        if (finished) return;
        if (data?.kind === 'phase' && ((phase === 'loading' && data.phase === 'compiling')
          || (phase === 'compiling' && data.phase === 'running'))) {
          phase = data.phase;
          deadline();
        } else if (data?.kind === 'result' && typeof data.output === 'string') {
          finish(null, data);
        } else finish(new Error('The runner returned an unreadable result.'));
      };
      worker.onerror = event => {event.preventDefault(); finish(new Error(event.message || 'The runner could not start.'));};
      worker.onmessageerror = () => finish(new Error('The runner returned an unreadable result.'));
      deadline();
      try {worker.postMessage({source});} catch (error) {finish(error);}
    });
  };
}
