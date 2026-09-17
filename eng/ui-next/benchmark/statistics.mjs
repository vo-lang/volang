export function distribution(values) {
  if (!values.length || !values.every(Number.isFinite)) throw new Error('A measurement distribution requires finite samples.');
  const sorted=[...values].sort((a,b)=>a-b);
  const percentile=p=>sorted[Math.ceil(sorted.length*p)-1];
  return {n:values.length,p50:percentile(.5),p95:percentile(.95),p99:percentile(.99)};
}
