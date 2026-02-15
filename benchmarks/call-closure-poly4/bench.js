function makeAdder(base) {
  return function (x) {
    return x + base;
  };
}

function main() {
  const iterations = 20000000;
  const f0 = makeAdder(1);
  const f1 = makeAdder(3);
  const f2 = makeAdder(5);
  const f3 = makeAdder(7);
  let acc = 0;

  for (let i = 0; i < iterations; i++) {
    const x = i & 1023;
    const slot = i & 3;
    if (slot === 0) {
      acc += f0(x);
    } else if (slot === 1) {
      acc += f1(x);
    } else if (slot === 2) {
      acc += f2(x);
    } else {
      acc += f3(x);
    }
  }

  console.log(acc);
}

main();
