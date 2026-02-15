function makeAdder(base) {
  return function (x) {
    return x + base;
  };
}

function main() {
  const iterations = 20000000;
  const f = makeAdder(7);
  let acc = 0;

  for (let i = 0; i < iterations; i++) {
    acc += f(i & 1023);
  }

  console.log(acc);
}

main();
