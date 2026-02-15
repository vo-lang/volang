class Inc {
  constructor(base) {
    this.base = base;
  }

  add(x) {
    return x + this.base;
  }
}

function main() {
  const iterations = 20000000;
  const adder = new Inc(7);
  let acc = 0;

  for (let i = 0; i < iterations; i++) {
    acc += adder.add(i & 1023);
  }

  console.log(acc);
}

main();
