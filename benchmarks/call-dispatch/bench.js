function makeAdder(base) {
  return function (x) {
    return x + base;
  };
}

class Inc {
  constructor(base) {
    this.base = base;
  }
  add(x) {
    return x + this.base;
  }
}

class Add1 { add(x) { return x + 1; } }
class Add3 { add(x) { return x + 3; } }
class Add5 { add(x) { return x + 5; } }
class Add7 { add(x) { return x + 7; } }

function main() {
  const n = 5000000;
  let acc = 0;

  // closure mono
  const f = makeAdder(7);
  for (let i = 0; i < n; i++) {
    acc += f(i & 1023);
  }

  // closure poly4
  const f0 = makeAdder(1);
  const f1 = makeAdder(3);
  const f2 = makeAdder(5);
  const f3 = makeAdder(7);
  for (let i = 0; i < n; i++) {
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

  // iface mono
  const adder = new Inc(7);
  for (let i = 0; i < n; i++) {
    acc += adder.add(i & 1023);
  }

  // iface poly4
  const a0 = new Add1();
  const a1 = new Add3();
  const a2 = new Add5();
  const a3 = new Add7();
  for (let i = 0; i < n; i++) {
    const x = i & 1023;
    const slot = i & 3;
    if (slot === 0) {
      acc += a0.add(x);
    } else if (slot === 1) {
      acc += a1.add(x);
    } else if (slot === 2) {
      acc += a2.add(x);
    } else {
      acc += a3.add(x);
    }
  }

  console.log(acc);
}

main();
