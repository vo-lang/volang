class Add1 { add(x) { return x + 1; } }
class Add3 { add(x) { return x + 3; } }
class Add5 { add(x) { return x + 5; } }
class Add7 { add(x) { return x + 7; } }

function main() {
  const iterations = 20000000;
  const a0 = new Add1();
  const a1 = new Add3();
  const a2 = new Add5();
  const a3 = new Add7();
  let acc = 0;

  for (let i = 0; i < iterations; i++) {
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
