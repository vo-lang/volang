function fib(n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

const n = 35;
const result = fib(n);
console.log(`fib(${n}) = ${result}`);
