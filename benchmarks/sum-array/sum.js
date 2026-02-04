const n = 10000000;
const arr = new Array(n);
for (let i = 0; i < n; i++) {
    arr[i] = i;
}

let sum = 0;
for (let i = 0; i < n; i++) {
    sum += arr[i];
}
console.log(sum);
