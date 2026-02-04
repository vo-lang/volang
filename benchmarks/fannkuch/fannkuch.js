function fannkuch(n) {
    const perm = new Array(n);
    const perm1 = new Array(n);
    const count = new Array(n);

    for (let i = 0; i < n; i++) {
        perm1[i] = i;
    }

    let maxFlips = 0;
    let checksum = 0;
    let sign = 1;
    let r = n;

    while (true) {
        // Generate next permutation
        while (r !== 1) {
            count[r - 1] = r;
            r--;
        }

        // Copy perm1 to perm
        for (let i = 0; i < n; i++) {
            perm[i] = perm1[i];
        }

        // Count flips
        let flips = 0;
        while (perm[0] !== 0) {
            const k = perm[0] + 1;
            // Reverse first k elements
            for (let i = 0, j = k - 1; i < j; i++, j--) {
                const tmp = perm[i];
                perm[i] = perm[j];
                perm[j] = tmp;
            }
            flips++;
        }

        if (flips > maxFlips) {
            maxFlips = flips;
        }
        checksum += sign * flips;
        sign = -sign;

        // Next permutation
        while (true) {
            if (r === n) {
                console.log(checksum);
                return maxFlips;
            }
            // Rotate perm1[0..r]
            const p0 = perm1[0];
            for (let i = 0; i < r; i++) {
                perm1[i] = perm1[i + 1];
            }
            perm1[r] = p0;

            count[r]--;
            if (count[r] > 0) {
                break;
            }
            r++;
        }
    }
}

const n = 9;
const result = fannkuch(n);
console.log(`Pfannkuchen(${n}) = ${result}`);
