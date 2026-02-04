function a(i, j) {
    return 1.0 / ((i + j) * (i + j + 1) / 2 + i + 1);
}

function multiplyAv(n, v, av) {
    for (let i = 0; i < n; i++) {
        av[i] = 0;
        for (let j = 0; j < n; j++) {
            av[i] += a(i, j) * v[j];
        }
    }
}

function multiplyAtv(n, v, atv) {
    for (let i = 0; i < n; i++) {
        atv[i] = 0;
        for (let j = 0; j < n; j++) {
            atv[i] += a(j, i) * v[j];
        }
    }
}

function multiplyAtAv(n, v, atav, av) {
    multiplyAv(n, v, av);
    multiplyAtv(n, av, atav);
}

function spectralNorm(n) {
    const u = new Array(n).fill(1);
    const v = new Array(n).fill(0);
    const av = new Array(n).fill(0);

    for (let i = 0; i < 10; i++) {
        multiplyAtAv(n, u, v, av);
        multiplyAtAv(n, v, u, av);
    }

    let vBv = 0, vv = 0;
    for (let i = 0; i < n; i++) {
        vBv += u[i] * v[i];
        vv += v[i] * v[i];
    }

    return Math.sqrt(vBv / vv);
}

const n = 500;
const result = spectralNorm(n);
console.log(result.toFixed(9));
