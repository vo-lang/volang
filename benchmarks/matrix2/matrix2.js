class Mat3 {
    constructor(m00, m01, m02, m10, m11, m12, m20, m21, m22) {
        this.m00 = m00; this.m01 = m01; this.m02 = m02;
        this.m10 = m10; this.m11 = m11; this.m12 = m12;
        this.m20 = m20; this.m21 = m21; this.m22 = m22;
    }
}

function matMulInto(result, a, b) {
    result.m00 = a.m00*b.m00 + a.m01*b.m10 + a.m02*b.m20;
    result.m01 = a.m00*b.m01 + a.m01*b.m11 + a.m02*b.m21;
    result.m02 = a.m00*b.m02 + a.m01*b.m12 + a.m02*b.m22;
    result.m10 = a.m10*b.m00 + a.m11*b.m10 + a.m12*b.m20;
    result.m11 = a.m10*b.m01 + a.m11*b.m11 + a.m12*b.m21;
    result.m12 = a.m10*b.m02 + a.m11*b.m12 + a.m12*b.m22;
    result.m20 = a.m20*b.m00 + a.m21*b.m10 + a.m22*b.m20;
    result.m21 = a.m20*b.m01 + a.m21*b.m11 + a.m22*b.m21;
    result.m22 = a.m20*b.m02 + a.m21*b.m12 + a.m22*b.m22;
}

const a = new Mat3(
    0.866, -0.5, 0.0,
    0.5, 0.866, 0.0,
    0.0, 0.0, 1.0
);

let result = new Mat3(
    1.0, 0.0, 0.0,
    0.0, 1.0, 0.0,
    0.0, 0.0, 1.0
);

const tmp = new Mat3(0, 0, 0, 0, 0, 0, 0, 0, 0);

for (let i = 0; i < 1000000; i++) {
    matMulInto(tmp, result, a);
    matMulInto(result, tmp, a);
}

console.log(`result[0][0]: ${result.m00}`);
