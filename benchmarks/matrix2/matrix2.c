#include <stdio.h>

typedef struct {
    double m00, m01, m02;
    double m10, m11, m12;
    double m20, m21, m22;
} Mat3;

void matMulInto(Mat3* result, Mat3* a, Mat3* b) {
    result->m00 = a->m00*b->m00 + a->m01*b->m10 + a->m02*b->m20;
    result->m01 = a->m00*b->m01 + a->m01*b->m11 + a->m02*b->m21;
    result->m02 = a->m00*b->m02 + a->m01*b->m12 + a->m02*b->m22;
    result->m10 = a->m10*b->m00 + a->m11*b->m10 + a->m12*b->m20;
    result->m11 = a->m10*b->m01 + a->m11*b->m11 + a->m12*b->m21;
    result->m12 = a->m10*b->m02 + a->m11*b->m12 + a->m12*b->m22;
    result->m20 = a->m20*b->m00 + a->m21*b->m10 + a->m22*b->m20;
    result->m21 = a->m20*b->m01 + a->m21*b->m11 + a->m22*b->m21;
    result->m22 = a->m20*b->m02 + a->m21*b->m12 + a->m22*b->m22;
}

int main() {
    Mat3 a = {
        0.866, -0.5, 0.0,
        0.5, 0.866, 0.0,
        0.0, 0.0, 1.0
    };

    Mat3 result = {
        1.0, 0.0, 0.0,
        0.0, 1.0, 0.0,
        0.0, 0.0, 1.0
    };

    Mat3 tmp = {0};

    for (int i = 0; i < 300000; i++) {
        matMulInto(&tmp, &result, &a);
        matMulInto(&result, &tmp, &a);
    }

    printf("result[0][0]: %f\n", result.m00);
    return 0;
}
