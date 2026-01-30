public class Matrix2 {
    static class Mat3 {
        double m00, m01, m02;
        double m10, m11, m12;
        double m20, m21, m22;
    }

    static void matMulInto(Mat3 result, Mat3 a, Mat3 b) {
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

    public static void main(String[] args) {
        Mat3 a = new Mat3();
        a.m00 = 0.866; a.m01 = -0.5; a.m02 = 0.0;
        a.m10 = 0.5; a.m11 = 0.866; a.m12 = 0.0;
        a.m20 = 0.0; a.m21 = 0.0; a.m22 = 1.0;

        Mat3 result = new Mat3();
        result.m00 = 1.0; result.m01 = 0.0; result.m02 = 0.0;
        result.m10 = 0.0; result.m11 = 1.0; result.m12 = 0.0;
        result.m20 = 0.0; result.m21 = 0.0; result.m22 = 1.0;

        Mat3 tmp = new Mat3();

        for (int i = 0; i < 300000; i++) {
            matMulInto(tmp, result, a);
            matMulInto(result, tmp, a);
        }

        System.out.printf("result[0][0]: %f%n", result.m00);
    }
}
