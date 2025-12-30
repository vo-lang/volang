def mat_mul_into(result, a, b)
  result[:m00] = a[:m00]*b[:m00] + a[:m01]*b[:m10] + a[:m02]*b[:m20]
  result[:m01] = a[:m00]*b[:m01] + a[:m01]*b[:m11] + a[:m02]*b[:m21]
  result[:m02] = a[:m00]*b[:m02] + a[:m01]*b[:m12] + a[:m02]*b[:m22]
  result[:m10] = a[:m10]*b[:m00] + a[:m11]*b[:m10] + a[:m12]*b[:m20]
  result[:m11] = a[:m10]*b[:m01] + a[:m11]*b[:m11] + a[:m12]*b[:m21]
  result[:m12] = a[:m10]*b[:m02] + a[:m11]*b[:m12] + a[:m12]*b[:m22]
  result[:m20] = a[:m20]*b[:m00] + a[:m21]*b[:m10] + a[:m22]*b[:m20]
  result[:m21] = a[:m20]*b[:m01] + a[:m21]*b[:m11] + a[:m22]*b[:m21]
  result[:m22] = a[:m20]*b[:m02] + a[:m21]*b[:m12] + a[:m22]*b[:m22]
end

a = {
  m00: 0.866, m01: -0.5, m02: 0.0,
  m10: 0.5, m11: 0.866, m12: 0.0,
  m20: 0.0, m21: 0.0, m22: 1.0,
}

result = {
  m00: 1.0, m01: 0.0, m02: 0.0,
  m10: 0.0, m11: 1.0, m12: 0.0,
  m20: 0.0, m21: 0.0, m22: 1.0,
}

tmp = {
  m00: 0, m01: 0, m02: 0,
  m10: 0, m11: 0, m12: 0,
  m20: 0, m21: 0, m22: 0,
}

3000000.times do
  mat_mul_into(tmp, result, a)
  mat_mul_into(result, tmp, a)
end

puts "result[0][0]: #{result[:m00]}"
