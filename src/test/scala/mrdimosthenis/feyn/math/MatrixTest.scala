package mrdimosthenis.feyn.math

import mrdimosthenis.feyn.math.extensions._
import minitest._

import scala.util.Random

object MatrixTest extends SimpleTestSuite {

  implicit val error: Threshold = 0.001

  val random: Random = new Random()

  test("Outer product of vectors") {
    val v1 = Vec(1.toComplex, 2.toComplex, 3.toComplex)
    val v2 = Vec(4.toComplex, 5.toComplex)
    val res = Matrix(
      Vec(4.toComplex, 5.toComplex),
      Vec(8.toComplex, 10.toComplex),
      Vec(12.toComplex, 15.toComplex)
    )
    assert(
      v1 ** v2 == res
    )
  }

  test("Addition") {
    val a1 = Matrix(
      Vec(Complex(0, 1), Complex(1, 1)),
      Vec(Complex(2, -3), 4.toComplex)
    )
    val a2 = Matrix(
      Vec(Complex(0, 2), Complex.zero),
      Vec(Complex(0, 1), Complex(1, 2))
    )
    val res = Matrix(
      Vec(Complex(0, 3), Complex(1, 1)),
      Vec(Complex(2, -2), Complex(5, 2))
    )
    assert(
      a1 + a2 == res
    )
  }

  test("Multiplication") {
    val a1 = Matrix(
      Vec(Complex(0, 2), Complex.zero),
      Vec(Complex(0, 1), Complex(1, 2))
    )
    val a2 = Matrix(
      Vec(Complex(0, 1), Complex(1, 1)),
      Vec(Complex(2, -3), 4.toComplex)
    )
    val res = Matrix(
      Vec(Complex(-2, 0), Complex(-2, 2)),
      Vec(Complex(7, 1), Complex(3, 9))
    )
    assert(
      a1 * a2 == res
    )
  }

  test("Tensor product") {
    val a1 = Matrix(
      Vec(1.toComplex, 2.toComplex),
      Vec(3.toComplex, 4.toComplex)
    )
    val a2 = Matrix(
      Vec(Complex.zero, 5.toComplex),
      Vec(6.toComplex, 7.toComplex)
    )
    val res = Matrix(
      Vec(Complex.zero, 5.toComplex, Complex.zero, 10.toComplex),
      Vec(6.toComplex, 7.toComplex, 12.toComplex, 14.toComplex),
      Vec(Complex.zero, 15.toComplex, Complex.zero, 20.toComplex),
      Vec(18.toComplex, 21.toComplex, 24.toComplex, 28.toComplex),
    )
    assert(
      a1 ** a2 == res
    )
  }

  test("Conjugate transpose") {
    val a = Matrix(
      Vec(Complex(3, 7), Complex.zero),
      Vec(Complex(0, 2), Complex(4, -1))
    )
    val res = Matrix(
      Vec(Complex(3, -7), Complex(0, -2)),
      Vec(Complex.zero, Complex(4, 1))
    )
    assert(
      a.transjugate == res
    )
  }

  test("Conjugate transpose properties of square matrices") {
    val z = random.nextComplex()
    val n = random.nextInt(10) + 1
    val a1 = random.nextMatrix(n, n)
    val a2 = random.nextMatrix(n, n)

    assert(
      a1.transjugate.transjugate =~ a1
    )

    assert(
      (a1 + a2).transjugate =~ a1.transjugate + a2.transjugate
    )

    assert(
      (z * a1).transjugate =~ z.conjugate * a1.transjugate
    )

    assert(
      (a1 * a2).transjugate =~ a2.transjugate * a1.transjugate
    )
  }

  test("Tensor product properties") {
    val z = random.nextComplex()
    val m1 = random.nextInt(10) + 1
    val n1 = random.nextInt(10) + 1
    val m2 = random.nextInt(10) + 1
    val n2 = random.nextInt(10) + 1
    val a1 = random.nextMatrix(m1, n1)
    val a2 = random.nextMatrix(m2, n2)
    val a3 = random.nextMatrix(m1, n1)
    val a4 = random.nextMatrix(m2, n2)

    assert(
      (a1 ** a2).transposed =~ a1.transposed ** a2.transposed
    )
    assert(
      (a1 ** a2).transjugate =~ a1.transjugate ** a2.transjugate
    )
    assert(
      z * (a1 ** a2) =~ (z * a1) ** a2 &&
        z * (a1 ** a2) =~ a1 ** (z * a2)
    )
    assert(
      (a1 + a3) ** a2 =~ a1 ** a2 + a3 ** a2
    )
    assert(
      a1 ** (a2 + a4) =~ a1 ** a2 + a1 ** a4
    )
  }

}
