package mrdimosthenis.feyn.math

import mrdimosthenis.feyn.math.extensions._
import minitest._

import scala.util.Random

object VecTest extends SimpleTestSuite {

  implicit val error: Threshold = 0.001

  val random: Random = new Random()

  test("Addition") {
    val v1 = Vec(Complex(1, 2), Complex(3, -1))
    val v2 = Vec(Complex(-2, 1), Complex(4, 0))
    assert(
      v1 + v2 == Vec(Complex(-1, 3), Complex(7, -1))
    )
  }

  test("Multiplication") {
    val z = Complex(2, 1)
    val v = Vec(Complex(1, 2), Complex(3, -1))
    assert(
      z * v == Vec(Complex(0, 5), Complex(7, 1))
    )
  }

  test("Inner product") {
    val v1 = Vec(Complex(2, 1), Complex.zero, Complex(4, -5))
    val v2 = Vec(Complex(1, 1), Complex(2, 1), Complex.zero)
    assert(
      v1 * v2 == Complex(3, -1)
    )
  }

  test("Combination of basic operations") {
    val z = Complex(5, -1)
    val v1 = Vec(Complex(1, 2), Complex(3, -1))
    val v2 = Vec(Complex(-2, 1), Complex(4, 0))
    assert(
      3 * v1 - z * v2 == Vec(Complex(12, -1), Complex(-11, 1))
    )
  }

  test("Norm") {
    val v1 = Vec(Complex(2, 1), Complex.zero, Complex(4, -5))
    val v2 = Vec(Complex(1, 1), Complex(2, 1), Complex.zero)

    assert(
      v1.norm == Math.sqrt(46)
    )

    assert(
      v2.norm == Math.sqrt(7)
    )
  }

  test("Basic representation") {
    val z1 = random.nextComplex()
    val z2 = random.nextComplex()
    val z3 = random.nextComplex()
    val one = Complex(1, 0)
    val v1 = Vec(one, Complex.zero, Complex.zero)
    val v2 = Vec(Complex.zero, one, Complex.zero)
    val v3 = Vec(Complex.zero, Complex.zero, one)
    assert(
      z1 * v1 + z2 * v2 + z3 * v3 == Vec(z1, z2, z3)
    )
  }

  test("Inner product properties") {
    val z = random.nextComplex()
    val n = random.nextInt(10) + 1
    val v1 = random.nextVec(n)
    val v2 = random.nextVec(n)
    val v3 = random.nextVec(n)

    assert(
      v1 * v2 =~ (v2 * v1).conjugate
    )

    assert(
      (v1 + v2) * v3 =~ v1 * v3 + v2 * v3
    )

    assert(
      (z * v1) * v2 =~ z * (v1 * v2)
    )

    assert(
      v1 * (z * v2) =~ z.conjugate * (v1 * v2)
    )

    assert(
      (v1 * v1).re >= 0 && (v1 * v1).im =~ 0
    )
  }

}
