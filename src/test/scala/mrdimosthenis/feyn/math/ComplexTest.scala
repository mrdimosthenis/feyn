package mrdimosthenis.feyn.math

import mrdimosthenis.feyn.math.extensions._
import minitest._

import scala.util.Random
import scala.util.chaining._

object ComplexTest extends SimpleTestSuite {

  implicit val error: Threshold = 0.04

  val random : Random = new Random()

  test("Addition") {
    assert(
      Complex(5, 2) + Complex(-4, 7) == Complex(1, 9)
    )
  }

  test("Multiplication") {
    assert(
      Complex(5, 2) * Complex(-4, 3) == Complex(-26, 7)
    )
  }

  test("Convert from polar") {
    assert(
      Complex.fromPolar(3, 40.toRadians) =~ Complex(2.3, 1.9)
    )
  }

  test("Convert to polar") {
    val (r, theta) = Complex(-1, 2).toPolar
    assert(
      (r =~ 2.2) && (theta =~ -63.4.toRadians)
    )
  }

  test("Rationalisation") {
    assert(
      Complex(5, 2)
        .inverse
        .pipe { case Some(z) =>
          z =~ Complex(5.0 / 29, -2.0 / 29)
        }
    )
  }

  test("Division of complex numbers") {
    assert(
      {
        Complex(3, 2) / Complex(0, 2)
      }.pipe { case Some(z) =>
        z =~ Complex(1, -3.0 / 2)
      }
    )
  }

  val z1: Complex = random.nextComplex()
  val z2: Complex = random.nextComplex()

  test("Commutativity of addition") {
    assert(
      z1 + z2 == z2 + z1
    )
  }

  test("Commutativity of multiplication") {
    assert(
      z1 * z2 == z2 * z1
    )
  }

}
