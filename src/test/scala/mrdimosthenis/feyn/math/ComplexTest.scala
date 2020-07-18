package mrdimosthenis.feyn.math

import mrdimosthenis.feyn.math.extensions._
import mrdimosthenis.feyn.graphics.svgExtensions._

import minitest._

import scala.util.Random
import scala.util.chaining._

object ComplexTest extends SimpleTestSuite {

  implicit val error: Threshold = 0.04

  val random: Random = new Random()

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
        .get
        .pipe {
          _ =~ Complex(5.0 / 29, -2.0 / 29)
        }
    )
  }

  test("Division of complex numbers") {
    assert(
      {
        Complex(3, 2) / Complex(0, 2)
      }.get
        .pipe {
          _ =~ Complex(1, -3.0 / 2)
        }
    )
  }

  val z1: Complex = random.nextComplex()
  val z2: Complex = random.nextComplex()

  test("Commutativity of addition property") {
    assert(
      z1 + z2 == z2 + z1
    )
  }

  test("Commutativity of multiplication property") {
    assert(
      z1 * z2 == z2 * z1
    )
  }

  test("SVG representation") {

    assert(
      Complex(0.2, -0.6).svg(60, "red").outerHTML ==
        "<svg height=\"60\" width=\"60\"><circle cx=\"30\" cy=\"30\" r=\"30\" fill-opacity=\"0\" stroke=\"red\"></circle><circle cx=\"30\" cy=\"30\" r=\"1\" fill-opacity=\"0.25\" stroke=\"red\"></circle><circle cx=\"36\" cy=\"48\" r=\"5\" fill-opacity=\"0.75\" fill=\"red\"></circle></svg>"
    )

    assert(
      Complex(-0.2, 0.6).svg(80, "blue").outerHTML ==
        "<svg height=\"80\" width=\"80\"><circle cx=\"40\" cy=\"40\" r=\"40\" fill-opacity=\"0\" stroke=\"blue\"></circle><circle cx=\"40\" cy=\"40\" r=\"1\" fill-opacity=\"0.25\" stroke=\"blue\"></circle><circle cx=\"32\" cy=\"16\" r=\"5\" fill-opacity=\"0.75\" fill=\"blue\"></circle></svg>"
    )
  }

}
