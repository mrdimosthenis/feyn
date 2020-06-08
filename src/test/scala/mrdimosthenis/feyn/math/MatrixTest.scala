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

  test("Outer product (of vectors) properties") {
    val z = random.nextComplex()
    val n = random.nextInt(10) + 1
    val v1 = random.nextVec(n)
    val v2 = random.nextVec(n)
    val v3 = random.nextVec(n)

//    assert(
//      (v1 ** v2).transposed =~ v2 ** v1
//    )

  }

}
