package mrdimosthenis.feyn.quantum

import minitest.SimpleTestSuite
import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._
import mrdimosthenis.feyn.quantum.gates._
import mrdimosthenis.feyn.quantum.extensions._

import scala.util.Random

object QStateTest extends SimpleTestSuite {

  implicit val error: Threshold = 0.001

  val random: Random = new Random()

  test("Not gate on single qubit state") {
    val initQState = QState.init(1)
    val resQState = QState(
      Vec(Complex.zero, 1.toComplex)
    )
    assert(
      initQState.getThrough(Q1Gate.x, 0) equal resQState
    )
  }

  test("NOT is its own inverse property") {
    val size = random.nextInt(3) + 1
    val k = random.nextInt(size)
    val qState = random.nextQState(size)

    val res =
      qState
        .getThrough(Q1Gate.x, k)
        .getThrough(Q1Gate.x, k)

    assert(
      qState almostEqual res
    )
  }

  test("HAD gate on single qubit state") {
    val initQState = QState.init(1)
    val resQState = QState(
      Vec(0.7071.toComplex, 0.7071.toComplex)
    )
    assert(
      initQState.getThrough(Q1Gate.h, 0) almostEqual resQState
    )
  }

  test("HAD is its own inverse property") {
    val size = random.nextInt(3) + 1
    val k = random.nextInt(size)
    val qState = random.nextQState(size)

    val res =
      qState
        .getThrough(Q1Gate.h, k)
        .getThrough(Q1Gate.h, k)

    assert(
      qState almostEqual res
    )
  }

}
