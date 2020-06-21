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
      initQState
        .getThrough(Q1Gate.x, 0)
        equal resQState
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
      initQState
        .getThrough(Q1Gate.h, 0)
        almostEqual resQState
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

  test("PHASE gate on single qubit state") {
    val initQState = QState.init(1).getThrough(Q1Gate.h, 0)
    val resQState = QState(
      Vec(0.7071.toComplex, Complex(0.6533, 0.2706))
    )
    assert(
      initQState
        .getThrough(Q1Gate.phase(Math.PI / 8), 0)
        almostEqual resQState
    )
  }

  test("ROTX and ROTY gates on single qubit states") {
    assert(
      QState.fromQubits(Qubit.zero)
        .getThrough(Q1Gate.rotX(Math.PI / 8), 0)
        almostEqual QState(
        Vec(0.9808.toComplex, Complex(0, -0.1951))
      )
    )

    assert(
      QState.fromQubits(Qubit.one)
        .getThrough(Q1Gate.rotX(Math.PI / 8), 0)
        almostEqual QState(
        Vec(Complex(0, -0.1951), 0.9808.toComplex)
      )
    )

    assert(
      QState.fromQubits(Qubit.zero)
        .getThrough(Q1Gate.rotY(Math.PI / 8), 0)
        almostEqual QState(
        Vec(0.9808.toComplex, 0.1951.toComplex)
      )
    )

    assert(
      QState.fromQubits(Qubit.one)
        .getThrough(Q1Gate.rotY(Math.PI / 8), 0)
        almostEqual QState(
        Vec(Complex(-0.1951, 0), 0.9808.toComplex)
      )
    )
  }

}
