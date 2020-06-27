package mrdimosthenis.feyn.quantum

import minitest.SimpleTestSuite
import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._
import mrdimosthenis.feyn.quantum.gates._
import mrdimosthenis.feyn.quantum.extensions._

import scala.util.chaining._
import scala.util.Random

object QStateTest extends SimpleTestSuite {

  implicit val error: Threshold = 0.001

  val random: Random = new Random()

  test("Not gate on single qubit state") {
    assert(
      QState
        .init(1)
        .getThrough(Q1Gate.not, 0)
        equal QState
        .fromQubits(Qubit.one)
    )
  }

  test("NOT is its own inverse property") {
    val size = random.nextInt(3) + 1
    val k = random.nextInt(size)
    val qState = random.nextQState(size)

    assert(
      qState
        .getThrough(Q1Gate.not, k)
        .getThrough(Q1Gate.not, k)
        almostEqual qState
    )
  }

  test("HAD gate on single qubit state") {
    assert(
      QState
        .init(1)
        .getThrough(Q1Gate.had, 0)
        almostEqual QState(
        Vec(0.7071.toComplex, 0.7071.toComplex)
      )
    )
  }

  test("HAD is its own inverse property") {
    val size = random.nextInt(3) + 1
    val k = random.nextInt(size)
    val qState = random.nextQState(size)

    assert(
      qState
        .getThrough(Q1Gate.had, k)
        .getThrough(Q1Gate.had, k)
        almostEqual qState
    )
  }

  test("PHASE gate on single qubit state") {
    assert(
      QState
        .init(1)
        .getThrough(Q1Gate.had, 0)
        .getThrough(Q1Gate.phase45, 0)
        almostEqual QState(
        Vec(0.7071.toComplex, Complex(0.5, 0.5))
      )
    )
  }

  test("ROTX and ROTY gates on single qubit states") {
    assert(
      QState.fromQubits(Qubit.zero)
        .getThrough(Q1Gate.rotX(Math.PI / 4), 0)
        almostEqual QState(
        Vec(0.9239.toComplex, Complex(0, -0.3827))
      )
    )

    assert(
      QState.fromQubits(Qubit.one)
        .getThrough(Q1Gate.rotX(Math.PI / 4), 0)
        almostEqual QState(
        Vec(Complex(0, -0.3827), 0.9239.toComplex)
      )
    )

    assert(
      QState.fromQubits(Qubit.zero)
        .getThrough(Q1Gate.rotY(Math.PI / 4), 0)
        almostEqual QState(
        Vec(0.9239.toComplex, 0.3827.toComplex)
      )
    )

    assert(
      QState.fromQubits(Qubit.one)
        .getThrough(Q1Gate.rotY(Math.PI / 4), 0)
        almostEqual QState(
        Vec(Complex(-0.3827, 0), 0.9239.toComplex)
      )
    )
  }

  test("HAD-PHASE(180)-HAD equals NOT property") {
    val size = random.nextInt(3) + 1
    val k = random.nextInt(size)
    val qState = random.nextQState(size)

    assert(
      qState
        .getThrough(Q1Gate.had, k)
        .getThrough(Q1Gate.phase180, k)
        .getThrough(Q1Gate.had, k)
        almostEqual qState
        .getThrough(Q1Gate.not, k)
    )
  }

  test("HAD-NOT-HAD equals PHASE(180) property") {
    val size = random.nextInt(3) + 1
    val k = random.nextInt(size)
    val qState = random.nextQState(size)

    assert(
      qState
        .getThrough(Q1Gate.had, k)
        .getThrough(Q1Gate.not, k)
        .getThrough(Q1Gate.had, k)
        almostEqual qState
        .getThrough(Q1Gate.phase180, k)
    )
  }

  test("ROOT-of-NOT property") {
    val size = random.nextInt(3) + 1
    val k = random.nextInt(size)
    val qState = random.nextQState(size)

    assert(
      qState
        .getThrough(Q1Gate.rootOfNot, k)
        .getThrough(Q1Gate.rootOfNot, k)
        almostEqual qState
        .getThrough(Q1Gate.not, k)
    )
  }

}
