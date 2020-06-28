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

  // Get through q1Gates

  test("NOT gate on single qubit state") {
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
        .getThrough(Q1Gate.phase(45), 0)
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
        .getThrough(Q1Gate.phase(180), k)
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
        .getThrough(Q1Gate.phase(180), k)
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

  // Get through q2Gates

  test("HAD and CNOT gates on two qubits") {
    assert(
      QState
        .init(2)
        .getThrough(Q1Gate.had, 0)
        .getThrough(Q2Gate.cNot, (0, 1))
        almostEqual QState(
        Vec(
          0.7071.toComplex,
          Complex.zero,
          Complex.zero,
          0.7071.toComplex
        )
      )
    )
  }

  test("HAD-CNOT-HAD equals CPHASE(180) property on two qubits") {
    val qState = random.nextQState(2)

    assert(
      qState
        .getThrough(Q1Gate.had, 1)
        .getThrough(Q2Gate.cNot, (0, 1))
        .getThrough(Q1Gate.had, 1)
        almostEqual qState
        .getThrough(Q2Gate.cPhase(180), (0, 1))
    )
  }

  test("HAD-CNOT-HAD equals CPHASE(180) property on multiple qubits") {
    val size = random.nextInt(3) + 3
    val i = random.nextInt(size - 1)
    val j = random.nextInt(size - i - 1) + i + 1
    val qState = random.nextQState(size)

    assert(
      qState
        .getThrough(Q1Gate.had, j)
        .getThrough(Q2Gate.cNot, (i, j))
        .getThrough(Q1Gate.had, j)
        almostEqual qState
        .getThrough(Q2Gate.cPhase(180), (i, j))
    )
  }

  test("3xCNOT equals SWAP property on two qubits") {
    val qState = random.nextQState(2)

    assert(
      qState
        .getThrough(Q2Gate.cNot, (0, 1))
        .getThrough(Q2Gate.swap, (0, 1))
        .getThrough(Q2Gate.cNot, (0, 1))
        .getThrough(Q2Gate.swap, (0, 1))
        .getThrough(Q2Gate.cNot, (0, 1))
        almostEqual qState
        .getThrough(Q2Gate.swap, (0, 1))
    )
  }

  test("3xCNOT equals SWAP property on multiple qubits") {
    val size = random.nextInt(3) + 3
    val i = random.nextInt(size - 1)
    val j = random.nextInt(size - i - 1) + i + 1
    val qState = random.nextQState(size)

    assert(
      qState
        .getThrough(Q2Gate.cNot, (i, j))
        .getThrough(Q2Gate.swap, (i, j))
        .getThrough(Q2Gate.cNot, (i, j))
        .getThrough(Q2Gate.swap, (i, j))
        .getThrough(Q2Gate.cNot, (i, j))
        almostEqual qState
        .getThrough(Q2Gate.swap, (i, j))
    )
  }

  // Get through q3Gates

  test("3xCCNOT equals CSWAP property on three qubits") {
    val qState = random.nextQState(3)

    assert(
      qState

        .getThrough(Q2Gate.swap, (1, 2))
        .getThrough(Q3Gate.ccNot, (0, 1, 2))
        .getThrough(Q2Gate.swap, (1, 2))

        .getThrough(Q2Gate.swap, (0, 2))
        .getThrough(Q3Gate.ccNot, (0, 1, 2))
        .getThrough(Q2Gate.swap, (0, 2))

        .getThrough(Q2Gate.swap, (1, 2))
        .getThrough(Q3Gate.ccNot, (0, 1, 2))
        .getThrough(Q2Gate.swap, (1, 2))

        almostEqual qState

        .getThrough(Q2Gate.swap, (0, 2))
        .getThrough(Q3Gate.cSwap, (0, 1, 2))
        .getThrough(Q2Gate.swap, (0, 2))
    )
  }

  test("3xCCNOT equals CSWAP property on multiple qubits") {
    val size = random.nextInt(4) + 4
    val i = random.nextInt(size - 2)
    val j = random.nextInt(size - i - 2) + i + 1
    val k = random.nextInt(size - j - 1) + j + 1
    val qState = random.nextQState(size)

    assert(
      qState

        .getThrough(Q2Gate.swap, (j, k))
        .getThrough(Q3Gate.ccNot, (i, j, k))
        .getThrough(Q2Gate.swap, (j, k))

        .getThrough(Q2Gate.swap, (i, k))
        .getThrough(Q3Gate.ccNot, (i, j, k))
        .getThrough(Q2Gate.swap, (i, k))

        .getThrough(Q2Gate.swap, (j, k))
        .getThrough(Q3Gate.ccNot, (i, j, k))
        .getThrough(Q2Gate.swap, (j, k))

        almostEqual qState

        .getThrough(Q2Gate.swap, (i, k))
        .getThrough(Q3Gate.cSwap, (i, j, k))
        .getThrough(Q2Gate.swap, (i, k))
    )
  }

}
