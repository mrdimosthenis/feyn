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
        .getThrough(Q1Gate.X, 0)
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
        .getThrough(Q1Gate.X, k)
        .getThrough(Q1Gate.X, k)
        almostEqual qState
    )
  }

  test("HAD gate on single qubit state") {
    assert(
      QState
        .init(1)
        .getThrough(Q1Gate.H, 0)
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
        .getThrough(Q1Gate.H, k)
        .getThrough(Q1Gate.H, k)
        almostEqual qState
    )
  }

  // Get through q2Gates

  test("HAD and CNOT gates on two qubits") {
    assert(
      QState
        .init(2)
        .getThrough(Q1Gate.H, 0)
        .getThrough(Q2Gate.CX, (0, 1))
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

  test("3xCNOT equals SWAP property on two qubits") {
    val qState = random.nextQState(2)

    assert(
      qState
        .getThrough(Q2Gate.CX, (0, 1))
        .getThrough(Q2Gate.SWAP, (0, 1))
        .getThrough(Q2Gate.CX, (0, 1))
        .getThrough(Q2Gate.SWAP, (0, 1))
        .getThrough(Q2Gate.CX, (0, 1))
        almostEqual qState
        .getThrough(Q2Gate.SWAP, (0, 1))
    )
  }

  test("3xCNOT equals SWAP property on multiple qubits") {
    val size = random.nextInt(3) + 3
    val i = random.nextInt(size - 1)
    val j = random.nextInt(size - i - 1) + i + 1
    val qState = random.nextQState(size)

    assert(
      qState
        .getThrough(Q2Gate.CX, (i, j))
        .getThrough(Q2Gate.SWAP, (i, j))
        .getThrough(Q2Gate.CX, (i, j))
        .getThrough(Q2Gate.SWAP, (i, j))
        .getThrough(Q2Gate.CX, (i, j))
        almostEqual qState
        .getThrough(Q2Gate.SWAP, (i, j))
    )
  }

  // Get through q3Gates

  test("3xCCNOT equals CSWAP property on three qubits") {
    val qState = random.nextQState(3)

    assert(
      qState

        .getThrough(Q2Gate.SWAP, (1, 2))
        .getThrough(Q3Gate.CCX, (0, 1, 2))
        .getThrough(Q2Gate.SWAP, (1, 2))

        .getThrough(Q2Gate.SWAP, (0, 2))
        .getThrough(Q3Gate.CCX, (0, 1, 2))
        .getThrough(Q2Gate.SWAP, (0, 2))

        .getThrough(Q2Gate.SWAP, (1, 2))
        .getThrough(Q3Gate.CCX, (0, 1, 2))
        .getThrough(Q2Gate.SWAP, (1, 2))

        almostEqual qState

        .getThrough(Q2Gate.SWAP, (0, 2))
        .getThrough(Q3Gate.CSWAP, (0, 1, 2))
        .getThrough(Q2Gate.SWAP, (0, 2))
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

        .getThrough(Q2Gate.SWAP, (j, k))
        .getThrough(Q3Gate.CCX, (i, j, k))
        .getThrough(Q2Gate.SWAP, (j, k))

        .getThrough(Q2Gate.SWAP, (i, k))
        .getThrough(Q3Gate.CCX, (i, j, k))
        .getThrough(Q2Gate.SWAP, (i, k))

        .getThrough(Q2Gate.SWAP, (j, k))
        .getThrough(Q3Gate.CCX, (i, j, k))
        .getThrough(Q2Gate.SWAP, (j, k))

        almostEqual qState

        .getThrough(Q2Gate.SWAP, (i, k))
        .getThrough(Q3Gate.CSWAP, (i, j, k))
        .getThrough(Q2Gate.SWAP, (i, k))
    )
  }

}
