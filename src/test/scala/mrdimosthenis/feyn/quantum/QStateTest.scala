package mrdimosthenis.feyn.quantum

import minitest.SimpleTestSuite
import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._
import mrdimosthenis.feyn.quantum.gates.Q1Gate

object QStateTest extends SimpleTestSuite {

  test("Not gate on single qubit state") {
    val initQState = QState.init(1)
    val resQState = QState(
      Vec(Complex.zero, 1.toComplex)
    )
    assert(
      initQState.getThrough(Q1Gate.x, 0) == resQState
    )
  }

}
