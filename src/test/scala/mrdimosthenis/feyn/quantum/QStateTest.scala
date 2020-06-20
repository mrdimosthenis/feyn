package mrdimosthenis.feyn.quantum

import minitest.SimpleTestSuite
import mrdimosthenis.feyn.math.Matrix
import mrdimosthenis.feyn.quantum.gates.Q1Gate

object QStateTest extends SimpleTestSuite {

  test("Not gate on single qubit state") {
    assert(
      QState.init(1).getThrough(Q1Gate.x, 0).matrix == Matrix()
    )
  }

}
