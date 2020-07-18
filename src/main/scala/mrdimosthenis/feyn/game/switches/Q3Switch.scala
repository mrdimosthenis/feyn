package mrdimosthenis.feyn.game.switches

import mrdimosthenis.feyn.quantum.gates.Q3Gate

case class Q3Switch(q3Gate: Q3Gate, ks: (Int, Int, Int), on: Boolean) extends Switch
