package mrdimosthenis.feyn.game.switches

import mrdimosthenis.feyn.quantum.gates.Q2Gate

case class Q2Switch(q2Gate: Q2Gate, ks: (Int, Int)) extends Switch
