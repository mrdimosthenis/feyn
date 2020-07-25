package mrdimosthenis.feyn.frontend.model

import mrdimosthenis.feyn.game.switches.Switch
import mrdimosthenis.feyn.quantum.Qubit

case class Puzzle(
                 qubits: LazyList[Qubit],
                 switchesWithSelection: LazyList[(Switch, Boolean)]
                 )
