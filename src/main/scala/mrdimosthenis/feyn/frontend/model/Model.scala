package mrdimosthenis.feyn.frontend.model

case class Model(
                  puzzle: Puzzle,
                  gateSelection: LazyList[Boolean],
                  selectedQubitIndex: Option[Int],
                  nextPuzzleQubits: Int,
                  nextPuzzleGates: Int,
                  isSolution: Boolean
                )