package mrdimosthenis.feyn.frontend.model

case class Model(
                  puzzle: Puzzle,
                  selectedQubitIndex: Option[Int],
                  selectedGateIndex: Option[Int],
                  nextPuzzleQubits: Int,
                  nextPuzzleGates: Int,
                  isSolution: Boolean
                )