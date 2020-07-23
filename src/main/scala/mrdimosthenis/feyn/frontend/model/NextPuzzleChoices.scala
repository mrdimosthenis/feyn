package mrdimosthenis.feyn.frontend.model

case class NextPuzzleChoices(numberOfQubits: Int, numberOfGates: Int)

object NextPuzzleChoices {

  def init(): NextPuzzleChoices =
    NextPuzzleChoices(3, 6)

}
