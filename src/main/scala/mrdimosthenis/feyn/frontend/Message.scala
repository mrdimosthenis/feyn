package mrdimosthenis.feyn.frontend

import mrdimosthenis.feyn.frontend.model.Puzzle

trait Message

case class SelectNumOfQubits(n: Int) extends Message
case class SelectNumOfGates(n: Int) extends Message
case object ClickGo

case class DrawPuzzle(puzzle: Puzzle)
