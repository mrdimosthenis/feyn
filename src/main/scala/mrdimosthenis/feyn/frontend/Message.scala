package mrdimosthenis.feyn.frontend

import mrdimosthenis.feyn.frontend.model.Puzzle

trait Message

case class SelectNumOfQubits(n: Int) extends Message
case class SelectNumOfGates(n: Int) extends Message
case object ClickGo

case class DrawPuzzle(puzzle: Puzzle, gateSelection: LazyList[Boolean], selectedQubit: Option[Int]) extends Message

case class ClickGate(i: Int) extends Message
case class ClickQubit(i: Int) extends Message
