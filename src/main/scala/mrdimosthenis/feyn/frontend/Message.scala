package mrdimosthenis.feyn.frontend

import mrdimosthenis.feyn.frontend.model.Model

trait Message

case class SelectNumOfQubits(n: Int) extends Message
case class SelectNumOfGates(n: Int) extends Message
case object ClickGo

case class DrawPuzzle(model: Model) extends Message

case class ClickGate(i: Int) extends Message
case class ClickQubit(i: Int) extends Message
