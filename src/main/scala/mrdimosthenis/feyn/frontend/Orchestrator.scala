package mrdimosthenis.feyn.frontend

import akka.actor.Actor
import mrdimosthenis.feyn.frontend.model._

import scala.util.chaining._

case class Orchestrator(initModel: Model) extends Actor {

  private def updated(currentModel: Model): Receive = {
    case SelectNumOfQubits(n) =>
      currentModel
        .copy(nextPuzzleQubits = n)
        .pipe(updated)
        .pipe(context.become)
    case SelectNumOfGates(n) =>
      currentModel
        .copy(nextPuzzleGates = n)
        .pipe(updated)
        .pipe(context.become)
    case ClickGo =>
      println(currentModel)
  }

  override def receive: Receive = updated(initModel)

}
