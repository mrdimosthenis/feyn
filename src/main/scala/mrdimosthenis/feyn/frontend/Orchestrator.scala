package mrdimosthenis.feyn.frontend

import mrdimosthenis.feyn.frontend.model._
import mrdimosthenis.feyn.frontend.model.extensions._

import akka.actor.{Actor, Props}
import scala.util.chaining._

case class Orchestrator(initModel: Model) extends Actor {

  private val decorator = context.system.actorOf(Props(Decorator))

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
      val newPuzzle =
        App.random.nextPuzzle(
          currentModel.nextPuzzleQubits,
          currentModel.nextPuzzleGates
        )
      val newGateSelection =
        LazyList.fill(currentModel.nextPuzzleGates)(false)
      decorator ! DrawPuzzle(newPuzzle, newGateSelection)
      currentModel
        .copy(puzzle = newPuzzle, gateSelection = newGateSelection)
        .pipe(updated)
        .pipe(context.become)
  }

  override def receive: Receive = updated(initModel)

}
