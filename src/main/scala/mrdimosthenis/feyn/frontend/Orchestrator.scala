package mrdimosthenis.feyn.frontend

import mrdimosthenis.feyn.frontend.model._
import mrdimosthenis.feyn.frontend.model.extensions._
import akka.actor.{Actor, Props}
import scala.util.chaining._

case class Orchestrator(initModel: Model) extends Actor {

  private val decorator = context.system.actorOf(Props(Decorator))

  private def updated(currentModel: Model): Receive = {
    case ClickQubit(i) =>
      val newSelectedQubitIndex =
        if (currentModel.selectedQubitIndex.contains(i)) None
        else Some(i)
      currentModel
        .gateSelection
        .updated(i, !currentModel.gateSelection(i))
      val newModel = currentModel
        .copy(selectedQubitIndex = newSelectedQubitIndex)
      decorator ! DrawPuzzle(newModel)
      newModel
        .pipe(updated)
        .pipe(context.become)
    case ClickGate(i) =>
      val newGateSelection =
        currentModel
          .gateSelection
          .updated(i, !currentModel.gateSelection(i))
      val newModel = currentModel
        .copy(gateSelection = newGateSelection)
      decorator ! DrawPuzzle(newModel)
      newModel
        .pipe(updated)
        .pipe(context.become)
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
      val newModel = currentModel
        .copy(puzzle = newPuzzle, gateSelection = newGateSelection)
      decorator ! DrawPuzzle(newModel)
      newModel
        .pipe(updated)
        .pipe(context.become)
    case a => println(a)
  }

  override def receive: Receive = updated(initModel)

}
