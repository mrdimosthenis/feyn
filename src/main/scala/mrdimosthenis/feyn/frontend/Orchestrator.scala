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
      decorator ! DrawPuzzle(
        currentModel.puzzle,
        currentModel.gateSelection,
        newSelectedQubitIndex
      )
      currentModel
        .copy(selectedQubitIndex = newSelectedQubitIndex)
        .pipe(updated)
        .pipe(context.become)
    case ClickGate(i) =>
      val newGateSelection =
        currentModel
          .gateSelection
          .updated(i, !currentModel.gateSelection(i))
      decorator ! DrawPuzzle(
        currentModel.puzzle,
        newGateSelection,
        currentModel.selectedQubitIndex
      )
      currentModel
        .copy(gateSelection = newGateSelection)
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
      decorator ! DrawPuzzle(newPuzzle, newGateSelection, currentModel.selectedQubitIndex)
      currentModel
        .copy(puzzle = newPuzzle, gateSelection = newGateSelection)
        .pipe(updated)
        .pipe(context.become)
    case a => println(a)
  }

  override def receive: Receive = updated(initModel)

}
