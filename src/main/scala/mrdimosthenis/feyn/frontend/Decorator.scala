package mrdimosthenis.feyn.frontend


import mrdimosthenis.feyn.frontend.model.Puzzle
import mrdimosthenis.feyn.graphics.textExtensions._
import mrdimosthenis.feyn.game.switches._
import akka.actor.{Actor, ActorRef}
import org.scalajs.dom.{Element, MouseEvent, document}

object Decorator extends Actor {

  private val table = document
    .getElementById("table")

  def rowElem(th: Element, tds: LazyList[Element])
  : Element = {
    val tr = document.createElement("tr")
    tr.appendChild(th)
    tds.foreach(tr.appendChild)
    tr
  }

  def tableElems(puzzle: Puzzle, gateSelection: LazyList[Boolean], sender: ActorRef)
  : LazyList[Element] = {
    val qubits = puzzle
      .qubits
      .text
      .map { txt =>
        val th = document.createElement("th")
        th.innerHTML = txt
        th
      }
    val gates = puzzle
      .switchesWithSelection
      .map(_._1)
      .map {
        case q1Switch: Q1Switch =>
          q1Switch
            .q1Gate
            .text(q1Switch.k, puzzle.qubits.length)
        case q2Switch: Q2Switch =>
          q2Switch
            .q2Gate
            .text(q2Switch.ks, puzzle.qubits.length)
        case q3Switch: Q3Switch =>
          q3Switch
            .q3Gate
            .text(q3Switch.ks, puzzle.qubits.length)
      }
      .map { texts =>
        texts.map { txt =>
          val td = document.createElement("td")
          td.innerHTML = txt
          td
        }
      }
    qubits
      .zip(gates.transpose)
      .map { case (th, tds) =>
        tds.zipWithIndex.foreach { case (td, i) =>
          if (gateSelection(i)) td.setAttribute("class", "is-info")
          td.addEventListener("click", { (e: MouseEvent) =>
            sender ! ClickGate(i)
          })
        }
        rowElem(th, tds)
      }
  }

  override def receive: Receive = {
    case DrawPuzzle(puzzle, gateSelection) =>
      table.innerHTML = ""
      tableElems(puzzle, gateSelection, context.sender())
        .foreach(table.appendChild)
  }

}
