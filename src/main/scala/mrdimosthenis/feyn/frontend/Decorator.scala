package mrdimosthenis.feyn.frontend

import mrdimosthenis.feyn.frontend.model.Model
import mrdimosthenis.feyn.frontend.extensions._
import mrdimosthenis.feyn.game.switches._
import akka.actor.{Actor, ActorRef}
import org.scalajs.dom._

import scala.util.chaining._

object Decorator extends Actor {

  private val svgUnitLength = 10.0

  private val table = document
    .getElementById("table")

  private val qStateDiv = document
    .getElementById("qStateDiv")

  private val goButton = document
    .getElementById("goButton")

  private def rowElem(th: Element, tds: LazyList[Element])
  : Element = {
    val tr = document.createElement("tr")
    tr.appendChild(th)
    tds.foreach(tr.appendChild)
    tr
  }

  private def tableElems(model: Model, sender: ActorRef)
  : LazyList[Element] = {
    val qubits = model
      .puzzle
      .qubits
      .svg(svgUnitLength)
      .map { svg =>
        val th = document.createElement("th")
        th.appendChild(svg.render)
        th
      }
    val gates = model
      .puzzle
      .switchesWithSelection
      .map(_._1)
      .map {
        case q1Switch: Q1Switch =>
          q1Switch
            .q1Gate
            .svg(svgUnitLength)(q1Switch.k, model.puzzle.qubits.length)
        case q2Switch: Q2Switch =>
          q2Switch
            .q2Gate
            .svg(svgUnitLength)(q2Switch.ks, model.puzzle.qubits.length)
        case q3Switch: Q3Switch =>
          q3Switch
            .q3Gate
            .svg(svgUnitLength)(q3Switch.ks, model.puzzle.qubits.length)
      }
      .map { texts =>
        texts.map { svg =>
          val td = document.createElement("td")
          td.appendChild(svg.render)
          td
        }
      }
    val transposedGates =
      gates
        .transpose
        .zipWithIndex
        .map { case (tds, i) =>
          model.selectedQubitIndex match {
            case Some(j) =>
              if (i == j) tds.foreach(_.setAttribute("class", "is-light"))
            case None =>
              ()
          }
          tds
        }
    qubits
      .zipWithIndex
      .map { case (th, i) =>
        model.selectedQubitIndex match {
          case Some(j) =>
            if (i == j) th.setAttribute("class", "is-light")
          case None =>
            ()
        }
        th.addEventListener("click", { (e: MouseEvent) =>
          sender ! ClickQubit(i)
        })
        th
      }
      .zip(transposedGates)
      .map { case (th, tds) =>
        tds.zipWithIndex.foreach { case (td, i) =>
          if (model.gateSelection(i)) td.setAttribute("class", "is-success")
          td.addEventListener("click", { (e: MouseEvent) =>
            sender ! ClickGate(i)
          })
        }
        rowElem(th, tds)
      }
  }

  private def qStateDivElems(model: Model)
  : LazyList[Element] = {
    val goalStateZs =
      model
        .goalQState
        .vec
        .lazyComponents
    val currentStateZs =
      model
        .currentQState
        .vec
        .lazyComponents
    goalStateZs
      .zip(currentStateZs)
      .zipWithIndex
      .map { case (zTuple, i) =>
        val binStr =
          i.toBinaryString
            .toCharArray
            .to(LazyList)
            .pipe { chars =>
              LazyList
                .fill(model.currentQState.size - chars.length)('0')
                .appendedAll(chars)
            }
            .mkString
        val isHighlighted =
          model.selectedQubitIndex match {
            case Some(j) =>
              binStr
                .charAt(j)
                .pipe(_ == '0')
            case None =>
              false
          }
        val div = document.createElement("div")
        div.setAttribute("class", "column is-narrow")
        val width = 250.0 / model.puzzle.qubits.length
        zTuple
          .svg(
            width,
            if (isHighlighted) ("lightgreen", "lightblue")
            else ("green", "blue")
          )
          .render
          .pipe(div.appendChild)
        val tag = document.createElement("span")
        tag.setAttribute("class", "tag is-unselectable")
        tag.innerText = binStr
        div.appendChild(tag)
        div
      }
  }

  override def receive: Receive = {
    case DrawPuzzle(model) =>
      table.innerHTML = ""
      tableElems(model, context.sender())
        .foreach(table.appendChild)
      qStateDiv.innerHTML = ""
      qStateDivElems(model)
        .foreach { div =>
          qStateDiv.appendChild(div)
        }
      goButton.setAttribute(
        "class",
        if (model.isSolution) "button  is-success"
        else "button"
      )
  }

}
