package mrdimosthenis.feyn.frontend

import mrdimosthenis.feyn.frontend.model.Model
import mrdimosthenis.feyn.frontend.model.extensions._
import akka.actor.{ActorSystem, Props}
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLSelectElement

import scala.util.Random
import scala.util.chaining._

object App {

  val random = new Random()

  private val initModel = Model(
    random.nextPuzzle(3, 6),
    LazyList.fill(6)(false),
    None,
    None,
    3,
    6,
    isSolution = false
  )

  private lazy val system = ActorSystem("actor-system")

  private val orchestrator = system.actorOf(Props(Orchestrator(initModel)))

  def main(args: Array[String]): Unit = {

    document
      .getElementById("goButton")
      .addEventListener("click", { (e: MouseEvent) =>
        orchestrator ! ClickGo
      })

    document
      .getElementById("qubitsSelectBox")
      .addEventListener("change", { (e: MouseEvent) =>
        e.target
          .asInstanceOf[HTMLSelectElement]
          .value
          .toInt
          .pipe(SelectNumOfQubits)
          .pipe(orchestrator.!)
      })

    document
      .getElementById("gatesSelectBox")
      .addEventListener("change", { (e: MouseEvent) =>
        e.target
          .asInstanceOf[HTMLSelectElement]
          .value
          .toInt
          .pipe(SelectNumOfGates)
          .pipe(orchestrator.!)
      })

    orchestrator ! ClickGo

  }

}
