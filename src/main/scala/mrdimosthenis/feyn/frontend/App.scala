package mrdimosthenis.feyn.frontend

import mrdimosthenis.feyn.frontend.model.extensions._
import akka.actor.{ActorSystem, Props}
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLSelectElement

import scala.util.Random
import scala.util.chaining._

object App {

  val random = new Random()

  private val initModel =
    random.nextModel(3, 6)

  private lazy val system = ActorSystem("actor-system")

  private val orchestrator =
    system.actorOf(Props(Orchestrator(initModel)))

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
