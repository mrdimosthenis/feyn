package mrdimosthenis.feyn.frontend

import org.scalajs.dom
import org.scalajs.dom.document

object App {

  document.onload

  def main(args: Array[String]): Unit = {

    val goButton = document.getElementById("goButton")
    goButton.addEventListener("click", { (e: dom.MouseEvent) =>
      MyActor.actorRef ! "just clicked"
    })

  }

}
