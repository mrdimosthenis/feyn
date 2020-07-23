package mrdimosthenis.feyn.frontend

import akka.actor.{Actor, Props}

object Orchestrator {

  def myActor(): Props = Props(
    new Actor {
      def receive: PartialFunction[Any, Unit] = {
        case message =>
          println(s"received $message")
      }
    }
  )

}
