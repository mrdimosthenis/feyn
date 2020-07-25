package mrdimosthenis.feyn.frontend

import akka.actor.Actor

object Decorator extends Actor {

  override def receive: Receive = {
    case message =>
      println(s"received $message")
  }

}
