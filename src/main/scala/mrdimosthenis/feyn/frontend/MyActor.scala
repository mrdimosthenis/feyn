package mrdimosthenis.feyn.frontend

import akka.actor._

object MyActor {

  lazy val system: ActorSystem = ActorSystem("actor-system")

  def myActor(): Props = Props(
    new Actor {
      def receive: PartialFunction[Any, Unit] = {
        case message =>
          println(s"received $message")
      }
    }
  )

  val actorRef: ActorRef = system.actorOf(myActor())

}
