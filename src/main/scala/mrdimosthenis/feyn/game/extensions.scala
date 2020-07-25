package mrdimosthenis.feyn.game

import mrdimosthenis.feyn.game.switches._
import mrdimosthenis.feyn.quantum.gates._
import mrdimosthenis.feyn.quantum.extensions._

import scala.util.Random

object extensions {

  implicit class SwitchesRandomExtension(val random: Random) {

    def nextSwitches(numOfQubits: Int, numOfSwitches: Int): LazyList[Switch] =
      random
        .nextGates(numOfSwitches)
        .map { gate =>
          val on = random.nextBoolean()
          gate match {
            case q1Gate: Q1Gate =>
              val k = random.nextInt(numOfQubits)
              Q1Switch(q1Gate, k, on)
            case q2Gate: Q2Gate =>
              val i = random.nextInt(numOfQubits - 1)
              val j = random.nextInt(numOfQubits - i - 1) + i + 1
              val ks = (i, j)
              Q2Switch(q2Gate, ks, on)
            case q3Gate: Q3Gate =>
              val i = random.nextInt(numOfQubits - 2)
              val j = random.nextInt(numOfQubits - i - 2) + i + 1
              val k = random.nextInt(numOfQubits - j - 1) + j + 1
              val ks = (i, j, k)
              Q3Switch(q3Gate, ks, on)
          }
        }

  }

}
