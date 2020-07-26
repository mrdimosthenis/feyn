package mrdimosthenis.feyn.frontend.model

import mrdimosthenis.feyn.quantum.{QState, Qubit}
import mrdimosthenis.feyn.game.extensions._

import scala.util.Random
import scala.util.chaining._

object extensions {

  implicit class PuzzleRandomExtension(val random: Random) {

    def nextPuzzle(numOfQubits: Int, numOfSwitches: Int): Puzzle = {
      val qubits =
        LazyList
          .from(0)
          .take(numOfQubits)
          .map { _ =>
            if (random.nextBoolean()) Qubit.one
            else Qubit.zero
          }
      val switches = random
        .nextSwitches(numOfQubits, numOfSwitches)
      val selections =
        LazyList
          .from(0)
          .take(numOfSwitches - 1)
          .map { _ =>
            random.nextBoolean()
          }
          .prepended(true)
          .pipe {
            random.shuffle(_)
          }
      val switchesWithSelection =
        switches.zip(selections)
      Puzzle(qubits, switchesWithSelection)
    }

  }

  implicit class ModelRandomExtension(val random: Random) {

    def nextModel(numOfQubits: Int, numOfSwitches: Int): Model =
      Model(
        random.nextPuzzle(numOfQubits, numOfSwitches),
        LazyList.fill(numOfSwitches)(false),
        None,
        numOfQubits,
        numOfSwitches
      )

  }

}
