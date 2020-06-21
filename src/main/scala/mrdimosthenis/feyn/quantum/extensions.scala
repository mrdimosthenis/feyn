package mrdimosthenis.feyn.quantum

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._

import scala.util.chaining._
import scala.util.Random

object extensions {

  implicit class QuantumRandomExtension(val random: Random) {

    def nextQState(size: Int): QState =
      QState
        .init(size)
        .vec
        .lazyComponents
        .map { _ => random.nextComplex() }
        .pipe(Vec.apply)
        .unit
        .map(QState.apply)
        .getOrElse {
          nextQState(size)
        }

  }

}
