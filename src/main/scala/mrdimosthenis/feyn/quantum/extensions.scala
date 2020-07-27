package mrdimosthenis.feyn.quantum

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._
import mrdimosthenis.feyn.quantum.gates._

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

    def nextGates(n: Int): LazyList[Gate] =
      LazyList(
        Q1Gate.X,
        Q1Gate.Y,
        Q1Gate.Z,
        Q1Gate.H,
        Q1Gate.H,
        Q1Gate.H,
        Q1Gate.H,
        Q1Gate.S,
        Q1Gate.T,
        Q2Gate.CX,
        Q2Gate.CX,
        Q2Gate.CY,
        Q2Gate.CZ,
        Q2Gate.SWAP,
        Q3Gate.CCX,
        Q3Gate.CSWAP,
      ).pipe {
        LazyList.fill(10)(_)
      }
        .flatten
        .pipe {
          random.shuffle(_)
        }
        .take(n)
  }

}
