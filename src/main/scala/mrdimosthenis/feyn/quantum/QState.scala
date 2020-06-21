package mrdimosthenis.feyn.quantum

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._
import mrdimosthenis.feyn.quantum.gates._

import scala.util.chaining._

case class QState(vec: Vec) {

  private def exceptInvalidIndex(k: Int): Unit =
    if (k < 0 || k >= size)
      throw new Exception("Invalid state index")

  override def toString: String = {
    val maxCompLength = vec
      .lazyComponents
      .map(_.toString.length)
      .max
    vec
      .lazyComponents
      .map { z =>
        (maxCompLength - z.toString.length)
          .pipe(" ".repeat)
          .pipe(_ + z)
      }.mkString("||", "||\n||", "||")
  }

  val size: Int =
    vec
      .lazyComponents
      .length
      .pipe {
        Math.log10(_) / Math.log10(2)
      }
      .pipe(Math.round)
      .toInt

  def getThrough(gate: Q1Gate, k: Int): QState = {
    exceptInvalidIndex(k)
    LazyList
      .fill(size)(Q1Gate.id)
      .updated(k, gate)
      .foldLeft(Matrix.id(1)) { (acc, g) =>
        acc ** g.matrix
      }
      .pipe(_ * vec.toVerticalMatrix)
      .pipe { a => QState(a.vecExpansion) }
  }

  def equal(qState: QState): Boolean =
    vec == qState.vec

  def almostEqual(qState: QState)(implicit error: Threshold): Boolean =
    vec =~ qState.vec

}

object QState {

  def fromQubits(qubits: Qubit*): QState =
    qubits
      .foldLeft(Matrix.id(1)) { (acc, qubit) =>
        acc ** Matrix(qubit.toVec)
      }
      .pipe { a => QState(a.vecExpansion) }

  def init(size: Int): QState =
    LazyList
      .fill(size)(Qubit.zero)
      .pipe(fromQubits)

}
