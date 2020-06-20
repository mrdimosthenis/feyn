package mrdimosthenis.feyn.quantum

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._
import mrdimosthenis.feyn.quantum.gates._

import scala.util.chaining._

case class QState(matrix: Matrix) {

  val size: Int =
    Math
      .sqrt(matrix.lazyRows.length)
      .toInt

  private def exceptInvalidIndex(k: Int): Unit =
    if (k < 0 || k >= size)
      throw new Exception("Invalid state index")

  def getThrough(gate: Q1Gate, k: Int): QState = {
    exceptInvalidIndex(k)
    LazyList
      .fill(size)(Q1Gate.id)
      .updated(k, gate)
      .foldLeft(Matrix.id(1)) { (acc, g) =>
        acc ** g.matrix
      }
      .pipe(matrix ** _)
      .pipe(QState.apply)
  }

}

object QState {

  def init(n: Int): QState =
    LazyList
      .fill(n)(Qubit.zero)
      .foldLeft(Matrix.id(1)) { (acc, qubit) =>
        acc ** Matrix(qubit.toVec)
      }
      .transposed
      .pipe(QState.apply)

  def fromQubits(qubits: Qubit*): QState =
    qubits
      .foldLeft(Matrix.id(1)) { (acc, qubit) =>
        acc ** Matrix(qubit.toVec)
      }
      .transposed
      .pipe(QState.apply)

}
