package mrdimosthenis.feyn.quantum

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._

import scala.util.chaining._

case class QState(matrix: Matrix)

object QState {

  private val trivialState =
    Matrix(Vec(1.toComplex))

  def init(n: Int): QState =
    LazyList
      .fill(n)(Qubit.zero)
      .foldLeft(trivialState) { (acc, qubit) =>
        acc ** Matrix(qubit.toVec)
      }
      .transposed
      .pipe(QState.apply)

  def fromQubits(qubits: Qubit*): QState =
    qubits
      .foldLeft(trivialState) { (acc, qubit) =>
        acc ** Matrix(qubit.toVec)
      }
      .transposed
      .pipe(QState.apply)

}
