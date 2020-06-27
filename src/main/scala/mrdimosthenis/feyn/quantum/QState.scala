package mrdimosthenis.feyn.quantum

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._
import mrdimosthenis.feyn.quantum.gates._

import scala.util.chaining._

case class QState(vec: Vec) {

  private def exceptInvalidIndex(k: Int): Unit =
    if (k < 0 || k >= size)
      throw new Exception("Invalid state index")

  private def exceptInvalidDistance(distance: Int): Unit =
    if (distance <= 0)
      throw new Exception("Invalid qubit distance")

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

  def getThrough(q1Gate: Q1Gate, k: Int): QState = {
    exceptInvalidIndex(k)
    LazyList
      .fill(size)(Matrix.id(2))
      .updated(k, q1Gate.matrix)
      .foldLeft(Matrix.id(1)) { (acc, a) =>
        acc ** a
      }
      .pipe(_ * vec.toVerticalMatrix)
      .pipe { a => QState(a.vecExpansion) }
  }

  private def applyBaseQ2Gate(baseIndex: Int, q2Gate: Q2Gate): QState = {
    exceptInvalidIndex(baseIndex)
    exceptInvalidIndex(baseIndex + 1)
    LazyList
      .fill(size)(Matrix.id(2))
      .updated(baseIndex, q2Gate.matrix)
      .updated(baseIndex + 1, Matrix.id(1))
      .foldLeft(Matrix.id(1)) { (acc, a) =>
        acc ** a
      }
      .pipe(_ * vec.toVerticalMatrix)
      .pipe { a => QState(a.vecExpansion) }
  }

  private def swapWithNext(baseIndex: Int): QState =
    applyBaseQ2Gate(baseIndex, Q2Gate.swap)

  private def getSideBySide(baseIndex: Int, distance: Int): QState = {
    if (distance == 1) swapWithNext(baseIndex)
    else {
      exceptInvalidDistance(distance)
      exceptInvalidIndex(baseIndex)
      exceptInvalidIndex(baseIndex + distance)
      LazyList
        .from(baseIndex + 1)
        .take(distance - 1)
        .reverse
        .foldLeft(this) { (acc, i) =>
          acc.swapWithNext(i)
        }
    }
  }

  private def moveFarApart(baseIndex: Int, distance: Int): QState = {
    if (distance == 1) swapWithNext(baseIndex)
    else {
      exceptInvalidDistance(distance)
      exceptInvalidIndex(baseIndex)
      exceptInvalidIndex(baseIndex + distance)
      LazyList
        .from(baseIndex + 1)
        .take(distance - 1)
        .foldLeft(this) { (acc, i) =>
          acc.swapWithNext(i)
        }
    }
  }

  def getThrough(q2Gate: Q2Gate, ks: (Int, Int)): QState = {
    if (ks._1 >= ks._2)
      throw new Exception("Invalid indices for q2 gate")
    exceptInvalidIndex(ks._1)
    exceptInvalidIndex(ks._2)
    if (ks._2 == ks._1) applyBaseQ2Gate(ks._1, q2Gate)
    else {
      val distance = ks._2 - ks._1
      this
        .getSideBySide(ks._1, distance)
        .applyBaseQ2Gate(ks._1, q2Gate)
        .moveFarApart(ks._1, distance)
    }
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
