package mrdimosthenis.feyn.quantum.gates

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._

import scala.util.chaining._

case class QGate(matrix: Matrix) {

  val size: Int =
    matrix
      .lazyRows
      .length
      .pipe {
        Math.log10(_) / Math.log10(2)
      }
      .pipe(Math.round)
      .toInt

  def controlled(controlSize: Int): QGate = {
    val controlRows =
      Math.pow(2, controlSize + size)
        .pipe(Math.round)
        .toInt
        .pipe(Matrix.id)
        .lazyRows
        .take(controlSize)
    val zeroGateComponents =
      Math.pow(2, controlSize)
        .pipe(Math.round)
        .toInt
        .pipe(Vec.zero)
        .lazyComponents
    val gateRows =
      matrix
        .lazyRows
        .map { row =>
          LazyList
            .concat(zeroGateComponents, row.lazyComponents)
            .pipe(Vec.apply)
        }
    LazyList
      .concat(controlRows, gateRows)
      .pipe(Matrix.apply)
      .pipe(QGate.apply)
  }

}

object QGate {

  // gates that acts on 1 qubits

  def not: QGate =
    Matrix(
      Vec(Complex.zero, 1.toComplex),
      Vec(1.toComplex, Complex.zero)
    ).pipe(QGate.apply)

  def had: QGate = {
    val matrix = Matrix(
      Vec(1.toComplex, 1.toComplex),
      Vec(1.toComplex, Complex(-1, 0))
    )
    val coefficient = 1.0 / Math.sqrt(2)
    QGate(coefficient * matrix)
  }

  def phase(degrees: Double): QGate = {
    val z = Complex(
      Math.cos(degrees.toRadians),
      Math.sin(degrees.toRadians)
    )
    Matrix(
      Vec(1.toComplex, Complex.zero),
      Vec(Complex.zero, z)
    ).pipe(QGate.apply)
  }

  def rotX(theta: Double): QGate = {
    val cosZ = Math.cos(theta / 2).toComplex
    val sinZ = Complex(0, -Math.sin(theta / 2))
    Matrix(
      Vec(cosZ, sinZ),
      Vec(sinZ, cosZ)
    ).pipe(QGate.apply)
  }

  def rotY(theta: Double): QGate = {
    val cosZ = Math.cos(theta / 2).toComplex
    val sinZ = Math.sin(theta / 2).toComplex
    Matrix(
      Vec(cosZ, Complex.zero - sinZ),
      Vec(sinZ, cosZ)
    ).pipe(QGate.apply)
  }

  // gate that acts on 2 qubits

  def swap: QGate =
    Matrix(
      Vec(1.toComplex, Complex.zero, Complex.zero, Complex.zero),
      Vec(Complex.zero, Complex.zero, 1.toComplex, Complex.zero),
      Vec(Complex.zero, 1.toComplex, Complex.zero, Complex.zero),
      Vec(Complex.zero, Complex.zero, Complex.zero, 1.toComplex)
    ).pipe(QGate.apply)

}
