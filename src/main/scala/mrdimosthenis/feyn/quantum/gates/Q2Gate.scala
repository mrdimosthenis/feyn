package mrdimosthenis.feyn.quantum.gates

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._

import scala.util.chaining._

case class Q2Gate(matrix: Matrix) extends Gate

object Q2Gate {

  def cNot: Q2Gate =
    Matrix(
      Vec(1.toComplex, Complex.zero, Complex.zero, Complex.zero),
      Vec(Complex.zero, 1.toComplex, Complex.zero, Complex.zero),
      Vec(Complex.zero, Complex.zero, Complex.zero, 1.toComplex),
      Vec(Complex.zero, Complex.zero, 1.toComplex, Complex.zero)
    ).pipe(Q2Gate.apply)

  def cPhase(degrees: Double): Q2Gate = {
    val z = Complex(
      Math.cos(degrees.toRadians),
      Math.sin(degrees.toRadians)
    )
    Matrix(
      Vec(1.toComplex, Complex.zero, Complex.zero, Complex.zero),
      Vec(Complex.zero, 1.toComplex, Complex.zero, Complex.zero),
      Vec(Complex.zero, Complex.zero, 1.toComplex, Complex.zero),
      Vec(Complex.zero, Complex.zero, Complex.zero, z)
    ).pipe(Q2Gate.apply)
  }

  def swap: Q2Gate =
    Matrix(
      Vec(1.toComplex, Complex.zero, Complex.zero, Complex.zero),
      Vec(Complex.zero, Complex.zero, 1.toComplex, Complex.zero),
      Vec(Complex.zero, 1.toComplex, Complex.zero, Complex.zero),
      Vec(Complex.zero, Complex.zero, Complex.zero, 1.toComplex)
    ).pipe(Q2Gate.apply)

}
