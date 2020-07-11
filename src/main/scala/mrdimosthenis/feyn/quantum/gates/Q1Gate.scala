package mrdimosthenis.feyn.quantum.gates

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._

import scala.util.chaining._

case class Q1Gate(matrix: Matrix) extends Gate

object Q1Gate {

  def not: Q1Gate =
    Matrix(
      Vec(Complex.zero, 1.toComplex),
      Vec(1.toComplex, Complex.zero)
    ).pipe(Q1Gate.apply)

  def had: Q1Gate = {
    val matrix = Matrix(
      Vec(1.toComplex, 1.toComplex),
      Vec(1.toComplex, Complex(-1, 0))
    )
    val coefficient = 1.0 / Math.sqrt(2)
    Q1Gate(coefficient * matrix)
  }

  def phase(degrees: Double): Q1Gate = {
    val z = Complex(
      Math.cos(degrees.toRadians),
      Math.sin(degrees.toRadians)
    )
    Matrix(
      Vec(1.toComplex, Complex.zero),
      Vec(Complex.zero, z)
    ).pipe(Q1Gate.apply)
  }

  def rotX(theta: Double): Q1Gate ={
    val cosZ = Math.cos(theta / 2).toComplex
    val sinZ = Complex(0, -Math.sin(theta / 2))
    Matrix(
      Vec(cosZ, sinZ),
      Vec(sinZ, cosZ)
    ).pipe(Q1Gate.apply)
  }

  def rotY(theta: Double): Q1Gate ={
    val cosZ = Math.cos(theta / 2).toComplex
    val sinZ = Math.sin(theta / 2).toComplex
    Matrix(
      Vec(cosZ, Complex.zero - sinZ),
      Vec(sinZ, cosZ)
    ).pipe(Q1Gate.apply)
  }

}
