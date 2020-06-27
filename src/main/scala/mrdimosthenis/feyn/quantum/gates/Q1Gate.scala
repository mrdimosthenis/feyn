package mrdimosthenis.feyn.quantum.gates

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._

import scala.util.chaining._

case class Q1Gate(matrix: Matrix) extends Gate {

  def followedBy(q1Gate: Q1Gate): Q1Gate =
    Q1Gate(q1Gate.matrix * matrix)

}

object Q1Gate {

  def id: Q1Gate =
    Matrix
      .id(2)
      .pipe(Q1Gate.apply)

  def not: Q1Gate =
    Matrix(
      Vec(Complex.zero, 1.toComplex),
      Vec(1.toComplex, Complex.zero)
    ).pipe(Q1Gate.apply)

  def rootOfNot: Q1Gate =
    had
      .followedBy(phase90)
      .followedBy(had)

  def pauliY: Q1Gate =
    Matrix(
      Vec(Complex.zero, Complex(0, -1)),
      Vec(Complex(0, 1), Complex.zero)
    ).pipe(Q1Gate.apply)

  def phase(theta: Double): Q1Gate = {
    val z = Complex(
      Math.cos(theta),
      Math.sin(theta)
    )
    Matrix(
      Vec(1.toComplex, Complex.zero),
      Vec(Complex.zero, z)
    ).pipe(Q1Gate.apply)
  }

  def phase45: Q1Gate =
    phase(Math.PI / 4)

  def phase90: Q1Gate =
    phase(Math.PI / 2)

  def phase180: Q1Gate =
    phase(Math.PI)

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

  def had: Q1Gate = {
    val matrix = Matrix(
      Vec(1.toComplex, 1.toComplex),
      Vec(1.toComplex, Complex(-1, 0))
    )
    val coefficient = 1.0 / Math.sqrt(2)
    Q1Gate(coefficient * matrix)
  }

}
