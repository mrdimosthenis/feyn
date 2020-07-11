package mrdimosthenis.feyn.quantum.gates

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._

case class Q1Gate(matrix: Matrix, txt: String)

object Q1Gate {

  val X: Q1Gate = {
    val matrix =
      Matrix(
        Vec(Complex.zero, 1.toComplex),
        Vec(1.toComplex, Complex.zero)
      )
    val txt =
      s""" ┌───┐
         | ┤ X ├
         | └───┘"""
        .stripMargin
    Q1Gate(matrix, txt)
  }

  val Y: Q1Gate = {
    val matrix =
      Matrix(
        Vec(Complex.zero, Complex(0, -1)),
        Vec(Complex(0, 1), Complex.zero)
      )
    val txt =
      s""" ┌───┐
         | ┤ Y ├
         | └───┘"""
        .stripMargin
    Q1Gate(matrix, txt)
  }

  val Z: Q1Gate = {
    val matrix =
      Matrix(
        Vec(1.toComplex, Complex.zero),
        Vec(Complex.zero, Complex(-1, 0))
      )
    val txt =
      s""" ┌───┐
         | ┤ Z ├
         | └───┘"""
        .stripMargin
    Q1Gate(matrix, txt)
  }

  val H: Q1Gate = {
    val matrix =
      Matrix(
        Vec(1.toComplex, 1.toComplex),
        Vec(1.toComplex, Complex(-1, 0))
      )
    val coefficient = 1.0 / Math.sqrt(2)
    val txt =
      s""" ┌───┐
         | ┤ H ├
         | └───┘"""
        .stripMargin
    Q1Gate(matrix, txt)
    Q1Gate(coefficient * matrix, txt)
  }

  val S: Q1Gate = {
    val matrix =
      Matrix(
        Vec(1.toComplex, Complex.zero),
        Vec(Complex.zero, Complex(0, 1))
      )
    val txt =
      s""" ┌───┐
         | ┤ S ├
         | └───┘"""
        .stripMargin
    Q1Gate(matrix, txt)
  }

  val T: Q1Gate = {
    val z = Complex(
      Math.cos(Math.PI / 4),
      Math.sin(Math.PI / 4)
    )
    val matrix =
      Matrix(
        Vec(1.toComplex, Complex.zero),
        Vec(Complex.zero, z)
      )
    val txt =
      s""" ┌───┐
         | ┤ T ├
         | └───┘"""
        .stripMargin
    Q1Gate(matrix, txt)
  }

}
