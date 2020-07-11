package mrdimosthenis.feyn.quantum.gates

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._

case class Q2Gate(matrix: Matrix, txt: (String, String))

object Q2Gate {

  private val controlTxt =
    s"""
       |──■──
       |  │  """
      .stripMargin

  val CX: Q2Gate = {
    val matrix =
      Matrix(
        Vec(1.toComplex, Complex.zero, Complex.zero, Complex.zero),
        Vec(Complex.zero, 1.toComplex, Complex.zero, Complex.zero),
        Vec(Complex.zero, Complex.zero, Complex.zero, 1.toComplex),
        Vec(Complex.zero, Complex.zero, 1.toComplex, Complex.zero)
      )
    val txt2 =
      s"""┌─┴─┐
         |┤ X ├
         |└───┘"""
        .stripMargin
    Q2Gate(matrix, (controlTxt, txt2))
  }

  val CY: Q2Gate = {
    val matrix =
      Matrix(
        Vec(1.toComplex, Complex.zero, Complex.zero, Complex.zero),
        Vec(Complex.zero, 1.toComplex, Complex.zero, Complex.zero),
        Vec(Complex.zero, Complex.zero, Complex.zero, Complex(0, -1)),
        Vec(Complex.zero, Complex.zero, Complex(0, 1), Complex.zero)
      )
    val txt2 =
      s"""┌─┴─┐
         |┤ Y ├
         |└───┘"""
        .stripMargin
    Q2Gate(matrix, (controlTxt, txt2))
  }

  val CZ: Q2Gate = {
    val matrix =
      Matrix(
        Vec(1.toComplex, Complex.zero, Complex.zero, Complex.zero),
        Vec(Complex.zero, 1.toComplex, Complex.zero, Complex.zero),
        Vec(Complex.zero, Complex.zero, 1.toComplex, Complex.zero),
        Vec(Complex.zero, Complex.zero, Complex.zero, Complex(-1, 0))
      )
    val txt2 =
      s"""┌─┴─┐
         |┤ Z ├
         |└───┘"""
        .stripMargin
    Q2Gate(matrix, (controlTxt, txt2))
  }

  val SWAP: Q2Gate = {
    val matrix =
      Matrix(
        Vec(1.toComplex, Complex.zero, Complex.zero, Complex.zero),
        Vec(Complex.zero, Complex.zero, 1.toComplex, Complex.zero),
        Vec(Complex.zero, 1.toComplex, Complex.zero, Complex.zero),
        Vec(Complex.zero, Complex.zero, Complex.zero, 1.toComplex)
      )
    val txt1 =
      s"""
         |──X──
         |  │  """
        .stripMargin
    val txt2 =
      s"""  │
         |──X──
         |     """
        .stripMargin
    Q2Gate(matrix, (txt1, txt2))
  }

}
