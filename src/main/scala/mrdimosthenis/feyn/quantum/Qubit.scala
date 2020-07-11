package mrdimosthenis.feyn.quantum

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._

case class Qubit(vec: Vec, txt: String)

object Qubit {

  def zero: Qubit = {
    val vec = Vec(1.toComplex, Complex.zero)
    val txt =
      s"""
         | 0>──
         |     """
        .stripMargin
    Qubit(vec, txt)
  }

  def one: Qubit = {
    val vec = Vec(Complex.zero, 1.toComplex)
    val txt =
      s"""
         | 1>──
         |     """
        .stripMargin
    Qubit(vec, txt)
  }

}
