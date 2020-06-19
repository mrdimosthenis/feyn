package mrdimosthenis.feyn.quantum

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._

case class Qubit(u0: Complex, u1: Complex) {

  def toVec: Vec = Vec(u0, u1)

}

object Qubit {

  def zero: Qubit =
    Qubit(1.toComplex, Complex.zero)

  def one: Qubit =
    Qubit(Complex.zero, 1.toComplex)

}
