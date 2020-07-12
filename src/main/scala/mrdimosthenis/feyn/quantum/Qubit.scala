package mrdimosthenis.feyn.quantum

import mrdimosthenis.feyn.math._
import mrdimosthenis.feyn.math.extensions._

case class Qubit(vec: Vec)

object Qubit {

  val zero: Qubit =
    Qubit(Vec(1.toComplex, Complex.zero))

  val one: Qubit =
    Qubit(Vec(Complex.zero, 1.toComplex))

}
