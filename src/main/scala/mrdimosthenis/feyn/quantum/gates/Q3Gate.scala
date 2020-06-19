package mrdimosthenis.feyn.quantum.gates

import mrdimosthenis.feyn.math._

import scala.util.chaining._

case class Q3Gate(matrix: Matrix) extends Gate

object Q3Gate {

  def toffoli: Q3Gate = {
    val (first6Rows, last2Rows) =
      Matrix
        .id(8)
        .lazyRows
        .splitAt(6)
    LazyList
      .concat(first6Rows, last2Rows.reverse)
      .pipe(Matrix.apply)
      .pipe(Q3Gate.apply)
  }

}
