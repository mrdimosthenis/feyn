package mrdimosthenis.feyn.quantum.gates

import mrdimosthenis.feyn.math._

import scala.util.chaining._

case class Q3Gate(matrix: Matrix, txt: (String, String, String))

object Q3Gate {

  private val controlTxt =
    s"""
       |──■──
       |  │  """
      .stripMargin

  val CCX: Q3Gate = {
    val (first6Rows, last2Rows) =
      Matrix
        .id(8)
        .lazyRows
        .splitAt(6)
    val matrix =
      LazyList
        .concat(first6Rows, last2Rows.reverse)
        .pipe(Matrix.apply)
    val txt2 =
      s"""  │
         |──■──
         |  │  """
        .stripMargin
    val txt3 =
      s"""┌─┴─┐
         |┤ X ├
         |└───┘"""
        .stripMargin
    Q3Gate(matrix, (controlTxt, txt2, txt3))
  }

  val CSWAP: Q3Gate = {
    val (first5Rows, last3Rows) =
      Matrix
        .id(8)
        .lazyRows
        .splitAt(5)
    val (lastRow, reversedButLastRows) =
      last3Rows.reverse.splitAt(1)
    val matrix =
      LazyList
        .concat(first5Rows, reversedButLastRows, lastRow)
        .pipe(Matrix.apply)
    val txt2 =
      s"""  │
         |──X──
         |  │  """
        .stripMargin
    val txt3 =
      s"""  │
         |──X──
         |     """
        .stripMargin
    Q3Gate(matrix, (controlTxt, txt2, txt3))
  }

}
