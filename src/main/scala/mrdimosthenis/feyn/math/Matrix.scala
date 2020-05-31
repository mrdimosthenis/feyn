package mrdimosthenis.feyn.math

import scala.util.chaining._

case class Matrix(columns: Vec*) {

  val lazyColumns: LazyList[Vec] =
    columns.to(LazyList)

  val size: (Int, Int) = {
    val m = columns.head.dim
    if (columns.exists(_.dim != m))
      throw new Exception("Matrix with columns of different dimension")
    val n = lazyColumns.length
    (m, n)
  }

  def transposed: Matrix =
    lazyColumns
      .map(_.lazyCoordinates)
      .transpose
      .map { x => Vec(x: _*) }
      .pipe { v => Matrix(v: _*) }

  def scalarProduct(c: Double): Matrix =
    lazyColumns
      .map {
        _.scalarProduct(c)
      }
      .pipe { v => Matrix(v: _*) }

  def plus(b: Matrix): Matrix = {
    if (size != b.size)
      throw new Exception("Matrices of different size")
    lazyColumns
      .zip(b.lazyColumns)
      .map { case (v1, v2) =>
        v1.plus(v2)
      }.pipe { v => Matrix(v: _*) }
  }

  def product(b: Matrix): Matrix = {
    if (size._2 != b.size._1)
      throw new Exception("Matrix multiplication of non-matching sizes")
    transposed.lazyColumns.map { row =>
      b.lazyColumns.map { v =>
        row.dotProduct(v)
      }
    }.map { x => Vec(x: _*) }
  }.pipe { v => Matrix(v: _*) }

}

object Matrix {

  def id(m: Int, n: Int): Matrix =
    LazyList
      .fill(n)(Vec.zero(m))
      .zipWithIndex
      .map { case (v, i) =>
        v.lazyCoordinates.zipWithIndex.map { case (x, j) =>
          if (j == i) 1.0
          else x
        }
      }.map { x => Vec(x: _*) }
      .pipe { v => Matrix(v: _*) }

}
