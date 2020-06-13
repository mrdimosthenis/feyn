package mrdimosthenis.feyn.math

import mrdimosthenis.feyn.math.extensions._

import scala.util.chaining._

case class Matrix(rows: Vec*) {

  def exceptDiffDims(a: Matrix): Unit =
    if (dims != a.dims)
      throw new Exception("Matrices of different dimensions")

  val lazyRows: LazyList[Vec] =
    rows.to(LazyList)

  override def toString: String = {
    val maxCompLength = lazyRows
      .flatMap(_.lazyComponents)
      .map(_.toString.length)
      .max
    lazyRows.map { v =>
      v.lazyComponents
        .map { z =>
          (maxCompLength - z.toString.length)
            .pipe(" ".repeat)
            .pipe(_ + z)
        }
        .mkString("\t")
    }.mkString("|", "|\n|", "|")
  }

  def dims: (Int, Int) = {
    val m = rows.head.dim
    if (rows.tail.exists(_.dim != m))
      throw new Exception("Matrix with columns of different dimension")
    val n = lazyRows.length
    (m, n)
  }

  def opposite: Matrix =
    lazyRows
      .map(_.opposite)
      .pipe(Matrix.apply)

  def transposed: Matrix =
    lazyRows
      .map(_.lazyComponents)
      .transpose
      .map(Vec.apply)
      .pipe(Matrix.apply)

  def transjugate: Matrix =
    lazyRows
      .map { v =>
        v.lazyComponents
          .map(_.conjugate)
      }.map(Vec.apply)
      .pipe(Matrix.apply)
      .transposed

  def +(a: Matrix): Matrix = {
    exceptDiffDims(a)
    lazyRows
      .zip(a.lazyRows)
      .map { case (v1, v2) => v1 + v2 }
      .pipe(Matrix.apply)
  }

  def -(a: Matrix): Matrix = {
    exceptDiffDims(a)
    this + a.opposite
  }

  def *(a: Matrix): Matrix = {
    if (dims._2 != a.dims._1)
      throw new Exception("Matrix multiplication of non-matching dimensions")
    lazyRows.map { row =>
      a.transjugate
        .lazyRows
        .map(v => row * v)
    }
      .map(Vec.apply)
      .pipe(Matrix.apply)
  }

  def ==(a: Matrix): Boolean = {
    exceptDiffDims(a)
    lazyRows
      .zip(a.lazyRows)
      .forall(z => z._1 == z._2)
  }

  def =~(a: Matrix)(implicit error: Threshold): Boolean = {
    exceptDiffDims(a)
    lazyRows
      .zip(a.lazyRows)
      .forall(x => x._1 =~ x._2)
  }

}

object Matrix {

  def zero(m: Int, n: Int): Matrix =
    LazyList
      .fill(n)(Vec.zero(m))
      .pipe(Matrix.apply)

  def id(m: Int, n: Int): Matrix =
    zero(m, n)
      .lazyRows
      .zipWithIndex
      .map { case (v, i) =>
        v.lazyComponents.zipWithIndex.map { case (x, j) =>
          if (j == i) Complex(1.0, 0.0)
          else x
        }
      }.map(Vec.apply)
      .pipe(Matrix.apply)

}
