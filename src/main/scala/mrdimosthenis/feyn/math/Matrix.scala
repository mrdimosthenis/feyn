package mrdimosthenis.feyn.math

import mrdimosthenis.feyn.math.extensions._

import scala.util.chaining._

case class Matrix(rows: Vec*) {

  private def exceptDiffDims(a: Matrix): Unit =
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
    val m = lazyRows.head.dim
    if (lazyRows.tail.exists(_.dim != m))
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

  def vecExpansion: Vec =
    lazyRows
      .flatMap(_.lazyComponents)
      .pipe(Vec.apply)

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
    if (dims._1 != a.dims._2)
      throw new Exception("Matrix multiplication of non-matching dimensions")
    lazyRows.map { row =>
      a.transjugate
        .lazyRows
        .map(v => row * v)
    }
      .map(Vec.apply)
      .pipe(Matrix.apply)
  }

  def **(a: Matrix): Matrix = {
    val (m1, n1) = dims
    val (m2, n2) = a.dims

    def toNestedArray(b: Matrix): Array[Array[Complex]] =
      b.lazyRows
        .map(_.lazyComponents.toArray)
        .toArray

    def kroneckerProduct(matrix1: Array[Array[Complex]],
                         matrix2: Array[Array[Complex]]):
    Array[Array[Complex]] = {
      val array = Array.ofDim[Complex](n1 * n2, m1 * m2)
      for {
        i <- 0 until n1
        j <- 0 until m1
        k <- 0 until n2
        l <- 0 until m2
      } {
        array(n2 * i + k)(m2 * j + l) = matrix1(i)(j) * matrix2(k)(l)
      }
      array
    }

    kroneckerProduct(
      toNestedArray(this),
      toNestedArray(a)
    )
      .to(LazyList)
      .map { array =>
        array.to(LazyList)
      }.map(Vec.apply)
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

  def id(n: Int): Matrix =
    zero(n, n)
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
