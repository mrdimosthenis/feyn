package mrdimosthenis.feyn.math

import Vec._
import Complex._

import scala.util.Random
import scala.util.chaining._

case class Matrix(columns: Vec*) {

  val lazyColumns: LazyList[Vec] =
    columns.to(LazyList)

  val dims: (Int, Int) = {
    val m = columns.head.dim
    if (columns.tail.exists(_.dim != m))
      throw new Exception("Matrix with columns of different dimension")
    val n = lazyColumns.length
    (m, n)
  }

  val opposite: Matrix =
    lazyColumns
      .map(_.opposite)
      .pipe { zs => Matrix(zs: _*) }

  val transposed: Matrix =
    lazyColumns
      .map(_.lazyComponents)
      .transpose
      .map { x => Vec(x: _*) }
      .pipe { v => Matrix(v: _*) }

  def +(a: Matrix): Matrix = {
    if (dims != a.dims)
      throw new Exception("Matrices of different dimensions")
    lazyColumns
      .zip(a.lazyColumns)
      .map { case (v1, v2) => v1 + v2 }
      .pipe { v => Matrix(v: _*) }
  }

  def -(a: Matrix): Matrix = {
    if (dims != a.dims)
      throw new Exception("Matrices of different dimensions")
    this + a.opposite
  }

  def *(a: Matrix): Matrix = {
    if (dims._2 != a.dims._1)
      throw new Exception("Matrix multiplication of non-matching dimensions")
    transposed.lazyColumns.map { row =>
      a.lazyColumns
        .map(v => row * v)
    }.map { x => Vec(x: _*) }
  }.pipe { v => Matrix(v: _*) }

}

object Matrix {

  def zero(m: Int, n: Int): Matrix =
    LazyList
      .fill(n)(Vec.zero(m))
      .pipe { v => Matrix(v: _*) }

  def id(m: Int, n: Int): Matrix =
    zero(m, n)
      .lazyColumns
      .zipWithIndex
      .map { case (v, i) =>
        v.lazyComponents.zipWithIndex.map { case (x, j) =>
          if (j == i) Complex(1.0, 0.0)
          else x
        }
      }.map { x => Vec(x: _*) }
      .pipe { v => Matrix(v: _*) }

  implicit class ComplexMatrixExtension(val z: Complex) {

    def *(a: Matrix): Matrix =
      a.lazyColumns
        .map(z * _)
        .pipe { v => Matrix(v: _*) }

  }

  implicit class DoubleMatrixExtension(val x: Double) {

    def *(a: Matrix): Matrix =
      x.toComplex * a

  }

  implicit class RandomMatrixExtension(val r: Random) {

    def nextMatrix(m: Int, n: Int): Matrix =
      zero(m, n)
        .lazyColumns
        .map(_ => r.nextVec(m))
        .pipe { v => Matrix(v: _*) }

  }

}
