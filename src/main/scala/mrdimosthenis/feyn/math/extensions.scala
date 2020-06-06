package mrdimosthenis.feyn.math

import scala.util.chaining._
import scala.util.Random

object extensions {

  type Threshold = Double

  implicit class DoubleExtension(val x: Double) {

    val toComplex: Complex =
      Complex(x, 0.0)

    def *(z: Complex): Complex =
      Complex(x, 0.0) * z

    def *(v: Vec): Vec =
      x.toComplex * v

    def *(a: Matrix): Matrix =
      x.toComplex * a

    def =~(y: Double)(implicit error: Threshold): Boolean = {
      Math.abs(x - y) < error
    }

  }

  implicit class RandomExtension(val r: Random) {

    def nextComplex(): Complex =
      Complex(r.nextDouble(), r.nextDouble())

    def nextVec(n: Int): Vec =
      Vec.zero(n)
        .lazyComponents
        .map(_ => r.nextComplex())
        .pipe { zs => Vec(zs: _*) }

    def nextMatrix(m: Int, n: Int): Matrix =
      Matrix.zero(m, n)
        .lazyColumns
        .map(_ => r.nextVec(m))
        .pipe { v => Matrix(v: _*) }

  }

  implicit class ComplexExtension(val z: Complex) {

    def *(v: Vec): Vec =
      v.lazyComponents
        .map(z * _)
        .pipe { zs => Vec(zs: _*) }

    def *(a: Matrix): Matrix =
      a.lazyColumns
        .map(z * _)
        .pipe { v => Matrix(v: _*) }

  }

}
