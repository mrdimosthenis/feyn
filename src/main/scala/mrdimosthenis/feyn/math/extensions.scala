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

  implicit class RandomExtension(val random: Random) {

    def nextComplex(): Complex =
      Complex(random.nextDouble(), random.nextDouble())

    def nextVec(n: Int): Vec =
      Vec.zero(n)
        .lazyComponents
        .map(_ => random.nextComplex())
        .pipe(Vec.apply)

    def nextMatrix(m: Int, n: Int): Matrix =
      Matrix.zero(m, n)
        .lazyRows
        .map(_ => random.nextVec(m))
        .pipe(Matrix.apply)

  }

  implicit class ComplexExtension(val z: Complex) {

    def *(v: Vec): Vec =
      v.lazyComponents
        .map(z * _)
        .pipe(Vec.apply)

    def *(a: Matrix): Matrix =
      a.lazyRows
        .map(z * _)
        .pipe(Matrix.apply)

  }

  implicit class VecExtension(val v: Vec) {

    def **(u: Vec): Matrix =
      v.lazyComponents
        .map { z =>
          u.lazyComponents
            .map(z * _.conjugate)
            .pipe(Vec.apply)
        }.pipe(Matrix.apply)

    def toVerticalMatrix: Matrix =
      Matrix(v).transposed

  }

}
