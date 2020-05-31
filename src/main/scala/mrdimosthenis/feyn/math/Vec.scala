package mrdimosthenis.feyn.math

import scala.util.chaining._

case class Vec(coordinates: Double*) {

  val lazyCoordinates: LazyList[Double] =
    coordinates.to(LazyList)

  val dim: Int = lazyCoordinates.length

  def exceptDiffDims(z: Vec): Unit =
    if (dim != z.dim)
      throw new Exception("Vectors of different dimension")

  val length: Double = lazyCoordinates
    .map(x => x * x)
    .sum
    .pipe(Math.sqrt)

  def scalarProduct(c: Double): Vec =
    lazyCoordinates
      .map(c * _)
      .pipe { x => Vec(x: _*) }

  def normalized: Vec = {
    if (length == 0)
      throw new Exception("Unit of zero vector")
    this.scalarProduct(1.0 / length)
  }

  def plus(z: Vec): Vec = {
    exceptDiffDims(z)
    lazyCoordinates
      .zip(z.lazyCoordinates)
      .map(x => x._1 + x._2)
      .pipe { x => Vec(x: _*) }
  }

  def dotProduct(z: Vec): Double = {
    exceptDiffDims(z)
    lazyCoordinates
      .zip(z.lazyCoordinates)
      .map(x => x._1 * x._2)
      .sum
  }

  def isEqual(z: Vec): Boolean = {
    exceptDiffDims(z)
    lazyCoordinates
      .zip(z.lazyCoordinates)
      .forall(x => x._1 == x._2)
  }

  def isAlmostEqual(z: Vec)(implicit error: Double): Boolean = {
    exceptDiffDims(z)
    lazyCoordinates
      .zip(z.lazyCoordinates)
      .forall { x =>
        Math.abs(x._1 - x._2) < error
      }
  }

}

object Vec {

  def zero(n: Int): Vec =
    LazyList
      .fill(n)(0.0)
      .pipe { x => Vec(x: _*) }

}
