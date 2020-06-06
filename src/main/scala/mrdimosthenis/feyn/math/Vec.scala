package mrdimosthenis.feyn.math

import mrdimosthenis.feyn.math.extensions._

import scala.util.chaining._

case class Vec(components: Complex*) {

  def exceptDiffDims(v: Vec): Unit =
    if (dim != v.dim)
      throw new Exception("Vectors of different dimension")

  val lazyComponents: LazyList[Complex] =
    components.to(LazyList)

  val dim: Int = lazyComponents.length

  val norm: Double = lazyComponents
    .map(z => z.abs * z.abs)
    .sum
    .pipe(Math.sqrt)

  val opposite: Vec =
    lazyComponents
      .map(_.opposite)
      .pipe { zs => Vec(zs: _*) }

  val unit: Option[Vec] =
    if (norm == 0) None
    else Some {
      (1.0 / norm) * this
    }

  def *(v: Vec): Complex = {
    exceptDiffDims(v)
    lazyComponents
      .zip(v.lazyComponents)
      .map(t => t._1 * t._2)
      .foldLeft(Complex.zero) { (z1, z2) => z1 + z2 }
  }

  def +(v: Vec): Vec = {
    exceptDiffDims(v)
    lazyComponents
      .zip(v.lazyComponents)
      .map(z => z._1 + z._2)
      .pipe { zs => Vec(zs: _*) }
  }

  def -(v: Vec): Vec =
    this + v.opposite

  def ==(v: Vec): Boolean = {
    exceptDiffDims(v)
    lazyComponents
      .zip(v.lazyComponents)
      .forall(z => z._1 == z._2)
  }

  def =~(v: Vec)(implicit error: Threshold): Boolean = {
    exceptDiffDims(v)
    lazyComponents
      .zip(v.lazyComponents)
      .forall(x => x._1 =~ x._2)
  }

}

object Vec {

  def zero(n: Int): Vec =
    LazyList
      .fill(n)(Complex.zero)
      .pipe { zs => Vec(zs: _*) }

}
