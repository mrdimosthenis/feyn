package mrdimosthenis.feyn.math

import mrdimosthenis.feyn.types._

import scala.util.Random

case class Complex(re: Double, im: Double) {

  val conjugate: Complex = Complex(re, -im)

  val abs: Double = Math.sqrt(re * re + im * im)

  val opposite: Complex =
    Complex(-re, -im)

  val inverse: Option[Complex] =
    if (this == Complex.zero) None
    else {
      val invRe = re / (abs * abs)
      val invIm = -im / (abs * abs)
      Some(Complex(invRe, invIm))
    }

  def +(z: Complex): Complex =
    Complex(re + z.re, im + z.im)

  def -(z: Complex): Complex =
    this + z.opposite

  def *(z: Complex): Complex =
    Complex(re * z.re - im * z.im, re * z.im + im * z.re)

  def /(z: Complex): Option[Complex] =
    inverse.map(_ * z)

  def ==(z: Complex): Boolean =
    re == z.re && im == z.im

  def =~(z: Complex)(implicit e: Threshold): Boolean =
    (Math.abs(re - z.re) < e) && (Math.abs(im - z.im) < e)

}

object Complex {

  val zero: Complex = Complex(0.0, 0.0)

  implicit class DoubleComplexExtension(val x: Double) {

    val toComplex: Complex =
      Complex(x, 0.0)

    def *(z: Complex): Complex =
      Complex(x, 0.0) * z

  }

  implicit class RandomComplexExtension(val r: Random) {

    def nextComplex(): Complex =
      Complex(r.nextDouble(), r.nextDouble())

  }

}
