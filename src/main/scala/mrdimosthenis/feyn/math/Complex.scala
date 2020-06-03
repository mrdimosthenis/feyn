package mrdimosthenis.feyn.math

import mrdimosthenis.feyn.types._

case class Complex(re: Double, im: Double) {

  val conjugate: Complex = Complex(re, -im)

  val abs: Double = Math.sqrt(re * re + im * im)

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
    this + Complex(-z.re, -z.im)

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

    def *(z: Complex): Complex =
      Complex(x, 0.0) * z

  }

}
