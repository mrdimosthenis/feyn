package mrdimosthenis.feyn.math

import mrdimosthenis.feyn.math.extensions._

case class Complex(re: Double, im: Double) {

  override def toString: String =
    s"$re ${if (im >= 0) "+" else "-"} ${Math.abs(im)}*i"

  def conjugate: Complex = Complex(re, -im)

  def abs: Double = Math.sqrt(re * re + im * im)

  def opposite: Complex =
    Complex(-re, -im)

  def inverse: Option[Complex] =
    if (this == Complex.zero) None
    else {
      val invRe = re / (abs * abs)
      val invIm = -im / (abs * abs)
      Some(Complex(invRe, invIm))
    }

  def toPolar: (Double, Double) = {
    val theta = Math.atan(im / re)
    (abs, theta)
  }

  def +(z: Complex): Complex =
    Complex(re + z.re, im + z.im)

  def -(z: Complex): Complex =
    this + z.opposite

  def *(z: Complex): Complex =
    Complex(re * z.re - im * z.im, re * z.im + im * z.re)

  def /(z: Complex): Option[Complex] =
    z.inverse.map(_ * this)

  def ==(z: Complex): Boolean =
    re == z.re && im == z.im

  def =~(z: Complex)(implicit error: Threshold): Boolean =
    re =~ z.re && im =~ z.im

}

object Complex {

  def zero: Complex = 0.toComplex

  def fromPolar(r: Double, theta: Double): Complex = {
    val re = r * Math.cos(theta)
    val im = r * Math.sin(theta)
    Complex(re, im)
  }

}
