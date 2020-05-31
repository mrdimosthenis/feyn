package mrdimosthenis.feyn.math

case class Complex(re: Double, im: Double) {

  val abs: Double = Math.sqrt(re * re + im * im)

  def plus(z: Complex): Complex =
    Complex(re + z.re, im + z.im)

  def times(z: Complex): Complex =
    Complex(re * z.re - im * z.im, re * z.im + im * z.re)

  def isEqual(z: Complex): Boolean =
    re == z.re && im == z.im

  def isAlmostEqual(z: Complex)(implicit error: Double): Boolean =
    (Math.abs(re - z.re) < error) && (Math.abs(im - z.im) < error)

}

object Complex {

  val zero: Complex = Complex(0.0, 0.0)

}
