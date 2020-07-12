package mrdimosthenis.feyn.graphics

import mrdimosthenis.feyn.math.Complex

object svgExtensions {

  implicit class ComplexSvgExtension(val z: Complex) {

    def svg(width: Double, color: String): String = {
      val r = width / 2
      val x = z.re * width
      val y = z.im * width
      s"""<svg height="$width" width="$width">
         |  <circle cx="$r" cy="$r" r="$r" stroke="$color" fill-opacity="0.0"/>
         |  <circle cx="$r" cy="$r" r="1" stroke="$color" fill-opacity="0.25"/>
         |  <circle cx="$x" cy="$y" r="3" fill="$color" fill-opacity="0.75"/>
         |</svg>""".stripMargin
    }

  }

}
