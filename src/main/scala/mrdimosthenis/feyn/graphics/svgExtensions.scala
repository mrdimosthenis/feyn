package mrdimosthenis.feyn.graphics

import mrdimosthenis.feyn.math.Complex

import org.scalajs.dom.{Element, document}

object svgExtensions {

  implicit class ComplexSvgExtension(val z: Complex) {

    private def svgCircle(cx: Double,
                          cy: Double,
                          r: Double,
                          fillOpacity: Double,
                          stroke: Option[String],
                          fill: Option[String])
    : Element = {
      val circle = document.createElement("circle")
      circle.setAttribute("cx", cx.toString)
      circle.setAttribute("cy", cy.toString)
      circle.setAttribute("r", r.toString)
      circle.setAttribute("fill-opacity", fillOpacity.toString)
      stroke match {
        case Some(color) =>
          circle.setAttribute("stroke", color)
        case _ => ()
      }
      fill match {
        case Some(color) =>
          circle.setAttribute("fill", color)
        case _ => ()
      }
      circle
    }


    def svg(width: Double, color: String): Element = {
      val r = width / 2
      val x = r + z.re * r
      val y = r - z.im * r

      val circle1 = svgCircle(r, r, r, 0.0, Some(color), None)
      val circle2 = svgCircle(r, r, 1.0, 0.25, Some(color), None)
      val circle3 = svgCircle(x, y, 5.0, 0.75, None, Some(color))

      val graphic = document.createElement("svg")
      graphic.setAttribute("height", width.toString)
      graphic.setAttribute("width", width.toString)

      graphic.appendChild(circle1)
      graphic.appendChild(circle2)
      graphic.appendChild(circle3)

      graphic
    }

  }

}
