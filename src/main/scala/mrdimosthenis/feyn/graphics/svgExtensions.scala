package mrdimosthenis.feyn.graphics

import mrdimosthenis.feyn.math.Complex

import org.scalajs.dom.{Element, document}

object svgExtensions {

  implicit class ComplexTupleSvgExtension(val zs: (Complex, Complex)) {

    private def svgCircle(cx: Double,
                          cy: Double,
                          r: Double,
                          fillOpacity: Double,
                          stroke: Option[String],
                          fill: Option[String],
                          filter: Option[String])
    : Element = {
      val circle = document.createElementNS("http://www.w3.org/2000/svg", "circle")
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
      filter match {
        case Some(filterUrl) =>
          circle.setAttribute("filter", filterUrl)
        case _ => ()
      }
      circle
    }

    def svg(width: Double, colors: (String, String)): Element = {
      val feOffset = document.createElementNS("http://www.w3.org/2000/svg", "feOffset")
      feOffset.setAttribute("result", "offOut")
      feOffset.setAttribute("in", "SourceAlpha")
      feOffset.setAttribute("dx", "1")
      feOffset.setAttribute("dy", "1")

      val feGaussianBlur = document.createElementNS("http://www.w3.org/2000/svg", "feGaussianBlur")
      feGaussianBlur.setAttribute("stdDeviation", "1")

      val feBlend = document.createElementNS("http://www.w3.org/2000/svg", "feBlend")
      feBlend.setAttribute("in", "SourceGraphic")
      feBlend.setAttribute("in2", "blurOut")
      feBlend.setAttribute("mode", "normal")

      val filter = document.createElementNS("http://www.w3.org/2000/svg", "filter")
      filter.setAttribute("id", "blurMe")
      filter.appendChild(feOffset)
      filter.appendChild(feGaussianBlur)
      filter.appendChild(feBlend)

      val r = width / 2
      val x1 = r + zs._1.re * r
      val y1 = r - zs._1.im * r
      val x2 = r + zs._2.re * r
      val y2 = r - zs._2.im * r

      val circle1 = svgCircle(r, r, r, 0.0, Some(colors._1), None, None)
      val circle2 = svgCircle(r, r, 1.0, 0.25, Some(colors._1), None, None)
      val circle3 = svgCircle(x1, y1, 3.0, 0.75, None, Some(colors._1), Some("url(#blurMe)"))
      val circle4 = svgCircle(x2, y2, 3.0, 0.75, None, Some(colors._2), Some("url(#blurMe)"))

      val graphic = document.createElementNS("http://www.w3.org/2000/svg", "svg")
      graphic.setAttribute("height", width.toString)
      graphic.setAttribute("width", width.toString)

      graphic.appendChild(filter)
      graphic.appendChild(circle1)
      graphic.appendChild(circle2)
      graphic.appendChild(circle3)
      graphic.appendChild(circle4)

      graphic
    }

  }

}
