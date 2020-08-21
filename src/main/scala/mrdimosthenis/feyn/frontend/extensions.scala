package mrdimosthenis.feyn.frontend

import mrdimosthenis.feyn.math.Complex
import org.scalajs.dom.svg._
import scalatags.JsDom._
import scalatags.JsDom.all._
import scalatags.JsDom.svgTags._
import scalatags.JsDom.svgAttrs._

import scala.util.chaining._

object extensions {

  implicit class ComplexTupleSvgExtension(val zs: (Complex, Complex)) {

    private val filters = svgTags.filter(svgAttrs.id := "blurMe")(
      feOffset(
        result := "offOut",
        in:="SourceAlpha",
        dx:=1,
        dy:=1,
      ),
      feGaussianBlur(
        stdDeviation := 1
      ),
      feBlend(
        in:="SourceGraphic",
        in2:="blurOut",
        mode:="normal",
      )
    )

    private def svgCircle(_cx: Double,
                  _cy: Double,
                  _r: Double,
                  _fillOpacity: Double,
                  _stroke: Option[String],
                  _fill: Option[String],
                  _filter: Option[String])
    : TypedTag[Circle] = {
      List(
        cx := _cx,
        cy := _cy,
        r := _r,
        fillOpacity := _fillOpacity
      ).concat {
        _stroke match {
          case Some(color) =>
            List(stroke := color)
          case _ => List()
        }
      }.concat {
        _fill match {
          case Some(color) =>
            List(fill := color)
          case _ => List()
        }
      }.concat {
        _filter match {
          case Some(_filter_url) =>
            List(svgAttrs.filter := _filter_url)
          case _ => List()
        }
      }.pipe (circle.apply)
    }

    def svg(width: Double, colors: (String, String)):
    TypedTag[SVG] = {
      val r = width / 2
      val x1 = r + zs._1.re * r
      val y1 = r - zs._1.im * r
      val x2 = r + zs._2.re * r
      val y2 = r - zs._2.im * r

      svgTags.svg(
        svgAttrs.height:=width,
        svgAttrs.width:=width
      )(
        filters,
          svgCircle(r, r, r, 0.0, Some(colors._1), None, None),
          svgCircle(r, r, 1.0, 0.25, Some(colors._1), None, None),
          svgCircle(x1, y1, 4.0, 0.75, None, Some(colors._1), Some("url(#blurMe)")),
          svgCircle(x2, y2, 4.0, 0.75, None, Some(colors._2), Some("url(#blurMe)"))
      )
    }

  }

}
