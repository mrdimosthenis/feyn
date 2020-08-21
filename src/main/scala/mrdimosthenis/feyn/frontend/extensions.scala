package mrdimosthenis.feyn.frontend

import mrdimosthenis.feyn.math.Complex
import mrdimosthenis.feyn.quantum.Qubit
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
        in := "SourceAlpha",
        dx := 1,
        dy := 1,
      ),
      feGaussianBlur(
        stdDeviation := 1
      ),
      feBlend(
        in := "SourceGraphic",
        in2 := "blurOut",
        mode := "normal",
      )
    )

    private def circleSvg(_cx: Double,
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
      }.pipe(circle.apply)
    }

    def svg(width: Double, colors: (String, String))
    : TypedTag[SVG] = {
      val r = width / 2
      val x1 = r + zs._1.re * r
      val y1 = r - zs._1.im * r
      val x2 = r + zs._2.re * r
      val y2 = r - zs._2.im * r

      svgTags.svg(
        svgAttrs.height := width,
        svgAttrs.width := width
      )(
        filters,
        circleSvg(r, r, r, 0.0, Some(colors._1), None, None),
        circleSvg(r, r, 1.0, 0.25, Some(colors._1), None, None),
        circleSvg(x1, y1, 4.0, 0.75, None, Some(colors._1), Some("url(#blurMe)")),
        circleSvg(x2, y2, 4.0, 0.75, None, Some(colors._2), Some("url(#blurMe)"))
      )
    }

  }

  private def leftCable(unitLength: Double)
  : TypedTag[Line] = line(
    x1 := 0.0,
    y1 := 3.0 * unitLength,
    x2 := unitLength,
    y2 := 3.0 * unitLength
  )

  private def rightCable(unitLength: Double)
  : TypedTag[Line] = line(
    x1 := 5.0 * unitLength,
    y1 := 3.0 * unitLength,
    x2 := 6.0 * unitLength,
    y2 := 3.0 * unitLength,
    stroke := "black"
  )

  private def topCable(unitLength: Double)
  : TypedTag[Line] = line(
    x1 := 3.0 * unitLength,
    y1 := 0.0,
    x2 := 3.0 * unitLength,
    y2 := unitLength,
    stroke := "black"
  )

  private def bottomCable(unitLength: Double)
  : TypedTag[Line] = line(
    x1 := 3.0 * unitLength,
    y1 := 5.0 * unitLength,
    x2 := 3.0 * unitLength,
    y2 := 6.0 * unitLength,
    stroke := "black"
  )

  private def svgBox(unitLength: Double)
  : TypedTag[RectElement] =
    rect(
      x := unitLength,
      y := unitLength,
      svgAttrs.width := 4.0 * unitLength,
      svgAttrs.height := 4.0 * unitLength
    )

  implicit class QubitsSvgExtension(val qubits: LazyList[Qubit]) {

    private def greaterThanSvg(unitLength: Double)
    : TypedTag[SVG] = svgTags.svg(
      svgAttrs.height := 6.0 * unitLength,
      svgAttrs.width := 6.0 * unitLength
    )(
      line(
        x1 := 4.0 * unitLength,
        y1 := 2.0 * unitLength,
        x2 := 5.0 * unitLength,
        y2 := 3.0 * unitLength,
        stroke := "black"
      ),
      line(
        x1 := 5.0 * unitLength,
        y1 := 3.0 * unitLength,
        x2 := 4.0 * unitLength,
        y2 := 4.0 * unitLength,
        stroke := "black"
      )
    )

    private def oneSvg(unitLength: Double)
    : TypedTag[Line] = line(
      x1 := 3.0 * unitLength,
      y1 := 2.0 * unitLength,
      x2 := 3.0 * unitLength,
      y2 := 4.0 * unitLength,
      stroke := "black"
    )

    private def zeroSvg(unitLength: Double)
    : TypedTag[Circle] = circle(
      cx := 3.0 * unitLength,
      cy := 3.0 * unitLength,
      r := unitLength,
      stroke := "black",
      fillOpacity:=0.0
    )

    private def qubitSvg(unitLength: Double)(isOne: Boolean)
    : TypedTag[SVG] =
      svgTags.svg(
        svgAttrs.height := 6.0 * unitLength,
        svgAttrs.width := 6.0 * unitLength
      )(
        if (isOne) oneSvg(unitLength) else zeroSvg(unitLength),
        greaterThanSvg(unitLength),
        rightCable(unitLength)
      )

    def svg(unitLength: Double): LazyList[TypedTag[SVG]] =
      qubits
        .map {
          case Qubit.one => true
          case Qubit.zero => false
        }
        .map(qubitSvg(unitLength))

  }

}
