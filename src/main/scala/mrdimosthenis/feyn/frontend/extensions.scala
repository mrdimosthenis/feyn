package mrdimosthenis.feyn.frontend

import mrdimosthenis.feyn.math.Complex
import mrdimosthenis.feyn.quantum.Qubit
import mrdimosthenis.feyn.quantum.gates._
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
    y2 := 3.0 * unitLength,
    stroke := "black"
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

  private def horizontalCable(unitLength: Double)
  : TypedTag[Line] = line(
    x1 := 0.0 * unitLength,
    y1 := 3.0 * unitLength,
    x2 := 6.0 * unitLength,
    y2 := 3.0 * unitLength,
    stroke := "black"
  )

  private def verticalCable(unitLength: Double)
  : TypedTag[Line] = line(
    x1 := 3.0 * unitLength,
    y1 := 0.0 * unitLength,
    x2 := 3.0 * unitLength,
    y2 := 6.0 * unitLength,
    stroke := "black"
  )

  private def boxSvg(unitLength: Double)
  : TypedTag[RectElement] =
    rect(
      x := unitLength,
      y := unitLength,
      svgAttrs.width := 4.0 * unitLength,
      svgAttrs.height := 4.0 * unitLength,
      stroke := "black",
      fillOpacity := 0.0
    )

  private def controlSvg(unitLength: Double)
  : TypedTag[Circle] = circle(
    cx := 3.0 * unitLength,
    cy := 3.0 * unitLength,
    r := unitLength / 2.0
  )

  private def xSvgLines(unitLength: Double)
  : LazyList[TypedTag[Line]] = LazyList(
    line(
      x1 := 2.0 * unitLength,
      y1 := 2.0 * unitLength,
      x2 := 4.0 * unitLength,
      y2 := 4.0 * unitLength,
      stroke := "black"
    ),
    line(
      x1 := 2.0 * unitLength,
      y1 := 4.0 * unitLength,
      x2 := 4.0 * unitLength,
      y2 := 2.0 * unitLength,
      stroke := "black"
    )
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
      fillOpacity := 0.0
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

  implicit class Q1GateSvgExtension(val q1Gate: Q1Gate) {

    def svg(unitLength: Double)(k: Int, n: Int)
    : LazyList[TypedTag[SVG]] = {
      val g = q1Gate match {
        case Q1Gate.X =>
          xSvgLines(unitLength)
        case Q1Gate.Y =>
          LazyList(
            line(
              x1 := 2.0 * unitLength,
              y1 := 2.0 * unitLength,
              x2 := 3.0 * unitLength,
              y2 := 3.0 * unitLength,
              stroke := "black"
            ),
            line(
              x1 := 3.0 * unitLength,
              y1 := 3.0 * unitLength,
              x2 := 4.0 * unitLength,
              y2 := 2.0 * unitLength,
              stroke := "black"
            ),
            line(
              x1 := 3.0 * unitLength,
              y1 := 3.0 * unitLength,
              x2 := 3.0 * unitLength,
              y2 := 4.0 * unitLength,
              stroke := "black"
            )
          )
        case Q1Gate.Z =>
          LazyList(
            line(
              x1 := 2.0 * unitLength,
              y1 := 2.0 * unitLength,
              x2 := 4.0 * unitLength,
              y2 := 2.0 * unitLength,
              stroke := "black"
            ),
            line(
              x1 := 4.0 * unitLength,
              y1 := 2.0 * unitLength,
              x2 := 2.0 * unitLength,
              y2 := 4.0 * unitLength,
              stroke := "black"
            ),
            line(
              x1 := 2.0 * unitLength,
              y1 := 4.0 * unitLength,
              x2 := 4.0 * unitLength,
              y2 := 4.0 * unitLength,
              stroke := "black"
            )
          )
        case Q1Gate.H =>
          LazyList(
            line(
              x1 := 2.0 * unitLength,
              y1 := 2.0 * unitLength,
              x2 := 2.0 * unitLength,
              y2 := 4.0 * unitLength,
              stroke := "black"
            ),
            line(
              x1 := 4.0 * unitLength,
              y1 := 2.0 * unitLength,
              x2 := 4.0 * unitLength,
              y2 := 4.0 * unitLength,
              stroke := "black"
            ),
            line(
              x1 := 2.0 * unitLength,
              y1 := 3.0 * unitLength,
              x2 := 4.0 * unitLength,
              y2 := 3.0 * unitLength,
              stroke := "black"
            )
          )
        case Q1Gate.S =>
          LazyList(
            line(
              x1 := 3.5 * unitLength,
              y1 := 2.0 * unitLength,
              x2 := 3.0 * unitLength,
              y2 := 2.0 * unitLength,
              stroke := "black"
            ),
            line(
              x1 := 3.0 * unitLength,
              y1 := 2.0 * unitLength,
              x2 := 2.5 * unitLength,
              y2 := 2.5 * unitLength,
              stroke := "black"
            ),
            line(
              x1 := 2.5 * unitLength,
              y1 := 2.5 * unitLength,
              x2 := 3.0 * unitLength,
              y2 := 3.0 * unitLength,
              stroke := "black"
            ),
            line(
              x1 := 3.0 * unitLength,
              y1 := 3.0 * unitLength,
              x2 := 3.5 * unitLength,
              y2 := 3.5 * unitLength,
              stroke := "black"
            ),
            line(
              x1 := 3.5 * unitLength,
              y1 := 3.5 * unitLength,
              x2 := 3.0 * unitLength,
              y2 := 4.0 * unitLength,
              stroke := "black"
            ),
            line(
              x1 := 3.0 * unitLength,
              y1 := 4.0 * unitLength,
              x2 := 2.5 * unitLength,
              y2 := 4.0 * unitLength,
              stroke := "black"
            )
          )
        case Q1Gate.T =>
          LazyList(
            line(
              x1 := 2.0 * unitLength,
              y1 := 2.0 * unitLength,
              x2 := 4.0 * unitLength,
              y2 := 2.0 * unitLength,
              stroke := "black"
            ),
            line(
              x1 := 3.0 * unitLength,
              y1 := 2.0 * unitLength,
              x2 := 3.0 * unitLength,
              y2 := 4.0 * unitLength,
              stroke := "black"
            )
          )
      }
      LazyList
        .from(0)
        .take(n)
        .map {
          case `k` =>
            g.prepended(leftCable(unitLength))
              .prepended(rightCable(unitLength))
              .prepended(boxSvg(unitLength))
          case _ =>
            LazyList(horizontalCable(unitLength))
        }.map {
        svgTags.svg(
          svgAttrs.height := 6.0 * unitLength,
          svgAttrs.width := 6.0 * unitLength
        ).apply
      }
    }

  }

  implicit class Q2GateSvgExtension(val q2Gate: Q2Gate) {

    def svg(unitLength: Double)(ks: (Int, Int), n: Int)
    : LazyList[TypedTag[SVG]] = {
      val g1 = q2Gate match {
        case Q2Gate.CX =>
          LazyList(
            controlSvg(unitLength),
            bottomCable(unitLength),
            horizontalCable(unitLength)
          )
        case Q2Gate.SWAP =>
          xSvgLines(unitLength)
            .prepended(bottomCable(unitLength))
            .prepended(horizontalCable(unitLength))
      }
      val g2 = q2Gate match {
        case Q2Gate.CX =>
          xSvgLines(unitLength)
            .prepended(leftCable(unitLength))
            .prepended(rightCable(unitLength))
            .prepended(topCable(unitLength))
            .prepended(boxSvg(unitLength))
        case Q2Gate.SWAP =>
          xSvgLines(unitLength)
            .prepended(topCable(unitLength))
            .prepended(horizontalCable(unitLength))
      }
      LazyList
        .from(0)
        .take(n)
        .map { i =>
          if (i == ks._1)
            g1
          else if (i == ks._2)
            g2
          else if (i > ks._1 && i < ks._2)
            LazyList(
              horizontalCable(unitLength),
              verticalCable(unitLength)
            )
          else
            LazyList(
              horizontalCable(unitLength)
            )
        }.map {
        svgTags.svg(
          svgAttrs.height := 6.0 * unitLength,
          svgAttrs.width := 6.0 * unitLength
        ).apply
      }
    }

  }

  implicit class Q3GateSvgExtension(val q3Gate: Q3Gate) {

    def svg(unitLength: Double)(ks: (Int, Int, Int), n: Int)
    : LazyList[TypedTag[SVG]] = {
      val (g1, g2, g3) = q3Gate match {
        case Q3Gate.CSWAP =>
          (
            LazyList(
              controlSvg(unitLength),
              bottomCable(unitLength),
              horizontalCable(unitLength)
            ),
            xSvgLines(unitLength)
              .prepended(topCable(unitLength))
              .prepended(bottomCable(unitLength))
              .prepended(horizontalCable(unitLength)),
            xSvgLines(unitLength)
              .prepended(topCable(unitLength))
              .prepended(horizontalCable(unitLength))
          )
        case Q3Gate.CCX =>
          (
            LazyList(
              controlSvg(unitLength),
              bottomCable(unitLength),
              horizontalCable(unitLength)
            ),
            LazyList(
              topCable(unitLength),
              controlSvg(unitLength),
              bottomCable(unitLength),
              horizontalCable(unitLength)
            ),
            xSvgLines(unitLength)
              .prepended(leftCable(unitLength))
              .prepended(rightCable(unitLength))
              .prepended(topCable(unitLength))
              .prepended(boxSvg(unitLength))
          )
      }
      LazyList
        .from(0)
        .take(n)
        .map { i =>
          if (i == ks._1)
            g1
          else if (i == ks._2)
            g2
          else if (i == ks._3)
            g3
          else if (i > ks._1 && i < ks._3)
            LazyList(
              horizontalCable(unitLength),
              verticalCable(unitLength)
            )
          else
            LazyList(horizontalCable(unitLength))
        }.map {
        svgTags.svg(
          svgAttrs.height := 6.0 * unitLength,
          svgAttrs.width := 6.0 * unitLength
        ).apply
      }
    }

  }

}
