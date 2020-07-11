package mrdimosthenis.feyn.graphics

import mrdimosthenis.feyn.math.{Complex, Vec}
import mrdimosthenis.feyn.quantum.Qubit
import mrdimosthenis.feyn.quantum.gates._

object elems {

  implicit class QubitsElem(val qubits: LazyList[Qubit]) {

    def text: String =
      qubits.map(_.txt).mkString("/n")

  }

  implicit class Q1GateElem(val q1Gate: Q1Gate) {

    def text(k: Int, n: Int): String =
      LazyList
        .from(0)
        .take(n)
        .map { i =>
          if (i == k)
            q1Gate.txt
          else
            s"""
               |─────
               |     """
              .stripMargin
        }.mkString("\n")

  }

  implicit class Q2GateElem(val q2Gate: Q2Gate) {

    def text(ks: (Int, Int), n: Int): String =
      LazyList
        .from(0)
        .take(n)
        .map { i =>
          if (i == ks._1)
            q2Gate.txt._1
          else if (i == ks._2)
            q2Gate.txt._2
          else if (i > ks._1 && i < ks._2)
            s"""  │
               |──■──
               |  │  """
              .stripMargin
          else
            s"""
               |─────
               |     """
              .stripMargin
        }.mkString("\n")

  }

  implicit class Q3GateElem(val q3Gate: Q3Gate) {

    def text(ks: (Int, Int, Int), n: Int): String = {
      LazyList
        .from(0)
        .take(n)
        .map { i =>
          if (i == ks._1)
            q3Gate.txt._1
          else if (i == ks._2)
            q3Gate.txt._2
          else if (i == ks._3)
            q3Gate.txt._3
          else if (i > ks._1 && i < ks._3)
            s"""  │
               |──■──
               |  │  """
              .stripMargin
          else
            s"""
               |─────
               |     """
              .stripMargin
        }.mkString("\n")
    }

  }

  implicit class ComplexElem(val z: Complex) {

    def svg(width: Double, color: String): String = {
      val r = width / 2
      val x = z.re * width
      val y = z.im * width
      s"""<svg height="$width" $width="80">
         |  <circle cx="$r" cy="$r" r="$r" stroke="$color" fill-opacity="0.0"/>
         |  <circle cx="$r" cy="$r" r="1" stroke="$color" fill-opacity="0.25"/>
         |  <circle cx="$x" cy="$y" r="3" fill="$color" fill-opacity="0.75"/>
         |</svg>""".stripMargin
    }

  }

}
