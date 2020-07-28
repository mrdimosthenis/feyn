package mrdimosthenis.feyn.graphics

import mrdimosthenis.feyn.quantum.Qubit
import mrdimosthenis.feyn.quantum.gates._

object textExtensions {

  // qubits

  implicit class QubitsTextExtension(val qubits: LazyList[Qubit]) {

    import mrdimosthenis.feyn.quantum.Qubit._

    private def qubitTxt(isOne: Boolean): String = {
      val q = if (isOne) "1" else "0"
      s"◌◌◌◌◌\n◌$q>──\n◌◌◌◌◌"
    }

    def text: LazyList[String] =
      qubits
        .map {
          case `zero` => false
          case `one` => true
        }
        .map(qubitTxt)

  }

  // gates

  private val emptyTxt =
    "◌◌◌◌◌\n─────\n◌◌◌◌◌"

  private val crossTxt =
    "◌◌│◌◌\n──│──\n◌◌│◌◌"

  private def controlTxt(isControlled: Boolean): String = {
    val c = if (isControlled) "│" else "◌"
    s"◌◌$c◌◌\n──■──\n◌◌│◌◌"
  }

  private def gateTxt(g: String, isControlled: Boolean): String = {
    val c = if (isControlled) "┴" else "─"
    s"┌─$c─┐\n┤◌$g◌├\n└───┘"
  }

  implicit class Q1GateTextExtension(val q1Gate: Q1Gate) {

    import mrdimosthenis.feyn.quantum.gates.Q1Gate._

    def text(k: Int, n: Int): LazyList[String] = {
      val g = q1Gate match {
        case `X` => "X"
        case `Y` => "Y"
        case `Z` => "Z"
        case `H` => "H"
        case `S` => "S"
        case `T` => "T"
      }
      LazyList
        .from(0)
        .take(n)
        .map {
          case `k` => gateTxt(g, isControlled = false)
          case _ => emptyTxt
        }
    }

  }

  implicit class Q2GateTextExtension(val q2Gate: Q2Gate) {

    import mrdimosthenis.feyn.quantum.gates.Q2Gate._

    def text(ks: (Int, Int), n: Int): LazyList[String] = {
      val txt1 = q2Gate match {
        case `SWAP` =>
          "◌◌◌◌◌\n──X──\n◌◌│◌◌"
        case _ =>
          controlTxt(false)
      }
      val txt2 = q2Gate match {
        case `SWAP` =>
          "◌◌│◌◌\n──X──\n◌◌◌◌◌"
        case cg =>
          val g = cg match {
            case `CX` => "X"
            case `CY` => "Y"
            case `CZ` => "Z"
          }
          gateTxt(g, isControlled = true)
      }
      LazyList
        .from(0)
        .take(n)
        .map { i =>
          if (i == ks._1) txt1
          else if (i == ks._2) txt2
          else if (i > ks._1 && i < ks._2) crossTxt
          else emptyTxt
        }
    }

  }

  implicit class Q3GateTextExtension(val q3Gate: Q3Gate) {

    import mrdimosthenis.feyn.quantum.gates.Q3Gate._

    def text(ks: (Int, Int, Int), n: Int): LazyList[String] = {
      val (txt1, txt2, txt3) = q3Gate match {
        case `CSWAP` =>
          (
            "◌◌◌◌◌\n──■──\n◌◌│◌◌",
            "◌◌│◌◌\n──X──\n◌◌│◌◌",
            "◌◌│◌◌\n──X──\n◌◌◌◌◌"
          )
        case `CCX` =>
          (
            controlTxt(false),
            controlTxt(true),
            gateTxt("X", isControlled = true)
          )
      }
      LazyList
        .from(0)
        .take(n)
        .map { i =>
          if (i == ks._1) txt1
          else if (i == ks._2) txt2
          else if (i == ks._3) txt3
          else if (i > ks._1 && i < ks._3) crossTxt
          else emptyTxt
        }
    }

  }

}
