package mrdimosthenis.feyn.frontend.model

import mrdimosthenis.feyn.game.switches._
import mrdimosthenis.feyn.math.extensions.Threshold
import mrdimosthenis.feyn.quantum.QState

import scala.util.chaining._

case class Model(
                  puzzle: Puzzle,
                  gateSelection: LazyList[Boolean],
                  selectedQubitIndex: Option[Int],
                  nextPuzzleQubits: Int,
                  nextPuzzleGates: Int
                ) {

  private def calculateQState(switches: LazyList[Switch],
                              selection: LazyList[Boolean])
  : QState = {
    switches.zip(selection)
      .filter(_._2)
      .map(_._1)
      .foldLeft(initQState) { (acc, switch) =>
        switch match {
          case q1Switch: Q1Switch =>
            acc.getThrough(
              q1Switch.q1Gate,
              q1Switch.k
            )
          case q2Switch: Q2Switch =>
            acc.getThrough(
              q2Switch.q2Gate,
              q2Switch.ks
            )
          case q3Switch: Q3Switch =>
            acc.getThrough(
              q3Switch.q3Gate,
              q3Switch.ks
            )
        }
      }
  }

  def initQState: QState =
    puzzle
      .qubits
      .pipe(QState.fromQubits)

  def goalQState: QState =
    calculateQState(
      puzzle.switchesWithSelection.map(_._1),
      puzzle.switchesWithSelection.map(_._2)
    )

  def currentQState: QState =
    calculateQState(
      puzzle.switchesWithSelection.map(_._1),
      gateSelection
    )

  def isSolution: Boolean = {
    implicit val error: Threshold = 0.001
    currentQState.almostEqual(goalQState)
  }

}

